// Browser tests for behaviour that cannot be driven with shinytest2.
//
// shinytest2 waits for Shiny to report itself idle after an interaction. A
// shinyTree output never returns a value -- renderTree() pushes the tree with
// sendInputMessage() and returns NULL -- so for an app that shows a tree inside
// a modalDialog, Shiny's output state machine never settles and both
// app$click() and app$get_value() block indefinitely. That is the same defect
// these tests exercise (#79), so the harness cannot be used to test it.
//
// Playwright drives the page directly instead, with real mouse events, and
// reads state out of the DOM. It uses the system Chrome, so CI needs no browser
// download.
//
// Run with: node tests/playwright/run.js
// Requires: playwright (JS), and R with shinyTree installed.

const { chromium } = require('playwright');
const { spawn } = require('child_process');
const path = require('path');
const net = require('net');

const CHROME = process.env.CHROMOTE_CHROME || '/opt/google/chrome/chrome';
const APPS = path.resolve(__dirname, '..', 'testthat', 'apps');

let failures = 0;
let assertions = 0;

function ok(cond, label) {
  assertions++;
  if (cond) {
    console.log(`  ok   ${label}`);
  } else {
    failures++;
    console.log(`  FAIL ${label}`);
  }
}

function equal(actual, expected, label) {
  ok(actual === expected, `${label} (got ${JSON.stringify(actual)}, expected ${JSON.stringify(expected)})`);
}

function freePort() {
  return new Promise((resolve, reject) => {
    const srv = net.createServer();
    srv.unref();
    srv.on('error', reject);
    srv.listen(0, '127.0.0.1', () => {
      const port = srv.address().port;
      srv.close(() => resolve(port));
    });
  });
}

function waitForHttp(url, timeoutMs = 45000) {
  const started = Date.now();
  return new Promise((resolve, reject) => {
    (function attempt() {
      const req = require('http').get(url, (res) => {
        res.resume();
        resolve();
      });
      req.on('error', () => {
        if (Date.now() - started > timeoutMs) {
          reject(new Error(`app did not start within ${timeoutMs}ms: ${url}`));
        } else {
          setTimeout(attempt, 300);
        }
      });
    })();
  });
}

async function withApp(appName, fn) {
  const port = await freePort();
  const appDir = path.join(APPS, appName);
  const libPaths = process.env.SHINYTREE_LIB
    ? `.libPaths(c(${JSON.stringify(process.env.SHINYTREE_LIB)}, .libPaths())); `
    : '';
  const proc = spawn('Rscript', [
    '-e',
    `${libPaths}shiny::runApp(${JSON.stringify(appDir)}, port = ${port}, host = "127.0.0.1", launch.browser = FALSE)`
  ], { stdio: ['ignore', 'pipe', 'pipe'] });

  let log = '';
  proc.stdout.on('data', (d) => { log += d; });
  proc.stderr.on('data', (d) => { log += d; });

  const url = `http://127.0.0.1:${port}`;
  let browser;
  try {
    await waitForHttp(url);
    browser = await chromium.launch({ executablePath: CHROME, headless: true });
    const page = await browser.newPage();
    await page.goto(url);
    await page.waitForFunction(() => window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected());
    await fn(page);
  } catch (err) {
    failures++;
    console.log(`  FAIL ${appName}: ${err.message}`);
    if (log) console.log(`       app log: ${log.trim().split('\n').slice(-3).join(' | ')}`);
  } finally {
    if (browser) await browser.close();
    proc.kill('SIGTERM');
  }
}

const nodeCount = (page) => page.evaluate(() => {
  const el = document.getElementById('tree');
  return el ? el.querySelectorAll('li').length : -1;
});

const renderCount = (page) => page.evaluate(() => {
  const el = document.getElementById('render_count');
  return el ? parseInt(el.textContent.trim(), 10) : -1;
});

async function dismissModal(page) {
  // The modal is created with data-backdrop="static" and data-keyboard="false",
  // so Escape and backdrop clicks will not close it.
  await page.evaluate(() => $('#shiny-modal').modal('hide'));
  await page.waitForFunction(() => !document.getElementById('shiny-modal'));
}

async function openModal(page) {
  await page.click('#show');
  await page.waitForSelector('#tree', { state: 'attached' });
}

// ---------------------------------------------------------------------------

async function testModalReopen() {
  console.log('a tree in a modal repopulates when the modal is reopened (#79)');
  await withApp('test_modal_rerender', async (page) => {
    await openModal(page);
    await page.waitForFunction(() => document.querySelectorAll('#tree li').length === 4);
    equal(await nodeCount(page), 4, 'first open renders 4 nodes');
    equal(await renderCount(page), 1, 'render function ran once');

    await dismissModal(page);
    equal(await nodeCount(page), -1, 'element removed from the DOM on dismiss');

    // The bug: before the fix this stayed at 0 nodes forever.
    await openModal(page);
    await page.waitForFunction(
      () => document.querySelectorAll('#tree li').length === 4,
      null,
      { timeout: 10000 }
    );
    equal(await nodeCount(page), 4, 'second open renders 4 nodes again');

    // The element was removed on dismiss, so the nodes were resent. They are
    // the tree last sent, so the render function does not need to run again.
    equal(await renderCount(page), 1, 'reopen resent the tree without re-rendering');
  });
}

async function testRepeatedReopen() {
  console.log('a tree in a modal survives repeated reopening (#79)');
  await withApp('test_modal_rerender', async (page) => {
    for (let i = 1; i <= 3; i++) {
      await openModal(page);
      await page.waitForFunction(
        () => document.querySelectorAll('#tree li').length === 4,
        null,
        { timeout: 10000 }
      );
      equal(await nodeCount(page), 4, `reopen ${i} renders 4 nodes`);
      await dismissModal(page);
    }
  });
}

async function testSearchAfterReopen() {
  console.log('search still filters in a reopened modal (#79)');
  await withApp('test_modal_rerender', async (page) => {
    await openModal(page);
    await page.waitForFunction(() => document.querySelectorAll('#tree li').length === 4);
    await dismissModal(page);
    await openModal(page);
    await page.waitForFunction(
      () => document.querySelectorAll('#tree li').length === 4,
      null,
      { timeout: 10000 }
    );

    await page.fill('#tree-search-input', 'a1');
    await page.evaluate(() => $('#tree-search-input').trigger('keyup'));
    await page.waitForFunction(
      () => document.querySelector('#tree .jstree-search') !== null,
      null,
      { timeout: 10000 }
    );

    const painted = await page.evaluate(() =>
      Array.from(document.querySelectorAll('#tree li'))
        .filter((l) => l.offsetParent !== null)
        .map((l) => (l.querySelector('a') || {}).textContent)
        .map((t) => (t || '').trim())
    );
    ok(painted.includes('a1'), 'the match is still painted after reopening');
    ok(!painted.includes('b'), 'non-matching nodes are hidden after reopening');
  });
}

async function testCachedTreeInModal() {
  console.log('a cached tree in a modal repopulates on reopen (#109, #79)');
  await withApp('test_bind_cache_modal', async (page) => {
    const labels = () => page.evaluate(() =>
      Array.from(document.querySelectorAll('#tree .jstree-anchor')).map((a) => a.textContent).join(','));
    const openAndWait = async (expected) => {
      await openModal(page);
      await page.waitForFunction(
        (e) => Array.from(document.querySelectorAll('#tree .jstree-anchor')).map((a) => a.textContent).join(',') === e,
        expected,
        { timeout: 10000 }
      );
    };
    const setKey = async (key) => {
      await page.check(`input[name=key][value=${key}]`);
      await page.waitForFunction(
        (k) => Shiny.shinyapp.$inputValues['key:shiny.radioInput'] === k || Shiny.shinyapp.$inputValues.key === k,
        key
      );
    };

    await openAndWait('a1,a2');
    equal(await renderCount(page), 1, 'key a renders');
    await dismissModal(page);

    await setKey('b');
    await openAndWait('b1,b2');
    equal(await renderCount(page), 2, 'new key b renders');
    await dismissModal(page);

    // A cache hit sends the cached tree, and reopening resends it.
    await setKey('a');
    await openAndWait('a1,a2');
    equal(await labels(), 'a1,a2', 'cache hit for key a shows its tree');
    await dismissModal(page);
    await openAndWait('a1,a2');
    equal(await labels(), 'a1,a2', 'reopening after a cache hit shows the same tree');
    equal(await renderCount(page), 2, 'neither the cache hit nor the reopen re-rendered');
  });
}

async function testSlowRenderSentOnce() {
  console.log('a slow render is sent once on page load, not twice');
  await withApp('test_slow_render', async (page) => {
    const messages = await page.evaluate(() => new Promise((resolve) => {
      let n = 0;
      $(document).on('shiny:message', (e) => {
        const m = e.message && e.message.inputMessages;
        if (m) m.forEach((x) => { if (x.id === 'tree') n++; });
      });
      // Long enough for a duplicate send after the 0.5s render to arrive.
      setTimeout(() => resolve(n), 3000);
    }));
    await page.waitForFunction(() => document.querySelectorAll('#tree li').length === 2);
    equal(await nodeCount(page), 2, 'tree renders');
    equal(messages, 1, 'the tree is sent once');
    equal(await renderCount(page), 1, 'the render function runs once');
  });
}

async function testEmptyTreeDoesNotLoop() {
  console.log('a legitimately empty tree does not resend forever (#79)');
  await withApp('test_empty_tree', async (page) => {
    await page.waitForFunction(() => document.getElementById('render_count') !== null);
    await page.waitForTimeout(3000);
    const first = await renderCount(page);
    // Give any feedback loop plenty of wall clock to run away.
    await page.waitForTimeout(5000);
    const second = await renderCount(page);

    ok(first > 0 && first < 5, `render count stays small (got ${first})`);
    equal(second, first, 'render count stops growing');
  });
}

(async () => {
  await testModalReopen();
  await testRepeatedReopen();
  await testSearchAfterReopen();
  await testCachedTreeInModal();
  await testSlowRenderSentOnce();
  await testEmptyTreeDoesNotLoop();

  console.log(`\n${assertions - failures}/${assertions} assertions passed`);
  if (failures > 0) {
    console.log(`${failures} FAILED`);
    process.exit(1);
  }
})();
