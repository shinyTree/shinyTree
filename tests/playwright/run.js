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
  // A test app's name, or an absolute path, e.g. to an installed example.
  const appDir = path.isAbsolute(appName) ? appName : path.join(APPS, appName);
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

// Reload the page and wait for the tree, keeping the browser's localStorage.
async function reloadTree(page, query) {
  const base = page.url().split('?')[0];
  await page.goto(base + (query || ''));
  await page.waitForFunction(() => window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected());
  await page.waitForFunction(() => document.querySelectorAll('#tree li').length > 0);
  await page.waitForTimeout(700);
}

// The tree's open / selected / checked nodes, as paths like "root2/b1".
const treeState = (page) => page.evaluate(() => {
  const t = $('#tree').jstree(true);
  const path = (id) => { const n = []; let x = t.get_node(id); while (x && x.id !== '#') { n.unshift(x.text); x = t.get_node(x.parent); } return n.join('/'); };
  const ids = Object.keys(t._model.data).filter((id) => id !== '#');
  return {
    open: ids.filter((id) => t.is_parent(id) && t.is_open(id)).map(path).sort().join(','),
    selected: t.get_selected().map(path).sort().join(','),
    checked: (typeof t.get_checked === 'function' ? t.get_checked() : []).map(path).sort().join(','),
    server: document.getElementById('report').textContent.trim().replace(/\s+/g, ' ')
  };
});

// Run a change against the tree, finding nodes by path.
const changeTree = (page, fn) => page.evaluate((src) => {
  const t = $('#tree').jstree(true);
  const id = (p) => Object.keys(t._model.data).find((i) => {
    if (i === '#') return false;
    const n = []; let x = t.get_node(i);
    while (x && x.id !== '#') { n.unshift(x.text); x = t.get_node(x.parent); }
    return n.join('/') === p;
  });
  (new Function('t', 'id', src))(t, id);
}, fn);

async function testStateRemembered() {
  console.log('state = TRUE remembers open/selected/checked nodes across reloads (#19)');
  await withApp('test_state', async (page) => {
    await page.evaluate(() => localStorage.clear());
    await reloadTree(page);
    let s = await treeState(page);
    equal(s.open, 'root2', 'first load uses the rendered stopened');
    equal(s.selected, 'root2/b1', 'first load uses the rendered stselected');

    // Change everything the app rendered: close root2, open root1 and its b,
    // and select the "dup" under b -- a name that also exists under a.
    await changeTree(page, "t.deselect_all(); t.close_node(id('root2')); t.open_node(id('root1'));");
    await page.waitForTimeout(200);
    await changeTree(page, "t.open_node(id('root1/b')); t.select_node(id('root1/b/dup'));");
    await page.waitForTimeout(400);

    await reloadTree(page);
    s = await treeState(page);
    equal(s.open, 'root1,root1/b', 'open nodes restored, overriding the rendered stopened');
    equal(s.selected, 'root1/b,root1/b/dup', 'selection restored by path, not by name');
    ok(/selected: root1\/b\/dup root1\/b/.test(s.server), `the restored selection reaches the server (got "${s.server}")`);

    // state_priority = "code": the rendered attributes win, the rest is restored.
    await reloadTree(page, '?priority=code');
    s = await treeState(page);
    equal(s.open, 'root1,root1/b,root2', 'code priority keeps the rendered stopened and adds the saved');
    equal(s.selected, 'root1/b,root1/b/dup,root2/b1', 'code priority keeps the rendered stselected and adds the saved');
  });
}

async function testStateUntiedUntickSaved() {
  console.log('unticking a child of a ticked parent is saved after the cascade (#19)');
  await withApp('test_state', async (page) => {
    await page.evaluate(() => localStorage.clear());
    await reloadTree(page, '?tied=0');
    await changeTree(page, "t.uncheck_all(); t.deselect_all(); t.open_all();");
    await page.waitForTimeout(300);
    // Tick root1 (cascading to all of it), then untick one leaf under it.
    await changeTree(page, "t.check_node(id('root1'));");
    await page.waitForTimeout(100);
    await changeTree(page, "t.uncheck_node(id('root1/a/a1'));");
    await page.waitForTimeout(400);
    const before = (await treeState(page)).checked;
    await reloadTree(page, '?tied=0');
    equal((await treeState(page)).checked, before, 'the reload restores what was visibly ticked');
  });
}

async function testStateUntiedCheckboxes() {
  console.log('state = TRUE remembers checkboxes untied from the selection (#19)');
  await withApp('test_state', async (page) => {
    await page.evaluate(() => localStorage.clear());
    await reloadTree(page, '?tied=0');
    await changeTree(page, "t.uncheck_all(); t.check_node(id('root2/b2')); t.deselect_all();");
    await page.waitForTimeout(400);
    await reloadTree(page, '?tied=0');
    const s = await treeState(page);
    equal(s.checked, 'root2/b2', 'checked node restored');
    equal(s.selected, '', 'selection restored separately (none)');
    ok(/checked: root2\/b2/.test(s.server), `the restored checks reach the server (got "${s.server}")`);
  });
}

// --- Lazy loading (#11) ------------------------------------------------------

const lazyModel = (page) => page.evaluate(() => {
  const t = $('#tree').jstree(true), m = t._model.data;
  const path = (id) => { const n = []; let x = t.get_node(id); while (x && x.id !== '#') { n.unshift(x.text); x = t.get_node(x.parent); } return n.join('/'); };
  const ids = Object.keys(m).filter((i) => i !== '#');
  const reach = new Set();
  (function walk(id) { (m[id].children || []).forEach((c) => { reach.add(c); walk(c); }); })('#');
  const dupKids = ids.filter((i) => m[i].children.length !== new Set(m[i].children.map((c) => m[c].text + '|' + JSON.stringify(m[c].data && m[c].data.stid))).size).length;
  return {
    nodes: ids.length, orphans: ids.length - reach.size, duplicated: dupKids,
    open: ids.filter((i) => t.is_open(i)).map(path).sort().join(','),
    selected: t.get_selected().map(path).sort().join(','),
    report: (document.getElementById('report') || {}).textContent || ''
  };
});
const lazyOpen = (page, p) => page.evaluate((p) => new Promise((resolve) => {
  const t = $('#tree').jstree(true);
  const id = Object.keys(t._model.data).find((i) => {
    if (i === '#') return false;
    const n = []; let x = t.get_node(i);
    while (x && x.id !== '#') { n.unshift(x.text); x = t.get_node(x.parent); }
    return n.join('/') === p;
  });
  if (!id) { resolve('missing'); return; }
  t.open_node(id, () => resolve('ok'));
  setTimeout(() => resolve('timeout'), 8000);
}), p);

async function testLazyLoading() {
  console.log('renderTree(lazy = TRUE) fetches nodes as they are opened (#11)');
  await withApp('test_lazy', async (page) => {
    await page.evaluate(() => localStorage.clear());
    await reloadTree(page);
    let s = await lazyModel(page);
    equal(s.nodes, 5, 'only the top level is sent');
    equal(await lazyOpen(page, 'g1_1'), 'ok', 'a branch opens');
    equal(await lazyOpen(page, 'g1_1/g1_1.2'), 'ok', 'a grandchild opens');
    s = await lazyModel(page);
    equal(s.nodes, 11, 'only the opened branches were fetched');
    equal(s.orphans + s.duplicated, 0, 'no orphaned or duplicated nodes');

    // Same-named siblings get their own children (function(path, node)).
    await page.evaluate(() => { const t = $('#tree').jstree(true); t.get_node('#').children.filter((c) => t.get_node(c).text === 'dup').forEach((c) => t.open_node(c)); });
    await page.waitForFunction(() => document.querySelectorAll('#tree a.jstree-anchor').length >= 13);
    const dupKids = await page.evaluate(() => Array.from(document.querySelectorAll('#tree a.jstree-anchor')).map((a) => a.textContent).filter((t) => /^child of dup/.test(t)).sort().join(','));
    equal(dupKids, 'child of dup 1,child of dup 2', 'same-named siblings are told apart by stid');

    // A search finds a node that was never fetched.
    await page.fill('#tree-search-input', 'g1_3.2.1');
    await page.evaluate(() => $('#tree-search-input').trigger('keyup'));
    await page.waitForFunction(() => Array.from(document.querySelectorAll('#tree .jstree-search')).some((a) => a.textContent === 'g1_3.2.1'), null, { timeout: 15000 });
    s = await lazyModel(page);
    ok(s.open.includes('g1_3/g1_3.2'), 'the search opened the branches leading to the match');
    equal(s.orphans + s.duplicated, 0, 'no orphaned or duplicated nodes after a search');
  });
}

async function testLazyRebuildDuringFetch() {
  console.log('a lazy tree rebuilt while a fetch is in flight keeps nothing of the old tree (#11)');
  await withApp('test_lazy', async (page) => {
    await page.evaluate(() => localStorage.clear());
    await reloadTree(page, '?delay=1');
    await page.evaluate(() => { const t = $('#tree').jstree(true); t.open_node(Object.keys(t._model.data).find((i) => t.get_node(i).text === 'g1_3')); });
    await page.waitForTimeout(300);
    await page.click('#rebuild');
    await page.waitForFunction(() => Array.from(document.querySelectorAll('#tree a.jstree-anchor')).some((a) => a.textContent === 'g2_1'));
    await page.waitForTimeout(2500);   // let the old tree's answer arrive
    const s = await lazyModel(page);
    equal(s.orphans, 0, 'no nodes left over from the old tree');
    equal(s.open, '', 'nothing from the old tree is open');
    const stored = await page.evaluate(() => localStorage.getItem('shinyTree-tree') || '');
    ok(!/g1_/.test(stored), `the saved state holds nothing from the old tree (got ${stored})`);
  });
}

async function testLazyStateRestore() {
  console.log('state = TRUE restores a lazy tree several levels deep (#11, #19)');
  await withApp('test_lazy', async (page) => {
    await page.evaluate(() => localStorage.clear());
    await reloadTree(page);
    await lazyOpen(page, 'g1_1'); await lazyOpen(page, 'g1_1/g1_1.2'); await lazyOpen(page, 'g1_1/g1_1.2/g1_1.2.3');
    await page.evaluate(() => { const t = $('#tree').jstree(true); t.select_node(Object.keys(t._model.data).find((i) => t.get_node(i).text === 'g1_1.2.3.1')); });
    await page.waitForTimeout(500);
    await reloadTree(page);
    // Not caught: if the restored selection never reaches the server, the
    // test fails here rather than going on to check a half-restored tree.
    await page.waitForFunction(() => /g1_1\.2\.3\.1/.test(document.getElementById('report').textContent), null, { timeout: 15000 });
    const s = await lazyModel(page);
    equal(s.open, 'g1_1,g1_1/g1_1.2,g1_1/g1_1.2/g1_1.2.3', 'the open branches are fetched and reopened');
    equal(s.selected, 'g1_1/g1_1.2/g1_1.2.3/g1_1.2.3.1', 'the selection deep inside them is restored');
    ok(/g1_1\/g1_1\.2\/g1_1\.2\.3\/g1_1\.2\.3\.1/.test(s.report), 'and reaches the server');
  });
}

async function testLazyRebuildReportsNewTree() {
  console.log('a rebuilt lazy tree is reported to the server, dropping the old selection (#11)');
  await withApp('test_lazy', async (page) => {
    await page.evaluate(() => localStorage.clear());
    await reloadTree(page);
    await page.evaluate(() => { const t = $('#tree').jstree(true); t.select_node(t.get_node('#').children[0]); });
    await page.waitForFunction(() => /selected: g1_1/.test(document.getElementById('report').textContent), null, { timeout: 10000 });
    await page.click('#rebuild');
    await page.waitForFunction(() => Array.from(document.querySelectorAll('#tree a.jstree-anchor')).some((a) => a.textContent === 'g2_1'));
    await page.waitForTimeout(1500);
    const browser = await page.evaluate(() => $('#tree').jstree(true).get_selected().length);
    const server = await page.evaluate(() => document.getElementById('report').textContent.trim());
    equal(browser, 0, 'the rebuilt tree has nothing selected');
    ok(!/g1_/.test(server), `input$tree no longer reports the old tree's selection (got "${server}")`);
  });
}

async function testLazyRebuildDuringRestore() {
  console.log('a rebuild during a state restore does not stop later changes being saved (#11, #19)');
  await withApp('test_lazy', async (page) => {
    await page.evaluate(() => localStorage.setItem('shinyTree-tree', JSON.stringify({
      version: 1,
      open: ['g1_3', 'g1_3\u0000g1_3.2', 'g1_3\u0000g1_3.2\u0000g1_3.2.1'],
      selected: ['g1_3\u0000g1_3.2\u0000g1_3.2.1\u0000g1_3.2.1.2'], checked: []
    })));
    await reloadTree(page, '?delay=1');
    // Rebuild while the restore is still fetching its branches.
    await page.waitForTimeout(800);
    await page.click('#rebuild');
    await page.waitForFunction(() => Array.from(document.querySelectorAll('#tree a.jstree-anchor')).some((a) => a.textContent === 'g2_1'));
    await page.waitForTimeout(3500);
    // A change to the rebuilt tree must be remembered.
    equal(await lazyOpen(page, 'g2_1'), 'ok', 'a branch of the rebuilt tree opens');
    await page.waitForTimeout(500);
    const stored = await page.evaluate(() => localStorage.getItem('shinyTree-tree') || '');
    ok(/g2_1/.test(stored) && !/g1_3/.test(stored), `the change is saved, not the old restore (got ${stored})`);
  });
}

async function testLazyStateDuplicateSiblings() {
  console.log('state = TRUE restores the right one of two same-named siblings (#11, #19, #104)');
  await withApp('test_lazy', async (page) => {
    await page.evaluate(() => localStorage.clear());
    await reloadTree(page);
    // Open the SECOND "dup" and select its child.
    await page.evaluate(() => new Promise((r) => { const t = $('#tree').jstree(true); const dups = t.get_node('#').children.filter((c) => t.get_node(c).text === 'dup'); t.open_node(dups[1], () => r()); }));
    await page.evaluate(() => { const t = $('#tree').jstree(true); t.select_node(Object.keys(t._model.data).find((i) => t.get_node(i).text === 'child of dup 2')); });
    await page.waitForTimeout(500);
    // With checkboxes, three_state also selects the second "dup", whose only
    // child this is; the restore must give back exactly this.
    const before = await page.evaluate(() => { const t = $('#tree').jstree(true); return t.get_selected(true).map((n) => n.text + (n.text === 'dup' ? '#' + t.get_node('#').children.indexOf(n.id) : '')).sort().join(','); });
    await reloadTree(page);
    await page.waitForTimeout(1500);
    const st = await page.evaluate(() => {
      const t = $('#tree').jstree(true);
      const dups = t.get_node('#').children.filter((c) => t.get_node(c).text === 'dup');
      return { first_open: t.is_open(dups[0]), second_open: t.is_open(dups[1]),
               selected: t.get_selected(true).map((n) => n.text + (n.text === 'dup' ? '#' + t.get_node('#').children.indexOf(n.id) : '')).sort().join(',') };
    });
    equal(st.second_open && !st.first_open, true, 'the second "dup" is reopened, not the first');
    equal(st.selected, before, 'and exactly the selection inside it is restored');
  });
}

async function testLazySearchDuplicateSiblings() {
  console.log('a search opens matches under every same-named sibling (#11, #104)');
  await withApp('test_lazy', async (page) => {
    await page.evaluate(() => localStorage.clear());
    await reloadTree(page);
    await page.fill('#tree-search-input', 'shared');
    await page.evaluate(() => $('#tree-search-input').trigger('keyup'));
    await page.waitForFunction(() => document.querySelectorAll('#tree .jstree-search').length >= 1, null, { timeout: 10000 });
    await page.waitForTimeout(600);
    const st = await page.evaluate(() => {
      const t = $('#tree').jstree(true);
      const dups = t.get_node('#').children.filter((c) => t.get_node(c).text === 'dup');
      return { open: dups.map((d) => t.is_open(d)).join(','), hits: document.querySelectorAll('#tree .jstree-search').length };
    });
    equal(st.open, 'true,true', 'both "dup"s are opened');
    equal(st.hits, 2, 'and both matches are highlighted');
  });
}

async function testLazyStateControlCharName() {
  console.log('state = TRUE restores below a node whose name contains \\u0001 (#11)');
  await withApp('test_lazy', async (page) => {
    await page.evaluate(() => localStorage.clear());
    await reloadTree(page);
    await lazyOpen(page, 'g1_3');
    await lazyOpen(page, 'g1_3/ctl\u0001x');
    await page.evaluate(() => { const t = $('#tree').jstree(true); t.select_node(Object.keys(t._model.data).find((i) => t.get_node(i).text === 'inner')); });
    await page.waitForTimeout(500);
    await reloadTree(page);
    await page.waitForTimeout(1200);
    const s = await lazyModel(page);
    equal(s.open, 'g1_3,g1_3/ctl\u0001x', 'both branches reopen');
    equal(s.selected, 'g1_3/ctl\u0001x/inner', 'and the selection inside is restored');
  });
}

async function testLazySupersededSearch() {
  console.log('a search replaced by a newer one stops opening branches (#11)');
  await withApp('test_lazy', async (page) => {
    await page.evaluate(() => localStorage.clear());
    await reloadTree(page, '?delay=0.6&searchfn=1');
    // A deep match, found at once by the search function, with three levels
    // of branches to fetch on the way to it at 0.6s each.
    await page.fill('#tree-search-input', 'g1_3.2.1.3');
    await page.evaluate(() => $('#tree-search-input').trigger('keyup'));
    await page.waitForTimeout(900);
    // Replace it before it has finished opening its branches.
    await page.fill('#tree-search-input', 'g1_1');
    await page.evaluate(() => $('#tree-search-input').trigger('keyup'));
    await page.waitForTimeout(3500);
    const s = await lazyModel(page);
    ok(!s.open.includes('g1_3/g1_3.2/g1_3.2.1'), `the first search stopped opening branches (open: ${s.open})`);
  });
}

async function testLazyRenameKeepsPath() {
  console.log('a renamed lazy node still fetches its children by its original path (#11)');
  await withApp('test_lazy', async (page) => {
    await page.evaluate(() => localStorage.clear());
    await reloadTree(page);
    await page.evaluate(() => { const t = $('#tree').jstree(true); t.settings.core.check_callback = true; t.rename_node(Object.keys(t._model.data).find((i) => t.get_node(i).text === 'g1_2'), 'Renamed'); });
    equal(await lazyOpen(page, 'Renamed'), 'ok', 'the renamed node opens');
    const kids = await page.evaluate(() => { const t = $('#tree').jstree(true); const id = Object.keys(t._model.data).find((i) => t.get_node(i).text === 'Renamed'); return t.get_node(id).children.map((c) => t.get_node(c).text).join(','); });
    equal(kids, 'g1_2.1,g1_2.2,g1_2.3', 'with the children of its original path');
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

const waitText = (page, id, re, timeout = 10000) => page.waitForFunction(
  ([id, src]) => new RegExp(src).test(document.getElementById(id).textContent), [id, re.source], { timeout });

async function testTreeReportedWithoutClick() {
  console.log('input$tree is set once the tree loads, before any click');
  await withApp('test_js_robust', async (page) => {
    await waitText(page, 'report', /names: a1,a2 \|/);
    ok(true, 'input$tree reports the rendered tree without any interaction');
  });
}

async function testRerenderAfterReqKeepsInput() {
  console.log('a tree re-rendered after a req() failure still updates input$tree');
  await withApp('test_js_robust', async (page) => {
    await waitText(page, 'report', /names: a1,a2 \|/);
    await page.click('#show');
    await page.waitForFunction(() => document.getElementById('tree').classList.contains('recalculating') ||
                                      !Shiny.shinyapp.$values.tree);
    await page.waitForTimeout(300);
    await page.click('#show');
    await page.waitForFunction(() => document.querySelectorAll('#tree li').length === 2);
    await page.click('#tree a.jstree-anchor >> text=a1');
    await waitText(page, 'report', /selected: a1$/);
    ok(true, 'a click after the re-render reaches the server');
  });
}

async function testServerTreeReplacesSelection() {
  console.log('a new tree from the server replaces the old one in input$tree');
  await withApp('test_js_robust', async (page) => {
    await waitText(page, 'report', /names: a1,a2 \|/);
    await page.click('#tree a.jstree-anchor >> text=a1');
    await waitText(page, 'report', /selected: a1$/);
    await page.click('#swap');
    await waitText(page, 'report', /names: b1,b2 \| selected: $/);
    ok(true, 'input$tree reports the new tree, with nothing selected');
  });
}

async function testIdNeedingEscape() {
  console.log('a tree whose id contains "." works, and does not stop other inputs binding');
  await withApp('test_js_dotted_id', async (page) => {
    await waitText(page, 'plain_report', /names: p/);
    ok(true, 'an ordinary tree on the same page still reports');
    await waitText(page, 'dotted', /names: x,y \|/);
    const info = await page.evaluate(() => {
      const el = document.getElementById('my.tree');
      return { checkbox: !!el.querySelector('.jstree-checkbox'), leafIcon: !!el.querySelector('.fa-leaf') };
    });
    ok(info.checkbox, 'its options are applied (checkboxes shown)');
    ok(info.leafIcon, 'its types are applied');
    await page.click('[id="my.tree"] a.jstree-anchor >> text=y');
    await waitText(page, 'dotted', /checked: y$/);
    await page.fill('[id="my.tree-search-input"]', 'x');
    await page.waitForFunction(() => document.querySelectorAll('[id="my.tree"] .jstree-search').length === 1);
    ok(true, 'its search box works');
  });
}

async function testMessagesBeforeTreeExists() {
  console.log('updateTree()/expandTree() on a tree that has not rendered do not throw');
  await withApp('test_js_robust', async (page) => {
    // Shiny catches an error in receiveMessage() and logs it to the console.
    const errors = [];
    page.on('pageerror', (e) => errors.push(e.message));
    page.on('console', (m) => { if (m.type() === 'error' && /receiveMessage/.test(m.text())) errors.push(m.text()); });
    await waitText(page, 'report', /names: a1,a2 \|/);
    await page.click('#update_hidden');
    await page.click('#expand_hidden');
    await waitText(page, 'hidden_report', /updates sent: 2/);
    await page.waitForTimeout(500);
    equal(errors.join('; '), '', 'no JavaScript errors');
  });
}

async function testDeletingLastNodeReported() {
  console.log('deleting every node is reported in input$tree');
  await withApp('test_js_robust', async (page) => {
    await waitText(page, 'report', /names: a1,a2 \|/);
    await page.evaluate(() => { const t = $('#tree').jstree(true); t.delete_node(t.get_node('#').children.slice()); });
    await waitText(page, 'report', /^names:  \| selected: $/);
    ok(true, 'input$tree is empty after the last node is deleted');
  });
}

// The installed copy of an example, as R would run it.
function exampleDir(name) {
  const lib = process.env.SHINYTREE_LIB ? `.libPaths(c(${JSON.stringify(process.env.SHINYTREE_LIB)}, .libPaths())); ` : '';
  return require('child_process').execFileSync('Rscript', ['-e',
    `${lib}cat(system.file("examples", ${JSON.stringify(name)}, package = "shinyTree"))`]).toString().trim();
}

async function testExample26State() {
  console.log('26-state: the tree comes back as the user left it, or as the code says');
  await withApp(exampleDir('26-state'), async (page) => {
    const nodeIn = (id, text) => page.evaluate(([id, text]) => {
      const t = $.jstree.reference(document.getElementById(id));
      return Object.keys(t._model.data).find((k) => k !== '#' && t.get_node(k).text === text) || null;
    }, [id, text]);
    const look = (id) => page.evaluate((id) => {
      const t = $.jstree.reference(document.getElementById(id));
      const find = (x) => Object.keys(t._model.data).find((k) => k !== '#' && t.get_node(k).text === x);
      return {
        europe: t.is_open(find('Europe')), asia: t.is_open(find('Asia')),
        selected: t.get_selected().map((k) => t.get_node(k).text).sort().join(',')
      };
    }, id);
    const ready = async () => {
      await page.waitForFunction(() => window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected());
      await page.waitForFunction(() => ['saved', 'code'].every((id) => {
        const el = document.getElementById(id); return el && el.querySelectorAll('li').length > 0;
      }));
      await page.waitForTimeout(500);  // the state is restored after the tree arrives
    };
    await page.evaluate(() => localStorage.clear());
    await page.reload(); await ready();
    for (const id of ['saved', 'code']) {
      const s = await look(id);
      ok(s.europe && !s.asia && s.selected === 'Paris', `${id}: first visit shows the app's own stopened and stselected (got ${JSON.stringify(s)})`);
    }
    await waitText(page, 'selected', /Europe \/ France \/ Paris/);

    // The user closes Europe, opens Asia and Japan, and picks Tokyo instead.
    for (const id of ['saved', 'code']) {
      await page.evaluate((id) => {
        const t = $.jstree.reference(document.getElementById(id));
        const find = (x) => Object.keys(t._model.data).find((k) => k !== '#' && t.get_node(k).text === x);
        t.deselect_all(); t.close_node(find('Europe')); t.open_node(find('Asia'), false, 0); t.open_node(find('Japan'), false, 0);
      }, id);
      const tokyo = await nodeIn(id, 'Tokyo');
      await page.click(`[id="${id}"] [id="${tokyo}_anchor"]`);
    }
    await waitText(page, 'selected', /Asia \/ Japan \/ Tokyo/);
    await page.waitForTimeout(500);  // saved after the change settles

    await page.reload(); await ready();
    let s = await look('saved');
    ok(!s.europe && s.asia && s.selected === 'Tokyo', `saved: the user's changes win after a reload (got ${JSON.stringify(s)})`);
    await waitText(page, 'selected', /Asia \/ Japan \/ Tokyo/);
    ok(!/Paris/.test(await page.textContent('#selected')), 'and the server sees the restored selection, not the app\'s');
    s = await look('code');
    ok(s.europe && s.asia && s.selected === 'Paris,Tokyo', `code: the app's Europe and Paris stay, the rest is restored around them (got ${JSON.stringify(s)})`);
    await page.evaluate(() => localStorage.clear());
  });
}

(async () => {
  await testModalReopen();
  await testRepeatedReopen();
  await testSearchAfterReopen();
  await testCachedTreeInModal();
  await testSlowRenderSentOnce();
  await testStateRemembered();
  await testStateUntiedCheckboxes();
  await testStateUntiedUntickSaved();
  await testLazyLoading();
  await testLazyRebuildDuringFetch();
  await testLazyStateRestore();
  await testLazyRebuildReportsNewTree();
  await testLazyRebuildDuringRestore();
  await testLazyStateDuplicateSiblings();
  await testLazySearchDuplicateSiblings();
  await testLazyStateControlCharName();
  await testLazySupersededSearch();
  await testLazyRenameKeepsPath();
  await testEmptyTreeDoesNotLoop();
  await testTreeReportedWithoutClick();
  await testRerenderAfterReqKeepsInput();
  await testServerTreeReplacesSelection();
  await testIdNeedingEscape();
  await testMessagesBeforeTreeExists();
  await testDeletingLastNodeReported();
  await testExample26State();

  console.log(`\n${assertions - failures}/${assertions} assertions passed`);
  if (failures > 0) {
    console.log(`${failures} FAILED`);
    process.exit(1);
  }
})();
