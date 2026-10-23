var shinyTree = function(){
  callbackCounter = 0;
  // Output ids that have been sent a tree at least once this session. Used to
  // tell an element that is being added back -- which needs its tree resent --
  // from one appearing for the first time, whose tree is already on its way.
  var treeReceived = {};

  // --- Lazy loading (#11) ---------------------------------------------------
  // renderTree(lazy = TRUE) sends a URL instead of the tree. jsTree's data
  // option is then a function, called for each node jsTree needs the children
  // of; it asks the server for them over HTTP. Requests made in the same tick
  // are batched into one (jsTree's "massload"), which matters when a search or
  // a state restore opens many branches at once.
  //
  // Nodes are identified to the server by their path of names, which is also
  // what the children function receives, so ids never have to be stable.
  // A node's path is the one the server gave it when it was fetched, kept in
  // its data as stpath, so renaming or moving it with the context menu or
  // drag and drop does not change what the children function is asked for.
  // Nodes without one (e.g. created in the browser) fall back to their labels.
  var lazyPath = function(tree, node) {
    if (node.id === '#') { return []; }
    if (node.data && $.isArray(node.data.stpath)) { return node.data.stpath.slice(); }
    var names = [];
    while (node && node.id !== '#') {
      names.unshift(node.text);
      node = tree.get_node(node.parent);
    }
    return names;
  };
  // The node's st* data, sent with its path so the children function can tell
  // siblings with the same name apart (by an stid, say).
  var lazyNodeData = function(node) {
    var data = {};
    if (node && node.id !== '#' && node.data) {
      $.each(node.data, function(k, v) { if (k !== 'stpath') { data[k] = v; } });
    }
    return data;
  };
  var lazyPost = function(el, body, done, fail) {
    var lazy = el.stLazy;
    body.generation = lazy.generation;
    $.ajax({
      url: lazy.url, type: 'POST', contentType: 'application/json',
      data: JSON.stringify(body), dataType: 'json', cache: false
    }).done(function(res) {
      // A tree rebuilt since the request was made ignores its answer.
      if (el.stLazy !== lazy) { return; }
      done(res);
    }).fail(function(xhr) {
      if (el.stLazy !== lazy) { return; }
      if (xhr.status === 409) { return; }  // stale: a rebuilt tree is on its way
      var msg = (xhr.responseJSON && xhr.responseJSON.error) || ('HTTP ' + xhr.status);
      fail(msg);
    });
  };
  // Children for one node, batched with any others asked for in the same tick.
  var lazyChildren = function(el, path, data, cb) {
    var lazy = el.stLazy;
    lazy.queue.push({path: path, data: data, cb: cb});
    if (lazy.timer) { return; }
    lazy.timer = setTimeout(function() {
      var batch = lazy.queue;
      lazy.queue = [];
      lazy.timer = null;
      lazyPost(el, {paths: batch.map(function(b) { return b.path; }),
                    data: batch.map(function(b) { return b.data; })}, function(res) {
        $.each(batch, function(i, b) {
          var node = res.nodes && res.nodes[i];
          if (node && node.silent) {
            // req(FALSE): not an error, just not yet. The node stays closed and
            // can be opened again.
            b.cb.call($.jstree.reference(el), false);
          } else if (!node || node.error) {
            lazyError(el, b.path, node ? node.error : 'no answer for this node');
            // false, not [], tells jsTree the load failed: the node stays
            // closed and loadable, so opening it again retries, rather than
            // being shown as an empty leaf.
            b.cb.call($.jstree.reference(el), false);
          } else {
            b.cb.call($.jstree.reference(el), node.children || []);
          }
        });
      }, function(msg) {
        $.each(batch, function(i, b) {
          lazyError(el, b.path, msg);
          // false tells jsTree loading failed, so the node can be opened again.
          b.cb.call($.jstree.reference(el), false);
        });
      });
    }, 0);
  };
  var lazyError = function(el, path, msg) {
    var where = path.length ? path.join(' / ') : 'the top level';
    if (window.console) { console.error('shinyTree: could not load ' + where + ': ' + msg); }
    if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
      Shiny.setInputValue(el.id + '_lazy_error', {path: path, message: msg, time: Date.now()},
                          {priority: 'event'});
    }
  };
  // Open every node on each path, including the last, fetching branches on the
  // way down, then call done(). A path that no longer exists stops where it
  // stops matching. One level is handled for all paths together, so the
  // fetches for a level go out as a single batched request.
  var lazyOpenPaths = function(el, paths, done, stillWanted) {
    var tree = $.jstree.reference(el);
    var lazy = el.stLazy;
    var pending = paths.filter(function(p) { return p.length > 0; })
                       .map(function(p) { return {path: p, depth: 0, parent: '#'}; });
    var step = function() {
      // Stop if the tree has been rebuilt, or the caller -- e.g. a search that
      // a newer search replaced -- no longer wants these nodes opened.
      // Otherwise it kept fetching and opening them, hidden, and they all
      // appeared open once the search was cleared.
      if (el.stLazy !== lazy || (stillWanted && !stillWanted())) { return; }
      var next = [];
      var waiting = 0;
      var finishOne = function() { if (--waiting === 0) { setTimeout(step, 0); } };
      var seen = {};
      $.each(pending, function(i, item) {
        var parent = tree.get_node(item.parent);
        if (!parent) { return; }
        // A step is a name, or {name, nth} from a saved state, nth saying which
        // of several same-named siblings it is.
        var want = item.path[item.depth];
        var name = typeof want === 'string' ? want : want.name;
        var nth = typeof want === 'string' ? null : want.nth;
        // With an index, the one sibling it names. With a plain name -- a
        // search result, which cannot say which of several same-named
        // siblings it means -- every sibling of that name, so a match under
        // the second of two "dup"s is opened, not only one under the first.
        var childIds = [];
        var seenSame = 0;
        $.each(parent.children, function(j, cid) {
          var p = lazyPath(tree, tree.get_node(cid));
          if (p[p.length - 1] !== name) { return; }
          if (nth === null || seenSame === nth) { childIds.push(cid); }
          seenSame++;
          if (nth !== null && childIds.length) { return false; }
        });
        // None: the path no longer exists.
        $.each(childIds, function(k, childId) {
          if (item.depth + 1 < item.path.length) {
            next.push({path: item.path, depth: item.depth + 1, parent: childId});
          }
          // Several paths can share a node; open it once. open_node() fetches
          // a branch that is not loaded yet and calls back once it is open --
          // loading it separately as well loaded it twice, and jsTree kept
          // both copies of its children.
          if (seen[childId] || tree.is_open(childId) || !tree.is_parent(childId)) { return; }
          seen[childId] = true;
          waiting++;
          tree.open_node(childId, finishOne, 0);
        });
      });
      pending = next;
      if (waiting > 0) { return; }  // finishOne() runs the next level
      if (!pending.length) { done(); return; }
      setTimeout(step, 0);
    };
    step();
  };

  // A remembered state on a lazy tree: the open branches have to be fetched
  // before they can be reopened, and before a remembered selection inside them
  // exists to restore. Fetch them, then restore as for any tree.
  var restoreLazyState = function(el) {
    var key = el.dataset.stState;
    var saved = null;
    try { saved = JSON.parse(window.localStorage.getItem(key)); } catch (e) { saved = null; }
    if (!saved || saved.version !== 1) { restoreState(el); return; }
    // [{name, nth}] per level; nth is null for state saved without it.
    var toPaths = function(list) {
      return (list || []).map(function(p) {
        return p.split('\u0000').map(splitStep);
      });
    };
    // Open every remembered open branch, and the parents of every remembered
    // selected or checked node, so all of them are loaded.
    var paths = toPaths(saved.open);
    $.each(toPaths(saved.selected).concat(toPaths(saved.checked)), function(i, p) {
      if (p.length > 1) { paths.push(p.slice(0, p.length - 1)); }
    });
    lazyOpenPaths(el, paths, function() { restoreState(el); });
  };

  // --- Remembering state (#19) ----------------------------------------------
  // jsTree's own state plugin restores as soon as jsTree initialises, which
  // in shinyTree is before the tree's contents arrive, so it restored onto an
  // empty tree. shinyTree saves and restores the state itself instead,
  // restoring once, after the first tree has arrived.
  //
  // Nodes are recorded by their path of names from the root rather than by
  // jsTree's ids, which are just positions: a remembered path either matches a
  // node with the same ancestry or matches nothing, so it cannot land on an
  // unrelated node if the tree has changed shape since.
  // Each name on the path is followed, after a \u0001, by which of its
  // same-named siblings the node is (0 for the first), so a remembered node
  // under the second of two siblings called "dup" (#104) is restored under
  // that one, not the first. A path saved before this had no \u0001 parts;
  // it still matches, against every same-named sibling as it did then.
  var nodePath = function(tree, id) {
    var node = tree.get_node(id);
    var names = [];
    while (node && node.id !== '#') {
      var parent = tree.get_node(node.parent);
      var nth = 0;
      $.each(parent.children, function(i, cid) {
        if (cid === node.id) { return false; }
        if (tree.get_node(cid).text === node.text) { nth++; }
      });
      names.unshift(node.text + '\u0001' + nth);
      node = parent;
    }
    return names.join('\u0000');
  };
  // Split one saved step into its name and sibling index. The index comes
  // after the last \u0001 and is all digits; a name can itself contain \u0001
  // (one read from a file, say), so splitting on the first lost the rest of
  // it. A step without an index -- saved by an earlier version -- gives
  // nth = null.
  var splitStep = function(part) {
    var m = /^([\s\S]*)\u0001(\d+)$/.exec(part);
    return m ? {name: m[1], nth: parseInt(m[2], 10)} : {name: part, nth: null};
  };
  var stripSiblingIndex = function(path) {
    return path.split('\u0000').map(function(p) { return splitStep(p).name; }).join('\u0000');
  };
  var hasSiblingIndex = function(path) {
    return path.split('\u0000').every(function(p) { return splitStep(p).nth !== null; });
  };
  var nodesByPath = function(tree) {
    var byPath = {};
    $.each(tree._model.data, function(id) {
      if (id === '#') { return; }
      var path = nodePath(tree, id);
      (byPath[path] = byPath[path] || []).push(id);
      // The old form, names only: kept for state saved by earlier versions.
      var plain = 'plain:' + stripSiblingIndex(path);
      (byPath[plain] = byPath[plain] || []).push(id);
    });
    return byPath;
  };
  var saveState = function(el) {
    var key = el.dataset.stState;
    var tree = $.jstree.reference(el);
    // Not before the first restore: building the first tree fires the change
    // events too, and saving then would overwrite the state about to be read.
    if (!key || !tree || el.stRestoringState || !el.stStateRestored) { return; }
    var open = [], selected = [], checked = [];
    $.each(tree._model.data, function(id, node) {
      if (id === '#') { return; }
      if (node.state.opened && tree.is_parent(id)) { open.push(nodePath(tree, id)); }
      if (node.state.selected) { selected.push(nodePath(tree, id)); }
      if (typeof tree.is_checked === 'function' && el.dataset.stTieSelection == "FALSE" &&
          tree.is_checked(id)) {
        checked.push(nodePath(tree, id));
      }
    });
    try {
      window.localStorage.setItem(key, JSON.stringify(
        {version: 1, open: open, selected: selected, checked: checked}));
    } catch (e) { /* Storage full or disabled: remembering is best effort. */ }
  };
  var restoreState = function(el) {
    var key = el.dataset.stState;
    var tree = $.jstree.reference(el);
    if (!key || !tree) { return; }
    el.stStateRestored = true;
    var saved = null;
    try { saved = JSON.parse(window.localStorage.getItem(key)); } catch (e) { saved = null; }
    if (!saved || saved.version !== 1) { return; }

    var byPath = nodesByPath(tree);
    var ids = function(paths) {
      var out = [];
      $.each(paths || [], function(i, path) {
        var key = hasSiblingIndex(path) ? path : 'plain:' + path;
        out = out.concat(byPath[key] || []);
      });
      return out;
    };
    // "code": the attributes the tree was rendered with win. A node the app
    // marked keeps that mark and the rest of the remembered state is restored.
    // "saved" (default): what the user had replaces what the app rendered.
    var codeWins = el.dataset.stStatePriority === 'code';
    var untied = el.dataset.stTieSelection == "FALSE" && typeof tree.check_node === 'function';

    el.stRestoringState = true;
    try {
      if (!codeWins) {
        tree.close_all(null, 0);
        tree.deselect_all(true);
        if (untied) { tree.uncheck_all(); }
      }
      $.each(ids(saved.open), function(i, id) {
        // Open each ancestor too, so a remembered open node is visible.
        $.each(tree.get_node(id).parents, function(j, p) { if (p !== '#') { tree.open_node(p, false, 0); } });
        tree.open_node(id, false, 0);
      });
      $.each(ids(saved.selected), function(i, id) { tree.select_node(id, true, true); });
      if (untied) {
        $.each(ids(saved.checked), function(i, id) { tree.check_node(id); });
      }
    } finally {
      el.stRestoringState = false;
    }
    // Report the restored state. select_node() above was told not to, so a
    // single update reaches the server rather than one per node.
    tree.trigger('changed', {action: 'restore_state', selected: tree.get_selected()});
    saveState(el);
  };

  var treeOutput = new Shiny.OutputBinding();
  $.extend(treeOutput, {
    find: function(scope) {
      return $(scope).find('.shiny-tree');
    },
    renderValue: function(el, data) {
      // Wipe the existing tree and create a new one.
      $elem = $('#' + el.id);
      
      $elem.jstree('destroy');
      
      $elem.html(data);
      var plugins = [];
      if ($elem.data('st-checkbox') === 'TRUE'){
        plugins.push('checkbox');
      }
      if ($elem.data('st-search') === 'TRUE'){
        plugins.push('search');
      }      
      if ($elem.data('st-dnd') === 'TRUE'){
        plugins.push('dnd');
      }
      if ($elem.data('st-types') === 'TRUE'){
        plugins.push('types');
      }
      if ($elem.data('st-unique') === 'TRUE'){
        plugins.push('unique');
      }
      if ($elem.data('st-sort') === 'TRUE'){
        plugins.push('sort');
      }
      if ($elem.data('st-wholerow') === 'TRUE'){
        plugins.push('wholerow');
      }
      if ($elem.data('st-contextmenu') === 'TRUE'){
        plugins.push('contextmenu');
      }
      var sttypes = null;
      // Replace every hyphen, not just the first. The R side builds this global's
      // name with gsub(), so a tree nested two or more Shiny modules deep has
      // more than one hyphen in its id and String.replace with a string argument
      // would leave all but the first in place -- the lookup then missed and the
      // types plugin failed on a null settings object (#129).
      var elidtypes = el.id.split('-').join('_');
      if (typeof window[elidtypes + "_sttypes"] !== 'undefined'){
      	sttypes = window[elidtypes + "_sttypes"];
      }
      var tree = $(el).jstree({
        'core': {
          "check_callback" : ($elem.data('st-dnd') === 'TRUE') || ($elem.data('st-contextmenu') === 'TRUE'),
          'multiple': ($elem.data('st-multiple') === 'TRUE'),
          'animation': $elem.data('st-animation'),
          'themes': {
            'name': $elem.data('st-theme'), 
            'responsive': true, 
            'stripes': ($elem.data('st-theme-stripes') === 'TRUE'),
            'icons': ($elem.data('st-theme-icons') === 'TRUE'),
            'dots': ($elem.data('st-theme-dots') === 'TRUE') 
          }
        },
        'checkbox': {
          'three_state': ($elem.data('st-three-state') === 'TRUE'),
          'tie_selection': ($elem.data('st-tie-selection') === 'TRUE'),
          'whole_node': ($elem.data('st-whole-node') === 'TRUE')
        },
        "types" : sttypes,
        // Filled in when the server sends a tree: a node array for an ordinary
        // tree, or the lazy loading function below.
        plugins: plugins});
    }
  });
  Shiny.outputBindings.register(treeOutput, 'shinyTree.treeOutput');
  
  var treeInput = new Shiny.InputBinding();
  $.extend(treeInput, {
    find: function(scope) {
      return $(scope).find(".shiny-tree");
    },
    getType: function(){
      return "shinyTree";
    },
    getValue: function(el, keys) {
      /**
       * Prune an object recursively to only include the specified keys.
       * Then add any data.
       **/
        var treeid = $('#' + el.id);
        var fixOutput = function(arr, keys){
        var arrToObj = function(ar){
          var obj = {};
          $.each(ar, function(i, el){
            //add the data for this node
            var data = {};
            // Node data is never rendered, so unlike the node text it must not
            // be trimmed -- doing so corrupted padded keys and fixed-width
            // codes stored in st* attributes (#3).
            $.each($(treeid).jstree(true).get_node(el.id).data, function(key, val){
              data[key] = val;
            });
            el.data = data;
            obj['' + i] = el;
          });
          return obj;
        };
        
        var toReturn = [];
        $.each(arr, function(i, obj){
          if (obj.children && obj.children.length > 0){
            obj.children = arrToObj(fixOutput(obj.children, keys));
          }

          var clean = {};
          // The node text is kept trimmed on purpose: it is a display label,
          // HTML collapses surrounding whitespace on screen anyway, and the
          // renderValue() path builds trees from raw HTML where Shiny's own
          // pretty-printed indentation would otherwise end up in every name.
          // It also keys input$tree, so trimming it is load-bearing (#3).
          $.each(obj, function(key, val){
            if (keys.indexOf(key) >= 0) {
                if (typeof val === 'string'){
                  if (key === "text" && val === "") {
                    val = "New node";
                  }
                  clean[key] = val.trim();
                } else {
                  clean[key] = val;
                }
              }
          });

          // get_json() reports state.checked straight from the node model, which
          // the checkbox plugin leaves at false when tie_selection is on -- it
          // derives checked-ness from the selection instead. is_checked() is the
          // plugin's own accessor and honours either mode, so ask it rather than
          // trusting the raw state. It only exists while the checkbox plugin is
          // loaded, hence the guard (#121).
          var jstreeRef = $(treeid).jstree(true);
          if (clean.state && typeof jstreeRef.is_checked === 'function') {
            clean.state.checked = jstreeRef.is_checked(obj.id);
          }

          toReturn.push(clean);
        });
        result = arrToObj(toReturn);
        return arrToObj(result);
      };
      
      var tree = $.jstree.reference(el);
      if (tree) { // May not be loaded yet.
        if(tree.get_container().find("li").length>0) { // The tree may be initialized but empty
          var js = tree.get_json();
          var fixed = fixOutput(js, ['id', 'state', 'text','children']);
          callbackCounter++;
          fixed.callbackCounter = callbackCounter;
          return fixed;
        }
      }
    },
    setValue: function(el, value) {},
    initialize: function(el) {
      // A tree's contents are pushed to the client rather than returned as an
      // output value, so Shiny has nothing cached to replay when an element is
      // removed from the page and added back -- a modalDialog that is dismissed
      // and reopened, for example. The output is still considered valid, so the
      // render function does not re-run and the new element stays empty (#79).
      //
      // Report an empty tree so the server can resend its contents -- but only
      // for an element added back after its tree was sent. For one appearing
      // for the first time the tree is already on its way: asking anyway made
      // the server send it a second time whenever the render took longer than
      // the check below, and jsTree rebuild it from scratch. treeReceived is
      // read now, when
      // the element is bound, rather than in the timeout, since by then the
      // first tree may already have arrived.
      //
      // The check still runs on a timeout because renderValue() may be on its
      // way for a tree that is being rebuilt.
      if (!treeReceived[el.id]) { return; }
      setTimeout(function(){
        if (!document.body.contains(el)) { return; }
        var tree = $.jstree.reference(el);
        var isEmpty = !tree || tree.get_container().find("li").length === 0;
        if (isEmpty && typeof Shiny !== 'undefined' && Shiny.setInputValue) {
          // priority: 'event' so a second rebind of an identical tree is not
          // discarded as a duplicate value.
          Shiny.setInputValue(el.id + '_rebind', new Date().getTime(),
                              {priority: 'event'});
        }
      }, 100);
    },
    subscribe: function(el, callback) {
      $(el).on("open_node.jstree", function(e) {
        callback();
      });
      
      $(el).on("close_node.jstree", function(e) {
        callback();
      });
      
      var opts = JSON.parse(el.attributes.options.nodeValue);
      // A refresh started by updateTree() re-reports a change the server has
      // just made, so notifying the server about it creates a feedback loop
      // (#36). receiveMessage() sets stServerRefresh before calling refresh();
      // the events that refresh emits then skip the callback while it is set.
      //
      // The flag is NOT cleared by these handlers. refresh() completes
      // asynchronously and the observed event order is
      // model -> load_node -> refresh.jstree -> set_state.jstree, so clearing
      // on either event unmutes the other one. receiveMessage() instead clears
      // the flag from a timeout that runs after the whole refresh has settled.
      //
      // Both handlers are registered unconditionally so that the suppression
      // applies regardless of which option is enabled; the option is checked
      // before notifying the server.
      $(el).on("set_state.jstree", function(e) {
        if (el.stServerRefresh) { return; }
        if (opts.setState && opts.setState === true) { callback(); }
      });
      $(el).on("refresh.jstree", function(e) {
        if (el.stServerRefresh) { return; }
        if (opts.refresh && opts.refresh === true) { callback(); }
      });
      
      $(el).on("changed.jstree", function(e) {
        callback();
      });

      if (el.dataset.stState) {
        // Deferred like the check_node callback above: these handlers are bound
        // before jsTree's own, which is where three_state cascades a check to
        // parents and children. Saving straight away recorded the state before
        // the cascade, so unticking a child of a ticked parent was saved with
        // the parent still ticked, and the reload ticked everything again.
        $(el).on("changed.jstree open_node.jstree close_node.jstree check_node.jstree uncheck_node.jstree check_all.jstree uncheck_all.jstree", function(e) {
          setTimeout(function() { saveState(el); }, 0);
        });
      }
      
      $(el).on("ready.jstree", function(e){
        // Initialize the data.
        callback();
      });
      
      $(el).on("move_node.jstree", function(e){
        callback();
      });
      
      // callbacks for contextmenue
      if (el.dataset.stContextmenu == "TRUE") {
        $(el).on("create_node.jstree", function(e){
          callback();
        });
        $(el).on("rename_node.jstree", function(e){
          callback();
        });
        $(el).on("delete_node.jstree", function(e){
          callback();
        });
        $(el).on("copy_node.jstree", function(e){
          callback();
        });
        $(el).on("cut.jstree", function(e){
          callback();
        });
        $(el).on("copy.jstree", function(e){
          callback();
        });
        $(el).on("paste.jstree", function(e){
          callback();
        });
      }
      
      if (el.dataset.stCheckbox == "TRUE" && el.dataset.stTieSelection == "FALSE") {
        // These handlers are bound when Shiny binds the input, before jsTree
        // initialises and registers its own check_node/uncheck_node handlers,
        // which is where three_state cascades the change to parents and
        // children. jQuery runs handlers in registration order, so reading the
        // tree here synchronously reported the state from before the cascade.
        // Defer the read until jsTree's handlers have run.
        $(el).on("check_node.jstree", function(e) {
          setTimeout(callback, 0);
        });
        $(el).on("uncheck_node.jstree", function(e) {
          setTimeout(callback, 0);
        });
        $(el).on("check_all.jstree", function(e) {
          callback();
        });
        $(el).on("uncheck_all.jstree", function(e) {
          callback();
        });
      }
      
    },
    unsubscribe: function(el) {
      $(el).off(".jstree");
    },
    receiveMessage: function(el, message) {
      // This receives messages of type "updateTree" from the server.
      if(message.type == 'updateTree' && typeof message.data !== 'undefined') {
          // Restore a remembered state only onto the first tree after the page
          // loads; one sent later may be shaped differently.
          if (el.dataset.stState && !treeReceived[el.id]) {
            $(el).one('refresh.jstree', function() { restoreState(el); });
          }
          treeReceived[el.id] = true;
          // Mark this refresh as server-initiated so subscribe() does not echo
          // it back and start a feedback loop (#36).
          //
          // refresh() completes asynchronously: it calls load_node(), and
          // refresh.jstree then set_state.jstree fire from that continuation.
          // The flag therefore has to outlive the refresh() call and is cleared
          // once the refresh has settled. The delay is a compromise -- long
          // enough to cover load_node()'s continuation, short enough that a
          // genuine refresh from elsewhere moments later is still reported.
          el.stServerRefresh = true;
          if (el.stServerRefreshTimer) { clearTimeout(el.stServerRefreshTimer); }
          el.stServerRefreshTimer = setTimeout(function(){
            el.stServerRefresh = false;
            el.stServerRefreshTimer = null;
          }, 100);
          $(el).jstree(true).settings.core.data = JSON.parse(message.data);
          $(el).jstree(true).refresh(true, true);
      }
      // Sent by renderTree(lazy = TRUE) (#11): a URL to fetch nodes from rather
      // than the nodes themselves. Every message starts the tree afresh.
      if (message.type == 'lazyTree') {
        var firstTree = !treeReceived[el.id];
        treeReceived[el.id] = true;
        var tree = $(el).jstree(true);
        if (el.stLazy && el.stLazy.timer) { clearTimeout(el.stLazy.timer); }
        el.stLazy = {url: message.url, generation: message.generation,
                     search: message.search, queue: [], timer: null};
        tree.settings.core.data = function(node, cb) {
          lazyChildren(el, lazyPath(tree, node), lazyNodeData(node), cb);
        };
        // jsTree normally builds nodes in a web worker, from a copy of the tree
        // taken when the work starts. An answer for the previous tree still
        // being built when the tree is rebuilt was then written back over the
        // new tree, leaving the old tree's nodes in it -- and state = TRUE then
        // saved and restored them. Lazy answers are a single level of nodes, so
        // build them in place, where a rebuild always comes after.
        tree.settings.core.worker = false;
        if (el.dataset.stState && firstTree) {
          $(el).one('refresh.jstree', function() { restoreLazyState(el); });
        } else if (el.dataset.stState) {
          // A rebuilt tree starts closed. Save that once it has loaded, so the
          // remembered state is the tree on screen rather than the last change
          // made to the tree it replaced. A restore the rebuild interrupted is
          // abandoned -- the remembered nodes belonged to the old tree -- so
          // saving is allowed from here on; otherwise it stayed off for the
          // rest of the page's life and every later change was lost.
          $(el).one('refresh.jstree', function() {
            el.stStateRestored = true;
            saveState(el);
          });
        }
        el.stServerRefresh = true;
        if (el.stServerRefreshTimer) { clearTimeout(el.stServerRefreshTimer); }
        el.stServerRefreshTimer = setTimeout(function(){
          el.stServerRefresh = false;
          el.stServerRefreshTimer = null;
        }, 100);
        tree.refresh(true, true);
      }
      // Sent by expandTree()/collapseTree(). The resulting open/close events
      // update input$tree as they would for a user clicking (#42).
      if (message.type == 'openAll') {
        $(el).jstree(true).open_all();
      }
      if (message.type == 'closeAll') {
        $(el).jstree(true).close_all();
      }
    }
  });
  
  Shiny.inputBindings.register(treeInput); 

  // A shinyTree is an input and an output under the same id by design: the
  // tree is rendered as an output and read back as input$<id>. Shiny warns
  // about every shared id, in the console and, in dev mode, in an on-page error
  // panel, so every app with a tree showed a false alarm (#110). Drop the
  // message when every id it lists is a tree bound once as each. Anything else,
  // such as a tree id that is genuinely reused, is passed through untouched.
  //
  // The listener captures on window so it runs before Shiny's own listener on
  // window, wherever the event was dispatched.
  window.addEventListener('shiny:client-message', function(ev) {
    var detail = ev.detail || {};
    if (typeof detail.message !== 'string') { return; }
    var lines = detail.message.split('\n').slice(1);
    if (lines.length === 0) { return; }
    var onlyTrees = lines.every(function(line) {
      var m = /^- "(.*)": 1 input and 1 output$/.exec(line);
      if (!m) { return false; }
      var el = document.getElementById(m[1]);
      return el !== null && $(el).hasClass('shiny-tree');
    });
    if (onlyTrees) { ev.stopImmediatePropagation(); }
  }, true);
  
  var exports = {};
  
  exports.initSearch = function(treeId, searchId, searchtime){
    $(function(){
      var to = false;
      $('#' + searchId).keyup(function () {
        if(to) { clearTimeout(to); }
        to = setTimeout(function () {
          var v = $('#' + searchId).val();
          var el = document.getElementById(treeId);
          if (el && el.stLazy) {
            lazySearch(el, v);
          } else {
            $.jstree.reference('#' + treeId).search(v);
          }
        }, searchtime);
      });
    });    
  }

  // Search a lazy tree (#11). The browser only has the nodes fetched so far, so
  // the server finds the matches; the branches leading to them are then
  // fetched and opened, and jsTree's search highlights the matching nodes.
  var lazySearch = function(el, text) {
    var tree = $.jstree.reference(el);
    var lazy = el.stLazy;
    // A newer search makes this one's answer irrelevant.
    var token = lazy.searchToken = (lazy.searchToken || 0) + 1;
    if (!text) {
      tree.clear_search();
      return;
    }
    lazyPost(el, {search: text}, function(res) {
      if (lazy.searchToken !== token) { return; }
      var matches = res.matches || [];
      var key = function(p) { return p.join('\u0000'); };
      var wanted = {};
      $.each(matches, function(i, p) { wanted[key(p)] = true; });
      // Open the parents of every match, so they are fetched and visible.
      var parents = matches.filter(function(p) { return p.length > 1; })
                           .map(function(p) { return p.slice(0, p.length - 1); });
      lazyOpenPaths(el, parents, function() {
        if (lazy.searchToken !== token) { return; }
        // Highlight exactly the server's matches, by path.
        tree.settings.search.search_callback = function(str, node) {
          return wanted[key(lazyPath(tree, node))] === true;
        };
        tree.search(text, true);
        tree.settings.search.search_callback = false;
      }, function() { return lazy.searchToken === token; });
    }, function(msg) {
      if (lazy.searchToken !== token) { return; }
      lazyError(el, [], 'search failed: ' + msg);
    });
  };
  
  return exports;
}()
