var shinyTree = function(){
  callbackCounter = 0;

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
      // Report an empty tree so the server can resend its contents. The check
      // runs on a timeout because renderValue() may still be on its way for a
      // genuinely new tree, and sending the signal then would make the server
      // resend a tree that is about to arrive anyway.
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
    }
  });
  
  Shiny.inputBindings.register(treeInput); 
  
  var exports = {};
  
  exports.initSearch = function(treeId, searchId, searchtime){
    $(function(){
      var to = false;
      $('#' + searchId).keyup(function () {
        if(to) { clearTimeout(to); }
        to = setTimeout(function () {
          var v = $('#' + searchId).val();
          $.jstree.reference('#' + treeId).search(v);
        }, searchtime);
      });
    });    
  }
  
  return exports;
}()
