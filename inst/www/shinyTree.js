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
      var elidtypes = el.id.replace("-", '_');
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
            $.each($(treeid).jstree(true).get_node(el.id).data, function(key, val){
              if (typeof val === 'string'){
                data[key] = val.trim();
              } else {
                data[key] = val; 
              }
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
          $.each(obj, function(key, val){
            if (keys.indexOf(key) >= 0) {
                if (typeof val === 'string'){
                  clean[key] = val.trim();
                } else {
                  clean[key] = val; 
                }
              }
          });
          
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
    subscribe: function(el, callback) {
      $(el).on("open_node.jstree", function(e) {
        callback();
      });
      
      $(el).on("close_node.jstree", function(e) {
        callback();
      });
      
      var opts = JSON.parse(el.attributes.options.nodeValue);
      if (opts.setState && opts.setState === true) {
        $(el).on("set_state.jstree", function(e) {
          callback();
        }); 
      }
      if (opts.refresh && opts.refresh === true) {
        $(el).on("refresh.jstree", function(e) {
          callback();
        }); 
      }
      
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
        $(el).on("check_node.jstree", function(e) {
          callback();
        });
        $(el).on("uncheck_node.jstree", function(e) {
          callback();
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
