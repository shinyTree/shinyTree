var shinyTree = function(){
  var treeOutput = new Shiny.OutputBinding();
  $.extend(treeOutput, {
    find: function(scope) {
      return $(scope).find('.shiny-tree');
    },
    renderValue: function(el, data) {
      // Wipe the existing tree and create a new one.
      $elem = $('#' + el.id)
      
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
      
      var tree = $(el).jstree({'core' : {
        "check_callback" : ($elem.data('st-dnd') === 'TRUE')
      },plugins: plugins});
    }
  });
  Shiny.outputBindings.register(treeOutput, 'shinyTree.treeOutput');
  
  var treeInput = new Shiny.InputBinding();
  $.extend(treeInput, {
    find: function(scope) {
      return $(scope).find(".shiny-tree");
    },
    getType: function(){
      return "shinyTree"
    },
    getValue: function(el, keys) {
      /**
       * Prune an object recursively to only include the specified keys.
       * 'li_attr' is a special key that will actually map to 'li_attrs.class' and
       * will be called 'class' in the output.
       **/
      var prune = function(arr, keys){
        var arrToObj = function(ar){
          var obj = {};
          $.each(ar, function(i, el){
            obj['' + i] = el;
          })
          return obj;
        }
        
        var toReturn = [];
        // Ensure 'children' property is retained.
        keys.push('children');
        
        $.each(arr, function(i, obj){
          if (obj.children && obj.children.length > 0){
            obj.children = arrToObj(prune(obj.children, keys));
          }
          var clean = {};
          $.each(obj, function(key, val){
            if (keys.indexOf(key) >= 0){
              if (key === 'li_attr'){ // We don't really want, just the class attr
                if (!val.class){
                  // Skip without adding element.
                  return;
                }
                val = val.class;
                key = 'class';
              }
              
              if (typeof val === 'string'){
                // TODO: We don't really want to trim but have to b/c of Shiny's pretty-printing.
                clean[key] = val.trim();
              } else {
                clean[key] = val; 
              }
            }
          });
          toReturn.push(clean);
        });
        return arrToObj(toReturn);
      }
      
      var tree = $.jstree.reference(el);
      if (tree){ // May not be loaded yet.
        var js = tree.get_json();
        var pruned =  prune(js, ['state', 'text', 'li_attr']);
        return pruned;
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
      
      $(el).on("changed.jstree", function(e) {
        callback();
      });
      
      $(el).on("ready.jstree", function(e){
        // Initialize the data.
        callback();
      })
      
      $(el).on("move_node.jstree", function(e){
        callback();
      })
    },
    unsubscribe: function(el) {
      $(el).off(".jstree");
    }
  });
  
  Shiny.inputBindings.register(treeInput); 
  
  var exports = {};
  
  exports.initSearch = function(treeId, searchId){
    $(function(){
      var to = false;
      $('#' + searchId).keyup(function () {
        if(to) { clearTimeout(to); }
        to = setTimeout(function () {
          var v = $('#' + searchId).val();
          $.jstree.reference('#' + treeId).search(v);
        }, 250);
      });
    });    
  }
  
  return exports;
}()