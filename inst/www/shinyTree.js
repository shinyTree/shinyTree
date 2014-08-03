var shinyTree = function(){
  var treeOutput = new Shiny.OutputBinding();
  $.extend(treeOutput, {
    find: function(scope) {
      return $(scope).find('.shiny-tree');
    },
    renderValue: function(el, data) {
      // Wipe the existing tree and create a new one.
      elem = $('#' + el.id)
      elem.html(data);
      var plugins = [];
      if (elem.data('checkbox') === 'TRUE'){
        plugins = ['checkbox'];
      }
      
      var tree = $.jstree.create(el, {plugins: plugins});
    }
  });
  Shiny.outputBindings.register(treeOutput, 'shinyTree.treeOutput');
}()