/// Load current JSON metamodel, build Backbone model and view
$(function () {
    /// @todo getJSON doesn't work
    $.get("model/",
          function(data) {
              metamodel = eval(data);
                  
              var M = backbonizeModel(metamodel);
              var V = backbonizeView(metamodel);
              $("#model-name").append(metamodel.name);
              obj = new M;
              view = new V({"el": $("#form"), "model": obj});
              view.render();
          });
});
