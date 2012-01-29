/// Load current JSON metamodel, build Backbone model and view,
/// setup view→model updater.
$(function () {
    /// @todo getJSON doesn't work
    $.get("model/",
          function(data) {
              metamodel = eval(data);
                  
              var metaM = backbonizeModel(metamodel);
              var metaV = backbonizeView(metamodel);
              
              $("#model-name").append(metamodel.name);

              FormModel = new metaM;
              FormView = new metaV({"el": $("#form"), "model": FormModel});
              FormUpdater = window.setInterval(function () {FormView.toModel()}, 2000);
          });
});
