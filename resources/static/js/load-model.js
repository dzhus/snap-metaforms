/// Load current JSON metamodel, build Backbone model and view,
/// setup view→model updater.
$(function () {
    /// @todo getJSON doesn't work
    $.get("model/",
          function(data) {
              metamodel = eval(data);
                  
              metaM = backbonizeModel(metamodel);
              metaV = backbonizeView(metamodel);
              
              $("#model-name").append(metamodel.name);

              FormModel = new metaM;
              FormView = new metaV({"tagName": "sx", "el": $("#form"), "model": FormModel});
              FormUpdater = window.setInterval(
                  function () {
                      FormView.toModel();
                  }, 2000);
          });
});

/// Clean form and rebind it to new model
function flushForm() {
    FormModel = new metaM;
    /// @todo Perhaps we should destroy this view and create new one
    /// from scratch.
    FormView.rebind(FormModel);
}

/// Remove current model from storage
function remove() {
    if (!FormModel.isNew())
    {
        FormModel.destroy();
        flushForm();
    }
}

/// Save current model and start fresh form
function proceed() {
    FormModel.save();
    flushForm();
}
