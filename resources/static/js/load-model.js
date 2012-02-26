/// Model loading, setting up Backbone model, form template and
/// Knockback integration.

/// Load current JSON model, build Backbone model and view,
/// setup view→model updater.
$(function () {
    window.global = {
        formElement: $("#form"),
        knockVM: {},
        loadedModel: {},
        modelName: null,
        mkBackboneModel: null,
    };

    setupModel("scp");
});

function modelMethod(modelName, method) {
    return "/_/" + modelName + "/" + method;
}

/// Load model definition, set up globals and TimelineUpdater
function setupModel(modelName) {
    $.getJSON(modelMethod(modelName, "model"),
              function(model) {
                  global.modelName = modelName;
                  global.loadedModel = model;
                  global.mkBackboneModel = backbonizeModel(model);
                  $("#model-name").append(model.title);
                  setupView(new global.mkBackboneModel);
              });
}

/// Show recently created models, highlight the currently loaded one
function refreshTimeline() {
    $.get(modelMethod(global.modelName, "timeline"),
          function(data) {
              var contents = "";
              var tpl = $("#timeline-item").html();

              _.each(data, function (id) {
                  contents += Mustache.render(tpl, {"sel": id == global.knockVM.model.id,
                                                    "id": id});
              });

              $("#timeline").html(contents);
          });
}

/// Delete form, release observables
function forgetView() {
    kb.vmRelease(global.knockVM);
    global.formElement.empty();
}

/// Create form for model and fill it
function setupView(instance) {
    global.knockVM = new kb.viewModel(instance);

    refreshTimeline();

    global.formElement.html(renderFormView(global.loadedModel));
    ko.applyBindings(global.knockVM);

    /// Wait a bit to populate model fields and bind form elements
    /// without PUT-backs to server
    window.setTimeout(function () {
        global.knockVM.model.setupServerSync();
    }, 1000);
}

/// Save current model instance
function save() {
    global.knockVM.model.save();
}

/// Save current model instance and start fresh form
function proceed() {
    save();
    forgetView();
    setupView(new global.mkBackboneModel);
}

/// Load existing model instance
function restore(id) {
    forgetView();
    setupView(new global.mkBackboneModel({"id": String(id)}));
}

/// Remove currently loaded instance from storage and start fresh form
function remove(id) {
    global.knockVM.model.destroy();
    forgetView();
    setupView(new global.mkBackboneModel);
}
