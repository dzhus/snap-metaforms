/// Model loading, setting up Backbone model, form template and
/// Knockback integration.

/// Load current JSON model, build Backbone model and view,
/// setup view→model updater.
$(function () {
    var MenuRouter = Backbone.Router.extend({
        routes: {
            ":model/:id/": "instanceSelect",
            ":model/":     "modelSelect"
        },

        modelSelect: function (m) {
            loadModel(m);
        },
        
        instanceSelect: function (m, id) {
            if (m == global.modelName)
                restore(id);
            else
                loadModel(m, id);
        }
    });    

    window.global = {
        formElement: $("#form"),
        knockVM: {},
        loadedModel: {},
        modelName: null,
        mkBackboneModel: null,
        router: new MenuRouter,
        timeliner: null,
    };

    /// Request readable models list from server and render side menu
    $.getJSON("/_/_models/", function (models) {
        var elem = $("#menu");
        var tpl = $("#model-menu-template").html();
        var contents = Mustache.render(tpl, {models: models});
        elem.html(contents);
        /// Start routing after menu has been rendered to highlight
        /// current model properly
        Backbone.history.start();
    });

});

function modelMethod(modelName, method) {
    return "/_/" + modelName + "/" + method;
}

/// Load model definition, set up globals and timeline updater. If id
/// is not null, render form for instance, otherwise render empty
/// form. Highlight active model menu item.
function loadModel(modelName, id) {
    $("#menu-" + global.modelName).removeClass("active");
    $("#menu-" + modelName).addClass("active");

    $.getJSON(modelMethod(modelName, "model"),
              function(model) {
                  global.loadedModel = model;
                  global.modelName = modelName;
                  global.mkBackboneModel = backbonizeModel(model, modelName);
                  $("#model-name").text(model.title);

                  if (_.isNull(global.timeliner))
                      global.timeliner = window.setInterval(refreshTimeline, 5000);

                  var idHash = {};
                  if (id)
                      idHash = {id: String(id)}
                  setupView(new global.mkBackboneModel(idHash));
              });
}

/// Show recently created models, highlight the currently loaded one
function refreshTimeline() {
    $.get(modelMethod(global.modelName, "timeline"),
          function(data) {
              var contents = "";
              var tpl = $("#timeline-item-template").html();
              var currentInstanceId = global.knockVM._kb_vm.model.id;
              _.each(data, function (id) {
                  contents += Mustache.render(tpl, 
                                              {"sel": id == currentInstanceId,
                                               "modelName": global.modelName,
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

/// Render form for model and bind it
function setupView(instance) {
    global.knockVM = new kb.ViewModel(instance);

    refreshTimeline();

    global.formElement.html(renderFormView(global.loadedModel));
    ko.applyBindings(global.knockVM);

    /// Wait a bit to populate model fields and bind form elements
    /// without PUT-backs to server
    window.setTimeout(function () {
        global.knockVM._kb_vm.model.setupServerSync();
    }, 1000);
}

/// Save current model instance
function save() {
    global.knockVM._kb_vm.model.save();
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
    global.knockVM._kb_vm.model.destroy();
    forgetView();
    setupView(new global.mkBackboneModel);
}
