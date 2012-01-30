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

              setupView(new metaM);

              refreshTimeline();
              TimelineUpdater = window.setInterval(refreshTimeline, 5000);
          });
});

function refreshTimeline() {
    $.get("timeline/",
          function(data) {
              var contents = "";
              var tpl = $("#timeline-item").html();

              _.each(data, function (v) {
                  contents += Mustache.render(tpl, {"sel": v == FormView.model.id,
                                                    "value": v});
              });
                  
              $("#timeline").html(contents);
          });                         
}

/// Delete view, unset updater
///
/// @todo Refactor so that no explicit clearInterval calls are needed.
function forgetView() {
    window.clearInterval(FormView.updater);
    FormView.remove();
}

/// Create form for model
function setupView(model) {
    FormView = new metaV({"el": $("#form"), 
                          "model": model});
    refreshTimeline();

    /// Postpone first (and only) form render until model.fetch()
    /// populates fields. After that, establish inverse mapping from
    /// form to model.
    var fetchCallback;
    fetchCallback = function () {
        FormView.render();
        FormView.updater = window.setInterval(
            function () {
                FormView.toModel();
            }, 2000);
        FormView.model.unbind("change", fetchCallback);
    }
    FormView.model.bind("change", fetchCallback);
}

/// Save current model and start fresh form
function proceed() {
    FormView.model.save();
    forgetView();
    setupView(new metaM);
}

/// Save current model and start fresh form
function restore(id) {
    forgetView();
    setupView(new metaM({"id": String(id)}));
}

function remove(id) {
    FormView.model.destroy();
    forgetView();
    setupView(new metaM);
} 
