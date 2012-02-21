/// Metamodel loading, setting up Backbone model, form template and
/// Knockback integration.

/// Load current JSON metamodel, build Backbone model and view,
/// setup view→model updater.
$(function () {
    $.getJSON("model/",
          function(metamodel) {
              mkBackboneModel = backbonizeModel(metamodel);
              $("#model-name").append(metamodel.title);
              $("#form").html(renderFormView(metamodel));
              setupView(new mkBackboneModel);

              refreshTimeline();
              TimelineUpdater = window.setInterval(refreshTimeline, 5000);

              setupEventWebsocket();
          });
});

/// Show recently created models, highlight the currently loaded one
function refreshTimeline() {
    $.get("timeline/",
          function(data) {
              var contents = "";
              var tpl = $("#timeline-item").html();

              _.each(data, function (id) {
                  contents += Mustache.render(tpl, {"sel": id == KnockVM.model.id,
                                                    "id": id});
              });
                  
              $("#timeline").html(contents);
          });                         
}

function setupEventWebsocket() {
    var ws = "ws://" + location.host + location.pathname + "events/";
    if ("undefined" == typeof(MozWebSocket))
        EventWebsocket = new WebSocket(ws);
    else
        EventWebsocket = new MozWebSocket(ws);

    EventWebsocket.onmessage = function(m) {
        var obj = JSON.parse(m.data);
        var tpl = $("#message-" + obj.event).html();
        $("#messages").append(Mustache.render(tpl, obj));
    }
};

/// Delete view, unset updater
///
/// @todo Refactor so that no explicit clearInterval calls are needed.
function forgetView() {
    kb.vmRelease(KnockVM);
}

/// Create form for model
function setupView(model) {
    KnockVM = new kb.viewModel(model);
    refreshTimeline();

    ko.applyBindings(KnockVM);
}

/// Save current model and start fresh form
function proceed() {
    KnockVM.model.save();
    forgetView();
    setupView(new mkBackboneModel);
}

/// Save current model and start fresh form
function restore(id) {
    forgetView();
    setupView(new mkBackboneModel({"id": String(id)}));
}

/// Remove currently loaded model from storage and start fresh form
function remove(id) {
    KnockVM.model.destroy();
    forgetView();
    setupView(new mkBackboneModel);
} 
