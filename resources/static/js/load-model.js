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
                  contents += Mustache.render(tpl, {"sel": id == FormView.model.id,
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
    kb.vmRelease(ViewModel);
    FormView.remove();
}

/// Create form for model
function setupView(model) {
    FormView = new metaV({"el": $("#form"), 
                          "model": model});
    ViewModel = new kb.viewModel(FormView.model);
    refreshTimeline();

    FormView.render();
    ko.applyBindings(ViewModel);
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

/// Remove currently loaded model from storage and start fresh form
function remove(id) {
    FormView.model.destroy();
    forgetView();
    setupView(new metaM);
} 
