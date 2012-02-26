/// Model loading, setting up Backbone model, form template and
/// Knockback integration.

/// Load current JSON model, build Backbone model and view,
/// setup view→model updater.
$(function () {
    $.getJSON("model/",
          function(model) {
              LoadedModel = model;
              FormElement = $("#form");

              mkBackboneModel = backbonizeModel(model);
              $("#model-name").append(model.title);
              setupView(new mkBackboneModel);

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

/// Subscribe to websocket notifications and print messages in
/// #messages
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

/// Delete form, release observables
function forgetView() {
    kb.vmRelease(KnockVM);
    FormElement.empty();
}

/// Create form for model and fill it
function setupView(model) {
    KnockVM = new kb.viewModel(model);

    refreshTimeline();

    FormElement.html(renderFormView(LoadedModel));
    ko.applyBindings(KnockVM);

    /// Wait a bit to populate model fields and bind form elements
    /// without PUT-backs to server
    window.setTimeout(function () {
        KnockVM.model.setupServerSync();
    }, 1000);
}

/// Save current model and start fresh form
function proceed() {
    KnockVM.model.save();
    forgetView();
    setupView(new mkBackboneModel);
}

/// Load existing model
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
