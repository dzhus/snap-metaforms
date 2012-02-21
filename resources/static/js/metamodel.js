/// Backbonize a metamodel, set default values for model
/// 
/// @return Constructor of Backbone model
function backbonizeModel(metamodel) {
    var defaults = {};
    
    _.each(metamodel.fields, 
          function(f) { 
              if (!(_.isUndefined(f.default)))
                  defaults[f.name] = f.default;
              else
                  defaults[f.name] = null;
          });

    var M = Backbone.Model.extend({
        "defaults": defaults,
        "initialize": function() {
            var furtherUpdates = function () {
                /// Do not resave model when id is set after
                /// first POST
                if (!this.hasChanged("id") && this.changedAttributes())
                    this.save();
            };

            /// Populate model from storage and start auto-saving to
            /// storage after first fetch() finished.
            /// Populate model from storage
            if (!this.isNew()) {
                var fetchCallback;
                fetchCallback = function() {
                    this.unbind("change", fetchCallback);
                    this.bind("change", furtherUpdates);
                }
                this.bind("change", fetchCallback);
                this.fetch();
            }
            else
                this.bind("change", furtherUpdates, this);
        },
        "urlRoot": "."
    });        

    return M;
}

DefaultFieldType = "text";

/// Convert metamodel to forest of HTML form elements with appropriate
/// data-bind parameters for Knockout.
///
/// TODO: We can do this on server as well.
///
/// @return String with form HTML
function renderFormView(metamodel) {
    var templates = [];

    _.each($(".field-template"), 
           function(tmp) {
               templates[/\w+/.exec(tmp.id)[0]] = tmp.text;
           });

    var contents = "";
    /// Pick an appropriate form widget for each metamodel
    /// field type and render actual model value in it
    _.each(metamodel.fields, 
           function (f) {
               f.type = f.type || DefaultFieldType;
               if (_.isUndefined(templates[f.type]))
                   f.type = "unknown";
               contents += Mustache.render(templates[f.type], f);
           });
    
    return contents;
}
