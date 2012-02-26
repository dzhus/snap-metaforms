/// Backbonize a model, set default values for model
///
/// @return Constructor of Backbone model
function backbonizeModel(model) {
    var defaults = {};
    var fieldHash = {};
    _.each(model.fields,
          function(f) {
              if (!(_.isUndefined(f.default)))
                  defaults[f.name] = f.default;
              else
                  defaults[f.name] = null;
              fieldHash[f.name] = f;
          });

    var M = Backbone.Model.extend({
        defaults: defaults,
        dirtyAttributes: {},
        initialize: function() {
            if (!this.isNew())
                this.fetch();
        },
        model: model,
        fieldHash: fieldHash,
        /// Bind model changes to server sync
        setupServerSync: function () {
            var realUpdates = function () {
                /// Do not resave model when id is set after
                /// first POST
                if (!this.hasChanged("id"))
                    this.save();
            };

            this.bind("change", _.throttle(realUpdates, 500), this);
        },
        set: function(attrs, options){
            Backbone.Model.prototype.set.call(this, attrs, options);
            /// Queue new values to be saved on server using
            /// dirtyAttributes
            ///
            /// TODO _.extend doesn't work here
            for (k in attrs)
                if (k != "id" && this.fieldHash[k].canWrite)
                    this.dirtyAttributes[k] = attrs[k];
        },
        /// For checkbox fields, translate "0"/"1" to false/true
        /// boolean.
        parse: function(json) {
            var m = this.model;
            for (k in json) {
                if ((k != "id") && (this.fieldHash[k].type == "checkbox")) {
                    if (json[k] == "1")
                        json[k] = true;
                    else
                        json[k] = false;
                }
            }
            return json;
        },        
        toJSON: function () {
            /// Send only dirtyAttributes instead of the whole object
            json = this.dirtyAttributes;
            /// Map boolean values to string "0"/"1"'s for server
            /// compatibility
            for (k in json)
                if (_.isBoolean(json[k]))
                    json[k] = String(json[k] ? "1" : "0");
            this.dirtyAttributes = {};
            return json;
        },
        urlRoot: "."
    });

    return M;
}

/// Convert model to forest of HTML form elements with appropriate
/// data-bind parameters for Knockout.
///
/// TODO: We can do this on server as well.
///
/// @return String with form HTML
function renderFormView(model) {
    var templates = [];

    _.each($(".field-template"),
           function(tmp) {
               templates[/\w+/.exec(tmp.id)[0]] = tmp.text;
           });

    var contents = "";
    var fType = "";
    /// Pick an appropriate form widget for each model
    /// field type and render actual model value in it
    ///
    /// Set readonly context attribute for fields which have
    /// canEdit=false in form description.
    _.each(model.fields,
           function (f) {
               if (_.isUndefined(templates[f.type]))
                   fType = "unknown";
               else
                   fType = f.type;
               contents += Mustache.render(templates[fType],
                                           _.extend(f, {readonly: !f.canWrite}));
           });

    return contents;
}
