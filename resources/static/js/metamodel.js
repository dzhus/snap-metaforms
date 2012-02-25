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
        defaults: defaults,
        dirtyAttributes: {},
        initialize: function() {
            if (!this.isNew())
                this.fetch();
        },
        metamodel: metamodel,
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
                this.dirtyAttributes[k] = attrs[k];
        },
        parse: function(json) {
            for (k in json) {
                if (json[k] == "0")
                    json[k] = false;
                else if (json[k] == "1")
                    json[k] = true;
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
