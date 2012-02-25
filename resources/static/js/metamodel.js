/// Backbonize a metamodel, set default values for model
///
/// @return Constructor of Backbone model
function backbonizeModel(metamodel) {
    var defaults = {};
    
    var fields = metamodel.fields;
    
    for (k in fields) {
        if (!(_.isUndefined(fields[k].default)))
            defaults[k] = fields[k].default;
        else
            defaults[k] = null;
    };

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
        /// For checkbox fields, translate "0"/"1" to false/true
        /// boolean.
        parse: function(json) {
            var m = this.metamodel;
            for (k in json) {
                if (m.fields[k].type == "checkbox") {
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
    var fields = metamodel.fields;
    
    for (k in fields) {
               var f = fields[k];
               f.type = f.type || DefaultFieldType;
               if (_.isUndefined(templates[f.type]))
                   f.type = "unknown";
               var ctx = _.extend(f, {"name": k});
               contents += Mustache.render(templates[f.type], ctx);
           };

    return contents;
}
