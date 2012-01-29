/// Enumeration
var fieldType = {
    "text": 0,
    "number": 1,
    "bool": 2,
    "choice": 3};

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
            this.bind("change", 
                      function () {
                          if (!this.hasChanged("id"))
                              this.save();
                      },
                      this);
        },
        "urlRoot": "."
        
    });        

    return M;
}

/// Build choice field list for use with Mustache templates
///
/// @todo Difference between values and option labels
function processChoice(choice, value) {
    return _.map(choice,
                 function(c) {
                     return {"value": c,
                             "selected": c == value}});
}

/// Backbonize a metamodel, producing a view which renders associated
/// Backbone model to form
///
/// @param context Dictionary with keys `name` and `value`
/// 
/// @return Constructor of Backbone view
function backbonizeView(metamodel) {
    var V = Backbone.View.extend({
        "initialize": function() {
            this.render();
        },

        /// Inverse mapping from view to model
        "toModel": function() {
            var newAttrs = {};
            _.each(metamodel.fields,
                   function (f) {
                       var v = this.$(".field." + f.name).val();
                       if (!!v )
                           newAttrs[f.name] = v;
                       else
                           newAttrs[f.name] = null;
                   });
            this.model.set(newAttrs);
        },

        
        "render": function() {
            var j = this.model.toJSON();
            /// @todo Update only parts which have changed
            $(this.el).empty();

            /// @todo To dict
            var lt = $("#longtext-field-template").html();
            var tt = $("#text-field-template").html();
            var ct = $("#choice-field-template").html();
            
            var contents = "";

            /// Pick an appropriate form widget for each metamodel
            /// field type and render actual model value in it
            _.each(metamodel.fields,
                   function(f) {
                       var ctx = {field: f, value: j[f.name]};
                       switch (f.type) {
                       case fieldType.number:
                       case fieldType.text:
                           contents += Mustache.render(tt, ctx);
                           break
                       case fieldType.longText:
                           contents += Mustache.render(lt, ctx);
                           break
                       case fieldType.choice:
                           contents += Mustache.render(ct, {field: f, value: j[f.name], 
                                                            choice: processChoice(f.choice, j[f.name])});
                           break
                       }
                   });

            /// @todo Unbreak my XML
            $(this.el).html(contents);
        }});

    return V;
}
