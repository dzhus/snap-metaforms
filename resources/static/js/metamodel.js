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
            /// Populate model from storage
            if (!this.isNew()) {
                this.fetch();
            }            
        },
        "urlRoot": "."
    });        

    return M;
}

/// Backbonize a metamodel, producing a Backbone.View which renders to
/// form with data-bind properties for Knockout.
/// 
/// @return Constructor of Backbone view
function backbonizeView(metamodel) {
    var V = Backbone.View.extend({
        "initialize": function() {
            this.render();
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
                       }
                   });

            /// @todo Unbreak my XML
            $(this.el).html(contents);
        },
        
        "remove": function() {
            $(this.el).empty();
        }
    });


    return V;
}
