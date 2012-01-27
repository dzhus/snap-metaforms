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
        "defaults": defaults});        

    return M;
}

/// Build choice field list for use with Mustache templates
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
        
        "render": function() {
            var j = this.model.toJSON();
            $(this.el).empty();

            /// @todo To dict
            var tt = $("#text-field-template").html();
            var ct = $("#choice-field-template").html();

            var contents = "";

            /// Pick an appropriate form widget for each metamodel
            /// field type and render actual model value in it
            _.each(metamodel.fields,
                   function(f) {
                       switch (f.type) {
                       case fieldType.number:
                       case fieldType.text:
                           contents += Mustache.render(tt, {field: f, value: j[f.name]});
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
