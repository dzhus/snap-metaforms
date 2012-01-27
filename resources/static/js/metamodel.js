var fieldType = {
    "text": 0,
    "number": 1,
    "bool": 2,
    "choice": 3};

/// Return Backbone model from metamodel
function backbonizeModel(metamodel)
{
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

/// Render text field
///
/// @param context Dictionary with keys `name` and `value`

/// Return Backbone view from metamodel
function backbonizeView(metamodel)
{
    var V = Backbone.View.extend({
        "initialize": function() {
            this.model.bind("change", this.render, this);
        },
        
        "render": function() {
            var j = this.model.toJSON();
            $(this.el).empty();

            var tt = $("#text-field-template").html();

            var contents = "";
            _.each(metamodel.fields,
                   function(f) {
                       contents += Mustache.render(tt, {field: f, value: j[f.name]});
                   });
            $(this.el).append(contents);
        }});

    return V;
}
