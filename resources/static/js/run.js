$(function () {
    var M = backbonizeModel(metamodel);
    var V = backbonizeView(metamodel);
    
    obj = new M;
    view = new V({"el": $("#form"), "model": obj});
    view.render();
});
