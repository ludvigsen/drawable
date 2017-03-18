var _ludvigsen$elm_paint$Native_LoadSVG = function () {
  function render(model) {
    var div = document.createElement('div');
    div.innerHTML = model.svg;
    return div;
  }

  function diff(a, b) {
    if (a === b) {
      return null;
    }
    return {
      applyPatch: function (domNode, data) {
        domNode.innerHtml = data;
        return domNode;
      },
      data: b
    };
  }

  var implementation = {
    render: render,
    diff: diff
  };

  function loadSVG(svgString) {
    return _elm_lang$virtual_dom$Native_VirtualDom.custom([], { svg: svgString }, implementation);
  }

  return {
    loadSVG: F2(loadSVG)
  };
}();
