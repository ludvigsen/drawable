console.log('OMGLOL!!');
var _ludvigsen$elm_paint$Native_LoadSVG = function () {
  console.log('OMGLOL212321321');
  function render(model) {
    console.log('RENDER');
    var div = document.createElement('div');
    div.innerHTML = model.svg;
    console.log('RENDERING!!!', div.childNodes[0]);
    return div;
  }

  function diff(a, b) {
    console.log('DIFF!!', a, b);
    if (a === b) {
      return null;
    }
    return {
      applyPatch: function (domNode, data) {
        console.log('apply patch!');
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
    console.log('LOAD SVG: ', svgString);
    return _elm_lang$virtual_dom$Native_VirtualDom.custom([], { svg: svgString }, implementation);
  }

  return {
    loadSVG: F2(loadSVG)
  };
}();
