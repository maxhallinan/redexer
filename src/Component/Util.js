exports._setFocus = function (elementId) {
  const el = document.getElementById(elementId);
  if (el) {
    // If setFocus is called in the handle action phase of the Halogen
    // lifecycle, this element might not be in the DOM until the next tic.
    setTimeout(function () {
      el.focus();
    }, 0);
  }
};

exports._getSelectionRange = function (index) {
  var selection = window.getSelection();
  var range = selection.getRangeAt(index);
  return range;
};

exports._getSelectionRangeOffset = function (range) {
  return {
    end: range.endOffset,
    start: range.startOffset,
  };
};

exports._setSelectionRange = function (elementId, offset, range) {
  window.setTimeout(function () {
    var selection = window.getSelection();
    const el = document.getElementById(elementId);
    if (el) {
      range.setStart(el, offset.start);
      range.setEnd(el, offset.end);
    }
  }, 10);
};
