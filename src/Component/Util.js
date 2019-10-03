"use string";
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

exports._setSelectionRange = function (elementId, parts) {
  console.log(parts);
  var element = document.getElementById(elementId);
  element.innerHTML = parts.before + "<span class=\"error-location\">" + parts.highlight + "</span>" + parts.after
  var selection = window.getSelection();
  selection.setPosition(element.childNodes[0], parts.before.length);
};

exports._setLineBreak = function (node) {
  node.innerHTML = "<br />";
}

exports._deleteEditorContent = function (node) {
  node.innerHTML = "<br />";
};

exports._highlightErrPos = function (parts, node) {
  var innerHTML = parts.before + "<span class=\"error-position\">" + parts.highlight + "</span>" + parts.after;
  node.innerHTML = innerHTML;
  var selection = window.getSelection();
  selection.setPosition(node.childNodes[0], parts.before.length);
};

exports._setEditorTextContent = function (textContent, node) {
  var selection = window.getSelection();
  var anchorOffset = selection.anchorOffset;
  var anchorNode = selection.anchorNode;
  node.textContent = textContent;
  try {
    selection.setPosition(node.childNodes[0], anchorOffset);
  } catch (error) {
    selection.setPosition(node.childNodes[0], 0);
  }
};
