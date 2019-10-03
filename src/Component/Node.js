"use string";

exports._deleteEditorContent = function (node) {
  var selection = window.getSelection();
  node.innerHTML = "<br />";
  selection.setPosition(node, 0); 
};

exports._markEditorErrPos = function (parts, node) {
  var selection = window.getSelection();
  var innerHTML = parts.before + "<span class=\"error-position\">" + parts.markText + "</span>" + parts.after;
  node.innerHTML = innerHTML;
  selection.setPosition(node.childNodes[0], parts.before.length);
};

exports._setEditorTextContent = function (textContent, node) {
  var selection = window.getSelection();
  var anchorOffset = selection.anchorOffset;
  node.textContent = textContent;
  try {
    selection.setPosition(node.childNodes[0], anchorOffset);
  } catch (error) {
    selection.setPosition(node.childNodes[0], 0);
  }
};
