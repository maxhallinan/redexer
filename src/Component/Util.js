"use string";

exports._setFocus = function (elementId) {
  const el = document.getElementById(elementId);
  if (el) {
    // If setFocus is called in the action handling phase of the Halogen
    // lifecycle, this element might not be in the DOM until the next tic.
    setTimeout(function () {
      el.focus();
    }, 0);
  }
};
