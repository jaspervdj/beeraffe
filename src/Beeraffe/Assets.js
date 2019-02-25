"use strict";

// module Beeraffe.Assets

exports.css = require('./beeraffe.css').toString();
exports.sprites = require('./sprites.png');
exports.sorceress = require('./sorceress.png');
exports.words = require('./words.txt');

exports.loadCSS = function(css) {
  return function() {
    var style = document.createElement("style");
    style.type = "text/css";
    style.innerHTML = css;
    document.body.appendChild(style);
  };
};
