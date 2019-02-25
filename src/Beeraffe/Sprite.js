"use strict";

// module Beeraffe.Sprite

exports.createPixelData = function(width, height) {
  return function() {
    return new Uint8ClampedArray(width * height * 4);
  };
};

exports.unsafeFreezePixelData = function(data) {
  return function() {
    return data;
  };
};

exports.toDataURL = function(sprite) {
  var canvas = document.createElement('canvas');
  canvas.width = sprite.width;
  canvas.height = sprite.height;

  var context = canvas.getContext('2d');
  var imageData = new ImageData(sprite.pixels, sprite.width, sprite.height);
  context.putImageData(imageData, 0, 0);
  return canvas.toDataURL('image/png');
};
