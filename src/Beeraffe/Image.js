"use strict";

// module Beeraffe.Image

exports.load = function(src) {
  return function(onError, onSuccess) {
    var image = new Image();
    image.onload = function() {
        onSuccess(image);
    };
    image.onerror = function(err) {
        onError(err);
    };
    image.src = src;

    return function(cancelError, onCancelerError, onCancelerSuccess) {
      onCancelerSuccess();
    };
  };
};

exports.fromPixelData = function(width, height, data) {
  var image = new Image(width, height);
  var canvas = document.createElement('canvas');
  canvas.width = width;
  canvas.height = height;

  var context = canvas.getContext('2d');
  var imageData = new ImageData(data, width, height);
  context.putImageData(imageData, 0, 0);
  image.src = canvas.toDataURL('image/png');
  return image;
}

exports.width = function(image) {
  return image.width;
};

exports.height = function(image) {
  return image.height;
};

exports.pixelData = function (x, y, width, height, image) {
  var canvas = document.createElement('canvas');
  canvas.width = width;
  canvas.height = height;

  var context = canvas.getContext('2d');
  context.drawImage(image, -x, -y);
  return context.getImageData(0, 0, width, height).data;
};
