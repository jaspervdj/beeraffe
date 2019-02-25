"use strict";

// module Beeraffe.Debug

exports.appendImage = function(img) {
  return function() {
    document.body.appendChild(img);
  };
};

exports.appendParagraph = function(para) {
  return function() {
    var p = document.createElement('p');
    p.innerText = para;
    document.body.append(p);
  };
};

exports.appendBenchmark = function(name) {
  return function(f) {
    return function() {
      var t0 = performance.now();
      var x = f();
      var t1 = performance.now();
      exports.appendParagraph('Benchmark ' + name + ': ' + (t1 - t0) + 'ms')();
      return x;
    };
  };
};
