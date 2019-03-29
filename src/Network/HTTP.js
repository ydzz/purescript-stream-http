var  lib = require('iso-stream-http');

exports.requestImpl = function (opts) {
    return function (k) {
      return function () {
        return lib.getRequest(opts, function (res) {
          k(res)();
        });
      };
    };
};

exports.setTimeout = function (r) {
  return function (ms) {
    return function (k) {
      return function () {
        r.setTimeout(ms, k);
      };
    };
  };
};