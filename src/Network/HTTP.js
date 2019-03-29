var  lib = require('iso-stream-http');

exports.requestImpl = function (opts) {
    return function (k) {
      return function () {
        var req = lib.getRequest(opts, function (res) {
          res.on('data', function (buf) {
            　console.log(buf);
          });
          k(res)();
        });
        console.log(req);
        return req;
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