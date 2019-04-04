var hexTable = (function() {
  var array = [];
  for (var i = 0; i < 256; ++i) {
    array.push("%" + ((i < 16 ? "0" : "") + i.toString(16)).toUpperCase());
  }

  return array;
})();

exports.defaultEncode = function encode(str) {
  return function(e) {
    return function(charset) {
      // This code was originally written by Brian White (mscdex) for the io.js core querystring library.
      // It has been adapted here for stricter adherence to RFC 3986
      if (str.length === 0) {
        return str;
      }

      var string = typeof str === "string" ? str : String(str);

      if (charset === "iso-8859-1") {
        return escape(string).replace(/%u[0-9a-f]{4}/gi, function($0) {
          return "%26%23" + parseInt($0.slice(2), 16) + "%3B";
        });
      }
      var out = "";
      for (var i = 0; i < string.length; ++i) {
        var c = string.charCodeAt(i);
        if (
          c === 0x2d || // -
          c === 0x2e || // .
          c === 0x5f || // _
          c === 0x7e || // ~
          (c >= 0x30 && c <= 0x39) || // 0-9
          (c >= 0x41 && c <= 0x5a) || // a-z
          (c >= 0x61 && c <= 0x7a) // A-Z
        ) {
          out += string.charAt(i);
          continue;
        }
        if (c < 0x80) {
          out = out + hexTable[c];
          continue;
        }
        if (c < 0x800) {
          out = out + (hexTable[0xc0 | (c >> 6)] + hexTable[0x80 | (c & 0x3f)]);
          continue;
        }
        if (c < 0xd800 || c >= 0xe000) {
          out =
            out +
            (hexTable[0xe0 | (c >> 12)] +
              hexTable[0x80 | ((c >> 6) & 0x3f)] +
              hexTable[0x80 | (c & 0x3f)]);
          continue;
        }
        i += 1;
        c = 0x10000 + (((c & 0x3ff) << 10) | (string.charCodeAt(i) & 0x3ff));
        out +=
          hexTable[0xf0 | (c >> 18)] +
          hexTable[0x80 | ((c >> 12) & 0x3f)] +
          hexTable[0x80 | ((c >> 6) & 0x3f)] +
          hexTable[0x80 | (c & 0x3f)];
      }
      return out;
    };
  };
};



var replace = String.prototype.replace;
var percentTwenties = /%20/g;
exports.rFC1738 = function(value){
  return replace.call(value, percentTwenties, '+');
}

exports.joinJsonArray = function(jsonArray) {
  return function(joinString) {
      return jsonArray.join(joinString);
  }
}

exports.jslog = function (a){
  console.log(a);
}

exports.jsKeys = function (obj) {
  return Object.keys(obj);
}

exports.getObjectByKey = function (obj) {
   return function(k){
      return obj[k]; 
   }
}