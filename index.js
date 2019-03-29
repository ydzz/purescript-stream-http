(function(){function r(e,n,t){function o(i,f){if(!n[i]){if(!e[i]){var c="function"==typeof require&&require;if(!f&&c)return c(i,!0);if(u)return u(i,!0);var a=new Error("Cannot find module '"+i+"'");throw a.code="MODULE_NOT_FOUND",a}var p=n[i]={exports:{}};e[i][0].call(p.exports,function(r){var n=e[i][1][r];return o(n||r)},p,p.exports,r,e,n,t)}return n[i].exports}for(var u="function"==typeof require&&require,i=0;i<t.length;i++)o(t[i]);return o}return r})()({1:[function(require,module,exports){
module.exports = {
  "100": "Continue",
  "101": "Switching Protocols",
  "102": "Processing",
  "200": "OK",
  "201": "Created",
  "202": "Accepted",
  "203": "Non-Authoritative Information",
  "204": "No Content",
  "205": "Reset Content",
  "206": "Partial Content",
  "207": "Multi-Status",
  "208": "Already Reported",
  "226": "IM Used",
  "300": "Multiple Choices",
  "301": "Moved Permanently",
  "302": "Found",
  "303": "See Other",
  "304": "Not Modified",
  "305": "Use Proxy",
  "307": "Temporary Redirect",
  "308": "Permanent Redirect",
  "400": "Bad Request",
  "401": "Unauthorized",
  "402": "Payment Required",
  "403": "Forbidden",
  "404": "Not Found",
  "405": "Method Not Allowed",
  "406": "Not Acceptable",
  "407": "Proxy Authentication Required",
  "408": "Request Timeout",
  "409": "Conflict",
  "410": "Gone",
  "411": "Length Required",
  "412": "Precondition Failed",
  "413": "Payload Too Large",
  "414": "URI Too Long",
  "415": "Unsupported Media Type",
  "416": "Range Not Satisfiable",
  "417": "Expectation Failed",
  "418": "I'm a teapot",
  "421": "Misdirected Request",
  "422": "Unprocessable Entity",
  "423": "Locked",
  "424": "Failed Dependency",
  "425": "Unordered Collection",
  "426": "Upgrade Required",
  "428": "Precondition Required",
  "429": "Too Many Requests",
  "431": "Request Header Fields Too Large",
  "451": "Unavailable For Legal Reasons",
  "500": "Internal Server Error",
  "501": "Not Implemented",
  "502": "Bad Gateway",
  "503": "Service Unavailable",
  "504": "Gateway Timeout",
  "505": "HTTP Version Not Supported",
  "506": "Variant Also Negotiates",
  "507": "Insufficient Storage",
  "508": "Loop Detected",
  "509": "Bandwidth Limit Exceeded",
  "510": "Not Extended",
  "511": "Network Authentication Required"
}

},{}],2:[function(require,module,exports){
if (typeof Object.create === 'function') {
  // implementation from standard node.js 'util' module
  module.exports = function inherits(ctor, superCtor) {
    ctor.super_ = superCtor
    ctor.prototype = Object.create(superCtor.prototype, {
      constructor: {
        value: ctor,
        enumerable: false,
        writable: true,
        configurable: true
      }
    });
  };
} else {
  // old school shim for old browsers
  module.exports = function inherits(ctor, superCtor) {
    ctor.super_ = superCtor
    var TempCtor = function () {}
    TempCtor.prototype = superCtor.prototype
    ctor.prototype = new TempCtor()
    ctor.prototype.constructor = ctor
  }
}

},{}],3:[function(require,module,exports){
const http = require('./lib/http')
const https = http
const URL = self.URL

module.exports = {
    http,
    https,
    getRequest: (options, cb) => {
        let protocol = 'http:'

        if (typeof options === 'string') {
            const url = new URL(options)
            protocol = url.protocol
        } else if (options.protocol) {
            protocol = options.protocol
        }

        return protocol === 'http:' ? http.request(options, cb) : https.request(options, cb)
    }
}

},{"./lib/http":5}],4:[function(require,module,exports){
function isFunction (value) {
	return typeof value === 'function'
}

module.exports = {
	fetch: 'fetch' in self && isFunction(self.fecth),
	writableStream: 'WritableStream' in self && isFunction(self.WritableStream),
	abortController: 'AbortController' in self && isFunction(self.AbortController),
	arrayBuffer: 'ArrayBuffer' in self
}
},{}],5:[function(require,module,exports){
(function (global){
var ClientRequest = require('./request')
var IncomingMessage = require('./incoming-message')
var statusCodes = require('builtin-status-codes')
var URL = self.URL


const request = (opts, cb) => {
	if (typeof opts === 'string') {

		opts = new URL(opts, self.location.protocol + '//' +self.location.host)
		opts.path = opts.pathname + opts.search
		opts.auth = (opts.username && opts.password) ? opts.username + ':' + opts.password : null
	} else {
		opts = Object.assign({}, opts)
	}

	// Normally, the page is loaded from http or https, so not specifying a protocol
	// will result in a (valid) protocol-relative url. However, this won't work if
	// the protocol is something else, like 'file:'
	var defaultProtocol = global.location.protocol.search(/^https?:$/) === -1 ? 'http:' : ''

	var protocol = opts.protocol || defaultProtocol
	var host = opts.hostname || opts.host
	var port = opts.port
	var path = opts.path || '/'

	// Necessary for IPv6 addresses
	if (host && host.indexOf(':') !== -1)
		host = '[' + host + ']'

	// This may be a relative url. The browser should always be able to interpret it correctly.
	opts.url = (host ? (protocol + '//' + host) : '') + (port ? ':' + port : '') + path
	opts.method = (opts.method || 'GET').toUpperCase()
	opts.headers = opts.headers || {}

	// Also valid opts.auth, opts.mode

	var req = new ClientRequest(opts)
	if (cb)
		req.on('response', cb)
	return req
}

const get = (opts, cb) => {
	var req = request(opts, cb)
	req.end()
	return req
}

const Agent = function () {}
Agent.defaultMaxSockets = 4

const METHODS = [
	'CHECKOUT',
	'CONNECT',
	'COPY',
	'DELETE',
	'GET',
	'HEAD',
	'LOCK',
	'M-SEARCH',
	'MERGE',
	'MKACTIVITY',
	'MKCOL',
	'MOVE',
	'NOTIFY',
	'OPTIONS',
	'PATCH',
	'POST',
	'PROPFIND',
	'PROPPATCH',
	'PURGE',
	'PUT',
	'REPORT',
	'SEARCH',
	'SUBSCRIBE',
	'TRACE',
	'UNLOCK',
	'UNSUBSCRIBE'
]

module.exports = {
	Agent,
	ClientRequest,
	IncomingMessage,
	METHODS,
	STATUS_CODES: statusCodes,
	get,
	globalAgent : new Agent(),
	request,
	maxHeaderSize: '8KB'

}
}).call(this,typeof global !== "undefined" ? global : typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{"./incoming-message":6,"./request":7,"builtin-status-codes":1}],6:[function(require,module,exports){
(function (process,Buffer){
var capability = require('./capability')
var inherits = require('inherits')
var stream = require('readable-stream')

var IncomingMessage = function (response, mode, fetchTimer) {
	stream.Readable.call(this)

	this._mode = mode
	this.headers = {}
	this.rawHeaders = []
	this.trailers = {}
	this.rawTrailers = []

	// Fake the 'close' event, but only once 'end' fires
	this.on('end',  () => {
		// The nextTick is necessary to prevent the 'request' module from causing an infinite loop
		process.nextTick(() =>this.emit('close'))
	})

	this._fetchResponse = response

	this.url = response.url
	this.statusCode = response.status
	this.statusMessage = response.statusText
	
	response.headers.forEach((header, key) => {
		this.headers[key.toLowerCase()] = header
		this.rawHeaders.push(key, header)
	})

	if (capability.writableStream) {
		var writable = new WritableStream({
			write: chunk => {
				return new Promise((resolve, reject) => {
					if (this._destroyed) {
						reject()
					} else if(this.push(Buffer.from(chunk))) {
						resolve()
					} else {
						this._resumeFetch = resolve
					}
				})
			},
			close: () => {
				clearTimeout(fetchTimer)
				if (!this._destroyed){
					this.push(null)
				}
			},
			abort: err => {
				if (!this._destroyed){
					this.emit('error', err)
				}
			}
		})

		try {
			response.body
				.pipeTo(writable)
				.catch(err => {
					console.log(err);
					self.clearTimeout(fetchTimer)
					if (!this._destroyed){
						this.emit('error', err)
					}
				})
			return
		} catch (e) {} // pipeTo method isn't defined. Can't find a better way to feature test this
	}
	// fallback for when writableStream or pipeTo aren't available
	var reader = response.body.getReader()
	
	function read (context) {
		reader.read()
		.then(result => {
			if (context._destroyed){
				return
			}
			if (result.done) {
				clearTimeout(fetchTimer)
				context.push(null)
				return
			}
			context.push(Buffer.from(result.value))
			read(context)
		})
		.catch(err => {
			clearTimeout(fetchTimer)
			if (!context._destroyed){
				context.emit('error', err)
			}
		})
	}

	read(this)
	
}

inherits(IncomingMessage, stream.Readable)

IncomingMessage.prototype._read = function () {
	var resolve = this._resumeFetch
	if (resolve) {
		this._resumeFetch = null
		resolve()
	}
}

module.exports = IncomingMessage
}).call(this,require('_process'),require("buffer").Buffer)
},{"./capability":4,"_process":220,"buffer":215,"inherits":2,"readable-stream":21}],7:[function(require,module,exports){
(function (Buffer){
var capability = require('./capability')
var inherits = require('inherits')
var IncomingMessage = require('./incoming-message')
var stream = require('readable-stream')

var fetch = self.fetch

var ClientRequest = module.exports = function (opts) {
	stream.Writable.call(this)

	this._opts = opts
	this._body = []
	this._headers = {}
	if (opts.auth)
		this.setHeader('Authorization', 'Basic ' + Buffer.from(opts.auth).toString('base64'))


	Object.keys(opts.headers).forEach(name => {
		this.setHeader(name, opts.headers[name])
	})

	this._mode = 'fetch'
	this._fetchTimer = null

	this.on('finish',  () => this._onFinish())
}

inherits(ClientRequest, stream.Writable)

ClientRequest.prototype.setHeader = function (name, value) {
	var lowerName = name.toLowerCase()
	// This check is not necessary, but it prevents warnings from browsers about setting unsafe
	// headers. To be honest I'm not entirely sure hiding these warnings is a good thing, but
	// http-browserify did it, so I will too.
	if (unsafeHeaders.indexOf(lowerName) !== -1){
		return
	}
	this._headers[lowerName] = value
}

ClientRequest.prototype.getHeader = function (name) {
	var header = this._headers[name.toLowerCase()]
	if (header)
		return header.value
	return null
}

ClientRequest.prototype.removeHeader = function (name) {
	delete this._headers[name.toLowerCase()]
}

ClientRequest.prototype._onFinish = function () {
	if (this._destroyed)
		return
	var opts = this._opts

	var headersObj = this._headers
	var body = null
	if (opts.method !== 'GET' && opts.method !== 'HEAD') {
        body = new Blob(this._body, {
            type: (headersObj['content-type'] || {}).value || ''
        });
    }

	var signal = null
	if (capability.abortController) {
		var controller = new AbortController()
		signal = controller.signal
		this._fetchAbortController = controller

		if ('requestTimeout' in opts && opts.requestTimeout !== 0) {
			this._fetchTimer = setTimeout(() => {
				this.emit('requestTimeout')
				if (this._fetchAbortController)
					this._fetchAbortController.abort()
			}, opts.requestTimeout)
		}
	}

	fetch(this._opts.url, {
		method: this._opts.method,
		headers: this._headers,
		body: body || undefined,
		mode: 'cors',
		credentials: opts.withCredentials ? 'include' : 'same-origin',
		signal: signal
	})
	.then(response => {
		this._fetchResponse = response
		this._connect()
	}, reason => {
		clearTimeout(this._fetchTimer)
		if (!this._destroyed)
			this.emit('error', reason)
	})
}

ClientRequest.prototype._connect = function () {
	if (this._destroyed)
		return

	this._response = new IncomingMessage(this._fetchResponse, this._mode, this._fetchTimer)
	this._response.on('error', (err) => this.emit('error', err))

	this.emit('response', this._response)
}

ClientRequest.prototype._write = function (chunk, encoding, cb) {
	this._body.push(chunk)
	cb()
}

ClientRequest.prototype.abort = ClientRequest.prototype.destroy = function () {
	this._destroyed = true

	
	clearTimeout(this._fetchTimer)
	
	if (this._response)
		this._response._destroyed = true
	if (this._xhr)
		this._xhr.abort()
	else if (this._fetchAbortController)
		this._fetchAbortController.abort()
}

ClientRequest.prototype.end = function (data, encoding, cb) {
	if (typeof data === 'function') {
		cb = data
		data = undefined
	}

	stream.Writable.prototype.end.call(this, data, encoding, cb)
}

ClientRequest.prototype.flushHeaders = function () {}
ClientRequest.prototype.setTimeout = function () {}
ClientRequest.prototype.setNoDelay = function () {}
ClientRequest.prototype.setSocketKeepAlive = function () {}

// Taken from http://www.w3.org/TR/XMLHttpRequest/#the-setrequestheader%28%29-method
var unsafeHeaders = [
	'accept-charset',
	'accept-encoding',
	'access-control-request-headers',
	'access-control-request-method',
	'connection',
	'content-length',
	'cookie',
	'cookie2',
	'date',
	'dnt',
	'expect',
	'host',
	'keep-alive',
	'origin',
	'referer',
	'te',
	'trailer',
	'transfer-encoding',
	'upgrade',
	'via'
]

}).call(this,require("buffer").Buffer)
},{"./capability":4,"./incoming-message":6,"buffer":215,"inherits":2,"readable-stream":21}],8:[function(require,module,exports){
'use strict';

function _inheritsLoose(subClass, superClass) { subClass.prototype = Object.create(superClass.prototype); subClass.prototype.constructor = subClass; subClass.__proto__ = superClass; }

var codes = {};

function createErrorType(code, message, Base) {
  if (!Base) {
    Base = Error;
  }

  function getMessage(arg1, arg2, arg3) {
    if (typeof message === 'string') {
      return message;
    } else {
      return message(arg1, arg2, arg3);
    }
  }

  var NodeError =
  /*#__PURE__*/
  function (_Base) {
    _inheritsLoose(NodeError, _Base);

    function NodeError(arg1, arg2, arg3) {
      return _Base.call(this, getMessage(arg1, arg2, arg3)) || this;
    }

    return NodeError;
  }(Base);

  NodeError.prototype.name = Base.name;
  NodeError.prototype.code = code;
  codes[code] = NodeError;
} // https://github.com/nodejs/node/blob/v10.8.0/lib/internal/errors.js


function oneOf(expected, thing) {
  if (Array.isArray(expected)) {
    var len = expected.length;
    expected = expected.map(function (i) {
      return String(i);
    });

    if (len > 2) {
      return "one of ".concat(thing, " ").concat(expected.slice(0, len - 1).join(', '), ", or ") + expected[len - 1];
    } else if (len === 2) {
      return "one of ".concat(thing, " ").concat(expected[0], " or ").concat(expected[1]);
    } else {
      return "of ".concat(thing, " ").concat(expected[0]);
    }
  } else {
    return "of ".concat(thing, " ").concat(String(expected));
  }
} // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/startsWith


function startsWith(str, search, pos) {
  return str.substr(!pos || pos < 0 ? 0 : +pos, search.length) === search;
} // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/endsWith


function endsWith(str, search, this_len) {
  if (this_len === undefined || this_len > str.length) {
    this_len = str.length;
  }

  return str.substring(this_len - search.length, this_len) === search;
} // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/includes


function includes(str, search, start) {
  if (typeof start !== 'number') {
    start = 0;
  }

  if (start + search.length > str.length) {
    return false;
  } else {
    return str.indexOf(search, start) !== -1;
  }
}

createErrorType('ERR_INVALID_OPT_VALUE', function (name, value) {
  return 'The value "' + value + '" is invalid for option "' + name + '"';
}, TypeError);
createErrorType('ERR_INVALID_ARG_TYPE', function (name, expected, actual) {
  // determiner: 'must be' or 'must not be'
  var determiner;

  if (typeof expected === 'string' && startsWith(expected, 'not ')) {
    determiner = 'must not be';
    expected = expected.replace(/^not /, '');
  } else {
    determiner = 'must be';
  }

  var msg;

  if (endsWith(name, ' argument')) {
    // For cases like 'first argument'
    msg = "The ".concat(name, " ").concat(determiner, " ").concat(oneOf(expected, 'type'));
  } else {
    var type = includes(name, '.') ? 'property' : 'argument';
    msg = "The \"".concat(name, "\" ").concat(type, " ").concat(determiner, " ").concat(oneOf(expected, 'type'));
  }

  msg += ". Received type ".concat(typeof actual);
  return msg;
}, TypeError);
createErrorType('ERR_STREAM_PUSH_AFTER_EOF', 'stream.push() after EOF');
createErrorType('ERR_METHOD_NOT_IMPLEMENTED', function (name) {
  return 'The ' + name + ' method is not implemented';
});
createErrorType('ERR_STREAM_PREMATURE_CLOSE', 'Premature close');
createErrorType('ERR_STREAM_DESTROYED', function (name) {
  return 'Cannot call ' + name + ' after a stream was destroyed';
});
createErrorType('ERR_MULTIPLE_CALLBACK', 'Callback called multiple times');
createErrorType('ERR_STREAM_CANNOT_PIPE', 'Cannot pipe, not readable');
createErrorType('ERR_STREAM_WRITE_AFTER_END', 'write after end');
createErrorType('ERR_STREAM_NULL_VALUES', 'May not write null values to stream', TypeError);
createErrorType('ERR_UNKNOWN_ENCODING', function (arg) {
  return 'Unknown encoding: ' + arg;
}, TypeError);
createErrorType('ERR_STREAM_UNSHIFT_AFTER_END_EVENT', 'stream.unshift() after end event');
module.exports.codes = codes;

},{}],9:[function(require,module,exports){
(function (process){
'use strict'

var experimentalWarnings = new Set();

function emitExperimentalWarning(feature) {
  if (experimentalWarnings.has(feature)) return;
  var msg = feature + ' is an experimental feature. This feature could ' +
       'change at any time';
  experimentalWarnings.add(feature);
  process.emitWarning(msg, 'ExperimentalWarning');
}

function noop() {}

module.exports.emitExperimentalWarning = process.emitWarning
  ? emitExperimentalWarning
  : noop;

}).call(this,require('_process'))
},{"_process":220}],10:[function(require,module,exports){
(function (process){
// Copyright Joyent, Inc. and other Node contributors.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit
// persons to whom the Software is furnished to do so, subject to the
// following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
// USE OR OTHER DEALINGS IN THE SOFTWARE.
// a duplex stream is just a stream that is both readable and writable.
// Since JS doesn't have multiple prototypal inheritance, this class
// prototypally inherits from Readable, and then parasitically from
// Writable.
'use strict';
/*<replacement>*/

var objectKeys = Object.keys || function (obj) {
  var keys = [];

  for (var key in obj) {
    keys.push(key);
  }

  return keys;
};
/*</replacement>*/


module.exports = Duplex;

var Readable = require('./_stream_readable');

var Writable = require('./_stream_writable');

require('inherits')(Duplex, Readable);

{
  // Allow the keys array to be GC'ed.
  var keys = objectKeys(Writable.prototype);

  for (var v = 0; v < keys.length; v++) {
    var method = keys[v];
    if (!Duplex.prototype[method]) Duplex.prototype[method] = Writable.prototype[method];
  }
}

function Duplex(options) {
  if (!(this instanceof Duplex)) return new Duplex(options);
  Readable.call(this, options);
  Writable.call(this, options);
  this.allowHalfOpen = true;

  if (options) {
    if (options.readable === false) this.readable = false;
    if (options.writable === false) this.writable = false;

    if (options.allowHalfOpen === false) {
      this.allowHalfOpen = false;
      this.once('end', onend);
    }
  }
}

Object.defineProperty(Duplex.prototype, 'writableHighWaterMark', {
  // making it explicit this property is not enumerable
  // because otherwise some prototype manipulation in
  // userland will fail
  enumerable: false,
  get: function get() {
    return this._writableState.highWaterMark;
  }
});
Object.defineProperty(Duplex.prototype, 'writableBuffer', {
  // making it explicit this property is not enumerable
  // because otherwise some prototype manipulation in
  // userland will fail
  enumerable: false,
  get: function get() {
    return this._writableState && this._writableState.getBuffer();
  }
});
Object.defineProperty(Duplex.prototype, 'writableLength', {
  // making it explicit this property is not enumerable
  // because otherwise some prototype manipulation in
  // userland will fail
  enumerable: false,
  get: function get() {
    return this._writableState.length;
  }
}); // the no-half-open enforcer

function onend() {
  // If the writable side ended, then we're ok.
  if (this._writableState.ended) return; // no more data can be written.
  // But allow more writes to happen in this tick.

  process.nextTick(onEndNT, this);
}

function onEndNT(self) {
  self.end();
}

Object.defineProperty(Duplex.prototype, 'destroyed', {
  // making it explicit this property is not enumerable
  // because otherwise some prototype manipulation in
  // userland will fail
  enumerable: false,
  get: function get() {
    if (this._readableState === undefined || this._writableState === undefined) {
      return false;
    }

    return this._readableState.destroyed && this._writableState.destroyed;
  },
  set: function set(value) {
    // we ignore the value if the stream
    // has not been initialized yet
    if (this._readableState === undefined || this._writableState === undefined) {
      return;
    } // backward compatibility, the user is explicitly
    // managing destroyed


    this._readableState.destroyed = value;
    this._writableState.destroyed = value;
  }
});
}).call(this,require('_process'))
},{"./_stream_readable":12,"./_stream_writable":14,"_process":220,"inherits":2}],11:[function(require,module,exports){
// Copyright Joyent, Inc. and other Node contributors.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit
// persons to whom the Software is furnished to do so, subject to the
// following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
// USE OR OTHER DEALINGS IN THE SOFTWARE.
// a passthrough stream.
// basically just the most minimal sort of Transform stream.
// Every written chunk gets output as-is.
'use strict';

module.exports = PassThrough;

var Transform = require('./_stream_transform');

require('inherits')(PassThrough, Transform);

function PassThrough(options) {
  if (!(this instanceof PassThrough)) return new PassThrough(options);
  Transform.call(this, options);
}

PassThrough.prototype._transform = function (chunk, encoding, cb) {
  cb(null, chunk);
};
},{"./_stream_transform":13,"inherits":2}],12:[function(require,module,exports){
(function (process,global){
// Copyright Joyent, Inc. and other Node contributors.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit
// persons to whom the Software is furnished to do so, subject to the
// following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
// USE OR OTHER DEALINGS IN THE SOFTWARE.
'use strict';

module.exports = Readable;
/*<replacement>*/

var Duplex;
/*</replacement>*/

Readable.ReadableState = ReadableState;
/*<replacement>*/

var EE = require('events').EventEmitter;

var EElistenerCount = function EElistenerCount(emitter, type) {
  return emitter.listeners(type).length;
};
/*</replacement>*/

/*<replacement>*/


var Stream = require('./internal/streams/stream');
/*</replacement>*/


var Buffer = require('buffer').Buffer;

var OurUint8Array = global.Uint8Array || function () {};

function _uint8ArrayToBuffer(chunk) {
  return Buffer.from(chunk);
}

function _isUint8Array(obj) {
  return Buffer.isBuffer(obj) || obj instanceof OurUint8Array;
}
/*<replacement>*/


var debugUtil = require('util');

var debug;

if (debugUtil && debugUtil.debuglog) {
  debug = debugUtil.debuglog('stream');
} else {
  debug = function debug() {};
}
/*</replacement>*/


var BufferList = require('./internal/streams/buffer_list');

var destroyImpl = require('./internal/streams/destroy');

var _require = require('./internal/streams/state'),
    getHighWaterMark = _require.getHighWaterMark;

var _require$codes = require('../errors').codes,
    ERR_INVALID_ARG_TYPE = _require$codes.ERR_INVALID_ARG_TYPE,
    ERR_STREAM_PUSH_AFTER_EOF = _require$codes.ERR_STREAM_PUSH_AFTER_EOF,
    ERR_METHOD_NOT_IMPLEMENTED = _require$codes.ERR_METHOD_NOT_IMPLEMENTED,
    ERR_STREAM_UNSHIFT_AFTER_END_EVENT = _require$codes.ERR_STREAM_UNSHIFT_AFTER_END_EVENT;

var _require2 = require('../experimentalWarning'),
    emitExperimentalWarning = _require2.emitExperimentalWarning; // Lazy loaded to improve the startup performance.


var StringDecoder;
var createReadableStreamAsyncIterator;

require('inherits')(Readable, Stream);

var kProxyEvents = ['error', 'close', 'destroy', 'pause', 'resume'];

function prependListener(emitter, event, fn) {
  // Sadly this is not cacheable as some libraries bundle their own
  // event emitter implementation with them.
  if (typeof emitter.prependListener === 'function') return emitter.prependListener(event, fn); // This is a hack to make sure that our error handler is attached before any
  // userland ones.  NEVER DO THIS. This is here only because this code needs
  // to continue to work with older versions of Node.js that do not include
  // the prependListener() method. The goal is to eventually remove this hack.

  if (!emitter._events || !emitter._events[event]) emitter.on(event, fn);else if (Array.isArray(emitter._events[event])) emitter._events[event].unshift(fn);else emitter._events[event] = [fn, emitter._events[event]];
}

function ReadableState(options, stream, isDuplex) {
  Duplex = Duplex || require('./_stream_duplex');
  options = options || {}; // Duplex streams are both readable and writable, but share
  // the same options object.
  // However, some cases require setting options to different
  // values for the readable and the writable sides of the duplex stream.
  // These options can be provided separately as readableXXX and writableXXX.

  if (typeof isDuplex !== 'boolean') isDuplex = stream instanceof Duplex; // object stream flag. Used to make read(n) ignore n and to
  // make all the buffer merging and length checks go away

  this.objectMode = !!options.objectMode;
  if (isDuplex) this.objectMode = this.objectMode || !!options.readableObjectMode; // the point at which it stops calling _read() to fill the buffer
  // Note: 0 is a valid value, means "don't call _read preemptively ever"

  this.highWaterMark = getHighWaterMark(this, options, 'readableHighWaterMark', isDuplex); // A linked list is used to store data chunks instead of an array because the
  // linked list can remove elements from the beginning faster than
  // array.shift()

  this.buffer = new BufferList();
  this.length = 0;
  this.pipes = null;
  this.pipesCount = 0;
  this.flowing = null;
  this.ended = false;
  this.endEmitted = false;
  this.reading = false; // a flag to be able to tell if the event 'readable'/'data' is emitted
  // immediately, or on a later tick.  We set this to true at first, because
  // any actions that shouldn't happen until "later" should generally also
  // not happen before the first read call.

  this.sync = true; // whenever we return null, then we set a flag to say
  // that we're awaiting a 'readable' event emission.

  this.needReadable = false;
  this.emittedReadable = false;
  this.readableListening = false;
  this.resumeScheduled = false;
  this.paused = true; // Should close be emitted on destroy. Defaults to true.

  this.emitClose = options.emitClose !== false; // has it been destroyed

  this.destroyed = false; // Crypto is kind of old and crusty.  Historically, its default string
  // encoding is 'binary' so we have to make this configurable.
  // Everything else in the universe uses 'utf8', though.

  this.defaultEncoding = options.defaultEncoding || 'utf8'; // the number of writers that are awaiting a drain event in .pipe()s

  this.awaitDrain = 0; // if true, a maybeReadMore has been scheduled

  this.readingMore = false;
  this.decoder = null;
  this.encoding = null;

  if (options.encoding) {
    if (!StringDecoder) StringDecoder = require('string_decoder/').StringDecoder;
    this.decoder = new StringDecoder(options.encoding);
    this.encoding = options.encoding;
  }
}

function Readable(options) {
  Duplex = Duplex || require('./_stream_duplex');
  if (!(this instanceof Readable)) return new Readable(options); // Checking for a Stream.Duplex instance is faster here instead of inside
  // the ReadableState constructor, at least with V8 6.5

  var isDuplex = this instanceof Duplex;
  this._readableState = new ReadableState(options, this, isDuplex); // legacy

  this.readable = true;

  if (options) {
    if (typeof options.read === 'function') this._read = options.read;
    if (typeof options.destroy === 'function') this._destroy = options.destroy;
  }

  Stream.call(this);
}

Object.defineProperty(Readable.prototype, 'destroyed', {
  // making it explicit this property is not enumerable
  // because otherwise some prototype manipulation in
  // userland will fail
  enumerable: false,
  get: function get() {
    if (this._readableState === undefined) {
      return false;
    }

    return this._readableState.destroyed;
  },
  set: function set(value) {
    // we ignore the value if the stream
    // has not been initialized yet
    if (!this._readableState) {
      return;
    } // backward compatibility, the user is explicitly
    // managing destroyed


    this._readableState.destroyed = value;
  }
});
Readable.prototype.destroy = destroyImpl.destroy;
Readable.prototype._undestroy = destroyImpl.undestroy;

Readable.prototype._destroy = function (err, cb) {
  cb(err);
}; // Manually shove something into the read() buffer.
// This returns true if the highWaterMark has not been hit yet,
// similar to how Writable.write() returns true if you should
// write() some more.


Readable.prototype.push = function (chunk, encoding) {
  var state = this._readableState;
  var skipChunkCheck;

  if (!state.objectMode) {
    if (typeof chunk === 'string') {
      encoding = encoding || state.defaultEncoding;

      if (encoding !== state.encoding) {
        chunk = Buffer.from(chunk, encoding);
        encoding = '';
      }

      skipChunkCheck = true;
    }
  } else {
    skipChunkCheck = true;
  }

  return readableAddChunk(this, chunk, encoding, false, skipChunkCheck);
}; // Unshift should *always* be something directly out of read()


Readable.prototype.unshift = function (chunk) {
  return readableAddChunk(this, chunk, null, true, false);
};

function readableAddChunk(stream, chunk, encoding, addToFront, skipChunkCheck) {
  debug('readableAddChunk', chunk);
  var state = stream._readableState;

  if (chunk === null) {
    state.reading = false;
    onEofChunk(stream, state);
  } else {
    var er;
    if (!skipChunkCheck) er = chunkInvalid(state, chunk);

    if (er) {
      stream.emit('error', er);
    } else if (state.objectMode || chunk && chunk.length > 0) {
      if (typeof chunk !== 'string' && !state.objectMode && Object.getPrototypeOf(chunk) !== Buffer.prototype) {
        chunk = _uint8ArrayToBuffer(chunk);
      }

      if (addToFront) {
        if (state.endEmitted) stream.emit('error', new ERR_STREAM_UNSHIFT_AFTER_END_EVENT());else addChunk(stream, state, chunk, true);
      } else if (state.ended) {
        stream.emit('error', new ERR_STREAM_PUSH_AFTER_EOF());
      } else if (state.destroyed) {
        return false;
      } else {
        state.reading = false;

        if (state.decoder && !encoding) {
          chunk = state.decoder.write(chunk);
          if (state.objectMode || chunk.length !== 0) addChunk(stream, state, chunk, false);else maybeReadMore(stream, state);
        } else {
          addChunk(stream, state, chunk, false);
        }
      }
    } else if (!addToFront) {
      state.reading = false;
      maybeReadMore(stream, state);
    }
  } // We can push more data if we are below the highWaterMark.
  // Also, if we have no data yet, we can stand some more bytes.
  // This is to work around cases where hwm=0, such as the repl.


  return !state.ended && (state.length < state.highWaterMark || state.length === 0);
}

function addChunk(stream, state, chunk, addToFront) {
  if (state.flowing && state.length === 0 && !state.sync) {
    state.awaitDrain = 0;
    stream.emit('data', chunk);
  } else {
    // update the buffer info.
    state.length += state.objectMode ? 1 : chunk.length;
    if (addToFront) state.buffer.unshift(chunk);else state.buffer.push(chunk);
    if (state.needReadable) emitReadable(stream);
  }

  maybeReadMore(stream, state);
}

function chunkInvalid(state, chunk) {
  var er;

  if (!_isUint8Array(chunk) && typeof chunk !== 'string' && chunk !== undefined && !state.objectMode) {
    er = new ERR_INVALID_ARG_TYPE('chunk', ['string', 'Buffer', 'Uint8Array'], chunk);
  }

  return er;
}

Readable.prototype.isPaused = function () {
  return this._readableState.flowing === false;
}; // backwards compatibility.


Readable.prototype.setEncoding = function (enc) {
  if (!StringDecoder) StringDecoder = require('string_decoder/').StringDecoder;
  this._readableState.decoder = new StringDecoder(enc); // if setEncoding(null), decoder.encoding equals utf8

  this._readableState.encoding = this._readableState.decoder.encoding;
  return this;
}; // Don't raise the hwm > 8MB


var MAX_HWM = 0x800000;

function computeNewHighWaterMark(n) {
  if (n >= MAX_HWM) {
    n = MAX_HWM;
  } else {
    // Get the next highest power of 2 to prevent increasing hwm excessively in
    // tiny amounts
    n--;
    n |= n >>> 1;
    n |= n >>> 2;
    n |= n >>> 4;
    n |= n >>> 8;
    n |= n >>> 16;
    n++;
  }

  return n;
} // This function is designed to be inlinable, so please take care when making
// changes to the function body.


function howMuchToRead(n, state) {
  if (n <= 0 || state.length === 0 && state.ended) return 0;
  if (state.objectMode) return 1;

  if (n !== n) {
    // Only flow one buffer at a time
    if (state.flowing && state.length) return state.buffer.head.data.length;else return state.length;
  } // If we're asking for more than the current hwm, then raise the hwm.


  if (n > state.highWaterMark) state.highWaterMark = computeNewHighWaterMark(n);
  if (n <= state.length) return n; // Don't have enough

  if (!state.ended) {
    state.needReadable = true;
    return 0;
  }

  return state.length;
} // you can override either this method, or the async _read(n) below.


Readable.prototype.read = function (n) {
  debug('read', n);
  n = parseInt(n, 10);
  var state = this._readableState;
  var nOrig = n;
  if (n !== 0) state.emittedReadable = false; // if we're doing read(0) to trigger a readable event, but we
  // already have a bunch of data in the buffer, then just trigger
  // the 'readable' event and move on.

  if (n === 0 && state.needReadable && ((state.highWaterMark !== 0 ? state.length >= state.highWaterMark : state.length > 0) || state.ended)) {
    debug('read: emitReadable', state.length, state.ended);
    if (state.length === 0 && state.ended) endReadable(this);else emitReadable(this);
    return null;
  }

  n = howMuchToRead(n, state); // if we've ended, and we're now clear, then finish it up.

  if (n === 0 && state.ended) {
    if (state.length === 0) endReadable(this);
    return null;
  } // All the actual chunk generation logic needs to be
  // *below* the call to _read.  The reason is that in certain
  // synthetic stream cases, such as passthrough streams, _read
  // may be a completely synchronous operation which may change
  // the state of the read buffer, providing enough data when
  // before there was *not* enough.
  //
  // So, the steps are:
  // 1. Figure out what the state of things will be after we do
  // a read from the buffer.
  //
  // 2. If that resulting state will trigger a _read, then call _read.
  // Note that this may be asynchronous, or synchronous.  Yes, it is
  // deeply ugly to write APIs this way, but that still doesn't mean
  // that the Readable class should behave improperly, as streams are
  // designed to be sync/async agnostic.
  // Take note if the _read call is sync or async (ie, if the read call
  // has returned yet), so that we know whether or not it's safe to emit
  // 'readable' etc.
  //
  // 3. Actually pull the requested chunks out of the buffer and return.
  // if we need a readable event, then we need to do some reading.


  var doRead = state.needReadable;
  debug('need readable', doRead); // if we currently have less than the highWaterMark, then also read some

  if (state.length === 0 || state.length - n < state.highWaterMark) {
    doRead = true;
    debug('length less than watermark', doRead);
  } // however, if we've ended, then there's no point, and if we're already
  // reading, then it's unnecessary.


  if (state.ended || state.reading) {
    doRead = false;
    debug('reading or ended', doRead);
  } else if (doRead) {
    debug('do read');
    state.reading = true;
    state.sync = true; // if the length is currently zero, then we *need* a readable event.

    if (state.length === 0) state.needReadable = true; // call internal read method

    this._read(state.highWaterMark);

    state.sync = false; // If _read pushed data synchronously, then `reading` will be false,
    // and we need to re-evaluate how much data we can return to the user.

    if (!state.reading) n = howMuchToRead(nOrig, state);
  }

  var ret;
  if (n > 0) ret = fromList(n, state);else ret = null;

  if (ret === null) {
    state.needReadable = true;
    n = 0;
  } else {
    state.length -= n;
    state.awaitDrain = 0;
  }

  if (state.length === 0) {
    // If we have nothing in the buffer, then we want to know
    // as soon as we *do* get something into the buffer.
    if (!state.ended) state.needReadable = true; // If we tried to read() past the EOF, then emit end on the next tick.

    if (nOrig !== n && state.ended) endReadable(this);
  }

  if (ret !== null) this.emit('data', ret);
  return ret;
};

function onEofChunk(stream, state) {
  if (state.ended) return;

  if (state.decoder) {
    var chunk = state.decoder.end();

    if (chunk && chunk.length) {
      state.buffer.push(chunk);
      state.length += state.objectMode ? 1 : chunk.length;
    }
  }

  state.ended = true;

  if (state.sync) {
    // if we are sync, wait until next tick to emit the data.
    // Otherwise we risk emitting data in the flow()
    // the readable code triggers during a read() call
    emitReadable(stream);
  } else {
    // emit 'readable' now to make sure it gets picked up.
    state.needReadable = false;

    if (!state.emittedReadable) {
      state.emittedReadable = true;
      emitReadable_(stream);
    }
  }
} // Don't emit readable right away in sync mode, because this can trigger
// another read() call => stack overflow.  This way, it might trigger
// a nextTick recursion warning, but that's not so bad.


function emitReadable(stream) {
  var state = stream._readableState;
  state.needReadable = false;

  if (!state.emittedReadable) {
    debug('emitReadable', state.flowing);
    state.emittedReadable = true;
    process.nextTick(emitReadable_, stream);
  }
}

function emitReadable_(stream) {
  var state = stream._readableState;
  debug('emitReadable_', state.destroyed, state.length, state.ended);

  if (!state.destroyed && (state.length || state.ended)) {
    stream.emit('readable');
  } // The stream needs another readable event if
  // 1. It is not flowing, as the flow mechanism will take
  //    care of it.
  // 2. It is not ended.
  // 3. It is below the highWaterMark, so we can schedule
  //    another readable later.


  state.needReadable = !state.flowing && !state.ended && state.length <= state.highWaterMark;
  flow(stream);
} // at this point, the user has presumably seen the 'readable' event,
// and called read() to consume some data.  that may have triggered
// in turn another _read(n) call, in which case reading = true if
// it's in progress.
// However, if we're not ended, or reading, and the length < hwm,
// then go ahead and try to read some more preemptively.


function maybeReadMore(stream, state) {
  if (!state.readingMore) {
    state.readingMore = true;
    process.nextTick(maybeReadMore_, stream, state);
  }
}

function maybeReadMore_(stream, state) {
  var len = state.length;

  while (!state.reading && !state.ended && state.length < state.highWaterMark) {
    debug('maybeReadMore read 0');
    stream.read(0);
    if (len === state.length) // didn't get any data, stop spinning.
      break;else len = state.length;
  }

  state.readingMore = false;
} // abstract method.  to be overridden in specific implementation classes.
// call cb(er, data) where data is <= n in length.
// for virtual (non-string, non-buffer) streams, "length" is somewhat
// arbitrary, and perhaps not very meaningful.


Readable.prototype._read = function (n) {
  this.emit('error', new ERR_METHOD_NOT_IMPLEMENTED('_read()'));
};

Readable.prototype.pipe = function (dest, pipeOpts) {
  var src = this;
  var state = this._readableState;

  switch (state.pipesCount) {
    case 0:
      state.pipes = dest;
      break;

    case 1:
      state.pipes = [state.pipes, dest];
      break;

    default:
      state.pipes.push(dest);
      break;
  }

  state.pipesCount += 1;
  debug('pipe count=%d opts=%j', state.pipesCount, pipeOpts);
  var doEnd = (!pipeOpts || pipeOpts.end !== false) && dest !== process.stdout && dest !== process.stderr;
  var endFn = doEnd ? onend : unpipe;
  if (state.endEmitted) process.nextTick(endFn);else src.once('end', endFn);
  dest.on('unpipe', onunpipe);

  function onunpipe(readable, unpipeInfo) {
    debug('onunpipe');

    if (readable === src) {
      if (unpipeInfo && unpipeInfo.hasUnpiped === false) {
        unpipeInfo.hasUnpiped = true;
        cleanup();
      }
    }
  }

  function onend() {
    debug('onend');
    dest.end();
  } // when the dest drains, it reduces the awaitDrain counter
  // on the source.  This would be more elegant with a .once()
  // handler in flow(), but adding and removing repeatedly is
  // too slow.


  var ondrain = pipeOnDrain(src);
  dest.on('drain', ondrain);
  var cleanedUp = false;

  function cleanup() {
    debug('cleanup'); // cleanup event handlers once the pipe is broken

    dest.removeListener('close', onclose);
    dest.removeListener('finish', onfinish);
    dest.removeListener('drain', ondrain);
    dest.removeListener('error', onerror);
    dest.removeListener('unpipe', onunpipe);
    src.removeListener('end', onend);
    src.removeListener('end', unpipe);
    src.removeListener('data', ondata);
    cleanedUp = true; // if the reader is waiting for a drain event from this
    // specific writer, then it would cause it to never start
    // flowing again.
    // So, if this is awaiting a drain, then we just call it now.
    // If we don't know, then assume that we are waiting for one.

    if (state.awaitDrain && (!dest._writableState || dest._writableState.needDrain)) ondrain();
  }

  src.on('data', ondata);

  function ondata(chunk) {
    debug('ondata');
    var ret = dest.write(chunk);
    debug('dest.write', ret);

    if (ret === false) {
      // If the user unpiped during `dest.write()`, it is possible
      // to get stuck in a permanently paused state if that write
      // also returned false.
      // => Check whether `dest` is still a piping destination.
      if ((state.pipesCount === 1 && state.pipes === dest || state.pipesCount > 1 && indexOf(state.pipes, dest) !== -1) && !cleanedUp) {
        debug('false write response, pause', state.awaitDrain);
        state.awaitDrain++;
      }

      src.pause();
    }
  } // if the dest has an error, then stop piping into it.
  // however, don't suppress the throwing behavior for this.


  function onerror(er) {
    debug('onerror', er);
    unpipe();
    dest.removeListener('error', onerror);
    if (EElistenerCount(dest, 'error') === 0) dest.emit('error', er);
  } // Make sure our error handler is attached before userland ones.


  prependListener(dest, 'error', onerror); // Both close and finish should trigger unpipe, but only once.

  function onclose() {
    dest.removeListener('finish', onfinish);
    unpipe();
  }

  dest.once('close', onclose);

  function onfinish() {
    debug('onfinish');
    dest.removeListener('close', onclose);
    unpipe();
  }

  dest.once('finish', onfinish);

  function unpipe() {
    debug('unpipe');
    src.unpipe(dest);
  } // tell the dest that it's being piped to


  dest.emit('pipe', src); // start the flow if it hasn't been started already.

  if (!state.flowing) {
    debug('pipe resume');
    src.resume();
  }

  return dest;
};

function pipeOnDrain(src) {
  return function pipeOnDrainFunctionResult() {
    var state = src._readableState;
    debug('pipeOnDrain', state.awaitDrain);
    if (state.awaitDrain) state.awaitDrain--;

    if (state.awaitDrain === 0 && EElistenerCount(src, 'data')) {
      state.flowing = true;
      flow(src);
    }
  };
}

Readable.prototype.unpipe = function (dest) {
  var state = this._readableState;
  var unpipeInfo = {
    hasUnpiped: false
  }; // if we're not piping anywhere, then do nothing.

  if (state.pipesCount === 0) return this; // just one destination.  most common case.

  if (state.pipesCount === 1) {
    // passed in one, but it's not the right one.
    if (dest && dest !== state.pipes) return this;
    if (!dest) dest = state.pipes; // got a match.

    state.pipes = null;
    state.pipesCount = 0;
    state.flowing = false;
    if (dest) dest.emit('unpipe', this, unpipeInfo);
    return this;
  } // slow case. multiple pipe destinations.


  if (!dest) {
    // remove all.
    var dests = state.pipes;
    var len = state.pipesCount;
    state.pipes = null;
    state.pipesCount = 0;
    state.flowing = false;

    for (var i = 0; i < len; i++) {
      dests[i].emit('unpipe', this, {
        hasUnpiped: false
      });
    }

    return this;
  } // try to find the right one.


  var index = indexOf(state.pipes, dest);
  if (index === -1) return this;
  state.pipes.splice(index, 1);
  state.pipesCount -= 1;
  if (state.pipesCount === 1) state.pipes = state.pipes[0];
  dest.emit('unpipe', this, unpipeInfo);
  return this;
}; // set up data events if they are asked for
// Ensure readable listeners eventually get something


Readable.prototype.on = function (ev, fn) {
  var res = Stream.prototype.on.call(this, ev, fn);
  var state = this._readableState;

  if (ev === 'data') {
    // update readableListening so that resume() may be a no-op
    // a few lines down. This is needed to support once('readable').
    state.readableListening = this.listenerCount('readable') > 0; // Try start flowing on next tick if stream isn't explicitly paused

    if (state.flowing !== false) this.resume();
  } else if (ev === 'readable') {
    if (!state.endEmitted && !state.readableListening) {
      state.readableListening = state.needReadable = true;
      state.flowing = false;
      state.emittedReadable = false;
      debug('on readable', state.length, state.reading);

      if (state.length) {
        emitReadable(this);
      } else if (!state.reading) {
        process.nextTick(nReadingNextTick, this);
      }
    }
  }

  return res;
};

Readable.prototype.addListener = Readable.prototype.on;

Readable.prototype.removeListener = function (ev, fn) {
  var res = Stream.prototype.removeListener.call(this, ev, fn);

  if (ev === 'readable') {
    // We need to check if there is someone still listening to
    // readable and reset the state. However this needs to happen
    // after readable has been emitted but before I/O (nextTick) to
    // support once('readable', fn) cycles. This means that calling
    // resume within the same tick will have no
    // effect.
    process.nextTick(updateReadableListening, this);
  }

  return res;
};

Readable.prototype.removeAllListeners = function (ev) {
  var res = Stream.prototype.removeAllListeners.apply(this, arguments);

  if (ev === 'readable' || ev === undefined) {
    // We need to check if there is someone still listening to
    // readable and reset the state. However this needs to happen
    // after readable has been emitted but before I/O (nextTick) to
    // support once('readable', fn) cycles. This means that calling
    // resume within the same tick will have no
    // effect.
    process.nextTick(updateReadableListening, this);
  }

  return res;
};

function updateReadableListening(self) {
  var state = self._readableState;
  state.readableListening = self.listenerCount('readable') > 0;

  if (state.resumeScheduled && !state.paused) {
    // flowing needs to be set to true now, otherwise
    // the upcoming resume will not flow.
    state.flowing = true; // crude way to check if we should resume
  } else if (self.listenerCount('data') > 0) {
    self.resume();
  }
}

function nReadingNextTick(self) {
  debug('readable nexttick read 0');
  self.read(0);
} // pause() and resume() are remnants of the legacy readable stream API
// If the user uses them, then switch into old mode.


Readable.prototype.resume = function () {
  var state = this._readableState;

  if (!state.flowing) {
    debug('resume'); // we flow only if there is no one listening
    // for readable, but we still have to call
    // resume()

    state.flowing = !state.readableListening;
    resume(this, state);
  }

  state.paused = false;
  return this;
};

function resume(stream, state) {
  if (!state.resumeScheduled) {
    state.resumeScheduled = true;
    process.nextTick(resume_, stream, state);
  }
}

function resume_(stream, state) {
  debug('resume', state.reading);

  if (!state.reading) {
    stream.read(0);
  }

  state.resumeScheduled = false;
  stream.emit('resume');
  flow(stream);
  if (state.flowing && !state.reading) stream.read(0);
}

Readable.prototype.pause = function () {
  debug('call pause flowing=%j', this._readableState.flowing);

  if (this._readableState.flowing !== false) {
    debug('pause');
    this._readableState.flowing = false;
    this.emit('pause');
  }

  this._readableState.paused = true;
  return this;
};

function flow(stream) {
  var state = stream._readableState;
  debug('flow', state.flowing);

  while (state.flowing && stream.read() !== null) {
    ;
  }
} // wrap an old-style stream as the async data source.
// This is *not* part of the readable stream interface.
// It is an ugly unfortunate mess of history.


Readable.prototype.wrap = function (stream) {
  var _this = this;

  var state = this._readableState;
  var paused = false;
  stream.on('end', function () {
    debug('wrapped end');

    if (state.decoder && !state.ended) {
      var chunk = state.decoder.end();
      if (chunk && chunk.length) _this.push(chunk);
    }

    _this.push(null);
  });
  stream.on('data', function (chunk) {
    debug('wrapped data');
    if (state.decoder) chunk = state.decoder.write(chunk); // don't skip over falsy values in objectMode

    if (state.objectMode && (chunk === null || chunk === undefined)) return;else if (!state.objectMode && (!chunk || !chunk.length)) return;

    var ret = _this.push(chunk);

    if (!ret) {
      paused = true;
      stream.pause();
    }
  }); // proxy all the other methods.
  // important when wrapping filters and duplexes.

  for (var i in stream) {
    if (this[i] === undefined && typeof stream[i] === 'function') {
      this[i] = function methodWrap(method) {
        return function methodWrapReturnFunction() {
          return stream[method].apply(stream, arguments);
        };
      }(i);
    }
  } // proxy certain important events.


  for (var n = 0; n < kProxyEvents.length; n++) {
    stream.on(kProxyEvents[n], this.emit.bind(this, kProxyEvents[n]));
  } // when we try to consume some more bytes, simply unpause the
  // underlying stream.


  this._read = function (n) {
    debug('wrapped _read', n);

    if (paused) {
      paused = false;
      stream.resume();
    }
  };

  return this;
};

if (typeof Symbol === 'function') {
  Readable.prototype[Symbol.asyncIterator] = function () {
    emitExperimentalWarning('Readable[Symbol.asyncIterator]');

    if (createReadableStreamAsyncIterator === undefined) {
      createReadableStreamAsyncIterator = require('./internal/streams/async_iterator');
    }

    return createReadableStreamAsyncIterator(this);
  };
}

Object.defineProperty(Readable.prototype, 'readableHighWaterMark', {
  // making it explicit this property is not enumerable
  // because otherwise some prototype manipulation in
  // userland will fail
  enumerable: false,
  get: function get() {
    return this._readableState.highWaterMark;
  }
});
Object.defineProperty(Readable.prototype, 'readableBuffer', {
  // making it explicit this property is not enumerable
  // because otherwise some prototype manipulation in
  // userland will fail
  enumerable: false,
  get: function get() {
    return this._readableState && this._readableState.buffer;
  }
});
Object.defineProperty(Readable.prototype, 'readableFlowing', {
  // making it explicit this property is not enumerable
  // because otherwise some prototype manipulation in
  // userland will fail
  enumerable: false,
  get: function get() {
    return this._readableState.flowing;
  },
  set: function set(state) {
    if (this._readableState) {
      this._readableState.flowing = state;
    }
  }
}); // exposed for testing purposes only.

Readable._fromList = fromList;
Object.defineProperty(Readable.prototype, 'readableLength', {
  // making it explicit this property is not enumerable
  // because otherwise some prototype manipulation in
  // userland will fail
  enumerable: false,
  get: function get() {
    return this._readableState.length;
  }
}); // Pluck off n bytes from an array of buffers.
// Length is the combined lengths of all the buffers in the list.
// This function is designed to be inlinable, so please take care when making
// changes to the function body.

function fromList(n, state) {
  // nothing buffered
  if (state.length === 0) return null;
  var ret;
  if (state.objectMode) ret = state.buffer.shift();else if (!n || n >= state.length) {
    // read it all, truncate the list
    if (state.decoder) ret = state.buffer.join('');else if (state.buffer.length === 1) ret = state.buffer.first();else ret = state.buffer.concat(state.length);
    state.buffer.clear();
  } else {
    // read part of list
    ret = state.buffer.consume(n, state.decoder);
  }
  return ret;
}

function endReadable(stream) {
  var state = stream._readableState;
  debug('endReadable', state.endEmitted);

  if (!state.endEmitted) {
    state.ended = true;
    process.nextTick(endReadableNT, state, stream);
  }
}

function endReadableNT(state, stream) {
  debug('endReadableNT', state.endEmitted, state.length); // Check that we didn't get one last unshift.

  if (!state.endEmitted && state.length === 0) {
    state.endEmitted = true;
    stream.readable = false;
    stream.emit('end');
  }
}

function indexOf(xs, x) {
  for (var i = 0, l = xs.length; i < l; i++) {
    if (xs[i] === x) return i;
  }

  return -1;
}
}).call(this,require('_process'),typeof global !== "undefined" ? global : typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{"../errors":8,"../experimentalWarning":9,"./_stream_duplex":10,"./internal/streams/async_iterator":15,"./internal/streams/buffer_list":16,"./internal/streams/destroy":17,"./internal/streams/state":19,"./internal/streams/stream":20,"_process":220,"buffer":215,"events":216,"inherits":2,"string_decoder/":23,"util":214}],13:[function(require,module,exports){
// Copyright Joyent, Inc. and other Node contributors.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit
// persons to whom the Software is furnished to do so, subject to the
// following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
// USE OR OTHER DEALINGS IN THE SOFTWARE.
// a transform stream is a readable/writable stream where you do
// something with the data.  Sometimes it's called a "filter",
// but that's not a great name for it, since that implies a thing where
// some bits pass through, and others are simply ignored.  (That would
// be a valid example of a transform, of course.)
//
// While the output is causally related to the input, it's not a
// necessarily symmetric or synchronous transformation.  For example,
// a zlib stream might take multiple plain-text writes(), and then
// emit a single compressed chunk some time in the future.
//
// Here's how this works:
//
// The Transform stream has all the aspects of the readable and writable
// stream classes.  When you write(chunk), that calls _write(chunk,cb)
// internally, and returns false if there's a lot of pending writes
// buffered up.  When you call read(), that calls _read(n) until
// there's enough pending readable data buffered up.
//
// In a transform stream, the written data is placed in a buffer.  When
// _read(n) is called, it transforms the queued up data, calling the
// buffered _write cb's as it consumes chunks.  If consuming a single
// written chunk would result in multiple output chunks, then the first
// outputted bit calls the readcb, and subsequent chunks just go into
// the read buffer, and will cause it to emit 'readable' if necessary.
//
// This way, back-pressure is actually determined by the reading side,
// since _read has to be called to start processing a new chunk.  However,
// a pathological inflate type of transform can cause excessive buffering
// here.  For example, imagine a stream where every byte of input is
// interpreted as an integer from 0-255, and then results in that many
// bytes of output.  Writing the 4 bytes {ff,ff,ff,ff} would result in
// 1kb of data being output.  In this case, you could write a very small
// amount of input, and end up with a very large amount of output.  In
// such a pathological inflating mechanism, there'd be no way to tell
// the system to stop doing the transform.  A single 4MB write could
// cause the system to run out of memory.
//
// However, even in such a pathological case, only a single written chunk
// would be consumed, and then the rest would wait (un-transformed) until
// the results of the previous transformed chunk were consumed.
'use strict';

module.exports = Transform;

var _require$codes = require('../errors').codes,
    ERR_METHOD_NOT_IMPLEMENTED = _require$codes.ERR_METHOD_NOT_IMPLEMENTED,
    ERR_MULTIPLE_CALLBACK = _require$codes.ERR_MULTIPLE_CALLBACK,
    ERR_TRANSFORM_ALREADY_TRANSFORMING = _require$codes.ERR_TRANSFORM_ALREADY_TRANSFORMING,
    ERR_TRANSFORM_WITH_LENGTH_0 = _require$codes.ERR_TRANSFORM_WITH_LENGTH_0;

var Duplex = require('./_stream_duplex');

require('inherits')(Transform, Duplex);

function afterTransform(er, data) {
  var ts = this._transformState;
  ts.transforming = false;
  var cb = ts.writecb;

  if (cb === null) {
    return this.emit('error', new ERR_MULTIPLE_CALLBACK());
  }

  ts.writechunk = null;
  ts.writecb = null;
  if (data != null) // single equals check for both `null` and `undefined`
    this.push(data);
  cb(er);
  var rs = this._readableState;
  rs.reading = false;

  if (rs.needReadable || rs.length < rs.highWaterMark) {
    this._read(rs.highWaterMark);
  }
}

function Transform(options) {
  if (!(this instanceof Transform)) return new Transform(options);
  Duplex.call(this, options);
  this._transformState = {
    afterTransform: afterTransform.bind(this),
    needTransform: false,
    transforming: false,
    writecb: null,
    writechunk: null,
    writeencoding: null
  }; // start out asking for a readable event once data is transformed.

  this._readableState.needReadable = true; // we have implemented the _read method, and done the other things
  // that Readable wants before the first _read call, so unset the
  // sync guard flag.

  this._readableState.sync = false;

  if (options) {
    if (typeof options.transform === 'function') this._transform = options.transform;
    if (typeof options.flush === 'function') this._flush = options.flush;
  } // When the writable side finishes, then flush out anything remaining.


  this.on('prefinish', prefinish);
}

function prefinish() {
  var _this = this;

  if (typeof this._flush === 'function' && !this._readableState.destroyed) {
    this._flush(function (er, data) {
      done(_this, er, data);
    });
  } else {
    done(this, null, null);
  }
}

Transform.prototype.push = function (chunk, encoding) {
  this._transformState.needTransform = false;
  return Duplex.prototype.push.call(this, chunk, encoding);
}; // This is the part where you do stuff!
// override this function in implementation classes.
// 'chunk' is an input chunk.
//
// Call `push(newChunk)` to pass along transformed output
// to the readable side.  You may call 'push' zero or more times.
//
// Call `cb(err)` when you are done with this chunk.  If you pass
// an error, then that'll put the hurt on the whole operation.  If you
// never call cb(), then you'll never get another chunk.


Transform.prototype._transform = function (chunk, encoding, cb) {
  cb(new ERR_METHOD_NOT_IMPLEMENTED('_transform()'));
};

Transform.prototype._write = function (chunk, encoding, cb) {
  var ts = this._transformState;
  ts.writecb = cb;
  ts.writechunk = chunk;
  ts.writeencoding = encoding;

  if (!ts.transforming) {
    var rs = this._readableState;
    if (ts.needTransform || rs.needReadable || rs.length < rs.highWaterMark) this._read(rs.highWaterMark);
  }
}; // Doesn't matter what the args are here.
// _transform does all the work.
// That we got here means that the readable side wants more data.


Transform.prototype._read = function (n) {
  var ts = this._transformState;

  if (ts.writechunk !== null && !ts.transforming) {
    ts.transforming = true;

    this._transform(ts.writechunk, ts.writeencoding, ts.afterTransform);
  } else {
    // mark that we need a transform, so that any data that comes in
    // will get processed, now that we've asked for it.
    ts.needTransform = true;
  }
};

Transform.prototype._destroy = function (err, cb) {
  Duplex.prototype._destroy.call(this, err, function (err2) {
    cb(err2);
  });
};

function done(stream, er, data) {
  if (er) return stream.emit('error', er);
  if (data != null) // single equals check for both `null` and `undefined`
    stream.push(data); // TODO(BridgeAR): Write a test for these two error cases
  // if there's nothing in the write buffer, then that means
  // that nothing more will ever be provided

  if (stream._writableState.length) throw new ERR_TRANSFORM_WITH_LENGTH_0();
  if (stream._transformState.transforming) throw new ERR_TRANSFORM_ALREADY_TRANSFORMING();
  return stream.push(null);
}
},{"../errors":8,"./_stream_duplex":10,"inherits":2}],14:[function(require,module,exports){
(function (process,global){
// Copyright Joyent, Inc. and other Node contributors.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit
// persons to whom the Software is furnished to do so, subject to the
// following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
// USE OR OTHER DEALINGS IN THE SOFTWARE.
// A bit simpler than readable streams.
// Implement an async ._write(chunk, encoding, cb), and it'll handle all
// the drain event emission and buffering.
'use strict';

module.exports = Writable;
/* <replacement> */

function WriteReq(chunk, encoding, cb) {
  this.chunk = chunk;
  this.encoding = encoding;
  this.callback = cb;
  this.next = null;
} // It seems a linked list but it is not
// there will be only 2 of these for each stream


function CorkedRequest(state) {
  var _this = this;

  this.next = null;
  this.entry = null;

  this.finish = function () {
    onCorkedFinish(_this, state);
  };
}
/* </replacement> */

/*<replacement>*/


var Duplex;
/*</replacement>*/

Writable.WritableState = WritableState;
/*<replacement>*/

var internalUtil = {
  deprecate: require('util-deprecate')
};
/*</replacement>*/

/*<replacement>*/

var Stream = require('./internal/streams/stream');
/*</replacement>*/


var Buffer = require('buffer').Buffer;

var OurUint8Array = global.Uint8Array || function () {};

function _uint8ArrayToBuffer(chunk) {
  return Buffer.from(chunk);
}

function _isUint8Array(obj) {
  return Buffer.isBuffer(obj) || obj instanceof OurUint8Array;
}

var destroyImpl = require('./internal/streams/destroy');

var _require = require('./internal/streams/state'),
    getHighWaterMark = _require.getHighWaterMark;

var _require$codes = require('../errors').codes,
    ERR_INVALID_ARG_TYPE = _require$codes.ERR_INVALID_ARG_TYPE,
    ERR_METHOD_NOT_IMPLEMENTED = _require$codes.ERR_METHOD_NOT_IMPLEMENTED,
    ERR_MULTIPLE_CALLBACK = _require$codes.ERR_MULTIPLE_CALLBACK,
    ERR_STREAM_CANNOT_PIPE = _require$codes.ERR_STREAM_CANNOT_PIPE,
    ERR_STREAM_DESTROYED = _require$codes.ERR_STREAM_DESTROYED,
    ERR_STREAM_NULL_VALUES = _require$codes.ERR_STREAM_NULL_VALUES,
    ERR_STREAM_WRITE_AFTER_END = _require$codes.ERR_STREAM_WRITE_AFTER_END,
    ERR_UNKNOWN_ENCODING = _require$codes.ERR_UNKNOWN_ENCODING;

require('inherits')(Writable, Stream);

function nop() {}

function WritableState(options, stream, isDuplex) {
  Duplex = Duplex || require('./_stream_duplex');
  options = options || {}; // Duplex streams are both readable and writable, but share
  // the same options object.
  // However, some cases require setting options to different
  // values for the readable and the writable sides of the duplex stream,
  // e.g. options.readableObjectMode vs. options.writableObjectMode, etc.

  if (typeof isDuplex !== 'boolean') isDuplex = stream instanceof Duplex; // object stream flag to indicate whether or not this stream
  // contains buffers or objects.

  this.objectMode = !!options.objectMode;
  if (isDuplex) this.objectMode = this.objectMode || !!options.writableObjectMode; // the point at which write() starts returning false
  // Note: 0 is a valid value, means that we always return false if
  // the entire buffer is not flushed immediately on write()

  this.highWaterMark = getHighWaterMark(this, options, 'writableHighWaterMark', isDuplex); // if _final has been called

  this.finalCalled = false; // drain event flag.

  this.needDrain = false; // at the start of calling end()

  this.ending = false; // when end() has been called, and returned

  this.ended = false; // when 'finish' is emitted

  this.finished = false; // has it been destroyed

  this.destroyed = false; // should we decode strings into buffers before passing to _write?
  // this is here so that some node-core streams can optimize string
  // handling at a lower level.

  var noDecode = options.decodeStrings === false;
  this.decodeStrings = !noDecode; // Crypto is kind of old and crusty.  Historically, its default string
  // encoding is 'binary' so we have to make this configurable.
  // Everything else in the universe uses 'utf8', though.

  this.defaultEncoding = options.defaultEncoding || 'utf8'; // not an actual buffer we keep track of, but a measurement
  // of how much we're waiting to get pushed to some underlying
  // socket or file.

  this.length = 0; // a flag to see when we're in the middle of a write.

  this.writing = false; // when true all writes will be buffered until .uncork() call

  this.corked = 0; // a flag to be able to tell if the onwrite cb is called immediately,
  // or on a later tick.  We set this to true at first, because any
  // actions that shouldn't happen until "later" should generally also
  // not happen before the first write call.

  this.sync = true; // a flag to know if we're processing previously buffered items, which
  // may call the _write() callback in the same tick, so that we don't
  // end up in an overlapped onwrite situation.

  this.bufferProcessing = false; // the callback that's passed to _write(chunk,cb)

  this.onwrite = function (er) {
    onwrite(stream, er);
  }; // the callback that the user supplies to write(chunk,encoding,cb)


  this.writecb = null; // the amount that is being written when _write is called.

  this.writelen = 0;
  this.bufferedRequest = null;
  this.lastBufferedRequest = null; // number of pending user-supplied write callbacks
  // this must be 0 before 'finish' can be emitted

  this.pendingcb = 0; // emit prefinish if the only thing we're waiting for is _write cbs
  // This is relevant for synchronous Transform streams

  this.prefinished = false; // True if the error was already emitted and should not be thrown again

  this.errorEmitted = false; // Should close be emitted on destroy. Defaults to true.

  this.emitClose = options.emitClose !== false; // count buffered requests

  this.bufferedRequestCount = 0; // allocate the first CorkedRequest, there is always
  // one allocated and free to use, and we maintain at most two

  this.corkedRequestsFree = new CorkedRequest(this);
}

WritableState.prototype.getBuffer = function getBuffer() {
  var current = this.bufferedRequest;
  var out = [];

  while (current) {
    out.push(current);
    current = current.next;
  }

  return out;
};

(function () {
  try {
    Object.defineProperty(WritableState.prototype, 'buffer', {
      get: internalUtil.deprecate(function writableStateBufferGetter() {
        return this.getBuffer();
      }, '_writableState.buffer is deprecated. Use _writableState.getBuffer ' + 'instead.', 'DEP0003')
    });
  } catch (_) {}
})(); // Test _writableState for inheritance to account for Duplex streams,
// whose prototype chain only points to Readable.


var realHasInstance;

if (typeof Symbol === 'function' && Symbol.hasInstance && typeof Function.prototype[Symbol.hasInstance] === 'function') {
  realHasInstance = Function.prototype[Symbol.hasInstance];
  Object.defineProperty(Writable, Symbol.hasInstance, {
    value: function value(object) {
      if (realHasInstance.call(this, object)) return true;
      if (this !== Writable) return false;
      return object && object._writableState instanceof WritableState;
    }
  });
} else {
  realHasInstance = function realHasInstance(object) {
    return object instanceof this;
  };
}

function Writable(options) {
  Duplex = Duplex || require('./_stream_duplex'); // Writable ctor is applied to Duplexes, too.
  // `realHasInstance` is necessary because using plain `instanceof`
  // would return false, as no `_writableState` property is attached.
  // Trying to use the custom `instanceof` for Writable here will also break the
  // Node.js LazyTransform implementation, which has a non-trivial getter for
  // `_writableState` that would lead to infinite recursion.
  // Checking for a Stream.Duplex instance is faster here instead of inside
  // the WritableState constructor, at least with V8 6.5

  var isDuplex = this instanceof Duplex;
  if (!isDuplex && !realHasInstance.call(Writable, this)) return new Writable(options);
  this._writableState = new WritableState(options, this, isDuplex); // legacy.

  this.writable = true;

  if (options) {
    if (typeof options.write === 'function') this._write = options.write;
    if (typeof options.writev === 'function') this._writev = options.writev;
    if (typeof options.destroy === 'function') this._destroy = options.destroy;
    if (typeof options.final === 'function') this._final = options.final;
  }

  Stream.call(this);
} // Otherwise people can pipe Writable streams, which is just wrong.


Writable.prototype.pipe = function () {
  this.emit('error', new ERR_STREAM_CANNOT_PIPE());
};

function writeAfterEnd(stream, cb) {
  var er = new ERR_STREAM_WRITE_AFTER_END(); // TODO: defer error events consistently everywhere, not just the cb

  stream.emit('error', er);
  process.nextTick(cb, er);
} // Checks that a user-supplied chunk is valid, especially for the particular
// mode the stream is in. Currently this means that `null` is never accepted
// and undefined/non-string values are only allowed in object mode.


function validChunk(stream, state, chunk, cb) {
  var er;

  if (chunk === null) {
    er = new ERR_STREAM_NULL_VALUES();
  } else if (typeof chunk !== 'string' && !state.objectMode) {
    er = new ERR_INVALID_ARG_TYPE('chunk', ['string', 'Buffer'], chunk);
  }

  if (er) {
    stream.emit('error', er);
    process.nextTick(cb, er);
    return false;
  }

  return true;
}

Writable.prototype.write = function (chunk, encoding, cb) {
  var state = this._writableState;
  var ret = false;

  var isBuf = !state.objectMode && _isUint8Array(chunk);

  if (isBuf && !Buffer.isBuffer(chunk)) {
    chunk = _uint8ArrayToBuffer(chunk);
  }

  if (typeof encoding === 'function') {
    cb = encoding;
    encoding = null;
  }

  if (isBuf) encoding = 'buffer';else if (!encoding) encoding = state.defaultEncoding;
  if (typeof cb !== 'function') cb = nop;
  if (state.ending) writeAfterEnd(this, cb);else if (isBuf || validChunk(this, state, chunk, cb)) {
    state.pendingcb++;
    ret = writeOrBuffer(this, state, isBuf, chunk, encoding, cb);
  }
  return ret;
};

Writable.prototype.cork = function () {
  this._writableState.corked++;
};

Writable.prototype.uncork = function () {
  var state = this._writableState;

  if (state.corked) {
    state.corked--;
    if (!state.writing && !state.corked && !state.bufferProcessing && state.bufferedRequest) clearBuffer(this, state);
  }
};

Writable.prototype.setDefaultEncoding = function setDefaultEncoding(encoding) {
  // node::ParseEncoding() requires lower case.
  if (typeof encoding === 'string') encoding = encoding.toLowerCase();
  if (!(['hex', 'utf8', 'utf-8', 'ascii', 'binary', 'base64', 'ucs2', 'ucs-2', 'utf16le', 'utf-16le', 'raw'].indexOf((encoding + '').toLowerCase()) > -1)) throw new ERR_UNKNOWN_ENCODING(encoding);
  this._writableState.defaultEncoding = encoding;
  return this;
};

Object.defineProperty(Writable.prototype, 'writableBuffer', {
  // making it explicit this property is not enumerable
  // because otherwise some prototype manipulation in
  // userland will fail
  enumerable: false,
  get: function get() {
    return this._writableState && this._writableState.getBuffer();
  }
});

function decodeChunk(state, chunk, encoding) {
  if (!state.objectMode && state.decodeStrings !== false && typeof chunk === 'string') {
    chunk = Buffer.from(chunk, encoding);
  }

  return chunk;
}

Object.defineProperty(Writable.prototype, 'writableHighWaterMark', {
  // making it explicit this property is not enumerable
  // because otherwise some prototype manipulation in
  // userland will fail
  enumerable: false,
  get: function get() {
    return this._writableState.highWaterMark;
  }
}); // if we're already writing something, then just put this
// in the queue, and wait our turn.  Otherwise, call _write
// If we return false, then we need a drain event, so set that flag.

function writeOrBuffer(stream, state, isBuf, chunk, encoding, cb) {
  if (!isBuf) {
    var newChunk = decodeChunk(state, chunk, encoding);

    if (chunk !== newChunk) {
      isBuf = true;
      encoding = 'buffer';
      chunk = newChunk;
    }
  }

  var len = state.objectMode ? 1 : chunk.length;
  state.length += len;
  var ret = state.length < state.highWaterMark; // we must ensure that previous needDrain will not be reset to false.

  if (!ret) state.needDrain = true;

  if (state.writing || state.corked) {
    var last = state.lastBufferedRequest;
    state.lastBufferedRequest = {
      chunk: chunk,
      encoding: encoding,
      isBuf: isBuf,
      callback: cb,
      next: null
    };

    if (last) {
      last.next = state.lastBufferedRequest;
    } else {
      state.bufferedRequest = state.lastBufferedRequest;
    }

    state.bufferedRequestCount += 1;
  } else {
    doWrite(stream, state, false, len, chunk, encoding, cb);
  }

  return ret;
}

function doWrite(stream, state, writev, len, chunk, encoding, cb) {
  state.writelen = len;
  state.writecb = cb;
  state.writing = true;
  state.sync = true;
  if (state.destroyed) state.onwrite(new ERR_STREAM_DESTROYED('write'));else if (writev) stream._writev(chunk, state.onwrite);else stream._write(chunk, encoding, state.onwrite);
  state.sync = false;
}

function onwriteError(stream, state, sync, er, cb) {
  --state.pendingcb;

  if (sync) {
    // defer the callback if we are being called synchronously
    // to avoid piling up things on the stack
    process.nextTick(cb, er); // this can emit finish, and it will always happen
    // after error

    process.nextTick(finishMaybe, stream, state);
    stream._writableState.errorEmitted = true;
    stream.emit('error', er);
  } else {
    // the caller expect this to happen before if
    // it is async
    cb(er);
    stream._writableState.errorEmitted = true;
    stream.emit('error', er); // this can emit finish, but finish must
    // always follow error

    finishMaybe(stream, state);
  }
}

function onwriteStateUpdate(state) {
  state.writing = false;
  state.writecb = null;
  state.length -= state.writelen;
  state.writelen = 0;
}

function onwrite(stream, er) {
  var state = stream._writableState;
  var sync = state.sync;
  var cb = state.writecb;
  if (typeof cb !== 'function') throw new ERR_MULTIPLE_CALLBACK();
  onwriteStateUpdate(state);
  if (er) onwriteError(stream, state, sync, er, cb);else {
    // Check if we're actually ready to finish, but don't emit yet
    var finished = needFinish(state) || stream.destroyed;

    if (!finished && !state.corked && !state.bufferProcessing && state.bufferedRequest) {
      clearBuffer(stream, state);
    }

    if (sync) {
      process.nextTick(afterWrite, stream, state, finished, cb);
    } else {
      afterWrite(stream, state, finished, cb);
    }
  }
}

function afterWrite(stream, state, finished, cb) {
  if (!finished) onwriteDrain(stream, state);
  state.pendingcb--;
  cb();
  finishMaybe(stream, state);
} // Must force callback to be called on nextTick, so that we don't
// emit 'drain' before the write() consumer gets the 'false' return
// value, and has a chance to attach a 'drain' listener.


function onwriteDrain(stream, state) {
  if (state.length === 0 && state.needDrain) {
    state.needDrain = false;
    stream.emit('drain');
  }
} // if there's something in the buffer waiting, then process it


function clearBuffer(stream, state) {
  state.bufferProcessing = true;
  var entry = state.bufferedRequest;

  if (stream._writev && entry && entry.next) {
    // Fast case, write everything using _writev()
    var l = state.bufferedRequestCount;
    var buffer = new Array(l);
    var holder = state.corkedRequestsFree;
    holder.entry = entry;
    var count = 0;
    var allBuffers = true;

    while (entry) {
      buffer[count] = entry;
      if (!entry.isBuf) allBuffers = false;
      entry = entry.next;
      count += 1;
    }

    buffer.allBuffers = allBuffers;
    doWrite(stream, state, true, state.length, buffer, '', holder.finish); // doWrite is almost always async, defer these to save a bit of time
    // as the hot path ends with doWrite

    state.pendingcb++;
    state.lastBufferedRequest = null;

    if (holder.next) {
      state.corkedRequestsFree = holder.next;
      holder.next = null;
    } else {
      state.corkedRequestsFree = new CorkedRequest(state);
    }

    state.bufferedRequestCount = 0;
  } else {
    // Slow case, write chunks one-by-one
    while (entry) {
      var chunk = entry.chunk;
      var encoding = entry.encoding;
      var cb = entry.callback;
      var len = state.objectMode ? 1 : chunk.length;
      doWrite(stream, state, false, len, chunk, encoding, cb);
      entry = entry.next;
      state.bufferedRequestCount--; // if we didn't call the onwrite immediately, then
      // it means that we need to wait until it does.
      // also, that means that the chunk and cb are currently
      // being processed, so move the buffer counter past them.

      if (state.writing) {
        break;
      }
    }

    if (entry === null) state.lastBufferedRequest = null;
  }

  state.bufferedRequest = entry;
  state.bufferProcessing = false;
}

Writable.prototype._write = function (chunk, encoding, cb) {
  cb(new ERR_METHOD_NOT_IMPLEMENTED('_write()'));
};

Writable.prototype._writev = null;

Writable.prototype.end = function (chunk, encoding, cb) {
  var state = this._writableState;

  if (typeof chunk === 'function') {
    cb = chunk;
    chunk = null;
    encoding = null;
  } else if (typeof encoding === 'function') {
    cb = encoding;
    encoding = null;
  }

  if (chunk !== null && chunk !== undefined) this.write(chunk, encoding); // .end() fully uncorks

  if (state.corked) {
    state.corked = 1;
    this.uncork();
  } // ignore unnecessary end() calls.


  if (!state.ending) endWritable(this, state, cb);
  return this;
};

Object.defineProperty(Writable.prototype, 'writableLength', {
  // making it explicit this property is not enumerable
  // because otherwise some prototype manipulation in
  // userland will fail
  enumerable: false,
  get: function get() {
    return this._writableState.length;
  }
});

function needFinish(state) {
  return state.ending && state.length === 0 && state.bufferedRequest === null && !state.finished && !state.writing;
}

function callFinal(stream, state) {
  stream._final(function (err) {
    state.pendingcb--;

    if (err) {
      stream.emit('error', err);
    }

    state.prefinished = true;
    stream.emit('prefinish');
    finishMaybe(stream, state);
  });
}

function prefinish(stream, state) {
  if (!state.prefinished && !state.finalCalled) {
    if (typeof stream._final === 'function' && !state.destroyed) {
      state.pendingcb++;
      state.finalCalled = true;
      process.nextTick(callFinal, stream, state);
    } else {
      state.prefinished = true;
      stream.emit('prefinish');
    }
  }
}

function finishMaybe(stream, state) {
  var need = needFinish(state);

  if (need) {
    prefinish(stream, state);

    if (state.pendingcb === 0) {
      state.finished = true;
      stream.emit('finish');
    }
  }

  return need;
}

function endWritable(stream, state, cb) {
  state.ending = true;
  finishMaybe(stream, state);

  if (cb) {
    if (state.finished) process.nextTick(cb);else stream.once('finish', cb);
  }

  state.ended = true;
  stream.writable = false;
}

function onCorkedFinish(corkReq, state, err) {
  var entry = corkReq.entry;
  corkReq.entry = null;

  while (entry) {
    var cb = entry.callback;
    state.pendingcb--;
    cb(err);
    entry = entry.next;
  } // reuse the free corkReq.


  state.corkedRequestsFree.next = corkReq;
}

Object.defineProperty(Writable.prototype, 'destroyed', {
  // making it explicit this property is not enumerable
  // because otherwise some prototype manipulation in
  // userland will fail
  enumerable: false,
  get: function get() {
    if (this._writableState === undefined) {
      return false;
    }

    return this._writableState.destroyed;
  },
  set: function set(value) {
    // we ignore the value if the stream
    // has not been initialized yet
    if (!this._writableState) {
      return;
    } // backward compatibility, the user is explicitly
    // managing destroyed


    this._writableState.destroyed = value;
  }
});
Writable.prototype.destroy = destroyImpl.destroy;
Writable.prototype._undestroy = destroyImpl.undestroy;

Writable.prototype._destroy = function (err, cb) {
  cb(err);
};
}).call(this,require('_process'),typeof global !== "undefined" ? global : typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{"../errors":8,"./_stream_duplex":10,"./internal/streams/destroy":17,"./internal/streams/state":19,"./internal/streams/stream":20,"_process":220,"buffer":215,"inherits":2,"util-deprecate":24}],15:[function(require,module,exports){
(function (process){
'use strict';

var _Object$setPrototypeO;

function _defineProperty(obj, key, value) { if (key in obj) { Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true }); } else { obj[key] = value; } return obj; }

var finished = require('./end-of-stream');

var kLastResolve = Symbol('lastResolve');
var kLastReject = Symbol('lastReject');
var kError = Symbol('error');
var kEnded = Symbol('ended');
var kLastPromise = Symbol('lastPromise');
var kHandlePromise = Symbol('handlePromise');
var kStream = Symbol('stream');

function createIterResult(value, done) {
  return {
    value: value,
    done: done
  };
}

function readAndResolve(iter) {
  var resolve = iter[kLastResolve];

  if (resolve !== null) {
    var data = iter[kStream].read(); // we defer if data is null
    // we can be expecting either 'end' or
    // 'error'

    if (data !== null) {
      iter[kLastPromise] = null;
      iter[kLastResolve] = null;
      iter[kLastReject] = null;
      resolve(createIterResult(data, false));
    }
  }
}

function onReadable(iter) {
  // we wait for the next tick, because it might
  // emit an error with process.nextTick
  process.nextTick(readAndResolve, iter);
}

function wrapForNext(lastPromise, iter) {
  return function (resolve, reject) {
    lastPromise.then(function () {
      iter[kHandlePromise](resolve, reject);
    }, reject);
  };
}

var AsyncIteratorPrototype = Object.getPrototypeOf(function () {});
var ReadableStreamAsyncIteratorPrototype = Object.setPrototypeOf((_Object$setPrototypeO = {
  get stream() {
    return this[kStream];
  },

  next: function next() {
    var _this = this;

    // if we have detected an error in the meanwhile
    // reject straight away
    var error = this[kError];

    if (error !== null) {
      return Promise.reject(error);
    }

    if (this[kEnded]) {
      return Promise.resolve(createIterResult(null, true));
    }

    if (this[kStream].destroyed) {
      // We need to defer via nextTick because if .destroy(err) is
      // called, the error will be emitted via nextTick, and
      // we cannot guarantee that there is no error lingering around
      // waiting to be emitted.
      return new Promise(function (resolve, reject) {
        process.nextTick(function () {
          if (_this[kError]) {
            reject(_this[kError]);
          } else {
            resolve(createIterResult(null, true));
          }
        });
      });
    } // if we have multiple next() calls
    // we will wait for the previous Promise to finish
    // this logic is optimized to support for await loops,
    // where next() is only called once at a time


    var lastPromise = this[kLastPromise];
    var promise;

    if (lastPromise) {
      promise = new Promise(wrapForNext(lastPromise, this));
    } else {
      // fast path needed to support multiple this.push()
      // without triggering the next() queue
      var data = this[kStream].read();

      if (data !== null) {
        return Promise.resolve(createIterResult(data, false));
      }

      promise = new Promise(this[kHandlePromise]);
    }

    this[kLastPromise] = promise;
    return promise;
  }
}, _defineProperty(_Object$setPrototypeO, Symbol.asyncIterator, function () {
  return this;
}), _defineProperty(_Object$setPrototypeO, "return", function _return() {
  var _this2 = this;

  // destroy(err, cb) is a private API
  // we can guarantee we have that here, because we control the
  // Readable class this is attached to
  return new Promise(function (resolve, reject) {
    _this2[kStream].destroy(null, function (err) {
      if (err) {
        reject(err);
        return;
      }

      resolve(createIterResult(null, true));
    });
  });
}), _Object$setPrototypeO), AsyncIteratorPrototype);

var createReadableStreamAsyncIterator = function createReadableStreamAsyncIterator(stream) {
  var _Object$create;

  var iterator = Object.create(ReadableStreamAsyncIteratorPrototype, (_Object$create = {}, _defineProperty(_Object$create, kStream, {
    value: stream,
    writable: true
  }), _defineProperty(_Object$create, kLastResolve, {
    value: null,
    writable: true
  }), _defineProperty(_Object$create, kLastReject, {
    value: null,
    writable: true
  }), _defineProperty(_Object$create, kError, {
    value: null,
    writable: true
  }), _defineProperty(_Object$create, kEnded, {
    value: stream._readableState.endEmitted,
    writable: true
  }), _defineProperty(_Object$create, kLastPromise, {
    value: null,
    writable: true
  }), _defineProperty(_Object$create, kHandlePromise, {
    value: function value(resolve, reject) {
      var data = iterator[kStream].read();

      if (data) {
        iterator[kLastPromise] = null;
        iterator[kLastResolve] = null;
        iterator[kLastReject] = null;
        resolve(createIterResult(data, false));
      } else {
        iterator[kLastResolve] = resolve;
        iterator[kLastReject] = reject;
      }
    },
    writable: true
  }), _Object$create));
  finished(stream, function (err) {
    if (err && err.code !== 'ERR_STREAM_PREMATURE_CLOSE') {
      var reject = iterator[kLastReject]; // reject if we are waiting for data in the Promise
      // returned by next() and store the error

      if (reject !== null) {
        iterator[kLastPromise] = null;
        iterator[kLastResolve] = null;
        iterator[kLastReject] = null;
        reject(err);
      }

      iterator[kError] = err;
      return;
    }

    var resolve = iterator[kLastResolve];

    if (resolve !== null) {
      iterator[kLastPromise] = null;
      iterator[kLastResolve] = null;
      iterator[kLastReject] = null;
      resolve(createIterResult(null, true));
    }

    iterator[kEnded] = true;
  });
  stream.on('readable', onReadable.bind(null, iterator));
  return iterator;
};

module.exports = createReadableStreamAsyncIterator;
}).call(this,require('_process'))
},{"./end-of-stream":18,"_process":220}],16:[function(require,module,exports){
'use strict';

function _objectSpread(target) { for (var i = 1; i < arguments.length; i++) { var source = arguments[i] != null ? arguments[i] : {}; var ownKeys = Object.keys(source); if (typeof Object.getOwnPropertySymbols === 'function') { ownKeys = ownKeys.concat(Object.getOwnPropertySymbols(source).filter(function (sym) { return Object.getOwnPropertyDescriptor(source, sym).enumerable; })); } ownKeys.forEach(function (key) { _defineProperty(target, key, source[key]); }); } return target; }

function _defineProperty(obj, key, value) { if (key in obj) { Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true }); } else { obj[key] = value; } return obj; }

var _require = require('buffer'),
    Buffer = _require.Buffer;

var _require2 = require('util'),
    inspect = _require2.inspect;

var custom = inspect && inspect.custom || 'inspect';

function copyBuffer(src, target, offset) {
  Buffer.prototype.copy.call(src, target, offset);
}

module.exports =
/*#__PURE__*/
function () {
  function BufferList() {
    this.head = null;
    this.tail = null;
    this.length = 0;
  }

  var _proto = BufferList.prototype;

  _proto.push = function push(v) {
    var entry = {
      data: v,
      next: null
    };
    if (this.length > 0) this.tail.next = entry;else this.head = entry;
    this.tail = entry;
    ++this.length;
  };

  _proto.unshift = function unshift(v) {
    var entry = {
      data: v,
      next: this.head
    };
    if (this.length === 0) this.tail = entry;
    this.head = entry;
    ++this.length;
  };

  _proto.shift = function shift() {
    if (this.length === 0) return;
    var ret = this.head.data;
    if (this.length === 1) this.head = this.tail = null;else this.head = this.head.next;
    --this.length;
    return ret;
  };

  _proto.clear = function clear() {
    this.head = this.tail = null;
    this.length = 0;
  };

  _proto.join = function join(s) {
    if (this.length === 0) return '';
    var p = this.head;
    var ret = '' + p.data;

    while (p = p.next) {
      ret += s + p.data;
    }

    return ret;
  };

  _proto.concat = function concat(n) {
    if (this.length === 0) return Buffer.alloc(0);
    var ret = Buffer.allocUnsafe(n >>> 0);
    var p = this.head;
    var i = 0;

    while (p) {
      copyBuffer(p.data, ret, i);
      i += p.data.length;
      p = p.next;
    }

    return ret;
  } // Consumes a specified amount of bytes or characters from the buffered data.
  ;

  _proto.consume = function consume(n, hasStrings) {
    var ret;

    if (n < this.head.data.length) {
      // `slice` is the same for buffers and strings.
      ret = this.head.data.slice(0, n);
      this.head.data = this.head.data.slice(n);
    } else if (n === this.head.data.length) {
      // First chunk is a perfect match.
      ret = this.shift();
    } else {
      // Result spans more than one buffer.
      ret = hasStrings ? this._getString(n) : this._getBuffer(n);
    }

    return ret;
  };

  _proto.first = function first() {
    return this.head.data;
  } // Consumes a specified amount of characters from the buffered data.
  ;

  _proto._getString = function _getString(n) {
    var p = this.head;
    var c = 1;
    var ret = p.data;
    n -= ret.length;

    while (p = p.next) {
      var str = p.data;
      var nb = n > str.length ? str.length : n;
      if (nb === str.length) ret += str;else ret += str.slice(0, n);
      n -= nb;

      if (n === 0) {
        if (nb === str.length) {
          ++c;
          if (p.next) this.head = p.next;else this.head = this.tail = null;
        } else {
          this.head = p;
          p.data = str.slice(nb);
        }

        break;
      }

      ++c;
    }

    this.length -= c;
    return ret;
  } // Consumes a specified amount of bytes from the buffered data.
  ;

  _proto._getBuffer = function _getBuffer(n) {
    var ret = Buffer.allocUnsafe(n);
    var p = this.head;
    var c = 1;
    p.data.copy(ret);
    n -= p.data.length;

    while (p = p.next) {
      var buf = p.data;
      var nb = n > buf.length ? buf.length : n;
      buf.copy(ret, ret.length - n, 0, nb);
      n -= nb;

      if (n === 0) {
        if (nb === buf.length) {
          ++c;
          if (p.next) this.head = p.next;else this.head = this.tail = null;
        } else {
          this.head = p;
          p.data = buf.slice(nb);
        }

        break;
      }

      ++c;
    }

    this.length -= c;
    return ret;
  } // Make sure the linked list only shows the minimal necessary information.
  ;

  _proto[custom] = function (_, options) {
    return inspect(this, _objectSpread({}, options, {
      // Only inspect one level.
      depth: 0,
      // It should not recurse.
      customInspect: false
    }));
  };

  return BufferList;
}();
},{"buffer":215,"util":214}],17:[function(require,module,exports){
(function (process){
'use strict'; // undocumented cb() API, needed for core, not for public API

function destroy(err, cb) {
  var _this = this;

  var readableDestroyed = this._readableState && this._readableState.destroyed;
  var writableDestroyed = this._writableState && this._writableState.destroyed;

  if (readableDestroyed || writableDestroyed) {
    if (cb) {
      cb(err);
    } else if (err && (!this._writableState || !this._writableState.errorEmitted)) {
      process.nextTick(emitErrorNT, this, err);
    }

    return this;
  } // we set destroyed to true before firing error callbacks in order
  // to make it re-entrance safe in case destroy() is called within callbacks


  if (this._readableState) {
    this._readableState.destroyed = true;
  } // if this is a duplex stream mark the writable part as destroyed as well


  if (this._writableState) {
    this._writableState.destroyed = true;
  }

  this._destroy(err || null, function (err) {
    if (!cb && err) {
      process.nextTick(emitErrorAndCloseNT, _this, err);

      if (_this._writableState) {
        _this._writableState.errorEmitted = true;
      }
    } else if (cb) {
      process.nextTick(emitCloseNT, _this);
      cb(err);
    } else {
      process.nextTick(emitCloseNT, _this);
    }
  });

  return this;
}

function emitErrorAndCloseNT(self, err) {
  emitErrorNT(self, err);
  emitCloseNT(self);
}

function emitCloseNT(self) {
  if (self._writableState && !self._writableState.emitClose) return;
  if (self._readableState && !self._readableState.emitClose) return;
  self.emit('close');
}

function undestroy() {
  if (this._readableState) {
    this._readableState.destroyed = false;
    this._readableState.reading = false;
    this._readableState.ended = false;
    this._readableState.endEmitted = false;
  }

  if (this._writableState) {
    this._writableState.destroyed = false;
    this._writableState.ended = false;
    this._writableState.ending = false;
    this._writableState.finalCalled = false;
    this._writableState.prefinished = false;
    this._writableState.finished = false;
    this._writableState.errorEmitted = false;
  }
}

function emitErrorNT(self, err) {
  self.emit('error', err);
}

module.exports = {
  destroy: destroy,
  undestroy: undestroy
};
}).call(this,require('_process'))
},{"_process":220}],18:[function(require,module,exports){
// Ported from https://github.com/mafintosh/end-of-stream with
// permission from the author, Mathias Buus (@mafintosh).
'use strict';

var ERR_STREAM_PREMATURE_CLOSE = require('../../../errors').codes.ERR_STREAM_PREMATURE_CLOSE;

function noop() {}

function isRequest(stream) {
  return stream.setHeader && typeof stream.abort === 'function';
}

function once(callback) {
  var called = false;
  return function (err) {
    if (called) return;
    called = true;
    callback.call(this, err);
  };
}

function eos(stream, opts, callback) {
  if (typeof opts === 'function') return eos(stream, null, opts);
  if (!opts) opts = {};
  callback = once(callback || noop);
  var ws = stream._writableState;
  var rs = stream._readableState;
  var readable = opts.readable || opts.readable !== false && stream.readable;
  var writable = opts.writable || opts.writable !== false && stream.writable;

  var onlegacyfinish = function onlegacyfinish() {
    if (!stream.writable) onfinish();
  };

  var onfinish = function onfinish() {
    writable = false;
    if (!readable) callback.call(stream);
  };

  var onend = function onend() {
    readable = false;
    if (!writable) callback.call(stream);
  };

  var onerror = function onerror(err) {
    callback.call(stream, err);
  };

  var onclose = function onclose() {
    if (readable && !(rs && rs.ended)) {
      return callback.call(stream, new ERR_STREAM_PREMATURE_CLOSE());
    }

    if (writable && !(ws && ws.ended)) {
      return callback.call(stream, new ERR_STREAM_PREMATURE_CLOSE());
    }
  };

  var onrequest = function onrequest() {
    stream.req.on('finish', onfinish);
  };

  if (isRequest(stream)) {
    stream.on('complete', onfinish);
    stream.on('abort', onclose);
    if (stream.req) onrequest();else stream.on('request', onrequest);
  } else if (writable && !ws) {
    // legacy streams
    stream.on('end', onlegacyfinish);
    stream.on('close', onlegacyfinish);
  }

  stream.on('end', onend);
  stream.on('finish', onfinish);
  if (opts.error !== false) stream.on('error', onerror);
  stream.on('close', onclose);
  return function () {
    stream.removeListener('complete', onfinish);
    stream.removeListener('abort', onclose);
    stream.removeListener('request', onrequest);
    if (stream.req) stream.req.removeListener('finish', onfinish);
    stream.removeListener('end', onlegacyfinish);
    stream.removeListener('close', onlegacyfinish);
    stream.removeListener('finish', onfinish);
    stream.removeListener('end', onend);
    stream.removeListener('error', onerror);
    stream.removeListener('close', onclose);
  };
}

module.exports = eos;
},{"../../../errors":8}],19:[function(require,module,exports){
'use strict';

var ERR_INVALID_OPT_VALUE = require('../../../errors').codes.ERR_INVALID_OPT_VALUE;

function highWaterMarkFrom(options, isDuplex, duplexKey) {
  return options.highWaterMark != null ? options.highWaterMark : isDuplex ? options[duplexKey] : null;
}

function getHighWaterMark(state, options, duplexKey, isDuplex) {
  var hwm = highWaterMarkFrom(options, isDuplex, duplexKey);

  if (hwm != null) {
    if (!(isFinite(hwm) && Math.floor(hwm) === hwm) || hwm < 0) {
      var name = isDuplex ? duplexKey : 'highWaterMark';
      throw new ERR_INVALID_OPT_VALUE(name, hwm);
    }

    return Math.floor(hwm);
  } // Default value


  return state.objectMode ? 16 : 16 * 1024;
}

module.exports = {
  getHighWaterMark: getHighWaterMark
};
},{"../../../errors":8}],20:[function(require,module,exports){
module.exports = require('events').EventEmitter;

},{"events":216}],21:[function(require,module,exports){
exports = module.exports = require('./lib/_stream_readable.js');
exports.Stream = exports;
exports.Readable = exports;
exports.Writable = require('./lib/_stream_writable.js');
exports.Duplex = require('./lib/_stream_duplex.js');
exports.Transform = require('./lib/_stream_transform.js');
exports.PassThrough = require('./lib/_stream_passthrough.js');

},{"./lib/_stream_duplex.js":10,"./lib/_stream_passthrough.js":11,"./lib/_stream_readable.js":12,"./lib/_stream_transform.js":13,"./lib/_stream_writable.js":14}],22:[function(require,module,exports){
/* eslint-disable node/no-deprecated-api */
var buffer = require('buffer')
var Buffer = buffer.Buffer

// alternative to using Object.keys for old browsers
function copyProps (src, dst) {
  for (var key in src) {
    dst[key] = src[key]
  }
}
if (Buffer.from && Buffer.alloc && Buffer.allocUnsafe && Buffer.allocUnsafeSlow) {
  module.exports = buffer
} else {
  // Copy properties from require('buffer')
  copyProps(buffer, exports)
  exports.Buffer = SafeBuffer
}

function SafeBuffer (arg, encodingOrOffset, length) {
  return Buffer(arg, encodingOrOffset, length)
}

// Copy static methods from Buffer
copyProps(Buffer, SafeBuffer)

SafeBuffer.from = function (arg, encodingOrOffset, length) {
  if (typeof arg === 'number') {
    throw new TypeError('Argument must not be a number')
  }
  return Buffer(arg, encodingOrOffset, length)
}

SafeBuffer.alloc = function (size, fill, encoding) {
  if (typeof size !== 'number') {
    throw new TypeError('Argument must be a number')
  }
  var buf = Buffer(size)
  if (fill !== undefined) {
    if (typeof encoding === 'string') {
      buf.fill(fill, encoding)
    } else {
      buf.fill(fill)
    }
  } else {
    buf.fill(0)
  }
  return buf
}

SafeBuffer.allocUnsafe = function (size) {
  if (typeof size !== 'number') {
    throw new TypeError('Argument must be a number')
  }
  return Buffer(size)
}

SafeBuffer.allocUnsafeSlow = function (size) {
  if (typeof size !== 'number') {
    throw new TypeError('Argument must be a number')
  }
  return buffer.SlowBuffer(size)
}

},{"buffer":215}],23:[function(require,module,exports){
// Copyright Joyent, Inc. and other Node contributors.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit
// persons to whom the Software is furnished to do so, subject to the
// following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
// USE OR OTHER DEALINGS IN THE SOFTWARE.

'use strict';

/*<replacement>*/

var Buffer = require('safe-buffer').Buffer;
/*</replacement>*/

var isEncoding = Buffer.isEncoding || function (encoding) {
  encoding = '' + encoding;
  switch (encoding && encoding.toLowerCase()) {
    case 'hex':case 'utf8':case 'utf-8':case 'ascii':case 'binary':case 'base64':case 'ucs2':case 'ucs-2':case 'utf16le':case 'utf-16le':case 'raw':
      return true;
    default:
      return false;
  }
};

function _normalizeEncoding(enc) {
  if (!enc) return 'utf8';
  var retried;
  while (true) {
    switch (enc) {
      case 'utf8':
      case 'utf-8':
        return 'utf8';
      case 'ucs2':
      case 'ucs-2':
      case 'utf16le':
      case 'utf-16le':
        return 'utf16le';
      case 'latin1':
      case 'binary':
        return 'latin1';
      case 'base64':
      case 'ascii':
      case 'hex':
        return enc;
      default:
        if (retried) return; // undefined
        enc = ('' + enc).toLowerCase();
        retried = true;
    }
  }
};

// Do not cache `Buffer.isEncoding` when checking encoding names as some
// modules monkey-patch it to support additional encodings
function normalizeEncoding(enc) {
  var nenc = _normalizeEncoding(enc);
  if (typeof nenc !== 'string' && (Buffer.isEncoding === isEncoding || !isEncoding(enc))) throw new Error('Unknown encoding: ' + enc);
  return nenc || enc;
}

// StringDecoder provides an interface for efficiently splitting a series of
// buffers into a series of JS strings without breaking apart multi-byte
// characters.
exports.StringDecoder = StringDecoder;
function StringDecoder(encoding) {
  this.encoding = normalizeEncoding(encoding);
  var nb;
  switch (this.encoding) {
    case 'utf16le':
      this.text = utf16Text;
      this.end = utf16End;
      nb = 4;
      break;
    case 'utf8':
      this.fillLast = utf8FillLast;
      nb = 4;
      break;
    case 'base64':
      this.text = base64Text;
      this.end = base64End;
      nb = 3;
      break;
    default:
      this.write = simpleWrite;
      this.end = simpleEnd;
      return;
  }
  this.lastNeed = 0;
  this.lastTotal = 0;
  this.lastChar = Buffer.allocUnsafe(nb);
}

StringDecoder.prototype.write = function (buf) {
  if (buf.length === 0) return '';
  var r;
  var i;
  if (this.lastNeed) {
    r = this.fillLast(buf);
    if (r === undefined) return '';
    i = this.lastNeed;
    this.lastNeed = 0;
  } else {
    i = 0;
  }
  if (i < buf.length) return r ? r + this.text(buf, i) : this.text(buf, i);
  return r || '';
};

StringDecoder.prototype.end = utf8End;

// Returns only complete characters in a Buffer
StringDecoder.prototype.text = utf8Text;

// Attempts to complete a partial non-UTF-8 character using bytes from a Buffer
StringDecoder.prototype.fillLast = function (buf) {
  if (this.lastNeed <= buf.length) {
    buf.copy(this.lastChar, this.lastTotal - this.lastNeed, 0, this.lastNeed);
    return this.lastChar.toString(this.encoding, 0, this.lastTotal);
  }
  buf.copy(this.lastChar, this.lastTotal - this.lastNeed, 0, buf.length);
  this.lastNeed -= buf.length;
};

// Checks the type of a UTF-8 byte, whether it's ASCII, a leading byte, or a
// continuation byte. If an invalid byte is detected, -2 is returned.
function utf8CheckByte(byte) {
  if (byte <= 0x7F) return 0;else if (byte >> 5 === 0x06) return 2;else if (byte >> 4 === 0x0E) return 3;else if (byte >> 3 === 0x1E) return 4;
  return byte >> 6 === 0x02 ? -1 : -2;
}

// Checks at most 3 bytes at the end of a Buffer in order to detect an
// incomplete multi-byte UTF-8 character. The total number of bytes (2, 3, or 4)
// needed to complete the UTF-8 character (if applicable) are returned.
function utf8CheckIncomplete(self, buf, i) {
  var j = buf.length - 1;
  if (j < i) return 0;
  var nb = utf8CheckByte(buf[j]);
  if (nb >= 0) {
    if (nb > 0) self.lastNeed = nb - 1;
    return nb;
  }
  if (--j < i || nb === -2) return 0;
  nb = utf8CheckByte(buf[j]);
  if (nb >= 0) {
    if (nb > 0) self.lastNeed = nb - 2;
    return nb;
  }
  if (--j < i || nb === -2) return 0;
  nb = utf8CheckByte(buf[j]);
  if (nb >= 0) {
    if (nb > 0) {
      if (nb === 2) nb = 0;else self.lastNeed = nb - 3;
    }
    return nb;
  }
  return 0;
}

// Validates as many continuation bytes for a multi-byte UTF-8 character as
// needed or are available. If we see a non-continuation byte where we expect
// one, we "replace" the validated continuation bytes we've seen so far with
// a single UTF-8 replacement character ('\ufffd'), to match v8's UTF-8 decoding
// behavior. The continuation byte check is included three times in the case
// where all of the continuation bytes for a character exist in the same buffer.
// It is also done this way as a slight performance increase instead of using a
// loop.
function utf8CheckExtraBytes(self, buf, p) {
  if ((buf[0] & 0xC0) !== 0x80) {
    self.lastNeed = 0;
    return '\ufffd';
  }
  if (self.lastNeed > 1 && buf.length > 1) {
    if ((buf[1] & 0xC0) !== 0x80) {
      self.lastNeed = 1;
      return '\ufffd';
    }
    if (self.lastNeed > 2 && buf.length > 2) {
      if ((buf[2] & 0xC0) !== 0x80) {
        self.lastNeed = 2;
        return '\ufffd';
      }
    }
  }
}

// Attempts to complete a multi-byte UTF-8 character using bytes from a Buffer.
function utf8FillLast(buf) {
  var p = this.lastTotal - this.lastNeed;
  var r = utf8CheckExtraBytes(this, buf, p);
  if (r !== undefined) return r;
  if (this.lastNeed <= buf.length) {
    buf.copy(this.lastChar, p, 0, this.lastNeed);
    return this.lastChar.toString(this.encoding, 0, this.lastTotal);
  }
  buf.copy(this.lastChar, p, 0, buf.length);
  this.lastNeed -= buf.length;
}

// Returns all complete UTF-8 characters in a Buffer. If the Buffer ended on a
// partial character, the character's bytes are buffered until the required
// number of bytes are available.
function utf8Text(buf, i) {
  var total = utf8CheckIncomplete(this, buf, i);
  if (!this.lastNeed) return buf.toString('utf8', i);
  this.lastTotal = total;
  var end = buf.length - (total - this.lastNeed);
  buf.copy(this.lastChar, 0, end);
  return buf.toString('utf8', i, end);
}

// For UTF-8, a replacement character is added when ending on a partial
// character.
function utf8End(buf) {
  var r = buf && buf.length ? this.write(buf) : '';
  if (this.lastNeed) return r + '\ufffd';
  return r;
}

// UTF-16LE typically needs two bytes per character, but even if we have an even
// number of bytes available, we need to check if we end on a leading/high
// surrogate. In that case, we need to wait for the next two bytes in order to
// decode the last character properly.
function utf16Text(buf, i) {
  if ((buf.length - i) % 2 === 0) {
    var r = buf.toString('utf16le', i);
    if (r) {
      var c = r.charCodeAt(r.length - 1);
      if (c >= 0xD800 && c <= 0xDBFF) {
        this.lastNeed = 2;
        this.lastTotal = 4;
        this.lastChar[0] = buf[buf.length - 2];
        this.lastChar[1] = buf[buf.length - 1];
        return r.slice(0, -1);
      }
    }
    return r;
  }
  this.lastNeed = 1;
  this.lastTotal = 2;
  this.lastChar[0] = buf[buf.length - 1];
  return buf.toString('utf16le', i, buf.length - 1);
}

// For UTF-16LE we do not explicitly append special replacement characters if we
// end on a partial character, we simply let v8 handle that.
function utf16End(buf) {
  var r = buf && buf.length ? this.write(buf) : '';
  if (this.lastNeed) {
    var end = this.lastTotal - this.lastNeed;
    return r + this.lastChar.toString('utf16le', 0, end);
  }
  return r;
}

function base64Text(buf, i) {
  var n = (buf.length - i) % 3;
  if (n === 0) return buf.toString('base64', i);
  this.lastNeed = 3 - n;
  this.lastTotal = 3;
  if (n === 1) {
    this.lastChar[0] = buf[buf.length - 1];
  } else {
    this.lastChar[0] = buf[buf.length - 2];
    this.lastChar[1] = buf[buf.length - 1];
  }
  return buf.toString('base64', i, buf.length - n);
}

function base64End(buf) {
  var r = buf && buf.length ? this.write(buf) : '';
  if (this.lastNeed) return r + this.lastChar.toString('base64', 0, 3 - this.lastNeed);
  return r;
}

// Pass bytes on through for single-byte encodings (e.g. ascii, latin1, hex)
function simpleWrite(buf) {
  return buf.toString(this.encoding);
}

function simpleEnd(buf) {
  return buf && buf.length ? this.write(buf) : '';
}
},{"safe-buffer":22}],24:[function(require,module,exports){
(function (global){

/**
 * Module exports.
 */

module.exports = deprecate;

/**
 * Mark that a method should not be used.
 * Returns a modified function which warns once by default.
 *
 * If `localStorage.noDeprecation = true` is set, then it is a no-op.
 *
 * If `localStorage.throwDeprecation = true` is set, then deprecated functions
 * will throw an Error when invoked.
 *
 * If `localStorage.traceDeprecation = true` is set, then deprecated functions
 * will invoke `console.trace()` instead of `console.error()`.
 *
 * @param {Function} fn - the function to deprecate
 * @param {String} msg - the string to print to the console when `fn` is invoked
 * @returns {Function} a new "deprecated" version of `fn`
 * @api public
 */

function deprecate (fn, msg) {
  if (config('noDeprecation')) {
    return fn;
  }

  var warned = false;
  function deprecated() {
    if (!warned) {
      if (config('throwDeprecation')) {
        throw new Error(msg);
      } else if (config('traceDeprecation')) {
        console.trace(msg);
      } else {
        console.warn(msg);
      }
      warned = true;
    }
    return fn.apply(this, arguments);
  }

  return deprecated;
}

/**
 * Checks `localStorage` for boolean values for the given `name`.
 *
 * @param {String} name
 * @returns {Boolean}
 * @api private
 */

function config (name) {
  // accessing global.localStorage can trigger a DOMException in sandboxed iframes
  try {
    if (!global.localStorage) return false;
  } catch (_) {
    return false;
  }
  var val = global.localStorage[name];
  if (null == val) return false;
  return String(val).toLowerCase() === 'true';
}

}).call(this,typeof global !== "undefined" ? global : typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{}],25:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Data_Functor = require("../Data.Functor/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Alt = function (Functor0, alt) {
    this.Functor0 = Functor0;
    this.alt = alt;
};
var altArray = new Alt(function () {
    return Data_Functor.functorArray;
}, Data_Semigroup.append(Data_Semigroup.semigroupArray));
var alt = function (dict) {
    return dict.alt;
};
module.exports = {
    Alt: Alt,
    alt: alt,
    altArray: altArray
};

},{"../Data.Functor/index.js":95,"../Data.Semigroup/index.js":139}],26:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Alt = require("../Control.Alt/index.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Plus = require("../Control.Plus/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Alternative = function (Applicative0, Plus1) {
    this.Applicative0 = Applicative0;
    this.Plus1 = Plus1;
};
var alternativeArray = new Alternative(function () {
    return Control_Applicative.applicativeArray;
}, function () {
    return Control_Plus.plusArray;
});
module.exports = {
    Alternative: Alternative,
    alternativeArray: alternativeArray
};

},{"../Control.Alt/index.js":25,"../Control.Applicative/index.js":27,"../Control.Apply/index.js":29,"../Control.Plus/index.js":55,"../Data.Functor/index.js":95}],27:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Apply = require("../Control.Apply/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Applicative = function (Apply0, pure) {
    this.Apply0 = Apply0;
    this.pure = pure;
};
var pure = function (dict) {
    return dict.pure;
};
var unless = function (dictApplicative) {
    return function (v) {
        return function (v1) {
            if (!v) {
                return v1;
            };
            if (v) {
                return pure(dictApplicative)(Data_Unit.unit);
            };
            throw new Error("Failed pattern match at Control.Applicative (line 62, column 1 - line 62, column 65): " + [ v.constructor.name, v1.constructor.name ]);
        };
    };
};
var when = function (dictApplicative) {
    return function (v) {
        return function (v1) {
            if (v) {
                return v1;
            };
            if (!v) {
                return pure(dictApplicative)(Data_Unit.unit);
            };
            throw new Error("Failed pattern match at Control.Applicative (line 57, column 1 - line 57, column 63): " + [ v.constructor.name, v1.constructor.name ]);
        };
    };
};
var liftA1 = function (dictApplicative) {
    return function (f) {
        return function (a) {
            return Control_Apply.apply(dictApplicative.Apply0())(pure(dictApplicative)(f))(a);
        };
    };
};
var applicativeFn = new Applicative(function () {
    return Control_Apply.applyFn;
}, function (x) {
    return function (v) {
        return x;
    };
});
var applicativeArray = new Applicative(function () {
    return Control_Apply.applyArray;
}, function (x) {
    return [ x ];
});
module.exports = {
    Applicative: Applicative,
    pure: pure,
    liftA1: liftA1,
    unless: unless,
    when: when,
    applicativeFn: applicativeFn,
    applicativeArray: applicativeArray
};

},{"../Control.Apply/index.js":29,"../Data.Functor/index.js":95,"../Data.Unit/index.js":162}],28:[function(require,module,exports){
"use strict";

exports.arrayApply = function (fs) {
  return function (xs) {
    var l = fs.length;
    var k = xs.length;
    var result = new Array(l*k);
    var n = 0;
    for (var i = 0; i < l; i++) {
      var f = fs[i];
      for (var j = 0; j < k; j++) {
        result[n++] = f(xs[j]);
      }
    }
    return result;
  };
};

},{}],29:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var $foreign = require("./foreign.js");
var Control_Category = require("../Control.Category/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Apply = function (Functor0, apply) {
    this.Functor0 = Functor0;
    this.apply = apply;
};
var applyFn = new Apply(function () {
    return Data_Functor.functorFn;
}, function (f) {
    return function (g) {
        return function (x) {
            return f(x)(g(x));
        };
    };
});
var applyArray = new Apply(function () {
    return Data_Functor.functorArray;
}, $foreign.arrayApply);
var apply = function (dict) {
    return dict.apply;
};
var applyFirst = function (dictApply) {
    return function (a) {
        return function (b) {
            return apply(dictApply)(Data_Functor.map(dictApply.Functor0())(Data_Function["const"])(a))(b);
        };
    };
};
var applySecond = function (dictApply) {
    return function (a) {
        return function (b) {
            return apply(dictApply)(Data_Functor.map(dictApply.Functor0())(Data_Function["const"](Control_Category.identity(Control_Category.categoryFn)))(a))(b);
        };
    };
};
var lift2 = function (dictApply) {
    return function (f) {
        return function (a) {
            return function (b) {
                return apply(dictApply)(Data_Functor.map(dictApply.Functor0())(f)(a))(b);
            };
        };
    };
};
var lift3 = function (dictApply) {
    return function (f) {
        return function (a) {
            return function (b) {
                return function (c) {
                    return apply(dictApply)(apply(dictApply)(Data_Functor.map(dictApply.Functor0())(f)(a))(b))(c);
                };
            };
        };
    };
};
var lift4 = function (dictApply) {
    return function (f) {
        return function (a) {
            return function (b) {
                return function (c) {
                    return function (d) {
                        return apply(dictApply)(apply(dictApply)(apply(dictApply)(Data_Functor.map(dictApply.Functor0())(f)(a))(b))(c))(d);
                    };
                };
            };
        };
    };
};
var lift5 = function (dictApply) {
    return function (f) {
        return function (a) {
            return function (b) {
                return function (c) {
                    return function (d) {
                        return function (e) {
                            return apply(dictApply)(apply(dictApply)(apply(dictApply)(apply(dictApply)(Data_Functor.map(dictApply.Functor0())(f)(a))(b))(c))(d))(e);
                        };
                    };
                };
            };
        };
    };
};
module.exports = {
    Apply: Apply,
    apply: apply,
    applyFirst: applyFirst,
    applySecond: applySecond,
    lift2: lift2,
    lift3: lift3,
    lift4: lift4,
    lift5: lift5,
    applyFn: applyFn,
    applyArray: applyArray
};

},{"../Control.Category/index.js":34,"../Data.Function/index.js":91,"../Data.Functor/index.js":95,"./foreign.js":28}],30:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Biapply = require("../Control.Biapply/index.js");
var Biapplicative = function (Biapply0, bipure) {
    this.Biapply0 = Biapply0;
    this.bipure = bipure;
};
var bipure = function (dict) {
    return dict.bipure;
};
module.exports = {
    bipure: bipure,
    Biapplicative: Biapplicative
};

},{"../Control.Biapply/index.js":31}],31:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Category = require("../Control.Category/index.js");
var Data_Bifunctor = require("../Data.Bifunctor/index.js");
var Data_Function = require("../Data.Function/index.js");
var Biapply = function (Bifunctor0, biapply) {
    this.Bifunctor0 = Bifunctor0;
    this.biapply = biapply;
};
var biapply = function (dict) {
    return dict.biapply;
};
var biapplyFirst = function (dictBiapply) {
    return function (a) {
        return function (b) {
            return biapply(dictBiapply)(Control_Category.identity(Control_Category.categoryFn)(Data_Bifunctor.bimap(dictBiapply.Bifunctor0())(Data_Function["const"](Control_Category.identity(Control_Category.categoryFn)))(Data_Function["const"](Control_Category.identity(Control_Category.categoryFn))))(a))(b);
        };
    };
};
var biapplySecond = function (dictBiapply) {
    return function (a) {
        return function (b) {
            return biapply(dictBiapply)(Control_Category.identity(Control_Category.categoryFn)(Data_Bifunctor.bimap(dictBiapply.Bifunctor0())(Data_Function["const"])(Data_Function["const"]))(a))(b);
        };
    };
};
var bilift2 = function (dictBiapply) {
    return function (f) {
        return function (g) {
            return function (a) {
                return function (b) {
                    return biapply(dictBiapply)(Control_Category.identity(Control_Category.categoryFn)(Data_Bifunctor.bimap(dictBiapply.Bifunctor0())(f)(g))(a))(b);
                };
            };
        };
    };
};
var bilift3 = function (dictBiapply) {
    return function (f) {
        return function (g) {
            return function (a) {
                return function (b) {
                    return function (c) {
                        return biapply(dictBiapply)(biapply(dictBiapply)(Control_Category.identity(Control_Category.categoryFn)(Data_Bifunctor.bimap(dictBiapply.Bifunctor0())(f)(g))(a))(b))(c);
                    };
                };
            };
        };
    };
};
module.exports = {
    biapply: biapply,
    Biapply: Biapply,
    biapplyFirst: biapplyFirst,
    biapplySecond: biapplySecond,
    bilift2: bilift2,
    bilift3: bilift3
};

},{"../Control.Category/index.js":34,"../Data.Bifunctor/index.js":71,"../Data.Function/index.js":91}],32:[function(require,module,exports){
"use strict";

exports.arrayBind = function (arr) {
  return function (f) {
    var result = [];
    for (var i = 0, l = arr.length; i < l; i++) {
      Array.prototype.push.apply(result, f(arr[i]));
    }
    return result;
  };
};

},{}],33:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var $foreign = require("./foreign.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Category = require("../Control.Category/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Bind = function (Apply0, bind) {
    this.Apply0 = Apply0;
    this.bind = bind;
};
var Discard = function (discard) {
    this.discard = discard;
};
var discard = function (dict) {
    return dict.discard;
};
var bindFn = new Bind(function () {
    return Control_Apply.applyFn;
}, function (m) {
    return function (f) {
        return function (x) {
            return f(m(x))(x);
        };
    };
});
var bindArray = new Bind(function () {
    return Control_Apply.applyArray;
}, $foreign.arrayBind);
var bind = function (dict) {
    return dict.bind;
};
var bindFlipped = function (dictBind) {
    return Data_Function.flip(bind(dictBind));
};
var composeKleisliFlipped = function (dictBind) {
    return function (f) {
        return function (g) {
            return function (a) {
                return bindFlipped(dictBind)(f)(g(a));
            };
        };
    };
};
var composeKleisli = function (dictBind) {
    return function (f) {
        return function (g) {
            return function (a) {
                return bind(dictBind)(f(a))(g);
            };
        };
    };
};
var discardUnit = new Discard(function (dictBind) {
    return bind(dictBind);
});
var ifM = function (dictBind) {
    return function (cond) {
        return function (t) {
            return function (f) {
                return bind(dictBind)(cond)(function (cond$prime) {
                    if (cond$prime) {
                        return t;
                    };
                    return f;
                });
            };
        };
    };
};
var join = function (dictBind) {
    return function (m) {
        return bind(dictBind)(m)(Control_Category.identity(Control_Category.categoryFn));
    };
};
module.exports = {
    Bind: Bind,
    bind: bind,
    bindFlipped: bindFlipped,
    Discard: Discard,
    discard: discard,
    join: join,
    composeKleisli: composeKleisli,
    composeKleisliFlipped: composeKleisliFlipped,
    ifM: ifM,
    bindFn: bindFn,
    bindArray: bindArray,
    discardUnit: discardUnit
};

},{"../Control.Applicative/index.js":27,"../Control.Apply/index.js":29,"../Control.Category/index.js":34,"../Data.Function/index.js":91,"../Data.Functor/index.js":95,"../Data.Unit/index.js":162,"./foreign.js":32}],34:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Semigroupoid = require("../Control.Semigroupoid/index.js");
var Category = function (Semigroupoid0, identity) {
    this.Semigroupoid0 = Semigroupoid0;
    this.identity = identity;
};
var identity = function (dict) {
    return dict.identity;
};
var categoryFn = new Category(function () {
    return Control_Semigroupoid.semigroupoidFn;
}, function (x) {
    return x;
});
module.exports = {
    Category: Category,
    identity: identity,
    categoryFn: categoryFn
};

},{"../Control.Semigroupoid/index.js":56}],35:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Extend = require("../Control.Extend/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Comonad = function (Extend0, extract) {
    this.Extend0 = Extend0;
    this.extract = extract;
};
var extract = function (dict) {
    return dict.extract;
};
module.exports = {
    Comonad: Comonad,
    extract: extract
};

},{"../Control.Extend/index.js":37,"../Data.Functor/index.js":95}],36:[function(require,module,exports){
"use strict";

exports.arrayExtend = function(f) {
  return function(xs) {
    return xs.map(function (_, i, xs) {
      return f(xs.slice(i));
    });
  };
};

},{}],37:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var $foreign = require("./foreign.js");
var Control_Category = require("../Control.Category/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Extend = function (Functor0, extend) {
    this.Functor0 = Functor0;
    this.extend = extend;
};
var extendFn = function (dictSemigroup) {
    return new Extend(function () {
        return Data_Functor.functorFn;
    }, function (f) {
        return function (g) {
            return function (w) {
                return f(function (w$prime) {
                    return g(Data_Semigroup.append(dictSemigroup)(w)(w$prime));
                });
            };
        };
    });
};
var extendArray = new Extend(function () {
    return Data_Functor.functorArray;
}, $foreign.arrayExtend);
var extend = function (dict) {
    return dict.extend;
};
var extendFlipped = function (dictExtend) {
    return function (w) {
        return function (f) {
            return extend(dictExtend)(f)(w);
        };
    };
};
var duplicate = function (dictExtend) {
    return extend(dictExtend)(Control_Category.identity(Control_Category.categoryFn));
};
var composeCoKleisliFlipped = function (dictExtend) {
    return function (f) {
        return function (g) {
            return function (w) {
                return f(extend(dictExtend)(g)(w));
            };
        };
    };
};
var composeCoKleisli = function (dictExtend) {
    return function (f) {
        return function (g) {
            return function (w) {
                return g(extend(dictExtend)(f)(w));
            };
        };
    };
};
module.exports = {
    Extend: Extend,
    extend: extend,
    extendFlipped: extendFlipped,
    composeCoKleisli: composeCoKleisli,
    composeCoKleisliFlipped: composeCoKleisliFlipped,
    duplicate: duplicate,
    extendFn: extendFn,
    extendArray: extendArray
};

},{"../Control.Category/index.js":34,"../Data.Functor/index.js":95,"../Data.Semigroup/index.js":139,"./foreign.js":36}],38:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Data_Unit = require("../Data.Unit/index.js");
var Lazy = function (defer) {
    this.defer = defer;
};
var lazyUnit = new Lazy(function (v) {
    return Data_Unit.unit;
});
var lazyFn = new Lazy(function (f) {
    return function (x) {
        return f(Data_Unit.unit)(x);
    };
});
var defer = function (dict) {
    return dict.defer;
};
var fix = function (dictLazy) {
    return function (f) {
        var go = defer(dictLazy)(function (v) {
            return f(go);
        });
        return go;
    };
};
module.exports = {
    defer: defer,
    Lazy: Lazy,
    fix: fix,
    lazyFn: lazyFn,
    lazyUnit: lazyUnit
};

},{"../Data.Unit/index.js":162}],39:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Prelude = require("../Prelude/index.js");
var MonadCont = function (Monad0, callCC) {
    this.Monad0 = Monad0;
    this.callCC = callCC;
};
var callCC = function (dict) {
    return dict.callCC;
};
module.exports = {
    MonadCont: MonadCont,
    callCC: callCC
};

},{"../Prelude/index.js":198}],40:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Semigroupoid = require("../Control.Semigroupoid/index.js");
var Data_Either = require("../Data.Either/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Effect = require("../Effect/index.js");
var Effect_Exception = require("../Effect.Exception/index.js");
var Prelude = require("../Prelude/index.js");
var MonadThrow = function (Monad0, throwError) {
    this.Monad0 = Monad0;
    this.throwError = throwError;
};
var MonadError = function (MonadThrow0, catchError) {
    this.MonadThrow0 = MonadThrow0;
    this.catchError = catchError;
};
var throwError = function (dict) {
    return dict.throwError;
};
var monadThrowMaybe = new MonadThrow(function () {
    return Data_Maybe.monadMaybe;
}, Data_Function["const"](Data_Maybe.Nothing.value));
var monadThrowEither = new MonadThrow(function () {
    return Data_Either.monadEither;
}, Data_Either.Left.create);
var monadThrowEffect = new MonadThrow(function () {
    return Effect.monadEffect;
}, Effect_Exception.throwException);
var monadErrorMaybe = new MonadError(function () {
    return monadThrowMaybe;
}, function (v) {
    return function (v1) {
        if (v instanceof Data_Maybe.Nothing) {
            return v1(Data_Unit.unit);
        };
        if (v instanceof Data_Maybe.Just) {
            return new Data_Maybe.Just(v.value0);
        };
        throw new Error("Failed pattern match at Control.Monad.Error.Class (line 79, column 1 - line 79, column 50): " + [ v.constructor.name, v1.constructor.name ]);
    };
});
var monadErrorEither = new MonadError(function () {
    return monadThrowEither;
}, function (v) {
    return function (v1) {
        if (v instanceof Data_Either.Left) {
            return v1(v.value0);
        };
        if (v instanceof Data_Either.Right) {
            return new Data_Either.Right(v.value0);
        };
        throw new Error("Failed pattern match at Control.Monad.Error.Class (line 72, column 1 - line 72, column 53): " + [ v.constructor.name, v1.constructor.name ]);
    };
});
var monadErrorEffect = new MonadError(function () {
    return monadThrowEffect;
}, Data_Function.flip(Effect_Exception.catchException));
var catchError = function (dict) {
    return dict.catchError;
};
var catchJust = function (dictMonadError) {
    return function (p) {
        return function (act) {
            return function (handler) {
                var handle = function (e) {
                    var v = p(e);
                    if (v instanceof Data_Maybe.Nothing) {
                        return throwError(dictMonadError.MonadThrow0())(e);
                    };
                    if (v instanceof Data_Maybe.Just) {
                        return handler(v.value0);
                    };
                    throw new Error("Failed pattern match at Control.Monad.Error.Class (line 57, column 5 - line 59, column 26): " + [ v.constructor.name ]);
                };
                return catchError(dictMonadError)(act)(handle);
            };
        };
    };
};
var $$try = function (dictMonadError) {
    return function (a) {
        return catchError(dictMonadError)(Data_Functor.map(((((dictMonadError.MonadThrow0()).Monad0()).Bind1()).Apply0()).Functor0())(Data_Either.Right.create)(a))(function ($21) {
            return Control_Applicative.pure(((dictMonadError.MonadThrow0()).Monad0()).Applicative0())(Data_Either.Left.create($21));
        });
    };
};
var withResource = function (dictMonadError) {
    return function (acquire) {
        return function (release) {
            return function (kleisli) {
                return Control_Bind.bind(((dictMonadError.MonadThrow0()).Monad0()).Bind1())(acquire)(function (v) {
                    return Control_Bind.bind(((dictMonadError.MonadThrow0()).Monad0()).Bind1())($$try(dictMonadError)(kleisli(v)))(function (v1) {
                        return Control_Bind.discard(Control_Bind.discardUnit)(((dictMonadError.MonadThrow0()).Monad0()).Bind1())(release(v))(function () {
                            return Data_Either.either(throwError(dictMonadError.MonadThrow0()))(Control_Applicative.pure(((dictMonadError.MonadThrow0()).Monad0()).Applicative0()))(v1);
                        });
                    });
                });
            };
        };
    };
};
module.exports = {
    catchError: catchError,
    throwError: throwError,
    MonadThrow: MonadThrow,
    MonadError: MonadError,
    catchJust: catchJust,
    "try": $$try,
    withResource: withResource,
    monadThrowEither: monadThrowEither,
    monadErrorEither: monadErrorEither,
    monadThrowMaybe: monadThrowMaybe,
    monadErrorMaybe: monadErrorMaybe,
    monadThrowEffect: monadThrowEffect,
    monadErrorEffect: monadErrorEffect
};

},{"../Control.Applicative/index.js":27,"../Control.Bind/index.js":33,"../Control.Semigroupoid/index.js":56,"../Data.Either/index.js":80,"../Data.Function/index.js":91,"../Data.Functor/index.js":95,"../Data.Maybe/index.js":110,"../Data.Unit/index.js":162,"../Effect.Exception/index.js":168,"../Effect/index.js":172,"../Prelude/index.js":198}],41:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Alt = require("../Control.Alt/index.js");
var Control_Alternative = require("../Control.Alternative/index.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Category = require("../Control.Category/index.js");
var Control_Monad = require("../Control.Monad/index.js");
var Control_Monad_Cont_Class = require("../Control.Monad.Cont.Class/index.js");
var Control_Monad_Error_Class = require("../Control.Monad.Error.Class/index.js");
var Control_Monad_Reader_Class = require("../Control.Monad.Reader.Class/index.js");
var Control_Monad_Rec_Class = require("../Control.Monad.Rec.Class/index.js");
var Control_Monad_State_Class = require("../Control.Monad.State.Class/index.js");
var Control_Monad_Trans_Class = require("../Control.Monad.Trans.Class/index.js");
var Control_Monad_Writer_Class = require("../Control.Monad.Writer.Class/index.js");
var Control_MonadPlus = require("../Control.MonadPlus/index.js");
var Control_MonadZero = require("../Control.MonadZero/index.js");
var Control_Plus = require("../Control.Plus/index.js");
var Control_Semigroupoid = require("../Control.Semigroupoid/index.js");
var Data_Either = require("../Data.Either/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Monoid = require("../Data.Monoid/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var Effect_Class = require("../Effect.Class/index.js");
var Prelude = require("../Prelude/index.js");
var ExceptT = function (x) {
    return x;
};
var withExceptT = function (dictFunctor) {
    return function (f) {
        return function (v) {
            var mapLeft = function (v1) {
                return function (v2) {
                    if (v2 instanceof Data_Either.Right) {
                        return new Data_Either.Right(v2.value0);
                    };
                    if (v2 instanceof Data_Either.Left) {
                        return new Data_Either.Left(v1(v2.value0));
                    };
                    throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 42, column 3 - line 42, column 32): " + [ v1.constructor.name, v2.constructor.name ]);
                };
            };
            return ExceptT(Data_Functor.map(dictFunctor)(mapLeft(f))(v));
        };
    };
};
var runExceptT = function (v) {
    return v;
};
var newtypeExceptT = new Data_Newtype.Newtype(function (n) {
    return n;
}, ExceptT);
var monadTransExceptT = new Control_Monad_Trans_Class.MonadTrans(function (dictMonad) {
    return function (m) {
        return Control_Bind.bind(dictMonad.Bind1())(m)(function (v) {
            return Control_Applicative.pure(dictMonad.Applicative0())(new Data_Either.Right(v));
        });
    };
});
var mapExceptT = function (f) {
    return function (v) {
        return f(v);
    };
};
var functorExceptT = function (dictFunctor) {
    return new Data_Functor.Functor(function (f) {
        return mapExceptT(Data_Functor.map(dictFunctor)(Data_Functor.map(Data_Either.functorEither)(f)));
    });
};
var except = function (dictApplicative) {
    return function ($96) {
        return ExceptT(Control_Applicative.pure(dictApplicative)($96));
    };
};
var monadExceptT = function (dictMonad) {
    return new Control_Monad.Monad(function () {
        return applicativeExceptT(dictMonad);
    }, function () {
        return bindExceptT(dictMonad);
    });
};
var bindExceptT = function (dictMonad) {
    return new Control_Bind.Bind(function () {
        return applyExceptT(dictMonad);
    }, function (v) {
        return function (k) {
            return Control_Bind.bind(dictMonad.Bind1())(v)(Data_Either.either(function ($97) {
                return Control_Applicative.pure(dictMonad.Applicative0())(Data_Either.Left.create($97));
            })(function (a) {
                var v1 = k(a);
                return v1;
            }));
        };
    });
};
var applyExceptT = function (dictMonad) {
    return new Control_Apply.Apply(function () {
        return functorExceptT(((dictMonad.Bind1()).Apply0()).Functor0());
    }, Control_Monad.ap(monadExceptT(dictMonad)));
};
var applicativeExceptT = function (dictMonad) {
    return new Control_Applicative.Applicative(function () {
        return applyExceptT(dictMonad);
    }, function ($98) {
        return ExceptT(Control_Applicative.pure(dictMonad.Applicative0())(Data_Either.Right.create($98)));
    });
};
var monadAskExceptT = function (dictMonadAsk) {
    return new Control_Monad_Reader_Class.MonadAsk(function () {
        return monadExceptT(dictMonadAsk.Monad0());
    }, Control_Monad_Trans_Class.lift(monadTransExceptT)(dictMonadAsk.Monad0())(Control_Monad_Reader_Class.ask(dictMonadAsk)));
};
var monadReaderExceptT = function (dictMonadReader) {
    return new Control_Monad_Reader_Class.MonadReader(function () {
        return monadAskExceptT(dictMonadReader.MonadAsk0());
    }, function (f) {
        return mapExceptT(Control_Monad_Reader_Class.local(dictMonadReader)(f));
    });
};
var monadContExceptT = function (dictMonadCont) {
    return new Control_Monad_Cont_Class.MonadCont(function () {
        return monadExceptT(dictMonadCont.Monad0());
    }, function (f) {
        return ExceptT(Control_Monad_Cont_Class.callCC(dictMonadCont)(function (c) {
            var v = f(function (a) {
                return ExceptT(c(new Data_Either.Right(a)));
            });
            return v;
        }));
    });
};
var monadEffectExceptT = function (dictMonadEffect) {
    return new Effect_Class.MonadEffect(function () {
        return monadExceptT(dictMonadEffect.Monad0());
    }, function ($99) {
        return Control_Monad_Trans_Class.lift(monadTransExceptT)(dictMonadEffect.Monad0())(Effect_Class.liftEffect(dictMonadEffect)($99));
    });
};
var monadRecExceptT = function (dictMonadRec) {
    return new Control_Monad_Rec_Class.MonadRec(function () {
        return monadExceptT(dictMonadRec.Monad0());
    }, function (f) {
        return function ($100) {
            return ExceptT(Control_Monad_Rec_Class.tailRecM(dictMonadRec)(function (a) {
                return Control_Bind.bind((dictMonadRec.Monad0()).Bind1())((function () {
                    var v = f(a);
                    return v;
                })())(function (m$prime) {
                    return Control_Applicative.pure((dictMonadRec.Monad0()).Applicative0())((function () {
                        if (m$prime instanceof Data_Either.Left) {
                            return new Control_Monad_Rec_Class.Done(new Data_Either.Left(m$prime.value0));
                        };
                        if (m$prime instanceof Data_Either.Right && m$prime.value0 instanceof Control_Monad_Rec_Class.Loop) {
                            return new Control_Monad_Rec_Class.Loop(m$prime.value0.value0);
                        };
                        if (m$prime instanceof Data_Either.Right && m$prime.value0 instanceof Control_Monad_Rec_Class.Done) {
                            return new Control_Monad_Rec_Class.Done(new Data_Either.Right(m$prime.value0.value0));
                        };
                        throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 74, column 14 - line 77, column 43): " + [ m$prime.constructor.name ]);
                    })());
                });
            })($100));
        };
    });
};
var monadStateExceptT = function (dictMonadState) {
    return new Control_Monad_State_Class.MonadState(function () {
        return monadExceptT(dictMonadState.Monad0());
    }, function (f) {
        return Control_Monad_Trans_Class.lift(monadTransExceptT)(dictMonadState.Monad0())(Control_Monad_State_Class.state(dictMonadState)(f));
    });
};
var monadTellExceptT = function (dictMonadTell) {
    return new Control_Monad_Writer_Class.MonadTell(function () {
        return monadExceptT(dictMonadTell.Monad0());
    }, function ($101) {
        return Control_Monad_Trans_Class.lift(monadTransExceptT)(dictMonadTell.Monad0())(Control_Monad_Writer_Class.tell(dictMonadTell)($101));
    });
};
var monadWriterExceptT = function (dictMonadWriter) {
    return new Control_Monad_Writer_Class.MonadWriter(function () {
        return monadTellExceptT(dictMonadWriter.MonadTell0());
    }, mapExceptT(function (m) {
        return Control_Bind.bind(((dictMonadWriter.MonadTell0()).Monad0()).Bind1())(Control_Monad_Writer_Class.listen(dictMonadWriter)(m))(function (v) {
            return Control_Applicative.pure(((dictMonadWriter.MonadTell0()).Monad0()).Applicative0())(Data_Functor.map(Data_Either.functorEither)(function (r) {
                return new Data_Tuple.Tuple(r, v.value1);
            })(v.value0));
        });
    }), mapExceptT(function (m) {
        return Control_Monad_Writer_Class.pass(dictMonadWriter)(Control_Bind.bind(((dictMonadWriter.MonadTell0()).Monad0()).Bind1())(m)(function (v) {
            return Control_Applicative.pure(((dictMonadWriter.MonadTell0()).Monad0()).Applicative0())((function () {
                if (v instanceof Data_Either.Left) {
                    return new Data_Tuple.Tuple(new Data_Either.Left(v.value0), Control_Category.identity(Control_Category.categoryFn));
                };
                if (v instanceof Data_Either.Right) {
                    return new Data_Tuple.Tuple(new Data_Either.Right(v.value0.value0), v.value0.value1);
                };
                throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 136, column 10 - line 138, column 44): " + [ v.constructor.name ]);
            })());
        }));
    }));
};
var monadThrowExceptT = function (dictMonad) {
    return new Control_Monad_Error_Class.MonadThrow(function () {
        return monadExceptT(dictMonad);
    }, function ($102) {
        return ExceptT(Control_Applicative.pure(dictMonad.Applicative0())(Data_Either.Left.create($102)));
    });
};
var monadErrorExceptT = function (dictMonad) {
    return new Control_Monad_Error_Class.MonadError(function () {
        return monadThrowExceptT(dictMonad);
    }, function (v) {
        return function (k) {
            return Control_Bind.bind(dictMonad.Bind1())(v)(Data_Either.either(function (a) {
                var v1 = k(a);
                return v1;
            })(function ($103) {
                return Control_Applicative.pure(dictMonad.Applicative0())(Data_Either.Right.create($103));
            }));
        };
    });
};
var altExceptT = function (dictSemigroup) {
    return function (dictMonad) {
        return new Control_Alt.Alt(function () {
            return functorExceptT(((dictMonad.Bind1()).Apply0()).Functor0());
        }, function (v) {
            return function (v1) {
                return Control_Bind.bind(dictMonad.Bind1())(v)(function (v2) {
                    if (v2 instanceof Data_Either.Right) {
                        return Control_Applicative.pure(dictMonad.Applicative0())(new Data_Either.Right(v2.value0));
                    };
                    if (v2 instanceof Data_Either.Left) {
                        return Control_Bind.bind(dictMonad.Bind1())(v1)(function (v3) {
                            if (v3 instanceof Data_Either.Right) {
                                return Control_Applicative.pure(dictMonad.Applicative0())(new Data_Either.Right(v3.value0));
                            };
                            if (v3 instanceof Data_Either.Left) {
                                return Control_Applicative.pure(dictMonad.Applicative0())(new Data_Either.Left(Data_Semigroup.append(dictSemigroup)(v2.value0)(v3.value0)));
                            };
                            throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 86, column 9 - line 88, column 49): " + [ v3.constructor.name ]);
                        });
                    };
                    throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 82, column 5 - line 88, column 49): " + [ v2.constructor.name ]);
                });
            };
        });
    };
};
var plusExceptT = function (dictMonoid) {
    return function (dictMonad) {
        return new Control_Plus.Plus(function () {
            return altExceptT(dictMonoid.Semigroup0())(dictMonad);
        }, Control_Monad_Error_Class.throwError(monadThrowExceptT(dictMonad))(Data_Monoid.mempty(dictMonoid)));
    };
};
var alternativeExceptT = function (dictMonoid) {
    return function (dictMonad) {
        return new Control_Alternative.Alternative(function () {
            return applicativeExceptT(dictMonad);
        }, function () {
            return plusExceptT(dictMonoid)(dictMonad);
        });
    };
};
var monadZeroExceptT = function (dictMonoid) {
    return function (dictMonad) {
        return new Control_MonadZero.MonadZero(function () {
            return alternativeExceptT(dictMonoid)(dictMonad);
        }, function () {
            return monadExceptT(dictMonad);
        });
    };
};
var monadPlusExceptT = function (dictMonoid) {
    return function (dictMonad) {
        return new Control_MonadPlus.MonadPlus(function () {
            return monadZeroExceptT(dictMonoid)(dictMonad);
        });
    };
};
module.exports = {
    ExceptT: ExceptT,
    runExceptT: runExceptT,
    withExceptT: withExceptT,
    mapExceptT: mapExceptT,
    except: except,
    newtypeExceptT: newtypeExceptT,
    functorExceptT: functorExceptT,
    applyExceptT: applyExceptT,
    applicativeExceptT: applicativeExceptT,
    bindExceptT: bindExceptT,
    monadExceptT: monadExceptT,
    monadRecExceptT: monadRecExceptT,
    altExceptT: altExceptT,
    plusExceptT: plusExceptT,
    alternativeExceptT: alternativeExceptT,
    monadPlusExceptT: monadPlusExceptT,
    monadZeroExceptT: monadZeroExceptT,
    monadTransExceptT: monadTransExceptT,
    monadEffectExceptT: monadEffectExceptT,
    monadContExceptT: monadContExceptT,
    monadThrowExceptT: monadThrowExceptT,
    monadErrorExceptT: monadErrorExceptT,
    monadAskExceptT: monadAskExceptT,
    monadReaderExceptT: monadReaderExceptT,
    monadStateExceptT: monadStateExceptT,
    monadTellExceptT: monadTellExceptT,
    monadWriterExceptT: monadWriterExceptT
};

},{"../Control.Alt/index.js":25,"../Control.Alternative/index.js":26,"../Control.Applicative/index.js":27,"../Control.Apply/index.js":29,"../Control.Bind/index.js":33,"../Control.Category/index.js":34,"../Control.Monad.Cont.Class/index.js":39,"../Control.Monad.Error.Class/index.js":40,"../Control.Monad.Reader.Class/index.js":43,"../Control.Monad.Rec.Class/index.js":44,"../Control.Monad.State.Class/index.js":49,"../Control.Monad.Trans.Class/index.js":50,"../Control.Monad.Writer.Class/index.js":51,"../Control.Monad/index.js":52,"../Control.MonadPlus/index.js":53,"../Control.MonadZero/index.js":54,"../Control.Plus/index.js":55,"../Control.Semigroupoid/index.js":56,"../Data.Either/index.js":80,"../Data.Function/index.js":91,"../Data.Functor/index.js":95,"../Data.Monoid/index.js":117,"../Data.Newtype/index.js":119,"../Data.Semigroup/index.js":139,"../Data.Tuple/index.js":156,"../Effect.Class/index.js":164,"../Prelude/index.js":198}],42:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Monad_Error_Class = require("../Control.Monad.Error.Class/index.js");
var Control_Monad_Except_Trans = require("../Control.Monad.Except.Trans/index.js");
var Control_Semigroupoid = require("../Control.Semigroupoid/index.js");
var Data_Either = require("../Data.Either/index.js");
var Data_Identity = require("../Data.Identity/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Prelude = require("../Prelude/index.js");
var withExcept = Control_Monad_Except_Trans.withExceptT(Data_Identity.functorIdentity);
var runExcept = function ($0) {
    return Data_Newtype.unwrap(Data_Identity.newtypeIdentity)(Control_Monad_Except_Trans.runExceptT($0));
};
var mapExcept = function (f) {
    return Control_Monad_Except_Trans.mapExceptT(function ($1) {
        return Data_Identity.Identity(f(Data_Newtype.unwrap(Data_Identity.newtypeIdentity)($1)));
    });
};
module.exports = {
    runExcept: runExcept,
    mapExcept: mapExcept,
    withExcept: withExcept
};

},{"../Control.Monad.Error.Class/index.js":40,"../Control.Monad.Except.Trans/index.js":41,"../Control.Semigroupoid/index.js":56,"../Data.Either/index.js":80,"../Data.Identity/index.js":100,"../Data.Newtype/index.js":119,"../Prelude/index.js":198}],43:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Category = require("../Control.Category/index.js");
var Control_Monad = require("../Control.Monad/index.js");
var Control_Semigroupoid = require("../Control.Semigroupoid/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Prelude = require("../Prelude/index.js");
var MonadAsk = function (Monad0, ask) {
    this.Monad0 = Monad0;
    this.ask = ask;
};
var MonadReader = function (MonadAsk0, local) {
    this.MonadAsk0 = MonadAsk0;
    this.local = local;
};
var monadAskFun = new MonadAsk(function () {
    return Control_Monad.monadFn;
}, Control_Category.identity(Control_Category.categoryFn));
var monadReaderFun = new MonadReader(function () {
    return monadAskFun;
}, Control_Semigroupoid.composeFlipped(Control_Semigroupoid.semigroupoidFn));
var local = function (dict) {
    return dict.local;
};
var ask = function (dict) {
    return dict.ask;
};
var asks = function (dictMonadAsk) {
    return function (f) {
        return Data_Functor.map((((dictMonadAsk.Monad0()).Bind1()).Apply0()).Functor0())(f)(ask(dictMonadAsk));
    };
};
module.exports = {
    ask: ask,
    local: local,
    MonadAsk: MonadAsk,
    asks: asks,
    MonadReader: MonadReader,
    monadAskFun: monadAskFun,
    monadReaderFun: monadReaderFun
};

},{"../Control.Category/index.js":34,"../Control.Monad/index.js":52,"../Control.Semigroupoid/index.js":56,"../Data.Functor/index.js":95,"../Prelude/index.js":198}],44:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Monad = require("../Control.Monad/index.js");
var Control_Semigroupoid = require("../Control.Semigroupoid/index.js");
var Data_Bifunctor = require("../Data.Bifunctor/index.js");
var Data_Either = require("../Data.Either/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Identity = require("../Data.Identity/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Effect = require("../Effect/index.js");
var Effect_Ref = require("../Effect.Ref/index.js");
var Partial_Unsafe = require("../Partial.Unsafe/index.js");
var Prelude = require("../Prelude/index.js");
var Loop = (function () {
    function Loop(value0) {
        this.value0 = value0;
    };
    Loop.create = function (value0) {
        return new Loop(value0);
    };
    return Loop;
})();
var Done = (function () {
    function Done(value0) {
        this.value0 = value0;
    };
    Done.create = function (value0) {
        return new Done(value0);
    };
    return Done;
})();
var MonadRec = function (Monad0, tailRecM) {
    this.Monad0 = Monad0;
    this.tailRecM = tailRecM;
};
var tailRecM = function (dict) {
    return dict.tailRecM;
};
var tailRecM2 = function (dictMonadRec) {
    return function (f) {
        return function (a) {
            return function (b) {
                return tailRecM(dictMonadRec)(function (o) {
                    return f(o.a)(o.b);
                })({
                    a: a,
                    b: b
                });
            };
        };
    };
};
var tailRecM3 = function (dictMonadRec) {
    return function (f) {
        return function (a) {
            return function (b) {
                return function (c) {
                    return tailRecM(dictMonadRec)(function (o) {
                        return f(o.a)(o.b)(o.c);
                    })({
                        a: a,
                        b: b,
                        c: c
                    });
                };
            };
        };
    };
};
var tailRec = function (f) {
    var go = function ($copy_v) {
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v) {
            if (v instanceof Loop) {
                $copy_v = f(v.value0);
                return;
            };
            if (v instanceof Done) {
                $tco_done = true;
                return v.value0;
            };
            throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 91, column 3 - line 91, column 25): " + [ v.constructor.name ]);
        };
        while (!$tco_done) {
            $tco_result = $tco_loop($copy_v);
        };
        return $tco_result;
    };
    return function ($53) {
        return go(f($53));
    };
};
var monadRecMaybe = new MonadRec(function () {
    return Data_Maybe.monadMaybe;
}, function (f) {
    return function (a0) {
        var g = function (v) {
            if (v instanceof Data_Maybe.Nothing) {
                return new Done(Data_Maybe.Nothing.value);
            };
            if (v instanceof Data_Maybe.Just && v.value0 instanceof Loop) {
                return new Loop(f(v.value0.value0));
            };
            if (v instanceof Data_Maybe.Just && v.value0 instanceof Done) {
                return new Done(new Data_Maybe.Just(v.value0.value0));
            };
            throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 127, column 7 - line 127, column 31): " + [ v.constructor.name ]);
        };
        return tailRec(g)(f(a0));
    };
});
var monadRecIdentity = new MonadRec(function () {
    return Data_Identity.monadIdentity;
}, function (f) {
    var runIdentity = function (v) {
        return v;
    };
    return function ($54) {
        return Data_Identity.Identity(tailRec(function ($55) {
            return runIdentity(f($55));
        })($54));
    };
});
var monadRecFunction = new MonadRec(function () {
    return Control_Monad.monadFn;
}, function (f) {
    return function (a0) {
        return function (e) {
            return tailRec(function (a) {
                return f(a)(e);
            })(a0);
        };
    };
});
var monadRecEither = new MonadRec(function () {
    return Data_Either.monadEither;
}, function (f) {
    return function (a0) {
        var g = function (v) {
            if (v instanceof Data_Either.Left) {
                return new Done(new Data_Either.Left(v.value0));
            };
            if (v instanceof Data_Either.Right && v.value0 instanceof Loop) {
                return new Loop(f(v.value0.value0));
            };
            if (v instanceof Data_Either.Right && v.value0 instanceof Done) {
                return new Done(new Data_Either.Right(v.value0.value0));
            };
            throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 119, column 7 - line 119, column 33): " + [ v.constructor.name ]);
        };
        return tailRec(g)(f(a0));
    };
});
var monadRecEffect = new MonadRec(function () {
    return Effect.monadEffect;
}, function (f) {
    return function (a) {
        var fromDone = function (v) {
            if (v instanceof Done) {
                return v.value0;
            };
            throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 111, column 30 - line 111, column 44): " + [ v.constructor.name ]);
        };
        return function __do() {
            var v = Control_Bind.bindFlipped(Effect.bindEffect)(Effect_Ref["new"])(f(a))();
            (function () {
                while (!(function __do() {
                    var v1 = Effect_Ref.read(v)();
                    if (v1 instanceof Loop) {
                        var v2 = f(v1.value0)();
                        var v3 = Effect_Ref.write(v2)(v)();
                        return false;
                    };
                    if (v1 instanceof Done) {
                        return true;
                    };
                    throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 102, column 22 - line 107, column 28): " + [ v1.constructor.name ]);
                })()) {

                };
                return {};
            })();
            return Data_Functor.map(Effect.functorEffect)(fromDone)(Effect_Ref.read(v))();
        };
    };
});
var functorStep = new Data_Functor.Functor(function (f) {
    return function (m) {
        if (m instanceof Loop) {
            return new Loop(m.value0);
        };
        if (m instanceof Done) {
            return new Done(f(m.value0));
        };
        throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 25, column 8 - line 25, column 48): " + [ m.constructor.name ]);
    };
});
var forever = function (dictMonadRec) {
    return function (ma) {
        return tailRecM(dictMonadRec)(function (u) {
            return Data_Functor.voidRight((((dictMonadRec.Monad0()).Bind1()).Apply0()).Functor0())(new Loop(u))(ma);
        })(Data_Unit.unit);
    };
};
var bifunctorStep = new Data_Bifunctor.Bifunctor(function (v) {
    return function (v1) {
        return function (v2) {
            if (v2 instanceof Loop) {
                return new Loop(v(v2.value0));
            };
            if (v2 instanceof Done) {
                return new Done(v1(v2.value0));
            };
            throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 27, column 1 - line 27, column 41): " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
        };
    };
});
module.exports = {
    Loop: Loop,
    Done: Done,
    MonadRec: MonadRec,
    tailRec: tailRec,
    tailRecM: tailRecM,
    tailRecM2: tailRecM2,
    tailRecM3: tailRecM3,
    forever: forever,
    functorStep: functorStep,
    bifunctorStep: bifunctorStep,
    monadRecIdentity: monadRecIdentity,
    monadRecEffect: monadRecEffect,
    monadRecFunction: monadRecFunction,
    monadRecEither: monadRecEither,
    monadRecMaybe: monadRecMaybe
};

},{"../Control.Applicative/index.js":27,"../Control.Bind/index.js":33,"../Control.Monad/index.js":52,"../Control.Semigroupoid/index.js":56,"../Data.Bifunctor/index.js":71,"../Data.Either/index.js":80,"../Data.Functor/index.js":95,"../Data.Identity/index.js":100,"../Data.Maybe/index.js":110,"../Data.Unit/index.js":162,"../Effect.Ref/index.js":170,"../Effect/index.js":172,"../Partial.Unsafe/index.js":195,"../Prelude/index.js":198}],45:[function(require,module,exports){
"use strict";

exports.map_ = function (f) {
  return function (a) {
    return function () {
      return f(a());
    };
  };
};

exports.pure_ = function (a) {
  return function () {
    return a;
  };
};

exports.bind_ = function (a) {
  return function (f) {
    return function () {
      return f(a())();
    };
  };
};

exports.run = function (f) {
  return f();
};

exports["while"] = function (f) {
  return function (a) {
    return function () {
      while (f()) {
        a();
      }
    };
  };
};

exports["for"] = function (lo) {
  return function (hi) {
    return function (f) {
      return function () {
        for (var i = lo; i < hi; i++) {
          f(i)();
        }
      };
    };
  };
};

exports.foreach = function (as) {
  return function (f) {
    return function () {
      for (var i = 0, l = as.length; i < l; i++) {
        f(as[i])();
      }
    };
  };
};

exports.new = function (val) {
  return function () {
    return { value: val };
  };
};

exports.read = function (ref) {
  return function () {
    return ref.value;
  };
};

exports["modify'"] = function (f) {
  return function (ref) {
    return function () {
      var t = f(ref.value);
      ref.value = t.state;
      return t.value;
    };
  };
};

exports.write = function (a) {
  return function (ref) {
    return function () {
      return ref.value = a; // eslint-disable-line no-return-assign
    };
  };
};

},{}],46:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var $foreign = require("./foreign.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Monad = require("../Control.Monad/index.js");
var Control_Monad_Rec_Class = require("../Control.Monad.Rec.Class/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Partial_Unsafe = require("../Partial.Unsafe/index.js");
var Prelude = require("../Prelude/index.js");
var modify = function (f) {
    return $foreign["modify'"](function (s) {
        var s$prime = f(s);
        return {
            state: s$prime,
            value: s$prime
        };
    });
};
var functorST = new Data_Functor.Functor($foreign.map_);
var monadST = new Control_Monad.Monad(function () {
    return applicativeST;
}, function () {
    return bindST;
});
var bindST = new Control_Bind.Bind(function () {
    return applyST;
}, $foreign.bind_);
var applyST = new Control_Apply.Apply(function () {
    return functorST;
}, Control_Monad.ap(monadST));
var applicativeST = new Control_Applicative.Applicative(function () {
    return applyST;
}, $foreign.pure_);
var monadRecST = new Control_Monad_Rec_Class.MonadRec(function () {
    return monadST;
}, function (f) {
    return function (a) {
        var isLooping = function (v) {
            if (v instanceof Control_Monad_Rec_Class.Loop) {
                return true;
            };
            return false;
        };
        var fromDone = function (v) {
            if (v instanceof Control_Monad_Rec_Class.Done) {
                return v.value0;
            };
            throw new Error("Failed pattern match at Control.Monad.ST.Internal (line 54, column 32 - line 54, column 46): " + [ v.constructor.name ]);
        };
        return Control_Bind.bind(bindST)(Control_Bind.bindFlipped(bindST)($foreign["new"])(f(a)))(function (v) {
            return Control_Bind.discard(Control_Bind.discardUnit)(bindST)($foreign["while"](Data_Functor.map(functorST)(isLooping)($foreign.read(v)))(Control_Bind.bind(bindST)($foreign.read(v))(function (v1) {
                if (v1 instanceof Control_Monad_Rec_Class.Loop) {
                    return Control_Bind.bind(bindST)(f(v1.value0))(function (v2) {
                        return Data_Functor["void"](functorST)($foreign.write(v2)(v));
                    });
                };
                if (v1 instanceof Control_Monad_Rec_Class.Done) {
                    return Control_Applicative.pure(applicativeST)(Data_Unit.unit);
                };
                throw new Error("Failed pattern match at Control.Monad.ST.Internal (line 46, column 18 - line 50, column 28): " + [ v1.constructor.name ]);
            })))(function () {
                return Data_Functor.map(functorST)(fromDone)($foreign.read(v));
            });
        });
    };
});
module.exports = {
    modify: modify,
    functorST: functorST,
    applyST: applyST,
    applicativeST: applicativeST,
    bindST: bindST,
    monadST: monadST,
    monadRecST: monadRecST,
    map_: $foreign.map_,
    pure_: $foreign.pure_,
    bind_: $foreign.bind_,
    run: $foreign.run,
    "while": $foreign["while"],
    "for": $foreign["for"],
    foreach: $foreign.foreach,
    "new": $foreign["new"],
    read: $foreign.read,
    "modify'": $foreign["modify'"],
    write: $foreign.write
};

},{"../Control.Applicative/index.js":27,"../Control.Apply/index.js":29,"../Control.Bind/index.js":33,"../Control.Monad.Rec.Class/index.js":44,"../Control.Monad/index.js":52,"../Data.Functor/index.js":95,"../Data.Unit/index.js":162,"../Partial.Unsafe/index.js":195,"../Prelude/index.js":198,"./foreign.js":45}],47:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Monad_ST_Internal = require("../Control.Monad.ST.Internal/index.js");
module.exports = {};

},{"../Control.Monad.ST.Internal/index.js":46}],48:[function(require,module,exports){
arguments[4][47][0].apply(exports,arguments)
},{"../Control.Monad.ST.Internal/index.js":46,"dup":47}],49:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Data_Tuple = require("../Data.Tuple/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Prelude = require("../Prelude/index.js");
var MonadState = function (Monad0, state) {
    this.Monad0 = Monad0;
    this.state = state;
};
var state = function (dict) {
    return dict.state;
};
var put = function (dictMonadState) {
    return function (s) {
        return state(dictMonadState)(function (v) {
            return new Data_Tuple.Tuple(Data_Unit.unit, s);
        });
    };
};
var modify_ = function (dictMonadState) {
    return function (f) {
        return state(dictMonadState)(function (s) {
            return new Data_Tuple.Tuple(Data_Unit.unit, f(s));
        });
    };
};
var modify = function (dictMonadState) {
    return function (f) {
        return state(dictMonadState)(function (s) {
            var s$prime = f(s);
            return new Data_Tuple.Tuple(s$prime, s$prime);
        });
    };
};
var gets = function (dictMonadState) {
    return function (f) {
        return state(dictMonadState)(function (s) {
            return new Data_Tuple.Tuple(f(s), s);
        });
    };
};
var get = function (dictMonadState) {
    return state(dictMonadState)(function (s) {
        return new Data_Tuple.Tuple(s, s);
    });
};
module.exports = {
    state: state,
    MonadState: MonadState,
    get: get,
    gets: gets,
    put: put,
    modify: modify,
    modify_: modify_
};

},{"../Data.Tuple/index.js":156,"../Data.Unit/index.js":162,"../Prelude/index.js":198}],50:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Prelude = require("../Prelude/index.js");
var MonadTrans = function (lift) {
    this.lift = lift;
};
var lift = function (dict) {
    return dict.lift;
};
module.exports = {
    lift: lift,
    MonadTrans: MonadTrans
};

},{"../Prelude/index.js":198}],51:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var Prelude = require("../Prelude/index.js");
var MonadTell = function (Monad0, tell) {
    this.Monad0 = Monad0;
    this.tell = tell;
};
var MonadWriter = function (MonadTell0, listen, pass) {
    this.MonadTell0 = MonadTell0;
    this.listen = listen;
    this.pass = pass;
};
var tell = function (dict) {
    return dict.tell;
};
var pass = function (dict) {
    return dict.pass;
};
var listen = function (dict) {
    return dict.listen;
};
var listens = function (dictMonadWriter) {
    return function (f) {
        return function (m) {
            return Control_Bind.bind(((dictMonadWriter.MonadTell0()).Monad0()).Bind1())(listen(dictMonadWriter)(m))(function (v) {
                return Control_Applicative.pure(((dictMonadWriter.MonadTell0()).Monad0()).Applicative0())(new Data_Tuple.Tuple(v.value0, f(v.value1)));
            });
        };
    };
};
var censor = function (dictMonadWriter) {
    return function (f) {
        return function (m) {
            return pass(dictMonadWriter)(Control_Bind.bind(((dictMonadWriter.MonadTell0()).Monad0()).Bind1())(m)(function (v) {
                return Control_Applicative.pure(((dictMonadWriter.MonadTell0()).Monad0()).Applicative0())(new Data_Tuple.Tuple(v, f));
            }));
        };
    };
};
module.exports = {
    listen: listen,
    pass: pass,
    tell: tell,
    MonadTell: MonadTell,
    MonadWriter: MonadWriter,
    listens: listens,
    censor: censor
};

},{"../Control.Applicative/index.js":27,"../Control.Bind/index.js":33,"../Data.Function/index.js":91,"../Data.Tuple/index.js":156,"../Prelude/index.js":198}],52:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Monad = function (Applicative0, Bind1) {
    this.Applicative0 = Applicative0;
    this.Bind1 = Bind1;
};
var whenM = function (dictMonad) {
    return function (mb) {
        return function (m) {
            return Control_Bind.bind(dictMonad.Bind1())(mb)(function (v) {
                return Control_Applicative.when(dictMonad.Applicative0())(v)(m);
            });
        };
    };
};
var unlessM = function (dictMonad) {
    return function (mb) {
        return function (m) {
            return Control_Bind.bind(dictMonad.Bind1())(mb)(function (v) {
                return Control_Applicative.unless(dictMonad.Applicative0())(v)(m);
            });
        };
    };
};
var monadFn = new Monad(function () {
    return Control_Applicative.applicativeFn;
}, function () {
    return Control_Bind.bindFn;
});
var monadArray = new Monad(function () {
    return Control_Applicative.applicativeArray;
}, function () {
    return Control_Bind.bindArray;
});
var liftM1 = function (dictMonad) {
    return function (f) {
        return function (a) {
            return Control_Bind.bind(dictMonad.Bind1())(a)(function (v) {
                return Control_Applicative.pure(dictMonad.Applicative0())(f(v));
            });
        };
    };
};
var ap = function (dictMonad) {
    return function (f) {
        return function (a) {
            return Control_Bind.bind(dictMonad.Bind1())(f)(function (v) {
                return Control_Bind.bind(dictMonad.Bind1())(a)(function (v1) {
                    return Control_Applicative.pure(dictMonad.Applicative0())(v(v1));
                });
            });
        };
    };
};
module.exports = {
    Monad: Monad,
    liftM1: liftM1,
    ap: ap,
    whenM: whenM,
    unlessM: unlessM,
    monadFn: monadFn,
    monadArray: monadArray
};

},{"../Control.Applicative/index.js":27,"../Control.Apply/index.js":29,"../Control.Bind/index.js":33,"../Data.Functor/index.js":95,"../Data.Unit/index.js":162}],53:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Alt = require("../Control.Alt/index.js");
var Control_Alternative = require("../Control.Alternative/index.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Monad = require("../Control.Monad/index.js");
var Control_MonadZero = require("../Control.MonadZero/index.js");
var Control_Plus = require("../Control.Plus/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var MonadPlus = function (MonadZero0) {
    this.MonadZero0 = MonadZero0;
};
var monadPlusArray = new MonadPlus(function () {
    return Control_MonadZero.monadZeroArray;
});
module.exports = {
    MonadPlus: MonadPlus,
    monadPlusArray: monadPlusArray
};

},{"../Control.Alt/index.js":25,"../Control.Alternative/index.js":26,"../Control.Applicative/index.js":27,"../Control.Apply/index.js":29,"../Control.Bind/index.js":33,"../Control.Monad/index.js":52,"../Control.MonadZero/index.js":54,"../Control.Plus/index.js":55,"../Data.Functor/index.js":95}],54:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Alt = require("../Control.Alt/index.js");
var Control_Alternative = require("../Control.Alternative/index.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Monad = require("../Control.Monad/index.js");
var Control_Plus = require("../Control.Plus/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var MonadZero = function (Alternative1, Monad0) {
    this.Alternative1 = Alternative1;
    this.Monad0 = Monad0;
};
var monadZeroArray = new MonadZero(function () {
    return Control_Alternative.alternativeArray;
}, function () {
    return Control_Monad.monadArray;
});
var guard = function (dictMonadZero) {
    return function (v) {
        if (v) {
            return Control_Applicative.pure((dictMonadZero.Alternative1()).Applicative0())(Data_Unit.unit);
        };
        if (!v) {
            return Control_Plus.empty((dictMonadZero.Alternative1()).Plus1());
        };
        throw new Error("Failed pattern match at Control.MonadZero (line 54, column 1 - line 54, column 52): " + [ v.constructor.name ]);
    };
};
module.exports = {
    MonadZero: MonadZero,
    guard: guard,
    monadZeroArray: monadZeroArray
};

},{"../Control.Alt/index.js":25,"../Control.Alternative/index.js":26,"../Control.Applicative/index.js":27,"../Control.Apply/index.js":29,"../Control.Bind/index.js":33,"../Control.Monad/index.js":52,"../Control.Plus/index.js":55,"../Data.Functor/index.js":95,"../Data.Unit/index.js":162}],55:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Alt = require("../Control.Alt/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Plus = function (Alt0, empty) {
    this.Alt0 = Alt0;
    this.empty = empty;
};
var plusArray = new Plus(function () {
    return Control_Alt.altArray;
}, [  ]);
var empty = function (dict) {
    return dict.empty;
};
module.exports = {
    Plus: Plus,
    empty: empty,
    plusArray: plusArray
};

},{"../Control.Alt/index.js":25,"../Data.Functor/index.js":95}],56:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Semigroupoid = function (compose) {
    this.compose = compose;
};
var semigroupoidFn = new Semigroupoid(function (f) {
    return function (g) {
        return function (x) {
            return f(g(x));
        };
    };
});
var compose = function (dict) {
    return dict.compose;
};
var composeFlipped = function (dictSemigroupoid) {
    return function (f) {
        return function (g) {
            return compose(dictSemigroupoid)(g)(f);
        };
    };
};
module.exports = {
    compose: compose,
    Semigroupoid: Semigroupoid,
    composeFlipped: composeFlipped,
    semigroupoidFn: semigroupoidFn
};

},{}],57:[function(require,module,exports){
"use strict";

exports.fold1Impl = function (f) {
  return function (xs) {
    var acc = xs[0];
    var len = xs.length;
    for (var i = 1; i < len; i++) {
      acc = f(acc)(xs[i]);
    }
    return acc;
  };
};

exports.traverse1Impl = function () {
  function Cont(fn) {
    this.fn = fn;
  }

  var emptyList = {};

  var ConsCell = function (head, tail) {
    this.head = head;
    this.tail = tail;
  };

  function finalCell(head) {
    return new ConsCell(head, emptyList);
  }

  function consList(x) {
    return function (xs) {
      return new ConsCell(x, xs);
    };
  }

  function listToArray(list) {
    var arr = [];
    var xs = list;
    while (xs !== emptyList) {
      arr.push(xs.head);
      xs = xs.tail;
    }
    return arr;
  }

  return function (apply) {
    return function (map) {
      return function (f) {
        var buildFrom = function (x, ys) {
          return apply(map(consList)(f(x)))(ys);
        };

        var go = function (acc, currentLen, xs) {
          if (currentLen === 0) {
            return acc;
          } else {
            var last = xs[currentLen - 1];
            return new Cont(function () {
              var built = go(buildFrom(last, acc), currentLen - 1, xs);
              return built;
            });
          }
        };

        return function (array) {
          var acc = map(finalCell)(f(array[array.length - 1]));
          var result = go(acc, array.length - 1, array);
          while (result instanceof Cont) {
            result = result.fn();
          }

          return map(listToArray)(result);
        };
      };
    };
  };
}();

},{}],58:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var $foreign = require("./foreign.js");
var Control_Alt = require("../Control.Alt/index.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Monad = require("../Control.Monad/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_FoldableWithIndex = require("../Data.FoldableWithIndex/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_FunctorWithIndex = require("../Data.FunctorWithIndex/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Semigroup_Foldable = require("../Data.Semigroup.Foldable/index.js");
var Data_Semigroup_Traversable = require("../Data.Semigroup.Traversable/index.js");
var Data_Show = require("../Data.Show/index.js");
var Data_Traversable = require("../Data.Traversable/index.js");
var Data_TraversableWithIndex = require("../Data.TraversableWithIndex/index.js");
var Data_Unfoldable1 = require("../Data.Unfoldable1/index.js");
var Prelude = require("../Prelude/index.js");
var NonEmptyArray = function (x) {
    return x;
};
var unfoldable1NonEmptyArray = Data_Unfoldable1.unfoldable1Array;
var traversableWithIndexNonEmptyArray = Data_TraversableWithIndex.traversableWithIndexArray;
var traversableNonEmptyArray = Data_Traversable.traversableArray;
var showNonEmptyArray = function (dictShow) {
    return new Data_Show.Show(function (v) {
        return "(NonEmptyArray " + (Data_Show.show(Data_Show.showArray(dictShow))(v) + ")");
    });
};
var semigroupNonEmptyArray = Data_Semigroup.semigroupArray;
var ordNonEmptyArray = function (dictOrd) {
    return Data_Ord.ordArray(dictOrd);
};
var ord1NonEmptyArray = Data_Ord.ord1Array;
var monadNonEmptyArray = Control_Monad.monadArray;
var functorWithIndexNonEmptyArray = Data_FunctorWithIndex.functorWithIndexArray;
var functorNonEmptyArray = Data_Functor.functorArray;
var foldableWithIndexNonEmptyArray = Data_FoldableWithIndex.foldableWithIndexArray;
var foldableNonEmptyArray = Data_Foldable.foldableArray;
var foldable1NonEmptyArray = new Data_Semigroup_Foldable.Foldable1(function () {
    return foldableNonEmptyArray;
}, function (dictSemigroup) {
    return $foreign.fold1Impl(Data_Semigroup.append(dictSemigroup));
}, function (dictSemigroup) {
    return Data_Semigroup_Foldable.foldMap1Default(foldable1NonEmptyArray)(functorNonEmptyArray)(dictSemigroup);
});
var traversable1NonEmptyArray = new Data_Semigroup_Traversable.Traversable1(function () {
    return foldable1NonEmptyArray;
}, function () {
    return traversableNonEmptyArray;
}, function (dictApply) {
    return Data_Semigroup_Traversable.sequence1Default(traversable1NonEmptyArray)(dictApply);
}, function (dictApply) {
    return $foreign.traverse1Impl(Control_Apply.apply(dictApply))(Data_Functor.map(dictApply.Functor0()));
});
var eqNonEmptyArray = function (dictEq) {
    return Data_Eq.eqArray(dictEq);
};
var eq1NonEmptyArray = Data_Eq.eq1Array;
var bindNonEmptyArray = Control_Bind.bindArray;
var applyNonEmptyArray = Control_Apply.applyArray;
var applicativeNonEmptyArray = Control_Applicative.applicativeArray;
var altNonEmptyArray = Control_Alt.altArray;
module.exports = {
    showNonEmptyArray: showNonEmptyArray,
    eqNonEmptyArray: eqNonEmptyArray,
    eq1NonEmptyArray: eq1NonEmptyArray,
    ordNonEmptyArray: ordNonEmptyArray,
    ord1NonEmptyArray: ord1NonEmptyArray,
    semigroupNonEmptyArray: semigroupNonEmptyArray,
    functorNonEmptyArray: functorNonEmptyArray,
    functorWithIndexNonEmptyArray: functorWithIndexNonEmptyArray,
    foldableNonEmptyArray: foldableNonEmptyArray,
    foldableWithIndexNonEmptyArray: foldableWithIndexNonEmptyArray,
    foldable1NonEmptyArray: foldable1NonEmptyArray,
    unfoldable1NonEmptyArray: unfoldable1NonEmptyArray,
    traversableNonEmptyArray: traversableNonEmptyArray,
    traversableWithIndexNonEmptyArray: traversableWithIndexNonEmptyArray,
    traversable1NonEmptyArray: traversable1NonEmptyArray,
    applyNonEmptyArray: applyNonEmptyArray,
    applicativeNonEmptyArray: applicativeNonEmptyArray,
    bindNonEmptyArray: bindNonEmptyArray,
    monadNonEmptyArray: monadNonEmptyArray,
    altNonEmptyArray: altNonEmptyArray
};

},{"../Control.Alt/index.js":25,"../Control.Applicative/index.js":27,"../Control.Apply/index.js":29,"../Control.Bind/index.js":33,"../Control.Monad/index.js":52,"../Data.Eq/index.js":82,"../Data.Foldable/index.js":87,"../Data.FoldableWithIndex/index.js":88,"../Data.Functor/index.js":95,"../Data.FunctorWithIndex/index.js":97,"../Data.Ord/index.js":130,"../Data.Semigroup.Foldable/index.js":135,"../Data.Semigroup.Traversable/index.js":137,"../Data.Semigroup/index.js":139,"../Data.Show/index.js":143,"../Data.Traversable/index.js":154,"../Data.TraversableWithIndex/index.js":155,"../Data.Unfoldable1/index.js":160,"../Prelude/index.js":198,"./foreign.js":57}],59:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Monad_ST = require("../Control.Monad.ST/index.js");
var Control_Monad_ST_Internal = require("../Control.Monad.ST.Internal/index.js");
var Control_Monad_ST_Ref = require("../Control.Monad.ST.Ref/index.js");
var Control_Semigroupoid = require("../Control.Semigroupoid/index.js");
var Data_Array_ST = require("../Data.Array.ST/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Semiring = require("../Data.Semiring/index.js");
var Prelude = require("../Prelude/index.js");
var Iterator = (function () {
    function Iterator(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Iterator.create = function (value0) {
        return function (value1) {
            return new Iterator(value0, value1);
        };
    };
    return Iterator;
})();
var peek = function (v) {
    return function __do() {
        var v1 = Control_Monad_ST_Internal.read(v.value1)();
        return v.value0(v1);
    };
};
var next = function (v) {
    return function __do() {
        var v1 = Control_Monad_ST_Internal.read(v.value1)();
        var v2 = Control_Monad_ST_Internal.modify(function (v2) {
            return v2 + 1 | 0;
        })(v.value1)();
        return v.value0(v1);
    };
};
var pushWhile = function (p) {
    return function (iter) {
        return function (array) {
            return function __do() {
                var v = Control_Monad_ST_Internal["new"](false)();
                while (Data_Functor.map(Control_Monad_ST_Internal.functorST)(Data_HeytingAlgebra.not(Data_HeytingAlgebra.heytingAlgebraBoolean))(Control_Monad_ST_Internal.read(v))()) {
                    (function __do() {
                        var v1 = peek(iter)();
                        if (v1 instanceof Data_Maybe.Just && p(v1.value0)) {
                            var v2 = Data_Array_ST.push(v1.value0)(array)();
                            return Data_Functor["void"](Control_Monad_ST_Internal.functorST)(next(iter))();
                        };
                        return Data_Functor["void"](Control_Monad_ST_Internal.functorST)(Control_Monad_ST_Internal.write(true)(v))();
                    })();
                };
                return {};
            };
        };
    };
};
var pushAll = pushWhile(Data_Function["const"](true));
var iterator = function (f) {
    return Data_Functor.map(Control_Monad_ST_Internal.functorST)(Iterator.create(f))(Control_Monad_ST_Internal["new"](0));
};
var iterate = function (iter) {
    return function (f) {
        return function __do() {
            var v = Control_Monad_ST_Internal["new"](false)();
            while (Data_Functor.map(Control_Monad_ST_Internal.functorST)(Data_HeytingAlgebra.not(Data_HeytingAlgebra.heytingAlgebraBoolean))(Control_Monad_ST_Internal.read(v))()) {
                (function __do() {
                    var v1 = next(iter)();
                    if (v1 instanceof Data_Maybe.Just) {
                        return f(v1.value0)();
                    };
                    if (v1 instanceof Data_Maybe.Nothing) {
                        return Data_Functor["void"](Control_Monad_ST_Internal.functorST)(Control_Monad_ST_Internal.write(true)(v))();
                    };
                    throw new Error("Failed pattern match at Data.Array.ST.Iterator (line 42, column 5 - line 44, column 47): " + [ v1.constructor.name ]);
                })();
            };
            return {};
        };
    };
};
var exhausted = function ($27) {
    return Data_Functor.map(Control_Monad_ST_Internal.functorST)(Data_Maybe.isNothing)(peek($27));
};
module.exports = {
    iterator: iterator,
    iterate: iterate,
    next: next,
    peek: peek,
    exhausted: exhausted,
    pushWhile: pushWhile,
    pushAll: pushAll
};

},{"../Control.Applicative/index.js":27,"../Control.Bind/index.js":33,"../Control.Monad.ST.Internal/index.js":46,"../Control.Monad.ST.Ref/index.js":47,"../Control.Monad.ST/index.js":48,"../Control.Semigroupoid/index.js":56,"../Data.Array.ST/index.js":61,"../Data.Function/index.js":91,"../Data.Functor/index.js":95,"../Data.HeytingAlgebra/index.js":99,"../Data.Maybe/index.js":110,"../Data.Semiring/index.js":141,"../Prelude/index.js":198}],60:[function(require,module,exports){
"use strict";

exports.empty = function () {
  return [];
};

exports.peekImpl = function (just) {
  return function (nothing) {
    return function (i) {
      return function (xs) {
        return function () {
          return i >= 0 && i < xs.length ? just(xs[i]) : nothing;
        };
      };
    };
  };
};

exports.poke = function (i) {
  return function (a) {
    return function (xs) {
      return function () {
        var ret = i >= 0 && i < xs.length;
        if (ret) xs[i] = a;
        return ret;
      };
    };
  };
};

exports.pushAll = function (as) {
  return function (xs) {
    return function () {
      return xs.push.apply(xs, as);
    };
  };
};

exports.splice = function (i) {
  return function (howMany) {
    return function (bs) {
      return function (xs) {
        return function () {
          return xs.splice.apply(xs, [i, howMany].concat(bs));
        };
      };
    };
  };
};

exports.copyImpl = function (xs) {
  return function () {
    return xs.slice();
  };
};

exports.sortByImpl = function (comp) {
  return function (xs) {
    return function () {
      return xs.sort(function (x, y) {
        return comp(x)(y);
      });
    };
  };
};

exports.toAssocArray = function (xs) {
  return function () {
    var n = xs.length;
    var as = new Array(n);
    for (var i = 0; i < n; i++) as[i] = { value: xs[i], index: i };
    return as;
  };
};

},{}],61:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var $foreign = require("./foreign.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Monad_ST = require("../Control.Monad.ST/index.js");
var Control_Monad_ST_Internal = require("../Control.Monad.ST.Internal/index.js");
var Control_Semigroupoid = require("../Control.Semigroupoid/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Ordering = require("../Data.Ordering/index.js");
var Data_Ring = require("../Data.Ring/index.js");
var Prelude = require("../Prelude/index.js");
var Unsafe_Coerce = require("../Unsafe.Coerce/index.js");
var unsafeThaw = function ($11) {
    return Control_Applicative.pure(Control_Monad_ST_Internal.applicativeST)($11);
};
var unsafeFreeze = function ($12) {
    return Control_Applicative.pure(Control_Monad_ST_Internal.applicativeST)($12);
};
var thaw = $foreign.copyImpl;
var withArray = function (f) {
    return function (xs) {
        return function __do() {
            var v = thaw(xs)();
            var v1 = f(v)();
            return unsafeFreeze(v)();
        };
    };
};
var sortBy = function (comp) {
    var comp$prime = function (x) {
        return function (y) {
            var v = comp(x)(y);
            if (v instanceof Data_Ordering.GT) {
                return 1;
            };
            if (v instanceof Data_Ordering.EQ) {
                return 0;
            };
            if (v instanceof Data_Ordering.LT) {
                return -1 | 0;
            };
            throw new Error("Failed pattern match at Data.Array.ST (line 94, column 15 - line 99, column 1): " + [ v.constructor.name ]);
        };
    };
    return $foreign.sortByImpl(comp$prime);
};
var sortWith = function (dictOrd) {
    return function (f) {
        return sortBy(Data_Ord.comparing(dictOrd)(f));
    };
};
var sort = function (dictOrd) {
    return sortBy(Data_Ord.compare(dictOrd));
};
var run = function (st) {
    return Control_Bind.bind(Control_Monad_ST_Internal.bindST)(st)(unsafeFreeze)();
};
var push = function (a) {
    return $foreign.pushAll([ a ]);
};
var peek = $foreign.peekImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var modify = function (i) {
    return function (f) {
        return function (xs) {
            return function __do() {
                var v = peek(i)(xs)();
                if (v instanceof Data_Maybe.Just) {
                    return $foreign.poke(i)(f(v.value0))(xs)();
                };
                if (v instanceof Data_Maybe.Nothing) {
                    return false;
                };
                throw new Error("Failed pattern match at Data.Array.ST (line 156, column 3 - line 158, column 26): " + [ v.constructor.name ]);
            };
        };
    };
};
var freeze = $foreign.copyImpl;
module.exports = {
    run: run,
    withArray: withArray,
    peek: peek,
    push: push,
    modify: modify,
    sort: sort,
    sortBy: sortBy,
    sortWith: sortWith,
    freeze: freeze,
    thaw: thaw,
    unsafeFreeze: unsafeFreeze,
    unsafeThaw: unsafeThaw,
    empty: $foreign.empty,
    poke: $foreign.poke,
    pushAll: $foreign.pushAll,
    splice: $foreign.splice,
    toAssocArray: $foreign.toAssocArray
};

},{"../Control.Applicative/index.js":27,"../Control.Bind/index.js":33,"../Control.Monad.ST.Internal/index.js":46,"../Control.Monad.ST/index.js":48,"../Control.Semigroupoid/index.js":56,"../Data.Maybe/index.js":110,"../Data.Ord/index.js":130,"../Data.Ordering/index.js":131,"../Data.Ring/index.js":133,"../Prelude/index.js":198,"../Unsafe.Coerce/index.js":211,"./foreign.js":60}],62:[function(require,module,exports){
"use strict";

//------------------------------------------------------------------------------
// Array creation --------------------------------------------------------------
//------------------------------------------------------------------------------

exports.range = function (start) {
  return function (end) {
    var step = start > end ? -1 : 1;
    var result = new Array(step * (end - start) + 1);
    var i = start, n = 0;
    while (i !== end) {
      result[n++] = i;
      i += step;
    }
    result[n] = i;
    return result;
  };
};

var replicateFill = function (count) {
  return function (value) {
    if (count < 1) {
      return [];
    }
    var result = new Array(count);
    return result.fill(value);
  };
};

var replicatePolyfill = function (count) {
  return function (value) {
    var result = [];
    var n = 0;
    for (var i = 0; i < count; i++) {
      result[n++] = value;
    }
    return result;
  };
};

// In browsers that have Array.prototype.fill we use it, as it's faster.
exports.replicate = typeof Array.prototype.fill === "function" ? replicateFill : replicatePolyfill;

exports.fromFoldableImpl = (function () {
  function Cons(head, tail) {
    this.head = head;
    this.tail = tail;
  }
  var emptyList = {};

  function curryCons(head) {
    return function (tail) {
      return new Cons(head, tail);
    };
  }

  function listToArray(list) {
    var result = [];
    var count = 0;
    var xs = list;
    while (xs !== emptyList) {
      result[count++] = xs.head;
      xs = xs.tail;
    }
    return result;
  }

  return function (foldr) {
    return function (xs) {
      return listToArray(foldr(curryCons)(emptyList)(xs));
    };
  };
})();

//------------------------------------------------------------------------------
// Array size ------------------------------------------------------------------
//------------------------------------------------------------------------------

exports.length = function (xs) {
  return xs.length;
};

//------------------------------------------------------------------------------
// Extending arrays ------------------------------------------------------------
//------------------------------------------------------------------------------

exports.cons = function (e) {
  return function (l) {
    return [e].concat(l);
  };
};

exports.snoc = function (l) {
  return function (e) {
    var l1 = l.slice();
    l1.push(e);
    return l1;
  };
};

//------------------------------------------------------------------------------
// Non-indexed reads -----------------------------------------------------------
//------------------------------------------------------------------------------

exports["uncons'"] = function (empty) {
  return function (next) {
    return function (xs) {
      return xs.length === 0 ? empty({}) : next(xs[0])(xs.slice(1));
    };
  };
};

//------------------------------------------------------------------------------
// Indexed operations ----------------------------------------------------------
//------------------------------------------------------------------------------

exports.indexImpl = function (just) {
  return function (nothing) {
    return function (xs) {
      return function (i) {
        return i < 0 || i >= xs.length ? nothing :  just(xs[i]);
      };
    };
  };
};

exports.findIndexImpl = function (just) {
  return function (nothing) {
    return function (f) {
      return function (xs) {
        for (var i = 0, l = xs.length; i < l; i++) {
          if (f(xs[i])) return just(i);
        }
        return nothing;
      };
    };
  };
};

exports.findLastIndexImpl = function (just) {
  return function (nothing) {
    return function (f) {
      return function (xs) {
        for (var i = xs.length - 1; i >= 0; i--) {
          if (f(xs[i])) return just(i);
        }
        return nothing;
      };
    };
  };
};

exports._insertAt = function (just) {
  return function (nothing) {
    return function (i) {
      return function (a) {
        return function (l) {
          if (i < 0 || i > l.length) return nothing;
          var l1 = l.slice();
          l1.splice(i, 0, a);
          return just(l1);
        };
      };
    };
  };
};

exports._deleteAt = function (just) {
  return function (nothing) {
    return function (i) {
      return function (l) {
        if (i < 0 || i >= l.length) return nothing;
        var l1 = l.slice();
        l1.splice(i, 1);
        return just(l1);
      };
    };
  };
};

exports._updateAt = function (just) {
  return function (nothing) {
    return function (i) {
      return function (a) {
        return function (l) {
          if (i < 0 || i >= l.length) return nothing;
          var l1 = l.slice();
          l1[i] = a;
          return just(l1);
        };
      };
    };
  };
};

//------------------------------------------------------------------------------
// Transformations -------------------------------------------------------------
//------------------------------------------------------------------------------

exports.reverse = function (l) {
  return l.slice().reverse();
};

exports.concat = function (xss) {
  if (xss.length <= 10000) {
    // This method is faster, but it crashes on big arrays.
    // So we use it when can and fallback to simple variant otherwise.
    return Array.prototype.concat.apply([], xss);
  }

  var result = [];
  for (var i = 0, l = xss.length; i < l; i++) {
    var xs = xss[i];
    for (var j = 0, m = xs.length; j < m; j++) {
      result.push(xs[j]);
    }
  }
  return result;
};

exports.filter = function (f) {
  return function (xs) {
    return xs.filter(f);
  };
};

exports.partition = function (f) {
  return function (xs) {
    var yes = [];
    var no  = [];
    for (var i = 0; i < xs.length; i++) {
      var x = xs[i];
      if (f(x))
        yes.push(x);
      else
        no.push(x);
    }
    return { yes: yes, no: no };
  };
};

//------------------------------------------------------------------------------
// Sorting ---------------------------------------------------------------------
//------------------------------------------------------------------------------

exports.sortImpl = function (f) {
  return function (l) {
    return l.slice().sort(function (x, y) {
      return f(x)(y);
    });
  };
};

//------------------------------------------------------------------------------
// Subarrays -------------------------------------------------------------------
//------------------------------------------------------------------------------

exports.slice = function (s) {
  return function (e) {
    return function (l) {
      return l.slice(s, e);
    };
  };
};

exports.take = function (n) {
  return function (l) {
    return n < 1 ? [] : l.slice(0, n);
  };
};

exports.drop = function (n) {
  return function (l) {
    return n < 1 ? l : l.slice(n);
  };
};

//------------------------------------------------------------------------------
// Zipping ---------------------------------------------------------------------
//------------------------------------------------------------------------------

exports.zipWith = function (f) {
  return function (xs) {
    return function (ys) {
      var l = xs.length < ys.length ? xs.length : ys.length;
      var result = new Array(l);
      for (var i = 0; i < l; i++) {
        result[i] = f(xs[i])(ys[i]);
      }
      return result;
    };
  };
};

//------------------------------------------------------------------------------
// Partial ---------------------------------------------------------------------
//------------------------------------------------------------------------------

exports.unsafeIndexImpl = function (xs) {
  return function (n) {
    return xs[n];
  };
};

},{}],63:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var $foreign = require("./foreign.js");
var Control_Alt = require("../Control.Alt/index.js");
var Control_Alternative = require("../Control.Alternative/index.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Category = require("../Control.Category/index.js");
var Control_Lazy = require("../Control.Lazy/index.js");
var Control_Monad_Rec_Class = require("../Control.Monad.Rec.Class/index.js");
var Control_Monad_ST = require("../Control.Monad.ST/index.js");
var Control_Monad_ST_Internal = require("../Control.Monad.ST.Internal/index.js");
var Control_Semigroupoid = require("../Control.Semigroupoid/index.js");
var Data_Array_NonEmpty_Internal = require("../Data.Array.NonEmpty.Internal/index.js");
var Data_Array_ST = require("../Data.Array.ST/index.js");
var Data_Array_ST_Iterator = require("../Data.Array.ST.Iterator/index.js");
var Data_Boolean = require("../Data.Boolean/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Ordering = require("../Data.Ordering/index.js");
var Data_Ring = require("../Data.Ring/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Semiring = require("../Data.Semiring/index.js");
var Data_Traversable = require("../Data.Traversable/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var Data_Unfoldable = require("../Data.Unfoldable/index.js");
var Partial_Unsafe = require("../Partial.Unsafe/index.js");
var Prelude = require("../Prelude/index.js");
var Unsafe_Coerce = require("../Unsafe.Coerce/index.js");
var zipWithA = function (dictApplicative) {
    return function (f) {
        return function (xs) {
            return function (ys) {
                return Data_Traversable.sequence(Data_Traversable.traversableArray)(dictApplicative)($foreign.zipWith(f)(xs)(ys));
            };
        };
    };
};
var zip = $foreign.zipWith(Data_Tuple.Tuple.create);
var updateAtIndices = function (dictFoldable) {
    return function (us) {
        return function (xs) {
            return Data_Array_ST.withArray(function (res) {
                return Data_Foldable.traverse_(Control_Monad_ST_Internal.applicativeST)(dictFoldable)(function (v) {
                    return Data_Array_ST.poke(v.value0)(v.value1)(res);
                })(us);
            })(xs)();
        };
    };
};
var updateAt = $foreign["_updateAt"](Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var unsafeIndex = function (dictPartial) {
    return $foreign.unsafeIndexImpl;
};
var uncons = $foreign["uncons'"](Data_Function["const"](Data_Maybe.Nothing.value))(function (x) {
    return function (xs) {
        return new Data_Maybe.Just({
            head: x,
            tail: xs
        });
    };
});
var toUnfoldable = function (dictUnfoldable) {
    return function (xs) {
        var len = $foreign.length(xs);
        var f = function (i) {
            if (i < len) {
                return new Data_Maybe.Just(new Data_Tuple.Tuple(unsafeIndex()(xs)(i), i + 1 | 0));
            };
            if (Data_Boolean.otherwise) {
                return Data_Maybe.Nothing.value;
            };
            throw new Error("Failed pattern match at Data.Array (line 143, column 3 - line 145, column 26): " + [ i.constructor.name ]);
        };
        return Data_Unfoldable.unfoldr(dictUnfoldable)(f)(0);
    };
};
var takeEnd = function (n) {
    return function (xs) {
        return $foreign.drop($foreign.length(xs) - n | 0)(xs);
    };
};
var tail = $foreign["uncons'"](Data_Function["const"](Data_Maybe.Nothing.value))(function (v) {
    return function (xs) {
        return new Data_Maybe.Just(xs);
    };
});
var sortBy = function (comp) {
    return function (xs) {
        var comp$prime = function (x) {
            return function (y) {
                var v = comp(x)(y);
                if (v instanceof Data_Ordering.GT) {
                    return 1;
                };
                if (v instanceof Data_Ordering.EQ) {
                    return 0;
                };
                if (v instanceof Data_Ordering.LT) {
                    return -1 | 0;
                };
                throw new Error("Failed pattern match at Data.Array (line 702, column 15 - line 707, column 1): " + [ v.constructor.name ]);
            };
        };
        return $foreign.sortImpl(comp$prime)(xs);
    };
};
var sortWith = function (dictOrd) {
    return function (f) {
        return sortBy(Data_Ord.comparing(dictOrd)(f));
    };
};
var sort = function (dictOrd) {
    return function (xs) {
        return sortBy(Data_Ord.compare(dictOrd))(xs);
    };
};
var singleton = function (a) {
    return [ a ];
};
var $$null = function (xs) {
    return $foreign.length(xs) === 0;
};
var nubByEq = function (eq) {
    return function (xs) {
        var v = uncons(xs);
        if (v instanceof Data_Maybe.Just) {
            return $foreign.cons(v.value0.head)(nubByEq(eq)($foreign.filter(function (y) {
                return !eq(v.value0.head)(y);
            })(v.value0.tail)));
        };
        if (v instanceof Data_Maybe.Nothing) {
            return [  ];
        };
        throw new Error("Failed pattern match at Data.Array (line 930, column 3 - line 932, column 18): " + [ v.constructor.name ]);
    };
};
var nubEq = function (dictEq) {
    return nubByEq(Data_Eq.eq(dictEq));
};
var modifyAtIndices = function (dictFoldable) {
    return function (is) {
        return function (f) {
            return function (xs) {
                return Data_Array_ST.withArray(function (res) {
                    return Data_Foldable.traverse_(Control_Monad_ST_Internal.applicativeST)(dictFoldable)(function (i) {
                        return Data_Array_ST.modify(i)(f)(res);
                    })(is);
                })(xs)();
            };
        };
    };
};
var mapWithIndex = function (f) {
    return function (xs) {
        return $foreign.zipWith(f)($foreign.range(0)($foreign.length(xs) - 1 | 0))(xs);
    };
};
var some = function (dictAlternative) {
    return function (dictLazy) {
        return function (v) {
            return Control_Apply.apply((dictAlternative.Applicative0()).Apply0())(Data_Functor.map(((dictAlternative.Plus1()).Alt0()).Functor0())($foreign.cons)(v))(Control_Lazy.defer(dictLazy)(function (v1) {
                return many(dictAlternative)(dictLazy)(v);
            }));
        };
    };
};
var many = function (dictAlternative) {
    return function (dictLazy) {
        return function (v) {
            return Control_Alt.alt((dictAlternative.Plus1()).Alt0())(some(dictAlternative)(dictLazy)(v))(Control_Applicative.pure(dictAlternative.Applicative0())([  ]));
        };
    };
};
var insertAt = $foreign["_insertAt"](Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var init = function (xs) {
    if ($$null(xs)) {
        return Data_Maybe.Nothing.value;
    };
    if (Data_Boolean.otherwise) {
        return new Data_Maybe.Just($foreign.slice(0)($foreign.length(xs) - 1 | 0)(xs));
    };
    throw new Error("Failed pattern match at Data.Array (line 323, column 1 - line 323, column 45): " + [ xs.constructor.name ]);
};
var index = $foreign.indexImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var last = function (xs) {
    return index(xs)($foreign.length(xs) - 1 | 0);
};
var unsnoc = function (xs) {
    return Control_Apply.apply(Data_Maybe.applyMaybe)(Data_Functor.map(Data_Maybe.functorMaybe)(function (v) {
        return function (v1) {
            return {
                init: v,
                last: v1
            };
        };
    })(init(xs)))(last(xs));
};
var modifyAt = function (i) {
    return function (f) {
        return function (xs) {
            var go = function (x) {
                return updateAt(i)(f(x))(xs);
            };
            return Data_Maybe.maybe(Data_Maybe.Nothing.value)(go)(index(xs)(i));
        };
    };
};
var span = function (p) {
    return function (arr) {
        var go = function ($copy_i) {
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(i) {
                var v = index(arr)(i);
                if (v instanceof Data_Maybe.Just) {
                    var $75 = p(v.value0);
                    if ($75) {
                        $copy_i = i + 1 | 0;
                        return;
                    };
                    $tco_done = true;
                    return new Data_Maybe.Just(i);
                };
                if (v instanceof Data_Maybe.Nothing) {
                    $tco_done = true;
                    return Data_Maybe.Nothing.value;
                };
                throw new Error("Failed pattern match at Data.Array (line 834, column 5 - line 836, column 25): " + [ v.constructor.name ]);
            };
            while (!$tco_done) {
                $tco_result = $tco_loop($copy_i);
            };
            return $tco_result;
        };
        var breakIndex = go(0);
        if (breakIndex instanceof Data_Maybe.Just && breakIndex.value0 === 0) {
            return {
                init: [  ],
                rest: arr
            };
        };
        if (breakIndex instanceof Data_Maybe.Just) {
            return {
                init: $foreign.slice(0)(breakIndex.value0)(arr),
                rest: $foreign.slice(breakIndex.value0)($foreign.length(arr))(arr)
            };
        };
        if (breakIndex instanceof Data_Maybe.Nothing) {
            return {
                init: arr,
                rest: [  ]
            };
        };
        throw new Error("Failed pattern match at Data.Array (line 821, column 3 - line 827, column 30): " + [ breakIndex.constructor.name ]);
    };
};
var takeWhile = function (p) {
    return function (xs) {
        return (span(p)(xs)).init;
    };
};
var unzip = function (xs) {
    return (function __do() {
        var v = Data_Array_ST.empty();
        var v1 = Data_Array_ST.empty();
        var v2 = Data_Array_ST_Iterator.iterator(function (v2) {
            return index(xs)(v2);
        })();
        Data_Array_ST_Iterator.iterate(v2)(function (v3) {
            return function __do() {
                Data_Functor["void"](Control_Monad_ST_Internal.functorST)(Data_Array_ST.push(v3.value0)(v))();
                return Data_Functor["void"](Control_Monad_ST_Internal.functorST)(Data_Array_ST.push(v3.value1)(v1))();
            };
        })();
        var v3 = Data_Array_ST.unsafeFreeze(v)();
        var v4 = Data_Array_ST.unsafeFreeze(v1)();
        return new Data_Tuple.Tuple(v3, v4);
    })();
};
var head = function (xs) {
    return index(xs)(0);
};
var nubBy = function (comp) {
    return function (xs) {
        var indexedAndSorted = sortBy(function (x) {
            return function (y) {
                return comp(Data_Tuple.snd(x))(Data_Tuple.snd(y));
            };
        })(mapWithIndex(Data_Tuple.Tuple.create)(xs));
        var v = head(indexedAndSorted);
        if (v instanceof Data_Maybe.Nothing) {
            return [  ];
        };
        if (v instanceof Data_Maybe.Just) {
            return Data_Functor.map(Data_Functor.functorArray)(Data_Tuple.snd)(sortWith(Data_Ord.ordInt)(Data_Tuple.fst)((function __do() {
                var v1 = Data_Array_ST.unsafeThaw(singleton(v.value0))();
                Control_Monad_ST_Internal.foreach(indexedAndSorted)(function (v2) {
                    return function __do() {
                        var v3 = Data_Functor.map(Control_Monad_ST_Internal.functorST)(function ($111) {
                            return Data_Tuple.snd((function ($112) {
                                return Data_Maybe.fromJust()(last($112));
                            })($111));
                        })(Data_Array_ST.unsafeFreeze(v1))();
                        return Control_Applicative.when(Control_Monad_ST_Internal.applicativeST)(Data_Eq.notEq(Data_Ordering.eqOrdering)(comp(v3)(v2.value1))(Data_Ordering.EQ.value))(Data_Functor["void"](Control_Monad_ST_Internal.functorST)(Data_Array_ST.push(v2)(v1)))();
                    };
                })();
                return Data_Array_ST.unsafeFreeze(v1)();
            })()));
        };
        throw new Error("Failed pattern match at Data.Array (line 903, column 17 - line 911, column 29): " + [ v.constructor.name ]);
    };
};
var nub = function (dictOrd) {
    return nubBy(Data_Ord.compare(dictOrd));
};
var groupBy = function (op) {
    return function (xs) {
        return (function __do() {
            var v = Data_Array_ST.empty();
            var v1 = Data_Array_ST_Iterator.iterator(function (v1) {
                return index(xs)(v1);
            })();
            Data_Array_ST_Iterator.iterate(v1)(function (x) {
                return Data_Functor["void"](Control_Monad_ST_Internal.functorST)(function __do() {
                    var v2 = Data_Array_ST.empty();
                    var v3 = Data_Array_ST.push(x)(v2)();
                    Data_Array_ST_Iterator.pushWhile(op(x))(v1)(v2)();
                    var v4 = Data_Array_ST.unsafeFreeze(v2)();
                    return Data_Array_ST.push(v4)(v)();
                });
            })();
            return Data_Array_ST.unsafeFreeze(v)();
        })();
    };
};
var group = function (dictEq) {
    return function (xs) {
        return groupBy(Data_Eq.eq(dictEq))(xs);
    };
};
var group$prime = function (dictOrd) {
    return function ($113) {
        return group(dictOrd.Eq0())(sort(dictOrd)($113));
    };
};
var fromFoldable = function (dictFoldable) {
    return $foreign.fromFoldableImpl(Data_Foldable.foldr(dictFoldable));
};
var foldRecM = function (dictMonadRec) {
    return function (f) {
        return function (a) {
            return function (array) {
                var go = function (res) {
                    return function (i) {
                        if (i >= $foreign.length(array)) {
                            return Control_Applicative.pure((dictMonadRec.Monad0()).Applicative0())(new Control_Monad_Rec_Class.Done(res));
                        };
                        if (Data_Boolean.otherwise) {
                            return Control_Bind.bind((dictMonadRec.Monad0()).Bind1())(f(res)(unsafeIndex()(array)(i)))(function (v) {
                                return Control_Applicative.pure((dictMonadRec.Monad0()).Applicative0())(new Control_Monad_Rec_Class.Loop({
                                    a: v,
                                    b: i + 1 | 0
                                }));
                            });
                        };
                        throw new Error("Failed pattern match at Data.Array (line 1099, column 3 - line 1103, column 42): " + [ res.constructor.name, i.constructor.name ]);
                    };
                };
                return Control_Monad_Rec_Class.tailRecM2(dictMonadRec)(go)(a)(0);
            };
        };
    };
};
var foldM = function (dictMonad) {
    return function (f) {
        return function (a) {
            return $foreign["uncons'"](function (v) {
                return Control_Applicative.pure(dictMonad.Applicative0())(a);
            })(function (b) {
                return function (bs) {
                    return Control_Bind.bind(dictMonad.Bind1())(f(a)(b))(function (a$prime) {
                        return foldM(dictMonad)(f)(a$prime)(bs);
                    });
                };
            });
        };
    };
};
var findLastIndex = $foreign.findLastIndexImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var insertBy = function (cmp) {
    return function (x) {
        return function (ys) {
            var i = Data_Maybe.maybe(0)(function (v) {
                return v + 1 | 0;
            })(findLastIndex(function (y) {
                return Data_Eq.eq(Data_Ordering.eqOrdering)(cmp(x)(y))(Data_Ordering.GT.value);
            })(ys));
            return Data_Maybe.fromJust()(insertAt(i)(x)(ys));
        };
    };
};
var insert = function (dictOrd) {
    return insertBy(Data_Ord.compare(dictOrd));
};
var findIndex = $foreign.findIndexImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var intersectBy = function (eq) {
    return function (xs) {
        return function (ys) {
            return $foreign.filter(function (x) {
                return Data_Maybe.isJust(findIndex(eq(x))(ys));
            })(xs);
        };
    };
};
var intersect = function (dictEq) {
    return intersectBy(Data_Eq.eq(dictEq));
};
var elemLastIndex = function (dictEq) {
    return function (x) {
        return findLastIndex(function (v) {
            return Data_Eq.eq(dictEq)(v)(x);
        });
    };
};
var elemIndex = function (dictEq) {
    return function (x) {
        return findIndex(function (v) {
            return Data_Eq.eq(dictEq)(v)(x);
        });
    };
};
var dropWhile = function (p) {
    return function (xs) {
        return (span(p)(xs)).rest;
    };
};
var dropEnd = function (n) {
    return function (xs) {
        return $foreign.take($foreign.length(xs) - n | 0)(xs);
    };
};
var deleteAt = $foreign["_deleteAt"](Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var deleteBy = function (v) {
    return function (v1) {
        return function (v2) {
            if (v2.length === 0) {
                return [  ];
            };
            return Data_Maybe.maybe(v2)(function (i) {
                return Data_Maybe.fromJust()(deleteAt(i)(v2));
            })(findIndex(v(v1))(v2));
        };
    };
};
var unionBy = function (eq) {
    return function (xs) {
        return function (ys) {
            return Data_Semigroup.append(Data_Semigroup.semigroupArray)(xs)(Data_Foldable.foldl(Data_Foldable.foldableArray)(Data_Function.flip(deleteBy(eq)))(nubByEq(eq)(ys))(xs));
        };
    };
};
var union = function (dictEq) {
    return unionBy(Data_Eq.eq(dictEq));
};
var $$delete = function (dictEq) {
    return deleteBy(Data_Eq.eq(dictEq));
};
var difference = function (dictEq) {
    return Data_Foldable.foldr(Data_Foldable.foldableArray)($$delete(dictEq));
};
var concatMap = Data_Function.flip(Control_Bind.bind(Control_Bind.bindArray));
var mapMaybe = function (f) {
    return concatMap(function ($114) {
        return Data_Maybe.maybe([  ])(singleton)(f($114));
    });
};
var filterA = function (dictApplicative) {
    return function (p) {
        return function ($115) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(mapMaybe(function (v) {
                if (v.value1) {
                    return new Data_Maybe.Just(v.value0);
                };
                return Data_Maybe.Nothing.value;
            }))(Data_Traversable.traverse(Data_Traversable.traversableArray)(dictApplicative)(function (x) {
                return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Tuple.Tuple.create(x))(p(x));
            })($115));
        };
    };
};
var catMaybes = mapMaybe(Control_Category.identity(Control_Category.categoryFn));
var alterAt = function (i) {
    return function (f) {
        return function (xs) {
            var go = function (x) {
                var v = f(x);
                if (v instanceof Data_Maybe.Nothing) {
                    return deleteAt(i)(xs);
                };
                if (v instanceof Data_Maybe.Just) {
                    return updateAt(i)(v.value0)(xs);
                };
                throw new Error("Failed pattern match at Data.Array (line 544, column 10 - line 546, column 32): " + [ v.constructor.name ]);
            };
            return Data_Maybe.maybe(Data_Maybe.Nothing.value)(go)(index(xs)(i));
        };
    };
};
module.exports = {
    fromFoldable: fromFoldable,
    toUnfoldable: toUnfoldable,
    singleton: singleton,
    some: some,
    many: many,
    "null": $$null,
    insert: insert,
    insertBy: insertBy,
    head: head,
    last: last,
    tail: tail,
    init: init,
    uncons: uncons,
    unsnoc: unsnoc,
    index: index,
    elemIndex: elemIndex,
    elemLastIndex: elemLastIndex,
    findIndex: findIndex,
    findLastIndex: findLastIndex,
    insertAt: insertAt,
    deleteAt: deleteAt,
    updateAt: updateAt,
    updateAtIndices: updateAtIndices,
    modifyAt: modifyAt,
    modifyAtIndices: modifyAtIndices,
    alterAt: alterAt,
    concatMap: concatMap,
    filterA: filterA,
    mapMaybe: mapMaybe,
    catMaybes: catMaybes,
    mapWithIndex: mapWithIndex,
    sort: sort,
    sortBy: sortBy,
    sortWith: sortWith,
    takeEnd: takeEnd,
    takeWhile: takeWhile,
    dropEnd: dropEnd,
    dropWhile: dropWhile,
    span: span,
    group: group,
    "group'": group$prime,
    groupBy: groupBy,
    nub: nub,
    nubEq: nubEq,
    nubBy: nubBy,
    nubByEq: nubByEq,
    union: union,
    unionBy: unionBy,
    "delete": $$delete,
    deleteBy: deleteBy,
    difference: difference,
    intersect: intersect,
    intersectBy: intersectBy,
    zipWithA: zipWithA,
    zip: zip,
    unzip: unzip,
    foldM: foldM,
    foldRecM: foldRecM,
    unsafeIndex: unsafeIndex,
    range: $foreign.range,
    replicate: $foreign.replicate,
    length: $foreign.length,
    cons: $foreign.cons,
    snoc: $foreign.snoc,
    reverse: $foreign.reverse,
    concat: $foreign.concat,
    filter: $foreign.filter,
    partition: $foreign.partition,
    slice: $foreign.slice,
    take: $foreign.take,
    drop: $foreign.drop,
    zipWith: $foreign.zipWith
};

},{"../Control.Alt/index.js":25,"../Control.Alternative/index.js":26,"../Control.Applicative/index.js":27,"../Control.Apply/index.js":29,"../Control.Bind/index.js":33,"../Control.Category/index.js":34,"../Control.Lazy/index.js":38,"../Control.Monad.Rec.Class/index.js":44,"../Control.Monad.ST.Internal/index.js":46,"../Control.Monad.ST/index.js":48,"../Control.Semigroupoid/index.js":56,"../Data.Array.NonEmpty.Internal/index.js":58,"../Data.Array.ST.Iterator/index.js":59,"../Data.Array.ST/index.js":61,"../Data.Boolean/index.js":73,"../Data.Eq/index.js":82,"../Data.Foldable/index.js":87,"../Data.Function/index.js":91,"../Data.Functor/index.js":95,"../Data.HeytingAlgebra/index.js":99,"../Data.Maybe/index.js":110,"../Data.Ord/index.js":130,"../Data.Ordering/index.js":131,"../Data.Ring/index.js":133,"../Data.Semigroup/index.js":139,"../Data.Semiring/index.js":141,"../Data.Traversable/index.js":154,"../Data.Tuple/index.js":156,"../Data.Unfoldable/index.js":158,"../Partial.Unsafe/index.js":195,"../Prelude/index.js":198,"../Unsafe.Coerce/index.js":211,"./foreign.js":62}],64:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
module.exports = {};

},{}],65:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Category = require("../Control.Category/index.js");
var Control_Semigroupoid = require("../Control.Semigroupoid/index.js");
var Data_Bifunctor_Clown = require("../Data.Bifunctor.Clown/index.js");
var Data_Bifunctor_Flip = require("../Data.Bifunctor.Flip/index.js");
var Data_Bifunctor_Joker = require("../Data.Bifunctor.Joker/index.js");
var Data_Bifunctor_Product = require("../Data.Bifunctor.Product/index.js");
var Data_Bifunctor_Wrap = require("../Data.Bifunctor.Wrap/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Monoid = require("../Data.Monoid/index.js");
var Data_Monoid_Conj = require("../Data.Monoid.Conj/index.js");
var Data_Monoid_Disj = require("../Data.Monoid.Disj/index.js");
var Data_Monoid_Dual = require("../Data.Monoid.Dual/index.js");
var Data_Monoid_Endo = require("../Data.Monoid.Endo/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Prelude = require("../Prelude/index.js");
var Bifoldable = function (bifoldMap, bifoldl, bifoldr) {
    this.bifoldMap = bifoldMap;
    this.bifoldl = bifoldl;
    this.bifoldr = bifoldr;
};
var bifoldr = function (dict) {
    return dict.bifoldr;
};
var bitraverse_ = function (dictBifoldable) {
    return function (dictApplicative) {
        return function (f) {
            return function (g) {
                return bifoldr(dictBifoldable)(function ($97) {
                    return Control_Apply.applySecond(dictApplicative.Apply0())(f($97));
                })(function ($98) {
                    return Control_Apply.applySecond(dictApplicative.Apply0())(g($98));
                })(Control_Applicative.pure(dictApplicative)(Data_Unit.unit));
            };
        };
    };
};
var bifor_ = function (dictBifoldable) {
    return function (dictApplicative) {
        return function (t) {
            return function (f) {
                return function (g) {
                    return bitraverse_(dictBifoldable)(dictApplicative)(f)(g)(t);
                };
            };
        };
    };
};
var bisequence_ = function (dictBifoldable) {
    return function (dictApplicative) {
        return bitraverse_(dictBifoldable)(dictApplicative)(Control_Category.identity(Control_Category.categoryFn))(Control_Category.identity(Control_Category.categoryFn));
    };
};
var bifoldl = function (dict) {
    return dict.bifoldl;
};
var bifoldableJoker = function (dictFoldable) {
    return new Bifoldable(function (dictMonoid) {
        return function (v) {
            return function (r) {
                return function (v1) {
                    return Data_Foldable.foldMap(dictFoldable)(dictMonoid)(r)(v1);
                };
            };
        };
    }, function (v) {
        return function (r) {
            return function (u) {
                return function (v1) {
                    return Data_Foldable.foldl(dictFoldable)(r)(u)(v1);
                };
            };
        };
    }, function (v) {
        return function (r) {
            return function (u) {
                return function (v1) {
                    return Data_Foldable.foldr(dictFoldable)(r)(u)(v1);
                };
            };
        };
    });
};
var bifoldableClown = function (dictFoldable) {
    return new Bifoldable(function (dictMonoid) {
        return function (l) {
            return function (v) {
                return function (v1) {
                    return Data_Foldable.foldMap(dictFoldable)(dictMonoid)(l)(v1);
                };
            };
        };
    }, function (l) {
        return function (v) {
            return function (u) {
                return function (v1) {
                    return Data_Foldable.foldl(dictFoldable)(l)(u)(v1);
                };
            };
        };
    }, function (l) {
        return function (v) {
            return function (u) {
                return function (v1) {
                    return Data_Foldable.foldr(dictFoldable)(l)(u)(v1);
                };
            };
        };
    });
};
var bifoldMapDefaultR = function (dictBifoldable) {
    return function (dictMonoid) {
        return function (f) {
            return function (g) {
                return bifoldr(dictBifoldable)(function ($99) {
                    return Data_Semigroup.append(dictMonoid.Semigroup0())(f($99));
                })(function ($100) {
                    return Data_Semigroup.append(dictMonoid.Semigroup0())(g($100));
                })(Data_Monoid.mempty(dictMonoid));
            };
        };
    };
};
var bifoldMapDefaultL = function (dictBifoldable) {
    return function (dictMonoid) {
        return function (f) {
            return function (g) {
                return bifoldl(dictBifoldable)(function (m) {
                    return function (a) {
                        return Data_Semigroup.append(dictMonoid.Semigroup0())(m)(f(a));
                    };
                })(function (m) {
                    return function (b) {
                        return Data_Semigroup.append(dictMonoid.Semigroup0())(m)(g(b));
                    };
                })(Data_Monoid.mempty(dictMonoid));
            };
        };
    };
};
var bifoldMap = function (dict) {
    return dict.bifoldMap;
};
var bifoldableFlip = function (dictBifoldable) {
    return new Bifoldable(function (dictMonoid) {
        return function (r) {
            return function (l) {
                return function (v) {
                    return bifoldMap(dictBifoldable)(dictMonoid)(l)(r)(v);
                };
            };
        };
    }, function (r) {
        return function (l) {
            return function (u) {
                return function (v) {
                    return bifoldl(dictBifoldable)(l)(r)(u)(v);
                };
            };
        };
    }, function (r) {
        return function (l) {
            return function (u) {
                return function (v) {
                    return bifoldr(dictBifoldable)(l)(r)(u)(v);
                };
            };
        };
    });
};
var bifoldableWrap = function (dictBifoldable) {
    return new Bifoldable(function (dictMonoid) {
        return function (l) {
            return function (r) {
                return function (v) {
                    return bifoldMap(dictBifoldable)(dictMonoid)(l)(r)(v);
                };
            };
        };
    }, function (l) {
        return function (r) {
            return function (u) {
                return function (v) {
                    return bifoldl(dictBifoldable)(l)(r)(u)(v);
                };
            };
        };
    }, function (l) {
        return function (r) {
            return function (u) {
                return function (v) {
                    return bifoldr(dictBifoldable)(l)(r)(u)(v);
                };
            };
        };
    });
};
var bifoldlDefault = function (dictBifoldable) {
    return function (f) {
        return function (g) {
            return function (z) {
                return function (p) {
                    return Data_Newtype.unwrap(Data_Newtype.newtypeEndo)(Data_Newtype.unwrap(Data_Newtype.newtypeDual)(bifoldMap(dictBifoldable)(Data_Monoid_Dual.monoidDual(Data_Monoid_Endo.monoidEndo(Control_Category.categoryFn)))(function ($101) {
                        return Data_Monoid_Dual.Dual(Data_Monoid_Endo.Endo(Data_Function.flip(f)($101)));
                    })(function ($102) {
                        return Data_Monoid_Dual.Dual(Data_Monoid_Endo.Endo(Data_Function.flip(g)($102)));
                    })(p)))(z);
                };
            };
        };
    };
};
var bifoldrDefault = function (dictBifoldable) {
    return function (f) {
        return function (g) {
            return function (z) {
                return function (p) {
                    return Data_Newtype.unwrap(Data_Newtype.newtypeEndo)(bifoldMap(dictBifoldable)(Data_Monoid_Endo.monoidEndo(Control_Category.categoryFn))(function ($103) {
                        return Data_Monoid_Endo.Endo(f($103));
                    })(function ($104) {
                        return Data_Monoid_Endo.Endo(g($104));
                    })(p))(z);
                };
            };
        };
    };
};
var bifoldableProduct = function (dictBifoldable) {
    return function (dictBifoldable1) {
        return new Bifoldable(function (dictMonoid) {
            return function (l) {
                return function (r) {
                    return function (v) {
                        return Data_Semigroup.append(dictMonoid.Semigroup0())(bifoldMap(dictBifoldable)(dictMonoid)(l)(r)(v.value0))(bifoldMap(dictBifoldable1)(dictMonoid)(l)(r)(v.value1));
                    };
                };
            };
        }, function (l) {
            return function (r) {
                return function (u) {
                    return function (m) {
                        return bifoldlDefault(bifoldableProduct(dictBifoldable)(dictBifoldable1))(l)(r)(u)(m);
                    };
                };
            };
        }, function (l) {
            return function (r) {
                return function (u) {
                    return function (m) {
                        return bifoldrDefault(bifoldableProduct(dictBifoldable)(dictBifoldable1))(l)(r)(u)(m);
                    };
                };
            };
        });
    };
};
var bifold = function (dictBifoldable) {
    return function (dictMonoid) {
        return bifoldMap(dictBifoldable)(dictMonoid)(Control_Category.identity(Control_Category.categoryFn))(Control_Category.identity(Control_Category.categoryFn));
    };
};
var biany = function (dictBifoldable) {
    return function (dictBooleanAlgebra) {
        return function (p) {
            return function (q) {
                return function ($105) {
                    return Data_Newtype.unwrap(Data_Newtype.newtypeDisj)(bifoldMap(dictBifoldable)(Data_Monoid_Disj.monoidDisj(dictBooleanAlgebra.HeytingAlgebra0()))(function ($106) {
                        return Data_Monoid_Disj.Disj(p($106));
                    })(function ($107) {
                        return Data_Monoid_Disj.Disj(q($107));
                    })($105));
                };
            };
        };
    };
};
var biall = function (dictBifoldable) {
    return function (dictBooleanAlgebra) {
        return function (p) {
            return function (q) {
                return function ($108) {
                    return Data_Newtype.unwrap(Data_Newtype.newtypeConj)(bifoldMap(dictBifoldable)(Data_Monoid_Conj.monoidConj(dictBooleanAlgebra.HeytingAlgebra0()))(function ($109) {
                        return Data_Monoid_Conj.Conj(p($109));
                    })(function ($110) {
                        return Data_Monoid_Conj.Conj(q($110));
                    })($108));
                };
            };
        };
    };
};
module.exports = {
    bifoldMap: bifoldMap,
    bifoldl: bifoldl,
    bifoldr: bifoldr,
    Bifoldable: Bifoldable,
    bifoldrDefault: bifoldrDefault,
    bifoldlDefault: bifoldlDefault,
    bifoldMapDefaultR: bifoldMapDefaultR,
    bifoldMapDefaultL: bifoldMapDefaultL,
    bifold: bifold,
    bitraverse_: bitraverse_,
    bifor_: bifor_,
    bisequence_: bisequence_,
    biany: biany,
    biall: biall,
    bifoldableClown: bifoldableClown,
    bifoldableJoker: bifoldableJoker,
    bifoldableFlip: bifoldableFlip,
    bifoldableProduct: bifoldableProduct,
    bifoldableWrap: bifoldableWrap
};

},{"../Control.Applicative/index.js":27,"../Control.Apply/index.js":29,"../Control.Category/index.js":34,"../Control.Semigroupoid/index.js":56,"../Data.Bifunctor.Clown/index.js":66,"../Data.Bifunctor.Flip/index.js":67,"../Data.Bifunctor.Joker/index.js":68,"../Data.Bifunctor.Product/index.js":69,"../Data.Bifunctor.Wrap/index.js":70,"../Data.Foldable/index.js":87,"../Data.Function/index.js":91,"../Data.Monoid.Conj/index.js":112,"../Data.Monoid.Disj/index.js":113,"../Data.Monoid.Dual/index.js":114,"../Data.Monoid.Endo/index.js":115,"../Data.Monoid/index.js":117,"../Data.Newtype/index.js":119,"../Data.Semigroup/index.js":139,"../Data.Unit/index.js":162,"../Prelude/index.js":198}],66:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Biapplicative = require("../Control.Biapplicative/index.js");
var Control_Biapply = require("../Control.Biapply/index.js");
var Data_Bifunctor = require("../Data.Bifunctor/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Show = require("../Data.Show/index.js");
var Prelude = require("../Prelude/index.js");
var Clown = function (x) {
    return x;
};
var showClown = function (dictShow) {
    return new Data_Show.Show(function (v) {
        return "(Clown " + (Data_Show.show(dictShow)(v) + ")");
    });
};
var ordClown = function (dictOrd) {
    return dictOrd;
};
var newtypeClown = new Data_Newtype.Newtype(function (n) {
    return n;
}, Clown);
var functorClown = new Data_Functor.Functor(function (v) {
    return function (v1) {
        return v1;
    };
});
var eqClown = function (dictEq) {
    return dictEq;
};
var bifunctorClown = function (dictFunctor) {
    return new Data_Bifunctor.Bifunctor(function (f) {
        return function (v) {
            return function (v1) {
                return Data_Functor.map(dictFunctor)(f)(v1);
            };
        };
    });
};
var biapplyClown = function (dictApply) {
    return new Control_Biapply.Biapply(function () {
        return bifunctorClown(dictApply.Functor0());
    }, function (v) {
        return function (v1) {
            return Control_Apply.apply(dictApply)(v)(v1);
        };
    });
};
var biapplicativeClown = function (dictApplicative) {
    return new Control_Biapplicative.Biapplicative(function () {
        return biapplyClown(dictApplicative.Apply0());
    }, function (a) {
        return function (v) {
            return Control_Applicative.pure(dictApplicative)(a);
        };
    });
};
module.exports = {
    Clown: Clown,
    newtypeClown: newtypeClown,
    eqClown: eqClown,
    ordClown: ordClown,
    showClown: showClown,
    functorClown: functorClown,
    bifunctorClown: bifunctorClown,
    biapplyClown: biapplyClown,
    biapplicativeClown: biapplicativeClown
};

},{"../Control.Applicative/index.js":27,"../Control.Apply/index.js":29,"../Control.Biapplicative/index.js":30,"../Control.Biapply/index.js":31,"../Data.Bifunctor/index.js":71,"../Data.Eq/index.js":82,"../Data.Functor/index.js":95,"../Data.Newtype/index.js":119,"../Data.Ord/index.js":130,"../Data.Semigroup/index.js":139,"../Data.Show/index.js":143,"../Prelude/index.js":198}],67:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Biapplicative = require("../Control.Biapplicative/index.js");
var Control_Biapply = require("../Control.Biapply/index.js");
var Data_Bifunctor = require("../Data.Bifunctor/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Show = require("../Data.Show/index.js");
var Prelude = require("../Prelude/index.js");
var Flip = function (x) {
    return x;
};
var showFlip = function (dictShow) {
    return new Data_Show.Show(function (v) {
        return "(Flip " + (Data_Show.show(dictShow)(v) + ")");
    });
};
var ordFlip = function (dictOrd) {
    return dictOrd;
};
var newtypeFlip = new Data_Newtype.Newtype(function (n) {
    return n;
}, Flip);
var functorFlip = function (dictBifunctor) {
    return new Data_Functor.Functor(function (f) {
        return function (v) {
            return Data_Bifunctor.lmap(dictBifunctor)(f)(v);
        };
    });
};
var eqFlip = function (dictEq) {
    return dictEq;
};
var bifunctorFlip = function (dictBifunctor) {
    return new Data_Bifunctor.Bifunctor(function (f) {
        return function (g) {
            return function (v) {
                return Data_Bifunctor.bimap(dictBifunctor)(g)(f)(v);
            };
        };
    });
};
var biapplyFlip = function (dictBiapply) {
    return new Control_Biapply.Biapply(function () {
        return bifunctorFlip(dictBiapply.Bifunctor0());
    }, function (v) {
        return function (v1) {
            return Control_Biapply.biapply(dictBiapply)(v)(v1);
        };
    });
};
var biapplicativeFlip = function (dictBiapplicative) {
    return new Control_Biapplicative.Biapplicative(function () {
        return biapplyFlip(dictBiapplicative.Biapply0());
    }, function (a) {
        return function (b) {
            return Control_Biapplicative.bipure(dictBiapplicative)(b)(a);
        };
    });
};
module.exports = {
    Flip: Flip,
    newtypeFlip: newtypeFlip,
    eqFlip: eqFlip,
    ordFlip: ordFlip,
    showFlip: showFlip,
    functorFlip: functorFlip,
    bifunctorFlip: bifunctorFlip,
    biapplyFlip: biapplyFlip,
    biapplicativeFlip: biapplicativeFlip
};

},{"../Control.Biapplicative/index.js":30,"../Control.Biapply/index.js":31,"../Data.Bifunctor/index.js":71,"../Data.Eq/index.js":82,"../Data.Functor/index.js":95,"../Data.Newtype/index.js":119,"../Data.Ord/index.js":130,"../Data.Semigroup/index.js":139,"../Data.Show/index.js":143,"../Prelude/index.js":198}],68:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Biapplicative = require("../Control.Biapplicative/index.js");
var Control_Biapply = require("../Control.Biapply/index.js");
var Data_Bifunctor = require("../Data.Bifunctor/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Show = require("../Data.Show/index.js");
var Prelude = require("../Prelude/index.js");
var Joker = function (x) {
    return x;
};
var showJoker = function (dictShow) {
    return new Data_Show.Show(function (v) {
        return "(Joker " + (Data_Show.show(dictShow)(v) + ")");
    });
};
var ordJoker = function (dictOrd) {
    return dictOrd;
};
var newtypeJoker = new Data_Newtype.Newtype(function (n) {
    return n;
}, Joker);
var functorJoker = function (dictFunctor) {
    return new Data_Functor.Functor(function (g) {
        return function (v) {
            return Data_Functor.map(dictFunctor)(g)(v);
        };
    });
};
var eqJoker = function (dictEq) {
    return dictEq;
};
var bifunctorJoker = function (dictFunctor) {
    return new Data_Bifunctor.Bifunctor(function (v) {
        return function (g) {
            return function (v1) {
                return Data_Functor.map(dictFunctor)(g)(v1);
            };
        };
    });
};
var biapplyJoker = function (dictApply) {
    return new Control_Biapply.Biapply(function () {
        return bifunctorJoker(dictApply.Functor0());
    }, function (v) {
        return function (v1) {
            return Control_Apply.apply(dictApply)(v)(v1);
        };
    });
};
var biapplicativeJoker = function (dictApplicative) {
    return new Control_Biapplicative.Biapplicative(function () {
        return biapplyJoker(dictApplicative.Apply0());
    }, function (v) {
        return function (b) {
            return Control_Applicative.pure(dictApplicative)(b);
        };
    });
};
module.exports = {
    Joker: Joker,
    newtypeJoker: newtypeJoker,
    eqJoker: eqJoker,
    ordJoker: ordJoker,
    showJoker: showJoker,
    functorJoker: functorJoker,
    bifunctorJoker: bifunctorJoker,
    biapplyJoker: biapplyJoker,
    biapplicativeJoker: biapplicativeJoker
};

},{"../Control.Applicative/index.js":27,"../Control.Apply/index.js":29,"../Control.Biapplicative/index.js":30,"../Control.Biapply/index.js":31,"../Data.Bifunctor/index.js":71,"../Data.Eq/index.js":82,"../Data.Functor/index.js":95,"../Data.Newtype/index.js":119,"../Data.Ord/index.js":130,"../Data.Semigroup/index.js":139,"../Data.Show/index.js":143,"../Prelude/index.js":198}],69:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Biapplicative = require("../Control.Biapplicative/index.js");
var Control_Biapply = require("../Control.Biapply/index.js");
var Data_Bifunctor = require("../Data.Bifunctor/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Ordering = require("../Data.Ordering/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Show = require("../Data.Show/index.js");
var Prelude = require("../Prelude/index.js");
var Product = (function () {
    function Product(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Product.create = function (value0) {
        return function (value1) {
            return new Product(value0, value1);
        };
    };
    return Product;
})();
var showProduct = function (dictShow) {
    return function (dictShow1) {
        return new Data_Show.Show(function (v) {
            return "(Product " + (Data_Show.show(dictShow)(v.value0) + (" " + (Data_Show.show(dictShow1)(v.value1) + ")")));
        });
    };
};
var eqProduct = function (dictEq) {
    return function (dictEq1) {
        return new Data_Eq.Eq(function (x) {
            return function (y) {
                return Data_Eq.eq(dictEq)(x.value0)(y.value0) && Data_Eq.eq(dictEq1)(x.value1)(y.value1);
            };
        });
    };
};
var ordProduct = function (dictOrd) {
    return function (dictOrd1) {
        return new Data_Ord.Ord(function () {
            return eqProduct(dictOrd.Eq0())(dictOrd1.Eq0());
        }, function (x) {
            return function (y) {
                var v = Data_Ord.compare(dictOrd)(x.value0)(y.value0);
                if (v instanceof Data_Ordering.LT) {
                    return Data_Ordering.LT.value;
                };
                if (v instanceof Data_Ordering.GT) {
                    return Data_Ordering.GT.value;
                };
                return Data_Ord.compare(dictOrd1)(x.value1)(y.value1);
            };
        });
    };
};
var bifunctorProduct = function (dictBifunctor) {
    return function (dictBifunctor1) {
        return new Data_Bifunctor.Bifunctor(function (f) {
            return function (g) {
                return function (v) {
                    return new Product(Data_Bifunctor.bimap(dictBifunctor)(f)(g)(v.value0), Data_Bifunctor.bimap(dictBifunctor1)(f)(g)(v.value1));
                };
            };
        });
    };
};
var biapplyProduct = function (dictBiapply) {
    return function (dictBiapply1) {
        return new Control_Biapply.Biapply(function () {
            return bifunctorProduct(dictBiapply.Bifunctor0())(dictBiapply1.Bifunctor0());
        }, function (v) {
            return function (v1) {
                return new Product(Control_Biapply.biapply(dictBiapply)(v.value0)(v1.value0), Control_Biapply.biapply(dictBiapply1)(v.value1)(v1.value1));
            };
        });
    };
};
var biapplicativeProduct = function (dictBiapplicative) {
    return function (dictBiapplicative1) {
        return new Control_Biapplicative.Biapplicative(function () {
            return biapplyProduct(dictBiapplicative.Biapply0())(dictBiapplicative1.Biapply0());
        }, function (a) {
            return function (b) {
                return new Product(Control_Biapplicative.bipure(dictBiapplicative)(a)(b), Control_Biapplicative.bipure(dictBiapplicative1)(a)(b));
            };
        });
    };
};
module.exports = {
    Product: Product,
    eqProduct: eqProduct,
    ordProduct: ordProduct,
    showProduct: showProduct,
    bifunctorProduct: bifunctorProduct,
    biapplyProduct: biapplyProduct,
    biapplicativeProduct: biapplicativeProduct
};

},{"../Control.Biapplicative/index.js":30,"../Control.Biapply/index.js":31,"../Data.Bifunctor/index.js":71,"../Data.Eq/index.js":82,"../Data.HeytingAlgebra/index.js":99,"../Data.Ord/index.js":130,"../Data.Ordering/index.js":131,"../Data.Semigroup/index.js":139,"../Data.Show/index.js":143,"../Prelude/index.js":198}],70:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Biapplicative = require("../Control.Biapplicative/index.js");
var Control_Biapply = require("../Control.Biapply/index.js");
var Data_Bifunctor = require("../Data.Bifunctor/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Show = require("../Data.Show/index.js");
var Prelude = require("../Prelude/index.js");
var Wrap = function (x) {
    return x;
};
var showWrap = function (dictShow) {
    return new Data_Show.Show(function (v) {
        return "(Wrap " + (Data_Show.show(dictShow)(v) + ")");
    });
};
var ordWrap = function (dictOrd) {
    return dictOrd;
};
var newtypeWrap = new Data_Newtype.Newtype(function (n) {
    return n;
}, Wrap);
var functorWrap = function (dictBifunctor) {
    return new Data_Functor.Functor(function (f) {
        return function (v) {
            return Data_Bifunctor.rmap(dictBifunctor)(f)(v);
        };
    });
};
var eqWrap = function (dictEq) {
    return dictEq;
};
var bifunctorWrap = function (dictBifunctor) {
    return new Data_Bifunctor.Bifunctor(function (f) {
        return function (g) {
            return function (v) {
                return Data_Bifunctor.bimap(dictBifunctor)(f)(g)(v);
            };
        };
    });
};
var biapplyWrap = function (dictBiapply) {
    return new Control_Biapply.Biapply(function () {
        return bifunctorWrap(dictBiapply.Bifunctor0());
    }, function (v) {
        return function (v1) {
            return Control_Biapply.biapply(dictBiapply)(v)(v1);
        };
    });
};
var biapplicativeWrap = function (dictBiapplicative) {
    return new Control_Biapplicative.Biapplicative(function () {
        return biapplyWrap(dictBiapplicative.Biapply0());
    }, function (a) {
        return function (b) {
            return Control_Biapplicative.bipure(dictBiapplicative)(a)(b);
        };
    });
};
module.exports = {
    Wrap: Wrap,
    newtypeWrap: newtypeWrap,
    eqWrap: eqWrap,
    ordWrap: ordWrap,
    showWrap: showWrap,
    functorWrap: functorWrap,
    bifunctorWrap: bifunctorWrap,
    biapplyWrap: biapplyWrap,
    biapplicativeWrap: biapplicativeWrap
};

},{"../Control.Biapplicative/index.js":30,"../Control.Biapply/index.js":31,"../Data.Bifunctor/index.js":71,"../Data.Eq/index.js":82,"../Data.Functor/index.js":95,"../Data.Newtype/index.js":119,"../Data.Ord/index.js":130,"../Data.Semigroup/index.js":139,"../Data.Show/index.js":143,"../Prelude/index.js":198}],71:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Category = require("../Control.Category/index.js");
var Bifunctor = function (bimap) {
    this.bimap = bimap;
};
var bimap = function (dict) {
    return dict.bimap;
};
var lmap = function (dictBifunctor) {
    return function (f) {
        return bimap(dictBifunctor)(f)(Control_Category.identity(Control_Category.categoryFn));
    };
};
var rmap = function (dictBifunctor) {
    return bimap(dictBifunctor)(Control_Category.identity(Control_Category.categoryFn));
};
module.exports = {
    bimap: bimap,
    Bifunctor: Bifunctor,
    lmap: lmap,
    rmap: rmap
};

},{"../Control.Category/index.js":34}],72:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Category = require("../Control.Category/index.js");
var Data_Bifoldable = require("../Data.Bifoldable/index.js");
var Data_Bifunctor = require("../Data.Bifunctor/index.js");
var Data_Bifunctor_Clown = require("../Data.Bifunctor.Clown/index.js");
var Data_Bifunctor_Flip = require("../Data.Bifunctor.Flip/index.js");
var Data_Bifunctor_Joker = require("../Data.Bifunctor.Joker/index.js");
var Data_Bifunctor_Product = require("../Data.Bifunctor.Product/index.js");
var Data_Bifunctor_Wrap = require("../Data.Bifunctor.Wrap/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Traversable = require("../Data.Traversable/index.js");
var Prelude = require("../Prelude/index.js");
var Bitraversable = function (Bifoldable1, Bifunctor0, bisequence, bitraverse) {
    this.Bifoldable1 = Bifoldable1;
    this.Bifunctor0 = Bifunctor0;
    this.bisequence = bisequence;
    this.bitraverse = bitraverse;
};
var bitraverse = function (dict) {
    return dict.bitraverse;
};
var lfor = function (dictBitraversable) {
    return function (dictApplicative) {
        return function (t) {
            return function (f) {
                return bitraverse(dictBitraversable)(dictApplicative)(f)(Control_Applicative.pure(dictApplicative))(t);
            };
        };
    };
};
var ltraverse = function (dictBitraversable) {
    return function (dictApplicative) {
        return function (f) {
            return bitraverse(dictBitraversable)(dictApplicative)(f)(Control_Applicative.pure(dictApplicative));
        };
    };
};
var rfor = function (dictBitraversable) {
    return function (dictApplicative) {
        return function (t) {
            return function (f) {
                return bitraverse(dictBitraversable)(dictApplicative)(Control_Applicative.pure(dictApplicative))(f)(t);
            };
        };
    };
};
var rtraverse = function (dictBitraversable) {
    return function (dictApplicative) {
        return bitraverse(dictBitraversable)(dictApplicative)(Control_Applicative.pure(dictApplicative));
    };
};
var bitraversableJoker = function (dictTraversable) {
    return new Bitraversable(function () {
        return Data_Bifoldable.bifoldableJoker(dictTraversable.Foldable1());
    }, function () {
        return Data_Bifunctor_Joker.bifunctorJoker(dictTraversable.Functor0());
    }, function (dictApplicative) {
        return function (v) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Bifunctor_Joker.Joker)(Data_Traversable.sequence(dictTraversable)(dictApplicative)(v));
        };
    }, function (dictApplicative) {
        return function (v) {
            return function (r) {
                return function (v1) {
                    return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Bifunctor_Joker.Joker)(Data_Traversable.traverse(dictTraversable)(dictApplicative)(r)(v1));
                };
            };
        };
    });
};
var bitraversableClown = function (dictTraversable) {
    return new Bitraversable(function () {
        return Data_Bifoldable.bifoldableClown(dictTraversable.Foldable1());
    }, function () {
        return Data_Bifunctor_Clown.bifunctorClown(dictTraversable.Functor0());
    }, function (dictApplicative) {
        return function (v) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Bifunctor_Clown.Clown)(Data_Traversable.sequence(dictTraversable)(dictApplicative)(v));
        };
    }, function (dictApplicative) {
        return function (l) {
            return function (v) {
                return function (v1) {
                    return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Bifunctor_Clown.Clown)(Data_Traversable.traverse(dictTraversable)(dictApplicative)(l)(v1));
                };
            };
        };
    });
};
var bisequenceDefault = function (dictBitraversable) {
    return function (dictApplicative) {
        return bitraverse(dictBitraversable)(dictApplicative)(Control_Category.identity(Control_Category.categoryFn))(Control_Category.identity(Control_Category.categoryFn));
    };
};
var bisequence = function (dict) {
    return dict.bisequence;
};
var bitraversableFlip = function (dictBitraversable) {
    return new Bitraversable(function () {
        return Data_Bifoldable.bifoldableFlip(dictBitraversable.Bifoldable1());
    }, function () {
        return Data_Bifunctor_Flip.bifunctorFlip(dictBitraversable.Bifunctor0());
    }, function (dictApplicative) {
        return function (v) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Bifunctor_Flip.Flip)(bisequence(dictBitraversable)(dictApplicative)(v));
        };
    }, function (dictApplicative) {
        return function (r) {
            return function (l) {
                return function (v) {
                    return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Bifunctor_Flip.Flip)(bitraverse(dictBitraversable)(dictApplicative)(l)(r)(v));
                };
            };
        };
    });
};
var bitraversableProduct = function (dictBitraversable) {
    return function (dictBitraversable1) {
        return new Bitraversable(function () {
            return Data_Bifoldable.bifoldableProduct(dictBitraversable.Bifoldable1())(dictBitraversable1.Bifoldable1());
        }, function () {
            return Data_Bifunctor_Product.bifunctorProduct(dictBitraversable.Bifunctor0())(dictBitraversable1.Bifunctor0());
        }, function (dictApplicative) {
            return function (v) {
                return Control_Apply.apply(dictApplicative.Apply0())(Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Bifunctor_Product.Product.create)(bisequence(dictBitraversable)(dictApplicative)(v.value0)))(bisequence(dictBitraversable1)(dictApplicative)(v.value1));
            };
        }, function (dictApplicative) {
            return function (l) {
                return function (r) {
                    return function (v) {
                        return Control_Apply.apply(dictApplicative.Apply0())(Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Bifunctor_Product.Product.create)(bitraverse(dictBitraversable)(dictApplicative)(l)(r)(v.value0)))(bitraverse(dictBitraversable1)(dictApplicative)(l)(r)(v.value1));
                    };
                };
            };
        });
    };
};
var bitraversableWrap = function (dictBitraversable) {
    return new Bitraversable(function () {
        return Data_Bifoldable.bifoldableWrap(dictBitraversable.Bifoldable1());
    }, function () {
        return Data_Bifunctor_Wrap.bifunctorWrap(dictBitraversable.Bifunctor0());
    }, function (dictApplicative) {
        return function (v) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Bifunctor_Wrap.Wrap)(bisequence(dictBitraversable)(dictApplicative)(v));
        };
    }, function (dictApplicative) {
        return function (l) {
            return function (r) {
                return function (v) {
                    return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Bifunctor_Wrap.Wrap)(bitraverse(dictBitraversable)(dictApplicative)(l)(r)(v));
                };
            };
        };
    });
};
var bitraverseDefault = function (dictBitraversable) {
    return function (dictApplicative) {
        return function (f) {
            return function (g) {
                return function (t) {
                    return bisequence(dictBitraversable)(dictApplicative)(Data_Bifunctor.bimap(dictBitraversable.Bifunctor0())(f)(g)(t));
                };
            };
        };
    };
};
var bifor = function (dictBitraversable) {
    return function (dictApplicative) {
        return function (t) {
            return function (f) {
                return function (g) {
                    return bitraverse(dictBitraversable)(dictApplicative)(f)(g)(t);
                };
            };
        };
    };
};
module.exports = {
    Bitraversable: Bitraversable,
    bitraverse: bitraverse,
    bisequence: bisequence,
    bitraverseDefault: bitraverseDefault,
    bisequenceDefault: bisequenceDefault,
    ltraverse: ltraverse,
    rtraverse: rtraverse,
    bifor: bifor,
    lfor: lfor,
    rfor: rfor,
    bitraversableClown: bitraversableClown,
    bitraversableJoker: bitraversableJoker,
    bitraversableFlip: bitraversableFlip,
    bitraversableProduct: bitraversableProduct,
    bitraversableWrap: bitraversableWrap
};

},{"../Control.Applicative/index.js":27,"../Control.Apply/index.js":29,"../Control.Category/index.js":34,"../Data.Bifoldable/index.js":65,"../Data.Bifunctor.Clown/index.js":66,"../Data.Bifunctor.Flip/index.js":67,"../Data.Bifunctor.Joker/index.js":68,"../Data.Bifunctor.Product/index.js":69,"../Data.Bifunctor.Wrap/index.js":70,"../Data.Bifunctor/index.js":71,"../Data.Functor/index.js":95,"../Data.Traversable/index.js":154,"../Prelude/index.js":198}],73:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var otherwise = true;
module.exports = {
    otherwise: otherwise
};

},{}],74:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra/index.js");
var Data_Symbol = require("../Data.Symbol/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var BooleanAlgebra = function (HeytingAlgebra0) {
    this.HeytingAlgebra0 = HeytingAlgebra0;
};
var BooleanAlgebraRecord = function (HeytingAlgebraRecord0) {
    this.HeytingAlgebraRecord0 = HeytingAlgebraRecord0;
};
var booleanAlgebraUnit = new BooleanAlgebra(function () {
    return Data_HeytingAlgebra.heytingAlgebraUnit;
});
var booleanAlgebraRecordNil = new BooleanAlgebraRecord(function () {
    return Data_HeytingAlgebra.heytingAlgebraRecordNil;
});
var booleanAlgebraRecordCons = function (dictIsSymbol) {
    return function (dictCons) {
        return function (dictBooleanAlgebraRecord) {
            return function (dictBooleanAlgebra) {
                return new BooleanAlgebraRecord(function () {
                    return Data_HeytingAlgebra.heytingAlgebraRecordCons(dictIsSymbol)(dictCons)(dictBooleanAlgebraRecord.HeytingAlgebraRecord0())(dictBooleanAlgebra.HeytingAlgebra0());
                });
            };
        };
    };
};
var booleanAlgebraRecord = function (dictRowToList) {
    return function (dictBooleanAlgebraRecord) {
        return new BooleanAlgebra(function () {
            return Data_HeytingAlgebra.heytingAlgebraRecord(dictRowToList)(dictBooleanAlgebraRecord.HeytingAlgebraRecord0());
        });
    };
};
var booleanAlgebraFn = function (dictBooleanAlgebra) {
    return new BooleanAlgebra(function () {
        return Data_HeytingAlgebra.heytingAlgebraFunction(dictBooleanAlgebra.HeytingAlgebra0());
    });
};
var booleanAlgebraBoolean = new BooleanAlgebra(function () {
    return Data_HeytingAlgebra.heytingAlgebraBoolean;
});
module.exports = {
    BooleanAlgebra: BooleanAlgebra,
    BooleanAlgebraRecord: BooleanAlgebraRecord,
    booleanAlgebraBoolean: booleanAlgebraBoolean,
    booleanAlgebraUnit: booleanAlgebraUnit,
    booleanAlgebraFn: booleanAlgebraFn,
    booleanAlgebraRecord: booleanAlgebraRecord,
    booleanAlgebraRecordNil: booleanAlgebraRecordNil,
    booleanAlgebraRecordCons: booleanAlgebraRecordCons
};

},{"../Data.HeytingAlgebra/index.js":99,"../Data.Symbol/index.js":150,"../Data.Unit/index.js":162}],75:[function(require,module,exports){
"use strict";

exports.topInt = 2147483647;
exports.bottomInt = -2147483648;

exports.topChar = String.fromCharCode(65535);
exports.bottomChar = String.fromCharCode(0);

exports.topNumber = Number.POSITIVE_INFINITY;
exports.bottomNumber = Number.NEGATIVE_INFINITY;

},{}],76:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var $foreign = require("./foreign.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Ordering = require("../Data.Ordering/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Bounded = function (Ord0, bottom, top) {
    this.Ord0 = Ord0;
    this.bottom = bottom;
    this.top = top;
};
var top = function (dict) {
    return dict.top;
};
var boundedUnit = new Bounded(function () {
    return Data_Ord.ordUnit;
}, Data_Unit.unit, Data_Unit.unit);
var boundedOrdering = new Bounded(function () {
    return Data_Ord.ordOrdering;
}, Data_Ordering.LT.value, Data_Ordering.GT.value);
var boundedNumber = new Bounded(function () {
    return Data_Ord.ordNumber;
}, $foreign.bottomNumber, $foreign.topNumber);
var boundedInt = new Bounded(function () {
    return Data_Ord.ordInt;
}, $foreign.bottomInt, $foreign.topInt);
var boundedChar = new Bounded(function () {
    return Data_Ord.ordChar;
}, $foreign.bottomChar, $foreign.topChar);
var boundedBoolean = new Bounded(function () {
    return Data_Ord.ordBoolean;
}, false, true);
var bottom = function (dict) {
    return dict.bottom;
};
module.exports = {
    Bounded: Bounded,
    bottom: bottom,
    top: top,
    boundedBoolean: boundedBoolean,
    boundedInt: boundedInt,
    boundedChar: boundedChar,
    boundedOrdering: boundedOrdering,
    boundedUnit: boundedUnit,
    boundedNumber: boundedNumber
};

},{"../Data.Ord/index.js":130,"../Data.Ordering/index.js":131,"../Data.Unit/index.js":162,"./foreign.js":75}],77:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Data_Ring = require("../Data.Ring/index.js");
var Data_Semiring = require("../Data.Semiring/index.js");
var Data_Symbol = require("../Data.Symbol/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var CommutativeRing = function (Ring0) {
    this.Ring0 = Ring0;
};
var CommutativeRingRecord = function (RingRecord0) {
    this.RingRecord0 = RingRecord0;
};
var commutativeRingUnit = new CommutativeRing(function () {
    return Data_Ring.ringUnit;
});
var commutativeRingRecordNil = new CommutativeRingRecord(function () {
    return Data_Ring.ringRecordNil;
});
var commutativeRingRecordCons = function (dictIsSymbol) {
    return function (dictCons) {
        return function (dictCommutativeRingRecord) {
            return function (dictCommutativeRing) {
                return new CommutativeRingRecord(function () {
                    return Data_Ring.ringRecordCons(dictIsSymbol)(dictCons)(dictCommutativeRingRecord.RingRecord0())(dictCommutativeRing.Ring0());
                });
            };
        };
    };
};
var commutativeRingRecord = function (dictRowToList) {
    return function (dictCommutativeRingRecord) {
        return new CommutativeRing(function () {
            return Data_Ring.ringRecord(dictRowToList)(dictCommutativeRingRecord.RingRecord0());
        });
    };
};
var commutativeRingNumber = new CommutativeRing(function () {
    return Data_Ring.ringNumber;
});
var commutativeRingInt = new CommutativeRing(function () {
    return Data_Ring.ringInt;
});
var commutativeRingFn = function (dictCommutativeRing) {
    return new CommutativeRing(function () {
        return Data_Ring.ringFn(dictCommutativeRing.Ring0());
    });
};
module.exports = {
    CommutativeRing: CommutativeRing,
    CommutativeRingRecord: CommutativeRingRecord,
    commutativeRingInt: commutativeRingInt,
    commutativeRingNumber: commutativeRingNumber,
    commutativeRingUnit: commutativeRingUnit,
    commutativeRingFn: commutativeRingFn,
    commutativeRingRecord: commutativeRingRecord,
    commutativeRingRecordNil: commutativeRingRecordNil,
    commutativeRingRecordCons: commutativeRingRecordCons
};

},{"../Data.Ring/index.js":133,"../Data.Semiring/index.js":141,"../Data.Symbol/index.js":150,"../Data.Unit/index.js":162}],78:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Category = require("../Control.Category/index.js");
var Control_Semigroupoid = require("../Control.Semigroupoid/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Identity = require("../Data.Identity/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Prelude = require("../Prelude/index.js");
var Distributive = function (Functor0, collect, distribute) {
    this.Functor0 = Functor0;
    this.collect = collect;
    this.distribute = distribute;
};
var distributiveIdentity = new Distributive(function () {
    return Data_Identity.functorIdentity;
}, function (dictFunctor) {
    return function (f) {
        return function ($11) {
            return Data_Identity.Identity(Data_Functor.map(dictFunctor)(function ($12) {
                return Data_Newtype.unwrap(Data_Identity.newtypeIdentity)(f($12));
            })($11));
        };
    };
}, function (dictFunctor) {
    return function ($13) {
        return Data_Identity.Identity(Data_Functor.map(dictFunctor)(Data_Newtype.unwrap(Data_Identity.newtypeIdentity))($13));
    };
});
var distribute = function (dict) {
    return dict.distribute;
};
var distributiveFunction = new Distributive(function () {
    return Data_Functor.functorFn;
}, function (dictFunctor) {
    return function (f) {
        return function ($14) {
            return distribute(distributiveFunction)(dictFunctor)(Data_Functor.map(dictFunctor)(f)($14));
        };
    };
}, function (dictFunctor) {
    return function (a) {
        return function (e) {
            return Data_Functor.map(dictFunctor)(function (v) {
                return v(e);
            })(a);
        };
    };
});
var cotraverse = function (dictDistributive) {
    return function (dictFunctor) {
        return function (f) {
            return function ($15) {
                return Data_Functor.map(dictDistributive.Functor0())(f)(distribute(dictDistributive)(dictFunctor)($15));
            };
        };
    };
};
var collectDefault = function (dictDistributive) {
    return function (dictFunctor) {
        return function (f) {
            return function ($16) {
                return distribute(dictDistributive)(dictFunctor)(Data_Functor.map(dictFunctor)(f)($16));
            };
        };
    };
};
var collect = function (dict) {
    return dict.collect;
};
var distributeDefault = function (dictDistributive) {
    return function (dictFunctor) {
        return collect(dictDistributive)(dictFunctor)(Control_Category.identity(Control_Category.categoryFn));
    };
};
module.exports = {
    collect: collect,
    distribute: distribute,
    Distributive: Distributive,
    distributeDefault: distributeDefault,
    collectDefault: collectDefault,
    cotraverse: cotraverse,
    distributiveIdentity: distributiveIdentity,
    distributiveFunction: distributiveFunction
};

},{"../Control.Category/index.js":34,"../Control.Semigroupoid/index.js":56,"../Data.Function/index.js":91,"../Data.Functor/index.js":95,"../Data.Identity/index.js":100,"../Data.Newtype/index.js":119,"../Prelude/index.js":198}],79:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Data_EuclideanRing = require("../Data.EuclideanRing/index.js");
var Data_Ring = require("../Data.Ring/index.js");
var Data_Semiring = require("../Data.Semiring/index.js");
var DivisionRing = function (Ring0, recip) {
    this.Ring0 = Ring0;
    this.recip = recip;
};
var recip = function (dict) {
    return dict.recip;
};
var rightDiv = function (dictDivisionRing) {
    return function (a) {
        return function (b) {
            return Data_Semiring.mul((dictDivisionRing.Ring0()).Semiring0())(a)(recip(dictDivisionRing)(b));
        };
    };
};
var leftDiv = function (dictDivisionRing) {
    return function (a) {
        return function (b) {
            return Data_Semiring.mul((dictDivisionRing.Ring0()).Semiring0())(recip(dictDivisionRing)(b))(a);
        };
    };
};
var divisionringNumber = new DivisionRing(function () {
    return Data_Ring.ringNumber;
}, function (x) {
    return 1.0 / x;
});
module.exports = {
    DivisionRing: DivisionRing,
    recip: recip,
    leftDiv: leftDiv,
    rightDiv: rightDiv,
    divisionringNumber: divisionringNumber
};

},{"../Data.EuclideanRing/index.js":84,"../Data.Ring/index.js":133,"../Data.Semiring/index.js":141}],80:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Alt = require("../Control.Alt/index.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Extend = require("../Control.Extend/index.js");
var Control_Monad = require("../Control.Monad/index.js");
var Control_Semigroupoid = require("../Control.Semigroupoid/index.js");
var Data_Bifoldable = require("../Data.Bifoldable/index.js");
var Data_Bifunctor = require("../Data.Bifunctor/index.js");
var Data_Bitraversable = require("../Data.Bitraversable/index.js");
var Data_Bounded = require("../Data.Bounded/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_FoldableWithIndex = require("../Data.FoldableWithIndex/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Functor_Invariant = require("../Data.Functor.Invariant/index.js");
var Data_FunctorWithIndex = require("../Data.FunctorWithIndex/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Monoid = require("../Data.Monoid/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Ordering = require("../Data.Ordering/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Show = require("../Data.Show/index.js");
var Data_Traversable = require("../Data.Traversable/index.js");
var Data_TraversableWithIndex = require("../Data.TraversableWithIndex/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Prelude = require("../Prelude/index.js");
var Left = (function () {
    function Left(value0) {
        this.value0 = value0;
    };
    Left.create = function (value0) {
        return new Left(value0);
    };
    return Left;
})();
var Right = (function () {
    function Right(value0) {
        this.value0 = value0;
    };
    Right.create = function (value0) {
        return new Right(value0);
    };
    return Right;
})();
var showEither = function (dictShow) {
    return function (dictShow1) {
        return new Data_Show.Show(function (v) {
            if (v instanceof Left) {
                return "(Left " + (Data_Show.show(dictShow)(v.value0) + ")");
            };
            if (v instanceof Right) {
                return "(Right " + (Data_Show.show(dictShow1)(v.value0) + ")");
            };
            throw new Error("Failed pattern match at Data.Either (line 163, column 1 - line 163, column 61): " + [ v.constructor.name ]);
        });
    };
};
var note$prime = function (f) {
    return Data_Maybe["maybe'"](function ($198) {
        return Left.create(f($198));
    })(Right.create);
};
var note = function (a) {
    return Data_Maybe.maybe(new Left(a))(Right.create);
};
var functorEither = new Data_Functor.Functor(function (f) {
    return function (m) {
        if (m instanceof Left) {
            return new Left(m.value0);
        };
        if (m instanceof Right) {
            return new Right(f(m.value0));
        };
        throw new Error("Failed pattern match at Data.Either (line 38, column 8 - line 38, column 52): " + [ m.constructor.name ]);
    };
});
var functorWithIndexEither = new Data_FunctorWithIndex.FunctorWithIndex(function () {
    return functorEither;
}, function (f) {
    return Data_Functor.map(functorEither)(f(Data_Unit.unit));
});
var invariantEither = new Data_Functor_Invariant.Invariant(Data_Functor_Invariant.imapF(functorEither));
var fromRight = function (dictPartial) {
    return function (v) {
        if (v instanceof Right) {
            return v.value0;
        };
        throw new Error("Failed pattern match at Data.Either (line 261, column 1 - line 261, column 52): " + [ v.constructor.name ]);
    };
};
var fromLeft = function (dictPartial) {
    return function (v) {
        if (v instanceof Left) {
            return v.value0;
        };
        throw new Error("Failed pattern match at Data.Either (line 256, column 1 - line 256, column 51): " + [ v.constructor.name ]);
    };
};
var foldableEither = new Data_Foldable.Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            if (v instanceof Left) {
                return Data_Monoid.mempty(dictMonoid);
            };
            if (v instanceof Right) {
                return f(v.value0);
            };
            throw new Error("Failed pattern match at Data.Either (line 187, column 1 - line 187, column 47): " + [ f.constructor.name, v.constructor.name ]);
        };
    };
}, function (v) {
    return function (z) {
        return function (v1) {
            if (v1 instanceof Left) {
                return z;
            };
            if (v1 instanceof Right) {
                return v(z)(v1.value0);
            };
            throw new Error("Failed pattern match at Data.Either (line 187, column 1 - line 187, column 47): " + [ v.constructor.name, z.constructor.name, v1.constructor.name ]);
        };
    };
}, function (v) {
    return function (z) {
        return function (v1) {
            if (v1 instanceof Left) {
                return z;
            };
            if (v1 instanceof Right) {
                return v(v1.value0)(z);
            };
            throw new Error("Failed pattern match at Data.Either (line 187, column 1 - line 187, column 47): " + [ v.constructor.name, z.constructor.name, v1.constructor.name ]);
        };
    };
});
var foldableWithIndexEither = new Data_FoldableWithIndex.FoldableWithIndex(function () {
    return foldableEither;
}, function (dictMonoid) {
    return function (f) {
        return function (v) {
            if (v instanceof Left) {
                return Data_Monoid.mempty(dictMonoid);
            };
            if (v instanceof Right) {
                return f(Data_Unit.unit)(v.value0);
            };
            throw new Error("Failed pattern match at Data.Either (line 195, column 1 - line 195, column 70): " + [ f.constructor.name, v.constructor.name ]);
        };
    };
}, function (v) {
    return function (z) {
        return function (v1) {
            if (v1 instanceof Left) {
                return z;
            };
            if (v1 instanceof Right) {
                return v(Data_Unit.unit)(z)(v1.value0);
            };
            throw new Error("Failed pattern match at Data.Either (line 195, column 1 - line 195, column 70): " + [ v.constructor.name, z.constructor.name, v1.constructor.name ]);
        };
    };
}, function (v) {
    return function (z) {
        return function (v1) {
            if (v1 instanceof Left) {
                return z;
            };
            if (v1 instanceof Right) {
                return v(Data_Unit.unit)(v1.value0)(z);
            };
            throw new Error("Failed pattern match at Data.Either (line 195, column 1 - line 195, column 70): " + [ v.constructor.name, z.constructor.name, v1.constructor.name ]);
        };
    };
});
var traversableEither = new Data_Traversable.Traversable(function () {
    return foldableEither;
}, function () {
    return functorEither;
}, function (dictApplicative) {
    return function (v) {
        if (v instanceof Left) {
            return Control_Applicative.pure(dictApplicative)(new Left(v.value0));
        };
        if (v instanceof Right) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Right.create)(v.value0);
        };
        throw new Error("Failed pattern match at Data.Either (line 211, column 1 - line 211, column 53): " + [ v.constructor.name ]);
    };
}, function (dictApplicative) {
    return function (v) {
        return function (v1) {
            if (v1 instanceof Left) {
                return Control_Applicative.pure(dictApplicative)(new Left(v1.value0));
            };
            if (v1 instanceof Right) {
                return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Right.create)(v(v1.value0));
            };
            throw new Error("Failed pattern match at Data.Either (line 211, column 1 - line 211, column 53): " + [ v.constructor.name, v1.constructor.name ]);
        };
    };
});
var traversableWithIndexEither = new Data_TraversableWithIndex.TraversableWithIndex(function () {
    return foldableWithIndexEither;
}, function () {
    return functorWithIndexEither;
}, function () {
    return traversableEither;
}, function (dictApplicative) {
    return function (v) {
        return function (v1) {
            if (v1 instanceof Left) {
                return Control_Applicative.pure(dictApplicative)(new Left(v1.value0));
            };
            if (v1 instanceof Right) {
                return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Right.create)(v(Data_Unit.unit)(v1.value0));
            };
            throw new Error("Failed pattern match at Data.Either (line 217, column 1 - line 217, column 76): " + [ v.constructor.name, v1.constructor.name ]);
        };
    };
});
var extendEither = new Control_Extend.Extend(function () {
    return functorEither;
}, function (v) {
    return function (v1) {
        if (v1 instanceof Left) {
            return new Left(v1.value0);
        };
        return new Right(v(v1));
    };
});
var eqEither = function (dictEq) {
    return function (dictEq1) {
        return new Data_Eq.Eq(function (x) {
            return function (y) {
                if (x instanceof Left && y instanceof Left) {
                    return Data_Eq.eq(dictEq)(x.value0)(y.value0);
                };
                if (x instanceof Right && y instanceof Right) {
                    return Data_Eq.eq(dictEq1)(x.value0)(y.value0);
                };
                return false;
            };
        });
    };
};
var ordEither = function (dictOrd) {
    return function (dictOrd1) {
        return new Data_Ord.Ord(function () {
            return eqEither(dictOrd.Eq0())(dictOrd1.Eq0());
        }, function (x) {
            return function (y) {
                if (x instanceof Left && y instanceof Left) {
                    return Data_Ord.compare(dictOrd)(x.value0)(y.value0);
                };
                if (x instanceof Left) {
                    return Data_Ordering.LT.value;
                };
                if (y instanceof Left) {
                    return Data_Ordering.GT.value;
                };
                if (x instanceof Right && y instanceof Right) {
                    return Data_Ord.compare(dictOrd1)(x.value0)(y.value0);
                };
                throw new Error("Failed pattern match at Data.Either (line 179, column 8 - line 179, column 64): " + [ x.constructor.name, y.constructor.name ]);
            };
        });
    };
};
var eq1Either = function (dictEq) {
    return new Data_Eq.Eq1(function (dictEq1) {
        return Data_Eq.eq(eqEither(dictEq)(dictEq1));
    });
};
var ord1Either = function (dictOrd) {
    return new Data_Ord.Ord1(function () {
        return eq1Either(dictOrd.Eq0());
    }, function (dictOrd1) {
        return Data_Ord.compare(ordEither(dictOrd)(dictOrd1));
    });
};
var either = function (v) {
    return function (v1) {
        return function (v2) {
            if (v2 instanceof Left) {
                return v(v2.value0);
            };
            if (v2 instanceof Right) {
                return v1(v2.value0);
            };
            throw new Error("Failed pattern match at Data.Either (line 238, column 1 - line 238, column 64): " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
        };
    };
};
var hush = either(Data_Function["const"](Data_Maybe.Nothing.value))(Data_Maybe.Just.create);
var isLeft = either(Data_Function["const"](true))(Data_Function["const"](false));
var isRight = either(Data_Function["const"](false))(Data_Function["const"](true));
var choose = function (dictAlt) {
    return function (a) {
        return function (b) {
            return Control_Alt.alt(dictAlt)(Data_Functor.map(dictAlt.Functor0())(Left.create)(a))(Data_Functor.map(dictAlt.Functor0())(Right.create)(b));
        };
    };
};
var boundedEither = function (dictBounded) {
    return function (dictBounded1) {
        return new Data_Bounded.Bounded(function () {
            return ordEither(dictBounded.Ord0())(dictBounded1.Ord0());
        }, new Left(Data_Bounded.bottom(dictBounded)), new Right(Data_Bounded.top(dictBounded1)));
    };
};
var bifunctorEither = new Data_Bifunctor.Bifunctor(function (v) {
    return function (v1) {
        return function (v2) {
            if (v2 instanceof Left) {
                return new Left(v(v2.value0));
            };
            if (v2 instanceof Right) {
                return new Right(v1(v2.value0));
            };
            throw new Error("Failed pattern match at Data.Either (line 46, column 1 - line 46, column 45): " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
        };
    };
});
var bifoldableEither = new Data_Bifoldable.Bifoldable(function (dictMonoid) {
    return function (v) {
        return function (v1) {
            return function (v2) {
                if (v2 instanceof Left) {
                    return v(v2.value0);
                };
                if (v2 instanceof Right) {
                    return v1(v2.value0);
                };
                throw new Error("Failed pattern match at Data.Either (line 203, column 1 - line 203, column 47): " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
            };
        };
    };
}, function (v) {
    return function (v1) {
        return function (z) {
            return function (v2) {
                if (v2 instanceof Left) {
                    return v(z)(v2.value0);
                };
                if (v2 instanceof Right) {
                    return v1(z)(v2.value0);
                };
                throw new Error("Failed pattern match at Data.Either (line 203, column 1 - line 203, column 47): " + [ v.constructor.name, v1.constructor.name, z.constructor.name, v2.constructor.name ]);
            };
        };
    };
}, function (v) {
    return function (v1) {
        return function (z) {
            return function (v2) {
                if (v2 instanceof Left) {
                    return v(v2.value0)(z);
                };
                if (v2 instanceof Right) {
                    return v1(v2.value0)(z);
                };
                throw new Error("Failed pattern match at Data.Either (line 203, column 1 - line 203, column 47): " + [ v.constructor.name, v1.constructor.name, z.constructor.name, v2.constructor.name ]);
            };
        };
    };
});
var bitraversableEither = new Data_Bitraversable.Bitraversable(function () {
    return bifoldableEither;
}, function () {
    return bifunctorEither;
}, function (dictApplicative) {
    return function (v) {
        if (v instanceof Left) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Left.create)(v.value0);
        };
        if (v instanceof Right) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Right.create)(v.value0);
        };
        throw new Error("Failed pattern match at Data.Either (line 221, column 1 - line 221, column 53): " + [ v.constructor.name ]);
    };
}, function (dictApplicative) {
    return function (v) {
        return function (v1) {
            return function (v2) {
                if (v2 instanceof Left) {
                    return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Left.create)(v(v2.value0));
                };
                if (v2 instanceof Right) {
                    return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Right.create)(v1(v2.value0));
                };
                throw new Error("Failed pattern match at Data.Either (line 221, column 1 - line 221, column 53): " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
            };
        };
    };
});
var applyEither = new Control_Apply.Apply(function () {
    return functorEither;
}, function (v) {
    return function (v1) {
        if (v instanceof Left) {
            return new Left(v.value0);
        };
        if (v instanceof Right) {
            return Data_Functor.map(functorEither)(v.value0)(v1);
        };
        throw new Error("Failed pattern match at Data.Either (line 82, column 1 - line 82, column 41): " + [ v.constructor.name, v1.constructor.name ]);
    };
});
var bindEither = new Control_Bind.Bind(function () {
    return applyEither;
}, either(function (e) {
    return function (v) {
        return new Left(e);
    };
})(function (a) {
    return function (f) {
        return f(a);
    };
}));
var semigroupEither = function (dictSemigroup) {
    return new Data_Semigroup.Semigroup(function (x) {
        return function (y) {
            return Control_Apply.apply(applyEither)(Data_Functor.map(functorEither)(Data_Semigroup.append(dictSemigroup))(x))(y);
        };
    });
};
var applicativeEither = new Control_Applicative.Applicative(function () {
    return applyEither;
}, Right.create);
var monadEither = new Control_Monad.Monad(function () {
    return applicativeEither;
}, function () {
    return bindEither;
});
var altEither = new Control_Alt.Alt(function () {
    return functorEither;
}, function (v) {
    return function (v1) {
        if (v instanceof Left) {
            return v1;
        };
        return v;
    };
});
module.exports = {
    Left: Left,
    Right: Right,
    either: either,
    choose: choose,
    isLeft: isLeft,
    isRight: isRight,
    fromLeft: fromLeft,
    fromRight: fromRight,
    note: note,
    "note'": note$prime,
    hush: hush,
    functorEither: functorEither,
    functorWithIndexEither: functorWithIndexEither,
    invariantEither: invariantEither,
    bifunctorEither: bifunctorEither,
    applyEither: applyEither,
    applicativeEither: applicativeEither,
    altEither: altEither,
    bindEither: bindEither,
    monadEither: monadEither,
    extendEither: extendEither,
    showEither: showEither,
    eqEither: eqEither,
    eq1Either: eq1Either,
    ordEither: ordEither,
    ord1Either: ord1Either,
    boundedEither: boundedEither,
    foldableEither: foldableEither,
    foldableWithIndexEither: foldableWithIndexEither,
    bifoldableEither: bifoldableEither,
    traversableEither: traversableEither,
    traversableWithIndexEither: traversableWithIndexEither,
    bitraversableEither: bitraversableEither,
    semigroupEither: semigroupEither
};

},{"../Control.Alt/index.js":25,"../Control.Applicative/index.js":27,"../Control.Apply/index.js":29,"../Control.Bind/index.js":33,"../Control.Extend/index.js":37,"../Control.Monad/index.js":52,"../Control.Semigroupoid/index.js":56,"../Data.Bifoldable/index.js":65,"../Data.Bifunctor/index.js":71,"../Data.Bitraversable/index.js":72,"../Data.Bounded/index.js":76,"../Data.Eq/index.js":82,"../Data.Foldable/index.js":87,"../Data.FoldableWithIndex/index.js":88,"../Data.Function/index.js":91,"../Data.Functor.Invariant/index.js":93,"../Data.Functor/index.js":95,"../Data.FunctorWithIndex/index.js":97,"../Data.Maybe/index.js":110,"../Data.Monoid/index.js":117,"../Data.Ord/index.js":130,"../Data.Ordering/index.js":131,"../Data.Semigroup/index.js":139,"../Data.Show/index.js":143,"../Data.Traversable/index.js":154,"../Data.TraversableWithIndex/index.js":155,"../Data.Unit/index.js":162,"../Prelude/index.js":198}],81:[function(require,module,exports){
"use strict";

exports.refEq = function (r1) {
  return function (r2) {
    return r1 === r2;
  };
};

exports.eqArrayImpl = function (f) {
  return function (xs) {
    return function (ys) {
      if (xs === ys) return true;
      if (xs.length !== ys.length) return false;
      for (var i = 0; i < xs.length; i++) {
        if (!f(xs[i])(ys[i])) return false;
      }
      return true;
    };
  };
};

},{}],82:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var $foreign = require("./foreign.js");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra/index.js");
var Data_Symbol = require("../Data.Symbol/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Data_Void = require("../Data.Void/index.js");
var Record_Unsafe = require("../Record.Unsafe/index.js");
var Type_Data_RowList = require("../Type.Data.RowList/index.js");
var Eq = function (eq) {
    this.eq = eq;
};
var Eq1 = function (eq1) {
    this.eq1 = eq1;
};
var EqRecord = function (eqRecord) {
    this.eqRecord = eqRecord;
};
var eqVoid = new Eq(function (v) {
    return function (v1) {
        return true;
    };
});
var eqUnit = new Eq(function (v) {
    return function (v1) {
        return true;
    };
});
var eqString = new Eq($foreign.refEq);
var eqRowNil = new EqRecord(function (v) {
    return function (v1) {
        return function (v2) {
            return true;
        };
    };
});
var eqRecord = function (dict) {
    return dict.eqRecord;
};
var eqRec = function (dictRowToList) {
    return function (dictEqRecord) {
        return new Eq(eqRecord(dictEqRecord)(Type_Data_RowList.RLProxy.value));
    };
};
var eqNumber = new Eq($foreign.refEq);
var eqInt = new Eq($foreign.refEq);
var eqChar = new Eq($foreign.refEq);
var eqBoolean = new Eq($foreign.refEq);
var eq1 = function (dict) {
    return dict.eq1;
};
var eq = function (dict) {
    return dict.eq;
};
var eqArray = function (dictEq) {
    return new Eq($foreign.eqArrayImpl(eq(dictEq)));
};
var eq1Array = new Eq1(function (dictEq) {
    return eq(eqArray(dictEq));
});
var eqRowCons = function (dictEqRecord) {
    return function (dictCons) {
        return function (dictIsSymbol) {
            return function (dictEq) {
                return new EqRecord(function (v) {
                    return function (ra) {
                        return function (rb) {
                            var tail = eqRecord(dictEqRecord)(Type_Data_RowList.RLProxy.value)(ra)(rb);
                            var key = Data_Symbol.reflectSymbol(dictIsSymbol)(Data_Symbol.SProxy.value);
                            var get = Record_Unsafe.unsafeGet(key);
                            return eq(dictEq)(get(ra))(get(rb)) && tail;
                        };
                    };
                });
            };
        };
    };
};
var notEq = function (dictEq) {
    return function (x) {
        return function (y) {
            return eq(eqBoolean)(eq(dictEq)(x)(y))(false);
        };
    };
};
var notEq1 = function (dictEq1) {
    return function (dictEq) {
        return function (x) {
            return function (y) {
                return eq(eqBoolean)(eq1(dictEq1)(dictEq)(x)(y))(false);
            };
        };
    };
};
module.exports = {
    Eq: Eq,
    eq: eq,
    notEq: notEq,
    Eq1: Eq1,
    eq1: eq1,
    notEq1: notEq1,
    EqRecord: EqRecord,
    eqRecord: eqRecord,
    eqBoolean: eqBoolean,
    eqInt: eqInt,
    eqNumber: eqNumber,
    eqChar: eqChar,
    eqString: eqString,
    eqUnit: eqUnit,
    eqVoid: eqVoid,
    eqArray: eqArray,
    eqRec: eqRec,
    eq1Array: eq1Array,
    eqRowNil: eqRowNil,
    eqRowCons: eqRowCons
};

},{"../Data.HeytingAlgebra/index.js":99,"../Data.Symbol/index.js":150,"../Data.Unit/index.js":162,"../Data.Void/index.js":163,"../Record.Unsafe/index.js":200,"../Type.Data.RowList/index.js":204,"./foreign.js":81}],83:[function(require,module,exports){
"use strict";

exports.intDegree = function (x) {
  return Math.min(Math.abs(x), 2147483647);
};

// See the Euclidean definition in
// https://en.m.wikipedia.org/wiki/Modulo_operation.
exports.intDiv = function (x) {
  return function (y) {
    if (y === 0) return 0;
    return y > 0 ? Math.floor(x / y) : -Math.floor(x / -y);
  };
};

exports.intMod = function (x) {
  return function (y) {
    if (y === 0) return 0;
    var yy = Math.abs(y);
    return ((x % yy) + yy) % yy;
  };
};

exports.numDiv = function (n1) {
  return function (n2) {
    return n1 / n2;
  };
};

},{}],84:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var $foreign = require("./foreign.js");
var Data_BooleanAlgebra = require("../Data.BooleanAlgebra/index.js");
var Data_CommutativeRing = require("../Data.CommutativeRing/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra/index.js");
var Data_Ring = require("../Data.Ring/index.js");
var Data_Semiring = require("../Data.Semiring/index.js");
var EuclideanRing = function (CommutativeRing0, degree, div, mod) {
    this.CommutativeRing0 = CommutativeRing0;
    this.degree = degree;
    this.div = div;
    this.mod = mod;
};
var mod = function (dict) {
    return dict.mod;
};
var gcd = function ($copy_dictEq) {
    return function ($copy_dictEuclideanRing) {
        return function ($copy_a) {
            return function ($copy_b) {
                var $tco_var_dictEq = $copy_dictEq;
                var $tco_var_dictEuclideanRing = $copy_dictEuclideanRing;
                var $tco_var_a = $copy_a;
                var $tco_done = false;
                var $tco_result;
                function $tco_loop(dictEq, dictEuclideanRing, a, b) {
                    var $7 = Data_Eq.eq(dictEq)(b)(Data_Semiring.zero(((dictEuclideanRing.CommutativeRing0()).Ring0()).Semiring0()));
                    if ($7) {
                        $tco_done = true;
                        return a;
                    };
                    $tco_var_dictEq = dictEq;
                    $tco_var_dictEuclideanRing = dictEuclideanRing;
                    $tco_var_a = b;
                    $copy_b = mod(dictEuclideanRing)(a)(b);
                    return;
                };
                while (!$tco_done) {
                    $tco_result = $tco_loop($tco_var_dictEq, $tco_var_dictEuclideanRing, $tco_var_a, $copy_b);
                };
                return $tco_result;
            };
        };
    };
};
var euclideanRingNumber = new EuclideanRing(function () {
    return Data_CommutativeRing.commutativeRingNumber;
}, function (v) {
    return 1;
}, $foreign.numDiv, function (v) {
    return function (v1) {
        return 0.0;
    };
});
var euclideanRingInt = new EuclideanRing(function () {
    return Data_CommutativeRing.commutativeRingInt;
}, $foreign.intDegree, $foreign.intDiv, $foreign.intMod);
var div = function (dict) {
    return dict.div;
};
var lcm = function (dictEq) {
    return function (dictEuclideanRing) {
        return function (a) {
            return function (b) {
                var $8 = Data_Eq.eq(dictEq)(a)(Data_Semiring.zero(((dictEuclideanRing.CommutativeRing0()).Ring0()).Semiring0())) || Data_Eq.eq(dictEq)(b)(Data_Semiring.zero(((dictEuclideanRing.CommutativeRing0()).Ring0()).Semiring0()));
                if ($8) {
                    return Data_Semiring.zero(((dictEuclideanRing.CommutativeRing0()).Ring0()).Semiring0());
                };
                return div(dictEuclideanRing)(Data_Semiring.mul(((dictEuclideanRing.CommutativeRing0()).Ring0()).Semiring0())(a)(b))(gcd(dictEq)(dictEuclideanRing)(a)(b));
            };
        };
    };
};
var degree = function (dict) {
    return dict.degree;
};
module.exports = {
    EuclideanRing: EuclideanRing,
    degree: degree,
    div: div,
    mod: mod,
    gcd: gcd,
    lcm: lcm,
    euclideanRingInt: euclideanRingInt,
    euclideanRingNumber: euclideanRingNumber
};

},{"../Data.BooleanAlgebra/index.js":74,"../Data.CommutativeRing/index.js":77,"../Data.Eq/index.js":82,"../Data.HeytingAlgebra/index.js":99,"../Data.Ring/index.js":133,"../Data.Semiring/index.js":141,"./foreign.js":83}],85:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Data_CommutativeRing = require("../Data.CommutativeRing/index.js");
var Data_DivisionRing = require("../Data.DivisionRing/index.js");
var Data_EuclideanRing = require("../Data.EuclideanRing/index.js");
var Data_Ring = require("../Data.Ring/index.js");
var Data_Semiring = require("../Data.Semiring/index.js");
var Field = function (DivisionRing1, EuclideanRing0) {
    this.DivisionRing1 = DivisionRing1;
    this.EuclideanRing0 = EuclideanRing0;
};
var field = function (dictEuclideanRing) {
    return function (dictDivisionRing) {
        return new Field(function () {
            return dictDivisionRing;
        }, function () {
            return dictEuclideanRing;
        });
    };
};
module.exports = {
    Field: Field,
    field: field
};

},{"../Data.CommutativeRing/index.js":77,"../Data.DivisionRing/index.js":79,"../Data.EuclideanRing/index.js":84,"../Data.Ring/index.js":133,"../Data.Semiring/index.js":141}],86:[function(require,module,exports){
"use strict";

exports.foldrArray = function (f) {
  return function (init) {
    return function (xs) {
      var acc = init;
      var len = xs.length;
      for (var i = len - 1; i >= 0; i--) {
        acc = f(xs[i])(acc);
      }
      return acc;
    };
  };
};

exports.foldlArray = function (f) {
  return function (init) {
    return function (xs) {
      var acc = init;
      var len = xs.length;
      for (var i = 0; i < len; i++) {
        acc = f(acc)(xs[i]);
      }
      return acc;
    };
  };
};

},{}],87:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var $foreign = require("./foreign.js");
var Control_Alt = require("../Control.Alt/index.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Category = require("../Control.Category/index.js");
var Control_Plus = require("../Control.Plus/index.js");
var Control_Semigroupoid = require("../Control.Semigroupoid/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Maybe_First = require("../Data.Maybe.First/index.js");
var Data_Maybe_Last = require("../Data.Maybe.Last/index.js");
var Data_Monoid = require("../Data.Monoid/index.js");
var Data_Monoid_Additive = require("../Data.Monoid.Additive/index.js");
var Data_Monoid_Conj = require("../Data.Monoid.Conj/index.js");
var Data_Monoid_Disj = require("../Data.Monoid.Disj/index.js");
var Data_Monoid_Dual = require("../Data.Monoid.Dual/index.js");
var Data_Monoid_Endo = require("../Data.Monoid.Endo/index.js");
var Data_Monoid_Multiplicative = require("../Data.Monoid.Multiplicative/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Ordering = require("../Data.Ordering/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Semiring = require("../Data.Semiring/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Prelude = require("../Prelude/index.js");
var Foldable = function (foldMap, foldl, foldr) {
    this.foldMap = foldMap;
    this.foldl = foldl;
    this.foldr = foldr;
};
var foldr = function (dict) {
    return dict.foldr;
};
var indexr = function (dictFoldable) {
    return function (idx) {
        var go = function (a) {
            return function (cursor) {
                if (cursor.elem instanceof Data_Maybe.Just) {
                    return cursor;
                };
                var $106 = cursor.pos === idx;
                if ($106) {
                    return {
                        elem: new Data_Maybe.Just(a),
                        pos: cursor.pos
                    };
                };
                return {
                    pos: cursor.pos + 1 | 0,
                    elem: cursor.elem
                };
            };
        };
        return function ($193) {
            return (function (v) {
                return v.elem;
            })(foldr(dictFoldable)(go)({
                elem: Data_Maybe.Nothing.value,
                pos: 0
            })($193));
        };
    };
};
var $$null = function (dictFoldable) {
    return foldr(dictFoldable)(function (v) {
        return function (v1) {
            return false;
        };
    })(true);
};
var oneOf = function (dictFoldable) {
    return function (dictPlus) {
        return foldr(dictFoldable)(Control_Alt.alt(dictPlus.Alt0()))(Control_Plus.empty(dictPlus));
    };
};
var oneOfMap = function (dictFoldable) {
    return function (dictPlus) {
        return function (f) {
            return foldr(dictFoldable)(function ($194) {
                return Control_Alt.alt(dictPlus.Alt0())(f($194));
            })(Control_Plus.empty(dictPlus));
        };
    };
};
var traverse_ = function (dictApplicative) {
    return function (dictFoldable) {
        return function (f) {
            return foldr(dictFoldable)(function ($195) {
                return Control_Apply.applySecond(dictApplicative.Apply0())(f($195));
            })(Control_Applicative.pure(dictApplicative)(Data_Unit.unit));
        };
    };
};
var for_ = function (dictApplicative) {
    return function (dictFoldable) {
        return Data_Function.flip(traverse_(dictApplicative)(dictFoldable));
    };
};
var sequence_ = function (dictApplicative) {
    return function (dictFoldable) {
        return traverse_(dictApplicative)(dictFoldable)(Control_Category.identity(Control_Category.categoryFn));
    };
};
var foldl = function (dict) {
    return dict.foldl;
};
var indexl = function (dictFoldable) {
    return function (idx) {
        var go = function (cursor) {
            return function (a) {
                if (cursor.elem instanceof Data_Maybe.Just) {
                    return cursor;
                };
                var $109 = cursor.pos === idx;
                if ($109) {
                    return {
                        elem: new Data_Maybe.Just(a),
                        pos: cursor.pos
                    };
                };
                return {
                    pos: cursor.pos + 1 | 0,
                    elem: cursor.elem
                };
            };
        };
        return function ($196) {
            return (function (v) {
                return v.elem;
            })(foldl(dictFoldable)(go)({
                elem: Data_Maybe.Nothing.value,
                pos: 0
            })($196));
        };
    };
};
var intercalate = function (dictFoldable) {
    return function (dictMonoid) {
        return function (sep) {
            return function (xs) {
                var go = function (v) {
                    return function (x) {
                        if (v.init) {
                            return {
                                init: false,
                                acc: x
                            };
                        };
                        return {
                            init: false,
                            acc: Data_Semigroup.append(dictMonoid.Semigroup0())(v.acc)(Data_Semigroup.append(dictMonoid.Semigroup0())(sep)(x))
                        };
                    };
                };
                return (foldl(dictFoldable)(go)({
                    init: true,
                    acc: Data_Monoid.mempty(dictMonoid)
                })(xs)).acc;
            };
        };
    };
};
var length = function (dictFoldable) {
    return function (dictSemiring) {
        return foldl(dictFoldable)(function (c) {
            return function (v) {
                return Data_Semiring.add(dictSemiring)(Data_Semiring.one(dictSemiring))(c);
            };
        })(Data_Semiring.zero(dictSemiring));
    };
};
var maximumBy = function (dictFoldable) {
    return function (cmp) {
        var max$prime = function (v) {
            return function (v1) {
                if (v instanceof Data_Maybe.Nothing) {
                    return new Data_Maybe.Just(v1);
                };
                if (v instanceof Data_Maybe.Just) {
                    return new Data_Maybe.Just((function () {
                        var $116 = Data_Eq.eq(Data_Ordering.eqOrdering)(cmp(v.value0)(v1))(Data_Ordering.GT.value);
                        if ($116) {
                            return v.value0;
                        };
                        return v1;
                    })());
                };
                throw new Error("Failed pattern match at Data.Foldable (line 389, column 3 - line 389, column 27): " + [ v.constructor.name, v1.constructor.name ]);
            };
        };
        return foldl(dictFoldable)(max$prime)(Data_Maybe.Nothing.value);
    };
};
var maximum = function (dictOrd) {
    return function (dictFoldable) {
        return maximumBy(dictFoldable)(Data_Ord.compare(dictOrd));
    };
};
var minimumBy = function (dictFoldable) {
    return function (cmp) {
        var min$prime = function (v) {
            return function (v1) {
                if (v instanceof Data_Maybe.Nothing) {
                    return new Data_Maybe.Just(v1);
                };
                if (v instanceof Data_Maybe.Just) {
                    return new Data_Maybe.Just((function () {
                        var $120 = Data_Eq.eq(Data_Ordering.eqOrdering)(cmp(v.value0)(v1))(Data_Ordering.LT.value);
                        if ($120) {
                            return v.value0;
                        };
                        return v1;
                    })());
                };
                throw new Error("Failed pattern match at Data.Foldable (line 402, column 3 - line 402, column 27): " + [ v.constructor.name, v1.constructor.name ]);
            };
        };
        return foldl(dictFoldable)(min$prime)(Data_Maybe.Nothing.value);
    };
};
var minimum = function (dictOrd) {
    return function (dictFoldable) {
        return minimumBy(dictFoldable)(Data_Ord.compare(dictOrd));
    };
};
var product = function (dictFoldable) {
    return function (dictSemiring) {
        return foldl(dictFoldable)(Data_Semiring.mul(dictSemiring))(Data_Semiring.one(dictSemiring));
    };
};
var sum = function (dictFoldable) {
    return function (dictSemiring) {
        return foldl(dictFoldable)(Data_Semiring.add(dictSemiring))(Data_Semiring.zero(dictSemiring));
    };
};
var foldableMultiplicative = new Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            return f(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(z)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(v)(z);
        };
    };
});
var foldableMaybe = new Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            if (v instanceof Data_Maybe.Nothing) {
                return Data_Monoid.mempty(dictMonoid);
            };
            if (v instanceof Data_Maybe.Just) {
                return f(v.value0);
            };
            throw new Error("Failed pattern match at Data.Foldable (line 129, column 1 - line 129, column 41): " + [ f.constructor.name, v.constructor.name ]);
        };
    };
}, function (v) {
    return function (z) {
        return function (v1) {
            if (v1 instanceof Data_Maybe.Nothing) {
                return z;
            };
            if (v1 instanceof Data_Maybe.Just) {
                return v(z)(v1.value0);
            };
            throw new Error("Failed pattern match at Data.Foldable (line 129, column 1 - line 129, column 41): " + [ v.constructor.name, z.constructor.name, v1.constructor.name ]);
        };
    };
}, function (v) {
    return function (z) {
        return function (v1) {
            if (v1 instanceof Data_Maybe.Nothing) {
                return z;
            };
            if (v1 instanceof Data_Maybe.Just) {
                return v(v1.value0)(z);
            };
            throw new Error("Failed pattern match at Data.Foldable (line 129, column 1 - line 129, column 41): " + [ v.constructor.name, z.constructor.name, v1.constructor.name ]);
        };
    };
});
var foldableDual = new Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            return f(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(z)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(v)(z);
        };
    };
});
var foldableDisj = new Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            return f(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(z)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(v)(z);
        };
    };
});
var foldableConj = new Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            return f(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(z)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(v)(z);
        };
    };
});
var foldableAdditive = new Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            return f(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(z)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(v)(z);
        };
    };
});
var foldMapDefaultR = function (dictFoldable) {
    return function (dictMonoid) {
        return function (f) {
            return foldr(dictFoldable)(function (x) {
                return function (acc) {
                    return Data_Semigroup.append(dictMonoid.Semigroup0())(f(x))(acc);
                };
            })(Data_Monoid.mempty(dictMonoid));
        };
    };
};
var foldableArray = new Foldable(function (dictMonoid) {
    return foldMapDefaultR(foldableArray)(dictMonoid);
}, $foreign.foldlArray, $foreign.foldrArray);
var foldMapDefaultL = function (dictFoldable) {
    return function (dictMonoid) {
        return function (f) {
            return foldl(dictFoldable)(function (acc) {
                return function (x) {
                    return Data_Semigroup.append(dictMonoid.Semigroup0())(acc)(f(x));
                };
            })(Data_Monoid.mempty(dictMonoid));
        };
    };
};
var foldMap = function (dict) {
    return dict.foldMap;
};
var foldableFirst = new Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            return foldMap(foldableMaybe)(dictMonoid)(f)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return foldl(foldableMaybe)(f)(z)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return foldr(foldableMaybe)(f)(z)(v);
        };
    };
});
var foldableLast = new Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            return foldMap(foldableMaybe)(dictMonoid)(f)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return foldl(foldableMaybe)(f)(z)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return foldr(foldableMaybe)(f)(z)(v);
        };
    };
});
var foldlDefault = function (dictFoldable) {
    return function (c) {
        return function (u) {
            return function (xs) {
                return Data_Newtype.unwrap(Data_Newtype.newtypeEndo)(Data_Newtype.unwrap(Data_Newtype.newtypeDual)(foldMap(dictFoldable)(Data_Monoid_Dual.monoidDual(Data_Monoid_Endo.monoidEndo(Control_Category.categoryFn)))(function ($197) {
                    return Data_Monoid_Dual.Dual(Data_Monoid_Endo.Endo(Data_Function.flip(c)($197)));
                })(xs)))(u);
            };
        };
    };
};
var foldrDefault = function (dictFoldable) {
    return function (c) {
        return function (u) {
            return function (xs) {
                return Data_Newtype.unwrap(Data_Newtype.newtypeEndo)(foldMap(dictFoldable)(Data_Monoid_Endo.monoidEndo(Control_Category.categoryFn))(function ($198) {
                    return Data_Monoid_Endo.Endo(c($198));
                })(xs))(u);
            };
        };
    };
};
var surroundMap = function (dictFoldable) {
    return function (dictSemigroup) {
        return function (d) {
            return function (t) {
                return function (f) {
                    var joined = function (a) {
                        return function (m) {
                            return Data_Semigroup.append(dictSemigroup)(d)(Data_Semigroup.append(dictSemigroup)(t(a))(m));
                        };
                    };
                    return Data_Newtype.unwrap(Data_Newtype.newtypeEndo)(foldMap(dictFoldable)(Data_Monoid_Endo.monoidEndo(Control_Category.categoryFn))(joined)(f))(d);
                };
            };
        };
    };
};
var surround = function (dictFoldable) {
    return function (dictSemigroup) {
        return function (d) {
            return surroundMap(dictFoldable)(dictSemigroup)(d)(Control_Category.identity(Control_Category.categoryFn));
        };
    };
};
var foldM = function (dictFoldable) {
    return function (dictMonad) {
        return function (f) {
            return function (a0) {
                return foldl(dictFoldable)(function (ma) {
                    return function (b) {
                        return Control_Bind.bind(dictMonad.Bind1())(ma)(Data_Function.flip(f)(b));
                    };
                })(Control_Applicative.pure(dictMonad.Applicative0())(a0));
            };
        };
    };
};
var fold = function (dictFoldable) {
    return function (dictMonoid) {
        return foldMap(dictFoldable)(dictMonoid)(Control_Category.identity(Control_Category.categoryFn));
    };
};
var findMap = function (dictFoldable) {
    return function (p) {
        var go = function (v) {
            return function (v1) {
                if (v instanceof Data_Maybe.Nothing) {
                    return p(v1);
                };
                return v;
            };
        };
        return foldl(dictFoldable)(go)(Data_Maybe.Nothing.value);
    };
};
var find = function (dictFoldable) {
    return function (p) {
        var go = function (v) {
            return function (v1) {
                if (v instanceof Data_Maybe.Nothing && p(v1)) {
                    return new Data_Maybe.Just(v1);
                };
                return v;
            };
        };
        return foldl(dictFoldable)(go)(Data_Maybe.Nothing.value);
    };
};
var any = function (dictFoldable) {
    return function (dictHeytingAlgebra) {
        return Data_Newtype.alaF(Data_Functor.functorFn)(Data_Functor.functorFn)(Data_Newtype.newtypeDisj)(Data_Newtype.newtypeDisj)(Data_Monoid_Disj.Disj)(foldMap(dictFoldable)(Data_Monoid_Disj.monoidDisj(dictHeytingAlgebra)));
    };
};
var elem = function (dictFoldable) {
    return function (dictEq) {
        return function ($199) {
            return any(dictFoldable)(Data_HeytingAlgebra.heytingAlgebraBoolean)(Data_Eq.eq(dictEq)($199));
        };
    };
};
var notElem = function (dictFoldable) {
    return function (dictEq) {
        return function (x) {
            return function ($200) {
                return !elem(dictFoldable)(dictEq)(x)($200);
            };
        };
    };
};
var or = function (dictFoldable) {
    return function (dictHeytingAlgebra) {
        return any(dictFoldable)(dictHeytingAlgebra)(Control_Category.identity(Control_Category.categoryFn));
    };
};
var all = function (dictFoldable) {
    return function (dictHeytingAlgebra) {
        return Data_Newtype.alaF(Data_Functor.functorFn)(Data_Functor.functorFn)(Data_Newtype.newtypeConj)(Data_Newtype.newtypeConj)(Data_Monoid_Conj.Conj)(foldMap(dictFoldable)(Data_Monoid_Conj.monoidConj(dictHeytingAlgebra)));
    };
};
var and = function (dictFoldable) {
    return function (dictHeytingAlgebra) {
        return all(dictFoldable)(dictHeytingAlgebra)(Control_Category.identity(Control_Category.categoryFn));
    };
};
module.exports = {
    Foldable: Foldable,
    foldr: foldr,
    foldl: foldl,
    foldMap: foldMap,
    foldrDefault: foldrDefault,
    foldlDefault: foldlDefault,
    foldMapDefaultL: foldMapDefaultL,
    foldMapDefaultR: foldMapDefaultR,
    fold: fold,
    foldM: foldM,
    traverse_: traverse_,
    for_: for_,
    sequence_: sequence_,
    oneOf: oneOf,
    oneOfMap: oneOfMap,
    intercalate: intercalate,
    surroundMap: surroundMap,
    surround: surround,
    and: and,
    or: or,
    all: all,
    any: any,
    sum: sum,
    product: product,
    elem: elem,
    notElem: notElem,
    indexl: indexl,
    indexr: indexr,
    find: find,
    findMap: findMap,
    maximum: maximum,
    maximumBy: maximumBy,
    minimum: minimum,
    minimumBy: minimumBy,
    "null": $$null,
    length: length,
    foldableArray: foldableArray,
    foldableMaybe: foldableMaybe,
    foldableFirst: foldableFirst,
    foldableLast: foldableLast,
    foldableAdditive: foldableAdditive,
    foldableDual: foldableDual,
    foldableDisj: foldableDisj,
    foldableConj: foldableConj,
    foldableMultiplicative: foldableMultiplicative
};

},{"../Control.Alt/index.js":25,"../Control.Applicative/index.js":27,"../Control.Apply/index.js":29,"../Control.Bind/index.js":33,"../Control.Category/index.js":34,"../Control.Plus/index.js":55,"../Control.Semigroupoid/index.js":56,"../Data.Eq/index.js":82,"../Data.Function/index.js":91,"../Data.Functor/index.js":95,"../Data.HeytingAlgebra/index.js":99,"../Data.Maybe.First/index.js":108,"../Data.Maybe.Last/index.js":109,"../Data.Maybe/index.js":110,"../Data.Monoid.Additive/index.js":111,"../Data.Monoid.Conj/index.js":112,"../Data.Monoid.Disj/index.js":113,"../Data.Monoid.Dual/index.js":114,"../Data.Monoid.Endo/index.js":115,"../Data.Monoid.Multiplicative/index.js":116,"../Data.Monoid/index.js":117,"../Data.Newtype/index.js":119,"../Data.Ord/index.js":130,"../Data.Ordering/index.js":131,"../Data.Semigroup/index.js":139,"../Data.Semiring/index.js":141,"../Data.Unit/index.js":162,"../Prelude/index.js":198,"./foreign.js":86}],88:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Category = require("../Control.Category/index.js");
var Control_Semigroupoid = require("../Control.Semigroupoid/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_FunctorWithIndex = require("../Data.FunctorWithIndex/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Maybe_First = require("../Data.Maybe.First/index.js");
var Data_Maybe_Last = require("../Data.Maybe.Last/index.js");
var Data_Monoid = require("../Data.Monoid/index.js");
var Data_Monoid_Additive = require("../Data.Monoid.Additive/index.js");
var Data_Monoid_Conj = require("../Data.Monoid.Conj/index.js");
var Data_Monoid_Disj = require("../Data.Monoid.Disj/index.js");
var Data_Monoid_Dual = require("../Data.Monoid.Dual/index.js");
var Data_Monoid_Endo = require("../Data.Monoid.Endo/index.js");
var Data_Monoid_Multiplicative = require("../Data.Monoid.Multiplicative/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Prelude = require("../Prelude/index.js");
var Tuple = (function () {
    function Tuple(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Tuple.create = function (value0) {
        return function (value1) {
            return new Tuple(value0, value1);
        };
    };
    return Tuple;
})();
var FoldableWithIndex = function (Foldable0, foldMapWithIndex, foldlWithIndex, foldrWithIndex) {
    this.Foldable0 = Foldable0;
    this.foldMapWithIndex = foldMapWithIndex;
    this.foldlWithIndex = foldlWithIndex;
    this.foldrWithIndex = foldrWithIndex;
};
var foldrWithIndex = function (dict) {
    return dict.foldrWithIndex;
};
var traverseWithIndex_ = function (dictApplicative) {
    return function (dictFoldableWithIndex) {
        return function (f) {
            return foldrWithIndex(dictFoldableWithIndex)(function (i) {
                return function ($46) {
                    return Control_Apply.applySecond(dictApplicative.Apply0())(f(i)($46));
                };
            })(Control_Applicative.pure(dictApplicative)(Data_Unit.unit));
        };
    };
};
var forWithIndex_ = function (dictApplicative) {
    return function (dictFoldableWithIndex) {
        return Data_Function.flip(traverseWithIndex_(dictApplicative)(dictFoldableWithIndex));
    };
};
var foldrDefault = function (dictFoldableWithIndex) {
    return function (f) {
        return foldrWithIndex(dictFoldableWithIndex)(Data_Function["const"](f));
    };
};
var foldlWithIndex = function (dict) {
    return dict.foldlWithIndex;
};
var foldlDefault = function (dictFoldableWithIndex) {
    return function (f) {
        return foldlWithIndex(dictFoldableWithIndex)(Data_Function["const"](f));
    };
};
var foldableWithIndexMultiplicative = new FoldableWithIndex(function () {
    return Data_Foldable.foldableMultiplicative;
}, function (dictMonoid) {
    return function (f) {
        return Data_Foldable.foldMap(Data_Foldable.foldableMultiplicative)(dictMonoid)(f(Data_Unit.unit));
    };
}, function (f) {
    return Data_Foldable.foldl(Data_Foldable.foldableMultiplicative)(f(Data_Unit.unit));
}, function (f) {
    return Data_Foldable.foldr(Data_Foldable.foldableMultiplicative)(f(Data_Unit.unit));
});
var foldableWithIndexMaybe = new FoldableWithIndex(function () {
    return Data_Foldable.foldableMaybe;
}, function (dictMonoid) {
    return function (f) {
        return Data_Foldable.foldMap(Data_Foldable.foldableMaybe)(dictMonoid)(f(Data_Unit.unit));
    };
}, function (f) {
    return Data_Foldable.foldl(Data_Foldable.foldableMaybe)(f(Data_Unit.unit));
}, function (f) {
    return Data_Foldable.foldr(Data_Foldable.foldableMaybe)(f(Data_Unit.unit));
});
var foldableWithIndexLast = new FoldableWithIndex(function () {
    return Data_Foldable.foldableLast;
}, function (dictMonoid) {
    return function (f) {
        return Data_Foldable.foldMap(Data_Foldable.foldableLast)(dictMonoid)(f(Data_Unit.unit));
    };
}, function (f) {
    return Data_Foldable.foldl(Data_Foldable.foldableLast)(f(Data_Unit.unit));
}, function (f) {
    return Data_Foldable.foldr(Data_Foldable.foldableLast)(f(Data_Unit.unit));
});
var foldableWithIndexFirst = new FoldableWithIndex(function () {
    return Data_Foldable.foldableFirst;
}, function (dictMonoid) {
    return function (f) {
        return Data_Foldable.foldMap(Data_Foldable.foldableFirst)(dictMonoid)(f(Data_Unit.unit));
    };
}, function (f) {
    return Data_Foldable.foldl(Data_Foldable.foldableFirst)(f(Data_Unit.unit));
}, function (f) {
    return Data_Foldable.foldr(Data_Foldable.foldableFirst)(f(Data_Unit.unit));
});
var foldableWithIndexDual = new FoldableWithIndex(function () {
    return Data_Foldable.foldableDual;
}, function (dictMonoid) {
    return function (f) {
        return Data_Foldable.foldMap(Data_Foldable.foldableDual)(dictMonoid)(f(Data_Unit.unit));
    };
}, function (f) {
    return Data_Foldable.foldl(Data_Foldable.foldableDual)(f(Data_Unit.unit));
}, function (f) {
    return Data_Foldable.foldr(Data_Foldable.foldableDual)(f(Data_Unit.unit));
});
var foldableWithIndexDisj = new FoldableWithIndex(function () {
    return Data_Foldable.foldableDisj;
}, function (dictMonoid) {
    return function (f) {
        return Data_Foldable.foldMap(Data_Foldable.foldableDisj)(dictMonoid)(f(Data_Unit.unit));
    };
}, function (f) {
    return Data_Foldable.foldl(Data_Foldable.foldableDisj)(f(Data_Unit.unit));
}, function (f) {
    return Data_Foldable.foldr(Data_Foldable.foldableDisj)(f(Data_Unit.unit));
});
var foldableWithIndexConj = new FoldableWithIndex(function () {
    return Data_Foldable.foldableConj;
}, function (dictMonoid) {
    return function (f) {
        return Data_Foldable.foldMap(Data_Foldable.foldableConj)(dictMonoid)(f(Data_Unit.unit));
    };
}, function (f) {
    return Data_Foldable.foldl(Data_Foldable.foldableConj)(f(Data_Unit.unit));
}, function (f) {
    return Data_Foldable.foldr(Data_Foldable.foldableConj)(f(Data_Unit.unit));
});
var foldableWithIndexAdditive = new FoldableWithIndex(function () {
    return Data_Foldable.foldableAdditive;
}, function (dictMonoid) {
    return function (f) {
        return Data_Foldable.foldMap(Data_Foldable.foldableAdditive)(dictMonoid)(f(Data_Unit.unit));
    };
}, function (f) {
    return Data_Foldable.foldl(Data_Foldable.foldableAdditive)(f(Data_Unit.unit));
}, function (f) {
    return Data_Foldable.foldr(Data_Foldable.foldableAdditive)(f(Data_Unit.unit));
});
var foldWithIndexM = function (dictFoldableWithIndex) {
    return function (dictMonad) {
        return function (f) {
            return function (a0) {
                return foldlWithIndex(dictFoldableWithIndex)(function (i) {
                    return function (ma) {
                        return function (b) {
                            return Control_Bind.bind(dictMonad.Bind1())(ma)(Data_Function.flip(f(i))(b));
                        };
                    };
                })(Control_Applicative.pure(dictMonad.Applicative0())(a0));
            };
        };
    };
};
var foldMapWithIndexDefaultR = function (dictFoldableWithIndex) {
    return function (dictMonoid) {
        return function (f) {
            return foldrWithIndex(dictFoldableWithIndex)(function (i) {
                return function (x) {
                    return function (acc) {
                        return Data_Semigroup.append(dictMonoid.Semigroup0())(f(i)(x))(acc);
                    };
                };
            })(Data_Monoid.mempty(dictMonoid));
        };
    };
};
var foldableWithIndexArray = new FoldableWithIndex(function () {
    return Data_Foldable.foldableArray;
}, function (dictMonoid) {
    return foldMapWithIndexDefaultR(foldableWithIndexArray)(dictMonoid);
}, function (f) {
    return function (z) {
        return function ($47) {
            return Data_Foldable.foldl(Data_Foldable.foldableArray)(function (y) {
                return function (v) {
                    return f(v.value0)(y)(v.value1);
                };
            })(z)(Data_FunctorWithIndex.mapWithIndex(Data_FunctorWithIndex.functorWithIndexArray)(Tuple.create)($47));
        };
    };
}, function (f) {
    return function (z) {
        return function ($48) {
            return Data_Foldable.foldr(Data_Foldable.foldableArray)(function (v) {
                return function (y) {
                    return f(v.value0)(v.value1)(y);
                };
            })(z)(Data_FunctorWithIndex.mapWithIndex(Data_FunctorWithIndex.functorWithIndexArray)(Tuple.create)($48));
        };
    };
});
var foldMapWithIndexDefaultL = function (dictFoldableWithIndex) {
    return function (dictMonoid) {
        return function (f) {
            return foldlWithIndex(dictFoldableWithIndex)(function (i) {
                return function (acc) {
                    return function (x) {
                        return Data_Semigroup.append(dictMonoid.Semigroup0())(acc)(f(i)(x));
                    };
                };
            })(Data_Monoid.mempty(dictMonoid));
        };
    };
};
var foldMapWithIndex = function (dict) {
    return dict.foldMapWithIndex;
};
var foldlWithIndexDefault = function (dictFoldableWithIndex) {
    return function (c) {
        return function (u) {
            return function (xs) {
                return Data_Newtype.unwrap(Data_Newtype.newtypeEndo)(Data_Newtype.unwrap(Data_Newtype.newtypeDual)(foldMapWithIndex(dictFoldableWithIndex)(Data_Monoid_Dual.monoidDual(Data_Monoid_Endo.monoidEndo(Control_Category.categoryFn)))(function (i) {
                    return function ($49) {
                        return Data_Monoid_Dual.Dual(Data_Monoid_Endo.Endo(Data_Function.flip(c(i))($49)));
                    };
                })(xs)))(u);
            };
        };
    };
};
var foldrWithIndexDefault = function (dictFoldableWithIndex) {
    return function (c) {
        return function (u) {
            return function (xs) {
                return Data_Newtype.unwrap(Data_Newtype.newtypeEndo)(foldMapWithIndex(dictFoldableWithIndex)(Data_Monoid_Endo.monoidEndo(Control_Category.categoryFn))(function (i) {
                    return function ($50) {
                        return Data_Monoid_Endo.Endo(c(i)($50));
                    };
                })(xs))(u);
            };
        };
    };
};
var surroundMapWithIndex = function (dictFoldableWithIndex) {
    return function (dictSemigroup) {
        return function (d) {
            return function (t) {
                return function (f) {
                    var joined = function (i) {
                        return function (a) {
                            return function (m) {
                                return Data_Semigroup.append(dictSemigroup)(d)(Data_Semigroup.append(dictSemigroup)(t(i)(a))(m));
                            };
                        };
                    };
                    return Data_Newtype.unwrap(Data_Newtype.newtypeEndo)(foldMapWithIndex(dictFoldableWithIndex)(Data_Monoid_Endo.monoidEndo(Control_Category.categoryFn))(joined)(f))(d);
                };
            };
        };
    };
};
var foldMapDefault = function (dictFoldableWithIndex) {
    return function (dictMonoid) {
        return function (f) {
            return foldMapWithIndex(dictFoldableWithIndex)(dictMonoid)(Data_Function["const"](f));
        };
    };
};
var findWithIndex = function (dictFoldableWithIndex) {
    return function (p) {
        var go = function (v) {
            return function (v1) {
                return function (v2) {
                    if (v1 instanceof Data_Maybe.Nothing && p(v)(v2)) {
                        return new Data_Maybe.Just({
                            index: v,
                            value: v2
                        });
                    };
                    return v1;
                };
            };
        };
        return foldlWithIndex(dictFoldableWithIndex)(go)(Data_Maybe.Nothing.value);
    };
};
var anyWithIndex = function (dictFoldableWithIndex) {
    return function (dictHeytingAlgebra) {
        return function (t) {
            return function ($51) {
                return Data_Newtype.unwrap(Data_Newtype.newtypeDisj)(foldMapWithIndex(dictFoldableWithIndex)(Data_Monoid_Disj.monoidDisj(dictHeytingAlgebra))(function (i) {
                    return function ($52) {
                        return Data_Monoid_Disj.Disj(t(i)($52));
                    };
                })($51));
            };
        };
    };
};
var allWithIndex = function (dictFoldableWithIndex) {
    return function (dictHeytingAlgebra) {
        return function (t) {
            return function ($53) {
                return Data_Newtype.unwrap(Data_Newtype.newtypeConj)(foldMapWithIndex(dictFoldableWithIndex)(Data_Monoid_Conj.monoidConj(dictHeytingAlgebra))(function (i) {
                    return function ($54) {
                        return Data_Monoid_Conj.Conj(t(i)($54));
                    };
                })($53));
            };
        };
    };
};
module.exports = {
    FoldableWithIndex: FoldableWithIndex,
    foldrWithIndex: foldrWithIndex,
    foldlWithIndex: foldlWithIndex,
    foldMapWithIndex: foldMapWithIndex,
    foldrWithIndexDefault: foldrWithIndexDefault,
    foldlWithIndexDefault: foldlWithIndexDefault,
    foldMapWithIndexDefaultR: foldMapWithIndexDefaultR,
    foldMapWithIndexDefaultL: foldMapWithIndexDefaultL,
    foldWithIndexM: foldWithIndexM,
    traverseWithIndex_: traverseWithIndex_,
    forWithIndex_: forWithIndex_,
    surroundMapWithIndex: surroundMapWithIndex,
    allWithIndex: allWithIndex,
    anyWithIndex: anyWithIndex,
    findWithIndex: findWithIndex,
    foldrDefault: foldrDefault,
    foldlDefault: foldlDefault,
    foldMapDefault: foldMapDefault,
    foldableWithIndexArray: foldableWithIndexArray,
    foldableWithIndexMaybe: foldableWithIndexMaybe,
    foldableWithIndexFirst: foldableWithIndexFirst,
    foldableWithIndexLast: foldableWithIndexLast,
    foldableWithIndexAdditive: foldableWithIndexAdditive,
    foldableWithIndexDual: foldableWithIndexDual,
    foldableWithIndexDisj: foldableWithIndexDisj,
    foldableWithIndexConj: foldableWithIndexConj,
    foldableWithIndexMultiplicative: foldableWithIndexMultiplicative
};

},{"../Control.Applicative/index.js":27,"../Control.Apply/index.js":29,"../Control.Bind/index.js":33,"../Control.Category/index.js":34,"../Control.Semigroupoid/index.js":56,"../Data.Foldable/index.js":87,"../Data.Function/index.js":91,"../Data.FunctorWithIndex/index.js":97,"../Data.Maybe.First/index.js":108,"../Data.Maybe.Last/index.js":109,"../Data.Maybe/index.js":110,"../Data.Monoid.Additive/index.js":111,"../Data.Monoid.Conj/index.js":112,"../Data.Monoid.Disj/index.js":113,"../Data.Monoid.Dual/index.js":114,"../Data.Monoid.Endo/index.js":115,"../Data.Monoid.Multiplicative/index.js":116,"../Data.Monoid/index.js":117,"../Data.Newtype/index.js":119,"../Data.Semigroup/index.js":139,"../Data.Unit/index.js":162,"../Prelude/index.js":198}],89:[function(require,module,exports){
"use strict";

// module Data.Function.Uncurried

exports.mkFn0 = function (fn) {
  return function () {
    return fn({});
  };
};

exports.mkFn2 = function (fn) {
  /* jshint maxparams: 2 */
  return function (a, b) {
    return fn(a)(b);
  };
};

exports.mkFn3 = function (fn) {
  /* jshint maxparams: 3 */
  return function (a, b, c) {
    return fn(a)(b)(c);
  };
};

exports.mkFn4 = function (fn) {
  /* jshint maxparams: 4 */
  return function (a, b, c, d) {
    return fn(a)(b)(c)(d);
  };
};

exports.mkFn5 = function (fn) {
  /* jshint maxparams: 5 */
  return function (a, b, c, d, e) {
    return fn(a)(b)(c)(d)(e);
  };
};

exports.mkFn6 = function (fn) {
  /* jshint maxparams: 6 */
  return function (a, b, c, d, e, f) {
    return fn(a)(b)(c)(d)(e)(f);
  };
};

exports.mkFn7 = function (fn) {
  /* jshint maxparams: 7 */
  return function (a, b, c, d, e, f, g) {
    return fn(a)(b)(c)(d)(e)(f)(g);
  };
};

exports.mkFn8 = function (fn) {
  /* jshint maxparams: 8 */
  return function (a, b, c, d, e, f, g, h) {
    return fn(a)(b)(c)(d)(e)(f)(g)(h);
  };
};

exports.mkFn9 = function (fn) {
  /* jshint maxparams: 9 */
  return function (a, b, c, d, e, f, g, h, i) {
    return fn(a)(b)(c)(d)(e)(f)(g)(h)(i);
  };
};

exports.mkFn10 = function (fn) {
  /* jshint maxparams: 10 */
  return function (a, b, c, d, e, f, g, h, i, j) {
    return fn(a)(b)(c)(d)(e)(f)(g)(h)(i)(j);
  };
};

exports.runFn0 = function (fn) {
  return fn();
};

exports.runFn2 = function (fn) {
  return function (a) {
    return function (b) {
      return fn(a, b);
    };
  };
};

exports.runFn3 = function (fn) {
  return function (a) {
    return function (b) {
      return function (c) {
        return fn(a, b, c);
      };
    };
  };
};

exports.runFn4 = function (fn) {
  return function (a) {
    return function (b) {
      return function (c) {
        return function (d) {
          return fn(a, b, c, d);
        };
      };
    };
  };
};

exports.runFn5 = function (fn) {
  return function (a) {
    return function (b) {
      return function (c) {
        return function (d) {
          return function (e) {
            return fn(a, b, c, d, e);
          };
        };
      };
    };
  };
};

exports.runFn6 = function (fn) {
  return function (a) {
    return function (b) {
      return function (c) {
        return function (d) {
          return function (e) {
            return function (f) {
              return fn(a, b, c, d, e, f);
            };
          };
        };
      };
    };
  };
};

exports.runFn7 = function (fn) {
  return function (a) {
    return function (b) {
      return function (c) {
        return function (d) {
          return function (e) {
            return function (f) {
              return function (g) {
                return fn(a, b, c, d, e, f, g);
              };
            };
          };
        };
      };
    };
  };
};

exports.runFn8 = function (fn) {
  return function (a) {
    return function (b) {
      return function (c) {
        return function (d) {
          return function (e) {
            return function (f) {
              return function (g) {
                return function (h) {
                  return fn(a, b, c, d, e, f, g, h);
                };
              };
            };
          };
        };
      };
    };
  };
};

exports.runFn9 = function (fn) {
  return function (a) {
    return function (b) {
      return function (c) {
        return function (d) {
          return function (e) {
            return function (f) {
              return function (g) {
                return function (h) {
                  return function (i) {
                    return fn(a, b, c, d, e, f, g, h, i);
                  };
                };
              };
            };
          };
        };
      };
    };
  };
};

exports.runFn10 = function (fn) {
  return function (a) {
    return function (b) {
      return function (c) {
        return function (d) {
          return function (e) {
            return function (f) {
              return function (g) {
                return function (h) {
                  return function (i) {
                    return function (j) {
                      return fn(a, b, c, d, e, f, g, h, i, j);
                    };
                  };
                };
              };
            };
          };
        };
      };
    };
  };
};

},{}],90:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var $foreign = require("./foreign.js");
var Data_Unit = require("../Data.Unit/index.js");
var runFn1 = function (f) {
    return f;
};
var mkFn1 = function (f) {
    return f;
};
module.exports = {
    mkFn1: mkFn1,
    runFn1: runFn1,
    mkFn0: $foreign.mkFn0,
    mkFn2: $foreign.mkFn2,
    mkFn3: $foreign.mkFn3,
    mkFn4: $foreign.mkFn4,
    mkFn5: $foreign.mkFn5,
    mkFn6: $foreign.mkFn6,
    mkFn7: $foreign.mkFn7,
    mkFn8: $foreign.mkFn8,
    mkFn9: $foreign.mkFn9,
    mkFn10: $foreign.mkFn10,
    runFn0: $foreign.runFn0,
    runFn2: $foreign.runFn2,
    runFn3: $foreign.runFn3,
    runFn4: $foreign.runFn4,
    runFn5: $foreign.runFn5,
    runFn6: $foreign.runFn6,
    runFn7: $foreign.runFn7,
    runFn8: $foreign.runFn8,
    runFn9: $foreign.runFn9,
    runFn10: $foreign.runFn10
};

},{"../Data.Unit/index.js":162,"./foreign.js":89}],91:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Category = require("../Control.Category/index.js");
var Data_Boolean = require("../Data.Boolean/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Ring = require("../Data.Ring/index.js");
var on = function (f) {
    return function (g) {
        return function (x) {
            return function (y) {
                return f(g(x))(g(y));
            };
        };
    };
};
var flip = function (f) {
    return function (b) {
        return function (a) {
            return f(a)(b);
        };
    };
};
var $$const = function (a) {
    return function (v) {
        return a;
    };
};
var applyN = function (f) {
    var go = function ($copy_n) {
        return function ($copy_acc) {
            var $tco_var_n = $copy_n;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(n, acc) {
                if (n <= 0) {
                    $tco_done = true;
                    return acc;
                };
                if (Data_Boolean.otherwise) {
                    $tco_var_n = n - 1 | 0;
                    $copy_acc = f(acc);
                    return;
                };
                throw new Error("Failed pattern match at Data.Function (line 94, column 3 - line 96, column 37): " + [ n.constructor.name, acc.constructor.name ]);
            };
            while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_n, $copy_acc);
            };
            return $tco_result;
        };
    };
    return go;
};
var applyFlipped = function (x) {
    return function (f) {
        return f(x);
    };
};
var apply = function (f) {
    return function (x) {
        return f(x);
    };
};
module.exports = {
    flip: flip,
    "const": $$const,
    apply: apply,
    applyFlipped: applyFlipped,
    applyN: applyN,
    on: on
};

},{"../Control.Category/index.js":34,"../Data.Boolean/index.js":73,"../Data.Ord/index.js":130,"../Data.Ring/index.js":133}],92:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Data_Functor = require("../Data.Functor/index.js");
var Data_Void = require("../Data.Void/index.js");
var Prelude = require("../Prelude/index.js");
var Contravariant = function (cmap) {
    this.cmap = cmap;
};
var cmap = function (dict) {
    return dict.cmap;
};
var cmapFlipped = function (dictContravariant) {
    return function (x) {
        return function (f) {
            return cmap(dictContravariant)(f)(x);
        };
    };
};
var coerce = function (dictContravariant) {
    return function (dictFunctor) {
        return function (a) {
            return Data_Functor.map(dictFunctor)(Data_Void.absurd)(cmap(dictContravariant)(Data_Void.absurd)(a));
        };
    };
};
var imapC = function (dictContravariant) {
    return function (v) {
        return function (f) {
            return cmap(dictContravariant)(f);
        };
    };
};
module.exports = {
    cmap: cmap,
    Contravariant: Contravariant,
    cmapFlipped: cmapFlipped,
    coerce: coerce,
    imapC: imapC
};

},{"../Data.Functor/index.js":95,"../Data.Void/index.js":163,"../Prelude/index.js":198}],93:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Semigroupoid = require("../Control.Semigroupoid/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Monoid_Additive = require("../Data.Monoid.Additive/index.js");
var Data_Monoid_Conj = require("../Data.Monoid.Conj/index.js");
var Data_Monoid_Disj = require("../Data.Monoid.Disj/index.js");
var Data_Monoid_Dual = require("../Data.Monoid.Dual/index.js");
var Data_Monoid_Endo = require("../Data.Monoid.Endo/index.js");
var Data_Monoid_Multiplicative = require("../Data.Monoid.Multiplicative/index.js");
var Invariant = function (imap) {
    this.imap = imap;
};
var invariantMultiplicative = new Invariant(function (f) {
    return function (v) {
        return function (v1) {
            return f(v1);
        };
    };
});
var invariantEndo = new Invariant(function (ab) {
    return function (ba) {
        return function (v) {
            return function ($31) {
                return ab(v(ba($31)));
            };
        };
    };
});
var invariantDual = new Invariant(function (f) {
    return function (v) {
        return function (v1) {
            return f(v1);
        };
    };
});
var invariantDisj = new Invariant(function (f) {
    return function (v) {
        return function (v1) {
            return f(v1);
        };
    };
});
var invariantConj = new Invariant(function (f) {
    return function (v) {
        return function (v1) {
            return f(v1);
        };
    };
});
var invariantAdditive = new Invariant(function (f) {
    return function (v) {
        return function (v1) {
            return f(v1);
        };
    };
});
var imapF = function (dictFunctor) {
    return function (f) {
        return function (v) {
            return Data_Functor.map(dictFunctor)(f);
        };
    };
};
var invariantArray = new Invariant(imapF(Data_Functor.functorArray));
var invariantFn = new Invariant(imapF(Data_Functor.functorFn));
var imap = function (dict) {
    return dict.imap;
};
module.exports = {
    imap: imap,
    Invariant: Invariant,
    imapF: imapF,
    invariantFn: invariantFn,
    invariantArray: invariantArray,
    invariantAdditive: invariantAdditive,
    invariantConj: invariantConj,
    invariantDisj: invariantDisj,
    invariantDual: invariantDual,
    invariantEndo: invariantEndo,
    invariantMultiplicative: invariantMultiplicative
};

},{"../Control.Semigroupoid/index.js":56,"../Data.Functor/index.js":95,"../Data.Monoid.Additive/index.js":111,"../Data.Monoid.Conj/index.js":112,"../Data.Monoid.Disj/index.js":113,"../Data.Monoid.Dual/index.js":114,"../Data.Monoid.Endo/index.js":115,"../Data.Monoid.Multiplicative/index.js":116}],94:[function(require,module,exports){
"use strict";

exports.arrayMap = function (f) {
  return function (arr) {
    var l = arr.length;
    var result = new Array(l);
    for (var i = 0; i < l; i++) {
      result[i] = f(arr[i]);
    }
    return result;
  };
};

},{}],95:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var $foreign = require("./foreign.js");
var Control_Semigroupoid = require("../Control.Semigroupoid/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Functor = function (map) {
    this.map = map;
};
var map = function (dict) {
    return dict.map;
};
var mapFlipped = function (dictFunctor) {
    return function (fa) {
        return function (f) {
            return map(dictFunctor)(f)(fa);
        };
    };
};
var $$void = function (dictFunctor) {
    return map(dictFunctor)(Data_Function["const"](Data_Unit.unit));
};
var voidLeft = function (dictFunctor) {
    return function (f) {
        return function (x) {
            return map(dictFunctor)(Data_Function["const"](x))(f);
        };
    };
};
var voidRight = function (dictFunctor) {
    return function (x) {
        return map(dictFunctor)(Data_Function["const"](x));
    };
};
var functorFn = new Functor(Control_Semigroupoid.compose(Control_Semigroupoid.semigroupoidFn));
var functorArray = new Functor($foreign.arrayMap);
var flap = function (dictFunctor) {
    return function (ff) {
        return function (x) {
            return map(dictFunctor)(function (f) {
                return f(x);
            })(ff);
        };
    };
};
module.exports = {
    Functor: Functor,
    map: map,
    mapFlipped: mapFlipped,
    "void": $$void,
    voidRight: voidRight,
    voidLeft: voidLeft,
    flap: flap,
    functorFn: functorFn,
    functorArray: functorArray
};

},{"../Control.Semigroupoid/index.js":56,"../Data.Function/index.js":91,"../Data.Unit/index.js":162,"./foreign.js":94}],96:[function(require,module,exports){
"use strict";

exports.mapWithIndexArray = function (f) {
  return function (xs) {
    var l = xs.length;
    var result = Array(l);
    for (var i = 0; i < l; i++) {
      result[i] = f(i)(xs[i]);
    }
    return result;
  };
};

},{}],97:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var $foreign = require("./foreign.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Maybe_First = require("../Data.Maybe.First/index.js");
var Data_Maybe_Last = require("../Data.Maybe.Last/index.js");
var Data_Monoid_Additive = require("../Data.Monoid.Additive/index.js");
var Data_Monoid_Conj = require("../Data.Monoid.Conj/index.js");
var Data_Monoid_Disj = require("../Data.Monoid.Disj/index.js");
var Data_Monoid_Dual = require("../Data.Monoid.Dual/index.js");
var Data_Monoid_Multiplicative = require("../Data.Monoid.Multiplicative/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Prelude = require("../Prelude/index.js");
var FunctorWithIndex = function (Functor0, mapWithIndex) {
    this.Functor0 = Functor0;
    this.mapWithIndex = mapWithIndex;
};
var mapWithIndex = function (dict) {
    return dict.mapWithIndex;
};
var mapDefault = function (dictFunctorWithIndex) {
    return function (f) {
        return mapWithIndex(dictFunctorWithIndex)(Data_Function["const"](f));
    };
};
var functorWithIndexMultiplicative = new FunctorWithIndex(function () {
    return Data_Monoid_Multiplicative.functorMultiplicative;
}, function (f) {
    return Data_Functor.map(Data_Monoid_Multiplicative.functorMultiplicative)(f(Data_Unit.unit));
});
var functorWithIndexMaybe = new FunctorWithIndex(function () {
    return Data_Maybe.functorMaybe;
}, function (f) {
    return Data_Functor.map(Data_Maybe.functorMaybe)(f(Data_Unit.unit));
});
var functorWithIndexLast = new FunctorWithIndex(function () {
    return Data_Maybe_Last.functorLast;
}, function (f) {
    return Data_Functor.map(Data_Maybe_Last.functorLast)(f(Data_Unit.unit));
});
var functorWithIndexFirst = new FunctorWithIndex(function () {
    return Data_Maybe_First.functorFirst;
}, function (f) {
    return Data_Functor.map(Data_Maybe_First.functorFirst)(f(Data_Unit.unit));
});
var functorWithIndexDual = new FunctorWithIndex(function () {
    return Data_Monoid_Dual.functorDual;
}, function (f) {
    return Data_Functor.map(Data_Monoid_Dual.functorDual)(f(Data_Unit.unit));
});
var functorWithIndexDisj = new FunctorWithIndex(function () {
    return Data_Monoid_Disj.functorDisj;
}, function (f) {
    return Data_Functor.map(Data_Monoid_Disj.functorDisj)(f(Data_Unit.unit));
});
var functorWithIndexConj = new FunctorWithIndex(function () {
    return Data_Monoid_Conj.functorConj;
}, function (f) {
    return Data_Functor.map(Data_Monoid_Conj.functorConj)(f(Data_Unit.unit));
});
var functorWithIndexArray = new FunctorWithIndex(function () {
    return Data_Functor.functorArray;
}, $foreign.mapWithIndexArray);
var functorWithIndexAdditive = new FunctorWithIndex(function () {
    return Data_Monoid_Additive.functorAdditive;
}, function (f) {
    return Data_Functor.map(Data_Monoid_Additive.functorAdditive)(f(Data_Unit.unit));
});
module.exports = {
    FunctorWithIndex: FunctorWithIndex,
    mapWithIndex: mapWithIndex,
    mapDefault: mapDefault,
    functorWithIndexArray: functorWithIndexArray,
    functorWithIndexMaybe: functorWithIndexMaybe,
    functorWithIndexFirst: functorWithIndexFirst,
    functorWithIndexLast: functorWithIndexLast,
    functorWithIndexAdditive: functorWithIndexAdditive,
    functorWithIndexDual: functorWithIndexDual,
    functorWithIndexConj: functorWithIndexConj,
    functorWithIndexDisj: functorWithIndexDisj,
    functorWithIndexMultiplicative: functorWithIndexMultiplicative
};

},{"../Data.Function/index.js":91,"../Data.Functor/index.js":95,"../Data.Maybe.First/index.js":108,"../Data.Maybe.Last/index.js":109,"../Data.Maybe/index.js":110,"../Data.Monoid.Additive/index.js":111,"../Data.Monoid.Conj/index.js":112,"../Data.Monoid.Disj/index.js":113,"../Data.Monoid.Dual/index.js":114,"../Data.Monoid.Multiplicative/index.js":116,"../Data.Unit/index.js":162,"../Prelude/index.js":198,"./foreign.js":96}],98:[function(require,module,exports){
"use strict";

exports.boolConj = function (b1) {
  return function (b2) {
    return b1 && b2;
  };
};

exports.boolDisj = function (b1) {
  return function (b2) {
    return b1 || b2;
  };
};

exports.boolNot = function (b) {
  return !b;
};

},{}],99:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var $foreign = require("./foreign.js");
var Data_Symbol = require("../Data.Symbol/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Record_Unsafe = require("../Record.Unsafe/index.js");
var Type_Data_Row = require("../Type.Data.Row/index.js");
var Type_Data_RowList = require("../Type.Data.RowList/index.js");
var HeytingAlgebra = function (conj, disj, ff, implies, not, tt) {
    this.conj = conj;
    this.disj = disj;
    this.ff = ff;
    this.implies = implies;
    this.not = not;
    this.tt = tt;
};
var HeytingAlgebraRecord = function (conjRecord, disjRecord, ffRecord, impliesRecord, notRecord, ttRecord) {
    this.conjRecord = conjRecord;
    this.disjRecord = disjRecord;
    this.ffRecord = ffRecord;
    this.impliesRecord = impliesRecord;
    this.notRecord = notRecord;
    this.ttRecord = ttRecord;
};
var ttRecord = function (dict) {
    return dict.ttRecord;
};
var tt = function (dict) {
    return dict.tt;
};
var notRecord = function (dict) {
    return dict.notRecord;
};
var not = function (dict) {
    return dict.not;
};
var impliesRecord = function (dict) {
    return dict.impliesRecord;
};
var implies = function (dict) {
    return dict.implies;
};
var heytingAlgebraUnit = new HeytingAlgebra(function (v) {
    return function (v1) {
        return Data_Unit.unit;
    };
}, function (v) {
    return function (v1) {
        return Data_Unit.unit;
    };
}, Data_Unit.unit, function (v) {
    return function (v1) {
        return Data_Unit.unit;
    };
}, function (v) {
    return Data_Unit.unit;
}, Data_Unit.unit);
var heytingAlgebraRecordNil = new HeytingAlgebraRecord(function (v) {
    return function (v1) {
        return function (v2) {
            return {};
        };
    };
}, function (v) {
    return function (v1) {
        return function (v2) {
            return {};
        };
    };
}, function (v) {
    return function (v1) {
        return {};
    };
}, function (v) {
    return function (v1) {
        return function (v2) {
            return {};
        };
    };
}, function (v) {
    return function (v1) {
        return {};
    };
}, function (v) {
    return function (v1) {
        return {};
    };
});
var ffRecord = function (dict) {
    return dict.ffRecord;
};
var ff = function (dict) {
    return dict.ff;
};
var disjRecord = function (dict) {
    return dict.disjRecord;
};
var disj = function (dict) {
    return dict.disj;
};
var heytingAlgebraBoolean = new HeytingAlgebra($foreign.boolConj, $foreign.boolDisj, false, function (a) {
    return function (b) {
        return disj(heytingAlgebraBoolean)(not(heytingAlgebraBoolean)(a))(b);
    };
}, $foreign.boolNot, true);
var conjRecord = function (dict) {
    return dict.conjRecord;
};
var heytingAlgebraRecord = function (dictRowToList) {
    return function (dictHeytingAlgebraRecord) {
        return new HeytingAlgebra(conjRecord(dictHeytingAlgebraRecord)(Type_Data_RowList.RLProxy.value), disjRecord(dictHeytingAlgebraRecord)(Type_Data_RowList.RLProxy.value), ffRecord(dictHeytingAlgebraRecord)(Type_Data_RowList.RLProxy.value)(Type_Data_Row.RProxy.value), impliesRecord(dictHeytingAlgebraRecord)(Type_Data_RowList.RLProxy.value), notRecord(dictHeytingAlgebraRecord)(Type_Data_RowList.RLProxy.value), ttRecord(dictHeytingAlgebraRecord)(Type_Data_RowList.RLProxy.value)(Type_Data_Row.RProxy.value));
    };
};
var conj = function (dict) {
    return dict.conj;
};
var heytingAlgebraFunction = function (dictHeytingAlgebra) {
    return new HeytingAlgebra(function (f) {
        return function (g) {
            return function (a) {
                return conj(dictHeytingAlgebra)(f(a))(g(a));
            };
        };
    }, function (f) {
        return function (g) {
            return function (a) {
                return disj(dictHeytingAlgebra)(f(a))(g(a));
            };
        };
    }, function (v) {
        return ff(dictHeytingAlgebra);
    }, function (f) {
        return function (g) {
            return function (a) {
                return implies(dictHeytingAlgebra)(f(a))(g(a));
            };
        };
    }, function (f) {
        return function (a) {
            return not(dictHeytingAlgebra)(f(a));
        };
    }, function (v) {
        return tt(dictHeytingAlgebra);
    });
};
var heytingAlgebraRecordCons = function (dictIsSymbol) {
    return function (dictCons) {
        return function (dictHeytingAlgebraRecord) {
            return function (dictHeytingAlgebra) {
                return new HeytingAlgebraRecord(function (v) {
                    return function (ra) {
                        return function (rb) {
                            var tail = conjRecord(dictHeytingAlgebraRecord)(Type_Data_RowList.RLProxy.value)(ra)(rb);
                            var key = Data_Symbol.reflectSymbol(dictIsSymbol)(Data_Symbol.SProxy.value);
                            var insert = Record_Unsafe.unsafeSet(key);
                            var get = Record_Unsafe.unsafeGet(key);
                            return insert(conj(dictHeytingAlgebra)(get(ra))(get(rb)))(tail);
                        };
                    };
                }, function (v) {
                    return function (ra) {
                        return function (rb) {
                            var tail = disjRecord(dictHeytingAlgebraRecord)(Type_Data_RowList.RLProxy.value)(ra)(rb);
                            var key = Data_Symbol.reflectSymbol(dictIsSymbol)(Data_Symbol.SProxy.value);
                            var insert = Record_Unsafe.unsafeSet(key);
                            var get = Record_Unsafe.unsafeGet(key);
                            return insert(disj(dictHeytingAlgebra)(get(ra))(get(rb)))(tail);
                        };
                    };
                }, function (v) {
                    return function (row) {
                        var tail = ffRecord(dictHeytingAlgebraRecord)(Type_Data_RowList.RLProxy.value)(row);
                        var key = Data_Symbol.reflectSymbol(dictIsSymbol)(Data_Symbol.SProxy.value);
                        var insert = Record_Unsafe.unsafeSet(key);
                        return insert(ff(dictHeytingAlgebra))(tail);
                    };
                }, function (v) {
                    return function (ra) {
                        return function (rb) {
                            var tail = impliesRecord(dictHeytingAlgebraRecord)(Type_Data_RowList.RLProxy.value)(ra)(rb);
                            var key = Data_Symbol.reflectSymbol(dictIsSymbol)(Data_Symbol.SProxy.value);
                            var insert = Record_Unsafe.unsafeSet(key);
                            var get = Record_Unsafe.unsafeGet(key);
                            return insert(implies(dictHeytingAlgebra)(get(ra))(get(rb)))(tail);
                        };
                    };
                }, function (v) {
                    return function (row) {
                        var tail = notRecord(dictHeytingAlgebraRecord)(Type_Data_RowList.RLProxy.value)(row);
                        var key = Data_Symbol.reflectSymbol(dictIsSymbol)(Data_Symbol.SProxy.value);
                        var insert = Record_Unsafe.unsafeSet(key);
                        var get = Record_Unsafe.unsafeGet(key);
                        return insert(not(dictHeytingAlgebra)(get(row)))(tail);
                    };
                }, function (v) {
                    return function (row) {
                        var tail = ttRecord(dictHeytingAlgebraRecord)(Type_Data_RowList.RLProxy.value)(row);
                        var key = Data_Symbol.reflectSymbol(dictIsSymbol)(Data_Symbol.SProxy.value);
                        var insert = Record_Unsafe.unsafeSet(key);
                        return insert(tt(dictHeytingAlgebra))(tail);
                    };
                });
            };
        };
    };
};
module.exports = {
    HeytingAlgebra: HeytingAlgebra,
    tt: tt,
    ff: ff,
    implies: implies,
    conj: conj,
    disj: disj,
    not: not,
    HeytingAlgebraRecord: HeytingAlgebraRecord,
    ffRecord: ffRecord,
    ttRecord: ttRecord,
    impliesRecord: impliesRecord,
    conjRecord: conjRecord,
    disjRecord: disjRecord,
    notRecord: notRecord,
    heytingAlgebraBoolean: heytingAlgebraBoolean,
    heytingAlgebraUnit: heytingAlgebraUnit,
    heytingAlgebraFunction: heytingAlgebraFunction,
    heytingAlgebraRecord: heytingAlgebraRecord,
    heytingAlgebraRecordNil: heytingAlgebraRecordNil,
    heytingAlgebraRecordCons: heytingAlgebraRecordCons
};

},{"../Data.Symbol/index.js":150,"../Data.Unit/index.js":162,"../Record.Unsafe/index.js":200,"../Type.Data.Row/index.js":203,"../Type.Data.RowList/index.js":204,"./foreign.js":98}],100:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Alt = require("../Control.Alt/index.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Comonad = require("../Control.Comonad/index.js");
var Control_Extend = require("../Control.Extend/index.js");
var Control_Lazy = require("../Control.Lazy/index.js");
var Control_Monad = require("../Control.Monad/index.js");
var Data_BooleanAlgebra = require("../Data.BooleanAlgebra/index.js");
var Data_Bounded = require("../Data.Bounded/index.js");
var Data_CommutativeRing = require("../Data.CommutativeRing/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_EuclideanRing = require("../Data.EuclideanRing/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_FoldableWithIndex = require("../Data.FoldableWithIndex/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Functor_Invariant = require("../Data.Functor.Invariant/index.js");
var Data_FunctorWithIndex = require("../Data.FunctorWithIndex/index.js");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra/index.js");
var Data_Monoid = require("../Data.Monoid/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Ring = require("../Data.Ring/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Semigroup_Foldable = require("../Data.Semigroup.Foldable/index.js");
var Data_Semigroup_Traversable = require("../Data.Semigroup.Traversable/index.js");
var Data_Semiring = require("../Data.Semiring/index.js");
var Data_Show = require("../Data.Show/index.js");
var Data_Traversable = require("../Data.Traversable/index.js");
var Data_TraversableWithIndex = require("../Data.TraversableWithIndex/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Prelude = require("../Prelude/index.js");
var Identity = function (x) {
    return x;
};
var showIdentity = function (dictShow) {
    return new Data_Show.Show(function (v) {
        return "(Identity " + (Data_Show.show(dictShow)(v) + ")");
    });
};
var semiringIdentity = function (dictSemiring) {
    return dictSemiring;
};
var semigroupIdenity = function (dictSemigroup) {
    return dictSemigroup;
};
var ringIdentity = function (dictRing) {
    return dictRing;
};
var ordIdentity = function (dictOrd) {
    return dictOrd;
};
var newtypeIdentity = new Data_Newtype.Newtype(function (n) {
    return n;
}, Identity);
var monoidIdentity = function (dictMonoid) {
    return dictMonoid;
};
var lazyIdentity = function (dictLazy) {
    return dictLazy;
};
var heytingAlgebraIdentity = function (dictHeytingAlgebra) {
    return dictHeytingAlgebra;
};
var functorIdentity = new Data_Functor.Functor(function (f) {
    return function (m) {
        return f(m);
    };
});
var functorWithIndexIdentity = new Data_FunctorWithIndex.FunctorWithIndex(function () {
    return functorIdentity;
}, function (f) {
    return function (v) {
        return f(Data_Unit.unit)(v);
    };
});
var invariantIdentity = new Data_Functor_Invariant.Invariant(Data_Functor_Invariant.imapF(functorIdentity));
var foldableIdentity = new Data_Foldable.Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            return f(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(z)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(v)(z);
        };
    };
});
var foldableWithIndexIdentity = new Data_FoldableWithIndex.FoldableWithIndex(function () {
    return foldableIdentity;
}, function (dictMonoid) {
    return function (f) {
        return function (v) {
            return f(Data_Unit.unit)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(Data_Unit.unit)(z)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(Data_Unit.unit)(v)(z);
        };
    };
});
var traversableIdentity = new Data_Traversable.Traversable(function () {
    return foldableIdentity;
}, function () {
    return functorIdentity;
}, function (dictApplicative) {
    return function (v) {
        return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Identity)(v);
    };
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Identity)(f(v));
        };
    };
});
var traversableWithIndexIdentity = new Data_TraversableWithIndex.TraversableWithIndex(function () {
    return foldableWithIndexIdentity;
}, function () {
    return functorWithIndexIdentity;
}, function () {
    return traversableIdentity;
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Identity)(f(Data_Unit.unit)(v));
        };
    };
});
var foldable1Identity = new Data_Semigroup_Foldable.Foldable1(function () {
    return foldableIdentity;
}, function (dictSemigroup) {
    return function (v) {
        return v;
    };
}, function (dictSemigroup) {
    return function (f) {
        return function (v) {
            return f(v);
        };
    };
});
var traversable1Identity = new Data_Semigroup_Traversable.Traversable1(function () {
    return foldable1Identity;
}, function () {
    return traversableIdentity;
}, function (dictApply) {
    return function (v) {
        return Data_Functor.map(dictApply.Functor0())(Identity)(v);
    };
}, function (dictApply) {
    return function (f) {
        return function (v) {
            return Data_Functor.map(dictApply.Functor0())(Identity)(f(v));
        };
    };
});
var extendIdentity = new Control_Extend.Extend(function () {
    return functorIdentity;
}, function (f) {
    return function (m) {
        return f(m);
    };
});
var euclideanRingIdentity = function (dictEuclideanRing) {
    return dictEuclideanRing;
};
var eqIdentity = function (dictEq) {
    return dictEq;
};
var eq1Identity = new Data_Eq.Eq1(function (dictEq) {
    return Data_Eq.eq(eqIdentity(dictEq));
});
var ord1Identity = new Data_Ord.Ord1(function () {
    return eq1Identity;
}, function (dictOrd) {
    return Data_Ord.compare(ordIdentity(dictOrd));
});
var comonadIdentity = new Control_Comonad.Comonad(function () {
    return extendIdentity;
}, function (v) {
    return v;
});
var commutativeRingIdentity = function (dictCommutativeRing) {
    return dictCommutativeRing;
};
var boundedIdentity = function (dictBounded) {
    return dictBounded;
};
var booleanAlgebraIdentity = function (dictBooleanAlgebra) {
    return dictBooleanAlgebra;
};
var applyIdentity = new Control_Apply.Apply(function () {
    return functorIdentity;
}, function (v) {
    return function (v1) {
        return v(v1);
    };
});
var bindIdentity = new Control_Bind.Bind(function () {
    return applyIdentity;
}, function (v) {
    return function (f) {
        return f(v);
    };
});
var applicativeIdentity = new Control_Applicative.Applicative(function () {
    return applyIdentity;
}, Identity);
var monadIdentity = new Control_Monad.Monad(function () {
    return applicativeIdentity;
}, function () {
    return bindIdentity;
});
var altIdentity = new Control_Alt.Alt(function () {
    return functorIdentity;
}, function (x) {
    return function (v) {
        return x;
    };
});
module.exports = {
    Identity: Identity,
    newtypeIdentity: newtypeIdentity,
    eqIdentity: eqIdentity,
    ordIdentity: ordIdentity,
    boundedIdentity: boundedIdentity,
    heytingAlgebraIdentity: heytingAlgebraIdentity,
    booleanAlgebraIdentity: booleanAlgebraIdentity,
    semigroupIdenity: semigroupIdenity,
    monoidIdentity: monoidIdentity,
    semiringIdentity: semiringIdentity,
    euclideanRingIdentity: euclideanRingIdentity,
    ringIdentity: ringIdentity,
    commutativeRingIdentity: commutativeRingIdentity,
    lazyIdentity: lazyIdentity,
    showIdentity: showIdentity,
    eq1Identity: eq1Identity,
    ord1Identity: ord1Identity,
    functorIdentity: functorIdentity,
    functorWithIndexIdentity: functorWithIndexIdentity,
    invariantIdentity: invariantIdentity,
    altIdentity: altIdentity,
    applyIdentity: applyIdentity,
    applicativeIdentity: applicativeIdentity,
    bindIdentity: bindIdentity,
    monadIdentity: monadIdentity,
    extendIdentity: extendIdentity,
    comonadIdentity: comonadIdentity,
    foldableIdentity: foldableIdentity,
    foldable1Identity: foldable1Identity,
    foldableWithIndexIdentity: foldableWithIndexIdentity,
    traversableIdentity: traversableIdentity,
    traversable1Identity: traversable1Identity,
    traversableWithIndexIdentity: traversableWithIndexIdentity
};

},{"../Control.Alt/index.js":25,"../Control.Applicative/index.js":27,"../Control.Apply/index.js":29,"../Control.Bind/index.js":33,"../Control.Comonad/index.js":35,"../Control.Extend/index.js":37,"../Control.Lazy/index.js":38,"../Control.Monad/index.js":52,"../Data.BooleanAlgebra/index.js":74,"../Data.Bounded/index.js":76,"../Data.CommutativeRing/index.js":77,"../Data.Eq/index.js":82,"../Data.EuclideanRing/index.js":84,"../Data.Foldable/index.js":87,"../Data.FoldableWithIndex/index.js":88,"../Data.Functor.Invariant/index.js":93,"../Data.Functor/index.js":95,"../Data.FunctorWithIndex/index.js":97,"../Data.HeytingAlgebra/index.js":99,"../Data.Monoid/index.js":117,"../Data.Newtype/index.js":119,"../Data.Ord/index.js":130,"../Data.Ring/index.js":133,"../Data.Semigroup.Foldable/index.js":135,"../Data.Semigroup.Traversable/index.js":137,"../Data.Semigroup/index.js":139,"../Data.Semiring/index.js":141,"../Data.Show/index.js":143,"../Data.Traversable/index.js":154,"../Data.TraversableWithIndex/index.js":155,"../Data.Unit/index.js":162,"../Prelude/index.js":198}],101:[function(require,module,exports){
"use strict";

// module Data.Int.Bits

exports.and = function (n1) {
  return function (n2) {
    /* jshint bitwise: false */
    return n1 & n2;
  };
};

exports.or = function (n1) {
  return function (n2) {
    /* jshint bitwise: false */
    return n1 | n2;
  };
};

exports.xor = function (n1) {
  return function (n2) {
    /* jshint bitwise: false */
    return n1 ^ n2;
  };
};

exports.shl = function (n1) {
  return function (n2) {
    /* jshint bitwise: false */
    return n1 << n2;
  };
};

exports.shr = function (n1) {
  return function (n2) {
    /* jshint bitwise: false */
    return n1 >> n2;
  };
};

exports.zshr = function (n1) {
  return function (n2) {
    /* jshint bitwise: false */
    return n1 >>> n2;
  };
};

exports.complement = function (n) {
  /* jshint bitwise: false */
  return ~n;
};

},{}],102:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var $foreign = require("./foreign.js");
module.exports = {
    and: $foreign.and,
    or: $foreign.or,
    xor: $foreign.xor,
    shl: $foreign.shl,
    shr: $foreign.shr,
    zshr: $foreign.zshr,
    complement: $foreign.complement
};

},{"./foreign.js":101}],103:[function(require,module,exports){
"use strict";

exports.fromNumberImpl = function (just) {
  return function (nothing) {
    return function (n) {
      /* jshint bitwise: false */
      return (n | 0) === n ? just(n) : nothing;
    };
  };
};

exports.toNumber = function (n) {
  return n;
};

exports.fromStringAsImpl = function (just) {
  return function (nothing) {
    return function (radix) {
      var digits;
      if (radix < 11) {
        digits = "[0-" + (radix - 1).toString() + "]";
      } else if (radix === 11) {
        digits = "[0-9a]";
      } else {
        digits = "[0-9a-" + String.fromCharCode(86 + radix) + "]";
      }
      var pattern = new RegExp("^[\\+\\-]?" + digits + "+$", "i");

      return function (s) {
        /* jshint bitwise: false */
        if (pattern.test(s)) {
          var i = parseInt(s, radix);
          return (i | 0) === i ? just(i) : nothing;
        } else {
          return nothing;
        }
      };
    };
  };
};

exports.toStringAs = function (radix) {
  return function (i) {
    return i.toString(radix);
  };
};


exports.quot = function (x) {
  return function (y) {
    /* jshint bitwise: false */
    return x / y | 0;
  };
};

exports.rem = function (x) {
  return function (y) {
    return x % y;
  };
};

exports.pow = function (x) {
  return function (y) {
    /* jshint bitwise: false */
    return Math.pow(x,y) | 0;
  };
};

},{}],104:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var $foreign = require("./foreign.js");
var Control_Category = require("../Control.Category/index.js");
var Control_Semigroupoid = require("../Control.Semigroupoid/index.js");
var Data_Boolean = require("../Data.Boolean/index.js");
var Data_Bounded = require("../Data.Bounded/index.js");
var Data_CommutativeRing = require("../Data.CommutativeRing/index.js");
var Data_DivisionRing = require("../Data.DivisionRing/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_EuclideanRing = require("../Data.EuclideanRing/index.js");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra/index.js");
var Data_Int_Bits = require("../Data.Int.Bits/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Ordering = require("../Data.Ordering/index.js");
var Data_Ring = require("../Data.Ring/index.js");
var Data_Semiring = require("../Data.Semiring/index.js");
var Data_Show = require("../Data.Show/index.js");
var Global = require("../Global/index.js");
var $$Math = require("../Math/index.js");
var Prelude = require("../Prelude/index.js");
var Radix = function (x) {
    return x;
};
var Even = (function () {
    function Even() {

    };
    Even.value = new Even();
    return Even;
})();
var Odd = (function () {
    function Odd() {

    };
    Odd.value = new Odd();
    return Odd;
})();
var showParity = new Data_Show.Show(function (v) {
    if (v instanceof Even) {
        return "Even";
    };
    if (v instanceof Odd) {
        return "Odd";
    };
    throw new Error("Failed pattern match at Data.Int (line 112, column 1 - line 112, column 35): " + [ v.constructor.name ]);
});
var radix = function (n) {
    if (n >= 2 && n <= 36) {
        return new Data_Maybe.Just(n);
    };
    if (Data_Boolean.otherwise) {
        return Data_Maybe.Nothing.value;
    };
    throw new Error("Failed pattern match at Data.Int (line 193, column 1 - line 193, column 28): " + [ n.constructor.name ]);
};
var odd = function (x) {
    return (x & 1) !== 0;
};
var octal = 8;
var hexadecimal = 16;
var fromStringAs = $foreign.fromStringAsImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var fromString = fromStringAs(10);
var fromNumber = $foreign.fromNumberImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var unsafeClamp = function (x) {
    if (x === Global.infinity) {
        return 0;
    };
    if (x === -Global.infinity) {
        return 0;
    };
    if (x >= $foreign.toNumber(Data_Bounded.top(Data_Bounded.boundedInt))) {
        return Data_Bounded.top(Data_Bounded.boundedInt);
    };
    if (x <= $foreign.toNumber(Data_Bounded.bottom(Data_Bounded.boundedInt))) {
        return Data_Bounded.bottom(Data_Bounded.boundedInt);
    };
    if (Data_Boolean.otherwise) {
        return Data_Maybe.fromMaybe(0)(fromNumber(x));
    };
    throw new Error("Failed pattern match at Data.Int (line 66, column 1 - line 66, column 29): " + [ x.constructor.name ]);
};
var round = function ($23) {
    return unsafeClamp($$Math.round($23));
};
var floor = function ($24) {
    return unsafeClamp($$Math.floor($24));
};
var even = function (x) {
    return (x & 1) === 0;
};
var parity = function (n) {
    var $14 = even(n);
    if ($14) {
        return Even.value;
    };
    return Odd.value;
};
var eqParity = new Data_Eq.Eq(function (x) {
    return function (y) {
        if (x instanceof Even && y instanceof Even) {
            return true;
        };
        if (x instanceof Odd && y instanceof Odd) {
            return true;
        };
        return false;
    };
});
var ordParity = new Data_Ord.Ord(function () {
    return eqParity;
}, function (x) {
    return function (y) {
        if (x instanceof Even && y instanceof Even) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof Even) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof Even) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof Odd && y instanceof Odd) {
            return Data_Ordering.EQ.value;
        };
        throw new Error("Failed pattern match at Data.Int (line 110, column 8 - line 110, column 40): " + [ x.constructor.name, y.constructor.name ]);
    };
});
var semiringParity = new Data_Semiring.Semiring(function (x) {
    return function (y) {
        var $19 = Data_Eq.eq(eqParity)(x)(y);
        if ($19) {
            return Even.value;
        };
        return Odd.value;
    };
}, function (v) {
    return function (v1) {
        if (v instanceof Odd && v1 instanceof Odd) {
            return Odd.value;
        };
        return Even.value;
    };
}, Odd.value, Even.value);
var ringParity = new Data_Ring.Ring(function () {
    return semiringParity;
}, Data_Semiring.add(semiringParity));
var divisionRingParity = new Data_DivisionRing.DivisionRing(function () {
    return ringParity;
}, Control_Category.identity(Control_Category.categoryFn));
var decimal = 10;
var commutativeRingParity = new Data_CommutativeRing.CommutativeRing(function () {
    return ringParity;
});
var euclideanRingParity = new Data_EuclideanRing.EuclideanRing(function () {
    return commutativeRingParity;
}, function (v) {
    if (v instanceof Even) {
        return 0;
    };
    if (v instanceof Odd) {
        return 1;
    };
    throw new Error("Failed pattern match at Data.Int (line 132, column 1 - line 132, column 53): " + [ v.constructor.name ]);
}, function (x) {
    return function (v) {
        return x;
    };
}, function (v) {
    return function (v1) {
        return Even.value;
    };
});
var ceil = function ($25) {
    return unsafeClamp($$Math.ceil($25));
};
var boundedParity = new Data_Bounded.Bounded(function () {
    return ordParity;
}, Even.value, Odd.value);
var binary = 2;
var base36 = 36;
module.exports = {
    fromNumber: fromNumber,
    ceil: ceil,
    floor: floor,
    round: round,
    fromString: fromString,
    radix: radix,
    binary: binary,
    octal: octal,
    decimal: decimal,
    hexadecimal: hexadecimal,
    base36: base36,
    fromStringAs: fromStringAs,
    Even: Even,
    Odd: Odd,
    parity: parity,
    even: even,
    odd: odd,
    eqParity: eqParity,
    ordParity: ordParity,
    showParity: showParity,
    boundedParity: boundedParity,
    semiringParity: semiringParity,
    ringParity: ringParity,
    commutativeRingParity: commutativeRingParity,
    euclideanRingParity: euclideanRingParity,
    divisionRingParity: divisionRingParity,
    toNumber: $foreign.toNumber,
    toStringAs: $foreign.toStringAs,
    quot: $foreign.quot,
    rem: $foreign.rem,
    pow: $foreign.pow
};

},{"../Control.Category/index.js":34,"../Control.Semigroupoid/index.js":56,"../Data.Boolean/index.js":73,"../Data.Bounded/index.js":76,"../Data.CommutativeRing/index.js":77,"../Data.DivisionRing/index.js":79,"../Data.Eq/index.js":82,"../Data.EuclideanRing/index.js":84,"../Data.HeytingAlgebra/index.js":99,"../Data.Int.Bits/index.js":102,"../Data.Maybe/index.js":110,"../Data.Ord/index.js":130,"../Data.Ordering/index.js":131,"../Data.Ring/index.js":133,"../Data.Semiring/index.js":141,"../Data.Show/index.js":143,"../Global/index.js":180,"../Math/index.js":183,"../Prelude/index.js":198,"./foreign.js":103}],105:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Bind = require("../Control.Bind/index.js");
var Control_Category = require("../Control.Category/index.js");
var Control_Semigroupoid = require("../Control.Semigroupoid/index.js");
var Data_Boolean = require("../Data.Boolean/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_FunctorWithIndex = require("../Data.FunctorWithIndex/index.js");
var Data_List = require("../Data.List/index.js");
var Data_List_Types = require("../Data.List.Types/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_NonEmpty = require("../Data.NonEmpty/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Ring = require("../Data.Ring/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Semigroup_Foldable = require("../Data.Semigroup.Foldable/index.js");
var Data_Semigroup_Traversable = require("../Data.Semigroup.Traversable/index.js");
var Data_Semiring = require("../Data.Semiring/index.js");
var Data_Traversable = require("../Data.Traversable/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var Data_Unfoldable = require("../Data.Unfoldable/index.js");
var Partial_Unsafe = require("../Partial.Unsafe/index.js");
var Prelude = require("../Prelude/index.js");
var zipWith = function (f) {
    return function (v) {
        return function (v1) {
            return new Data_NonEmpty.NonEmpty(f(v.value0)(v1.value0), Data_List.zipWith(f)(v.value1)(v1.value1));
        };
    };
};
var zipWithA = function (dictApplicative) {
    return function (f) {
        return function (xs) {
            return function (ys) {
                return Data_Semigroup_Traversable.sequence1(Data_List_Types.traversable1NonEmptyList)(dictApplicative.Apply0())(zipWith(f)(xs)(ys));
            };
        };
    };
};
var zip = zipWith(Data_Tuple.Tuple.create);
var wrappedOperation2 = function (name) {
    return function (f) {
        return function (v) {
            return function (v1) {
                var v2 = f(new Data_List_Types.Cons(v.value0, v.value1))(new Data_List_Types.Cons(v1.value0, v1.value1));
                if (v2 instanceof Data_List_Types.Cons) {
                    return new Data_NonEmpty.NonEmpty(v2.value0, v2.value1);
                };
                if (v2 instanceof Data_List_Types.Nil) {
                    return Partial_Unsafe.unsafeCrashWith("Impossible: empty list in NonEmptyList " + name);
                };
                throw new Error("Failed pattern match at Data.List.NonEmpty (line 104, column 3 - line 106, column 81): " + [ v2.constructor.name ]);
            };
        };
    };
};
var wrappedOperation = function (name) {
    return function (f) {
        return function (v) {
            var v1 = f(new Data_List_Types.Cons(v.value0, v.value1));
            if (v1 instanceof Data_List_Types.Cons) {
                return new Data_NonEmpty.NonEmpty(v1.value0, v1.value1);
            };
            if (v1 instanceof Data_List_Types.Nil) {
                return Partial_Unsafe.unsafeCrashWith("Impossible: empty list in NonEmptyList " + name);
            };
            throw new Error("Failed pattern match at Data.List.NonEmpty (line 91, column 3 - line 93, column 81): " + [ v1.constructor.name ]);
        };
    };
};
var updateAt = function (i) {
    return function (a) {
        return function (v) {
            if (i === 0) {
                return new Data_Maybe.Just(new Data_NonEmpty.NonEmpty(a, v.value1));
            };
            if (Data_Boolean.otherwise) {
                return Data_Functor.map(Data_Maybe.functorMaybe)(function ($161) {
                    return Data_List_Types.NonEmptyList((function (v1) {
                        return new Data_NonEmpty.NonEmpty(v.value0, v1);
                    })($161));
                })(Data_List.updateAt(i - 1 | 0)(a)(v.value1));
            };
            throw new Error("Failed pattern match at Data.List.NonEmpty (line 197, column 1 - line 197, column 75): " + [ i.constructor.name, a.constructor.name, v.constructor.name ]);
        };
    };
};
var unzip = function (ts) {
    return new Data_Tuple.Tuple(Data_Functor.map(Data_List_Types.functorNonEmptyList)(Data_Tuple.fst)(ts), Data_Functor.map(Data_List_Types.functorNonEmptyList)(Data_Tuple.snd)(ts));
};
var unsnoc = function (v) {
    var v1 = Data_List.unsnoc(v.value1);
    if (v1 instanceof Data_Maybe.Nothing) {
        return {
            init: Data_List_Types.Nil.value,
            last: v.value0
        };
    };
    if (v1 instanceof Data_Maybe.Just) {
        return {
            init: new Data_List_Types.Cons(v.value0, v1.value0.init),
            last: v1.value0.last
        };
    };
    throw new Error("Failed pattern match at Data.List.NonEmpty (line 159, column 35 - line 161, column 50): " + [ v1.constructor.name ]);
};
var unionBy = function ($162) {
    return wrappedOperation2("unionBy")(Data_List.unionBy($162));
};
var union = function (dictEq) {
    return wrappedOperation2("union")(Data_List.union(dictEq));
};
var uncons = function (v) {
    return {
        head: v.value0,
        tail: v.value1
    };
};
var toList = function (v) {
    return new Data_List_Types.Cons(v.value0, v.value1);
};
var toUnfoldable = function (dictUnfoldable) {
    return function ($163) {
        return Data_Unfoldable.unfoldr(dictUnfoldable)(function (xs) {
            return Data_Functor.map(Data_Maybe.functorMaybe)(function (rec) {
                return new Data_Tuple.Tuple(rec.head, rec.tail);
            })(Data_List.uncons(xs));
        })(toList($163));
    };
};
var tail = function (v) {
    return v.value1;
};
var sortBy = function ($164) {
    return wrappedOperation("sortBy")(Data_List.sortBy($164));
};
var sort = function (dictOrd) {
    return function (xs) {
        return sortBy(Data_Ord.compare(dictOrd))(xs);
    };
};
var snoc = function (v) {
    return function (y) {
        return new Data_NonEmpty.NonEmpty(v.value0, Data_List.snoc(v.value1)(y));
    };
};
var singleton = function ($165) {
    return Data_List_Types.NonEmptyList(Data_NonEmpty.singleton(Data_List_Types.plusList)($165));
};
var snoc$prime = function (v) {
    return function (y) {
        if (v instanceof Data_List_Types.Cons) {
            return new Data_NonEmpty.NonEmpty(v.value0, Data_List.snoc(v.value1)(y));
        };
        if (v instanceof Data_List_Types.Nil) {
            return singleton(y);
        };
        throw new Error("Failed pattern match at Data.List.NonEmpty (line 139, column 1 - line 139, column 51): " + [ v.constructor.name, y.constructor.name ]);
    };
};
var reverse = wrappedOperation("reverse")(Data_List.reverse);
var nubBy = function ($166) {
    return wrappedOperation("nubBy")(Data_List.nubBy($166));
};
var nub = function (dictEq) {
    return wrappedOperation("nub")(Data_List.nub(dictEq));
};
var modifyAt = function (i) {
    return function (f) {
        return function (v) {
            if (i === 0) {
                return new Data_Maybe.Just(new Data_NonEmpty.NonEmpty(f(v.value0), v.value1));
            };
            if (Data_Boolean.otherwise) {
                return Data_Functor.map(Data_Maybe.functorMaybe)(function ($167) {
                    return Data_List_Types.NonEmptyList((function (v1) {
                        return new Data_NonEmpty.NonEmpty(v.value0, v1);
                    })($167));
                })(Data_List.modifyAt(i - 1 | 0)(f)(v.value1));
            };
            throw new Error("Failed pattern match at Data.List.NonEmpty (line 202, column 1 - line 202, column 82): " + [ i.constructor.name, f.constructor.name, v.constructor.name ]);
        };
    };
};
var mapWithIndex = Data_FunctorWithIndex.mapWithIndex(Data_List_Types.functorWithIndexNonEmptyList);
var lift = function (f) {
    return function (v) {
        return f(new Data_List_Types.Cons(v.value0, v.value1));
    };
};
var mapMaybe = function ($168) {
    return lift(Data_List.mapMaybe($168));
};
var partition = function ($169) {
    return lift(Data_List.partition($169));
};
var span = function ($170) {
    return lift(Data_List.span($170));
};
var take = function ($171) {
    return lift(Data_List.take($171));
};
var takeWhile = function ($172) {
    return lift(Data_List.takeWhile($172));
};
var length = function (v) {
    return 1 + Data_List.length(v.value1) | 0;
};
var last = function (v) {
    return Data_Maybe.fromMaybe(v.value0)(Data_List.last(v.value1));
};
var intersectBy = function ($173) {
    return wrappedOperation2("intersectBy")(Data_List.intersectBy($173));
};
var intersect = function (dictEq) {
    return wrappedOperation2("intersect")(Data_List.intersect(dictEq));
};
var insertAt = function (i) {
    return function (a) {
        return function (v) {
            if (i === 0) {
                return new Data_Maybe.Just(new Data_NonEmpty.NonEmpty(a, new Data_List_Types.Cons(v.value0, v.value1)));
            };
            if (Data_Boolean.otherwise) {
                return Data_Functor.map(Data_Maybe.functorMaybe)(function ($174) {
                    return Data_List_Types.NonEmptyList((function (v1) {
                        return new Data_NonEmpty.NonEmpty(v.value0, v1);
                    })($174));
                })(Data_List.insertAt(i - 1 | 0)(a)(v.value1));
            };
            throw new Error("Failed pattern match at Data.List.NonEmpty (line 192, column 1 - line 192, column 75): " + [ i.constructor.name, a.constructor.name, v.constructor.name ]);
        };
    };
};
var init = function (v) {
    return Data_Maybe.maybe(Data_List_Types.Nil.value)(function (v1) {
        return new Data_List_Types.Cons(v.value0, v1);
    })(Data_List.init(v.value1));
};
var index = function (v) {
    return function (i) {
        if (i === 0) {
            return new Data_Maybe.Just(v.value0);
        };
        if (Data_Boolean.otherwise) {
            return Data_List.index(v.value1)(i - 1 | 0);
        };
        throw new Error("Failed pattern match at Data.List.NonEmpty (line 166, column 1 - line 166, column 52): " + [ v.constructor.name, i.constructor.name ]);
    };
};
var head = function (v) {
    return v.value0;
};
var groupBy = function ($175) {
    return wrappedOperation("groupBy")(Data_List.groupBy($175));
};
var group$prime = function (dictOrd) {
    return wrappedOperation("group'")(Data_List["group'"](dictOrd));
};
var group = function (dictEq) {
    return wrappedOperation("group")(Data_List.group(dictEq));
};
var fromList = function (v) {
    if (v instanceof Data_List_Types.Nil) {
        return Data_Maybe.Nothing.value;
    };
    if (v instanceof Data_List_Types.Cons) {
        return new Data_Maybe.Just(new Data_NonEmpty.NonEmpty(v.value0, v.value1));
    };
    throw new Error("Failed pattern match at Data.List.NonEmpty (line 120, column 1 - line 120, column 57): " + [ v.constructor.name ]);
};
var fromFoldable = function (dictFoldable) {
    return function ($176) {
        return fromList(Data_List.fromFoldable(dictFoldable)($176));
    };
};
var foldM = function (dictMonad) {
    return function (f) {
        return function (a) {
            return function (v) {
                return Control_Bind.bind(dictMonad.Bind1())(f(a)(v.value0))(function (a$prime) {
                    return Data_List.foldM(dictMonad)(f)(a$prime)(v.value1);
                });
            };
        };
    };
};
var findLastIndex = function (f) {
    return function (v) {
        var v1 = Data_List.findLastIndex(f)(v.value1);
        if (v1 instanceof Data_Maybe.Just) {
            return new Data_Maybe.Just(v1.value0 + 1 | 0);
        };
        if (v1 instanceof Data_Maybe.Nothing) {
            if (f(v.value0)) {
                return new Data_Maybe.Just(0);
            };
            if (Data_Boolean.otherwise) {
                return Data_Maybe.Nothing.value;
            };
        };
        throw new Error("Failed pattern match at Data.List.NonEmpty (line 186, column 3 - line 190, column 29): " + [ v1.constructor.name ]);
    };
};
var findIndex = function (f) {
    return function (v) {
        if (f(v.value0)) {
            return new Data_Maybe.Just(0);
        };
        if (Data_Boolean.otherwise) {
            return Data_Functor.map(Data_Maybe.functorMaybe)(function (v1) {
                return v1 + 1 | 0;
            })(Data_List.findIndex(f)(v.value1));
        };
        throw new Error("Failed pattern match at Data.List.NonEmpty (line 179, column 1 - line 179, column 69): " + [ f.constructor.name, v.constructor.name ]);
    };
};
var filterM = function (dictMonad) {
    return function ($177) {
        return lift(Data_List.filterM(dictMonad)($177));
    };
};
var filter = function ($178) {
    return lift(Data_List.filter($178));
};
var elemLastIndex = function (dictEq) {
    return function (x) {
        return findLastIndex(function (v) {
            return Data_Eq.eq(dictEq)(v)(x);
        });
    };
};
var elemIndex = function (dictEq) {
    return function (x) {
        return findIndex(function (v) {
            return Data_Eq.eq(dictEq)(v)(x);
        });
    };
};
var dropWhile = function ($179) {
    return lift(Data_List.dropWhile($179));
};
var drop = function ($180) {
    return lift(Data_List.drop($180));
};
var cons$prime = function (x) {
    return function (xs) {
        return new Data_NonEmpty.NonEmpty(x, xs);
    };
};
var cons = function (y) {
    return function (v) {
        return new Data_NonEmpty.NonEmpty(y, new Data_List_Types.Cons(v.value0, v.value1));
    };
};
var concatMap = Data_Function.flip(Control_Bind.bind(Data_List_Types.bindNonEmptyList));
var concat = function (v) {
    return Control_Bind.bind(Data_List_Types.bindNonEmptyList)(v)(Control_Category.identity(Control_Category.categoryFn));
};
var catMaybes = lift(Data_List.catMaybes);
var appendFoldable = function (dictFoldable) {
    return function (v) {
        return function (ys) {
            return new Data_NonEmpty.NonEmpty(v.value0, Data_Semigroup.append(Data_List_Types.semigroupList)(v.value1)(Data_List.fromFoldable(dictFoldable)(ys)));
        };
    };
};
module.exports = {
    toUnfoldable: toUnfoldable,
    fromFoldable: fromFoldable,
    fromList: fromList,
    toList: toList,
    singleton: singleton,
    length: length,
    cons: cons,
    "cons'": cons$prime,
    snoc: snoc,
    "snoc'": snoc$prime,
    head: head,
    last: last,
    tail: tail,
    init: init,
    uncons: uncons,
    unsnoc: unsnoc,
    index: index,
    elemIndex: elemIndex,
    elemLastIndex: elemLastIndex,
    findIndex: findIndex,
    findLastIndex: findLastIndex,
    insertAt: insertAt,
    updateAt: updateAt,
    modifyAt: modifyAt,
    reverse: reverse,
    concat: concat,
    concatMap: concatMap,
    filter: filter,
    filterM: filterM,
    mapMaybe: mapMaybe,
    catMaybes: catMaybes,
    appendFoldable: appendFoldable,
    mapWithIndex: mapWithIndex,
    sort: sort,
    sortBy: sortBy,
    take: take,
    takeWhile: takeWhile,
    drop: drop,
    dropWhile: dropWhile,
    span: span,
    group: group,
    "group'": group$prime,
    groupBy: groupBy,
    partition: partition,
    nub: nub,
    nubBy: nubBy,
    union: union,
    unionBy: unionBy,
    intersect: intersect,
    intersectBy: intersectBy,
    zipWith: zipWith,
    zipWithA: zipWithA,
    zip: zip,
    unzip: unzip,
    foldM: foldM
};

},{"../Control.Bind/index.js":33,"../Control.Category/index.js":34,"../Control.Semigroupoid/index.js":56,"../Data.Boolean/index.js":73,"../Data.Eq/index.js":82,"../Data.Foldable/index.js":87,"../Data.Function/index.js":91,"../Data.Functor/index.js":95,"../Data.FunctorWithIndex/index.js":97,"../Data.List.Types/index.js":106,"../Data.List/index.js":107,"../Data.Maybe/index.js":110,"../Data.NonEmpty/index.js":120,"../Data.Ord/index.js":130,"../Data.Ring/index.js":133,"../Data.Semigroup.Foldable/index.js":135,"../Data.Semigroup.Traversable/index.js":137,"../Data.Semigroup/index.js":139,"../Data.Semiring/index.js":141,"../Data.Traversable/index.js":154,"../Data.Tuple/index.js":156,"../Data.Unfoldable/index.js":158,"../Partial.Unsafe/index.js":195,"../Prelude/index.js":198}],106:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Alt = require("../Control.Alt/index.js");
var Control_Alternative = require("../Control.Alternative/index.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Category = require("../Control.Category/index.js");
var Control_Comonad = require("../Control.Comonad/index.js");
var Control_Extend = require("../Control.Extend/index.js");
var Control_Monad = require("../Control.Monad/index.js");
var Control_MonadPlus = require("../Control.MonadPlus/index.js");
var Control_MonadZero = require("../Control.MonadZero/index.js");
var Control_Plus = require("../Control.Plus/index.js");
var Control_Semigroupoid = require("../Control.Semigroupoid/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_FoldableWithIndex = require("../Data.FoldableWithIndex/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_FunctorWithIndex = require("../Data.FunctorWithIndex/index.js");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Monoid = require("../Data.Monoid/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_NonEmpty = require("../Data.NonEmpty/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Ordering = require("../Data.Ordering/index.js");
var Data_Ring = require("../Data.Ring/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Semigroup_Foldable = require("../Data.Semigroup.Foldable/index.js");
var Data_Semigroup_Traversable = require("../Data.Semigroup.Traversable/index.js");
var Data_Semiring = require("../Data.Semiring/index.js");
var Data_Show = require("../Data.Show/index.js");
var Data_Traversable = require("../Data.Traversable/index.js");
var Data_TraversableWithIndex = require("../Data.TraversableWithIndex/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var Data_Unfoldable = require("../Data.Unfoldable/index.js");
var Data_Unfoldable1 = require("../Data.Unfoldable1/index.js");
var Prelude = require("../Prelude/index.js");
var Nil = (function () {
    function Nil() {

    };
    Nil.value = new Nil();
    return Nil;
})();
var Cons = (function () {
    function Cons(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Cons.create = function (value0) {
        return function (value1) {
            return new Cons(value0, value1);
        };
    };
    return Cons;
})();
var NonEmptyList = function (x) {
    return x;
};
var toList = function (v) {
    return new Cons(v.value0, v.value1);
};
var newtypeNonEmptyList = new Data_Newtype.Newtype(function (n) {
    return n;
}, NonEmptyList);
var nelCons = function (a) {
    return function (v) {
        return new Data_NonEmpty.NonEmpty(a, new Cons(v.value0, v.value1));
    };
};
var foldableList = new Data_Foldable.Foldable(function (dictMonoid) {
    return function (f) {
        return Data_Foldable.foldl(foldableList)(function (acc) {
            return function ($174) {
                return Data_Semigroup.append(dictMonoid.Semigroup0())(acc)(f($174));
            };
        })(Data_Monoid.mempty(dictMonoid));
    };
}, function (f) {
    var go = function ($copy_b) {
        return function ($copy_v) {
            var $tco_var_b = $copy_b;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(b, v) {
                if (v instanceof Nil) {
                    $tco_done = true;
                    return b;
                };
                if (v instanceof Cons) {
                    $tco_var_b = f(b)(v.value0);
                    $copy_v = v.value1;
                    return;
                };
                throw new Error("Failed pattern match at Data.List.Types (line 81, column 12 - line 83, column 30): " + [ v.constructor.name ]);
            };
            while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_b, $copy_v);
            };
            return $tco_result;
        };
    };
    return go;
}, function (f) {
    return function (b) {
        var rev = Data_Foldable.foldl(foldableList)(Data_Function.flip(Cons.create))(Nil.value);
        return function ($175) {
            return Data_Foldable.foldl(foldableList)(Data_Function.flip(f))(b)(rev($175));
        };
    };
});
var foldableNonEmptyList = Data_NonEmpty.foldableNonEmpty(foldableList);
var foldableWithIndexList = new Data_FoldableWithIndex.FoldableWithIndex(function () {
    return foldableList;
}, function (dictMonoid) {
    return function (f) {
        return Data_FoldableWithIndex.foldlWithIndex(foldableWithIndexList)(function (i) {
            return function (acc) {
                return function ($176) {
                    return Data_Semigroup.append(dictMonoid.Semigroup0())(acc)(f(i)($176));
                };
            };
        })(Data_Monoid.mempty(dictMonoid));
    };
}, function (f) {
    return function (acc) {
        return function ($177) {
            return Data_Tuple.snd(Data_Foldable.foldl(foldableList)(function (v) {
                return function (a) {
                    return new Data_Tuple.Tuple(v.value0 + 1 | 0, f(v.value0)(v.value1)(a));
                };
            })(new Data_Tuple.Tuple(0, acc))($177));
        };
    };
}, function (f) {
    return function (b) {
        return function (xs) {
            var v = (function () {
                var rev = Data_Foldable.foldl(foldableList)(function (v1) {
                    return function (a) {
                        return new Data_Tuple.Tuple(v1.value0 + 1 | 0, new Cons(a, v1.value1));
                    };
                });
                return rev(new Data_Tuple.Tuple(0, Nil.value))(xs);
            })();
            return Data_Tuple.snd(Data_Foldable.foldl(foldableList)(function (v1) {
                return function (a) {
                    return new Data_Tuple.Tuple(v1.value0 - 1 | 0, f(v1.value0 - 1 | 0)(a)(v1.value1));
                };
            })(new Data_Tuple.Tuple(v.value0, b))(v.value1));
        };
    };
});
var foldableWithIndexNonEmptyList = new Data_FoldableWithIndex.FoldableWithIndex(function () {
    return foldableNonEmptyList;
}, function (dictMonoid) {
    return function (f) {
        return function (v) {
            return Data_FoldableWithIndex.foldMapWithIndex(Data_NonEmpty.foldableWithIndexNonEmpty(foldableWithIndexList))(dictMonoid)(function ($178) {
                return f(Data_Maybe.maybe(0)(Data_Semiring.add(Data_Semiring.semiringInt)(1))($178));
            })(v);
        };
    };
}, function (f) {
    return function (b) {
        return function (v) {
            return Data_FoldableWithIndex.foldlWithIndex(Data_NonEmpty.foldableWithIndexNonEmpty(foldableWithIndexList))(function ($179) {
                return f(Data_Maybe.maybe(0)(Data_Semiring.add(Data_Semiring.semiringInt)(1))($179));
            })(b)(v);
        };
    };
}, function (f) {
    return function (b) {
        return function (v) {
            return Data_FoldableWithIndex.foldrWithIndex(Data_NonEmpty.foldableWithIndexNonEmpty(foldableWithIndexList))(function ($180) {
                return f(Data_Maybe.maybe(0)(Data_Semiring.add(Data_Semiring.semiringInt)(1))($180));
            })(b)(v);
        };
    };
});
var functorList = new Data_Functor.Functor(function (f) {
    return Data_Foldable.foldr(foldableList)(function (x) {
        return function (acc) {
            return new Cons(f(x), acc);
        };
    })(Nil.value);
});
var functorNonEmptyList = Data_NonEmpty.functorNonEmpty(functorList);
var functorWithIndexList = new Data_FunctorWithIndex.FunctorWithIndex(function () {
    return functorList;
}, function (f) {
    return Data_FoldableWithIndex.foldrWithIndex(foldableWithIndexList)(function (i) {
        return function (x) {
            return function (acc) {
                return new Cons(f(i)(x), acc);
            };
        };
    })(Nil.value);
});
var functorWithIndexNonEmptyList = new Data_FunctorWithIndex.FunctorWithIndex(function () {
    return functorNonEmptyList;
}, function (fn) {
    return function (v) {
        return NonEmptyList(Data_FunctorWithIndex.mapWithIndex(Data_NonEmpty.functorWithIndex(functorWithIndexList))(function ($181) {
            return fn(Data_Maybe.maybe(0)(Data_Semiring.add(Data_Semiring.semiringInt)(1))($181));
        })(v));
    };
});
var semigroupList = new Data_Semigroup.Semigroup(function (xs) {
    return function (ys) {
        return Data_Foldable.foldr(foldableList)(Cons.create)(ys)(xs);
    };
});
var monoidList = new Data_Monoid.Monoid(function () {
    return semigroupList;
}, Nil.value);
var semigroupNonEmptyList = new Data_Semigroup.Semigroup(function (v) {
    return function (as$prime) {
        return new Data_NonEmpty.NonEmpty(v.value0, Data_Semigroup.append(semigroupList)(v.value1)(toList(as$prime)));
    };
});
var showList = function (dictShow) {
    return new Data_Show.Show(function (v) {
        if (v instanceof Nil) {
            return "Nil";
        };
        return "(" + (Data_Foldable.intercalate(foldableList)(Data_Monoid.monoidString)(" : ")(Data_Functor.map(functorList)(Data_Show.show(dictShow))(v)) + " : Nil)");
    });
};
var showNonEmptyList = function (dictShow) {
    return new Data_Show.Show(function (v) {
        return "(NonEmptyList " + (Data_Show.show(Data_NonEmpty.showNonEmpty(dictShow)(showList(dictShow)))(v) + ")");
    });
};
var traversableList = new Data_Traversable.Traversable(function () {
    return foldableList;
}, function () {
    return functorList;
}, function (dictApplicative) {
    return Data_Traversable.traverse(traversableList)(dictApplicative)(Control_Category.identity(Control_Category.categoryFn));
}, function (dictApplicative) {
    return function (f) {
        return function ($182) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Foldable.foldl(foldableList)(Data_Function.flip(Cons.create))(Nil.value))(Data_Foldable.foldl(foldableList)(function (acc) {
                return function ($183) {
                    return Control_Apply.lift2(dictApplicative.Apply0())(Data_Function.flip(Cons.create))(acc)(f($183));
                };
            })(Control_Applicative.pure(dictApplicative)(Nil.value))($182));
        };
    };
});
var traversableNonEmptyList = Data_NonEmpty.traversableNonEmpty(traversableList);
var traversableWithIndexList = new Data_TraversableWithIndex.TraversableWithIndex(function () {
    return foldableWithIndexList;
}, function () {
    return functorWithIndexList;
}, function () {
    return traversableList;
}, function (dictApplicative) {
    return function (f) {
        var rev = Data_Foldable.foldl(foldableList)(Data_Function.flip(Cons.create))(Nil.value);
        return function ($184) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(rev)(Data_FoldableWithIndex.foldlWithIndex(foldableWithIndexList)(function (i) {
                return function (acc) {
                    return function ($185) {
                        return Control_Apply.lift2(dictApplicative.Apply0())(Data_Function.flip(Cons.create))(acc)(f(i)($185));
                    };
                };
            })(Control_Applicative.pure(dictApplicative)(Nil.value))($184));
        };
    };
});
var traversableWithIndexNonEmptyList = new Data_TraversableWithIndex.TraversableWithIndex(function () {
    return foldableWithIndexNonEmptyList;
}, function () {
    return functorWithIndexNonEmptyList;
}, function () {
    return traversableNonEmptyList;
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(NonEmptyList)(Data_TraversableWithIndex.traverseWithIndex(Data_NonEmpty.traversableWithIndexNonEmpty(traversableWithIndexList))(dictApplicative)(function ($186) {
                return f(Data_Maybe.maybe(0)(Data_Semiring.add(Data_Semiring.semiringInt)(1))($186));
            })(v));
        };
    };
});
var unfoldable1List = new Data_Unfoldable1.Unfoldable1(function (f) {
    return function (b) {
        var go = function ($copy_source) {
            return function ($copy_memo) {
                var $tco_var_source = $copy_source;
                var $tco_done = false;
                var $tco_result;
                function $tco_loop(source, memo) {
                    var v = f(source);
                    if (v.value1 instanceof Data_Maybe.Just) {
                        $tco_var_source = v.value1.value0;
                        $copy_memo = new Cons(v.value0, memo);
                        return;
                    };
                    if (v.value1 instanceof Data_Maybe.Nothing) {
                        $tco_done = true;
                        return Data_Foldable.foldl(foldableList)(Data_Function.flip(Cons.create))(Nil.value)(new Cons(v.value0, memo));
                    };
                    throw new Error("Failed pattern match at Data.List.Types (line 105, column 22 - line 107, column 61): " + [ v.constructor.name ]);
                };
                while (!$tco_done) {
                    $tco_result = $tco_loop($tco_var_source, $copy_memo);
                };
                return $tco_result;
            };
        };
        return go(b)(Nil.value);
    };
});
var unfoldableList = new Data_Unfoldable.Unfoldable(function () {
    return unfoldable1List;
}, function (f) {
    return function (b) {
        var go = function ($copy_source) {
            return function ($copy_memo) {
                var $tco_var_source = $copy_source;
                var $tco_done = false;
                var $tco_result;
                function $tco_loop(source, memo) {
                    var v = f(source);
                    if (v instanceof Data_Maybe.Nothing) {
                        $tco_done = true;
                        return Data_Foldable.foldl(foldableList)(Data_Function.flip(Cons.create))(Nil.value)(memo);
                    };
                    if (v instanceof Data_Maybe.Just) {
                        $tco_var_source = v.value0.value1;
                        $copy_memo = new Cons(v.value0.value0, memo);
                        return;
                    };
                    throw new Error("Failed pattern match at Data.List.Types (line 112, column 22 - line 114, column 52): " + [ v.constructor.name ]);
                };
                while (!$tco_done) {
                    $tco_result = $tco_loop($tco_var_source, $copy_memo);
                };
                return $tco_result;
            };
        };
        return go(b)(Nil.value);
    };
});
var unfoldable1NonEmptyList = Data_NonEmpty.unfoldable1NonEmpty(unfoldableList);
var foldable1NonEmptyList = Data_NonEmpty.foldable1NonEmpty(foldableList);
var extendNonEmptyList = new Control_Extend.Extend(function () {
    return functorNonEmptyList;
}, function (f) {
    return function (v) {
        var go = function (a) {
            return function (v1) {
                return {
                    val: new Cons(f(new Data_NonEmpty.NonEmpty(a, v1.acc)), v1.val),
                    acc: new Cons(a, v1.acc)
                };
            };
        };
        return new Data_NonEmpty.NonEmpty(f(v), (Data_Foldable.foldr(foldableList)(go)({
            val: Nil.value,
            acc: Nil.value
        })(v.value1)).val);
    };
});
var extendList = new Control_Extend.Extend(function () {
    return functorList;
}, function (f) {
    return function (v) {
        if (v instanceof Nil) {
            return Nil.value;
        };
        if (v instanceof Cons) {
            var go = function (a$prime) {
                return function (v1) {
                    var acc$prime = new Cons(a$prime, v1.acc);
                    return {
                        val: new Cons(f(acc$prime), v1.val),
                        acc: acc$prime
                    };
                };
            };
            return new Cons(f(v), (Data_Foldable.foldr(foldableList)(go)({
                val: Nil.value,
                acc: Nil.value
            })(v.value1)).val);
        };
        throw new Error("Failed pattern match at Data.List.Types (line 152, column 1 - line 152, column 35): " + [ f.constructor.name, v.constructor.name ]);
    };
});
var eq1List = new Data_Eq.Eq1(function (dictEq) {
    return function (xs) {
        return function (ys) {
            var go = function ($copy_v) {
                return function ($copy_v1) {
                    return function ($copy_v2) {
                        var $tco_var_v = $copy_v;
                        var $tco_var_v1 = $copy_v1;
                        var $tco_done = false;
                        var $tco_result;
                        function $tco_loop(v, v1, v2) {
                            if (!v2) {
                                $tco_done = true;
                                return false;
                            };
                            if (v instanceof Nil && v1 instanceof Nil) {
                                $tco_done = true;
                                return v2;
                            };
                            if (v instanceof Cons && v1 instanceof Cons) {
                                $tco_var_v = v.value1;
                                $tco_var_v1 = v1.value1;
                                $copy_v2 = v2 && Data_Eq.eq(dictEq)(v1.value0)(v.value0);
                                return;
                            };
                            $tco_done = true;
                            return false;
                        };
                        while (!$tco_done) {
                            $tco_result = $tco_loop($tco_var_v, $tco_var_v1, $copy_v2);
                        };
                        return $tco_result;
                    };
                };
            };
            return go(xs)(ys)(true);
        };
    };
});
var eqList = function (dictEq) {
    return new Data_Eq.Eq(Data_Eq.eq1(eq1List)(dictEq));
};
var eqNonEmptyList = function (dictEq) {
    return Data_NonEmpty.eqNonEmpty(eq1List)(dictEq);
};
var ord1List = new Data_Ord.Ord1(function () {
    return eq1List;
}, function (dictOrd) {
    return function (xs) {
        return function (ys) {
            var go = function ($copy_v) {
                return function ($copy_v1) {
                    var $tco_var_v = $copy_v;
                    var $tco_done = false;
                    var $tco_result;
                    function $tco_loop(v, v1) {
                        if (v instanceof Nil && v1 instanceof Nil) {
                            $tco_done = true;
                            return Data_Ordering.EQ.value;
                        };
                        if (v instanceof Nil) {
                            $tco_done = true;
                            return Data_Ordering.LT.value;
                        };
                        if (v1 instanceof Nil) {
                            $tco_done = true;
                            return Data_Ordering.GT.value;
                        };
                        if (v instanceof Cons && v1 instanceof Cons) {
                            var v2 = Data_Ord.compare(dictOrd)(v.value0)(v1.value0);
                            if (v2 instanceof Data_Ordering.EQ) {
                                $tco_var_v = v.value1;
                                $copy_v1 = v1.value1;
                                return;
                            };
                            $tco_done = true;
                            return v2;
                        };
                        throw new Error("Failed pattern match at Data.List.Types (line 55, column 5 - line 55, column 20): " + [ v.constructor.name, v1.constructor.name ]);
                    };
                    while (!$tco_done) {
                        $tco_result = $tco_loop($tco_var_v, $copy_v1);
                    };
                    return $tco_result;
                };
            };
            return go(xs)(ys);
        };
    };
});
var ordList = function (dictOrd) {
    return new Data_Ord.Ord(function () {
        return eqList(dictOrd.Eq0());
    }, Data_Ord.compare1(ord1List)(dictOrd));
};
var ordNonEmptyList = function (dictOrd) {
    return Data_NonEmpty.ordNonEmpty(ord1List)(dictOrd);
};
var comonadNonEmptyList = new Control_Comonad.Comonad(function () {
    return extendNonEmptyList;
}, function (v) {
    return v.value0;
});
var applyList = new Control_Apply.Apply(function () {
    return functorList;
}, function (v) {
    return function (v1) {
        if (v instanceof Nil) {
            return Nil.value;
        };
        if (v instanceof Cons) {
            return Data_Semigroup.append(semigroupList)(Data_Functor.map(functorList)(v.value0)(v1))(Control_Apply.apply(applyList)(v.value1)(v1));
        };
        throw new Error("Failed pattern match at Data.List.Types (line 127, column 1 - line 127, column 33): " + [ v.constructor.name, v1.constructor.name ]);
    };
});
var applyNonEmptyList = new Control_Apply.Apply(function () {
    return functorNonEmptyList;
}, function (v) {
    return function (v1) {
        return new Data_NonEmpty.NonEmpty(v.value0(v1.value0), Data_Semigroup.append(semigroupList)(Control_Apply.apply(applyList)(v.value1)(new Cons(v1.value0, Nil.value)))(Control_Apply.apply(applyList)(new Cons(v.value0, v.value1))(v1.value1)));
    };
});
var bindList = new Control_Bind.Bind(function () {
    return applyList;
}, function (v) {
    return function (v1) {
        if (v instanceof Nil) {
            return Nil.value;
        };
        if (v instanceof Cons) {
            return Data_Semigroup.append(semigroupList)(v1(v.value0))(Control_Bind.bind(bindList)(v.value1)(v1));
        };
        throw new Error("Failed pattern match at Data.List.Types (line 134, column 1 - line 134, column 31): " + [ v.constructor.name, v1.constructor.name ]);
    };
});
var bindNonEmptyList = new Control_Bind.Bind(function () {
    return applyNonEmptyList;
}, function (v) {
    return function (f) {
        var v1 = f(v.value0);
        return new Data_NonEmpty.NonEmpty(v1.value0, Data_Semigroup.append(semigroupList)(v1.value1)(Control_Bind.bind(bindList)(v.value1)(function ($187) {
            return toList(f($187));
        })));
    };
});
var applicativeList = new Control_Applicative.Applicative(function () {
    return applyList;
}, function (a) {
    return new Cons(a, Nil.value);
});
var monadList = new Control_Monad.Monad(function () {
    return applicativeList;
}, function () {
    return bindList;
});
var altNonEmptyList = new Control_Alt.Alt(function () {
    return functorNonEmptyList;
}, Data_Semigroup.append(semigroupNonEmptyList));
var altList = new Control_Alt.Alt(function () {
    return functorList;
}, Data_Semigroup.append(semigroupList));
var plusList = new Control_Plus.Plus(function () {
    return altList;
}, Nil.value);
var alternativeList = new Control_Alternative.Alternative(function () {
    return applicativeList;
}, function () {
    return plusList;
});
var monadZeroList = new Control_MonadZero.MonadZero(function () {
    return alternativeList;
}, function () {
    return monadList;
});
var monadPlusList = new Control_MonadPlus.MonadPlus(function () {
    return monadZeroList;
});
var applicativeNonEmptyList = new Control_Applicative.Applicative(function () {
    return applyNonEmptyList;
}, function ($188) {
    return NonEmptyList(Data_NonEmpty.singleton(plusList)($188));
});
var monadNonEmptyList = new Control_Monad.Monad(function () {
    return applicativeNonEmptyList;
}, function () {
    return bindNonEmptyList;
});
var traversable1NonEmptyList = new Data_Semigroup_Traversable.Traversable1(function () {
    return foldable1NonEmptyList;
}, function () {
    return traversableNonEmptyList;
}, function (dictApply) {
    return Data_Semigroup_Traversable.traverse1(traversable1NonEmptyList)(dictApply)(Control_Category.identity(Control_Category.categoryFn));
}, function (dictApply) {
    return function (f) {
        return function (v) {
            return Data_Functor.mapFlipped(dictApply.Functor0())(Data_Foldable.foldl(foldableList)(function (acc) {
                return function ($189) {
                    return Control_Apply.lift2(dictApply)(Data_Function.flip(nelCons))(acc)(f($189));
                };
            })(Data_Functor.map(dictApply.Functor0())(Control_Applicative.pure(applicativeNonEmptyList))(f(v.value0)))(v.value1))(function (v1) {
                return Data_Foldable.foldl(foldableList)(Data_Function.flip(nelCons))(Control_Applicative.pure(applicativeNonEmptyList)(v1.value0))(v1.value1);
            });
        };
    };
});
module.exports = {
    Nil: Nil,
    Cons: Cons,
    NonEmptyList: NonEmptyList,
    toList: toList,
    nelCons: nelCons,
    showList: showList,
    eqList: eqList,
    eq1List: eq1List,
    ordList: ordList,
    ord1List: ord1List,
    semigroupList: semigroupList,
    monoidList: monoidList,
    functorList: functorList,
    functorWithIndexList: functorWithIndexList,
    foldableList: foldableList,
    foldableWithIndexList: foldableWithIndexList,
    unfoldable1List: unfoldable1List,
    unfoldableList: unfoldableList,
    traversableList: traversableList,
    traversableWithIndexList: traversableWithIndexList,
    applyList: applyList,
    applicativeList: applicativeList,
    bindList: bindList,
    monadList: monadList,
    altList: altList,
    plusList: plusList,
    alternativeList: alternativeList,
    monadZeroList: monadZeroList,
    monadPlusList: monadPlusList,
    extendList: extendList,
    newtypeNonEmptyList: newtypeNonEmptyList,
    eqNonEmptyList: eqNonEmptyList,
    ordNonEmptyList: ordNonEmptyList,
    showNonEmptyList: showNonEmptyList,
    functorNonEmptyList: functorNonEmptyList,
    applyNonEmptyList: applyNonEmptyList,
    applicativeNonEmptyList: applicativeNonEmptyList,
    bindNonEmptyList: bindNonEmptyList,
    monadNonEmptyList: monadNonEmptyList,
    altNonEmptyList: altNonEmptyList,
    extendNonEmptyList: extendNonEmptyList,
    comonadNonEmptyList: comonadNonEmptyList,
    semigroupNonEmptyList: semigroupNonEmptyList,
    foldableNonEmptyList: foldableNonEmptyList,
    traversableNonEmptyList: traversableNonEmptyList,
    foldable1NonEmptyList: foldable1NonEmptyList,
    unfoldable1NonEmptyList: unfoldable1NonEmptyList,
    functorWithIndexNonEmptyList: functorWithIndexNonEmptyList,
    foldableWithIndexNonEmptyList: foldableWithIndexNonEmptyList,
    traversableWithIndexNonEmptyList: traversableWithIndexNonEmptyList,
    traversable1NonEmptyList: traversable1NonEmptyList
};

},{"../Control.Alt/index.js":25,"../Control.Alternative/index.js":26,"../Control.Applicative/index.js":27,"../Control.Apply/index.js":29,"../Control.Bind/index.js":33,"../Control.Category/index.js":34,"../Control.Comonad/index.js":35,"../Control.Extend/index.js":37,"../Control.Monad/index.js":52,"../Control.MonadPlus/index.js":53,"../Control.MonadZero/index.js":54,"../Control.Plus/index.js":55,"../Control.Semigroupoid/index.js":56,"../Data.Eq/index.js":82,"../Data.Foldable/index.js":87,"../Data.FoldableWithIndex/index.js":88,"../Data.Function/index.js":91,"../Data.Functor/index.js":95,"../Data.FunctorWithIndex/index.js":97,"../Data.HeytingAlgebra/index.js":99,"../Data.Maybe/index.js":110,"../Data.Monoid/index.js":117,"../Data.Newtype/index.js":119,"../Data.NonEmpty/index.js":120,"../Data.Ord/index.js":130,"../Data.Ordering/index.js":131,"../Data.Ring/index.js":133,"../Data.Semigroup.Foldable/index.js":135,"../Data.Semigroup.Traversable/index.js":137,"../Data.Semigroup/index.js":139,"../Data.Semiring/index.js":141,"../Data.Show/index.js":143,"../Data.Traversable/index.js":154,"../Data.TraversableWithIndex/index.js":155,"../Data.Tuple/index.js":156,"../Data.Unfoldable/index.js":158,"../Data.Unfoldable1/index.js":160,"../Prelude/index.js":198}],107:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Alt = require("../Control.Alt/index.js");
var Control_Alternative = require("../Control.Alternative/index.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Category = require("../Control.Category/index.js");
var Control_Lazy = require("../Control.Lazy/index.js");
var Control_Monad_Rec_Class = require("../Control.Monad.Rec.Class/index.js");
var Control_Semigroupoid = require("../Control.Semigroupoid/index.js");
var Data_Bifunctor = require("../Data.Bifunctor/index.js");
var Data_Boolean = require("../Data.Boolean/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_FunctorWithIndex = require("../Data.FunctorWithIndex/index.js");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra/index.js");
var Data_List_Types = require("../Data.List.Types/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_NonEmpty = require("../Data.NonEmpty/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Ordering = require("../Data.Ordering/index.js");
var Data_Ring = require("../Data.Ring/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Semiring = require("../Data.Semiring/index.js");
var Data_Show = require("../Data.Show/index.js");
var Data_Traversable = require("../Data.Traversable/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var Data_Unfoldable = require("../Data.Unfoldable/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Prelude = require("../Prelude/index.js");
var Pattern = function (x) {
    return x;
};
var updateAt = function (v) {
    return function (v1) {
        return function (v2) {
            if (v === 0 && v2 instanceof Data_List_Types.Cons) {
                return new Data_Maybe.Just(new Data_List_Types.Cons(v1, v2.value1));
            };
            if (v2 instanceof Data_List_Types.Cons) {
                return Data_Functor.map(Data_Maybe.functorMaybe)(function (v3) {
                    return new Data_List_Types.Cons(v2.value0, v3);
                })(updateAt(v - 1 | 0)(v1)(v2.value1));
            };
            return Data_Maybe.Nothing.value;
        };
    };
};
var unzip = Data_Foldable.foldr(Data_List_Types.foldableList)(function (v) {
    return function (v1) {
        return new Data_Tuple.Tuple(new Data_List_Types.Cons(v.value0, v1.value0), new Data_List_Types.Cons(v.value1, v1.value1));
    };
})(new Data_Tuple.Tuple(Data_List_Types.Nil.value, Data_List_Types.Nil.value));
var uncons = function (v) {
    if (v instanceof Data_List_Types.Nil) {
        return Data_Maybe.Nothing.value;
    };
    if (v instanceof Data_List_Types.Cons) {
        return new Data_Maybe.Just({
            head: v.value0,
            tail: v.value1
        });
    };
    throw new Error("Failed pattern match at Data.List (line 259, column 1 - line 259, column 66): " + [ v.constructor.name ]);
};
var toUnfoldable = function (dictUnfoldable) {
    return Data_Unfoldable.unfoldr(dictUnfoldable)(function (xs) {
        return Data_Functor.map(Data_Maybe.functorMaybe)(function (rec) {
            return new Data_Tuple.Tuple(rec.head, rec.tail);
        })(uncons(xs));
    });
};
var tail = function (v) {
    if (v instanceof Data_List_Types.Nil) {
        return Data_Maybe.Nothing.value;
    };
    if (v instanceof Data_List_Types.Cons) {
        return new Data_Maybe.Just(v.value1);
    };
    throw new Error("Failed pattern match at Data.List (line 245, column 1 - line 245, column 43): " + [ v.constructor.name ]);
};
var stripPrefix = function (dictEq) {
    return function (v) {
        return function (s) {
            var go = function (prefix) {
                return function (input) {
                    if (prefix instanceof Data_List_Types.Cons && (input instanceof Data_List_Types.Cons && Data_Eq.eq(dictEq)(prefix.value0)(input.value0))) {
                        return Data_Maybe.Just.create(new Control_Monad_Rec_Class.Loop({
                            a: prefix.value1,
                            b: input.value1
                        }));
                    };
                    if (prefix instanceof Data_List_Types.Nil) {
                        return Data_Maybe.Just.create(new Control_Monad_Rec_Class.Done(input));
                    };
                    return Data_Maybe.Nothing.value;
                };
            };
            return Control_Monad_Rec_Class.tailRecM2(Control_Monad_Rec_Class.monadRecMaybe)(go)(v)(s);
        };
    };
};
var span = function (v) {
    return function (v1) {
        if (v1 instanceof Data_List_Types.Cons && v(v1.value0)) {
            var v2 = span(v)(v1.value1);
            return {
                init: new Data_List_Types.Cons(v1.value0, v2.init),
                rest: v2.rest
            };
        };
        return {
            init: Data_List_Types.Nil.value,
            rest: v1
        };
    };
};
var snoc = function (xs) {
    return function (x) {
        return Data_Foldable.foldr(Data_List_Types.foldableList)(Data_List_Types.Cons.create)(new Data_List_Types.Cons(x, Data_List_Types.Nil.value))(xs);
    };
};
var singleton = function (a) {
    return new Data_List_Types.Cons(a, Data_List_Types.Nil.value);
};
var sortBy = function (cmp) {
    var merge = function (v) {
        return function (v1) {
            if (v instanceof Data_List_Types.Cons && v1 instanceof Data_List_Types.Cons) {
                if (Data_Eq.eq(Data_Ordering.eqOrdering)(cmp(v.value0)(v1.value0))(Data_Ordering.GT.value)) {
                    return new Data_List_Types.Cons(v1.value0, merge(v)(v1.value1));
                };
                if (Data_Boolean.otherwise) {
                    return new Data_List_Types.Cons(v.value0, merge(v.value1)(v1));
                };
            };
            if (v instanceof Data_List_Types.Nil) {
                return v1;
            };
            if (v1 instanceof Data_List_Types.Nil) {
                return v;
            };
            throw new Error("Failed pattern match at Data.List (line 473, column 3 - line 473, column 38): " + [ v.constructor.name, v1.constructor.name ]);
        };
    };
    var mergePairs = function (v) {
        if (v instanceof Data_List_Types.Cons && v.value1 instanceof Data_List_Types.Cons) {
            return new Data_List_Types.Cons(merge(v.value0)(v.value1.value0), mergePairs(v.value1.value1));
        };
        return v;
    };
    var mergeAll = function ($copy_v) {
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v) {
            if (v instanceof Data_List_Types.Cons && v.value1 instanceof Data_List_Types.Nil) {
                $tco_done = true;
                return v.value0;
            };
            $copy_v = mergePairs(v);
            return;
        };
        while (!$tco_done) {
            $tco_result = $tco_loop($copy_v);
        };
        return $tco_result;
    };
    var sequences = function (v) {
        if (v instanceof Data_List_Types.Cons && v.value1 instanceof Data_List_Types.Cons) {
            if (Data_Eq.eq(Data_Ordering.eqOrdering)(cmp(v.value0)(v.value1.value0))(Data_Ordering.GT.value)) {
                return descending(v.value1.value0)(singleton(v.value0))(v.value1.value1);
            };
            if (Data_Boolean.otherwise) {
                return ascending(v.value1.value0)(function (v1) {
                    return new Data_List_Types.Cons(v.value0, v1);
                })(v.value1.value1);
            };
        };
        return singleton(v);
    };
    var descending = function ($copy_a) {
        return function ($copy_as) {
            return function ($copy_v) {
                var $tco_var_a = $copy_a;
                var $tco_var_as = $copy_as;
                var $tco_done = false;
                var $tco_result;
                function $tco_loop(a, as, v) {
                    if (v instanceof Data_List_Types.Cons && Data_Eq.eq(Data_Ordering.eqOrdering)(cmp(a)(v.value0))(Data_Ordering.GT.value)) {
                        $tco_var_a = v.value0;
                        $tco_var_as = new Data_List_Types.Cons(a, as);
                        $copy_v = v.value1;
                        return;
                    };
                    $tco_done = true;
                    return new Data_List_Types.Cons(new Data_List_Types.Cons(a, as), sequences(v));
                };
                while (!$tco_done) {
                    $tco_result = $tco_loop($tco_var_a, $tco_var_as, $copy_v);
                };
                return $tco_result;
            };
        };
    };
    var ascending = function ($copy_a) {
        return function ($copy_as) {
            return function ($copy_v) {
                var $tco_var_a = $copy_a;
                var $tco_var_as = $copy_as;
                var $tco_done = false;
                var $tco_result;
                function $tco_loop(a, as, v) {
                    if (v instanceof Data_List_Types.Cons && Data_Eq.notEq(Data_Ordering.eqOrdering)(cmp(a)(v.value0))(Data_Ordering.GT.value)) {
                        $tco_var_a = v.value0;
                        $tco_var_as = function (ys) {
                            return as(new Data_List_Types.Cons(a, ys));
                        };
                        $copy_v = v.value1;
                        return;
                    };
                    $tco_done = true;
                    return new Data_List_Types.Cons(as(singleton(a)), sequences(v));
                };
                while (!$tco_done) {
                    $tco_result = $tco_loop($tco_var_a, $tco_var_as, $copy_v);
                };
                return $tco_result;
            };
        };
    };
    return function ($337) {
        return mergeAll(sequences($337));
    };
};
var sort = function (dictOrd) {
    return function (xs) {
        return sortBy(Data_Ord.compare(dictOrd))(xs);
    };
};
var tails = function (v) {
    if (v instanceof Data_List_Types.Nil) {
        return singleton(Data_List_Types.Nil.value);
    };
    if (v instanceof Data_List_Types.Cons) {
        return new Data_List_Types.Cons(v, tails(v.value1));
    };
    throw new Error("Failed pattern match at Data.List (line 626, column 1 - line 626, column 43): " + [ v.constructor.name ]);
};
var showPattern = function (dictShow) {
    return new Data_Show.Show(function (v) {
        return "(Pattern " + (Data_Show.show(Data_List_Types.showList(dictShow))(v) + ")");
    });
};
var reverse = (function () {
    var go = function ($copy_acc) {
        return function ($copy_v) {
            var $tco_var_acc = $copy_acc;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(acc, v) {
                if (v instanceof Data_List_Types.Nil) {
                    $tco_done = true;
                    return acc;
                };
                if (v instanceof Data_List_Types.Cons) {
                    $tco_var_acc = new Data_List_Types.Cons(v.value0, acc);
                    $copy_v = v.value1;
                    return;
                };
                throw new Error("Failed pattern match at Data.List (line 368, column 3 - line 368, column 19): " + [ acc.constructor.name, v.constructor.name ]);
            };
            while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_acc, $copy_v);
            };
            return $tco_result;
        };
    };
    return go(Data_List_Types.Nil.value);
})();
var take = (function () {
    var go = function ($copy_acc) {
        return function ($copy_v) {
            return function ($copy_v1) {
                var $tco_var_acc = $copy_acc;
                var $tco_var_v = $copy_v;
                var $tco_done = false;
                var $tco_result;
                function $tco_loop(acc, v, v1) {
                    if (v < 1) {
                        $tco_done = true;
                        return reverse(acc);
                    };
                    if (v1 instanceof Data_List_Types.Nil) {
                        $tco_done = true;
                        return reverse(acc);
                    };
                    if (v1 instanceof Data_List_Types.Cons) {
                        $tco_var_acc = new Data_List_Types.Cons(v1.value0, acc);
                        $tco_var_v = v - 1 | 0;
                        $copy_v1 = v1.value1;
                        return;
                    };
                    throw new Error("Failed pattern match at Data.List (line 520, column 3 - line 520, column 35): " + [ acc.constructor.name, v.constructor.name, v1.constructor.name ]);
                };
                while (!$tco_done) {
                    $tco_result = $tco_loop($tco_var_acc, $tco_var_v, $copy_v1);
                };
                return $tco_result;
            };
        };
    };
    return go(Data_List_Types.Nil.value);
})();
var takeWhile = function (p) {
    var go = function ($copy_acc) {
        return function ($copy_v) {
            var $tco_var_acc = $copy_acc;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(acc, v) {
                if (v instanceof Data_List_Types.Cons && p(v.value0)) {
                    $tco_var_acc = new Data_List_Types.Cons(v.value0, acc);
                    $copy_v = v.value1;
                    return;
                };
                $tco_done = true;
                return reverse(acc);
            };
            while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_acc, $copy_v);
            };
            return $tco_result;
        };
    };
    return go(Data_List_Types.Nil.value);
};
var unsnoc = function (lst) {
    var go = function ($copy_v) {
        return function ($copy_acc) {
            var $tco_var_v = $copy_v;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(v, acc) {
                if (v instanceof Data_List_Types.Nil) {
                    $tco_done = true;
                    return Data_Maybe.Nothing.value;
                };
                if (v instanceof Data_List_Types.Cons && v.value1 instanceof Data_List_Types.Nil) {
                    $tco_done = true;
                    return new Data_Maybe.Just({
                        revInit: acc,
                        last: v.value0
                    });
                };
                if (v instanceof Data_List_Types.Cons) {
                    $tco_var_v = v.value1;
                    $copy_acc = new Data_List_Types.Cons(v.value0, acc);
                    return;
                };
                throw new Error("Failed pattern match at Data.List (line 270, column 3 - line 270, column 23): " + [ v.constructor.name, acc.constructor.name ]);
            };
            while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_v, $copy_acc);
            };
            return $tco_result;
        };
    };
    return Data_Functor.map(Data_Maybe.functorMaybe)(function (h) {
        return {
            init: reverse(h.revInit),
            last: h.last
        };
    })(go(lst)(Data_List_Types.Nil.value));
};
var zipWith = function (f) {
    return function (xs) {
        return function (ys) {
            var go = function ($copy_v) {
                return function ($copy_v1) {
                    return function ($copy_acc) {
                        var $tco_var_v = $copy_v;
                        var $tco_var_v1 = $copy_v1;
                        var $tco_done = false;
                        var $tco_result;
                        function $tco_loop(v, v1, acc) {
                            if (v instanceof Data_List_Types.Nil) {
                                $tco_done = true;
                                return acc;
                            };
                            if (v1 instanceof Data_List_Types.Nil) {
                                $tco_done = true;
                                return acc;
                            };
                            if (v instanceof Data_List_Types.Cons && v1 instanceof Data_List_Types.Cons) {
                                $tco_var_v = v.value1;
                                $tco_var_v1 = v1.value1;
                                $copy_acc = new Data_List_Types.Cons(f(v.value0)(v1.value0), acc);
                                return;
                            };
                            throw new Error("Failed pattern match at Data.List (line 718, column 3 - line 718, column 21): " + [ v.constructor.name, v1.constructor.name, acc.constructor.name ]);
                        };
                        while (!$tco_done) {
                            $tco_result = $tco_loop($tco_var_v, $tco_var_v1, $copy_acc);
                        };
                        return $tco_result;
                    };
                };
            };
            return reverse(go(xs)(ys)(Data_List_Types.Nil.value));
        };
    };
};
var zip = zipWith(Data_Tuple.Tuple.create);
var zipWithA = function (dictApplicative) {
    return function (f) {
        return function (xs) {
            return function (ys) {
                return Data_Traversable.sequence(Data_List_Types.traversableList)(dictApplicative)(zipWith(f)(xs)(ys));
            };
        };
    };
};
var range = function (start) {
    return function (end) {
        if (start === end) {
            return singleton(start);
        };
        if (Data_Boolean.otherwise) {
            var go = function ($copy_s) {
                return function ($copy_e) {
                    return function ($copy_step) {
                        return function ($copy_rest) {
                            var $tco_var_s = $copy_s;
                            var $tco_var_e = $copy_e;
                            var $tco_var_step = $copy_step;
                            var $tco_done = false;
                            var $tco_result;
                            function $tco_loop(s, e, step, rest) {
                                if (s === e) {
                                    $tco_done = true;
                                    return new Data_List_Types.Cons(s, rest);
                                };
                                if (Data_Boolean.otherwise) {
                                    $tco_var_s = s + step | 0;
                                    $tco_var_e = e;
                                    $tco_var_step = step;
                                    $copy_rest = new Data_List_Types.Cons(s, rest);
                                    return;
                                };
                                throw new Error("Failed pattern match at Data.List (line 148, column 3 - line 149, column 65): " + [ s.constructor.name, e.constructor.name, step.constructor.name, rest.constructor.name ]);
                            };
                            while (!$tco_done) {
                                $tco_result = $tco_loop($tco_var_s, $tco_var_e, $tco_var_step, $copy_rest);
                            };
                            return $tco_result;
                        };
                    };
                };
            };
            return go(end)(start)((function () {
                var $223 = start > end;
                if ($223) {
                    return 1;
                };
                return -1 | 0;
            })())(Data_List_Types.Nil.value);
        };
        throw new Error("Failed pattern match at Data.List (line 144, column 1 - line 144, column 32): " + [ start.constructor.name, end.constructor.name ]);
    };
};
var partition = function (p) {
    return function (xs) {
        var select = function (x) {
            return function (v) {
                var $226 = p(x);
                if ($226) {
                    return {
                        no: v.no,
                        yes: new Data_List_Types.Cons(x, v.yes)
                    };
                };
                return {
                    no: new Data_List_Types.Cons(x, v.no),
                    yes: v.yes
                };
            };
        };
        return Data_Foldable.foldr(Data_List_Types.foldableList)(select)({
            no: Data_List_Types.Nil.value,
            yes: Data_List_Types.Nil.value
        })(xs);
    };
};
var $$null = function (v) {
    if (v instanceof Data_List_Types.Nil) {
        return true;
    };
    return false;
};
var newtypePattern = new Data_Newtype.Newtype(function (n) {
    return n;
}, Pattern);
var mapWithIndex = Data_FunctorWithIndex.mapWithIndex(Data_List_Types.functorWithIndexList);
var mapMaybe = function (f) {
    var go = function ($copy_acc) {
        return function ($copy_v) {
            var $tco_var_acc = $copy_acc;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(acc, v) {
                if (v instanceof Data_List_Types.Nil) {
                    $tco_done = true;
                    return reverse(acc);
                };
                if (v instanceof Data_List_Types.Cons) {
                    var v1 = f(v.value0);
                    if (v1 instanceof Data_Maybe.Nothing) {
                        $tco_var_acc = acc;
                        $copy_v = v.value1;
                        return;
                    };
                    if (v1 instanceof Data_Maybe.Just) {
                        $tco_var_acc = new Data_List_Types.Cons(v1.value0, acc);
                        $copy_v = v.value1;
                        return;
                    };
                    throw new Error("Failed pattern match at Data.List (line 419, column 5 - line 421, column 32): " + [ v1.constructor.name ]);
                };
                throw new Error("Failed pattern match at Data.List (line 417, column 3 - line 417, column 27): " + [ acc.constructor.name, v.constructor.name ]);
            };
            while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_acc, $copy_v);
            };
            return $tco_result;
        };
    };
    return go(Data_List_Types.Nil.value);
};
var manyRec = function (dictMonadRec) {
    return function (dictAlternative) {
        return function (p) {
            var go = function (acc) {
                return Control_Bind.bind((dictMonadRec.Monad0()).Bind1())(Control_Alt.alt((dictAlternative.Plus1()).Alt0())(Data_Functor.map(((dictAlternative.Plus1()).Alt0()).Functor0())(Control_Monad_Rec_Class.Loop.create)(p))(Control_Applicative.pure(dictAlternative.Applicative0())(new Control_Monad_Rec_Class.Done(Data_Unit.unit))))(function (v) {
                    return Control_Applicative.pure(dictAlternative.Applicative0())(Data_Bifunctor.bimap(Control_Monad_Rec_Class.bifunctorStep)(function (v1) {
                        return new Data_List_Types.Cons(v1, acc);
                    })(function (v1) {
                        return reverse(acc);
                    })(v));
                });
            };
            return Control_Monad_Rec_Class.tailRecM(dictMonadRec)(go)(Data_List_Types.Nil.value);
        };
    };
};
var someRec = function (dictMonadRec) {
    return function (dictAlternative) {
        return function (v) {
            return Control_Apply.apply((dictAlternative.Applicative0()).Apply0())(Data_Functor.map(((dictAlternative.Plus1()).Alt0()).Functor0())(Data_List_Types.Cons.create)(v))(manyRec(dictMonadRec)(dictAlternative)(v));
        };
    };
};
var some = function (dictAlternative) {
    return function (dictLazy) {
        return function (v) {
            return Control_Apply.apply((dictAlternative.Applicative0()).Apply0())(Data_Functor.map(((dictAlternative.Plus1()).Alt0()).Functor0())(Data_List_Types.Cons.create)(v))(Control_Lazy.defer(dictLazy)(function (v1) {
                return many(dictAlternative)(dictLazy)(v);
            }));
        };
    };
};
var many = function (dictAlternative) {
    return function (dictLazy) {
        return function (v) {
            return Control_Alt.alt((dictAlternative.Plus1()).Alt0())(some(dictAlternative)(dictLazy)(v))(Control_Applicative.pure(dictAlternative.Applicative0())(Data_List_Types.Nil.value));
        };
    };
};
var length = Data_Foldable.foldl(Data_List_Types.foldableList)(function (acc) {
    return function (v) {
        return acc + 1 | 0;
    };
})(0);
var last = function ($copy_v) {
    var $tco_done = false;
    var $tco_result;
    function $tco_loop(v) {
        if (v instanceof Data_List_Types.Cons && v.value1 instanceof Data_List_Types.Nil) {
            $tco_done = true;
            return new Data_Maybe.Just(v.value0);
        };
        if (v instanceof Data_List_Types.Cons) {
            $copy_v = v.value1;
            return;
        };
        $tco_done = true;
        return Data_Maybe.Nothing.value;
    };
    while (!$tco_done) {
        $tco_result = $tco_loop($copy_v);
    };
    return $tco_result;
};
var insertBy = function (v) {
    return function (x) {
        return function (v1) {
            if (v1 instanceof Data_List_Types.Nil) {
                return singleton(x);
            };
            if (v1 instanceof Data_List_Types.Cons) {
                var v2 = v(x)(v1.value0);
                if (v2 instanceof Data_Ordering.GT) {
                    return new Data_List_Types.Cons(v1.value0, insertBy(v)(x)(v1.value1));
                };
                return new Data_List_Types.Cons(x, v1);
            };
            throw new Error("Failed pattern match at Data.List (line 216, column 1 - line 216, column 68): " + [ v.constructor.name, x.constructor.name, v1.constructor.name ]);
        };
    };
};
var insertAt = function (v) {
    return function (v1) {
        return function (v2) {
            if (v === 0) {
                return new Data_Maybe.Just(new Data_List_Types.Cons(v1, v2));
            };
            if (v2 instanceof Data_List_Types.Cons) {
                return Data_Functor.map(Data_Maybe.functorMaybe)(function (v3) {
                    return new Data_List_Types.Cons(v2.value0, v3);
                })(insertAt(v - 1 | 0)(v1)(v2.value1));
            };
            return Data_Maybe.Nothing.value;
        };
    };
};
var insert = function (dictOrd) {
    return insertBy(Data_Ord.compare(dictOrd));
};
var init = function (lst) {
    return Data_Functor.map(Data_Maybe.functorMaybe)(function (v) {
        return v.init;
    })(unsnoc(lst));
};
var index = function ($copy_v) {
    return function ($copy_v1) {
        var $tco_var_v = $copy_v;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v, v1) {
            if (v instanceof Data_List_Types.Nil) {
                $tco_done = true;
                return Data_Maybe.Nothing.value;
            };
            if (v instanceof Data_List_Types.Cons && v1 === 0) {
                $tco_done = true;
                return new Data_Maybe.Just(v.value0);
            };
            if (v instanceof Data_List_Types.Cons) {
                $tco_var_v = v.value1;
                $copy_v1 = v1 - 1 | 0;
                return;
            };
            throw new Error("Failed pattern match at Data.List (line 281, column 1 - line 281, column 44): " + [ v.constructor.name, v1.constructor.name ]);
        };
        while (!$tco_done) {
            $tco_result = $tco_loop($tco_var_v, $copy_v1);
        };
        return $tco_result;
    };
};
var head = function (v) {
    if (v instanceof Data_List_Types.Nil) {
        return Data_Maybe.Nothing.value;
    };
    if (v instanceof Data_List_Types.Cons) {
        return new Data_Maybe.Just(v.value0);
    };
    throw new Error("Failed pattern match at Data.List (line 230, column 1 - line 230, column 22): " + [ v.constructor.name ]);
};
var transpose = function (v) {
    if (v instanceof Data_List_Types.Nil) {
        return Data_List_Types.Nil.value;
    };
    if (v instanceof Data_List_Types.Cons && v.value0 instanceof Data_List_Types.Nil) {
        return transpose(v.value1);
    };
    if (v instanceof Data_List_Types.Cons && v.value0 instanceof Data_List_Types.Cons) {
        return new Data_List_Types.Cons(new Data_List_Types.Cons(v.value0.value0, mapMaybe(head)(v.value1)), transpose(new Data_List_Types.Cons(v.value0.value1, mapMaybe(tail)(v.value1))));
    };
    throw new Error("Failed pattern match at Data.List (line 752, column 1 - line 752, column 54): " + [ v.constructor.name ]);
};
var groupBy = function (v) {
    return function (v1) {
        if (v1 instanceof Data_List_Types.Nil) {
            return Data_List_Types.Nil.value;
        };
        if (v1 instanceof Data_List_Types.Cons) {
            var v2 = span(v(v1.value0))(v1.value1);
            return new Data_List_Types.Cons(new Data_NonEmpty.NonEmpty(v1.value0, v2.init), groupBy(v)(v2.rest));
        };
        throw new Error("Failed pattern match at Data.List (line 605, column 1 - line 605, column 80): " + [ v.constructor.name, v1.constructor.name ]);
    };
};
var group = function (dictEq) {
    return groupBy(Data_Eq.eq(dictEq));
};
var group$prime = function (dictOrd) {
    return function ($338) {
        return group(dictOrd.Eq0())(sort(dictOrd)($338));
    };
};
var fromFoldable = function (dictFoldable) {
    return Data_Foldable.foldr(dictFoldable)(Data_List_Types.Cons.create)(Data_List_Types.Nil.value);
};
var foldM = function (dictMonad) {
    return function (v) {
        return function (a) {
            return function (v1) {
                if (v1 instanceof Data_List_Types.Nil) {
                    return Control_Applicative.pure(dictMonad.Applicative0())(a);
                };
                if (v1 instanceof Data_List_Types.Cons) {
                    return Control_Bind.bind(dictMonad.Bind1())(v(a)(v1.value0))(function (a$prime) {
                        return foldM(dictMonad)(v)(a$prime)(v1.value1);
                    });
                };
                throw new Error("Failed pattern match at Data.List (line 763, column 1 - line 763, column 72): " + [ v.constructor.name, a.constructor.name, v1.constructor.name ]);
            };
        };
    };
};
var findIndex = function (fn) {
    var go = function ($copy_v) {
        return function ($copy_v1) {
            var $tco_var_v = $copy_v;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(v, v1) {
                if (v1 instanceof Data_List_Types.Cons) {
                    if (fn(v1.value0)) {
                        $tco_done = true;
                        return new Data_Maybe.Just(v);
                    };
                    if (Data_Boolean.otherwise) {
                        $tco_var_v = v + 1 | 0;
                        $copy_v1 = v1.value1;
                        return;
                    };
                };
                if (v1 instanceof Data_List_Types.Nil) {
                    $tco_done = true;
                    return Data_Maybe.Nothing.value;
                };
                throw new Error("Failed pattern match at Data.List (line 301, column 3 - line 301, column 35): " + [ v.constructor.name, v1.constructor.name ]);
            };
            while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_v, $copy_v1);
            };
            return $tco_result;
        };
    };
    return go(0);
};
var findLastIndex = function (fn) {
    return function (xs) {
        return Data_Functor.map(Data_Maybe.functorMaybe)(function (v) {
            return (length(xs) - 1 | 0) - v | 0;
        })(findIndex(fn)(reverse(xs)));
    };
};
var filterM = function (dictMonad) {
    return function (v) {
        return function (v1) {
            if (v1 instanceof Data_List_Types.Nil) {
                return Control_Applicative.pure(dictMonad.Applicative0())(Data_List_Types.Nil.value);
            };
            if (v1 instanceof Data_List_Types.Cons) {
                return Control_Bind.bind(dictMonad.Bind1())(v(v1.value0))(function (v2) {
                    return Control_Bind.bind(dictMonad.Bind1())(filterM(dictMonad)(v)(v1.value1))(function (v3) {
                        return Control_Applicative.pure(dictMonad.Applicative0())((function () {
                            if (v2) {
                                return new Data_List_Types.Cons(v1.value0, v3);
                            };
                            return v3;
                        })());
                    });
                });
            };
            throw new Error("Failed pattern match at Data.List (line 403, column 1 - line 403, column 75): " + [ v.constructor.name, v1.constructor.name ]);
        };
    };
};
var filter = function (p) {
    var go = function ($copy_acc) {
        return function ($copy_v) {
            var $tco_var_acc = $copy_acc;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(acc, v) {
                if (v instanceof Data_List_Types.Nil) {
                    $tco_done = true;
                    return reverse(acc);
                };
                if (v instanceof Data_List_Types.Cons) {
                    if (p(v.value0)) {
                        $tco_var_acc = new Data_List_Types.Cons(v.value0, acc);
                        $copy_v = v.value1;
                        return;
                    };
                    if (Data_Boolean.otherwise) {
                        $tco_var_acc = acc;
                        $copy_v = v.value1;
                        return;
                    };
                };
                throw new Error("Failed pattern match at Data.List (line 390, column 3 - line 390, column 27): " + [ acc.constructor.name, v.constructor.name ]);
            };
            while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_acc, $copy_v);
            };
            return $tco_result;
        };
    };
    return go(Data_List_Types.Nil.value);
};
var intersectBy = function (v) {
    return function (v1) {
        return function (v2) {
            if (v1 instanceof Data_List_Types.Nil) {
                return Data_List_Types.Nil.value;
            };
            if (v2 instanceof Data_List_Types.Nil) {
                return Data_List_Types.Nil.value;
            };
            return filter(function (x) {
                return Data_Foldable.any(Data_List_Types.foldableList)(Data_HeytingAlgebra.heytingAlgebraBoolean)(v(x))(v2);
            })(v1);
        };
    };
};
var intersect = function (dictEq) {
    return intersectBy(Data_Eq.eq(dictEq));
};
var nubBy = function (v) {
    return function (v1) {
        if (v1 instanceof Data_List_Types.Nil) {
            return Data_List_Types.Nil.value;
        };
        if (v1 instanceof Data_List_Types.Cons) {
            return new Data_List_Types.Cons(v1.value0, nubBy(v)(filter(function (y) {
                return !v(v1.value0)(y);
            })(v1.value1)));
        };
        throw new Error("Failed pattern match at Data.List (line 644, column 1 - line 644, column 59): " + [ v.constructor.name, v1.constructor.name ]);
    };
};
var nub = function (dictEq) {
    return nubBy(Data_Eq.eq(dictEq));
};
var eqPattern = function (dictEq) {
    return new Data_Eq.Eq(function (x) {
        return function (y) {
            return Data_Eq.eq(Data_List_Types.eqList(dictEq))(x)(y);
        };
    });
};
var ordPattern = function (dictOrd) {
    return new Data_Ord.Ord(function () {
        return eqPattern(dictOrd.Eq0());
    }, function (x) {
        return function (y) {
            return Data_Ord.compare(Data_List_Types.ordList(dictOrd))(x)(y);
        };
    });
};
var elemLastIndex = function (dictEq) {
    return function (x) {
        return findLastIndex(function (v) {
            return Data_Eq.eq(dictEq)(v)(x);
        });
    };
};
var elemIndex = function (dictEq) {
    return function (x) {
        return findIndex(function (v) {
            return Data_Eq.eq(dictEq)(v)(x);
        });
    };
};
var dropWhile = function (p) {
    var go = function ($copy_v) {
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v) {
            if (v instanceof Data_List_Types.Cons && p(v.value0)) {
                $copy_v = v.value1;
                return;
            };
            $tco_done = true;
            return v;
        };
        while (!$tco_done) {
            $tco_result = $tco_loop($copy_v);
        };
        return $tco_result;
    };
    return go;
};
var dropEnd = function (n) {
    return function (xs) {
        return take(length(xs) - n | 0)(xs);
    };
};
var drop = function ($copy_v) {
    return function ($copy_v1) {
        var $tco_var_v = $copy_v;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v, v1) {
            if (v < 1) {
                $tco_done = true;
                return v1;
            };
            if (v1 instanceof Data_List_Types.Nil) {
                $tco_done = true;
                return Data_List_Types.Nil.value;
            };
            if (v1 instanceof Data_List_Types.Cons) {
                $tco_var_v = v - 1 | 0;
                $copy_v1 = v1.value1;
                return;
            };
            throw new Error("Failed pattern match at Data.List (line 543, column 1 - line 543, column 42): " + [ v.constructor.name, v1.constructor.name ]);
        };
        while (!$tco_done) {
            $tco_result = $tco_loop($tco_var_v, $copy_v1);
        };
        return $tco_result;
    };
};
var slice = function (start) {
    return function (end) {
        return function (xs) {
            return take(end - start | 0)(drop(start)(xs));
        };
    };
};
var takeEnd = function (n) {
    return function (xs) {
        return drop(length(xs) - n | 0)(xs);
    };
};
var deleteBy = function (v) {
    return function (v1) {
        return function (v2) {
            if (v2 instanceof Data_List_Types.Nil) {
                return Data_List_Types.Nil.value;
            };
            if (v2 instanceof Data_List_Types.Cons && v(v1)(v2.value0)) {
                return v2.value1;
            };
            if (v2 instanceof Data_List_Types.Cons) {
                return new Data_List_Types.Cons(v2.value0, deleteBy(v)(v1)(v2.value1));
            };
            throw new Error("Failed pattern match at Data.List (line 671, column 1 - line 671, column 67): " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
        };
    };
};
var unionBy = function (eq) {
    return function (xs) {
        return function (ys) {
            return Data_Semigroup.append(Data_List_Types.semigroupList)(xs)(Data_Foldable.foldl(Data_List_Types.foldableList)(Data_Function.flip(deleteBy(eq)))(nubBy(eq)(ys))(xs));
        };
    };
};
var union = function (dictEq) {
    return unionBy(Data_Eq.eq(dictEq));
};
var deleteAt = function (v) {
    return function (v1) {
        if (v === 0 && v1 instanceof Data_List_Types.Cons) {
            return new Data_Maybe.Just(v1.value1);
        };
        if (v1 instanceof Data_List_Types.Cons) {
            return Data_Functor.map(Data_Maybe.functorMaybe)(function (v2) {
                return new Data_List_Types.Cons(v1.value0, v2);
            })(deleteAt(v - 1 | 0)(v1.value1));
        };
        return Data_Maybe.Nothing.value;
    };
};
var $$delete = function (dictEq) {
    return deleteBy(Data_Eq.eq(dictEq));
};
var difference = function (dictEq) {
    return Data_Foldable.foldl(Data_List_Types.foldableList)(Data_Function.flip($$delete(dictEq)));
};
var concatMap = Data_Function.flip(Control_Bind.bind(Data_List_Types.bindList));
var concat = function (v) {
    return Control_Bind.bind(Data_List_Types.bindList)(v)(Control_Category.identity(Control_Category.categoryFn));
};
var catMaybes = mapMaybe(Control_Category.identity(Control_Category.categoryFn));
var alterAt = function (v) {
    return function (v1) {
        return function (v2) {
            if (v === 0 && v2 instanceof Data_List_Types.Cons) {
                return Data_Maybe.Just.create((function () {
                    var v3 = v1(v2.value0);
                    if (v3 instanceof Data_Maybe.Nothing) {
                        return v2.value1;
                    };
                    if (v3 instanceof Data_Maybe.Just) {
                        return new Data_List_Types.Cons(v3.value0, v2.value1);
                    };
                    throw new Error("Failed pattern match at Data.List (line 352, column 3 - line 354, column 23): " + [ v3.constructor.name ]);
                })());
            };
            if (v2 instanceof Data_List_Types.Cons) {
                return Data_Functor.map(Data_Maybe.functorMaybe)(function (v3) {
                    return new Data_List_Types.Cons(v2.value0, v3);
                })(alterAt(v - 1 | 0)(v1)(v2.value1));
            };
            return Data_Maybe.Nothing.value;
        };
    };
};
var modifyAt = function (n) {
    return function (f) {
        return alterAt(n)(function ($339) {
            return Data_Maybe.Just.create(f($339));
        });
    };
};
module.exports = {
    toUnfoldable: toUnfoldable,
    fromFoldable: fromFoldable,
    singleton: singleton,
    range: range,
    some: some,
    someRec: someRec,
    many: many,
    manyRec: manyRec,
    "null": $$null,
    length: length,
    snoc: snoc,
    insert: insert,
    insertBy: insertBy,
    head: head,
    last: last,
    tail: tail,
    init: init,
    uncons: uncons,
    unsnoc: unsnoc,
    index: index,
    elemIndex: elemIndex,
    elemLastIndex: elemLastIndex,
    findIndex: findIndex,
    findLastIndex: findLastIndex,
    insertAt: insertAt,
    deleteAt: deleteAt,
    updateAt: updateAt,
    modifyAt: modifyAt,
    alterAt: alterAt,
    reverse: reverse,
    concat: concat,
    concatMap: concatMap,
    filter: filter,
    filterM: filterM,
    mapMaybe: mapMaybe,
    catMaybes: catMaybes,
    mapWithIndex: mapWithIndex,
    sort: sort,
    sortBy: sortBy,
    Pattern: Pattern,
    stripPrefix: stripPrefix,
    slice: slice,
    take: take,
    takeEnd: takeEnd,
    takeWhile: takeWhile,
    drop: drop,
    dropEnd: dropEnd,
    dropWhile: dropWhile,
    span: span,
    group: group,
    "group'": group$prime,
    groupBy: groupBy,
    partition: partition,
    nub: nub,
    nubBy: nubBy,
    union: union,
    unionBy: unionBy,
    "delete": $$delete,
    deleteBy: deleteBy,
    difference: difference,
    intersect: intersect,
    intersectBy: intersectBy,
    zipWith: zipWith,
    zipWithA: zipWithA,
    zip: zip,
    unzip: unzip,
    transpose: transpose,
    foldM: foldM,
    eqPattern: eqPattern,
    ordPattern: ordPattern,
    newtypePattern: newtypePattern,
    showPattern: showPattern
};

},{"../Control.Alt/index.js":25,"../Control.Alternative/index.js":26,"../Control.Applicative/index.js":27,"../Control.Apply/index.js":29,"../Control.Bind/index.js":33,"../Control.Category/index.js":34,"../Control.Lazy/index.js":38,"../Control.Monad.Rec.Class/index.js":44,"../Control.Semigroupoid/index.js":56,"../Data.Bifunctor/index.js":71,"../Data.Boolean/index.js":73,"../Data.Eq/index.js":82,"../Data.Foldable/index.js":87,"../Data.Function/index.js":91,"../Data.Functor/index.js":95,"../Data.FunctorWithIndex/index.js":97,"../Data.HeytingAlgebra/index.js":99,"../Data.List.Types/index.js":106,"../Data.Maybe/index.js":110,"../Data.Newtype/index.js":119,"../Data.NonEmpty/index.js":120,"../Data.Ord/index.js":130,"../Data.Ordering/index.js":131,"../Data.Ring/index.js":133,"../Data.Semigroup/index.js":139,"../Data.Semiring/index.js":141,"../Data.Show/index.js":143,"../Data.Traversable/index.js":154,"../Data.Tuple/index.js":156,"../Data.Unfoldable/index.js":158,"../Data.Unit/index.js":162,"../Prelude/index.js":198}],108:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Alt = require("../Control.Alt/index.js");
var Control_Alternative = require("../Control.Alternative/index.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Extend = require("../Control.Extend/index.js");
var Control_Monad = require("../Control.Monad/index.js");
var Control_MonadZero = require("../Control.MonadZero/index.js");
var Control_Plus = require("../Control.Plus/index.js");
var Data_Bounded = require("../Data.Bounded/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Functor_Invariant = require("../Data.Functor.Invariant/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Monoid = require("../Data.Monoid/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Show = require("../Data.Show/index.js");
var Prelude = require("../Prelude/index.js");
var First = function (x) {
    return x;
};
var showFirst = function (dictShow) {
    return new Data_Show.Show(function (v) {
        return "First (" + (Data_Show.show(Data_Maybe.showMaybe(dictShow))(v) + ")");
    });
};
var semigroupFirst = new Data_Semigroup.Semigroup(function (v) {
    return function (v1) {
        if (v instanceof Data_Maybe.Just) {
            return v;
        };
        return v1;
    };
});
var ordFirst = function (dictOrd) {
    return Data_Maybe.ordMaybe(dictOrd);
};
var ord1First = Data_Maybe.ord1Maybe;
var newtypeFirst = new Data_Newtype.Newtype(function (n) {
    return n;
}, First);
var monoidFirst = new Data_Monoid.Monoid(function () {
    return semigroupFirst;
}, Data_Maybe.Nothing.value);
var monadFirst = Data_Maybe.monadMaybe;
var invariantFirst = Data_Maybe.invariantMaybe;
var functorFirst = Data_Maybe.functorMaybe;
var extendFirst = Data_Maybe.extendMaybe;
var eqFirst = function (dictEq) {
    return Data_Maybe.eqMaybe(dictEq);
};
var eq1First = Data_Maybe.eq1Maybe;
var boundedFirst = function (dictBounded) {
    return Data_Maybe.boundedMaybe(dictBounded);
};
var bindFirst = Data_Maybe.bindMaybe;
var applyFirst = Data_Maybe.applyMaybe;
var applicativeFirst = Data_Maybe.applicativeMaybe;
var altFirst = new Control_Alt.Alt(function () {
    return functorFirst;
}, Data_Semigroup.append(semigroupFirst));
var plusFirst = new Control_Plus.Plus(function () {
    return altFirst;
}, Data_Monoid.mempty(monoidFirst));
var alternativeFirst = new Control_Alternative.Alternative(function () {
    return applicativeFirst;
}, function () {
    return plusFirst;
});
var monadZeroFirst = new Control_MonadZero.MonadZero(function () {
    return alternativeFirst;
}, function () {
    return monadFirst;
});
module.exports = {
    First: First,
    newtypeFirst: newtypeFirst,
    eqFirst: eqFirst,
    eq1First: eq1First,
    ordFirst: ordFirst,
    ord1First: ord1First,
    boundedFirst: boundedFirst,
    functorFirst: functorFirst,
    invariantFirst: invariantFirst,
    applyFirst: applyFirst,
    applicativeFirst: applicativeFirst,
    bindFirst: bindFirst,
    monadFirst: monadFirst,
    extendFirst: extendFirst,
    showFirst: showFirst,
    semigroupFirst: semigroupFirst,
    monoidFirst: monoidFirst,
    altFirst: altFirst,
    plusFirst: plusFirst,
    alternativeFirst: alternativeFirst,
    monadZeroFirst: monadZeroFirst
};

},{"../Control.Alt/index.js":25,"../Control.Alternative/index.js":26,"../Control.Applicative/index.js":27,"../Control.Apply/index.js":29,"../Control.Bind/index.js":33,"../Control.Extend/index.js":37,"../Control.Monad/index.js":52,"../Control.MonadZero/index.js":54,"../Control.Plus/index.js":55,"../Data.Bounded/index.js":76,"../Data.Eq/index.js":82,"../Data.Functor.Invariant/index.js":93,"../Data.Functor/index.js":95,"../Data.Maybe/index.js":110,"../Data.Monoid/index.js":117,"../Data.Newtype/index.js":119,"../Data.Ord/index.js":130,"../Data.Semigroup/index.js":139,"../Data.Show/index.js":143,"../Prelude/index.js":198}],109:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Alt = require("../Control.Alt/index.js");
var Control_Alternative = require("../Control.Alternative/index.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Extend = require("../Control.Extend/index.js");
var Control_Monad = require("../Control.Monad/index.js");
var Control_MonadZero = require("../Control.MonadZero/index.js");
var Control_Plus = require("../Control.Plus/index.js");
var Data_Bounded = require("../Data.Bounded/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Functor_Invariant = require("../Data.Functor.Invariant/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Monoid = require("../Data.Monoid/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Show = require("../Data.Show/index.js");
var Prelude = require("../Prelude/index.js");
var Last = function (x) {
    return x;
};
var showLast = function (dictShow) {
    return new Data_Show.Show(function (v) {
        return "(Last " + (Data_Show.show(Data_Maybe.showMaybe(dictShow))(v) + ")");
    });
};
var semigroupLast = new Data_Semigroup.Semigroup(function (v) {
    return function (v1) {
        if (v1 instanceof Data_Maybe.Just) {
            return v1;
        };
        if (v1 instanceof Data_Maybe.Nothing) {
            return v;
        };
        throw new Error("Failed pattern match at Data.Maybe.Last (line 52, column 1 - line 52, column 45): " + [ v.constructor.name, v1.constructor.name ]);
    };
});
var ordLast = function (dictOrd) {
    return Data_Maybe.ordMaybe(dictOrd);
};
var ord1Last = Data_Maybe.ord1Maybe;
var newtypeLast = new Data_Newtype.Newtype(function (n) {
    return n;
}, Last);
var monoidLast = new Data_Monoid.Monoid(function () {
    return semigroupLast;
}, Data_Maybe.Nothing.value);
var monadLast = Data_Maybe.monadMaybe;
var invariantLast = Data_Maybe.invariantMaybe;
var functorLast = Data_Maybe.functorMaybe;
var extendLast = Data_Maybe.extendMaybe;
var eqLast = function (dictEq) {
    return Data_Maybe.eqMaybe(dictEq);
};
var eq1Last = Data_Maybe.eq1Maybe;
var boundedLast = function (dictBounded) {
    return Data_Maybe.boundedMaybe(dictBounded);
};
var bindLast = Data_Maybe.bindMaybe;
var applyLast = Data_Maybe.applyMaybe;
var applicativeLast = Data_Maybe.applicativeMaybe;
var altLast = new Control_Alt.Alt(function () {
    return functorLast;
}, Data_Semigroup.append(semigroupLast));
var plusLast = new Control_Plus.Plus(function () {
    return altLast;
}, Data_Monoid.mempty(monoidLast));
var alternativeLast = new Control_Alternative.Alternative(function () {
    return applicativeLast;
}, function () {
    return plusLast;
});
var monadZeroLast = new Control_MonadZero.MonadZero(function () {
    return alternativeLast;
}, function () {
    return monadLast;
});
module.exports = {
    Last: Last,
    newtypeLast: newtypeLast,
    eqLast: eqLast,
    eq1Last: eq1Last,
    ordLast: ordLast,
    ord1Last: ord1Last,
    boundedLast: boundedLast,
    functorLast: functorLast,
    invariantLast: invariantLast,
    applyLast: applyLast,
    applicativeLast: applicativeLast,
    bindLast: bindLast,
    monadLast: monadLast,
    extendLast: extendLast,
    showLast: showLast,
    semigroupLast: semigroupLast,
    monoidLast: monoidLast,
    altLast: altLast,
    plusLast: plusLast,
    alternativeLast: alternativeLast,
    monadZeroLast: monadZeroLast
};

},{"../Control.Alt/index.js":25,"../Control.Alternative/index.js":26,"../Control.Applicative/index.js":27,"../Control.Apply/index.js":29,"../Control.Bind/index.js":33,"../Control.Extend/index.js":37,"../Control.Monad/index.js":52,"../Control.MonadZero/index.js":54,"../Control.Plus/index.js":55,"../Data.Bounded/index.js":76,"../Data.Eq/index.js":82,"../Data.Functor.Invariant/index.js":93,"../Data.Functor/index.js":95,"../Data.Maybe/index.js":110,"../Data.Monoid/index.js":117,"../Data.Newtype/index.js":119,"../Data.Ord/index.js":130,"../Data.Semigroup/index.js":139,"../Data.Show/index.js":143,"../Prelude/index.js":198}],110:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Alt = require("../Control.Alt/index.js");
var Control_Alternative = require("../Control.Alternative/index.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Category = require("../Control.Category/index.js");
var Control_Extend = require("../Control.Extend/index.js");
var Control_Monad = require("../Control.Monad/index.js");
var Control_MonadZero = require("../Control.MonadZero/index.js");
var Control_Plus = require("../Control.Plus/index.js");
var Data_Bounded = require("../Data.Bounded/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Functor_Invariant = require("../Data.Functor.Invariant/index.js");
var Data_Monoid = require("../Data.Monoid/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Ordering = require("../Data.Ordering/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Show = require("../Data.Show/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Prelude = require("../Prelude/index.js");
var Nothing = (function () {
    function Nothing() {

    };
    Nothing.value = new Nothing();
    return Nothing;
})();
var Just = (function () {
    function Just(value0) {
        this.value0 = value0;
    };
    Just.create = function (value0) {
        return new Just(value0);
    };
    return Just;
})();
var showMaybe = function (dictShow) {
    return new Data_Show.Show(function (v) {
        if (v instanceof Just) {
            return "(Just " + (Data_Show.show(dictShow)(v.value0) + ")");
        };
        if (v instanceof Nothing) {
            return "Nothing";
        };
        throw new Error("Failed pattern match at Data.Maybe (line 205, column 1 - line 205, column 47): " + [ v.constructor.name ]);
    });
};
var semigroupMaybe = function (dictSemigroup) {
    return new Data_Semigroup.Semigroup(function (v) {
        return function (v1) {
            if (v instanceof Nothing) {
                return v1;
            };
            if (v1 instanceof Nothing) {
                return v;
            };
            if (v instanceof Just && v1 instanceof Just) {
                return new Just(Data_Semigroup.append(dictSemigroup)(v.value0)(v1.value0));
            };
            throw new Error("Failed pattern match at Data.Maybe (line 174, column 1 - line 174, column 62): " + [ v.constructor.name, v1.constructor.name ]);
        };
    });
};
var optional = function (dictAlternative) {
    return function (a) {
        return Control_Alt.alt((dictAlternative.Plus1()).Alt0())(Data_Functor.map(((dictAlternative.Plus1()).Alt0()).Functor0())(Just.create)(a))(Control_Applicative.pure(dictAlternative.Applicative0())(Nothing.value));
    };
};
var monoidMaybe = function (dictSemigroup) {
    return new Data_Monoid.Monoid(function () {
        return semigroupMaybe(dictSemigroup);
    }, Nothing.value);
};
var maybe$prime = function (v) {
    return function (v1) {
        return function (v2) {
            if (v2 instanceof Nothing) {
                return v(Data_Unit.unit);
            };
            if (v2 instanceof Just) {
                return v1(v2.value0);
            };
            throw new Error("Failed pattern match at Data.Maybe (line 230, column 1 - line 230, column 62): " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
        };
    };
};
var maybe = function (v) {
    return function (v1) {
        return function (v2) {
            if (v2 instanceof Nothing) {
                return v;
            };
            if (v2 instanceof Just) {
                return v1(v2.value0);
            };
            throw new Error("Failed pattern match at Data.Maybe (line 217, column 1 - line 217, column 51): " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
        };
    };
};
var isNothing = maybe(true)(Data_Function["const"](false));
var isJust = maybe(false)(Data_Function["const"](true));
var functorMaybe = new Data_Functor.Functor(function (v) {
    return function (v1) {
        if (v1 instanceof Just) {
            return new Just(v(v1.value0));
        };
        return Nothing.value;
    };
});
var invariantMaybe = new Data_Functor_Invariant.Invariant(Data_Functor_Invariant.imapF(functorMaybe));
var fromMaybe$prime = function (a) {
    return maybe$prime(a)(Control_Category.identity(Control_Category.categoryFn));
};
var fromMaybe = function (a) {
    return maybe(a)(Control_Category.identity(Control_Category.categoryFn));
};
var fromJust = function (dictPartial) {
    return function (v) {
        if (v instanceof Just) {
            return v.value0;
        };
        throw new Error("Failed pattern match at Data.Maybe (line 268, column 1 - line 268, column 46): " + [ v.constructor.name ]);
    };
};
var extendMaybe = new Control_Extend.Extend(function () {
    return functorMaybe;
}, function (v) {
    return function (v1) {
        if (v1 instanceof Nothing) {
            return Nothing.value;
        };
        return new Just(v(v1));
    };
});
var eqMaybe = function (dictEq) {
    return new Data_Eq.Eq(function (x) {
        return function (y) {
            if (x instanceof Nothing && y instanceof Nothing) {
                return true;
            };
            if (x instanceof Just && y instanceof Just) {
                return Data_Eq.eq(dictEq)(x.value0)(y.value0);
            };
            return false;
        };
    });
};
var ordMaybe = function (dictOrd) {
    return new Data_Ord.Ord(function () {
        return eqMaybe(dictOrd.Eq0());
    }, function (x) {
        return function (y) {
            if (x instanceof Nothing && y instanceof Nothing) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof Nothing) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof Nothing) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof Just && y instanceof Just) {
                return Data_Ord.compare(dictOrd)(x.value0)(y.value0);
            };
            throw new Error("Failed pattern match at Data.Maybe (line 194, column 8 - line 194, column 51): " + [ x.constructor.name, y.constructor.name ]);
        };
    });
};
var eq1Maybe = new Data_Eq.Eq1(function (dictEq) {
    return Data_Eq.eq(eqMaybe(dictEq));
});
var ord1Maybe = new Data_Ord.Ord1(function () {
    return eq1Maybe;
}, function (dictOrd) {
    return Data_Ord.compare(ordMaybe(dictOrd));
});
var boundedMaybe = function (dictBounded) {
    return new Data_Bounded.Bounded(function () {
        return ordMaybe(dictBounded.Ord0());
    }, Nothing.value, new Just(Data_Bounded.top(dictBounded)));
};
var applyMaybe = new Control_Apply.Apply(function () {
    return functorMaybe;
}, function (v) {
    return function (v1) {
        if (v instanceof Just) {
            return Data_Functor.map(functorMaybe)(v.value0)(v1);
        };
        if (v instanceof Nothing) {
            return Nothing.value;
        };
        throw new Error("Failed pattern match at Data.Maybe (line 67, column 1 - line 67, column 35): " + [ v.constructor.name, v1.constructor.name ]);
    };
});
var bindMaybe = new Control_Bind.Bind(function () {
    return applyMaybe;
}, function (v) {
    return function (v1) {
        if (v instanceof Just) {
            return v1(v.value0);
        };
        if (v instanceof Nothing) {
            return Nothing.value;
        };
        throw new Error("Failed pattern match at Data.Maybe (line 125, column 1 - line 125, column 33): " + [ v.constructor.name, v1.constructor.name ]);
    };
});
var applicativeMaybe = new Control_Applicative.Applicative(function () {
    return applyMaybe;
}, Just.create);
var monadMaybe = new Control_Monad.Monad(function () {
    return applicativeMaybe;
}, function () {
    return bindMaybe;
});
var altMaybe = new Control_Alt.Alt(function () {
    return functorMaybe;
}, function (v) {
    return function (v1) {
        if (v instanceof Nothing) {
            return v1;
        };
        return v;
    };
});
var plusMaybe = new Control_Plus.Plus(function () {
    return altMaybe;
}, Nothing.value);
var alternativeMaybe = new Control_Alternative.Alternative(function () {
    return applicativeMaybe;
}, function () {
    return plusMaybe;
});
var monadZeroMaybe = new Control_MonadZero.MonadZero(function () {
    return alternativeMaybe;
}, function () {
    return monadMaybe;
});
module.exports = {
    Nothing: Nothing,
    Just: Just,
    maybe: maybe,
    "maybe'": maybe$prime,
    fromMaybe: fromMaybe,
    "fromMaybe'": fromMaybe$prime,
    isJust: isJust,
    isNothing: isNothing,
    fromJust: fromJust,
    optional: optional,
    functorMaybe: functorMaybe,
    applyMaybe: applyMaybe,
    applicativeMaybe: applicativeMaybe,
    altMaybe: altMaybe,
    plusMaybe: plusMaybe,
    alternativeMaybe: alternativeMaybe,
    bindMaybe: bindMaybe,
    monadMaybe: monadMaybe,
    monadZeroMaybe: monadZeroMaybe,
    extendMaybe: extendMaybe,
    invariantMaybe: invariantMaybe,
    semigroupMaybe: semigroupMaybe,
    monoidMaybe: monoidMaybe,
    eqMaybe: eqMaybe,
    eq1Maybe: eq1Maybe,
    ordMaybe: ordMaybe,
    ord1Maybe: ord1Maybe,
    boundedMaybe: boundedMaybe,
    showMaybe: showMaybe
};

},{"../Control.Alt/index.js":25,"../Control.Alternative/index.js":26,"../Control.Applicative/index.js":27,"../Control.Apply/index.js":29,"../Control.Bind/index.js":33,"../Control.Category/index.js":34,"../Control.Extend/index.js":37,"../Control.Monad/index.js":52,"../Control.MonadZero/index.js":54,"../Control.Plus/index.js":55,"../Data.Bounded/index.js":76,"../Data.Eq/index.js":82,"../Data.Function/index.js":91,"../Data.Functor.Invariant/index.js":93,"../Data.Functor/index.js":95,"../Data.Monoid/index.js":117,"../Data.Ord/index.js":130,"../Data.Ordering/index.js":131,"../Data.Semigroup/index.js":139,"../Data.Show/index.js":143,"../Data.Unit/index.js":162,"../Prelude/index.js":198}],111:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Monad = require("../Control.Monad/index.js");
var Data_Bounded = require("../Data.Bounded/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Monoid = require("../Data.Monoid/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Semiring = require("../Data.Semiring/index.js");
var Data_Show = require("../Data.Show/index.js");
var Prelude = require("../Prelude/index.js");
var Additive = function (x) {
    return x;
};
var showAdditive = function (dictShow) {
    return new Data_Show.Show(function (v) {
        return "(Additive " + (Data_Show.show(dictShow)(v) + ")");
    });
};
var semigroupAdditive = function (dictSemiring) {
    return new Data_Semigroup.Semigroup(function (v) {
        return function (v1) {
            return Data_Semiring.add(dictSemiring)(v)(v1);
        };
    });
};
var ordAdditive = function (dictOrd) {
    return dictOrd;
};
var monoidAdditive = function (dictSemiring) {
    return new Data_Monoid.Monoid(function () {
        return semigroupAdditive(dictSemiring);
    }, Data_Semiring.zero(dictSemiring));
};
var functorAdditive = new Data_Functor.Functor(function (f) {
    return function (m) {
        return f(m);
    };
});
var eqAdditive = function (dictEq) {
    return dictEq;
};
var eq1Additive = new Data_Eq.Eq1(function (dictEq) {
    return Data_Eq.eq(eqAdditive(dictEq));
});
var ord1Additive = new Data_Ord.Ord1(function () {
    return eq1Additive;
}, function (dictOrd) {
    return Data_Ord.compare(ordAdditive(dictOrd));
});
var boundedAdditive = function (dictBounded) {
    return dictBounded;
};
var applyAdditive = new Control_Apply.Apply(function () {
    return functorAdditive;
}, function (v) {
    return function (v1) {
        return v(v1);
    };
});
var bindAdditive = new Control_Bind.Bind(function () {
    return applyAdditive;
}, function (v) {
    return function (f) {
        return f(v);
    };
});
var applicativeAdditive = new Control_Applicative.Applicative(function () {
    return applyAdditive;
}, Additive);
var monadAdditive = new Control_Monad.Monad(function () {
    return applicativeAdditive;
}, function () {
    return bindAdditive;
});
module.exports = {
    Additive: Additive,
    eqAdditive: eqAdditive,
    eq1Additive: eq1Additive,
    ordAdditive: ordAdditive,
    ord1Additive: ord1Additive,
    boundedAdditive: boundedAdditive,
    showAdditive: showAdditive,
    functorAdditive: functorAdditive,
    applyAdditive: applyAdditive,
    applicativeAdditive: applicativeAdditive,
    bindAdditive: bindAdditive,
    monadAdditive: monadAdditive,
    semigroupAdditive: semigroupAdditive,
    monoidAdditive: monoidAdditive
};

},{"../Control.Applicative/index.js":27,"../Control.Apply/index.js":29,"../Control.Bind/index.js":33,"../Control.Monad/index.js":52,"../Data.Bounded/index.js":76,"../Data.Eq/index.js":82,"../Data.Functor/index.js":95,"../Data.Monoid/index.js":117,"../Data.Ord/index.js":130,"../Data.Semigroup/index.js":139,"../Data.Semiring/index.js":141,"../Data.Show/index.js":143,"../Prelude/index.js":198}],112:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Monad = require("../Control.Monad/index.js");
var Data_Bounded = require("../Data.Bounded/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra/index.js");
var Data_Monoid = require("../Data.Monoid/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Semiring = require("../Data.Semiring/index.js");
var Data_Show = require("../Data.Show/index.js");
var Prelude = require("../Prelude/index.js");
var Conj = function (x) {
    return x;
};
var showConj = function (dictShow) {
    return new Data_Show.Show(function (v) {
        return "(Conj " + (Data_Show.show(dictShow)(v) + ")");
    });
};
var semiringConj = function (dictHeytingAlgebra) {
    return new Data_Semiring.Semiring(function (v) {
        return function (v1) {
            return Data_HeytingAlgebra.conj(dictHeytingAlgebra)(v)(v1);
        };
    }, function (v) {
        return function (v1) {
            return Data_HeytingAlgebra.disj(dictHeytingAlgebra)(v)(v1);
        };
    }, Data_HeytingAlgebra.ff(dictHeytingAlgebra), Data_HeytingAlgebra.tt(dictHeytingAlgebra));
};
var semigroupConj = function (dictHeytingAlgebra) {
    return new Data_Semigroup.Semigroup(function (v) {
        return function (v1) {
            return Data_HeytingAlgebra.conj(dictHeytingAlgebra)(v)(v1);
        };
    });
};
var ordConj = function (dictOrd) {
    return dictOrd;
};
var monoidConj = function (dictHeytingAlgebra) {
    return new Data_Monoid.Monoid(function () {
        return semigroupConj(dictHeytingAlgebra);
    }, Data_HeytingAlgebra.tt(dictHeytingAlgebra));
};
var functorConj = new Data_Functor.Functor(function (f) {
    return function (m) {
        return f(m);
    };
});
var eqConj = function (dictEq) {
    return dictEq;
};
var eq1Conj = new Data_Eq.Eq1(function (dictEq) {
    return Data_Eq.eq(eqConj(dictEq));
});
var ord1Conj = new Data_Ord.Ord1(function () {
    return eq1Conj;
}, function (dictOrd) {
    return Data_Ord.compare(ordConj(dictOrd));
});
var boundedConj = function (dictBounded) {
    return dictBounded;
};
var applyConj = new Control_Apply.Apply(function () {
    return functorConj;
}, function (v) {
    return function (v1) {
        return v(v1);
    };
});
var bindConj = new Control_Bind.Bind(function () {
    return applyConj;
}, function (v) {
    return function (f) {
        return f(v);
    };
});
var applicativeConj = new Control_Applicative.Applicative(function () {
    return applyConj;
}, Conj);
var monadConj = new Control_Monad.Monad(function () {
    return applicativeConj;
}, function () {
    return bindConj;
});
module.exports = {
    Conj: Conj,
    eqConj: eqConj,
    eq1Conj: eq1Conj,
    ordConj: ordConj,
    ord1Conj: ord1Conj,
    boundedConj: boundedConj,
    showConj: showConj,
    functorConj: functorConj,
    applyConj: applyConj,
    applicativeConj: applicativeConj,
    bindConj: bindConj,
    monadConj: monadConj,
    semigroupConj: semigroupConj,
    monoidConj: monoidConj,
    semiringConj: semiringConj
};

},{"../Control.Applicative/index.js":27,"../Control.Apply/index.js":29,"../Control.Bind/index.js":33,"../Control.Monad/index.js":52,"../Data.Bounded/index.js":76,"../Data.Eq/index.js":82,"../Data.Functor/index.js":95,"../Data.HeytingAlgebra/index.js":99,"../Data.Monoid/index.js":117,"../Data.Ord/index.js":130,"../Data.Semigroup/index.js":139,"../Data.Semiring/index.js":141,"../Data.Show/index.js":143,"../Prelude/index.js":198}],113:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Monad = require("../Control.Monad/index.js");
var Data_Bounded = require("../Data.Bounded/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra/index.js");
var Data_Monoid = require("../Data.Monoid/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Semiring = require("../Data.Semiring/index.js");
var Data_Show = require("../Data.Show/index.js");
var Prelude = require("../Prelude/index.js");
var Disj = function (x) {
    return x;
};
var showDisj = function (dictShow) {
    return new Data_Show.Show(function (v) {
        return "(Disj " + (Data_Show.show(dictShow)(v) + ")");
    });
};
var semiringDisj = function (dictHeytingAlgebra) {
    return new Data_Semiring.Semiring(function (v) {
        return function (v1) {
            return Data_HeytingAlgebra.disj(dictHeytingAlgebra)(v)(v1);
        };
    }, function (v) {
        return function (v1) {
            return Data_HeytingAlgebra.conj(dictHeytingAlgebra)(v)(v1);
        };
    }, Data_HeytingAlgebra.tt(dictHeytingAlgebra), Data_HeytingAlgebra.ff(dictHeytingAlgebra));
};
var semigroupDisj = function (dictHeytingAlgebra) {
    return new Data_Semigroup.Semigroup(function (v) {
        return function (v1) {
            return Data_HeytingAlgebra.disj(dictHeytingAlgebra)(v)(v1);
        };
    });
};
var ordDisj = function (dictOrd) {
    return dictOrd;
};
var monoidDisj = function (dictHeytingAlgebra) {
    return new Data_Monoid.Monoid(function () {
        return semigroupDisj(dictHeytingAlgebra);
    }, Data_HeytingAlgebra.ff(dictHeytingAlgebra));
};
var functorDisj = new Data_Functor.Functor(function (f) {
    return function (m) {
        return f(m);
    };
});
var eqDisj = function (dictEq) {
    return dictEq;
};
var eq1Disj = new Data_Eq.Eq1(function (dictEq) {
    return Data_Eq.eq(eqDisj(dictEq));
});
var ord1Disj = new Data_Ord.Ord1(function () {
    return eq1Disj;
}, function (dictOrd) {
    return Data_Ord.compare(ordDisj(dictOrd));
});
var boundedDisj = function (dictBounded) {
    return dictBounded;
};
var applyDisj = new Control_Apply.Apply(function () {
    return functorDisj;
}, function (v) {
    return function (v1) {
        return v(v1);
    };
});
var bindDisj = new Control_Bind.Bind(function () {
    return applyDisj;
}, function (v) {
    return function (f) {
        return f(v);
    };
});
var applicativeDisj = new Control_Applicative.Applicative(function () {
    return applyDisj;
}, Disj);
var monadDisj = new Control_Monad.Monad(function () {
    return applicativeDisj;
}, function () {
    return bindDisj;
});
module.exports = {
    Disj: Disj,
    eqDisj: eqDisj,
    eq1Disj: eq1Disj,
    ordDisj: ordDisj,
    ord1Disj: ord1Disj,
    boundedDisj: boundedDisj,
    showDisj: showDisj,
    functorDisj: functorDisj,
    applyDisj: applyDisj,
    applicativeDisj: applicativeDisj,
    bindDisj: bindDisj,
    monadDisj: monadDisj,
    semigroupDisj: semigroupDisj,
    monoidDisj: monoidDisj,
    semiringDisj: semiringDisj
};

},{"../Control.Applicative/index.js":27,"../Control.Apply/index.js":29,"../Control.Bind/index.js":33,"../Control.Monad/index.js":52,"../Data.Bounded/index.js":76,"../Data.Eq/index.js":82,"../Data.Functor/index.js":95,"../Data.HeytingAlgebra/index.js":99,"../Data.Monoid/index.js":117,"../Data.Ord/index.js":130,"../Data.Semigroup/index.js":139,"../Data.Semiring/index.js":141,"../Data.Show/index.js":143,"../Prelude/index.js":198}],114:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Monad = require("../Control.Monad/index.js");
var Data_Bounded = require("../Data.Bounded/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Monoid = require("../Data.Monoid/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Show = require("../Data.Show/index.js");
var Prelude = require("../Prelude/index.js");
var Dual = function (x) {
    return x;
};
var showDual = function (dictShow) {
    return new Data_Show.Show(function (v) {
        return "(Dual " + (Data_Show.show(dictShow)(v) + ")");
    });
};
var semigroupDual = function (dictSemigroup) {
    return new Data_Semigroup.Semigroup(function (v) {
        return function (v1) {
            return Data_Semigroup.append(dictSemigroup)(v1)(v);
        };
    });
};
var ordDual = function (dictOrd) {
    return dictOrd;
};
var monoidDual = function (dictMonoid) {
    return new Data_Monoid.Monoid(function () {
        return semigroupDual(dictMonoid.Semigroup0());
    }, Data_Monoid.mempty(dictMonoid));
};
var functorDual = new Data_Functor.Functor(function (f) {
    return function (m) {
        return f(m);
    };
});
var eqDual = function (dictEq) {
    return dictEq;
};
var eq1Dual = new Data_Eq.Eq1(function (dictEq) {
    return Data_Eq.eq(eqDual(dictEq));
});
var ord1Dual = new Data_Ord.Ord1(function () {
    return eq1Dual;
}, function (dictOrd) {
    return Data_Ord.compare(ordDual(dictOrd));
});
var boundedDual = function (dictBounded) {
    return dictBounded;
};
var applyDual = new Control_Apply.Apply(function () {
    return functorDual;
}, function (v) {
    return function (v1) {
        return v(v1);
    };
});
var bindDual = new Control_Bind.Bind(function () {
    return applyDual;
}, function (v) {
    return function (f) {
        return f(v);
    };
});
var applicativeDual = new Control_Applicative.Applicative(function () {
    return applyDual;
}, Dual);
var monadDual = new Control_Monad.Monad(function () {
    return applicativeDual;
}, function () {
    return bindDual;
});
module.exports = {
    Dual: Dual,
    eqDual: eqDual,
    eq1Dual: eq1Dual,
    ordDual: ordDual,
    ord1Dual: ord1Dual,
    boundedDual: boundedDual,
    showDual: showDual,
    functorDual: functorDual,
    applyDual: applyDual,
    applicativeDual: applicativeDual,
    bindDual: bindDual,
    monadDual: monadDual,
    semigroupDual: semigroupDual,
    monoidDual: monoidDual
};

},{"../Control.Applicative/index.js":27,"../Control.Apply/index.js":29,"../Control.Bind/index.js":33,"../Control.Monad/index.js":52,"../Data.Bounded/index.js":76,"../Data.Eq/index.js":82,"../Data.Functor/index.js":95,"../Data.Monoid/index.js":117,"../Data.Ord/index.js":130,"../Data.Semigroup/index.js":139,"../Data.Show/index.js":143,"../Prelude/index.js":198}],115:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Category = require("../Control.Category/index.js");
var Control_Semigroupoid = require("../Control.Semigroupoid/index.js");
var Data_Bounded = require("../Data.Bounded/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Monoid = require("../Data.Monoid/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Show = require("../Data.Show/index.js");
var Prelude = require("../Prelude/index.js");
var Endo = function (x) {
    return x;
};
var showEndo = function (dictShow) {
    return new Data_Show.Show(function (v) {
        return "(Endo " + (Data_Show.show(dictShow)(v) + ")");
    });
};
var semigroupEndo = function (dictSemigroupoid) {
    return new Data_Semigroup.Semigroup(function (v) {
        return function (v1) {
            return Control_Semigroupoid.compose(dictSemigroupoid)(v)(v1);
        };
    });
};
var ordEndo = function (dictOrd) {
    return dictOrd;
};
var monoidEndo = function (dictCategory) {
    return new Data_Monoid.Monoid(function () {
        return semigroupEndo(dictCategory.Semigroupoid0());
    }, Control_Category.identity(dictCategory));
};
var eqEndo = function (dictEq) {
    return dictEq;
};
var boundedEndo = function (dictBounded) {
    return dictBounded;
};
module.exports = {
    Endo: Endo,
    eqEndo: eqEndo,
    ordEndo: ordEndo,
    boundedEndo: boundedEndo,
    showEndo: showEndo,
    semigroupEndo: semigroupEndo,
    monoidEndo: monoidEndo
};

},{"../Control.Category/index.js":34,"../Control.Semigroupoid/index.js":56,"../Data.Bounded/index.js":76,"../Data.Eq/index.js":82,"../Data.Monoid/index.js":117,"../Data.Ord/index.js":130,"../Data.Semigroup/index.js":139,"../Data.Show/index.js":143,"../Prelude/index.js":198}],116:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Monad = require("../Control.Monad/index.js");
var Data_Bounded = require("../Data.Bounded/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Monoid = require("../Data.Monoid/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Semiring = require("../Data.Semiring/index.js");
var Data_Show = require("../Data.Show/index.js");
var Prelude = require("../Prelude/index.js");
var Multiplicative = function (x) {
    return x;
};
var showMultiplicative = function (dictShow) {
    return new Data_Show.Show(function (v) {
        return "(Multiplicative " + (Data_Show.show(dictShow)(v) + ")");
    });
};
var semigroupMultiplicative = function (dictSemiring) {
    return new Data_Semigroup.Semigroup(function (v) {
        return function (v1) {
            return Data_Semiring.mul(dictSemiring)(v)(v1);
        };
    });
};
var ordMultiplicative = function (dictOrd) {
    return dictOrd;
};
var monoidMultiplicative = function (dictSemiring) {
    return new Data_Monoid.Monoid(function () {
        return semigroupMultiplicative(dictSemiring);
    }, Data_Semiring.one(dictSemiring));
};
var functorMultiplicative = new Data_Functor.Functor(function (f) {
    return function (m) {
        return f(m);
    };
});
var eqMultiplicative = function (dictEq) {
    return dictEq;
};
var eq1Multiplicative = new Data_Eq.Eq1(function (dictEq) {
    return Data_Eq.eq(eqMultiplicative(dictEq));
});
var ord1Multiplicative = new Data_Ord.Ord1(function () {
    return eq1Multiplicative;
}, function (dictOrd) {
    return Data_Ord.compare(ordMultiplicative(dictOrd));
});
var boundedMultiplicative = function (dictBounded) {
    return dictBounded;
};
var applyMultiplicative = new Control_Apply.Apply(function () {
    return functorMultiplicative;
}, function (v) {
    return function (v1) {
        return v(v1);
    };
});
var bindMultiplicative = new Control_Bind.Bind(function () {
    return applyMultiplicative;
}, function (v) {
    return function (f) {
        return f(v);
    };
});
var applicativeMultiplicative = new Control_Applicative.Applicative(function () {
    return applyMultiplicative;
}, Multiplicative);
var monadMultiplicative = new Control_Monad.Monad(function () {
    return applicativeMultiplicative;
}, function () {
    return bindMultiplicative;
});
module.exports = {
    Multiplicative: Multiplicative,
    eqMultiplicative: eqMultiplicative,
    eq1Multiplicative: eq1Multiplicative,
    ordMultiplicative: ordMultiplicative,
    ord1Multiplicative: ord1Multiplicative,
    boundedMultiplicative: boundedMultiplicative,
    showMultiplicative: showMultiplicative,
    functorMultiplicative: functorMultiplicative,
    applyMultiplicative: applyMultiplicative,
    applicativeMultiplicative: applicativeMultiplicative,
    bindMultiplicative: bindMultiplicative,
    monadMultiplicative: monadMultiplicative,
    semigroupMultiplicative: semigroupMultiplicative,
    monoidMultiplicative: monoidMultiplicative
};

},{"../Control.Applicative/index.js":27,"../Control.Apply/index.js":29,"../Control.Bind/index.js":33,"../Control.Monad/index.js":52,"../Data.Bounded/index.js":76,"../Data.Eq/index.js":82,"../Data.Functor/index.js":95,"../Data.Monoid/index.js":117,"../Data.Ord/index.js":130,"../Data.Semigroup/index.js":139,"../Data.Semiring/index.js":141,"../Data.Show/index.js":143,"../Prelude/index.js":198}],117:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Data_Boolean = require("../Data.Boolean/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_EuclideanRing = require("../Data.EuclideanRing/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Ordering = require("../Data.Ordering/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Symbol = require("../Data.Symbol/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Record_Unsafe = require("../Record.Unsafe/index.js");
var Type_Data_RowList = require("../Type.Data.RowList/index.js");
var Monoid = function (Semigroup0, mempty) {
    this.Semigroup0 = Semigroup0;
    this.mempty = mempty;
};
var MonoidRecord = function (SemigroupRecord0, memptyRecord) {
    this.SemigroupRecord0 = SemigroupRecord0;
    this.memptyRecord = memptyRecord;
};
var monoidUnit = new Monoid(function () {
    return Data_Semigroup.semigroupUnit;
}, Data_Unit.unit);
var monoidString = new Monoid(function () {
    return Data_Semigroup.semigroupString;
}, "");
var monoidRecordNil = new MonoidRecord(function () {
    return Data_Semigroup.semigroupRecordNil;
}, function (v) {
    return {};
});
var monoidOrdering = new Monoid(function () {
    return Data_Ordering.semigroupOrdering;
}, Data_Ordering.EQ.value);
var monoidArray = new Monoid(function () {
    return Data_Semigroup.semigroupArray;
}, [  ]);
var memptyRecord = function (dict) {
    return dict.memptyRecord;
};
var monoidRecord = function (dictRowToList) {
    return function (dictMonoidRecord) {
        return new Monoid(function () {
            return Data_Semigroup.semigroupRecord(dictRowToList)(dictMonoidRecord.SemigroupRecord0());
        }, memptyRecord(dictMonoidRecord)(Type_Data_RowList.RLProxy.value));
    };
};
var mempty = function (dict) {
    return dict.mempty;
};
var monoidFn = function (dictMonoid) {
    return new Monoid(function () {
        return Data_Semigroup.semigroupFn(dictMonoid.Semigroup0());
    }, function (v) {
        return mempty(dictMonoid);
    });
};
var monoidRecordCons = function (dictIsSymbol) {
    return function (dictMonoid) {
        return function (dictCons) {
            return function (dictMonoidRecord) {
                return new MonoidRecord(function () {
                    return Data_Semigroup.semigroupRecordCons(dictIsSymbol)(dictCons)(dictMonoidRecord.SemigroupRecord0())(dictMonoid.Semigroup0());
                }, function (v) {
                    var tail = memptyRecord(dictMonoidRecord)(Type_Data_RowList.RLProxy.value);
                    var key = Data_Symbol.reflectSymbol(dictIsSymbol)(Data_Symbol.SProxy.value);
                    var insert = Record_Unsafe.unsafeSet(key);
                    return insert(mempty(dictMonoid))(tail);
                });
            };
        };
    };
};
var power = function (dictMonoid) {
    return function (x) {
        var go = function (p) {
            if (p <= 0) {
                return mempty(dictMonoid);
            };
            if (p === 1) {
                return x;
            };
            if (Data_EuclideanRing.mod(Data_EuclideanRing.euclideanRingInt)(p)(2) === 0) {
                var x$prime = go(Data_EuclideanRing.div(Data_EuclideanRing.euclideanRingInt)(p)(2));
                return Data_Semigroup.append(dictMonoid.Semigroup0())(x$prime)(x$prime);
            };
            if (Data_Boolean.otherwise) {
                var x$prime = go(Data_EuclideanRing.div(Data_EuclideanRing.euclideanRingInt)(p)(2));
                return Data_Semigroup.append(dictMonoid.Semigroup0())(x$prime)(Data_Semigroup.append(dictMonoid.Semigroup0())(x$prime)(x));
            };
            throw new Error("Failed pattern match at Data.Monoid (line 66, column 3 - line 66, column 17): " + [ p.constructor.name ]);
        };
        return go;
    };
};
var guard = function (dictMonoid) {
    return function (v) {
        return function (v1) {
            if (v) {
                return v1;
            };
            if (!v) {
                return mempty(dictMonoid);
            };
            throw new Error("Failed pattern match at Data.Monoid (line 74, column 1 - line 74, column 49): " + [ v.constructor.name, v1.constructor.name ]);
        };
    };
};
module.exports = {
    Monoid: Monoid,
    mempty: mempty,
    power: power,
    guard: guard,
    MonoidRecord: MonoidRecord,
    memptyRecord: memptyRecord,
    monoidUnit: monoidUnit,
    monoidOrdering: monoidOrdering,
    monoidFn: monoidFn,
    monoidString: monoidString,
    monoidArray: monoidArray,
    monoidRecord: monoidRecord,
    monoidRecordNil: monoidRecordNil,
    monoidRecordCons: monoidRecordCons
};

},{"../Data.Boolean/index.js":73,"../Data.Eq/index.js":82,"../Data.EuclideanRing/index.js":84,"../Data.Ord/index.js":130,"../Data.Ordering/index.js":131,"../Data.Semigroup/index.js":139,"../Data.Symbol/index.js":150,"../Data.Unit/index.js":162,"../Record.Unsafe/index.js":200,"../Type.Data.RowList/index.js":204}],118:[function(require,module,exports){
arguments[4][64][0].apply(exports,arguments)
},{"dup":64}],119:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Semigroupoid = require("../Control.Semigroupoid/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Monoid_Additive = require("../Data.Monoid.Additive/index.js");
var Data_Monoid_Conj = require("../Data.Monoid.Conj/index.js");
var Data_Monoid_Disj = require("../Data.Monoid.Disj/index.js");
var Data_Monoid_Dual = require("../Data.Monoid.Dual/index.js");
var Data_Monoid_Endo = require("../Data.Monoid.Endo/index.js");
var Data_Monoid_Multiplicative = require("../Data.Monoid.Multiplicative/index.js");
var Data_Semigroup_First = require("../Data.Semigroup.First/index.js");
var Data_Semigroup_Last = require("../Data.Semigroup.Last/index.js");
var Prelude = require("../Prelude/index.js");
var Newtype = function (unwrap, wrap) {
    this.unwrap = unwrap;
    this.wrap = wrap;
};
var wrap = function (dict) {
    return dict.wrap;
};
var unwrap = function (dict) {
    return dict.unwrap;
};
var underF2 = function (dictFunctor) {
    return function (dictFunctor1) {
        return function (dictNewtype) {
            return function (dictNewtype1) {
                return function (v) {
                    return function (f) {
                        return function ($66) {
                            return function ($67) {
                                return Data_Functor.map(dictFunctor1)(unwrap(dictNewtype1))(Data_Function.on(f)(Data_Functor.map(dictFunctor)(wrap(dictNewtype)))($66)($67));
                            };
                        };
                    };
                };
            };
        };
    };
};
var underF = function (dictFunctor) {
    return function (dictFunctor1) {
        return function (dictNewtype) {
            return function (dictNewtype1) {
                return function (v) {
                    return function (f) {
                        return function ($68) {
                            return Data_Functor.map(dictFunctor1)(unwrap(dictNewtype1))(f(Data_Functor.map(dictFunctor)(wrap(dictNewtype))($68)));
                        };
                    };
                };
            };
        };
    };
};
var under2 = function (dictNewtype) {
    return function (dictNewtype1) {
        return function (v) {
            return function (f) {
                return function ($69) {
                    return function ($70) {
                        return unwrap(dictNewtype1)(Data_Function.on(f)(wrap(dictNewtype))($69)($70));
                    };
                };
            };
        };
    };
};
var under = function (dictNewtype) {
    return function (dictNewtype1) {
        return function (v) {
            return function (f) {
                return function ($71) {
                    return unwrap(dictNewtype1)(f(wrap(dictNewtype)($71)));
                };
            };
        };
    };
};
var un = function (dictNewtype) {
    return function (v) {
        return unwrap(dictNewtype);
    };
};
var traverse = function (dictFunctor) {
    return function (dictNewtype) {
        return function (v) {
            return function (f) {
                return function ($72) {
                    return Data_Functor.map(dictFunctor)(wrap(dictNewtype))(f(unwrap(dictNewtype)($72)));
                };
            };
        };
    };
};
var overF2 = function (dictFunctor) {
    return function (dictFunctor1) {
        return function (dictNewtype) {
            return function (dictNewtype1) {
                return function (v) {
                    return function (f) {
                        return function ($73) {
                            return function ($74) {
                                return Data_Functor.map(dictFunctor1)(wrap(dictNewtype1))(Data_Function.on(f)(Data_Functor.map(dictFunctor)(unwrap(dictNewtype)))($73)($74));
                            };
                        };
                    };
                };
            };
        };
    };
};
var overF = function (dictFunctor) {
    return function (dictFunctor1) {
        return function (dictNewtype) {
            return function (dictNewtype1) {
                return function (v) {
                    return function (f) {
                        return function ($75) {
                            return Data_Functor.map(dictFunctor1)(wrap(dictNewtype1))(f(Data_Functor.map(dictFunctor)(unwrap(dictNewtype))($75)));
                        };
                    };
                };
            };
        };
    };
};
var over2 = function (dictNewtype) {
    return function (dictNewtype1) {
        return function (v) {
            return function (f) {
                return function ($76) {
                    return function ($77) {
                        return wrap(dictNewtype1)(Data_Function.on(f)(unwrap(dictNewtype))($76)($77));
                    };
                };
            };
        };
    };
};
var over = function (dictNewtype) {
    return function (dictNewtype1) {
        return function (v) {
            return function (f) {
                return function ($78) {
                    return wrap(dictNewtype1)(f(unwrap(dictNewtype)($78)));
                };
            };
        };
    };
};
var op = function (dictNewtype) {
    return un(dictNewtype);
};
var newtypeMultiplicative = new Newtype(function (v) {
    return v;
}, Data_Monoid_Multiplicative.Multiplicative);
var newtypeLast = new Newtype(function (v) {
    return v;
}, Data_Semigroup_Last.Last);
var newtypeFirst = new Newtype(function (v) {
    return v;
}, Data_Semigroup_First.First);
var newtypeEndo = new Newtype(function (v) {
    return v;
}, Data_Monoid_Endo.Endo);
var newtypeDual = new Newtype(function (v) {
    return v;
}, Data_Monoid_Dual.Dual);
var newtypeDisj = new Newtype(function (v) {
    return v;
}, Data_Monoid_Disj.Disj);
var newtypeConj = new Newtype(function (v) {
    return v;
}, Data_Monoid_Conj.Conj);
var newtypeAdditive = new Newtype(function (v) {
    return v;
}, Data_Monoid_Additive.Additive);
var collect = function (dictFunctor) {
    return function (dictNewtype) {
        return function (v) {
            return function (f) {
                return function ($79) {
                    return wrap(dictNewtype)(f(Data_Functor.map(dictFunctor)(unwrap(dictNewtype))($79)));
                };
            };
        };
    };
};
var alaF = function (dictFunctor) {
    return function (dictFunctor1) {
        return function (dictNewtype) {
            return function (dictNewtype1) {
                return function (v) {
                    return function (f) {
                        return function ($80) {
                            return Data_Functor.map(dictFunctor1)(unwrap(dictNewtype1))(f(Data_Functor.map(dictFunctor)(wrap(dictNewtype))($80)));
                        };
                    };
                };
            };
        };
    };
};
var ala = function (dictFunctor) {
    return function (dictNewtype) {
        return function (dictNewtype1) {
            return function (v) {
                return function (f) {
                    return Data_Functor.map(dictFunctor)(unwrap(dictNewtype))(f(wrap(dictNewtype1)));
                };
            };
        };
    };
};
module.exports = {
    unwrap: unwrap,
    wrap: wrap,
    Newtype: Newtype,
    un: un,
    op: op,
    ala: ala,
    alaF: alaF,
    over: over,
    overF: overF,
    under: under,
    underF: underF,
    over2: over2,
    overF2: overF2,
    under2: under2,
    underF2: underF2,
    traverse: traverse,
    collect: collect,
    newtypeAdditive: newtypeAdditive,
    newtypeMultiplicative: newtypeMultiplicative,
    newtypeConj: newtypeConj,
    newtypeDisj: newtypeDisj,
    newtypeDual: newtypeDual,
    newtypeEndo: newtypeEndo,
    newtypeFirst: newtypeFirst,
    newtypeLast: newtypeLast
};

},{"../Control.Semigroupoid/index.js":56,"../Data.Function/index.js":91,"../Data.Functor/index.js":95,"../Data.Monoid.Additive/index.js":111,"../Data.Monoid.Conj/index.js":112,"../Data.Monoid.Disj/index.js":113,"../Data.Monoid.Dual/index.js":114,"../Data.Monoid.Endo/index.js":115,"../Data.Monoid.Multiplicative/index.js":116,"../Data.Semigroup.First/index.js":134,"../Data.Semigroup.Last/index.js":136,"../Prelude/index.js":198}],120:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Alt = require("../Control.Alt/index.js");
var Control_Alternative = require("../Control.Alternative/index.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Category = require("../Control.Category/index.js");
var Control_Plus = require("../Control.Plus/index.js");
var Control_Semigroupoid = require("../Control.Semigroupoid/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_FoldableWithIndex = require("../Data.FoldableWithIndex/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_FunctorWithIndex = require("../Data.FunctorWithIndex/index.js");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Ordering = require("../Data.Ordering/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Semigroup_Foldable = require("../Data.Semigroup.Foldable/index.js");
var Data_Show = require("../Data.Show/index.js");
var Data_Traversable = require("../Data.Traversable/index.js");
var Data_TraversableWithIndex = require("../Data.TraversableWithIndex/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var Data_Unfoldable = require("../Data.Unfoldable/index.js");
var Data_Unfoldable1 = require("../Data.Unfoldable1/index.js");
var Prelude = require("../Prelude/index.js");
var NonEmpty = (function () {
    function NonEmpty(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    NonEmpty.create = function (value0) {
        return function (value1) {
            return new NonEmpty(value0, value1);
        };
    };
    return NonEmpty;
})();
var unfoldable1NonEmpty = function (dictUnfoldable) {
    return new Data_Unfoldable1.Unfoldable1(function (f) {
        return function (b) {
            return Data_Tuple.uncurry(NonEmpty.create)(Data_Functor.map(Data_Tuple.functorTuple)(Data_Unfoldable.unfoldr(dictUnfoldable)(Data_Functor.map(Data_Maybe.functorMaybe)(f)))(f(b)));
        };
    });
};
var tail = function (v) {
    return v.value1;
};
var singleton = function (dictPlus) {
    return function (a) {
        return new NonEmpty(a, Control_Plus.empty(dictPlus));
    };
};
var showNonEmpty = function (dictShow) {
    return function (dictShow1) {
        return new Data_Show.Show(function (v) {
            return "(NonEmpty " + (Data_Show.show(dictShow)(v.value0) + (" " + (Data_Show.show(dictShow1)(v.value1) + ")")));
        });
    };
};
var oneOf = function (dictAlternative) {
    return function (v) {
        return Control_Alt.alt((dictAlternative.Plus1()).Alt0())(Control_Applicative.pure(dictAlternative.Applicative0())(v.value0))(v.value1);
    };
};
var head = function (v) {
    return v.value0;
};
var functorNonEmpty = function (dictFunctor) {
    return new Data_Functor.Functor(function (f) {
        return function (m) {
            return new NonEmpty(f(m.value0), Data_Functor.map(dictFunctor)(f)(m.value1));
        };
    });
};
var functorWithIndex = function (dictFunctorWithIndex) {
    return new Data_FunctorWithIndex.FunctorWithIndex(function () {
        return functorNonEmpty(dictFunctorWithIndex.Functor0());
    }, function (f) {
        return function (v) {
            return new NonEmpty(f(Data_Maybe.Nothing.value)(v.value0), Data_FunctorWithIndex.mapWithIndex(dictFunctorWithIndex)(function ($146) {
                return f(Data_Maybe.Just.create($146));
            })(v.value1));
        };
    });
};
var fromNonEmpty = function (f) {
    return function (v) {
        return f(v.value0)(v.value1);
    };
};
var foldl1 = function (dictFoldable) {
    return function (f) {
        return function (v) {
            return Data_Foldable.foldl(dictFoldable)(f)(v.value0)(v.value1);
        };
    };
};
var foldableNonEmpty = function (dictFoldable) {
    return new Data_Foldable.Foldable(function (dictMonoid) {
        return function (f) {
            return function (v) {
                return Data_Semigroup.append(dictMonoid.Semigroup0())(f(v.value0))(Data_Foldable.foldMap(dictFoldable)(dictMonoid)(f)(v.value1));
            };
        };
    }, function (f) {
        return function (b) {
            return function (v) {
                return Data_Foldable.foldl(dictFoldable)(f)(f(b)(v.value0))(v.value1);
            };
        };
    }, function (f) {
        return function (b) {
            return function (v) {
                return f(v.value0)(Data_Foldable.foldr(dictFoldable)(f)(b)(v.value1));
            };
        };
    });
};
var foldableWithIndexNonEmpty = function (dictFoldableWithIndex) {
    return new Data_FoldableWithIndex.FoldableWithIndex(function () {
        return foldableNonEmpty(dictFoldableWithIndex.Foldable0());
    }, function (dictMonoid) {
        return function (f) {
            return function (v) {
                return Data_Semigroup.append(dictMonoid.Semigroup0())(f(Data_Maybe.Nothing.value)(v.value0))(Data_FoldableWithIndex.foldMapWithIndex(dictFoldableWithIndex)(dictMonoid)(function ($147) {
                    return f(Data_Maybe.Just.create($147));
                })(v.value1));
            };
        };
    }, function (f) {
        return function (b) {
            return function (v) {
                return Data_FoldableWithIndex.foldlWithIndex(dictFoldableWithIndex)(function ($148) {
                    return f(Data_Maybe.Just.create($148));
                })(f(Data_Maybe.Nothing.value)(b)(v.value0))(v.value1);
            };
        };
    }, function (f) {
        return function (b) {
            return function (v) {
                return f(Data_Maybe.Nothing.value)(v.value0)(Data_FoldableWithIndex.foldrWithIndex(dictFoldableWithIndex)(function ($149) {
                    return f(Data_Maybe.Just.create($149));
                })(b)(v.value1));
            };
        };
    });
};
var traversableNonEmpty = function (dictTraversable) {
    return new Data_Traversable.Traversable(function () {
        return foldableNonEmpty(dictTraversable.Foldable1());
    }, function () {
        return functorNonEmpty(dictTraversable.Functor0());
    }, function (dictApplicative) {
        return function (v) {
            return Control_Apply.apply(dictApplicative.Apply0())(Data_Functor.map((dictApplicative.Apply0()).Functor0())(NonEmpty.create)(v.value0))(Data_Traversable.sequence(dictTraversable)(dictApplicative)(v.value1));
        };
    }, function (dictApplicative) {
        return function (f) {
            return function (v) {
                return Control_Apply.apply(dictApplicative.Apply0())(Data_Functor.map((dictApplicative.Apply0()).Functor0())(NonEmpty.create)(f(v.value0)))(Data_Traversable.traverse(dictTraversable)(dictApplicative)(f)(v.value1));
            };
        };
    });
};
var traversableWithIndexNonEmpty = function (dictTraversableWithIndex) {
    return new Data_TraversableWithIndex.TraversableWithIndex(function () {
        return foldableWithIndexNonEmpty(dictTraversableWithIndex.FoldableWithIndex1());
    }, function () {
        return functorWithIndex(dictTraversableWithIndex.FunctorWithIndex0());
    }, function () {
        return traversableNonEmpty(dictTraversableWithIndex.Traversable2());
    }, function (dictApplicative) {
        return function (f) {
            return function (v) {
                return Control_Apply.apply(dictApplicative.Apply0())(Data_Functor.map((dictApplicative.Apply0()).Functor0())(NonEmpty.create)(f(Data_Maybe.Nothing.value)(v.value0)))(Data_TraversableWithIndex.traverseWithIndex(dictTraversableWithIndex)(dictApplicative)(function ($150) {
                    return f(Data_Maybe.Just.create($150));
                })(v.value1));
            };
        };
    });
};
var foldable1NonEmpty = function (dictFoldable) {
    return new Data_Semigroup_Foldable.Foldable1(function () {
        return foldableNonEmpty(dictFoldable);
    }, function (dictSemigroup) {
        return Data_Semigroup_Foldable.foldMap1(foldable1NonEmpty(dictFoldable))(dictSemigroup)(Control_Category.identity(Control_Category.categoryFn));
    }, function (dictSemigroup) {
        return function (f) {
            return function (v) {
                return Data_Foldable.foldl(dictFoldable)(function (s) {
                    return function (a1) {
                        return Data_Semigroup.append(dictSemigroup)(s)(f(a1));
                    };
                })(f(v.value0))(v.value1);
            };
        };
    });
};
var eqNonEmpty = function (dictEq1) {
    return function (dictEq) {
        return new Data_Eq.Eq(function (x) {
            return function (y) {
                return Data_Eq.eq(dictEq)(x.value0)(y.value0) && Data_Eq.eq1(dictEq1)(dictEq)(x.value1)(y.value1);
            };
        });
    };
};
var ordNonEmpty = function (dictOrd1) {
    return function (dictOrd) {
        return new Data_Ord.Ord(function () {
            return eqNonEmpty(dictOrd1.Eq10())(dictOrd.Eq0());
        }, function (x) {
            return function (y) {
                var v = Data_Ord.compare(dictOrd)(x.value0)(y.value0);
                if (v instanceof Data_Ordering.LT) {
                    return Data_Ordering.LT.value;
                };
                if (v instanceof Data_Ordering.GT) {
                    return Data_Ordering.GT.value;
                };
                return Data_Ord.compare1(dictOrd1)(dictOrd)(x.value1)(y.value1);
            };
        });
    };
};
var eq1NonEmpty = function (dictEq1) {
    return new Data_Eq.Eq1(function (dictEq) {
        return Data_Eq.eq(eqNonEmpty(dictEq1)(dictEq));
    });
};
var ord1NonEmpty = function (dictOrd1) {
    return new Data_Ord.Ord1(function () {
        return eq1NonEmpty(dictOrd1.Eq10());
    }, function (dictOrd) {
        return Data_Ord.compare(ordNonEmpty(dictOrd1)(dictOrd));
    });
};
module.exports = {
    NonEmpty: NonEmpty,
    singleton: singleton,
    foldl1: foldl1,
    fromNonEmpty: fromNonEmpty,
    oneOf: oneOf,
    head: head,
    tail: tail,
    showNonEmpty: showNonEmpty,
    eqNonEmpty: eqNonEmpty,
    eq1NonEmpty: eq1NonEmpty,
    ordNonEmpty: ordNonEmpty,
    ord1NonEmpty: ord1NonEmpty,
    functorNonEmpty: functorNonEmpty,
    functorWithIndex: functorWithIndex,
    foldableNonEmpty: foldableNonEmpty,
    foldableWithIndexNonEmpty: foldableWithIndexNonEmpty,
    traversableNonEmpty: traversableNonEmpty,
    traversableWithIndexNonEmpty: traversableWithIndexNonEmpty,
    foldable1NonEmpty: foldable1NonEmpty,
    unfoldable1NonEmpty: unfoldable1NonEmpty
};

},{"../Control.Alt/index.js":25,"../Control.Alternative/index.js":26,"../Control.Applicative/index.js":27,"../Control.Apply/index.js":29,"../Control.Category/index.js":34,"../Control.Plus/index.js":55,"../Control.Semigroupoid/index.js":56,"../Data.Eq/index.js":82,"../Data.Foldable/index.js":87,"../Data.FoldableWithIndex/index.js":88,"../Data.Function/index.js":91,"../Data.Functor/index.js":95,"../Data.FunctorWithIndex/index.js":97,"../Data.HeytingAlgebra/index.js":99,"../Data.Maybe/index.js":110,"../Data.Ord/index.js":130,"../Data.Ordering/index.js":131,"../Data.Semigroup.Foldable/index.js":135,"../Data.Semigroup/index.js":139,"../Data.Show/index.js":143,"../Data.Traversable/index.js":154,"../Data.TraversableWithIndex/index.js":155,"../Data.Tuple/index.js":156,"../Data.Unfoldable/index.js":158,"../Data.Unfoldable1/index.js":160,"../Prelude/index.js":198}],121:[function(require,module,exports){
"use strict";

exports["null"] = null;

exports.nullable = function (a, r, f) {
  return a == null ? r : f(a);
};

exports.notNull = function (x) {
  return x;
};

},{}],122:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var $foreign = require("./foreign.js");
var Control_Semigroupoid = require("../Control.Semigroupoid/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Function_Uncurried = require("../Data.Function.Uncurried/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Show = require("../Data.Show/index.js");
var Prelude = require("../Prelude/index.js");
var toNullable = Data_Maybe.maybe($foreign["null"])($foreign.notNull);
var toMaybe = function (n) {
    return $foreign.nullable(n, Data_Maybe.Nothing.value, Data_Maybe.Just.create);
};
var showNullable = function (dictShow) {
    return new Data_Show.Show(function ($5) {
        return Data_Maybe.maybe("null")(Data_Show.show(dictShow))(toMaybe($5));
    });
};
var eqNullable = function (dictEq) {
    return new Data_Eq.Eq(Data_Function.on(Data_Eq.eq(Data_Maybe.eqMaybe(dictEq)))(toMaybe));
};
var ordNullable = function (dictOrd) {
    return new Data_Ord.Ord(function () {
        return eqNullable(dictOrd.Eq0());
    }, Data_Function.on(Data_Ord.compare(Data_Maybe.ordMaybe(dictOrd)))(toMaybe));
};
var eq1Nullable = new Data_Eq.Eq1(function (dictEq) {
    return Data_Eq.eq(eqNullable(dictEq));
});
var ord1Nullable = new Data_Ord.Ord1(function () {
    return eq1Nullable;
}, function (dictOrd) {
    return Data_Ord.compare(ordNullable(dictOrd));
});
module.exports = {
    toMaybe: toMaybe,
    toNullable: toNullable,
    showNullable: showNullable,
    eqNullable: eqNullable,
    eq1Nullable: eq1Nullable,
    ordNullable: ordNullable,
    ord1Nullable: ord1Nullable,
    "null": $foreign["null"],
    notNull: $foreign.notNull
};

},{"../Control.Semigroupoid/index.js":56,"../Data.Eq/index.js":82,"../Data.Function.Uncurried/index.js":90,"../Data.Function/index.js":91,"../Data.Maybe/index.js":110,"../Data.Ord/index.js":130,"../Data.Show/index.js":143,"../Prelude/index.js":198,"./foreign.js":121}],123:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Category = require("../Control.Category/index.js");
var Control_Semigroupoid = require("../Control.Semigroupoid/index.js");
var Data_Functor_Contravariant = require("../Data.Functor.Contravariant/index.js");
var Data_Monoid = require("../Data.Monoid/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Prelude = require("../Prelude/index.js");
var Op = function (x) {
    return x;
};
var semigroupoidOp = new Control_Semigroupoid.Semigroupoid(function (v) {
    return function (v1) {
        return function ($12) {
            return v1(v($12));
        };
    };
});
var semigroupOp = function (dictSemigroup) {
    return Data_Semigroup.semigroupFn(dictSemigroup);
};
var newtypeOp = new Data_Newtype.Newtype(function (n) {
    return n;
}, Op);
var monoidOp = function (dictMonoid) {
    return Data_Monoid.monoidFn(dictMonoid);
};
var contravariantOp = new Data_Functor_Contravariant.Contravariant(function (f) {
    return function (v) {
        return function ($13) {
            return v(f($13));
        };
    };
});
var categoryOp = new Control_Category.Category(function () {
    return semigroupoidOp;
}, Control_Category.identity(Control_Category.categoryFn));
module.exports = {
    Op: Op,
    newtypeOp: newtypeOp,
    semigroupOp: semigroupOp,
    monoidOp: monoidOp,
    semigroupoidOp: semigroupoidOp,
    categoryOp: categoryOp,
    contravariantOp: contravariantOp
};

},{"../Control.Category/index.js":34,"../Control.Semigroupoid/index.js":56,"../Data.Functor.Contravariant/index.js":92,"../Data.Monoid/index.js":117,"../Data.Newtype/index.js":119,"../Data.Semigroup/index.js":139,"../Prelude/index.js":198}],124:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Semigroupoid = require("../Control.Semigroupoid/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Monoid = require("../Data.Monoid/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Op = require("../Data.Op/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var Foreign = require("../Foreign/index.js");
var Foreign_Object = require("../Foreign.Object/index.js");
var Prelude = require("../Prelude/index.js");
var Options = function (x) {
    return x;
};
var semigroupOptions = Data_Semigroup.semigroupArray;
var options = function (v) {
    return Foreign.unsafeToForeign(Foreign_Object.fromFoldable(Data_Foldable.foldableArray)(v));
};
var newtypeOptions = new Data_Newtype.Newtype(function (n) {
    return n;
}, Options);
var monoidOptions = Data_Monoid.monoidArray;
var defaultToOptions = function (k) {
    return function (v) {
        return [ new Data_Tuple.Tuple(k, Foreign.unsafeToForeign(v)) ];
    };
};
var opt = function ($7) {
    return Data_Op.Op(defaultToOptions($7));
};
var assoc = Data_Newtype.unwrap(Data_Op.newtypeOp);
var optional = function (option) {
    return Data_Op.Op(Data_Maybe.maybe(Data_Monoid.mempty(monoidOptions))(function (v) {
        return assoc(option)(v);
    }));
};
var tag = function (o) {
    return function (value) {
        return function (v) {
            return assoc(o)(value);
        };
    };
};
module.exports = {
    Options: Options,
    options: options,
    assoc: assoc,
    optional: optional,
    opt: opt,
    tag: tag,
    defaultToOptions: defaultToOptions,
    newtypeOptions: newtypeOptions,
    semigroupOptions: semigroupOptions,
    monoidOptions: monoidOptions
};

},{"../Control.Semigroupoid/index.js":56,"../Data.Foldable/index.js":87,"../Data.Function/index.js":91,"../Data.Maybe/index.js":110,"../Data.Monoid/index.js":117,"../Data.Newtype/index.js":119,"../Data.Op/index.js":123,"../Data.Semigroup/index.js":139,"../Data.Tuple/index.js":156,"../Foreign.Object/index.js":176,"../Foreign/index.js":178,"../Prelude/index.js":198}],125:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Data_Bounded = require("../Data.Bounded/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Monoid = require("../Data.Monoid/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Show = require("../Data.Show/index.js");
var Prelude = require("../Prelude/index.js");
var Max = function (x) {
    return x;
};
var showMax = function (dictShow) {
    return new Data_Show.Show(function (v) {
        return "(Max " + (Data_Show.show(dictShow)(v) + ")");
    });
};
var semigroupMax = function (dictOrd) {
    return new Data_Semigroup.Semigroup(function (v) {
        return function (v1) {
            return Data_Ord.max(dictOrd)(v)(v1);
        };
    });
};
var newtypeMax = new Data_Newtype.Newtype(function (n) {
    return n;
}, Max);
var monoidMax = function (dictBounded) {
    return new Data_Monoid.Monoid(function () {
        return semigroupMax(dictBounded.Ord0());
    }, Data_Bounded.bottom(dictBounded));
};
var eqMax = function (dictEq) {
    return dictEq;
};
var ordMax = function (dictOrd) {
    return new Data_Ord.Ord(function () {
        return eqMax(dictOrd.Eq0());
    }, function (v) {
        return function (v1) {
            return Data_Ord.compare(dictOrd)(v)(v1);
        };
    });
};
module.exports = {
    Max: Max,
    newtypeMax: newtypeMax,
    eqMax: eqMax,
    ordMax: ordMax,
    semigroupMax: semigroupMax,
    monoidMax: monoidMax,
    showMax: showMax
};

},{"../Data.Bounded/index.js":76,"../Data.Eq/index.js":82,"../Data.Monoid/index.js":117,"../Data.Newtype/index.js":119,"../Data.Ord/index.js":130,"../Data.Semigroup/index.js":139,"../Data.Show/index.js":143,"../Prelude/index.js":198}],126:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Data_Bounded = require("../Data.Bounded/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Monoid = require("../Data.Monoid/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Show = require("../Data.Show/index.js");
var Prelude = require("../Prelude/index.js");
var Min = function (x) {
    return x;
};
var showMin = function (dictShow) {
    return new Data_Show.Show(function (v) {
        return "(Min " + (Data_Show.show(dictShow)(v) + ")");
    });
};
var semigroupMin = function (dictOrd) {
    return new Data_Semigroup.Semigroup(function (v) {
        return function (v1) {
            return Data_Ord.min(dictOrd)(v)(v1);
        };
    });
};
var newtypeMin = new Data_Newtype.Newtype(function (n) {
    return n;
}, Min);
var monoidMin = function (dictBounded) {
    return new Data_Monoid.Monoid(function () {
        return semigroupMin(dictBounded.Ord0());
    }, Data_Bounded.top(dictBounded));
};
var eqMin = function (dictEq) {
    return dictEq;
};
var ordMin = function (dictOrd) {
    return new Data_Ord.Ord(function () {
        return eqMin(dictOrd.Eq0());
    }, function (v) {
        return function (v1) {
            return Data_Ord.compare(dictOrd)(v)(v1);
        };
    });
};
module.exports = {
    Min: Min,
    newtypeMin: newtypeMin,
    eqMin: eqMin,
    ordMin: ordMin,
    semigroupMin: semigroupMin,
    monoidMin: monoidMin,
    showMin: showMin
};

},{"../Data.Bounded/index.js":76,"../Data.Eq/index.js":82,"../Data.Monoid/index.js":117,"../Data.Newtype/index.js":119,"../Data.Ord/index.js":130,"../Data.Semigroup/index.js":139,"../Data.Show/index.js":143,"../Prelude/index.js":198}],127:[function(require,module,exports){
"use strict";

exports.unsafeCompareImpl = function (lt) {
  return function (eq) {
    return function (gt) {
      return function (x) {
        return function (y) {
          return x < y ? lt : x === y ? eq : gt;
        };
      };
    };
  };
};

},{}],128:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var $foreign = require("./foreign.js");
var Data_Ordering = require("../Data.Ordering/index.js");
var unsafeCompare = $foreign.unsafeCompareImpl(Data_Ordering.LT.value)(Data_Ordering.EQ.value)(Data_Ordering.GT.value);
module.exports = {
    unsafeCompare: unsafeCompare
};

},{"../Data.Ordering/index.js":131,"./foreign.js":127}],129:[function(require,module,exports){
"use strict";

exports.ordArrayImpl = function (f) {
  return function (xs) {
    return function (ys) {
      var i = 0;
      var xlen = xs.length;
      var ylen = ys.length;
      while (i < xlen && i < ylen) {
        var x = xs[i];
        var y = ys[i];
        var o = f(x)(y);
        if (o !== 0) {
          return o;
        }
        i++;
      }
      if (xlen === ylen) {
        return 0;
      } else if (xlen > ylen) {
        return -1;
      } else {
        return 1;
      }
    };
  };
};

},{}],130:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var $foreign = require("./foreign.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Ord_Unsafe = require("../Data.Ord.Unsafe/index.js");
var Data_Ordering = require("../Data.Ordering/index.js");
var Data_Ring = require("../Data.Ring/index.js");
var Data_Semiring = require("../Data.Semiring/index.js");
var Data_Symbol = require("../Data.Symbol/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Data_Void = require("../Data.Void/index.js");
var Record_Unsafe = require("../Record.Unsafe/index.js");
var Type_Data_RowList = require("../Type.Data.RowList/index.js");
var Ord = function (Eq0, compare) {
    this.Eq0 = Eq0;
    this.compare = compare;
};
var Ord1 = function (Eq10, compare1) {
    this.Eq10 = Eq10;
    this.compare1 = compare1;
};
var OrdRecord = function (EqRecord0, compareRecord) {
    this.EqRecord0 = EqRecord0;
    this.compareRecord = compareRecord;
};
var ordVoid = new Ord(function () {
    return Data_Eq.eqVoid;
}, function (v) {
    return function (v1) {
        return Data_Ordering.EQ.value;
    };
});
var ordUnit = new Ord(function () {
    return Data_Eq.eqUnit;
}, function (v) {
    return function (v1) {
        return Data_Ordering.EQ.value;
    };
});
var ordString = new Ord(function () {
    return Data_Eq.eqString;
}, Data_Ord_Unsafe.unsafeCompare);
var ordRecordNil = new OrdRecord(function () {
    return Data_Eq.eqRowNil;
}, function (v) {
    return function (v1) {
        return function (v2) {
            return Data_Ordering.EQ.value;
        };
    };
});
var ordOrdering = new Ord(function () {
    return Data_Ordering.eqOrdering;
}, function (v) {
    return function (v1) {
        if (v instanceof Data_Ordering.LT && v1 instanceof Data_Ordering.LT) {
            return Data_Ordering.EQ.value;
        };
        if (v instanceof Data_Ordering.EQ && v1 instanceof Data_Ordering.EQ) {
            return Data_Ordering.EQ.value;
        };
        if (v instanceof Data_Ordering.GT && v1 instanceof Data_Ordering.GT) {
            return Data_Ordering.EQ.value;
        };
        if (v instanceof Data_Ordering.LT) {
            return Data_Ordering.LT.value;
        };
        if (v instanceof Data_Ordering.EQ && v1 instanceof Data_Ordering.LT) {
            return Data_Ordering.GT.value;
        };
        if (v instanceof Data_Ordering.EQ && v1 instanceof Data_Ordering.GT) {
            return Data_Ordering.LT.value;
        };
        if (v instanceof Data_Ordering.GT) {
            return Data_Ordering.GT.value;
        };
        throw new Error("Failed pattern match at Data.Ord (line 73, column 1 - line 73, column 37): " + [ v.constructor.name, v1.constructor.name ]);
    };
});
var ordNumber = new Ord(function () {
    return Data_Eq.eqNumber;
}, Data_Ord_Unsafe.unsafeCompare);
var ordInt = new Ord(function () {
    return Data_Eq.eqInt;
}, Data_Ord_Unsafe.unsafeCompare);
var ordChar = new Ord(function () {
    return Data_Eq.eqChar;
}, Data_Ord_Unsafe.unsafeCompare);
var ordBoolean = new Ord(function () {
    return Data_Eq.eqBoolean;
}, Data_Ord_Unsafe.unsafeCompare);
var compareRecord = function (dict) {
    return dict.compareRecord;
};
var ordRecord = function (dictRowToList) {
    return function (dictOrdRecord) {
        return new Ord(function () {
            return Data_Eq.eqRec(dictRowToList)(dictOrdRecord.EqRecord0());
        }, compareRecord(dictOrdRecord)(Type_Data_RowList.RLProxy.value));
    };
};
var compare1 = function (dict) {
    return dict.compare1;
};
var compare = function (dict) {
    return dict.compare;
};
var comparing = function (dictOrd) {
    return function (f) {
        return function (x) {
            return function (y) {
                return compare(dictOrd)(f(x))(f(y));
            };
        };
    };
};
var greaterThan = function (dictOrd) {
    return function (a1) {
        return function (a2) {
            var v = compare(dictOrd)(a1)(a2);
            if (v instanceof Data_Ordering.GT) {
                return true;
            };
            return false;
        };
    };
};
var greaterThanOrEq = function (dictOrd) {
    return function (a1) {
        return function (a2) {
            var v = compare(dictOrd)(a1)(a2);
            if (v instanceof Data_Ordering.LT) {
                return false;
            };
            return true;
        };
    };
};
var signum = function (dictOrd) {
    return function (dictRing) {
        return function (x) {
            var $43 = greaterThanOrEq(dictOrd)(x)(Data_Semiring.zero(dictRing.Semiring0()));
            if ($43) {
                return Data_Semiring.one(dictRing.Semiring0());
            };
            return Data_Ring.negate(dictRing)(Data_Semiring.one(dictRing.Semiring0()));
        };
    };
};
var lessThan = function (dictOrd) {
    return function (a1) {
        return function (a2) {
            var v = compare(dictOrd)(a1)(a2);
            if (v instanceof Data_Ordering.LT) {
                return true;
            };
            return false;
        };
    };
};
var lessThanOrEq = function (dictOrd) {
    return function (a1) {
        return function (a2) {
            var v = compare(dictOrd)(a1)(a2);
            if (v instanceof Data_Ordering.GT) {
                return false;
            };
            return true;
        };
    };
};
var max = function (dictOrd) {
    return function (x) {
        return function (y) {
            var v = compare(dictOrd)(x)(y);
            if (v instanceof Data_Ordering.LT) {
                return y;
            };
            if (v instanceof Data_Ordering.EQ) {
                return x;
            };
            if (v instanceof Data_Ordering.GT) {
                return x;
            };
            throw new Error("Failed pattern match at Data.Ord (line 128, column 3 - line 131, column 12): " + [ v.constructor.name ]);
        };
    };
};
var min = function (dictOrd) {
    return function (x) {
        return function (y) {
            var v = compare(dictOrd)(x)(y);
            if (v instanceof Data_Ordering.LT) {
                return x;
            };
            if (v instanceof Data_Ordering.EQ) {
                return x;
            };
            if (v instanceof Data_Ordering.GT) {
                return y;
            };
            throw new Error("Failed pattern match at Data.Ord (line 119, column 3 - line 122, column 12): " + [ v.constructor.name ]);
        };
    };
};
var ordArray = function (dictOrd) {
    return new Ord(function () {
        return Data_Eq.eqArray(dictOrd.Eq0());
    }, (function () {
        var toDelta = function (x) {
            return function (y) {
                var v = compare(dictOrd)(x)(y);
                if (v instanceof Data_Ordering.EQ) {
                    return 0;
                };
                if (v instanceof Data_Ordering.LT) {
                    return 1;
                };
                if (v instanceof Data_Ordering.GT) {
                    return -1 | 0;
                };
                throw new Error("Failed pattern match at Data.Ord (line 66, column 7 - line 71, column 1): " + [ v.constructor.name ]);
            };
        };
        return function (xs) {
            return function (ys) {
                return compare(ordInt)(0)($foreign.ordArrayImpl(toDelta)(xs)(ys));
            };
        };
    })());
};
var ord1Array = new Ord1(function () {
    return Data_Eq.eq1Array;
}, function (dictOrd) {
    return compare(ordArray(dictOrd));
});
var ordRecordCons = function (dictOrdRecord) {
    return function (dictCons) {
        return function (dictIsSymbol) {
            return function (dictOrd) {
                return new OrdRecord(function () {
                    return Data_Eq.eqRowCons(dictOrdRecord.EqRecord0())(dictCons)(dictIsSymbol)(dictOrd.Eq0());
                }, function (v) {
                    return function (ra) {
                        return function (rb) {
                            var key = Data_Symbol.reflectSymbol(dictIsSymbol)(Data_Symbol.SProxy.value);
                            var left = compare(dictOrd)(Record_Unsafe.unsafeGet(key)(ra))(Record_Unsafe.unsafeGet(key)(rb));
                            var $49 = Data_Eq.notEq(Data_Ordering.eqOrdering)(left)(Data_Ordering.EQ.value);
                            if ($49) {
                                return left;
                            };
                            return compareRecord(dictOrdRecord)(Type_Data_RowList.RLProxy.value)(ra)(rb);
                        };
                    };
                });
            };
        };
    };
};
var clamp = function (dictOrd) {
    return function (low) {
        return function (hi) {
            return function (x) {
                return min(dictOrd)(hi)(max(dictOrd)(low)(x));
            };
        };
    };
};
var between = function (dictOrd) {
    return function (low) {
        return function (hi) {
            return function (x) {
                if (lessThan(dictOrd)(x)(low)) {
                    return false;
                };
                if (greaterThan(dictOrd)(x)(hi)) {
                    return false;
                };
                return true;
            };
        };
    };
};
var abs = function (dictOrd) {
    return function (dictRing) {
        return function (x) {
            var $53 = greaterThanOrEq(dictOrd)(x)(Data_Semiring.zero(dictRing.Semiring0()));
            if ($53) {
                return x;
            };
            return Data_Ring.negate(dictRing)(x);
        };
    };
};
module.exports = {
    Ord: Ord,
    compare: compare,
    Ord1: Ord1,
    compare1: compare1,
    lessThan: lessThan,
    lessThanOrEq: lessThanOrEq,
    greaterThan: greaterThan,
    greaterThanOrEq: greaterThanOrEq,
    comparing: comparing,
    min: min,
    max: max,
    clamp: clamp,
    between: between,
    abs: abs,
    signum: signum,
    OrdRecord: OrdRecord,
    compareRecord: compareRecord,
    ordBoolean: ordBoolean,
    ordInt: ordInt,
    ordNumber: ordNumber,
    ordString: ordString,
    ordChar: ordChar,
    ordUnit: ordUnit,
    ordVoid: ordVoid,
    ordArray: ordArray,
    ordOrdering: ordOrdering,
    ord1Array: ord1Array,
    ordRecordNil: ordRecordNil,
    ordRecordCons: ordRecordCons,
    ordRecord: ordRecord
};

},{"../Data.Eq/index.js":82,"../Data.Ord.Unsafe/index.js":128,"../Data.Ordering/index.js":131,"../Data.Ring/index.js":133,"../Data.Semiring/index.js":141,"../Data.Symbol/index.js":150,"../Data.Unit/index.js":162,"../Data.Void/index.js":163,"../Record.Unsafe/index.js":200,"../Type.Data.RowList/index.js":204,"./foreign.js":129}],131:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Data_Eq = require("../Data.Eq/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Show = require("../Data.Show/index.js");
var LT = (function () {
    function LT() {

    };
    LT.value = new LT();
    return LT;
})();
var GT = (function () {
    function GT() {

    };
    GT.value = new GT();
    return GT;
})();
var EQ = (function () {
    function EQ() {

    };
    EQ.value = new EQ();
    return EQ;
})();
var showOrdering = new Data_Show.Show(function (v) {
    if (v instanceof LT) {
        return "LT";
    };
    if (v instanceof GT) {
        return "GT";
    };
    if (v instanceof EQ) {
        return "EQ";
    };
    throw new Error("Failed pattern match at Data.Ordering (line 26, column 1 - line 26, column 39): " + [ v.constructor.name ]);
});
var semigroupOrdering = new Data_Semigroup.Semigroup(function (v) {
    return function (v1) {
        if (v instanceof LT) {
            return LT.value;
        };
        if (v instanceof GT) {
            return GT.value;
        };
        if (v instanceof EQ) {
            return v1;
        };
        throw new Error("Failed pattern match at Data.Ordering (line 21, column 1 - line 21, column 49): " + [ v.constructor.name, v1.constructor.name ]);
    };
});
var invert = function (v) {
    if (v instanceof GT) {
        return LT.value;
    };
    if (v instanceof EQ) {
        return EQ.value;
    };
    if (v instanceof LT) {
        return GT.value;
    };
    throw new Error("Failed pattern match at Data.Ordering (line 33, column 1 - line 33, column 31): " + [ v.constructor.name ]);
};
var eqOrdering = new Data_Eq.Eq(function (v) {
    return function (v1) {
        if (v instanceof LT && v1 instanceof LT) {
            return true;
        };
        if (v instanceof GT && v1 instanceof GT) {
            return true;
        };
        if (v instanceof EQ && v1 instanceof EQ) {
            return true;
        };
        return false;
    };
});
module.exports = {
    LT: LT,
    GT: GT,
    EQ: EQ,
    invert: invert,
    eqOrdering: eqOrdering,
    semigroupOrdering: semigroupOrdering,
    showOrdering: showOrdering
};

},{"../Data.Eq/index.js":82,"../Data.Semigroup/index.js":139,"../Data.Show/index.js":143}],132:[function(require,module,exports){
"use strict";

exports.intSub = function (x) {
  return function (y) {
    /* jshint bitwise: false */
    return x - y | 0;
  };
};

exports.numSub = function (n1) {
  return function (n2) {
    return n1 - n2;
  };
};

},{}],133:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var $foreign = require("./foreign.js");
var Data_Semiring = require("../Data.Semiring/index.js");
var Data_Symbol = require("../Data.Symbol/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Record_Unsafe = require("../Record.Unsafe/index.js");
var Type_Data_RowList = require("../Type.Data.RowList/index.js");
var Ring = function (Semiring0, sub) {
    this.Semiring0 = Semiring0;
    this.sub = sub;
};
var RingRecord = function (SemiringRecord0, subRecord) {
    this.SemiringRecord0 = SemiringRecord0;
    this.subRecord = subRecord;
};
var subRecord = function (dict) {
    return dict.subRecord;
};
var sub = function (dict) {
    return dict.sub;
};
var ringUnit = new Ring(function () {
    return Data_Semiring.semiringUnit;
}, function (v) {
    return function (v1) {
        return Data_Unit.unit;
    };
});
var ringRecordNil = new RingRecord(function () {
    return Data_Semiring.semiringRecordNil;
}, function (v) {
    return function (v1) {
        return function (v2) {
            return {};
        };
    };
});
var ringRecordCons = function (dictIsSymbol) {
    return function (dictCons) {
        return function (dictRingRecord) {
            return function (dictRing) {
                return new RingRecord(function () {
                    return Data_Semiring.semiringRecordCons(dictIsSymbol)(dictCons)(dictRingRecord.SemiringRecord0())(dictRing.Semiring0());
                }, function (v) {
                    return function (ra) {
                        return function (rb) {
                            var tail = subRecord(dictRingRecord)(Type_Data_RowList.RLProxy.value)(ra)(rb);
                            var key = Data_Symbol.reflectSymbol(dictIsSymbol)(Data_Symbol.SProxy.value);
                            var insert = Record_Unsafe.unsafeSet(key);
                            var get = Record_Unsafe.unsafeGet(key);
                            return insert(sub(dictRing)(get(ra))(get(rb)))(tail);
                        };
                    };
                });
            };
        };
    };
};
var ringRecord = function (dictRowToList) {
    return function (dictRingRecord) {
        return new Ring(function () {
            return Data_Semiring.semiringRecord(dictRowToList)(dictRingRecord.SemiringRecord0());
        }, subRecord(dictRingRecord)(Type_Data_RowList.RLProxy.value));
    };
};
var ringNumber = new Ring(function () {
    return Data_Semiring.semiringNumber;
}, $foreign.numSub);
var ringInt = new Ring(function () {
    return Data_Semiring.semiringInt;
}, $foreign.intSub);
var ringFn = function (dictRing) {
    return new Ring(function () {
        return Data_Semiring.semiringFn(dictRing.Semiring0());
    }, function (f) {
        return function (g) {
            return function (x) {
                return sub(dictRing)(f(x))(g(x));
            };
        };
    });
};
var negate = function (dictRing) {
    return function (a) {
        return sub(dictRing)(Data_Semiring.zero(dictRing.Semiring0()))(a);
    };
};
module.exports = {
    Ring: Ring,
    sub: sub,
    negate: negate,
    RingRecord: RingRecord,
    subRecord: subRecord,
    ringInt: ringInt,
    ringNumber: ringNumber,
    ringUnit: ringUnit,
    ringFn: ringFn,
    ringRecord: ringRecord,
    ringRecordNil: ringRecordNil,
    ringRecordCons: ringRecordCons
};

},{"../Data.Semiring/index.js":141,"../Data.Symbol/index.js":150,"../Data.Unit/index.js":162,"../Record.Unsafe/index.js":200,"../Type.Data.RowList/index.js":204,"./foreign.js":132}],134:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Monad = require("../Control.Monad/index.js");
var Data_Bounded = require("../Data.Bounded/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Show = require("../Data.Show/index.js");
var Prelude = require("../Prelude/index.js");
var First = function (x) {
    return x;
};
var showFirst = function (dictShow) {
    return new Data_Show.Show(function (v) {
        return "(First " + (Data_Show.show(dictShow)(v) + ")");
    });
};
var semigroupFirst = new Data_Semigroup.Semigroup(function (x) {
    return function (v) {
        return x;
    };
});
var ordFirst = function (dictOrd) {
    return dictOrd;
};
var functorFirst = new Data_Functor.Functor(function (f) {
    return function (m) {
        return f(m);
    };
});
var eqFirst = function (dictEq) {
    return dictEq;
};
var eq1First = new Data_Eq.Eq1(function (dictEq) {
    return Data_Eq.eq(eqFirst(dictEq));
});
var ord1First = new Data_Ord.Ord1(function () {
    return eq1First;
}, function (dictOrd) {
    return Data_Ord.compare(ordFirst(dictOrd));
});
var boundedFirst = function (dictBounded) {
    return dictBounded;
};
var applyFirst = new Control_Apply.Apply(function () {
    return functorFirst;
}, function (v) {
    return function (v1) {
        return v(v1);
    };
});
var bindFirst = new Control_Bind.Bind(function () {
    return applyFirst;
}, function (v) {
    return function (f) {
        return f(v);
    };
});
var applicativeFirst = new Control_Applicative.Applicative(function () {
    return applyFirst;
}, First);
var monadFirst = new Control_Monad.Monad(function () {
    return applicativeFirst;
}, function () {
    return bindFirst;
});
module.exports = {
    First: First,
    eqFirst: eqFirst,
    eq1First: eq1First,
    ordFirst: ordFirst,
    ord1First: ord1First,
    boundedFirst: boundedFirst,
    showFirst: showFirst,
    functorFirst: functorFirst,
    applyFirst: applyFirst,
    applicativeFirst: applicativeFirst,
    bindFirst: bindFirst,
    monadFirst: monadFirst,
    semigroupFirst: semigroupFirst
};

},{"../Control.Applicative/index.js":27,"../Control.Apply/index.js":29,"../Control.Bind/index.js":33,"../Control.Monad/index.js":52,"../Data.Bounded/index.js":76,"../Data.Eq/index.js":82,"../Data.Functor/index.js":95,"../Data.Ord/index.js":130,"../Data.Semigroup/index.js":139,"../Data.Show/index.js":143,"../Prelude/index.js":198}],135:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Apply = require("../Control.Apply/index.js");
var Control_Category = require("../Control.Category/index.js");
var Control_Semigroupoid = require("../Control.Semigroupoid/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Monoid_Dual = require("../Data.Monoid.Dual/index.js");
var Data_Monoid_Multiplicative = require("../Data.Monoid.Multiplicative/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Ord_Max = require("../Data.Ord.Max/index.js");
var Data_Ord_Min = require("../Data.Ord.Min/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Prelude = require("../Prelude/index.js");
var JoinWith = function (x) {
    return x;
};
var Act = function (x) {
    return x;
};
var Foldable1 = function (Foldable0, fold1, foldMap1) {
    this.Foldable0 = Foldable0;
    this.fold1 = fold1;
    this.foldMap1 = foldMap1;
};
var semigroupJoinWith = function (dictSemigroup) {
    return new Data_Semigroup.Semigroup(function (v) {
        return function (v1) {
            return JoinWith(function (j) {
                return Data_Semigroup.append(dictSemigroup)(v(j))(Data_Semigroup.append(dictSemigroup)(j)(v1(j)));
            });
        };
    });
};
var semigroupAct = function (dictApply) {
    return new Data_Semigroup.Semigroup(function (v) {
        return function (v1) {
            return Control_Apply.applySecond(dictApply)(v)(v1);
        };
    });
};
var joinee = function (v) {
    return v;
};
var getAct = function (v) {
    return v;
};
var foldMap1 = function (dict) {
    return dict.foldMap1;
};
var intercalateMap = function (dictFoldable1) {
    return function (dictSemigroup) {
        return function (j) {
            return function (f) {
                return function (foldable) {
                    return joinee(foldMap1(dictFoldable1)(semigroupJoinWith(dictSemigroup))(function ($43) {
                        return JoinWith(Data_Function["const"](f($43)));
                    })(foldable))(j);
                };
            };
        };
    };
};
var intercalate = function (dictFoldable1) {
    return function (dictSemigroup) {
        return Data_Function.flip(intercalateMap(dictFoldable1)(dictSemigroup))(Control_Category.identity(Control_Category.categoryFn));
    };
};
var maximum = function (dictOrd) {
    return function (dictFoldable1) {
        return Data_Newtype.ala(Data_Functor.functorFn)(Data_Ord_Max.newtypeMax)(Data_Ord_Max.newtypeMax)(Data_Ord_Max.Max)(foldMap1(dictFoldable1)(Data_Ord_Max.semigroupMax(dictOrd)));
    };
};
var minimum = function (dictOrd) {
    return function (dictFoldable1) {
        return Data_Newtype.ala(Data_Functor.functorFn)(Data_Ord_Min.newtypeMin)(Data_Ord_Min.newtypeMin)(Data_Ord_Min.Min)(foldMap1(dictFoldable1)(Data_Ord_Min.semigroupMin(dictOrd)));
    };
};
var traverse1_ = function (dictFoldable1) {
    return function (dictApply) {
        return function (f) {
            return function (t) {
                return Data_Functor.voidRight(dictApply.Functor0())(Data_Unit.unit)(getAct(foldMap1(dictFoldable1)(semigroupAct(dictApply))(function ($44) {
                    return Act(f($44));
                })(t)));
            };
        };
    };
};
var for1_ = function (dictFoldable1) {
    return function (dictApply) {
        return Data_Function.flip(traverse1_(dictFoldable1)(dictApply));
    };
};
var sequence1_ = function (dictFoldable1) {
    return function (dictApply) {
        return traverse1_(dictFoldable1)(dictApply)(Control_Category.identity(Control_Category.categoryFn));
    };
};
var fold1Default = function (dictFoldable1) {
    return function (dictSemigroup) {
        return foldMap1(dictFoldable1)(dictSemigroup)(Control_Category.identity(Control_Category.categoryFn));
    };
};
var foldableDual = new Foldable1(function () {
    return Data_Foldable.foldableDual;
}, function (dictSemigroup) {
    return fold1Default(foldableDual)(dictSemigroup);
}, function (dictSemigroup) {
    return function (f) {
        return function (v) {
            return f(v);
        };
    };
});
var foldableMultiplicative = new Foldable1(function () {
    return Data_Foldable.foldableMultiplicative;
}, function (dictSemigroup) {
    return fold1Default(foldableMultiplicative)(dictSemigroup);
}, function (dictSemigroup) {
    return function (f) {
        return function (v) {
            return f(v);
        };
    };
});
var fold1 = function (dict) {
    return dict.fold1;
};
var foldMap1Default = function (dictFoldable1) {
    return function (dictFunctor) {
        return function (dictSemigroup) {
            return function (f) {
                return function ($45) {
                    return fold1(dictFoldable1)(dictSemigroup)(Data_Functor.map(dictFunctor)(f)($45));
                };
            };
        };
    };
};
module.exports = {
    Foldable1: Foldable1,
    foldMap1: foldMap1,
    fold1: fold1,
    traverse1_: traverse1_,
    for1_: for1_,
    sequence1_: sequence1_,
    foldMap1Default: foldMap1Default,
    fold1Default: fold1Default,
    intercalate: intercalate,
    intercalateMap: intercalateMap,
    maximum: maximum,
    minimum: minimum,
    foldableDual: foldableDual,
    foldableMultiplicative: foldableMultiplicative
};

},{"../Control.Apply/index.js":29,"../Control.Category/index.js":34,"../Control.Semigroupoid/index.js":56,"../Data.Foldable/index.js":87,"../Data.Function/index.js":91,"../Data.Functor/index.js":95,"../Data.Monoid.Dual/index.js":114,"../Data.Monoid.Multiplicative/index.js":116,"../Data.Newtype/index.js":119,"../Data.Ord.Max/index.js":125,"../Data.Ord.Min/index.js":126,"../Data.Semigroup/index.js":139,"../Data.Unit/index.js":162,"../Prelude/index.js":198}],136:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Monad = require("../Control.Monad/index.js");
var Data_Bounded = require("../Data.Bounded/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Show = require("../Data.Show/index.js");
var Prelude = require("../Prelude/index.js");
var Last = function (x) {
    return x;
};
var showLast = function (dictShow) {
    return new Data_Show.Show(function (v) {
        return "(Last " + (Data_Show.show(dictShow)(v) + ")");
    });
};
var semigroupLast = new Data_Semigroup.Semigroup(function (v) {
    return function (x) {
        return x;
    };
});
var ordLast = function (dictOrd) {
    return dictOrd;
};
var functorLast = new Data_Functor.Functor(function (f) {
    return function (m) {
        return f(m);
    };
});
var eqLast = function (dictEq) {
    return dictEq;
};
var eq1Last = new Data_Eq.Eq1(function (dictEq) {
    return Data_Eq.eq(eqLast(dictEq));
});
var ord1Last = new Data_Ord.Ord1(function () {
    return eq1Last;
}, function (dictOrd) {
    return Data_Ord.compare(ordLast(dictOrd));
});
var boundedLast = function (dictBounded) {
    return dictBounded;
};
var applyLast = new Control_Apply.Apply(function () {
    return functorLast;
}, function (v) {
    return function (v1) {
        return v(v1);
    };
});
var bindLast = new Control_Bind.Bind(function () {
    return applyLast;
}, function (v) {
    return function (f) {
        return f(v);
    };
});
var applicativeLast = new Control_Applicative.Applicative(function () {
    return applyLast;
}, Last);
var monadLast = new Control_Monad.Monad(function () {
    return applicativeLast;
}, function () {
    return bindLast;
});
module.exports = {
    Last: Last,
    eqLast: eqLast,
    eq1Last: eq1Last,
    ordLast: ordLast,
    ord1Last: ord1Last,
    boundedLast: boundedLast,
    showLast: showLast,
    functorLast: functorLast,
    applyLast: applyLast,
    applicativeLast: applicativeLast,
    bindLast: bindLast,
    monadLast: monadLast,
    semigroupLast: semigroupLast
};

},{"../Control.Applicative/index.js":27,"../Control.Apply/index.js":29,"../Control.Bind/index.js":33,"../Control.Monad/index.js":52,"../Data.Bounded/index.js":76,"../Data.Eq/index.js":82,"../Data.Functor/index.js":95,"../Data.Ord/index.js":130,"../Data.Semigroup/index.js":139,"../Data.Show/index.js":143,"../Prelude/index.js":198}],137:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Category = require("../Control.Category/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Monoid_Dual = require("../Data.Monoid.Dual/index.js");
var Data_Monoid_Multiplicative = require("../Data.Monoid.Multiplicative/index.js");
var Data_Semigroup_Foldable = require("../Data.Semigroup.Foldable/index.js");
var Data_Traversable = require("../Data.Traversable/index.js");
var Prelude = require("../Prelude/index.js");
var Traversable1 = function (Foldable10, Traversable1, sequence1, traverse1) {
    this.Foldable10 = Foldable10;
    this.Traversable1 = Traversable1;
    this.sequence1 = sequence1;
    this.traverse1 = traverse1;
};
var traverse1 = function (dict) {
    return dict.traverse1;
};
var sequence1Default = function (dictTraversable1) {
    return function (dictApply) {
        return traverse1(dictTraversable1)(dictApply)(Control_Category.identity(Control_Category.categoryFn));
    };
};
var traversableDual = new Traversable1(function () {
    return Data_Semigroup_Foldable.foldableDual;
}, function () {
    return Data_Traversable.traversableDual;
}, function (dictApply) {
    return sequence1Default(traversableDual)(dictApply);
}, function (dictApply) {
    return function (f) {
        return function (v) {
            return Data_Functor.map(dictApply.Functor0())(Data_Monoid_Dual.Dual)(f(v));
        };
    };
});
var traversableMultiplicative = new Traversable1(function () {
    return Data_Semigroup_Foldable.foldableMultiplicative;
}, function () {
    return Data_Traversable.traversableMultiplicative;
}, function (dictApply) {
    return sequence1Default(traversableMultiplicative)(dictApply);
}, function (dictApply) {
    return function (f) {
        return function (v) {
            return Data_Functor.map(dictApply.Functor0())(Data_Monoid_Multiplicative.Multiplicative)(f(v));
        };
    };
});
var sequence1 = function (dict) {
    return dict.sequence1;
};
var traverse1Default = function (dictTraversable1) {
    return function (dictApply) {
        return function (f) {
            return function (ta) {
                return sequence1(dictTraversable1)(dictApply)(Data_Functor.map((dictTraversable1.Traversable1()).Functor0())(f)(ta));
            };
        };
    };
};
module.exports = {
    sequence1: sequence1,
    traverse1: traverse1,
    Traversable1: Traversable1,
    traverse1Default: traverse1Default,
    sequence1Default: sequence1Default,
    traversableDual: traversableDual,
    traversableMultiplicative: traversableMultiplicative
};

},{"../Control.Category/index.js":34,"../Data.Functor/index.js":95,"../Data.Monoid.Dual/index.js":114,"../Data.Monoid.Multiplicative/index.js":116,"../Data.Semigroup.Foldable/index.js":135,"../Data.Traversable/index.js":154,"../Prelude/index.js":198}],138:[function(require,module,exports){
"use strict";

exports.concatString = function (s1) {
  return function (s2) {
    return s1 + s2;
  };
};

exports.concatArray = function (xs) {
  return function (ys) {
    if (xs.length === 0) return ys;
    if (ys.length === 0) return xs;
    return xs.concat(ys);
  };
};

},{}],139:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var $foreign = require("./foreign.js");
var Data_Symbol = require("../Data.Symbol/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Data_Void = require("../Data.Void/index.js");
var Record_Unsafe = require("../Record.Unsafe/index.js");
var Type_Data_RowList = require("../Type.Data.RowList/index.js");
var Semigroup = function (append) {
    this.append = append;
};
var SemigroupRecord = function (appendRecord) {
    this.appendRecord = appendRecord;
};
var semigroupVoid = new Semigroup(function (v) {
    return Data_Void.absurd;
});
var semigroupUnit = new Semigroup(function (v) {
    return function (v1) {
        return Data_Unit.unit;
    };
});
var semigroupString = new Semigroup($foreign.concatString);
var semigroupRecordNil = new SemigroupRecord(function (v) {
    return function (v1) {
        return function (v2) {
            return {};
        };
    };
});
var semigroupArray = new Semigroup($foreign.concatArray);
var appendRecord = function (dict) {
    return dict.appendRecord;
};
var semigroupRecord = function (dictRowToList) {
    return function (dictSemigroupRecord) {
        return new Semigroup(appendRecord(dictSemigroupRecord)(Type_Data_RowList.RLProxy.value));
    };
};
var append = function (dict) {
    return dict.append;
};
var semigroupFn = function (dictSemigroup) {
    return new Semigroup(function (f) {
        return function (g) {
            return function (x) {
                return append(dictSemigroup)(f(x))(g(x));
            };
        };
    });
};
var semigroupRecordCons = function (dictIsSymbol) {
    return function (dictCons) {
        return function (dictSemigroupRecord) {
            return function (dictSemigroup) {
                return new SemigroupRecord(function (v) {
                    return function (ra) {
                        return function (rb) {
                            var tail = appendRecord(dictSemigroupRecord)(Type_Data_RowList.RLProxy.value)(ra)(rb);
                            var key = Data_Symbol.reflectSymbol(dictIsSymbol)(Data_Symbol.SProxy.value);
                            var insert = Record_Unsafe.unsafeSet(key);
                            var get = Record_Unsafe.unsafeGet(key);
                            return insert(append(dictSemigroup)(get(ra))(get(rb)))(tail);
                        };
                    };
                });
            };
        };
    };
};
module.exports = {
    Semigroup: Semigroup,
    append: append,
    SemigroupRecord: SemigroupRecord,
    appendRecord: appendRecord,
    semigroupString: semigroupString,
    semigroupUnit: semigroupUnit,
    semigroupVoid: semigroupVoid,
    semigroupFn: semigroupFn,
    semigroupArray: semigroupArray,
    semigroupRecord: semigroupRecord,
    semigroupRecordNil: semigroupRecordNil,
    semigroupRecordCons: semigroupRecordCons
};

},{"../Data.Symbol/index.js":150,"../Data.Unit/index.js":162,"../Data.Void/index.js":163,"../Record.Unsafe/index.js":200,"../Type.Data.RowList/index.js":204,"./foreign.js":138}],140:[function(require,module,exports){
"use strict";

exports.intAdd = function (x) {
  return function (y) {
    /* jshint bitwise: false */
    return x + y | 0;
  };
};

exports.intMul = function (x) {
  return function (y) {
    /* jshint bitwise: false */
    return x * y | 0;
  };
};

exports.numAdd = function (n1) {
  return function (n2) {
    return n1 + n2;
  };
};

exports.numMul = function (n1) {
  return function (n2) {
    return n1 * n2;
  };
};

},{}],141:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var $foreign = require("./foreign.js");
var Data_Symbol = require("../Data.Symbol/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Record_Unsafe = require("../Record.Unsafe/index.js");
var Type_Data_Row = require("../Type.Data.Row/index.js");
var Type_Data_RowList = require("../Type.Data.RowList/index.js");
var Semiring = function (add, mul, one, zero) {
    this.add = add;
    this.mul = mul;
    this.one = one;
    this.zero = zero;
};
var SemiringRecord = function (addRecord, mulRecord, oneRecord, zeroRecord) {
    this.addRecord = addRecord;
    this.mulRecord = mulRecord;
    this.oneRecord = oneRecord;
    this.zeroRecord = zeroRecord;
};
var zeroRecord = function (dict) {
    return dict.zeroRecord;
};
var zero = function (dict) {
    return dict.zero;
};
var semiringUnit = new Semiring(function (v) {
    return function (v1) {
        return Data_Unit.unit;
    };
}, function (v) {
    return function (v1) {
        return Data_Unit.unit;
    };
}, Data_Unit.unit, Data_Unit.unit);
var semiringRecordNil = new SemiringRecord(function (v) {
    return function (v1) {
        return function (v2) {
            return {};
        };
    };
}, function (v) {
    return function (v1) {
        return function (v2) {
            return {};
        };
    };
}, function (v) {
    return function (v1) {
        return {};
    };
}, function (v) {
    return function (v1) {
        return {};
    };
});
var semiringNumber = new Semiring($foreign.numAdd, $foreign.numMul, 1.0, 0.0);
var semiringInt = new Semiring($foreign.intAdd, $foreign.intMul, 1, 0);
var oneRecord = function (dict) {
    return dict.oneRecord;
};
var one = function (dict) {
    return dict.one;
};
var mulRecord = function (dict) {
    return dict.mulRecord;
};
var mul = function (dict) {
    return dict.mul;
};
var addRecord = function (dict) {
    return dict.addRecord;
};
var semiringRecord = function (dictRowToList) {
    return function (dictSemiringRecord) {
        return new Semiring(addRecord(dictSemiringRecord)(Type_Data_RowList.RLProxy.value), mulRecord(dictSemiringRecord)(Type_Data_RowList.RLProxy.value), oneRecord(dictSemiringRecord)(Type_Data_RowList.RLProxy.value)(Type_Data_Row.RProxy.value), zeroRecord(dictSemiringRecord)(Type_Data_RowList.RLProxy.value)(Type_Data_Row.RProxy.value));
    };
};
var add = function (dict) {
    return dict.add;
};
var semiringFn = function (dictSemiring) {
    return new Semiring(function (f) {
        return function (g) {
            return function (x) {
                return add(dictSemiring)(f(x))(g(x));
            };
        };
    }, function (f) {
        return function (g) {
            return function (x) {
                return mul(dictSemiring)(f(x))(g(x));
            };
        };
    }, function (v) {
        return one(dictSemiring);
    }, function (v) {
        return zero(dictSemiring);
    });
};
var semiringRecordCons = function (dictIsSymbol) {
    return function (dictCons) {
        return function (dictSemiringRecord) {
            return function (dictSemiring) {
                return new SemiringRecord(function (v) {
                    return function (ra) {
                        return function (rb) {
                            var tail = addRecord(dictSemiringRecord)(Type_Data_RowList.RLProxy.value)(ra)(rb);
                            var key = Data_Symbol.reflectSymbol(dictIsSymbol)(Data_Symbol.SProxy.value);
                            var insert = Record_Unsafe.unsafeSet(key);
                            var get = Record_Unsafe.unsafeGet(key);
                            return insert(add(dictSemiring)(get(ra))(get(rb)))(tail);
                        };
                    };
                }, function (v) {
                    return function (ra) {
                        return function (rb) {
                            var tail = mulRecord(dictSemiringRecord)(Type_Data_RowList.RLProxy.value)(ra)(rb);
                            var key = Data_Symbol.reflectSymbol(dictIsSymbol)(Data_Symbol.SProxy.value);
                            var insert = Record_Unsafe.unsafeSet(key);
                            var get = Record_Unsafe.unsafeGet(key);
                            return insert(mul(dictSemiring)(get(ra))(get(rb)))(tail);
                        };
                    };
                }, function (v) {
                    return function (v1) {
                        var tail = oneRecord(dictSemiringRecord)(Type_Data_RowList.RLProxy.value)(Type_Data_Row.RProxy.value);
                        var key = Data_Symbol.reflectSymbol(dictIsSymbol)(Data_Symbol.SProxy.value);
                        var insert = Record_Unsafe.unsafeSet(key);
                        return insert(one(dictSemiring))(tail);
                    };
                }, function (v) {
                    return function (v1) {
                        var tail = zeroRecord(dictSemiringRecord)(Type_Data_RowList.RLProxy.value)(Type_Data_Row.RProxy.value);
                        var key = Data_Symbol.reflectSymbol(dictIsSymbol)(Data_Symbol.SProxy.value);
                        var insert = Record_Unsafe.unsafeSet(key);
                        return insert(zero(dictSemiring))(tail);
                    };
                });
            };
        };
    };
};
module.exports = {
    Semiring: Semiring,
    add: add,
    zero: zero,
    mul: mul,
    one: one,
    SemiringRecord: SemiringRecord,
    addRecord: addRecord,
    mulRecord: mulRecord,
    oneRecord: oneRecord,
    zeroRecord: zeroRecord,
    semiringInt: semiringInt,
    semiringNumber: semiringNumber,
    semiringFn: semiringFn,
    semiringUnit: semiringUnit,
    semiringRecord: semiringRecord,
    semiringRecordNil: semiringRecordNil,
    semiringRecordCons: semiringRecordCons
};

},{"../Data.Symbol/index.js":150,"../Data.Unit/index.js":162,"../Record.Unsafe/index.js":200,"../Type.Data.Row/index.js":203,"../Type.Data.RowList/index.js":204,"./foreign.js":140}],142:[function(require,module,exports){
"use strict";

exports.showIntImpl = function (n) {
  return n.toString();
};

exports.showNumberImpl = function (n) {
  var str = n.toString();
  return isNaN(str + ".0") ? str : str + ".0";
};

exports.showCharImpl = function (c) {
  var code = c.charCodeAt(0);
  if (code < 0x20 || code === 0x7F) {
    switch (c) {
      case "\x07": return "'\\a'";
      case "\b": return "'\\b'";
      case "\f": return "'\\f'";
      case "\n": return "'\\n'";
      case "\r": return "'\\r'";
      case "\t": return "'\\t'";
      case "\v": return "'\\v'";
    }
    return "'\\" + code.toString(10) + "'";
  }
  return c === "'" || c === "\\" ? "'\\" + c + "'" : "'" + c + "'";
};

exports.showStringImpl = function (s) {
  var l = s.length;
  return "\"" + s.replace(
    /[\0-\x1F\x7F"\\]/g, // eslint-disable-line no-control-regex
    function (c, i) {
      switch (c) {
        case "\"":
        case "\\":
          return "\\" + c;
        case "\x07": return "\\a";
        case "\b": return "\\b";
        case "\f": return "\\f";
        case "\n": return "\\n";
        case "\r": return "\\r";
        case "\t": return "\\t";
        case "\v": return "\\v";
      }
      var k = i + 1;
      var empty = k < l && s[k] >= "0" && s[k] <= "9" ? "\\&" : "";
      return "\\" + c.charCodeAt(0).toString(10) + empty;
    }
  ) + "\"";
};

exports.showArrayImpl = function (f) {
  return function (xs) {
    var ss = [];
    for (var i = 0, l = xs.length; i < l; i++) {
      ss[i] = f(xs[i]);
    }
    return "[" + ss.join(",") + "]";
  };
};

exports.cons = function (head) {
  return function (tail) {
    return [head].concat(tail);
  };
};

exports.join = function (separator) {
  return function (xs) {
    return xs.join(separator);
  };
};

},{}],143:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var $foreign = require("./foreign.js");
var Data_Symbol = require("../Data.Symbol/index.js");
var Record_Unsafe = require("../Record.Unsafe/index.js");
var Type_Data_RowList = require("../Type.Data.RowList/index.js");
var Show = function (show) {
    this.show = show;
};
var ShowRecordFields = function (showRecordFields) {
    this.showRecordFields = showRecordFields;
};
var showString = new Show($foreign.showStringImpl);
var showRecordFieldsNil = new ShowRecordFields(function (v) {
    return function (v1) {
        return [  ];
    };
});
var showRecordFields = function (dict) {
    return dict.showRecordFields;
};
var showRecord = function (dictRowToList) {
    return function (dictShowRecordFields) {
        return new Show(function (record) {
            var v = showRecordFields(dictShowRecordFields)(Type_Data_RowList.RLProxy.value)(record);
            if (v.length === 0) {
                return "{}";
            };
            return $foreign.join(" ")([ "{", $foreign.join(", ")(v), "}" ]);
        });
    };
};
var showNumber = new Show($foreign.showNumberImpl);
var showInt = new Show($foreign.showIntImpl);
var showChar = new Show($foreign.showCharImpl);
var showBoolean = new Show(function (v) {
    if (v) {
        return "true";
    };
    if (!v) {
        return "false";
    };
    throw new Error("Failed pattern match at Data.Show (line 20, column 1 - line 20, column 37): " + [ v.constructor.name ]);
});
var show = function (dict) {
    return dict.show;
};
var showArray = function (dictShow) {
    return new Show($foreign.showArrayImpl(show(dictShow)));
};
var showRecordFieldsCons = function (dictIsSymbol) {
    return function (dictShowRecordFields) {
        return function (dictShow) {
            return new ShowRecordFields(function (v) {
                return function (record) {
                    var tail = showRecordFields(dictShowRecordFields)(Type_Data_RowList.RLProxy.value)(record);
                    var key = Data_Symbol.reflectSymbol(dictIsSymbol)(Data_Symbol.SProxy.value);
                    var focus = Record_Unsafe.unsafeGet(key)(record);
                    return $foreign.cons($foreign.join(": ")([ key, show(dictShow)(focus) ]))(tail);
                };
            });
        };
    };
};
module.exports = {
    Show: Show,
    show: show,
    ShowRecordFields: ShowRecordFields,
    showRecordFields: showRecordFields,
    showBoolean: showBoolean,
    showInt: showInt,
    showNumber: showNumber,
    showChar: showChar,
    showString: showString,
    showArray: showArray,
    showRecord: showRecord,
    showRecordFieldsNil: showRecordFieldsNil,
    showRecordFieldsCons: showRecordFieldsCons
};

},{"../Data.Symbol/index.js":150,"../Record.Unsafe/index.js":200,"../Type.Data.RowList/index.js":204,"./foreign.js":142}],144:[function(require,module,exports){
"use strict";

exports.fromCharArray = function (a) {
  return a.join("");
};

exports.toCharArray = function (s) {
  return s.split("");
};

exports.singleton = function (c) {
  return c;
};

exports._charAt = function (just) {
  return function (nothing) {
    return function (i) {
      return function (s) {
        return i >= 0 && i < s.length ? just(s.charAt(i)) : nothing;
      };
    };
  };
};

exports._toChar = function (just) {
  return function (nothing) {
    return function (s) {
      return s.length === 1 ? just(s) : nothing;
    };
  };
};

exports.length = function (s) {
  return s.length;
};

exports.countPrefix = function (p) {
  return function (s) {
    var i = 0;
    while (i < s.length && p(s.charAt(i))) i++;
    return i;
  };
};

exports._indexOf = function (just) {
  return function (nothing) {
    return function (x) {
      return function (s) {
        var i = s.indexOf(x);
        return i === -1 ? nothing : just(i);
      };
    };
  };
};

exports["_indexOf'"] = function (just) {
  return function (nothing) {
    return function (x) {
      return function (startAt) {
        return function (s) {
          if (startAt < 0 || startAt > s.length) return nothing;
          var i = s.indexOf(x, startAt);
          return i === -1 ? nothing : just(i);
        };
      };
    };
  };
};

exports._lastIndexOf = function (just) {
  return function (nothing) {
    return function (x) {
      return function (s) {
        var i = s.lastIndexOf(x);
        return i === -1 ? nothing : just(i);
      };
    };
  };
};

exports["_lastIndexOf'"] = function (just) {
  return function (nothing) {
    return function (x) {
      return function (startAt) {
        return function (s) {
          if (startAt < 0 || startAt > s.length) return nothing;
          var i = s.lastIndexOf(x, startAt);
          return i === -1 ? nothing : just(i);
        };
      };
    };
  };
};

exports.take = function (n) {
  return function (s) {
    return s.substr(0, n);
  };
};

exports.drop = function (n) {
  return function (s) {
    return s.substring(n);
  };
};

exports._slice = function (b) {
  return function (e) {
    return function (s) {
      return s.slice(b,e);
    };
  };
};

exports.splitAt = function (i) {
  return function (s) {
    return { before: s.substring(0, i), after: s.substring(i) };
  };
};

},{}],145:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var $foreign = require("./foreign.js");
var Control_Semigroupoid = require("../Control.Semigroupoid/index.js");
var Data_Boolean = require("../Data.Boolean/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Ring = require("../Data.Ring/index.js");
var Data_Semiring = require("../Data.Semiring/index.js");
var Data_String_Pattern = require("../Data.String.Pattern/index.js");
var Data_String_Unsafe = require("../Data.String.Unsafe/index.js");
var Prelude = require("../Prelude/index.js");
var uncons = function (v) {
    if (v === "") {
        return Data_Maybe.Nothing.value;
    };
    return new Data_Maybe.Just({
        head: Data_String_Unsafe.charAt(0)(v),
        tail: $foreign.drop(1)(v)
    });
};
var toChar = $foreign["_toChar"](Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var takeWhile = function (p) {
    return function (s) {
        return $foreign.take($foreign.countPrefix(p)(s))(s);
    };
};
var takeRight = function (i) {
    return function (s) {
        return $foreign.drop($foreign.length(s) - i | 0)(s);
    };
};
var slice = function (b) {
    return function (e) {
        return function (s) {
            var l = $foreign.length(s);
            var norm = function (x) {
                if (x < 0) {
                    return l + x | 0;
                };
                if (Data_Boolean.otherwise) {
                    return x;
                };
                throw new Error("Failed pattern match at Data.String.CodeUnits (line 314, column 5 - line 315, column 27): " + [ x.constructor.name ]);
            };
            var e$prime = norm(e);
            var b$prime = norm(b);
            var $7 = b$prime < 0 || (b$prime >= l || (e$prime < 0 || (e$prime > l || b$prime > e$prime)));
            if ($7) {
                return Data_Maybe.Nothing.value;
            };
            return new Data_Maybe.Just($foreign["_slice"](b)(e)(s));
        };
    };
};
var lastIndexOf$prime = $foreign["_lastIndexOf'"](Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var lastIndexOf = $foreign["_lastIndexOf"](Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var stripSuffix = function (v) {
    return function (str) {
        var v1 = lastIndexOf(v)(str);
        if (v1 instanceof Data_Maybe.Just && v1.value0 === ($foreign.length(str) - $foreign.length(v) | 0)) {
            return Data_Maybe.Just.create($foreign.take(v1.value0)(str));
        };
        return Data_Maybe.Nothing.value;
    };
};
var indexOf$prime = $foreign["_indexOf'"](Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var indexOf = $foreign["_indexOf"](Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var stripPrefix = function (v) {
    return function (str) {
        var v1 = indexOf(v)(str);
        if (v1 instanceof Data_Maybe.Just && v1.value0 === 0) {
            return Data_Maybe.Just.create($foreign.drop($foreign.length(v))(str));
        };
        return Data_Maybe.Nothing.value;
    };
};
var dropWhile = function (p) {
    return function (s) {
        return $foreign.drop($foreign.countPrefix(p)(s))(s);
    };
};
var dropRight = function (i) {
    return function (s) {
        return $foreign.take($foreign.length(s) - i | 0)(s);
    };
};
var contains = function (pat) {
    return function ($16) {
        return Data_Maybe.isJust(indexOf(pat)($16));
    };
};
var charAt = $foreign["_charAt"](Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
module.exports = {
    stripPrefix: stripPrefix,
    stripSuffix: stripSuffix,
    contains: contains,
    charAt: charAt,
    toChar: toChar,
    uncons: uncons,
    indexOf: indexOf,
    "indexOf'": indexOf$prime,
    lastIndexOf: lastIndexOf,
    "lastIndexOf'": lastIndexOf$prime,
    takeRight: takeRight,
    takeWhile: takeWhile,
    dropRight: dropRight,
    dropWhile: dropWhile,
    slice: slice,
    singleton: $foreign.singleton,
    fromCharArray: $foreign.fromCharArray,
    toCharArray: $foreign.toCharArray,
    length: $foreign.length,
    countPrefix: $foreign.countPrefix,
    take: $foreign.take,
    drop: $foreign.drop,
    splitAt: $foreign.splitAt
};

},{"../Control.Semigroupoid/index.js":56,"../Data.Boolean/index.js":73,"../Data.Eq/index.js":82,"../Data.Function/index.js":91,"../Data.HeytingAlgebra/index.js":99,"../Data.Maybe/index.js":110,"../Data.Ord/index.js":130,"../Data.Ring/index.js":133,"../Data.Semiring/index.js":141,"../Data.String.Pattern/index.js":146,"../Data.String.Unsafe/index.js":148,"../Prelude/index.js":198,"./foreign.js":144}],146:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Data_Eq = require("../Data.Eq/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Show = require("../Data.Show/index.js");
var Prelude = require("../Prelude/index.js");
var Replacement = function (x) {
    return x;
};
var Pattern = function (x) {
    return x;
};
var showReplacement = new Data_Show.Show(function (v) {
    return "(Replacement " + (Data_Show.show(Data_Show.showString)(v) + ")");
});
var showPattern = new Data_Show.Show(function (v) {
    return "(Pattern " + (Data_Show.show(Data_Show.showString)(v) + ")");
});
var newtypeReplacement = new Data_Newtype.Newtype(function (n) {
    return n;
}, Replacement);
var newtypePattern = new Data_Newtype.Newtype(function (n) {
    return n;
}, Pattern);
var eqReplacement = new Data_Eq.Eq(function (x) {
    return function (y) {
        return x === y;
    };
});
var ordReplacement = new Data_Ord.Ord(function () {
    return eqReplacement;
}, function (x) {
    return function (y) {
        return Data_Ord.compare(Data_Ord.ordString)(x)(y);
    };
});
var eqPattern = new Data_Eq.Eq(function (x) {
    return function (y) {
        return x === y;
    };
});
var ordPattern = new Data_Ord.Ord(function () {
    return eqPattern;
}, function (x) {
    return function (y) {
        return Data_Ord.compare(Data_Ord.ordString)(x)(y);
    };
});
module.exports = {
    Pattern: Pattern,
    Replacement: Replacement,
    eqPattern: eqPattern,
    ordPattern: ordPattern,
    newtypePattern: newtypePattern,
    showPattern: showPattern,
    eqReplacement: eqReplacement,
    ordReplacement: ordReplacement,
    newtypeReplacement: newtypeReplacement,
    showReplacement: showReplacement
};

},{"../Data.Eq/index.js":82,"../Data.Newtype/index.js":119,"../Data.Ord/index.js":130,"../Data.Semigroup/index.js":139,"../Data.Show/index.js":143,"../Prelude/index.js":198}],147:[function(require,module,exports){
"use strict";

exports.charAt = function (i) {
  return function (s) {
    if (i >= 0 && i < s.length) return s.charAt(i);
    throw new Error("Data.String.Unsafe.charAt: Invalid index.");
  };
};

exports.char = function (s) {
  if (s.length === 1) return s.charAt(0);
  throw new Error("Data.String.Unsafe.char: Expected string of length 1.");
};

},{}],148:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var $foreign = require("./foreign.js");
module.exports = {
    "char": $foreign["char"],
    charAt: $foreign.charAt
};

},{"./foreign.js":147}],149:[function(require,module,exports){
"use strict";

// module Data.Symbol

exports.unsafeCoerce = function (arg) {
  return arg;
};


},{}],150:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var $foreign = require("./foreign.js");
var SProxy = (function () {
    function SProxy() {

    };
    SProxy.value = new SProxy();
    return SProxy;
})();
var IsSymbol = function (reflectSymbol) {
    this.reflectSymbol = reflectSymbol;
};
var reifySymbol = function (s) {
    return function (f) {
        return $foreign.unsafeCoerce(function (dictIsSymbol) {
            return f(dictIsSymbol);
        })({
            reflectSymbol: function (v) {
                return s;
            }
        })(SProxy.value);
    };
};
var reflectSymbol = function (dict) {
    return dict.reflectSymbol;
};
module.exports = {
    IsSymbol: IsSymbol,
    reflectSymbol: reflectSymbol,
    reifySymbol: reifySymbol,
    SProxy: SProxy
};

},{"./foreign.js":149}],151:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Traversable_Accum = require("../Data.Traversable.Accum/index.js");
var Prelude = require("../Prelude/index.js");
var StateR = function (x) {
    return x;
};
var StateL = function (x) {
    return x;
};
var stateR = function (v) {
    return v;
};
var stateL = function (v) {
    return v;
};
var functorStateR = new Data_Functor.Functor(function (f) {
    return function (k) {
        return function (s) {
            var v = stateR(k)(s);
            return {
                accum: v.accum,
                value: f(v.value)
            };
        };
    };
});
var functorStateL = new Data_Functor.Functor(function (f) {
    return function (k) {
        return function (s) {
            var v = stateL(k)(s);
            return {
                accum: v.accum,
                value: f(v.value)
            };
        };
    };
});
var applyStateR = new Control_Apply.Apply(function () {
    return functorStateR;
}, function (f) {
    return function (x) {
        return function (s) {
            var v = stateR(x)(s);
            var v1 = stateR(f)(v.accum);
            return {
                accum: v1.accum,
                value: v1.value(v.value)
            };
        };
    };
});
var applyStateL = new Control_Apply.Apply(function () {
    return functorStateL;
}, function (f) {
    return function (x) {
        return function (s) {
            var v = stateL(f)(s);
            var v1 = stateL(x)(v.accum);
            return {
                accum: v1.accum,
                value: v.value(v1.value)
            };
        };
    };
});
var applicativeStateR = new Control_Applicative.Applicative(function () {
    return applyStateR;
}, function (a) {
    return function (s) {
        return {
            accum: s,
            value: a
        };
    };
});
var applicativeStateL = new Control_Applicative.Applicative(function () {
    return applyStateL;
}, function (a) {
    return function (s) {
        return {
            accum: s,
            value: a
        };
    };
});
module.exports = {
    StateL: StateL,
    stateL: stateL,
    StateR: StateR,
    stateR: stateR,
    functorStateL: functorStateL,
    applyStateL: applyStateL,
    applicativeStateL: applicativeStateL,
    functorStateR: functorStateR,
    applyStateR: applyStateR,
    applicativeStateR: applicativeStateR
};

},{"../Control.Applicative/index.js":27,"../Control.Apply/index.js":29,"../Data.Functor/index.js":95,"../Data.Traversable.Accum/index.js":152,"../Prelude/index.js":198}],152:[function(require,module,exports){
arguments[4][64][0].apply(exports,arguments)
},{"dup":64}],153:[function(require,module,exports){
"use strict";

// jshint maxparams: 3

exports.traverseArrayImpl = function () {
  function array1(a) {
    return [a];
  }

  function array2(a) {
    return function (b) {
      return [a, b];
    };
  }

  function array3(a) {
    return function (b) {
      return function (c) {
        return [a, b, c];
      };
    };
  }

  function concat2(xs) {
    return function (ys) {
      return xs.concat(ys);
    };
  }

  return function (apply) {
    return function (map) {
      return function (pure) {
        return function (f) {
          return function (array) {
            function go(bot, top) {
              switch (top - bot) {
              case 0: return pure([]);
              case 1: return map(array1)(f(array[bot]));
              case 2: return apply(map(array2)(f(array[bot])))(f(array[bot + 1]));
              case 3: return apply(apply(map(array3)(f(array[bot])))(f(array[bot + 1])))(f(array[bot + 2]));
              default:
                // This slightly tricky pivot selection aims to produce two
                // even-length partitions where possible.
                var pivot = bot + Math.floor((top - bot) / 4) * 2;
                return apply(map(concat2)(go(bot, pivot)))(go(pivot, top));
              }
            }
            return go(0, array.length);
          };
        };
      };
    };
  };
}();

},{}],154:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var $foreign = require("./foreign.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Category = require("../Control.Category/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Maybe_First = require("../Data.Maybe.First/index.js");
var Data_Maybe_Last = require("../Data.Maybe.Last/index.js");
var Data_Monoid_Additive = require("../Data.Monoid.Additive/index.js");
var Data_Monoid_Conj = require("../Data.Monoid.Conj/index.js");
var Data_Monoid_Disj = require("../Data.Monoid.Disj/index.js");
var Data_Monoid_Dual = require("../Data.Monoid.Dual/index.js");
var Data_Monoid_Multiplicative = require("../Data.Monoid.Multiplicative/index.js");
var Data_Traversable_Accum = require("../Data.Traversable.Accum/index.js");
var Data_Traversable_Accum_Internal = require("../Data.Traversable.Accum.Internal/index.js");
var Prelude = require("../Prelude/index.js");
var Traversable = function (Foldable1, Functor0, sequence, traverse) {
    this.Foldable1 = Foldable1;
    this.Functor0 = Functor0;
    this.sequence = sequence;
    this.traverse = traverse;
};
var traverse = function (dict) {
    return dict.traverse;
};
var traversableMultiplicative = new Traversable(function () {
    return Data_Foldable.foldableMultiplicative;
}, function () {
    return Data_Monoid_Multiplicative.functorMultiplicative;
}, function (dictApplicative) {
    return function (v) {
        return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Monoid_Multiplicative.Multiplicative)(v);
    };
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Monoid_Multiplicative.Multiplicative)(f(v));
        };
    };
});
var traversableMaybe = new Traversable(function () {
    return Data_Foldable.foldableMaybe;
}, function () {
    return Data_Maybe.functorMaybe;
}, function (dictApplicative) {
    return function (v) {
        if (v instanceof Data_Maybe.Nothing) {
            return Control_Applicative.pure(dictApplicative)(Data_Maybe.Nothing.value);
        };
        if (v instanceof Data_Maybe.Just) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Maybe.Just.create)(v.value0);
        };
        throw new Error("Failed pattern match at Data.Traversable (line 86, column 1 - line 86, column 47): " + [ v.constructor.name ]);
    };
}, function (dictApplicative) {
    return function (v) {
        return function (v1) {
            if (v1 instanceof Data_Maybe.Nothing) {
                return Control_Applicative.pure(dictApplicative)(Data_Maybe.Nothing.value);
            };
            if (v1 instanceof Data_Maybe.Just) {
                return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Maybe.Just.create)(v(v1.value0));
            };
            throw new Error("Failed pattern match at Data.Traversable (line 86, column 1 - line 86, column 47): " + [ v.constructor.name, v1.constructor.name ]);
        };
    };
});
var traversableDual = new Traversable(function () {
    return Data_Foldable.foldableDual;
}, function () {
    return Data_Monoid_Dual.functorDual;
}, function (dictApplicative) {
    return function (v) {
        return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Monoid_Dual.Dual)(v);
    };
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Monoid_Dual.Dual)(f(v));
        };
    };
});
var traversableDisj = new Traversable(function () {
    return Data_Foldable.foldableDisj;
}, function () {
    return Data_Monoid_Disj.functorDisj;
}, function (dictApplicative) {
    return function (v) {
        return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Monoid_Disj.Disj)(v);
    };
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Monoid_Disj.Disj)(f(v));
        };
    };
});
var traversableConj = new Traversable(function () {
    return Data_Foldable.foldableConj;
}, function () {
    return Data_Monoid_Conj.functorConj;
}, function (dictApplicative) {
    return function (v) {
        return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Monoid_Conj.Conj)(v);
    };
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Monoid_Conj.Conj)(f(v));
        };
    };
});
var traversableAdditive = new Traversable(function () {
    return Data_Foldable.foldableAdditive;
}, function () {
    return Data_Monoid_Additive.functorAdditive;
}, function (dictApplicative) {
    return function (v) {
        return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Monoid_Additive.Additive)(v);
    };
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Monoid_Additive.Additive)(f(v));
        };
    };
});
var sequenceDefault = function (dictTraversable) {
    return function (dictApplicative) {
        return traverse(dictTraversable)(dictApplicative)(Control_Category.identity(Control_Category.categoryFn));
    };
};
var traversableArray = new Traversable(function () {
    return Data_Foldable.foldableArray;
}, function () {
    return Data_Functor.functorArray;
}, function (dictApplicative) {
    return sequenceDefault(traversableArray)(dictApplicative);
}, function (dictApplicative) {
    return $foreign.traverseArrayImpl(Control_Apply.apply(dictApplicative.Apply0()))(Data_Functor.map((dictApplicative.Apply0()).Functor0()))(Control_Applicative.pure(dictApplicative));
});
var sequence = function (dict) {
    return dict.sequence;
};
var traversableFirst = new Traversable(function () {
    return Data_Foldable.foldableFirst;
}, function () {
    return Data_Maybe_First.functorFirst;
}, function (dictApplicative) {
    return function (v) {
        return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Maybe_First.First)(sequence(traversableMaybe)(dictApplicative)(v));
    };
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Maybe_First.First)(traverse(traversableMaybe)(dictApplicative)(f)(v));
        };
    };
});
var traversableLast = new Traversable(function () {
    return Data_Foldable.foldableLast;
}, function () {
    return Data_Maybe_Last.functorLast;
}, function (dictApplicative) {
    return function (v) {
        return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Maybe_Last.Last)(sequence(traversableMaybe)(dictApplicative)(v));
    };
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Maybe_Last.Last)(traverse(traversableMaybe)(dictApplicative)(f)(v));
        };
    };
});
var traverseDefault = function (dictTraversable) {
    return function (dictApplicative) {
        return function (f) {
            return function (ta) {
                return sequence(dictTraversable)(dictApplicative)(Data_Functor.map(dictTraversable.Functor0())(f)(ta));
            };
        };
    };
};
var mapAccumR = function (dictTraversable) {
    return function (f) {
        return function (s0) {
            return function (xs) {
                return Data_Traversable_Accum_Internal.stateR(traverse(dictTraversable)(Data_Traversable_Accum_Internal.applicativeStateR)(function (a) {
                    return function (s) {
                        return f(s)(a);
                    };
                })(xs))(s0);
            };
        };
    };
};
var scanr = function (dictTraversable) {
    return function (f) {
        return function (b0) {
            return function (xs) {
                return (mapAccumR(dictTraversable)(function (b) {
                    return function (a) {
                        var b$prime = f(a)(b);
                        return {
                            accum: b$prime,
                            value: b$prime
                        };
                    };
                })(b0)(xs)).value;
            };
        };
    };
};
var mapAccumL = function (dictTraversable) {
    return function (f) {
        return function (s0) {
            return function (xs) {
                return Data_Traversable_Accum_Internal.stateL(traverse(dictTraversable)(Data_Traversable_Accum_Internal.applicativeStateL)(function (a) {
                    return function (s) {
                        return f(s)(a);
                    };
                })(xs))(s0);
            };
        };
    };
};
var scanl = function (dictTraversable) {
    return function (f) {
        return function (b0) {
            return function (xs) {
                return (mapAccumL(dictTraversable)(function (b) {
                    return function (a) {
                        var b$prime = f(b)(a);
                        return {
                            accum: b$prime,
                            value: b$prime
                        };
                    };
                })(b0)(xs)).value;
            };
        };
    };
};
var $$for = function (dictApplicative) {
    return function (dictTraversable) {
        return function (x) {
            return function (f) {
                return traverse(dictTraversable)(dictApplicative)(f)(x);
            };
        };
    };
};
module.exports = {
    Traversable: Traversable,
    traverse: traverse,
    sequence: sequence,
    traverseDefault: traverseDefault,
    sequenceDefault: sequenceDefault,
    "for": $$for,
    scanl: scanl,
    scanr: scanr,
    mapAccumL: mapAccumL,
    mapAccumR: mapAccumR,
    traversableArray: traversableArray,
    traversableMaybe: traversableMaybe,
    traversableFirst: traversableFirst,
    traversableLast: traversableLast,
    traversableAdditive: traversableAdditive,
    traversableDual: traversableDual,
    traversableConj: traversableConj,
    traversableDisj: traversableDisj,
    traversableMultiplicative: traversableMultiplicative
};

},{"../Control.Applicative/index.js":27,"../Control.Apply/index.js":29,"../Control.Category/index.js":34,"../Data.Foldable/index.js":87,"../Data.Functor/index.js":95,"../Data.Maybe.First/index.js":108,"../Data.Maybe.Last/index.js":109,"../Data.Maybe/index.js":110,"../Data.Monoid.Additive/index.js":111,"../Data.Monoid.Conj/index.js":112,"../Data.Monoid.Disj/index.js":113,"../Data.Monoid.Dual/index.js":114,"../Data.Monoid.Multiplicative/index.js":116,"../Data.Traversable.Accum.Internal/index.js":151,"../Data.Traversable.Accum/index.js":152,"../Prelude/index.js":198,"./foreign.js":153}],155:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Semigroupoid = require("../Control.Semigroupoid/index.js");
var Data_FoldableWithIndex = require("../Data.FoldableWithIndex/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_FunctorWithIndex = require("../Data.FunctorWithIndex/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Maybe_First = require("../Data.Maybe.First/index.js");
var Data_Maybe_Last = require("../Data.Maybe.Last/index.js");
var Data_Monoid_Additive = require("../Data.Monoid.Additive/index.js");
var Data_Monoid_Conj = require("../Data.Monoid.Conj/index.js");
var Data_Monoid_Disj = require("../Data.Monoid.Disj/index.js");
var Data_Monoid_Dual = require("../Data.Monoid.Dual/index.js");
var Data_Monoid_Multiplicative = require("../Data.Monoid.Multiplicative/index.js");
var Data_Traversable = require("../Data.Traversable/index.js");
var Data_Traversable_Accum = require("../Data.Traversable.Accum/index.js");
var Data_Traversable_Accum_Internal = require("../Data.Traversable.Accum.Internal/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Prelude = require("../Prelude/index.js");
var TraversableWithIndex = function (FoldableWithIndex1, FunctorWithIndex0, Traversable2, traverseWithIndex) {
    this.FoldableWithIndex1 = FoldableWithIndex1;
    this.FunctorWithIndex0 = FunctorWithIndex0;
    this.Traversable2 = Traversable2;
    this.traverseWithIndex = traverseWithIndex;
};
var traverseWithIndexDefault = function (dictTraversableWithIndex) {
    return function (dictApplicative) {
        return function (f) {
            return function ($19) {
                return Data_Traversable.sequence(dictTraversableWithIndex.Traversable2())(dictApplicative)(Data_FunctorWithIndex.mapWithIndex(dictTraversableWithIndex.FunctorWithIndex0())(f)($19));
            };
        };
    };
};
var traverseWithIndex = function (dict) {
    return dict.traverseWithIndex;
};
var traverseDefault = function (dictTraversableWithIndex) {
    return function (dictApplicative) {
        return function (f) {
            return traverseWithIndex(dictTraversableWithIndex)(dictApplicative)(Data_Function["const"](f));
        };
    };
};
var traversableWithIndexMultiplicative = new TraversableWithIndex(function () {
    return Data_FoldableWithIndex.foldableWithIndexMultiplicative;
}, function () {
    return Data_FunctorWithIndex.functorWithIndexMultiplicative;
}, function () {
    return Data_Traversable.traversableMultiplicative;
}, function (dictApplicative) {
    return function (f) {
        return Data_Traversable.traverse(Data_Traversable.traversableMultiplicative)(dictApplicative)(f(Data_Unit.unit));
    };
});
var traversableWithIndexMaybe = new TraversableWithIndex(function () {
    return Data_FoldableWithIndex.foldableWithIndexMaybe;
}, function () {
    return Data_FunctorWithIndex.functorWithIndexMaybe;
}, function () {
    return Data_Traversable.traversableMaybe;
}, function (dictApplicative) {
    return function (f) {
        return Data_Traversable.traverse(Data_Traversable.traversableMaybe)(dictApplicative)(f(Data_Unit.unit));
    };
});
var traversableWithIndexLast = new TraversableWithIndex(function () {
    return Data_FoldableWithIndex.foldableWithIndexLast;
}, function () {
    return Data_FunctorWithIndex.functorWithIndexLast;
}, function () {
    return Data_Traversable.traversableLast;
}, function (dictApplicative) {
    return function (f) {
        return Data_Traversable.traverse(Data_Traversable.traversableLast)(dictApplicative)(f(Data_Unit.unit));
    };
});
var traversableWithIndexFirst = new TraversableWithIndex(function () {
    return Data_FoldableWithIndex.foldableWithIndexFirst;
}, function () {
    return Data_FunctorWithIndex.functorWithIndexFirst;
}, function () {
    return Data_Traversable.traversableFirst;
}, function (dictApplicative) {
    return function (f) {
        return Data_Traversable.traverse(Data_Traversable.traversableFirst)(dictApplicative)(f(Data_Unit.unit));
    };
});
var traversableWithIndexDual = new TraversableWithIndex(function () {
    return Data_FoldableWithIndex.foldableWithIndexDual;
}, function () {
    return Data_FunctorWithIndex.functorWithIndexDual;
}, function () {
    return Data_Traversable.traversableDual;
}, function (dictApplicative) {
    return function (f) {
        return Data_Traversable.traverse(Data_Traversable.traversableDual)(dictApplicative)(f(Data_Unit.unit));
    };
});
var traversableWithIndexDisj = new TraversableWithIndex(function () {
    return Data_FoldableWithIndex.foldableWithIndexDisj;
}, function () {
    return Data_FunctorWithIndex.functorWithIndexDisj;
}, function () {
    return Data_Traversable.traversableDisj;
}, function (dictApplicative) {
    return function (f) {
        return Data_Traversable.traverse(Data_Traversable.traversableDisj)(dictApplicative)(f(Data_Unit.unit));
    };
});
var traversableWithIndexConj = new TraversableWithIndex(function () {
    return Data_FoldableWithIndex.foldableWithIndexConj;
}, function () {
    return Data_FunctorWithIndex.functorWithIndexConj;
}, function () {
    return Data_Traversable.traversableConj;
}, function (dictApplicative) {
    return function (f) {
        return Data_Traversable.traverse(Data_Traversable.traversableConj)(dictApplicative)(f(Data_Unit.unit));
    };
});
var traversableWithIndexArray = new TraversableWithIndex(function () {
    return Data_FoldableWithIndex.foldableWithIndexArray;
}, function () {
    return Data_FunctorWithIndex.functorWithIndexArray;
}, function () {
    return Data_Traversable.traversableArray;
}, function (dictApplicative) {
    return traverseWithIndexDefault(traversableWithIndexArray)(dictApplicative);
});
var traversableWithIndexAdditive = new TraversableWithIndex(function () {
    return Data_FoldableWithIndex.foldableWithIndexAdditive;
}, function () {
    return Data_FunctorWithIndex.functorWithIndexAdditive;
}, function () {
    return Data_Traversable.traversableAdditive;
}, function (dictApplicative) {
    return function (f) {
        return Data_Traversable.traverse(Data_Traversable.traversableAdditive)(dictApplicative)(f(Data_Unit.unit));
    };
});
var mapAccumRWithIndex = function (dictTraversableWithIndex) {
    return function (f) {
        return function (s0) {
            return function (xs) {
                return Data_Traversable_Accum_Internal.stateR(traverseWithIndex(dictTraversableWithIndex)(Data_Traversable_Accum_Internal.applicativeStateR)(function (i) {
                    return function (a) {
                        return function (s) {
                            return f(i)(s)(a);
                        };
                    };
                })(xs))(s0);
            };
        };
    };
};
var scanrWithIndex = function (dictTraversableWithIndex) {
    return function (f) {
        return function (b0) {
            return function (xs) {
                return (mapAccumRWithIndex(dictTraversableWithIndex)(function (i) {
                    return function (b) {
                        return function (a) {
                            var b$prime = f(i)(a)(b);
                            return {
                                accum: b$prime,
                                value: b$prime
                            };
                        };
                    };
                })(b0)(xs)).value;
            };
        };
    };
};
var mapAccumLWithIndex = function (dictTraversableWithIndex) {
    return function (f) {
        return function (s0) {
            return function (xs) {
                return Data_Traversable_Accum_Internal.stateL(traverseWithIndex(dictTraversableWithIndex)(Data_Traversable_Accum_Internal.applicativeStateL)(function (i) {
                    return function (a) {
                        return function (s) {
                            return f(i)(s)(a);
                        };
                    };
                })(xs))(s0);
            };
        };
    };
};
var scanlWithIndex = function (dictTraversableWithIndex) {
    return function (f) {
        return function (b0) {
            return function (xs) {
                return (mapAccumLWithIndex(dictTraversableWithIndex)(function (i) {
                    return function (b) {
                        return function (a) {
                            var b$prime = f(i)(b)(a);
                            return {
                                accum: b$prime,
                                value: b$prime
                            };
                        };
                    };
                })(b0)(xs)).value;
            };
        };
    };
};
var forWithIndex = function (dictApplicative) {
    return function (dictTraversableWithIndex) {
        return Data_Function.flip(traverseWithIndex(dictTraversableWithIndex)(dictApplicative));
    };
};
module.exports = {
    TraversableWithIndex: TraversableWithIndex,
    traverseWithIndex: traverseWithIndex,
    traverseWithIndexDefault: traverseWithIndexDefault,
    forWithIndex: forWithIndex,
    scanlWithIndex: scanlWithIndex,
    mapAccumLWithIndex: mapAccumLWithIndex,
    scanrWithIndex: scanrWithIndex,
    mapAccumRWithIndex: mapAccumRWithIndex,
    traverseDefault: traverseDefault,
    traversableWithIndexArray: traversableWithIndexArray,
    traversableWithIndexMaybe: traversableWithIndexMaybe,
    traversableWithIndexFirst: traversableWithIndexFirst,
    traversableWithIndexLast: traversableWithIndexLast,
    traversableWithIndexAdditive: traversableWithIndexAdditive,
    traversableWithIndexDual: traversableWithIndexDual,
    traversableWithIndexConj: traversableWithIndexConj,
    traversableWithIndexDisj: traversableWithIndexDisj,
    traversableWithIndexMultiplicative: traversableWithIndexMultiplicative
};

},{"../Control.Semigroupoid/index.js":56,"../Data.FoldableWithIndex/index.js":88,"../Data.Function/index.js":91,"../Data.FunctorWithIndex/index.js":97,"../Data.Maybe.First/index.js":108,"../Data.Maybe.Last/index.js":109,"../Data.Maybe/index.js":110,"../Data.Monoid.Additive/index.js":111,"../Data.Monoid.Conj/index.js":112,"../Data.Monoid.Disj/index.js":113,"../Data.Monoid.Dual/index.js":114,"../Data.Monoid.Multiplicative/index.js":116,"../Data.Traversable.Accum.Internal/index.js":151,"../Data.Traversable.Accum/index.js":152,"../Data.Traversable/index.js":154,"../Data.Unit/index.js":162,"../Prelude/index.js":198}],156:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Biapplicative = require("../Control.Biapplicative/index.js");
var Control_Biapply = require("../Control.Biapply/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Comonad = require("../Control.Comonad/index.js");
var Control_Extend = require("../Control.Extend/index.js");
var Control_Lazy = require("../Control.Lazy/index.js");
var Control_Monad = require("../Control.Monad/index.js");
var Control_Semigroupoid = require("../Control.Semigroupoid/index.js");
var Data_Bifoldable = require("../Data.Bifoldable/index.js");
var Data_Bifunctor = require("../Data.Bifunctor/index.js");
var Data_Bitraversable = require("../Data.Bitraversable/index.js");
var Data_BooleanAlgebra = require("../Data.BooleanAlgebra/index.js");
var Data_Bounded = require("../Data.Bounded/index.js");
var Data_CommutativeRing = require("../Data.CommutativeRing/index.js");
var Data_Distributive = require("../Data.Distributive/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_FoldableWithIndex = require("../Data.FoldableWithIndex/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Functor_Invariant = require("../Data.Functor.Invariant/index.js");
var Data_FunctorWithIndex = require("../Data.FunctorWithIndex/index.js");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Maybe_First = require("../Data.Maybe.First/index.js");
var Data_Monoid = require("../Data.Monoid/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Ordering = require("../Data.Ordering/index.js");
var Data_Ring = require("../Data.Ring/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Semigroup_Foldable = require("../Data.Semigroup.Foldable/index.js");
var Data_Semigroup_Traversable = require("../Data.Semigroup.Traversable/index.js");
var Data_Semiring = require("../Data.Semiring/index.js");
var Data_Show = require("../Data.Show/index.js");
var Data_Traversable = require("../Data.Traversable/index.js");
var Data_TraversableWithIndex = require("../Data.TraversableWithIndex/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Prelude = require("../Prelude/index.js");
var Type_Equality = require("../Type.Equality/index.js");
var Tuple = (function () {
    function Tuple(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Tuple.create = function (value0) {
        return function (value1) {
            return new Tuple(value0, value1);
        };
    };
    return Tuple;
})();
var uncurry = function (f) {
    return function (v) {
        return f(v.value0)(v.value1);
    };
};
var swap = function (v) {
    return new Tuple(v.value1, v.value0);
};
var snd = function (v) {
    return v.value1;
};
var showTuple = function (dictShow) {
    return function (dictShow1) {
        return new Data_Show.Show(function (v) {
            return "(Tuple " + (Data_Show.show(dictShow)(v.value0) + (" " + (Data_Show.show(dictShow1)(v.value1) + ")")));
        });
    };
};
var semiringTuple = function (dictSemiring) {
    return function (dictSemiring1) {
        return new Data_Semiring.Semiring(function (v) {
            return function (v1) {
                return new Tuple(Data_Semiring.add(dictSemiring)(v.value0)(v1.value0), Data_Semiring.add(dictSemiring1)(v.value1)(v1.value1));
            };
        }, function (v) {
            return function (v1) {
                return new Tuple(Data_Semiring.mul(dictSemiring)(v.value0)(v1.value0), Data_Semiring.mul(dictSemiring1)(v.value1)(v1.value1));
            };
        }, new Tuple(Data_Semiring.one(dictSemiring), Data_Semiring.one(dictSemiring1)), new Tuple(Data_Semiring.zero(dictSemiring), Data_Semiring.zero(dictSemiring1)));
    };
};
var semigroupoidTuple = new Control_Semigroupoid.Semigroupoid(function (v) {
    return function (v1) {
        return new Tuple(v1.value0, v.value1);
    };
});
var semigroupTuple = function (dictSemigroup) {
    return function (dictSemigroup1) {
        return new Data_Semigroup.Semigroup(function (v) {
            return function (v1) {
                return new Tuple(Data_Semigroup.append(dictSemigroup)(v.value0)(v1.value0), Data_Semigroup.append(dictSemigroup1)(v.value1)(v1.value1));
            };
        });
    };
};
var ringTuple = function (dictRing) {
    return function (dictRing1) {
        return new Data_Ring.Ring(function () {
            return semiringTuple(dictRing.Semiring0())(dictRing1.Semiring0());
        }, function (v) {
            return function (v1) {
                return new Tuple(Data_Ring.sub(dictRing)(v.value0)(v1.value0), Data_Ring.sub(dictRing1)(v.value1)(v1.value1));
            };
        });
    };
};
var monoidTuple = function (dictMonoid) {
    return function (dictMonoid1) {
        return new Data_Monoid.Monoid(function () {
            return semigroupTuple(dictMonoid.Semigroup0())(dictMonoid1.Semigroup0());
        }, new Tuple(Data_Monoid.mempty(dictMonoid), Data_Monoid.mempty(dictMonoid1)));
    };
};
var lookup = function (dictFoldable) {
    return function (dictEq) {
        return function (a) {
            return function ($312) {
                return Data_Newtype.unwrap(Data_Maybe_First.newtypeFirst)(Data_Foldable.foldMap(dictFoldable)(Data_Maybe_First.monoidFirst)(function (v) {
                    var $163 = Data_Eq.eq(dictEq)(a)(v.value0);
                    if ($163) {
                        return new Data_Maybe.Just(v.value1);
                    };
                    return Data_Maybe.Nothing.value;
                })($312));
            };
        };
    };
};
var heytingAlgebraTuple = function (dictHeytingAlgebra) {
    return function (dictHeytingAlgebra1) {
        return new Data_HeytingAlgebra.HeytingAlgebra(function (v) {
            return function (v1) {
                return new Tuple(Data_HeytingAlgebra.conj(dictHeytingAlgebra)(v.value0)(v1.value0), Data_HeytingAlgebra.conj(dictHeytingAlgebra1)(v.value1)(v1.value1));
            };
        }, function (v) {
            return function (v1) {
                return new Tuple(Data_HeytingAlgebra.disj(dictHeytingAlgebra)(v.value0)(v1.value0), Data_HeytingAlgebra.disj(dictHeytingAlgebra1)(v.value1)(v1.value1));
            };
        }, new Tuple(Data_HeytingAlgebra.ff(dictHeytingAlgebra), Data_HeytingAlgebra.ff(dictHeytingAlgebra1)), function (v) {
            return function (v1) {
                return new Tuple(Data_HeytingAlgebra.implies(dictHeytingAlgebra)(v.value0)(v1.value0), Data_HeytingAlgebra.implies(dictHeytingAlgebra1)(v.value1)(v1.value1));
            };
        }, function (v) {
            return new Tuple(Data_HeytingAlgebra.not(dictHeytingAlgebra)(v.value0), Data_HeytingAlgebra.not(dictHeytingAlgebra1)(v.value1));
        }, new Tuple(Data_HeytingAlgebra.tt(dictHeytingAlgebra), Data_HeytingAlgebra.tt(dictHeytingAlgebra1)));
    };
};
var functorTuple = new Data_Functor.Functor(function (f) {
    return function (m) {
        return new Tuple(m.value0, f(m.value1));
    };
});
var functorWithIndexTuple = new Data_FunctorWithIndex.FunctorWithIndex(function () {
    return functorTuple;
}, function (f) {
    return Data_Functor.map(functorTuple)(f(Data_Unit.unit));
});
var invariantTuple = new Data_Functor_Invariant.Invariant(Data_Functor_Invariant.imapF(functorTuple));
var fst = function (v) {
    return v.value0;
};
var lazyTuple = function (dictLazy) {
    return function (dictLazy1) {
        return new Control_Lazy.Lazy(function (f) {
            return new Tuple(Control_Lazy.defer(dictLazy)(function (v) {
                return fst(f(Data_Unit.unit));
            }), Control_Lazy.defer(dictLazy1)(function (v) {
                return snd(f(Data_Unit.unit));
            }));
        });
    };
};
var foldableTuple = new Data_Foldable.Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            return f(v.value1);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(z)(v.value1);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(v.value1)(z);
        };
    };
});
var foldableWithIndexTuple = new Data_FoldableWithIndex.FoldableWithIndex(function () {
    return foldableTuple;
}, function (dictMonoid) {
    return function (f) {
        return function (v) {
            return f(Data_Unit.unit)(v.value1);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(Data_Unit.unit)(z)(v.value1);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(Data_Unit.unit)(v.value1)(z);
        };
    };
});
var traversableTuple = new Data_Traversable.Traversable(function () {
    return foldableTuple;
}, function () {
    return functorTuple;
}, function (dictApplicative) {
    return function (v) {
        return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Tuple.create(v.value0))(v.value1);
    };
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Tuple.create(v.value0))(f(v.value1));
        };
    };
});
var traversableWithIndexTuple = new Data_TraversableWithIndex.TraversableWithIndex(function () {
    return foldableWithIndexTuple;
}, function () {
    return functorWithIndexTuple;
}, function () {
    return traversableTuple;
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Tuple.create(v.value0))(f(Data_Unit.unit)(v.value1));
        };
    };
});
var foldable1Tuple = new Data_Semigroup_Foldable.Foldable1(function () {
    return foldableTuple;
}, function (dictSemigroup) {
    return function (v) {
        return v.value1;
    };
}, function (dictSemigroup) {
    return function (f) {
        return function (v) {
            return f(v.value1);
        };
    };
});
var traversable1Tuple = new Data_Semigroup_Traversable.Traversable1(function () {
    return foldable1Tuple;
}, function () {
    return traversableTuple;
}, function (dictApply) {
    return function (v) {
        return Data_Functor.map(dictApply.Functor0())(Tuple.create(v.value0))(v.value1);
    };
}, function (dictApply) {
    return function (f) {
        return function (v) {
            return Data_Functor.map(dictApply.Functor0())(Tuple.create(v.value0))(f(v.value1));
        };
    };
});
var extendTuple = new Control_Extend.Extend(function () {
    return functorTuple;
}, function (f) {
    return function (v) {
        return new Tuple(v.value0, f(v));
    };
});
var eqTuple = function (dictEq) {
    return function (dictEq1) {
        return new Data_Eq.Eq(function (x) {
            return function (y) {
                return Data_Eq.eq(dictEq)(x.value0)(y.value0) && Data_Eq.eq(dictEq1)(x.value1)(y.value1);
            };
        });
    };
};
var ordTuple = function (dictOrd) {
    return function (dictOrd1) {
        return new Data_Ord.Ord(function () {
            return eqTuple(dictOrd.Eq0())(dictOrd1.Eq0());
        }, function (x) {
            return function (y) {
                var v = Data_Ord.compare(dictOrd)(x.value0)(y.value0);
                if (v instanceof Data_Ordering.LT) {
                    return Data_Ordering.LT.value;
                };
                if (v instanceof Data_Ordering.GT) {
                    return Data_Ordering.GT.value;
                };
                return Data_Ord.compare(dictOrd1)(x.value1)(y.value1);
            };
        });
    };
};
var eq1Tuple = function (dictEq) {
    return new Data_Eq.Eq1(function (dictEq1) {
        return Data_Eq.eq(eqTuple(dictEq)(dictEq1));
    });
};
var ord1Tuple = function (dictOrd) {
    return new Data_Ord.Ord1(function () {
        return eq1Tuple(dictOrd.Eq0());
    }, function (dictOrd1) {
        return Data_Ord.compare(ordTuple(dictOrd)(dictOrd1));
    });
};
var distributiveTuple = function (dictTypeEquals) {
    return new Data_Distributive.Distributive(function () {
        return functorTuple;
    }, function (dictFunctor) {
        return Data_Distributive.collectDefault(distributiveTuple(dictTypeEquals))(dictFunctor);
    }, function (dictFunctor) {
        return function ($313) {
            return Tuple.create(Type_Equality.from(dictTypeEquals)(Data_Unit.unit))(Data_Functor.map(dictFunctor)(snd)($313));
        };
    });
};
var curry = function (f) {
    return function (a) {
        return function (b) {
            return f(new Tuple(a, b));
        };
    };
};
var comonadTuple = new Control_Comonad.Comonad(function () {
    return extendTuple;
}, snd);
var commutativeRingTuple = function (dictCommutativeRing) {
    return function (dictCommutativeRing1) {
        return new Data_CommutativeRing.CommutativeRing(function () {
            return ringTuple(dictCommutativeRing.Ring0())(dictCommutativeRing1.Ring0());
        });
    };
};
var boundedTuple = function (dictBounded) {
    return function (dictBounded1) {
        return new Data_Bounded.Bounded(function () {
            return ordTuple(dictBounded.Ord0())(dictBounded1.Ord0());
        }, new Tuple(Data_Bounded.bottom(dictBounded), Data_Bounded.bottom(dictBounded1)), new Tuple(Data_Bounded.top(dictBounded), Data_Bounded.top(dictBounded1)));
    };
};
var booleanAlgebraTuple = function (dictBooleanAlgebra) {
    return function (dictBooleanAlgebra1) {
        return new Data_BooleanAlgebra.BooleanAlgebra(function () {
            return heytingAlgebraTuple(dictBooleanAlgebra.HeytingAlgebra0())(dictBooleanAlgebra1.HeytingAlgebra0());
        });
    };
};
var bifunctorTuple = new Data_Bifunctor.Bifunctor(function (f) {
    return function (g) {
        return function (v) {
            return new Tuple(f(v.value0), g(v.value1));
        };
    };
});
var bifoldableTuple = new Data_Bifoldable.Bifoldable(function (dictMonoid) {
    return function (f) {
        return function (g) {
            return function (v) {
                return Data_Semigroup.append(dictMonoid.Semigroup0())(f(v.value0))(g(v.value1));
            };
        };
    };
}, function (f) {
    return function (g) {
        return function (z) {
            return function (v) {
                return g(f(z)(v.value0))(v.value1);
            };
        };
    };
}, function (f) {
    return function (g) {
        return function (z) {
            return function (v) {
                return f(v.value0)(g(v.value1)(z));
            };
        };
    };
});
var bitraversableTuple = new Data_Bitraversable.Bitraversable(function () {
    return bifoldableTuple;
}, function () {
    return bifunctorTuple;
}, function (dictApplicative) {
    return function (v) {
        return Control_Apply.apply(dictApplicative.Apply0())(Data_Functor.map((dictApplicative.Apply0()).Functor0())(Tuple.create)(v.value0))(v.value1);
    };
}, function (dictApplicative) {
    return function (f) {
        return function (g) {
            return function (v) {
                return Control_Apply.apply(dictApplicative.Apply0())(Data_Functor.map((dictApplicative.Apply0()).Functor0())(Tuple.create)(f(v.value0)))(g(v.value1));
            };
        };
    };
});
var biapplyTuple = new Control_Biapply.Biapply(function () {
    return bifunctorTuple;
}, function (v) {
    return function (v1) {
        return new Tuple(v.value0(v1.value0), v.value1(v1.value1));
    };
});
var biapplicativeTuple = new Control_Biapplicative.Biapplicative(function () {
    return biapplyTuple;
}, Tuple.create);
var applyTuple = function (dictSemigroup) {
    return new Control_Apply.Apply(function () {
        return functorTuple;
    }, function (v) {
        return function (v1) {
            return new Tuple(Data_Semigroup.append(dictSemigroup)(v.value0)(v1.value0), v.value1(v1.value1));
        };
    });
};
var bindTuple = function (dictSemigroup) {
    return new Control_Bind.Bind(function () {
        return applyTuple(dictSemigroup);
    }, function (v) {
        return function (f) {
            var v1 = f(v.value1);
            return new Tuple(Data_Semigroup.append(dictSemigroup)(v.value0)(v1.value0), v1.value1);
        };
    });
};
var applicativeTuple = function (dictMonoid) {
    return new Control_Applicative.Applicative(function () {
        return applyTuple(dictMonoid.Semigroup0());
    }, Tuple.create(Data_Monoid.mempty(dictMonoid)));
};
var monadTuple = function (dictMonoid) {
    return new Control_Monad.Monad(function () {
        return applicativeTuple(dictMonoid);
    }, function () {
        return bindTuple(dictMonoid.Semigroup0());
    });
};
module.exports = {
    Tuple: Tuple,
    fst: fst,
    snd: snd,
    curry: curry,
    uncurry: uncurry,
    swap: swap,
    lookup: lookup,
    showTuple: showTuple,
    eqTuple: eqTuple,
    eq1Tuple: eq1Tuple,
    ordTuple: ordTuple,
    ord1Tuple: ord1Tuple,
    boundedTuple: boundedTuple,
    semigroupoidTuple: semigroupoidTuple,
    semigroupTuple: semigroupTuple,
    monoidTuple: monoidTuple,
    semiringTuple: semiringTuple,
    ringTuple: ringTuple,
    commutativeRingTuple: commutativeRingTuple,
    heytingAlgebraTuple: heytingAlgebraTuple,
    booleanAlgebraTuple: booleanAlgebraTuple,
    functorTuple: functorTuple,
    functorWithIndexTuple: functorWithIndexTuple,
    invariantTuple: invariantTuple,
    bifunctorTuple: bifunctorTuple,
    applyTuple: applyTuple,
    biapplyTuple: biapplyTuple,
    applicativeTuple: applicativeTuple,
    biapplicativeTuple: biapplicativeTuple,
    bindTuple: bindTuple,
    monadTuple: monadTuple,
    extendTuple: extendTuple,
    comonadTuple: comonadTuple,
    lazyTuple: lazyTuple,
    foldableTuple: foldableTuple,
    foldable1Tuple: foldable1Tuple,
    foldableWithIndexTuple: foldableWithIndexTuple,
    bifoldableTuple: bifoldableTuple,
    traversableTuple: traversableTuple,
    traversable1Tuple: traversable1Tuple,
    traversableWithIndexTuple: traversableWithIndexTuple,
    bitraversableTuple: bitraversableTuple,
    distributiveTuple: distributiveTuple
};

},{"../Control.Applicative/index.js":27,"../Control.Apply/index.js":29,"../Control.Biapplicative/index.js":30,"../Control.Biapply/index.js":31,"../Control.Bind/index.js":33,"../Control.Comonad/index.js":35,"../Control.Extend/index.js":37,"../Control.Lazy/index.js":38,"../Control.Monad/index.js":52,"../Control.Semigroupoid/index.js":56,"../Data.Bifoldable/index.js":65,"../Data.Bifunctor/index.js":71,"../Data.Bitraversable/index.js":72,"../Data.BooleanAlgebra/index.js":74,"../Data.Bounded/index.js":76,"../Data.CommutativeRing/index.js":77,"../Data.Distributive/index.js":78,"../Data.Eq/index.js":82,"../Data.Foldable/index.js":87,"../Data.FoldableWithIndex/index.js":88,"../Data.Function/index.js":91,"../Data.Functor.Invariant/index.js":93,"../Data.Functor/index.js":95,"../Data.FunctorWithIndex/index.js":97,"../Data.HeytingAlgebra/index.js":99,"../Data.Maybe.First/index.js":108,"../Data.Maybe/index.js":110,"../Data.Monoid/index.js":117,"../Data.Newtype/index.js":119,"../Data.Ord/index.js":130,"../Data.Ordering/index.js":131,"../Data.Ring/index.js":133,"../Data.Semigroup.Foldable/index.js":135,"../Data.Semigroup.Traversable/index.js":137,"../Data.Semigroup/index.js":139,"../Data.Semiring/index.js":141,"../Data.Show/index.js":143,"../Data.Traversable/index.js":154,"../Data.TraversableWithIndex/index.js":155,"../Data.Unit/index.js":162,"../Prelude/index.js":198,"../Type.Equality/index.js":206}],157:[function(require,module,exports){
"use strict";

exports.unfoldrArrayImpl = function (isNothing) {
  return function (fromJust) {
    return function (fst) {
      return function (snd) {
        return function (f) {
          return function (b) {
            var result = [];
            var value = b;
            while (true) { // eslint-disable-line no-constant-condition
              var maybe = f(value);
              if (isNothing(maybe)) return result;
              var tuple = fromJust(maybe);
              result.push(fst(tuple));
              value = snd(tuple);
            }
          };
        };
      };
    };
  };
};

},{}],158:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var $foreign = require("./foreign.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Ring = require("../Data.Ring/index.js");
var Data_Traversable = require("../Data.Traversable/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var Data_Unfoldable1 = require("../Data.Unfoldable1/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Partial_Unsafe = require("../Partial.Unsafe/index.js");
var Prelude = require("../Prelude/index.js");
var Unfoldable = function (Unfoldable10, unfoldr) {
    this.Unfoldable10 = Unfoldable10;
    this.unfoldr = unfoldr;
};
var unfoldr = function (dict) {
    return dict.unfoldr;
};
var unfoldableArray = new Unfoldable(function () {
    return Data_Unfoldable1.unfoldable1Array;
}, $foreign.unfoldrArrayImpl(Data_Maybe.isNothing)(Data_Maybe.fromJust())(Data_Tuple.fst)(Data_Tuple.snd));
var replicate = function (dictUnfoldable) {
    return function (n) {
        return function (v) {
            var step = function (i) {
                var $7 = i <= 0;
                if ($7) {
                    return Data_Maybe.Nothing.value;
                };
                return new Data_Maybe.Just(new Data_Tuple.Tuple(v, i - 1 | 0));
            };
            return unfoldr(dictUnfoldable)(step)(n);
        };
    };
};
var replicateA = function (dictApplicative) {
    return function (dictUnfoldable) {
        return function (dictTraversable) {
            return function (n) {
                return function (m) {
                    return Data_Traversable.sequence(dictTraversable)(dictApplicative)(replicate(dictUnfoldable)(n)(m));
                };
            };
        };
    };
};
var none = function (dictUnfoldable) {
    return unfoldr(dictUnfoldable)(Data_Function["const"](Data_Maybe.Nothing.value))(Data_Unit.unit);
};
var fromMaybe = function (dictUnfoldable) {
    return unfoldr(dictUnfoldable)(function (b) {
        return Data_Functor.map(Data_Maybe.functorMaybe)(Data_Function.flip(Data_Tuple.Tuple.create)(Data_Maybe.Nothing.value))(b);
    });
};
module.exports = {
    Unfoldable: Unfoldable,
    unfoldr: unfoldr,
    replicate: replicate,
    replicateA: replicateA,
    none: none,
    fromMaybe: fromMaybe,
    unfoldableArray: unfoldableArray
};

},{"../Data.Function/index.js":91,"../Data.Functor/index.js":95,"../Data.Maybe/index.js":110,"../Data.Ord/index.js":130,"../Data.Ring/index.js":133,"../Data.Traversable/index.js":154,"../Data.Tuple/index.js":156,"../Data.Unfoldable1/index.js":160,"../Data.Unit/index.js":162,"../Partial.Unsafe/index.js":195,"../Prelude/index.js":198,"./foreign.js":157}],159:[function(require,module,exports){
"use strict";

exports.unfoldr1ArrayImpl = function (isNothing) {
  return function (fromJust) {
    return function (fst) {
      return function (snd) {
        return function (f) {
          return function (b) {
            var result = [];
            var value = b;
            while (true) { // eslint-disable-line no-constant-condition
              var tuple = f(value);
              result.push(fst(tuple));
              var maybe = snd(tuple);
              if (isNothing(maybe)) return result;
              value = fromJust(maybe);
            }
          };
        };
      };
    };
  };
};

},{}],160:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var $foreign = require("./foreign.js");
var Data_Boolean = require("../Data.Boolean/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Ring = require("../Data.Ring/index.js");
var Data_Semigroup_Traversable = require("../Data.Semigroup.Traversable/index.js");
var Data_Semiring = require("../Data.Semiring/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var Partial_Unsafe = require("../Partial.Unsafe/index.js");
var Prelude = require("../Prelude/index.js");
var Unfoldable1 = function (unfoldr1) {
    this.unfoldr1 = unfoldr1;
};
var unfoldr1 = function (dict) {
    return dict.unfoldr1;
};
var unfoldable1Array = new Unfoldable1($foreign.unfoldr1ArrayImpl(Data_Maybe.isNothing)(Data_Maybe.fromJust())(Data_Tuple.fst)(Data_Tuple.snd));
var replicate1 = function (dictUnfoldable1) {
    return function (n) {
        return function (v) {
            var step = function (i) {
                if (i <= 0) {
                    return new Data_Tuple.Tuple(v, Data_Maybe.Nothing.value);
                };
                if (Data_Boolean.otherwise) {
                    return new Data_Tuple.Tuple(v, new Data_Maybe.Just(i - 1 | 0));
                };
                throw new Error("Failed pattern match at Data.Unfoldable1 (line 47, column 5 - line 47, column 39): " + [ i.constructor.name ]);
            };
            return unfoldr1(dictUnfoldable1)(step)(n - 1 | 0);
        };
    };
};
var replicate1A = function (dictApply) {
    return function (dictUnfoldable1) {
        return function (dictTraversable1) {
            return function (n) {
                return function (m) {
                    return Data_Semigroup_Traversable.sequence1(dictTraversable1)(dictApply)(replicate1(dictUnfoldable1)(n)(m));
                };
            };
        };
    };
};
var singleton = function (dictUnfoldable1) {
    return replicate1(dictUnfoldable1)(1);
};
var range = function (dictUnfoldable1) {
    return function (start) {
        return function (end) {
            var go = function (delta) {
                return function (i) {
                    var i$prime = i + delta | 0;
                    return new Data_Tuple.Tuple(i, (function () {
                        var $8 = i === end;
                        if ($8) {
                            return Data_Maybe.Nothing.value;
                        };
                        return new Data_Maybe.Just(i$prime);
                    })());
                };
            };
            var delta = (function () {
                var $9 = end >= start;
                if ($9) {
                    return 1;
                };
                return -1 | 0;
            })();
            return unfoldr1(dictUnfoldable1)(go(delta))(start);
        };
    };
};
module.exports = {
    Unfoldable1: Unfoldable1,
    unfoldr1: unfoldr1,
    replicate1: replicate1,
    replicate1A: replicate1A,
    singleton: singleton,
    range: range,
    unfoldable1Array: unfoldable1Array
};

},{"../Data.Boolean/index.js":73,"../Data.Eq/index.js":82,"../Data.Maybe/index.js":110,"../Data.Ord/index.js":130,"../Data.Ring/index.js":133,"../Data.Semigroup.Traversable/index.js":137,"../Data.Semiring/index.js":141,"../Data.Tuple/index.js":156,"../Partial.Unsafe/index.js":195,"../Prelude/index.js":198,"./foreign.js":159}],161:[function(require,module,exports){
"use strict";

exports.unit = {};

},{}],162:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var $foreign = require("./foreign.js");
var Data_Show = require("../Data.Show/index.js");
var showUnit = new Data_Show.Show(function (v) {
    return "unit";
});
module.exports = {
    showUnit: showUnit,
    unit: $foreign.unit
};

},{"../Data.Show/index.js":143,"./foreign.js":161}],163:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Data_Show = require("../Data.Show/index.js");
var Void = function (x) {
    return x;
};
var absurd = function (a) {
    var spin = function ($copy_v) {
        var $tco_result;
        function $tco_loop(v) {
            $copy_v = v;
            return;
        };
        while (!false) {
            $tco_result = $tco_loop($copy_v);
        };
        return $tco_result;
    };
    return spin(a);
};
var showVoid = new Data_Show.Show(absurd);
module.exports = {
    absurd: absurd,
    showVoid: showVoid
};

},{"../Data.Show/index.js":143}],164:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Category = require("../Control.Category/index.js");
var Control_Monad = require("../Control.Monad/index.js");
var Effect = require("../Effect/index.js");
var MonadEffect = function (Monad0, liftEffect) {
    this.Monad0 = Monad0;
    this.liftEffect = liftEffect;
};
var monadEffectEffect = new MonadEffect(function () {
    return Effect.monadEffect;
}, Control_Category.identity(Control_Category.categoryFn));
var liftEffect = function (dict) {
    return dict.liftEffect;
};
module.exports = {
    liftEffect: liftEffect,
    MonadEffect: MonadEffect,
    monadEffectEffect: monadEffectEffect
};

},{"../Control.Category/index.js":34,"../Control.Monad/index.js":52,"../Effect/index.js":172}],165:[function(require,module,exports){
"use strict";

exports.log = function (s) {
  return function () {
    console.log(s);
    return {};
  };
};

exports.warn = function (s) {
  return function () {
    console.warn(s);
    return {};
  };
};

exports.error = function (s) {
  return function () {
    console.error(s);
    return {};
  };
};

exports.info = function (s) {
  return function () {
    console.info(s);
    return {};
  };
};

exports.time = function (s) {
  return function () {
    console.time(s);
    return {};
  };
};

exports.timeEnd = function (s) {
  return function () {
    console.timeEnd(s);
    return {};
  };
};

},{}],166:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var $foreign = require("./foreign.js");
var Data_Show = require("../Data.Show/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Effect = require("../Effect/index.js");
var warnShow = function (dictShow) {
    return function (a) {
        return $foreign.warn(Data_Show.show(dictShow)(a));
    };
};
var logShow = function (dictShow) {
    return function (a) {
        return $foreign.log(Data_Show.show(dictShow)(a));
    };
};
var infoShow = function (dictShow) {
    return function (a) {
        return $foreign.info(Data_Show.show(dictShow)(a));
    };
};
var errorShow = function (dictShow) {
    return function (a) {
        return $foreign.error(Data_Show.show(dictShow)(a));
    };
};
module.exports = {
    logShow: logShow,
    warnShow: warnShow,
    errorShow: errorShow,
    infoShow: infoShow,
    log: $foreign.log,
    warn: $foreign.warn,
    error: $foreign.error,
    info: $foreign.info,
    time: $foreign.time,
    timeEnd: $foreign.timeEnd
};

},{"../Data.Show/index.js":143,"../Data.Unit/index.js":162,"../Effect/index.js":172,"./foreign.js":165}],167:[function(require,module,exports){
"use strict";

exports.showErrorImpl = function (err) {
  return err.stack || err.toString();
};

exports.error = function (msg) {
  return new Error(msg);
};

exports.message = function (e) {
  return e.message;
};

exports.name = function (e) {
  return e.name || "Error";
};

exports.stackImpl = function (just) {
  return function (nothing) {
    return function (e) {
      return e.stack ? just(e.stack) : nothing;
    };
  };
};

exports.throwException = function (e) {
  return function () {
    throw e;
  };
};

exports.catchException = function (c) {
  return function (t) {
    return function () {
      try {
        return t();
      } catch (e) {
        if (e instanceof Error || Object.prototype.toString.call(e) === "[object Error]") {
          return c(e)();
        } else {
          return c(new Error(e.toString()))();
        }
      }
    };
  };
};

},{}],168:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var $foreign = require("./foreign.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Semigroupoid = require("../Control.Semigroupoid/index.js");
var Data_Either = require("../Data.Either/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Show = require("../Data.Show/index.js");
var Effect = require("../Effect/index.js");
var Prelude = require("../Prelude/index.js");
var $$try = function (action) {
    return $foreign.catchException(function ($0) {
        return Control_Applicative.pure(Effect.applicativeEffect)(Data_Either.Left.create($0));
    })(Data_Functor.map(Effect.functorEffect)(Data_Either.Right.create)(action));
};
var $$throw = function ($1) {
    return $foreign.throwException($foreign.error($1));
};
var stack = $foreign.stackImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var showError = new Data_Show.Show($foreign.showErrorImpl);
module.exports = {
    stack: stack,
    "throw": $$throw,
    "try": $$try,
    showError: showError,
    error: $foreign.error,
    message: $foreign.message,
    name: $foreign.name,
    throwException: $foreign.throwException,
    catchException: $foreign.catchException
};

},{"../Control.Applicative/index.js":27,"../Control.Semigroupoid/index.js":56,"../Data.Either/index.js":80,"../Data.Functor/index.js":95,"../Data.Maybe/index.js":110,"../Data.Show/index.js":143,"../Effect/index.js":172,"../Prelude/index.js":198,"./foreign.js":167}],169:[function(require,module,exports){
"use strict";

exports.new = function (val) {
  return function () {
    return { value: val };
  };
};

exports.read = function (ref) {
  return function () {
    return ref.value;
  };
};

exports["modify'"] = function (f) {
  return function (ref) {
    return function () {
      var t = f(ref.value);
      ref.value = t.state;
      return t.value;
    };
  };
};

exports.write = function (val) {
  return function (ref) {
    return function () {
      ref.value = val;
      return {};
    };
  };
};

},{}],170:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var $foreign = require("./foreign.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Effect = require("../Effect/index.js");
var Prelude = require("../Prelude/index.js");
var modify = function (f) {
    return $foreign["modify'"](function (s) {
        var s$prime = f(s);
        return {
            state: s$prime,
            value: s$prime
        };
    });
};
var modify_ = function (f) {
    return function (s) {
        return Data_Functor["void"](Effect.functorEffect)(modify(f)(s));
    };
};
module.exports = {
    modify: modify,
    modify_: modify_,
    "new": $foreign["new"],
    read: $foreign.read,
    "modify'": $foreign["modify'"],
    write: $foreign.write
};

},{"../Data.Function/index.js":91,"../Data.Functor/index.js":95,"../Effect/index.js":172,"../Prelude/index.js":198,"./foreign.js":169}],171:[function(require,module,exports){
"use strict";

exports.pureE = function (a) {
  return function () {
    return a;
  };
};

exports.bindE = function (a) {
  return function (f) {
    return function () {
      return f(a())();
    };
  };
};

exports.untilE = function (f) {
  return function () {
    while (!f());
    return {};
  };
};

exports.whileE = function (f) {
  return function (a) {
    return function () {
      while (f()) {
        a();
      }
      return {};
    };
  };
};

exports.forE = function (lo) {
  return function (hi) {
    return function (f) {
      return function () {
        for (var i = lo; i < hi; i++) {
          f(i)();
        }
      };
    };
  };
};

exports.foreachE = function (as) {
  return function (f) {
    return function () {
      for (var i = 0, l = as.length; i < l; i++) {
        f(as[i])();
      }
    };
  };
};

},{}],172:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var $foreign = require("./foreign.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Monad = require("../Control.Monad/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Monoid = require("../Data.Monoid/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Prelude = require("../Prelude/index.js");
var monadEffect = new Control_Monad.Monad(function () {
    return applicativeEffect;
}, function () {
    return bindEffect;
});
var bindEffect = new Control_Bind.Bind(function () {
    return applyEffect;
}, $foreign.bindE);
var applyEffect = new Control_Apply.Apply(function () {
    return functorEffect;
}, Control_Monad.ap(monadEffect));
var applicativeEffect = new Control_Applicative.Applicative(function () {
    return applyEffect;
}, $foreign.pureE);
var functorEffect = new Data_Functor.Functor(Control_Applicative.liftA1(applicativeEffect));
var semigroupEffect = function (dictSemigroup) {
    return new Data_Semigroup.Semigroup(Control_Apply.lift2(applyEffect)(Data_Semigroup.append(dictSemigroup)));
};
var monoidEffect = function (dictMonoid) {
    return new Data_Monoid.Monoid(function () {
        return semigroupEffect(dictMonoid.Semigroup0());
    }, $foreign.pureE(Data_Monoid.mempty(dictMonoid)));
};
module.exports = {
    functorEffect: functorEffect,
    applyEffect: applyEffect,
    applicativeEffect: applicativeEffect,
    bindEffect: bindEffect,
    monadEffect: monadEffect,
    semigroupEffect: semigroupEffect,
    monoidEffect: monoidEffect,
    untilE: $foreign.untilE,
    whileE: $foreign.whileE,
    forE: $foreign.forE,
    foreachE: $foreign.foreachE
};

},{"../Control.Applicative/index.js":27,"../Control.Apply/index.js":29,"../Control.Bind/index.js":33,"../Control.Monad/index.js":52,"../Data.Functor/index.js":95,"../Data.Monoid/index.js":117,"../Data.Semigroup/index.js":139,"../Prelude/index.js":198,"./foreign.js":171}],173:[function(require,module,exports){
"use strict";

exports["new"] = function () {
  return {};
};

exports.peekImpl = function (just) {
  return function (nothing) {
    return function (k) {
      return function (m) {
        return function () {
          return {}.hasOwnProperty.call(m, k) ? just(m[k]) : nothing;
        };
      };
    };
  };
};

exports.poke = function (k) {
  return function (v) {
    return function (m) {
      return function () {
        m[k] = v;
        return m;
      };
    };
  };
};

exports["delete"] = function (k) {
  return function (m) {
    return function () {
      delete m[k];
      return m;
    };
  };
};

},{}],174:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var $foreign = require("./foreign.js");
var Control_Monad_ST = require("../Control.Monad.ST/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var peek = $foreign.peekImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
module.exports = {
    peek: peek,
    "new": $foreign["new"],
    poke: $foreign.poke,
    "delete": $foreign["delete"]
};

},{"../Control.Monad.ST/index.js":48,"../Data.Maybe/index.js":110,"./foreign.js":173}],175:[function(require,module,exports){
"use strict";

exports._copyST = function (m) {
  return function () {
    var r = {};
    for (var k in m) {
      if (hasOwnProperty.call(m, k)) {
        r[k] = m[k];
      }
    }
    return r;
  };
};

exports.empty = {};

exports.runST = function (f) {
  return f();
};

exports._fmapObject = function (m0, f) {
  var m = {};
  for (var k in m0) {
    if (hasOwnProperty.call(m0, k)) {
      m[k] = f(m0[k]);
    }
  }
  return m;
};

exports._mapWithKey = function (m0, f) {
  var m = {};
  for (var k in m0) {
    if (hasOwnProperty.call(m0, k)) {
      m[k] = f(k)(m0[k]);
    }
  }
  return m;
};

exports._foldM = function (bind) {
  return function (f) {
    return function (mz) {
      return function (m) {
        var acc = mz;
        function g(k) {
          return function (z) {
            return f(z)(k)(m[k]);
          };
        }
        for (var k in m) {
          if (hasOwnProperty.call(m, k)) {
            acc = bind(acc)(g(k));
          }
        }
        return acc;
      };
    };
  };
};

exports._foldSCObject = function (m, z, f, fromMaybe) {
  var acc = z;
  for (var k in m) {
    if (hasOwnProperty.call(m, k)) {
      var maybeR = f(acc)(k)(m[k]);
      var r = fromMaybe(null)(maybeR);
      if (r === null) return acc;
      else acc = r;
    }
  }
  return acc;
};

exports.all = function (f) {
  return function (m) {
    for (var k in m) {
      if (hasOwnProperty.call(m, k) && !f(k)(m[k])) return false;
    }
    return true;
  };
};

exports.size = function (m) {
  var s = 0;
  for (var k in m) {
    if (hasOwnProperty.call(m, k)) {
      ++s;
    }
  }
  return s;
};

exports._lookup = function (no, yes, k, m) {
  return k in m ? yes(m[k]) : no;
};

exports._unsafeDeleteObject = function (m, k) {
  delete m[k];
  return m;
};

exports._lookupST = function (no, yes, k, m) {
  return function () {
    return k in m ? yes(m[k]) : no;
  };
};

function toArrayWithKey(f) {
  return function (m) {
    var r = [];
    for (var k in m) {
      if (hasOwnProperty.call(m, k)) {
        r.push(f(k)(m[k]));
      }
    }
    return r;
  };
}

exports.toArrayWithKey = toArrayWithKey;

exports.keys = Object.keys || toArrayWithKey(function (k) {
  return function () { return k; };
});

},{}],176:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var $foreign = require("./foreign.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Category = require("../Control.Category/index.js");
var Control_Monad_ST = require("../Control.Monad.ST/index.js");
var Control_Monad_ST_Internal = require("../Control.Monad.ST.Internal/index.js");
var Control_Semigroupoid = require("../Control.Semigroupoid/index.js");
var Data_Array = require("../Data.Array/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_FoldableWithIndex = require("../Data.FoldableWithIndex/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Function_Uncurried = require("../Data.Function.Uncurried/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_FunctorWithIndex = require("../Data.FunctorWithIndex/index.js");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Monoid = require("../Data.Monoid/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Show = require("../Data.Show/index.js");
var Data_Traversable = require("../Data.Traversable/index.js");
var Data_TraversableWithIndex = require("../Data.TraversableWithIndex/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var Data_Unfoldable = require("../Data.Unfoldable/index.js");
var Foreign_Object_ST = require("../Foreign.Object.ST/index.js");
var Prelude = require("../Prelude/index.js");
var Type_Row_Homogeneous = require("../Type.Row.Homogeneous/index.js");
var Unsafe_Coerce = require("../Unsafe.Coerce/index.js");
var values = $foreign.toArrayWithKey(function (v) {
    return function (v1) {
        return v1;
    };
});
var toUnfoldable = function (dictUnfoldable) {
    return function ($45) {
        return Data_Array.toUnfoldable(dictUnfoldable)($foreign.toArrayWithKey(Data_Tuple.Tuple.create)($45));
    };
};
var toAscUnfoldable = function (dictUnfoldable) {
    return function ($46) {
        return Data_Array.toUnfoldable(dictUnfoldable)(Data_Array.sortWith(Data_Ord.ordString)(Data_Tuple.fst)($foreign.toArrayWithKey(Data_Tuple.Tuple.create)($46)));
    };
};
var toAscArray = toAscUnfoldable(Data_Unfoldable.unfoldableArray);
var toArray = $foreign.toArrayWithKey(Data_Tuple.Tuple.create);
var thawST = $foreign["_copyST"];
var singleton = function (k) {
    return function (v) {
        return $foreign.runST(Control_Bind.bindFlipped(Control_Monad_ST_Internal.bindST)(Foreign_Object_ST.poke(k)(v))(Foreign_Object_ST["new"]));
    };
};
var showObject = function (dictShow) {
    return new Data_Show.Show(function (m) {
        return "(fromFoldable " + (Data_Show.show(Data_Show.showArray(Data_Tuple.showTuple(Data_Show.showString)(dictShow)))(toArray(m)) + ")");
    });
};
var mutate = function (f) {
    return function (m) {
        return $foreign.runST(function __do() {
            var v = thawST(m)();
            var v1 = f(v)();
            return v;
        });
    };
};
var member = Data_Function_Uncurried.runFn4($foreign["_lookup"])(false)(Data_Function["const"](true));
var mapWithKey = function (f) {
    return function (m) {
        return $foreign["_mapWithKey"](m, f);
    };
};
var lookup = Data_Function_Uncurried.runFn4($foreign["_lookup"])(Data_Maybe.Nothing.value)(Data_Maybe.Just.create);
var isSubmap = function (dictEq) {
    return function (m1) {
        return function (m2) {
            var f = function (k) {
                return function (v) {
                    return $foreign["_lookup"](false, Data_Eq.eq(dictEq)(v), k, m2);
                };
            };
            return $foreign.all(f)(m1);
        };
    };
};
var isEmpty = $foreign.all(function (v) {
    return function (v1) {
        return false;
    };
});
var insert = function (k) {
    return function (v) {
        return mutate(Foreign_Object_ST.poke(k)(v));
    };
};
var functorObject = new Data_Functor.Functor(function (f) {
    return function (m) {
        return $foreign["_fmapObject"](m, f);
    };
});
var functorWithIndexObject = new Data_FunctorWithIndex.FunctorWithIndex(function () {
    return functorObject;
}, mapWithKey);
var fromHomogeneous = function (dictHomogeneous) {
    return Unsafe_Coerce.unsafeCoerce;
};
var fromFoldableWith = function (dictFoldable) {
    return function (f) {
        return function (l) {
            return $foreign.runST(function __do() {
                var v = Foreign_Object_ST["new"]();
                Data_Foldable.for_(Control_Monad_ST_Internal.applicativeST)(dictFoldable)(l)(function (v1) {
                    return function __do() {
                        var v$prime = $foreign["_lookupST"](v1.value1, f(v1.value1), v1.value0, v)();
                        return Foreign_Object_ST.poke(v1.value0)(v$prime)(v)();
                    };
                })();
                return v;
            });
        };
    };
};
var fromFoldable = function (dictFoldable) {
    return function (l) {
        return $foreign.runST(function __do() {
            var v = Foreign_Object_ST["new"]();
            Data_Foldable.for_(Control_Monad_ST_Internal.applicativeST)(Data_Foldable.foldableArray)(Data_Array.fromFoldable(dictFoldable)(l))(function (v1) {
                return Foreign_Object_ST.poke(v1.value0)(v1.value1)(v);
            })();
            return v;
        });
    };
};
var freezeST = $foreign["_copyST"];
var foldMaybe = function (f) {
    return function (z) {
        return function (m) {
            return $foreign["_foldSCObject"](m, z, f, Data_Maybe.fromMaybe);
        };
    };
};
var foldM = function (dictMonad) {
    return function (f) {
        return function (z) {
            return $foreign["_foldM"](Control_Bind.bind(dictMonad.Bind1()))(f)(Control_Applicative.pure(dictMonad.Applicative0())(z));
        };
    };
};
var semigroupObject = function (dictSemigroup) {
    return new Data_Semigroup.Semigroup(function (m1) {
        return function (m2) {
            return mutate(function (s1) {
                return foldM(Control_Monad_ST_Internal.monadST)(function (s2) {
                    return function (k) {
                        return function (v2) {
                            return Foreign_Object_ST.poke(k)($foreign["_lookup"](v2, function (v1) {
                                return Data_Semigroup.append(dictSemigroup)(v1)(v2);
                            }, k, m2))(s2);
                        };
                    };
                })(s1)(m1);
            })(m2);
        };
    });
};
var monoidObject = function (dictSemigroup) {
    return new Data_Monoid.Monoid(function () {
        return semigroupObject(dictSemigroup);
    }, $foreign.empty);
};
var union = function (m) {
    return mutate(function (s) {
        return foldM(Control_Monad_ST_Internal.monadST)(function (s$prime) {
            return function (k) {
                return function (v) {
                    return Foreign_Object_ST.poke(k)(v)(s$prime);
                };
            };
        })(s)(m);
    });
};
var unions = function (dictFoldable) {
    return Data_Foldable.foldl(dictFoldable)(union)($foreign.empty);
};
var fold = $foreign["_foldM"](Data_Function.applyFlipped);
var foldMap = function (dictMonoid) {
    return function (f) {
        return fold(function (acc) {
            return function (k) {
                return function (v) {
                    return Data_Semigroup.append(dictMonoid.Semigroup0())(acc)(f(k)(v));
                };
            };
        })(Data_Monoid.mempty(dictMonoid));
    };
};
var foldableObject = new Data_Foldable.Foldable(function (dictMonoid) {
    return function (f) {
        return foldMap(dictMonoid)(Data_Function["const"](f));
    };
}, function (f) {
    return fold(function (z) {
        return function (v) {
            return f(z);
        };
    });
}, function (f) {
    return function (z) {
        return function (m) {
            return Data_Foldable.foldr(Data_Foldable.foldableArray)(f)(z)(values(m));
        };
    };
});
var foldableWithIndexObject = new Data_FoldableWithIndex.FoldableWithIndex(function () {
    return foldableObject;
}, function (dictMonoid) {
    return foldMap(dictMonoid);
}, function (f) {
    return fold(Data_Function.flip(f));
}, function (f) {
    return function (z) {
        return function (m) {
            return Data_Foldable.foldr(Data_Foldable.foldableArray)(Data_Tuple.uncurry(f))(z)($foreign.toArrayWithKey(Data_Tuple.Tuple.create)(m));
        };
    };
});
var traversableWithIndexObject = new Data_TraversableWithIndex.TraversableWithIndex(function () {
    return foldableWithIndexObject;
}, function () {
    return functorWithIndexObject;
}, function () {
    return traversableObject;
}, function (dictApplicative) {
    return function (f) {
        return function (ms) {
            return fold(function (acc) {
                return function (k) {
                    return function (v) {
                        return Control_Apply.apply(dictApplicative.Apply0())(Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Function.flip(insert(k)))(acc))(f(k)(v));
                    };
                };
            })(Control_Applicative.pure(dictApplicative)($foreign.empty))(ms);
        };
    };
});
var traversableObject = new Data_Traversable.Traversable(function () {
    return foldableObject;
}, function () {
    return functorObject;
}, function (dictApplicative) {
    return Data_Traversable.traverse(traversableObject)(dictApplicative)(Control_Category.identity(Control_Category.categoryFn));
}, function (dictApplicative) {
    return function ($47) {
        return Data_TraversableWithIndex.traverseWithIndex(traversableWithIndexObject)(dictApplicative)(Data_Function["const"]($47));
    };
});
var filterWithKey = function (predicate) {
    return function (m) {
        var go = (function () {
            var step = function (acc) {
                return function (k) {
                    return function (v) {
                        var $41 = predicate(k)(v);
                        if ($41) {
                            return Foreign_Object_ST.poke(k)(v)(acc);
                        };
                        return Control_Applicative.pure(Control_Monad_ST_Internal.applicativeST)(acc);
                    };
                };
            };
            return function __do() {
                var v = Foreign_Object_ST["new"]();
                return foldM(Control_Monad_ST_Internal.monadST)(step)(v)(m)();
            };
        })();
        return $foreign.runST(go);
    };
};
var filterKeys = function (predicate) {
    return filterWithKey(function ($48) {
        return Data_Function["const"](predicate($48));
    });
};
var filter = function (predicate) {
    return filterWithKey(Data_Function["const"](predicate));
};
var eqObject = function (dictEq) {
    return new Data_Eq.Eq(function (m1) {
        return function (m2) {
            return isSubmap(dictEq)(m1)(m2) && isSubmap(dictEq)(m2)(m1);
        };
    });
};
var ordObject = function (dictOrd) {
    return new Data_Ord.Ord(function () {
        return eqObject(dictOrd.Eq0());
    }, function (m1) {
        return function (m2) {
            return Data_Ord.compare(Data_Ord.ordArray(Data_Tuple.ordTuple(Data_Ord.ordString)(dictOrd)))(toAscArray(m1))(toAscArray(m2));
        };
    });
};
var eq1Object = new Data_Eq.Eq1(function (dictEq) {
    return Data_Eq.eq(eqObject(dictEq));
});
var $$delete = function (k) {
    return mutate(Foreign_Object_ST["delete"](k));
};
var pop = function (k) {
    return function (m) {
        return Data_Functor.mapFlipped(Data_Maybe.functorMaybe)(lookup(k)(m))(function (a) {
            return new Data_Tuple.Tuple(a, $$delete(k)(m));
        });
    };
};
var alter = function (f) {
    return function (k) {
        return function (m) {
            var v = f(lookup(k)(m));
            if (v instanceof Data_Maybe.Nothing) {
                return $$delete(k)(m);
            };
            if (v instanceof Data_Maybe.Just) {
                return insert(k)(v.value0)(m);
            };
            throw new Error("Failed pattern match at Foreign.Object (line 207, column 15 - line 209, column 25): " + [ v.constructor.name ]);
        };
    };
};
var update = function (f) {
    return function (k) {
        return function (m) {
            return alter(Data_Maybe.maybe(Data_Maybe.Nothing.value)(f))(k)(m);
        };
    };
};
module.exports = {
    isEmpty: isEmpty,
    singleton: singleton,
    insert: insert,
    lookup: lookup,
    toUnfoldable: toUnfoldable,
    toAscUnfoldable: toAscUnfoldable,
    fromFoldable: fromFoldable,
    fromFoldableWith: fromFoldableWith,
    fromHomogeneous: fromHomogeneous,
    "delete": $$delete,
    pop: pop,
    member: member,
    alter: alter,
    update: update,
    mapWithKey: mapWithKey,
    filterWithKey: filterWithKey,
    filterKeys: filterKeys,
    filter: filter,
    values: values,
    union: union,
    unions: unions,
    isSubmap: isSubmap,
    fold: fold,
    foldMap: foldMap,
    foldM: foldM,
    foldMaybe: foldMaybe,
    thawST: thawST,
    freezeST: freezeST,
    functorObject: functorObject,
    functorWithIndexObject: functorWithIndexObject,
    foldableObject: foldableObject,
    foldableWithIndexObject: foldableWithIndexObject,
    traversableObject: traversableObject,
    traversableWithIndexObject: traversableWithIndexObject,
    eqObject: eqObject,
    eq1Object: eq1Object,
    ordObject: ordObject,
    showObject: showObject,
    semigroupObject: semigroupObject,
    monoidObject: monoidObject,
    empty: $foreign.empty,
    size: $foreign.size,
    keys: $foreign.keys,
    all: $foreign.all,
    runST: $foreign.runST,
    toArrayWithKey: $foreign.toArrayWithKey
};

},{"../Control.Applicative/index.js":27,"../Control.Apply/index.js":29,"../Control.Bind/index.js":33,"../Control.Category/index.js":34,"../Control.Monad.ST.Internal/index.js":46,"../Control.Monad.ST/index.js":48,"../Control.Semigroupoid/index.js":56,"../Data.Array/index.js":63,"../Data.Eq/index.js":82,"../Data.Foldable/index.js":87,"../Data.FoldableWithIndex/index.js":88,"../Data.Function.Uncurried/index.js":90,"../Data.Function/index.js":91,"../Data.Functor/index.js":95,"../Data.FunctorWithIndex/index.js":97,"../Data.HeytingAlgebra/index.js":99,"../Data.Maybe/index.js":110,"../Data.Monoid/index.js":117,"../Data.Ord/index.js":130,"../Data.Semigroup/index.js":139,"../Data.Show/index.js":143,"../Data.Traversable/index.js":154,"../Data.TraversableWithIndex/index.js":155,"../Data.Tuple/index.js":156,"../Data.Unfoldable/index.js":158,"../Foreign.Object.ST/index.js":174,"../Prelude/index.js":198,"../Type.Row.Homogeneous/index.js":208,"../Unsafe.Coerce/index.js":211,"./foreign.js":175}],177:[function(require,module,exports){
"use strict";

exports.unsafeToForeign = function (value) {
  return value;
};

exports.unsafeFromForeign = function (value) {
  return value;
};

exports.typeOf = function (value) {
  return typeof value;
};

exports.tagOf = function (value) {
  return Object.prototype.toString.call(value).slice(8, -1);
};

exports.isNull = function (value) {
  return value === null;
};

exports.isUndefined = function (value) {
  return value === undefined;
};

exports.isArray = Array.isArray || function (value) {
  return Object.prototype.toString.call(value) === "[object Array]";
};

},{}],178:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var $foreign = require("./foreign.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Monad_Error_Class = require("../Control.Monad.Error.Class/index.js");
var Control_Monad_Except = require("../Control.Monad.Except/index.js");
var Control_Monad_Except_Trans = require("../Control.Monad.Except.Trans/index.js");
var Control_Semigroupoid = require("../Control.Semigroupoid/index.js");
var Data_Boolean = require("../Data.Boolean/index.js");
var Data_Either = require("../Data.Either/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra/index.js");
var Data_Identity = require("../Data.Identity/index.js");
var Data_Int = require("../Data.Int/index.js");
var Data_List_NonEmpty = require("../Data.List.NonEmpty/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Ordering = require("../Data.Ordering/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Show = require("../Data.Show/index.js");
var Data_String_CodeUnits = require("../Data.String.CodeUnits/index.js");
var Prelude = require("../Prelude/index.js");
var ForeignError = (function () {
    function ForeignError(value0) {
        this.value0 = value0;
    };
    ForeignError.create = function (value0) {
        return new ForeignError(value0);
    };
    return ForeignError;
})();
var TypeMismatch = (function () {
    function TypeMismatch(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    TypeMismatch.create = function (value0) {
        return function (value1) {
            return new TypeMismatch(value0, value1);
        };
    };
    return TypeMismatch;
})();
var ErrorAtIndex = (function () {
    function ErrorAtIndex(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    ErrorAtIndex.create = function (value0) {
        return function (value1) {
            return new ErrorAtIndex(value0, value1);
        };
    };
    return ErrorAtIndex;
})();
var ErrorAtProperty = (function () {
    function ErrorAtProperty(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    ErrorAtProperty.create = function (value0) {
        return function (value1) {
            return new ErrorAtProperty(value0, value1);
        };
    };
    return ErrorAtProperty;
})();
var showForeignError = new Data_Show.Show(function (v) {
    if (v instanceof ForeignError) {
        return "(ForeignError " + (Data_Show.show(Data_Show.showString)(v.value0) + ")");
    };
    if (v instanceof ErrorAtIndex) {
        return "(ErrorAtIndex " + (Data_Show.show(Data_Show.showInt)(v.value0) + (" " + (Data_Show.show(showForeignError)(v.value1) + ")")));
    };
    if (v instanceof ErrorAtProperty) {
        return "(ErrorAtProperty " + (Data_Show.show(Data_Show.showString)(v.value0) + (" " + (Data_Show.show(showForeignError)(v.value1) + ")")));
    };
    if (v instanceof TypeMismatch) {
        return "(TypeMismatch " + (Data_Show.show(Data_Show.showString)(v.value0) + (" " + (Data_Show.show(Data_Show.showString)(v.value1) + ")")));
    };
    throw new Error("Failed pattern match at Foreign (line 63, column 1 - line 63, column 47): " + [ v.constructor.name ]);
});
var renderForeignError = function (v) {
    if (v instanceof ForeignError) {
        return v.value0;
    };
    if (v instanceof ErrorAtIndex) {
        return "Error at array index " + (Data_Show.show(Data_Show.showInt)(v.value0) + (": " + renderForeignError(v.value1)));
    };
    if (v instanceof ErrorAtProperty) {
        return "Error at property " + (Data_Show.show(Data_Show.showString)(v.value0) + (": " + renderForeignError(v.value1)));
    };
    if (v instanceof TypeMismatch) {
        return "Type mismatch: expected " + (v.value0 + (", found " + v.value1));
    };
    throw new Error("Failed pattern match at Foreign (line 72, column 1 - line 72, column 45): " + [ v.constructor.name ]);
};
var readUndefined = function (value) {
    if ($foreign.isUndefined(value)) {
        return Control_Applicative.pure(Control_Monad_Except_Trans.applicativeExceptT(Data_Identity.monadIdentity))(Data_Maybe.Nothing.value);
    };
    if (Data_Boolean.otherwise) {
        return Control_Applicative.pure(Control_Monad_Except_Trans.applicativeExceptT(Data_Identity.monadIdentity))(new Data_Maybe.Just(value));
    };
    throw new Error("Failed pattern match at Foreign (line 157, column 1 - line 157, column 46): " + [ value.constructor.name ]);
};
var readNullOrUndefined = function (value) {
    if ($foreign.isNull(value) || $foreign.isUndefined(value)) {
        return Control_Applicative.pure(Control_Monad_Except_Trans.applicativeExceptT(Data_Identity.monadIdentity))(Data_Maybe.Nothing.value);
    };
    if (Data_Boolean.otherwise) {
        return Control_Applicative.pure(Control_Monad_Except_Trans.applicativeExceptT(Data_Identity.monadIdentity))(new Data_Maybe.Just(value));
    };
    throw new Error("Failed pattern match at Foreign (line 162, column 1 - line 162, column 52): " + [ value.constructor.name ]);
};
var readNull = function (value) {
    if ($foreign.isNull(value)) {
        return Control_Applicative.pure(Control_Monad_Except_Trans.applicativeExceptT(Data_Identity.monadIdentity))(Data_Maybe.Nothing.value);
    };
    if (Data_Boolean.otherwise) {
        return Control_Applicative.pure(Control_Monad_Except_Trans.applicativeExceptT(Data_Identity.monadIdentity))(new Data_Maybe.Just(value));
    };
    throw new Error("Failed pattern match at Foreign (line 152, column 1 - line 152, column 41): " + [ value.constructor.name ]);
};
var fail = function ($107) {
    return Control_Monad_Error_Class.throwError(Control_Monad_Except_Trans.monadThrowExceptT(Data_Identity.monadIdentity))(Data_List_NonEmpty.singleton($107));
};
var readArray = function (value) {
    if ($foreign.isArray(value)) {
        return Control_Applicative.pure(Control_Monad_Except_Trans.applicativeExceptT(Data_Identity.monadIdentity))($foreign.unsafeFromForeign(value));
    };
    if (Data_Boolean.otherwise) {
        return fail(new TypeMismatch("array", $foreign.tagOf(value)));
    };
    throw new Error("Failed pattern match at Foreign (line 147, column 1 - line 147, column 42): " + [ value.constructor.name ]);
};
var unsafeReadTagged = function (tag) {
    return function (value) {
        if ($foreign.tagOf(value) === tag) {
            return Control_Applicative.pure(Control_Monad_Except_Trans.applicativeExceptT(Data_Identity.monadIdentity))($foreign.unsafeFromForeign(value));
        };
        if (Data_Boolean.otherwise) {
            return fail(new TypeMismatch(tag, $foreign.tagOf(value)));
        };
        throw new Error("Failed pattern match at Foreign (line 106, column 1 - line 106, column 55): " + [ tag.constructor.name, value.constructor.name ]);
    };
};
var readBoolean = unsafeReadTagged("Boolean");
var readNumber = unsafeReadTagged("Number");
var readInt = function (value) {
    var error = Data_Either.Left.create(Data_List_NonEmpty.singleton(new TypeMismatch("Int", $foreign.tagOf(value))));
    var fromNumber = function ($108) {
        return Data_Maybe.maybe(error)(Control_Applicative.pure(Data_Either.applicativeEither))(Data_Int.fromNumber($108));
    };
    return Control_Monad_Except.mapExcept(Data_Either.either(Data_Function["const"](error))(fromNumber))(readNumber(value));
};
var readString = unsafeReadTagged("String");
var readChar = function (value) {
    var error = Data_Either.Left.create(Data_List_NonEmpty.singleton(new TypeMismatch("Char", $foreign.tagOf(value))));
    var fromString = function ($109) {
        return Data_Maybe.maybe(error)(Control_Applicative.pure(Data_Either.applicativeEither))(Data_String_CodeUnits.toChar($109));
    };
    return Control_Monad_Except.mapExcept(Data_Either.either(Data_Function["const"](error))(fromString))(readString(value));
};
var eqForeignError = new Data_Eq.Eq(function (x) {
    return function (y) {
        if (x instanceof ForeignError && y instanceof ForeignError) {
            return x.value0 === y.value0;
        };
        if (x instanceof TypeMismatch && y instanceof TypeMismatch) {
            return x.value0 === y.value0 && x.value1 === y.value1;
        };
        if (x instanceof ErrorAtIndex && y instanceof ErrorAtIndex) {
            return x.value0 === y.value0 && Data_Eq.eq(eqForeignError)(x.value1)(y.value1);
        };
        if (x instanceof ErrorAtProperty && y instanceof ErrorAtProperty) {
            return x.value0 === y.value0 && Data_Eq.eq(eqForeignError)(x.value1)(y.value1);
        };
        return false;
    };
});
var ordForeignError = new Data_Ord.Ord(function () {
    return eqForeignError;
}, function (x) {
    return function (y) {
        if (x instanceof ForeignError && y instanceof ForeignError) {
            return Data_Ord.compare(Data_Ord.ordString)(x.value0)(y.value0);
        };
        if (x instanceof ForeignError) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof ForeignError) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof TypeMismatch && y instanceof TypeMismatch) {
            var v = Data_Ord.compare(Data_Ord.ordString)(x.value0)(y.value0);
            if (v instanceof Data_Ordering.LT) {
                return Data_Ordering.LT.value;
            };
            if (v instanceof Data_Ordering.GT) {
                return Data_Ordering.GT.value;
            };
            return Data_Ord.compare(Data_Ord.ordString)(x.value1)(y.value1);
        };
        if (x instanceof TypeMismatch) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof TypeMismatch) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof ErrorAtIndex && y instanceof ErrorAtIndex) {
            var v = Data_Ord.compare(Data_Ord.ordInt)(x.value0)(y.value0);
            if (v instanceof Data_Ordering.LT) {
                return Data_Ordering.LT.value;
            };
            if (v instanceof Data_Ordering.GT) {
                return Data_Ordering.GT.value;
            };
            return Data_Ord.compare(ordForeignError)(x.value1)(y.value1);
        };
        if (x instanceof ErrorAtIndex) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof ErrorAtIndex) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof ErrorAtProperty && y instanceof ErrorAtProperty) {
            var v = Data_Ord.compare(Data_Ord.ordString)(x.value0)(y.value0);
            if (v instanceof Data_Ordering.LT) {
                return Data_Ordering.LT.value;
            };
            if (v instanceof Data_Ordering.GT) {
                return Data_Ordering.GT.value;
            };
            return Data_Ord.compare(ordForeignError)(x.value1)(y.value1);
        };
        throw new Error("Failed pattern match at Foreign (line 61, column 8 - line 61, column 52): " + [ x.constructor.name, y.constructor.name ]);
    };
});
module.exports = {
    ForeignError: ForeignError,
    TypeMismatch: TypeMismatch,
    ErrorAtIndex: ErrorAtIndex,
    ErrorAtProperty: ErrorAtProperty,
    renderForeignError: renderForeignError,
    unsafeReadTagged: unsafeReadTagged,
    readString: readString,
    readChar: readChar,
    readBoolean: readBoolean,
    readNumber: readNumber,
    readInt: readInt,
    readArray: readArray,
    readNull: readNull,
    readUndefined: readUndefined,
    readNullOrUndefined: readNullOrUndefined,
    fail: fail,
    eqForeignError: eqForeignError,
    ordForeignError: ordForeignError,
    showForeignError: showForeignError,
    unsafeToForeign: $foreign.unsafeToForeign,
    unsafeFromForeign: $foreign.unsafeFromForeign,
    typeOf: $foreign.typeOf,
    tagOf: $foreign.tagOf,
    isNull: $foreign.isNull,
    isUndefined: $foreign.isUndefined,
    isArray: $foreign.isArray
};

},{"../Control.Applicative/index.js":27,"../Control.Monad.Error.Class/index.js":40,"../Control.Monad.Except.Trans/index.js":41,"../Control.Monad.Except/index.js":42,"../Control.Semigroupoid/index.js":56,"../Data.Boolean/index.js":73,"../Data.Either/index.js":80,"../Data.Eq/index.js":82,"../Data.Function/index.js":91,"../Data.HeytingAlgebra/index.js":99,"../Data.Identity/index.js":100,"../Data.Int/index.js":104,"../Data.List.NonEmpty/index.js":105,"../Data.Maybe/index.js":110,"../Data.Ord/index.js":130,"../Data.Ordering/index.js":131,"../Data.Semigroup/index.js":139,"../Data.Show/index.js":143,"../Data.String.CodeUnits/index.js":145,"../Prelude/index.js":198,"./foreign.js":177}],179:[function(require,module,exports){
/* globals exports */
"use strict";

exports.nan = NaN;

exports.isNaN = isNaN;

exports.infinity = Infinity;

exports.isFinite = isFinite;

exports.readInt = function (radix) {
  return function (n) {
    return parseInt(n, radix);
  };
};

exports.readFloat = parseFloat;

},{}],180:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var $foreign = require("./foreign.js");
module.exports = {
    nan: $foreign.nan,
    "isNaN": $foreign["isNaN"],
    infinity: $foreign.infinity,
    "isFinite": $foreign["isFinite"],
    readInt: $foreign.readInt,
    readFloat: $foreign.readFloat
};

},{"./foreign.js":179}],181:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Data_Show = require("../Data.Show/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Effect = require("../Effect/index.js");
var Effect_Console = require("../Effect.Console/index.js");
var Network_HTTP = require("../Network.HTTP/index.js");
var Node_Stream = require("../Node.Stream/index.js");
var Prelude = require("../Prelude/index.js");
var logResp = function (resp) {
    return Effect_Console.logShow(Data_Show.showString)("resp");
};
var main = function __do() {
    Effect_Console.log("You should add some tests.")();
    var v = Network_HTTP.requestFromURI("/index.js")(logResp)();
    Node_Stream.end(Network_HTTP.requestAsStream(v))(Control_Applicative.pure(Effect.applicativeEffect)(Data_Unit.unit))();
    return Effect_Console.log("aaa")();
};
module.exports = {
    main: main,
    logResp: logResp
};

},{"../Control.Applicative/index.js":27,"../Control.Bind/index.js":33,"../Data.Show/index.js":143,"../Data.Unit/index.js":162,"../Effect.Console/index.js":166,"../Effect/index.js":172,"../Network.HTTP/index.js":185,"../Node.Stream/index.js":191,"../Prelude/index.js":198}],182:[function(require,module,exports){
"use strict";

// module Math

exports.abs = Math.abs;

exports.acos = Math.acos;

exports.asin = Math.asin;

exports.atan = Math.atan;

exports.atan2 = function (y) {
  return function (x) {
    return Math.atan2(y, x);
  };
};

exports.ceil = Math.ceil;

exports.cos = Math.cos;

exports.exp = Math.exp;

exports.floor = Math.floor;

exports.trunc = Math.trunc || function (n) {
  return n < 0 ? Math.ceil(n) : Math.floor(n);
};

exports.log = Math.log;

exports.max = function (n1) {
  return function (n2) {
    return Math.max(n1, n2);
  };
};

exports.min = function (n1) {
  return function (n2) {
    return Math.min(n1, n2);
  };
};

exports.pow = function (n) {
  return function (p) {
    return Math.pow(n, p);
  };
};

exports.remainder = function (n) {
  return function (m) {
    return n % m;
  };
};

exports.round = Math.round;

exports.sin = Math.sin;

exports.sqrt = Math.sqrt;

exports.tan = Math.tan;

exports.e = Math.E;

exports.ln2 = Math.LN2;

exports.ln10 = Math.LN10;

exports.log2e = Math.LOG2E;

exports.log10e = Math.LOG10E;

exports.pi = Math.PI;

exports.tau = 2 * Math.PI;

exports.sqrt1_2 = Math.SQRT1_2;

exports.sqrt2 = Math.SQRT2;

},{}],183:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var $foreign = require("./foreign.js");
module.exports = {
    abs: $foreign.abs,
    acos: $foreign.acos,
    asin: $foreign.asin,
    atan: $foreign.atan,
    atan2: $foreign.atan2,
    ceil: $foreign.ceil,
    cos: $foreign.cos,
    exp: $foreign.exp,
    floor: $foreign.floor,
    log: $foreign.log,
    max: $foreign.max,
    min: $foreign.min,
    pow: $foreign.pow,
    round: $foreign.round,
    sin: $foreign.sin,
    sqrt: $foreign.sqrt,
    tan: $foreign.tan,
    trunc: $foreign.trunc,
    remainder: $foreign.remainder,
    e: $foreign.e,
    ln2: $foreign.ln2,
    ln10: $foreign.ln10,
    log2e: $foreign.log2e,
    log10e: $foreign.log10e,
    pi: $foreign.pi,
    tau: $foreign.tau,
    sqrt1_2: $foreign.sqrt1_2,
    sqrt2: $foreign.sqrt2
};

},{"./foreign.js":182}],184:[function(require,module,exports){
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
},{"iso-stream-http":3}],185:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var $foreign = require("./foreign.js");
var Control_Semigroupoid = require("../Control.Semigroupoid/index.js");
var Data_Options = require("../Data.Options/index.js");
var Effect = require("../Effect/index.js");
var Foreign = require("../Foreign/index.js");
var Foreign_Object = require("../Foreign.Object/index.js");
var Node_Stream = require("../Node.Stream/index.js");
var Node_URL = require("../Node.URL/index.js");
var Prelude = require("../Prelude/index.js");
var Unsafe_Coerce = require("../Unsafe.Coerce/index.js");
var RequestHeaders = function (x) {
    return x;
};
var IPV4 = (function () {
    function IPV4() {

    };
    IPV4.value = new IPV4();
    return IPV4;
})();
var IPV6 = (function () {
    function IPV6() {

    };
    IPV6.value = new IPV6();
    return IPV6;
})();
var responseAsStream = Unsafe_Coerce.unsafeCoerce;
var requestFromURI = function ($0) {
    return $foreign.requestImpl(Foreign.unsafeToForeign(Node_URL.parse($0)));
};
var requestAsStream = Unsafe_Coerce.unsafeCoerce;
var request = function ($1) {
    return $foreign.requestImpl(Data_Options.options($1));
};
var protocol = Data_Options.opt("protocol");
var port = Data_Options.opt("port");
var path = Data_Options.opt("path");
var method = Data_Options.opt("method");
var key = Data_Options.opt("key");
var hostname = Data_Options.opt("hostname");
var headers = Data_Options.opt("headers");
var cert = Data_Options.opt("cert");
var auth = Data_Options.opt("auth");
module.exports = {
    IPV4: IPV4,
    IPV6: IPV6,
    RequestHeaders: RequestHeaders,
    protocol: protocol,
    hostname: hostname,
    port: port,
    method: method,
    path: path,
    headers: headers,
    auth: auth,
    key: key,
    cert: cert,
    request: request,
    responseAsStream: responseAsStream,
    requestAsStream: requestAsStream,
    requestFromURI: requestFromURI,
    requestImpl: $foreign.requestImpl,
    setTimeout: $foreign.setTimeout
};

},{"../Control.Semigroupoid/index.js":56,"../Data.Options/index.js":124,"../Effect/index.js":172,"../Foreign.Object/index.js":176,"../Foreign/index.js":178,"../Node.Stream/index.js":191,"../Node.URL/index.js":193,"../Prelude/index.js":198,"../Unsafe.Coerce/index.js":211,"./foreign.js":184}],186:[function(require,module,exports){
(function (Buffer){
/* global exports */
/* global Buffer */
/* global require */
"use strict";

exports.showImpl = require('util').inspect;

exports.create = function (size) {
  return function() {
    return Buffer.alloc(size);
  };
};

exports.fromArray = function (octets) {
  return function() {
    return Buffer.from(octets);
  };
};

exports.fromStringImpl = function (str) {
  return function (encoding) {
    return function() {
      return Buffer.from(str, encoding);
    };
  };
};

exports.fromArrayBuffer = function(ab) {
  return function() {
    return Buffer.from(ab);
  };
};

exports.toArrayBuffer = function(buff) {
  return function() {
    return buff.buffer.slice(buff.byteOffset, buff.byteOffset + buff.byteLength);
  };
};

exports.readImpl = function (ty) {
  return function (offset) {
    return function (buf) {
      return function() {
        return buf['read' + ty](offset);
      };
    };
  };
};

exports.readStringImpl = function (enc) {
  return function (start) {
    return function (end) {
      return function (buff) {
        return function() {
          return buff.toString(enc, start, end);
        };
      };
    };
  };
};

exports.toStringImpl = function (enc) {
  return function (buff) {
    return function() {
      return buff.toString(enc);
    };
  };
};

exports.writeImpl = function (ty) {
  return function (value) {
    return function (offset) {
      return function (buf) {
        return function() {
          buf['write' + ty](value, offset);
          return {};
        }
      };
    };
  };
};

exports.writeStringImpl = function (encoding) {
  return function (offset) {
    return function (length) {
      return function (value) {
        return function (buff) {
          return function() {
            return buff.write(value, offset, length, encoding);
          }
        };
      };
    };
  };
};

exports.toArray = function (buff) {
  return function() {
    var json = buff.toJSON()
    return json.data || json;
  };
};

exports.getAtOffsetImpl = function (just) {
  return function (nothing) {
    return function (offset) {
      return function (buff) {
        return function() {
          var octet = buff[offset];
          return octet == null ? nothing
                               : just(octet);
        };
      };
    };
  };
};

exports.setAtOffset = function (value) {
  return function (offset) {
    return function (buff) {
      return function() {
        buff[offset] = value;
        return {};
      };
    };
  };
};

exports.size = function (buff) {
  return function() {
    return buff.length;
  };
};



exports.concat = function (buffs) {
  return function() {
    return Buffer.concat(buffs);
  };
};

exports["concat'"] = function (buffs) {
  return function (totalLength) {
    return function() {
      return Buffer.concat(buffs, totalLength);
    };
  };
};

exports.copy = function (srcStart) {
  return function (srcEnd) {
    return function (src) {
      return function (targStart) {
        return function (targ) {
          return function() {
            return src.copy(targ, targStart, srcStart, srcEnd);
          };
        };
      };
    };
  };
};

exports.fill = function (octet) {
  return function (start) {
    return function (end) {
      return function (buf) {
        return function() {
          buf.fill(octet, start, end);
          return {};
        };
      };
    };
  };
};

}).call(this,require("buffer").Buffer)
},{"buffer":215,"util":228}],187:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var $foreign = require("./foreign.js");
var Control_Semigroupoid = require("../Control.Semigroupoid/index.js");
var Data_ArrayBuffer_Types = require("../Data.ArrayBuffer.Types/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Show = require("../Data.Show/index.js");
var Effect = require("../Effect/index.js");
var Node_Encoding = require("../Node.Encoding/index.js");
var Prelude = require("../Prelude/index.js");
var UInt8 = (function () {
    function UInt8() {

    };
    UInt8.value = new UInt8();
    return UInt8;
})();
var UInt16LE = (function () {
    function UInt16LE() {

    };
    UInt16LE.value = new UInt16LE();
    return UInt16LE;
})();
var UInt16BE = (function () {
    function UInt16BE() {

    };
    UInt16BE.value = new UInt16BE();
    return UInt16BE;
})();
var UInt32LE = (function () {
    function UInt32LE() {

    };
    UInt32LE.value = new UInt32LE();
    return UInt32LE;
})();
var UInt32BE = (function () {
    function UInt32BE() {

    };
    UInt32BE.value = new UInt32BE();
    return UInt32BE;
})();
var Int8 = (function () {
    function Int8() {

    };
    Int8.value = new Int8();
    return Int8;
})();
var Int16LE = (function () {
    function Int16LE() {

    };
    Int16LE.value = new Int16LE();
    return Int16LE;
})();
var Int16BE = (function () {
    function Int16BE() {

    };
    Int16BE.value = new Int16BE();
    return Int16BE;
})();
var Int32LE = (function () {
    function Int32LE() {

    };
    Int32LE.value = new Int32LE();
    return Int32LE;
})();
var Int32BE = (function () {
    function Int32BE() {

    };
    Int32BE.value = new Int32BE();
    return Int32BE;
})();
var FloatLE = (function () {
    function FloatLE() {

    };
    FloatLE.value = new FloatLE();
    return FloatLE;
})();
var FloatBE = (function () {
    function FloatBE() {

    };
    FloatBE.value = new FloatBE();
    return FloatBE;
})();
var DoubleLE = (function () {
    function DoubleLE() {

    };
    DoubleLE.value = new DoubleLE();
    return DoubleLE;
})();
var DoubleBE = (function () {
    function DoubleBE() {

    };
    DoubleBE.value = new DoubleBE();
    return DoubleBE;
})();
var writeString = function ($2) {
    return $foreign.writeStringImpl(Node_Encoding.encodingToNode($2));
};
var toString = function ($3) {
    return $foreign.toStringImpl(Node_Encoding.encodingToNode($3));
};
var showBufferValueType = new Data_Show.Show(function (v) {
    if (v instanceof UInt8) {
        return "UInt8";
    };
    if (v instanceof UInt16LE) {
        return "UInt16LE";
    };
    if (v instanceof UInt16BE) {
        return "UInt16BE";
    };
    if (v instanceof UInt32LE) {
        return "UInt32LE";
    };
    if (v instanceof UInt32BE) {
        return "UInt32BE";
    };
    if (v instanceof Int8) {
        return "Int8";
    };
    if (v instanceof Int16LE) {
        return "Int16LE";
    };
    if (v instanceof Int16BE) {
        return "Int16BE";
    };
    if (v instanceof Int32LE) {
        return "Int32LE";
    };
    if (v instanceof Int32BE) {
        return "Int32BE";
    };
    if (v instanceof FloatLE) {
        return "FloatLE";
    };
    if (v instanceof FloatBE) {
        return "FloatBE";
    };
    if (v instanceof DoubleLE) {
        return "DoubleLE";
    };
    if (v instanceof DoubleBE) {
        return "DoubleBE";
    };
    throw new Error("Failed pattern match at Node.Buffer (line 65, column 1 - line 65, column 53): " + [ v.constructor.name ]);
});
var write = function ($4) {
    return $foreign.writeImpl(Data_Show.show(showBufferValueType)($4));
};
var showBuffer = new Data_Show.Show($foreign.showImpl);
var readString = function ($5) {
    return $foreign.readStringImpl(Node_Encoding.encodingToNode($5));
};
var read = function ($6) {
    return $foreign.readImpl(Data_Show.show(showBufferValueType)($6));
};
var getAtOffset = $foreign.getAtOffsetImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var fromString = function (str) {
    return function ($7) {
        return $foreign.fromStringImpl(str)(Node_Encoding.encodingToNode($7));
    };
};
module.exports = {
    UInt8: UInt8,
    UInt16LE: UInt16LE,
    UInt16BE: UInt16BE,
    UInt32LE: UInt32LE,
    UInt32BE: UInt32BE,
    Int8: Int8,
    Int16LE: Int16LE,
    Int16BE: Int16BE,
    Int32LE: Int32LE,
    Int32BE: Int32BE,
    FloatLE: FloatLE,
    FloatBE: FloatBE,
    DoubleLE: DoubleLE,
    DoubleBE: DoubleBE,
    fromString: fromString,
    read: read,
    readString: readString,
    toString: toString,
    write: write,
    writeString: writeString,
    getAtOffset: getAtOffset,
    showBuffer: showBuffer,
    showBufferValueType: showBufferValueType,
    create: $foreign.create,
    fromArray: $foreign.fromArray,
    fromArrayBuffer: $foreign.fromArrayBuffer,
    toArrayBuffer: $foreign.toArrayBuffer,
    toArray: $foreign.toArray,
    setAtOffset: $foreign.setAtOffset,
    size: $foreign.size,
    concat: $foreign.concat,
    "concat'": $foreign["concat'"],
    copy: $foreign.copy,
    fill: $foreign.fill
};

},{"../Control.Semigroupoid/index.js":56,"../Data.ArrayBuffer.Types/index.js":64,"../Data.Maybe/index.js":110,"../Data.Show/index.js":143,"../Effect/index.js":172,"../Node.Encoding/index.js":189,"../Prelude/index.js":198,"./foreign.js":186}],188:[function(require,module,exports){
(function (Buffer){
/* global exports */
/* global Buffer */
"use strict";

exports.byteLengthImpl = function (str) {
  return function (enc) {
    return Buffer.byteLength(str, enc);
  };
};

}).call(this,require("buffer").Buffer)
},{"buffer":215}],189:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var $foreign = require("./foreign.js");
var Data_Show = require("../Data.Show/index.js");
var Prelude = require("../Prelude/index.js");
var ASCII = (function () {
    function ASCII() {

    };
    ASCII.value = new ASCII();
    return ASCII;
})();
var UTF8 = (function () {
    function UTF8() {

    };
    UTF8.value = new UTF8();
    return UTF8;
})();
var UTF16LE = (function () {
    function UTF16LE() {

    };
    UTF16LE.value = new UTF16LE();
    return UTF16LE;
})();
var UCS2 = (function () {
    function UCS2() {

    };
    UCS2.value = new UCS2();
    return UCS2;
})();
var Base64 = (function () {
    function Base64() {

    };
    Base64.value = new Base64();
    return Base64;
})();
var Latin1 = (function () {
    function Latin1() {

    };
    Latin1.value = new Latin1();
    return Latin1;
})();
var Binary = (function () {
    function Binary() {

    };
    Binary.value = new Binary();
    return Binary;
})();
var Hex = (function () {
    function Hex() {

    };
    Hex.value = new Hex();
    return Hex;
})();
var showEncoding = new Data_Show.Show(function (v) {
    if (v instanceof ASCII) {
        return "ASCII";
    };
    if (v instanceof UTF8) {
        return "UTF8";
    };
    if (v instanceof UTF16LE) {
        return "UTF16LE";
    };
    if (v instanceof UCS2) {
        return "UCS2";
    };
    if (v instanceof Base64) {
        return "Base64";
    };
    if (v instanceof Latin1) {
        return "Latin1";
    };
    if (v instanceof Binary) {
        return "Binary";
    };
    if (v instanceof Hex) {
        return "Hex";
    };
    throw new Error("Failed pattern match at Node.Encoding (line 19, column 1 - line 19, column 39): " + [ v.constructor.name ]);
});
var encodingToNode = function (v) {
    if (v instanceof ASCII) {
        return "ascii";
    };
    if (v instanceof UTF8) {
        return "utf8";
    };
    if (v instanceof UTF16LE) {
        return "utf16le";
    };
    if (v instanceof UCS2) {
        return "ucs2";
    };
    if (v instanceof Base64) {
        return "base64";
    };
    if (v instanceof Latin1) {
        return "latin1";
    };
    if (v instanceof Binary) {
        return "binary";
    };
    if (v instanceof Hex) {
        return "hex";
    };
    throw new Error("Failed pattern match at Node.Encoding (line 31, column 1 - line 31, column 37): " + [ v.constructor.name ]);
};
var byteLength = function (str) {
    return function (enc) {
        return $foreign.byteLengthImpl(str)(encodingToNode(enc));
    };
};
module.exports = {
    ASCII: ASCII,
    UTF8: UTF8,
    UTF16LE: UTF16LE,
    UCS2: UCS2,
    Base64: Base64,
    Latin1: Latin1,
    Binary: Binary,
    Hex: Hex,
    encodingToNode: encodingToNode,
    byteLength: byteLength,
    showEncoding: showEncoding
};

},{"../Data.Show/index.js":143,"../Prelude/index.js":198,"./foreign.js":188}],190:[function(require,module,exports){
(function (Buffer){
"use strict";

exports.undefined = undefined;

exports.setEncodingImpl = function (s) {
  return function (enc) {
    return function () {
      s.setEncoding(enc);
    };
  };
};

exports.readChunkImpl = function (Left) {
  return function (Right) {
    return function (chunk) {
      if (chunk instanceof Buffer) {
        return Right(chunk);
      } else if (typeof chunk === "string") {
        return Left(chunk);
      } else {
        throw new Error(
          "Node.Stream.readChunkImpl: Unrecognised " +
          "chunk type; expected String or Buffer, got: " +
          chunk);
      }
    };
  };
};

exports.onDataEitherImpl = function (readChunk) {
  return function (r) {
    return function (f) {
      return function () {
        r.on("data", function (data) {
          f(readChunk(data))();
        });
      };
    };
  };
};

exports.onEnd = function (s) {
  return function (f) {
    return function () {
      s.on("end", f);
    };
  };
};

exports.onFinish = function (s) {
  return function (f) {
    return function () {
      s.on("finish", f);
    };
  };
};

exports.onReadable = function (s) {
  return function (f) {
    return function () {
      s.on("readable", f);
    };
  };
};

exports.onError = function (s) {
  return function (f) {
    return function () {
      s.on("error", function (e) {
        f(e)();
      });
    };
  };
};

exports.onClose = function (s) {
  return function (f) {
    return function () {
      s.on("close", f);
    };
  };
};

exports.resume = function (s) {
  return function () {
    s.resume();
  };
};

exports.pause = function (s) {
  return function () {
    s.pause();
  };
};

exports.isPaused = function (s) {
  return function () {
    return s.isPaused();
  };
};

exports.pipe = function (r) {
  return function (w) {
    return function () {
      return r.pipe(w);
    };
  };
};

exports.unpipe = function (r) {
  return function (w) {
    return function () {
      return r.unpipe(w);
    };
  };
};

exports.unpipeAll = function (r) {
  return function () {
    return r.unpipe();
  };
};

exports.readImpl = function (readChunk) {
  return function (Nothing) {
    return function (Just) {
      return function (r) {
        return function (s) {
          return function () {
            var v = r.read(s);
            if (v === null) {
              return Nothing;
            } else {
              return Just(readChunk(v));
            }
          };
        };
      };
    };
  };
};

exports.write = function (w) {
  return function (chunk) {
    return function (done) {
      return function () {
        return w.write(chunk, null, done);
      };
    };
  };
};

exports.writeStringImpl = function (w) {
  return function (enc) {
    return function (s) {
      return function (done) {
        return function () {
          return w.write(s, enc, done);
        };
      };
    };
  };
};

exports.cork = function (w) {
  return function () {
    return w.cork();
  };
};

exports.uncork = function (w) {
  return function () {
    return w.uncork();
  };
};

exports.setDefaultEncodingImpl = function (w) {
  return function (enc) {
    return function () {
      w.setDefaultEncoding(enc);
    };
  };
};

exports.end = function (w) {
  return function (done) {
    return function () {
      w.end(null, null, function () {
        done();
      });
    };
  };
};

exports.destroy = function (strm) {
  return function () {
    strm.destroy(null);
  };
};

exports.destroyWithError = function (strm) {
  return function (e) {
    return function () {
      strm.destroy(e);
    };
  };
};

}).call(this,require("buffer").Buffer)
},{"buffer":215}],191:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var $foreign = require("./foreign.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Data_Either = require("../Data.Either/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Show = require("../Data.Show/index.js");
var Effect = require("../Effect/index.js");
var Effect_Exception = require("../Effect.Exception/index.js");
var Node_Buffer = require("../Node.Buffer/index.js");
var Node_Encoding = require("../Node.Encoding/index.js");
var Prelude = require("../Prelude/index.js");
var writeString = function (w) {
    return function (enc) {
        return $foreign.writeStringImpl(w)(Data_Show.show(Node_Encoding.showEncoding)(enc));
    };
};
var setEncoding = function (r) {
    return function (enc) {
        return $foreign.setEncodingImpl(r)(Data_Show.show(Node_Encoding.showEncoding)(enc));
    };
};
var setDefaultEncoding = function (r) {
    return function (enc) {
        return $foreign.setDefaultEncodingImpl(r)(Data_Show.show(Node_Encoding.showEncoding)(enc));
    };
};
var readChunk = $foreign.readChunkImpl(Data_Either.Left.create)(Data_Either.Right.create);
var readEither = function (r) {
    return function (size) {
        return $foreign.readImpl(readChunk)(Data_Maybe.Nothing.value)(Data_Maybe.Just.create)(r)(Data_Maybe.fromMaybe($foreign["undefined"])(size));
    };
};
var readString = function (r) {
    return function (size) {
        return function (enc) {
            return function __do() {
                var v = readEither(r)(size)();
                if (v instanceof Data_Maybe.Nothing) {
                    return Data_Maybe.Nothing.value;
                };
                if (v instanceof Data_Maybe.Just && v.value0 instanceof Data_Either.Left) {
                    return Effect_Exception["throw"]("Stream encoding should not be set")();
                };
                if (v instanceof Data_Maybe.Just && v.value0 instanceof Data_Either.Right) {
                    return Data_Functor.map(Effect.functorEffect)(Data_Maybe.Just.create)(Node_Buffer.toString(enc)(v.value0.value0))();
                };
                throw new Error("Failed pattern match at Node.Stream (line 120, column 3 - line 123, column 60): " + [ v.constructor.name ]);
            };
        };
    };
};
var read = function (r) {
    return function (size) {
        return function __do() {
            var v = readEither(r)(size)();
            if (v instanceof Data_Maybe.Nothing) {
                return Data_Maybe.Nothing.value;
            };
            if (v instanceof Data_Maybe.Just && v.value0 instanceof Data_Either.Left) {
                return Effect_Exception["throw"]("Stream encoding should not be set")();
            };
            if (v instanceof Data_Maybe.Just && v.value0 instanceof Data_Either.Right) {
                return new Data_Maybe.Just(v.value0.value0);
            };
            throw new Error("Failed pattern match at Node.Stream (line 107, column 3 - line 110, column 36): " + [ v.constructor.name ]);
        };
    };
};
var onDataEither = function (r) {
    return function (cb) {
        return $foreign.onDataEitherImpl(readChunk)(r)(cb);
    };
};
var onData = function (r) {
    return function (cb) {
        var fromEither = function (x) {
            if (x instanceof Data_Either.Left) {
                return Effect_Exception["throw"]("Stream encoding should not be set");
            };
            if (x instanceof Data_Either.Right) {
                return Control_Applicative.pure(Effect.applicativeEffect)(x.value0);
            };
            throw new Error("Failed pattern match at Node.Stream (line 94, column 5 - line 98, column 17): " + [ x.constructor.name ]);
        };
        return onDataEither(r)(Control_Bind.composeKleisliFlipped(Effect.bindEffect)(cb)(fromEither));
    };
};
var onDataString = function (r) {
    return function (enc) {
        return function (cb) {
            return onData(r)(Control_Bind.composeKleisliFlipped(Effect.bindEffect)(cb)(Node_Buffer.toString(enc)));
        };
    };
};
module.exports = {
    onData: onData,
    onDataString: onDataString,
    onDataEither: onDataEither,
    setEncoding: setEncoding,
    read: read,
    readString: readString,
    readEither: readEither,
    writeString: writeString,
    setDefaultEncoding: setDefaultEncoding,
    onReadable: $foreign.onReadable,
    onEnd: $foreign.onEnd,
    onFinish: $foreign.onFinish,
    onClose: $foreign.onClose,
    onError: $foreign.onError,
    resume: $foreign.resume,
    pause: $foreign.pause,
    isPaused: $foreign.isPaused,
    pipe: $foreign.pipe,
    unpipe: $foreign.unpipe,
    unpipeAll: $foreign.unpipeAll,
    write: $foreign.write,
    cork: $foreign.cork,
    uncork: $foreign.uncork,
    end: $foreign.end,
    destroy: $foreign.destroy
};

},{"../Control.Applicative/index.js":27,"../Control.Bind/index.js":33,"../Data.Either/index.js":80,"../Data.Functor/index.js":95,"../Data.Maybe/index.js":110,"../Data.Show/index.js":143,"../Effect.Exception/index.js":168,"../Effect/index.js":172,"../Node.Buffer/index.js":187,"../Node.Encoding/index.js":189,"../Prelude/index.js":198,"./foreign.js":190}],192:[function(require,module,exports){
"use strict";

var url = require("url");
var queryString = require("querystring");

exports.parse = url.parse;

exports.format = url.format;

exports.resolve = function (from) {
  return function (to) {
    return url.resolve(from, to);
  };
};

exports.parseQueryString = queryString.parse;

exports.toQueryString = queryString.stringify;

},{"querystring":224,"url":225}],193:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var $foreign = require("./foreign.js");
var Data_Nullable = require("../Data.Nullable/index.js");
module.exports = {
    parse: $foreign.parse,
    format: $foreign.format,
    resolve: $foreign.resolve,
    parseQueryString: $foreign.parseQueryString,
    toQueryString: $foreign.toQueryString
};

},{"../Data.Nullable/index.js":122,"./foreign.js":192}],194:[function(require,module,exports){
"use strict";

// module Partial.Unsafe

exports.unsafePartial = function (f) {
  return f();
};

},{}],195:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var $foreign = require("./foreign.js");
var Partial = require("../Partial/index.js");
var unsafePartialBecause = function (v) {
    return function (x) {
        return $foreign.unsafePartial(function (dictPartial) {
            return x(dictPartial);
        });
    };
};
var unsafeCrashWith = function (msg) {
    return $foreign.unsafePartial(function (dictPartial) {
        return Partial.crashWith(dictPartial)(msg);
    });
};
module.exports = {
    unsafePartialBecause: unsafePartialBecause,
    unsafeCrashWith: unsafeCrashWith,
    unsafePartial: $foreign.unsafePartial
};

},{"../Partial/index.js":197,"./foreign.js":194}],196:[function(require,module,exports){
"use strict";

// module Partial

exports.crashWith = function () {
  return function (msg) {
    throw new Error(msg);
  };
};

},{}],197:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var $foreign = require("./foreign.js");
var crash = function (dictPartial) {
    return $foreign.crashWith(dictPartial)("Partial.crash: partial function");
};
module.exports = {
    crash: crash,
    crashWith: $foreign.crashWith
};

},{"./foreign.js":196}],198:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Category = require("../Control.Category/index.js");
var Control_Monad = require("../Control.Monad/index.js");
var Control_Semigroupoid = require("../Control.Semigroupoid/index.js");
var Data_Boolean = require("../Data.Boolean/index.js");
var Data_BooleanAlgebra = require("../Data.BooleanAlgebra/index.js");
var Data_Bounded = require("../Data.Bounded/index.js");
var Data_CommutativeRing = require("../Data.CommutativeRing/index.js");
var Data_DivisionRing = require("../Data.DivisionRing/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_EuclideanRing = require("../Data.EuclideanRing/index.js");
var Data_Field = require("../Data.Field/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra/index.js");
var Data_Monoid = require("../Data.Monoid/index.js");
var Data_NaturalTransformation = require("../Data.NaturalTransformation/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Ordering = require("../Data.Ordering/index.js");
var Data_Ring = require("../Data.Ring/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Semiring = require("../Data.Semiring/index.js");
var Data_Show = require("../Data.Show/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Data_Void = require("../Data.Void/index.js");
module.exports = {};

},{"../Control.Applicative/index.js":27,"../Control.Apply/index.js":29,"../Control.Bind/index.js":33,"../Control.Category/index.js":34,"../Control.Monad/index.js":52,"../Control.Semigroupoid/index.js":56,"../Data.Boolean/index.js":73,"../Data.BooleanAlgebra/index.js":74,"../Data.Bounded/index.js":76,"../Data.CommutativeRing/index.js":77,"../Data.DivisionRing/index.js":79,"../Data.Eq/index.js":82,"../Data.EuclideanRing/index.js":84,"../Data.Field/index.js":85,"../Data.Function/index.js":91,"../Data.Functor/index.js":95,"../Data.HeytingAlgebra/index.js":99,"../Data.Monoid/index.js":117,"../Data.NaturalTransformation/index.js":118,"../Data.Ord/index.js":130,"../Data.Ordering/index.js":131,"../Data.Ring/index.js":133,"../Data.Semigroup/index.js":139,"../Data.Semiring/index.js":141,"../Data.Show/index.js":143,"../Data.Unit/index.js":162,"../Data.Void/index.js":163}],199:[function(require,module,exports){
"use strict";

exports.unsafeHas = function (label) {
  return function (rec) {
    return {}.hasOwnProperty.call(rec, label);
  };
};

exports.unsafeGet = function (label) {
  return function (rec) {
    return rec[label];
  };
};

exports.unsafeSet = function (label) {
  return function (value) {
    return function (rec) {
      var copy = {};
      for (var key in rec) {
        if ({}.hasOwnProperty.call(rec, key)) {
          copy[key] = rec[key];
        }
      }
      copy[label] = value;
      return copy;
    };
  };
};

exports.unsafeDelete = function (label) {
  return function (rec) {
    var copy = {};
    for (var key in rec) {
      if (key !== label && {}.hasOwnProperty.call(rec, key)) {
        copy[key] = rec[key];
      }
    }
    return copy;
  };
};

},{}],200:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var $foreign = require("./foreign.js");
module.exports = {
    unsafeHas: $foreign.unsafeHas,
    unsafeGet: $foreign.unsafeGet,
    unsafeSet: $foreign.unsafeSet,
    unsafeDelete: $foreign.unsafeDelete
};

},{"./foreign.js":199}],201:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Type_Proxy = require("../Type.Proxy/index.js");
var BProxy = (function () {
    function BProxy() {

    };
    BProxy.value = new BProxy();
    return BProxy;
})();
var IsBoolean = function (reflectBoolean) {
    this.reflectBoolean = reflectBoolean;
};
var And = {};
var Or = {};
var Not = {};
var If = {};
var reflectBoolean = function (dict) {
    return dict.reflectBoolean;
};
var orTrue = Or;
var orFalse = Or;
var or = function (dictOr) {
    return function (v) {
        return function (v1) {
            return BProxy.value;
        };
    };
};
var notTrue = Not;
var notFalse = Not;
var not = function (dictNot) {
    return function (v) {
        return BProxy.value;
    };
};
var isBooleanTrue = new IsBoolean(function (v) {
    return true;
});
var isBooleanFalse = new IsBoolean(function (v) {
    return false;
});
var reifyBoolean = function (v) {
    return function (f) {
        if (v) {
            return f(isBooleanTrue)(BProxy.value);
        };
        if (!v) {
            return f(isBooleanFalse)(BProxy.value);
        };
        throw new Error("Failed pattern match at Type.Data.Boolean (line 36, column 1 - line 36, column 83): " + [ v.constructor.name, f.constructor.name ]);
    };
};
var if_ = function (dictIf) {
    return function (v) {
        return function (v1) {
            return function (v2) {
                return Type_Proxy["Proxy"].value;
            };
        };
    };
};
var ifTrue = If;
var ifFalse = If;
var andTrue = And;
var andFalse = And;
var and = function (dictAnd) {
    return function (v) {
        return function (v1) {
            return BProxy.value;
        };
    };
};
module.exports = {
    BProxy: BProxy,
    IsBoolean: IsBoolean,
    reflectBoolean: reflectBoolean,
    reifyBoolean: reifyBoolean,
    And: And,
    and: and,
    Or: Or,
    or: or,
    Not: Not,
    not: not,
    If: If,
    if_: if_,
    isBooleanTrue: isBooleanTrue,
    isBooleanFalse: isBooleanFalse,
    andTrue: andTrue,
    andFalse: andFalse,
    orTrue: orTrue,
    orFalse: orFalse,
    notTrue: notTrue,
    notFalse: notFalse,
    ifTrue: ifTrue,
    ifFalse: ifFalse
};

},{"../Type.Proxy/index.js":207}],202:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Data_Ordering = require("../Data.Ordering/index.js");
var Type_Data_Boolean = require("../Type.Data.Boolean/index.js");
var OProxy = (function () {
    function OProxy() {

    };
    OProxy.value = new OProxy();
    return OProxy;
})();
var IsOrdering = function (reflectOrdering) {
    this.reflectOrdering = reflectOrdering;
};
var Append = {};
var Invert = {};
var Equals = {};
var reflectOrdering = function (dict) {
    return dict.reflectOrdering;
};
var isOrderingLT = new IsOrdering(function (v) {
    return Data_Ordering.LT.value;
});
var isOrderingGT = new IsOrdering(function (v) {
    return Data_Ordering.GT.value;
});
var isOrderingEQ = new IsOrdering(function (v) {
    return Data_Ordering.EQ.value;
});
var reifyOrdering = function (v) {
    return function (f) {
        if (v instanceof Data_Ordering.LT) {
            return f(isOrderingLT)(OProxy.value);
        };
        if (v instanceof Data_Ordering.EQ) {
            return f(isOrderingEQ)(OProxy.value);
        };
        if (v instanceof Data_Ordering.GT) {
            return f(isOrderingGT)(OProxy.value);
        };
        throw new Error("Failed pattern match at Type.Data.Ordering (line 31, column 1 - line 31, column 86): " + [ v.constructor.name, f.constructor.name ]);
    };
};
var invertOrderingLT = Invert;
var invertOrderingGT = Invert;
var invertOrderingEQ = Invert;
var invert = function (dictInvert) {
    return function (v) {
        return OProxy.value;
    };
};
var equalsLTLT = Equals;
var equalsLTGT = Equals;
var equalsLTEQ = Equals;
var equalsGTLT = Equals;
var equalsGTGT = Equals;
var equalsGTEQ = Equals;
var equalsEQLT = Equals;
var equalsEQGT = Equals;
var equalsEQEQ = Equals;
var equals = function (dictEquals) {
    return function (v) {
        return function (v1) {
            return Type_Data_Boolean.BProxy.value;
        };
    };
};
var appendOrderingLT = Append;
var appendOrderingGT = Append;
var appendOrderingEQ = Append;
var append = function (dictAppend) {
    return function (v) {
        return function (v1) {
            return OProxy.value;
        };
    };
};
module.exports = {
    OProxy: OProxy,
    IsOrdering: IsOrdering,
    reflectOrdering: reflectOrdering,
    reifyOrdering: reifyOrdering,
    Append: Append,
    append: append,
    Invert: Invert,
    invert: invert,
    Equals: Equals,
    equals: equals,
    isOrderingLT: isOrderingLT,
    isOrderingEQ: isOrderingEQ,
    isOrderingGT: isOrderingGT,
    appendOrderingLT: appendOrderingLT,
    appendOrderingEQ: appendOrderingEQ,
    appendOrderingGT: appendOrderingGT,
    invertOrderingLT: invertOrderingLT,
    invertOrderingEQ: invertOrderingEQ,
    invertOrderingGT: invertOrderingGT,
    equalsEQEQ: equalsEQEQ,
    equalsLTLT: equalsLTLT,
    equalsGTGT: equalsGTGT,
    equalsEQLT: equalsEQLT,
    equalsEQGT: equalsEQGT,
    equalsLTEQ: equalsLTEQ,
    equalsLTGT: equalsLTGT,
    equalsGTLT: equalsGTLT,
    equalsGTEQ: equalsGTEQ
};

},{"../Data.Ordering/index.js":131,"../Type.Data.Boolean/index.js":201}],203:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var RProxy = (function () {
    function RProxy() {

    };
    RProxy.value = new RProxy();
    return RProxy;
})();
module.exports = {
    RProxy: RProxy
};

},{}],204:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var RLProxy = (function () {
    function RLProxy() {

    };
    RLProxy.value = new RLProxy();
    return RLProxy;
})();
module.exports = {
    RLProxy: RLProxy
};

},{}],205:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Data_Symbol = require("../Data.Symbol/index.js");
var Type_Data_Boolean = require("../Type.Data.Boolean/index.js");
var Type_Data_Ordering = require("../Type.Data.Ordering/index.js");
var Equals = {};
var uncons = function (dictCons) {
    return function (v) {
        return {
            head: Data_Symbol.SProxy.value,
            tail: Data_Symbol.SProxy.value
        };
    };
};
var equalsSymbol = function (dictCompare) {
    return function (dictEquals) {
        return Equals;
    };
};
var equals = function (dictEquals) {
    return function (v) {
        return function (v1) {
            return Type_Data_Boolean.BProxy.value;
        };
    };
};
var compare = function (dictCompare) {
    return function (v) {
        return function (v1) {
            return Type_Data_Ordering.OProxy.value;
        };
    };
};
var append = function (dictAppend) {
    return function (v) {
        return function (v1) {
            return Data_Symbol.SProxy.value;
        };
    };
};
module.exports = {
    append: append,
    compare: compare,
    uncons: uncons,
    Equals: Equals,
    equals: equals,
    equalsSymbol: equalsSymbol
};

},{"../Data.Symbol/index.js":150,"../Type.Data.Boolean/index.js":201,"../Type.Data.Ordering/index.js":202}],206:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var TypeEquals = function (from, to) {
    this.from = from;
    this.to = to;
};
var to = function (dict) {
    return dict.to;
};
var refl = new TypeEquals(function (a) {
    return a;
}, function (a) {
    return a;
});
var from = function (dict) {
    return dict.from;
};
module.exports = {
    TypeEquals: TypeEquals,
    to: to,
    from: from,
    refl: refl
};

},{}],207:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Monad = require("../Control.Monad/index.js");
var Data_BooleanAlgebra = require("../Data.BooleanAlgebra/index.js");
var Data_Bounded = require("../Data.Bounded/index.js");
var Data_CommutativeRing = require("../Data.CommutativeRing/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Ordering = require("../Data.Ordering/index.js");
var Data_Ring = require("../Data.Ring/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Semiring = require("../Data.Semiring/index.js");
var Data_Show = require("../Data.Show/index.js");
var Prelude = require("../Prelude/index.js");
var Proxy3 = (function () {
    function Proxy3() {

    };
    Proxy3.value = new Proxy3();
    return Proxy3;
})();
var Proxy2 = (function () {
    function Proxy2() {

    };
    Proxy2.value = new Proxy2();
    return Proxy2;
})();
var $$Proxy = (function () {
    function $$Proxy() {

    };
    $$Proxy.value = new $$Proxy();
    return $$Proxy;
})();
var showProxy3 = new Data_Show.Show(function (v) {
    return "Proxy3";
});
var showProxy2 = new Data_Show.Show(function (v) {
    return "Proxy2";
});
var showProxy = new Data_Show.Show(function (v) {
    return "Proxy";
});
var semiringProxy3 = new Data_Semiring.Semiring(function (v) {
    return function (v1) {
        return Proxy3.value;
    };
}, function (v) {
    return function (v1) {
        return Proxy3.value;
    };
}, Proxy3.value, Proxy3.value);
var semiringProxy2 = new Data_Semiring.Semiring(function (v) {
    return function (v1) {
        return Proxy2.value;
    };
}, function (v) {
    return function (v1) {
        return Proxy2.value;
    };
}, Proxy2.value, Proxy2.value);
var semiringProxy = new Data_Semiring.Semiring(function (v) {
    return function (v1) {
        return $$Proxy.value;
    };
}, function (v) {
    return function (v1) {
        return $$Proxy.value;
    };
}, $$Proxy.value, $$Proxy.value);
var semigroupProxy3 = new Data_Semigroup.Semigroup(function (v) {
    return function (v1) {
        return Proxy3.value;
    };
});
var semigroupProxy2 = new Data_Semigroup.Semigroup(function (v) {
    return function (v1) {
        return Proxy2.value;
    };
});
var semigroupProxy = new Data_Semigroup.Semigroup(function (v) {
    return function (v1) {
        return $$Proxy.value;
    };
});
var ringProxy3 = new Data_Ring.Ring(function () {
    return semiringProxy3;
}, function (v) {
    return function (v1) {
        return Proxy3.value;
    };
});
var ringProxy2 = new Data_Ring.Ring(function () {
    return semiringProxy2;
}, function (v) {
    return function (v1) {
        return Proxy2.value;
    };
});
var ringProxy = new Data_Ring.Ring(function () {
    return semiringProxy;
}, function (v) {
    return function (v1) {
        return $$Proxy.value;
    };
});
var heytingAlgebraProxy3 = new Data_HeytingAlgebra.HeytingAlgebra(function (v) {
    return function (v1) {
        return Proxy3.value;
    };
}, function (v) {
    return function (v1) {
        return Proxy3.value;
    };
}, Proxy3.value, function (v) {
    return function (v1) {
        return Proxy3.value;
    };
}, function (v) {
    return Proxy3.value;
}, Proxy3.value);
var heytingAlgebraProxy2 = new Data_HeytingAlgebra.HeytingAlgebra(function (v) {
    return function (v1) {
        return Proxy2.value;
    };
}, function (v) {
    return function (v1) {
        return Proxy2.value;
    };
}, Proxy2.value, function (v) {
    return function (v1) {
        return Proxy2.value;
    };
}, function (v) {
    return Proxy2.value;
}, Proxy2.value);
var heytingAlgebraProxy = new Data_HeytingAlgebra.HeytingAlgebra(function (v) {
    return function (v1) {
        return $$Proxy.value;
    };
}, function (v) {
    return function (v1) {
        return $$Proxy.value;
    };
}, $$Proxy.value, function (v) {
    return function (v1) {
        return $$Proxy.value;
    };
}, function (v) {
    return $$Proxy.value;
}, $$Proxy.value);
var functorProxy = new Data_Functor.Functor(function (f) {
    return function (m) {
        return $$Proxy.value;
    };
});
var eqProxy3 = new Data_Eq.Eq(function (x) {
    return function (y) {
        return true;
    };
});
var ordProxy3 = new Data_Ord.Ord(function () {
    return eqProxy3;
}, function (x) {
    return function (y) {
        return Data_Ordering.EQ.value;
    };
});
var eqProxy2 = new Data_Eq.Eq(function (x) {
    return function (y) {
        return true;
    };
});
var ordProxy2 = new Data_Ord.Ord(function () {
    return eqProxy2;
}, function (x) {
    return function (y) {
        return Data_Ordering.EQ.value;
    };
});
var eqProxy = new Data_Eq.Eq(function (x) {
    return function (y) {
        return true;
    };
});
var ordProxy = new Data_Ord.Ord(function () {
    return eqProxy;
}, function (x) {
    return function (y) {
        return Data_Ordering.EQ.value;
    };
});
var discardProxy3 = new Control_Bind.Discard(function (dictBind) {
    return Control_Bind.bind(dictBind);
});
var discardProxy2 = new Control_Bind.Discard(function (dictBind) {
    return Control_Bind.bind(dictBind);
});
var discardProxy = new Control_Bind.Discard(function (dictBind) {
    return Control_Bind.bind(dictBind);
});
var commutativeRingProxy3 = new Data_CommutativeRing.CommutativeRing(function () {
    return ringProxy3;
});
var commutativeRingProxy2 = new Data_CommutativeRing.CommutativeRing(function () {
    return ringProxy2;
});
var commutativeRingProxy = new Data_CommutativeRing.CommutativeRing(function () {
    return ringProxy;
});
var boundedProxy3 = new Data_Bounded.Bounded(function () {
    return ordProxy3;
}, Proxy3.value, Proxy3.value);
var boundedProxy2 = new Data_Bounded.Bounded(function () {
    return ordProxy2;
}, Proxy2.value, Proxy2.value);
var boundedProxy = new Data_Bounded.Bounded(function () {
    return ordProxy;
}, $$Proxy.value, $$Proxy.value);
var booleanAlgebraProxy3 = new Data_BooleanAlgebra.BooleanAlgebra(function () {
    return heytingAlgebraProxy3;
});
var booleanAlgebraProxy2 = new Data_BooleanAlgebra.BooleanAlgebra(function () {
    return heytingAlgebraProxy2;
});
var booleanAlgebraProxy = new Data_BooleanAlgebra.BooleanAlgebra(function () {
    return heytingAlgebraProxy;
});
var applyProxy = new Control_Apply.Apply(function () {
    return functorProxy;
}, function (v) {
    return function (v1) {
        return $$Proxy.value;
    };
});
var bindProxy = new Control_Bind.Bind(function () {
    return applyProxy;
}, function (v) {
    return function (v1) {
        return $$Proxy.value;
    };
});
var applicativeProxy = new Control_Applicative.Applicative(function () {
    return applyProxy;
}, function (v) {
    return $$Proxy.value;
});
var monadProxy = new Control_Monad.Monad(function () {
    return applicativeProxy;
}, function () {
    return bindProxy;
});
module.exports = {
    "Proxy": $$Proxy,
    Proxy2: Proxy2,
    Proxy3: Proxy3,
    eqProxy: eqProxy,
    functorProxy: functorProxy,
    ordProxy: ordProxy,
    applicativeProxy: applicativeProxy,
    applyProxy: applyProxy,
    bindProxy: bindProxy,
    booleanAlgebraProxy: booleanAlgebraProxy,
    boundedProxy: boundedProxy,
    commutativeRingProxy: commutativeRingProxy,
    discardProxy: discardProxy,
    heytingAlgebraProxy: heytingAlgebraProxy,
    monadProxy: monadProxy,
    ringProxy: ringProxy,
    semigroupProxy: semigroupProxy,
    semiringProxy: semiringProxy,
    showProxy: showProxy,
    eqProxy2: eqProxy2,
    ordProxy2: ordProxy2,
    booleanAlgebraProxy2: booleanAlgebraProxy2,
    boundedProxy2: boundedProxy2,
    commutativeRingProxy2: commutativeRingProxy2,
    discardProxy2: discardProxy2,
    heytingAlgebraProxy2: heytingAlgebraProxy2,
    ringProxy2: ringProxy2,
    semigroupProxy2: semigroupProxy2,
    semiringProxy2: semiringProxy2,
    showProxy2: showProxy2,
    eqProxy3: eqProxy3,
    ordProxy3: ordProxy3,
    booleanAlgebraProxy3: booleanAlgebraProxy3,
    boundedProxy3: boundedProxy3,
    commutativeRingProxy3: commutativeRingProxy3,
    discardProxy3: discardProxy3,
    heytingAlgebraProxy3: heytingAlgebraProxy3,
    ringProxy3: ringProxy3,
    semigroupProxy3: semigroupProxy3,
    semiringProxy3: semiringProxy3,
    showProxy3: showProxy3
};

},{"../Control.Applicative/index.js":27,"../Control.Apply/index.js":29,"../Control.Bind/index.js":33,"../Control.Monad/index.js":52,"../Data.BooleanAlgebra/index.js":74,"../Data.Bounded/index.js":76,"../Data.CommutativeRing/index.js":77,"../Data.Eq/index.js":82,"../Data.Functor/index.js":95,"../Data.HeytingAlgebra/index.js":99,"../Data.Ord/index.js":130,"../Data.Ordering/index.js":131,"../Data.Ring/index.js":133,"../Data.Semigroup/index.js":139,"../Data.Semiring/index.js":141,"../Data.Show/index.js":143,"../Prelude/index.js":198}],208:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Type_Equality = require("../Type.Equality/index.js");
var Type_Row = require("../Type.Row/index.js");
var Homogeneous = {};
var HomogeneousRowList = {};
var homogeneousRowListNil = HomogeneousRowList;
var homogeneousRowListCons = function (dictHomogeneousRowList) {
    return function (dictTypeEquals) {
        return HomogeneousRowList;
    };
};
var homogeneous = function (dictRowToList) {
    return function (dictHomogeneousRowList) {
        return Homogeneous;
    };
};
module.exports = {
    Homogeneous: Homogeneous,
    HomogeneousRowList: HomogeneousRowList,
    homogeneous: homogeneous,
    homogeneousRowListCons: homogeneousRowListCons,
    homogeneousRowListNil: homogeneousRowListNil
};

},{"../Type.Equality/index.js":206,"../Type.Row/index.js":209}],209:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var Type_Data_Boolean = require("../Type.Data.Boolean/index.js");
var Type_Data_Symbol = require("../Type.Data.Symbol/index.js");
var Type_Equality = require("../Type.Equality/index.js");
var RProxy = (function () {
    function RProxy() {

    };
    RProxy.value = new RProxy();
    return RProxy;
})();
var RLProxy = (function () {
    function RLProxy() {

    };
    RLProxy.value = new RLProxy();
    return RLProxy;
})();
var ListToRow = {};
var RowListRemove = {};
var RowListSet = {};
var RowListNub = {};
var RowListAppend = {};
var rowListSetImpl = function (dictTypeEquals) {
    return function (dictTypeEquals1) {
        return function (dictRowListRemove) {
            return RowListSet;
        };
    };
};
var rowListRemoveNil = RowListRemove;
var rowListRemoveCons = function (dictRowListRemove) {
    return function (dictEquals) {
        return function (dictIf) {
            return RowListRemove;
        };
    };
};
var rowListNubNil = RowListNub;
var rowListNubCons = function (dictTypeEquals) {
    return function (dictTypeEquals1) {
        return function (dictTypeEquals2) {
            return function (dictRowListRemove) {
                return function (dictRowListNub) {
                    return RowListNub;
                };
            };
        };
    };
};
var rowListAppendNil = function (dictTypeEquals) {
    return RowListAppend;
};
var rowListAppendCons = function (dictRowListAppend) {
    return function (dictTypeEquals) {
        return RowListAppend;
    };
};
var listToRowNil = ListToRow;
var listToCons = function (dictListToRow) {
    return function (dictCons) {
        return ListToRow;
    };
};
module.exports = {
    RProxy: RProxy,
    RLProxy: RLProxy,
    ListToRow: ListToRow,
    RowListRemove: RowListRemove,
    RowListSet: RowListSet,
    RowListNub: RowListNub,
    RowListAppend: RowListAppend,
    listToRowNil: listToRowNil,
    listToCons: listToCons,
    rowListRemoveNil: rowListRemoveNil,
    rowListRemoveCons: rowListRemoveCons,
    rowListSetImpl: rowListSetImpl,
    rowListNubNil: rowListNubNil,
    rowListNubCons: rowListNubCons,
    rowListAppendNil: rowListAppendNil,
    rowListAppendCons: rowListAppendCons
};

},{"../Type.Data.Boolean/index.js":201,"../Type.Data.Symbol/index.js":205,"../Type.Equality/index.js":206}],210:[function(require,module,exports){
"use strict";

// module Unsafe.Coerce

exports.unsafeCoerce = function (x) {
  return x;
};

},{}],211:[function(require,module,exports){
// Generated by purs version 0.12.2
"use strict";
var $foreign = require("./foreign.js");
module.exports = {
    unsafeCoerce: $foreign.unsafeCoerce
};

},{"./foreign.js":210}],212:[function(require,module,exports){
require('Main').main();

},{"Main":181}],213:[function(require,module,exports){
'use strict'

exports.byteLength = byteLength
exports.toByteArray = toByteArray
exports.fromByteArray = fromByteArray

var lookup = []
var revLookup = []
var Arr = typeof Uint8Array !== 'undefined' ? Uint8Array : Array

var code = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'
for (var i = 0, len = code.length; i < len; ++i) {
  lookup[i] = code[i]
  revLookup[code.charCodeAt(i)] = i
}

// Support decoding URL-safe base64 strings, as Node.js does.
// See: https://en.wikipedia.org/wiki/Base64#URL_applications
revLookup['-'.charCodeAt(0)] = 62
revLookup['_'.charCodeAt(0)] = 63

function getLens (b64) {
  var len = b64.length

  if (len % 4 > 0) {
    throw new Error('Invalid string. Length must be a multiple of 4')
  }

  // Trim off extra bytes after placeholder bytes are found
  // See: https://github.com/beatgammit/base64-js/issues/42
  var validLen = b64.indexOf('=')
  if (validLen === -1) validLen = len

  var placeHoldersLen = validLen === len
    ? 0
    : 4 - (validLen % 4)

  return [validLen, placeHoldersLen]
}

// base64 is 4/3 + up to two characters of the original data
function byteLength (b64) {
  var lens = getLens(b64)
  var validLen = lens[0]
  var placeHoldersLen = lens[1]
  return ((validLen + placeHoldersLen) * 3 / 4) - placeHoldersLen
}

function _byteLength (b64, validLen, placeHoldersLen) {
  return ((validLen + placeHoldersLen) * 3 / 4) - placeHoldersLen
}

function toByteArray (b64) {
  var tmp
  var lens = getLens(b64)
  var validLen = lens[0]
  var placeHoldersLen = lens[1]

  var arr = new Arr(_byteLength(b64, validLen, placeHoldersLen))

  var curByte = 0

  // if there are placeholders, only get up to the last complete 4 chars
  var len = placeHoldersLen > 0
    ? validLen - 4
    : validLen

  for (var i = 0; i < len; i += 4) {
    tmp =
      (revLookup[b64.charCodeAt(i)] << 18) |
      (revLookup[b64.charCodeAt(i + 1)] << 12) |
      (revLookup[b64.charCodeAt(i + 2)] << 6) |
      revLookup[b64.charCodeAt(i + 3)]
    arr[curByte++] = (tmp >> 16) & 0xFF
    arr[curByte++] = (tmp >> 8) & 0xFF
    arr[curByte++] = tmp & 0xFF
  }

  if (placeHoldersLen === 2) {
    tmp =
      (revLookup[b64.charCodeAt(i)] << 2) |
      (revLookup[b64.charCodeAt(i + 1)] >> 4)
    arr[curByte++] = tmp & 0xFF
  }

  if (placeHoldersLen === 1) {
    tmp =
      (revLookup[b64.charCodeAt(i)] << 10) |
      (revLookup[b64.charCodeAt(i + 1)] << 4) |
      (revLookup[b64.charCodeAt(i + 2)] >> 2)
    arr[curByte++] = (tmp >> 8) & 0xFF
    arr[curByte++] = tmp & 0xFF
  }

  return arr
}

function tripletToBase64 (num) {
  return lookup[num >> 18 & 0x3F] +
    lookup[num >> 12 & 0x3F] +
    lookup[num >> 6 & 0x3F] +
    lookup[num & 0x3F]
}

function encodeChunk (uint8, start, end) {
  var tmp
  var output = []
  for (var i = start; i < end; i += 3) {
    tmp =
      ((uint8[i] << 16) & 0xFF0000) +
      ((uint8[i + 1] << 8) & 0xFF00) +
      (uint8[i + 2] & 0xFF)
    output.push(tripletToBase64(tmp))
  }
  return output.join('')
}

function fromByteArray (uint8) {
  var tmp
  var len = uint8.length
  var extraBytes = len % 3 // if we have 1 byte left, pad 2 bytes
  var parts = []
  var maxChunkLength = 16383 // must be multiple of 3

  // go through the array every three bytes, we'll deal with trailing stuff later
  for (var i = 0, len2 = len - extraBytes; i < len2; i += maxChunkLength) {
    parts.push(encodeChunk(
      uint8, i, (i + maxChunkLength) > len2 ? len2 : (i + maxChunkLength)
    ))
  }

  // pad the end with zeros, but make sure to not forget the extra bytes
  if (extraBytes === 1) {
    tmp = uint8[len - 1]
    parts.push(
      lookup[tmp >> 2] +
      lookup[(tmp << 4) & 0x3F] +
      '=='
    )
  } else if (extraBytes === 2) {
    tmp = (uint8[len - 2] << 8) + uint8[len - 1]
    parts.push(
      lookup[tmp >> 10] +
      lookup[(tmp >> 4) & 0x3F] +
      lookup[(tmp << 2) & 0x3F] +
      '='
    )
  }

  return parts.join('')
}

},{}],214:[function(require,module,exports){

},{}],215:[function(require,module,exports){
(function (global){
/*!
 * The buffer module from node.js, for the browser.
 *
 * @author   Feross Aboukhadijeh <feross@feross.org> <http://feross.org>
 * @license  MIT
 */
/* eslint-disable no-proto */

'use strict'

var base64 = require('base64-js')
var ieee754 = require('ieee754')
var isArray = require('isarray')

exports.Buffer = Buffer
exports.SlowBuffer = SlowBuffer
exports.INSPECT_MAX_BYTES = 50

/**
 * If `Buffer.TYPED_ARRAY_SUPPORT`:
 *   === true    Use Uint8Array implementation (fastest)
 *   === false   Use Object implementation (most compatible, even IE6)
 *
 * Browsers that support typed arrays are IE 10+, Firefox 4+, Chrome 7+, Safari 5.1+,
 * Opera 11.6+, iOS 4.2+.
 *
 * Due to various browser bugs, sometimes the Object implementation will be used even
 * when the browser supports typed arrays.
 *
 * Note:
 *
 *   - Firefox 4-29 lacks support for adding new properties to `Uint8Array` instances,
 *     See: https://bugzilla.mozilla.org/show_bug.cgi?id=695438.
 *
 *   - Chrome 9-10 is missing the `TypedArray.prototype.subarray` function.
 *
 *   - IE10 has a broken `TypedArray.prototype.subarray` function which returns arrays of
 *     incorrect length in some situations.

 * We detect these buggy browsers and set `Buffer.TYPED_ARRAY_SUPPORT` to `false` so they
 * get the Object implementation, which is slower but behaves correctly.
 */
Buffer.TYPED_ARRAY_SUPPORT = global.TYPED_ARRAY_SUPPORT !== undefined
  ? global.TYPED_ARRAY_SUPPORT
  : typedArraySupport()

/*
 * Export kMaxLength after typed array support is determined.
 */
exports.kMaxLength = kMaxLength()

function typedArraySupport () {
  try {
    var arr = new Uint8Array(1)
    arr.__proto__ = {__proto__: Uint8Array.prototype, foo: function () { return 42 }}
    return arr.foo() === 42 && // typed array instances can be augmented
        typeof arr.subarray === 'function' && // chrome 9-10 lack `subarray`
        arr.subarray(1, 1).byteLength === 0 // ie10 has broken `subarray`
  } catch (e) {
    return false
  }
}

function kMaxLength () {
  return Buffer.TYPED_ARRAY_SUPPORT
    ? 0x7fffffff
    : 0x3fffffff
}

function createBuffer (that, length) {
  if (kMaxLength() < length) {
    throw new RangeError('Invalid typed array length')
  }
  if (Buffer.TYPED_ARRAY_SUPPORT) {
    // Return an augmented `Uint8Array` instance, for best performance
    that = new Uint8Array(length)
    that.__proto__ = Buffer.prototype
  } else {
    // Fallback: Return an object instance of the Buffer class
    if (that === null) {
      that = new Buffer(length)
    }
    that.length = length
  }

  return that
}

/**
 * The Buffer constructor returns instances of `Uint8Array` that have their
 * prototype changed to `Buffer.prototype`. Furthermore, `Buffer` is a subclass of
 * `Uint8Array`, so the returned instances will have all the node `Buffer` methods
 * and the `Uint8Array` methods. Square bracket notation works as expected -- it
 * returns a single octet.
 *
 * The `Uint8Array` prototype remains unmodified.
 */

function Buffer (arg, encodingOrOffset, length) {
  if (!Buffer.TYPED_ARRAY_SUPPORT && !(this instanceof Buffer)) {
    return new Buffer(arg, encodingOrOffset, length)
  }

  // Common case.
  if (typeof arg === 'number') {
    if (typeof encodingOrOffset === 'string') {
      throw new Error(
        'If encoding is specified then the first argument must be a string'
      )
    }
    return allocUnsafe(this, arg)
  }
  return from(this, arg, encodingOrOffset, length)
}

Buffer.poolSize = 8192 // not used by this implementation

// TODO: Legacy, not needed anymore. Remove in next major version.
Buffer._augment = function (arr) {
  arr.__proto__ = Buffer.prototype
  return arr
}

function from (that, value, encodingOrOffset, length) {
  if (typeof value === 'number') {
    throw new TypeError('"value" argument must not be a number')
  }

  if (typeof ArrayBuffer !== 'undefined' && value instanceof ArrayBuffer) {
    return fromArrayBuffer(that, value, encodingOrOffset, length)
  }

  if (typeof value === 'string') {
    return fromString(that, value, encodingOrOffset)
  }

  return fromObject(that, value)
}

/**
 * Functionally equivalent to Buffer(arg, encoding) but throws a TypeError
 * if value is a number.
 * Buffer.from(str[, encoding])
 * Buffer.from(array)
 * Buffer.from(buffer)
 * Buffer.from(arrayBuffer[, byteOffset[, length]])
 **/
Buffer.from = function (value, encodingOrOffset, length) {
  return from(null, value, encodingOrOffset, length)
}

if (Buffer.TYPED_ARRAY_SUPPORT) {
  Buffer.prototype.__proto__ = Uint8Array.prototype
  Buffer.__proto__ = Uint8Array
  if (typeof Symbol !== 'undefined' && Symbol.species &&
      Buffer[Symbol.species] === Buffer) {
    // Fix subarray() in ES2016. See: https://github.com/feross/buffer/pull/97
    Object.defineProperty(Buffer, Symbol.species, {
      value: null,
      configurable: true
    })
  }
}

function assertSize (size) {
  if (typeof size !== 'number') {
    throw new TypeError('"size" argument must be a number')
  } else if (size < 0) {
    throw new RangeError('"size" argument must not be negative')
  }
}

function alloc (that, size, fill, encoding) {
  assertSize(size)
  if (size <= 0) {
    return createBuffer(that, size)
  }
  if (fill !== undefined) {
    // Only pay attention to encoding if it's a string. This
    // prevents accidentally sending in a number that would
    // be interpretted as a start offset.
    return typeof encoding === 'string'
      ? createBuffer(that, size).fill(fill, encoding)
      : createBuffer(that, size).fill(fill)
  }
  return createBuffer(that, size)
}

/**
 * Creates a new filled Buffer instance.
 * alloc(size[, fill[, encoding]])
 **/
Buffer.alloc = function (size, fill, encoding) {
  return alloc(null, size, fill, encoding)
}

function allocUnsafe (that, size) {
  assertSize(size)
  that = createBuffer(that, size < 0 ? 0 : checked(size) | 0)
  if (!Buffer.TYPED_ARRAY_SUPPORT) {
    for (var i = 0; i < size; ++i) {
      that[i] = 0
    }
  }
  return that
}

/**
 * Equivalent to Buffer(num), by default creates a non-zero-filled Buffer instance.
 * */
Buffer.allocUnsafe = function (size) {
  return allocUnsafe(null, size)
}
/**
 * Equivalent to SlowBuffer(num), by default creates a non-zero-filled Buffer instance.
 */
Buffer.allocUnsafeSlow = function (size) {
  return allocUnsafe(null, size)
}

function fromString (that, string, encoding) {
  if (typeof encoding !== 'string' || encoding === '') {
    encoding = 'utf8'
  }

  if (!Buffer.isEncoding(encoding)) {
    throw new TypeError('"encoding" must be a valid string encoding')
  }

  var length = byteLength(string, encoding) | 0
  that = createBuffer(that, length)

  var actual = that.write(string, encoding)

  if (actual !== length) {
    // Writing a hex string, for example, that contains invalid characters will
    // cause everything after the first invalid character to be ignored. (e.g.
    // 'abxxcd' will be treated as 'ab')
    that = that.slice(0, actual)
  }

  return that
}

function fromArrayLike (that, array) {
  var length = array.length < 0 ? 0 : checked(array.length) | 0
  that = createBuffer(that, length)
  for (var i = 0; i < length; i += 1) {
    that[i] = array[i] & 255
  }
  return that
}

function fromArrayBuffer (that, array, byteOffset, length) {
  array.byteLength // this throws if `array` is not a valid ArrayBuffer

  if (byteOffset < 0 || array.byteLength < byteOffset) {
    throw new RangeError('\'offset\' is out of bounds')
  }

  if (array.byteLength < byteOffset + (length || 0)) {
    throw new RangeError('\'length\' is out of bounds')
  }

  if (byteOffset === undefined && length === undefined) {
    array = new Uint8Array(array)
  } else if (length === undefined) {
    array = new Uint8Array(array, byteOffset)
  } else {
    array = new Uint8Array(array, byteOffset, length)
  }

  if (Buffer.TYPED_ARRAY_SUPPORT) {
    // Return an augmented `Uint8Array` instance, for best performance
    that = array
    that.__proto__ = Buffer.prototype
  } else {
    // Fallback: Return an object instance of the Buffer class
    that = fromArrayLike(that, array)
  }
  return that
}

function fromObject (that, obj) {
  if (Buffer.isBuffer(obj)) {
    var len = checked(obj.length) | 0
    that = createBuffer(that, len)

    if (that.length === 0) {
      return that
    }

    obj.copy(that, 0, 0, len)
    return that
  }

  if (obj) {
    if ((typeof ArrayBuffer !== 'undefined' &&
        obj.buffer instanceof ArrayBuffer) || 'length' in obj) {
      if (typeof obj.length !== 'number' || isnan(obj.length)) {
        return createBuffer(that, 0)
      }
      return fromArrayLike(that, obj)
    }

    if (obj.type === 'Buffer' && isArray(obj.data)) {
      return fromArrayLike(that, obj.data)
    }
  }

  throw new TypeError('First argument must be a string, Buffer, ArrayBuffer, Array, or array-like object.')
}

function checked (length) {
  // Note: cannot use `length < kMaxLength()` here because that fails when
  // length is NaN (which is otherwise coerced to zero.)
  if (length >= kMaxLength()) {
    throw new RangeError('Attempt to allocate Buffer larger than maximum ' +
                         'size: 0x' + kMaxLength().toString(16) + ' bytes')
  }
  return length | 0
}

function SlowBuffer (length) {
  if (+length != length) { // eslint-disable-line eqeqeq
    length = 0
  }
  return Buffer.alloc(+length)
}

Buffer.isBuffer = function isBuffer (b) {
  return !!(b != null && b._isBuffer)
}

Buffer.compare = function compare (a, b) {
  if (!Buffer.isBuffer(a) || !Buffer.isBuffer(b)) {
    throw new TypeError('Arguments must be Buffers')
  }

  if (a === b) return 0

  var x = a.length
  var y = b.length

  for (var i = 0, len = Math.min(x, y); i < len; ++i) {
    if (a[i] !== b[i]) {
      x = a[i]
      y = b[i]
      break
    }
  }

  if (x < y) return -1
  if (y < x) return 1
  return 0
}

Buffer.isEncoding = function isEncoding (encoding) {
  switch (String(encoding).toLowerCase()) {
    case 'hex':
    case 'utf8':
    case 'utf-8':
    case 'ascii':
    case 'latin1':
    case 'binary':
    case 'base64':
    case 'ucs2':
    case 'ucs-2':
    case 'utf16le':
    case 'utf-16le':
      return true
    default:
      return false
  }
}

Buffer.concat = function concat (list, length) {
  if (!isArray(list)) {
    throw new TypeError('"list" argument must be an Array of Buffers')
  }

  if (list.length === 0) {
    return Buffer.alloc(0)
  }

  var i
  if (length === undefined) {
    length = 0
    for (i = 0; i < list.length; ++i) {
      length += list[i].length
    }
  }

  var buffer = Buffer.allocUnsafe(length)
  var pos = 0
  for (i = 0; i < list.length; ++i) {
    var buf = list[i]
    if (!Buffer.isBuffer(buf)) {
      throw new TypeError('"list" argument must be an Array of Buffers')
    }
    buf.copy(buffer, pos)
    pos += buf.length
  }
  return buffer
}

function byteLength (string, encoding) {
  if (Buffer.isBuffer(string)) {
    return string.length
  }
  if (typeof ArrayBuffer !== 'undefined' && typeof ArrayBuffer.isView === 'function' &&
      (ArrayBuffer.isView(string) || string instanceof ArrayBuffer)) {
    return string.byteLength
  }
  if (typeof string !== 'string') {
    string = '' + string
  }

  var len = string.length
  if (len === 0) return 0

  // Use a for loop to avoid recursion
  var loweredCase = false
  for (;;) {
    switch (encoding) {
      case 'ascii':
      case 'latin1':
      case 'binary':
        return len
      case 'utf8':
      case 'utf-8':
      case undefined:
        return utf8ToBytes(string).length
      case 'ucs2':
      case 'ucs-2':
      case 'utf16le':
      case 'utf-16le':
        return len * 2
      case 'hex':
        return len >>> 1
      case 'base64':
        return base64ToBytes(string).length
      default:
        if (loweredCase) return utf8ToBytes(string).length // assume utf8
        encoding = ('' + encoding).toLowerCase()
        loweredCase = true
    }
  }
}
Buffer.byteLength = byteLength

function slowToString (encoding, start, end) {
  var loweredCase = false

  // No need to verify that "this.length <= MAX_UINT32" since it's a read-only
  // property of a typed array.

  // This behaves neither like String nor Uint8Array in that we set start/end
  // to their upper/lower bounds if the value passed is out of range.
  // undefined is handled specially as per ECMA-262 6th Edition,
  // Section 13.3.3.7 Runtime Semantics: KeyedBindingInitialization.
  if (start === undefined || start < 0) {
    start = 0
  }
  // Return early if start > this.length. Done here to prevent potential uint32
  // coercion fail below.
  if (start > this.length) {
    return ''
  }

  if (end === undefined || end > this.length) {
    end = this.length
  }

  if (end <= 0) {
    return ''
  }

  // Force coersion to uint32. This will also coerce falsey/NaN values to 0.
  end >>>= 0
  start >>>= 0

  if (end <= start) {
    return ''
  }

  if (!encoding) encoding = 'utf8'

  while (true) {
    switch (encoding) {
      case 'hex':
        return hexSlice(this, start, end)

      case 'utf8':
      case 'utf-8':
        return utf8Slice(this, start, end)

      case 'ascii':
        return asciiSlice(this, start, end)

      case 'latin1':
      case 'binary':
        return latin1Slice(this, start, end)

      case 'base64':
        return base64Slice(this, start, end)

      case 'ucs2':
      case 'ucs-2':
      case 'utf16le':
      case 'utf-16le':
        return utf16leSlice(this, start, end)

      default:
        if (loweredCase) throw new TypeError('Unknown encoding: ' + encoding)
        encoding = (encoding + '').toLowerCase()
        loweredCase = true
    }
  }
}

// The property is used by `Buffer.isBuffer` and `is-buffer` (in Safari 5-7) to detect
// Buffer instances.
Buffer.prototype._isBuffer = true

function swap (b, n, m) {
  var i = b[n]
  b[n] = b[m]
  b[m] = i
}

Buffer.prototype.swap16 = function swap16 () {
  var len = this.length
  if (len % 2 !== 0) {
    throw new RangeError('Buffer size must be a multiple of 16-bits')
  }
  for (var i = 0; i < len; i += 2) {
    swap(this, i, i + 1)
  }
  return this
}

Buffer.prototype.swap32 = function swap32 () {
  var len = this.length
  if (len % 4 !== 0) {
    throw new RangeError('Buffer size must be a multiple of 32-bits')
  }
  for (var i = 0; i < len; i += 4) {
    swap(this, i, i + 3)
    swap(this, i + 1, i + 2)
  }
  return this
}

Buffer.prototype.swap64 = function swap64 () {
  var len = this.length
  if (len % 8 !== 0) {
    throw new RangeError('Buffer size must be a multiple of 64-bits')
  }
  for (var i = 0; i < len; i += 8) {
    swap(this, i, i + 7)
    swap(this, i + 1, i + 6)
    swap(this, i + 2, i + 5)
    swap(this, i + 3, i + 4)
  }
  return this
}

Buffer.prototype.toString = function toString () {
  var length = this.length | 0
  if (length === 0) return ''
  if (arguments.length === 0) return utf8Slice(this, 0, length)
  return slowToString.apply(this, arguments)
}

Buffer.prototype.equals = function equals (b) {
  if (!Buffer.isBuffer(b)) throw new TypeError('Argument must be a Buffer')
  if (this === b) return true
  return Buffer.compare(this, b) === 0
}

Buffer.prototype.inspect = function inspect () {
  var str = ''
  var max = exports.INSPECT_MAX_BYTES
  if (this.length > 0) {
    str = this.toString('hex', 0, max).match(/.{2}/g).join(' ')
    if (this.length > max) str += ' ... '
  }
  return '<Buffer ' + str + '>'
}

Buffer.prototype.compare = function compare (target, start, end, thisStart, thisEnd) {
  if (!Buffer.isBuffer(target)) {
    throw new TypeError('Argument must be a Buffer')
  }

  if (start === undefined) {
    start = 0
  }
  if (end === undefined) {
    end = target ? target.length : 0
  }
  if (thisStart === undefined) {
    thisStart = 0
  }
  if (thisEnd === undefined) {
    thisEnd = this.length
  }

  if (start < 0 || end > target.length || thisStart < 0 || thisEnd > this.length) {
    throw new RangeError('out of range index')
  }

  if (thisStart >= thisEnd && start >= end) {
    return 0
  }
  if (thisStart >= thisEnd) {
    return -1
  }
  if (start >= end) {
    return 1
  }

  start >>>= 0
  end >>>= 0
  thisStart >>>= 0
  thisEnd >>>= 0

  if (this === target) return 0

  var x = thisEnd - thisStart
  var y = end - start
  var len = Math.min(x, y)

  var thisCopy = this.slice(thisStart, thisEnd)
  var targetCopy = target.slice(start, end)

  for (var i = 0; i < len; ++i) {
    if (thisCopy[i] !== targetCopy[i]) {
      x = thisCopy[i]
      y = targetCopy[i]
      break
    }
  }

  if (x < y) return -1
  if (y < x) return 1
  return 0
}

// Finds either the first index of `val` in `buffer` at offset >= `byteOffset`,
// OR the last index of `val` in `buffer` at offset <= `byteOffset`.
//
// Arguments:
// - buffer - a Buffer to search
// - val - a string, Buffer, or number
// - byteOffset - an index into `buffer`; will be clamped to an int32
// - encoding - an optional encoding, relevant is val is a string
// - dir - true for indexOf, false for lastIndexOf
function bidirectionalIndexOf (buffer, val, byteOffset, encoding, dir) {
  // Empty buffer means no match
  if (buffer.length === 0) return -1

  // Normalize byteOffset
  if (typeof byteOffset === 'string') {
    encoding = byteOffset
    byteOffset = 0
  } else if (byteOffset > 0x7fffffff) {
    byteOffset = 0x7fffffff
  } else if (byteOffset < -0x80000000) {
    byteOffset = -0x80000000
  }
  byteOffset = +byteOffset  // Coerce to Number.
  if (isNaN(byteOffset)) {
    // byteOffset: it it's undefined, null, NaN, "foo", etc, search whole buffer
    byteOffset = dir ? 0 : (buffer.length - 1)
  }

  // Normalize byteOffset: negative offsets start from the end of the buffer
  if (byteOffset < 0) byteOffset = buffer.length + byteOffset
  if (byteOffset >= buffer.length) {
    if (dir) return -1
    else byteOffset = buffer.length - 1
  } else if (byteOffset < 0) {
    if (dir) byteOffset = 0
    else return -1
  }

  // Normalize val
  if (typeof val === 'string') {
    val = Buffer.from(val, encoding)
  }

  // Finally, search either indexOf (if dir is true) or lastIndexOf
  if (Buffer.isBuffer(val)) {
    // Special case: looking for empty string/buffer always fails
    if (val.length === 0) {
      return -1
    }
    return arrayIndexOf(buffer, val, byteOffset, encoding, dir)
  } else if (typeof val === 'number') {
    val = val & 0xFF // Search for a byte value [0-255]
    if (Buffer.TYPED_ARRAY_SUPPORT &&
        typeof Uint8Array.prototype.indexOf === 'function') {
      if (dir) {
        return Uint8Array.prototype.indexOf.call(buffer, val, byteOffset)
      } else {
        return Uint8Array.prototype.lastIndexOf.call(buffer, val, byteOffset)
      }
    }
    return arrayIndexOf(buffer, [ val ], byteOffset, encoding, dir)
  }

  throw new TypeError('val must be string, number or Buffer')
}

function arrayIndexOf (arr, val, byteOffset, encoding, dir) {
  var indexSize = 1
  var arrLength = arr.length
  var valLength = val.length

  if (encoding !== undefined) {
    encoding = String(encoding).toLowerCase()
    if (encoding === 'ucs2' || encoding === 'ucs-2' ||
        encoding === 'utf16le' || encoding === 'utf-16le') {
      if (arr.length < 2 || val.length < 2) {
        return -1
      }
      indexSize = 2
      arrLength /= 2
      valLength /= 2
      byteOffset /= 2
    }
  }

  function read (buf, i) {
    if (indexSize === 1) {
      return buf[i]
    } else {
      return buf.readUInt16BE(i * indexSize)
    }
  }

  var i
  if (dir) {
    var foundIndex = -1
    for (i = byteOffset; i < arrLength; i++) {
      if (read(arr, i) === read(val, foundIndex === -1 ? 0 : i - foundIndex)) {
        if (foundIndex === -1) foundIndex = i
        if (i - foundIndex + 1 === valLength) return foundIndex * indexSize
      } else {
        if (foundIndex !== -1) i -= i - foundIndex
        foundIndex = -1
      }
    }
  } else {
    if (byteOffset + valLength > arrLength) byteOffset = arrLength - valLength
    for (i = byteOffset; i >= 0; i--) {
      var found = true
      for (var j = 0; j < valLength; j++) {
        if (read(arr, i + j) !== read(val, j)) {
          found = false
          break
        }
      }
      if (found) return i
    }
  }

  return -1
}

Buffer.prototype.includes = function includes (val, byteOffset, encoding) {
  return this.indexOf(val, byteOffset, encoding) !== -1
}

Buffer.prototype.indexOf = function indexOf (val, byteOffset, encoding) {
  return bidirectionalIndexOf(this, val, byteOffset, encoding, true)
}

Buffer.prototype.lastIndexOf = function lastIndexOf (val, byteOffset, encoding) {
  return bidirectionalIndexOf(this, val, byteOffset, encoding, false)
}

function hexWrite (buf, string, offset, length) {
  offset = Number(offset) || 0
  var remaining = buf.length - offset
  if (!length) {
    length = remaining
  } else {
    length = Number(length)
    if (length > remaining) {
      length = remaining
    }
  }

  // must be an even number of digits
  var strLen = string.length
  if (strLen % 2 !== 0) throw new TypeError('Invalid hex string')

  if (length > strLen / 2) {
    length = strLen / 2
  }
  for (var i = 0; i < length; ++i) {
    var parsed = parseInt(string.substr(i * 2, 2), 16)
    if (isNaN(parsed)) return i
    buf[offset + i] = parsed
  }
  return i
}

function utf8Write (buf, string, offset, length) {
  return blitBuffer(utf8ToBytes(string, buf.length - offset), buf, offset, length)
}

function asciiWrite (buf, string, offset, length) {
  return blitBuffer(asciiToBytes(string), buf, offset, length)
}

function latin1Write (buf, string, offset, length) {
  return asciiWrite(buf, string, offset, length)
}

function base64Write (buf, string, offset, length) {
  return blitBuffer(base64ToBytes(string), buf, offset, length)
}

function ucs2Write (buf, string, offset, length) {
  return blitBuffer(utf16leToBytes(string, buf.length - offset), buf, offset, length)
}

Buffer.prototype.write = function write (string, offset, length, encoding) {
  // Buffer#write(string)
  if (offset === undefined) {
    encoding = 'utf8'
    length = this.length
    offset = 0
  // Buffer#write(string, encoding)
  } else if (length === undefined && typeof offset === 'string') {
    encoding = offset
    length = this.length
    offset = 0
  // Buffer#write(string, offset[, length][, encoding])
  } else if (isFinite(offset)) {
    offset = offset | 0
    if (isFinite(length)) {
      length = length | 0
      if (encoding === undefined) encoding = 'utf8'
    } else {
      encoding = length
      length = undefined
    }
  // legacy write(string, encoding, offset, length) - remove in v0.13
  } else {
    throw new Error(
      'Buffer.write(string, encoding, offset[, length]) is no longer supported'
    )
  }

  var remaining = this.length - offset
  if (length === undefined || length > remaining) length = remaining

  if ((string.length > 0 && (length < 0 || offset < 0)) || offset > this.length) {
    throw new RangeError('Attempt to write outside buffer bounds')
  }

  if (!encoding) encoding = 'utf8'

  var loweredCase = false
  for (;;) {
    switch (encoding) {
      case 'hex':
        return hexWrite(this, string, offset, length)

      case 'utf8':
      case 'utf-8':
        return utf8Write(this, string, offset, length)

      case 'ascii':
        return asciiWrite(this, string, offset, length)

      case 'latin1':
      case 'binary':
        return latin1Write(this, string, offset, length)

      case 'base64':
        // Warning: maxLength not taken into account in base64Write
        return base64Write(this, string, offset, length)

      case 'ucs2':
      case 'ucs-2':
      case 'utf16le':
      case 'utf-16le':
        return ucs2Write(this, string, offset, length)

      default:
        if (loweredCase) throw new TypeError('Unknown encoding: ' + encoding)
        encoding = ('' + encoding).toLowerCase()
        loweredCase = true
    }
  }
}

Buffer.prototype.toJSON = function toJSON () {
  return {
    type: 'Buffer',
    data: Array.prototype.slice.call(this._arr || this, 0)
  }
}

function base64Slice (buf, start, end) {
  if (start === 0 && end === buf.length) {
    return base64.fromByteArray(buf)
  } else {
    return base64.fromByteArray(buf.slice(start, end))
  }
}

function utf8Slice (buf, start, end) {
  end = Math.min(buf.length, end)
  var res = []

  var i = start
  while (i < end) {
    var firstByte = buf[i]
    var codePoint = null
    var bytesPerSequence = (firstByte > 0xEF) ? 4
      : (firstByte > 0xDF) ? 3
      : (firstByte > 0xBF) ? 2
      : 1

    if (i + bytesPerSequence <= end) {
      var secondByte, thirdByte, fourthByte, tempCodePoint

      switch (bytesPerSequence) {
        case 1:
          if (firstByte < 0x80) {
            codePoint = firstByte
          }
          break
        case 2:
          secondByte = buf[i + 1]
          if ((secondByte & 0xC0) === 0x80) {
            tempCodePoint = (firstByte & 0x1F) << 0x6 | (secondByte & 0x3F)
            if (tempCodePoint > 0x7F) {
              codePoint = tempCodePoint
            }
          }
          break
        case 3:
          secondByte = buf[i + 1]
          thirdByte = buf[i + 2]
          if ((secondByte & 0xC0) === 0x80 && (thirdByte & 0xC0) === 0x80) {
            tempCodePoint = (firstByte & 0xF) << 0xC | (secondByte & 0x3F) << 0x6 | (thirdByte & 0x3F)
            if (tempCodePoint > 0x7FF && (tempCodePoint < 0xD800 || tempCodePoint > 0xDFFF)) {
              codePoint = tempCodePoint
            }
          }
          break
        case 4:
          secondByte = buf[i + 1]
          thirdByte = buf[i + 2]
          fourthByte = buf[i + 3]
          if ((secondByte & 0xC0) === 0x80 && (thirdByte & 0xC0) === 0x80 && (fourthByte & 0xC0) === 0x80) {
            tempCodePoint = (firstByte & 0xF) << 0x12 | (secondByte & 0x3F) << 0xC | (thirdByte & 0x3F) << 0x6 | (fourthByte & 0x3F)
            if (tempCodePoint > 0xFFFF && tempCodePoint < 0x110000) {
              codePoint = tempCodePoint
            }
          }
      }
    }

    if (codePoint === null) {
      // we did not generate a valid codePoint so insert a
      // replacement char (U+FFFD) and advance only 1 byte
      codePoint = 0xFFFD
      bytesPerSequence = 1
    } else if (codePoint > 0xFFFF) {
      // encode to utf16 (surrogate pair dance)
      codePoint -= 0x10000
      res.push(codePoint >>> 10 & 0x3FF | 0xD800)
      codePoint = 0xDC00 | codePoint & 0x3FF
    }

    res.push(codePoint)
    i += bytesPerSequence
  }

  return decodeCodePointsArray(res)
}

// Based on http://stackoverflow.com/a/22747272/680742, the browser with
// the lowest limit is Chrome, with 0x10000 args.
// We go 1 magnitude less, for safety
var MAX_ARGUMENTS_LENGTH = 0x1000

function decodeCodePointsArray (codePoints) {
  var len = codePoints.length
  if (len <= MAX_ARGUMENTS_LENGTH) {
    return String.fromCharCode.apply(String, codePoints) // avoid extra slice()
  }

  // Decode in chunks to avoid "call stack size exceeded".
  var res = ''
  var i = 0
  while (i < len) {
    res += String.fromCharCode.apply(
      String,
      codePoints.slice(i, i += MAX_ARGUMENTS_LENGTH)
    )
  }
  return res
}

function asciiSlice (buf, start, end) {
  var ret = ''
  end = Math.min(buf.length, end)

  for (var i = start; i < end; ++i) {
    ret += String.fromCharCode(buf[i] & 0x7F)
  }
  return ret
}

function latin1Slice (buf, start, end) {
  var ret = ''
  end = Math.min(buf.length, end)

  for (var i = start; i < end; ++i) {
    ret += String.fromCharCode(buf[i])
  }
  return ret
}

function hexSlice (buf, start, end) {
  var len = buf.length

  if (!start || start < 0) start = 0
  if (!end || end < 0 || end > len) end = len

  var out = ''
  for (var i = start; i < end; ++i) {
    out += toHex(buf[i])
  }
  return out
}

function utf16leSlice (buf, start, end) {
  var bytes = buf.slice(start, end)
  var res = ''
  for (var i = 0; i < bytes.length; i += 2) {
    res += String.fromCharCode(bytes[i] + bytes[i + 1] * 256)
  }
  return res
}

Buffer.prototype.slice = function slice (start, end) {
  var len = this.length
  start = ~~start
  end = end === undefined ? len : ~~end

  if (start < 0) {
    start += len
    if (start < 0) start = 0
  } else if (start > len) {
    start = len
  }

  if (end < 0) {
    end += len
    if (end < 0) end = 0
  } else if (end > len) {
    end = len
  }

  if (end < start) end = start

  var newBuf
  if (Buffer.TYPED_ARRAY_SUPPORT) {
    newBuf = this.subarray(start, end)
    newBuf.__proto__ = Buffer.prototype
  } else {
    var sliceLen = end - start
    newBuf = new Buffer(sliceLen, undefined)
    for (var i = 0; i < sliceLen; ++i) {
      newBuf[i] = this[i + start]
    }
  }

  return newBuf
}

/*
 * Need to make sure that buffer isn't trying to write out of bounds.
 */
function checkOffset (offset, ext, length) {
  if ((offset % 1) !== 0 || offset < 0) throw new RangeError('offset is not uint')
  if (offset + ext > length) throw new RangeError('Trying to access beyond buffer length')
}

Buffer.prototype.readUIntLE = function readUIntLE (offset, byteLength, noAssert) {
  offset = offset | 0
  byteLength = byteLength | 0
  if (!noAssert) checkOffset(offset, byteLength, this.length)

  var val = this[offset]
  var mul = 1
  var i = 0
  while (++i < byteLength && (mul *= 0x100)) {
    val += this[offset + i] * mul
  }

  return val
}

Buffer.prototype.readUIntBE = function readUIntBE (offset, byteLength, noAssert) {
  offset = offset | 0
  byteLength = byteLength | 0
  if (!noAssert) {
    checkOffset(offset, byteLength, this.length)
  }

  var val = this[offset + --byteLength]
  var mul = 1
  while (byteLength > 0 && (mul *= 0x100)) {
    val += this[offset + --byteLength] * mul
  }

  return val
}

Buffer.prototype.readUInt8 = function readUInt8 (offset, noAssert) {
  if (!noAssert) checkOffset(offset, 1, this.length)
  return this[offset]
}

Buffer.prototype.readUInt16LE = function readUInt16LE (offset, noAssert) {
  if (!noAssert) checkOffset(offset, 2, this.length)
  return this[offset] | (this[offset + 1] << 8)
}

Buffer.prototype.readUInt16BE = function readUInt16BE (offset, noAssert) {
  if (!noAssert) checkOffset(offset, 2, this.length)
  return (this[offset] << 8) | this[offset + 1]
}

Buffer.prototype.readUInt32LE = function readUInt32LE (offset, noAssert) {
  if (!noAssert) checkOffset(offset, 4, this.length)

  return ((this[offset]) |
      (this[offset + 1] << 8) |
      (this[offset + 2] << 16)) +
      (this[offset + 3] * 0x1000000)
}

Buffer.prototype.readUInt32BE = function readUInt32BE (offset, noAssert) {
  if (!noAssert) checkOffset(offset, 4, this.length)

  return (this[offset] * 0x1000000) +
    ((this[offset + 1] << 16) |
    (this[offset + 2] << 8) |
    this[offset + 3])
}

Buffer.prototype.readIntLE = function readIntLE (offset, byteLength, noAssert) {
  offset = offset | 0
  byteLength = byteLength | 0
  if (!noAssert) checkOffset(offset, byteLength, this.length)

  var val = this[offset]
  var mul = 1
  var i = 0
  while (++i < byteLength && (mul *= 0x100)) {
    val += this[offset + i] * mul
  }
  mul *= 0x80

  if (val >= mul) val -= Math.pow(2, 8 * byteLength)

  return val
}

Buffer.prototype.readIntBE = function readIntBE (offset, byteLength, noAssert) {
  offset = offset | 0
  byteLength = byteLength | 0
  if (!noAssert) checkOffset(offset, byteLength, this.length)

  var i = byteLength
  var mul = 1
  var val = this[offset + --i]
  while (i > 0 && (mul *= 0x100)) {
    val += this[offset + --i] * mul
  }
  mul *= 0x80

  if (val >= mul) val -= Math.pow(2, 8 * byteLength)

  return val
}

Buffer.prototype.readInt8 = function readInt8 (offset, noAssert) {
  if (!noAssert) checkOffset(offset, 1, this.length)
  if (!(this[offset] & 0x80)) return (this[offset])
  return ((0xff - this[offset] + 1) * -1)
}

Buffer.prototype.readInt16LE = function readInt16LE (offset, noAssert) {
  if (!noAssert) checkOffset(offset, 2, this.length)
  var val = this[offset] | (this[offset + 1] << 8)
  return (val & 0x8000) ? val | 0xFFFF0000 : val
}

Buffer.prototype.readInt16BE = function readInt16BE (offset, noAssert) {
  if (!noAssert) checkOffset(offset, 2, this.length)
  var val = this[offset + 1] | (this[offset] << 8)
  return (val & 0x8000) ? val | 0xFFFF0000 : val
}

Buffer.prototype.readInt32LE = function readInt32LE (offset, noAssert) {
  if (!noAssert) checkOffset(offset, 4, this.length)

  return (this[offset]) |
    (this[offset + 1] << 8) |
    (this[offset + 2] << 16) |
    (this[offset + 3] << 24)
}

Buffer.prototype.readInt32BE = function readInt32BE (offset, noAssert) {
  if (!noAssert) checkOffset(offset, 4, this.length)

  return (this[offset] << 24) |
    (this[offset + 1] << 16) |
    (this[offset + 2] << 8) |
    (this[offset + 3])
}

Buffer.prototype.readFloatLE = function readFloatLE (offset, noAssert) {
  if (!noAssert) checkOffset(offset, 4, this.length)
  return ieee754.read(this, offset, true, 23, 4)
}

Buffer.prototype.readFloatBE = function readFloatBE (offset, noAssert) {
  if (!noAssert) checkOffset(offset, 4, this.length)
  return ieee754.read(this, offset, false, 23, 4)
}

Buffer.prototype.readDoubleLE = function readDoubleLE (offset, noAssert) {
  if (!noAssert) checkOffset(offset, 8, this.length)
  return ieee754.read(this, offset, true, 52, 8)
}

Buffer.prototype.readDoubleBE = function readDoubleBE (offset, noAssert) {
  if (!noAssert) checkOffset(offset, 8, this.length)
  return ieee754.read(this, offset, false, 52, 8)
}

function checkInt (buf, value, offset, ext, max, min) {
  if (!Buffer.isBuffer(buf)) throw new TypeError('"buffer" argument must be a Buffer instance')
  if (value > max || value < min) throw new RangeError('"value" argument is out of bounds')
  if (offset + ext > buf.length) throw new RangeError('Index out of range')
}

Buffer.prototype.writeUIntLE = function writeUIntLE (value, offset, byteLength, noAssert) {
  value = +value
  offset = offset | 0
  byteLength = byteLength | 0
  if (!noAssert) {
    var maxBytes = Math.pow(2, 8 * byteLength) - 1
    checkInt(this, value, offset, byteLength, maxBytes, 0)
  }

  var mul = 1
  var i = 0
  this[offset] = value & 0xFF
  while (++i < byteLength && (mul *= 0x100)) {
    this[offset + i] = (value / mul) & 0xFF
  }

  return offset + byteLength
}

Buffer.prototype.writeUIntBE = function writeUIntBE (value, offset, byteLength, noAssert) {
  value = +value
  offset = offset | 0
  byteLength = byteLength | 0
  if (!noAssert) {
    var maxBytes = Math.pow(2, 8 * byteLength) - 1
    checkInt(this, value, offset, byteLength, maxBytes, 0)
  }

  var i = byteLength - 1
  var mul = 1
  this[offset + i] = value & 0xFF
  while (--i >= 0 && (mul *= 0x100)) {
    this[offset + i] = (value / mul) & 0xFF
  }

  return offset + byteLength
}

Buffer.prototype.writeUInt8 = function writeUInt8 (value, offset, noAssert) {
  value = +value
  offset = offset | 0
  if (!noAssert) checkInt(this, value, offset, 1, 0xff, 0)
  if (!Buffer.TYPED_ARRAY_SUPPORT) value = Math.floor(value)
  this[offset] = (value & 0xff)
  return offset + 1
}

function objectWriteUInt16 (buf, value, offset, littleEndian) {
  if (value < 0) value = 0xffff + value + 1
  for (var i = 0, j = Math.min(buf.length - offset, 2); i < j; ++i) {
    buf[offset + i] = (value & (0xff << (8 * (littleEndian ? i : 1 - i)))) >>>
      (littleEndian ? i : 1 - i) * 8
  }
}

Buffer.prototype.writeUInt16LE = function writeUInt16LE (value, offset, noAssert) {
  value = +value
  offset = offset | 0
  if (!noAssert) checkInt(this, value, offset, 2, 0xffff, 0)
  if (Buffer.TYPED_ARRAY_SUPPORT) {
    this[offset] = (value & 0xff)
    this[offset + 1] = (value >>> 8)
  } else {
    objectWriteUInt16(this, value, offset, true)
  }
  return offset + 2
}

Buffer.prototype.writeUInt16BE = function writeUInt16BE (value, offset, noAssert) {
  value = +value
  offset = offset | 0
  if (!noAssert) checkInt(this, value, offset, 2, 0xffff, 0)
  if (Buffer.TYPED_ARRAY_SUPPORT) {
    this[offset] = (value >>> 8)
    this[offset + 1] = (value & 0xff)
  } else {
    objectWriteUInt16(this, value, offset, false)
  }
  return offset + 2
}

function objectWriteUInt32 (buf, value, offset, littleEndian) {
  if (value < 0) value = 0xffffffff + value + 1
  for (var i = 0, j = Math.min(buf.length - offset, 4); i < j; ++i) {
    buf[offset + i] = (value >>> (littleEndian ? i : 3 - i) * 8) & 0xff
  }
}

Buffer.prototype.writeUInt32LE = function writeUInt32LE (value, offset, noAssert) {
  value = +value
  offset = offset | 0
  if (!noAssert) checkInt(this, value, offset, 4, 0xffffffff, 0)
  if (Buffer.TYPED_ARRAY_SUPPORT) {
    this[offset + 3] = (value >>> 24)
    this[offset + 2] = (value >>> 16)
    this[offset + 1] = (value >>> 8)
    this[offset] = (value & 0xff)
  } else {
    objectWriteUInt32(this, value, offset, true)
  }
  return offset + 4
}

Buffer.prototype.writeUInt32BE = function writeUInt32BE (value, offset, noAssert) {
  value = +value
  offset = offset | 0
  if (!noAssert) checkInt(this, value, offset, 4, 0xffffffff, 0)
  if (Buffer.TYPED_ARRAY_SUPPORT) {
    this[offset] = (value >>> 24)
    this[offset + 1] = (value >>> 16)
    this[offset + 2] = (value >>> 8)
    this[offset + 3] = (value & 0xff)
  } else {
    objectWriteUInt32(this, value, offset, false)
  }
  return offset + 4
}

Buffer.prototype.writeIntLE = function writeIntLE (value, offset, byteLength, noAssert) {
  value = +value
  offset = offset | 0
  if (!noAssert) {
    var limit = Math.pow(2, 8 * byteLength - 1)

    checkInt(this, value, offset, byteLength, limit - 1, -limit)
  }

  var i = 0
  var mul = 1
  var sub = 0
  this[offset] = value & 0xFF
  while (++i < byteLength && (mul *= 0x100)) {
    if (value < 0 && sub === 0 && this[offset + i - 1] !== 0) {
      sub = 1
    }
    this[offset + i] = ((value / mul) >> 0) - sub & 0xFF
  }

  return offset + byteLength
}

Buffer.prototype.writeIntBE = function writeIntBE (value, offset, byteLength, noAssert) {
  value = +value
  offset = offset | 0
  if (!noAssert) {
    var limit = Math.pow(2, 8 * byteLength - 1)

    checkInt(this, value, offset, byteLength, limit - 1, -limit)
  }

  var i = byteLength - 1
  var mul = 1
  var sub = 0
  this[offset + i] = value & 0xFF
  while (--i >= 0 && (mul *= 0x100)) {
    if (value < 0 && sub === 0 && this[offset + i + 1] !== 0) {
      sub = 1
    }
    this[offset + i] = ((value / mul) >> 0) - sub & 0xFF
  }

  return offset + byteLength
}

Buffer.prototype.writeInt8 = function writeInt8 (value, offset, noAssert) {
  value = +value
  offset = offset | 0
  if (!noAssert) checkInt(this, value, offset, 1, 0x7f, -0x80)
  if (!Buffer.TYPED_ARRAY_SUPPORT) value = Math.floor(value)
  if (value < 0) value = 0xff + value + 1
  this[offset] = (value & 0xff)
  return offset + 1
}

Buffer.prototype.writeInt16LE = function writeInt16LE (value, offset, noAssert) {
  value = +value
  offset = offset | 0
  if (!noAssert) checkInt(this, value, offset, 2, 0x7fff, -0x8000)
  if (Buffer.TYPED_ARRAY_SUPPORT) {
    this[offset] = (value & 0xff)
    this[offset + 1] = (value >>> 8)
  } else {
    objectWriteUInt16(this, value, offset, true)
  }
  return offset + 2
}

Buffer.prototype.writeInt16BE = function writeInt16BE (value, offset, noAssert) {
  value = +value
  offset = offset | 0
  if (!noAssert) checkInt(this, value, offset, 2, 0x7fff, -0x8000)
  if (Buffer.TYPED_ARRAY_SUPPORT) {
    this[offset] = (value >>> 8)
    this[offset + 1] = (value & 0xff)
  } else {
    objectWriteUInt16(this, value, offset, false)
  }
  return offset + 2
}

Buffer.prototype.writeInt32LE = function writeInt32LE (value, offset, noAssert) {
  value = +value
  offset = offset | 0
  if (!noAssert) checkInt(this, value, offset, 4, 0x7fffffff, -0x80000000)
  if (Buffer.TYPED_ARRAY_SUPPORT) {
    this[offset] = (value & 0xff)
    this[offset + 1] = (value >>> 8)
    this[offset + 2] = (value >>> 16)
    this[offset + 3] = (value >>> 24)
  } else {
    objectWriteUInt32(this, value, offset, true)
  }
  return offset + 4
}

Buffer.prototype.writeInt32BE = function writeInt32BE (value, offset, noAssert) {
  value = +value
  offset = offset | 0
  if (!noAssert) checkInt(this, value, offset, 4, 0x7fffffff, -0x80000000)
  if (value < 0) value = 0xffffffff + value + 1
  if (Buffer.TYPED_ARRAY_SUPPORT) {
    this[offset] = (value >>> 24)
    this[offset + 1] = (value >>> 16)
    this[offset + 2] = (value >>> 8)
    this[offset + 3] = (value & 0xff)
  } else {
    objectWriteUInt32(this, value, offset, false)
  }
  return offset + 4
}

function checkIEEE754 (buf, value, offset, ext, max, min) {
  if (offset + ext > buf.length) throw new RangeError('Index out of range')
  if (offset < 0) throw new RangeError('Index out of range')
}

function writeFloat (buf, value, offset, littleEndian, noAssert) {
  if (!noAssert) {
    checkIEEE754(buf, value, offset, 4, 3.4028234663852886e+38, -3.4028234663852886e+38)
  }
  ieee754.write(buf, value, offset, littleEndian, 23, 4)
  return offset + 4
}

Buffer.prototype.writeFloatLE = function writeFloatLE (value, offset, noAssert) {
  return writeFloat(this, value, offset, true, noAssert)
}

Buffer.prototype.writeFloatBE = function writeFloatBE (value, offset, noAssert) {
  return writeFloat(this, value, offset, false, noAssert)
}

function writeDouble (buf, value, offset, littleEndian, noAssert) {
  if (!noAssert) {
    checkIEEE754(buf, value, offset, 8, 1.7976931348623157E+308, -1.7976931348623157E+308)
  }
  ieee754.write(buf, value, offset, littleEndian, 52, 8)
  return offset + 8
}

Buffer.prototype.writeDoubleLE = function writeDoubleLE (value, offset, noAssert) {
  return writeDouble(this, value, offset, true, noAssert)
}

Buffer.prototype.writeDoubleBE = function writeDoubleBE (value, offset, noAssert) {
  return writeDouble(this, value, offset, false, noAssert)
}

// copy(targetBuffer, targetStart=0, sourceStart=0, sourceEnd=buffer.length)
Buffer.prototype.copy = function copy (target, targetStart, start, end) {
  if (!start) start = 0
  if (!end && end !== 0) end = this.length
  if (targetStart >= target.length) targetStart = target.length
  if (!targetStart) targetStart = 0
  if (end > 0 && end < start) end = start

  // Copy 0 bytes; we're done
  if (end === start) return 0
  if (target.length === 0 || this.length === 0) return 0

  // Fatal error conditions
  if (targetStart < 0) {
    throw new RangeError('targetStart out of bounds')
  }
  if (start < 0 || start >= this.length) throw new RangeError('sourceStart out of bounds')
  if (end < 0) throw new RangeError('sourceEnd out of bounds')

  // Are we oob?
  if (end > this.length) end = this.length
  if (target.length - targetStart < end - start) {
    end = target.length - targetStart + start
  }

  var len = end - start
  var i

  if (this === target && start < targetStart && targetStart < end) {
    // descending copy from end
    for (i = len - 1; i >= 0; --i) {
      target[i + targetStart] = this[i + start]
    }
  } else if (len < 1000 || !Buffer.TYPED_ARRAY_SUPPORT) {
    // ascending copy from start
    for (i = 0; i < len; ++i) {
      target[i + targetStart] = this[i + start]
    }
  } else {
    Uint8Array.prototype.set.call(
      target,
      this.subarray(start, start + len),
      targetStart
    )
  }

  return len
}

// Usage:
//    buffer.fill(number[, offset[, end]])
//    buffer.fill(buffer[, offset[, end]])
//    buffer.fill(string[, offset[, end]][, encoding])
Buffer.prototype.fill = function fill (val, start, end, encoding) {
  // Handle string cases:
  if (typeof val === 'string') {
    if (typeof start === 'string') {
      encoding = start
      start = 0
      end = this.length
    } else if (typeof end === 'string') {
      encoding = end
      end = this.length
    }
    if (val.length === 1) {
      var code = val.charCodeAt(0)
      if (code < 256) {
        val = code
      }
    }
    if (encoding !== undefined && typeof encoding !== 'string') {
      throw new TypeError('encoding must be a string')
    }
    if (typeof encoding === 'string' && !Buffer.isEncoding(encoding)) {
      throw new TypeError('Unknown encoding: ' + encoding)
    }
  } else if (typeof val === 'number') {
    val = val & 255
  }

  // Invalid ranges are not set to a default, so can range check early.
  if (start < 0 || this.length < start || this.length < end) {
    throw new RangeError('Out of range index')
  }

  if (end <= start) {
    return this
  }

  start = start >>> 0
  end = end === undefined ? this.length : end >>> 0

  if (!val) val = 0

  var i
  if (typeof val === 'number') {
    for (i = start; i < end; ++i) {
      this[i] = val
    }
  } else {
    var bytes = Buffer.isBuffer(val)
      ? val
      : utf8ToBytes(new Buffer(val, encoding).toString())
    var len = bytes.length
    for (i = 0; i < end - start; ++i) {
      this[i + start] = bytes[i % len]
    }
  }

  return this
}

// HELPER FUNCTIONS
// ================

var INVALID_BASE64_RE = /[^+\/0-9A-Za-z-_]/g

function base64clean (str) {
  // Node strips out invalid characters like \n and \t from the string, base64-js does not
  str = stringtrim(str).replace(INVALID_BASE64_RE, '')
  // Node converts strings with length < 2 to ''
  if (str.length < 2) return ''
  // Node allows for non-padded base64 strings (missing trailing ===), base64-js does not
  while (str.length % 4 !== 0) {
    str = str + '='
  }
  return str
}

function stringtrim (str) {
  if (str.trim) return str.trim()
  return str.replace(/^\s+|\s+$/g, '')
}

function toHex (n) {
  if (n < 16) return '0' + n.toString(16)
  return n.toString(16)
}

function utf8ToBytes (string, units) {
  units = units || Infinity
  var codePoint
  var length = string.length
  var leadSurrogate = null
  var bytes = []

  for (var i = 0; i < length; ++i) {
    codePoint = string.charCodeAt(i)

    // is surrogate component
    if (codePoint > 0xD7FF && codePoint < 0xE000) {
      // last char was a lead
      if (!leadSurrogate) {
        // no lead yet
        if (codePoint > 0xDBFF) {
          // unexpected trail
          if ((units -= 3) > -1) bytes.push(0xEF, 0xBF, 0xBD)
          continue
        } else if (i + 1 === length) {
          // unpaired lead
          if ((units -= 3) > -1) bytes.push(0xEF, 0xBF, 0xBD)
          continue
        }

        // valid lead
        leadSurrogate = codePoint

        continue
      }

      // 2 leads in a row
      if (codePoint < 0xDC00) {
        if ((units -= 3) > -1) bytes.push(0xEF, 0xBF, 0xBD)
        leadSurrogate = codePoint
        continue
      }

      // valid surrogate pair
      codePoint = (leadSurrogate - 0xD800 << 10 | codePoint - 0xDC00) + 0x10000
    } else if (leadSurrogate) {
      // valid bmp char, but last char was a lead
      if ((units -= 3) > -1) bytes.push(0xEF, 0xBF, 0xBD)
    }

    leadSurrogate = null

    // encode utf8
    if (codePoint < 0x80) {
      if ((units -= 1) < 0) break
      bytes.push(codePoint)
    } else if (codePoint < 0x800) {
      if ((units -= 2) < 0) break
      bytes.push(
        codePoint >> 0x6 | 0xC0,
        codePoint & 0x3F | 0x80
      )
    } else if (codePoint < 0x10000) {
      if ((units -= 3) < 0) break
      bytes.push(
        codePoint >> 0xC | 0xE0,
        codePoint >> 0x6 & 0x3F | 0x80,
        codePoint & 0x3F | 0x80
      )
    } else if (codePoint < 0x110000) {
      if ((units -= 4) < 0) break
      bytes.push(
        codePoint >> 0x12 | 0xF0,
        codePoint >> 0xC & 0x3F | 0x80,
        codePoint >> 0x6 & 0x3F | 0x80,
        codePoint & 0x3F | 0x80
      )
    } else {
      throw new Error('Invalid code point')
    }
  }

  return bytes
}

function asciiToBytes (str) {
  var byteArray = []
  for (var i = 0; i < str.length; ++i) {
    // Node's code seems to be doing this and not & 0x7F..
    byteArray.push(str.charCodeAt(i) & 0xFF)
  }
  return byteArray
}

function utf16leToBytes (str, units) {
  var c, hi, lo
  var byteArray = []
  for (var i = 0; i < str.length; ++i) {
    if ((units -= 2) < 0) break

    c = str.charCodeAt(i)
    hi = c >> 8
    lo = c % 256
    byteArray.push(lo)
    byteArray.push(hi)
  }

  return byteArray
}

function base64ToBytes (str) {
  return base64.toByteArray(base64clean(str))
}

function blitBuffer (src, dst, offset, length) {
  for (var i = 0; i < length; ++i) {
    if ((i + offset >= dst.length) || (i >= src.length)) break
    dst[i + offset] = src[i]
  }
  return i
}

function isnan (val) {
  return val !== val // eslint-disable-line no-self-compare
}

}).call(this,typeof global !== "undefined" ? global : typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{"base64-js":213,"ieee754":217,"isarray":219}],216:[function(require,module,exports){
// Copyright Joyent, Inc. and other Node contributors.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit
// persons to whom the Software is furnished to do so, subject to the
// following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
// USE OR OTHER DEALINGS IN THE SOFTWARE.

function EventEmitter() {
  this._events = this._events || {};
  this._maxListeners = this._maxListeners || undefined;
}
module.exports = EventEmitter;

// Backwards-compat with node 0.10.x
EventEmitter.EventEmitter = EventEmitter;

EventEmitter.prototype._events = undefined;
EventEmitter.prototype._maxListeners = undefined;

// By default EventEmitters will print a warning if more than 10 listeners are
// added to it. This is a useful default which helps finding memory leaks.
EventEmitter.defaultMaxListeners = 10;

// Obviously not all Emitters should be limited to 10. This function allows
// that to be increased. Set to zero for unlimited.
EventEmitter.prototype.setMaxListeners = function(n) {
  if (!isNumber(n) || n < 0 || isNaN(n))
    throw TypeError('n must be a positive number');
  this._maxListeners = n;
  return this;
};

EventEmitter.prototype.emit = function(type) {
  var er, handler, len, args, i, listeners;

  if (!this._events)
    this._events = {};

  // If there is no 'error' event listener then throw.
  if (type === 'error') {
    if (!this._events.error ||
        (isObject(this._events.error) && !this._events.error.length)) {
      er = arguments[1];
      if (er instanceof Error) {
        throw er; // Unhandled 'error' event
      } else {
        // At least give some kind of context to the user
        var err = new Error('Uncaught, unspecified "error" event. (' + er + ')');
        err.context = er;
        throw err;
      }
    }
  }

  handler = this._events[type];

  if (isUndefined(handler))
    return false;

  if (isFunction(handler)) {
    switch (arguments.length) {
      // fast cases
      case 1:
        handler.call(this);
        break;
      case 2:
        handler.call(this, arguments[1]);
        break;
      case 3:
        handler.call(this, arguments[1], arguments[2]);
        break;
      // slower
      default:
        args = Array.prototype.slice.call(arguments, 1);
        handler.apply(this, args);
    }
  } else if (isObject(handler)) {
    args = Array.prototype.slice.call(arguments, 1);
    listeners = handler.slice();
    len = listeners.length;
    for (i = 0; i < len; i++)
      listeners[i].apply(this, args);
  }

  return true;
};

EventEmitter.prototype.addListener = function(type, listener) {
  var m;

  if (!isFunction(listener))
    throw TypeError('listener must be a function');

  if (!this._events)
    this._events = {};

  // To avoid recursion in the case that type === "newListener"! Before
  // adding it to the listeners, first emit "newListener".
  if (this._events.newListener)
    this.emit('newListener', type,
              isFunction(listener.listener) ?
              listener.listener : listener);

  if (!this._events[type])
    // Optimize the case of one listener. Don't need the extra array object.
    this._events[type] = listener;
  else if (isObject(this._events[type]))
    // If we've already got an array, just append.
    this._events[type].push(listener);
  else
    // Adding the second element, need to change to array.
    this._events[type] = [this._events[type], listener];

  // Check for listener leak
  if (isObject(this._events[type]) && !this._events[type].warned) {
    if (!isUndefined(this._maxListeners)) {
      m = this._maxListeners;
    } else {
      m = EventEmitter.defaultMaxListeners;
    }

    if (m && m > 0 && this._events[type].length > m) {
      this._events[type].warned = true;
      console.error('(node) warning: possible EventEmitter memory ' +
                    'leak detected. %d listeners added. ' +
                    'Use emitter.setMaxListeners() to increase limit.',
                    this._events[type].length);
      if (typeof console.trace === 'function') {
        // not supported in IE 10
        console.trace();
      }
    }
  }

  return this;
};

EventEmitter.prototype.on = EventEmitter.prototype.addListener;

EventEmitter.prototype.once = function(type, listener) {
  if (!isFunction(listener))
    throw TypeError('listener must be a function');

  var fired = false;

  function g() {
    this.removeListener(type, g);

    if (!fired) {
      fired = true;
      listener.apply(this, arguments);
    }
  }

  g.listener = listener;
  this.on(type, g);

  return this;
};

// emits a 'removeListener' event iff the listener was removed
EventEmitter.prototype.removeListener = function(type, listener) {
  var list, position, length, i;

  if (!isFunction(listener))
    throw TypeError('listener must be a function');

  if (!this._events || !this._events[type])
    return this;

  list = this._events[type];
  length = list.length;
  position = -1;

  if (list === listener ||
      (isFunction(list.listener) && list.listener === listener)) {
    delete this._events[type];
    if (this._events.removeListener)
      this.emit('removeListener', type, listener);

  } else if (isObject(list)) {
    for (i = length; i-- > 0;) {
      if (list[i] === listener ||
          (list[i].listener && list[i].listener === listener)) {
        position = i;
        break;
      }
    }

    if (position < 0)
      return this;

    if (list.length === 1) {
      list.length = 0;
      delete this._events[type];
    } else {
      list.splice(position, 1);
    }

    if (this._events.removeListener)
      this.emit('removeListener', type, listener);
  }

  return this;
};

EventEmitter.prototype.removeAllListeners = function(type) {
  var key, listeners;

  if (!this._events)
    return this;

  // not listening for removeListener, no need to emit
  if (!this._events.removeListener) {
    if (arguments.length === 0)
      this._events = {};
    else if (this._events[type])
      delete this._events[type];
    return this;
  }

  // emit removeListener for all listeners on all events
  if (arguments.length === 0) {
    for (key in this._events) {
      if (key === 'removeListener') continue;
      this.removeAllListeners(key);
    }
    this.removeAllListeners('removeListener');
    this._events = {};
    return this;
  }

  listeners = this._events[type];

  if (isFunction(listeners)) {
    this.removeListener(type, listeners);
  } else if (listeners) {
    // LIFO order
    while (listeners.length)
      this.removeListener(type, listeners[listeners.length - 1]);
  }
  delete this._events[type];

  return this;
};

EventEmitter.prototype.listeners = function(type) {
  var ret;
  if (!this._events || !this._events[type])
    ret = [];
  else if (isFunction(this._events[type]))
    ret = [this._events[type]];
  else
    ret = this._events[type].slice();
  return ret;
};

EventEmitter.prototype.listenerCount = function(type) {
  if (this._events) {
    var evlistener = this._events[type];

    if (isFunction(evlistener))
      return 1;
    else if (evlistener)
      return evlistener.length;
  }
  return 0;
};

EventEmitter.listenerCount = function(emitter, type) {
  return emitter.listenerCount(type);
};

function isFunction(arg) {
  return typeof arg === 'function';
}

function isNumber(arg) {
  return typeof arg === 'number';
}

function isObject(arg) {
  return typeof arg === 'object' && arg !== null;
}

function isUndefined(arg) {
  return arg === void 0;
}

},{}],217:[function(require,module,exports){
exports.read = function (buffer, offset, isLE, mLen, nBytes) {
  var e, m
  var eLen = (nBytes * 8) - mLen - 1
  var eMax = (1 << eLen) - 1
  var eBias = eMax >> 1
  var nBits = -7
  var i = isLE ? (nBytes - 1) : 0
  var d = isLE ? -1 : 1
  var s = buffer[offset + i]

  i += d

  e = s & ((1 << (-nBits)) - 1)
  s >>= (-nBits)
  nBits += eLen
  for (; nBits > 0; e = (e * 256) + buffer[offset + i], i += d, nBits -= 8) {}

  m = e & ((1 << (-nBits)) - 1)
  e >>= (-nBits)
  nBits += mLen
  for (; nBits > 0; m = (m * 256) + buffer[offset + i], i += d, nBits -= 8) {}

  if (e === 0) {
    e = 1 - eBias
  } else if (e === eMax) {
    return m ? NaN : ((s ? -1 : 1) * Infinity)
  } else {
    m = m + Math.pow(2, mLen)
    e = e - eBias
  }
  return (s ? -1 : 1) * m * Math.pow(2, e - mLen)
}

exports.write = function (buffer, value, offset, isLE, mLen, nBytes) {
  var e, m, c
  var eLen = (nBytes * 8) - mLen - 1
  var eMax = (1 << eLen) - 1
  var eBias = eMax >> 1
  var rt = (mLen === 23 ? Math.pow(2, -24) - Math.pow(2, -77) : 0)
  var i = isLE ? 0 : (nBytes - 1)
  var d = isLE ? 1 : -1
  var s = value < 0 || (value === 0 && 1 / value < 0) ? 1 : 0

  value = Math.abs(value)

  if (isNaN(value) || value === Infinity) {
    m = isNaN(value) ? 1 : 0
    e = eMax
  } else {
    e = Math.floor(Math.log(value) / Math.LN2)
    if (value * (c = Math.pow(2, -e)) < 1) {
      e--
      c *= 2
    }
    if (e + eBias >= 1) {
      value += rt / c
    } else {
      value += rt * Math.pow(2, 1 - eBias)
    }
    if (value * c >= 2) {
      e++
      c /= 2
    }

    if (e + eBias >= eMax) {
      m = 0
      e = eMax
    } else if (e + eBias >= 1) {
      m = ((value * c) - 1) * Math.pow(2, mLen)
      e = e + eBias
    } else {
      m = value * Math.pow(2, eBias - 1) * Math.pow(2, mLen)
      e = 0
    }
  }

  for (; mLen >= 8; buffer[offset + i] = m & 0xff, i += d, m /= 256, mLen -= 8) {}

  e = (e << mLen) | m
  eLen += mLen
  for (; eLen > 0; buffer[offset + i] = e & 0xff, i += d, e /= 256, eLen -= 8) {}

  buffer[offset + i - d] |= s * 128
}

},{}],218:[function(require,module,exports){
arguments[4][2][0].apply(exports,arguments)
},{"dup":2}],219:[function(require,module,exports){
var toString = {}.toString;

module.exports = Array.isArray || function (arr) {
  return toString.call(arr) == '[object Array]';
};

},{}],220:[function(require,module,exports){
// shim for using process in browser
var process = module.exports = {};

// cached from whatever global is present so that test runners that stub it
// don't break things.  But we need to wrap it in a try catch in case it is
// wrapped in strict mode code which doesn't define any globals.  It's inside a
// function because try/catches deoptimize in certain engines.

var cachedSetTimeout;
var cachedClearTimeout;

function defaultSetTimout() {
    throw new Error('setTimeout has not been defined');
}
function defaultClearTimeout () {
    throw new Error('clearTimeout has not been defined');
}
(function () {
    try {
        if (typeof setTimeout === 'function') {
            cachedSetTimeout = setTimeout;
        } else {
            cachedSetTimeout = defaultSetTimout;
        }
    } catch (e) {
        cachedSetTimeout = defaultSetTimout;
    }
    try {
        if (typeof clearTimeout === 'function') {
            cachedClearTimeout = clearTimeout;
        } else {
            cachedClearTimeout = defaultClearTimeout;
        }
    } catch (e) {
        cachedClearTimeout = defaultClearTimeout;
    }
} ())
function runTimeout(fun) {
    if (cachedSetTimeout === setTimeout) {
        //normal enviroments in sane situations
        return setTimeout(fun, 0);
    }
    // if setTimeout wasn't available but was latter defined
    if ((cachedSetTimeout === defaultSetTimout || !cachedSetTimeout) && setTimeout) {
        cachedSetTimeout = setTimeout;
        return setTimeout(fun, 0);
    }
    try {
        // when when somebody has screwed with setTimeout but no I.E. maddness
        return cachedSetTimeout(fun, 0);
    } catch(e){
        try {
            // When we are in I.E. but the script has been evaled so I.E. doesn't trust the global object when called normally
            return cachedSetTimeout.call(null, fun, 0);
        } catch(e){
            // same as above but when it's a version of I.E. that must have the global object for 'this', hopfully our context correct otherwise it will throw a global error
            return cachedSetTimeout.call(this, fun, 0);
        }
    }


}
function runClearTimeout(marker) {
    if (cachedClearTimeout === clearTimeout) {
        //normal enviroments in sane situations
        return clearTimeout(marker);
    }
    // if clearTimeout wasn't available but was latter defined
    if ((cachedClearTimeout === defaultClearTimeout || !cachedClearTimeout) && clearTimeout) {
        cachedClearTimeout = clearTimeout;
        return clearTimeout(marker);
    }
    try {
        // when when somebody has screwed with setTimeout but no I.E. maddness
        return cachedClearTimeout(marker);
    } catch (e){
        try {
            // When we are in I.E. but the script has been evaled so I.E. doesn't  trust the global object when called normally
            return cachedClearTimeout.call(null, marker);
        } catch (e){
            // same as above but when it's a version of I.E. that must have the global object for 'this', hopfully our context correct otherwise it will throw a global error.
            // Some versions of I.E. have different rules for clearTimeout vs setTimeout
            return cachedClearTimeout.call(this, marker);
        }
    }



}
var queue = [];
var draining = false;
var currentQueue;
var queueIndex = -1;

function cleanUpNextTick() {
    if (!draining || !currentQueue) {
        return;
    }
    draining = false;
    if (currentQueue.length) {
        queue = currentQueue.concat(queue);
    } else {
        queueIndex = -1;
    }
    if (queue.length) {
        drainQueue();
    }
}

function drainQueue() {
    if (draining) {
        return;
    }
    var timeout = runTimeout(cleanUpNextTick);
    draining = true;

    var len = queue.length;
    while(len) {
        currentQueue = queue;
        queue = [];
        while (++queueIndex < len) {
            if (currentQueue) {
                currentQueue[queueIndex].run();
            }
        }
        queueIndex = -1;
        len = queue.length;
    }
    currentQueue = null;
    draining = false;
    runClearTimeout(timeout);
}

process.nextTick = function (fun) {
    var args = new Array(arguments.length - 1);
    if (arguments.length > 1) {
        for (var i = 1; i < arguments.length; i++) {
            args[i - 1] = arguments[i];
        }
    }
    queue.push(new Item(fun, args));
    if (queue.length === 1 && !draining) {
        runTimeout(drainQueue);
    }
};

// v8 likes predictible objects
function Item(fun, array) {
    this.fun = fun;
    this.array = array;
}
Item.prototype.run = function () {
    this.fun.apply(null, this.array);
};
process.title = 'browser';
process.browser = true;
process.env = {};
process.argv = [];
process.version = ''; // empty string to avoid regexp issues
process.versions = {};

function noop() {}

process.on = noop;
process.addListener = noop;
process.once = noop;
process.off = noop;
process.removeListener = noop;
process.removeAllListeners = noop;
process.emit = noop;
process.prependListener = noop;
process.prependOnceListener = noop;

process.listeners = function (name) { return [] }

process.binding = function (name) {
    throw new Error('process.binding is not supported');
};

process.cwd = function () { return '/' };
process.chdir = function (dir) {
    throw new Error('process.chdir is not supported');
};
process.umask = function() { return 0; };

},{}],221:[function(require,module,exports){
(function (global){
/*! https://mths.be/punycode v1.4.1 by @mathias */
;(function(root) {

	/** Detect free variables */
	var freeExports = typeof exports == 'object' && exports &&
		!exports.nodeType && exports;
	var freeModule = typeof module == 'object' && module &&
		!module.nodeType && module;
	var freeGlobal = typeof global == 'object' && global;
	if (
		freeGlobal.global === freeGlobal ||
		freeGlobal.window === freeGlobal ||
		freeGlobal.self === freeGlobal
	) {
		root = freeGlobal;
	}

	/**
	 * The `punycode` object.
	 * @name punycode
	 * @type Object
	 */
	var punycode,

	/** Highest positive signed 32-bit float value */
	maxInt = 2147483647, // aka. 0x7FFFFFFF or 2^31-1

	/** Bootstring parameters */
	base = 36,
	tMin = 1,
	tMax = 26,
	skew = 38,
	damp = 700,
	initialBias = 72,
	initialN = 128, // 0x80
	delimiter = '-', // '\x2D'

	/** Regular expressions */
	regexPunycode = /^xn--/,
	regexNonASCII = /[^\x20-\x7E]/, // unprintable ASCII chars + non-ASCII chars
	regexSeparators = /[\x2E\u3002\uFF0E\uFF61]/g, // RFC 3490 separators

	/** Error messages */
	errors = {
		'overflow': 'Overflow: input needs wider integers to process',
		'not-basic': 'Illegal input >= 0x80 (not a basic code point)',
		'invalid-input': 'Invalid input'
	},

	/** Convenience shortcuts */
	baseMinusTMin = base - tMin,
	floor = Math.floor,
	stringFromCharCode = String.fromCharCode,

	/** Temporary variable */
	key;

	/*--------------------------------------------------------------------------*/

	/**
	 * A generic error utility function.
	 * @private
	 * @param {String} type The error type.
	 * @returns {Error} Throws a `RangeError` with the applicable error message.
	 */
	function error(type) {
		throw new RangeError(errors[type]);
	}

	/**
	 * A generic `Array#map` utility function.
	 * @private
	 * @param {Array} array The array to iterate over.
	 * @param {Function} callback The function that gets called for every array
	 * item.
	 * @returns {Array} A new array of values returned by the callback function.
	 */
	function map(array, fn) {
		var length = array.length;
		var result = [];
		while (length--) {
			result[length] = fn(array[length]);
		}
		return result;
	}

	/**
	 * A simple `Array#map`-like wrapper to work with domain name strings or email
	 * addresses.
	 * @private
	 * @param {String} domain The domain name or email address.
	 * @param {Function} callback The function that gets called for every
	 * character.
	 * @returns {Array} A new string of characters returned by the callback
	 * function.
	 */
	function mapDomain(string, fn) {
		var parts = string.split('@');
		var result = '';
		if (parts.length > 1) {
			// In email addresses, only the domain name should be punycoded. Leave
			// the local part (i.e. everything up to `@`) intact.
			result = parts[0] + '@';
			string = parts[1];
		}
		// Avoid `split(regex)` for IE8 compatibility. See #17.
		string = string.replace(regexSeparators, '\x2E');
		var labels = string.split('.');
		var encoded = map(labels, fn).join('.');
		return result + encoded;
	}

	/**
	 * Creates an array containing the numeric code points of each Unicode
	 * character in the string. While JavaScript uses UCS-2 internally,
	 * this function will convert a pair of surrogate halves (each of which
	 * UCS-2 exposes as separate characters) into a single code point,
	 * matching UTF-16.
	 * @see `punycode.ucs2.encode`
	 * @see <https://mathiasbynens.be/notes/javascript-encoding>
	 * @memberOf punycode.ucs2
	 * @name decode
	 * @param {String} string The Unicode input string (UCS-2).
	 * @returns {Array} The new array of code points.
	 */
	function ucs2decode(string) {
		var output = [],
		    counter = 0,
		    length = string.length,
		    value,
		    extra;
		while (counter < length) {
			value = string.charCodeAt(counter++);
			if (value >= 0xD800 && value <= 0xDBFF && counter < length) {
				// high surrogate, and there is a next character
				extra = string.charCodeAt(counter++);
				if ((extra & 0xFC00) == 0xDC00) { // low surrogate
					output.push(((value & 0x3FF) << 10) + (extra & 0x3FF) + 0x10000);
				} else {
					// unmatched surrogate; only append this code unit, in case the next
					// code unit is the high surrogate of a surrogate pair
					output.push(value);
					counter--;
				}
			} else {
				output.push(value);
			}
		}
		return output;
	}

	/**
	 * Creates a string based on an array of numeric code points.
	 * @see `punycode.ucs2.decode`
	 * @memberOf punycode.ucs2
	 * @name encode
	 * @param {Array} codePoints The array of numeric code points.
	 * @returns {String} The new Unicode string (UCS-2).
	 */
	function ucs2encode(array) {
		return map(array, function(value) {
			var output = '';
			if (value > 0xFFFF) {
				value -= 0x10000;
				output += stringFromCharCode(value >>> 10 & 0x3FF | 0xD800);
				value = 0xDC00 | value & 0x3FF;
			}
			output += stringFromCharCode(value);
			return output;
		}).join('');
	}

	/**
	 * Converts a basic code point into a digit/integer.
	 * @see `digitToBasic()`
	 * @private
	 * @param {Number} codePoint The basic numeric code point value.
	 * @returns {Number} The numeric value of a basic code point (for use in
	 * representing integers) in the range `0` to `base - 1`, or `base` if
	 * the code point does not represent a value.
	 */
	function basicToDigit(codePoint) {
		if (codePoint - 48 < 10) {
			return codePoint - 22;
		}
		if (codePoint - 65 < 26) {
			return codePoint - 65;
		}
		if (codePoint - 97 < 26) {
			return codePoint - 97;
		}
		return base;
	}

	/**
	 * Converts a digit/integer into a basic code point.
	 * @see `basicToDigit()`
	 * @private
	 * @param {Number} digit The numeric value of a basic code point.
	 * @returns {Number} The basic code point whose value (when used for
	 * representing integers) is `digit`, which needs to be in the range
	 * `0` to `base - 1`. If `flag` is non-zero, the uppercase form is
	 * used; else, the lowercase form is used. The behavior is undefined
	 * if `flag` is non-zero and `digit` has no uppercase form.
	 */
	function digitToBasic(digit, flag) {
		//  0..25 map to ASCII a..z or A..Z
		// 26..35 map to ASCII 0..9
		return digit + 22 + 75 * (digit < 26) - ((flag != 0) << 5);
	}

	/**
	 * Bias adaptation function as per section 3.4 of RFC 3492.
	 * https://tools.ietf.org/html/rfc3492#section-3.4
	 * @private
	 */
	function adapt(delta, numPoints, firstTime) {
		var k = 0;
		delta = firstTime ? floor(delta / damp) : delta >> 1;
		delta += floor(delta / numPoints);
		for (/* no initialization */; delta > baseMinusTMin * tMax >> 1; k += base) {
			delta = floor(delta / baseMinusTMin);
		}
		return floor(k + (baseMinusTMin + 1) * delta / (delta + skew));
	}

	/**
	 * Converts a Punycode string of ASCII-only symbols to a string of Unicode
	 * symbols.
	 * @memberOf punycode
	 * @param {String} input The Punycode string of ASCII-only symbols.
	 * @returns {String} The resulting string of Unicode symbols.
	 */
	function decode(input) {
		// Don't use UCS-2
		var output = [],
		    inputLength = input.length,
		    out,
		    i = 0,
		    n = initialN,
		    bias = initialBias,
		    basic,
		    j,
		    index,
		    oldi,
		    w,
		    k,
		    digit,
		    t,
		    /** Cached calculation results */
		    baseMinusT;

		// Handle the basic code points: let `basic` be the number of input code
		// points before the last delimiter, or `0` if there is none, then copy
		// the first basic code points to the output.

		basic = input.lastIndexOf(delimiter);
		if (basic < 0) {
			basic = 0;
		}

		for (j = 0; j < basic; ++j) {
			// if it's not a basic code point
			if (input.charCodeAt(j) >= 0x80) {
				error('not-basic');
			}
			output.push(input.charCodeAt(j));
		}

		// Main decoding loop: start just after the last delimiter if any basic code
		// points were copied; start at the beginning otherwise.

		for (index = basic > 0 ? basic + 1 : 0; index < inputLength; /* no final expression */) {

			// `index` is the index of the next character to be consumed.
			// Decode a generalized variable-length integer into `delta`,
			// which gets added to `i`. The overflow checking is easier
			// if we increase `i` as we go, then subtract off its starting
			// value at the end to obtain `delta`.
			for (oldi = i, w = 1, k = base; /* no condition */; k += base) {

				if (index >= inputLength) {
					error('invalid-input');
				}

				digit = basicToDigit(input.charCodeAt(index++));

				if (digit >= base || digit > floor((maxInt - i) / w)) {
					error('overflow');
				}

				i += digit * w;
				t = k <= bias ? tMin : (k >= bias + tMax ? tMax : k - bias);

				if (digit < t) {
					break;
				}

				baseMinusT = base - t;
				if (w > floor(maxInt / baseMinusT)) {
					error('overflow');
				}

				w *= baseMinusT;

			}

			out = output.length + 1;
			bias = adapt(i - oldi, out, oldi == 0);

			// `i` was supposed to wrap around from `out` to `0`,
			// incrementing `n` each time, so we'll fix that now:
			if (floor(i / out) > maxInt - n) {
				error('overflow');
			}

			n += floor(i / out);
			i %= out;

			// Insert `n` at position `i` of the output
			output.splice(i++, 0, n);

		}

		return ucs2encode(output);
	}

	/**
	 * Converts a string of Unicode symbols (e.g. a domain name label) to a
	 * Punycode string of ASCII-only symbols.
	 * @memberOf punycode
	 * @param {String} input The string of Unicode symbols.
	 * @returns {String} The resulting Punycode string of ASCII-only symbols.
	 */
	function encode(input) {
		var n,
		    delta,
		    handledCPCount,
		    basicLength,
		    bias,
		    j,
		    m,
		    q,
		    k,
		    t,
		    currentValue,
		    output = [],
		    /** `inputLength` will hold the number of code points in `input`. */
		    inputLength,
		    /** Cached calculation results */
		    handledCPCountPlusOne,
		    baseMinusT,
		    qMinusT;

		// Convert the input in UCS-2 to Unicode
		input = ucs2decode(input);

		// Cache the length
		inputLength = input.length;

		// Initialize the state
		n = initialN;
		delta = 0;
		bias = initialBias;

		// Handle the basic code points
		for (j = 0; j < inputLength; ++j) {
			currentValue = input[j];
			if (currentValue < 0x80) {
				output.push(stringFromCharCode(currentValue));
			}
		}

		handledCPCount = basicLength = output.length;

		// `handledCPCount` is the number of code points that have been handled;
		// `basicLength` is the number of basic code points.

		// Finish the basic string - if it is not empty - with a delimiter
		if (basicLength) {
			output.push(delimiter);
		}

		// Main encoding loop:
		while (handledCPCount < inputLength) {

			// All non-basic code points < n have been handled already. Find the next
			// larger one:
			for (m = maxInt, j = 0; j < inputLength; ++j) {
				currentValue = input[j];
				if (currentValue >= n && currentValue < m) {
					m = currentValue;
				}
			}

			// Increase `delta` enough to advance the decoder's <n,i> state to <m,0>,
			// but guard against overflow
			handledCPCountPlusOne = handledCPCount + 1;
			if (m - n > floor((maxInt - delta) / handledCPCountPlusOne)) {
				error('overflow');
			}

			delta += (m - n) * handledCPCountPlusOne;
			n = m;

			for (j = 0; j < inputLength; ++j) {
				currentValue = input[j];

				if (currentValue < n && ++delta > maxInt) {
					error('overflow');
				}

				if (currentValue == n) {
					// Represent delta as a generalized variable-length integer
					for (q = delta, k = base; /* no condition */; k += base) {
						t = k <= bias ? tMin : (k >= bias + tMax ? tMax : k - bias);
						if (q < t) {
							break;
						}
						qMinusT = q - t;
						baseMinusT = base - t;
						output.push(
							stringFromCharCode(digitToBasic(t + qMinusT % baseMinusT, 0))
						);
						q = floor(qMinusT / baseMinusT);
					}

					output.push(stringFromCharCode(digitToBasic(q, 0)));
					bias = adapt(delta, handledCPCountPlusOne, handledCPCount == basicLength);
					delta = 0;
					++handledCPCount;
				}
			}

			++delta;
			++n;

		}
		return output.join('');
	}

	/**
	 * Converts a Punycode string representing a domain name or an email address
	 * to Unicode. Only the Punycoded parts of the input will be converted, i.e.
	 * it doesn't matter if you call it on a string that has already been
	 * converted to Unicode.
	 * @memberOf punycode
	 * @param {String} input The Punycoded domain name or email address to
	 * convert to Unicode.
	 * @returns {String} The Unicode representation of the given Punycode
	 * string.
	 */
	function toUnicode(input) {
		return mapDomain(input, function(string) {
			return regexPunycode.test(string)
				? decode(string.slice(4).toLowerCase())
				: string;
		});
	}

	/**
	 * Converts a Unicode string representing a domain name or an email address to
	 * Punycode. Only the non-ASCII parts of the domain name will be converted,
	 * i.e. it doesn't matter if you call it with a domain that's already in
	 * ASCII.
	 * @memberOf punycode
	 * @param {String} input The domain name or email address to convert, as a
	 * Unicode string.
	 * @returns {String} The Punycode representation of the given domain name or
	 * email address.
	 */
	function toASCII(input) {
		return mapDomain(input, function(string) {
			return regexNonASCII.test(string)
				? 'xn--' + encode(string)
				: string;
		});
	}

	/*--------------------------------------------------------------------------*/

	/** Define the public API */
	punycode = {
		/**
		 * A string representing the current Punycode.js version number.
		 * @memberOf punycode
		 * @type String
		 */
		'version': '1.4.1',
		/**
		 * An object of methods to convert from JavaScript's internal character
		 * representation (UCS-2) to Unicode code points, and back.
		 * @see <https://mathiasbynens.be/notes/javascript-encoding>
		 * @memberOf punycode
		 * @type Object
		 */
		'ucs2': {
			'decode': ucs2decode,
			'encode': ucs2encode
		},
		'decode': decode,
		'encode': encode,
		'toASCII': toASCII,
		'toUnicode': toUnicode
	};

	/** Expose `punycode` */
	// Some AMD build optimizers, like r.js, check for specific condition patterns
	// like the following:
	if (
		typeof define == 'function' &&
		typeof define.amd == 'object' &&
		define.amd
	) {
		define('punycode', function() {
			return punycode;
		});
	} else if (freeExports && freeModule) {
		if (module.exports == freeExports) {
			// in Node.js, io.js, or RingoJS v0.8.0+
			freeModule.exports = punycode;
		} else {
			// in Narwhal or RingoJS v0.7.0-
			for (key in punycode) {
				punycode.hasOwnProperty(key) && (freeExports[key] = punycode[key]);
			}
		}
	} else {
		// in Rhino or a web browser
		root.punycode = punycode;
	}

}(this));

}).call(this,typeof global !== "undefined" ? global : typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{}],222:[function(require,module,exports){
// Copyright Joyent, Inc. and other Node contributors.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit
// persons to whom the Software is furnished to do so, subject to the
// following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
// USE OR OTHER DEALINGS IN THE SOFTWARE.

'use strict';

// If obj.hasOwnProperty has been overridden, then calling
// obj.hasOwnProperty(prop) will break.
// See: https://github.com/joyent/node/issues/1707
function hasOwnProperty(obj, prop) {
  return Object.prototype.hasOwnProperty.call(obj, prop);
}

module.exports = function(qs, sep, eq, options) {
  sep = sep || '&';
  eq = eq || '=';
  var obj = {};

  if (typeof qs !== 'string' || qs.length === 0) {
    return obj;
  }

  var regexp = /\+/g;
  qs = qs.split(sep);

  var maxKeys = 1000;
  if (options && typeof options.maxKeys === 'number') {
    maxKeys = options.maxKeys;
  }

  var len = qs.length;
  // maxKeys <= 0 means that we should not limit keys count
  if (maxKeys > 0 && len > maxKeys) {
    len = maxKeys;
  }

  for (var i = 0; i < len; ++i) {
    var x = qs[i].replace(regexp, '%20'),
        idx = x.indexOf(eq),
        kstr, vstr, k, v;

    if (idx >= 0) {
      kstr = x.substr(0, idx);
      vstr = x.substr(idx + 1);
    } else {
      kstr = x;
      vstr = '';
    }

    k = decodeURIComponent(kstr);
    v = decodeURIComponent(vstr);

    if (!hasOwnProperty(obj, k)) {
      obj[k] = v;
    } else if (isArray(obj[k])) {
      obj[k].push(v);
    } else {
      obj[k] = [obj[k], v];
    }
  }

  return obj;
};

var isArray = Array.isArray || function (xs) {
  return Object.prototype.toString.call(xs) === '[object Array]';
};

},{}],223:[function(require,module,exports){
// Copyright Joyent, Inc. and other Node contributors.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit
// persons to whom the Software is furnished to do so, subject to the
// following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
// USE OR OTHER DEALINGS IN THE SOFTWARE.

'use strict';

var stringifyPrimitive = function(v) {
  switch (typeof v) {
    case 'string':
      return v;

    case 'boolean':
      return v ? 'true' : 'false';

    case 'number':
      return isFinite(v) ? v : '';

    default:
      return '';
  }
};

module.exports = function(obj, sep, eq, name) {
  sep = sep || '&';
  eq = eq || '=';
  if (obj === null) {
    obj = undefined;
  }

  if (typeof obj === 'object') {
    return map(objectKeys(obj), function(k) {
      var ks = encodeURIComponent(stringifyPrimitive(k)) + eq;
      if (isArray(obj[k])) {
        return map(obj[k], function(v) {
          return ks + encodeURIComponent(stringifyPrimitive(v));
        }).join(sep);
      } else {
        return ks + encodeURIComponent(stringifyPrimitive(obj[k]));
      }
    }).join(sep);

  }

  if (!name) return '';
  return encodeURIComponent(stringifyPrimitive(name)) + eq +
         encodeURIComponent(stringifyPrimitive(obj));
};

var isArray = Array.isArray || function (xs) {
  return Object.prototype.toString.call(xs) === '[object Array]';
};

function map (xs, f) {
  if (xs.map) return xs.map(f);
  var res = [];
  for (var i = 0; i < xs.length; i++) {
    res.push(f(xs[i], i));
  }
  return res;
}

var objectKeys = Object.keys || function (obj) {
  var res = [];
  for (var key in obj) {
    if (Object.prototype.hasOwnProperty.call(obj, key)) res.push(key);
  }
  return res;
};

},{}],224:[function(require,module,exports){
'use strict';

exports.decode = exports.parse = require('./decode');
exports.encode = exports.stringify = require('./encode');

},{"./decode":222,"./encode":223}],225:[function(require,module,exports){
// Copyright Joyent, Inc. and other Node contributors.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit
// persons to whom the Software is furnished to do so, subject to the
// following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
// USE OR OTHER DEALINGS IN THE SOFTWARE.

'use strict';

var punycode = require('punycode');
var util = require('./util');

exports.parse = urlParse;
exports.resolve = urlResolve;
exports.resolveObject = urlResolveObject;
exports.format = urlFormat;

exports.Url = Url;

function Url() {
  this.protocol = null;
  this.slashes = null;
  this.auth = null;
  this.host = null;
  this.port = null;
  this.hostname = null;
  this.hash = null;
  this.search = null;
  this.query = null;
  this.pathname = null;
  this.path = null;
  this.href = null;
}

// Reference: RFC 3986, RFC 1808, RFC 2396

// define these here so at least they only have to be
// compiled once on the first module load.
var protocolPattern = /^([a-z0-9.+-]+:)/i,
    portPattern = /:[0-9]*$/,

    // Special case for a simple path URL
    simplePathPattern = /^(\/\/?(?!\/)[^\?\s]*)(\?[^\s]*)?$/,

    // RFC 2396: characters reserved for delimiting URLs.
    // We actually just auto-escape these.
    delims = ['<', '>', '"', '`', ' ', '\r', '\n', '\t'],

    // RFC 2396: characters not allowed for various reasons.
    unwise = ['{', '}', '|', '\\', '^', '`'].concat(delims),

    // Allowed by RFCs, but cause of XSS attacks.  Always escape these.
    autoEscape = ['\''].concat(unwise),
    // Characters that are never ever allowed in a hostname.
    // Note that any invalid chars are also handled, but these
    // are the ones that are *expected* to be seen, so we fast-path
    // them.
    nonHostChars = ['%', '/', '?', ';', '#'].concat(autoEscape),
    hostEndingChars = ['/', '?', '#'],
    hostnameMaxLen = 255,
    hostnamePartPattern = /^[+a-z0-9A-Z_-]{0,63}$/,
    hostnamePartStart = /^([+a-z0-9A-Z_-]{0,63})(.*)$/,
    // protocols that can allow "unsafe" and "unwise" chars.
    unsafeProtocol = {
      'javascript': true,
      'javascript:': true
    },
    // protocols that never have a hostname.
    hostlessProtocol = {
      'javascript': true,
      'javascript:': true
    },
    // protocols that always contain a // bit.
    slashedProtocol = {
      'http': true,
      'https': true,
      'ftp': true,
      'gopher': true,
      'file': true,
      'http:': true,
      'https:': true,
      'ftp:': true,
      'gopher:': true,
      'file:': true
    },
    querystring = require('querystring');

function urlParse(url, parseQueryString, slashesDenoteHost) {
  if (url && util.isObject(url) && url instanceof Url) return url;

  var u = new Url;
  u.parse(url, parseQueryString, slashesDenoteHost);
  return u;
}

Url.prototype.parse = function(url, parseQueryString, slashesDenoteHost) {
  if (!util.isString(url)) {
    throw new TypeError("Parameter 'url' must be a string, not " + typeof url);
  }

  // Copy chrome, IE, opera backslash-handling behavior.
  // Back slashes before the query string get converted to forward slashes
  // See: https://code.google.com/p/chromium/issues/detail?id=25916
  var queryIndex = url.indexOf('?'),
      splitter =
          (queryIndex !== -1 && queryIndex < url.indexOf('#')) ? '?' : '#',
      uSplit = url.split(splitter),
      slashRegex = /\\/g;
  uSplit[0] = uSplit[0].replace(slashRegex, '/');
  url = uSplit.join(splitter);

  var rest = url;

  // trim before proceeding.
  // This is to support parse stuff like "  http://foo.com  \n"
  rest = rest.trim();

  if (!slashesDenoteHost && url.split('#').length === 1) {
    // Try fast path regexp
    var simplePath = simplePathPattern.exec(rest);
    if (simplePath) {
      this.path = rest;
      this.href = rest;
      this.pathname = simplePath[1];
      if (simplePath[2]) {
        this.search = simplePath[2];
        if (parseQueryString) {
          this.query = querystring.parse(this.search.substr(1));
        } else {
          this.query = this.search.substr(1);
        }
      } else if (parseQueryString) {
        this.search = '';
        this.query = {};
      }
      return this;
    }
  }

  var proto = protocolPattern.exec(rest);
  if (proto) {
    proto = proto[0];
    var lowerProto = proto.toLowerCase();
    this.protocol = lowerProto;
    rest = rest.substr(proto.length);
  }

  // figure out if it's got a host
  // user@server is *always* interpreted as a hostname, and url
  // resolution will treat //foo/bar as host=foo,path=bar because that's
  // how the browser resolves relative URLs.
  if (slashesDenoteHost || proto || rest.match(/^\/\/[^@\/]+@[^@\/]+/)) {
    var slashes = rest.substr(0, 2) === '//';
    if (slashes && !(proto && hostlessProtocol[proto])) {
      rest = rest.substr(2);
      this.slashes = true;
    }
  }

  if (!hostlessProtocol[proto] &&
      (slashes || (proto && !slashedProtocol[proto]))) {

    // there's a hostname.
    // the first instance of /, ?, ;, or # ends the host.
    //
    // If there is an @ in the hostname, then non-host chars *are* allowed
    // to the left of the last @ sign, unless some host-ending character
    // comes *before* the @-sign.
    // URLs are obnoxious.
    //
    // ex:
    // http://a@b@c/ => user:a@b host:c
    // http://a@b?@c => user:a host:c path:/?@c

    // v0.12 TODO(isaacs): This is not quite how Chrome does things.
    // Review our test case against browsers more comprehensively.

    // find the first instance of any hostEndingChars
    var hostEnd = -1;
    for (var i = 0; i < hostEndingChars.length; i++) {
      var hec = rest.indexOf(hostEndingChars[i]);
      if (hec !== -1 && (hostEnd === -1 || hec < hostEnd))
        hostEnd = hec;
    }

    // at this point, either we have an explicit point where the
    // auth portion cannot go past, or the last @ char is the decider.
    var auth, atSign;
    if (hostEnd === -1) {
      // atSign can be anywhere.
      atSign = rest.lastIndexOf('@');
    } else {
      // atSign must be in auth portion.
      // http://a@b/c@d => host:b auth:a path:/c@d
      atSign = rest.lastIndexOf('@', hostEnd);
    }

    // Now we have a portion which is definitely the auth.
    // Pull that off.
    if (atSign !== -1) {
      auth = rest.slice(0, atSign);
      rest = rest.slice(atSign + 1);
      this.auth = decodeURIComponent(auth);
    }

    // the host is the remaining to the left of the first non-host char
    hostEnd = -1;
    for (var i = 0; i < nonHostChars.length; i++) {
      var hec = rest.indexOf(nonHostChars[i]);
      if (hec !== -1 && (hostEnd === -1 || hec < hostEnd))
        hostEnd = hec;
    }
    // if we still have not hit it, then the entire thing is a host.
    if (hostEnd === -1)
      hostEnd = rest.length;

    this.host = rest.slice(0, hostEnd);
    rest = rest.slice(hostEnd);

    // pull out port.
    this.parseHost();

    // we've indicated that there is a hostname,
    // so even if it's empty, it has to be present.
    this.hostname = this.hostname || '';

    // if hostname begins with [ and ends with ]
    // assume that it's an IPv6 address.
    var ipv6Hostname = this.hostname[0] === '[' &&
        this.hostname[this.hostname.length - 1] === ']';

    // validate a little.
    if (!ipv6Hostname) {
      var hostparts = this.hostname.split(/\./);
      for (var i = 0, l = hostparts.length; i < l; i++) {
        var part = hostparts[i];
        if (!part) continue;
        if (!part.match(hostnamePartPattern)) {
          var newpart = '';
          for (var j = 0, k = part.length; j < k; j++) {
            if (part.charCodeAt(j) > 127) {
              // we replace non-ASCII char with a temporary placeholder
              // we need this to make sure size of hostname is not
              // broken by replacing non-ASCII by nothing
              newpart += 'x';
            } else {
              newpart += part[j];
            }
          }
          // we test again with ASCII char only
          if (!newpart.match(hostnamePartPattern)) {
            var validParts = hostparts.slice(0, i);
            var notHost = hostparts.slice(i + 1);
            var bit = part.match(hostnamePartStart);
            if (bit) {
              validParts.push(bit[1]);
              notHost.unshift(bit[2]);
            }
            if (notHost.length) {
              rest = '/' + notHost.join('.') + rest;
            }
            this.hostname = validParts.join('.');
            break;
          }
        }
      }
    }

    if (this.hostname.length > hostnameMaxLen) {
      this.hostname = '';
    } else {
      // hostnames are always lower case.
      this.hostname = this.hostname.toLowerCase();
    }

    if (!ipv6Hostname) {
      // IDNA Support: Returns a punycoded representation of "domain".
      // It only converts parts of the domain name that
      // have non-ASCII characters, i.e. it doesn't matter if
      // you call it with a domain that already is ASCII-only.
      this.hostname = punycode.toASCII(this.hostname);
    }

    var p = this.port ? ':' + this.port : '';
    var h = this.hostname || '';
    this.host = h + p;
    this.href += this.host;

    // strip [ and ] from the hostname
    // the host field still retains them, though
    if (ipv6Hostname) {
      this.hostname = this.hostname.substr(1, this.hostname.length - 2);
      if (rest[0] !== '/') {
        rest = '/' + rest;
      }
    }
  }

  // now rest is set to the post-host stuff.
  // chop off any delim chars.
  if (!unsafeProtocol[lowerProto]) {

    // First, make 100% sure that any "autoEscape" chars get
    // escaped, even if encodeURIComponent doesn't think they
    // need to be.
    for (var i = 0, l = autoEscape.length; i < l; i++) {
      var ae = autoEscape[i];
      if (rest.indexOf(ae) === -1)
        continue;
      var esc = encodeURIComponent(ae);
      if (esc === ae) {
        esc = escape(ae);
      }
      rest = rest.split(ae).join(esc);
    }
  }


  // chop off from the tail first.
  var hash = rest.indexOf('#');
  if (hash !== -1) {
    // got a fragment string.
    this.hash = rest.substr(hash);
    rest = rest.slice(0, hash);
  }
  var qm = rest.indexOf('?');
  if (qm !== -1) {
    this.search = rest.substr(qm);
    this.query = rest.substr(qm + 1);
    if (parseQueryString) {
      this.query = querystring.parse(this.query);
    }
    rest = rest.slice(0, qm);
  } else if (parseQueryString) {
    // no query string, but parseQueryString still requested
    this.search = '';
    this.query = {};
  }
  if (rest) this.pathname = rest;
  if (slashedProtocol[lowerProto] &&
      this.hostname && !this.pathname) {
    this.pathname = '/';
  }

  //to support http.request
  if (this.pathname || this.search) {
    var p = this.pathname || '';
    var s = this.search || '';
    this.path = p + s;
  }

  // finally, reconstruct the href based on what has been validated.
  this.href = this.format();
  return this;
};

// format a parsed object into a url string
function urlFormat(obj) {
  // ensure it's an object, and not a string url.
  // If it's an obj, this is a no-op.
  // this way, you can call url_format() on strings
  // to clean up potentially wonky urls.
  if (util.isString(obj)) obj = urlParse(obj);
  if (!(obj instanceof Url)) return Url.prototype.format.call(obj);
  return obj.format();
}

Url.prototype.format = function() {
  var auth = this.auth || '';
  if (auth) {
    auth = encodeURIComponent(auth);
    auth = auth.replace(/%3A/i, ':');
    auth += '@';
  }

  var protocol = this.protocol || '',
      pathname = this.pathname || '',
      hash = this.hash || '',
      host = false,
      query = '';

  if (this.host) {
    host = auth + this.host;
  } else if (this.hostname) {
    host = auth + (this.hostname.indexOf(':') === -1 ?
        this.hostname :
        '[' + this.hostname + ']');
    if (this.port) {
      host += ':' + this.port;
    }
  }

  if (this.query &&
      util.isObject(this.query) &&
      Object.keys(this.query).length) {
    query = querystring.stringify(this.query);
  }

  var search = this.search || (query && ('?' + query)) || '';

  if (protocol && protocol.substr(-1) !== ':') protocol += ':';

  // only the slashedProtocols get the //.  Not mailto:, xmpp:, etc.
  // unless they had them to begin with.
  if (this.slashes ||
      (!protocol || slashedProtocol[protocol]) && host !== false) {
    host = '//' + (host || '');
    if (pathname && pathname.charAt(0) !== '/') pathname = '/' + pathname;
  } else if (!host) {
    host = '';
  }

  if (hash && hash.charAt(0) !== '#') hash = '#' + hash;
  if (search && search.charAt(0) !== '?') search = '?' + search;

  pathname = pathname.replace(/[?#]/g, function(match) {
    return encodeURIComponent(match);
  });
  search = search.replace('#', '%23');

  return protocol + host + pathname + search + hash;
};

function urlResolve(source, relative) {
  return urlParse(source, false, true).resolve(relative);
}

Url.prototype.resolve = function(relative) {
  return this.resolveObject(urlParse(relative, false, true)).format();
};

function urlResolveObject(source, relative) {
  if (!source) return relative;
  return urlParse(source, false, true).resolveObject(relative);
}

Url.prototype.resolveObject = function(relative) {
  if (util.isString(relative)) {
    var rel = new Url();
    rel.parse(relative, false, true);
    relative = rel;
  }

  var result = new Url();
  var tkeys = Object.keys(this);
  for (var tk = 0; tk < tkeys.length; tk++) {
    var tkey = tkeys[tk];
    result[tkey] = this[tkey];
  }

  // hash is always overridden, no matter what.
  // even href="" will remove it.
  result.hash = relative.hash;

  // if the relative url is empty, then there's nothing left to do here.
  if (relative.href === '') {
    result.href = result.format();
    return result;
  }

  // hrefs like //foo/bar always cut to the protocol.
  if (relative.slashes && !relative.protocol) {
    // take everything except the protocol from relative
    var rkeys = Object.keys(relative);
    for (var rk = 0; rk < rkeys.length; rk++) {
      var rkey = rkeys[rk];
      if (rkey !== 'protocol')
        result[rkey] = relative[rkey];
    }

    //urlParse appends trailing / to urls like http://www.example.com
    if (slashedProtocol[result.protocol] &&
        result.hostname && !result.pathname) {
      result.path = result.pathname = '/';
    }

    result.href = result.format();
    return result;
  }

  if (relative.protocol && relative.protocol !== result.protocol) {
    // if it's a known url protocol, then changing
    // the protocol does weird things
    // first, if it's not file:, then we MUST have a host,
    // and if there was a path
    // to begin with, then we MUST have a path.
    // if it is file:, then the host is dropped,
    // because that's known to be hostless.
    // anything else is assumed to be absolute.
    if (!slashedProtocol[relative.protocol]) {
      var keys = Object.keys(relative);
      for (var v = 0; v < keys.length; v++) {
        var k = keys[v];
        result[k] = relative[k];
      }
      result.href = result.format();
      return result;
    }

    result.protocol = relative.protocol;
    if (!relative.host && !hostlessProtocol[relative.protocol]) {
      var relPath = (relative.pathname || '').split('/');
      while (relPath.length && !(relative.host = relPath.shift()));
      if (!relative.host) relative.host = '';
      if (!relative.hostname) relative.hostname = '';
      if (relPath[0] !== '') relPath.unshift('');
      if (relPath.length < 2) relPath.unshift('');
      result.pathname = relPath.join('/');
    } else {
      result.pathname = relative.pathname;
    }
    result.search = relative.search;
    result.query = relative.query;
    result.host = relative.host || '';
    result.auth = relative.auth;
    result.hostname = relative.hostname || relative.host;
    result.port = relative.port;
    // to support http.request
    if (result.pathname || result.search) {
      var p = result.pathname || '';
      var s = result.search || '';
      result.path = p + s;
    }
    result.slashes = result.slashes || relative.slashes;
    result.href = result.format();
    return result;
  }

  var isSourceAbs = (result.pathname && result.pathname.charAt(0) === '/'),
      isRelAbs = (
          relative.host ||
          relative.pathname && relative.pathname.charAt(0) === '/'
      ),
      mustEndAbs = (isRelAbs || isSourceAbs ||
                    (result.host && relative.pathname)),
      removeAllDots = mustEndAbs,
      srcPath = result.pathname && result.pathname.split('/') || [],
      relPath = relative.pathname && relative.pathname.split('/') || [],
      psychotic = result.protocol && !slashedProtocol[result.protocol];

  // if the url is a non-slashed url, then relative
  // links like ../.. should be able
  // to crawl up to the hostname, as well.  This is strange.
  // result.protocol has already been set by now.
  // Later on, put the first path part into the host field.
  if (psychotic) {
    result.hostname = '';
    result.port = null;
    if (result.host) {
      if (srcPath[0] === '') srcPath[0] = result.host;
      else srcPath.unshift(result.host);
    }
    result.host = '';
    if (relative.protocol) {
      relative.hostname = null;
      relative.port = null;
      if (relative.host) {
        if (relPath[0] === '') relPath[0] = relative.host;
        else relPath.unshift(relative.host);
      }
      relative.host = null;
    }
    mustEndAbs = mustEndAbs && (relPath[0] === '' || srcPath[0] === '');
  }

  if (isRelAbs) {
    // it's absolute.
    result.host = (relative.host || relative.host === '') ?
                  relative.host : result.host;
    result.hostname = (relative.hostname || relative.hostname === '') ?
                      relative.hostname : result.hostname;
    result.search = relative.search;
    result.query = relative.query;
    srcPath = relPath;
    // fall through to the dot-handling below.
  } else if (relPath.length) {
    // it's relative
    // throw away the existing file, and take the new path instead.
    if (!srcPath) srcPath = [];
    srcPath.pop();
    srcPath = srcPath.concat(relPath);
    result.search = relative.search;
    result.query = relative.query;
  } else if (!util.isNullOrUndefined(relative.search)) {
    // just pull out the search.
    // like href='?foo'.
    // Put this after the other two cases because it simplifies the booleans
    if (psychotic) {
      result.hostname = result.host = srcPath.shift();
      //occationaly the auth can get stuck only in host
      //this especially happens in cases like
      //url.resolveObject('mailto:local1@domain1', 'local2@domain2')
      var authInHost = result.host && result.host.indexOf('@') > 0 ?
                       result.host.split('@') : false;
      if (authInHost) {
        result.auth = authInHost.shift();
        result.host = result.hostname = authInHost.shift();
      }
    }
    result.search = relative.search;
    result.query = relative.query;
    //to support http.request
    if (!util.isNull(result.pathname) || !util.isNull(result.search)) {
      result.path = (result.pathname ? result.pathname : '') +
                    (result.search ? result.search : '');
    }
    result.href = result.format();
    return result;
  }

  if (!srcPath.length) {
    // no path at all.  easy.
    // we've already handled the other stuff above.
    result.pathname = null;
    //to support http.request
    if (result.search) {
      result.path = '/' + result.search;
    } else {
      result.path = null;
    }
    result.href = result.format();
    return result;
  }

  // if a url ENDs in . or .., then it must get a trailing slash.
  // however, if it ends in anything else non-slashy,
  // then it must NOT get a trailing slash.
  var last = srcPath.slice(-1)[0];
  var hasTrailingSlash = (
      (result.host || relative.host || srcPath.length > 1) &&
      (last === '.' || last === '..') || last === '');

  // strip single dots, resolve double dots to parent dir
  // if the path tries to go above the root, `up` ends up > 0
  var up = 0;
  for (var i = srcPath.length; i >= 0; i--) {
    last = srcPath[i];
    if (last === '.') {
      srcPath.splice(i, 1);
    } else if (last === '..') {
      srcPath.splice(i, 1);
      up++;
    } else if (up) {
      srcPath.splice(i, 1);
      up--;
    }
  }

  // if the path is allowed to go above the root, restore leading ..s
  if (!mustEndAbs && !removeAllDots) {
    for (; up--; up) {
      srcPath.unshift('..');
    }
  }

  if (mustEndAbs && srcPath[0] !== '' &&
      (!srcPath[0] || srcPath[0].charAt(0) !== '/')) {
    srcPath.unshift('');
  }

  if (hasTrailingSlash && (srcPath.join('/').substr(-1) !== '/')) {
    srcPath.push('');
  }

  var isAbsolute = srcPath[0] === '' ||
      (srcPath[0] && srcPath[0].charAt(0) === '/');

  // put the host back
  if (psychotic) {
    result.hostname = result.host = isAbsolute ? '' :
                                    srcPath.length ? srcPath.shift() : '';
    //occationaly the auth can get stuck only in host
    //this especially happens in cases like
    //url.resolveObject('mailto:local1@domain1', 'local2@domain2')
    var authInHost = result.host && result.host.indexOf('@') > 0 ?
                     result.host.split('@') : false;
    if (authInHost) {
      result.auth = authInHost.shift();
      result.host = result.hostname = authInHost.shift();
    }
  }

  mustEndAbs = mustEndAbs || (result.host && srcPath.length);

  if (mustEndAbs && !isAbsolute) {
    srcPath.unshift('');
  }

  if (!srcPath.length) {
    result.pathname = null;
    result.path = null;
  } else {
    result.pathname = srcPath.join('/');
  }

  //to support request.http
  if (!util.isNull(result.pathname) || !util.isNull(result.search)) {
    result.path = (result.pathname ? result.pathname : '') +
                  (result.search ? result.search : '');
  }
  result.auth = relative.auth || result.auth;
  result.slashes = result.slashes || relative.slashes;
  result.href = result.format();
  return result;
};

Url.prototype.parseHost = function() {
  var host = this.host;
  var port = portPattern.exec(host);
  if (port) {
    port = port[0];
    if (port !== ':') {
      this.port = port.substr(1);
    }
    host = host.substr(0, host.length - port.length);
  }
  if (host) this.hostname = host;
};

},{"./util":226,"punycode":221,"querystring":224}],226:[function(require,module,exports){
'use strict';

module.exports = {
  isString: function(arg) {
    return typeof(arg) === 'string';
  },
  isObject: function(arg) {
    return typeof(arg) === 'object' && arg !== null;
  },
  isNull: function(arg) {
    return arg === null;
  },
  isNullOrUndefined: function(arg) {
    return arg == null;
  }
};

},{}],227:[function(require,module,exports){
module.exports = function isBuffer(arg) {
  return arg && typeof arg === 'object'
    && typeof arg.copy === 'function'
    && typeof arg.fill === 'function'
    && typeof arg.readUInt8 === 'function';
}
},{}],228:[function(require,module,exports){
(function (process,global){
// Copyright Joyent, Inc. and other Node contributors.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit
// persons to whom the Software is furnished to do so, subject to the
// following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
// USE OR OTHER DEALINGS IN THE SOFTWARE.

var formatRegExp = /%[sdj%]/g;
exports.format = function(f) {
  if (!isString(f)) {
    var objects = [];
    for (var i = 0; i < arguments.length; i++) {
      objects.push(inspect(arguments[i]));
    }
    return objects.join(' ');
  }

  var i = 1;
  var args = arguments;
  var len = args.length;
  var str = String(f).replace(formatRegExp, function(x) {
    if (x === '%%') return '%';
    if (i >= len) return x;
    switch (x) {
      case '%s': return String(args[i++]);
      case '%d': return Number(args[i++]);
      case '%j':
        try {
          return JSON.stringify(args[i++]);
        } catch (_) {
          return '[Circular]';
        }
      default:
        return x;
    }
  });
  for (var x = args[i]; i < len; x = args[++i]) {
    if (isNull(x) || !isObject(x)) {
      str += ' ' + x;
    } else {
      str += ' ' + inspect(x);
    }
  }
  return str;
};


// Mark that a method should not be used.
// Returns a modified function which warns once by default.
// If --no-deprecation is set, then it is a no-op.
exports.deprecate = function(fn, msg) {
  // Allow for deprecating things in the process of starting up.
  if (isUndefined(global.process)) {
    return function() {
      return exports.deprecate(fn, msg).apply(this, arguments);
    };
  }

  if (process.noDeprecation === true) {
    return fn;
  }

  var warned = false;
  function deprecated() {
    if (!warned) {
      if (process.throwDeprecation) {
        throw new Error(msg);
      } else if (process.traceDeprecation) {
        console.trace(msg);
      } else {
        console.error(msg);
      }
      warned = true;
    }
    return fn.apply(this, arguments);
  }

  return deprecated;
};


var debugs = {};
var debugEnviron;
exports.debuglog = function(set) {
  if (isUndefined(debugEnviron))
    debugEnviron = process.env.NODE_DEBUG || '';
  set = set.toUpperCase();
  if (!debugs[set]) {
    if (new RegExp('\\b' + set + '\\b', 'i').test(debugEnviron)) {
      var pid = process.pid;
      debugs[set] = function() {
        var msg = exports.format.apply(exports, arguments);
        console.error('%s %d: %s', set, pid, msg);
      };
    } else {
      debugs[set] = function() {};
    }
  }
  return debugs[set];
};


/**
 * Echos the value of a value. Trys to print the value out
 * in the best way possible given the different types.
 *
 * @param {Object} obj The object to print out.
 * @param {Object} opts Optional options object that alters the output.
 */
/* legacy: obj, showHidden, depth, colors*/
function inspect(obj, opts) {
  // default options
  var ctx = {
    seen: [],
    stylize: stylizeNoColor
  };
  // legacy...
  if (arguments.length >= 3) ctx.depth = arguments[2];
  if (arguments.length >= 4) ctx.colors = arguments[3];
  if (isBoolean(opts)) {
    // legacy...
    ctx.showHidden = opts;
  } else if (opts) {
    // got an "options" object
    exports._extend(ctx, opts);
  }
  // set default options
  if (isUndefined(ctx.showHidden)) ctx.showHidden = false;
  if (isUndefined(ctx.depth)) ctx.depth = 2;
  if (isUndefined(ctx.colors)) ctx.colors = false;
  if (isUndefined(ctx.customInspect)) ctx.customInspect = true;
  if (ctx.colors) ctx.stylize = stylizeWithColor;
  return formatValue(ctx, obj, ctx.depth);
}
exports.inspect = inspect;


// http://en.wikipedia.org/wiki/ANSI_escape_code#graphics
inspect.colors = {
  'bold' : [1, 22],
  'italic' : [3, 23],
  'underline' : [4, 24],
  'inverse' : [7, 27],
  'white' : [37, 39],
  'grey' : [90, 39],
  'black' : [30, 39],
  'blue' : [34, 39],
  'cyan' : [36, 39],
  'green' : [32, 39],
  'magenta' : [35, 39],
  'red' : [31, 39],
  'yellow' : [33, 39]
};

// Don't use 'blue' not visible on cmd.exe
inspect.styles = {
  'special': 'cyan',
  'number': 'yellow',
  'boolean': 'yellow',
  'undefined': 'grey',
  'null': 'bold',
  'string': 'green',
  'date': 'magenta',
  // "name": intentionally not styling
  'regexp': 'red'
};


function stylizeWithColor(str, styleType) {
  var style = inspect.styles[styleType];

  if (style) {
    return '\u001b[' + inspect.colors[style][0] + 'm' + str +
           '\u001b[' + inspect.colors[style][1] + 'm';
  } else {
    return str;
  }
}


function stylizeNoColor(str, styleType) {
  return str;
}


function arrayToHash(array) {
  var hash = {};

  array.forEach(function(val, idx) {
    hash[val] = true;
  });

  return hash;
}


function formatValue(ctx, value, recurseTimes) {
  // Provide a hook for user-specified inspect functions.
  // Check that value is an object with an inspect function on it
  if (ctx.customInspect &&
      value &&
      isFunction(value.inspect) &&
      // Filter out the util module, it's inspect function is special
      value.inspect !== exports.inspect &&
      // Also filter out any prototype objects using the circular check.
      !(value.constructor && value.constructor.prototype === value)) {
    var ret = value.inspect(recurseTimes, ctx);
    if (!isString(ret)) {
      ret = formatValue(ctx, ret, recurseTimes);
    }
    return ret;
  }

  // Primitive types cannot have properties
  var primitive = formatPrimitive(ctx, value);
  if (primitive) {
    return primitive;
  }

  // Look up the keys of the object.
  var keys = Object.keys(value);
  var visibleKeys = arrayToHash(keys);

  if (ctx.showHidden) {
    keys = Object.getOwnPropertyNames(value);
  }

  // IE doesn't make error fields non-enumerable
  // http://msdn.microsoft.com/en-us/library/ie/dww52sbt(v=vs.94).aspx
  if (isError(value)
      && (keys.indexOf('message') >= 0 || keys.indexOf('description') >= 0)) {
    return formatError(value);
  }

  // Some type of object without properties can be shortcutted.
  if (keys.length === 0) {
    if (isFunction(value)) {
      var name = value.name ? ': ' + value.name : '';
      return ctx.stylize('[Function' + name + ']', 'special');
    }
    if (isRegExp(value)) {
      return ctx.stylize(RegExp.prototype.toString.call(value), 'regexp');
    }
    if (isDate(value)) {
      return ctx.stylize(Date.prototype.toString.call(value), 'date');
    }
    if (isError(value)) {
      return formatError(value);
    }
  }

  var base = '', array = false, braces = ['{', '}'];

  // Make Array say that they are Array
  if (isArray(value)) {
    array = true;
    braces = ['[', ']'];
  }

  // Make functions say that they are functions
  if (isFunction(value)) {
    var n = value.name ? ': ' + value.name : '';
    base = ' [Function' + n + ']';
  }

  // Make RegExps say that they are RegExps
  if (isRegExp(value)) {
    base = ' ' + RegExp.prototype.toString.call(value);
  }

  // Make dates with properties first say the date
  if (isDate(value)) {
    base = ' ' + Date.prototype.toUTCString.call(value);
  }

  // Make error with message first say the error
  if (isError(value)) {
    base = ' ' + formatError(value);
  }

  if (keys.length === 0 && (!array || value.length == 0)) {
    return braces[0] + base + braces[1];
  }

  if (recurseTimes < 0) {
    if (isRegExp(value)) {
      return ctx.stylize(RegExp.prototype.toString.call(value), 'regexp');
    } else {
      return ctx.stylize('[Object]', 'special');
    }
  }

  ctx.seen.push(value);

  var output;
  if (array) {
    output = formatArray(ctx, value, recurseTimes, visibleKeys, keys);
  } else {
    output = keys.map(function(key) {
      return formatProperty(ctx, value, recurseTimes, visibleKeys, key, array);
    });
  }

  ctx.seen.pop();

  return reduceToSingleString(output, base, braces);
}


function formatPrimitive(ctx, value) {
  if (isUndefined(value))
    return ctx.stylize('undefined', 'undefined');
  if (isString(value)) {
    var simple = '\'' + JSON.stringify(value).replace(/^"|"$/g, '')
                                             .replace(/'/g, "\\'")
                                             .replace(/\\"/g, '"') + '\'';
    return ctx.stylize(simple, 'string');
  }
  if (isNumber(value))
    return ctx.stylize('' + value, 'number');
  if (isBoolean(value))
    return ctx.stylize('' + value, 'boolean');
  // For some reason typeof null is "object", so special case here.
  if (isNull(value))
    return ctx.stylize('null', 'null');
}


function formatError(value) {
  return '[' + Error.prototype.toString.call(value) + ']';
}


function formatArray(ctx, value, recurseTimes, visibleKeys, keys) {
  var output = [];
  for (var i = 0, l = value.length; i < l; ++i) {
    if (hasOwnProperty(value, String(i))) {
      output.push(formatProperty(ctx, value, recurseTimes, visibleKeys,
          String(i), true));
    } else {
      output.push('');
    }
  }
  keys.forEach(function(key) {
    if (!key.match(/^\d+$/)) {
      output.push(formatProperty(ctx, value, recurseTimes, visibleKeys,
          key, true));
    }
  });
  return output;
}


function formatProperty(ctx, value, recurseTimes, visibleKeys, key, array) {
  var name, str, desc;
  desc = Object.getOwnPropertyDescriptor(value, key) || { value: value[key] };
  if (desc.get) {
    if (desc.set) {
      str = ctx.stylize('[Getter/Setter]', 'special');
    } else {
      str = ctx.stylize('[Getter]', 'special');
    }
  } else {
    if (desc.set) {
      str = ctx.stylize('[Setter]', 'special');
    }
  }
  if (!hasOwnProperty(visibleKeys, key)) {
    name = '[' + key + ']';
  }
  if (!str) {
    if (ctx.seen.indexOf(desc.value) < 0) {
      if (isNull(recurseTimes)) {
        str = formatValue(ctx, desc.value, null);
      } else {
        str = formatValue(ctx, desc.value, recurseTimes - 1);
      }
      if (str.indexOf('\n') > -1) {
        if (array) {
          str = str.split('\n').map(function(line) {
            return '  ' + line;
          }).join('\n').substr(2);
        } else {
          str = '\n' + str.split('\n').map(function(line) {
            return '   ' + line;
          }).join('\n');
        }
      }
    } else {
      str = ctx.stylize('[Circular]', 'special');
    }
  }
  if (isUndefined(name)) {
    if (array && key.match(/^\d+$/)) {
      return str;
    }
    name = JSON.stringify('' + key);
    if (name.match(/^"([a-zA-Z_][a-zA-Z_0-9]*)"$/)) {
      name = name.substr(1, name.length - 2);
      name = ctx.stylize(name, 'name');
    } else {
      name = name.replace(/'/g, "\\'")
                 .replace(/\\"/g, '"')
                 .replace(/(^"|"$)/g, "'");
      name = ctx.stylize(name, 'string');
    }
  }

  return name + ': ' + str;
}


function reduceToSingleString(output, base, braces) {
  var numLinesEst = 0;
  var length = output.reduce(function(prev, cur) {
    numLinesEst++;
    if (cur.indexOf('\n') >= 0) numLinesEst++;
    return prev + cur.replace(/\u001b\[\d\d?m/g, '').length + 1;
  }, 0);

  if (length > 60) {
    return braces[0] +
           (base === '' ? '' : base + '\n ') +
           ' ' +
           output.join(',\n  ') +
           ' ' +
           braces[1];
  }

  return braces[0] + base + ' ' + output.join(', ') + ' ' + braces[1];
}


// NOTE: These type checking functions intentionally don't use `instanceof`
// because it is fragile and can be easily faked with `Object.create()`.
function isArray(ar) {
  return Array.isArray(ar);
}
exports.isArray = isArray;

function isBoolean(arg) {
  return typeof arg === 'boolean';
}
exports.isBoolean = isBoolean;

function isNull(arg) {
  return arg === null;
}
exports.isNull = isNull;

function isNullOrUndefined(arg) {
  return arg == null;
}
exports.isNullOrUndefined = isNullOrUndefined;

function isNumber(arg) {
  return typeof arg === 'number';
}
exports.isNumber = isNumber;

function isString(arg) {
  return typeof arg === 'string';
}
exports.isString = isString;

function isSymbol(arg) {
  return typeof arg === 'symbol';
}
exports.isSymbol = isSymbol;

function isUndefined(arg) {
  return arg === void 0;
}
exports.isUndefined = isUndefined;

function isRegExp(re) {
  return isObject(re) && objectToString(re) === '[object RegExp]';
}
exports.isRegExp = isRegExp;

function isObject(arg) {
  return typeof arg === 'object' && arg !== null;
}
exports.isObject = isObject;

function isDate(d) {
  return isObject(d) && objectToString(d) === '[object Date]';
}
exports.isDate = isDate;

function isError(e) {
  return isObject(e) &&
      (objectToString(e) === '[object Error]' || e instanceof Error);
}
exports.isError = isError;

function isFunction(arg) {
  return typeof arg === 'function';
}
exports.isFunction = isFunction;

function isPrimitive(arg) {
  return arg === null ||
         typeof arg === 'boolean' ||
         typeof arg === 'number' ||
         typeof arg === 'string' ||
         typeof arg === 'symbol' ||  // ES6 symbol
         typeof arg === 'undefined';
}
exports.isPrimitive = isPrimitive;

exports.isBuffer = require('./support/isBuffer');

function objectToString(o) {
  return Object.prototype.toString.call(o);
}


function pad(n) {
  return n < 10 ? '0' + n.toString(10) : n.toString(10);
}


var months = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep',
              'Oct', 'Nov', 'Dec'];

// 26 Feb 16:19:34
function timestamp() {
  var d = new Date();
  var time = [pad(d.getHours()),
              pad(d.getMinutes()),
              pad(d.getSeconds())].join(':');
  return [d.getDate(), months[d.getMonth()], time].join(' ');
}


// log is just a thin wrapper to console.log that prepends a timestamp
exports.log = function() {
  console.log('%s - %s', timestamp(), exports.format.apply(exports, arguments));
};


/**
 * Inherit the prototype methods from one constructor into another.
 *
 * The Function.prototype.inherits from lang.js rewritten as a standalone
 * function (not on Function.prototype). NOTE: If this file is to be loaded
 * during bootstrapping this function needs to be rewritten using some native
 * functions as prototype setup using normal JavaScript does not work as
 * expected during bootstrapping (see mirror.js in r114903).
 *
 * @param {function} ctor Constructor function which needs to inherit the
 *     prototype.
 * @param {function} superCtor Constructor function to inherit prototype from.
 */
exports.inherits = require('inherits');

exports._extend = function(origin, add) {
  // Don't do anything if add isn't an object
  if (!add || !isObject(add)) return origin;

  var keys = Object.keys(add);
  var i = keys.length;
  while (i--) {
    origin[keys[i]] = add[keys[i]];
  }
  return origin;
};

function hasOwnProperty(obj, prop) {
  return Object.prototype.hasOwnProperty.call(obj, prop);
}

}).call(this,require('_process'),typeof global !== "undefined" ? global : typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{"./support/isBuffer":227,"_process":220,"inherits":218}]},{},[212]);
