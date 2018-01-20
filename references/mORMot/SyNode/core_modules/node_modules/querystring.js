// Copyright Joyent, Inc. and other Node contributors.
// Modified by UnityBase core team to be compatible with SyNode
// Query String Utilities

/**
 * @module querystring
 */

var QueryString = exports;

/**
 * This module provides utilities for dealing with query strings. Call `require('url')` to use it. This is port of NodeJS <a href="http://nodejs.org/api/querystring.html">querystring</a> module.
 * Small sample:
 *
 *        var querystring = require('querystring');
 *        querystring.stringify({param1: 'value1', param2: ['arr1', 'arr2'], paramEmpty: '' })
 *        // returns 'param1=value1&param2=arr1&param2=arr2&paramEmpty='
 *
 * @module querystring
 */

// If obj.hasOwnProperty has been overridden, then calling
// obj.hasOwnProperty(prop) will break.
// See: https://github.com/joyent/node/issues/1707
function hasOwnProperty(obj, prop) {
  return Object.prototype.hasOwnProperty.call(obj, prop);
}


function charCode(c) {
  return c.charCodeAt(0);
}


// a safe fast alternative to decodeURIComponent
QueryString.unescapeBuffer = function(s, decodeSpaces) {
  var out = new Buffer(s.length);
  var state = 'CHAR'; // states: CHAR, HEX0, HEX1
  var n, m, hexchar;

  for (var inIndex = 0, outIndex = 0; inIndex <= s.length; inIndex++) {
    var c = s.charCodeAt(inIndex);
    switch (state) {
      case 'CHAR':
          //noinspection FallThroughInSwitchStatementJS
          switch (c) {
          case charCode('%'):
            n = 0;
            m = 0;
            state = 'HEX0';
            break;
          case charCode('+'):
            if (decodeSpaces) c = charCode(' ');
            // pass thru
          default:
            out[outIndex++] = c;
            break;
        }
        break;

      case 'HEX0':
        state = 'HEX1';
        hexchar = c;
        if (charCode('0') <= c && c <= charCode('9')) {
          n = c - charCode('0');
        } else if (charCode('a') <= c && c <= charCode('f')) {
          n = c - charCode('a') + 10;
        } else if (charCode('A') <= c && c <= charCode('F')) {
          n = c - charCode('A') + 10;
        } else {
          out[outIndex++] = charCode('%');
          out[outIndex++] = c;
          state = 'CHAR';
          break;
        }
        break;

      case 'HEX1':
        state = 'CHAR';
        if (charCode('0') <= c && c <= charCode('9')) {
          m = c - charCode('0');
        } else if (charCode('a') <= c && c <= charCode('f')) {
          m = c - charCode('a') + 10;
        } else if (charCode('A') <= c && c <= charCode('F')) {
          m = c - charCode('A') + 10;
        } else {
          out[outIndex++] = charCode('%');
          out[outIndex++] = hexchar;
          out[outIndex++] = c;
          break;
        }
        out[outIndex++] = 16 * n + m;
        break;
    }
  }

  // TODO support returning arbitrary buffers.

  return out.slice(0, outIndex - 1);
};

/**
 * The unescape function used by querystring.parse, provided so that it could be overridden if necessary.
 *
 * @param s
 * @param decodeSpaces
 * @return {*}
 */
QueryString.unescape = function(s, decodeSpaces) {
  return QueryString.unescapeBuffer(s, decodeSpaces).toString();
};

/**
 * The escape function used by querystring.stringify, provided so that it could be overridden if necessary.
 * @param str
 * @return {string}
 */
QueryString.escape = function(str) {
  return encodeURIComponent(str);
};

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

/**
 * Serialize an object to a query string. Optionally override the default separator ('&') and assignment ('=') characters.
 *
 *   Example:
 *
 *        querystring.stringify({ foo: 'bar', baz: ['qux', 'quux'], corge: '' })
 *        // returns
 *        'foo=bar&baz=qux&baz=quux&corge='
 *
 *        querystring.stringify({foo: 'bar', baz: 'qux'}, ';', ':')
 *        // returns
 *        'foo:bar;baz:qux'
 *
 * @param {Object} obj
 * @param {String} [sep="&"]
 * @param {String} [eq="="]
 */
QueryString.stringify = QueryString.encode = function(obj, sep, eq, name) {
  sep = sep || '&';
  eq = eq || '=';
  if (obj === null) {
    obj = undefined;
  }

  if (typeof obj === 'object') {
    return Object.keys(obj).map(function(k) {
      var ks = QueryString.escape(stringifyPrimitive(k)) + eq;
      if (Array.isArray(obj[k])) {
        return obj[k].map(function(v) {
          return ks + QueryString.escape(stringifyPrimitive(v));
        }).join(sep);
      } else {
        return ks + QueryString.escape(stringifyPrimitive(obj[k]));
      }
    }).join(sep);

  }

  if (!name) return '';
  return QueryString.escape(stringifyPrimitive(name)) + eq +
         QueryString.escape(stringifyPrimitive(obj));
};

/**
 * Deserialize a query string to an object. Optionally override the default separator ('&') and assignment ('=') characters.
 *
 * Options object may contain maxKeys property (equal to 1000 by default), it'll be used to limit processed keys. Set it to 0 to remove key count limitation.
 *
 * Example:
 *
 *         querystring.parse('foo=bar&baz=qux&baz=quux&corge')
 *         // returns
 *         { foo: 'bar', baz: ['qux', 'quux'], corge: '' }
 *
 * @method parse
 * @param {Object} obj
 * @param {String} [sep="&"]
 * @param {String} [eq="="]
 * @param {Object} [options]
 * @param {Number} [options.maxKeys=1000]
 */
QueryString.parse = QueryString.decode = function(qs, sep, eq, options) {
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

    try {
      k = decodeURIComponent(kstr);
      v = decodeURIComponent(vstr);
    } catch (e) {
      k = QueryString.unescape(kstr, true);
      v = QueryString.unescape(vstr, true);
    }

    if (!hasOwnProperty(obj, k)) {
      obj[k] = v;
    } else if (Array.isArray(obj[k])) {
      obj[k].push(v);
    } else {
      obj[k] = [obj[k], v];
    }
  }

  return obj;
};
