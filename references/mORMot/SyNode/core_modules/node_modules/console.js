// Copyright Joyent, Inc. and other Node contributors.
// Modified by UnityBase core team to be compatible with SyNode

var util = require('util');

/**
 * Console & log output functions
 * Put something to log with log levels depending on method. In case of GUI server do echo to GUI log (if enabled).
 * In case of command line - echo to `stdout`.
 *
 * Do not create this class directly - use global {@link console} already created by UB.
 *
 *      console.log('%s is a %s usually with weight less then %dgr', 'apple', 'fruit', 100);
 *      //Will output "apple is a fruit usually with weight less then 100gr"
 *      console.log('apple', 'fruit', 100);
 *      //will output "apple fruit 100"
 *      console.debug('something');
 *      // will output to log only in "Debug" build (UBD.exe)
 *
 * Arguments, passed to console output functions are transformed to string using {@link util.format} call.
 *
 * @module console
 */

/**
 * Do not create directly, use {@link console} instance from `global`.
 *
 *      console.debug('Yeh!');
 *
 * @class Console
 */
function Console(stdout, stderr) {
  if (!(this instanceof Console)) {
    return new Console(stdout, stderr);
  }
  if (!stdout || typeof stdout.write !== 'function') {
    throw new TypeError('Console expects a writable stream instance');
  }
  if (!stderr) {
    stderr = stdout;
  }
  var prop = {
    writable: true,
    enumerable: false,
    configurable: true
  };
  prop.value = stdout;
  Object.defineProperty(this, '_stdout', prop);
  prop.value = stderr;
  Object.defineProperty(this, '_stderr', prop);
  prop.value = {};
  Object.defineProperty(this, '_times', prop);

  // bind the prototype functions to this Console instance
  Object.keys(Console.prototype).forEach(function(k) {
    this[k] = this[k].bind(this);
  }, this);
}

/**
 * Output to log with log level `Info`. Internally use util.format for create output, so
 * format chars can be used:
 *
 *  - %s - String.
 *  - %d - Number (both integer and float).
 *  - %j - JSON.
 *  - % - single percent sign ('%'). This does not consume an argument.
 *
 *      console.log('%s is a %s usually with weight less then %dgr', 'apple', 'fruit', 100);
 *      //Will output "apple is a fruit usually with weight less then 100gr"
 *
 *      console.log('apple', 'fruit', 100);
 *      //will output "apple fruit 100"
 *
 *      console.log('the object JSON is %j', {a: 12, b: {inner: 11}});
 *      // will output a JSON object instead of [object Object]
 *
 * @param {...*}
 */
Console.prototype.log = function() {
  this._stdout.write(util.format.apply(this, arguments) + '\n');
};

/**
 * Output to log with log level `Debug`. In case {@link process.isDebug} is false - do nothing
 * @method
 * @param {...*}
 */
Console.prototype.debug = process.isDebug ?
function() {
    this._stdout.write(util.format.apply(this, arguments) + '\n', 2); //UB specific
} :
function() {
};

/**
 * Output to log with log level `Info` (alias for console.log)
 * @method
 * @param {...*}
 */
Console.prototype.info = Console.prototype.log;


/**
 * Output to log with log level `Warning`. In case of OS console echo output to stderr
 * @param {...*}
 */
Console.prototype.warn = function() {
  this._stderr.write(util.format.apply(this, arguments) + '\n', 4); //UB specific
};

/**
 * Output to log with log level `Error`. In case of OS console echo output to stderr
 * @param {...*}
 */
Console.prototype.error = function() {
  this._stderr.write(util.format.apply(this, arguments) + '\n', 5); //UB specific
};

/**
 * Uses util.inspect on obj and prints resulting string to stdout.
 * @param {Object} object
 */
Console.prototype.dir = function(object) {
  this._stdout.write(util.inspect(object) + '\n');
};

/**
 * Mark a time.
 * @param {String} label
 */
Console.prototype.time = function(label) {
  this._times[label] = Date.now();
};

/**
 * Finish timer, record output
 * @example
 *
 *      console.time('100-elements');
 *        for (var i = 0; i < 100; i++) {
 *         ;
 *      }
 *      console.timeEnd('100-elements');
 *
 * @param {string} label
 */
Console.prototype.timeEnd = function(label) {
  var time = this._times[label];
  if (!time) {
    throw new Error('No such label: ' + label);
  }
  var duration = Date.now() - time;
  this.log('%s: %dms', label, duration);
};


Console.prototype.trace = function() {
  // TODO probably can to do this better with V8's debug object once that is
  // exposed.
  var err = new Error;
  err.name = 'Trace';
  err.message = util.format.apply(this, arguments);
  //MPV Error.captureStackTrace(err, arguments.callee);
  this.error(err.stack);
};

/**
 * Similar to {@link assert#ok}, but the error message is formatted as {@link util#format util.format(message...)}.
 * @param expression
 */
Console.prototype.assert = function(expression) {
  if (!expression) {
    var arr = Array.prototype.slice.call(arguments, 1);
    require('assert').ok(false, util.format.apply(this, arr));
  }
};

module.exports = new Console(process.stdout, process.stderr);
module.exports.Console = Console;
