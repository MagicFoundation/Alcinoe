angular.module('syn-auth', ['em-utils'], function($provide) {

	// the number of login retries on 403 before logout
	var MAX_RECONNECT_COUNT = 5;

	$provide.factory('SynAuth', function(crc32, sha256) {

		function SynAuth(host, port) {

			var defaults = {
				host: '',
				defaultPort: '888',

				User : "",
				fRoot : "",
				fSessionID : 0,
				fSessionIDHexa8 : "",
				fSessionPrivateKey : 0,
				fSessionTickCountOffset : 0,
				fLastSessionTickCount : 0,

				PasswordHashHexa : "",
				fServerTimeStampOffset : 0,
				fcallBack : null,
				ffailCallBack : null
			};  // SynAuth namespace

			this.connectionReady = false;
			this.readyCallbacks = [];

			for ( var p in defaults ) {
				this[p] = defaults[p];
			}

			if ( host ) {
				this.setHost(host, port);	
			}
		}

		var sp = SynAuth.prototype;

		sp.ready = function(cb) {
			if ( this.connectionReady ) {
				cb(this);
			} else {
				this.readyCallbacks.push(cb);
			}
		};

		sp.fireReadyCbs = function() {
			var _this = this;
			angular.forEach(this.readyCallbacks, function(cb){
				cb(_this);
			});
		};

		sp.wrap = function(method){
			var that = this;
			return function(){
				method.apply(that, arguments);
			};
		};

		sp.setHost = function(host, port) {
			if ( !host.match(/\:\d+$/) ) {
				host += ':'+(port || this.defaultPort);
			}
			this.host = host;
		};
				
		sp.LogIn = function (root, username, password, callback, failCallback){
			this.fRoot = root;
			this.User = username;
			this.PasswordHashHexa = sha256.hash("salt"+password);
			if (callback) {this.fcallBack = callback;}
			if (failCallback) {this.ffailCallback = failCallback;}
			$.get(this.host+"/"+root+"/TimeStamp", this.wrap(this.gotTimeStamp));
		}

		sp.LogInAgain = function(callback){ //after timeout error for silent re-login
			this.fSessionID = 0;
			this.fSessionIDHexa8 = "";
			this.fSessionPrivateKey = 0;
			if (callback) {this.fcallBack = callback;} else {this.fcallBack = null;}
			$.get(this.host+"/"+this.fRoot+"/TimeStamp", this.wrap(this.gotTimeStamp));
		}

		sp.gotTimeStamp = function (timestamp) {
			var s = '', d = new Date(), clientTime = '';
			timestamp = parseInt(timestamp, 10);
			s = d.getFullYear().toString(2);
			while(s.length < 13) { s = '0'+s;}
			clientTime = s;
			s = d.getMonth().toString(2);
			while(s.length < 4) { s = '0'+s;}
			clientTime = clientTime +s;
			s = (d.getDate()-1).toString(2);
			while(s.length < 5) { s = '0'+s;}
			clientTime = clientTime +s;
			s = d.getHours().toString(2);
			while(s.length < 5) { s = '0'+s;}
			clientTime = clientTime +s;
			s = d.getMinutes().toString(2);
			while(s.length < 6) { s = '0'+s;}
			clientTime = clientTime +s;
			s = d.getSeconds().toString(2);
			while(s.length < 6) { s = '0'+s;}
			clientTime = clientTime +s;

			this.fServerTimeStampOffset = (timestamp - Math.floor(d.getTime()/10));
			$.get(this.host+"/"+this.fRoot+"/auth?UserName="+this.User, this.wrap(this.gotNonce));
		}

		sp.gotNonce = function (aNonce){
			var that = this;
			//create client nonce
			var aClientNonce = "", s = "", d = new Date();
			aClientNonce = d.getFullYear().toString();
			s = d.getMonth().toString();
			if (s.length === 1) { s = '0'+s;}
			aClientNonce = aClientNonce + '-' + s;
			s = d.getDate().toString();
			if (s.length === 1) { s = '0'+s;}
			aClientNonce = aClientNonce + '-' + s + ' ';
			s = d.getHours().toString();
			if (s.length === 1) { s = '0'+s;}
			aClientNonce = aClientNonce + s;
			s = d.getMinutes().toString();
			if (s.length === 1) { s = '0'+s;}
			aClientNonce = aClientNonce + ':' + s;
			s = d.getSeconds().toString();
			if (s.length === 1) { s = '0'+s;}
			aClientNonce = aClientNonce + ':' + s;			
			aClientNonce = sha256.hash(aClientNonce);
			s = this.host+"/"+ this.fRoot+"/auth?UserName="+this.User+"&Password=" + 
			 sha256.hash(this.fRoot+aNonce.result+aClientNonce+this.User+this.PasswordHashHexa )+
			 "&ClientNonce="+aClientNonce;
			$.ajax({
				type: "GET",
				dataType: "json",
				url: s,
				success: function(){ that.gotSession.apply(that, arguments); },
				error: function(){ that.ffailCallback.apply(that, arguments); }
			});                   
		};

		sp.gotSession = function (aSessionKey){
			var sessArr = aSessionKey.result.split('+');

			this.fSessionID = parseInt(sessArr[0], 10);
			this.fSessionIDHexa8 = this.fSessionID.toString(16);

			while ( this.fSessionIDHexa8.length < 8 ) { this.fSessionIDHexa8 = '0'+this.fSessionIDHexa8; }

			this.fSessionPrivateKey = crc32(this.PasswordHashHexa, crc32(aSessionKey.result, 0));

			if (this.fcallBack != null) {
				this.connectionReady = true;
				this.fireReadyCbs();
				this.fcallBack();
			}
		}

		sp.SessionSign = function (url) {
			var Tix, Nonce, s, ss; 
			Tix = Date.now(); // # of ms since Epoch

			if ( Tix <= this.fLastSessionTickCount ) {
				this.fLastSessionTickCount += 1;
			} else {
				this.fLastSessionTickCount = Tix;	
			}			
			Nonce = Tix.toString(16);

			while ( Nonce.length < 8 ) { Nonce = '0'+Nonce; }
			if ( Nonce.length > 8 ) { Nonce = Nonce.slice(Nonce.length-8) }

			ss = crc32(url, crc32(Nonce, this.fSessionPrivateKey)).toString(16);

			while ( ss.length < 8 ) { ss = '0'+ss; }

			s = url.indexOf("?") == -1 ? url+'?session_signature=' : url+'&session_signature=';

			return s + this.fSessionIDHexa8 + Nonce + ss;  
		}

		sp.getURL = function(uri) {
			return this.host+'/'+this.SessionSign(uri);
		}

		sp.Logout = function (callback) {
			if (this.fSessionID == 0) {if (callback){callback();}} else {
				$.get(this.host+"/"+this.fRoot+"/auth?UserName="+this.User+"&Session="+this.fSessionID, callback);
				this.fRoot = '';
				this.User = '';
				this.fSessionID = 0;
				this.fSessionIDHexa8 = "";
				this.fSessionPrivateKey = 0;
			}
		}

		return SynAuth;
	});

	
	// wps
	// wp/%/settings
	// wp/%/group
	// wp/%/bounce

	// infinitypropertycomau

	// garethinfinitypropertycomau

	// bernieinfinitypropertycomau

	var forEach = angular.forEach;

	function isArray(value) {
	  return Object.prototype.toString.apply(value) == '[object Array]';
	}

	function isObject(value){return value != null && typeof value == 'object';}

	function sortedKeys(obj) {
	  var keys = [];
	  for (var key in obj) {
	    if (obj.hasOwnProperty(key)) {
	      keys.push(key);
	    }
	  }
	  return keys.sort();
	}

	function forEachSorted(obj, iterator, context) {
	  var keys = sortedKeys(obj);
	  for ( var i = 0; i < keys.length; i++) {
	    iterator.call(context, obj[keys[i]], keys[i]);
	  }
	  return keys;
	}

	function encodeUriQuery(val, pctEncodeSpaces) {
	  return encodeURIComponent(val).
	             replace(/%40/gi, '@').
	             replace(/%3A/gi, ':').
	             replace(/%24/g, '$').
	             replace(/%2C/gi, ',').
	             replace(/%20/g, (pctEncodeSpaces ? '%20' : '+'));
	}	

	function buildQueryString(params) {
		var parts = [];
		forEachSorted(params, function(value, key) {
			if (value == null || value == undefined) return;
			if (!isArray(value)) value = [value];

			forEach(value, function(v) {
				if (isObject(v)) {
					v = toJson(v);
				}
				parts.push(encodeUriQuery(key) + '=' +
				         encodeUriQuery(v));
			});
		});
		return parts.join('&');
	}

    function buildUrl(url, params) {
      if (!params) return url;
      var qs = buildQueryString(params);

      return url + ((url.indexOf('?') == -1) ? '?' : '&') + qs;
    }

	$provide.factory('synConn', function(SynAuth, emAuth, $http, $q, emsLoadingBar) {
		var synConn = {
			opts: {
				host: emAuth.get('host'),
				port: emAuth.get('port'),
				email: emAuth.get('email'),
				password: emAuth.get('password'),
				secure: emAuth.get('secure')
			}
		};

		var roots = [
			'wps',
			'wp/\\d+/settings',
			'wp/\\d+/group',
			'wp/\\d+/bounce'
		];

		var connCache = {};	

		function setOpts(o) {
			synConn.opts = o;
			if ( !o.host ) { throw new Exception('[SynConn] setOpts. "host" option is not defined.'); }
			if ( !o.email ) { throw new Exception('[SynConn] setOpts. "email" option is not defined.'); }
			if ( !o.password ) { throw new Exception('[SynConn] setOpts. "password" option is not defined.'); }
			o.port = o.port || '888';
		}

		 function supplant (str, o) {
		    return str.replace(
		        /\{([^{}]*)\}/g,
		        function (a, b) {
		            var r = o[b];
		            return typeof r === 'string' || typeof r === 'number' ? r : a;
		        }
		    );
		}

		function getBaseURL() {
			var pr = synConn.opts.secure ? 'https' : 'http';
			return pr+'://'+synConn.opts.host+':'+synConn.opts.port+'/';
		}

		function createNewSynAuth(root, done) {
			var pr = synConn.opts.secure ? 'https' : 'http';
			//return { SynAuth: 'dummy'+(new Date()).getTime() };

			var sa = new SynAuth(pr+'://'+synConn.opts.host, synConn.opts.port);

			sa.LogIn(root, synConn.opts.email, synConn.opts.password, function(){
				done(null, sa);
			}, function(xhr, status, msg){
				// failback
				var err = new Error(msg);
				err.status = xhr.status;
				done(err);
			});			

			return sa;
		}

		function getConnectionFromURi(uri, done) {
			var found = false;
			// looking for cached connection
			for ( var root in connCache ) {
				if ( uri.indexOf(root) === 0 ) {
					return connCache[root].ready(function(conn){
						done(null, conn);	
					});					
				}
			}

			// creating new connection
			for (var r, res, i = 0; i < roots.length; i++) {
				if ( res = (new RegExp(roots[i],'i')).exec(uri) ) {
					r = res[0]; // matched root uri, like wp/1/settings
					found = true;
					connCache[r] = createNewSynAuth(r, function(err, sa){
						if ( err ) {
							delete connCache[r];
							return done(err);
						}
						return done(null, sa);
					});
				}
			}
			if ( !found ) {
				done(new Error('getConnectionFromURi: Cannot get rootURi for '+uri));
			}
		}

		function removeCachedConnection(sa) {
			var found = false;
			// looking for cached connection
			for ( var root in connCache ) {
				if ( connCache[root] === sa) {
					delete connCache[root];
					break;
				}
			}
		}		

		function http(o) {
			var that = {
				sa: null,
				err: null,
				errorCb: null,
				successCb: null
			};

			if ( !o.headers ) {
				o.headers = {};
			}

			var reconnect = 0;

			var wp = emAuth.get('workplace');

			if ( wp ) {
				synConn.opts.wpId = emAuth.get('workplace').ID;	
			}		

			o.uri = supplant(o.uri, synConn.opts);

			function connect() {
				getConnectionFromURi(o.uri, function(err, sa){
					if ( err ) { that.err = err;  }
					that.sa = sa;

					setTimeout(function() {
						if ( err ) { return that.errorCb(err); }
						runRequest();
					}, 1);
				});				
			}

			connect();

			function runRequest() {

				var url;

				if ( o.params ) {
					url = that.sa.getURL( buildUrl(o.uri, o.params) );
					delete o.params;
				} else {
					url = that.sa.getURL(o.uri);
				}

				if ( o.data ) {
					if ( o.headers['Content-Type'] === 'application/x-www-form-urlencoded' ) {
						o.data = buildQueryString(o.data);
					}
				}

				var opts = angular.extend({ url : url }, o);
				delete opts.uri;
				opts.method = opts.method || 'GET';

				$http(opts).success(function(){					
					that.successCb.apply(this, arguments);
					emsLoadingBar.stop();
					reconnect = 0;
				}).error(function(data){
					var err = new Error(data.ErrorText);
						err.ErrorCode = data.ErrorCode;

					if ( err && err.ErrorCode == 403 ) {
						if ( reconnect == MAX_RECONNECT_COUNT ) {
							alert('Authentication failure. You will be logged out now. Shall this error repeat, contact your server administrator.');						
							window.location.hash = '#/logout';
							return;
						}
						removeCachedConnection(that.sa);
						reconnect += 1;
						connect();
					} else {
						var args = Array.prototype.slice.call(arguments);
						args.unshift(err);
						that.errorCb.apply(this, args);
						emsLoadingBar.stop();
					}					
				});
				emsLoadingBar.start();
			}

			return {
				success: function(cb){
					that.successCb = cb;
					return this;
				},
				error: function(cb){
					that.errorCb = cb;
					return this;
				}

			};
		}

		function touch(uri, done) {
			getConnectionFromURi(uri, done);
		}

		synConn.logout = function() {
			connCache = {};
		};

		//getConnectionFromURi('wp/1/settings/GetTemplateMessage?ID=');

		synConn.setOpts = setOpts;
		synConn.http = http;
		synConn.touch = touch;
		synConn.getBaseURL = getBaseURL;

		return synConn;
	});
});