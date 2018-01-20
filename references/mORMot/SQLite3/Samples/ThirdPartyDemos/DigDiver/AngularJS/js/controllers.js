function MainCtrl($rootScope, $scope, synConn, emAuth, $timeout, EmDrafts, $location, EmEml) {
	$scope.workplace = null;	
	$scope.fullUserName = null;
	$scope.authenticated = false;

	$scope.email = null;
	$scope.template = null;
	$scope.draft = null;

	$scope.sub = null;

	$scope.itemParams = null;

	$scope.toggleCollaped = function(){
		$rootScope.isCollapsed = !$rootScope.isCollapsed;
	};

	$scope.item = null;

	$scope.setItem = function(item) {
		$scope.item = item;
		window.item = item;
	};

	$scope.emlCanBeSent = false;
	$scope.emlCanBeStopped = false;

	$scope.$watch('item.Status', function(itms){		
		if ( itms ) {
			$scope.emlCanBeSent = EmEml.canBeSent(itms);
			$scope.emlCanBeStopped = EmEml.canBeStopped(itms);			
		}
	});

	$scope.startSending = function() {
		if ( $scope.item ) {
			$scope.item.startSending();
		}
	}

	$scope.stopSending = function(){
		if ( $scope.item ) {
			$scope.item.stopSending();
		}
	};

	$scope.alerts = [];

	$scope.alert = function(a) {
		$scope.alerts.push(a);
		var idx = $scope.alerts.length - 1;
		$timeout(function() {
			$scope.alerts.splice(idx, 1);
		}, 3000);
	};

	if ( emAuth.get('userName') ) {		
		$scope.fullUserName = emAuth.get('userName');
	}

	$scope.setUserName = function(userName) {
		emAuth.set('userName', userName);
		$scope.fullUserName = userName;
	};

	$scope.setWorkplace = function(wp) {
		$scope.workplace = wp;
		emAuth.set('workplace', wp);
	};

	$scope.setEmail = function(eml) {
		$scope.email = eml;
	};

	$scope.setTemplate = function(tpl) {
		$scope.template = tpl;
	}

	$scope.setDraft = function(drf) {
		$scope.draft = drf;
	};

	$scope.setWPS = function(wps) {
		$scope.workplaces = wps;
	};

	$scope.setItemParams = function(params) {
		$scope.itemParams = params;
	};

	$scope.setSub = function(s) {
		$scope.sub = s;
	}

	$scope.compose = function() {
		(new EmDrafts()).createDraft(function(drf){
			window.location.hash = '#/workplaces/'+emAuth.get('workplace').ID+'/drafts/'+drf.ID+'/edit';
		});
	};

	// preview for mobile devices

	$scope.showPreview = false;
	$scope.togglePreview = function() {
		$scope.showPreview = !$scope.showPreview;
	};

	$scope.showExclusionList = false;
	$scope.toggleExclusionList = function() {
		$scope.showExclusionList = !$scope.showExclusionList;
	};
}

var LS = (function(){

	var l = window.localStorage;


	return {
		get: function(key) {
			var v = l.getItem(key);
			if ( v && v.indexOf('__JSON__') === 0 ) {
				var vObj = null;
				try {
					vObj = JSON.parse(v.substr(8))	
					v = vObj;
				} catch(e) {					
					console.error('cannot parse localStorage value '+v);
				}
			}
			return v;
		},
		set: function(key, value) {
			if ( typeof value !== 'string' ) {
				value = '__JSON__'+JSON.stringify(value);
			}
			l.setItem(key, value);
		}
	}
})();

function LoginCtrl($window, $scope, $http, emAuth, synConn) {

	var u = (window.location+'').match(/http(s|)\:\/\/([^\/]+)(?:\:(\d+)|)/);

	var port = u[3] || null,
		secure = (u[1] == 's');

	if ( !port ) {
		port = secure ? '443' : '50888';
	} else if ( port == '443' ) {
		secure = true;
	}

	$scope.conn = {
		host: u[2]+':'+port,
		email: '',
		password: '',
		secure: secure
	};	

	$scope.remember = false;

	$scope.savedHosts = LS.get('savedHosts') || [];
	var lastUsedHostID = LS.get('lastUsedHostID');

	if ( lastUsedHostID !== null ) {
		$scope.conn = angular.extend({}, $scope.savedHosts[lastUsedHostID]);
		if ( typeof $scope.conn.secure === 'undefined' ) {
			$scope.conn.secure = !!$scope.conn.host.match(/\:443$/);
		}
	}



	$scope.loadHost = function(idx){
		var h = $scope.savedHosts[idx];
		if ( h ) {
			$scope.conn = angular.extend({}, h);
			lastUsedHostID = idx;
		} else {
			console.error('cannot load host data with index '+idx);
		}
	};

	function isHostSaved(hostData) {
		var saved = false;

		angular.forEach($scope.savedHosts, function(conn, i){
			if (
				conn.host == hostData.host &&
				conn.email == hostData.email &&
				conn.password == hostData.password
			) {
				saved = i;
				return false; // breaking the loop
			}
		});
		return saved;
	}

	$scope.signIn = function() {

		var hostData = {
			host: $scope.conn.host.trim(),
			email: $scope.conn.email.trim(),
			password: $scope.conn.password.trim()
		};

		var hostSaved = isHostSaved(hostData);

		if ( $scope.remember ) {
			if ( hostSaved === false ) {
				$scope.savedHosts.push(hostData);
				LS.set('savedHosts', $scope.savedHosts);
				lastUsedHostID = $scope.savedHosts.length-1;
			} else {
				lastUsedHostID = hostSaved;
			}
		}

		if ( lastUsedHostID !== null ) {
			LS.set('lastUsedHostID', lastUsedHostID);
		}

		var arr = $scope.conn.host.split(':'),
			host = arr[0],
			port = arr[1] || '50888';

			emAuth
				.set('host', host)
				.set('port', port)
				.set('email', $scope.conn.email)
				.set('password', $scope.conn.password)
				.set('secure', $scope.conn.secure);

			synConn.setOpts({
				host: host,
				port: port,
				email: $scope.conn.email,
				password: $scope.conn.password,
				secure: $scope.conn.secure
			});

			synConn.touch('wps', function(err, sa){
				if ( err ) {
					$scope.error = err;
				} else {
					emAuth.setAuthenticated(true);

					$scope.setUserName(sa.User);
					$window.location.hash = '#/workplaces';
				}
			});
	};
}

function WorkplacesCtrl($scope, $stateParams, wps) {
	//var wps = $route.current.locals.wps;
	$scope.setWPS(wps);
}

WorkplacesCtrl.resolve = {
	wps: function($q, $http, synConn) {
		var defer = $q.defer();

		synConn
			.http({ uri: 'wps/GetUserWorkplaces?UserName={email}' })
			.success(function(data){
				defer.resolve(data);
			})
			.error(function(data, status, headers, config){
				defer.reject(data);
			});		

		return defer.promise;
	}
}

app.controller('TopToolbarCtrl', function($scope){
	//console.log('TopToolbarCtrl section: '+$scope.section);
	//$scope.itemPath
});

app.controller('SidebarMenuCtrl', function($scope, emCategories){
	 var cats = emCategories.getAll();

	 if ( cats.then ) {
	 	cats.then(function(c){
			$scope.templateCats	= c;
	 	});
	 } else {
	 	$scope.templateCats	= cats;
	 } 
});

function openPreview(item) {

	// // 	var zoomStyle = ['<style>',
	// // 		'html { ',
	// // 			'-ms-zoom: 0.30;',
	// // 			'-moz-transform:scale(0.30);',
	// // 			'-moz-tansform-origin: 0 0;',
	// // 			'-o-transform: scale(0.30);',
	// // 			'-o-transform-origin: 0 0;',
	// // 			'-webkit-transform: scale(0.30);',
	// // 			'-webkit-transform-origin: 0 0;',
	// // 		'}</style>'].join('');

	// // 	zoomStyle = []; console.warn('zoom style disabled');


	// // 	data.EmailSource = data.EmailSource.replace(/(<\/head>)/i, zoomStyle+'$1');
	// // 	data.EmailSource = emUtils.expandAssetURLs(data.EmailSource, data.BaseURL);

	if ( item.then ) {
		item.then(
			function(itm){
				openPreview(itm);
			},
			function(err){
				console.error(err);				
			}
		);
		return;
	}

	var body = item.getBody();

	if ( body.then ) {
		body.then(
			function(b){
				setIFrameContent(b);
			},
			function(err){
				if ( err instanceof URIError || (err+'').match(/URI\s+malformed/i) ) {
					err = 'cannot unpack EmailSource';
				}
				setIFrameContent('An error occured: '+err);
			}
		);
	} else {
		setIFrameContent(body);
	}
}

function setIFrameContent(data) {

	var iframeId = 'preview-container-iframe',
		container = document.getElementById('preview-container'),
		iframe = document.getElementById(iframeId);

	if ( !container ) { return; }

	if ( !iframe ) {	

		var iframe = document.createElement('iframe');
		container.appendChild(iframe);

		iframe.id = iframeId;
	}

	var doc = iframe.document;
	if ( iframe.contentDocument ) {
		doc = iframe.contentDocument; // For NS6				
	} else if(iframe.contentWindow) {
		doc = iframe.contentWindow.document; // For IE5.5 and IE6
	}

	// Put the content in the iframe
	doc.open();
	doc.writeln(data);
	doc.close();
}

function CtrlConsole($scope, synConn) {

	$scope.params = [];
	$scope.headers = [];

	$scope.method = 'GET';
	$scope.uri = '/wp/1/settings/Drafts?Select=*&where=deleted=0&STARTINDEX=0&RESULTS=10';
	$scope.contentType = '';

	$scope.response = '';

	$scope.useRawBody = false;
	$scope.rawBodyBtn = 'Use raw body';
	$scope.rawBody = '';

	$scope.$watch('useRawBody', function(use){
		$scope.rawBodyBtn = $scope.useRawBody ? 'Don\'t use raw body' : 'Use raw body';
	});

	$scope.addParam = function() {
		$scope.useRawBody = false;
		$scope.params.push({
			name: '',
			value: ''
		})
	};

	$scope.toggleRawBody = function(){
		$scope.useRawBody = !$scope.useRawBody;

		if ( $scope.useRawBody ) {
			$scope.params = [];
		}
	};

	$scope.delParam = function(idx) {
		$scope.params.splice(idx, 1);
	};

	$scope.addHeader = function() {
		$scope.headers.push({
			name: '',
			value: ''
		})
	};

	$scope.delHeader = function(idx) {
		$scope.headers.splice(idx, 1);
	};

	$scope.sendRequest = function() {
		var uri = $scope.uri;
		if ( uri[0] === '/' ) {
			uri = uri.substr(1);
		}

		var params = $scope.params.length ? {} : null;
		angular.forEach($scope.params, function(p){
			params[p.name] = p.value;
		});

		var headers = {};
		if ( $scope.contentType ) {
			headers['Content-Type'] = $scope.contentType;
		}

		angular.forEach($scope.headers, function(h){
			headers[h.name] = h.value;
		});

		var opts = {
			method: $scope.method,
			uri: uri,
			headers: headers,
			synReq: true
		};

		if ( params ) {
			opts[ ( opts.method === 'GET' ) ? 'params' : 'data' ] = params;
		}

		if ( $scope.useRawBody ) {
			opts.data = $scope.rawBody;
		}

		synConn
			.http(opts)
			.success(function(data){
				if ( typeof data !== 'string' ) {
					data = JSON.stringify(data, null, '\t');
				}
				$scope.response = data;
			})
			.error(function(err){
				$scope.$apply(function(){
					$scope.response = err.status+' : '+err.message;
				});				
			});		
	};

}