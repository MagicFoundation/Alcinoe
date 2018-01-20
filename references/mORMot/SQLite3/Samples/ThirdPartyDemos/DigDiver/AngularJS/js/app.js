var app = angular.module('em7frontend', [
	'ui.router', 'ui.bootstrap', 'infinite-scroll', 'syn-auth', 'grf.uploader',
	'ui.multiselect', 'gt.select', 'gt.dropdown'
]);

app.config(function($stateProvider, $urlRouterProvider){
	$stateProvider
		.state('login', {
			url: '/login',
			requireLogin: false,
			selectServer: false,
			views: {
				"root": {					
					templateUrl: 'partials/login.html',
					controller: LoginCtrl
				}
			}
		})
		.state('loginto', {
			url: '/loginto',
			requireLogin: false,
			selectServer: true,
			views: {
				"root": {					
					templateUrl: 'partials/login.html',
					controller: LoginCtrl
				}
			}
		})		
		.state('console', {
			url: '/console',
			requireLogin: true,
			views: {
				'root': {
					templateUrl: 'partials/console.html',
					controller: CtrlConsole
				},
			}
		})		
		//*
		.state('workplaces', {
			url: '/workplaces',
			requireLogin: true,
			views: {
				"root": {
					templateUrl: 'partials/workplaces.html',
					controller: WorkplacesCtrl,
					resolve: WorkplacesCtrl.resolve
				}
			}
		})
		.state('wp', {
			url: '/workplaces/:wpid',
			requireLogin: true,
			views: {
				'root': {
					templateUrl: 'partials/wp-base.html',
					controller: function($scope, $stateParams, $location, $state, wps) {

						if ( !$scope.workplaces ) {
							$scope.setWPS(wps);
						}

						$scope.wpid = $stateParams.wpid;

						if ( $scope.workplaces && $stateParams.wpid ) {
							angular.forEach($scope.workplaces, function(wp) {
								if ( wp.ID == $stateParams.wpid ) {
									$scope.setWorkplace(wp);
								}
							});
						}

						// $scope.$on('$stateChangeSuccess', function(event, toState, toParams, fromState, fromParams){
						// 	$scope.section = null;
						// 	if ( toState.name.match(/^wp\./i) ) {
						// 		var p = event.currentScope.$parent.$parent.$parent,
						// 			nameArr = toState.name.split('.');
						// 		$scope.section = nameArr[1].split('-')[0];
						// 		$scope.subSection = nameArr[1].split('-')[1] || null;
						// 	}
						// });
					},
					resolve: WorkplacesCtrl.resolve
				}
			}
		})
			.state('wp.contacts', {
				url:'/contacts',
				requireLogin: true,
				views: {
					'topbar': {
						templateUrl: 'partials/topbar/contacts.html',
						controller: 'ContactsSidebarMenuCtrl',
						resolve: ContactsSidebarMenuCtrlResolve						
					},
					'content': { templateUrl: 'partials/sidebar-main-layuot.html' },
					'contentsidebar@wp.contacts': {
						templateUrl: 'partials/contacts-sidebar.html',
						controller: 'ContactsSidebarMenuCtrl',
						resolve: ContactsSidebarMenuCtrlResolve
					},
					'contentmain@wp.contacts': {
						templateUrl: 'partials/contacts-blank.html',
						controller: 'ContactsSidebarMenuCtrl',
						resolve: ContactsSidebarMenuCtrlResolve						
					}					
				}
			})
			.state('wp.contacts.group', {
				url:'/:placement/:gid',
				requireLogin: true,
				views: {
					'topbar': {
						templateUrl: 'partials/topbar/search.html'
					},
					'content': { templateUrl: 'partials/sidebar-main-layuot.html' },
					'contentsidebar@wp.contacts.group': {
						templateUrl: 'partials/contacts-sidebar.html',
						controller: 'ContactsSidebarMenuCtrl',
						resolve: ContactsSidebarMenuCtrlResolve
					},
					'contentmain@wp.contacts': {
						templateUrl: 'partials/contacts.html',
						controller: 'ContactsCtrl'
					}					
				}
			})
			.state('wp.templates', {
				url: '/templates',
				requireLogin: true,
				views: {
					'topbar': {
						templateUrl: 'partials/topbar/search.html'
					},
					'content': { templateUrl: 'partials/sidebar-main-layuot.html' },
					'contentsidebar@wp.templates': {
						templateUrl: 'partials/content-sidebar.html',
						controller: 'SidebarMenuCtrl'
					},
					'contentmain@wp.templates': {
						templateUrl: 'partials/templates.html',
						controller: TemplatesCtrl						
					}
				}
			})
			.state('wp.templates-subfolder', {
				url: '/templates/folder/:folder',
				requireLogin: true,
				views: {
					'topbar': {
						templateUrl: 'partials/topbar/search.html'
					},
					'content': { templateUrl: 'partials/sidebar-main-layuot.html' },
					'contentsidebar@wp.templates-subfolder': {
						templateUrl: 'partials/content-sidebar.html',
						controller: 'SidebarMenuCtrl'
					},
					'contentmain@wp.templates-subfolder': {
						templateUrl: 'partials/templates.html',
						controller: TemplatesCtrl						
					}
				}
			})
			.state('wp.templates-trash', {
				url: '/templates/trash',
				requireLogin: true,
				views: {
					'topbar': {
						templateUrl: 'partials/topbar/search.html'
					},
					'content': { templateUrl: 'partials/sidebar-main-layuot.html' },
					'contentsidebar@wp.templates-trash': {
						templateUrl: 'partials/content-sidebar.html',
						controller: 'SidebarMenuCtrl'
					},
					'contentmain@wp.templates-trash': {
						templateUrl: 'partials/templates.html',
						controller: TemplatesCtrl						
					}
				}
			})
				.state('wp.templates.edit', {
					url: '/:template/edit',
					requireLogin: true,
					views: {
						// 'contentsidebar': {
						// 	//templateUrl: 'partials/content-sidebar.html'
						// 	template: 'contentsidebar!'
						// },
						// 'contentmain': {
						// 	template: 'contentmain!'
						// 	// templateUrl: 'partials/templates.html',
						// 	// controller: TemplatesCtrl						
						// },
						// 'content@wp': {
						// 	template: 'CONTENT!'
						// }
						'topbar@wp': {
							//template: 'top!'
							templateUrl: 'partials/topbar/editor.html'
						},
						'content@wp': {
							//template: 'content!'
							templateUrl: 'partials/editor-template.html',
							controller: 'TemplateEditorCtrl',
							resolve: TemplateEditorResolve
						}
					}
				})
				.state('wp.templates.restore', {
					url: '/:template/restore',
					requireLogin: true,
					views: {
						'content@wp': {
							template: '<ui-view/>',
							controller: function($scope, $stateParams, emAuth, synConn, $location){

								synConn
									.http({
										uri: 'wp/{wpId}/settings/Templates/'+$stateParams.template
									})
									.success(function(tpl){
										tpl.CategoryID = 0;

										synConn
											.http({
												method: 'PUT',
												headers: {
													'Content-Type': 'application/json; charset=UTF-8'
												},
												uri: 'wp/{wpId}/settings/Templates/'+$stateParams.template,
												data: JSON.stringify(tpl)
											})
											.success(function(data){
												$location.path('/workplaces/'+emAuth.workplace.ID+'/templates');
											})
											.error(function(err, status, headers, config){
												console.log(arguments);
											});
									})
									.error(function(err, status, headers, config){
										console.log(arguments);
									});
							}
						}
					}
				})
				.state('wp.templates.delete', {
					url: '/:template/delete',
					requireLogin: true,
					views: {
						'content@wp': {
							template: '<ui-view/>',
							controller: function($scope, $stateParams, emAuth, synConn, $location){

								// synConn
								// 	.http({
								// 		uri: 'wp/{wpId}/settings/Templates/'+$stateParams.template
								// 	})
								// 	.success(function(tpl){
								// 		tpl.CategoryID = -1;

										synConn
											.http({
												method: 'PUT',
												headers: {
													'Content-Type': 'application/json; charset=UTF-8'
												},
												uri: 'wp/{wpId}/settings/Templates/'+$stateParams.template,
												data: JSON.stringify({ CategoryID: -1 })
											})
											.success(function(data){
												$location.path('/workplaces/'+emAuth.workplace.ID+'/templates');
											})
											.error(function(err, status, headers, config){
												console.log(arguments);
											});
									// })
									// .error(function(err, status, headers, config){
									// 	debugger;
									// 	console.log('TEMPLATE ERR');
									// 	console.log(arguments);
									// });
							}
						}
					}
				})
			.state('wp.outbox', {
				url: '/outbox',
				requireLogin: true,
				views: {
					'topbar': {
						templateUrl: 'partials/topbar/sending-search.html'
					},
					'content': {
						templateUrl: 'partials/sidebar-main-layuot.html'
					},
					'contentsidebar@wp.outbox': {
						templateUrl: 'partials/content-sidebar.html',
						controller: 'SidebarMenuCtrl'
					},
					'contentmain@wp.outbox': {
						templateUrl: 'partials/outbox.html',
						controller: EmailsCtrl						
					}					
				}
			})
				.state('wp.outbox.item', {
					url: '/:id',
					requireLogin: true,
					views: {
						'itemdetails@wp.outbox': {
							templateUrl: 'partials/email-item-details.html',
							controller: 'EmailsItemDetailsCtrl'						
						}
					}
				})
					.state('wp.outbox.item.subview', {
						url: '/:subview',
						requireLogin: true,
						views: {
							'itemdetails@wp.outbox': {
								templateUrl: 'partials/email-item-details.html',
								controller: 'EmailsItemDetailsCtrl'						
							}
						}
					})				
			.state('wp.outbox-trash', {
				url: '/outbox/trash',
				requireLogin: true,
				views: {
					'topbar': {
						templateUrl: 'partials/topbar/sending-search.html'
					},
					'content': {
						templateUrl: 'partials/sidebar-main-layuot.html'
					},
					'contentsidebar@wp.outbox-trash': {
						templateUrl: 'partials/content-sidebar.html',
						controller: 'SidebarMenuCtrl'
					},
					'contentmain@wp.outbox-trash': {
						templateUrl: 'partials/outbox.html',
						controller: EmailsCtrl						
					}					
				}
			})			
			.state('wp.drafts', {
				url: '/drafts',
				requireLogin: true,
				views: {
					'topbar': {
						templateUrl: 'partials/topbar/search.html'
					},
					'content': { templateUrl: 'partials/sidebar-main-layuot.html' },
					'contentsidebar@wp.drafts': {
						templateUrl: 'partials/content-sidebar.html',
						controller: 'SidebarMenuCtrl'
					},
					'contentmain@wp.drafts': {
						templateUrl: 'partials/drafts.html',
						controller: DraftsCtrl						
					}
				}
			})
			.state('wp.drafts-trash', {
				url: '/drafts/trash',
				requireLogin: true,
				views: {
					'topbar': {
						templateUrl: 'partials/topbar/search.html'
					},
					'content': { templateUrl: 'partials/sidebar-main-layuot.html' },
					'contentsidebar@wp.drafts-trash': {
						templateUrl: 'partials/content-sidebar.html',
						controller: 'SidebarMenuCtrl'
					},
					'contentmain@wp.drafts-trash': {
						templateUrl: 'partials/drafts.html',
						controller: DraftsCtrl						
					}
				}
			})			
				.state('wp.drafts.edit', {
					url: '/:draft/edit',
					requireLogin: true,
					views: {
						'topbar@wp': {
							//template: 'top!'
							templateUrl: 'partials/topbar/editor.html'
						},
						'content@wp': {
							//template: 'content!'
							templateUrl: 'partials/editor-draft.html',
							controller: 'DraftEditorCtrl',
							resolve: DraftEditorResolve
						}
					}
				})			
		.state('logout', {
			url: '/logout',
			requireLogin: false,
			views: {
				"root": {					
					template: '<div></div>',
					controller: function($scope, $window, synConn){
						synConn.logout();

						$scope.setUserName(null);
						$scope.setWorkplace(null);				
						window.location.hash = '#/login';
					}
				}
			}
		})

		//*/	

		$urlRouterProvider.otherwise('/login');
});

app.run(function($rootScope, $location, emAuth, emsLoadingBar) {
    $rootScope.$on("$stateChangeStart", function(event, toState, toParams, fromState, fromParams) {

    	$rootScope.isCollapsed = true;

    	$rootScope.selectServer = toState.selectServer;
    	
    	setTimeout(function(){
    		emsLoadingBar.enabled = true;
    	}, 5000);

    	if ( toState.requireLogin && !emAuth.isAuthenticated() ) {
    		event.preventDefault();
    		$location.path('/login');
    	} else {
    		if ( typeof toParams.gid !== 'undefined' ) {
    			$rootScope.gid = toParams.gid;
    		}
			$rootScope.section = null;
			if ( toState.name.match(/^wp\./i) ) {
				var nameArr = toState.name.split('.');

				$rootScope.section = nameArr[1].split('-')[0];
				$rootScope.subSection = nameArr[1].split('-')[1] || null;
				$rootScope.uiSection = ($rootScope.section == 'contacts') ? 'Contacts' : 'Emails';
			}
    	}
    });
});

app.factory('emAuth', function(){
	var auth = {},
		authenticated = true,
		rememberMe = true,
		ls = window.localStorage;

	function getItem(key) {
		var v = ls.getItem(key);

		if ( v && v.indexOf('__JSON__') === 0 ) {
			v = v.substr(8);
			return ( v === 'undefined' ) ? undefined : JSON.parse(v);
		}
		return v;
	}		

	if ( rememberMe ) {
		auth = {
			'host': getItem('host'),
			'port': getItem('port'),
			'email': getItem('email'),
			'password': getItem('password'),
			'workplace': getItem('workplace'),
			'userName': getItem('userName'),
			'secure': getItem('secure')
		};
	}

	return {
		setAuthenticated: function(a) {
			authenticated = a;
		},
		isAuthenticated: function() {
			return authenticated;
		},
		set: function(key, value) {
			auth[key] = value;
			if ( rememberMe ) {
				if ( typeof value !== 'string' ) {
					value = '__JSON__'+JSON.stringify(value);
				}
				// console.log('key: '+typeof key);
				// console.log(key);

				// console.log('value: '+typeof value);
				// console.log(value);

				ls.setItem(key, value);
			}
			return this;
		},
		get: function(key) {
			return auth[key];
		}
	};
});

app.factory('emGroups', function(emAuth, synConn){
	var that = new Emmi();
	that.cache = {
		//id: {...group...}
	};

	function getGroup(gid, done) {
		
		if ( ['object', 'string', 'number'].indexOf(typeof gid) == -1 ) {
			return done({ ID: -1, GroupName: 'GROUP ERROR' });
		}

		if ( typeof that.cache[gid] === 'object' && that.cache[gid] !== null ) {
			done(that.cache[gid]);
		} else if ( that.cache[gid] === null ) {
			that.once('group'+gid, done);
		} else {
			that.cache[gid] = null;
			synConn
				.http({ uri: 'wp/{wpId}/settings/Groups/'+gid+'/' })
				.success(function(data){					
					that.cache[gid] = data;
					that.emit('group'+gid, data);
					done(data);
				})
				.error(function(err, status, headers, config){
					delete that.cache[gid];
					that.emit('group'+gid);
					done({ ID: gid, GroupName: 'GROUP DELETED' });
				});				
		}
	}

	that.resolve = function(groupsArr, done) {
		if ( typeof groupsArr === 'string' ) {
			try {
				groupsArr = JSON.parse(groupsArr);
			} catch ( e ){
				return done(e);
			}
		}
		var resolveCount = groupsArr.length,
			resolved = [];

		if ( resolveCount === 0 ) { return done(null, resolved); }

		for (var gid, i = 0; i < groupsArr.length; i++) {

			gid = (typeof groupsArr[i] === 'string') ? parseInt(groupsArr[i], 10) : groupsArr[i];

			getGroup(gid, function(data){
				resolved[this.i] = data;
				resolveCount -= 1;
				if ( resolveCount <= 0 ) {
					return done(null, resolved);
				}
			}.bind({ i: i, gid: gid }));
		}
	};

	that.getGroup = getGroup;

	return that;
});

app.factory('emAccounts', function(emAuth, synConn){
	var that = new Emmi();
	that.cache = {
		//id: {...group...}
	};

	function getAcc(aid, done) {
		var deleted = { ID: -1, AccountName: 'DELETED' };
		if ( isNaN(aid) ) {
			return done(deleted);
		}

		if ( typeof that.cache[aid] === 'object' && that.cache[aid] !== null ) {
			done(that.cache[aid]);
		} else if ( that.cache[aid] === null ) {
			that.once('group'+aid, done);
		} else {
			that.cache[aid] = null;
			synConn
				.http({ uri: 'wp/{wpId}/settings/Account/'+aid+'/' })
				.success(function(data){					
					that.cache[aid] = data;
					that.emit('group'+aid, data);
					done(data);
				})
				.error(function(err, status, headers, config){
					delete that.cache[aid];
					that.emit('group'+aid);
					return done(deleted);
				});				
		}
	}

	that.resolve = function(accsArr, done) {
		if ( typeof accsArr === 'string' ) {
			try {
				accsArr = JSON.parse(accsArr);
			} catch ( e ){
				return done(e);
			}
		}

		var resolveCount = accsArr.length,
			resolved = [];

		if ( resolveCount === 0 ) { return done(null, resolved); }

		for (var aid, i = 0; i < accsArr.length; i++) {

			aid = accsArr[i];

			getAcc(aid, function(data){
				resolved[this.i] = data;
				resolveCount -= 1;
				if ( resolveCount <= 0 ) {
					return done(null, resolved);
				}
			}.bind({ i: i, aid: aid }));
		}
	};

	return that;
});

app.factory('emUtils', function(synConn){
	var that = {};

	that.supplant = function (str, o) {
	    return str.replace(
	        /\{([^{}]*)\}/g,
	        function (a, b) {
	            var r = o[b];
	            return typeof r === 'string' || typeof r === 'number' ? r : a;
	        }
	    );
	};

	that.shrinkAssetURLs = function(src, baseURL) {

		var base = synConn.getBaseURL();
			base = base.substring(0, base.length-1);

			base += baseURL;		

		var baseLength = base.length;

		function replacer(match, p1, p2, p3, p4) {
			var url = p3;

			if ( url.match(/^\w+:\/\//) ) {
				if ( url.indexOf(base) === 0 ) {
					return p1+url.substr(baseLength)+p4;
				}				
			}
			return p1+url+p4;			
		}

		return src
				.replace(/(\b(?:src|url|background)=(\\\'|"))(.*?)(\2)/ig, replacer)
				.replace(/(\burl\(([\\\'"]{0,1}))(.*?)(\2\))/ig, replacer);
	}
	
	that.expandAssetURLs = function(src, baseURL) {

		var base = synConn.getBaseURL();
			base = base.substring(0, base.length-1);

			base += baseURL;		

		function replacer(match, p1, p2, p3, p4) {
			var url = p3;

			if ( url.match(/^\w+:\/\//) ) {
				return p1+url+p4;
			}

			if ( base[base.length-1] !== '/' && url[0] !== '/' ) { base += '/'; }

			url = base+url;
			return p1+url+p4;			
		}

		return src
				.replace(/(\b(?:src|url|background)=(\\\'|"))(.*?)(\2)/ig, replacer)
				.replace(/(\burl\(([\\\'"]{0,1}))(.*?)(\2\))/ig, replacer);
	}

	that.unpackEmailSource = function(src) {
		function uintToString(uintArray) {
		    var encodedString = String.fromCharCode.apply(null, uintArray),
		        decodedString = decodeURIComponent(escape(encodedString));
		    return decodedString;
		}

		if ( src && src[0] === 'X' ) {
			var intArr = [];
			angular.forEach(src.substr(2, src.length-3).match(/(.{1,2})/g), function(s){
				intArr.push(parseInt(s, 16));
			});
			src = uintToString(intArr);
		}

		return src;
	};

	that.timeLogToDate = function(long_datetime) {
		// - bits 0..5 = Seconds (0..59)    - 6 
		// - bits 6..11 = Minutes (0..59)   - 6
		// - bits 12..16 = Hours (0..23)    - 5
		// - bits 17..21 = Day-1 (0..31)    - 5
		// - bits 22..25 = Month-1 (0..11)  - 4
		// - bits 26..38 = Year (0..4095)   - 13

		var x = goog.math.Long.fromNumber(long_datetime);
		/*
		function TTimeLogBits.ToDateTime: TDateTime;
		begin
		if Value=0 then
		result := 0 else
		result := EncodeDate(
		(Value shr (6+6+5+5+4)) and 4095, // year
		1+(Int64Rec(Value).Lo shr (6+6+5+5)) and 15, // month
		1+(Int64Rec(Value).Lo shr (6+6+5)) and 31)+ // day
		EncodeTime(
		(Int64Rec(Value).Lo shr (6+6)) and 31, // hour
		(Int64Rec(Value).Lo shr 6) and 63,  // min
		Int64Rec(Value).Lo and 63, 0);  // sec, msec
		end;
		*/

        var d = new Date();
        d.setFullYear( ((x.high_ << 6) + (x.low_ >> (6+6+5+5+4)) ).toString(10) );
        d.setMonth(  (x.low_ >> (6+6+5+5)) & 0xf );
        d.setDate(   (x.low_ >> (6+6+5)) & 0x1f );

        d.setHours( (x.low_ >> (6+6)) & 0x1f );
        d.setMinutes((x.low_ >> 6) & 0x3f );
        d.setSeconds( x.low_ & 0x3f );
        d.setMilliseconds(0);
        return d;
        
     };
 
	that.UUID = (function(){
		return function b(a){return a?(a^Math.random()*16>>a/4).toString(16):([1e7]+-1e3+-4e3+-8e3+-1e11).replace(/[018]/g,b)};
	})();

	return that;
});

app.factory('emCategories', function($q, synConn, emAuth){
	var that = {
		categories: null,
		get: function(id) {
			var d = $q.defer();

			if ( !this.categories ) {
				getCategories(function(err, cats){
					that.categories = {};
					angular.forEach(cats, function(o){
						that.categories[o.ID] = o.Name;						
					});
					d.resolve(that.categories[id]);	
				});
			} else {
				d.resolve(that.categories[id]);
			}
			return d.promise;
		},
		getAll: function(){
			var d = $q.defer();

			if ( !that.categories ) {
				getCategories(function(err, cats){
					that.categories = {};
					angular.forEach(cats, function(o){
						that.categories[o.ID] = o.Name;						
					});
					d.resolve(that.categories);	
				});				
			} else {
				d.resolve(that.categories);
			}

			return d.promise;
		}
	};

	function getCategories(done) {
		synConn
			.http({ uri: 'wp/{wpId}/settings/TemplateCategory?Select=*' })
			.success(function(data){
				var cats = ( typeof  data.rowCount !== 'undefined' ) ? [] : data;
				done(null, cats);
			})
			.error(function(err, status, headers, config){
				done(err);
			});
	}


	return that;
});

app.directive('ckEditor', [function () {
    return {
        scope: {
        	'ckSource': '='
        },
        link: function ($scope, elm, attrs) {
			var item;

			function logError(err){
				console.error(typeof err);
			}

			// attrs.$observe('ngModel', function(value) {
			// 	console.log('ngModel has changed value to ' + value);
			// });

            var ck = CKEDITOR.replace(elm[0], {
				startupFocus : true,
				width: 'auto',
				height: 400,
				customConfig: '../../../js/custom_config.js'            	
            });

            function gotBody(body) {
            	if ( CKEDITOR.status === 'loaded' ) {
            		ck.setData(body);
            	} else {
					CKEDITOR.on( 'instanceReady', function( evt ) {
						ck.setData(body);
					});
            	}

            	ck.on('change', function(){
            		item.setBody(ck.getData());
            	});
            }

            function gotItem(it) {
            	item = it;
            	window.item = item;
				var body = it.getBody();
				if ( body.then ) {
					body.then(gotBody, logError);
				} else {
					gotBody(body);
				}
            }

			$scope.$watch('ckSource', function(src) {
				if ( src ) {
					if ( src.then ) {
						src.then(gotItem, logError);
					} else {
						gotItem(src);
					}
				}
			});


            // ck.on('pasteState', function () {
            //     $scope.$apply(function () {
            //     	$scope.ckSource = ck.getData();
            //         //ngModel.$setViewValue(ck.getData());
            //     });
            // });

            // ngModel.$render = function (value) {
            //     ck.setData(ngModel.$modelValue);
            // };
        }
    };
}]);

app.directive('emNobubble', function() {
    return function(scope, element, attrs) {
        $(element).click(function(event) {
            event.stopPropagation();
        });
    }
});

app.directive('emNodefault', function() {
    return function(scope, element, attrs) {
        $(element).click(function(event) {
            event.preventDefault();
        });
    }
});

// em-loading-bar

app
.factory('emsLoadingBar', function($rootScope, grfCreateTransport, $timeout){
	var that = new Emmi();

	that.enabled = false;

	that.currentProgress = 0;

	var iv = null;
	var dur = 2 * 60 * 1000; // 2 minutes


	// t: current time, b: begInnIng value, c: change In value, d: duration
	function easeOutExpo(t, b, c, d) {
		return (t==d) ? b+c : c * (-Math.pow(2, -10 * t/d) + 1) + b;
	}

	that.setProgress = function(p) {
		that.currentProgress = p;
		that.emit('progress', p);
	};

	that.enable = function() {
		that.enabled = true;
	};	

	function runSlowIncrementer() {
		var t = 0;		
		iv = setInterval(function() {
			t += 1000;
			if ( t >= dur ) {
				that.end();
			} else {
				var p = Math.round(easeOutExpo(t, 60, 40, dur));
				that.setProgress( p );
			}			
		}, 1000);		
	};

	that.start = function() {
		if ( !that.enabled ) { return; }
		that.setProgress(0);
		if ( iv ) {
			clearInterval(iv);
			iv = null;
		}
		// quickly filling up to 60% filling
		iv = setInterval(function(){
			that.setProgress(that.currentProgress + 10);
			if ( that.currentProgress >= 60 ) {
				clearInterval(iv);
				iv = null;
				runSlowIncrementer();
			}
		}, 200);
	};

	that.stop = function() {
		if ( !that.enabled ) { return; }
		clearInterval(iv);
		iv = null;
		that.setProgress(100);
		$timeout(function() {
			that.setProgress(0);
		}, 1000);
	};

	return that;
})
.directive('emdLoadingbar', function($rootScope, emsLoadingBar) {

	window.emsLB = emsLoadingBar;

	return {
		retrict: 'AE',
		template: '<div class="em-loading-bar"></div>',
		replace: true,
		link: function($scope, el, attrs, controller) {
			emsLoadingBar.on('progress', function(p){

				function move() {
					var st = el[0].style,
						s = 'translate3d('+( p-100 )+'%, 0, 0)';

					if ( typeof st.webkitTransform !== 'undefined' ) st.webkitTransform = s;
					if ( typeof st.MozTransform !== 'undefined' ) st.MozTransform = s;
					if ( typeof st.transform !== 'undefined' ) st.transform = s;					
				}

				if ( p == 0 ) {
					el.hide();
					setTimeout(function(){
						move();
						el.show();
					}, 1)
				} else {
					move();
				}
			});
		}
	};
});


app.filter('capitalize', function() {
    return function(input, scope) {

        return (typeof input !== 'string') ? input : input.substring(0,1).toUpperCase()+input.substring(1);
    }
});
