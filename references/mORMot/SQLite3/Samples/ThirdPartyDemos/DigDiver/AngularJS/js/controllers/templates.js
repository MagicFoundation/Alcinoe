function TemplatesCtrl($scope, $http, $location, $stateParams, $state, EmTemplates, emAuth, synConn, emUtils) {

	var sub = $stateParams.folder || $scope.subSection;

	$scope.setSub(sub);
	$scope.templates = new EmTemplates($scope, sub);

	function expandAssetURLs(src, baseURL) {

		function replacer(match, p1, p2, p3, p4) {
			var url = p3;

			if ( url.match(/^\w+:\/\//) ) {
				return p1+url+p4;
			}

			if ( baseURL[baseURL.length-1] !== '/' && url[0] !== '/' ) { baseURL += '/'; }

			url = baseURL+url;
			return p1+url+p4;			
		}

		return src
				.replace(/(\b(?:src|url|background)=(\\\'|"))(.*?)(\2)/ig, replacer)
				.replace(/(\burl\(([\\\'"]{0,1}))(.*?)(\2\))/ig, replacer);
	}

	$scope.tplClick = function(tpl) {
		$scope.templates.activeItem = tpl.ID;
		$scope.item = tpl;

		var tpl = $scope.templates.getTemplate(tpl.ID);
		openPreview(tpl);
	};

	$scope.tplDelete = function(e, tpl) {
		e.stopPropagation();
		e.preventDefault();
		var delfromTrash = $scope.subSection === 'trash';
		if ( delfromTrash && !confirm('Are you sure you want to delete this template?') ) { return; }
		tpl.del(function(err){
			if ( err ) { console.log(err); return; }
			$scope.templates.del(tpl);
		}, delfromTrash);
	};

	$scope.tplRestore = function(e, tpl) {
		e.stopPropagation();
		e.preventDefault();
		tpl.restore(function(err){
			if ( err ) { console.log(err); return; }
			$scope.templates.del(tpl);
		});
	};	

	$scope.tplCreateMsg = function(e, tpl) {
		e.preventDefault();
		e.stopPropagation();

		synConn
			.http({
				method: 'PUT',
				uri: 'wp/{wpId}/settings/NewMessageFromTemplate?ID='+tpl.ID
			})
			.success(function(data){
				$location.path('/workplaces/'+emAuth.get('workplace').ID+'/drafts/'+data.ID+'/edit');
			})
			.error(function(err){
				console.error(err);
			});
	};

	$scope.startSending = function($scope, $http) {
		$http({ method: 'GET', url: '' })
	};

	$scope.setPreview = function(){

		$http({ method: 'GET', url: 'http://blog.dev/wp-admin/admin.php?page=newsman-templates&action=edit&id=16&tpl-source=1' })
			.success(function(data, status, headers, config) {

				var zoomStyle = ['<style>',
					'html { ',
						'-ms-zoom: 0.65;',
						'-moz-transform:scale(0.65);',
						'-moz-tansform-origin: 0 0;',
						'-o-transform: scale(0.65);',
						'-o-transform-origin: 0 0;',
						'-webkit-transform: scale(0.65);',
						'-webkit-transform-origin: 0 0;',
					'}</style>'].join('');

				data = data.replace('</head>', zoomStyle+'</head>');
				openPreview(data);
			// this callback will be called asynchronously
			// when the response is available
			})
			.error(function(data, status, headers, config) {
			// called asynchronously if an error occurs
			// or server returns response with an error status.
			});
	};
	
}

app.controller('TemplateEditorCtrl', function($scope, $controller, $modal, $stateParams, emAuth, synConn, emUtils, emCategories, EmTemplates, groups){
	$controller('CommonEditorController', { $scope: $scope });

	$scope.stateParams = $stateParams;

	var gotScopeItem;

	$scope.itemType = '';

	$scope.groups = groups;
	$scope.to = [];

	$scope.$watch('item', function(newVal){
		if ( typeof newVal !== 'undefined' && !newVal.then ) {
			$scope.to = $scope.item.GroupList;
		}
	});

	$scope.$watch('to',	function(newVal){
		if ( $scope.item ) {
			$scope.item.GroupList = newVal
		}
	});

	$scope.enableTracking = false;

	$scope.$watch('item', function(item){
		if ( typeof item !== 'undefined' && !item.then ) {
			$scope.enableTracking = item.TrackMethod > 0;
		}
	});

	if ( $stateParams.template ) {
		$scope.itemType = 'template';

		$scope.setItem( (new EmTemplates()).getTemplate($stateParams.template) );

		var p = { root: 'templates' };

		gotScopeItem = function(tpl) {
			//p.name = drf.Subject;
			p.item = tpl;
			$scope.setItemParams(p);
			//$scope.item = tpl;
			$scope.setItem(tpl);
			window.tpl = tpl;
		};
	}

	// ----------

	if ( $scope.item.then ) {
		$scope.item.then(
			gotScopeItem,
			function(err){
				console.error(err);
			}
		);
	} else {
		gotScopeItem($scope.item);
	}	


	$scope.save = function() {
		$scope.item.save(function(err){
			$scope.alert({
				type:'success',
				msg: 'Successfully saved'
			});
		});
	};

});

var TemplateEditorResolve = {
	groups: function($q, synConn) {
		var defer = $q.defer();

		synConn
			.http({
				uri: 'wp/{wpId}/settings/Groups?Select=ID,GroupName,GroupKind&WHERE=Hidden=0'
			})
			.success(function(data){
				angular.forEach(data, function(g){
					g.type = (g.GroupKind == 1) ? 'My Groups' : 'My Databases';
				});
				defer.resolve(data);
			})
			.error(function(err){
				console.error(err);
				defer.reject(err);
			});

		return defer.promise;		
	}
};

app.factory('EmTpl', function($http, $q, synConn, emUtils, emAuth){
	var EmTpl = function(data){

		var defaults = {
			AttachList: "[]",
			CategoryID: 0,
			CharSet: "UTF-8",
			GroupList: [],
			Guid: "{"+emUtils.UUID()+"}",
			//ID: 17,
			MessageFormat: 0,
			ModTime: (new Date()).getTime(),
			//StoreID: 6,
			Subject: "No Subject",
			TemplateName: "Unnamed",
		};

		angular.extend(this, defaults);
		if ( typeof data !== 'undefined' ) {
			angular.extend(this, data);
		}
		this.ModTime = emUtils.timeLogToDate(this.ModTime);

		this.GroupList = this._map(this.GroupList, function(itm){ return (typeof itm === 'string') ? parseInt(itm, 10) : itm; });
	};
	var p = EmTpl.prototype;

	p.getBaseURL = function() {
		return  '/wps/templates/'+emAuth.get('workplace').ID+'/'+this.Guid+'/';
	};

	p._map = function(arr, mapper) {
		for (var i = 0; i < arr.length; i++) {
			arr[i] = mapper(arr[i], i);
		}
		return arr;
	};	

	p.save = function(done) {
		done = done || function(){};

		var obj = {
			ID: this.ID,
  			CharSet : 'UTF-8',
  			CategoryID: 0,
  			TemplateName: this.TemplateName,
  			Subject: this.Subject,
  			MessageFormat: 0, // 0 - html, 1 - plain  
			GroupList: this._map(this.GroupList, function(itm){ return itm+''; }), // ~
			AttachList: this.AttachList,
  			Guid: item.Guid
		};

		if ( typeof this._data !== 'undefined' ) {
			obj.EmailSource = emUtils.shrinkAssetURLs(this.EmailSource, this.getBaseURL());
		}

		synConn
			.http({
				method: 'PUT',
				uri: 'wp/{wpId}/settings/SetTemplateMessage',
				data: obj
			})
			.success(function(data){
				done(null);
			})
			.error(function(err){
				console.error(err);				
				done(err);
			});
	};

	p.setBody = function(body) {
		this.EmailSource = body;
	}

	p.getBody = function() {
		var _this = this;
		if ( typeof this._data !== 'undefined' ) {
			return this.EmailSource;
		}

		var defer = $q.defer();

		synConn
			.http({ uri: 'wp/{wpId}/settings/GetTemplateMessage?ID='+this.ID })
			.success(function(data){
				_this._data = data;
				_this.Guid = data.Guid;
				data.EmailSource = emUtils.unpackEmailSource(data.EmailSource);
				_this.EmailSource = emUtils.expandAssetURLs(data.EmailSource, _this.getBaseURL());
				defer.resolve(_this.EmailSource);
			})
			.error(function(err){
				defer.reject(err);
			});

		return defer.promise;
	};

	p.del = function(done, forever) {
		var req;
		if ( forever ) {
			req = {
				method: 'DELETE',
				uri: 'wp/{wpId}/settings/templates/'+this.ID
			};
		} else {
			req = {
				method: 'PUT',
				uri: 'wp/{wpId}/settings/templates/'+this.ID,
				data: {
					CategoryID: -1
				}				
			};
		}

		synConn
			.http(req)
			.success(function(data){
				done(null);
			})
			.error(function(err){
				console.error(err);
				done(err);
			});
	}

	p.restore = function(done) {
		synConn
			.http({
				method: 'PUT',
				uri: 'wp/{wpId}/settings/templates/'+this.ID,
				data: {
					CategoryID: 0
				}				
			})
			.success(function(data){
				done(null);
			})
			.error(function(err){
				console.error(err);
				done(err);
			});
	};

	return EmTpl;
});

app.factory('EmTemplates', function($http, $q, emAuth, synConn, emGroups, emAccounts, EmTpl) {
	var EmTemplates = function($scope, cat) {
		this.$scope = $scope;
		this.items = [];
		this.busy = false;
		this.noMoreData = false;
		this.after = '';
		this.category = cat ? cat : 0;

		if ( this.category === 'trash' ) {
			this.category = -1;
		}
	};

	function resolveGroups(templates, done) {
		var templatesToProcess = templates.length;

		for (var i = 0; i < templates.length; i++) {
			emGroups.resolve(templates[i].GroupList, function(err, resolved){
				if (err) { throw err; } 
				templates[this.i].GroupList = resolved;
					templatesToProcess -= 1;
					if ( templatesToProcess === 0 ) {
						done(templates);
					}
			}.bind({ i: i }));
		}
	}

	EmTemplates.prototype.getTemplate = function(id) {
		
		var tpl;

		if ( this.items ) {
			angular.forEach(this.items, function(t, i){
				if ( t.ID == id ) {
					tpl = t;
					return false;
				}
			});
			if ( tpl ) {
				return tpl;
			}
		}

		var defer = $q.defer();
		synConn
			.http({ uri: 'wp/{wpId}/settings/GetTemplateMessage?ID='+id })
			.success(function(data){
				defer.resolve(new EmTpl(data));
			})
			.error(function(err){
				defer.reject(err);
			});
		return defer.promise;
	};

	EmTemplates.prototype.nextPage = function() {
		var that = this,
			defer = $q.defer();

		if ( that.busy ) return;
		that.busy = true;

		var sql = '*';
		var start = this.items.length;

		synConn
			.http({ uri: 'wp/{wpId}/settings/Templates?Select=*&where=CategoryID='+that.category+'&STARTINDEX='+start+'&RESULTS=10&SORT=ModTime&DIR=DESC' })
			.success(function(data){				
				if ( typeof data.fieldCount !== 'undefined' ) {
					// no more templates
					that.busy = false;
					that.noMoreData = true;
				} else {
					resolveGroups(data, function(templates){
						for (var i = 0; i < templates.length; i++) {
							that.items.push(new EmTpl(templates[i]));
						}
						that.busy = false;
						defer.resolve(data);
					});
				}

			})
			.error(function(err){
				defer.reject(err);
				console.error(err)
			});
		return defer.promise;
  	};

  	EmTemplates.prototype.del = function(tpl) {
		for (var i = 0; i < this.items.length; i++) {
			if ( this.items[i].id == tpl.id ) {
				this.items.splice(i, 1);
				break;
			}
		}
  	};

  return EmTemplates;
});