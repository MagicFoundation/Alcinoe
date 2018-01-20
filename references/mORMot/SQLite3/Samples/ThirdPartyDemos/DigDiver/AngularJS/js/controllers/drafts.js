function DraftsCtrl($scope, $http, $stateParams, EmDrafts, emAuth, synConn, emUtils) {

	$scope.drafts = new EmDrafts({ showDeleted: $scope.subSection === 'trash' });

	$scope.drfClick = function(drf) {
		$scope.drafts.activeItem = drf.ID;
		$scope.setEmail(drf);

		openPreview(drf);
	};

	$scope.drfDelete = function(ev, drf){
		ev.preventDefault();
		ev.stopPropagation();
		var delFromTrash = ($scope.subSection === 'trash');
		if ( delFromTrash && !confirm('Are sure you want to delete this draft?') ) { return; }
		drf.del(function(err){
			if ( !err ) {
				$scope.drafts.del(drf);	
			}			
		}, delFromTrash);
	};

	$scope.drfRestore = function(ev, drf){
		ev.preventDefault();
		ev.stopPropagation();
		drf.restore(function(err){
			if ( !err ) {
				$scope.drafts.del(drf);
			}			
		});
	};	
}

app.controller('DraftEditorCtrl', function($scope, $controller, $modal, $stateParams, emAuth, synConn, emUtils, emCategories, EmTemplates, EmDrafts, accounts, groups){
	$controller('CommonEditorController', { $scope: $scope });

	$scope.stateParams = $stateParams;

	var gotScopeItem;

	$scope.itemType = '';
	$scope.accounts = accounts;
	$scope.account = [];

	$scope.$watch('account', function(newVal){
		if ( $scope.item ) {
			var nv = parseInt(newVal, 10);
			$scope.item.AccountList = !isNaN(nv) ? [nv] : [];
		}		
	});

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

	if ( $stateParams.draft ) {
		$scope.itemType = 'draft';

		$scope.setItem( (new EmDrafts()).getDraft($stateParams.draft) );

		var p = { root: 'drafts' };

		gotScopeItem = function(drf) {
			//p.name = drf.Subject;
			p.item = drf;
			$scope.setItemParams(p);
			//$scope.item = drf;
			$scope.setItem(drf);

			$scope.account = drf.AccountList[0];
			window.drf = drf;
		};
	}

	// ----------

	if ( $scope.item.then ) {
		$scope.item.then(
			gotScopeItem,
			function(err){
				console.error(err+'');
			}
		);
	} else {
		gotScopeItem($scope.item);
	}	


	$scope.save = function() {
		$scope.item.save(function(err){
			console.log(arguments);
			if ( !err ) {
				$scope.alert({ type:'success', msg: 'Successfully saved' });
			} else {
				$scope.alert({ type:'danger', msg: err+'' });
			}			
		});
	};

	$scope.sendNow = function(){
		$scope.item.save(function(err){
			if ( !err ) {
				$scope.item.startSending(function(err, outboxEmlID){
					if ( !err ) {
						window.location.hash = '#/workplaces/'+emAuth.get('workplace').ID+'/outbox/'+outboxEmlID+'/preview';
					} else {
						$scope.alert({
							type:'danger',
							msg: err+''
						});						
					}
				});	
			} else {
				$scope.alert({
					type:'danger',
					msg: 'Error while saving draft: '+err
				});				
			}			
		});			
	};

	$scope.removeAttachment = function(fileName) {
		$scope.item.removeAttachment(fileName, function(err){
			if ( err ) { return console.error(err); }
			for (var i = $scope.item.AttachList.length - 1; i >= 0; i--) {
				if ( $scope.item.AttachList[i] == fileName ) {
					$scope.item.AttachList.splice(i, 1);
					break;
				}
			}
			$scope.item.save();
		});
	};
});

var DraftEditorResolve = {
	accounts: function($q, synConn) {
		var defer = $q.defer();

		synConn
			.http({
				uri: 'wp/{wpId}/settings/Account?Select=AccountName,ID'
			})
			.success(function(data){
				defer.resolve(data);
			})
			.error(function(err){
				console.error(err);
				defer.reject(err);
			});

		return defer.promise;
	},
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

app.factory('EmDrf', function($http, $q, synConn, emUtils, emAuth){
	var EmDrf = function(data){
		var defaults = {
			AccountList: [],
			PersonalAttachList: [],
			CID: 0,
			CName: "",
			CharSet: "UTF-8",
			ConfirmRead: 0,
			Deleted: 0,
			GroupList: [],
			Guid: '{'+emUtils.UUID()+'}',
			ID: 0,
			MailTo: "",
			MessageFormat: 0, // 1 - html, 0 - plain
			ModTime: (new Date()).getTime(),
			Priority: 3,
			Subject: "Please Enter Subject Here",
			TrackMethod: 0,
			TrackName: "",
			UseAnalytics: 0,
		};

		angular.extend(this, defaults);

		if ( typeof data !== 'undefined' ) {
			angular.extend(this, data);
		}

		this.ModTime = emUtils.timeLogToDate(this.ModTime);

		this.GroupList = this._map(this.GroupList, function(itm){ return parseInt(itm, 10); });
		this.AccountList = this._map(this.AccountList, function(itm){ return parseInt(itm, 10); });
	};
	var p = EmDrf.prototype;

	p.getBaseURL = function() {
		return '/wps/drafts/'+emAuth.get('workplace').ID+'/'+this.Guid+'/';
	};

	p._map = function(arr, mapper) {
		for (var i = 0; i < arr.length; i++) {
			arr[i] = mapper(arr[i], i);
		}
		return arr;
	};

	p.del = function(done, forever) {
		var req;
		if ( forever ) {
			req = {
				method: 'DELETE',
				uri: 'wp/{wpId}/settings/drafts/'+this.ID
			};
		} else {
			req = {
				method: 'PUT',
				uri: 'wp/{wpId}/settings/drafts/'+this.ID,
				data: {
					deleted: 1
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
				uri: 'wp/{wpId}/settings/drafts/'+this.ID,
				data: {
					deleted: 0
				}				
			})
			.success(function(data){
				done(null);
			})
			.error(function(err){
				console.error(err);
				done(err);
			});
	}	

	p.save = function(done) {
		var _this = this;
		done = done || function(){};

		var obj = {
			'ID': this.ID,
			'CharSet': 'UTF-8',
			'Subject': this.Subject,

			'MessageFormat': 0,  // 0 - html, 1 - plain
			'Guid': this.Guid,
			'EmailSource': emUtils.shrinkAssetURLs(this.EmailSource, this.getBaseURL()),
			'Name': this.TemplateName,

			'TrackMethod': parseInt(this.TrackMethod, 10), // - numeric, values (0: not used; 1: google; 2: piwik)
			'TrackName': this.TrackName, //  - string, tracking campaign name			
			
			'AccountList': this._map(this.AccountList, function(itm){ return itm+''; }), // converting array of nums into arr of strings
			'GroupList': this._map(this.GroupList, function(itm){ return itm+''; }), // ~
			'AttachList': this.AttachList,
			'PersonalAttachList': this.PersonalAttachList
		};

		synConn
			.http({
				method: 'POST',
				uri: 'wp/{wpId}/settings/SetDraftMessage',
				data: obj
			})
			.success(function(data){
				_this.ID = data.ID;
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
			.http({ uri: 'wp/{wpId}/settings/GetDraftMessage?ID='+this.ID })
			.success(function(data){
				_this._data = data;

				data.EmailSource = emUtils.unpackEmailSource(data.EmailSource);

				//data.BaseURL = emUtils.supplant('/wps/drafts/{wpid}/{guid}/',{ guid: data.Guid, wpid: emAuth.get('workplace').ID });
				_this.EmailSource = emUtils.expandAssetURLs(data.EmailSource, _this.getBaseURL());
				defer.resolve(_this.EmailSource);
			})
			.error(function(err){
				console.error(err);
				defer.reject(err);
			});

		return defer.promise;
	};

	p.startSending = function(done) {
		done = done || function(){};
		synConn
			.http({ uri: 'wp/{wpId}/settings/SendDraftMessage?ID='+this.ID })
			.success(function(data){

				console.warn('SendDraftMessage');
				console.log(data);
				done(null, data.ID);

			})
			.error(function(err){
				console.error(err);
				done(err);
			});		
		// SendDraftMessage
		// param - ID
		// StopSending - ID
		// SendMessage - ID
	};

	p.removeAttachment = function(fileName, done) {
		synConn
			.http({ uri: 'wp/{wpId}/settings/RemoveContent?GUID='+this.Guid+'&type=drafts&Folder=attachment&FileName='+fileName })
			.success(function(data){
				done(null);
			})
			.error(function(err){
				done(err);
			});
	};

	p.addAttachment = function(fileName) {
		var idx = this.AttachList.indexOf(fileName);
		if ( idx === -1 ) {
			this.AttachList.push(fileName);
			this.save();
		}
	}

	return EmDrf;
});

app.factory('EmDrafts', function($http, $q, emAuth, synConn, emGroups, emAccounts, EmDrf) {
	var EmDrafts = function(opts) {
		opts = opts || {};
		this.items = [];
		this.busy = false;
		this.after = '';
		this.options = {
			showDeleted: false
		};

		this.noMoreData = false;

		angular.extend(this.options, opts);
	};

	function resolveGroups(drafts, done) {
		var draftsToProcess = drafts.length;
		
		for (var i = 0; i < drafts.length; i++) {			
			emGroups.resolve(drafts[i].GroupList, function(err, resolved){
				if (err) { 
					return console.log(err);
				} 
				try {
					drafts[this.i].GroupList = JSON.parse(drafts[this.i].GroupList);
				} catch (e) {
					drafts[this.i].GroupList = [];
				}
				drafts[this.i].GroupListResolved = resolved;
				//drafts[this.i].GroupList = resolved;

				emAccounts.resolve(drafts[this.i].AccountList, function(err, resolvedAccounts){
					if (err) { return console.log(err); } 

					try {
						drafts[this.i].AccountList = JSON.parse(drafts[this.i].AccountList) || [];
					} catch (e) {
						drafts[this.i].AccountList = [];
					}
					drafts[this.i].AccountListResolved = resolvedAccounts;
					//drafts[this.i].AccountList = resolvedAccounts;

					draftsToProcess -= 1;
					if ( draftsToProcess === 0 ) {
						done(drafts);
					}
				}.bind({ i: this.i }));
			}.bind({ i: i }));
		}
	}

	EmDrafts.prototype.nextPage = function() {
		var that = this,
			defer = $q.defer();

		if ( that.busy ) return;
		that.busy = true;

		//var sql = 'ID, Subject, MailTo, Total, Processed, Sent, Excluded, Failed, Status, StatusMessage, CreateTime, UseAnalytics, CID, AccountList, GroupList, Deleted, Log From Outbox Order by CreateTime DESC LIMIT 10';
		var sql = '*';		

		var start = this.items.length;

		synConn
			.http({ uri: 'wp/{wpId}/settings/Drafts?Select=*&where=deleted='+(this.options.showDeleted ? '1' : '0')+'&STARTINDEX='+start+'&RESULTS=10&SORT=ModTime&DIR=DESC' })
			.success(function(data){

				// no more emails
				if ( typeof data.fieldCount !== 'undefined' ) {
					that.busy = false;
					that.noMoreData = true;
				} else {
					resolveGroups(data, function(drafts){
						for (var i = 0; i < drafts.length; i++) {
							that.items.push(new EmDrf(drafts[i]));
						}
						that.busy = false;
						defer.resolve();
					});
				}
			})
			.error(function(err){
				defer.reject(err);
				console.error(err)
			});
		return defer.promise;
	};

  	EmDrafts.prototype.getDraft = function(id) {
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
			//.http({ uri: 'wp/{wpId}/settings/Drafts?Select=*&where=ID='+id+'&RESULTS=1' })
			.http({ uri: 'wp/{wpId}/settings/GetDraftMessage?ID='+id })
			.success(function(data){
				defer.resolve( new EmDrf(data) );
			})
			.error(function(err){
				defer.reject(err);
			});
		return defer.promise;
	};

	EmDrafts.prototype.createDraft = function(done) {
		var _this = this;
		synConn
			.http({
				method: 'GET',
				uri: 'wp/{wpId}/settings/CreateNewMessage'
			})
			.success(function(draft){
				console.log('createDraft results:');
				console.log(draft);
				var drf = new EmDrf(draft)
				_this.items.push(drf);
				done(drf);
			})
			.error(function(err){
				console.error(err);
				done(err);
			});
	};

	EmDrafts.prototype.del = function(drf) {
		for (var i = 0; i < this.items.length; i++) {
			if ( this.items[i] === drf ) {
				this.items.splice(i, 1);
				break;
			}
		}
	};

  return EmDrafts;
});

