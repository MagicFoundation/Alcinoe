app.controller('ContactsCtrl', function($scope, $stateParams, EmContacts, EmContactsEL){
	if ( $stateParams.gid ) {
		$scope.contacts = new EmContacts({
			groupId: parseInt($stateParams.gid, 10),
			placement: $stateParams.placement
		});		
	}

	if ( $stateParams.gid ) {
		$scope.contactsExcList = new EmContactsEL({
			groupId: parseInt($stateParams.gid, 10)
		});		
	}

	if ( $scope.contacts ) { 
		$scope.contacts.nextPage();
	}
	if ( $scope.contactsExcList ) {
		$scope.contactsExcList.nextPage();	
	}	
});

app.factory('EmContacts', function($http, $q, emAuth, synConn, emGroups, emAccounts, EmDrf) {
	var EmContacts = function(opts) {
		opts = opts || {};
		this.items = [];
		this.busy = false;
		this.after = '';
		this.options = {
			groupId: null
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

	function parseFields(fields) {
		if ( typeof fields === 'string' ) {
			try {
				fields = JSON.parse(fields);	
			} catch(e) {
				return fields;
			}
		}
		var res = {};
		angular.forEach(fields, function(fd){
			res[fd.Name] = fd.Value;
		});
		return res;
	}

	function reformatResponseObj(o) {
		var res = {
			Fields: {}
		};
		var directMappingFields = [
			'Email',
			'First_Name',
			'Last_Name',
			'Recipient_Name',
			'Subscribed',
			'Subscribe_Date'
		];

		function createRX(fieldName) {
			var rxStr = '^'+fieldName.replace(/W+/, '.*?').toLowerCase()+'$';
			return new RegExp(rxStr, 'i');
		}

		angular.forEach(directMappingFields, function(dmf){
			var rx = createRX(dmf);
			for ( var p in o ) {
				if ( rx.exec(p) ) {
					res[dmf] = o[p];
					delete o[p];
					break;
				}
			}
		});

		res.Fields = o;

		return res;	
	}

	EmContacts.prototype.getExclusionList = function(done) {
		done = done || function(){};

		if ( this._exclusionList ) {
			return done(null, this._exclusionList);
		} else {
			var start = 0;
			var uri = 'wp/{wpId}/group/ExclusionList?SELECT=*&WHERE=GroupID='+this.options.groupId+'&STARTINDEX='+start+'&RESULTS=25';

			//var uri = 'Select Email, Date_Added from ExclusionList WHERE GROUPID=ID LIMIT 25 OFFSET 0"';

			synConn
				.http({ uri: uri })
				.success(function(data){
					done(null, data);
				})
				.error(function(err){
					console.error(err);
					done(err);
				});
		}
	};

	EmContacts.prototype.nextPage = function() {
		var that = this,
			defer = $q.defer();

		if ( that.busy ) return;
		that.busy = true;

		var start = this.items.length;

		var groupWhere = (typeof this.options.groupId !== 'undefined' && this.options.groupId !== null)
							? '&where=GroupID='+this.options.groupId 
							: '';
		var uri,
			isLocalGroup = this.options.placement == 'local';

		if ( groupWhere ) {
			this.getExclusionList();
		}

		if ( isLocalGroup ) {
			uri = 'wp/{wpId}/group/Emails?SELECT=*'+groupWhere+'&STARTINDEX='+start+'&RESULTS=25';
		} else { // remote
			uri = 'wp/{wpId}/settings/ConnectToDB?ID='+this.options.groupId+'&STARTINDEX='+start+'&RESULTS=25';
		}

		synConn
			.http({ uri: uri })
			.success(function(data){

				// console.warn('Got data: '+data.length);
				// console.log(data);

				// no more emails
				if ( typeof data.fieldCount !== 'undefined' ) {
					that.busy = false;
					that.noMoreData = true;
				} else {
					for (var i = 0; i < data.length; i++) {
						if ( isLocalGroup ) {
							if ( data[i].Fields ) {
								data[i].Fields = parseFields(data[i].Fields);
							}
						} else { // remote group
							data[i] = reformatResponseObj(data[i]);
						}
						that.items.push(data[i]);
					}
					that.busy = false;
					defer.resolve();
				}
			})
			.error(function(err){
				defer.reject(err);
				console.error(err)
			});
		return defer.promise;
	};

  return EmContacts;
});

app.factory('EmContactsEL', function($http, $q, emAuth, synConn, emGroups, emAccounts, EmDrf) {
	var EmContactsEL = function(opts) {
		opts = opts || {};
		this.items = [];
		this.busy = false;
		this.after = '';
		this.options = {
			groupId: null
		};

		this.noMoreData = false;

		angular.extend(this.options, opts);
	};

	EmContactsEL.prototype.nextPage = function() {
		var that = this,
			defer = $q.defer();

		if ( that.busy ) return;
		that.busy = true;

		var start = this.items.length;
		var uri = 'wp/{wpId}/group/ExclusionList?SELECT=*&WHERE=GroupID='+this.options.groupId+'&STARTINDEX='+start+'&RESULTS=25';

		synConn
			.http({ uri: uri })
			.success(function(data){

				// console.warn('Got data: '+data.length);
				// console.log(data);

				// no more items
				if ( typeof data.fieldCount !== 'undefined' ) {
					that.busy = false;
					that.noMoreData = true;
				} else {
					for (var i = 0; i < data.length; i++) {
						that.items.push(data[i]);
					}
					that.busy = false;
					defer.resolve();
				}
			})
			.error(function(err){
				console.error(err);
				defer.reject(err);
			});
		return defer.promise;
	};

  return EmContactsEL;
});

app.controller('ContactsSidebarMenuCtrl', function($scope, $stateParams, groups){
	var sortedGroups = {
		local: [],
		db: []
	};

	$scope.wpid = $stateParams.wpid;

	angular.forEach(groups, function(g){
		sortedGroups[ (g.GroupKind == 1) ? 'local':'db' ].push(g);
	});

	$scope.groups = sortedGroups;
});

var ContactsSidebarMenuCtrlResolve = {
	groups: function($q, synConn) {
		var defer = $q.defer();

		synConn
			.http({
				uri: 'wp/{wpId}/settings/Groups?Select=ID,GroupName,GroupKind&WHERE=Hidden=0'
			})
			.success(function(data){
				angular.forEach(data, function(g){
					console.log(g.GroupKind);
					switch ( g.GroupKind ) {
						case 1: // internal
							g.type = 'My Groups';
							g.placement = 'local';
							break;						
						case 0: // external
						case 2: //wpnewsman
							g.type = 'My Databases';
							g.placement = 'remote';
							break;
					}
					
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
