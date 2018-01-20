/*
	Setn log
	/wp/1/settings/GetSentItems?MessageID=6&SQL=Select+*+from+SentItems&LIMIT=10

	Plain Log
	/wp/1/settings/Outbox/6/Log

*/

function EmailsCtrl($scope, $http, $stateParams, EmEmails, emAuth, synConn, emUtils, EmSentLog) {

	$scope.emails = new EmEmails($scope, {
		showDeleted: $scope.subSection === 'trash'
	});

	$scope.log = null;

	$scope.activateEmail = function(eml) {
		$scope.log = null;

		$scope.emails.activeItem = eml.ID;
		$scope.setEmail(eml);

		if ( $scope.item && $scope.item.stopPolling ) {
			$scope.item.stopPolling();
		}

		$scope.setItem(eml);

		if ( eml.Status != 2 && eml.startPolling ) {
			eml.startPolling();
		}
		openPreview(eml);				
	};

	$scope.emlClick = function(eml) {
		window.location.hash = '#/workplaces/'+$scope.wpid+'/outbox/'+eml.ID+'/preview';
	};

	$scope.startSending = function($scope, $http) {
		$http({ method: 'GET', url: '' })
	};

	$scope.setPreview = function(){
		$scope.log = null;

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

	$scope.viewLog = function(emailId) {
		$scope.log = null;

		var htmlWrapper = [
			'<html><head><style>',
				'pre {',
					'font-size: 12px;',
					'font-family: sans-serif;',
				'}',
			'</style></head><body><pre>{body}</pre></body></html>'].join('');

		synConn
			.http({
				method: 'GET',
				uri: 'wp/{wpId}/settings/Outbox/'+emailId+'/Log'
			})
			.success(function(data){				
				setIFrameContent(emUtils.supplant(htmlWrapper, { body: data }));
			})
			.error(function(err){
				console.error(err);
			});
	}

	$scope.emDelete = function(eml) {
		var delFromTrash = $scope.subSection === 'trash';

		if ( delFromTrash && !confirm('Are you sure you want to delete this email?') ) { return; }

		eml.del(function(err){
			if ( err ) { console.log(err); return; }
			$scope.emails.del(eml);
		}, delFromTrash);
	};

	$scope.viewSentLog = function(emailId, type) {
		$scope.log = new EmSentLog($scope, emailId, type);
		$scope.log.nextPage();
	}
}

app.controller('EmailsItemDetailsCtrl', function($scope, $stateParams){

	var subView = ( typeof $stateParams.subview === 'undefined' ) ? 'preview' : $stateParams.subview;

	switch ( subView ) {
		case 'preview':
			$scope.emails.getEmail($stateParams.id, function(err, eml){
				if ( err ) { return console.error(err);	}
				$scope.activateEmail(eml);
			});
			break;

		case 'log':
			$scope.viewLog($stateParams.id);
			break;

		case 'processed':
			$scope.viewSentLog($stateParams.id, 'processed');
			break;

		case 'sent':
			$scope.viewSentLog($stateParams.id, 'sent');
			break;

		case 'failed':
			$scope.viewSentLog($stateParams.id, 'failed');
			break;
	}

});

app.factory('EmEml', function($http, $q, synConn, emUtils, emAuth, $timeout, $interval){

	var EmEml = function(data){
		var defaults = {
			AccountList: [],
			AttachList: [],
			Attempt: 0,
			Bounced: 0,
			CID: 0,
			CName: "",
			CharSet: "WINDOWS-1252",
			Complaint: 0,
			ConfirmRead: 0,
			CreateTime: 135125718208,
			Deleted: 0,
			Excluded: 0,
			Failed: 0,
			FinishTime: "",
			GroupList: [],
			Guid: '{'+emUtils.UUID()+'}',
			ID: 0,
			MailTo: "",
			MessageFormat: 0,
			Priority: 3,
			Processed: 2,
			ResumeTime: "",
			SendMode: 0,
			Sent: 2,
			StartTime: "",
			Status: 2, // 0- RedyForSent; 1 - InSent; 2- Sending Finished; 3 - Pending
			StatusMessage: "Finished [View Log]",
			Subject: "Pure Papers",
			Total: 0,
			Total_Clicks: 0,
			Total_Opens: 0,
			TrackMethod: 0,
			TrackName: "",
			Unique_Clicks: 0,
			Unique_Opens: 0,
			Unsubscribed: 0,
			UseAnalytics: 0,
			User: null			
		};

		angular.extend(this, defaults);
		if ( typeof data !== 'undefined' ) {
			angular.extend(this, data);
		}

		this.CreateTime = emUtils.timeLogToDate(this.CreateTime);
	};

	EmEml.status = {
		READY_FOR_SENT: 0,
		SENDING: 1,
		FINISHED: 2,
		PENDING: 3
		//0: RedyForSent; 1 - InSent; 2- Sending Finished; 3 - Pending
	};

	EmEml.canBeSent = function(o) {
		var st = (typeof o === 'number') ? o : o.Status;
		var x =
			st == EmEml.status.FINISHED ||
			st == EmEml.status.READY_FOR_SENT;

		return x;
	};

	EmEml.canBeStopped = function(o) {
		var st = (typeof o === 'number') ? o : o.Status;
		var x =
			st == EmEml.status.PENDING ||
			st == EmEml.status.SENDING;
		return x;
	};

	var p = EmEml.prototype;

	p.getBaseURL = function() {
		return  '/wps/outbox/'+emAuth.get('workplace').ID+'/'+this.Guid+'/';
	};	

	p.save = function(done) {
		var _this = this;
		done = done || function(){};

		/*
		'/wp/1/settings/Drafts?session_signature=004888DF0000838811DF13D9',
		'POST',

		'Content-Type: application/json; charset=UTF-8'

		{
			"CharSet":"UTF-8",
			"Subject":"Test",
			"MailTo":"",
			"ModTime":135135803300,"UseAnalytics":false,"CID":0,"CName":"",
			"TrackMethod":0,"TrackName":"",
			"Priority":1,
			"ConfirmRead":false,"Deleted":false,"MessageFormat":0,"AttachList":"[]",
			"AccountList":"[\"1\"]",
			"GroupList":"[\"1\"]",
			"Guid":""
		}

		//*/

		//'id', 'MessageFormat', 'Guid', 'EmailSource', 'Name', 'Subject', 'AccountList', 'GroupList'		

		var obj = {
			'id': this.ID,
			'MessageFormat': 0,  // 0 - html, 1 - plain
			'Guid': item.Guid,
			'EmailSource': emUtils.shrinkAssetURLs(this.EmailSource, this.getBaseURL()),
			'Subject': this.Subject,
			'AccountList': [],
			'GroupList': []
		};

		synConn
			.http({
				method: 'POST',
				uri: 'wp/{wpId}/settings/SetOutboxMessage',
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
			.http({ uri: 'wp/{wpId}/settings/GetOutboxMessage?ID='+this.ID })
			.success(function(data){
				_this._data = data;

				try {
					data.EmailSource = emUtils.unpackEmailSource(data.EmailSource);	
				} catch(e) {
					data.EmailSource = 'error';
					defer.reject(e);
				}
				

				//data.BaseURL = emUtils.supplant('/wps/outbox/{wpid}/{guid}/',{ guid: data.Guid, wpid: emAuth.get('workplace').ID });
				_this.EmailSource = emUtils.expandAssetURLs(data.EmailSource, _this.getBaseURL());
				defer.resolve(_this.EmailSource);

				//this.CreateTime = emUtils.timeLogToDate(this.CreateTime);
			})
			.error(function(err){
				console.error(err);
				defer.reject(err);
			});

		return defer.promise;
	};

	p.reloadData = function(done) {
		done = done || function(){};
		var _this = this;
		synConn
			.http({ uri: 'wp/{wpId}/settings/GetOutboxMessage?ID='+this.ID })
			.success(function(data){
				_this._data = data;

				try {
					data.EmailSource = emUtils.unpackEmailSource(data.EmailSource);	
				} catch(e) {
					data.EmailSource = 'error';
				}
				

				//data.BaseURL = emUtils.supplant('/wps/outbox/{wpid}/{guid}/',{ guid: data.Guid, wpid: emAuth.get('workplace').ID });
				_this.EmailSource = emUtils.expandAssetURLs(data.EmailSource, _this.getBaseURL());

				// refreshing the rest of data
				if ( typeof data !== 'undefined' ) {
					angular.extend(_this, data);
				}
				done(null, data);
				//this.CreateTime = emUtils.timeLogToDate(this.CreateTime);
			})
			.error(function(err){
				console.error(err);
				done(err);
			});
	};

	p.del = function(done, forever) {
		var req;

		if ( forever ) {
			req = {
				method: 'DELETE',
				uri: 'wp/{wpId}/settings/outbox/'+this.ID
			};
		} else {
			req = {
				method: 'PUT',
				uri: 'wp/{wpId}/settings/outbox/'+this.ID,
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

	p.startPolling = function() {
		var _this = this;
		this.pollForUpdates();
	};

	p.stopPolling = function() {
		if ( this.pollTO ) {
			$timeout.cancel(this.pollTO);	
		}		
	};

	p.pollForUpdates = function(){
		var _this = this;
		_this.reloadData(function(err, data){
			if ( !err && data && typeof _this._oldStatus !== 'undefined' && _this._oldStatus !== data.Status ) {
				// if status changed - stop the polling
				delete _this._oldStatus;
				return;
			}
			_this.pollTO = $timeout(function(){
				_this.pollForUpdates();
			}, 3000);
		});
	};

	p.startSending = function() {
		var _this = this;
		synConn
			.http({ uri: 'wp/{wpId}/settings/SendMessage?ID='+this.ID })
			.success(function(data){

				_this.startPolling();

			})
			.error(function(err){
				console.error(data);
			});
	};

	p.stopSending = function() {
		var _this = this;

		this._oldStatus = this.Status;

		// saving old status, then check in poll function for changes and stop

		synConn
			.http({ uri: 'wp/{wpId}/settings/StopSending?ID='+this.ID })
			.success(function(data){

				$timeout(function(){
					_this.reloadData();	
				}, 3000);

				// if ( typeof data !== 'undefined' ) {
				// 	angular.extend(this, data);
				// }

				// this.CreateTime = emUtils.timeLogToDate(this.CreateTime);				

			})
			.error(function(err){
				console.error(err);
			});
		// SendDraftMessage
		// param - ID
		// StopSending - ID
		// SendMessage - ID			
	};	

	return EmEml;
});

// Reddit constructor function to encapsulate HTTP and pagination logic
app.factory('EmEmails', function($http, $q, emAuth, synConn, emGroups, emAccounts, EmEml) {
  var EmEmails = function($scope, opts) {
  	opts = opts || {};
  	this.$scope = $scope;
    this.items = [];
    this.busy = false;
    this.after = '';
    this.noMoreData = false;
    this.options = {
    	showDeleted: false
    };
    angular.extend(this.options, opts);
    this.m = new Emmi();
  };

  	EmEmails.prototype.getEmail = function(id, done) {
  		var _this = this;

  		if ( !this.items.length ) {
  			this.m.once('data', function(){
  				_this.getEmail(id, done);
  			});
  			return;
  		}

  		var found = false;
  		angular.forEach(this.items, function(eml){
  			if ( eml.ID == id ) {
  				done(null, eml);
  				found = true;
  				return false; // breaking the loop
  			}
  		});
  		if ( !found ) {
  			done(new Error('Email with id '+id+' is not found.'));
  		}
  	};

	EmEmails.prototype.nextPage = function() {
		var that = this,
			defer = $q.defer();

		if ( that.noMoreData ) { return; }
		if ( that.busy ) return;

		that.busy = true;

		var sql = '*';

		function resolveGroups(emails, done) {

			var emailsToProcess = emails.length;

			for (var i = 0; i < emails.length; i++) {
				
				emGroups.resolve(emails[i].GroupList, function(err, resolved){
					if (err) { return console.log(err); } 
					emails[this.i].resGroupList = resolved;

					emAccounts.resolve(emails[this.i].AccountList, function(err, resolvedAccounts){
						if (err) { return console.log(err); } 

						emails[this.i].resAccountList = resolvedAccounts;

						emailsToProcess -= 1;
						if ( emailsToProcess === 0 ) {
							done(emails);
						}

					}.bind({ i: this.i }));

				}.bind({ i: i }));			
			}
		}

		var start = this.items.length;

		synConn
			.http({ uri: 'wp/{wpId}/settings/Outbox?Select=*&where=deleted='+(this.options.showDeleted ? '1' : '0')+'&STARTINDEX='+start+'&RESULTS=10&SORT=CreateTime&DIR=DESC' })
			.success(function(data){

				// no more emails
				if ( typeof data.fieldCount !== 'undefined' ) {
					that.busy = false;
					that.noMoreData = true;
				} else {
					resolveGroups(data, function(emails){
						for (var i = 0; i < emails.length; i++) {
							that.items.push(new EmEml(emails[i]));
						}
						that.busy = false;
						that.m.emit('data');
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

	EmEmails.prototype.del = function(eml) {
		for (var i = 0; i < this.items.length; i++) {
			if ( this.items[i] === eml ) {
				this.items.splice(i, 1);
				break;
			}
		}
	};  

  return EmEmails;
});

app.factory('EmSentLog', function($http, $q, emAuth, synConn, emGroups, emAccounts, EmEml) {
  var EmSentLog = function($scope, msgId, type) {
  	this.$scope = $scope;
  	this.type = type;
  	this.msgId = msgId;
    this.items = [];
    this.busy = false;
    this.noMoreData = false;
    this.after = '';
  };

  EmSentLog.prototype.nextPage = function() {
  	var that = this,
  	defer = $q.defer();

    if ( that.busy || that.noMoreData ) return;
    that.busy = true;

	function resolveGroups(emails, done) {

		var emailsToProcess = emails.length;

		for (var i = 0; i < emails.length; i++) {
			
			emGroups.resolve(emails[i].GroupList, function(err, resolved){
				if (err) { throw err; } 
				emails[this.i].GroupList = resolved;

				emAccounts.resolve(emails[this.i].AccountList, function(err, resolvedAccounts){
					if (err) { throw err; } 

					emails[this.i].AccountList = resolvedAccounts;

					emailsToProcess -= 1;
					if ( emailsToProcess === 0 ) {
						done(emails);
					}
				}.bind({ i: this.i }));
			}.bind({ i: i }));			
		}
	}

	var start = this.items.length;

	/*
	$$hashKey: "01Q"
	Clicked: 0
	Email: "sales@cartridgecare.com"
	GroupName: "Import From SQL Server EasyMail"
	ID: 1
	LastResponse: "63"
	Log: "￰PFNlbmRSYXdFbWFpbFJlc3BvbnNlIHhtbG5zPSJodHRwOi8vc2VzLmFtYXpvbmF3cy5jb20vZG9jLzIwMTAtMTItMDEvIj4KICA8U2VuZFJhd0VtYWlsUmVzdWx0PgogICAgPE1lc3NhZ2VJZD4wMDAwMDE0MTNiN2RjY2E2LWExMTdhMjllLWIwYzAtNGI0NS05ZTRlLTQ2ZmE1MTlhMThkYy0wMDAwMDA8L01lc3NhZ2VJZD4KICA8L1NlbmRSYXdFbWFpbFJlc3VsdD4KICA8UmVzcG9uc2VNZXRhZGF0YT4KICAgIDxSZXF1ZXN0SWQ+ZjU4NzBmMmItMjFmNS0xMWUzLWEwNWEtMjc4Y2Q5NmI3ZGFmPC9SZXF1ZXN0SWQ+CiAgPC9SZXNwb25zZU1ldGFkYXRhPgo8L1NlbmRSYXdFbWFpbFJlc3BvbnNlPgoNCg0KODg5"
	Opened: 0
	RecipientName: "cartridgecare"
	SendDate: "2013-09-20T13:10:44"
	Status: 1
	*/	
	var ipp = 25,
		sql = '';

	switch (this.type) {
		case 'processed': 
			sql = 'Select * from SentItems LIMIT '+start+','+ipp;
		break;
		case 'sent': 
			sql = 'Select * from SentItems where Status = 1 LIMIT '+start+','+ipp;
		break;
		case 'failed': 
			sql = 'Select * from SentItems where Status != 1 LIMIT '+start+','+ipp;
		break;
	} 

	synConn
		.http({ uri: 'wp/{wpId}/settings/GetSentItems?MessageID='+this.msgId+'&SQL='+sql.replace(/\s+/g, '+') })
		.success(function(data){

			if ( typeof data.fieldCount !== 'undefined' ) {
				that.busy = false;
				that.noMoreData = true;
				return;
			}

			for (var i = 0; i < data.length; i++) {				
				data[i].SendDate = new Date(data[i].SendDate);
				switch ( data[i].Status ) {
					case 1: 
						data[i].StatusText = 'Successfully delivered';
					break; 
					case 2: 
						data[i].StatusText = 'Temporary error. Sending attempt will be repeted in a minute';
					break; 
					case 3: 
						data[i].StatusText = 'Error';
					break;
				}
				that.items.push(data[i]);
			}
			that.busy = false;
			defer.resolve();
		})
		.error(function(err){
			console.error(err);
			defer.reject(err);
		});

	return defer.promise;
  };

  return EmSentLog;
});