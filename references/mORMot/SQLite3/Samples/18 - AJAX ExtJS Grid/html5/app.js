 Ext.Ajax.useDefaultXhrHeader = false;
 Ext.Ajax.cors = true;

 Ext.Loader.setConfig({
 	enabled : true
 });

 Ext.application({
 	autoCreateViewport : true,
 	name : 'ExtMVC',
 	controllers : [
 		'Contacts'
 	]
 });