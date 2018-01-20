Ext.define('ExtMVC.view.Viewport', {
	extend : 'ExtMVC.view.contact.Grid',
	renderTo : Ext.getBody(),
	requires : [
		'ExtMVC.view.contact.Grid',
		'ExtMVC.view.contact.Edit',
		'ExtMVC.view.contact.Filtro',
		'ExtMVC.view.contact.Filtrod'
	]
});