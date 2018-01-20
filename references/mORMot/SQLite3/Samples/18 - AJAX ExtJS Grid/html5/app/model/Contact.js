Ext.define('ExtMVC.model.Contact', {
	extend : 'Ext.data.Model',
	idProperty : 'ID',
	totalproperty : 'total',
	fields : [{
			name : 'ID',
			type : 'int'
		}, {
			name : 'TimeD',
			type : 'datetime'
		}, {
			name : 'Name',
			type : 'string'
		}, {
			name : 'Question',
			type : 'string'
		}
	]
});