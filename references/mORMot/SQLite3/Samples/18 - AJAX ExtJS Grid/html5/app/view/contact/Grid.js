Ext.define('RemoteCountrySearch', {
	extend : 'Ext.form.ComboBox',
	alias : 'widget.remoteCountrySearch',
	queryMode : 'remote',
	displayField : 'Name',
	valueField : 'ID',
	forceSelection : true,
	id : 'countryBox',
	style : 'font-size: medium; color: blue;',
	fieldStyle : 'font-size: medium; color: red; heigth: 50px;',
	labelWidth : 80,
	fieldLabel : 'Select a Name',
	size : 50,
	maxLength : 50,
	allowBlank : true,
	name : 'remoteCountry',
	store : 'Contacts',
	minChars : 2,
	hideTrigger : true,
	triggerAction : 'All',
	typeAhead : false,
	listeners : {
		'beforequery' : function (queryEvent, options) {
			this.store.loadPage(1);
			this.store.remoteFilter = false;
			this.store.clearFilter(); 
			this.store.remoteFilter = true;
			this.store.getProxy().extraParams = {
				where : 'Name like :("' + queryEvent.query + '%"):'
			};
			this.store.load();			
		},'keyup': function() {/*TODO*/}
	}
});

Ext.define('ExtMVC.view.contact.Grid', {
	extend : 'Ext.grid.Panel',
	alias : 'widget.contactgrid',
	renderTo : 'warleyalex',
	padding : 10,
	id : 'mygrid',
	iconCls : 'icon-grid',
	title : 'Using mORMot with ExtJS >> by warleyalex',
	store : 'Contacts',
	initComponent : function () {
		var me = this;

		Ext.applyIf(me, {
			columns : [{
					xtype : 'gridcolumn',
					text : "id",
					width : 10,
					dataIndex : 'ID',
					sortable : true,
					hidden : true
				}, {
					xtype : 'gridcolumn',
					width : 50,
					dataIndex : 'Name',
					flex : 1,
					text : 'Name'
				}, {
					xtype : 'gridcolumn',
					width : 10,
					dataIndex : 'TimeD',
					flex : 1,
					text : 'Date'
				}, {
					xtype : 'gridcolumn',
					width : 50,
					dataIndex : 'Question',
					flex : 1,
					text : 'Question'
				}
			],
			viewConfig : {
				emptyText : 'no records found'
			},
			dockedItems : [{
					xtype : 'toolbar',
					dock : 'top',
					items : [{
							xtype : 'button',
							action : 'add',
							iconCls : 'icon-save',
							text : 'Add'
						}, {
							xtype : 'button',
							action : 'delete',
							iconCls : 'icon-delete',
							text : 'Delete'
						}, {
							xtype : 'button',
							action : 'filtrar',
							iconCls : 'icon-search',
							text : 'Full Search'
						}, {
							xtype : 'button',
							action : 'filtrar_name',
							text : 'Date Range'
						}, {
							xtype : 'remoteCountrySearch'
						}
					]
				}, {
					xtype : 'pagingtoolbar',
					dock : 'bottom',
					width : 1004,
					displayInfo : true,
					emptyMsg : 'No contact to display',
					store : 'Contacts'
				}
			]
		});
		me.callParent(arguments);
	}
});