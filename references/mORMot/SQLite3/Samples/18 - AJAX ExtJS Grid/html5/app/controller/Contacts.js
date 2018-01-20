Ext.define('ExtMVC.controller.Contacts', {
	extend : 'Ext.app.Controller',

	models : [
		'Contact'
	],
	stores : [
		'Contacts'
	],
	views : [
		'contact.Grid',
		'contact.Edit',
		'contact.Filtro'
	],

	refs : [{
			ref : 'contactGrid',
			selector : 'contactgrid',
			xtype : 'gridpanel'
		}, {
			ref : 'contatoFiltro',
			xtype : 'contatofiltro',
			selector : 'contatofiltro'
		}, {
			ref : 'contatoFiltrod',
			xtype : 'contatofiltrod',
			selector : 'contatofiltrod'
		}
	],

	dblClickEdit : function (dataview, record, item, index, e, options) {
		var edit = Ext.create('ExtMVC.view.contact.Edit'); 
		if (record) {
			edit.down('form').loadRecord(record);
		}

		var grid = this.getContactGrid();
		var rec = grid.getSelectionModel().getSelection();
		
	},

	onButtonClickAdd : function (button, e, options) {
		this.dblClickEdit();
	},

	onButtonClickDelete : function (button, e, options) {
		var grid = this.getContactGrid(),
		record = grid.getSelectionModel().getSelection();
		cont = grid.getSelectionModel().getSelection()[0].data.Name;
		store = this.getContactsStore();

		Ext.MessageBox.show({
			title : 'Delete Record',
			buttons : Ext.MessageBox.YESNO,
			msg : 'Delete this record ' + cont + '?',
			icon : Ext.Msg.WARNING,
			fn : function (btn) {
				if (btn == 'yes') {
					store.remove(record);
					store.sync({
						success : function () {
							store.load();
						}
					});
				}

			}
		});

	},

	onButtonClickSave : function (button, e, options) {
		var win = button.up('window'),
		form = win.down('form'),
		record = form.getRecord();
		values = form.getValues();
		store = this.getContactsStore();
		var isNew = false;
		if (values.ID > 0) {
			record.set(values);
		} else {
			Ext.Ajax.on("beforerequest", function( conn, options, eOpts){
			  if (options.action=='create')
			  {
				var newData = values; 
				delete newData.ID;
				options.jsonData = newData;
			  }
			});			
			record = Ext.create('ExtMVC.model.Contact');
			record.set(values);
			store.add(record);
			isNew = true;
		}
		win.close();

		Ext.MessageBox.show({
			title : 'Save Record',
			buttons : Ext.MessageBox.YESNO,
			msg : 'Save this record?',
			icon : Ext.Msg.WARNING,
			fn : function (btn) {
				if (btn == 'yes') {
					store.sync({
						success : function () {
							if (isNew) {
								store.load();
							}
						}
					});
				}

			}
		});
	},

	onButtonClickCancel : function (button, e, options) {
	var win = button.up('window'),
		form = win.down('form');
		form.getForm().reset();
		win.close();
	},

	loadFilter : function (button) {
		var win = Ext.widget('contatofiltro');
		win.show();
	},

	setFilter : function (btn) {
		var me = this;
		var win = btn.up('contatofiltro');
		var item = win.down('form').getValues();
		var store = me.getContactsStore();
		store.remoteFilter = false;
		store.clearFilter(); 
		store.remoteFilter = true;
		store.getProxy().extraParams = {
			where : 'Name LIKE :("' + Ext.getCmp("Fname").value + '%"): AND TimeD=:("' + Ext.Date.format(Ext.getCmp("Ftime").value, "Y-m-d") + '"):'
		};

		var obj = item
			for (var prop in obj) {
				var xname = obj.name;
				var xphone = obj.timed;
				var xemail = obj.question;
			}
			store.filter([
					Ext.create('Ext.util.Filter', {property : "Name", value : xname,root : 'values'}),
					Ext.create('Ext.util.Filter', {property : "TimeD", value : xphone,root : 'values'}),
					Ext.create('Ext.util.Filter', {property : "Question", value : xemail,root : 'values'})
				]);
	},

	reset : function (btn) {
		var win = btn.up('contatofiltro');
		win.down('form').getForm().reset();
		Ext.getCmp('mygrid').getStore().getProxy().extraParams = '';
		Ext.getCmp('mygrid').getStore().load();
	},

	loadFilterData : function (button) {
		var win = Ext.widget('contatofiltrod');
		win.show();
	},

	setFilterData : function (btn) {
		var me = this;
		var win = btn.up('contatofiltrod');
		var item = win.down('form').getValues();
		var store = me.getContactsStore();
		store.remoteFilter = false;
		store.clearFilter();
		store.remoteFilter = true;
		store.getProxy().extraParams = {
			where : 'TimeD BETWEEN :("' + Ext.Date.format(Ext.getCmp("startdt").value, "Y-m-d") + '"): AND :("' + Ext.Date.format(Ext.getCmp("enddt").value, "Y-m-d") + '"):'
		};
		var obj = item
			for (var prop in obj) {
				var xphone = obj.timed;
			}
			store.filter([
					Ext.create('Ext.util.Filter', {
						property : "TimeD",
						value : xphone,
						root : 'values'
					})
				]);
	},

	deleteRange : function (btn) {
		var me = this;
		var win = btn.up('contatofiltrod');
		var item = win.down('form').getValues();
		var store = me.getContactsStore();
		store.remoteFilter = false;
		store.clearFilter();
		store.remoteFilter = true;
		store.getProxy().extraParams = {
			where : 'TimeD BETWEEN :("' + Ext.Date.format(Ext.getCmp("startdt").value, "Y-m-d") + '"): AND :("' + Ext.Date.format(Ext.getCmp("enddt").value, "Y-m-d") + '"):'
		};
		var obj = item
			for (var prop in obj) {
				var xphone = obj.timed;
			}
			store.filter([
					Ext.create('Ext.util.Filter', {
						property : "TimeD",
						value : xphone,
						root : 'values'
					})
				]);

		Ext.MessageBox.show({
			title : 'Delete Record',
			buttons : Ext.MessageBox.YESNO,
			msg : 'Delete Selection Records',
			icon : Ext.Msg.WARNING,
			fn : function (btn) {
				if (btn == 'yes') {
					Ext.Ajax.request({
						url : ((store.getProxy().api.read + '&where=' + store.getProxy().extraParams.where)),
						method : 'DELETE',
						scope : this
					});
					store.sync({
						success : function () {
							store.load();
						}
					});
				}
			}
		});

	},
	resetData : function (btn) {
		var win = btn.up('contatofiltrod');
		win.down('form').getForm().reset();
		Ext.getCmp('mygrid').getStore().getProxy().extraParams = '';
		Ext.getCmp('mygrid').getStore().load();
		win.close();
	},

	init : function (application) {
		this.control({
			"contactgrid dataview" : {
				itemdblclick : this.dblClickEdit
			},
			 "contactgrid button[action=add]": {
				click: this.onButtonClickAdd
            },
			"contactgrid button[action=delete]" : {
				click : this.onButtonClickDelete
			},
			"contactform button[action=save]" : {
				click : this.onButtonClickSave
			},
			"contactform button[action=cancel]" : {
				click : this.onButtonClickCancel
			},
			"contactgrid button[action=filtrar]" : {
				click : this.loadFilter
			},
			"contatofiltro button[action=filtrar_busca]" : {
				click : this.setFilter
			},
			"contatofiltro button[action=reset]" : {
				click : this.reset
			},
			"contactgrid button[action=filtrar_name]" : {
				click : this.loadFilterData
			},
			"contatofiltrod button[action=filtrar_buscad]" : {
				click : this.setFilterData
			},
			"contatofiltrod button[action=deleterange]" : {
				click : this.deleteRange
			},
			"contatofiltrod button[action=resetd]" : {
				click : this.resetData
			}
		});
	}
});