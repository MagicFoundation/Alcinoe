Ext.require([
    'Ext.util.*'
]);

Ext.define('ExtMVC.view.contact.Edit', {
	extend : 'Ext.window.Window',
	alias : 'widget.contactform',
	id: 'contactform',
	autoShow : true,
	height : 168,
	width : 398,
	layout : {
		type : 'fit'
	},
	iconCls : 'icon-user',
	title : 'Create/Edit SampleRecord',
	initComponent : function () {
		var me = this;

		Ext.applyIf(me, {
			items : [{
					xtype : 'form',
					style : 'background-color: #fff;',
					bodyPadding : 10,
					items : [{
							xtype : 'hiddenfield',
							anchor : '100%',
							name : 'ID',
							fieldLabel : 'Label'
						}, {
							xtype : 'textfield',
							anchor : '100%',
							id : 'FnameE',
							name : 'Name',
							fieldLabel : 'Name',
							msgTarget : 'side',
							allowBlank : false,
							maxLength : 255
						}, {
							xtype : 'datefield',
							anchor : '100%',
							id : 'FtimeE',
							name : 'TimeD',
							fieldLabel : 'Date',
							msgTarget : 'side',
							allowBlank : false,
							format : 'Y-m-d',
							altformat: 'Y-m-d H:i',
							maxLength : 255
						}, {
							xtype : 'textfield',
							anchor : '100%',
							id : 'FquestionE',
							name : 'Question',
							fieldLabel : 'Question',
							msgTarget : 'side',
							allowBlank : false,
							maxLength : 255
						}
					],
					dockedItems : [{
							xtype : 'toolbar',
							dock : 'bottom',
							items : [{
									xtype : 'tbfill'
								}, {
								  xtype : 'button',
								  action : 'cancel',
								  iconCls : 'icon-reset',
								  text : 'Cancel'
								}, {
									xtype : 'button',
									action : 'save',
									formBind : true,
									iconCls : 'icon-save',
									text : 'Update'
								}
							]
						}
					]
				}
			]
		});
		me.callParent(arguments);
	}
});