Ext.define('ExtMVC.view.contact.Filtro', {
	extend : 'Ext.window.Window',
	alias : 'widget.contatofiltro',
	autoShow : true,
	height : 168,
	width : 398,
	layout : {
		type : 'fit'
	},
	iconCls : 'icon-user',
	title : 'Filtrar/Buscar Contatos',
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
							id : 'Fname',
							name : 'Name',
							fieldLabel : 'Name',
							msgTarget : 'side',
							allowBlank : false,
							maxLength : 255
						}, {
							xtype : 'datefield',
							anchor : '100%',
							id : 'Ftime',
							name : 'TimeD',
							fieldLabel : 'Time',
							msgTarget : 'side',
							allowBlank : false,
							format : 'd-m-Y',
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
									action : 'reset',
									text : 'Reset Filter'
								}, {
									xtype : 'button',
									action : 'filtrar_busca',
									formBind : true,
									text : 'Filter'
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