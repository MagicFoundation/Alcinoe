    Ext.apply(Ext.form.field.VTypes, {
    	daterange : function (val, field) {
    		var date = field.parseDate(val);
    		if (!date) {
    			return false;
    		}
    		if (field.startDateField && (!this.dateRangeMax || (date.getTime() != this.dateRangeMax.getTime()))) {
    			var start = field.up('form').down('#' + field.startDateField);
    			start.setMaxValue(date);
    			start.validate();
    			this.dateRangeMax = date;
    		} else if (field.endDateField && (!this.dateRangeMin || (date.getTime() != this.dateRangeMin.getTime()))) {
    			var end = field.up('form').down('#' + field.endDateField);
    			end.setMinValue(date);
    			end.validate();
    			this.dateRangeMin = date;
    		}
    		return true;
    	},
    	daterangeText : 'Start date must be before end date'
    });

    Ext.tip.QuickTipManager.init();
    Ext.define('ExtMVC.view.contact.Filtrod', {
    	extend : 'Ext.form.Panel',
    	alias : 'widget.contatofiltrod',
    	autoShow : true,
    	renderTo : 'dr',
    	height : 160,
    	width : 400,
    	padding : 10,
    	layout : {
    		type : 'fit'
    	},
    	iconCls : 'icon-user',
    	title : '<h1>Filtering Date Range</h1>',

    	initComponent : function () {
    		var me = this;
    		Ext.applyIf(me, {
    			items : [{
    					xtype : 'form',
    					style : 'background-color: #fff;',
    					bodyPadding : 10,
    					items : [{
    							xtype : 'datefield',
    							fieldLabel : 'Start Date',
    							name : 'startdt',
    							itemId : 'startdt',
    							format : 'd-m-Y',
    							id : 'startdt',
    							vtype : 'daterange',
    							endDateField : 'enddt' // id of the end date field
    						}, {
    							xtype : 'datefield',
    							fieldLabel : 'End Date',
    							name : 'enddt',
    							itemId : 'enddt',
    							format : 'd-m-Y',
    							id : 'enddt',
    							vtype : 'daterange',
    							startDateField : 'startdt' // id of the start date field
    						}
    					],
    					dockedItems : [{
    							xtype : 'toolbar',
    							dock : 'bottom',
    							items : [{
    									xtype : 'tbfill'
    								}, {
    									xtype : 'button',
    									action : 'resetd',
    									iconCls : 'icon-default',
    									text : 'Reset Filter'
    								}, {
    									xtype : 'button',
    									action : 'filtrar_buscad',
    									formBind : true,
    									iconCls : 'icon-filter',
    									text : 'Filter'
    								}, {
    									xtype : 'button',
    									action : 'deleterange',
    									iconCls : 'icon-delete',
    									text : 'Delete Date Range'
    								},
    							]
    						}
    					]
    				}
    			]
    		});
    		me.callParent(arguments);
    	}
    });