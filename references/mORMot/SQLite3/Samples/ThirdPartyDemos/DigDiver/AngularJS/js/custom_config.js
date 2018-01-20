var ckeditorBasePath = CKEDITOR.basePath.substr(0, CKEDITOR.basePath.indexOf('ckeditor/'));
var customPluginsRoot = ckeditorBasePath + 'custom_plugins/';

CKEDITOR.plugins.addExternal('resizewithwindow', customPluginsRoot+'resizewithwindow/plugin.js', '');
CKEDITOR.plugins.addExternal('iframedialog', customPluginsRoot+'iframedialog/plugin.js', '');
CKEDITOR.plugins.addExternal('em_image_gallery', customPluginsRoot+'em_image_gallery/plugin.js', '');

CKEDITOR.editorConfig = function( config ) {
	// Define changes to default configuration here.
	// For the complete reference:
	// http://docs.ckeditor.com/#!/api/CKEDITOR.config

	// %REMOVE_START%
	//The configuration options below are needed when running CKEditor from source files.
	//config.plugins = 'dialogui,dialog,about,a11yhelp,basicstyles,blockquote,clipboard,panel,floatpanel,menu,contextmenu,resize,button,toolbar,elementspath,list,indent,enterkey,entities,popup,filebrowser,floatingspace,listblock,richcombo,format,htmlwriter,horizontalrule,wysiwygarea,image,fakeobjects,link,magicline,maximize,pastetext,pastefromword,removeformat,sourcearea,specialchar,menubutton,scayt,stylescombo,tab,table,tabletools,undo,wsc,panelbutton,colorbutton,font,justify,liststyle';
	//config.skin = 'moono';
	// %REMOVE_END%
	

	config.fullPage = true;
	config.allowedContent = true;

	config.entities = false;
	config.basicEntities = true;
	//config.entities_latin = false;

	config.resize_enabled = false;

	config.extraPlugins = 'resizewithwindow,iframedialog,em_image_gallery';

	config.enterMode = CKEDITOR.ENTER_BR;
	config.shiftEnterMode = CKEDITOR.ENTER_P;	
	config.fillEmptyBlocks = false;

	config.codemirror = {
		// showSearchButton: true,
		showCommentButton: false,
		showUncommentButton: false
	};

	// The toolbar groups arrangement, optimized for two toolbar rows.
	config.toolbarGroups = [
		{ name: 'document',	   groups: [ 'mode', 'document' ] },
		{ name: 'clipboard',   groups: [ 'clipboard', 'undo' ] },
		{ name: 'editing',     groups: [ 'find', 'selection', 'spellchecker' ] },		
		//'/',
		{ name: 'styles' },		
		{ name: 'links' },
		'/',
		{ name: 'basicstyles', groups: [ 'basicstyles', 'cleanup' ] },		
		{ name: 'paragraph',   groups: [ 'list', 'indent', 'blocks', 'align' ] },
		{ name: 'colors' },
		{ name: 'tools' },
		{ name: 'document',	groups: [ 'document', 'doctools' ] },
		{ name: 'emBar' },
		{ name: 'insert' },
		{ name: 'about' },		
		{ name: 'others' }
	];

	// Remove some buttons, provided by the standard plugins, which we don't
	// need to have in the Standard(s) toolbar.
	config.removeButtons = 'Subscript,Superscript,Save,Styles';
};
