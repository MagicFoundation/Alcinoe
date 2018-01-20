//------------------------------------------------------------------------------
//
//  Free Pascal for Android
//
//------------------------------------------------------------------------------

library main;

{$mode delphi}
{$packrecords c}

uses
 And_jni,
 And_Controls_Types,And_Controls,
 Form_Main;

//------------------------------------------------------------------------------
// Application Define
//
// XRef.
//       App.java            : package com.kredix;
//       Controls.java       : package com.kredix;
//
//       AndroidManifest.xml : package = "com.kredix"
//       uninstall.bat       : adb.exe uninstall com.kredix
//------------------------------------------------------------------------------
Const
 cAppName = 'com.kredix';          //   ! Use your app name
 cAppJ    = 'com_kredix_Controls'; //   ! Pascal Lib. Export name

//-----------------------------------------------------------------------------
//  Java Interface
//-----------------------------------------------------------------------------
exports
 // JNI
 JNI_OnLoad                             name 'JNI_OnLoad',
 JNI_OnUnload                           name 'JNI_OnUnload',

 // App Event
 Java_Event_pOnAppCreate                name 'Java_' + cAppJ + '_pOnAppCreate'              ,
 Java_Event_pOnAppScreenStyle           name 'Java_' + cAppJ + '_pOnAppScreenStyle'         ,
 Java_Event_pOnAppScreenOrientation     name 'Java_' + cAppJ + '_pOnAppScreenOrientation'   ,
 Java_Event_pOnAppNewIntent             name 'Java_' + cAppJ + '_pOnAppNewIntent'           ,
 Java_Event_pOnAppDestroy               name 'Java_' + cAppJ + '_pOnAppDestroy'             ,
 Java_Event_pOnAppPause                 name 'Java_' + cAppJ + '_pOnAppPause'               ,
 Java_Event_pOnAppRestart               name 'Java_' + cAppJ + '_pOnAppRestart'             ,
 Java_Event_pOnAppResume                name 'Java_' + cAppJ + '_pOnAppResume'              ,
 Java_Event_pOnAppActive                name 'Java_' + cAppJ + '_pOnAppActive'              ,
 Java_Event_pOnAppStop                  name 'Java_' + cAppJ + '_pOnAppStop'                ,
 Java_Event_pOnAppBackPressed           name 'Java_' + cAppJ + '_pOnAppBackPressed'         ,
 Java_Event_pOnAppRotate                name 'Java_' + cAppJ + '_pOnAppRotate'              ,
 Java_Event_pOnAppConfigurationChanged  name 'Java_' + cAppJ + '_pOnAppConfigurationChanged',
 Java_Event_pOnAppActivityResult        name 'Java_' + cAppJ + '_pOnAppActivityResult'      ,

 // Control Event
 Java_Event_pOnChange                   name 'Java_' + cAppJ + '_pOnChange',
 Java_Event_pOnClick                    name 'Java_' + cAppJ + '_pOnClick' ,
 Java_Event_pOnDraw                     name 'Java_' + cAppJ + '_pOnDraw'  ,
 Java_Event_pOnEnter                    name 'Java_' + cAppJ + '_pOnEnter' ,
 Java_Event_pOnTimer                    name 'Java_' + cAppJ + '_pOnTimer' ,
 Java_Event_pOnTouch                    name 'Java_' + cAppJ + '_pOnTouch' ,

 // Form Event
 Java_Event_pOnClose                    name 'Java_' + cAppJ + '_pOnClose' ,

 // State Event
 Java_Event_pOnGLViewState              name 'Java_' + cAppJ + '_pOnGLViewState'   ,
 Java_Event_pOnWebViewState             name 'Java_' + cAppJ + '_pOnWebViewState'  ,
 Java_Event_pOnAsyncTaskState           name 'Java_' + cAppJ + '_pOnAsyncTaskState',
 Java_Event_pOnHttpState                name 'Java_' + cAppJ + '_pOnHttpState'     ,

 // Camera Preview Event
 Java_Event_pOnCameraFrame              name 'Java_' + cAppJ + '_pOnCameraFrame';

//-----------------------------------------------------------------------------
//  Application StartUp
//-----------------------------------------------------------------------------

Procedure OnAppCreate;
 begin
  App.Form   := jForm_Main.Create(nil);
  App.Form.Show;
 end;

begin
  Dbg('Lib Start');
  jAppInit(App,                          // App Object Create & Init
           cAppName,                     // App Name
           Screen_Style_Normal,          // Screen Style [Normal,Full]
           Screen_Orientation_Portrait,  // Screen Orientation [Portrait,Landscape]
           cLogModeOn,                   // Log Mode (Default : On)
           OnAppCreate);                 // App Create Event
end.
