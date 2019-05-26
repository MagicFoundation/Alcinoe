{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2013-2018 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Androidapi.JNI.App;

interface

uses
  Androidapi.JNIBridge,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os,
  Androidapi.JNI.Util,
  Androidapi.JNI.Widget,
  Androidapi.JNI.Net; // https://quality.embarcadero.com/browse/RSP-21294

type
// ===== Forward declarations =====

  JActionBar = interface;//android.app.ActionBar
  JActionBar_LayoutParams = interface;//android.app.ActionBar$LayoutParams
  JActionBar_OnMenuVisibilityListener = interface;//android.app.ActionBar$OnMenuVisibilityListener
  JActionBar_OnNavigationListener = interface;//android.app.ActionBar$OnNavigationListener
  JActionBar_Tab = interface;//android.app.ActionBar$Tab
  JActionBar_TabListener = interface;//android.app.ActionBar$TabListener
  JActivity = interface;//android.app.Activity
  JActivityManager = interface;//android.app.ActivityManager
  JActivityManager_MemoryInfo = interface;//android.app.ActivityManager$MemoryInfo
  JActivityManager_RunningAppProcessInfo = interface;//android.app.ActivityManager$RunningAppProcessInfo
  JActivityManager_TaskDescription = interface;//android.app.ActivityManager$TaskDescription
  JAlarmManager = interface;//android.app.AlarmManager
  JAlarmManager_AlarmClockInfo = interface;//android.app.AlarmManager$AlarmClockInfo
  JAlarmManager_OnAlarmListener = interface;//android.app.AlarmManager$OnAlarmListener
  JDialog = interface;//android.app.Dialog
  JAlertDialog = interface;//android.app.AlertDialog
  JAlertDialog_Builder = interface;//android.app.AlertDialog$Builder
  JApplication = interface;//android.app.Application
  JApplication_ActivityLifecycleCallbacks = interface;//android.app.Application$ActivityLifecycleCallbacks
  JApplication_OnProvideAssistDataListener = interface;//android.app.Application$OnProvideAssistDataListener
  JAutomaticZenRule = interface;//android.app.AutomaticZenRule
  JFragment = interface;//android.app.Fragment
  JDialogFragment = interface;//android.app.DialogFragment
  JFragment_SavedState = interface;//android.app.Fragment$SavedState
  JFragmentManager = interface;//android.app.FragmentManager
  JFragmentManager_BackStackEntry = interface;//android.app.FragmentManager$BackStackEntry
  JFragmentManager_FragmentLifecycleCallbacks = interface;//android.app.FragmentManager$FragmentLifecycleCallbacks
  JFragmentManager_OnBackStackChangedListener = interface;//android.app.FragmentManager$OnBackStackChangedListener
  JFragmentTransaction = interface;//android.app.FragmentTransaction
  JService = interface;//android.app.Service
  JIntentService = interface;//android.app.IntentService
  JLoaderManager = interface;//android.app.LoaderManager
  JLoaderManager_LoaderCallbacks = interface;//android.app.LoaderManager$LoaderCallbacks
  JNativeActivity = interface;//android.app.NativeActivity
  JNotification = interface;//android.app.Notification
  JNotification_Action = interface;//android.app.Notification$Action
  JNotificationChannel = interface;//android.app.NotificationChannel
  JNotificationChannelGroup = interface;//android.app.NotificationChannelGroup
  JNotificationManager = interface;//android.app.NotificationManager
  JNotificationManager_Policy = interface;//android.app.NotificationManager$Policy
  JPendingIntent = interface;//android.app.PendingIntent
  JPendingIntent_OnFinished = interface;//android.app.PendingIntent$OnFinished
  JPictureInPictureArgs = interface;//android.app.PictureInPictureArgs
  JPictureInPictureParams = interface;//android.app.PictureInPictureParams
  JRemoteInput = interface;//android.app.RemoteInput
  JSharedElementCallback = interface;//android.app.SharedElementCallback
  JSharedElementCallback_OnSharedElementsReadyListener = interface;//android.app.SharedElementCallback$OnSharedElementsReadyListener
  JTaskStackBuilder = interface;//android.app.TaskStackBuilder
  JVoiceInteractor = interface;//android.app.VoiceInteractor
  JVoiceInteractor_Request = interface;//android.app.VoiceInteractor$Request
  JAssistContent = interface;//android.app.assist.AssistContent
  JAppWidgetProviderInfo = interface;//android.appwidget.AppWidgetProviderInfo

// ===== Interface declarations =====

  // https://quality.embarcadero.com/browse/RSP-21294
  // This dummy class is just to avoid circular reference
  JAudioAttributesClass = interface(JObjectClass)
    ['{FDBDFCB3-4B3F-4E1B-9007-4E5B9EA6546A}']
  end;

  // https://quality.embarcadero.com/browse/RSP-21294
  // This dummy class is just to avoid circular reference
  [JavaSignature('android/media/AudioAttributes')]
  JAudioAttributes = interface(JObject)
    ['{CEC09B65-8360-46BD-A5E8-780E4927C9AA}']
  end;
  TJAudioAttributes = class(TJavaGenericImport<JAudioAttributesClass, JAudioAttributes>) end;

  JActionBarClass = interface(JObjectClass)
    ['{C1A41981-8687-4794-91A5-AA384F60A546}']
    {class} function _GetDISPLAY_HOME_AS_UP: Integer; cdecl;
    {class} function _GetDISPLAY_SHOW_CUSTOM: Integer; cdecl;
    {class} function _GetDISPLAY_SHOW_HOME: Integer; cdecl;
    {class} function _GetDISPLAY_SHOW_TITLE: Integer; cdecl;
    {class} function _GetDISPLAY_USE_LOGO: Integer; cdecl;
    {class} function _GetNAVIGATION_MODE_LIST: Integer; cdecl;
    {class} function _GetNAVIGATION_MODE_STANDARD: Integer; cdecl;
    {class} function _GetNAVIGATION_MODE_TABS: Integer; cdecl;
    {class} function init: JActionBar; cdecl;
    {class} property DISPLAY_HOME_AS_UP: Integer read _GetDISPLAY_HOME_AS_UP;
    {class} property DISPLAY_SHOW_CUSTOM: Integer read _GetDISPLAY_SHOW_CUSTOM;
    {class} property DISPLAY_SHOW_HOME: Integer read _GetDISPLAY_SHOW_HOME;
    {class} property DISPLAY_SHOW_TITLE: Integer read _GetDISPLAY_SHOW_TITLE;
    {class} property DISPLAY_USE_LOGO: Integer read _GetDISPLAY_USE_LOGO;
    {class} property NAVIGATION_MODE_LIST: Integer read _GetNAVIGATION_MODE_LIST;
    {class} property NAVIGATION_MODE_STANDARD: Integer read _GetNAVIGATION_MODE_STANDARD;
    {class} property NAVIGATION_MODE_TABS: Integer read _GetNAVIGATION_MODE_TABS;
  end;

  [JavaSignature('android/app/ActionBar')]
  JActionBar = interface(JObject)
    ['{607E590E-509A-40CB-A31C-448056752FAB}']
    procedure addOnMenuVisibilityListener(listener: JActionBar_OnMenuVisibilityListener); cdecl;
    procedure addTab(tab: JActionBar_Tab); cdecl; overload;//Deprecated
    procedure addTab(tab: JActionBar_Tab; setSelected: Boolean); cdecl; overload;//Deprecated
    procedure addTab(tab: JActionBar_Tab; position: Integer); cdecl; overload;//Deprecated
    procedure addTab(tab: JActionBar_Tab; position: Integer; setSelected: Boolean); cdecl; overload;//Deprecated
    function getCustomView: JView; cdecl;
    function getDisplayOptions: Integer; cdecl;
    function getElevation: Single; cdecl;
    function getHeight: Integer; cdecl;
    function getHideOffset: Integer; cdecl;
    function getNavigationItemCount: Integer; cdecl;//Deprecated
    function getNavigationMode: Integer; cdecl;//Deprecated
    function getSelectedNavigationIndex: Integer; cdecl;//Deprecated
    function getSelectedTab: JActionBar_Tab; cdecl;//Deprecated
    function getSubtitle: JCharSequence; cdecl;
    function getTabAt(index: Integer): JActionBar_Tab; cdecl;//Deprecated
    function getTabCount: Integer; cdecl;//Deprecated
    function getThemedContext: JContext; cdecl;
    function getTitle: JCharSequence; cdecl;
    procedure hide; cdecl;
    function isHideOnContentScrollEnabled: Boolean; cdecl;
    function isShowing: Boolean; cdecl;
    function newTab: JActionBar_Tab; cdecl;//Deprecated
    procedure removeAllTabs; cdecl;//Deprecated
    procedure removeOnMenuVisibilityListener(listener: JActionBar_OnMenuVisibilityListener); cdecl;
    procedure removeTab(tab: JActionBar_Tab); cdecl;//Deprecated
    procedure removeTabAt(position: Integer); cdecl;//Deprecated
    procedure selectTab(tab: JActionBar_Tab); cdecl;//Deprecated
    procedure setBackgroundDrawable(d: JDrawable); cdecl;
    procedure setCustomView(view: JView); cdecl; overload;
    procedure setCustomView(view: JView; layoutParams: JActionBar_LayoutParams); cdecl; overload;
    procedure setCustomView(resId: Integer); cdecl; overload;
    procedure setDisplayHomeAsUpEnabled(showHomeAsUp: Boolean); cdecl;
    procedure setDisplayOptions(options: Integer); cdecl; overload;
    procedure setDisplayOptions(options: Integer; mask: Integer); cdecl; overload;
    procedure setDisplayShowCustomEnabled(showCustom: Boolean); cdecl;
    procedure setDisplayShowHomeEnabled(showHome: Boolean); cdecl;
    procedure setDisplayShowTitleEnabled(showTitle: Boolean); cdecl;
    procedure setDisplayUseLogoEnabled(useLogo: Boolean); cdecl;
    procedure setElevation(elevation: Single); cdecl;
    procedure setHideOffset(offset: Integer); cdecl;
    procedure setHideOnContentScrollEnabled(hideOnContentScroll: Boolean); cdecl;
    procedure setHomeActionContentDescription(description: JCharSequence); cdecl; overload;
    procedure setHomeActionContentDescription(resId: Integer); cdecl; overload;
    procedure setHomeAsUpIndicator(indicator: JDrawable); cdecl; overload;
    procedure setHomeAsUpIndicator(resId: Integer); cdecl; overload;
    procedure setHomeButtonEnabled(enabled: Boolean); cdecl;
    procedure setIcon(resId: Integer); cdecl; overload;
    procedure setIcon(icon: JDrawable); cdecl; overload;
    procedure setListNavigationCallbacks(adapter: JSpinnerAdapter; callback: JActionBar_OnNavigationListener); cdecl;//Deprecated
    procedure setLogo(resId: Integer); cdecl; overload;
    procedure setLogo(logo: JDrawable); cdecl; overload;
    procedure setNavigationMode(mode: Integer); cdecl;//Deprecated
    procedure setSelectedNavigationItem(position: Integer); cdecl;//Deprecated
    procedure setSplitBackgroundDrawable(d: JDrawable); cdecl;
    procedure setStackedBackgroundDrawable(d: JDrawable); cdecl;
    procedure setSubtitle(subtitle: JCharSequence); cdecl; overload;
    procedure setSubtitle(resId: Integer); cdecl; overload;
    procedure setTitle(title: JCharSequence); cdecl; overload;
    procedure setTitle(resId: Integer); cdecl; overload;
    procedure show; cdecl;
  end;
  TJActionBar = class(TJavaGenericImport<JActionBarClass, JActionBar>) end;

  JActionBar_LayoutParamsClass = interface(JViewGroup_MarginLayoutParamsClass)
    ['{EE4F0254-72CB-4FAC-8827-660F24B0EE45}']
    {class} function init(c: JContext; attrs: JAttributeSet): JActionBar_LayoutParams; cdecl; overload;
    {class} function init(width: Integer; height: Integer): JActionBar_LayoutParams; cdecl; overload;
    {class} function init(width: Integer; height: Integer; gravity: Integer): JActionBar_LayoutParams; cdecl; overload;
    {class} function init(gravity: Integer): JActionBar_LayoutParams; cdecl; overload;
    {class} function init(source: JActionBar_LayoutParams): JActionBar_LayoutParams; cdecl; overload;
    {class} function init(source: JViewGroup_LayoutParams): JActionBar_LayoutParams; cdecl; overload;
  end;

  [JavaSignature('android/app/ActionBar$LayoutParams')]
  JActionBar_LayoutParams = interface(JViewGroup_MarginLayoutParams)
    ['{A6E6BC68-48E8-4FCC-8011-66834CDEC359}']
    function _Getgravity: Integer; cdecl;
    procedure _Setgravity(Value: Integer); cdecl;
    property gravity: Integer read _Getgravity write _Setgravity;
  end;
  TJActionBar_LayoutParams = class(TJavaGenericImport<JActionBar_LayoutParamsClass, JActionBar_LayoutParams>) end;

  JActionBar_OnMenuVisibilityListenerClass = interface(IJavaClass)
    ['{1E888E42-9D09-452B-9B44-2ABBB5BF0C1E}']
  end;

  [JavaSignature('android/app/ActionBar$OnMenuVisibilityListener')]
  JActionBar_OnMenuVisibilityListener = interface(IJavaInstance)
    ['{3BBAA971-1B13-4420-937F-D4DFEA9100FF}']
    procedure onMenuVisibilityChanged(isVisible: Boolean); cdecl;
  end;
  TJActionBar_OnMenuVisibilityListener = class(TJavaGenericImport<JActionBar_OnMenuVisibilityListenerClass, JActionBar_OnMenuVisibilityListener>) end;

  JActionBar_OnNavigationListenerClass = interface(IJavaClass)
    ['{AE55F295-D4A1-4ED2-989C-83DCA6409D2A}']
  end;

  [JavaSignature('android/app/ActionBar$OnNavigationListener')]
  JActionBar_OnNavigationListener = interface(IJavaInstance)
    ['{E16E1F51-1909-4712-B76A-0CB4F3BC178E}']
    function onNavigationItemSelected(itemPosition: Integer; itemId: Int64): Boolean; cdecl;
  end;
  TJActionBar_OnNavigationListener = class(TJavaGenericImport<JActionBar_OnNavigationListenerClass, JActionBar_OnNavigationListener>) end;

  JActionBar_TabClass = interface(JObjectClass)
    ['{142282F8-609C-42B5-8953-9BD4E01A9412}']
    {class} function _GetINVALID_POSITION: Integer; cdecl;
    {class} function init: JActionBar_Tab; cdecl;
    {class} property INVALID_POSITION: Integer read _GetINVALID_POSITION;
  end;

  [JavaSignature('android/app/ActionBar$Tab')]
  JActionBar_Tab = interface(JObject)
    ['{75064B3A-7834-47BD-9C0E-5FCBE4668F27}']
    function getContentDescription: JCharSequence; cdecl;
    function getCustomView: JView; cdecl;
    function getIcon: JDrawable; cdecl;
    function getPosition: Integer; cdecl;
    function getTag: JObject; cdecl;
    function getText: JCharSequence; cdecl;
    procedure select; cdecl;
    function setContentDescription(resId: Integer): JActionBar_Tab; cdecl; overload;
    function setContentDescription(contentDesc: JCharSequence): JActionBar_Tab; cdecl; overload;
    function setCustomView(view: JView): JActionBar_Tab; cdecl; overload;
    function setCustomView(layoutResId: Integer): JActionBar_Tab; cdecl; overload;
    function setIcon(icon: JDrawable): JActionBar_Tab; cdecl; overload;
    function setIcon(resId: Integer): JActionBar_Tab; cdecl; overload;
    function setTabListener(listener: JActionBar_TabListener): JActionBar_Tab; cdecl;
    function setTag(obj: JObject): JActionBar_Tab; cdecl;
    function setText(text: JCharSequence): JActionBar_Tab; cdecl; overload;
    function setText(resId: Integer): JActionBar_Tab; cdecl; overload;
  end;
  TJActionBar_Tab = class(TJavaGenericImport<JActionBar_TabClass, JActionBar_Tab>) end;

  JActionBar_TabListenerClass = interface(IJavaClass)
    ['{3F89E13F-E7E0-45A7-9A8F-D64422BA7B0E}']
  end;

  [JavaSignature('android/app/ActionBar$TabListener')]
  JActionBar_TabListener = interface(IJavaInstance)
    ['{C7B5769A-4307-4E57-8F73-31E022332836}']
    procedure onTabReselected(tab: JActionBar_Tab; ft: JFragmentTransaction); cdecl;
    procedure onTabSelected(tab: JActionBar_Tab; ft: JFragmentTransaction); cdecl;
    procedure onTabUnselected(tab: JActionBar_Tab; ft: JFragmentTransaction); cdecl;
  end;
  TJActionBar_TabListener = class(TJavaGenericImport<JActionBar_TabListenerClass, JActionBar_TabListener>) end;

  JActivityClass = interface(JContextThemeWrapperClass)
    ['{5269A525-12C7-4E15-AE63-4BD764EB357D}']
    {class} function _GetDEFAULT_KEYS_DIALER: Integer; cdecl;
    {class} function _GetDEFAULT_KEYS_DISABLE: Integer; cdecl;
    {class} function _GetDEFAULT_KEYS_SEARCH_GLOBAL: Integer; cdecl;
    {class} function _GetDEFAULT_KEYS_SEARCH_LOCAL: Integer; cdecl;
    {class} function _GetDEFAULT_KEYS_SHORTCUT: Integer; cdecl;
    {class} function _GetRESULT_CANCELED: Integer; cdecl;
    {class} function _GetRESULT_FIRST_USER: Integer; cdecl;
    {class} function _GetRESULT_OK: Integer; cdecl;
    {class} function init: JActivity; cdecl;
    {class} property DEFAULT_KEYS_DIALER: Integer read _GetDEFAULT_KEYS_DIALER;
    {class} property DEFAULT_KEYS_DISABLE: Integer read _GetDEFAULT_KEYS_DISABLE;
    {class} property DEFAULT_KEYS_SEARCH_GLOBAL: Integer read _GetDEFAULT_KEYS_SEARCH_GLOBAL;
    {class} property DEFAULT_KEYS_SEARCH_LOCAL: Integer read _GetDEFAULT_KEYS_SEARCH_LOCAL;
    {class} property DEFAULT_KEYS_SHORTCUT: Integer read _GetDEFAULT_KEYS_SHORTCUT;
    {class} property RESULT_CANCELED: Integer read _GetRESULT_CANCELED;
    {class} property RESULT_FIRST_USER: Integer read _GetRESULT_FIRST_USER;
    {class} property RESULT_OK: Integer read _GetRESULT_OK;
  end;

  [JavaSignature('android/app/Activity')]
  JActivity = interface(JContextThemeWrapper)
    ['{A91E133E-CBAB-48EB-99A8-AA047E90D3B8}']
    procedure addContentView(view: JView; params: JViewGroup_LayoutParams); cdecl;
    procedure closeContextMenu; cdecl;
    procedure closeOptionsMenu; cdecl;
    function createPendingResult(requestCode: Integer; data: JIntent; flags: Integer): JPendingIntent; cdecl;
    procedure dismissDialog(id: Integer); cdecl;//Deprecated
    procedure dismissKeyboardShortcutsHelper; cdecl;
    function dispatchGenericMotionEvent(ev: JMotionEvent): Boolean; cdecl;
    function dispatchKeyEvent(event: JKeyEvent): Boolean; cdecl;
    function dispatchKeyShortcutEvent(event: JKeyEvent): Boolean; cdecl;
    function dispatchPopulateAccessibilityEvent(event: JAccessibilityEvent): Boolean; cdecl;
    function dispatchTouchEvent(ev: JMotionEvent): Boolean; cdecl;
    function dispatchTrackballEvent(ev: JMotionEvent): Boolean; cdecl;
    procedure dump(prefix: JString; fd: JFileDescriptor; writer: JPrintWriter; args: TJavaObjectArray<JString>); cdecl;
    procedure enterPictureInPictureMode; cdecl; overload;//Deprecated
    function enterPictureInPictureMode(args: JPictureInPictureArgs): Boolean; cdecl; overload;//Deprecated
    function enterPictureInPictureMode(params: JPictureInPictureParams): Boolean; cdecl; overload;
    function findViewById(id: Integer): JView; cdecl;
    procedure finish; cdecl;
    procedure finishActivity(requestCode: Integer); cdecl;
    procedure finishActivityFromChild(child: JActivity; requestCode: Integer); cdecl;
    procedure finishAffinity; cdecl;
    procedure finishAfterTransition; cdecl;
    procedure finishAndRemoveTask; cdecl;
    procedure finishFromChild(child: JActivity); cdecl;
    function getActionBar: JActionBar; cdecl;
    function getApplication: JApplication; cdecl;
    function getCallingActivity: JComponentName; cdecl;
    function getCallingPackage: JString; cdecl;
    function getChangingConfigurations: Integer; cdecl;
    function getComponentName: JComponentName; cdecl;
    //function getContentScene: JScene; cdecl;
    //function getContentTransitionManager: JTransitionManager; cdecl;
    function getCurrentFocus: JView; cdecl;
    function getFragmentManager: JFragmentManager; cdecl;
    function getIntent: JIntent; cdecl;
    function getLastNonConfigurationInstance: JObject; cdecl;
    function getLayoutInflater: JLayoutInflater; cdecl;
    function getLoaderManager: JLoaderManager; cdecl;
    function getLocalClassName: JString; cdecl;
    function getMaxNumPictureInPictureActions: Integer; cdecl;
    //function getMediaController: Jsession_MediaController; cdecl;
    function getMenuInflater: JMenuInflater; cdecl;
    function getParent: JActivity; cdecl;
    function getParentActivityIntent: JIntent; cdecl;
    function getPreferences(mode: Integer): JSharedPreferences; cdecl;
    //function getReferrer: Jnet_Uri; cdecl;
    function getRequestedOrientation: Integer; cdecl;
    function getSearchEvent: JSearchEvent; cdecl;
    function getSystemService(name: JString): JObject; cdecl;
    function getTaskId: Integer; cdecl;
    function getTitle: JCharSequence; cdecl;
    function getTitleColor: Integer; cdecl;
    function getVoiceInteractor: JVoiceInteractor; cdecl;
    function getVolumeControlStream: Integer; cdecl;
    function getWindow: JWindow; cdecl;
    function getWindowManager: JWindowManager; cdecl;
    function hasWindowFocus: Boolean; cdecl;
    procedure invalidateOptionsMenu; cdecl;
    function isActivityTransitionRunning: Boolean; cdecl;
    function isChangingConfigurations: Boolean; cdecl;
    function isChild: Boolean; cdecl;
    function isDestroyed: Boolean; cdecl;
    function isFinishing: Boolean; cdecl;
    function isImmersive: Boolean; cdecl;
    function isInMultiWindowMode: Boolean; cdecl;
    function isInPictureInPictureMode: Boolean; cdecl;
    function isLocalVoiceInteractionSupported: Boolean; cdecl;
    function isTaskRoot: Boolean; cdecl;
    function isVoiceInteraction: Boolean; cdecl;
    function isVoiceInteractionRoot: Boolean; cdecl;
    //function managedQuery(uri: Jnet_Uri; projection: TJavaObjectArray<JString>; selection: JString; selectionArgs: TJavaObjectArray<JString>; sortOrder: JString): JCursor; cdecl;//Deprecated
    function moveTaskToBack(nonRoot: Boolean): Boolean; cdecl;
    function navigateUpTo(upIntent: JIntent): Boolean; cdecl;
    function navigateUpToFromChild(child: JActivity; upIntent: JIntent): Boolean; cdecl;
    procedure onActionModeFinished(mode: JActionMode); cdecl;
    procedure onActionModeStarted(mode: JActionMode); cdecl;
    procedure onActivityReenter(resultCode: Integer; data: JIntent); cdecl;
    procedure onAttachFragment(fragment: JFragment); cdecl;
    procedure onAttachedToWindow; cdecl;
    procedure onBackPressed; cdecl;
    procedure onConfigurationChanged(newConfig: JConfiguration); cdecl;
    procedure onContentChanged; cdecl;
    function onContextItemSelected(item: JMenuItem): Boolean; cdecl;
    procedure onContextMenuClosed(menu: JMenu); cdecl;
    procedure onCreate(savedInstanceState: JBundle; persistentState: JPersistableBundle); cdecl;
    procedure onCreateContextMenu(menu: JContextMenu; v: JView; menuInfo: JContextMenu_ContextMenuInfo); cdecl;
    function onCreateDescription: JCharSequence; cdecl;
    procedure onCreateNavigateUpTaskStack(builder: JTaskStackBuilder); cdecl;
    function onCreateOptionsMenu(menu: JMenu): Boolean; cdecl;
    function onCreatePanelMenu(featureId: Integer; menu: JMenu): Boolean; cdecl;
    function onCreatePanelView(featureId: Integer): JView; cdecl;
    function onCreateThumbnail(outBitmap: JBitmap; canvas: JCanvas): Boolean; cdecl;
    function onCreateView(name: JString; context: JContext; attrs: JAttributeSet): JView; cdecl; overload;
    function onCreateView(parent: JView; name: JString; context: JContext; attrs: JAttributeSet): JView; cdecl; overload;
    procedure onDetachedFromWindow; cdecl;
    procedure onEnterAnimationComplete; cdecl;
    function onGenericMotionEvent(event: JMotionEvent): Boolean; cdecl;
    function onKeyDown(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
    function onKeyLongPress(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
    function onKeyMultiple(keyCode: Integer; repeatCount: Integer; event: JKeyEvent): Boolean; cdecl;
    function onKeyShortcut(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
    function onKeyUp(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
    procedure onLocalVoiceInteractionStarted; cdecl;
    procedure onLocalVoiceInteractionStopped; cdecl;
    procedure onLowMemory; cdecl;
    function onMenuItemSelected(featureId: Integer; item: JMenuItem): Boolean; cdecl;
    function onMenuOpened(featureId: Integer; menu: JMenu): Boolean; cdecl;
    procedure onMultiWindowModeChanged(isInMultiWindowMode: Boolean; newConfig: JConfiguration); cdecl; overload;
    procedure onMultiWindowModeChanged(isInMultiWindowMode: Boolean); cdecl; overload;//Deprecated
    function onNavigateUp: Boolean; cdecl;
    function onNavigateUpFromChild(child: JActivity): Boolean; cdecl;
    function onOptionsItemSelected(item: JMenuItem): Boolean; cdecl;
    procedure onOptionsMenuClosed(menu: JMenu); cdecl;
    procedure onPanelClosed(featureId: Integer; menu: JMenu); cdecl;
    procedure onPictureInPictureModeChanged(isInPictureInPictureMode: Boolean; newConfig: JConfiguration); cdecl; overload;
    procedure onPictureInPictureModeChanged(isInPictureInPictureMode: Boolean); cdecl; overload;//Deprecated
    procedure onPostCreate(savedInstanceState: JBundle; persistentState: JPersistableBundle); cdecl;
    procedure onPrepareNavigateUpTaskStack(builder: JTaskStackBuilder); cdecl;
    function onPrepareOptionsMenu(menu: JMenu): Boolean; cdecl;
    function onPreparePanel(featureId: Integer; view: JView; menu: JMenu): Boolean; cdecl;
    procedure onProvideAssistContent(outContent: JAssistContent); cdecl;
    procedure onProvideAssistData(data: JBundle); cdecl;
    procedure onProvideKeyboardShortcuts(data: JList; menu: JMenu; deviceId: Integer); cdecl;
    //function onProvideReferrer: Jnet_Uri; cdecl;
    procedure onRequestPermissionsResult(requestCode: Integer; permissions: TJavaObjectArray<JString>; grantResults: TJavaArray<Integer>); cdecl;
    procedure onRestoreInstanceState(savedInstanceState: JBundle; persistentState: JPersistableBundle); cdecl;
    function onRetainNonConfigurationInstance: JObject; cdecl;
    procedure onSaveInstanceState(outState: JBundle; outPersistentState: JPersistableBundle); cdecl;
    function onSearchRequested(searchEvent: JSearchEvent): Boolean; cdecl; overload;
    function onSearchRequested: Boolean; cdecl; overload;
    procedure onStateNotSaved; cdecl;
    function onTouchEvent(event: JMotionEvent): Boolean; cdecl;
    function onTrackballEvent(event: JMotionEvent): Boolean; cdecl;
    procedure onTrimMemory(level: Integer); cdecl;
    procedure onUserInteraction; cdecl;
    procedure onVisibleBehindCanceled; cdecl;//Deprecated
    procedure onWindowAttributesChanged(params: JWindowManager_LayoutParams); cdecl;
    procedure onWindowFocusChanged(hasFocus: Boolean); cdecl;
    function onWindowStartingActionMode(callback: JActionMode_Callback): JActionMode; cdecl; overload;
    function onWindowStartingActionMode(callback: JActionMode_Callback; type_: Integer): JActionMode; cdecl; overload;
    procedure openContextMenu(view: JView); cdecl;
    procedure openOptionsMenu; cdecl;
    procedure overridePendingTransition(enterAnim: Integer; exitAnim: Integer); cdecl;
    procedure postponeEnterTransition; cdecl;
    procedure recreate; cdecl;
    procedure registerForContextMenu(view: JView); cdecl;
    function releaseInstance: Boolean; cdecl;
    procedure removeDialog(id: Integer); cdecl;//Deprecated
    procedure reportFullyDrawn; cdecl;
    function requestDragAndDropPermissions(event: JDragEvent): JDragAndDropPermissions; cdecl;
    procedure requestPermissions(permissions: TJavaObjectArray<JString>; requestCode: Integer); cdecl;
    procedure requestShowKeyboardShortcuts; cdecl;
    function requestVisibleBehind(visible: Boolean): Boolean; cdecl;//Deprecated
    function requestWindowFeature(featureId: Integer): Boolean; cdecl;
    procedure runOnUiThread(action: JRunnable); cdecl;
    procedure setActionBar(toolbar: JToolbar); cdecl;
    //procedure setContentTransitionManager(tm: JTransitionManager); cdecl;
    procedure setContentView(layoutResID: Integer); cdecl; overload;
    procedure setContentView(view: JView); cdecl; overload;
    procedure setContentView(view: JView; params: JViewGroup_LayoutParams); cdecl; overload;
    procedure setDefaultKeyMode(mode: Integer); cdecl;
    procedure setEnterSharedElementCallback(callback: JSharedElementCallback); cdecl;
    procedure setExitSharedElementCallback(callback: JSharedElementCallback); cdecl;
    procedure setFeatureDrawable(featureId: Integer; drawable: JDrawable); cdecl;
    procedure setFeatureDrawableAlpha(featureId: Integer; alpha: Integer); cdecl;
    procedure setFeatureDrawableResource(featureId: Integer; resId: Integer); cdecl;
    //procedure setFeatureDrawableUri(featureId: Integer; uri: Jnet_Uri); cdecl;
    procedure setFinishOnTouchOutside(finish: Boolean); cdecl;
    procedure setImmersive(i: Boolean); cdecl;
    procedure setIntent(newIntent: JIntent); cdecl;
    //procedure setMediaController(controller: Jsession_MediaController); cdecl;
    procedure setPictureInPictureArgs(args: JPictureInPictureArgs); cdecl;//Deprecated
    procedure setPictureInPictureParams(params: JPictureInPictureParams); cdecl;
    procedure setProgress(progress: Integer); cdecl;//Deprecated
    procedure setProgressBarIndeterminate(indeterminate: Boolean); cdecl;//Deprecated
    procedure setProgressBarIndeterminateVisibility(visible: Boolean); cdecl;//Deprecated
    procedure setProgressBarVisibility(visible: Boolean); cdecl;//Deprecated
    procedure setRequestedOrientation(requestedOrientation: Integer); cdecl;
    procedure setResult(resultCode: Integer); cdecl; overload;
    procedure setResult(resultCode: Integer; data: JIntent); cdecl; overload;
    procedure setSecondaryProgress(secondaryProgress: Integer); cdecl;//Deprecated
    procedure setTaskDescription(taskDescription: JActivityManager_TaskDescription); cdecl;
    procedure setTheme(resid: Integer); cdecl;
    procedure setTitle(title: JCharSequence); cdecl; overload;
    procedure setTitle(titleId: Integer); cdecl; overload;
    procedure setTitleColor(textColor: Integer); cdecl;//Deprecated
    procedure setVisible(visible: Boolean); cdecl;
    procedure setVolumeControlStream(streamType: Integer); cdecl;
    procedure setVrModeEnabled(enabled: Boolean; requestedComponent: JComponentName); cdecl;
    function shouldShowRequestPermissionRationale(permission: JString): Boolean; cdecl;
    function shouldUpRecreateTask(targetIntent: JIntent): Boolean; cdecl;
    function showAssist(args: JBundle): Boolean; cdecl;
    procedure showDialog(id: Integer); cdecl; overload;//Deprecated
    function showDialog(id: Integer; args: JBundle): Boolean; cdecl; overload;//Deprecated
    procedure showLockTaskEscapeMessage; cdecl;
    function startActionMode(callback: JActionMode_Callback): JActionMode; cdecl; overload;
    function startActionMode(callback: JActionMode_Callback; type_: Integer): JActionMode; cdecl; overload;
    procedure startActivities(intents: TJavaObjectArray<JIntent>); cdecl; overload;
    procedure startActivities(intents: TJavaObjectArray<JIntent>; options: JBundle); cdecl; overload;
    procedure startActivity(intent: JIntent); cdecl; overload;
    procedure startActivity(intent: JIntent; options: JBundle); cdecl; overload;
    procedure startActivityForResult(intent: JIntent; requestCode: Integer); cdecl; overload;
    procedure startActivityForResult(intent: JIntent; requestCode: Integer; options: JBundle); cdecl; overload;
    procedure startActivityFromChild(child: JActivity; intent: JIntent; requestCode: Integer); cdecl; overload;
    procedure startActivityFromChild(child: JActivity; intent: JIntent; requestCode: Integer; options: JBundle); cdecl; overload;
    procedure startActivityFromFragment(fragment: JFragment; intent: JIntent; requestCode: Integer); cdecl; overload;
    procedure startActivityFromFragment(fragment: JFragment; intent: JIntent; requestCode: Integer; options: JBundle); cdecl; overload;
    function startActivityIfNeeded(intent: JIntent; requestCode: Integer): Boolean; cdecl; overload;
    function startActivityIfNeeded(intent: JIntent; requestCode: Integer; options: JBundle): Boolean; cdecl; overload;
    procedure startIntentSender(intent: JIntentSender; fillInIntent: JIntent; flagsMask: Integer; flagsValues: Integer; extraFlags: Integer); cdecl; overload;
    procedure startIntentSender(intent: JIntentSender; fillInIntent: JIntent; flagsMask: Integer; flagsValues: Integer; extraFlags: Integer; options: JBundle); cdecl; overload;
    procedure startIntentSenderForResult(intent: JIntentSender; requestCode: Integer; fillInIntent: JIntent; flagsMask: Integer; flagsValues: Integer; extraFlags: Integer); cdecl; overload;
    procedure startIntentSenderForResult(intent: JIntentSender; requestCode: Integer; fillInIntent: JIntent; flagsMask: Integer; flagsValues: Integer; extraFlags: Integer; options: JBundle); cdecl; overload;
    procedure startIntentSenderFromChild(child: JActivity; intent: JIntentSender; requestCode: Integer; fillInIntent: JIntent; flagsMask: Integer; flagsValues: Integer; extraFlags: Integer); cdecl; overload;
    procedure startIntentSenderFromChild(child: JActivity; intent: JIntentSender; requestCode: Integer; fillInIntent: JIntent; flagsMask: Integer; flagsValues: Integer; extraFlags: Integer; options: JBundle); cdecl; overload;
    procedure startLocalVoiceInteraction(privateOptions: JBundle); cdecl;
    procedure startLockTask; cdecl;
    procedure startManagingCursor(c: JCursor); cdecl;//Deprecated
    function startNextMatchingActivity(intent: JIntent): Boolean; cdecl; overload;
    function startNextMatchingActivity(intent: JIntent; options: JBundle): Boolean; cdecl; overload;
    procedure startPostponedEnterTransition; cdecl;
    procedure startSearch(initialQuery: JString; selectInitialQuery: Boolean; appSearchData: JBundle; globalSearch: Boolean); cdecl;
    procedure stopLocalVoiceInteraction; cdecl;
    procedure stopLockTask; cdecl;
    procedure stopManagingCursor(c: JCursor); cdecl;//Deprecated
    procedure takeKeyEvents(get_: Boolean); cdecl;
    procedure triggerSearch(query: JString; appSearchData: JBundle); cdecl;
    procedure unregisterForContextMenu(view: JView); cdecl;
  end;
  TJActivity = class(TJavaGenericImport<JActivityClass, JActivity>) end;

  JActivityManagerClass = interface(JObjectClass)
    ['{4C501FA2-A977-439E-A82A-538A3A87BCBE}']
    {class} function _GetACTION_REPORT_HEAP_LIMIT: JString; cdecl;
    {class} function _GetLOCK_TASK_MODE_LOCKED: Integer; cdecl;
    {class} function _GetLOCK_TASK_MODE_NONE: Integer; cdecl;
    {class} function _GetLOCK_TASK_MODE_PINNED: Integer; cdecl;
    {class} function _GetMETA_HOME_ALTERNATE: JString; cdecl;
    {class} function _GetMOVE_TASK_NO_USER_ACTION: Integer; cdecl;
    {class} function _GetMOVE_TASK_WITH_HOME: Integer; cdecl;
    {class} function _GetRECENT_IGNORE_UNAVAILABLE: Integer; cdecl;
    {class} function _GetRECENT_WITH_EXCLUDED: Integer; cdecl;
    {class} function getMaxNumPictureInPictureActions: Integer; cdecl;//Deprecated
    {class} procedure getMyMemoryState(outState: JActivityManager_RunningAppProcessInfo); cdecl;
    {class} function isRunningInTestHarness: Boolean; cdecl;
    {class} function isUserAMonkey: Boolean; cdecl;
    {class} procedure setVrThread(tid: Integer); cdecl;
    {class} property ACTION_REPORT_HEAP_LIMIT: JString read _GetACTION_REPORT_HEAP_LIMIT;
    {class} property LOCK_TASK_MODE_LOCKED: Integer read _GetLOCK_TASK_MODE_LOCKED;
    {class} property LOCK_TASK_MODE_NONE: Integer read _GetLOCK_TASK_MODE_NONE;
    {class} property LOCK_TASK_MODE_PINNED: Integer read _GetLOCK_TASK_MODE_PINNED;
    {class} property META_HOME_ALTERNATE: JString read _GetMETA_HOME_ALTERNATE;
    {class} property MOVE_TASK_NO_USER_ACTION: Integer read _GetMOVE_TASK_NO_USER_ACTION;
    {class} property MOVE_TASK_WITH_HOME: Integer read _GetMOVE_TASK_WITH_HOME;
    {class} property RECENT_IGNORE_UNAVAILABLE: Integer read _GetRECENT_IGNORE_UNAVAILABLE;
    {class} property RECENT_WITH_EXCLUDED: Integer read _GetRECENT_WITH_EXCLUDED;
  end;

  [JavaSignature('android/app/ActivityManager')]
  JActivityManager = interface(JObject)
    ['{F4D6753D-F28A-4E3E-87A0-64B46C89C6EB}']
    function addAppTask(activity: JActivity; intent: JIntent; description: JActivityManager_TaskDescription; thumbnail: JBitmap): Integer; cdecl;
    function clearApplicationUserData: Boolean; cdecl;
    procedure clearWatchHeapLimit; cdecl;
    procedure dumpPackageState(fd: JFileDescriptor; packageName: JString); cdecl;
    function getAppTaskThumbnailSize: Jutil_Size; cdecl;
    function getAppTasks: JList; cdecl;
    function getDeviceConfigurationInfo: JConfigurationInfo; cdecl;
    function getLargeMemoryClass: Integer; cdecl;
    function getLauncherLargeIconDensity: Integer; cdecl;
    function getLauncherLargeIconSize: Integer; cdecl;
    function getLockTaskModeState: Integer; cdecl;
    function getMemoryClass: Integer; cdecl;
    procedure getMemoryInfo(outInfo: JActivityManager_MemoryInfo); cdecl;
    function getProcessMemoryInfo(pids: TJavaArray<Integer>): TJavaObjectArray<JDebug_MemoryInfo>; cdecl;
    function getProcessesInErrorState: JList; cdecl;
    function getRecentTasks(maxNum: Integer; flags: Integer): JList; cdecl;//Deprecated
    function getRunningAppProcesses: JList; cdecl;
    function getRunningServiceControlPanel(service: JComponentName): JPendingIntent; cdecl;
    function getRunningServices(maxNum: Integer): JList; cdecl;//Deprecated
    function getRunningTasks(maxNum: Integer): JList; cdecl;//Deprecated
    function isInLockTaskMode: Boolean; cdecl;//Deprecated
    function isLowRamDevice: Boolean; cdecl;
    procedure killBackgroundProcesses(packageName: JString); cdecl;
    procedure moveTaskToFront(taskId: Integer; flags: Integer); cdecl; overload;
    procedure moveTaskToFront(taskId: Integer; flags: Integer; options: JBundle); cdecl; overload;
    procedure restartPackage(packageName: JString); cdecl;//Deprecated
    procedure setWatchHeapLimit(pssSize: Int64); cdecl;
  end;
  TJActivityManager = class(TJavaGenericImport<JActivityManagerClass, JActivityManager>) end;

  JActivityManager_MemoryInfoClass = interface(JObjectClass)
    ['{647AEED3-4AE3-48B7-88F1-ADD7DA84EA6E}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function init: JActivityManager_MemoryInfo; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('android/app/ActivityManager$MemoryInfo')]
  JActivityManager_MemoryInfo = interface(JObject)
    ['{1A52DA44-8A7A-4C46-BE08-20C6190D593C}']
    function _GetavailMem: Int64; cdecl;
    procedure _SetavailMem(Value: Int64); cdecl;
    function _GetlowMemory: Boolean; cdecl;
    procedure _SetlowMemory(Value: Boolean); cdecl;
    function _Getthreshold: Int64; cdecl;
    procedure _Setthreshold(Value: Int64); cdecl;
    function _GettotalMem: Int64; cdecl;
    procedure _SettotalMem(Value: Int64); cdecl;
    function describeContents: Integer; cdecl;
    procedure readFromParcel(source: JParcel); cdecl;
    procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
    property availMem: Int64 read _GetavailMem write _SetavailMem;
    property lowMemory: Boolean read _GetlowMemory write _SetlowMemory;
    property threshold: Int64 read _Getthreshold write _Setthreshold;
    property totalMem: Int64 read _GettotalMem write _SettotalMem;
  end;
  TJActivityManager_MemoryInfo = class(TJavaGenericImport<JActivityManager_MemoryInfoClass, JActivityManager_MemoryInfo>) end;

  JActivityManager_RunningAppProcessInfoClass = interface(JObjectClass)
    ['{92E3280A-D655-40EA-99A4-08FFEC6BD1C1}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function _GetIMPORTANCE_BACKGROUND: Integer; cdecl;
    {class} function _GetIMPORTANCE_CACHED: Integer; cdecl;
    {class} function _GetIMPORTANCE_EMPTY: Integer; cdecl;
    {class} function _GetIMPORTANCE_FOREGROUND: Integer; cdecl;
    {class} function _GetIMPORTANCE_FOREGROUND_SERVICE: Integer; cdecl;
    {class} function _GetIMPORTANCE_GONE: Integer; cdecl;
    {class} function _GetIMPORTANCE_PERCEPTIBLE: Integer; cdecl;
    {class} function _GetIMPORTANCE_PERCEPTIBLE_PRE_26: Integer; cdecl;
    {class} function _GetIMPORTANCE_SERVICE: Integer; cdecl;
    {class} function _GetIMPORTANCE_TOP_SLEEPING: Integer; cdecl;
    {class} function _GetIMPORTANCE_VISIBLE: Integer; cdecl;
    {class} function _GetREASON_PROVIDER_IN_USE: Integer; cdecl;
    {class} function _GetREASON_SERVICE_IN_USE: Integer; cdecl;
    {class} function _GetREASON_UNKNOWN: Integer; cdecl;
    {class} function init: JActivityManager_RunningAppProcessInfo; cdecl; overload;
    {class} function init(pProcessName: JString; pPid: Integer; pArr: TJavaObjectArray<JString>): JActivityManager_RunningAppProcessInfo; cdecl; overload;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
    {class} property IMPORTANCE_BACKGROUND: Integer read _GetIMPORTANCE_BACKGROUND;
    {class} property IMPORTANCE_CACHED: Integer read _GetIMPORTANCE_CACHED;
    {class} property IMPORTANCE_EMPTY: Integer read _GetIMPORTANCE_EMPTY;
    {class} property IMPORTANCE_FOREGROUND: Integer read _GetIMPORTANCE_FOREGROUND;
    {class} property IMPORTANCE_FOREGROUND_SERVICE: Integer read _GetIMPORTANCE_FOREGROUND_SERVICE;
    {class} property IMPORTANCE_GONE: Integer read _GetIMPORTANCE_GONE;
    {class} property IMPORTANCE_PERCEPTIBLE: Integer read _GetIMPORTANCE_PERCEPTIBLE;
    {class} property IMPORTANCE_PERCEPTIBLE_PRE_26: Integer read _GetIMPORTANCE_PERCEPTIBLE_PRE_26;
    {class} property IMPORTANCE_SERVICE: Integer read _GetIMPORTANCE_SERVICE;
    {class} property IMPORTANCE_TOP_SLEEPING: Integer read _GetIMPORTANCE_TOP_SLEEPING;
    {class} property IMPORTANCE_VISIBLE: Integer read _GetIMPORTANCE_VISIBLE;
    {class} property REASON_PROVIDER_IN_USE: Integer read _GetREASON_PROVIDER_IN_USE;
    {class} property REASON_SERVICE_IN_USE: Integer read _GetREASON_SERVICE_IN_USE;
    {class} property REASON_UNKNOWN: Integer read _GetREASON_UNKNOWN;
  end;

  [JavaSignature('android/app/ActivityManager$RunningAppProcessInfo')]
  JActivityManager_RunningAppProcessInfo = interface(JObject)
    ['{247C91D3-5F45-4EA5-8D68-3392FB0D96C2}']
    function _Getimportance: Integer; cdecl;
    procedure _Setimportance(Value: Integer); cdecl;
    function _GetimportanceReasonCode: Integer; cdecl;
    procedure _SetimportanceReasonCode(Value: Integer); cdecl;
    function _GetimportanceReasonComponent: JComponentName; cdecl;
    procedure _SetimportanceReasonComponent(Value: JComponentName); cdecl;
    function _GetimportanceReasonPid: Integer; cdecl;
    procedure _SetimportanceReasonPid(Value: Integer); cdecl;
    function _GetlastTrimLevel: Integer; cdecl;
    procedure _SetlastTrimLevel(Value: Integer); cdecl;
    function _Getlru: Integer; cdecl;
    procedure _Setlru(Value: Integer); cdecl;
    function _Getpid: Integer; cdecl;
    procedure _Setpid(Value: Integer); cdecl;
    function _GetpkgList: TJavaObjectArray<JString>; cdecl;
    procedure _SetpkgList(Value: TJavaObjectArray<JString>); cdecl;
    function _GetprocessName: JString; cdecl;
    procedure _SetprocessName(Value: JString); cdecl;
    function _Getuid: Integer; cdecl;
    procedure _Setuid(Value: Integer); cdecl;
    function describeContents: Integer; cdecl;
    procedure readFromParcel(source: JParcel); cdecl;
    procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
    property importance: Integer read _Getimportance write _Setimportance;
    property importanceReasonCode: Integer read _GetimportanceReasonCode write _SetimportanceReasonCode;
    property importanceReasonComponent: JComponentName read _GetimportanceReasonComponent write _SetimportanceReasonComponent;
    property importanceReasonPid: Integer read _GetimportanceReasonPid write _SetimportanceReasonPid;
    property lastTrimLevel: Integer read _GetlastTrimLevel write _SetlastTrimLevel;
    property lru: Integer read _Getlru write _Setlru;
    property pid: Integer read _Getpid write _Setpid;
    property pkgList: TJavaObjectArray<JString> read _GetpkgList write _SetpkgList;
    property processName: JString read _GetprocessName write _SetprocessName;
    property uid: Integer read _Getuid write _Setuid;
  end;
  TJActivityManager_RunningAppProcessInfo = class(TJavaGenericImport<JActivityManager_RunningAppProcessInfoClass, JActivityManager_RunningAppProcessInfo>) end;

  JActivityManager_TaskDescriptionClass = interface(JObjectClass)
    ['{CD8A19ED-7D7A-442C-90DE-2AF76B691643}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function init(label_: JString; icon: JBitmap; colorPrimary: Integer): JActivityManager_TaskDescription; cdecl; overload;
    {class} function init(label_: JString; icon: JBitmap): JActivityManager_TaskDescription; cdecl; overload;
    {class} function init(label_: JString): JActivityManager_TaskDescription; cdecl; overload;
    {class} function init: JActivityManager_TaskDescription; cdecl; overload;
    {class} function init(td: JActivityManager_TaskDescription): JActivityManager_TaskDescription; cdecl; overload;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('android/app/ActivityManager$TaskDescription')]
  JActivityManager_TaskDescription = interface(JObject)
    ['{132EDB47-BAFB-4AF0-A3CD-4B6743FB2BAB}']
    function describeContents: Integer; cdecl;
    function getIcon: JBitmap; cdecl;
    function getLabel: JString; cdecl;
    function getPrimaryColor: Integer; cdecl;
    procedure readFromParcel(source: JParcel); cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
  end;
  TJActivityManager_TaskDescription = class(TJavaGenericImport<JActivityManager_TaskDescriptionClass, JActivityManager_TaskDescription>) end;

  JAlarmManagerClass = interface(JObjectClass)
    ['{B26537D0-A769-4DD1-A5C3-2E2D9344193B}']
    {class} function _GetACTION_NEXT_ALARM_CLOCK_CHANGED: JString; cdecl;
    {class} function _GetELAPSED_REALTIME: Integer; cdecl;
    {class} function _GetELAPSED_REALTIME_WAKEUP: Integer; cdecl;
    {class} function _GetINTERVAL_DAY: Int64; cdecl;
    {class} function _GetINTERVAL_FIFTEEN_MINUTES: Int64; cdecl;
    {class} function _GetINTERVAL_HALF_DAY: Int64; cdecl;
    {class} function _GetINTERVAL_HALF_HOUR: Int64; cdecl;
    {class} function _GetINTERVAL_HOUR: Int64; cdecl;
    {class} function _GetRTC: Integer; cdecl;
    {class} function _GetRTC_WAKEUP: Integer; cdecl;
    {class} property ACTION_NEXT_ALARM_CLOCK_CHANGED: JString read _GetACTION_NEXT_ALARM_CLOCK_CHANGED;
    {class} property ELAPSED_REALTIME: Integer read _GetELAPSED_REALTIME;
    {class} property ELAPSED_REALTIME_WAKEUP: Integer read _GetELAPSED_REALTIME_WAKEUP;
    {class} property INTERVAL_DAY: Int64 read _GetINTERVAL_DAY;
    {class} property INTERVAL_FIFTEEN_MINUTES: Int64 read _GetINTERVAL_FIFTEEN_MINUTES;
    {class} property INTERVAL_HALF_DAY: Int64 read _GetINTERVAL_HALF_DAY;
    {class} property INTERVAL_HALF_HOUR: Int64 read _GetINTERVAL_HALF_HOUR;
    {class} property INTERVAL_HOUR: Int64 read _GetINTERVAL_HOUR;
    {class} property RTC: Integer read _GetRTC;
    {class} property RTC_WAKEUP: Integer read _GetRTC_WAKEUP;
  end;

  [JavaSignature('android/app/AlarmManager')]
  JAlarmManager = interface(JObject)
    ['{D4B2A2E3-48AD-491C-823B-DC301F6DA456}']
    procedure cancel(operation: JPendingIntent); cdecl; overload;
    procedure cancel(listener: JAlarmManager_OnAlarmListener); cdecl; overload;
    function getNextAlarmClock: JAlarmManager_AlarmClockInfo; cdecl;
    procedure &set(type_: Integer; triggerAtMillis: Int64; operation: JPendingIntent); cdecl; overload;
    procedure &set(type_: Integer; triggerAtMillis: Int64; tag: JString; listener: JAlarmManager_OnAlarmListener; targetHandler: JHandler); cdecl; overload;
    procedure setAlarmClock(info: JAlarmManager_AlarmClockInfo; operation: JPendingIntent); cdecl;
    procedure setAndAllowWhileIdle(type_: Integer; triggerAtMillis: Int64; operation: JPendingIntent); cdecl;
    procedure setExact(type_: Integer; triggerAtMillis: Int64; operation: JPendingIntent); cdecl; overload;
    procedure setExact(type_: Integer; triggerAtMillis: Int64; tag: JString; listener: JAlarmManager_OnAlarmListener; targetHandler: JHandler); cdecl; overload;
    procedure setExactAndAllowWhileIdle(type_: Integer; triggerAtMillis: Int64; operation: JPendingIntent); cdecl;
    procedure setInexactRepeating(type_: Integer; triggerAtMillis: Int64; intervalMillis: Int64; operation: JPendingIntent); cdecl;
    procedure setRepeating(type_: Integer; triggerAtMillis: Int64; intervalMillis: Int64; operation: JPendingIntent); cdecl;
    procedure setTime(millis: Int64); cdecl;
    procedure setTimeZone(timeZone: JString); cdecl;
    procedure setWindow(type_: Integer; windowStartMillis: Int64; windowLengthMillis: Int64; operation: JPendingIntent); cdecl; overload;
    procedure setWindow(type_: Integer; windowStartMillis: Int64; windowLengthMillis: Int64; tag: JString; listener: JAlarmManager_OnAlarmListener; targetHandler: JHandler); cdecl; overload;
  end;
  TJAlarmManager = class(TJavaGenericImport<JAlarmManagerClass, JAlarmManager>) end;

  JAlarmManager_AlarmClockInfoClass = interface(JObjectClass)
    ['{39D4619E-2EDE-4B79-8BF2-C3086B93CAED}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function init(triggerTime: Int64; showIntent: JPendingIntent): JAlarmManager_AlarmClockInfo; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('android/app/AlarmManager$AlarmClockInfo')]
  JAlarmManager_AlarmClockInfo = interface(JObject)
    ['{8111AA00-8405-46AC-AA53-EB6B2180F109}']
    function describeContents: Integer; cdecl;
    function getShowIntent: JPendingIntent; cdecl;
    function getTriggerTime: Int64; cdecl;
    procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
  end;
  TJAlarmManager_AlarmClockInfo = class(TJavaGenericImport<JAlarmManager_AlarmClockInfoClass, JAlarmManager_AlarmClockInfo>) end;

  JAlarmManager_OnAlarmListenerClass = interface(IJavaClass)
    ['{9250883C-E848-4F85-A019-1698209522A6}']
  end;

  [JavaSignature('android/app/AlarmManager$OnAlarmListener')]
  JAlarmManager_OnAlarmListener = interface(IJavaInstance)
    ['{77AB3FF1-DF96-4CD9-A8AA-D0922DA74926}']
    procedure onAlarm; cdecl;
  end;
  TJAlarmManager_OnAlarmListener = class(TJavaGenericImport<JAlarmManager_OnAlarmListenerClass, JAlarmManager_OnAlarmListener>) end;

  JDialogClass = interface(JObjectClass)
    ['{92218268-7592-4242-B041-BE3C84FFC844}']
    {class} function init(context: JContext): JDialog; cdecl; overload;
    {class} function init(context: JContext; themeResId: Integer): JDialog; cdecl; overload;
  end;

  [JavaSignature('android/app/Dialog')]
  JDialog = interface(JObject)
    ['{71959C70-0CB3-42B3-9654-30005BAEB3F5}']
    procedure addContentView(view: JView; params: JViewGroup_LayoutParams); cdecl;
    procedure cancel; cdecl;
    procedure closeOptionsMenu; cdecl;
    procedure create; cdecl;
    procedure dismiss; cdecl;
    function dispatchGenericMotionEvent(ev: JMotionEvent): Boolean; cdecl;
    function dispatchKeyEvent(event: JKeyEvent): Boolean; cdecl;
    function dispatchKeyShortcutEvent(event: JKeyEvent): Boolean; cdecl;
    function dispatchPopulateAccessibilityEvent(event: JAccessibilityEvent): Boolean; cdecl;
    function dispatchTouchEvent(ev: JMotionEvent): Boolean; cdecl;
    function dispatchTrackballEvent(ev: JMotionEvent): Boolean; cdecl;
    function findViewById(id: Integer): JView; cdecl;
    function getActionBar: JActionBar; cdecl;
    function getContext: JContext; cdecl;
    function getCurrentFocus: JView; cdecl;
    function getLayoutInflater: JLayoutInflater; cdecl;
    function getOwnerActivity: JActivity; cdecl;
    function getSearchEvent: JSearchEvent; cdecl;
    function getVolumeControlStream: Integer; cdecl;
    function getWindow: JWindow; cdecl;
    procedure hide; cdecl;
    procedure invalidateOptionsMenu; cdecl;
    function isShowing: Boolean; cdecl;
    procedure onActionModeFinished(mode: JActionMode); cdecl;
    procedure onActionModeStarted(mode: JActionMode); cdecl;
    procedure onAttachedToWindow; cdecl;
    procedure onBackPressed; cdecl;
    procedure onContentChanged; cdecl;
    function onContextItemSelected(item: JMenuItem): Boolean; cdecl;
    procedure onContextMenuClosed(menu: JMenu); cdecl;
    procedure onCreateContextMenu(menu: JContextMenu; v: JView; menuInfo: JContextMenu_ContextMenuInfo); cdecl;
    function onCreateOptionsMenu(menu: JMenu): Boolean; cdecl;
    function onCreatePanelMenu(featureId: Integer; menu: JMenu): Boolean; cdecl;
    function onCreatePanelView(featureId: Integer): JView; cdecl;
    procedure onDetachedFromWindow; cdecl;
    function onGenericMotionEvent(event: JMotionEvent): Boolean; cdecl;
    function onKeyDown(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
    function onKeyLongPress(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
    function onKeyMultiple(keyCode: Integer; repeatCount: Integer; event: JKeyEvent): Boolean; cdecl;
    function onKeyShortcut(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
    function onKeyUp(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
    function onMenuItemSelected(featureId: Integer; item: JMenuItem): Boolean; cdecl;
    function onMenuOpened(featureId: Integer; menu: JMenu): Boolean; cdecl;
    function onOptionsItemSelected(item: JMenuItem): Boolean; cdecl;
    procedure onOptionsMenuClosed(menu: JMenu); cdecl;
    procedure onPanelClosed(featureId: Integer; menu: JMenu); cdecl;
    function onPrepareOptionsMenu(menu: JMenu): Boolean; cdecl;
    function onPreparePanel(featureId: Integer; view: JView; menu: JMenu): Boolean; cdecl;
    procedure onRestoreInstanceState(savedInstanceState: JBundle); cdecl;
    function onSaveInstanceState: JBundle; cdecl;
    function onSearchRequested(searchEvent: JSearchEvent): Boolean; cdecl; overload;
    function onSearchRequested: Boolean; cdecl; overload;
    function onTouchEvent(event: JMotionEvent): Boolean; cdecl;
    function onTrackballEvent(event: JMotionEvent): Boolean; cdecl;
    procedure onWindowAttributesChanged(params: JWindowManager_LayoutParams); cdecl;
    procedure onWindowFocusChanged(hasFocus: Boolean); cdecl;
    function onWindowStartingActionMode(callback: JActionMode_Callback): JActionMode; cdecl; overload;
    function onWindowStartingActionMode(callback: JActionMode_Callback; type_: Integer): JActionMode; cdecl; overload;
    procedure openContextMenu(view: JView); cdecl;
    procedure openOptionsMenu; cdecl;
    procedure registerForContextMenu(view: JView); cdecl;
    function requestWindowFeature(featureId: Integer): Boolean; cdecl;
    procedure setCancelMessage(msg: JMessage); cdecl;
    procedure setCancelable(flag: Boolean); cdecl;
    procedure setCanceledOnTouchOutside(cancel: Boolean); cdecl;
    procedure setContentView(layoutResID: Integer); cdecl; overload;
    procedure setContentView(view: JView); cdecl; overload;
    procedure setContentView(view: JView; params: JViewGroup_LayoutParams); cdecl; overload;
    procedure setDismissMessage(msg: JMessage); cdecl;
    procedure setFeatureDrawable(featureId: Integer; drawable: JDrawable); cdecl;
    procedure setFeatureDrawableAlpha(featureId: Integer; alpha: Integer); cdecl;
    procedure setFeatureDrawableResource(featureId: Integer; resId: Integer); cdecl;
    //procedure setFeatureDrawableUri(featureId: Integer; uri: Jnet_Uri); cdecl;
    procedure setOnCancelListener(listener: JDialogInterface_OnCancelListener); cdecl;
    procedure setOnDismissListener(listener: JDialogInterface_OnDismissListener); cdecl;
    procedure setOnKeyListener(onKeyListener: JDialogInterface_OnKeyListener); cdecl;
    procedure setOnShowListener(listener: JDialogInterface_OnShowListener); cdecl;
    procedure setOwnerActivity(activity: JActivity); cdecl;
    procedure setTitle(title: JCharSequence); cdecl; overload;
    procedure setTitle(titleId: Integer); cdecl; overload;
    procedure setVolumeControlStream(streamType: Integer); cdecl;
    procedure show; cdecl;
    procedure takeKeyEvents(get_: Boolean); cdecl;
    procedure unregisterForContextMenu(view: JView); cdecl;
  end;
  TJDialog = class(TJavaGenericImport<JDialogClass, JDialog>) end;

  JAlertDialogClass = interface(JDialogClass)
    ['{36BE871D-2AD4-4CA9-8524-C14BEB73AC56}']
    {class} function _GetTHEME_DEVICE_DEFAULT_DARK: Integer; cdecl;
    {class} function _GetTHEME_DEVICE_DEFAULT_LIGHT: Integer; cdecl;
    {class} function _GetTHEME_HOLO_DARK: Integer; cdecl;
    {class} function _GetTHEME_HOLO_LIGHT: Integer; cdecl;
    {class} function _GetTHEME_TRADITIONAL: Integer; cdecl;
    {class} property THEME_DEVICE_DEFAULT_DARK: Integer read _GetTHEME_DEVICE_DEFAULT_DARK;
    {class} property THEME_DEVICE_DEFAULT_LIGHT: Integer read _GetTHEME_DEVICE_DEFAULT_LIGHT;
    {class} property THEME_HOLO_DARK: Integer read _GetTHEME_HOLO_DARK;
    {class} property THEME_HOLO_LIGHT: Integer read _GetTHEME_HOLO_LIGHT;
    {class} property THEME_TRADITIONAL: Integer read _GetTHEME_TRADITIONAL;
  end;

  [JavaSignature('android/app/AlertDialog')]
  JAlertDialog = interface(JDialog)
    ['{53DA68C7-828A-4527-8A92-1BC09462F4E2}']
    function getButton(whichButton: Integer): JButton; cdecl;
    function getListView: JListView; cdecl;
    function onKeyDown(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
    function onKeyUp(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
    procedure setButton(whichButton: Integer; text: JCharSequence; msg: JMessage); cdecl; overload;
    procedure setButton(whichButton: Integer; text: JCharSequence; listener: JDialogInterface_OnClickListener); cdecl; overload;
    procedure setButton(text: JCharSequence; msg: JMessage); cdecl; overload;//Deprecated
    procedure setButton(text: JCharSequence; listener: JDialogInterface_OnClickListener); cdecl; overload;//Deprecated
    procedure setButton2(text: JCharSequence; msg: JMessage); cdecl; overload;//Deprecated
    procedure setButton2(text: JCharSequence; listener: JDialogInterface_OnClickListener); cdecl; overload;//Deprecated
    procedure setButton3(text: JCharSequence; msg: JMessage); cdecl; overload;//Deprecated
    procedure setButton3(text: JCharSequence; listener: JDialogInterface_OnClickListener); cdecl; overload;//Deprecated
    procedure setCustomTitle(customTitleView: JView); cdecl;
    procedure setIcon(resId: Integer); cdecl; overload;
    procedure setIcon(icon: JDrawable); cdecl; overload;
    procedure setIconAttribute(attrId: Integer); cdecl;
    procedure setInverseBackgroundForced(forceInverseBackground: Boolean); cdecl;
    procedure setMessage(message: JCharSequence); cdecl;
    procedure setTitle(title: JCharSequence); cdecl;
    procedure setView(view: JView); cdecl; overload;
    procedure setView(view: JView; viewSpacingLeft: Integer; viewSpacingTop: Integer; viewSpacingRight: Integer; viewSpacingBottom: Integer); cdecl; overload;
  end;
  TJAlertDialog = class(TJavaGenericImport<JAlertDialogClass, JAlertDialog>) end;

  JAlertDialog_BuilderClass = interface(JObjectClass)
    ['{69C77054-20E9-4492-BCBB-FB1F9F062BFD}']
    {class} function init(context: JContext): JAlertDialog_Builder; cdecl; overload;
    {class} function init(context: JContext; themeResId: Integer): JAlertDialog_Builder; cdecl; overload;
  end;

  [JavaSignature('android/app/AlertDialog$Builder')]
  JAlertDialog_Builder = interface(JObject)
    ['{25BC41E2-CA0A-4EA8-A854-993A0E86087C}']
    function create: JAlertDialog; cdecl;
    function getContext: JContext; cdecl;
    function setAdapter(adapter: JListAdapter; listener: JDialogInterface_OnClickListener): JAlertDialog_Builder; cdecl;
    function setCancelable(cancelable: Boolean): JAlertDialog_Builder; cdecl;
    function setCursor(cursor: JCursor; listener: JDialogInterface_OnClickListener; labelColumn: JString): JAlertDialog_Builder; cdecl;
    function setCustomTitle(customTitleView: JView): JAlertDialog_Builder; cdecl;
    function setIcon(iconId: Integer): JAlertDialog_Builder; cdecl; overload;
    function setIcon(icon: JDrawable): JAlertDialog_Builder; cdecl; overload;
    function setIconAttribute(attrId: Integer): JAlertDialog_Builder; cdecl;
    function setInverseBackgroundForced(useInverseBackground: Boolean): JAlertDialog_Builder; cdecl;//Deprecated
    function setItems(itemsId: Integer; listener: JDialogInterface_OnClickListener): JAlertDialog_Builder; cdecl; overload;
    function setItems(items: TJavaObjectArray<JCharSequence>; listener: JDialogInterface_OnClickListener): JAlertDialog_Builder; cdecl; overload;
    function setMessage(messageId: Integer): JAlertDialog_Builder; cdecl; overload;
    function setMessage(message: JCharSequence): JAlertDialog_Builder; cdecl; overload;
    function setMultiChoiceItems(itemsId: Integer; checkedItems: TJavaArray<Boolean>; listener: JDialogInterface_OnMultiChoiceClickListener): JAlertDialog_Builder; cdecl; overload;
    function setMultiChoiceItems(items: TJavaObjectArray<JCharSequence>; checkedItems: TJavaArray<Boolean>; listener: JDialogInterface_OnMultiChoiceClickListener): JAlertDialog_Builder; cdecl; overload;
    function setMultiChoiceItems(cursor: JCursor; isCheckedColumn: JString; labelColumn: JString; listener: JDialogInterface_OnMultiChoiceClickListener): JAlertDialog_Builder; cdecl; overload;
    function setNegativeButton(textId: Integer; listener: JDialogInterface_OnClickListener): JAlertDialog_Builder; cdecl; overload;
    function setNegativeButton(text: JCharSequence; listener: JDialogInterface_OnClickListener): JAlertDialog_Builder; cdecl; overload;
    function setNeutralButton(textId: Integer; listener: JDialogInterface_OnClickListener): JAlertDialog_Builder; cdecl; overload;
    function setNeutralButton(text: JCharSequence; listener: JDialogInterface_OnClickListener): JAlertDialog_Builder; cdecl; overload;
    function setOnCancelListener(onCancelListener: JDialogInterface_OnCancelListener): JAlertDialog_Builder; cdecl;
    function setOnDismissListener(onDismissListener: JDialogInterface_OnDismissListener): JAlertDialog_Builder; cdecl;
    function setOnItemSelectedListener(listener: JAdapterView_OnItemSelectedListener): JAlertDialog_Builder; cdecl;
    function setOnKeyListener(onKeyListener: JDialogInterface_OnKeyListener): JAlertDialog_Builder; cdecl;
    function setPositiveButton(textId: Integer; listener: JDialogInterface_OnClickListener): JAlertDialog_Builder; cdecl; overload;
    function setPositiveButton(text: JCharSequence; listener: JDialogInterface_OnClickListener): JAlertDialog_Builder; cdecl; overload;
    function setSingleChoiceItems(itemsId: Integer; checkedItem: Integer; listener: JDialogInterface_OnClickListener): JAlertDialog_Builder; cdecl; overload;
    function setSingleChoiceItems(cursor: JCursor; checkedItem: Integer; labelColumn: JString; listener: JDialogInterface_OnClickListener): JAlertDialog_Builder; cdecl; overload;
    function setSingleChoiceItems(items: TJavaObjectArray<JCharSequence>; checkedItem: Integer; listener: JDialogInterface_OnClickListener): JAlertDialog_Builder; cdecl; overload;
    function setSingleChoiceItems(adapter: JListAdapter; checkedItem: Integer; listener: JDialogInterface_OnClickListener): JAlertDialog_Builder; cdecl; overload;
    function setTitle(titleId: Integer): JAlertDialog_Builder; cdecl; overload;
    function setTitle(title: JCharSequence): JAlertDialog_Builder; cdecl; overload;
    function setView(layoutResId: Integer): JAlertDialog_Builder; cdecl; overload;
    function setView(view: JView): JAlertDialog_Builder; cdecl; overload;
    function show: JAlertDialog; cdecl;
  end;
  TJAlertDialog_Builder = class(TJavaGenericImport<JAlertDialog_BuilderClass, JAlertDialog_Builder>) end;

  JApplicationClass = interface(JContextWrapperClass)
    ['{B2E72B31-70E4-4B3A-A067-F11C002F8275}']
    {class} function init: JApplication; cdecl;
  end;

  [JavaSignature('android/app/Application')]
  JApplication = interface(JContextWrapper)
    ['{28FC188F-36BA-4C06-843B-22F715AC831D}']
    procedure onConfigurationChanged(newConfig: JConfiguration); cdecl;
    procedure onCreate; cdecl;
    procedure onLowMemory; cdecl;
    procedure onTerminate; cdecl;
    procedure onTrimMemory(level: Integer); cdecl;
    procedure registerActivityLifecycleCallbacks(callback: JApplication_ActivityLifecycleCallbacks); cdecl;
    procedure registerComponentCallbacks(callback: JComponentCallbacks); cdecl;
    procedure registerOnProvideAssistDataListener(callback: JApplication_OnProvideAssistDataListener); cdecl;
    procedure unregisterActivityLifecycleCallbacks(callback: JApplication_ActivityLifecycleCallbacks); cdecl;
    procedure unregisterComponentCallbacks(callback: JComponentCallbacks); cdecl;
    procedure unregisterOnProvideAssistDataListener(callback: JApplication_OnProvideAssistDataListener); cdecl;
  end;
  TJApplication = class(TJavaGenericImport<JApplicationClass, JApplication>) end;

  JApplication_ActivityLifecycleCallbacksClass = interface(IJavaClass)
    ['{82C4F2D1-0E03-4C87-940A-E2EE997EAC0C}']
  end;

  [JavaSignature('android/app/Application$ActivityLifecycleCallbacks')]
  JApplication_ActivityLifecycleCallbacks = interface(IJavaInstance)
    ['{45381B4E-F44D-4BB9-883F-CB7DC2EC4AE3}']
    procedure onActivityCreated(activity: JActivity; savedInstanceState: JBundle); cdecl;
    procedure onActivityDestroyed(activity: JActivity); cdecl;
    procedure onActivityPaused(activity: JActivity); cdecl;
    procedure onActivityResumed(activity: JActivity); cdecl;
    procedure onActivitySaveInstanceState(activity: JActivity; outState: JBundle); cdecl;
    procedure onActivityStarted(activity: JActivity); cdecl;
    procedure onActivityStopped(activity: JActivity); cdecl;
  end;
  TJApplication_ActivityLifecycleCallbacks = class(TJavaGenericImport<JApplication_ActivityLifecycleCallbacksClass, JApplication_ActivityLifecycleCallbacks>) end;

  JApplication_OnProvideAssistDataListenerClass = interface(IJavaClass)
    ['{E7660DEF-A22F-4C70-AB39-F3B68B072D1A}']
  end;

  [JavaSignature('android/app/Application$OnProvideAssistDataListener')]
  JApplication_OnProvideAssistDataListener = interface(IJavaInstance)
    ['{53921961-9D0B-4BF4-A841-C690812FADDC}']
    procedure onProvideAssistData(activity: JActivity; data: JBundle); cdecl;
  end;
  TJApplication_OnProvideAssistDataListener = class(TJavaGenericImport<JApplication_OnProvideAssistDataListenerClass, JApplication_OnProvideAssistDataListener>) end;

  JAutomaticZenRuleClass = interface(JObjectClass)
    ['{6F7058E7-BAD7-40DC-91FA-A5FEC86F8BED}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} //function init(name: JString; owner: JComponentName; conditionId: Jnet_Uri; interruptionFilter: Integer; enabled: Boolean): JAutomaticZenRule; cdecl; overload;
    {class} function init(source: JParcel): JAutomaticZenRule; cdecl; overload;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('android/app/AutomaticZenRule')]
  JAutomaticZenRule = interface(JObject)
    ['{8D31FD86-EC63-4902-9D99-2078956CA676}']
    function describeContents: Integer; cdecl;
    function equals(o: JObject): Boolean; cdecl;
    //function getConditionId: Jnet_Uri; cdecl;
    function getCreationTime: Int64; cdecl;
    function getInterruptionFilter: Integer; cdecl;
    function getName: JString; cdecl;
    function getOwner: JComponentName; cdecl;
    function hashCode: Integer; cdecl;
    function isEnabled: Boolean; cdecl;
    //procedure setConditionId(conditionId: Jnet_Uri); cdecl;
    procedure setEnabled(enabled: Boolean); cdecl;
    procedure setInterruptionFilter(interruptionFilter: Integer); cdecl;
    procedure setName(name: JString); cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
  end;
  TJAutomaticZenRule = class(TJavaGenericImport<JAutomaticZenRuleClass, JAutomaticZenRule>) end;

  JFragmentClass = interface(JObjectClass)
    ['{3C54B094-6ECA-4E7F-9A49-E8B77FE2B156}']
    {class} function init: JFragment; cdecl;
    {class} function instantiate(context: JContext; fname: JString): JFragment; cdecl; overload;
    {class} function instantiate(context: JContext; fname: JString; args: JBundle): JFragment; cdecl; overload;
  end;

  [JavaSignature('android/app/Fragment')]
  JFragment = interface(JObject)
    ['{A17DB3FE-8968-475D-988E-0CB75BE207CF}']
    procedure dump(prefix: JString; fd: JFileDescriptor; writer: JPrintWriter; args: TJavaObjectArray<JString>); cdecl;
    function equals(o: JObject): Boolean; cdecl;
    function getActivity: JActivity; cdecl;
    function getAllowEnterTransitionOverlap: Boolean; cdecl;
    function getAllowReturnTransitionOverlap: Boolean; cdecl;
    function getArguments: JBundle; cdecl;
    function getChildFragmentManager: JFragmentManager; cdecl;
    function getContext: JContext; cdecl;
    //function getEnterTransition: JTransition; cdecl;
    //function getExitTransition: JTransition; cdecl;
    function getFragmentManager: JFragmentManager; cdecl;
    function getHost: JObject; cdecl;
    function getId: Integer; cdecl;
    function getLayoutInflater: JLayoutInflater; cdecl;
    function getLoaderManager: JLoaderManager; cdecl;
    function getParentFragment: JFragment; cdecl;
    //function getReenterTransition: JTransition; cdecl;
    function getResources: JResources; cdecl;
    function getRetainInstance: Boolean; cdecl;
    //function getReturnTransition: JTransition; cdecl;
    //function getSharedElementEnterTransition: JTransition; cdecl;
    //function getSharedElementReturnTransition: JTransition; cdecl;
    function getString(resId: Integer): JString; cdecl; overload;
    function getTag: JString; cdecl;
    function getTargetFragment: JFragment; cdecl;
    function getTargetRequestCode: Integer; cdecl;
    function getText(resId: Integer): JCharSequence; cdecl;
    function getUserVisibleHint: Boolean; cdecl;
    function getView: JView; cdecl;
    function hashCode: Integer; cdecl;
    function isAdded: Boolean; cdecl;
    function isDetached: Boolean; cdecl;
    function isHidden: Boolean; cdecl;
    function isInLayout: Boolean; cdecl;
    function isRemoving: Boolean; cdecl;
    function isResumed: Boolean; cdecl;
    function isStateSaved: Boolean; cdecl;
    function isVisible: Boolean; cdecl;
    procedure onActivityCreated(savedInstanceState: JBundle); cdecl;
    procedure onActivityResult(requestCode: Integer; resultCode: Integer; data: JIntent); cdecl;
    procedure onAttach(context: JContext); cdecl; overload;
    procedure onAttach(activity: JActivity); cdecl; overload;//Deprecated
    procedure onAttachFragment(childFragment: JFragment); cdecl;
    procedure onConfigurationChanged(newConfig: JConfiguration); cdecl;
    function onContextItemSelected(item: JMenuItem): Boolean; cdecl;
    procedure onCreate(savedInstanceState: JBundle); cdecl;
    //function onCreateAnimator(transit: Integer; enter: Boolean; nextAnim: Integer): JAnimator; cdecl;
    procedure onCreateContextMenu(menu: JContextMenu; v: JView; menuInfo: JContextMenu_ContextMenuInfo); cdecl;
    procedure onCreateOptionsMenu(menu: JMenu; inflater: JMenuInflater); cdecl;
    function onCreateView(inflater: JLayoutInflater; container: JViewGroup; savedInstanceState: JBundle): JView; cdecl;
    procedure onDestroy; cdecl;
    procedure onDestroyOptionsMenu; cdecl;
    procedure onDestroyView; cdecl;
    procedure onDetach; cdecl;
    function onGetLayoutInflater(savedInstanceState: JBundle): JLayoutInflater; cdecl;
    procedure onHiddenChanged(hidden: Boolean); cdecl;
    procedure onInflate(attrs: JAttributeSet; savedInstanceState: JBundle); cdecl; overload;//Deprecated
    procedure onInflate(context: JContext; attrs: JAttributeSet; savedInstanceState: JBundle); cdecl; overload;
    procedure onInflate(activity: JActivity; attrs: JAttributeSet; savedInstanceState: JBundle); cdecl; overload;//Deprecated
    procedure onLowMemory; cdecl;
    procedure onMultiWindowModeChanged(isInMultiWindowMode: Boolean; newConfig: JConfiguration); cdecl; overload;
    procedure onMultiWindowModeChanged(isInMultiWindowMode: Boolean); cdecl; overload;//Deprecated
    function onOptionsItemSelected(item: JMenuItem): Boolean; cdecl;
    procedure onOptionsMenuClosed(menu: JMenu); cdecl;
    procedure onPause; cdecl;
    procedure onPictureInPictureModeChanged(isInPictureInPictureMode: Boolean; newConfig: JConfiguration); cdecl; overload;
    procedure onPictureInPictureModeChanged(isInPictureInPictureMode: Boolean); cdecl; overload;//Deprecated
    procedure onPrepareOptionsMenu(menu: JMenu); cdecl;
    procedure onRequestPermissionsResult(requestCode: Integer; permissions: TJavaObjectArray<JString>; grantResults: TJavaArray<Integer>); cdecl;
    procedure onResume; cdecl;
    procedure onSaveInstanceState(outState: JBundle); cdecl;
    procedure onStart; cdecl;
    procedure onStop; cdecl;
    procedure onTrimMemory(level: Integer); cdecl;
    procedure onViewCreated(view: JView; savedInstanceState: JBundle); cdecl;
    procedure onViewStateRestored(savedInstanceState: JBundle); cdecl;
    procedure postponeEnterTransition; cdecl;
    procedure registerForContextMenu(view: JView); cdecl;
    procedure requestPermissions(permissions: TJavaObjectArray<JString>; requestCode: Integer); cdecl;
    procedure setAllowEnterTransitionOverlap(allow: Boolean); cdecl;
    procedure setAllowReturnTransitionOverlap(allow: Boolean); cdecl;
    procedure setArguments(args: JBundle); cdecl;
    procedure setEnterSharedElementCallback(callback: JSharedElementCallback); cdecl;
    //procedure setEnterTransition(transition: JTransition); cdecl;
    procedure setExitSharedElementCallback(callback: JSharedElementCallback); cdecl;
    //procedure setExitTransition(transition: JTransition); cdecl;
    procedure setHasOptionsMenu(hasMenu: Boolean); cdecl;
    procedure setInitialSavedState(state: JFragment_SavedState); cdecl;
    procedure setMenuVisibility(menuVisible: Boolean); cdecl;
    //procedure setReenterTransition(transition: JTransition); cdecl;
    procedure setRetainInstance(retain: Boolean); cdecl;
    //procedure setReturnTransition(transition: JTransition); cdecl;
    //procedure setSharedElementEnterTransition(transition: JTransition); cdecl;
    //procedure setSharedElementReturnTransition(transition: JTransition); cdecl;
    procedure setTargetFragment(fragment: JFragment; requestCode: Integer); cdecl;
    procedure setUserVisibleHint(isVisibleToUser: Boolean); cdecl;
    function shouldShowRequestPermissionRationale(permission: JString): Boolean; cdecl;
    procedure startActivity(intent: JIntent); cdecl; overload;
    procedure startActivity(intent: JIntent; options: JBundle); cdecl; overload;
    procedure startActivityForResult(intent: JIntent; requestCode: Integer); cdecl; overload;
    procedure startActivityForResult(intent: JIntent; requestCode: Integer; options: JBundle); cdecl; overload;
    procedure startIntentSenderForResult(intent: JIntentSender; requestCode: Integer; fillInIntent: JIntent; flagsMask: Integer; flagsValues: Integer; extraFlags: Integer; options: JBundle); cdecl;
    procedure startPostponedEnterTransition; cdecl;
    function toString: JString; cdecl;
    procedure unregisterForContextMenu(view: JView); cdecl;
  end;
  TJFragment = class(TJavaGenericImport<JFragmentClass, JFragment>) end;

  JDialogFragmentClass = interface(JFragmentClass)
    ['{0BA4C933-9BC4-4D8B-9757-C02740EBC511}']
    {class} function _GetSTYLE_NORMAL: Integer; cdecl;
    {class} function _GetSTYLE_NO_FRAME: Integer; cdecl;
    {class} function _GetSTYLE_NO_INPUT: Integer; cdecl;
    {class} function _GetSTYLE_NO_TITLE: Integer; cdecl;
    {class} function init: JDialogFragment; cdecl;
    {class} property STYLE_NORMAL: Integer read _GetSTYLE_NORMAL;
    {class} property STYLE_NO_FRAME: Integer read _GetSTYLE_NO_FRAME;
    {class} property STYLE_NO_INPUT: Integer read _GetSTYLE_NO_INPUT;
    {class} property STYLE_NO_TITLE: Integer read _GetSTYLE_NO_TITLE;
  end;

  [JavaSignature('android/app/DialogFragment')]
  JDialogFragment = interface(JFragment)
    ['{D5B8C228-5BCF-43DF-B26A-18A3EB4C893E}']
    procedure dismiss; cdecl;
    procedure dismissAllowingStateLoss; cdecl;
    procedure dump(prefix: JString; fd: JFileDescriptor; writer: JPrintWriter; args: TJavaObjectArray<JString>); cdecl;
    function getDialog: JDialog; cdecl;
    function getShowsDialog: Boolean; cdecl;
    function getTheme: Integer; cdecl;
    function isCancelable: Boolean; cdecl;
    procedure onActivityCreated(savedInstanceState: JBundle); cdecl;
    procedure onAttach(context: JContext); cdecl;
    procedure onCancel(dialog: JDialogInterface); cdecl;
    procedure onCreate(savedInstanceState: JBundle); cdecl;
    function onCreateDialog(savedInstanceState: JBundle): JDialog; cdecl;
    procedure onDestroyView; cdecl;
    procedure onDetach; cdecl;
    procedure onDismiss(dialog: JDialogInterface); cdecl;
    procedure onSaveInstanceState(outState: JBundle); cdecl;
    procedure onStart; cdecl;
    procedure onStop; cdecl;
    procedure setCancelable(cancelable: Boolean); cdecl;
    procedure setShowsDialog(showsDialog: Boolean); cdecl;
    procedure setStyle(style: Integer; theme: Integer); cdecl;
    procedure show(manager: JFragmentManager; tag: JString); cdecl; overload;
    function show(transaction: JFragmentTransaction; tag: JString): Integer; cdecl; overload;
  end;
  TJDialogFragment = class(TJavaGenericImport<JDialogFragmentClass, JDialogFragment>) end;

  JFragment_SavedStateClass = interface(JObjectClass)
    ['{5BA60ED2-A67D-4A23-8B92-E4F26A4799D9}']
    {class} function _GetCREATOR: JParcelable_ClassLoaderCreator; cdecl;
    {class} property CREATOR: JParcelable_ClassLoaderCreator read _GetCREATOR;
  end;

  [JavaSignature('android/app/Fragment$SavedState')]
  JFragment_SavedState = interface(JObject)
    ['{76BE11A9-82B9-4AC9-8C54-80BAFBD3A578}']
    function describeContents: Integer; cdecl;
    procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
  end;
  TJFragment_SavedState = class(TJavaGenericImport<JFragment_SavedStateClass, JFragment_SavedState>) end;

  JFragmentManagerClass = interface(JObjectClass)
    ['{AF70B1C7-BC2A-4F71-83F4-BCB987DC7F04}']
    {class} function _GetPOP_BACK_STACK_INCLUSIVE: Integer; cdecl;
    {class} function init: JFragmentManager; cdecl;
    {class} procedure enableDebugLogging(enabled: Boolean); cdecl;
    {class} property POP_BACK_STACK_INCLUSIVE: Integer read _GetPOP_BACK_STACK_INCLUSIVE;
  end;

  [JavaSignature('android/app/FragmentManager')]
  JFragmentManager = interface(JObject)
    ['{4B06A33C-C4AF-4BE8-82E2-82E3B8BCBFCF}']
    procedure addOnBackStackChangedListener(listener: JFragmentManager_OnBackStackChangedListener); cdecl;
    function beginTransaction: JFragmentTransaction; cdecl;
    procedure dump(prefix: JString; fd: JFileDescriptor; writer: JPrintWriter; args: TJavaObjectArray<JString>); cdecl;
    function executePendingTransactions: Boolean; cdecl;
    function findFragmentById(id: Integer): JFragment; cdecl;
    function findFragmentByTag(tag: JString): JFragment; cdecl;
    function getBackStackEntryAt(index: Integer): JFragmentManager_BackStackEntry; cdecl;
    function getBackStackEntryCount: Integer; cdecl;
    function getFragment(bundle: JBundle; key: JString): JFragment; cdecl;
    function getFragments: JList; cdecl;
    function getPrimaryNavigationFragment: JFragment; cdecl;
    procedure invalidateOptionsMenu; cdecl;
    function isDestroyed: Boolean; cdecl;
    function isStateSaved: Boolean; cdecl;
    procedure popBackStack; cdecl; overload;
    procedure popBackStack(name: JString; flags: Integer); cdecl; overload;
    procedure popBackStack(id: Integer; flags: Integer); cdecl; overload;
    function popBackStackImmediate: Boolean; cdecl; overload;
    function popBackStackImmediate(name: JString; flags: Integer): Boolean; cdecl; overload;
    function popBackStackImmediate(id: Integer; flags: Integer): Boolean; cdecl; overload;
    procedure putFragment(bundle: JBundle; key: JString; fragment: JFragment); cdecl;
    procedure registerFragmentLifecycleCallbacks(cb: JFragmentManager_FragmentLifecycleCallbacks; recursive: Boolean); cdecl;
    procedure removeOnBackStackChangedListener(listener: JFragmentManager_OnBackStackChangedListener); cdecl;
    function saveFragmentInstanceState(f: JFragment): JFragment_SavedState; cdecl;
    procedure unregisterFragmentLifecycleCallbacks(cb: JFragmentManager_FragmentLifecycleCallbacks); cdecl;
  end;
  TJFragmentManager = class(TJavaGenericImport<JFragmentManagerClass, JFragmentManager>) end;

  JFragmentManager_BackStackEntryClass = interface(IJavaClass)
    ['{8B736CA5-FB98-4A84-8BDE-7A43FEE35224}']
  end;

  [JavaSignature('android/app/FragmentManager$BackStackEntry')]
  JFragmentManager_BackStackEntry = interface(IJavaInstance)
    ['{F5CD98DC-3473-4D0E-B189-93124ABAFDCD}']
    function getBreadCrumbShortTitle: JCharSequence; cdecl;
    function getBreadCrumbShortTitleRes: Integer; cdecl;
    function getBreadCrumbTitle: JCharSequence; cdecl;
    function getBreadCrumbTitleRes: Integer; cdecl;
    function getId: Integer; cdecl;
    function getName: JString; cdecl;
  end;
  TJFragmentManager_BackStackEntry = class(TJavaGenericImport<JFragmentManager_BackStackEntryClass, JFragmentManager_BackStackEntry>) end;

  JFragmentManager_FragmentLifecycleCallbacksClass = interface(JObjectClass)
    ['{D8A87D2A-B9E9-4E27-8F33-8FA717AD2EC7}']
    {class} function init: JFragmentManager_FragmentLifecycleCallbacks; cdecl;
  end;

  [JavaSignature('android/app/FragmentManager$FragmentLifecycleCallbacks')]
  JFragmentManager_FragmentLifecycleCallbacks = interface(JObject)
    ['{FEEF51BF-5BBA-4592-9DC6-E5A739296FEE}']
    procedure onFragmentActivityCreated(fm: JFragmentManager; f: JFragment; savedInstanceState: JBundle); cdecl;
    procedure onFragmentAttached(fm: JFragmentManager; f: JFragment; context: JContext); cdecl;
    procedure onFragmentCreated(fm: JFragmentManager; f: JFragment; savedInstanceState: JBundle); cdecl;
    procedure onFragmentDestroyed(fm: JFragmentManager; f: JFragment); cdecl;
    procedure onFragmentDetached(fm: JFragmentManager; f: JFragment); cdecl;
    procedure onFragmentPaused(fm: JFragmentManager; f: JFragment); cdecl;
    procedure onFragmentPreAttached(fm: JFragmentManager; f: JFragment; context: JContext); cdecl;
    procedure onFragmentPreCreated(fm: JFragmentManager; f: JFragment; savedInstanceState: JBundle); cdecl;
    procedure onFragmentResumed(fm: JFragmentManager; f: JFragment); cdecl;
    procedure onFragmentSaveInstanceState(fm: JFragmentManager; f: JFragment; outState: JBundle); cdecl;
    procedure onFragmentStarted(fm: JFragmentManager; f: JFragment); cdecl;
    procedure onFragmentStopped(fm: JFragmentManager; f: JFragment); cdecl;
    procedure onFragmentViewCreated(fm: JFragmentManager; f: JFragment; v: JView; savedInstanceState: JBundle); cdecl;
    procedure onFragmentViewDestroyed(fm: JFragmentManager; f: JFragment); cdecl;
  end;
  TJFragmentManager_FragmentLifecycleCallbacks = class(TJavaGenericImport<JFragmentManager_FragmentLifecycleCallbacksClass, JFragmentManager_FragmentLifecycleCallbacks>) end;

  JFragmentManager_OnBackStackChangedListenerClass = interface(IJavaClass)
    ['{7255BA0C-3B6C-4C55-B7EB-50A193BC8186}']
  end;

  [JavaSignature('android/app/FragmentManager$OnBackStackChangedListener')]
  JFragmentManager_OnBackStackChangedListener = interface(IJavaInstance)
    ['{B3418C11-9DEC-46E8-B17D-262F80649BC8}']
    procedure onBackStackChanged; cdecl;
  end;
  TJFragmentManager_OnBackStackChangedListener = class(TJavaGenericImport<JFragmentManager_OnBackStackChangedListenerClass, JFragmentManager_OnBackStackChangedListener>) end;

  JFragmentTransactionClass = interface(JObjectClass)
    ['{F09CC327-C48D-46CD-AE7A-4E7036F3C90E}']
    {class} function _GetTRANSIT_ENTER_MASK: Integer; cdecl;
    {class} function _GetTRANSIT_EXIT_MASK: Integer; cdecl;
    {class} function _GetTRANSIT_FRAGMENT_CLOSE: Integer; cdecl;
    {class} function _GetTRANSIT_FRAGMENT_FADE: Integer; cdecl;
    {class} function _GetTRANSIT_FRAGMENT_OPEN: Integer; cdecl;
    {class} function _GetTRANSIT_NONE: Integer; cdecl;
    {class} function _GetTRANSIT_UNSET: Integer; cdecl;
    {class} function init: JFragmentTransaction; cdecl;
    {class} property TRANSIT_ENTER_MASK: Integer read _GetTRANSIT_ENTER_MASK;
    {class} property TRANSIT_EXIT_MASK: Integer read _GetTRANSIT_EXIT_MASK;
    {class} property TRANSIT_FRAGMENT_CLOSE: Integer read _GetTRANSIT_FRAGMENT_CLOSE;
    {class} property TRANSIT_FRAGMENT_FADE: Integer read _GetTRANSIT_FRAGMENT_FADE;
    {class} property TRANSIT_FRAGMENT_OPEN: Integer read _GetTRANSIT_FRAGMENT_OPEN;
    {class} property TRANSIT_NONE: Integer read _GetTRANSIT_NONE;
    {class} property TRANSIT_UNSET: Integer read _GetTRANSIT_UNSET;
  end;

  [JavaSignature('android/app/FragmentTransaction')]
  JFragmentTransaction = interface(JObject)
    ['{A55DB157-F3A6-4F55-AD4C-6BC7A21C6FD8}']
    function add(fragment: JFragment; tag: JString): JFragmentTransaction; cdecl; overload;
    function add(containerViewId: Integer; fragment: JFragment): JFragmentTransaction; cdecl; overload;
    function add(containerViewId: Integer; fragment: JFragment; tag: JString): JFragmentTransaction; cdecl; overload;
    function addSharedElement(sharedElement: JView; name: JString): JFragmentTransaction; cdecl;
    function addToBackStack(name: JString): JFragmentTransaction; cdecl;
    function attach(fragment: JFragment): JFragmentTransaction; cdecl;
    function commit: Integer; cdecl;
    function commitAllowingStateLoss: Integer; cdecl;
    procedure commitNow; cdecl;
    procedure commitNowAllowingStateLoss; cdecl;
    function detach(fragment: JFragment): JFragmentTransaction; cdecl;
    function disallowAddToBackStack: JFragmentTransaction; cdecl;
    function hide(fragment: JFragment): JFragmentTransaction; cdecl;
    function isAddToBackStackAllowed: Boolean; cdecl;
    function isEmpty: Boolean; cdecl;
    function remove(fragment: JFragment): JFragmentTransaction; cdecl;
    function replace(containerViewId: Integer; fragment: JFragment): JFragmentTransaction; cdecl; overload;
    function replace(containerViewId: Integer; fragment: JFragment; tag: JString): JFragmentTransaction; cdecl; overload;
    function runOnCommit(runnable: JRunnable): JFragmentTransaction; cdecl;
    function setBreadCrumbShortTitle(res: Integer): JFragmentTransaction; cdecl; overload;
    function setBreadCrumbShortTitle(text: JCharSequence): JFragmentTransaction; cdecl; overload;
    function setBreadCrumbTitle(res: Integer): JFragmentTransaction; cdecl; overload;
    function setBreadCrumbTitle(text: JCharSequence): JFragmentTransaction; cdecl; overload;
    function setCustomAnimations(enter: Integer; exit: Integer): JFragmentTransaction; cdecl; overload;
    function setCustomAnimations(enter: Integer; exit: Integer; popEnter: Integer; popExit: Integer): JFragmentTransaction; cdecl; overload;
    function setPrimaryNavigationFragment(fragment: JFragment): JFragmentTransaction; cdecl;
    function setReorderingAllowed(reorderingAllowed: Boolean): JFragmentTransaction; cdecl;
    function setTransition(transit: Integer): JFragmentTransaction; cdecl;
    function setTransitionStyle(styleRes: Integer): JFragmentTransaction; cdecl;
    function show(fragment: JFragment): JFragmentTransaction; cdecl;
  end;
  TJFragmentTransaction = class(TJavaGenericImport<JFragmentTransactionClass, JFragmentTransaction>) end;

  JServiceClass = interface(JContextWrapperClass)
    ['{50459866-E20A-4564-B22B-9A8AB2716406}']
    {class} function _GetSTART_CONTINUATION_MASK: Integer; cdecl;
    {class} function _GetSTART_FLAG_REDELIVERY: Integer; cdecl;
    {class} function _GetSTART_FLAG_RETRY: Integer; cdecl;
    {class} function _GetSTART_NOT_STICKY: Integer; cdecl;
    {class} function _GetSTART_REDELIVER_INTENT: Integer; cdecl;
    {class} function _GetSTART_STICKY: Integer; cdecl;
    {class} function _GetSTART_STICKY_COMPATIBILITY: Integer; cdecl;
    {class} function _GetSTOP_FOREGROUND_DETACH: Integer; cdecl;
    {class} function _GetSTOP_FOREGROUND_REMOVE: Integer; cdecl;
    {class} function init: JService; cdecl;
    {class} property START_CONTINUATION_MASK: Integer read _GetSTART_CONTINUATION_MASK;
    {class} property START_FLAG_REDELIVERY: Integer read _GetSTART_FLAG_REDELIVERY;
    {class} property START_FLAG_RETRY: Integer read _GetSTART_FLAG_RETRY;
    {class} property START_NOT_STICKY: Integer read _GetSTART_NOT_STICKY;
    {class} property START_REDELIVER_INTENT: Integer read _GetSTART_REDELIVER_INTENT;
    {class} property START_STICKY: Integer read _GetSTART_STICKY;
    {class} property START_STICKY_COMPATIBILITY: Integer read _GetSTART_STICKY_COMPATIBILITY;
    {class} property STOP_FOREGROUND_DETACH: Integer read _GetSTOP_FOREGROUND_DETACH;
    {class} property STOP_FOREGROUND_REMOVE: Integer read _GetSTOP_FOREGROUND_REMOVE;
  end;

  [JavaSignature('android/app/Service')]
  JService = interface(JContextWrapper)
    ['{C1C969BF-8725-4920-9691-4981E4892C1F}']
    function getApplication: JApplication; cdecl;
    function onBind(intent: JIntent): JIBinder; cdecl;
    procedure onConfigurationChanged(newConfig: JConfiguration); cdecl;
    procedure onCreate; cdecl;
    procedure onDestroy; cdecl;
    procedure onLowMemory; cdecl;
    procedure onRebind(intent: JIntent); cdecl;
    procedure onStart(intent: JIntent; startId: Integer); cdecl;//Deprecated
    function onStartCommand(intent: JIntent; flags: Integer; startId: Integer): Integer; cdecl;
    procedure onTaskRemoved(rootIntent: JIntent); cdecl;
    procedure onTrimMemory(level: Integer); cdecl;
    function onUnbind(intent: JIntent): Boolean; cdecl;
    procedure startForeground(id: Integer; notification: JNotification); cdecl;
    procedure stopForeground(removeNotification: Boolean); cdecl; overload;
    procedure stopForeground(flags: Integer); cdecl; overload;
    procedure stopSelf; cdecl; overload;
    procedure stopSelf(startId: Integer); cdecl; overload;
    function stopSelfResult(startId: Integer): Boolean; cdecl;
  end;
  TJService = class(TJavaGenericImport<JServiceClass, JService>) end;

  JIntentServiceClass = interface(JServiceClass)
    ['{46298ACB-D39D-4C26-B9BB-90888D68848F}']
    {class} function init(name: JString): JIntentService; cdecl;
  end;

  [JavaSignature('android/app/IntentService')]
  JIntentService = interface(JService)
    ['{E5F9EDF4-5A96-49F9-A1FF-F69868B5946D}']
    function onBind(intent: JIntent): JIBinder; cdecl;
    procedure onCreate; cdecl;
    procedure onDestroy; cdecl;
    procedure onStart(intent: JIntent; startId: Integer); cdecl;
    function onStartCommand(intent: JIntent; flags: Integer; startId: Integer): Integer; cdecl;
    procedure setIntentRedelivery(enabled: Boolean); cdecl;
  end;
  TJIntentService = class(TJavaGenericImport<JIntentServiceClass, JIntentService>) end;

  JLoaderManagerClass = interface(JObjectClass)
    ['{50FB82E8-7ED8-409E-BC2D-0A385DFE7B94}']
    {class} function init: JLoaderManager; cdecl;
    {class} procedure enableDebugLogging(enabled: Boolean); cdecl;
  end;

  [JavaSignature('android/app/LoaderManager')]
  JLoaderManager = interface(JObject)
    ['{D45E200D-E969-42B8-9537-22C62850C7C5}']
    procedure destroyLoader(id: Integer); cdecl;
    procedure dump(prefix: JString; fd: JFileDescriptor; writer: JPrintWriter; args: TJavaObjectArray<JString>); cdecl;
    function getLoader(id: Integer): JLoader; cdecl;
    function initLoader(id: Integer; args: JBundle; callback: JLoaderManager_LoaderCallbacks): JLoader; cdecl;
    function restartLoader(id: Integer; args: JBundle; callback: JLoaderManager_LoaderCallbacks): JLoader; cdecl;
  end;
  TJLoaderManager = class(TJavaGenericImport<JLoaderManagerClass, JLoaderManager>) end;

  JLoaderManager_LoaderCallbacksClass = interface(IJavaClass)
    ['{5143414F-8245-45AA-A70D-DB8CA12DC90C}']
  end;

  [JavaSignature('android/app/LoaderManager$LoaderCallbacks')]
  JLoaderManager_LoaderCallbacks = interface(IJavaInstance)
    ['{F28327C2-3A34-4296-997C-F69AEDE35ECF}']
    function onCreateLoader(id: Integer; args: JBundle): JLoader; cdecl;
    procedure onLoadFinished(loader: JLoader; data: JObject); cdecl;
    procedure onLoaderReset(loader: JLoader); cdecl;
  end;
  TJLoaderManager_LoaderCallbacks = class(TJavaGenericImport<JLoaderManager_LoaderCallbacksClass, JLoaderManager_LoaderCallbacks>) end;

  JNativeActivityClass = interface(JActivityClass)
    ['{F54B4BFD-1C6B-4CD6-88E7-68ACDD301C01}']
    {class} function _GetMETA_DATA_FUNC_NAME: JString; cdecl;
    {class} function _GetMETA_DATA_LIB_NAME: JString; cdecl;
    {class} function init: JNativeActivity; cdecl;
    {class} property META_DATA_FUNC_NAME: JString read _GetMETA_DATA_FUNC_NAME;
    {class} property META_DATA_LIB_NAME: JString read _GetMETA_DATA_LIB_NAME;
  end;

  [JavaSignature('android/app/NativeActivity')]
  JNativeActivity = interface(JActivity)
    ['{DFE875A8-A174-41E7-91C0-D959852DE56D}']
    procedure onConfigurationChanged(newConfig: JConfiguration); cdecl;
    procedure onGlobalLayout; cdecl;
    procedure onInputQueueCreated(queue: JInputQueue); cdecl;
    procedure onInputQueueDestroyed(queue: JInputQueue); cdecl;
    procedure onLowMemory; cdecl;
    procedure onWindowFocusChanged(hasFocus: Boolean); cdecl;
    procedure surfaceChanged(holder: JSurfaceHolder; format: Integer; width: Integer; height: Integer); cdecl;
    procedure surfaceCreated(holder: JSurfaceHolder); cdecl;
    procedure surfaceDestroyed(holder: JSurfaceHolder); cdecl;
    procedure surfaceRedrawNeeded(holder: JSurfaceHolder); cdecl;
  end;
  TJNativeActivity = class(TJavaGenericImport<JNativeActivityClass, JNativeActivity>) end;

  JNotificationClass = interface(JObjectClass)
    ['{4584515F-7E22-49A9-A55B-A6DE3BFD7E1C}']
    {class} function _GetAUDIO_ATTRIBUTES_DEFAULT: JAudioAttributes; cdecl; // https://quality.embarcadero.com/browse/RSP-21296
    {class} function _GetBADGE_ICON_LARGE: Integer; cdecl;
    {class} function _GetBADGE_ICON_NONE: Integer; cdecl;
    {class} function _GetBADGE_ICON_SMALL: Integer; cdecl;
    {class} function _GetCATEGORY_ALARM: JString; cdecl;
    {class} function _GetCATEGORY_CALL: JString; cdecl;
    {class} function _GetCATEGORY_EMAIL: JString; cdecl;
    {class} function _GetCATEGORY_ERROR: JString; cdecl;
    {class} function _GetCATEGORY_EVENT: JString; cdecl;
    {class} function _GetCATEGORY_MESSAGE: JString; cdecl;
    {class} function _GetCATEGORY_PROGRESS: JString; cdecl;
    {class} function _GetCATEGORY_PROMO: JString; cdecl;
    {class} function _GetCATEGORY_RECOMMENDATION: JString; cdecl;
    {class} function _GetCATEGORY_REMINDER: JString; cdecl;
    {class} function _GetCATEGORY_SERVICE: JString; cdecl;
    {class} function _GetCATEGORY_SOCIAL: JString; cdecl;
    {class} function _GetCATEGORY_STATUS: JString; cdecl;
    {class} function _GetCATEGORY_SYSTEM: JString; cdecl;
    {class} function _GetCATEGORY_TRANSPORT: JString; cdecl;
    {class} function _GetCOLOR_DEFAULT: Integer; cdecl;
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function _GetDEFAULT_ALL: Integer; cdecl;
    {class} function _GetDEFAULT_LIGHTS: Integer; cdecl;
    {class} function _GetDEFAULT_SOUND: Integer; cdecl;
    {class} function _GetDEFAULT_VIBRATE: Integer; cdecl;
    {class} function _GetEXTRA_AUDIO_CONTENTS_URI: JString; cdecl;
    {class} function _GetEXTRA_BACKGROUND_IMAGE_URI: JString; cdecl;
    {class} function _GetEXTRA_BIG_TEXT: JString; cdecl;
    {class} function _GetEXTRA_CHANNEL_ID: JString; cdecl;
    {class} function _GetEXTRA_CHRONOMETER_COUNT_DOWN: JString; cdecl;
    {class} function _GetEXTRA_COLORIZED: JString; cdecl;
    {class} function _GetEXTRA_COMPACT_ACTIONS: JString; cdecl;
    {class} function _GetEXTRA_CONVERSATION_TITLE: JString; cdecl;
    {class} function _GetEXTRA_HISTORIC_MESSAGES: JString; cdecl;
    {class} function _GetEXTRA_INFO_TEXT: JString; cdecl;
    {class} function _GetEXTRA_LARGE_ICON: JString; cdecl;
    {class} function _GetEXTRA_LARGE_ICON_BIG: JString; cdecl;
    {class} function _GetEXTRA_MEDIA_SESSION: JString; cdecl;
    {class} function _GetEXTRA_MESSAGES: JString; cdecl;
    {class} function _GetEXTRA_NOTIFICATION_ID: JString; cdecl;
    {class} function _GetEXTRA_NOTIFICATION_TAG: JString; cdecl;
    {class} function _GetEXTRA_PEOPLE: JString; cdecl;
    {class} function _GetEXTRA_PICTURE: JString; cdecl;
    {class} function _GetEXTRA_PROGRESS: JString; cdecl;
    {class} function _GetEXTRA_PROGRESS_INDETERMINATE: JString; cdecl;
    {class} function _GetEXTRA_PROGRESS_MAX: JString; cdecl;
    {class} function _GetEXTRA_REMOTE_INPUT_HISTORY: JString; cdecl;
    {class} function _GetEXTRA_SELF_DISPLAY_NAME: JString; cdecl;
    {class} function _GetEXTRA_SHOW_CHRONOMETER: JString; cdecl;
    {class} function _GetEXTRA_SHOW_WHEN: JString; cdecl;
    {class} function _GetEXTRA_SMALL_ICON: JString; cdecl;
    {class} function _GetEXTRA_SUB_TEXT: JString; cdecl;
    {class} function _GetEXTRA_SUMMARY_TEXT: JString; cdecl;
    {class} function _GetEXTRA_TEMPLATE: JString; cdecl;
    {class} function _GetEXTRA_TEXT: JString; cdecl;
    {class} function _GetEXTRA_TEXT_LINES: JString; cdecl;
    {class} function _GetEXTRA_TITLE: JString; cdecl;
    {class} function _GetEXTRA_TITLE_BIG: JString; cdecl;
    {class} function _GetFLAG_AUTO_CANCEL: Integer; cdecl;
    {class} function _GetFLAG_FOREGROUND_SERVICE: Integer; cdecl;
    {class} function _GetFLAG_GROUP_SUMMARY: Integer; cdecl;
    {class} function _GetFLAG_HIGH_PRIORITY: Integer; cdecl;
    {class} function _GetFLAG_INSISTENT: Integer; cdecl;
    {class} function _GetFLAG_LOCAL_ONLY: Integer; cdecl;
    {class} function _GetFLAG_NO_CLEAR: Integer; cdecl;
    {class} function _GetFLAG_ONGOING_EVENT: Integer; cdecl;
    {class} function _GetFLAG_ONLY_ALERT_ONCE: Integer; cdecl;
    {class} function _GetFLAG_SHOW_LIGHTS: Integer; cdecl;
    {class} function _GetGROUP_ALERT_ALL: Integer; cdecl;
    {class} function _GetGROUP_ALERT_CHILDREN: Integer; cdecl;
    {class} function _GetGROUP_ALERT_SUMMARY: Integer; cdecl;
    {class} function _GetINTENT_CATEGORY_NOTIFICATION_PREFERENCES: JString; cdecl;
    {class} function _GetPRIORITY_DEFAULT: Integer; cdecl;
    {class} function _GetPRIORITY_HIGH: Integer; cdecl;
    {class} function _GetPRIORITY_LOW: Integer; cdecl;
    {class} function _GetPRIORITY_MAX: Integer; cdecl;
    {class} function _GetPRIORITY_MIN: Integer; cdecl;
    {class} function _GetSTREAM_DEFAULT: Integer; cdecl;
    {class} function _GetVISIBILITY_PRIVATE: Integer; cdecl;
    {class} function _GetVISIBILITY_PUBLIC: Integer; cdecl;
    {class} function _GetVISIBILITY_SECRET: Integer; cdecl;
    {class} function init: JNotification; cdecl; overload;
    {class} function init(icon: Integer; tickerText: JCharSequence; when: Int64): JNotification; cdecl; overload;//Deprecated
    {class} function init(parcel: JParcel): JNotification; cdecl; overload;
    {class} property AUDIO_ATTRIBUTES_DEFAULT: JAudioAttributes read _GetAUDIO_ATTRIBUTES_DEFAULT; // https://quality.embarcadero.com/browse/RSP-21296
    {class} property BADGE_ICON_LARGE: Integer read _GetBADGE_ICON_LARGE;
    {class} property BADGE_ICON_NONE: Integer read _GetBADGE_ICON_NONE;
    {class} property BADGE_ICON_SMALL: Integer read _GetBADGE_ICON_SMALL;
    {class} property CATEGORY_ALARM: JString read _GetCATEGORY_ALARM;
    {class} property CATEGORY_CALL: JString read _GetCATEGORY_CALL;
    {class} property CATEGORY_EMAIL: JString read _GetCATEGORY_EMAIL;
    {class} property CATEGORY_ERROR: JString read _GetCATEGORY_ERROR;
    {class} property CATEGORY_EVENT: JString read _GetCATEGORY_EVENT;
    {class} property CATEGORY_MESSAGE: JString read _GetCATEGORY_MESSAGE;
    {class} property CATEGORY_PROGRESS: JString read _GetCATEGORY_PROGRESS;
    {class} property CATEGORY_PROMO: JString read _GetCATEGORY_PROMO;
    {class} property CATEGORY_RECOMMENDATION: JString read _GetCATEGORY_RECOMMENDATION;
    {class} property CATEGORY_REMINDER: JString read _GetCATEGORY_REMINDER;
    {class} property CATEGORY_SERVICE: JString read _GetCATEGORY_SERVICE;
    {class} property CATEGORY_SOCIAL: JString read _GetCATEGORY_SOCIAL;
    {class} property CATEGORY_STATUS: JString read _GetCATEGORY_STATUS;
    {class} property CATEGORY_SYSTEM: JString read _GetCATEGORY_SYSTEM;
    {class} property CATEGORY_TRANSPORT: JString read _GetCATEGORY_TRANSPORT;
    {class} property COLOR_DEFAULT: Integer read _GetCOLOR_DEFAULT;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
    {class} property DEFAULT_ALL: Integer read _GetDEFAULT_ALL;
    {class} property DEFAULT_LIGHTS: Integer read _GetDEFAULT_LIGHTS;
    {class} property DEFAULT_SOUND: Integer read _GetDEFAULT_SOUND;
    {class} property DEFAULT_VIBRATE: Integer read _GetDEFAULT_VIBRATE;
    {class} property EXTRA_AUDIO_CONTENTS_URI: JString read _GetEXTRA_AUDIO_CONTENTS_URI;
    {class} property EXTRA_BACKGROUND_IMAGE_URI: JString read _GetEXTRA_BACKGROUND_IMAGE_URI;
    {class} property EXTRA_BIG_TEXT: JString read _GetEXTRA_BIG_TEXT;
    {class} property EXTRA_CHANNEL_ID: JString read _GetEXTRA_CHANNEL_ID;
    {class} property EXTRA_CHRONOMETER_COUNT_DOWN: JString read _GetEXTRA_CHRONOMETER_COUNT_DOWN;
    {class} property EXTRA_COLORIZED: JString read _GetEXTRA_COLORIZED;
    {class} property EXTRA_COMPACT_ACTIONS: JString read _GetEXTRA_COMPACT_ACTIONS;
    {class} property EXTRA_CONVERSATION_TITLE: JString read _GetEXTRA_CONVERSATION_TITLE;
    {class} property EXTRA_HISTORIC_MESSAGES: JString read _GetEXTRA_HISTORIC_MESSAGES;
    {class} property EXTRA_INFO_TEXT: JString read _GetEXTRA_INFO_TEXT;
    {class} property EXTRA_LARGE_ICON: JString read _GetEXTRA_LARGE_ICON;
    {class} property EXTRA_LARGE_ICON_BIG: JString read _GetEXTRA_LARGE_ICON_BIG;
    {class} property EXTRA_MEDIA_SESSION: JString read _GetEXTRA_MEDIA_SESSION;
    {class} property EXTRA_MESSAGES: JString read _GetEXTRA_MESSAGES;
    {class} property EXTRA_NOTIFICATION_ID: JString read _GetEXTRA_NOTIFICATION_ID;
    {class} property EXTRA_NOTIFICATION_TAG: JString read _GetEXTRA_NOTIFICATION_TAG;
    {class} property EXTRA_PEOPLE: JString read _GetEXTRA_PEOPLE;
    {class} property EXTRA_PICTURE: JString read _GetEXTRA_PICTURE;
    {class} property EXTRA_PROGRESS: JString read _GetEXTRA_PROGRESS;
    {class} property EXTRA_PROGRESS_INDETERMINATE: JString read _GetEXTRA_PROGRESS_INDETERMINATE;
    {class} property EXTRA_PROGRESS_MAX: JString read _GetEXTRA_PROGRESS_MAX;
    {class} property EXTRA_REMOTE_INPUT_HISTORY: JString read _GetEXTRA_REMOTE_INPUT_HISTORY;
    {class} property EXTRA_SELF_DISPLAY_NAME: JString read _GetEXTRA_SELF_DISPLAY_NAME;
    {class} property EXTRA_SHOW_CHRONOMETER: JString read _GetEXTRA_SHOW_CHRONOMETER;
    {class} property EXTRA_SHOW_WHEN: JString read _GetEXTRA_SHOW_WHEN;
    {class} property EXTRA_SMALL_ICON: JString read _GetEXTRA_SMALL_ICON;
    {class} property EXTRA_SUB_TEXT: JString read _GetEXTRA_SUB_TEXT;
    {class} property EXTRA_SUMMARY_TEXT: JString read _GetEXTRA_SUMMARY_TEXT;
    {class} property EXTRA_TEMPLATE: JString read _GetEXTRA_TEMPLATE;
    {class} property EXTRA_TEXT: JString read _GetEXTRA_TEXT;
    {class} property EXTRA_TEXT_LINES: JString read _GetEXTRA_TEXT_LINES;
    {class} property EXTRA_TITLE: JString read _GetEXTRA_TITLE;
    {class} property EXTRA_TITLE_BIG: JString read _GetEXTRA_TITLE_BIG;
    {class} property FLAG_AUTO_CANCEL: Integer read _GetFLAG_AUTO_CANCEL;
    {class} property FLAG_FOREGROUND_SERVICE: Integer read _GetFLAG_FOREGROUND_SERVICE;
    {class} property FLAG_GROUP_SUMMARY: Integer read _GetFLAG_GROUP_SUMMARY;
    {class} property FLAG_HIGH_PRIORITY: Integer read _GetFLAG_HIGH_PRIORITY;
    {class} property FLAG_INSISTENT: Integer read _GetFLAG_INSISTENT;
    {class} property FLAG_LOCAL_ONLY: Integer read _GetFLAG_LOCAL_ONLY;
    {class} property FLAG_NO_CLEAR: Integer read _GetFLAG_NO_CLEAR;
    {class} property FLAG_ONGOING_EVENT: Integer read _GetFLAG_ONGOING_EVENT;
    {class} property FLAG_ONLY_ALERT_ONCE: Integer read _GetFLAG_ONLY_ALERT_ONCE;
    {class} property FLAG_SHOW_LIGHTS: Integer read _GetFLAG_SHOW_LIGHTS;
    {class} property GROUP_ALERT_ALL: Integer read _GetGROUP_ALERT_ALL;
    {class} property GROUP_ALERT_CHILDREN: Integer read _GetGROUP_ALERT_CHILDREN;
    {class} property GROUP_ALERT_SUMMARY: Integer read _GetGROUP_ALERT_SUMMARY;
    {class} property INTENT_CATEGORY_NOTIFICATION_PREFERENCES: JString read _GetINTENT_CATEGORY_NOTIFICATION_PREFERENCES;
    {class} property PRIORITY_DEFAULT: Integer read _GetPRIORITY_DEFAULT;
    {class} property PRIORITY_HIGH: Integer read _GetPRIORITY_HIGH;
    {class} property PRIORITY_LOW: Integer read _GetPRIORITY_LOW;
    {class} property PRIORITY_MAX: Integer read _GetPRIORITY_MAX;
    {class} property PRIORITY_MIN: Integer read _GetPRIORITY_MIN;
    {class} property STREAM_DEFAULT: Integer read _GetSTREAM_DEFAULT;
    {class} property VISIBILITY_PRIVATE: Integer read _GetVISIBILITY_PRIVATE;
    {class} property VISIBILITY_PUBLIC: Integer read _GetVISIBILITY_PUBLIC;
    {class} property VISIBILITY_SECRET: Integer read _GetVISIBILITY_SECRET;
  end;

  [JavaSignature('android/app/Notification')]
  JNotification = interface(JObject)
    ['{4BCE0ADE-6C94-464C-806C-115E4F4DAFF3}']
    function _Getactions: TJavaObjectArray<JNotification_Action>; cdecl;
    procedure _Setactions(Value: TJavaObjectArray<JNotification_Action>); cdecl;
    //function _GetaudioAttributes: JAudioAttributes; cdecl;
    //procedure _SetaudioAttributes(Value: JAudioAttributes); cdecl;
    function _GetaudioStreamType: Integer; cdecl;
    procedure _SetaudioStreamType(Value: Integer); cdecl;
    function _GetbigContentView: JRemoteViews; cdecl;
    procedure _SetbigContentView(Value: JRemoteViews); cdecl;
    function _Getcategory: JString; cdecl;
    procedure _Setcategory(Value: JString); cdecl;
    function _Getcolor: Integer; cdecl;
    procedure _Setcolor(Value: Integer); cdecl;
    function _GetcontentIntent: JPendingIntent; cdecl;
    procedure _SetcontentIntent(Value: JPendingIntent); cdecl;
    function _GetcontentView: JRemoteViews; cdecl;
    procedure _SetcontentView(Value: JRemoteViews); cdecl;
    function _Getdefaults: Integer; cdecl;
    procedure _Setdefaults(Value: Integer); cdecl;
    function _GetdeleteIntent: JPendingIntent; cdecl;
    procedure _SetdeleteIntent(Value: JPendingIntent); cdecl;
    function _Getextras: JBundle; cdecl;
    procedure _Setextras(Value: JBundle); cdecl;
    function _Getflags: Integer; cdecl;
    procedure _Setflags(Value: Integer); cdecl;
    function _GetfullScreenIntent: JPendingIntent; cdecl;
    procedure _SetfullScreenIntent(Value: JPendingIntent); cdecl;
    function _GetheadsUpContentView: JRemoteViews; cdecl;
    procedure _SetheadsUpContentView(Value: JRemoteViews); cdecl;
    function _Geticon: Integer; cdecl;
    procedure _Seticon(Value: Integer); cdecl;
    function _GeticonLevel: Integer; cdecl;
    procedure _SeticonLevel(Value: Integer); cdecl;
    function _GetlargeIcon: JBitmap; cdecl;
    procedure _SetlargeIcon(Value: JBitmap); cdecl;
    function _GetledARGB: Integer; cdecl;
    procedure _SetledARGB(Value: Integer); cdecl;
    function _GetledOffMS: Integer; cdecl;
    procedure _SetledOffMS(Value: Integer); cdecl;
    function _GetledOnMS: Integer; cdecl;
    procedure _SetledOnMS(Value: Integer); cdecl;
    function _Getnumber: Integer; cdecl;
    procedure _Setnumber(Value: Integer); cdecl;
    function _Getpriority: Integer; cdecl;
    procedure _Setpriority(Value: Integer); cdecl;
    function _GetpublicVersion: JNotification; cdecl;
    procedure _SetpublicVersion(Value: JNotification); cdecl;
    //function _Getsound: Jnet_Uri; cdecl;
    //procedure _Setsound(Value: Jnet_Uri); cdecl;
    function _GettickerText: JCharSequence; cdecl;
    procedure _SettickerText(Value: JCharSequence); cdecl;
    function _GettickerView: JRemoteViews; cdecl;
    procedure _SettickerView(Value: JRemoteViews); cdecl;
    function _Getvibrate: TJavaArray<Int64>; cdecl;
    procedure _Setvibrate(Value: TJavaArray<Int64>); cdecl;
    function _Getvisibility: Integer; cdecl;
    procedure _Setvisibility(Value: Integer); cdecl;
    function _Getwhen: Int64; cdecl;
    procedure _Setwhen(Value: Int64); cdecl;
    function clone: JNotification; cdecl;
    function describeContents: Integer; cdecl;
    function getBadgeIconType: Integer; cdecl;
    function getChannel: JString; cdecl;//Deprecated
    function getChannelId: JString; cdecl;
    function getGroup: JString; cdecl;
    function getGroupAlertBehavior: Integer; cdecl;
    function getLargeIcon: JIcon; cdecl;
    function getSettingsText: JCharSequence; cdecl;
    function getShortcutId: JString; cdecl;
    function getSmallIcon: JIcon; cdecl;
    function getSortKey: JString; cdecl;
    function getTimeout: Int64; cdecl;//Deprecated
    function getTimeoutAfter: Int64; cdecl;
    procedure setLatestEventInfo(context: JContext; contentTitle: JCharSequence; contentText: JCharSequence; contentIntent: JPendingIntent); cdecl;//Deprecated
    function toString: JString; cdecl;
    procedure writeToParcel(parcel: JParcel; flags: Integer); cdecl;
    property actions: TJavaObjectArray<JNotification_Action> read _Getactions write _Setactions;
    //property audioAttributes: JAudioAttributes read _GetaudioAttributes write _SetaudioAttributes;
    property audioStreamType: Integer read _GetaudioStreamType write _SetaudioStreamType;
    property bigContentView: JRemoteViews read _GetbigContentView write _SetbigContentView;
    property category: JString read _Getcategory write _Setcategory;
    property color: Integer read _Getcolor write _Setcolor;
    property contentIntent: JPendingIntent read _GetcontentIntent write _SetcontentIntent;
    property contentView: JRemoteViews read _GetcontentView write _SetcontentView;
    property defaults: Integer read _Getdefaults write _Setdefaults;
    property deleteIntent: JPendingIntent read _GetdeleteIntent write _SetdeleteIntent;
    property extras: JBundle read _Getextras write _Setextras;
    property flags: Integer read _Getflags write _Setflags;
    property fullScreenIntent: JPendingIntent read _GetfullScreenIntent write _SetfullScreenIntent;
    property headsUpContentView: JRemoteViews read _GetheadsUpContentView write _SetheadsUpContentView;
    property icon: Integer read _Geticon write _Seticon;
    property iconLevel: Integer read _GeticonLevel write _SeticonLevel;
    property largeIcon: JBitmap read _GetlargeIcon write _SetlargeIcon;
    property ledARGB: Integer read _GetledARGB write _SetledARGB;
    property ledOffMS: Integer read _GetledOffMS write _SetledOffMS;
    property ledOnMS: Integer read _GetledOnMS write _SetledOnMS;
    property number: Integer read _Getnumber write _Setnumber;
    property priority: Integer read _Getpriority write _Setpriority;
    property publicVersion: JNotification read _GetpublicVersion write _SetpublicVersion;
    //property sound: Jnet_Uri read _Getsound write _Setsound;
    property tickerText: JCharSequence read _GettickerText write _SettickerText;
    property tickerView: JRemoteViews read _GettickerView write _SettickerView;
    property vibrate: TJavaArray<Int64> read _Getvibrate write _Setvibrate;
    property visibility: Integer read _Getvisibility write _Setvisibility;
    property when: Int64 read _Getwhen write _Setwhen;
  end;
  TJNotification = class(TJavaGenericImport<JNotificationClass, JNotification>) end;

  JNotification_ActionClass = interface(JObjectClass)
    ['{B148F977-2DDA-434E-A8D3-E742AB22F8EC}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function init(icon: Integer; title: JCharSequence; intent: JPendingIntent): JNotification_Action; cdecl;//Deprecated
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('android/app/Notification$Action')]
  JNotification_Action = interface(JObject)
    ['{673AFCAA-5D84-4B7F-B972-8BA0170EE184}']
    function _GetactionIntent: JPendingIntent; cdecl;
    procedure _SetactionIntent(Value: JPendingIntent); cdecl;
    function _Geticon: Integer; cdecl;
    procedure _Seticon(Value: Integer); cdecl;
    function _Gettitle: JCharSequence; cdecl;
    procedure _Settitle(Value: JCharSequence); cdecl;
    function clone: JNotification_Action; cdecl;
    function describeContents: Integer; cdecl;
    function getAllowGeneratedReplies: Boolean; cdecl;
    function getDataOnlyRemoteInputs: TJavaObjectArray<JRemoteInput>; cdecl;
    function getExtras: JBundle; cdecl;
    function getIcon: JIcon; cdecl;
    function getRemoteInputs: TJavaObjectArray<JRemoteInput>; cdecl;
    procedure writeToParcel(out_: JParcel; flags: Integer); cdecl;
    property actionIntent: JPendingIntent read _GetactionIntent write _SetactionIntent;
    property icon: Integer read _Geticon write _Seticon;
    property title: JCharSequence read _Gettitle write _Settitle;
  end;
  TJNotification_Action = class(TJavaGenericImport<JNotification_ActionClass, JNotification_Action>) end;

  JNotificationChannelClass = interface(JObjectClass)
    ['{6A1CADA7-9A73-4D5D-8A23-4A054FF0D388}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function _GetDEFAULT_CHANNEL_ID: JString; cdecl;
    {class} function init(id: JString; name: JCharSequence; importance: Integer): JNotificationChannel; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
    {class} property DEFAULT_CHANNEL_ID: JString read _GetDEFAULT_CHANNEL_ID;
  end;

  [JavaSignature('android/app/NotificationChannel')]
  JNotificationChannel = interface(JObject)
    ['{717B07CA-7843-4334-97A7-DD51210E263E}']
    function canBypassDnd: Boolean; cdecl;
    function canShowBadge: Boolean; cdecl;
    function describeContents: Integer; cdecl;
    procedure enableLights(lights: Boolean); cdecl;
    procedure enableVibration(vibration: Boolean); cdecl;
    function equals(o: JObject): Boolean; cdecl;
    //function getAudioAttributes: JAudioAttributes; cdecl;
    function getDescription: JString; cdecl;
    function getGroup: JString; cdecl;
    function getId: JString; cdecl;
    function getImportance: Integer; cdecl;
    function getLightColor: Integer; cdecl;
    function getLockscreenVisibility: Integer; cdecl;
    function getName: JCharSequence; cdecl;
    //function getSound: Jnet_Uri; cdecl;
    function getVibrationPattern: TJavaArray<Int64>; cdecl;
    function hashCode: Integer; cdecl;
    procedure setBypassDnd(bypassDnd: Boolean); cdecl;
    procedure setDescription(description: JString); cdecl;
    procedure setGroup(groupId: JString); cdecl;
    procedure setImportance(importance: Integer); cdecl;
    procedure setLightColor(argb: Integer); cdecl;
    procedure setLockscreenVisibility(lockscreenVisibility: Integer); cdecl;
    procedure setName(name: JCharSequence); cdecl;
    procedure setShowBadge(showBadge: Boolean); cdecl;
    procedure setSound(sound: Jnet_Uri; audioAttributes: JAudioAttributes); cdecl; // https://quality.embarcadero.com/browse/RSP-21294
    procedure setVibrationPattern(vibrationPattern: TJavaArray<Int64>); cdecl;
    function shouldShowLights: Boolean; cdecl;
    function shouldVibrate: Boolean; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
  end;
  TJNotificationChannel = class(TJavaGenericImport<JNotificationChannelClass, JNotificationChannel>) end;

  JNotificationChannelGroupClass = interface(JObjectClass)
    ['{FB06B823-E876-440D-A9EE-399ED2173A61}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function init(id: JString; name: JCharSequence): JNotificationChannelGroup; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('android/app/NotificationChannelGroup')]
  JNotificationChannelGroup = interface(JObject)
    ['{62A94A65-A75E-4351-8FEB-CCAAD6ED5E2D}']
    function clone: JNotificationChannelGroup; cdecl;
    function describeContents: Integer; cdecl;
    function equals(o: JObject): Boolean; cdecl;
    function getChannels: JList; cdecl;
    function getId: JString; cdecl;
    function getName: JCharSequence; cdecl;
    function hashCode: Integer; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
  end;
  TJNotificationChannelGroup = class(TJavaGenericImport<JNotificationChannelGroupClass, JNotificationChannelGroup>) end;

  JNotificationManagerClass = interface(JObjectClass)
    ['{66101C50-DAE9-4C81-8186-81A0A43A73BD}']
    {class} function _GetACTION_INTERRUPTION_FILTER_CHANGED: JString; cdecl;
    {class} function _GetACTION_NOTIFICATION_POLICY_ACCESS_GRANTED_CHANGED: JString; cdecl;
    {class} function _GetACTION_NOTIFICATION_POLICY_CHANGED: JString; cdecl;
    {class} function _GetIMPORTANCE_DEFAULT: Integer; cdecl;
    {class} function _GetIMPORTANCE_HIGH: Integer; cdecl;
    {class} function _GetIMPORTANCE_LOW: Integer; cdecl;
    {class} function _GetIMPORTANCE_MAX: Integer; cdecl;
    {class} function _GetIMPORTANCE_MIN: Integer; cdecl;
    {class} function _GetIMPORTANCE_NONE: Integer; cdecl;
    {class} function _GetIMPORTANCE_UNSPECIFIED: Integer; cdecl;
    {class} function _GetINTERRUPTION_FILTER_ALARMS: Integer; cdecl;
    {class} function _GetINTERRUPTION_FILTER_ALL: Integer; cdecl;
    {class} function _GetINTERRUPTION_FILTER_NONE: Integer; cdecl;
    {class} function _GetINTERRUPTION_FILTER_PRIORITY: Integer; cdecl;
    {class} function _GetINTERRUPTION_FILTER_UNKNOWN: Integer; cdecl;
    {class} property ACTION_INTERRUPTION_FILTER_CHANGED: JString read _GetACTION_INTERRUPTION_FILTER_CHANGED;
    {class} property ACTION_NOTIFICATION_POLICY_ACCESS_GRANTED_CHANGED: JString read _GetACTION_NOTIFICATION_POLICY_ACCESS_GRANTED_CHANGED;
    {class} property ACTION_NOTIFICATION_POLICY_CHANGED: JString read _GetACTION_NOTIFICATION_POLICY_CHANGED;
    {class} property IMPORTANCE_DEFAULT: Integer read _GetIMPORTANCE_DEFAULT;
    {class} property IMPORTANCE_HIGH: Integer read _GetIMPORTANCE_HIGH;
    {class} property IMPORTANCE_LOW: Integer read _GetIMPORTANCE_LOW;
    {class} property IMPORTANCE_MAX: Integer read _GetIMPORTANCE_MAX;
    {class} property IMPORTANCE_MIN: Integer read _GetIMPORTANCE_MIN;
    {class} property IMPORTANCE_NONE: Integer read _GetIMPORTANCE_NONE;
    {class} property IMPORTANCE_UNSPECIFIED: Integer read _GetIMPORTANCE_UNSPECIFIED;
    {class} property INTERRUPTION_FILTER_ALARMS: Integer read _GetINTERRUPTION_FILTER_ALARMS;
    {class} property INTERRUPTION_FILTER_ALL: Integer read _GetINTERRUPTION_FILTER_ALL;
    {class} property INTERRUPTION_FILTER_NONE: Integer read _GetINTERRUPTION_FILTER_NONE;
    {class} property INTERRUPTION_FILTER_PRIORITY: Integer read _GetINTERRUPTION_FILTER_PRIORITY;
    {class} property INTERRUPTION_FILTER_UNKNOWN: Integer read _GetINTERRUPTION_FILTER_UNKNOWN;
  end;

  [JavaSignature('android/app/NotificationManager')]
  JNotificationManager = interface(JObject)
    ['{C3E111F8-A16B-4E14-AEDA-C4E38AF03C49}']
    function addAutomaticZenRule(automaticZenRule: JAutomaticZenRule): JString; cdecl;
    function areNotificationsEnabled: Boolean; cdecl;
    procedure cancel(id: Integer); cdecl; overload;
    procedure cancel(tag: JString; id: Integer); cdecl; overload;
    procedure cancelAll; cdecl;
    procedure createNotificationChannel(channel: JNotificationChannel); cdecl;
    procedure createNotificationChannelGroup(group: JNotificationChannelGroup); cdecl;
    procedure createNotificationChannelGroups(groups: JList); cdecl;
    procedure createNotificationChannels(channels: JList); cdecl;
    procedure deleteNotificationChannel(channelId: JString); cdecl;
    procedure deleteNotificationChannelGroup(groupId: JString); cdecl;
    //function getActiveNotifications: TJavaObjectArray<JStatusBarNotification>; cdecl;
    function getAutomaticZenRule(id: JString): JAutomaticZenRule; cdecl;
    function getAutomaticZenRules: JMap; cdecl;
    function getCurrentInterruptionFilter: Integer; cdecl;
    function getImportance: Integer; cdecl;
    function getNotificationChannel(channelId: JString): JNotificationChannel; cdecl;
    function getNotificationChannelGroups: JList; cdecl;
    function getNotificationChannels: JList; cdecl;
    function getNotificationPolicy: JNotificationManager_Policy; cdecl;
    function isNotificationPolicyAccessGranted: Boolean; cdecl;
    procedure notify(id: Integer; notification: JNotification); cdecl; overload;
    procedure notify(tag: JString; id: Integer; notification: JNotification); cdecl; overload;
    function removeAutomaticZenRule(id: JString): Boolean; cdecl;
    procedure setInterruptionFilter(interruptionFilter: Integer); cdecl;
    procedure setNotificationPolicy(policy: JNotificationManager_Policy); cdecl;
    function updateAutomaticZenRule(id: JString; automaticZenRule: JAutomaticZenRule): Boolean; cdecl;
  end;
  TJNotificationManager = class(TJavaGenericImport<JNotificationManagerClass, JNotificationManager>) end;

  JNotificationManager_PolicyClass = interface(JObjectClass)
    ['{4943DA07-3DCB-4AB1-9E33-4BE16146172F}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function _GetPRIORITY_CATEGORY_CALLS: Integer; cdecl;
    {class} function _GetPRIORITY_CATEGORY_EVENTS: Integer; cdecl;
    {class} function _GetPRIORITY_CATEGORY_MESSAGES: Integer; cdecl;
    {class} function _GetPRIORITY_CATEGORY_REMINDERS: Integer; cdecl;
    {class} function _GetPRIORITY_CATEGORY_REPEAT_CALLERS: Integer; cdecl;
    {class} function _GetPRIORITY_SENDERS_ANY: Integer; cdecl;
    {class} function _GetPRIORITY_SENDERS_CONTACTS: Integer; cdecl;
    {class} function _GetPRIORITY_SENDERS_STARRED: Integer; cdecl;
    {class} function _GetSUPPRESSED_EFFECT_SCREEN_OFF: Integer; cdecl;
    {class} function _GetSUPPRESSED_EFFECT_SCREEN_ON: Integer; cdecl;
    {class} function init(priorityCategories: Integer; priorityCallSenders: Integer; priorityMessageSenders: Integer): JNotificationManager_Policy; cdecl; overload;
    {class} function init(priorityCategories: Integer; priorityCallSenders: Integer; priorityMessageSenders: Integer; suppressedVisualEffects: Integer): JNotificationManager_Policy; cdecl; overload;
    {class} function priorityCategoriesToString(priorityCategories: Integer): JString; cdecl;
    {class} function prioritySendersToString(prioritySenders: Integer): JString; cdecl;
    {class} function suppressedEffectsToString(effects: Integer): JString; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
    {class} property PRIORITY_CATEGORY_CALLS: Integer read _GetPRIORITY_CATEGORY_CALLS;
    {class} property PRIORITY_CATEGORY_EVENTS: Integer read _GetPRIORITY_CATEGORY_EVENTS;
    {class} property PRIORITY_CATEGORY_MESSAGES: Integer read _GetPRIORITY_CATEGORY_MESSAGES;
    {class} property PRIORITY_CATEGORY_REMINDERS: Integer read _GetPRIORITY_CATEGORY_REMINDERS;
    {class} property PRIORITY_CATEGORY_REPEAT_CALLERS: Integer read _GetPRIORITY_CATEGORY_REPEAT_CALLERS;
    {class} property PRIORITY_SENDERS_ANY: Integer read _GetPRIORITY_SENDERS_ANY;
    {class} property PRIORITY_SENDERS_CONTACTS: Integer read _GetPRIORITY_SENDERS_CONTACTS;
    {class} property PRIORITY_SENDERS_STARRED: Integer read _GetPRIORITY_SENDERS_STARRED;
    {class} property SUPPRESSED_EFFECT_SCREEN_OFF: Integer read _GetSUPPRESSED_EFFECT_SCREEN_OFF;
    {class} property SUPPRESSED_EFFECT_SCREEN_ON: Integer read _GetSUPPRESSED_EFFECT_SCREEN_ON;
  end;

  [JavaSignature('android/app/NotificationManager$Policy')]
  JNotificationManager_Policy = interface(JObject)
    ['{D3E4D12A-9CA9-40B2-8130-58F9A19DF9D2}']
    function _GetpriorityCallSenders: Integer; cdecl;
    function _GetpriorityCategories: Integer; cdecl;
    function _GetpriorityMessageSenders: Integer; cdecl;
    function _GetsuppressedVisualEffects: Integer; cdecl;
    function describeContents: Integer; cdecl;
    function equals(o: JObject): Boolean; cdecl;
    function hashCode: Integer; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
    property priorityCallSenders: Integer read _GetpriorityCallSenders;
    property priorityCategories: Integer read _GetpriorityCategories;
    property priorityMessageSenders: Integer read _GetpriorityMessageSenders;
    property suppressedVisualEffects: Integer read _GetsuppressedVisualEffects;
  end;
  TJNotificationManager_Policy = class(TJavaGenericImport<JNotificationManager_PolicyClass, JNotificationManager_Policy>) end;

  JPendingIntentClass = interface(JObjectClass)
    ['{3C96E145-86EE-487F-9068-C48BA32D0E89}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function _GetFLAG_CANCEL_CURRENT: Integer; cdecl;
    {class} function _GetFLAG_IMMUTABLE: Integer; cdecl;
    {class} function _GetFLAG_NO_CREATE: Integer; cdecl;
    {class} function _GetFLAG_ONE_SHOT: Integer; cdecl;
    {class} function _GetFLAG_UPDATE_CURRENT: Integer; cdecl;
    {class} function getActivities(context: JContext; requestCode: Integer; intents: TJavaObjectArray<JIntent>; flags: Integer): JPendingIntent; cdecl; overload;
    {class} function getActivities(context: JContext; requestCode: Integer; intents: TJavaObjectArray<JIntent>; flags: Integer; options: JBundle): JPendingIntent; cdecl; overload;
    {class} function getActivity(context: JContext; requestCode: Integer; intent: JIntent; flags: Integer): JPendingIntent; cdecl; overload;
    {class} function getActivity(context: JContext; requestCode: Integer; intent: JIntent; flags: Integer; options: JBundle): JPendingIntent; cdecl; overload;
    {class} function getBroadcast(context: JContext; requestCode: Integer; intent: JIntent; flags: Integer): JPendingIntent; cdecl;
    {class} function getForegroundService(context: JContext; requestCode: Integer; intent: JIntent; flags: Integer): JPendingIntent; cdecl;
    {class} function getService(context: JContext; requestCode: Integer; intent: JIntent; flags: Integer): JPendingIntent; cdecl;
    {class} function readPendingIntentOrNullFromParcel(in_: JParcel): JPendingIntent; cdecl;
    {class} procedure writePendingIntentOrNullToParcel(sender: JPendingIntent; out_: JParcel); cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
    {class} property FLAG_CANCEL_CURRENT: Integer read _GetFLAG_CANCEL_CURRENT;
    {class} property FLAG_IMMUTABLE: Integer read _GetFLAG_IMMUTABLE;
    {class} property FLAG_NO_CREATE: Integer read _GetFLAG_NO_CREATE;
    {class} property FLAG_ONE_SHOT: Integer read _GetFLAG_ONE_SHOT;
    {class} property FLAG_UPDATE_CURRENT: Integer read _GetFLAG_UPDATE_CURRENT;
  end;

  [JavaSignature('android/app/PendingIntent')]
  JPendingIntent = interface(JObject)
    ['{6475B1E1-B04F-4269-B4EE-33EED669FDDD}']
    procedure cancel; cdecl;
    function describeContents: Integer; cdecl;
    function equals(otherObj: JObject): Boolean; cdecl;
    function getCreatorPackage: JString; cdecl;
    function getCreatorUid: Integer; cdecl;
    function getCreatorUserHandle: JUserHandle; cdecl;
    function getIntentSender: JIntentSender; cdecl;
    function getTargetPackage: JString; cdecl;//Deprecated
    function hashCode: Integer; cdecl;
    procedure send; cdecl; overload;
    procedure send(code: Integer); cdecl; overload;
    procedure send(context: JContext; code: Integer; intent: JIntent); cdecl; overload;
    procedure send(code: Integer; onFinished: JPendingIntent_OnFinished; handler: JHandler); cdecl; overload;
    procedure send(context: JContext; code: Integer; intent: JIntent; onFinished: JPendingIntent_OnFinished; handler: JHandler); cdecl; overload;
    procedure send(context: JContext; code: Integer; intent: JIntent; onFinished: JPendingIntent_OnFinished; handler: JHandler; requiredPermission: JString); cdecl; overload;
    procedure send(context: JContext; code: Integer; intent: JIntent; onFinished: JPendingIntent_OnFinished; handler: JHandler; requiredPermission: JString; options: JBundle); cdecl; overload;
    function toString: JString; cdecl;
    procedure writeToParcel(out_: JParcel; flags: Integer); cdecl;
  end;
  TJPendingIntent = class(TJavaGenericImport<JPendingIntentClass, JPendingIntent>) end;

  JPendingIntent_OnFinishedClass = interface(IJavaClass)
    ['{CA268228-C808-4F93-AF5A-F9B15D8A480C}']
  end;

  [JavaSignature('android/app/PendingIntent$OnFinished')]
  JPendingIntent_OnFinished = interface(IJavaInstance)
    ['{7420C1F1-8701-4496-B2EB-20C427D3FC50}']
    procedure onSendFinished(pendingIntent: JPendingIntent; intent: JIntent; resultCode: Integer; resultData: JString; resultExtras: JBundle); cdecl;
  end;
  TJPendingIntent_OnFinished = class(TJavaGenericImport<JPendingIntent_OnFinishedClass, JPendingIntent_OnFinished>) end;

  JPictureInPictureArgsClass = interface(JObjectClass)
    ['{EC161415-177E-4D89-8AB2-9AB761219D70}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function convert(params: JPictureInPictureParams): JPictureInPictureArgs; cdecl; overload;
    {class} function convert(args: JPictureInPictureArgs): JPictureInPictureParams; cdecl; overload;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('android/app/PictureInPictureArgs')]
  JPictureInPictureArgs = interface(JObject)
    ['{641C62E3-E159-4A8F-9DBF-62A16C8B22F7}']
    function describeContents: Integer; cdecl;
    procedure writeToParcel(out_: JParcel; flags: Integer); cdecl;
  end;
  TJPictureInPictureArgs = class(TJavaGenericImport<JPictureInPictureArgsClass, JPictureInPictureArgs>) end;

  JPictureInPictureParamsClass = interface(JObjectClass)
    ['{B7630B29-BF9E-4B7B-B429-09CB1FC45170}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('android/app/PictureInPictureParams')]
  JPictureInPictureParams = interface(JObject)
    ['{F8BAC3CF-E8D5-4D65-853F-050837FD4B2C}']
    function describeContents: Integer; cdecl;
    procedure writeToParcel(out_: JParcel; flags: Integer); cdecl;
  end;
  TJPictureInPictureParams = class(TJavaGenericImport<JPictureInPictureParamsClass, JPictureInPictureParams>) end;

  JRemoteInputClass = interface(JObjectClass)
    ['{C6208158-16A4-43A6-A8EE-65EACD620921}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function _GetEXTRA_RESULTS_DATA: JString; cdecl;
    {class} function _GetRESULTS_CLIP_LABEL: JString; cdecl;
    {class} procedure addDataResultToIntent(remoteInput: JRemoteInput; intent: JIntent; results: JMap); cdecl;
    {class} procedure addResultsToIntent(remoteInputs: TJavaObjectArray<JRemoteInput>; intent: JIntent; results: JBundle); cdecl;
    {class} function getDataResultsFromIntent(intent: JIntent; remoteInputResultKey: JString): JMap; cdecl;
    {class} function getResultsFromIntent(intent: JIntent): JBundle; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
    {class} property EXTRA_RESULTS_DATA: JString read _GetEXTRA_RESULTS_DATA;
    {class} property RESULTS_CLIP_LABEL: JString read _GetRESULTS_CLIP_LABEL;
  end;

  [JavaSignature('android/app/RemoteInput')]
  JRemoteInput = interface(JObject)
    ['{1B83290E-C8F7-4F27-B5FE-E6E58EB14C44}']
    function describeContents: Integer; cdecl;
    function getAllowFreeFormInput: Boolean; cdecl;
    function getAllowedDataTypes: JSet; cdecl;
    function getChoices: TJavaObjectArray<JCharSequence>; cdecl;
    function getExtras: JBundle; cdecl;
    function getLabel: JCharSequence; cdecl;
    function getResultKey: JString; cdecl;
    function isDataOnly: Boolean; cdecl;
    procedure writeToParcel(out_: JParcel; flags: Integer); cdecl;
  end;
  TJRemoteInput = class(TJavaGenericImport<JRemoteInputClass, JRemoteInput>) end;

  JSharedElementCallbackClass = interface(JObjectClass)
    ['{F1C6F8CE-732A-4B37-A24F-C2BB1AA5AABB}']
    {class} function init: JSharedElementCallback; cdecl;
  end;

  [JavaSignature('android/app/SharedElementCallback')]
  JSharedElementCallback = interface(JObject)
    ['{1A8B6AC8-E66F-4832-AC50-A9A7941026C7}']
    function onCaptureSharedElementSnapshot(sharedElement: JView; viewToGlobalMatrix: JMatrix; screenBounds: JRectF): JParcelable; cdecl;
    function onCreateSnapshotView(context: JContext; snapshot: JParcelable): JView; cdecl;
    procedure onMapSharedElements(names: JList; sharedElements: JMap); cdecl;
    procedure onRejectSharedElements(rejectedSharedElements: JList); cdecl;
    procedure onSharedElementEnd(sharedElementNames: JList; sharedElements: JList; sharedElementSnapshots: JList); cdecl;
    procedure onSharedElementStart(sharedElementNames: JList; sharedElements: JList; sharedElementSnapshots: JList); cdecl;
    procedure onSharedElementsArrived(sharedElementNames: JList; sharedElements: JList; listener: JSharedElementCallback_OnSharedElementsReadyListener); cdecl;
  end;
  TJSharedElementCallback = class(TJavaGenericImport<JSharedElementCallbackClass, JSharedElementCallback>) end;

  JSharedElementCallback_OnSharedElementsReadyListenerClass = interface(IJavaClass)
    ['{683B85EE-34A2-4CFC-B8E3-73597DD63505}']
  end;

  [JavaSignature('android/app/SharedElementCallback$OnSharedElementsReadyListener')]
  JSharedElementCallback_OnSharedElementsReadyListener = interface(IJavaInstance)
    ['{CF94CD8E-B75D-4BF5-899D-C338B6A1FA0F}']
    procedure onSharedElementsReady; cdecl;
  end;
  TJSharedElementCallback_OnSharedElementsReadyListener = class(TJavaGenericImport<JSharedElementCallback_OnSharedElementsReadyListenerClass, JSharedElementCallback_OnSharedElementsReadyListener>) end;

  JTaskStackBuilderClass = interface(JObjectClass)
    ['{CBF458C4-738C-4F67-BCF1-C94E8A1DA3FE}']
    {class} function create(context: JContext): JTaskStackBuilder; cdecl;
  end;

  [JavaSignature('android/app/TaskStackBuilder')]
  JTaskStackBuilder = interface(JObject)
    ['{53D84177-E954-404C-A9C9-ED6578A29492}']
    function addNextIntent(nextIntent: JIntent): JTaskStackBuilder; cdecl;
    function addNextIntentWithParentStack(nextIntent: JIntent): JTaskStackBuilder; cdecl;
    function addParentStack(sourceActivity: JActivity): JTaskStackBuilder; cdecl; overload;
    function addParentStack(sourceActivityClass: Jlang_Class): JTaskStackBuilder; cdecl; overload;
    function addParentStack(sourceActivityName: JComponentName): JTaskStackBuilder; cdecl; overload;
    function editIntentAt(index: Integer): JIntent; cdecl;
    function getIntentCount: Integer; cdecl;
    function getIntents: TJavaObjectArray<JIntent>; cdecl;
    function getPendingIntent(requestCode: Integer; flags: Integer): JPendingIntent; cdecl; overload;
    function getPendingIntent(requestCode: Integer; flags: Integer; options: JBundle): JPendingIntent; cdecl; overload;
    procedure startActivities; cdecl; overload;
    procedure startActivities(options: JBundle); cdecl; overload;
  end;
  TJTaskStackBuilder = class(TJavaGenericImport<JTaskStackBuilderClass, JTaskStackBuilder>) end;

  JVoiceInteractorClass = interface(JObjectClass)
    ['{AD10DB7A-5114-46EC-87FD-2092621052F1}']
  end;

  [JavaSignature('android/app/VoiceInteractor')]
  JVoiceInteractor = interface(JObject)
    ['{11EB56F4-1B91-408F-A7B1-E38D5D2FD1DC}']
    function getActiveRequest(name: JString): JVoiceInteractor_Request; cdecl;
    function getActiveRequests: TJavaObjectArray<JVoiceInteractor_Request>; cdecl;
    function submitRequest(request: JVoiceInteractor_Request): Boolean; cdecl; overload;
    function submitRequest(request: JVoiceInteractor_Request; name: JString): Boolean; cdecl; overload;
    function supportsCommands(commands: TJavaObjectArray<JString>): TJavaArray<Boolean>; cdecl;
  end;
  TJVoiceInteractor = class(TJavaGenericImport<JVoiceInteractorClass, JVoiceInteractor>) end;

  JVoiceInteractor_RequestClass = interface(JObjectClass)
    ['{0D89D94A-0D69-4050-BAC4-1E912A60A72B}']
  end;

  [JavaSignature('android/app/VoiceInteractor$Request')]
  JVoiceInteractor_Request = interface(JObject)
    ['{96811C7A-4003-4F43-9914-75DA30B0914E}']
    procedure cancel; cdecl;
    function getActivity: JActivity; cdecl;
    function getContext: JContext; cdecl;
    function getName: JString; cdecl;
    procedure onAttached(activity: JActivity); cdecl;
    procedure onCancel; cdecl;
    procedure onDetached; cdecl;
    function toString: JString; cdecl;
  end;
  TJVoiceInteractor_Request = class(TJavaGenericImport<JVoiceInteractor_RequestClass, JVoiceInteractor_Request>) end;

  JAssistContentClass = interface(JObjectClass)
    ['{C6C2FEE4-0A35-4D12-AD42-C718B4B98962}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function init: JAssistContent; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('android/app/assist/AssistContent')]
  JAssistContent = interface(JObject)
    ['{0422FFD0-CE51-4315-981B-55EE11692512}']
    function describeContents: Integer; cdecl;
    function getClipData: JClipData; cdecl;
    function getExtras: JBundle; cdecl;
    function getIntent: JIntent; cdecl;
    function getStructuredData: JString; cdecl;
    //function getWebUri: Jnet_Uri; cdecl;
    function isAppProvidedIntent: Boolean; cdecl;
    function isAppProvidedWebUri: Boolean; cdecl;
    procedure setClipData(clip: JClipData); cdecl;
    procedure setIntent(intent: JIntent); cdecl;
    procedure setStructuredData(structuredData: JString); cdecl;
    //procedure setWebUri(uri: Jnet_Uri); cdecl;
    procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
  end;
  TJAssistContent = class(TJavaGenericImport<JAssistContentClass, JAssistContent>) end;

  JAppWidgetProviderInfoClass = interface(JObjectClass)
    ['{EEE7161A-CC7D-498F-871A-DCD837668C6C}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function _GetRESIZE_BOTH: Integer; cdecl;
    {class} function _GetRESIZE_HORIZONTAL: Integer; cdecl;
    {class} function _GetRESIZE_NONE: Integer; cdecl;
    {class} function _GetRESIZE_VERTICAL: Integer; cdecl;
    {class} function _GetWIDGET_CATEGORY_HOME_SCREEN: Integer; cdecl;
    {class} function _GetWIDGET_CATEGORY_KEYGUARD: Integer; cdecl;
    {class} function _GetWIDGET_CATEGORY_SEARCHBOX: Integer; cdecl;
    {class} function init: JAppWidgetProviderInfo; cdecl; overload;
    {class} function init(in_: JParcel): JAppWidgetProviderInfo; cdecl; overload;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
    {class} property RESIZE_BOTH: Integer read _GetRESIZE_BOTH;
    {class} property RESIZE_HORIZONTAL: Integer read _GetRESIZE_HORIZONTAL;
    {class} property RESIZE_NONE: Integer read _GetRESIZE_NONE;
    {class} property RESIZE_VERTICAL: Integer read _GetRESIZE_VERTICAL;
    {class} property WIDGET_CATEGORY_HOME_SCREEN: Integer read _GetWIDGET_CATEGORY_HOME_SCREEN;
    {class} property WIDGET_CATEGORY_KEYGUARD: Integer read _GetWIDGET_CATEGORY_KEYGUARD;
    {class} property WIDGET_CATEGORY_SEARCHBOX: Integer read _GetWIDGET_CATEGORY_SEARCHBOX;
  end;

  [JavaSignature('android/appwidget/AppWidgetProviderInfo')]
  JAppWidgetProviderInfo = interface(JObject)
    ['{42141953-BFF9-4343-8F8A-84550E4F3A85}']
    function _GetautoAdvanceViewId: Integer; cdecl;
    procedure _SetautoAdvanceViewId(Value: Integer); cdecl;
    function _Getconfigure: JComponentName; cdecl;
    procedure _Setconfigure(Value: JComponentName); cdecl;
    function _Geticon: Integer; cdecl;
    procedure _Seticon(Value: Integer); cdecl;
    function _GetinitialKeyguardLayout: Integer; cdecl;
    procedure _SetinitialKeyguardLayout(Value: Integer); cdecl;
    function _GetinitialLayout: Integer; cdecl;
    procedure _SetinitialLayout(Value: Integer); cdecl;
    function _Getlabel: JString; cdecl;
    procedure _Setlabel(Value: JString); cdecl;
    function _GetminHeight: Integer; cdecl;
    procedure _SetminHeight(Value: Integer); cdecl;
    function _GetminResizeHeight: Integer; cdecl;
    procedure _SetminResizeHeight(Value: Integer); cdecl;
    function _GetminResizeWidth: Integer; cdecl;
    procedure _SetminResizeWidth(Value: Integer); cdecl;
    function _GetminWidth: Integer; cdecl;
    procedure _SetminWidth(Value: Integer); cdecl;
    function _GetpreviewImage: Integer; cdecl;
    procedure _SetpreviewImage(Value: Integer); cdecl;
    function _Getprovider: JComponentName; cdecl;
    procedure _Setprovider(Value: JComponentName); cdecl;
    function _GetresizeMode: Integer; cdecl;
    procedure _SetresizeMode(Value: Integer); cdecl;
    function _GetupdatePeriodMillis: Integer; cdecl;
    procedure _SetupdatePeriodMillis(Value: Integer); cdecl;
    function _GetwidgetCategory: Integer; cdecl;
    procedure _SetwidgetCategory(Value: Integer); cdecl;
    function clone: JAppWidgetProviderInfo; cdecl;
    function describeContents: Integer; cdecl;
    function getProfile: JUserHandle; cdecl;
    function loadIcon(context: JContext; density: Integer): JDrawable; cdecl;
    function loadLabel(packageManager: JPackageManager): JString; cdecl;
    function loadPreviewImage(context: JContext; density: Integer): JDrawable; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(out_: JParcel; flags: Integer); cdecl;
    property autoAdvanceViewId: Integer read _GetautoAdvanceViewId write _SetautoAdvanceViewId;
    property configure: JComponentName read _Getconfigure write _Setconfigure;
    property icon: Integer read _Geticon write _Seticon;
    property initialKeyguardLayout: Integer read _GetinitialKeyguardLayout write _SetinitialKeyguardLayout;
    property initialLayout: Integer read _GetinitialLayout write _SetinitialLayout;
    property &label: JString read _Getlabel write _Setlabel;
    property minHeight: Integer read _GetminHeight write _SetminHeight;
    property minResizeHeight: Integer read _GetminResizeHeight write _SetminResizeHeight;
    property minResizeWidth: Integer read _GetminResizeWidth write _SetminResizeWidth;
    property minWidth: Integer read _GetminWidth write _SetminWidth;
    property previewImage: Integer read _GetpreviewImage write _SetpreviewImage;
    property provider: JComponentName read _Getprovider write _Setprovider;
    property resizeMode: Integer read _GetresizeMode write _SetresizeMode;
    property updatePeriodMillis: Integer read _GetupdatePeriodMillis write _SetupdatePeriodMillis;
    property widgetCategory: Integer read _GetwidgetCategory write _SetwidgetCategory;
  end;
  TJAppWidgetProviderInfo = class(TJavaGenericImport<JAppWidgetProviderInfoClass, JAppWidgetProviderInfo>) end;

implementation

procedure RegisterTypes;
begin
  TRegTypes.RegisterType('Androidapi.JNI.App.JAudioAttributes', TypeInfo(Androidapi.JNI.App.JAudioAttributes)); // dummy class to avoid circular reference - https://quality.embarcadero.com/browse/RSP-21294
  TRegTypes.RegisterType('Androidapi.JNI.App.JActionBar', TypeInfo(Androidapi.JNI.App.JActionBar));
  TRegTypes.RegisterType('Androidapi.JNI.App.JActionBar_LayoutParams', TypeInfo(Androidapi.JNI.App.JActionBar_LayoutParams));
  TRegTypes.RegisterType('Androidapi.JNI.App.JActionBar_OnMenuVisibilityListener', TypeInfo(Androidapi.JNI.App.JActionBar_OnMenuVisibilityListener));
  TRegTypes.RegisterType('Androidapi.JNI.App.JActionBar_OnNavigationListener', TypeInfo(Androidapi.JNI.App.JActionBar_OnNavigationListener));
  TRegTypes.RegisterType('Androidapi.JNI.App.JActionBar_Tab', TypeInfo(Androidapi.JNI.App.JActionBar_Tab));
  TRegTypes.RegisterType('Androidapi.JNI.App.JActionBar_TabListener', TypeInfo(Androidapi.JNI.App.JActionBar_TabListener));
  TRegTypes.RegisterType('Androidapi.JNI.App.JActivity', TypeInfo(Androidapi.JNI.App.JActivity));
  TRegTypes.RegisterType('Androidapi.JNI.App.JActivityManager', TypeInfo(Androidapi.JNI.App.JActivityManager));
  TRegTypes.RegisterType('Androidapi.JNI.App.JActivityManager_MemoryInfo', TypeInfo(Androidapi.JNI.App.JActivityManager_MemoryInfo));
  TRegTypes.RegisterType('Androidapi.JNI.App.JActivityManager_RunningAppProcessInfo', TypeInfo(Androidapi.JNI.App.JActivityManager_RunningAppProcessInfo));
  TRegTypes.RegisterType('Androidapi.JNI.App.JActivityManager_TaskDescription', TypeInfo(Androidapi.JNI.App.JActivityManager_TaskDescription));
  TRegTypes.RegisterType('Androidapi.JNI.App.JAlarmManager', TypeInfo(Androidapi.JNI.App.JAlarmManager));
  TRegTypes.RegisterType('Androidapi.JNI.App.JAlarmManager_AlarmClockInfo', TypeInfo(Androidapi.JNI.App.JAlarmManager_AlarmClockInfo));
  TRegTypes.RegisterType('Androidapi.JNI.App.JAlarmManager_OnAlarmListener', TypeInfo(Androidapi.JNI.App.JAlarmManager_OnAlarmListener));
  TRegTypes.RegisterType('Androidapi.JNI.App.JDialog', TypeInfo(Androidapi.JNI.App.JDialog));
  TRegTypes.RegisterType('Androidapi.JNI.App.JAlertDialog', TypeInfo(Androidapi.JNI.App.JAlertDialog));
  TRegTypes.RegisterType('Androidapi.JNI.App.JAlertDialog_Builder', TypeInfo(Androidapi.JNI.App.JAlertDialog_Builder));
  TRegTypes.RegisterType('Androidapi.JNI.App.JApplication', TypeInfo(Androidapi.JNI.App.JApplication));
  TRegTypes.RegisterType('Androidapi.JNI.App.JApplication_ActivityLifecycleCallbacks', TypeInfo(Androidapi.JNI.App.JApplication_ActivityLifecycleCallbacks));
  TRegTypes.RegisterType('Androidapi.JNI.App.JApplication_OnProvideAssistDataListener', TypeInfo(Androidapi.JNI.App.JApplication_OnProvideAssistDataListener));
  TRegTypes.RegisterType('Androidapi.JNI.App.JAutomaticZenRule', TypeInfo(Androidapi.JNI.App.JAutomaticZenRule));
  TRegTypes.RegisterType('Androidapi.JNI.App.JFragment', TypeInfo(Androidapi.JNI.App.JFragment));
  TRegTypes.RegisterType('Androidapi.JNI.App.JDialogFragment', TypeInfo(Androidapi.JNI.App.JDialogFragment));
  TRegTypes.RegisterType('Androidapi.JNI.App.JFragment_SavedState', TypeInfo(Androidapi.JNI.App.JFragment_SavedState));
  TRegTypes.RegisterType('Androidapi.JNI.App.JFragmentManager', TypeInfo(Androidapi.JNI.App.JFragmentManager));
  TRegTypes.RegisterType('Androidapi.JNI.App.JFragmentManager_BackStackEntry', TypeInfo(Androidapi.JNI.App.JFragmentManager_BackStackEntry));
  TRegTypes.RegisterType('Androidapi.JNI.App.JFragmentManager_FragmentLifecycleCallbacks', TypeInfo(Androidapi.JNI.App.JFragmentManager_FragmentLifecycleCallbacks));
  TRegTypes.RegisterType('Androidapi.JNI.App.JFragmentManager_OnBackStackChangedListener', TypeInfo(Androidapi.JNI.App.JFragmentManager_OnBackStackChangedListener));
  TRegTypes.RegisterType('Androidapi.JNI.App.JFragmentTransaction', TypeInfo(Androidapi.JNI.App.JFragmentTransaction));
  TRegTypes.RegisterType('Androidapi.JNI.App.JService', TypeInfo(Androidapi.JNI.App.JService));
  TRegTypes.RegisterType('Androidapi.JNI.App.JIntentService', TypeInfo(Androidapi.JNI.App.JIntentService));
  TRegTypes.RegisterType('Androidapi.JNI.App.JLoaderManager', TypeInfo(Androidapi.JNI.App.JLoaderManager));
  TRegTypes.RegisterType('Androidapi.JNI.App.JLoaderManager_LoaderCallbacks', TypeInfo(Androidapi.JNI.App.JLoaderManager_LoaderCallbacks));
  TRegTypes.RegisterType('Androidapi.JNI.App.JNativeActivity', TypeInfo(Androidapi.JNI.App.JNativeActivity));
  TRegTypes.RegisterType('Androidapi.JNI.App.JNotification', TypeInfo(Androidapi.JNI.App.JNotification));
  TRegTypes.RegisterType('Androidapi.JNI.App.JNotification_Action', TypeInfo(Androidapi.JNI.App.JNotification_Action));
  TRegTypes.RegisterType('Androidapi.JNI.App.JNotificationChannel', TypeInfo(Androidapi.JNI.App.JNotificationChannel));
  TRegTypes.RegisterType('Androidapi.JNI.App.JNotificationChannelGroup', TypeInfo(Androidapi.JNI.App.JNotificationChannelGroup));
  TRegTypes.RegisterType('Androidapi.JNI.App.JNotificationManager', TypeInfo(Androidapi.JNI.App.JNotificationManager));
  TRegTypes.RegisterType('Androidapi.JNI.App.JNotificationManager_Policy', TypeInfo(Androidapi.JNI.App.JNotificationManager_Policy));
  TRegTypes.RegisterType('Androidapi.JNI.App.JPendingIntent', TypeInfo(Androidapi.JNI.App.JPendingIntent));
  TRegTypes.RegisterType('Androidapi.JNI.App.JPendingIntent_OnFinished', TypeInfo(Androidapi.JNI.App.JPendingIntent_OnFinished));
  TRegTypes.RegisterType('Androidapi.JNI.App.JPictureInPictureArgs', TypeInfo(Androidapi.JNI.App.JPictureInPictureArgs));
  TRegTypes.RegisterType('Androidapi.JNI.App.JPictureInPictureParams', TypeInfo(Androidapi.JNI.App.JPictureInPictureParams));
  TRegTypes.RegisterType('Androidapi.JNI.App.JRemoteInput', TypeInfo(Androidapi.JNI.App.JRemoteInput));
  TRegTypes.RegisterType('Androidapi.JNI.App.JSharedElementCallback', TypeInfo(Androidapi.JNI.App.JSharedElementCallback));
  TRegTypes.RegisterType('Androidapi.JNI.App.JSharedElementCallback_OnSharedElementsReadyListener', TypeInfo(Androidapi.JNI.App.JSharedElementCallback_OnSharedElementsReadyListener));
  TRegTypes.RegisterType('Androidapi.JNI.App.JTaskStackBuilder', TypeInfo(Androidapi.JNI.App.JTaskStackBuilder));
  TRegTypes.RegisterType('Androidapi.JNI.App.JVoiceInteractor', TypeInfo(Androidapi.JNI.App.JVoiceInteractor));
  TRegTypes.RegisterType('Androidapi.JNI.App.JVoiceInteractor_Request', TypeInfo(Androidapi.JNI.App.JVoiceInteractor_Request));
  TRegTypes.RegisterType('Androidapi.JNI.App.JAssistContent', TypeInfo(Androidapi.JNI.App.JAssistContent));
  TRegTypes.RegisterType('Androidapi.JNI.App.JAppWidgetProviderInfo', TypeInfo(Androidapi.JNI.App.JAppWidgetProviderInfo));
end;

initialization
  RegisterTypes;
end.


