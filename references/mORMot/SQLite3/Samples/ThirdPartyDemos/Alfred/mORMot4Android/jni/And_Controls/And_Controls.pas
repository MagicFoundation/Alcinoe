//------------------------------------------------------------------------------
//
//  And_Controls for Free Pascal & Delphi
//
//   Authors
//     Simon,Choi / 최원식 ,
//                  simonsayz@naver.com
//                  http://blog.naver.com/simonsayz
//
//     LoadMan    / 장양호 ,
//                  wkddidgh@naver.com
//
//     jmpessoa   / José Marques Pessoa
//                  jmpessoa@hotmail.com
//
//   History
//               2013.02.24 ver0.01 Started
//               2013.02.28 ver0.02 added Delphi Style
//               2013.03.01 ver0.03 added sysInfo
//                                  added ImageView
//                                  create Lazarus Project
//               2013.03.02         added Timer
//                                  fixed compiler options ( 1.6MB -> 0.7MB )
//               2013.03.03         added GLSurfaceView
//               2013.03.04         added Native Loading Jpeg,Png (NanoJpeg,BeroPng)
//                                  added GL Texture
//               2013.03.05 ver0.04 added Java Loading Png
//                                  added jIntArray example
//                                  Info. lib size ( fpc 2.6.0 800K -> fpc 2.7.1 280K )
//                                  Restructuring (Iteration #01) -----------
//               2013.03.08 ver0.05 Restructuring (Iteration #02)
//                                  added ScrollView
//                                  added Image WH,Resampler,Save
//               2013.07.13 ver0.06 added TForm
//                                  Restructuring (Iteration #03) -----------
//               2013.07.22 ver0.07 added Back Event for Close
//                                  Restructuring (Itelation #04) -----------
//                                  fixed lower form's event firing
//                                  added Custom View
//                                  added Toast
//                                  added Dialog
//               2013.07.26 ver0.08 Class,Method Cache (Single Thread,Class)
//                                  added ListView
//               2013.07.27         added Object, Class Free
//                                  rename source code
//                                  added Global Variable : App,Class
//                                  fixed close,free
//               2013.07.30 ver0.09 added TEditText Keyboard,Focus
//                                  fixed TEditText Prevent Scroll when 1-Line
//               2013.08.02 ver0.10 added TextView - Enabled
//                                  added ListView - Font Color,Size
//                                  added Control - Color
//                                  added Form - OnClick
//               2013.08.03         added WebView - JavaScript, Event
//               2013.08.05 ver0.11 added Form Object
//                                  Restructuring (Iteration #05) -----------
//                                  jDialogProgress
//               2013.08.11 ver0.12 added Canvas
//                                  added Direct Bitmap access
//               2013.08.14 ver0.13 Fixed Memory Leak
//               2013.08.18 ver0.14 added OpenGL ES1 2D (Stencil)
//               2013.08.21 ver0.15 Fixed jImageBtn Memory Leak
//                                  Fixed Socket Buffer
//               2013.08.23 ver0.16 Fixed Memory Leak for Form,Control
//                                  added Form Stack
//               2013.08.24 ver0.17 added Thread
//               2013.08.26 ver0.18 added OpenGL ES2 2D/3D
//                                  added Button Font Color/Height
//               2013.08.31 ver0.19 added Unified OpenGL Canvas
//                                  added OpenGL ES1,ES2 Simulator
//               2013.09.01 ver0.20 added GLThread on Canvas
//                                  fixed OpenGL Crash
//                                  rename example Name
//               2013.09.06 ver0.21 added Camera Activity
//               2013.09.08 ver0.22 added OpenGL Basic Drawing API
//                                  fixed jImageBtn's Enabled
//               2013.12.22 ver0.23 fixed cdecl (jni function)
//                                  x86 android tested
//                                  Plan to combine jmpessoa's android module wizard
//                                  https://github.com/jmpessoa/lazandroidmodulewizard/blob/master/Laz_And_Controls.pas
//               2014.12.21         Fixed for Android 5.0
//               2015.02.23         Restructuring (Iteraltion #06) ------------
//                                  added Conceptual versioin for tutorial
//               2015.02.27 ver0.24 Tested on Android 5.0.2 (Nexus7 / Galaxy S4)
//               2015.02.28 ver0.25 Fixed AsyncTask,
//                                  added Camera Preview
//               2015.03.01 ver0.26 added jHttp (get,download)
//               2015.03.04 ver0.27 added jHttp (get,download,upload)
//               2015.03.04         added multiline property to TextView  
//                                        by DonAlfredo
//
//
//------------------------------------------------------------------------------
{
jApp
//----------------------
jForm
jTextView
jEditText
jButton
jBtnImage
jButtonExtend
jCheckBox
jRadioButton
jProgressBar

jImageView
jImageViewSubscale

jListViews
jListViewHorizontal
jListViewExtend
jListViewTable
jListViewExpand

jGridView
jScrollView
jScrollViewHorizontal
jViewFlipper
jMyViewFlipper
jViewPager
jMapViewfragment
jMapView
jWebView
jBitmap
jCanvas
jView
jGLViewEvent

jTimer
jTimerThread

jDialogYN
jDialogProgress

jAsyncTask
jApkUpDateThread
jHttpDownThread
jSignPad
jBluetooth
jSH2000prt
jKISvan
jPhoneBook
jSocket
jMP3Player
jVideoView
}

unit And_Controls;

{$IFDef FPC} {$mode delphi} {$packrecords c} {$EndIf}

interface

uses
  SysUtils, Types, Classes, Math,
  And_jni,And_jni_Bridge,And_Controls_Types;

// ----------------------------------------------------------------------------
//  Event Handler  : Java -> Pascal
// ----------------------------------------------------------------------------

// App Event
Procedure Java_Event_pOnAppCreate              (env: PJNIEnv; this: jObject; layout : jObject); cdecl;
Function  Java_Event_pOnAppScreenStyle         (env: PJNIEnv; this: jobject): TScreen_Style; cdecl;
Function  Java_Event_pOnAppScreenOrientation   (env: PJNIEnv; this: jobject): TScreen_Orientation; cdecl;
Procedure Java_Event_pOnAppNewIntent           (env: PJNIEnv; this: jobject); cdecl;
Procedure Java_Event_pOnAppDestroy             (env: PJNIEnv; this: jobject); cdecl;
Procedure Java_Event_pOnAppPause               (env: PJNIEnv; this: jobject); cdecl;
Procedure Java_Event_pOnAppRestart             (env: PJNIEnv; this: jobject); cdecl;
Procedure Java_Event_pOnAppResume              (env: PJNIEnv; this: jobject); cdecl;
Procedure Java_Event_pOnAppActive              (env: PJNIEnv; this: jobject); cdecl;
Procedure Java_Event_pOnAppStop                (env: PJNIEnv; this: jobject); cdecl;
Procedure Java_Event_pOnAppBackPressed         (env: PJNIEnv; this: jobject); cdecl;
Function  Java_Event_pOnAppRotate              (env: PJNIEnv; this: jobject; rotate : TScreen_Rotate) : TScreen_Rotate; cdecl;
Procedure Java_Event_pOnAppConfigurationChanged(env: PJNIEnv; this: jobject); cdecl;
Procedure Java_Event_pOnAppActivityResult      (env: PJNIEnv; this: jobject; requestCode : Integer; resultCode : TActivity_Result; jData : jObject); cdecl;

// Control Event
Procedure Java_Event_pOnDraw                   (env: PJNIEnv; this: jobject; Obj: TObject; jCanvas: jObject); cdecl;
Procedure Java_Event_pOnClick                  (env: PJNIEnv; this: jobject; Obj: TObject; Value: integer); cdecl;
Procedure Java_Event_pOnChange                 (env: PJNIEnv; this: jobject; Obj: TObject; EventType : integer); cdecl;
Procedure Java_Event_pOnEnter                  (env: PJNIEnv; this: jobject; Obj: TObject); cdecl;
Procedure Java_Event_pOnTimer                  (env: PJNIEnv; this: jobject; Obj: TObject); cdecl;
Procedure Java_Event_pOnTouch                  (env: PJNIEnv; this: jobject; Obj: TObject;act,cnt: integer; x1,y1,x2,y2: single); cdecl;

// Form Event
Procedure Java_Event_pOnClose                  (env: PJNIEnv; this: jobject); cdecl;

//
Procedure Java_Event_pOnGLViewState            (env: PJNIEnv; this: jobject; Obj : TObject; state : TGLView_State   ; w, h: integer); cdecl;
Function  Java_Event_pOnWebViewState           (env: PJNIEnv; this: jobject; Obj : TObject; state : TWebView_State  ; URL : jString): TWebView_Act; cdecl;
Procedure Java_Event_pOnAsyncTaskState         (env: PJNIEnv; this: jobject; Obj : TObject; state : TAsyncTask_State; Progress : integer); cdecl;
Procedure Java_Event_pOnHttpState              (env: PJNIEnv; this: jobject; Obj : TObject; act   : THttp_Act; state : THttp_State; Progress : Integer; msg : jString); cdecl;

// Other Event
Procedure Java_Event_pOnCameraFrame            (env: PJNIEnv; this: jobject; Obj : TObject; w,h,fmt : Integer; data : jByteArray); cdecl;

Type
 //
 jForm       = Class;          // Forward Declaration
 jCanvas     = Class;
 TOnDraw     = Procedure(Sender: TObject; Canvas: jCanvas) of object;

// App ----------------------------------------------------------------------
 jApp         = class
  private
   FOnAppCreate : TOnAppCreate;
   //
   FLock        : Boolean;
   FPaths       : TPaths;      // App Paths
  protected
   Function  GetLog : Boolean;
   Procedure SetLog (Value : Boolean);
  public
   Env          : TEnv;        // Java Env.
   Device       : TDevice;     // Device Info
   //
   Form         : jForm;       // Main Form
   Forms        : TForms;      // Form Stack
   //
   Constructor Create; virtual;
   Destructor Destroy; override;
   //
   Procedure Init(env: PJNIEnv; Controls,layout : jObject);
   Procedure Finish;
   //
   Procedure SetScreenStyle      ( ScreenStyle       : TScreen_Style       );
   Procedure SetScreenOrientation( ScreenOrientation : TScreen_Orientation );
   Procedure SetTitleBar( visible:boolean );

   // Property
   property Lock              : Boolean             read FLock        write FLock;
   Property Paths             : TPaths              read FPaths;
   Property Log               : Boolean             read GetLog       write SetLog;
   // Event
   Property OnAppCreate       : TOnAppCreate        read FOnAppCreate write FOnAppCreate;
  end;

 // --------------------------------------------------------------------------
 jForm = class
  private
   // Java
   FjObject       : jObject;      // Java : Java Object
   FjLayout       : jObject;      // Java Relative Layout
   //
   FName          : String;       // Form name
   FScreenWH      : TWH;
   FAnimation     : TAnimation;
   FOwner         : jForm;
   FEnabled       : Boolean;
   FVisible       : Boolean;
   FTitle         : Boolean;
   FColor         : TColor;       // Background Color

   FFormState     : TFormState;
   FCloseCallback : TCallBack;    // Close Call Back Event
   FBackEnabled   : Boolean;      // Android Back Button Enabled

   FOnActive      : TOnNotify;
   FOnClose       : TOnNotify;
   FOnCloseQuery  : TOnCloseQuery;
   FOnRotate      : TOnRotate;
   FOnClick       : TOnNotify;

   FOnPause       : TOnNotify;
   FOnRestart     : TOnNotify;
   FOnResume      : TOnNotify;
   FOnStop        : TOnNotify;

   FOnActivityRst : TOnActivityRst;

   Procedure setEnabled (Value : Boolean);
   Procedure setVisible (Value : Boolean);
   Procedure setTitle   (Value : Boolean);
   Procedure setColor   (Value : TColor );
 protected
   Procedure GenEvent_OnClick(Obj: TObject);
 public

   Constructor Create(Owner: jForm); virtual;
   Destructor Destroy; override;
   Procedure Free;

   Procedure Show;
   Procedure Close;
   Procedure Refresh;

   Procedure SetCloseCallBack(Func : TOnNotify; Sender : TObject);

   // Utility
   Function pH ( Percent : single ) : Integer;  // Percent 100% = 1:1
   Function pW ( Percent : single ) : Integer;
   Function sH ( Scale   : single ) : Integer;  // Scale   1 = 100%
   Function sW ( Scale   : single ) : Integer;

   // Property
   property View         : jObject        read FjLayout       write FjLayout;
   property Enabled      : Boolean        read FEnabled       write SetEnabled;
   property Visible      : Boolean        read FVisible       write SetVisible;
   property Title        : Boolean        read FTitle         write SetTitle;
   property Name         : String         read FName          write FName;
   property Color        : TColor         read FColor         write SetColor;
   property Width        : integer        read FScreenWH.Width;
   property Height       : integer        read FScreenWH.Height;
   property Animation    : TAnimation     read FAnimation     write FAnimation;
   property BackEnabled  : Boolean        read FBackEnabled   write FBackEnabled;
   property State        : TFormState     read FFormState     write FFormState;
   // Event
   property OnActive     : TOnNotify      read FOnActive      write FOnActive;
   property OnClose      : TOnNotify      read FOnClose       write FOnClose;
   property OnCloseQuery : TOnCloseQuery  read FOnCloseQuery  write FOnCloseQuery;
   property OnRotate     : TOnRotate      read FOnRotate      write FOnRotate;
   property OnClick      : TOnNotify      read FOnClick       write FOnClick;
   //
   property OnPause      : TOnNotify      read FOnPause       write FOnPause;
   property OnRestart    : TOnNotify      read FOnRestart     write FOnRestart;
   property OnResume     : TOnNotify      read FOnResume      write FOnResume;
   property OnStop       : TOnNotify      read FOnStop        write FOnStop;
   //
   property OnActivityRst: TOnActivityRst read FOnActivityRst write FOnActivityRst;
 end;

 // ------------------------------------------------------------------
 jTextView = class
  private
    FOwner     : jForm;   // Owner
    // Java
    FjObject   : jObject; // Java : TextView
    FjPLayout  : jObject; // Java : Parent Relative Layout
    //
    FVisible   : Boolean;
    FColor     : TColor;
    FFontColor : TColor;
    FFontSize  : DWord;
    FEnabled   : Boolean;

    FxyWH      : TxyWH;

    FOnClick   : TOnNotify;

    Procedure SetParent   (Value : jObject);
    Procedure SetVisible  (Value : Boolean);
    Procedure SetColor    (Value : TColor );
    Procedure SetEnabled  (Value : Boolean);
    Procedure SetXYWH     (Value : TXYWH  );
    Function  GetText            : string;
    Procedure SetText     (Str   : string );
    Procedure SetFontColor(Value : TColor );
    Procedure SetFontSize (Value : DWord  );
    Procedure setTextAlignment(Value: TText_Alignment);
  protected
    Procedure GenEvent_OnClick(Obj: TObject);
  public
    Constructor Create(Owner: jForm); virtual;
    Destructor Destroy; override;
    Procedure Free;
    Procedure Refresh;
    // Property
    property Parent    : jObject read FjPLayout  write SetParent;
    property Visible   : Boolean read FVisible   write SetVisible;
    property Color     : TColor  read FColor     write SetColor;
    property Enabled   : Boolean read FEnabled   write SetEnabled;
    property FontColor : TColor  read FFontColor write SetFontColor;
    property FontSize  : DWord   read FFontSize  write SetFontSize;
    property XYWH      : TXYWH   read FXYWH      write SetXYWH;
    property Text      : string  read GetText    write SetText;
    property Align     : TText_Alignment         write setTextAlignment;
    // Event
    property OnClick : TOnNotify read FOnClick   write FOnClick;
  end;

  // ------------------------------------------------------------------
  jEditText = class
  private
    FOwner    : jForm;   // Owner
    // Java
    FjObject  : jObject; // Java : EditView
    FjPLayout : jObject; // Java : Parent Relative Layout

    FVisible  : Boolean;
    FColor    : TColor;
    FFontColor: TColor;
    FFontSize : DWOrd;
    FEditStyle: TEdit_Style;
    FEditType : TEdit_Type;
    FHint     : String;

    Fxy       : Txyi;      // Edit Cursor X,Y
    FxyWH     : TxyWH;     // Object Left,Top, Width, Height

    FOnEnter  : TOnNotify;
    FOnChange : TOnChange;

    Procedure SetParent   (Value : jObject);
    Procedure SetVisible  (Value : Boolean);
    Procedure SetColor    (Value : TColor );
    Procedure SetXYWH     (Value : TXYWH  );
    Function  GetText            : string;
    Procedure SetText     (Str   : string );
    Procedure SetFontColor(Value : TColor );
    Procedure SetFontSize (Value : DWord  );
    Procedure SetHint     (Value : String );
    //
    Procedure SetEditStyle(Value : TEdit_Style);
    Procedure SetEditType (Value : TEdit_Type);
    Procedure SetMaxLength(Value     : DWord     );
    Function  GetCursorPos           : TXYi;
    Procedure SetCursorPos(Value     : TXYi      );
    Procedure setTextAlignment (Value : DWord  );

  protected
    Procedure GenEvent_OnEnter (Obj: TObject);
    Procedure GenEvent_OnChange(Obj: TObject; EventType : Integer);
  public
    Constructor Create(Owner: jForm); virtual;
    Destructor  Destroy; override;
    Procedure Free;
    Procedure Refresh;
    //
    Procedure SetFocus;
    Procedure immShow;
    Procedure immHide;
    // Property
    property Parent    : jObject     read FjPLayout     write SetParent;
    property Visible   : Boolean     read FVisible      write SetVisible;
    property Color     : TColor      read FColor        write SetColor;
    property FontColor : TColor      read FFontColor    write SetFontColor;
    property FontSize  : DWord       read FFontSize     write SetFontSize;
    property XYWH      : TXYWH       read FXYWH         write SetXYWH;
    property Text      : string      read GetText       write SetText;
    property Hint      : string      read FHint         write SetHint;
    property EditStyle : TEdit_Style read FEditStyle    write SetEditStyle;
    property EditType  : TEdit_Type  read FEditType     write SetEditType;
    property MaxLength : DWord                          write SetMaxLength;
    property CursorPos : TXYi        read GetCursorPos  write SetCursorPos;
    property Alignment : DWord                          write SetTextAlignment;
    // Event 
    property OnEnter   : TOnNotify   read FOnEnter      write FOnEnter;
    property OnChange  : TOnChange   read FOnChange     write FOnChange;
  end;

  // ------------------------------------------------------------------
  jButton = class
  private
    FOwner    : jForm;   // Owner
    // Java
    FjObject  : jObject; // Java : Button
    FjPLayout : jObject; // Java : Parent Relative Layout

    FVisible  : Boolean;
    FColor    : TColor;
    FFontColor: TColor;
    FFontSize : DWOrd;
    FxyWH     : TxyWH;

    FOnClick  : TOnNotify;

    Procedure SetParent   (Value : jObject);
    Procedure SetVisible  (Value : Boolean);
    Procedure SetColor    (Value : TColor );
    Procedure SetXYWH     (Value : TXYWH  );
    Function  GetText            : string;
    Procedure SetText     (Str   : string );
    Procedure SetFontColor(Value : TColor );
    Procedure SetFontSize (Value : DWord  );
  protected
    Procedure GenEvent_OnClick(Obj: TObject);
  public
    Constructor Create(Owner: jForm); virtual;
    Destructor  Destroy; override;
    Procedure Free;
    Procedure Refresh;
    // Property
    property Parent    : jObject   read FjPLayout  write SetParent;
    property Visible   : Boolean   read FVisible   write SetVisible;
    property Color     : TColor    read FColor     write SetColor;
    property XYWH      : TXYWH     read FXYWH      write SetXYWH;
    property Text      : string    read GetText    write SetText;
    property FontColor : TColor    read FFontColor write SetFontColor;
    property FontSize  : DWord     read FFontSize  write SetFontSize;
    // Event
    property OnClick   : TOnNotify read FOnClick   write FOnClick;
  end;

  // ------------------------------------------------------------------
  jCheckBox = class
  private
    FOwner     : jForm;   // Owner
    // Java
    FjObject   : jObject; // Java : CheckBox
    FjPLayout  : jObject; // Java : Parent Relative Layout

    FVisible   : Boolean;
    FColor     : TColor;
    FFontColor : TColor;
    FFontSize  : DWOrd;
    FxyWH      : TxyWH;

    FOnClick   : TOnNotify;

    Procedure SetParent   (Value : jObject);
    Procedure SetVisible  (Value : Boolean);
    Procedure SetColor    (Value : TColor );
    Procedure SetXYWH     (Value : TXYWH  );
    Function  GetText            : string;
    Procedure SetText     (Str   : string );
    Procedure SetFontColor(Value : TColor );
    Procedure SetFontSize (Value : DWord  );
    Function  GetChecked         : boolean;
    Procedure SetChecked  (Value : boolean);
  protected
    Procedure GenEvent_OnClick(Obj: TObject);
  public
    Constructor Create (Owner: jForm); virtual;
    Destructor Destroy; override;
    Procedure Free;
    Procedure Refresh;
    // Property
    property Parent    : jObject   read FjPLayout  write SetParent;
    property Visible   : Boolean   read FVisible   write SetVisible;
    property Color     : TColor    read FColor     write SetColor;
    property FontColor : TColor    read FFontColor write SetFontColor;
    property FontSize  : DWord     read FFontSize  write SetFontSize;
    property XYWH      : TXYWH     read FXYWH      write SetXYWH;
    property Text      : string    read GetText    write SetText;
    property Checked   : boolean   read GetChecked write SetChecked;
    // Event
    property OnClick   : TOnNotify read FOnClick   write FOnClick;
  end;

  // ------------------------------------------------------------------
  jRadioButton = class
  private
    FOwner     : jForm;   // Owner
    // Java
    FjObject   : jObject; // Java : RadioButton
    FjPLayout  : jObject; // Java : Parent Relative Layout

    FVisible   : Boolean;
    FColor     : TColor;
    FFontColor : TColor;
    FFontSize  : DWOrd;
    FxyWH      : TxyWH;

    FOnClick   : TOnNotify;

    Procedure SetParent   (Value : jObject);
    Procedure SetVisible  (Value : Boolean);
    Procedure SetColor    (Value : TColor );
    Procedure SetXYWH     (Value : TXYWH  );
    Function  GetText            : string;
    Procedure SetText     (Str   : string );
    Procedure SetFontColor(Value : TColor );
    Procedure SetFontSize (Value : DWord  );
    Function  GetChecked         : boolean;
    Procedure SetChecked  (Value : boolean);
  protected
    Procedure GenEvent_OnClick(Obj: TObject);
  public
    Constructor Create(Owner: jForm); virtual;
    Destructor Destroy; override;
    Procedure Free;
    Procedure Refresh;
    // Property
    property Parent    : jObject   read FjPLayout  write SetParent;
    property Visible   : Boolean   read FVisible   write SetVisible;
    property Color     : TColor    read FColor     write SetColor;
    property FontColor : TColor    read FFontColor write SetFontColor;
    property FontSize  : DWord     read FFontSize  write SetFontSize;
    property XYWH      : TXYWH     read FXYWH      write SetXYWH;
    property Text      : string    read GetText    write SetText;
    property Checked   : boolean   read GetChecked write SetChecked;
    // Event
    property OnClick   : TOnNotify read FOnClick   write FOnClick;
  end;

  // ------------------------------------------------------------------
  jProgressBar = class
  private
    FOwner     : jForm;   // Owner
    // Java
    FjObject   : jObject; // Java : ProgressBar
    FjPLayout  : jObject; // Java : Parent Relative Layout

    FVisible   : Boolean;
    FColor     : TColor;
    FxyWH      : TxyWH;

    Procedure SetParent   (Value : jObject);
    Procedure SetVisible  (Value : Boolean);
    Procedure SetColor    (Value : TColor );
    Procedure SetXYWH     (Value : TXYWH);
    Function  GetProgress: integer;
    Procedure SetProgress (Value : integer);
  protected
  public
    Constructor Create(Owner: jForm; Style: TProgressBarStyle = ProgressBarStyle_Horizontal); virtual;
    Destructor Destroy; override;
    Procedure Free;
    Procedure Refresh;
    // Property
    property Parent    : jObject read FjPLayout   write SetParent;
    property Visible   : Boolean read FVisible    write SetVisible;
    property Color     : TColor  read FColor      write SetColor;
    property XYWH      : TXYWH   read FXYWH       write SetXYWH;
    property Progress  : integer read GetProgress write SetProgress;
  end;

  // ------------------------------------------------------------------
  jImageView = class
  private
    FOwner     : jForm;   // Owner
    // Java
    FjObject   : jObject; // Java : EditView
    FjPLayout  : jObject; // Java : Parent Relative Layout

    FVisible   : Boolean;
    FColor     : TColor;
    FxyWH      : TxyWH;
    FImageName : string;

    FOnClick   : TOnNotify;

    Procedure SetParent   (Value : jObject);
    Procedure SetVisible  (Value : Boolean);
    Procedure SetColor    (Value : TColor );
    Procedure SetXYWH     (Value : TXYWH);
    Function  GetImageName       : string;
    Procedure SetImageName(Str   : string);
  protected
    Procedure   GenEvent_OnClick(Obj: TObject);
  public
    Constructor Create(Owner: jForm); virtual;
    Destructor  Destroy; override;
    Procedure Free;
    Procedure Refresh;
    // Property
    property Parent    : jObject   read FjPLayout    write SetParent;
    property Visible   : Boolean   read FVisible     write SetVisible;
    property Color     : TColor    read FColor       write SetColor;
    property XYWH      : TXYWH     read FXYWH        write SetXYWH;
    property ImageName : string    read GetImageName write SetImageName;
    // Event
    property OnClick   : TOnNotify read FOnClick     write FOnClick;
  end;

  // ------------------------------------------------------------------
  jListView = class
  private
    FOwner       : jForm;   // Owner
    // Java
    FjObject     : jObject; // Java : Scroll View
    FjPLayout    : jObject; // Java : Parent Relative Layout
    FjLayout     : jObject; // Java : Self Layout

    FVisible     : Boolean;
    FColor       : TColor;
    FFontColor   : TColor;
    FFontSize    : DWOrd;
    FxyWH        : TxyWH;

    FOnClickItem : TOnClickItem;

    Procedure SetParent       (Value : jObject);
    Procedure SetVisible      (Value : Boolean);
    Procedure SetColor        (Value : TColor );
    Procedure SetXYWH         (Value : TXYWH  );
    Procedure SetFontColor    (Value : TColor );
    Procedure SetFontSize     (Value : DWord  );
    Procedure SetItemPosition (Value : TXYi   );
  protected
    Procedure GenEvent_OnClick(Obj: TObject; Value: integer);
  public
    Constructor Create(Owner: jForm); virtual;
    Destructor  Destroy; override;
    Procedure   Free;
    Procedure Refresh;
    //
    Procedure Item_Add   (item : String);
    Procedure Item_Delete(index: Integer);
    Procedure Item_Clear;
    // Property
    property Parent    : jObject   read FjPLayout  write SetParent;
    property Visible   : Boolean   read FVisible   write SetVisible;
    property Color     : TColor    read FColor     write SetColor;
    property View      : jObject   read FjLayout   write FjLayout ;
    property XYWH      : TXYWH     read FXYWH      write SetXYWH;
    property FontColor : TColor    read FFontColor write SetFontColor;
    property FontSize  : DWord     read FFontSize  write SetFontSize;

    // Event
    property OnClickItem : TOnClickItem read FOnClickItem write FOnClickItem;
    property setItemIndex: TXYi                           write SetItemPosition;
  end;

  // ------------------------------------------------------------------
  jScrollView = class
  private
    FOwner      : jForm;   // Owner
    // Java
    FjObject    : jObject; // Java : Scroll View
    FjPLayout   : jObject; // Java : Parent Relative Layout
    FjLayout    : jObject; // Java : Self Layout

    FVisible    : Boolean;
    FColor      : TColor;
    FxyWH       : TxyWH;
    FScrollSize : integer;

    Procedure SetParent     (Value : jObject);
    Procedure SetVisible    (Value : Boolean);
    Procedure SetColor      (Value : TColor );
    Procedure SetXYWH       (Value : TXYWH  );
    Procedure SetScrollSize (Value : integer);
  protected
  public
    Constructor Create(Owner: jForm); virtual;
    Destructor  Destroy; override;
    Procedure   Free;
    Procedure Refresh;
    // Property
    property Parent    : jObject read FjPLayout   write SetParent;
    property Visible   : Boolean read FVisible    write SetVisible;
    property Color     : TColor  read FColor      write SetColor;
    property View      : jObject read FjLayout    write FjLayout ;
    property XYWH      : TXYWH   read FXYWH       write SetXYWH;
    property ScrollSize: integer read FScrollSize write SetScrollSize;
  end;

  jHorizontalScrollView = class
  private
    FOwner      : jForm;   // Owner
    // Java
    FjObject    : jObject; // Java : Scroll View
    FjPLayout   : jObject; // Java : Parent Relative Layout
    FjLayout    : jObject; // Java : Self Layout

    FVisible    : Boolean;
    FColor      : TColor;
    FxyWH       : TxyWH;
    FScrollSize : integer;

    Procedure SetParent     (Value : jObject);
    Procedure SetVisible    (Value : Boolean);
    Procedure SetColor      (Value : TColor );
    Procedure SetXYWH       (Value : TXYWH  );
    Procedure SetScrollSize (Value : integer);
  protected
  public
    Constructor Create(Owner: jForm); virtual;
    Destructor  Destroy; override;
    Procedure   Free;
    Procedure Refresh;
    // Property
    property Parent    : jObject read FjPLayout   write SetParent;
    property Visible   : Boolean read FVisible    write SetVisible;
    property Color     : TColor  read FColor      write SetColor;
    property View      : jObject read FjLayout    write FjLayout ;
    property XYWH      : TXYWH   read FXYWH       write SetXYWH;
    property ScrollSize: integer read FScrollSize write SetScrollSize;
  end;

  // ------------------------------------------------------------------
  jViewFlipper = class
  private
    FOwner    : jForm;   // Owner
    // Java
    FjObject  : jObject; // Java : EditView
    FjPLayout : jObject; // Java : Parent Relative Layout

    FVisible  : Boolean;
    FColor    : TColor;
    FxyWH     : TxyWH;

    FOnClick  : TOnNotify;

    Procedure SetParent   (Value : jObject);
    Procedure SetVisible  (Value : Boolean);
    Procedure SetColor    (Value : TColor );
    Procedure SetXYWH     (Value : TXYWH  );
  protected
  public
    Constructor Create(Owner: jForm); virtual;
    Destructor  Destroy; override;
    Procedure Free;
    Procedure Refresh;
    // Property
    property Parent    : jObject read FjPLayout  write SetParent;
    property Visible   : Boolean read FVisible   write SetVisible;
    property Color     : TColor  read FColor     write SetColor;
    property XYWH      : TXYWH   read FXYWH      write SetXYWH;
  end;

  // ------------------------------------------------------------------
  jWebView = class
  private
    FOwner      : jForm;   // Owner
    // Java
    FjObject    : jObject; // Java : EditView
    FjPLayout   : jObject; // Java : Parent Relative Layout
    //
    FVisible    : Boolean;
    FColor      : TColor;
    FxyWH       : TxyWH;
    FJavaScript : Boolean;
    //
    FOnState    : TOnWebViewState;

    Procedure SetParent    (Value : jObject);
    Procedure SetVisible   (Value : Boolean);
    Procedure SetColor     (Value : TColor );
    Procedure SetXYWH      (Value : TXYWH  );
    Procedure SetJavaScript(Value : Boolean);
  protected
  public
    Constructor Create(Owner: jForm); virtual;
    Destructor  Destroy; override;
    Procedure Free;
    Procedure Refresh;

    Procedure Navigate(url: string);
    // Property
    property Parent    : jObject          read FjPLayout   write SetParent;
    property Visible   : Boolean          read FVisible    write SetVisible;
    property Color     : TColor           read FColor      write SetColor;
    property XYWH      : TXYWH            read FXYWH       write SetXYWH;
    property JavaScript: Boolean          read FJavaScript write SetJavaScript;
    // Event
    property OnState   : TOnWebViewState  read FOnState    write FOnState;
  end;

  // ------------------------------------------------------------------
  jBitmap = class
  private
    // Java
    FjObject     : jObject; // Java : View
    //
    FWidth       : Integer; // Bitmap Width
    FHeight      : Integer; // Bitmap Height;
  protected
  public
    Constructor Create; virtual;
    Destructor  Destroy; override;
    Procedure Free;
    Procedure LoadFromFile(const FileName : String);
    Procedure CreateBitmap(w,h : Integer);
    Function  GetJavaBitmap : jObject;
    //Function  GetBitmap : Pointer;
    //Procedure SetBitmap ( data : pointer );
    // Property
    property  JavaObj : jObject           read FjObject;
    property  Width   : Integer           read FWidth      write FWidth;
    property  Height  : Integer           read FHeight     write FHeight;
  end;

  // ------------------------------------------------------------------
  jCanvas = class
  private
    // Java
    FjObject     : jObject; // Java : View
  protected
  public
    Constructor Create; virtual;
    Destructor  Destroy; override;
    Procedure Free;
    //
    Procedure setStrokeWidth       (width : single );
    Procedure setStyle             (style : TPaint_Style);
    Procedure setColor             (color : TColor );
    Procedure setTextSize          (textsize : single );
    //
    Procedure drawLine             (x1,y1,x2,y2 : single);
    // LORDMAN 2013-08-13
    Procedure drawPoint            (x1,y1 : single);

    Procedure drawText             (Text : String; x,y : single);

    Procedure drawBitmap           (bmp : jBitmap; b,l,r,t : integer);
    // Property
    property  JavaObj : jObject           read FjObject;
  end;

  // ------------------------------------------------------------------
  jView = class
  private
    FOwner       : jForm;   // Owner
    // Java
    FjObject     : jObject; // Java : View
    FjPLayout    : jObject; // Java : Parent Relative Layout
    //
    FjCanvas     : jCanvas; // Java : jCanvas

    FVisible     : Boolean;
    FColor       : TColor;
    FxyWH        : TxyWH;
    FMouches     : TMouches;
    //
    FOnDraw      : TOnDraw;

    FOnTouchDown : TOnTouchEvent;
    FOnTouchMove : TOnTouchEvent;
    FOnTouchUp   : TOnTouchEvent;

    Procedure SetParent   (Value : jObject);
    Procedure SetVisible  (Value : Boolean);
    Procedure SetColor    (Value : TColor );
    Procedure SetXYWH     (Value : TXYWH  );
  protected
    Procedure GenEvent_OnTouch(Obj: TObject; Act,Cnt: integer; X1,Y1,X2,Y2: single);
    Procedure GenEvent_OnDraw (Obj: TObject; jCanvas: jObject);
  public
    Constructor Create(Owner: jForm); virtual;
    Destructor  Destroy; override;
    Procedure Free;
    Procedure Refresh;
    Procedure SaveToFile(sFileName:String);

    // Property
    property Parent      : jObject       read FjPLayout    write SetParent;
    property Visible     : Boolean       read FVisible     write SetVisible;
    property Color       : TColor        read FColor       write SetColor;
    property XYWH        : TXYWH         read FXYWH        write SetXYWH;
    property Canvas      : jCanvas       read FjCanvas     write FjCanvas;
    // Event - Drawing
    property OnDraw      : TOnDraw       read FOnDraw      write FOnDraw;
    // Event - Touch
    property OnTouchDown : TOnTouchEvent read FOnTouchDown write FOnTouchDown;
    property OnTouchMove : TOnTouchEvent read FOnTouchMove write FOnTouchMove;
    property OnTouchUp   : TOnTouchEvent read FOnTouchUp   write FOnTouchUp;
  end;

  // ------------------------------------------------------------------
  jGLViewEvent = class
  private
    //
    FOnGLCreate  : TOnNotify;
    FOnGLSize    : TOnGLViewSize;
    FOnGLDraw    : TOnNotify;
    FOnGLDestroy : TOnNotify;
    FOnGLThread  : TOnNotify;
    //
    FMouches     : TMouches;
    //
    FOnTouchDown : TOnTouchEvent;
    FOnTouchMove : TOnTouchEvent;
    FOnTouchUp   : TOnTouchEvent;
  public
    Constructor Create; virtual;
    Destructor  Destroy; override;
    Procedure   Free;
    //
    Procedure GenEvent_OnTouch       (Obj: TObject; Act,Cnt: integer; X1,Y1,X2,Y2: single);
    Procedure GenEvent_OnGLViewState (Obj: TObject; State : TGLView_State; w, h: integer);
    // Event - Drawing
    property OnGLCreate  : TOnNotify      read FOnGLCreate  write FOnGLCreate;
    property OnGLSize    : TOnGLViewSize  read FOnGLSize    write FOnGLSize;
    property OnGLDraw    : TOnNotify      read FOnGLDraw    write FOnGLDraw;
    property OnGLDestroy : TOnNotify      read FOnGLDestroy write FOnGLDestroy;
    property OnGLThread  : TOnNotify      read FOnGLThread  write FOnGLThread;
    // Event - Touch
    property OnTouchDown : TOnTouchEvent  read FOnTouchDown write FOnTouchDown;
    property OnTouchMove : TOnTouchEvent  read FOnTouchMove write FOnTouchMove;
    property OnTouchUp   : TOnTouchEvent  read FOnTouchUp   write FOnTouchUp;
  end;

  // ------------------------------------------------------------------
  jCameraView = class
  private
    FOwner       : jForm;   // Owner
    // Java
    FjObject     : jObject; // Java : View
    FjPLayout    : jObject; // Java : Parent Relative Layout
    //
    FOnFrame     : TOnCameraFrame;
    //
    FVisible     : Boolean;
    FColor       : TColor;
    FxyWH        : TxyWH;
    //
    Procedure SetParent   (Value : jObject);
    Procedure SetVisible  (Value : Boolean);
    Procedure SetXYWH     (Value : TXYWH  );
  protected
  public
    Constructor Create(Owner: jForm); virtual;
    Destructor  Destroy; override;
    Procedure   Free;
    //
    Procedure   GenEvent_OnCameraFrame(Obj: TObject; w, h, fmt : integer; data : pointer);
    //
    Procedure   SaveToFile( fileName : String);
    // Property
    property Parent      : jObject        read FjPLayout    write SetParent;
    property Visible     : Boolean        read FVisible     write SetVisible;
    property XYWH        : TXYWH          read FXYWH        write SetXYWH;
    // Event - Drawing
    property OnFrame     : TOnCameraFrame read FOnFrame     write FOnFrame;
  end;

  // ------------------------------------------------------------------
  jTimer = class
   private
     // Java
     FjObject  : jObject; //Self
     FParent   : jForm;

     FEnabled  : Boolean;
     FInterval : Integer;
     FOnNotify : TOnNotify;

     Procedure SetEnabled (Value: boolean  );
     Procedure SetInterval(Value: integer  );
     Procedure SetOnTimer (Value: TOnNotify);
   protected
   public
     Constructor Create(Form: jForm); virtual;
     Destructor Destroy; override;
     Procedure Free;
     // Property
     property Parent   : jForm     read FParent   write FParent;
     property Enabled  : boolean   read FEnabled  write SetEnabled;
     property Interval : integer   read FInterval write SetInterval;
     // Event
     property OnTimer  : TOnNotify read FOnNotify write SetOnTimer;
   end;

  // ------------------------------------------------------------------
   jDialogYN = class
   private
     // Java
     FjObject    : jObject; //Self
     FParent     : jForm;

     FEnabled    : boolean;
     FOnDialogYN : TOnClickYN;

   protected
     Procedure GenEvent_OnClick(Obj: TObject; Value: integer);
   public
     Constructor Create(Form: jForm; Title, Msg, Y, N: string); virtual;
     Destructor Destroy; override;

     Procedure Show;
     Procedure Free;

     property OnClickYN: TOnClickYN read FOnDialogYN write FOnDialogYN;
   end;

   // ------------------------------------------------------------------
   jDialogProgress = class
   private
     // Java
     FjObject    : jObject; //Self
     FParent     : jForm;
   protected
   public
     Constructor Create(Form: jForm; Title, Msg : string); virtual;
     Destructor Destroy; override;
     Procedure Free;
   end;

   // ------------------------------------------------------------------
   jImageBtn = class
   private
     FOwner    : jForm;   // Owner
     // Java
     FjObject  : jObject; // Java : ImageBtn
     FjPLayout : jObject; // Java : Parent Relative Layout
     //
     FVisible  : Boolean;
     FColor    : TColor;
     FxyWH     : TxyWH;
     FEnabled  : Boolean;

     FOnClick  : TOnNotify;
     Procedure SetParent   (Value : jObject);
     Procedure SetVisible  (Value : Boolean);
     Procedure SetColor    (Value : TColor );
     Procedure SetXYWH     (Value : TXYWH  );
     Procedure SetEnabled  (Value : Boolean);
   protected
     Procedure GenEvent_OnClick(Obj: TObject);
   public
     Constructor Create(Owner: jForm); virtual;
     Destructor  Destroy; override;
     Procedure Free;
     Procedure Refresh;

     Procedure SetButton(up, dn: string);
     // Property
     property Parent  : jObject   read FjPLayout  write SetParent;
     property Visible : Boolean   read FVisible   write SetVisible;
     property Color   : TColor    read FColor     write SetColor;
     property XYWH    : TXYWH     read FXYWH      write SetXYWH;
     property Enabled : Boolean   read FEnabled   write SetEnabled;

     // Event
     property OnClick : TOnNotify read FOnClick   write FOnClick;
   end;

   // ------------------------------------------------------------------
   jAsyncTask = class
   private
     FOwner        : jForm;         // Owner
     // Java
     FEnv          : TEnv;          // Java : Thread -> Self FEnv
     FjObject      : jObject;       // Java : AsyncTask
     FBusy         : Boolean;
     //
     FOnState      : TOnAsyncTaskState;
   protected
     Procedure GenEvent_OnAsyncTaskState(Obj: TObject; State : TAsyncTask_State; Progress : Integer);
   public
     //
     Constructor Create(Owner: jForm); virtual;
     Destructor  Destroy; override;
     Procedure Free;

     Procedure Execute;
     Procedure UpdateUI (Progress : Integer);
     Procedure UpdateEnv(Env : PJniEnv);
     // Proeprty
     property  Busy    : Boolean           read FBusy    write FBusy;
     // Event
     property  OnState : TOnAsyncTaskState read FOnState write FOnState;
   end;

   // ------------------------------------------------------------------
   jHttp = class
   private
     FOwner        : jForm;   // Owner
     // Java
     FjObject      : jObject; // Java : AsyncTask
     FBusy         : Boolean;
     //
     FOnState      : TOnHttpState;
   protected
     procedure GenEvent_OnHttpEvent(Obj: TObject; Act : THttp_Act; State : THttp_State; Progress : Integer; Msg : String);
   public
     Env           : TEnv;    // Java : FpjEnv;
     //
     Constructor Create(Owner: jForm); virtual;
     Destructor  Destroy; override;
     Procedure Free;

     Procedure getText     (url : String);
     Procedure DownloadFile(url : String; localFile : string);
     Procedure UploadFile  (url : String; localFile : string);
     // Event
     property  OnState : TOnHttpState read FOnState write FOnState;
   end;

 // Helper Function
 Procedure jAppInit        ( Var App           : jApp;
                             JVM               : PJavaVm;
                             AppName           : String;
                             ScreenStyle       : TScreen_Style;
                             ScreenOrientation : TScreen_Orientation;
                             Log               : Boolean;
                             OnAppCreate       : TOnAppCreate);
 Function  jObjHex         ( jObj : jObject ) : String;
 Procedure jGetPaths       ( Var Paths  : TPaths  );
 Procedure jGetDevice      ( Var Device : TDevice );
 //
 Function  Asset_SaveToFile (AssetName,FileName :String; OverWrite : Boolean =  False ) : Boolean;
 Function  Memory_SaveToFile(ptr : Pointer; Len : Integer; FileName : String) : Boolean;
 //
 Procedure Dbg          (Const Msg : String; LogType : TAndroid_Log = Android_Log_DEBUG);
 Function  ARGB         (A,R,G,B : Byte) : TColor;
 Function  RGB          (R,G,B   : Byte) : TColor;
 Function  XYi          (x, y: integer) : TXYi;
 Function  XYf          (x, y: Single ) : TXYf;
 Function  xyWH         (x, y, w, h: integer;Scale : Single = 1.0 ): TXYWH;
 Function  MinMax       ( Val : Integer; Min,Max : Integer ) : Integer;
 Function  GetStrLen    (Str: String): Integer;
 Function  GetDateTime : String;
 Function  GetAnimation (AniIn,AniOut : TAnimation_Effect ): TAnimation;
 Procedure ShowMessage  (msg: string);
 // Touch
 Procedure VHandler_touchesDown(Sender        : TObject;
                                TouchCnt      : Integer;
                                Touch1        : TXYf;
                                Touch2        : TXYf;
                                Var TouchDown : TOnTouchEvent;
                                Var Mouches   : TMouches);
 Procedure VHandler_touchesMove(Sender        : TObject;
                                TouchCnt      : Integer;
                                Touch1        : TXYf;
                                Touch2        : TXYf;
                                Var TouchMove : TOnTouchEvent;
                                Var Mouches   : TMouches);
 Procedure VHandler_touchesUp  (Sender        : TObject;
                                TouchCnt      : Integer;
                                Touch1        : TXYf;
                                Touch2        : TXYf;
                                Var TouchUp   : TOnTouchEvent;
                                Var Mouches   : TMouches);
 // Conv
 Procedure  YUV_to_Img  ( Width,Height : Integer;
                                pData  : pBytes;
                               Var Img : TImgRGB;
                                Rotate : Boolean = True );
Var
 App : jApp = nil;

implementation


Procedure jAppInit        ( Var App           : jApp;
                            JVM               : PJavaVM;
                            AppName           : String;
                            ScreenStyle       : TScreen_Style;
                            ScreenOrientation : TScreen_Orientation;
                            Log               : Boolean;
                            OnAppCreate       : TOnAppCreate);
 begin
  App                           := jApp.Create;
  App.Env.jVM                   := jVM;
  App.Env.AppName               := AppName;
  App.Device.ScreenStyle        := ScreenStyle;
  App.Device.ScreenOrientation  := ScreenOrientation;
  App.Log                       := Log;
  //
  App.OnAppCreate               := OnAppCreate;
 end;

Function jObjHex  ( jObj : jObject ) : String;
 begin
  Result := IntToHex( Integer(jObj), 8);
 end;

Procedure jGetPaths     ( Var Paths : TPaths );
 begin
  //
  Paths.ALARMS        := jSystem_GetPath(App.Env,DIRECTORY_ALARMS       );
  Paths.DCIM          := jSystem_GetPath(App.Env,DIRECTORY_DCIM         );
  Paths.DOCUMENTS     := jSystem_GetPath(App.Env,DIRECTORY_DOCUMENTS    );
  Paths.DOWNLOADS     := jSystem_GetPath(App.Env,DIRECTORY_DOWNLOADS    );
  Paths.MOVIES        := jSystem_GetPath(App.Env,DIRECTORY_MOVIES       );
  Paths.MUSIC         := jSystem_GetPath(App.Env,DIRECTORY_MUSIC        );
  Paths.NOTIFICATIONS := jSystem_GetPath(App.Env,DIRECTORY_NOTIFICATIONS);
  Paths.PICTURES      := jSystem_GetPath(App.Env,DIRECTORY_PICTURES     );
  Paths.PODCASTS      := jSystem_GetPath(App.Env,DIRECTORY_PODCASTS     );
  Paths.RINGTONES     := jSystem_GetPath(App.Env,DIRECTORY_RINGTONES    );
  //
  Paths.App           := jSystem_GetPath(App.Env,DIRECTORY_App          );
  Paths.Files         := jSystem_GetPath(App.Env,DIRECTORY_Files        );
  Paths.SDCard        := jSystem_GetPath(App.Env,DIRECTORY_SDCard       );
  Paths.DataBase      := jSystem_GetPath(App.Env,DIRECTORY_DataBase     );
  Paths.Shared_Prefs  := jSystem_GetPath(App.Env,DIRECTORY_Shared_Prefs );
 end;

Procedure jGetDevice   ( Var Device : TDevice );
 begin
  Device.ScreenOrientation := jDevice_GetScreenOrientation(App.Env);
  Device.ScreenWH          := jDevice_GetScreenWH         (App.Env);
  Device.Model             := jDevice_GetModel            (App.Env);
  Device.Version           := jDevice_GetVersion          (App.Env);
  Device.PhoneNumber       := jDevice_GetPhoneNumber      (App.Env);
  Device.ID                := jDevice_GetID               (App.Env);
  Device.Network           := jDevice_GetNetwork          (App.Env);
 end;

//
Function  Asset_SaveToFile( AssetName,FileName :String; OverWrite : Boolean =  False ) : Boolean;
 begin
  Exit;
  Result := False;
  jLog('#Before Asset#');
  If Not(Overwrite) and FileExists(FileName) then Exit;
  jLog('#After Asset#');
  Result := jAsset_SaveToFile (App.Env, AssetName, FileName );
 end;

//
Function  Memory_SaveToFile(ptr : Pointer; Len : Integer; FileName : String) : Boolean;
 Var
  FStream : TFileStream;
 begin
  Result := False;
  FStream := TFileStream.Create(FileName,fmCreate);
  FStream.WriteBuffer(ptr^,len);
  FStream.Free;
  Result := FileExists(FileName);
 end;

Procedure Dbg (Const Msg : String; LogType : TAndroid_Log = Android_Log_DEBUG);
 begin
  jLog(Msg,LogType);
 end;

Function  ARGB         (A,R,G,B : Byte) : TColor;
 begin
  Result := (A Shl 24) or
            (R Shl 16) or
            (G Shl  8) or
            B;
 end;

Function  RGB          (R,G,B   : Byte) : TColor;
 begin
  Result := ($FF Shl 24) or
            (R   Shl 16) or
            (G   Shl  8) or
             B;
 end;

Function  XYi(x, y: integer): TXYi;
 begin
  Result.x := x;
  Result.y := y;
 end;

Function  XYf(x, y: single ): TXYf;
 begin
  Result.x := x;
  Result.y := y;
 end;

Function  xyWH (x, y, w, h: integer;Scale : Single = 1.0 ): TXYWH;
 begin
  Result.x := Round(x*Scale);
  Result.y := Round(y*Scale);
  Result.w := Round(w*Scale);
  Result.h := Round(h*Scale);
 end;

Function MinMax      ( Val : Integer; Min,Max : Integer ) : Integer; inline;
 begin
  If Val < min then Val := min;
  If Val > max then Val := max;
  Result := Val;
 end;

 // LORDMAN - 2013-07-28
Function GetStrLen (Str: String): Integer;
 begin
  result := jGetStrLength(App.Env, str);
 end;

// LORDMAN - 2013-07-30
Function GetDateTime(): String;
 begin
  result := jGetStrDateTime(App.Env);
 end;

Function  GetAnimation(AniIn,AniOut : TAnimation_Effect ): TAnimation;
 begin
  Result.AniIn  := AniIn;
  Result.AniOut := AniOut;
 end;

Procedure ShowMessage(msg: string);
 begin
  jToast(App.Env, msg);
 end;

//----------------------------------------------------------------------------
// Multi Touch
//----------------------------------------------------------------------------

//
Function  csAvg        (const A,B : TXYf) : TXYf;
 begin
  Result.X := (A.X + B.X) / 2;
  Result.Y := (A.Y + B.Y) / 2;
 end;

//
Function  csLen        (const A,B : TXYf) : Single;
 begin
  Result := Sqrt( (A.X - B.X)*(A.X - B.X) +
                  (A.Y - B.Y)*(A.Y - B.Y) );
 end;


//
Function  csAngle      (const A,B : TXYf) : Single;
 Var
  Pt    : TXYf;
  Len   : Single;
  Angle : Single;
 begin
  // 변수 초기화
  Angle := 0;
  Pt.X  := B.X-A.X;
  Pt.Y  := B.Y-A.Y;
  // Prevent Div Zero
  If (Pt.X = 0) and (Pt.Y = 0) then
   begin
    Pt.X := 1;
    Pt.Y := 1;
   end;
  Len := Sqrt( (Pt.X*Pt.X) + (Pt.Y*Pt.Y) );
  //
  Case (Pt.X > 0) of
   True : Case (Pt.Y > 0) of
           False: Angle :=      ArcSin( Pt.Y *-1/ Len ) * (180/pi);       // 1사분면
           True : Angle := 360- ArcSin( Pt.Y    / Len ) * (180/pi);       // 4사분면
          End;
   False: Case (Pt.Y > 0) of
           False: Angle := (90 -ArcSin( Pt.Y*-1 / Len ) * (180/pi))+90;   // 2사분면
           True : Angle :=      ArcSin( Pt.Y    / Len ) * (180/pi)+180;   // 3사분면
          End;
  End;
  //
  Angle := 360-Angle;
  While Angle >= 360 do Angle := Angle - 360;
  While Angle <    0 do Angle := Angle + 360;

  Result := Angle;
 end;

// Input  Touches
// Output MTouch
Procedure MultiTouch_Calc(Var Mouches : TMouches );
 begin
  //
  If Mouches.Cnt > 1 then
   begin
    If Not(Mouches.Mouch.Active) then
     begin
      Mouches.sPt    := csAvg  ( Mouches.XYs[0], Mouches.XYs[1] );
      Mouches.sLen   := csLen  ( Mouches.XYs[0], Mouches.XYs[1] );
      Mouches.sAngle := csAngle( Mouches.XYs[0], Mouches.XYs[1] );
      Mouches.Mouch.Active := True;
     end;
    Inc(Mouches.sCount);
    Mouches.Mouch.Start  := (Mouches.sCount = 1);
    //If Touches.MTouch.Start then dbg('Start###################################');
    Mouches.Mouch.Pt    :=  csAvg  ( Mouches.XYs[0],Mouches.XYs[1] );
    Mouches.Mouch.Zoom  :=  csLen  ( Mouches.XYs[0],Mouches.XYs[1] ) / Mouches.sLen;
    Mouches.Mouch.Angle :=  csAngle( Mouches.XYs[0],Mouches.XYs[1] ) - Mouches.sAngle;
   end
  else
   begin
    Mouches.Mouch.Pt    :=  Mouches.XYs[0];
   end;
 end;

Procedure MultiTouch_End(Var Mouches : TMouches);
 begin
  //If Touches.Cnt = 2 then
  Mouches.Mouch.Active := False;
  Mouches.sCount       := 0;
 end;

//------------------------------------------------------------------------------
// Touch Event
//------------------------------------------------------------------------------

Procedure VHandler_touchesDown(Sender        : TObject;
                               TouchCnt      : Integer;
                               Touch1        : TXYf;
                               Touch2        : TXYf;
                               Var TouchDown : TOnTouchEvent;
                               Var Mouches   : TMouches);

 begin
  //
  If Not(Assigned(TouchDown)) then Exit;
  //
  Mouches.Cnt    := Min(TouchCnt,cMaxTouch);
  Mouches.XYs[0] := Touch1;
  Mouches.XYs[1] := Touch2;

  MultiTouch_Calc(Mouches);
  TouchDown(Sender,Mouches.Mouch);
 end;

//
Procedure VHandler_touchesMove(Sender        : TObject;
                               TouchCnt      : Integer;
                               Touch1        : TXYf;
                               Touch2        : TXYf;
                               Var TouchMove : TOnTouchEvent;
                               Var Mouches   : TMouches);
 begin
  //
  If Not(Assigned(TouchMove)) then Exit;
  //
  Mouches.Cnt    := Min(TouchCnt,cMaxTouch);
  Mouches.XYs[0] := Touch1;
  Mouches.XYs[1] := Touch2;
  MultiTouch_Calc(Mouches);
  TouchMove(Sender,Mouches.Mouch);
 end;

//
Procedure VHandler_touchesUp (Sender         : TObject;
                              TouchCnt       : Integer;
                              Touch1         : TXYf;
                              Touch2         : TXYf;
                              Var TouchUp    : TOnTouchEvent;
                              Var Mouches    : TMouches);
 begin
  //
  If Not(Assigned(TouchUp)) then Exit;
  //
  Mouches.Cnt    := Min(TouchCnt,cMaxTouch);
  Mouches.XYs[0] := Touch1;
  Mouches.XYs[1] := Touch2;

  MultiTouch_End(Mouches);
  TouchUp  (Sender,Mouches.Mouch);
 end;

// Step #1. YUV -> RGB
// Step #2. RGB -> Transpose 90 CW
//
// Ref. http://www.41post.com/3470/programming/android-retrieving-the-camera-preview-as-a-pixel-array
//
Procedure  YUV_to_Img  ( Width,Height : Integer;
                               pData  : pBytes;
                              Var Img : TImgRGB;
                               Rotate : Boolean = True );
 Var
  FrameSize : Integer;
  j,i,k     : Integer;
  yp        : Integer;
  uvp       : Integer;
  y,u,v     : Integer;
  y1192     : Integer;
  r,g,b     : Integer;
 begin
  // Initial
  Case Rotate of
   True  : begin
            Img.Width  := Height;
            Img.Height := Width;
           end;
   False : begin
            Img.Width  := Width;
            Img.Height := Height;
           end;
  End;
  //
  GetMem(Img.pRGBs_,Width*Height*3);
  //
  FrameSize := Width*Height;
  //
  yp := 0;
  k  := 0;
  for j := 0 to Height-1 do
   begin
    //
    uvp := frameSize + ( j shr 1) * Width;
    u   := 0;
    v   := 0;
    //
    For i := 0 to Width-1 do
     begin
      inc(yp);
      //
      y := $FF and pData^[yp] - 16;
      If y < 0 then y := 0;
      // bit 연산  20150206
      // 예. (0 and 1) = 0
      //     (1 and 1) = 1
      // Ycc에서 cb,cr은 2픽셀당 한개가 있기 때문에, 짝수,홀수 계산용
      if (i and 1) = 0 then
       begin
        v := $FF and pData^[uvp] - 128; inc(uvp);
        u := $FF and pData^[uvp] - 128; inc(uvp);
       end;
      //
      y1192 := 1192* y;
      //
      r := (y1192 + 1634*v);
      g := (y1192 -  833*v - 400 * u );
      b := (y1192 + 2066*u);
      //
      r := MinMax(r,0,262143);
      g := MinMax(g,0,262143);
      b := MinMax(b,0,262143);

      Case Rotate of
       True  : K := i*Height + (Height-1-j);  // 90 CW
       False : K := j*Width+i;                // Normal
      End;
      //
      Img.pRGBs_[K].R := ( r shr 10) and $FF;
      Img.pRGBs_[K].G := ( g shr 10) and $FF;
      Img.pRGBs_[K].B := ( b shr 10) and $FF;
     end;
   end;
 end;

//------------------------------------------------------------------------------
//  App Event
//------------------------------------------------------------------------------

//
Procedure Java_Event_pOnAppCreate(env:PJNIEnv; this:jObject;
                                  layout : jObject);cdecl;
begin
 App.Init(env, this, layout);
 //
 If Assigned(App.OnAppCreate) then
  App.OnAppCreate;
end;

Function Java_Event_pOnAppScreenStyle(env: PJNIEnv; this: jobject): TScreen_Style;
 begin
  Result := App.Device.ScreenStyle;
 end;

Function Java_Event_pOnAppScreenOrientation(env: PJNIEnv; this: jobject): TScreen_Orientation;
 begin
  Result := App.Device.ScreenOrientation;
 end;

Procedure Java_Event_pOnAppNewIntent(env: PJNIEnv; this: jObject);
 begin
  jLog('pAppOnNewIntent');
 end;

Procedure Java_Event_pOnAppDestroy(env: PJNIEnv; this: jobject);
 begin
  jLog('pAppOnDestroy');
 end;

Procedure Java_Event_pOnAppPause(env: PJNIEnv; this: jobject);
 begin
  jLog('pAppOnPause');
 end;

Procedure Java_Event_pOnAppRestart(env: PJNIEnv; this: jobject);
 begin
  jLog('pAppOnRestart');
 end;

Procedure Java_Event_pOnAppResume(env: PJNIEnv; this: jobject);
 Var
   Form : jForm;
 begin
  jLog('pAppOnResume');
  //
  Form := App.Forms.Stack[App.Forms.Index-1].jForm;
  if Not( Assigned(Form) )         then Exit;
  if Not( Assigned(Form.onResume)) then Exit;
  Form.onResume(Form);
 end;

Procedure Java_Event_pOnAppActive(env: PJNIEnv; this: jObject);
 begin
  jLog('pAppOnActive');
 end;

Procedure Java_Event_pOnAppStop(env: PJNIEnv; this: jobject);
 begin
  jLog('pAppOnStop');
 end;

// Event : OnBackPressed -> Form OnClose
Procedure Java_Event_pOnAppBackPressed(env: PJNIEnv; this: jobject);
 Var
  Form : jForm;
 begin
  //
  If App.Forms.Index = 0 then Exit;
  //
  Form := App.Forms.Stack[App.Forms.Index-1].jForm;
  if Not(Assigned(Form))          then Exit;
  if Not(Form.FBackEnabled)       then Exit;
  if Form.State <> fsFormWork then Exit;
  //
  jLog('AppOnBackPress : Form Name :' + Form.name );
  Form.Close;
 end;

// Event : OnRotate -> Form OnRotate
Function Java_Event_pOnAppRotate(env: PJNIEnv; this: jobject;
                                 rotate : TScreen_Rotate) : TScreen_Rotate;
 Var
  Form      : jForm;
  rstRotate : TScreen_Rotate;
 begin
  //
  jLog('pAppOnRotate');
  Result := rotate;
  Form := App.Forms.Stack[App.Forms.Index-1].jForm;
  if Not( Assigned(Form         )) then Exit;
  if Not( Assigned(Form.OnRotate)) then Exit;
  Form.OnRotate(Form,rotate,rstRotate);
  Result := rstRotate;
 end;

Procedure Java_Event_pOnAppConfigurationChanged(env: PJNIEnv; this: jobject);
 begin
  jLog('pAppOnConfigurationChanged');
 end;

Procedure Java_Event_pOnAppActivityResult      (env: PJNIEnv; this: jobject;
                                                requestCode : Integer;
                                                resultCode  : TActivity_Result;
                                                jData : jObject);
 Var
  Form      : jForm;
 begin
  Form := App.Forms.Stack[App.Forms.Index-1].jForm;
  if Not( Assigned(Form              )) then Exit;
  if Not( Assigned(Form.OnActivityRst)) then Exit;
  Form.OnActivityRst(Form,requestCode,resultCode,jData);
 end;

//------------------------------------------------------------------------------
//  Control Event
//------------------------------------------------------------------------------

Procedure Java_Event_pOnDraw(env: PJNIEnv; this: jobject;
                             Obj: TObject; jCanvas: jObject);
 begin
  if not (Assigned(Obj)) then Exit;
  //
  if Obj is jView        then begin jView(Obj).GenEvent_OnDraw(Obj, jCanvas);            end;
end;

Procedure Java_Event_pOnClick(env: PJNIEnv; this: jobject;
                              Obj: TObject; Value: integer);
 begin
  //
  if not (Assigned(Obj)) then Exit;
  //App.
  if Obj is jForm        then begin jForm       (Obj).GenEvent_OnClick(Obj);       exit; end;
  if Obj is jTextView    then begin jTextView   (Obj).GenEvent_OnClick(Obj);       exit; end;
  if Obj is jButton      then begin jButton     (Obj).GenEvent_OnClick(Obj);       exit; end;
  if Obj is jCheckBox    then begin jCheckBox   (Obj).GenEvent_OnClick(Obj);       exit; end;
  if Obj is jRadioButton then begin jRadioButton(Obj).GenEvent_OnClick(Obj);       exit; end;
  if Obj is jDialogYN    then begin jDialogYN   (Obj).GenEvent_OnClick(Obj,Value); exit; end;
  if Obj is jImageBtn    then begin jImageBtn   (Obj).GenEvent_OnClick(Obj);       exit; end;
  if Obj is jListView    then begin jListVIew   (Obj).GenEvent_OnClick(Obj,Value); exit; end;
  if Obj is jImageView   then begin jImageView  (Obj).GenEvent_OnClick(Obj);       exit; end;
 end;

Procedure Java_Event_pOnChange(env: PJNIEnv; this: jobject;
                               Obj: TObject; EventType : integer);
 begin
  if not (Assigned(Obj)) then Exit;
  //
  if Obj is jEditText    then begin jEditText   (Obj).GenEvent_OnChange(Obj,EventType); exit; end;
 end;

// LORDMAN
Procedure Java_Event_pOnEnter(env: PJNIEnv; this: jobject; Obj: TObject);
 begin
  //
  if not (Assigned(Obj)) then Exit;
  //
  if Obj is jEditText    then begin jEditText   (Obj).GenEvent_OnEnter(Obj);       exit; end;
 end;

Procedure Java_Event_pOnTimer(env: PJNIEnv; this: jobject; Obj: TObject);
 Var
  Timer : jTimer;
 begin
  If App.Lock then Exit;
  //
  if not (Assigned(Obj)) then Exit;
  if not (Obj is jTimer) then Exit;
  Timer := jTimer(Obj);
  If not (Timer.Enabled)   then Exit;
  If Timer.Parent.State = fsFormClose then Exit;
  If not (Assigned(Timer.OnTimer)) then Exit;
  Timer.OnTimer(Timer);
 end;

Procedure Java_Event_pOnTouch(env: PJNIEnv; this: jobject;
                              Obj: TObject;
                              act,cnt: integer; x1,y1,x2,y2 : single);
 begin
  if not (Assigned(Obj))  then Exit;
  //
  if Obj is jGLViewEvent  then begin jGLViewEvent(Obj).GenEvent_OnTouch(Obj,act,cnt,x1,y1,x2,y2); exit; end;
  if Obj is jView         then begin jView       (Obj).GenEvent_OnTouch(Obj,act,cnt,x1,y1,x2,y2); exit; end;
 end;

//------------------------------------------------------------------------------
//  Form Event
//------------------------------------------------------------------------------

// 2013.09.05 LoadMan  Add CallBack
Procedure Java_Event_pOnClose(env: PJNIEnv; this: jobject);
 Var
  Form  : jForm;
  Inx   : Integer;
  Event : TOnNotify;
  Sender: TObject;
 begin
  jLog('pOnClose');
  Form := App.Forms.stack[App.Forms.Index-1].jForm;
  if Not( Assigned(Form) ) then Exit;
  Form.FVisible  := False;
  If Assigned(Form.OnClose) then Form.OnClose(Form);
  //
  If App.Forms.Index > 0   then Dec(App.Forms.Index);
  // LORDMAN - 2013-08-01 / Call Back - 현재 Form 이전것을 한다.
  Inx := App.Forms.Index;
  if Assigned(App.Forms.Stack[Inx].CloseCB.Event) then
   begin
    Event  := App.Forms.Stack[Inx].CloseCB.Event;
    Sender := App.Forms.Stack[Inx].CloseCB.Sender;
    //
    App.Forms.Stack[Inx].CloseCB.Event  := nil;
    App.Forms.Stack[Inx].CloseCB.Sender := nil;
   end;
 end;

//------------------------------------------------------------------------------
//  Control Event
//------------------------------------------------------------------------------

//
Procedure Java_Event_pOnGLViewState(env: PJNIEnv; this: jobject;
                                    Obj: TObject; State : TGLView_State; w, h: integer);
 begin
  if not (Assigned(Obj))  then Exit;
  //
  if Obj is jGLViewEvent  then
    begin jGLViewEvent(Obj).GenEvent_OnGLViewState(Obj, State, w, h); exit; end;
 end;

//
Function  Java_Event_pOnWebViewState(env: PJNIEnv; this: jobject;
                                     Obj   : TObject;
                                     State : TWebView_State;
                                     URL   : jString) : TWebView_Act;
 Var
  pasWebView : jWebView;
  pasURL     : String;
  pasRst     : Integer;
  pasCanNavi : Boolean;
  _jBoolean  : jBoolean;
 begin
  //
  Result     := WebView_Act_Continue;
  pasWebView := jWebView(Obj);
  if Not(assigned(pasWebView        )) then Exit;
  if Not(assigned(pasWebView.OnState)) then Exit;
  //
  pasURL := '';
  If URL <> nil then
   begin
    _jBoolean := JNI_False;
    pasURL    := String( env^.GetStringUTFChars(Env,URL,@_jBoolean) );
   end;
  //
  pasCanNavi := True;
  pasWebView.OnState(pasWebView,State,pasURL,pasCanNavi);
  If Not(pasCanNavi) then
   Result := WebView_Act_Break;
 end;

// By Simon 2015.03.01
// Warning. AsyncTask is Thread
//          So, Env must be updated
Procedure Java_Event_pOnAsyncTaskState(env: PJNIEnv; this: jobject;
                                       Obj: TObject; State : TAsyncTask_State; Progress : integer);
 begin
  if not (Assigned(Obj)     ) then Exit;
  if not (Obj is jAsyncTask ) then Exit;
  //
  jAsyncTask(Obj).UpdateEnv(env);
  jAsyncTask(Obj).GenEvent_OnAsyncTaskState(Obj,State,Progress);
 end;

//
Procedure Java_Event_pOnHttpState    (env: PJNIEnv; this: jobject;
                                      Obj : TObject; Act : THttp_Act; State : THttp_State; progress : Integer; msg : jString);
Var
 pasMsg    : String;
 _jBoolean : jBoolean;
 begin
  if not (Assigned(Obj)) then Exit;
  if not (Obj is jHttp ) then Exit;
  //
  pasMsg := '';
  If msg <> nil then
   begin
    _jBoolean := JNI_False;
    pasMsg    := String( env^.GetStringUTFChars(Env,Msg,@_jBoolean) );
   end;
  //
  jHttp(Obj).GenEvent_OnHttpEvent(Obj,Act,State,Progress,pasMsg);
 end;

// Camera Preview Event
Procedure Java_Event_pOnCameraFrame (env: PJNIEnv; this: jobject;
                                     Obj: TObject; w, h, fmt : integer; data : jByteArray );
 Var
  pData     : Pointer;
  _jBoolean : jBoolean;
 begin
  jLog('pOnCameraFrame');
  //
  if not (Assigned(Obj))     then Exit;
  if Not(Obj is jCameraView) then Exit;
  //
  _jBoolean  := JNI_False;
  pData := env^.GetByteArrayElements(env,data,_jBoolean);
  jCameraView(Obj).GenEvent_OnCameraFrame(Obj, w, h,fmt,pData);
  env^.ReleaseByteArrayElements(env,data,pData,0);
 end;

//------------------------------------------------------------------------------
// jApp
//------------------------------------------------------------------------------

Constructor jApp.Create;
 begin
  //
  Env.AppName    := '';
  Env.jVM        := nil;
  Env.jEnv       := nil;
  Env.jClass     := nil;
  Env.jControls  := nil;
  Env.jLayout    := nil;
  //
  FPaths.ALARMS        := '';
  FPaths.DCIM          := '';
  FPaths.DOCUMENTS     := '';
  FPaths.DOWNLOADS     := '';
  FPaths.MOVIES        := '';
  FPaths.MUSIC         := '';
  FPaths.NOTIFICATIONS := '';
  FPaths.PICTURES      := '';
  FPaths.PODCASTS      := '';
  FPaths.RINGTONES     := '';
  //
  FPaths.App           := '';
  FPaths.Files         := '';
  FPaths.SDCard        := '';
  FPaths.DataBase      := '';
  FPaths.Shared_Prefs  := '';
  //
  FLock          := False;
  //
  Form           := nil;
  FillChar(Forms,SizeOf(Forms),0);
  //
  OnAppCreate    := nil;
 end;

Destructor jApp.Destroy;
 begin
  inherited;
 end;

Procedure jApp.Init(env: PJNIEnv; Controls : jObject; layout: jObject);
 Var
  JVM : JavaVM;
 begin
  // App.Env.jVM         := _lVM; // by JNI_OnLoad
  App.Env.jEnv           := Env;
  //
  App.Env.jClass         := Env^.NewGlobalRef(Env,Env^.GetObjectClass(Env,Controls));
  App.Env.jControls      := Env^.NewGlobalRef(Env,Controls);  // Global
  App.Env.jLayout        := Env^.NewGlobalRef(Env,layout  );  // Global

  jGetPaths  ( FPaths     );  // Get Path
  jGetDevice ( App.Device );  // Get Device
  //
  jLog('jApp.Init');
 end;

Procedure jApp.Finish;
 begin
  jApp_Finish(App.Env);
 end;

Procedure jApp.SetScreenStyle      ( ScreenStyle       : TScreen_Style       );
 begin
  If Device.ScreenStyle = ScreenStyle then Exit;
  //
  Device.ScreenStyle := ScreenStyle;
  jDevice_SetScreenStyle(App.Env,Device.ScreenStyle);
 end;

Procedure jApp.SetScreenOrientation( ScreenOrientation : TScreen_Orientation );
 begin
  If Device.ScreenOrientation = ScreenOrientation then Exit;
  //
  Device.ScreenOrientation := ScreenOrientation;
  jDevice_SetScreenOrientation(App.Env,Device.ScreenOrientation);
 end;

Procedure jApp.SetTitleBar( visible:boolean );
 begin
  jApp_SetTitleBar(App.Env,visible);
 end;


Function  jApp.GetLog : Boolean;
 begin
  Result := gLog;
 end;

Procedure jApp.SetLog(Value : Boolean);
 begin
  gLog := Value;
 end;

//------------------------------------------------------------------------------
// jForm
//------------------------------------------------------------------------------

Constructor jForm.Create(Owner: jForm);
 begin
  // Initialize
  FjObject              := nil;
  FjLayout              := nil;
  FVisible              := False;
  FEnabled              := True;
  FOwner                := Owner;
  FColor                := clSilver;
  FName                 := '';

  FFormState            := fsFormCreate;
  FCloseCallBack.Event  := nil;
  FCloseCallBack.Sender := nil;
  FBackEnabled          := True;        // Back Button -> Close Form

  FOnActive             := nil;
  FOnClose              := nil;
  FOnCloseQuery         := nil;
  FOnRotate             := nil;
  FOnClick              := nil;
  FOnActivityRst        := nil;

  FjObject              := jForm_Create     (App.Env, Self);
  FjLayout              := jForm_GetlayoutG (App.Env, FjObject);
  FScreenWH             := App.Device.ScreenWH;
  FAnimation.AniIn      := Animation_Effect_None;
  FAnimation.AniOut     := Animation_Effect_None;
  //
  jLog('jForm.Create , Name:' + Self.Name );
 end;

Destructor jForm.Destroy;
 begin
  jLog('jForm.Destroy , Name:' + Self.Name);
  //
  jForm_Free(App.Env, FjObject);
  Self.View := nil;
  //
  jSystem_GC(App.Env);
  {$IfDef FPC} Self      := nil; {$EndIf}
 end;

Procedure jForm.Free;
 begin
  if Self <> nil then Destroy;
 end;

Procedure jForm.setEnabled(Value: Boolean);
begin
 FEnabled := Value;
 jForm_SetEnabled(App.Env, FjObject,FEnabled);
end;

Procedure jForm.setVisible(Value: Boolean);
begin
 FVisible := Value;
 jForm_SetVisibility(App.Env, FjObject ,FVisible);
end;

Procedure jForm.setTitle(Value: Boolean);
begin
 FTitle := Value;
 jForm_SetTitle(App.Env, FjObject ,FTitle);
end;

Procedure jForm.setColor(Value: TColor);
begin
  jView_SetBackGroundColor(App.Env, self.View, Value);
end;

Procedure jForm.Show;
 begin
  if FVisible then Exit;
  if App.Forms.Index = cMaxForm then Exit;
  //
  FVisible       := True;
  //
  App.Forms.Stack[App.Forms.Index].jForm  := Self;
  App.Forms.Stack[App.Forms.Index].CloseCB := FCloseCallBack;
  Inc(App.Forms.Index);
  //
  jForm_Show (App.Env, Self.FjObject,FAnimation.AniIn);
  if Assigned(Self.FOnActive) then Self.OnActive(self);
  FFormState := fsFormWork;
 end;

// Ref. Destroy
Procedure jForm.Close;
 var
  CanClose : boolean;
  rst : Integer;
 begin
  jLog('jForm.Close Name:' + Self.Name );
  // Event : OnCloseQuery
  if Assigned(FOnCloseQuery) then
   begin
    CanClose := False;
    Self.OnCloseQuery(Self, CanClose);
    if CanClose = False then
      begin
       jLog('jForm:Canclose = Cancel');
       Exit;
      end;
   end;
  //
  jLog('jForm:Closing...');
  FFormState := fsFormClose;
  jForm_Close (App.Env, FjObject,FAnimation.AniOut);

  // Post Closing Step
  // --------------------------------------------------------------------------
  // Java           Java          Java-> Pascal
  // jForm_Close -> RemoveView -> Java_Event_pOnClose
 end;

Procedure jForm.Refresh;
 begin
  jView_Invalidate(App.Env, self.View);
 end;

Procedure jForm.SetCloseCallBack(func : TOnNotify; Sender : TObject);
 begin
  FCloseCallBack.Event  := func;
  FCloseCallBack.Sender := Sender;
 end;

// Event : Java -> Pascal
Procedure jForm.GenEvent_OnClick(Obj: TObject);
 begin
  if Assigned(FOnClick) then FOnClick(Obj);
 end;

// Utility
Function jForm.pH ( percent : single ) : Integer; //
 begin
  Result := Round( Height  * percent / 100 );
 end;

Function jForm.pW ( percent : single ) : Integer;
 begin
  Result := Round( Width   * percent / 100 );
 end;

Function jForm.sH ( Scale   : single ) : Integer;
 begin
  Result := Round( Height * Scale );
 end;

Function jForm.sW ( Scale   : single ) : Integer;
 begin
  Result := Round( Width  * Scale );
 end;

//------------------------------------------------------------------------------
// jTextView
//------------------------------------------------------------------------------

Constructor jTextView.Create(Owner: jForm);
 begin
  FOwner     := Owner;
  FjPLayout  := nil;
  FjObject   := jTextView_Create(App.Env, self);
  FVisible   := True;
  FColor     := clTransparent;
  FEnabled   := True;
  FFontColor := clBlack;
  FFontSize  := 15;
 end;

//
Destructor jTextView.Destroy;
 begin
  jLog('AND_Control : jTextView.Destroy');
  inherited;
  if FjObject <> nil then
   begin
    jTextView_Free (App.Env, FjObject);
    FjObject := nil;
   end;
  {$IFDef FPC} Self := nil; {$EndIf}
 end;

Procedure jTextView.Free;
 begin
  if Self <> nil then Destroy;
 end;

Procedure jTextView.SetParent(Value: jObject);
 begin
  FjPLayout := Value;
  jTextView_setParent(App.Env, FjObject, FjPLayout);
 end;

Procedure jTextView.SetXYWH(Value: TXYWH);
 begin
  FXYWH := Value;
  jTextView_setXYWH(App.Env, FjObject, FXYWH.X, FXYWH.Y, FXYWH.W, FXYWH.H);
 end;

Procedure jTextView.SetVisible  (Value : Boolean);
 begin
  FVisible := Value;
  jView_SetVisible(App.Env, FjObject, FVisible);
 end;

Procedure jTextView.setColor(Value: TColor);
 begin
  FColor := Value;
  jView_SetBackGroundColor(App.Env, FjObject, FColor);
 end;

Procedure jTextView.SetEnabled  (Value : Boolean);
 begin
  FEnabled := Value;
  jTextView_setEnabled(App.Env, FjObject, FEnabled);
 end;

Function jTextView.GetText: string;
 begin
  Result := jTextView_getText(App.Env, FjObject);
 end;


Procedure jTextView.SetText(str: string);
 begin
  jTextView_setText(App.Env, FjObject, str);
 end;


Procedure jTextView.SetFontColor(Value: TColor);
 begin
  jTextView_setTextColor(App.Env, FjObject, Value);
 end;


Procedure jTextView.SetFontSize(Value: DWord);
 begin
  jTextView_setTextSize (App.Env, FjObject, Value);
 end;

// LORDMAN 2013-08-12
Procedure jTextView.setTextAlignment(Value: TText_Alignment);
 begin
  jTextView_setTextAlignment(App.Env, FjObject, Value);
 end;

Procedure jTextView.Refresh;
 begin
  jView_Invalidate(App.Env, FjObject);
 end;

// Event : Java -> Pascal
Procedure jTextView.GenEvent_OnClick(Obj: TObject);
 begin
  if Assigned(FOnClick) then FOnClick(Obj);
 end;

//------------------------------------------------------------------------------
// jEditText
//------------------------------------------------------------------------------

Constructor jEditText.Create(Owner: jForm);
 begin
  FOwner     := Owner;
  FjPLayout  := nil;
  FVisible   := True;
  FColor     := clGray;
  FOnEnter   := nil;
  FOnChange  := nil;
  FjObject   := jEditText_Create(App.Env, self);
  FFontColor := clBlack;
  FFontSize  := 15;
  FEditStyle := Edit_Style_SingleLine;
  FEditType  := Edit_Type_Text;
  FHint      := '';
 end;

Destructor jEditText.Destroy;
 begin
  jLog('AND_COntrol : jEditText.Destroy');
  inherited;
  if FjObject <> nil then
   begin
    jEditText_Free (App.Env, FjObject);
    FjObject := nil;
   end;
  {$IFDef FPC} Self := nil; {$EndIf}
 end;

Procedure jEditText.Free;
 begin
  if Self <> nil then
    Destroy;
 end;

Procedure jEditText.SetParent(Value: jObject);
 begin
  FjPLayout  := Value;
  jEditText_setParent(App.Env, FjObject, FjPLayout );
 end;

Procedure jEditText.SetXYWH(Value: TXYWH);
 begin
  FXYWH := Value;
  jEditText_setXYWH(App.Env, FjObject, FXYWH.X, FXYWH.Y, FXYWH.W, FXYWH.H);
 end;

Procedure jEditText.SetVisible  (Value : Boolean);
 begin
  FVisible := Value;
  jView_SetVisible(App.Env, FjObject, FVisible);
 end;

Procedure jEditText.setColor(Value: TColor);
 begin
  FColor := Value;
  jView_SetBackGroundColor(App.Env, FjObject, FColor);
 end;

Procedure jEditText.Refresh;
 begin
  jView_Invalidate(App.Env, FjObject);
 end;

Function jEditText.GetText: string;
 begin
  Result := jEditText_getText(App.Env, FjObject);
 end;

Procedure jEditText.SetText(str: string);
 begin
  jEditText_setText(App.Env, FjObject, str);
 end;

Procedure jEditText.SetFontColor(Value: TColor);
 begin
  jEditText_setTextColor(App.Env, FjObject, Value);
 end;

Procedure jEditText.SetFontSize(Value: DWord);
 begin
  jEditText_setTextSize (App.Env, FjObject, Value);
 end;

Procedure jEditText.SetHint(Value : String);
 begin
  jEditText_setHint(App.Env, FjObject, Value);
 end;

// LORDMAN - 2013-07-26
Procedure jEditText.SetFocus();
 begin
  jEditText_SetFocus(App.Env, FjObject );
 end;

// LORDMAN - 2013-07-26
Procedure jEditText.immShow();
 begin
  jEditText_immShow(App.Env, FjObject );
 end;

// LORDMAN - 2013-07-26
Procedure jEditText.immHide();
 begin
  jEditText_immHide(App.Env, FjObject );
 end;
 
// 2013.07.26 LORDMAN 
// 2015.03.04 DonAlfredo
Procedure jEditText.SetEditStyle(Value : TEdit_Style);
 begin
  jEditText_setEditStyle(App.Env, FjObject,Value);
 end; 

// LORDMAN - 2013-07-26
Procedure jEditText.SetEditType(Value : TEdit_Type);
 begin
  jEditText_setEditType(App.Env, FjObject,Value);
 end;

// LORDMAN - 2013-07-26
Procedure jEditText.SetMaxLength(Value : DWord  );
 begin
  jEditText_maxLength(App.Env, FjObject, Value);
 end;

// LORDMAN - 2013-07-26
Function jEditText.GetCursorPos : TXYi;
 begin
  Result.x := 0;
  Result.y := 0;
  jEditText_GetCursorPos(App.Env, FjObject,Result.x,Result.y);
 end;

// LORDMAN - 2013-07-26
Procedure jEditText.SetCursorPos(Value : TXYi);
 begin
  jEditText_SetCursorPos(App.Env, FjObject, Value.X,Value.Y);
 end;

// LORDMAN 2013-08-12
Procedure jEditText.setTextAlignment(Value: DWord);
begin
  jEditText_setTextAlignment(App.Env, FjObject, Value);
end;

// Event : Java -> Pascal
// LORDMAN - 2013-07-26
Procedure jEditText.GenEvent_OnEnter(Obj: TObject);
 begin
  if Assigned(FOnEnter) then FOnEnter(Obj);
 end;

Procedure jEditText.GenEvent_OnChange(Obj: TObject; EventType : Integer);
 begin
  If Not(Assigned(FOnChange))        then Exit;
  IF Self.FOwner.State = fsFormClose then Exit;

  Case EventType of
   0 : FOnChange(Obj,ctChangeBefore);
   1 : FOnChange(Obj,ctChange      );
   2 : FOnChange(Obj,ctChangeAfter );
  End;
 end;

//------------------------------------------------------------------------------
// jButton
//------------------------------------------------------------------------------

Constructor jButton.Create(Owner: jForm);
 begin
  // Init
  FOwner     := Owner;
  FjPLayout  := nil;
  FVisible   := True;
  FColor     := clGray;
  FOnClick   := nil;

  FjObject   := jButton_Create(App.Env, self);
  FFontColor := clBlack;
  FFontSize  := 15;
 end;

Destructor jButton.Destroy;
 begin
  jLog('AND_Control : jButton.Destroy');
  inherited;
  IF FjObject <> nil then
   begin
    jButton_Free   (App.Env, FjObject);
    FjObject := nil;
   end;
  {$IFDef FPC} Self := nil; {$EndIf}
 end;

Procedure jButton.Free;
 begin
  IF Self <> nil then Destroy;
 end;

Procedure jButton.SetParent(Value: jObject);
 begin
  FjPLayout  := Value;
  jButton_setParent(App.Env, FjObject, FjPLayout );
 end;

Procedure jButton.SetXYWH(Value: TXYWH);
 begin
  FXYWH := Value;
  jButton_setXYWH    (App.Env,FjObject, FXYWH.X, FXYWH.Y, FXYWH.W, FXYWH.H);
  // Default Text Size
  FFontSize := Round(FXYWH.H/2.5);
  SetFontSize (FFontSize);
 end;

Procedure jButton.SetVisible  (Value : Boolean);
 begin
  FVisible := Value;
  jView_SetVisible(App.Env, FjObject, FVisible);
 end;

Procedure jButton.SetColor(Value: TColor);
 begin
  FColor := Value;
  jView_SetBackGroundColor(App.Env, FjObject, FColor);
 end;

Procedure jButton.Refresh;
 begin
  jView_Invalidate(App.Env, FjObject);
 end;

Function jButton.GetText: string;
 begin
  Result := jButton_getText(App.Env, FjObject);
 end;

Procedure jButton.SetText(str: string);
 begin
  jButton_setText(App.Env, FjObject, str);
 end;

Procedure jButton.SetFontColor(Value : TColor );
 begin
  jButton_setTextColor(App.Env, FjObject, Value);
 end;

Procedure jButton.SetFontSize (Value : DWord  );
 begin
  jButton_setTextSize(App.Env, FjObject, Value);
 end;

// Event : Java -> Pascal
Procedure jButton.GenEvent_OnClick(Obj: TObject);
 begin
  IF Assigned(FOnClick) then FOnClick(Obj);
 end;

//------------------------------------------------------------------------------
// jCheckBox
//------------------------------------------------------------------------------

Constructor jCheckBox.Create(Owner: jForm);
 begin
  FOwner     := Owner;
  FjPLayout  := nil;
  FVisible   := True;
  FColor     := clGray;
  FOnClick   := nil;
  FFontColor := clBlack;
  FFontSize  := 15;

  FjObject := jCheckBox_Create(App.Env, self);
 end;

Destructor jCheckBox.Destroy;
 begin
  jLog('AND_Control : jCheckBox.Destroy');
  inherited;
  IF FjObject <> nil then
   begin
    jCheckBox_Free (App.Env, FjObject);
    FjObject := nil;
   end;
  {$IFDef FPC} Self := nil; {$EndIf}
 end;

Procedure jCheckBox.Free;
 begin
  IF Self <> nil then Destroy;
 end;

Procedure jCheckBox.SetParent(Value: jObject);
 begin
  FjPLayout  := Value;
  jCheckBox_setParent(App.Env, FjObject, FjPLayout );
 end;

Procedure jCheckBox.SetXYWH(Value: TXYWH);
 begin
  FXYWH := Value;
  jCheckBox_setXYWH(App.Env,FjObject, FXYWH.X, FXYWH.Y, FXYWH.W, FXYWH.H);
 end;

Procedure jCheckBox.SetVisible  (Value : Boolean);
 begin
  FVisible := Value;
  jView_SetVisible(App.Env, FjObject, FVisible);
 end;

Procedure jCheckBox.SetColor(Value: TColor);
 begin
  FColor := Value;
  jView_SetBackGroundColor(App.Env, FjObject, FColor);
 end;

Procedure jCheckBox.Refresh;
 begin
  jView_Invalidate(App.Env, FjObject);
 end;

Function jCheckBox.GetText: string;
 begin
  Result := jCheckBox_getText(App.Env, FjObject);
 end;

Procedure jCheckBox.SetText(str: string);
 begin
  jCheckBox_setText(App.Env, FjObject, str);
 end;

Procedure jCheckBox.SetFontColor(Value: TColor);
 begin
  jCheckBox_setTextColor(App.Env, FjObject, Value);
 end;

Procedure jCheckBox.SetFontSize(Value: DWord);
 begin
  jCheckBox_setTextSize(App.Env, FjObject, Value);
 end;

Function jCheckBox.GetChecked: boolean;
 begin
  Result := jCheckBox_isChecked(App.Env, FjObject);
 end;

Procedure jCheckBox.SetChecked(Value: boolean);
 begin
  jCheckBox_setChecked(App.Env, FjObject, Value);
 end;

// Event Java -> Pascal
Procedure jCheckBox.GenEvent_OnClick(Obj: TObject);
 begin
  IF Assigned(FOnClick) then FOnClick(Obj);
 end;

//------------------------------------------------------------------------------
// jRadioButton
//------------------------------------------------------------------------------

Constructor jRadioButton.Create(Owner: jForm);
 begin
  FOwner     := Owner;
  FjPLayout  := nil;
  FVisible   := True;
  FColor     := clGray;
  FOnClick   := nil;
  FFontColor := clBlack;
  FFontSize  := 15;
  FjObject   := jRadioButton_Create(App.Env, self);
 end;

Destructor jRadioButton.Destroy;
 begin
  jLog('AND_Control : jRadioButton.Destroy');
  inherited;
  IF FjObject <> nil then
   begin
    jRadioButton_Free (App.Env, FjObject);
    FjObject := nil;
   end;
  {$IFDef FPC} Self := nil; {$EndIf}
 end;

Procedure jRadioButton.Free;
 begin
  IF Self <> nil then Destroy;
 end;

Procedure jRadioButton.SetParent(Value: jObject);
 begin
  FjPLayout  := Value;
  jRadioButton_setParent(App.Env, FjObject, FjPLayout );
 end;

Procedure jRadioButton.SetXYWH(Value: TXYWH);
 begin
  FXYWH := Value;
  jRadioButton_setXYWH(App.Env,FjObject, FXYWH.X, FXYWH.Y, FXYWH.W, FXYWH.H);
 end;

Procedure jRadioButton.SetVisible  (Value : Boolean);
 begin
  FVisible := Value;
  jView_SetVisible(App.Env, FjObject, FVisible);
 end;

Procedure jRadioButton.SetColor(Value: TColor);
 begin
  FColor := Value;
  jView_SetBackGroundColor(App.Env, FjObject, FColor);
 end;

Procedure jRadioButton.Refresh;
 begin
  jView_Invalidate(App.Env, FjObject);
 end;

Function jRadioButton.GetText: string;
 begin
  Result := jRadioButton_getText(App.Env, FjObject);
 end;

Procedure jRadioButton.SetText(str: string);
 begin
  jRadioButton_setText(App.Env, FjObject, str);
 end;

Procedure jRadioButton.SetFontColor(Value: TColor);
 begin
  jRadioButton_setTextColor(App.Env, FjObject, Value);
 end;

Procedure jRadioButton.SetFontSize(Value: DWord);
 begin
  jRadioButton_setTextSize(App.Env, FjObject, Value);
 end;

Function jRadioButton.GetChecked: boolean;
 begin
  Result := jRadioButton_isChecked(App.Env, FjObject);
 end;

Procedure jRadioButton.SetChecked(Value: boolean);
 begin
  jRadioButton_setChecked(App.Env, FjObject, Value);
 end;

// Event Java -> Pascal
Procedure jRadioButton.GenEvent_OnClick(Obj: TObject);
 begin
  IF Assigned(FOnClick) then FOnClick(Obj);
 end;

//------------------------------------------------------------------------------
// jProgressBar
//------------------------------------------------------------------------------

Constructor jProgressBar.Create(Owner: jForm;
                                Style: TProgressBarStyle = ProgressBarStyle_Horizontal);
 begin
  FOwner     := Owner;
  FjPLayout  := nil;
  FVisible   := True;
  FColor     := clGray;
  FjObject   := jProgressBar_Create(App.Env, self, Integer(Style));
 end;

Destructor jProgressBar.Destroy;
 begin
  jLog('AND_Control : jProgressBar.Destroy');
  inherited;
  IF FjObject <> nil then
   begin
    jProgressBar_Free(App.Env, FjObject);
    FjObject := nil;
   end;
  {$IFDef FPC} Self := nil; {$EndIf}
 end;

Procedure jProgressBar.Free;
 begin
  IF Self <> nil then Destroy;
 end;

Procedure jProgressBar.SetParent(Value: jObject);
 begin
  FjPLayout  := Value;
  jProgressBar_setParent(App.Env, FjObject, FjPLayout );
 end;

Procedure jProgressBar.SetXYWH(Value: TXYWH);
 begin
  FXYWH := Value;
  jProgressBar_setXYWH(App.Env, FjObject, FXYWH.X, FXYWH.Y, FXYWH.W, FXYWH.H);
 end;

Procedure jProgressBar.SetVisible  (Value : Boolean);
 begin
  FVisible := Value;
  jView_SetVisible(App.Env, FjObject, FVisible);
 end;

Procedure jProgressBar.SetColor(Value: TColor);
 begin
  FColor := Value;
  jView_SetBackGroundColor(App.Env, FjObject, FColor);
 end;

Procedure jProgressBar.Refresh;
 begin
  jView_Invalidate(App.Env, FjObject);
 end;

Function jProgressBar.GetProgress: integer;
 begin
  Result := jProgressBar_getProgress(App.Env, FjObject);
 end;

Procedure jProgressBar.SetProgress(Value: integer);
 begin
  jProgressBar_setProgress(App.Env, FjObject, Value);
 end;

//------------------------------------------------------------------------------
// jImageView
//------------------------------------------------------------------------------

Constructor jImageView.Create(Owner: jForm);
 begin
  // Init
  FOwner     := Owner;
  FjPLayout  := nil;
  FVisible   := True;
  FImageName := '';
  FOnClick   := nil;
  FjObject   := jImageView_Create(App.Env, Self);
 end;

Destructor jImageView.Destroy;
 begin
  jLog('AND_Control : jImageView.Destroy');
  inherited;
  IF FjObject <> nil then
   begin
    jImageView_Free (App.Env, FjObject);
    FjObject := nil;
   end;
  {$IFDef FPC} Self := nil; {$EndIf}
 end;

Procedure jImageView.Free;
 begin
  IF Self <> nil then Destroy;
 end;

Procedure jImageView.SetParent(Value: jObject);
 begin
  FjPLayout  := Value;
  jImageView_setParent(App.Env,FjObject, FjPLayout );
 end;

Procedure jImageView.SetXYWH(Value: TXYWH);
 begin
  FXYWH := Value;
  jImageView_setXYWH(App.Env, FjObject, FXYWH.X, FXYWH.Y, FXYWH.W, FXYWH.H);
 end;

Procedure jImageView.SetVisible  (Value : Boolean);
 begin
  FVisible := Value;
  jView_SetVisible (App.Env, FjObject, FVisible);
 end;

Procedure jImageView.SetColor(Value: TColor);
 begin
  FColor := Value;
  jView_SetBackGroundColor(App.Env, FjObject, FColor);
 end;

Procedure jImageView.Refresh;
 begin
  jView_Invalidate(App.Env, FjObject);
 end;

Function jImageView.GetImageName: string;
 begin
  Result := FImageName;
 end;

Procedure jImageView.SetImageName(str: string);
 begin
  IF str = '' then str := 'null';
  jImageView_setImage(App.Env, FjObject, str);
 end;

// Event : Java -> Pascal
Procedure jImageView.GenEvent_OnClick(Obj: TObject);
 begin
  IF Assigned(FOnClick) then FOnClick(Obj);
 end;

//------------------------------------------------------------------------------
// jListlView
//------------------------------------------------------------------------------

Constructor jListView.Create(Owner: jForm);
 begin
  // Init
  FOwner      := Owner;
  FjPLayout   := nil;
  FVisible    := True;
  FColor      := clGray;
  FFontSize   := 20;
  FFontColor  := clBlack;
  FjObject    := jListView_Create (App.Env, Self);
 end;

Destructor jListView.Destroy;
 begin
  jLog('AND_Control : jListView.Destroy');
  inherited;
  IF FjObject <> nil then
   begin
    jListView_Free (App.Env, FjObject);
    FjObject := nil;
   end;
  {$IFDef FPC} Self := nil; {$EndIf}
 end;

Procedure jListView.Free;
 begin
  IF Self <> nil then Destroy;
 end;

Procedure jListView.SetParent(Value: jObject);
 begin
  FjPLayout  := Value;
  jListView_setParent(App.Env, FjObject, FjPLayout );
 end;

Procedure jListView.SetXYWH(Value: TXYWH);
 begin
  FXYWH := Value;
  jListView_setXYWH(App.Env, FjObject, FXYWH.X, FXYWH.Y, FXYWH.W, FXYWH.H);
 end;

Procedure jListView.SetVisible  (Value : Boolean);
 begin
  FVisible := Value;
  jView_SetVisible(App.Env, FjObject, FVisible);
 end;

Procedure jListView.setColor (Value: TColor);
 begin
  FColor := Value;
  jView_SetBackGroundColor(App.Env, FjObject, FColor);
 end;

Procedure jListView.Refresh;
 begin
  jView_Invalidate(App.Env, FjObject);
 end;

Procedure jListView.SetFontColor(Value: TColor);
 begin
  jListView_setTextColor(App.Env, FjObject, Value);
 end;


Procedure jListView.SetFontSize(Value: DWord);
begin
  jListView_setTextSize(App.Env, FjObject, Value);
end;

// LORDMAN 2013-08-07
Procedure jListView.SetItemPosition(Value: TXYi);
begin
  jListView_setItemPosition(App.Env, FjObject, Value.X, Value.Y);
end;

//
Procedure jListView.Item_Add   (item : String);
 begin
  jListView_add(App.Env, FjObject, item);
 end;

//
Procedure jListView.Item_Delete(index: Integer);
 begin
  jListView_delete(App.Env, FjObject, index);
 end;

//
Procedure jListView.Item_Clear;
 begin
  jListView_clear(App.Env, FjObject);
 end;

// Event : Java -> Pascal
Procedure jListView.GenEvent_OnClick(Obj: TObject; Value: integer);
 begin
  IF not (Assigned(FOnClickItem)) then Exit;
  FOnClickItem(Obj,Value);
 end;

//------------------------------------------------------------------------------
// jScrollView
//------------------------------------------------------------------------------

Constructor jScrollView.Create(Owner: jForm);
 begin
  // Init
  FOwner      := Owner;
  FjPLayout   := nil;
  FVisible    := True;
  FColor      := clGray;
  FScrollSize := 100;

  FjObject  := jScrollView_Create (App.Env, Self);
  FjLayout  := jScrollView_getView(App.Env, FjObject);
 end;

Destructor jScrollView.Destroy;
 begin
  jLog('AND_Control : jScrollView.Destroy');
  inherited;
  IF FjObject <> nil then
   begin
    jScrollView_Free (App.Env, FjObject);
    FjObject := nil;
   end;
  {$IFDef FPC} Self := nil; {$EndIf}
 end;

Procedure jScrollView.Free;
 begin
  IF Self <> nil then Destroy;
 end;

Procedure jScrollView.SetParent(Value: jObject);
 begin
  FjPLayout  := Value;
  jScrollView_setParent(App.Env, FjObject, FjPLayout );
 end;

Procedure jScrollView.SetXYWH(Value: TXYWH);
 begin
  FXYWH := Value;
  jScrollView_setXYWH(App.Env, FjObject, FXYWH.X, FXYWH.Y, FXYWH.W, FXYWH.H);
 end;

Procedure jScrollView.SetVisible  (Value : Boolean);
 begin
  FVisible := Value;
  jView_SetVisible(App.Env, FjObject, FVisible);
 end;

Procedure jScrollView.SetColor(Value: TColor);
 begin
  FColor := Value;
  jView_SetBackGroundColor(App.Env, FjObject, FColor);
 end;

Procedure jScrollView.Refresh;
 begin
  jView_Invalidate(App.Env, FjObject);
 end;

Procedure jScrollView.SetScrollSize(Value: integer);
 begin
  FScrollSize := Value;
  jScrollView_setScrollSize(App.Env,FjObject, FScrollSize);
 end;

//------------------------------------------------------------------------------
// jHorizontalScrollView
// LORDMAN 2013-09-03
//------------------------------------------------------------------------------

Constructor jHorizontalScrollView.Create(Owner: jForm);
 begin
  // Init
  FOwner      := Owner;
  FjPLayout   := nil;
  FVisible    := True;
  FColor      := clGray;
  FScrollSize := 100;

  FjObject  := jHorizontalScrollView_Create (App.Env, Self);
  FjLayout  := jHorizontalScrollView_getView(App.Env, FjObject);
 end;

Destructor jHorizontalScrollView.Destroy;
 begin
  jLog('AND_Control : jHorizontalScrollView.Destroy');
  inherited;
  IF FjObject <> nil then
   begin
    jHorizontalScrollView_Free (App.Env, FjObject);
    FjObject := nil;
   end;
  {$IFDef FPC} Self := nil; {$EndIf}
 end;

Procedure jHorizontalScrollView.Free;
 begin
  IF Self <> nil then Destroy;
 end;

Procedure jHorizontalScrollView.SetParent(Value: jObject);
 begin
  FjPLayout  := Value;
  jHorizontalScrollView_setParent(App.Env, FjObject, FjPLayout );
 end;

Procedure jHorizontalScrollView.SetXYWH(Value: TXYWH);
 begin
  FXYWH := Value;
  jHorizontalScrollView_setXYWH(App.Env,FjObject, FXYWH.X, FXYWH.Y, FXYWH.W, FXYWH.H);
 end;

Procedure jHorizontalScrollView.SetVisible  (Value : Boolean);
 begin
  FVisible := Value;
  jView_SetVisible(App.Env, FjObject, FVisible);
 end;

Procedure jHorizontalScrollView.SetColor(Value: TColor);
 begin
  FColor := Value;
  jView_SetBackGroundColor(App.Env, FjObject, FColor);
 end;

Procedure jHorizontalScrollView.Refresh;
 begin
  jView_Invalidate(App.Env, FjObject);
 end;

Procedure jHorizontalScrollView.SetScrollSize(Value: integer);
 begin
  FScrollSize := Value;
  jHorizontalScrollView_setScrollSize(App.Env, FjObject, FScrollSize);
 end;

//------------------------------------------------------------------------------
// jViewFlipper
//------------------------------------------------------------------------------

Constructor jViewFlipper.Create(Owner: jForm);
 begin
  // Init
  FOwner     := Owner;
  FjPLayout  := nil;
  FVisible   := True;
  FOnClick   := nil;

  FjObject := jViewFlipper_Create(App.Env, self);
 end;

Destructor jViewFlipper.Destroy;
 begin
  jLog('AND_Control : jViewFlipper.Destroy');
  inherited;
  IF FjObject <> nil then
   begin
    jViewFlipper_Free (App.Env, FjObject);
    FjObject := nil;
   end;
  {$IFDef FPC} Self := nil; {$EndIf}
 end;

Procedure jViewFlipper.Free;
 begin
  IF Self <> nil then Destroy;
 end;

Procedure jViewFlipper.SetParent(Value: jObject);
 begin
  FjPLayout  := Value;
  jViewFlipper_setParent(App.Env, FjObject, FjPLayout );
 end;

Procedure jViewFlipper.SetXYWH(Value: TXYWH);
 begin
  FXYWH := Value;
  jViewFlipper_setXYWH(App.Env, FjObject, FXYWH.X, FXYWH.Y, FXYWH.W, FXYWH.H);
 end;

Procedure jViewFlipper.SetVisible  (Value : Boolean);
 begin
  FVisible := Value;
  jView_SetVisible(App.Env, FjObject, FVisible);
 end;

Procedure jViewFlipper.SetColor(Value: TColor);
 begin
  FColor := Value;
  jView_SetBackGroundColor(App.Env, FjObject, FColor);
 end;

Procedure jViewFlipper.Refresh;
 begin
  jView_Invalidate(App.Env, FjObject);
 end;

//------------------------------------------------------------------------------
// jWebView
//------------------------------------------------------------------------------

Constructor jWebView.Create(Owner: jForm);
 begin
  // Init
  FOwner      := Owner;
  FjPLayout   := nil;
  FVisible    := True;
  FColor      := clGray;
  FJavaScript := False;
  FOnState    := nil;

  FjObject   := jWebView_Create(App.Env, Self);
 end;

Destructor jWebView.Destroy;
 begin
  jLog('AND_Control : jWebView.Destroy');
  inherited;
  IF FjObject <> nil then
   begin
    jWebView_Free  (App.Env, FjObject);
    FjObject := nil;
   end;
  {$IFDef FPC} Self := nil; {$EndIf}
 end;

Procedure jWebView.Free;
 begin
  IF Self <> nil then Destroy;
 end;

Procedure jWebView.SetParent(Value: jObject);
 begin
  FjPLayout  := Value;
  jWebView_setParent(App.Env, FjObject, FjPLayout );
 end;

Procedure jWebView.SetXYWH(Value: TXYWH);
 begin
  FXYWH := Value;
  jWebView_setXYWH(App.Env, FjObject, FXYWH.X, FXYWH.Y, FXYWH.W, FXYWH.H);
 end;

Procedure jWebView.SetVisible  (Value : Boolean);
 begin
  FVisible := Value;
  jView_SetVisible(App.Env, FjObject, FVisible);
 end;

Procedure jWebView.SetColor(Value: TColor);
 begin
  FColor := Value;
  jView_SetBackGroundColor(App.Env, FjObject, FColor);
 end;

Procedure jWebView.Refresh;
 begin
  jView_Invalidate(App.Env, FjObject);
 end;

Procedure jWebView.SetJavaScript(Value : Boolean);
 begin
  FJavaScript := Value;
  jWebView_SetJavaScript(App.Env, FjObject, FJavaScript);
 end;

Procedure jWebView.Navigate(url: string);
 begin
  jWebView_loadURL(App.Env, FjObject, url);
 end;

//------------------------------------------------------------------------------
// jBitmap
//------------------------------------------------------------------------------

Constructor jBitmap.Create;
 begin
  // Init
  FWidth   := 0;
  FHeight  := 0;
  //
  FjObject := jBitmap_Create(App.Env, Self);
 end;

Destructor jBitmap.Destroy;
 begin
  jLog('AND_Control : jBitmap.Destroy');
  inherited;
  IF FjObject <> nil then
   begin
    jBitmap_Free   (App.Env, FjObject);
    FjObject := nil;
   end;
  {$IFDef FPC} Self := nil; {$EndIf}
 end;

Procedure jBitmap.Free;
 begin
  IF Self <> nil then Destroy;
 end;

Procedure jBitmap.LoadFromFile(const FileName : String);
 begin
  jBitmap_loadFile(App.Env, FjObject, filename);
  jBitmap_getWH   (App.Env, FjObject,FWidth,FHeight);
 end;

Procedure jBitmap.createBitmap(w,h : Integer);
 begin
  jBitmap_createBitmap(App.Env, FjObject, w,h );
  FWidth  := w;
  FHeight := h;
 end;

Function  jBitmap.GetJavaBitmap : jObject;
 begin
  Result := jBitmap_getJavaBitmap(App.Env, FjObject);
 end;

//------------------------------------------------------------------------------
// jCanvas
//------------------------------------------------------------------------------

Constructor jCanvas.Create;
 begin
  // Init
  FjObject := jCanvas_Create(App.Env, Self);
 end;

Destructor jCanvas.Destroy;
 begin
  jLog('AND_Control : jCanvas.Destroy');
  inherited;
  IF FjObject <> nil then
   begin
    jCanvas_Free   (App.Env, FjObject);
    FjObject := nil;
   end;
  {$IFDef FPC} Self := nil; {$EndIf}
 end;

Procedure jCanvas.Free;
 begin
  IF Self <> nil then Destroy;
 end;

Procedure jCanvas.setStrokeWidth       (width : single );
 begin
  jCanvas_setStrokeWidth(App.Env,FjObject,width);
 end;

Procedure jCanvas.setStyle             (style : TPaint_Style);
 begin
  jCanvas_setStyle      (App.Env,FjObject,Integer(style));
 end;

Procedure jCanvas.setColor             (color : TColor );
 begin
  jCanvas_setColor      (App.Env,FjObject,color);
 end;

Procedure jCanvas.setTextSize          (textsize : single );
 begin
  jCanvas_setTextSize   (App.Env,FjObject,textsize);
 end;

Procedure jCanvas.drawLine             (x1,y1,x2,y2 : single);
 begin
  jCanvas_drawLine      (App.Env,FjObject,x1,y1,x2,y2);
 end;

Procedure jCanvas.drawPoint            (x1,y1 : single);
 begin
  jCanvas_drawPoint     (App.Env,FjObject,x1,y1);
 end;

Procedure jCanvas.drawText             (Text : String; x,y : single);
 begin
  jCanvas_drawText      (App.Env,FjObject,text,x,y);
 end;

Procedure jCanvas.drawBitmap           (bmp : jBitmap; b,l,r,t : integer);
 begin
  jCanvas_drawBitmap    (App.Env,FjObject,bmp.GetjavaBitmap,b,l,r,t);
 end;

//------------------------------------------------------------------------------
// jView
//------------------------------------------------------------------------------

Constructor jView.Create(Owner: jForm);
 begin
  // Init
  FOwner          := Owner;
  FjPLayout       := nil;
  FjCanvas        := nil;
  FVisible        := True;
  FColor          := clGray;
  //
  FMouches.Mouch.Active := False;
  FMouches.Mouch.Start  := False;
  FMouches.Mouch.Zoom   := 1.0;
  FMouches.Mouch.Angle  := 0.0;
  //
  FjCanvas := jCanvas.Create; // Java : jCanvas
  FjObject := jView_Create(App.Env, Self);
  jView_setjCanvas(App.Env, Self.FjObject, FjCanvas.JavaObj);
 end;

Destructor jView.Destroy;
 begin
  jLog('AND_Control : jView.Destroy');
  inherited;
  IF FjObject <> nil then
   begin
    //
    jView_Free     (App.Env, FjObject);
    FjObject := nil;
    //
    FjCanvas.Free;
   end;
  {$IFDef FPC} Self := nil; {$EndIf}
 end;

Procedure jView.Free;
 begin
  IF Self <> nil then Destroy;
 end;

Procedure jView.SetParent(Value: jObject);
 begin
  FjPLayout  := Value;
  jView_setParent(App.Env,FjObject, FjPLayout );
 end;

Procedure jView.SetXYWH(Value: TXYWH);
 begin
  FXYWH := Value;
  jView_setXYWH(App.Env,FjObject, FXYWH.X, FXYWH.Y, FXYWH.W, FXYWH.H);
 end;

Procedure jView.SetVisible  (Value : Boolean);
 begin
  FVisible := Value;
  jView_SetVisible(App.Env, FjObject, FVisible);
 end;

Procedure jView.SetColor(Value: TColor);
 begin
  FColor := Value;
  jView_SetBackGroundColor(App.Env, FjObject, FColor);
 end;

// LORDMAN 2013-08-14
Procedure jView.SaveToFile(sFileName:String);
begin
 jView_SaveToFile(App.Env, FjObject, sFileName);
end;

Procedure jView.Refresh;
 begin
  jView_Invalidate(App.Env, FjObject);
 end;

// Event : Java Event -> Pascal
Procedure jView.GenEvent_OnTouch(Obj: TObject; Act,Cnt: integer; X1,Y1,X2,Y2: Single);
 begin
  case TTouch_Type(Act) of
   Touch_Down : VHandler_touchesDown (Obj,Cnt,XYf(X1,Y1),XYf(X2,Y2),FOnTouchDown,FMouches);
   Touch_Move : VHandler_touchesMove (Obj,Cnt,XYf(X1,Y1),XYf(X2,Y2),FOnTouchMove,FMouches);
   Touch_Up   : VHandler_touchesUp   (Obj,Cnt,XYf(X1,Y1),XYf(X2,Y2),FOnTouchUp  ,FMouches);
  end;
 end;

// Event : Java Event -> Pascal
Procedure jView.GenEvent_OnDraw(Obj: TObject; jCanvas: jObject);
 begin
  IF Assigned(FOnDraw) then FOnDraw(Obj,FjCanvas);
 end;

//------------------------------------------------------------------------------
// jGLViewEvent
//------------------------------------------------------------------------------
Constructor jGLViewEvent.Create;
 begin
  // Clear Event
  FOnGLCreate  := nil;
  FOnGLSize    := nil;
  FOnGLDraw    := nil;
  FOnGLDestroy := nil;
  //
  FOnTouchDown := nil;
  FOnTouchMove := nil;
  FOnTouchUp   := nil;
  //
  FMouches.Mouch.Active := False;
  FMouches.Mouch.Start  := False;
  FMouches.Mouch.Zoom   := 1.0;
  FMouches.Mouch.Angle  := 0.0;
 end;

Destructor jGLViewEvent.Destroy;
 begin
  jLog('AND_Control : jGLSurfaceView.Destroy');
  inherited;
  FOnGLCreate  := nil;
  FOnGLSize    := nil;
  FOnGLDraw    := nil;
  FOnGLDestroy := nil;
  //
  FOnTouchDown := nil;
  FOnTouchMove := nil;
  FOnTouchUp   := nil;
  //
 end;

Procedure jGLViewEvent.Free;
 begin
  IF Self <> nil then Destroy;
 end;

// Event : Java Event -> Pascal
Procedure jGLViewEvent.GenEvent_OnTouch(Obj: TObject; Act,Cnt: integer; X1,Y1,X2,Y2: single);
 begin
  App.Lock := True;
  case TTouch_Type(Act) of
   Touch_Down : VHandler_touchesDown(Obj,Cnt,XYf(X1,Y1),XYf(X2,Y2),FOnTouchDown,FMouches);
   Touch_Move : VHandler_touchesMove(Obj,Cnt,XYf(X1,Y1),XYf(X2,Y2),FOnTouchMove,FMouches);
   Touch_Up   : VHandler_touchesUp  (Obj,Cnt,XYf(X1,Y1),XYf(X2,Y2),FOnTouchUp  ,FMouches);
  end;
  App.Lock := False;
 end;

//
Procedure jGLViewEvent.GenEvent_OnGLViewState (Obj: TObject; State : TGLView_State; w, h: integer);
 begin
  App.Lock := True;
  Case State of
   GLView_State_SurfaceCreated   : If Assigned(FOnGLCreate ) then FOnGLCreate (Obj);
   GLView_State_SurfaceChanged   : If Assigned(FOnGLSize   ) then FOnGLSize   (Obj,w,h);
   GLView_State_DrawFrame        : If Assigned(FOnGLDraw   ) then FOnGLDraw   (Obj);
   GLView_State_SurfaceDestroyed : If Assigned(FOnGLDestroy) then FOnGLDestroy(Obj);
   GLView_State_SurfaceThread    : If Assigned(FOnGLThread ) then FOnGLThread (Obj);
  end;
  App.Lock := False;
 end;

//------------------------------------------------------------------------------
// jCameraView
//------------------------------------------------------------------------------

Constructor jCameraView.Create(Owner: jForm);
 begin
  // Init
  FOwner          := Owner;
  FjPLayout       := nil;
  FVisible        := True;
  //
  FjObject := jCameraView_Create(App.Env, Self);
 end;

Destructor jCameraView.Destroy;
 begin
  jLog('AND_Control : jView.Destroy');
  inherited;
  IF FjObject <> nil then
   begin
    //
    jCameraView_Free     (App.Env, FjObject);
    FjObject := nil;
   end;
  {$IFDef FPC} Self := nil; {$EndIf}
 end;

Procedure jCameraView.Free;
 begin
  IF Self <> nil then Destroy;
 end;

Procedure jCameraView.SetParent(Value: jObject);
 begin
  FjPLayout  := Value;
  jCameraView_setParent(App.Env,FjObject, FjPLayout );
 end;

Procedure jCameraView.SetXYWH(Value: TXYWH);
 begin
  FXYWH := Value;
  jCameraView_setXYWH(App.Env,FjObject, FXYWH.X, FXYWH.Y, FXYWH.W, FXYWH.H);
 end;

Procedure jCameraView.SetVisible  (Value : Boolean);
 begin
  FVisible := Value;
  jView_SetVisible(App.Env, FjObject, FVisible);
 end;

Procedure jCameraView.SaveToFile  (FileName : String);
 begin
  jCameraView_saveImage(App.Env,FjObject, FileName );
 end;

Procedure jCameraView.GenEvent_OnCameraFrame(Obj: TObject; w, h, fmt : integer; data : pointer);
 begin
  App.Lock := True;
  If Assigned(FOnFrame ) then FOnFrame (Obj,w,h,fmt,data);
  App.Lock := False;
 end;

//------------------------------------------------------------------------------
// jTimer
//------------------------------------------------------------------------------

Constructor jTimer.Create(Form: jForm);
 begin
  // Init
  FEnabled  := False;
  FInterval := 1000;
  FOnNotIFy := nil;
  FParent   := Form;

  FjObject  := jTimer_Create(App.Env, Self);
  jLog('Timer Create');
 end;

Destructor jTimer.Destroy;
 begin
  jLog('AND_Control : jTimer.Destroy');
  inherited;
  IF FjObject <> nil then
   begin
    FOnNotify := nil;
    jTimer_Free    (App.Env, FjObject);
    FjObject := nil;
   end;
  {$IFDef FPC} Self := nil; {$EndIf}
 end;

Procedure jTimer.Free;
 begin
  IF Self <> nil then
    Destroy;
 end;

Procedure jTimer.SetEnabled(Value: boolean);
 begin
  FEnabled := Value;
  jTimer_SetEnabled(App.Env, FjObject, FEnabled);
 end;

Procedure jTimer.SetInterval(Value: integer);
begin
  FInterval := Value;
  jTimer_SetInterval(App.Env, FjObject, Value);
end;

Procedure jTimer.SetOnTimer(Value: TOnNotIFy);
 begin
  FOnNotify := Value;
 end;

//------------------------------------------------------------------------------
// jDialog YN
//------------------------------------------------------------------------------

Constructor jDialogYN.Create(Form: jForm; Title, Msg, Y, N: string);
 begin
  // Init
  FParent  := Form;
  FjObject := jDialogYN_Create(App.Env, Self, Title, Msg, Y, N);
 end;

Destructor jDialogYN.Destroy;
 begin
  jLog('AND_Control : jDialogYN.Destroy');
  inherited;
  IF FjObject <> nil then
   begin
    jDialogYN_Free (App.Env, FjObject);
    FjObject := nil;
   end;
  {$IFDef FPC} Self := nil; {$EndIf}
 end;

Procedure jDialogYN.Free;
 begin
  IF Self <> nil then Destroy;
 end;

Procedure jDialogYN.Show;
 begin
  jDialogYN_Show(App.Env, FjObject);
 end;

// Event : Java -> Pascal
Procedure jDialogYN.GenEvent_OnClick(Obj: TObject; Value: integer);
 begin
  IF not (Assigned(FOnDialogYN)) then Exit;

  jLog('Click :' + IntToStr(Value));
  Case TClick(Value) of
   Click_Yes : FOnDialogYN(Obj, Click_Yes);
   Click_No  : FOnDialogYN(Obj, Click_No );
  end;
 end;

//------------------------------------------------------------------------------
// jDialog Progress
//------------------------------------------------------------------------------

Constructor jDialogProgress.Create(Form: jForm; Title, Msg : string);
 begin
  // Init
  FParent  := Form;
  FjObject := jDialogProgress_Create(App.Env, Self, Title, Msg);
 end;

Destructor jDialogProgress.Destroy;
 begin
  jLog('AND_Control : jDialogProgress.Destroy');
  inherited;
  IF FjObject <> nil then
   begin
    jDialogProgress_Free(App.Env, FjObject);
    FjObject := nil;
   end;
  {$IFDef FPC} Self := nil; {$EndIf}
 end;

Procedure jDialogProgress.Free;
 begin
  IF Self <> nil then Destroy;
 end;

//------------------------------------------------------------------------------
// jImageBtn
//------------------------------------------------------------------------------

Constructor jImageBtn.Create(Owner: jForm);
 begin
  // Init
  FOwner     := Owner;
  FjPLayout  := nil;
  FVisible   := True;
  FColor     := clGray;
  FjObject   := jImageBtn_Create(App.Env, Self);
 end;

Destructor jImageBtn.Destroy;
 begin
  jLog('AND_Control : jImageBtn.Destroy');
  inherited;
  IF FjObject <> nil then
   begin
    jImageBtn_Free (App.Env, FjObject);
    FjObject := nil;
   end;
  {$IFDef FPC} Self := nil; {$EndIf}
 end;

Procedure jImageBtn.Free;
 begin
  IF Self <> nil then
    Destroy;
 end;

Procedure jImageBtn.SetParent(Value: jObject);
 begin
  FjPLayout  := Value;
  jImageBtn_setParent(App.Env, FjObject, FjPLayout );
 end;

Procedure jImageBtn.SetXYWH(Value: TXYWH);
 begin
  FXYWH := Value;
  jImageBtn_setXYWH(App.Env,
                    FjObject, FXYWH.X, FXYWH.Y, FXYWH.W, FXYWH.H);
 end;

Procedure jImageBtn.SetVisible  (Value : Boolean);
 begin
  FVisible := Value;
  jView_SetVisible(App.Env, FjObject, FVisible);
 end;

Procedure jImageBtn.SetColor (Value: TColor);
 begin
  FColor := Value;
  jView_SetBackGroundColor(App.Env, FjObject, FColor);
 end;

// LORDMAN 2013-08-16
Procedure jImageBtn.SetEnabled  (Value : Boolean);
 begin
  FEnabled := Value;
  jImageBtn_SetEnabled(App.Env, FjObject,FEnabled);
 end;

Procedure jImageBtn.Refresh;
 begin
  jView_Invalidate(App.Env, FjObject);
 end;

Procedure jImageBtn.SetButton(up, dn: string);
 begin
  jImageBtn_setButton(App.Env, FjObject, up, dn);
 end;

// Event : Java -> Pascal
Procedure jImageBtn.GenEvent_OnClick(Obj: TObject);
 begin
  IF Assigned(FOnClick) then
   FOnClick(Obj);
 end;

//------------------------------------------------------------------------------
// jAsyncTask
// http://stackoverflow.com/questions/5517641/publishprogress-from-inside-a-function-in-doinbackground
//------------------------------------------------------------------------------

//
Constructor jAsyncTask.Create(Owner: jForm);
 begin
  // Init
  FOwner        := Owner;
  FEnv          := App.Env;
  FjObject      := jAsyncTask_Create(App.Env, Self);
  FBusy         := False;
  //
  FOnState      := nil;
 end;

Destructor jAsyncTask.Destroy;
 begin
  jLog('AND_Control : jAsyncTask.Destroy');
  inherited;
  IF FjObject <> nil then
   begin
    jAsyncTask_Free (App.Env, FjObject);
    FjObject := nil;
   end;
  {$IFDef FPC} Self := nil; {$EndIf}
 end;

Procedure jAsyncTask.Free;
 begin
  IF Self <> nil then
    Destroy;
 end;

Procedure jAsyncTask.Execute;
 begin
  FBusy := True;
  jAsyncTask_Execute(App.Env, FjObject);
 end;

Procedure jAsyncTask.UpdateUI(Progress : Integer);
 begin
  jAsyncTask_setProgress(FEnv, FjObject,Progress);
 end;

Procedure jAsyncTask.UpdateEnv(Env : PJniEnv);
 begin
  Self.FEnv.jEnv := Env;
 end;

Procedure jAsyncTask.GenEvent_OnAsyncTaskState(Obj: TObject; State : TAsyncTask_State; Progress:Integer);
 begin
  If Not(Assigned(FOnState)) then Exit;
  //
  FBusy := True;
  FOnState(Obj,State,Progress);
  If State = AsyncTask_State_PostExecute then
   FBusy := False;
 end;

//------------------------------------------------------------------------------
// jHttp
//------------------------------------------------------------------------------

//
Constructor jHttp.Create(Owner: jForm);
 begin
  // Init
  FOwner        := Owner;
  Env           := App.Env;
  FjObject      := jHttp_Create(App.Env, Self);
  //
  FOnState      := nil;
 end;

Destructor jHttp.Destroy;
 begin
  jLog('jHttp.Destroy');
  inherited;
  IF FjObject <> nil then
   begin
    jHttp_Free (App.Env, FjObject);
    FjObject := nil;
   end;
  {$IFDef FPC} Self := nil; {$EndIf}
 end;

Procedure jHttp.Free;
 begin
  IF Self <> nil then
    Destroy;
 end;

Procedure jHttp.getText(url : String);
 begin
  FBusy := True;
  jHttp_getText(App.Env, FjObject,url);
 end;

Procedure jHttp.DownloadFile(url : String; localFile : string);
 begin
  jHttp_DownloadFile(App.Env, FjObject,url,localFile);
 end;

Procedure jHttp.UploadFile  (url : String; localFile : string);
 begin
  jHttp_UploadFile(App.Env, FjObject,url,localFile);
 end;

procedure jHttp.GenEvent_OnHttpEvent(Obj: TObject;
                                     Act: THttp_Act; State : THttp_State; Progress : Integer; Msg : String);
 begin
  If Not(Assigned(FOnState)) then Exit;
  FOnState(Obj,Act,State,Progress,Msg);
 end;

end.
