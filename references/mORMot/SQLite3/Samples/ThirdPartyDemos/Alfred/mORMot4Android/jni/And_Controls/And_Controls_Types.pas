//
//  And_Controls Base Type & Constant
//
//     simonsayz@naver.com / 최원식옹
//
//   2015.02.24 Started
//

unit And_Controls_Types;

{$IFDef FPC} {$mode delphi} {$packrecords c} {$EndIF}

interface

uses
  SysUtils, Types, Classes,
  And_Jni;

Const
 // standard colors
 clBlack                = $FF000000;
 clMaroon               = $FF800000;
 clGreen                = $FF008000;
 clOlive                = $FF808000;
 clNavy                 = $FF000080;
 clPurple               = $FF800080;
 clTeal                 = $FF008080;
 clGray                 = $FF808080;
 clSilver               = $FFC0C0C0;
 clRed                  = $FFFF0000;
 clLime                 = $FF00FF00;
 clYellow               = $FFFFFF00;
 clBlue                 = $FF0000FF;
 clFuchsia              = $FFFF00FF;
 clAqua                 = $FF00FFFF;
 clWhite                = $FFFFFFFF;
 clLtGray               = clSilver;
 clDkGray               = clGray;

 // extended colors
 clMoneyGreen           = $FFC0DCC0;
 clSkyBlue              = $FFA6CAF0;
 clCream                = $FFFFFBF0;
 clMedGray              = $FFA0A0A4;

 clCyan                 = clAqua;
 clMagenta              = clFuchsia;
 clTransparent          = $00000000;

 // Max
 cMaxTouch              =  10; // Max Touch
 cMaxForm               =  40; // Max Form Stack Count

 //
 cLogModeOn             = True;
 cLogModeOff            = False;

Type
 //----------------------------------------------------------------------------
 // Base Types
 //----------------------------------------------------------------------------
 cInt    = Int32;
 PInt32  = PjInt;
 PSingle = PjFloat;

 {$IfDef DCC}
 pAnsiChar  = MarshaledAString;
 {$EndIf}
 //----------------------------------------------------------------------------
 // General Types
 //----------------------------------------------------------------------------
 TOnAppCreate           = Procedure;
 TOnNotify              = Procedure(Sender: TObject) of object;
 //
 TLangType              = (ltNone,
                           ltPascal,
                           ltJava,
                           ltAll);
 //
 TBytes                 = Array[0..0] of Byte;
 pBytes                 = ^TBytes;
 //
 TRGB                   = Packed Record
                           R,G,B : Byte;
                          End;
 pRGB                   = ^TRGB;
 TRGBs                  = Packed Array[0..0] of TRGB;
 pRGBs                  = ^TRGBs;
 TImgRGB                = Record
                           Width  : Integer;
                           Height : Integer;
                           pRGBs_ : pRGBs;
                          End;
 //
 TXYi                   = Record
                           X,Y : Integer;
                          end;
 TXYf                   = Record
                           X,Y : Single;
                          end;
 TWH                    = Record
                           Width  : Integer;
                           Height : Integer;
                          End;
 TWidthHeight           = TWH;
 TXYWH                  = Record
                           X, Y, W, H: Integer;
                          End;
 TColor                 = DWord;
 TChangeType            = (ctChangeBefore,
                           ctChange,
                           ctChangeAfter);
 TFormState             = (fsFormCreate,  // Initializing
                           fsFormWork,    // Working
                           fsFormClose);  // Closing
 //----------------------------------------------------------------------------
 // Java - And_Controls Types  (And_Controls Const )
 //        XRef. Controls.java - Const
 //----------------------------------------------------------------------------
 //
 TDirectory_Type        = ( DIRECTORY_ALARMS                     =     0,
                            DIRECTORY_DCIM                       =     1,
                            DIRECTORY_DOCUMENTS                  =     2,
                            DIRECTORY_DOWNLOADS                  =     3,
                            DIRECTORY_MOVIES                     =     4,
                            DIRECTORY_MUSIC                      =     5,
                            DIRECTORY_NOTIFICATIONS              =     6,
                            DIRECTORY_PICTURES                   =     7,
                            DIRECTORY_PODCASTS                   =     8,
                            DIRECTORY_RINGTONES                  =     9,
                            DIRECTORY_App                        = 10001,
                            DIRECTORY_Files                      = 10002,
                            DIRECTORY_SDCard                     = 10003,
                            DIRECTORY_DataBase                   = 10004,
                            DIRECTORY_Shared_Prefs               = 10005);
 //
 TScreen_Style          = ( Screen_Style_Normal                  =  0,
                            Screen_Style_Full                    =  1);
 //
 TNetwork_Type          = ( Network_Type_None                    =  0,
                            Network_Type_Wifi                    =  1,
                            Network_Type_Mobile                  =  2);
 //
 TPaint_Style           = ( Paint_Style_Fill                     =  0,
                            Paint_Style_Fill_And_Stroke          =  1,
                            Paint_Style_Stroke                   =  2);
 //
 TCompress_Format       = ( Compress_Format_PNG                  =  0,
                            Compress_Format_JPEG                 =  1);
 //
 TTouch_Type            = ( Touch_Down                           =  0,
                            Touch_Move                           =  1,
                            Touch_Up                             =  2);
 //
 TClick                 = ( Click_Default                        =  0,
                            Click_Yes                            = -1,
                            Click_No                             = -2);
 // Animation
 TAnimation_Effect      = ( Animation_Effect_None                = $00000000,
                            Animation_Effect_iR2L                = $00000001,
                            Animation_Effect_oR2L                = $00000002,
                            Animation_Effect_iL2R                = $00000004,
                            Animation_Effect_oL2R                = $00000008,
                            Animation_Effect_FadeIn              = $00000010,
                            Animation_Effect_FadeOut             = $00000020);
 //
 TGLView_State          = ( GLView_State_SurfaceCreated          =  0,
                            GLView_State_SurfaceChanged          =  1,
                            GLView_State_DrawFrame               =  2,
                            GLView_State_SurfaceDestroyed        =  3,
                            GLView_State_SurfaceThread           =  4);
 //
 TWebView_Act           = ( WebView_Act_Continue                 =  0,
                            WebView_Act_Break                    =  1);
 //
 TWebView_State         = ( WebView_State_Unknown                =  0,
                            WebView_State_Before                 =  1,
                            WebView_State_Finish                 =  2,
                            WebView_State_Error                  =  3);
 //
 TAsyncTask_State       = ( AsyncTask_State_PreExecute           =  0,
                            AsyncTask_State_Update               =  1,
                            AsyncTask_State_PostExecute          =  2,
                            AsyncTask_State_BackGround           =  3);
 //
 TEdit_Style            = ( Edit_Style_SingleLine                =  0,
                            Edit_Style_MultiLine                 =  1);                                                      
 //
 TEdit_Type             = ( Edit_Type_Number                     =  0,
                            Edit_Type_Text                       =  1,
                            Edit_Type_Phone                      =  2,
                            Edit_Type_PassNumber                 =  3,
                            Edit_Type_PassText                   =  4);
 //
 THttp_Act              = ( Http_Act_Text                        =  0,
                            Http_Act_Download                    =  1,
                            Http_Act_Upload                      =  2);
 //
 THttp_State            = ( Http_State_Xfer                      =  0,
                            Http_State_Done                      =  1,
                            Http_State_Error                     =  2);

 //----------------------------------------------------------------------------
 // Java - Android Native Types
 //----------------------------------------------------------------------------
 TText_Alignment        = ( Text_Alignment_INHERIT               =  0,
                            Text_Alignment_GRAVITY               =  1,
                            Text_Alignment_TEXT_START            =  2,
                            Text_Alignment_TEXT_END              =  3,
                            Text_Alignment_CENTER                =  4,
                            Text_Alignment_VIEW_START            =  5,
                            Text_Alignment_VIEW_END              =  6);

 TAndroid_Log           = ( Android_Log_UNKNOWN                  =  0,
                            Android_Log_DEFAULT                  =  1,
                            Android_Log_VERBOSE                  =  2,
                            Android_Log_DEBUG                    =  3,
                            Android_Log_INFO                     =  4,
                            Android_Log_WARN                     =  5,
                            Android_Log_ERROR                    =  6,
                            Android_Log_FATAL                    =  7,
                            Android_Log_SILENT                   =  8);
 //
 TActivity_Result       = ( Activity_Result_OK                   = -1,
                            Activity_Result_CANCELED             =  0);
 //
 TScreen_Rotate         = ( Screen_Rotate_UNDEFINED              =  0,
                            Screen_Rotate_PORTRAIT               =  1,
                            Screen_Rotate_LANDSCAPE              =  2,
                            Screen_Rotate_SQUARE                 =  3); // Deprecated API 16
 //
 TScreen_Orientation    = ( Screen_Orientation_UNSPECIFIED       = -1,
                            Screen_Orientation_LANDSCAPE         =  0,
                            Screen_Orientation_PORTRAIT          =  1,
                            Screen_Orientation_USER              =  2,
                            Screen_Orientation_BEHIND            =  3,
                            Screen_Orientation_SENSOR            =  4,
                            Screen_Orientation_NOSENSOR          =  5,
                            Screen_Orientation_SENSOR_LANDSCAPE  =  6,
                            Screen_Orientation_SENSOR_PORTRAIT   =  7,
                            Screen_Orientation_REVERSE_LANDSCAPE =  8,
                            Screen_Orientation_REVERSE_PORTRAIT  =  9,
                            Screen_Orientation_FULL_SENSOR       = 10,
                            Screen_Orientation_USER_LANDSCAPE    = 11,
                            Screen_Orientation_USER_PORTRAIT     = 12,
                            Screen_Orientation_FULL_USER         = 13,
                            Screen_Orientation_LOCKED            = 14);
 //
 TGLES_Version          = ( GLES_v1                              =  1,
                            GLES_v2                              =  2);
 //
 TProgressBarStyle      = ( ProgressBarStyle_Horizontal          = $01010078,
                            ProgressBarStyle_Large               = $0101007a);

 // jnigraphics
 TAndroid_Bitmap_Result = ( Android_Bitmap_Result_SUCCESS           =  0,
                            Android_Bitmap_Result_BAD_PARAMETER     = -1,
                            Android_Bitmap_Result_JNI_EXCEPTION     = -2,
                            Android_Bitmap_Result_ALLOCATION_FAILED = -3);

 TAndroid_Bitmap_Format = ( Android_Bitmap_Format_NONE              =  0,
                            Android_Bitmap_Format_RGBA_8888         =  1,
                            Android_Bitmap_Format_RGB_565           =  4,
                            Android_Bitmap_Format_RGBA_4444         =  7,
                            Android_Bitmap_Format_A_8               =  8);

 TAndroid_Bitmap_Info   = Record
                           width  : Cardinal; // uint32_t;
                           height : Cardinal; // uint32_t
                           stride : Cardinal; // uint32_t
                           format : Integer;  //  int32_t
                           flags  : Cardinal; // uint32_t - 0 for now
                          End;

 PAndroid_Bitmap_Info   = ^TAndroid_Bitmap_Info;

 //----------------------------------------------------------------------------
 // Mouch = Mouse + Touch
 //----------------------------------------------------------------------------
 TMouch                 = Record  // Ref. iOS_Controls.pas TiOSMTouch
                           // Result
                           Active      : Boolean;
                           Pt          : TXYf;
                           Zoom        : Single;
                           Angle       : Single;
                           Start       : Boolean; // Multi Touch start Event
                          End;
 TMouches               = Record
                           // Input
                           Cnt         : Integer;
                           XYs         : Array[0..cMaxTouch-1] of TXYf;
                           // Status Save
                           sLen        : Single;
                           sAngle      : Single;
                           sPt         : TXYf;
                           sPt1        : TXYf;
                           sPt2        : TXYf;
                           sCount      : Integer; // Total Event Count
                           Mouch       : TMouch;  // MultiTouch Result (Pt,Zoom,Angle)
                          End;

 // Java Env Info.
 TEnv                   = Record
                           //
                           AppName     : String;   // [Global] AppName ex. "com.kredix"
                           //
                           jVM         : PJavaVM;  // [Global] App
                           jEnv        : PJNIEnv;  // [Not Global] Env per Thread
                           //
                           jClass      : jClass;   // [Global] Java I/F Controls - Class
                           jControls   : jObject;  // [Global] Java I/F Controls - Object
                           jLayout     : jObject;  // [Global] Java App          - Base Layout
                          End;
 TPaths                 = Record
                           //
                           ALARMS        : String;
                           DCIM          : String;
                           DOCUMENTS     : String;
                           DOWNLOADS     : String;
                           MOVIES        : String;
                           MUSIC         : String;
                           NOTIFICATIONS : String;
                           PICTURES      : String;
                           PODCASTS      : String;
                           RINGTONES     : String;
                           //
                           App           : String;
                           Files         : String;
                           SDCard        : String;
                           DataBase      : String;
                           Shared_Prefs  : String;
                          End;
 // Device Info.
 TDevice                = Record
                           ScreenStyle       : TScreen_Style;
                           ScreenOrientation : TScreen_Orientation;
                           ScreenWH          : TWH;     // ex.
                           //
                           Model             : String;  // ex. Nexus 7
                           Version           : String;  // ex. 5.0.3
                           PhoneNumber       : String;  // ex.
                           ID                : String;  // ex.
                           Network           : TNetwork_Type; // None,Wifi,Mobile
                          End;
 TAnimation             = Record
                           AniIn       : TAnimation_Effect;
                           AniOut      : TAnimation_Effect;
                          end;
 // Form Stack
 TCallBack              = Record
                           Event       : TOnNotify;
                           Sender      : TObject;
                          End;
 TFormCallBack          = Record
                           jForm       : jObject;
                           CloseCB     : TCallBack; // Close Call Back Event
                          End;
 TForms                 = Record
                           Index       : Integer;
                           Stack       : Array[0..cMaxForm-1] of TFormCallBack;
                          end;

 //
 TOnClickEx       = Procedure(Sender: TObject; Value: integer) of object;
 TOnClickYN       = Procedure(Sender: TObject; Click : TClick  ) of object;
 TOnClickItem     = Procedure(Sender: TObject; Item  : Integer ) of object;

 //
 TOnChange        = Procedure(Sender: TObject; EventType : TChangeType) of object;
 TOnTouch         = Procedure(Sender: TObject; ID: integer; X, Y: single) of object;
 TOnTouchEvent    = Procedure(Sender: TObject; Touch : TMouch ) of Object;
 TOnCloseQuery    = Procedure(Sender: TObject; var CanClose: boolean) of object;
 TOnRotate        = Procedure(Sender: TObject; rotate : TScreen_Rotate; Var rstRotate : TScreen_Rotate) of Object;
 TOnActivityRst   = Procedure(Sender: TObject; requestCode : Integer; resultCode : TActivity_Result; jData : jObject) of Object;

 // State Event
 TOnGLViewSize    = Procedure(Sender: TObject; W, H  : integer) of object;
 TOnWebViewState  = Procedure(Sender: TObject; State : TWebView_State  ; URL : String; Var CanNavi : Boolean) of object;
 TOnAsyncTaskState= Procedure(Sender: TObject; State : TAsyncTask_State; Progress : Integer) of object;
 TOnHttpState     = Procedure(Sender: TObject; Act   : THttp_Act; State : THttp_State; Progress : Integer;Const Msg : String) of object;

 // Camera Preview Event
 TOnCameraFrame   = Procedure(Sender: TObject; W,H,Fmt : Integer; Data : Pointer) of Object;

 // Type To String
 Function  strWH               ( WH                : TWH                 ) : String;
 Function  strScreenStyle      ( ScreenStyle       : TScreen_Style       ) : String;
 Function  strScreenOrientation( ScreenOrientation : TScreen_Orientation ) : String;
 Function  strNetworkType      ( NetworkType       : TNetwork_Type       ) : String;
 Function  strEditType         ( EditType          : TEdit_Type          ) : String;

 Var
  gLog : Boolean = True; // Log Write On/Off

implementation

Function  strWH               ( WH                : TWH                 ) : String;
 begin
  Result := IntToStr(WH.Width) + 'x' + IntToStr(WH.Height);
 end;

Function  strScreenStyle      ( ScreenStyle       : TScreen_Style       ) : String;
 begin
  Case ScreenStyle of
   Screen_Style_Normal : Result := 'Normal';
   Screen_Style_Full   : Result := 'Full';
   else                  Result := 'Unknown';
  end;
 end;

Function  strScreenOrientation( ScreenOrientation : TScreen_Orientation ) : String;
 begin
  Case ScreenOrientation of
   Screen_Orientation_UNSPECIFIED       : Result := 'Unspecified';
   Screen_Orientation_LANDSCAPE         : Result := 'LANDSCAPE';
   Screen_Orientation_PORTRAIT          : Result := 'PORTRAIT';
   Screen_Orientation_USER              : Result := 'USER';
   Screen_Orientation_BEHIND            : Result := 'BEHIND';
   Screen_Orientation_SENSOR            : Result := 'SENSOR';
   Screen_Orientation_NOSENSOR          : Result := 'NOSENSOR';
   Screen_Orientation_SENSOR_LANDSCAPE  : Result := 'SENSOR_LANDSCAPE';
   Screen_Orientation_SENSOR_PORTRAIT   : Result := 'SENSOR_PORTRAIT';
   Screen_Orientation_REVERSE_LANDSCAPE : Result := 'REVERSE_LANDSCAPE';
   Screen_Orientation_REVERSE_PORTRAIT  : Result := 'REVERSE_PORTRAIT';
   Screen_Orientation_FULL_SENSOR       : Result := 'FULL_SENSOR';
   Screen_Orientation_USER_LANDSCAPE    : Result := 'USER_LANDSCAPE';
   Screen_Orientation_USER_PORTRAIT     : Result := 'USER_PORTRAIT';
   Screen_Orientation_FULL_USER         : Result := 'FULL_USER';
   Screen_Orientation_LOCKED            : Result := 'LOCKED';
   else                                   Result := 'Unknown';
  end;
 end;

Function  strNetworkType      ( NetworkType : TNetwork_Type ) : String;
 begin
  Case NetworkType of
   Network_Type_None   : Result := 'None';
   Network_Type_Wifi   : Result := 'Wifi';
   Network_Type_Mobile : Result := 'Mobile';
  End;
 end;

Function  strEditType         ( EditType    : TEdit_Type          ) : String;
 begin
  Case EditType of
   Edit_Type_Text       : Result := 'TEXT';
   Edit_Type_Number     : Result := 'NUMBER';
   Edit_Type_Phone      : Result := 'PHONE';
   Edit_Type_PassNumber : Result := 'PASSNUMBER';
   Edit_Type_PassText   : Result := 'PASSTEXT';
   else                   Result := 'Unknown';
  end;
 end;

end.                
