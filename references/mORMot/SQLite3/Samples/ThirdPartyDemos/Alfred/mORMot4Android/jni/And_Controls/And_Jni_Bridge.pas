//
//  Pascal-Java Interface Unit
//
//   main.pas
//    |
//   And_Controls.pas
//    |
//   And_jni_Bridge.pas
//
//  ---------------------------------------------------------------------------
//
//   Authors
//     Simon,Choi / 최원식
//                  simonsayz@naver.com
//                  http://blog.naver.com/simonsayz
//
//     LoadMan    / 장양호
//                  wkddidgh@naver.com
//
//     jmpessoa   / José Marques Pessoa
//                  jmpessoa@hotmail.com
//
//
//   Ref.
//     1. Non-Visual Lib. by Simon,Choi & LoadMan
//           http://blog.naver.com/simonsayz
//
//     2. Visual Lib. by jmpessoa
//           https://github.com/jmpessoa/lazandroidmodulewizard
//
//
//   -> Check
//   http://stackoverflow.com/questions/5859673/should-you-call-releasestringutfchars-if-getstringutfchars-returned-a-copy
//     GetStringUTFChars(Env.jEnv,_jString,@_jBoolean);
//     Env.jEnv^.ReleaseStringUTFChars(Env.
//
unit And_Jni_Bridge;

{$IfDef FPC} {$mode delphi} {$packrecords c} {$EndIf}

interface

uses
  SysUtils,Types,Classes,
  And_jni,And_Controls_Types;

 //----------------------------------------------------------------------------
 // Android Helper Functions
 //----------------------------------------------------------------------------

 // Log
 procedure jLog                                (Const Msg : String; LogType : TAndroid_Log = Android_Log_DEBUG);

 // Base
 Function  jBool                               ( Bool : Boolean  ) : Integer;
 Function  jGetMethodID                        (Const Env : TEnv; FuncName, FuncSig : PChar; Var Method_ :jMethodID) : Boolean;
 Function  jGetStrLength                       (Const Env : TEnv; Str : String): Integer;
 Function  jGetStrDateTime                     (Const Env : TEnv): String;

 // System
 Procedure jSystem_GC                          (Const Env : TEnv);
 Function  jSystem_GetTick                     (Const Env : TEnv) : LongInt;
 Function  jSystem_GetPath                     (Const Env : TEnv; DirectoryType : TDirectory_Type) : String;

 // Device Info
 Function  jDevice_GetScreenStyle              (Const Env : TEnv ) : TScreen_Style;
 Procedure jDevice_SetScreenStyle              (Const Env : TEnv; ScreenStyle : TScreen_Style);
 Function  jDevice_GetScreenOrientation        (Const Env : TEnv ) : TScreen_Orientation;
 Procedure jDevice_SetScreenOrientation        (Const Env : TEnv; Orientation : TScreen_Orientation);
 Function  jDevice_GetScreenWH                 (Const Env : TEnv) : TWH;
 //
 Function  jDevice_GetModel                    (Const Env : TEnv) : String;
 Function  jDevice_GetVersion                  (Const Env : TEnv) : String;
 Function  jDevice_GetPhoneNumber              (Const Env : TEnv) : String;
 Function  jDevice_GetID                       (Const Env : TEnv) : String;
 Function  jDevice_GetNetwork                  (Const Env : TEnv) : TNetwork_Type;

 // Class
 Procedure jClass_setNull                      (Const Env : TEnv; ClassObj : jClass);
 Function  jClass_chkNull                      (Const Env : TEnv; ClassObj : jClass) : Boolean;

 // App - Activity
 Procedure jApp_Finish                         (Const Env : TEnv);
 Procedure jApp_KillProcess                    (Const Env : TEnv);
 Procedure jApp_SetTitleBar                    (Const Env : TEnv; visible:boolean);

 // Asset
 Function  jAsset_SaveToFile                   (Const Env : TEnv; Asset,FileName :String) : Boolean;

 // AndroidBitmap
 Function  jBitmap_getInfo                     (Const Env : TEnv; jbitmap: jobject; info: PAndroid_Bitmap_Info ) : cInt;
 Function  jBitmap_lockPixels                  (Const Env : TEnv; jbitmap: jobject; addrPtr : PPointer) : cInt;
 Function  jBitmap_unlockPixels                (Const Env : TEnv; jbitmap: jobject) : cInt;

 // Image
 Function  jImage_getWH                        (Const Env : TEnv; filename : String) : TWH;
 Function  jImage_resample                     (Const Env : TEnv; filename : String; size : integer ) : jObject;
 Procedure jImage_save                         (Const Env : TEnv; Bitmap : jObject; filename : String);

 //----------------------------------------------------------------------------
 // Java Functions
 //----------------------------------------------------------------------------

 // Form
 Function  jForm_Create                        (Const Env : TEnv; pasObj   : TObject) : jObject;
 Procedure jForm_Free                          (Const Env : TEnv; Form     : jObject);
 Procedure jForm_Show                          (Const Env : TEnv; Form     : jObject; Effect : TAnimation_Effect = Animation_Effect_None);
 Procedure jForm_Close                         (Const Env : TEnv; Form     : jObject; Effect : TAnimation_Effect = Animation_Effect_None);

 Function  jForm_GetLayoutG                    (Const Env : TEnv; Form     : jObject) : jObject;
 Procedure jForm_SetVisibility                 (Const Env : TEnv; Form     : jObject; visible : Boolean);
 Procedure jForm_SetTitle                      (Const Env : TEnv; Form     : jObject; visible : Boolean);
 Procedure jForm_SetEnabled                    (Const Env : TEnv; Form     : jObject; enabled : Boolean);

 // TextView
 Function  jTextView_Create                    (Const Env : TEnv; pasObj   : TObject) : jObject;
 Procedure jTextView_Free                      (Const Env : TEnv; TextView : jObject );
 Procedure jTextView_setParent                 (Const Env : TEnv; TextView : jObject; ViewGroup : jObject);
 Procedure jTextView_setEnabled                (Const Env : TEnv; TextView : jObject; Enabled   : Boolean);

 Procedure jTextView_setXYWH                   (Const Env : TEnv; TextView : jObject;x,y,w,h : integer);
 Function  jTextView_getText                   (Const Env : TEnv; TextView : jObject) : String;
 Procedure jTextView_setText                   (Const Env : TEnv; TextView : jObject; Str : String);
 Procedure jTextView_setTextColor              (Const Env : TEnv; TextView : jObject; color : TColor);
 Procedure jTextView_setTextSize               (Const Env : TEnv; TextView : jObject; size  : DWord);
 Procedure jTextView_setTextAlignment          (Const Env : TEnv; TextView : jObject; align : TText_Alignment);

 // EditText
 Function  jEditText_Create                    (Const Env : TEnv; pasObj   : TObject) : jObject;
 Procedure jEditText_Free                      (Const Env : TEnv; EditText : jObject);
 Procedure jEditText_setParent                 (Const Env : TEnv; EditText : jObject; ViewGroup : jObject);
 Procedure jEditText_setEnabled                (Const Env : TEnv; EditText : jObject; enabled   : Boolean);
 
 Procedure jEditText_setXYWH                   (Const Env : TEnv; EditText : jObject;x,y,w,h : integer);
 Function  jEditText_getText                   (Const Env : TEnv; EditText : jObject) : String;
 Procedure jEditText_setText                   (Const Env : TEnv; EditText : jObject; Str : String);
 Procedure jEditText_setTextColor              (Const Env : TEnv; EditText : jObject; color : DWord);
 Procedure jEditText_setTextSize               (Const Env : TEnv; EditText : jObject; size  : DWord);
 Procedure jEditText_setHint                   (Const Env : TEnv; EditText : jObject; Str : String);
 Procedure jEditText_SetFocus                  (Const Env : TEnv; EditText : jObject);
 Procedure jEditText_immShow                   (Const Env : TEnv; EditText : jObject);
 Procedure jEditText_immHide                   (Const Env : TEnv; EditText : jObject);
 Procedure jEditText_setEditStyle              (Const Env : TEnv; EditText : jObject; editStyle : TEdit_Style);
 Procedure jEditText_setEditType               (Const Env : TEnv; EditText : jObject; editType : TEdit_Type);
 Procedure jEditText_maxLength                 (Const Env : TEnv; EditText : jObject; size  : DWord);
 Procedure jEditText_getCursorPos              (Const Env : TEnv; EditText : jObject; Var x,y : Integer);
 Procedure jEditText_setCursorPos              (Const Env : TEnv; EditText : jObject; startPos,endPos : Integer);
 Procedure jEditText_setTextAlignment          (Const Env : TEnv; EditText : jObject; align : DWord);
 Procedure jEditText_setEditable               (Const Env : TEnv; EditText : jObject; enabled : Boolean);

 // Button
 Function  jButton_Create                      (Const Env : TEnv; pasObj : TObject ) : jObject;
 Procedure jButton_Free                        (Const Env : TEnv; Button : jObject);
 Procedure jButton_setParent                   (Const Env : TEnv; Button : jObject;ViewGroup : jObject);

 Procedure jButton_setXYWH                     (Const Env : TEnv; Button : jObject;x,y,w,h : integer);
 Function  jButton_getText                     (Const Env : TEnv; Button : jObject) : String;
 Procedure jButton_setText                     (Const Env : TEnv; Button : jObject; Str : String);
 Procedure jButton_setTextColor                (Const Env : TEnv; Button : jObject; color : DWord);
 Procedure jButton_setTextSize                 (Const Env : TEnv; Button : jObject; size  : DWord);
 Procedure jButton_setOnClick                  (Const Env : TEnv; Button : jObject);

 // CheckBox
 Function  jCheckBox_Create                    (Const Env : TEnv; pasObj : TObject ) : jObject;
 Procedure jCheckBox_Free                      (Const Env : TEnv; CheckBox : jObject);
 Procedure jCheckBox_setParent                 (Const Env : TEnv; CheckBox : jObject;ViewGroup : jObject);

 Procedure jCheckBox_setXYWH                   (Const Env : TEnv; CheckBox : jObject;x,y,w,h : integer);
 Function  jCheckBox_getText                   (Const Env : TEnv; CheckBox : jObject) : String;
 Procedure jCheckBox_setText                   (Const Env : TEnv; CheckBox : jObject; Str : String);
 Procedure jCheckBox_setTextColor              (Const Env : TEnv; CheckBox : jObject; color : DWord);
 Procedure jCheckBox_setTextSize               (Const Env : TEnv; CheckBox : jObject; size  : DWord);
 Function  jCheckBox_isChecked                 (Const Env : TEnv; CheckBox : jObject) : Boolean;
 Procedure jCheckBox_setChecked                (Const Env : TEnv; CheckBox : jObject; value : Boolean);

 // RadioButton
 Function  jRadioButton_Create                 (Const Env : TEnv; pasObj : TObject ) : jObject;
 Procedure jRadioButton_Free                   (Const Env : TEnv; RadioButton : jObject);
 Procedure jRadioButton_setParent              (Const Env : TEnv; RadioButton : jObject;ViewGroup : jObject);

 Procedure jRadioButton_setXYWH                (Const Env : TEnv; RadioButton : jObject;x,y,w,h : integer);
 Function  jRadioButton_getText                (Const Env : TEnv; RadioButton : jObject) : String;
 Procedure jRadioButton_setText                (Const Env : TEnv; RadioButton : jObject; Str : String);
 Procedure jRadioButton_setTextColor           (Const Env : TEnv; RadioButton : jObject; color : DWord);
 Procedure jRadioButton_setTextSize            (Const Env : TEnv; RadioButton : jObject; size  : DWord);
 Function  jRadioButton_isChecked              (Const Env : TEnv; RadioButton : jObject) : Boolean;
 Procedure jRadioButton_setChecked             (Const Env : TEnv; RadioButton : jObject; value : Boolean);

 // ProgressBar
 Function  jProgressBar_Create                 (Const Env : TEnv; pasObj : TObject; Style : DWord ) : jObject;
 Procedure jProgressBar_Free                   (Const Env : TEnv; ProgressBar : jObject);
 Procedure jProgressBar_setParent              (Const Env : TEnv; ProgressBar : jObject;ViewGroup : jObject);

 Procedure jProgressBar_setXYWH                (Const Env : TEnv; ProgressBar : jObject;x,y,w,h : integer);
 Function  jProgressBar_getProgress            (Const Env : TEnv; ProgressBar : jObject) : Integer;
 Procedure jProgressBar_setProgress            (Const Env : TEnv; ProgressBar : jObject; value : integer);

 // ImageView
 Function  jImageView_Create                   (Const Env : TEnv; pasObj : TObject) : jObject;
 Procedure jImageView_Free                     (Const Env : TEnv; ImageView : jObject);
 Procedure jImageView_setParent                (Const Env : TEnv; ImageView : jObject;ViewGroup : jObject);

 Procedure jImageView_setXYWH                  (Const Env : TEnv; ImageView : jObject;x,y,w,h : integer);
 Procedure jImageView_setImage                 (Const Env : TEnv; ImageView : jObject; Str : String);

 // ListView
 Function  jListView_Create                    (Const Env : TEnv; pasObj : TObject) : jObject;
 Procedure jListView_Free                      (Const Env : TEnv; ListView : jObject);
 Procedure jListView_setParent                 (Const Env : TEnv; ListView : jObject;ViewGroup : jObject);

 Procedure jListView_setXYWH                   (Const Env : TEnv; ListView : jObject;x,y,w,h : integer);
 Procedure jListView_setTextColor              (Const Env : TEnv; ListView : jObject; color : DWord);
 Procedure jListView_setTextSize               (Const Env : TEnv; ListView : jObject; size  : DWord);
 Procedure jListView_setItemPosition           (Const Env : TEnv; ListView : jObject; Pos: integer; y:Integer );

 Procedure jListView_add                       (Const Env : TEnv; ListView : jObject; Str : String);
 Procedure jListView_clear                     (Const Env : TEnv; ListView : jObject);
 Procedure jListView_delete                    (Const Env : TEnv; ListView : jObject; index : integer);

 // ScrollView
 Function  jScrollView_Create                  (Const Env : TEnv; pasObj : TObject) : jObject;
 Procedure jScrollView_Free                    (Const Env : TEnv; ScrollView : jObject);
 Procedure jScrollView_setParent               (Const Env : TEnv; ScrollView : jObject;ViewGroup : jObject);

 Procedure jScrollView_setXYWH                 (Const Env : TEnv; ScrollView : jObject;x,y,w,h : integer);
 Procedure jScrollView_setScrollSize           (Const Env : TEnv; ScrollView : jObject; size : integer);
 Function  jScrollView_getView                 (Const Env : TEnv; ScrollView : jObject) : jObject;

 // HorizontalScrollView
 Function  jHorizontalScrollView_Create        (Const Env : TEnv; pasObj : TObject) : jObject;
 Procedure jHorizontalScrollView_Free          (Const Env : TEnv; ScrollView : jObject);
 Procedure jHorizontalScrollView_setParent     (Const Env : TEnv; ScrollView : jObject;ViewGroup : jObject);

 Procedure jHorizontalScrollView_setXYWH       (Const Env : TEnv; ScrollView : jObject;x,y,w,h : integer);
 Procedure jHorizontalScrollView_setScrollSize (Const Env : TEnv; ScrollView : jObject; size : integer);
 Function  jHorizontalScrollView_getView       (Const Env : TEnv; ScrollView : jObject) : jObject;

 // ViewFlipper
 Function  jViewFlipper_Create                 (Const Env : TEnv; pasObj : TObject ) : jObject;
 Procedure jViewFlipper_Free                   (Const Env : TEnv; ViewFlipper : jObject);
 //
 Procedure jViewFlipper_setXYWH                (Const Env : TEnv; ViewFlipper : jObject;x,y,w,h : integer);
 Procedure jViewFlipper_setParent              (Const Env : TEnv; ViewFlipper : jObject;ViewGroup : jObject);

 // WebView
 Function  jWebView_Create                     (Const Env : TEnv;  pasObj : TObject) : jObject;
 Procedure jWebView_Free                       (Const Env : TEnv; WebView : jObject);
 Procedure jWebView_setParent                  (Const Env : TEnv; WebView : jObject;ViewGroup : jObject);
 //
 Procedure jWebView_setXYWH                    (Const Env : TEnv; WebView : jObject;x,y,w,h : integer);
 Procedure jWebView_setJavaScript              (Const Env : TEnv; WebView : jObject; javascript : boolean);
 Procedure jWebView_loadURL                    (Const Env : TEnv; WebView : jObject; Str : String);

 // Bitmap
 Function  jBitmap_Create                      (Const Env : TEnv; pasObj : TObject) : jObject;
 Procedure jBitmap_Free                        (Const Env : TEnv; jbitmap : jObject);
 Procedure jBitmap_loadFile                    (Const Env : TEnv; jbitmap : jObject; filename : String);
 Procedure jBitmap_createBitmap                (Const Env : TEnv; jbitmap : jObject; w,h : integer);
 Procedure jBitmap_getWH                       (Const Env : TEnv; jbitmap : jObject; var w,h : integer);
 Function  jBitmap_getJavaBitmap               (Const Env : TEnv; jbitmap : jObject) : jObject;

 // Canvas
 Function  jCanvas_Create                      (Const Env : TEnv;  pasObj : TObject) : jObject;
 Procedure jCanvas_Free                        (Const Env : TEnv; jCanvas : jObject);

 Procedure jCanvas_setStrokeWidth              (Const Env : TEnv; jCanvas : jObject;width : single);
 Procedure jCanvas_setStyle                    (Const Env : TEnv; jCanvas : jObject; style : integer);
 Procedure jCanvas_setColor                    (Const Env : TEnv; jCanvas : jObject; color : DWord  );
 Procedure jCanvas_setTextSize                 (Const Env : TEnv; jCanvas : jObject; textsize : single);
 Procedure jCanvas_drawLine                    (Const Env : TEnv; jCanvas : jObject; x1,y1,x2,y2 : single);
 Procedure jCanvas_drawPoint                   (Const Env : TEnv; jCanvas:jObject; x1,y1:single);
 Procedure jCanvas_drawText                    (Const Env : TEnv; jCanvas : jObject; const text : string; x,y : single);
 Procedure jCanvas_drawBitmap                  (Const Env : TEnv; jCanvas : jObject; bmp : jObject; b,l,r,t : integer);

 // View
 Function  jView_Create                        (Const Env : TEnv; pasObj : TObject) : jObject;
 Procedure jView_Free                          (Const Env : TEnv; View : jObject);
 Procedure jView_setParent                     (Const Env : TEnv; View : jObject; ViewGroup : jObject);

 Procedure jView_setXYWH                       (Const Env : TEnv; View : jObject;x,y,w,h : integer);
 Procedure jView_setjCanvas                    (Const Env : TEnv; View : jObject; jCanvas   : jObject);
 Procedure jView_SaveToFile                    (Const Env : TEnv; View : jObject; Filename  : String );

 Procedure jView_SetVisible                    (Const Env : TEnv; view : jObject; visible : Boolean);
 Procedure jView_SetBackGroundColor            (Const Env : TEnv; view : jObject; color   : DWord  );
 Procedure jView_Invalidate                    (Const Env : TEnv; view : jObject);

 // GLSurfaceView
 Function  jGLSurfaceView_Create               (Const Env : TEnv; pasObj : TObject; version : TGLES_Version) : jObject;
 Procedure jGLSurfaceView_Free                 (Const Env : TEnv; GLSurfaceView : jObject);
 Procedure jGLSurfaceView_setParent            (Const Env : TEnv; GLSurfaceView: jObject;ViewGroup : jObject);

 Procedure jGLSurfaceView_setXYWH              (Const Env : TEnv; GLSurfaceView: jObject;x,y,w,h : integer);
 Procedure jGLSurfaceView_SetAutoRefresh       (Const Env : TEnv; glView : jObject; Active : Boolean);
 Procedure jGLSurfaceView_Refresh              (Const Env : TEnv; glView : jObject);
 Procedure jGLSurfaceView_deleteTexture        (Const Env : TEnv; glView : jObject; id : Integer);
 Procedure jGLSurfaceView_getBmpArray          (Const Env : TEnv; filename : String);
 Procedure jGLSurfaceView_requestGLThread      (Const Env : TEnv; glView : jObject);

 // CameraView
 Function  jCameraView_Create                  (Const Env : TEnv;      pasObj : TObject) : jObject;
 Procedure jCameraView_Free                    (Const Env : TEnv; SurfaceView : jObject);

 Procedure jCameraView_setXYWH                 (Const Env : TEnv; SurfaceView: jObject;x,y,w,h : integer);
 Procedure jCameraView_setParent               (Const Env : TEnv; SurfaceView: jObject;ViewGroup : jObject);
 Procedure jCameraView_saveImage               (Const Env : TEnv; SurfaceView: jObject;fileName : String);

 // Timer
 Function  jTimer_Create                       (Const Env : TEnv;  pasObj : TObject): jObject;
 Procedure jTimer_Free                         (Const Env : TEnv;   Timer : jObject);
 Procedure jTimer_SetInterval                  (Const Env : TEnv;   Timer : jObject; Interval : Integer);
 Procedure jTimer_SetEnabled                   (Const Env : TEnv;   Timer : jObject; Active   : Boolean);

 // Dialog YN
 Function  jDialogYN_Create                    (Const Env : TEnv;   pasObj : TObject; title,msg,y,n : string ): jObject;
 Procedure jDialogYN_Free                      (Const Env : TEnv; DialogYN : jObject);
 Procedure jDialogYN_Show                      (Const Env : TEnv; DialogYN : jObject);

 // Dialog Progress
 Function  jDialogProgress_Create              (Const Env : TEnv;         pasObj : TObject; title,msg : string ): jObject;
 Procedure jDialogProgress_Free                (Const Env : TEnv; DialogProgress : jObject);

 // Toast
 Procedure jToast                              (Const Env : TEnv; Str : String);

 // ImageBtn
 Function  jImageBtn_Create                    (Const Env : TEnv;   pasObj : TObject) : jObject;
 Procedure jImageBtn_Free                      (Const Env : TEnv; ImageBtn : jObject);
 Procedure jImageBtn_setXYWH                   (Const Env : TEnv; ImageBtn : jObject;x,y,w,h : integer);
 Procedure jImageBtn_setParent                 (Const Env : TEnv; ImageBtn : jObject;ViewGroup : jObject);
 Procedure jImageBtn_setButton                 (Const Env : TEnv; ImageBtn : jObject;up,dn : String);
 Procedure jImageBtn_SetEnabled                (Const Env : TEnv; ImageBtn : jObject; Active : Boolean);

 // AsyncTask / Thread
 Function  jAsyncTask_Create                   (Const Env : TEnv;    pasObj : TObject) : jObject;
 Procedure jAsyncTask_Free                     (Const Env : TEnv; AsyncTask : jObject);
 Procedure jAsyncTask_Execute                  (Const Env : TEnv; AsyncTask : jObject);
 Procedure jAsyncTask_setProgress              (Const Env : TEnv; AsyncTask : jObject; Progress : Integer);

 // Http / Thread
 Function  jHttp_Create                        (Const Env : TEnv;    pasObj : TObject) : jObject;
 Procedure jHttp_Free                          (Const Env : TEnv;      Http : jObject);
 Procedure jHttp_getText                       (Const Env : TEnv;      Http : jObject; url : String);
 Procedure jHttp_DownloadFile                  (Const Env : TEnv;      Http : jObject; url : String; localFile : String);
 Procedure jHttp_UploadFile                    (Const Env : TEnv;      Http : jObject; url : String; localFile : String);

 // Camera Activity
 Procedure jTakePhoto                          (Const Env : TEnv; FileName : String);

 // BenchMark
 Procedure jBenchMark_Java                     (Const Env : TEnv;var mSec : Integer;var value : single);
 Procedure jBenchMark_Pascal                   (Const Env : TEnv;var mSec : Integer;var value : single);

implementation

Const
 NDKLibLog      = 'liblog.so';
 libjnigraphics = 'libjnigraphics.so';

// log
Function __android_log_write(prio:longint;tag,text:pAnsiChar):longint; cdecl; external NDKLibLog;

// jnigraphics
Function AndroidBitmap_getInfo      (env: PJNIEnv; jbitmap: jobject; info: PAndroid_Bitmap_Info): cint; cdecl; external libjnigraphics;
Function AndroidBitmap_lockPixels   (env: PJNIEnv; jbitmap: jobject; addrPtr: PPointer): cint; cdecl; external libjnigraphics;
Function AndroidBitmap_unlockPixels (env: PJNIEnv; jbitmap: jobject): cint; cdecl; external libjnigraphics;

//------------------------------------------------------------------------------
// Android Helper Functions
//------------------------------------------------------------------------------
Type
 {$IfDef DCC}
 pAnsiChar  = MarshaledAString;
 {$EndIf}

 StrToPAnsi = Record
               Bytes: array of Byte;
               Function Str(const Value: string) : PAnsiChar;
              end;

Function StrToPAnsi.Str(const Value: string) : PAnsiChar;
 Var
  i : Integer;
begin
 SetLength( Bytes, Length(Value) + 1 );
 for i := 0 to Length(Value) - 1 do
  begin
   Bytes[i] := Ord(Value[i]) and 255;
  end;
 Bytes[Length(Value)] := 0;
 //
 Result := @Bytes[0];
end;

// Delphi Only
Function UTFToStr( UTF : PChar ) : String;
 Var
  Ptr  : PByte;
  PtrS : PByte;
  i,j  : Integer;
 begin
  {$IfDef FPC}
   Result := String(UTF);
   Exit;
  {$EndIf}
  //
  Result := '';
  Ptr    := pByte(UTF);
  PtrS   := Ptr;
  //
  i    := 0;
  if Ptr^ = 0 then
   begin
    Result := '';
    Exit;
   end;
  //
  While ( Ptr^ <> 0 ) do
   begin
    Inc(i);
    Inc(Ptr);
   end;
  //
  Ptr := PtrS;
  for j := 1 to i do
   begin
    Result := Result + Char(Ptr^);
    Inc(Ptr);
   end;
 end;

// Log
procedure jLog(Const Msg : String; LogType : TAndroid_Log = Android_Log_DEBUG);
 Var
  S : StrToPAnsi;
 begin
  if gLog then
   __android_log_write(Integer(LogType),'AndCtrls_Pas',
                       {$IfDef FPC} PChar(Msg)            {$EndIf}
                       {$IfDef Dcc} PAnsiChar(S.Str(Msg)) {$EndIf} );
 end;

//
Function jBool ( Bool : Boolean ) : Integer;
 begin
  Case Bool of
   True  : Result := 1;
   False : Result := 0;
  End;
 end;

//
Function jGetMethodID(Const Env : TEnv;
                      FuncName, FuncSig : PChar; Var Method_ :jMethodID) : Boolean;
 begin
  //
  Result := True;
  {$IFDef FPC} If Method_ <> nil then Exit; {$EndIF}
  Method_ := Env.jEnv^.GetMethodID( Env.jEnv, Env.jClass, FuncName, FuncSig);
  Result := Method_ <> nil;
  If Not(Result) then
   jLog('Err MethodID : ' + String(FuncName) );
 end;

// LORDMAN - 2013-07-28 / String.length()
Function  jGetStrLength (Const Env : TEnv; Str : String): Integer;
 Const
  _cFuncName = 'getStrLength';
  _cFuncSig  = '(Ljava/lang/String;)I';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams.l := Env.jEnv^.NewStringUTF(Env.jEnv, pchar(Str) );
  Result     := Env.jEnv^.CallIntMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
  Env.jEnv^.DeleteLocalRef (Env.jEnv,_jParams.l);
 end;

// by LORDMAN - 2013-07-28 / yyyy-MM-dd HH:mm:ss
// by jmpessoa  Locale.getDefault()
Function  jGetStrDateTime (Const Env : TEnv): String;
 Const
  _cFuncName = 'getStrDateTime';
  _cFuncSig  = '()Ljava/lang/String;';
 Var
  _jMethod  : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jString  : jString;
  _jBoolean : jBoolean;
  _jStrUTF  : PChar;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jString  := Env.jEnv^.CallObjectMethod(Env.jEnv, Env.jControls,_jMethod);
  Case _jString = nil of
   True : Result    := '';
   False: begin
           _jBoolean := JNI_False;
           _jStrUTF  := Env.jEnv^.GetStringUTFChars(Env.jEnv,_jString,@_jBoolean);
           Result    := String( _jStrUTF );
           Env.jEnv^.ReleaseStringUTFChars(Env.jEnv,_jString,_jStrUTF);
          end;
  end;
 end;

//-----------------------------------------------------------------------------
// System
//-----------------------------------------------------------------------------

// Garbage Collection
Procedure jSystem_GC      (Const Env : TEnv);
 Const
  _cFuncName = 'systemGC';
  _cFuncSig  = '()V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  Env.jEnv^.CallVoidMethod(Env.jEnv, Env.jControls,_jMethod);
 end;

// LORDMAN - 2013-07-28  / System.currentTimeMills()
Function  jSystem_GetTick (Const Env : TEnv) : LongInt;
 Const
  _cFuncName = 'getTick';
  _cFuncSig  = '()J';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  Result     := Env.jEnv^.CallLongMethod(Env.jEnv, Env.jControls,_jMethod);
 end;

//
Function  jSystem_GetPath (Const Env : TEnv; DirectoryType : TDirectory_Type) : String;
 Const
  _cFuncName = 'getPath';
  _cFuncSig  = '(ILjava/lang/String;)Ljava/lang/String;';
 Var
  AppName  : String;
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : Array[0..1] of jValue;
  _jString : jString;
  _jBoolean: jBoolean;
  _jStrUTF : PChar;
 begin
  //
  AppName := Env.AppName;
  //
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].i := LongInt(DirectoryType);
  _jParams[1].l := Env.jEnv^.NewStringUTF(Env.jEnv, pchar(Env.AppName) );
  _jString      := Env.jEnv^.CallObjectMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
  Env.jEnv^.DeleteLocalRef(Env.jEnv,_jParams[1].l);
  Case _jString = nil of
   True : Result    := '';
   False: begin
           _jBoolean := JNI_False;
           _jStrUTF  := Env.jEnv^.GetStringUTFChars(Env.jEnv,_jString,@_jBoolean);
           Result    := UTFToStr (_jStrUTF);
           Env.jEnv^.ReleaseStringUTFChars(Env.jEnv,_jString,_jStrUTF);
          end;
  end;
  jLog('System_Path:'+ Result);
 end;


//-----------------------------------------------------------------------------
// Device
//-----------------------------------------------------------------------------

Function jDevice_GetScreenStyle  (Const Env : TEnv ) : TScreen_Style;
 Const
  _cFuncName = 'systemGetScreenStyle';
  _cFuncSig  = '()I';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParam  : jValue;
  Rst      : LongInt;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  Rst := Env.jEnv^.CallIntMethod(Env.jEnv, Env.jControls,_jMethod);
  Result := TScreen_Style(Rst);
 end;

Procedure jDevice_SetScreenStyle (Const Env : TEnv; ScreenStyle: TScreen_Style);
 Const
  _cFuncName = 'systemSetScreenStyle';
  _cFuncSig  = '(I)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParam  : jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParam.i := LongInt(ScreenStyle);
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParam);
 end;

Function jDevice_GetScreenOrientation (Const Env : TEnv ) : TScreen_Orientation;
 Const
  _cFuncName = 'systemGetScreenOrientation';
  _cFuncSig  = '()I';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParam  : jValue;
  Rst      : LongInt;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  Rst := Env.jEnv^.CallIntMethod(Env.jEnv, Env.jControls,_jMethod);
  Result := TScreen_Orientation(Rst);
 end;

Procedure jDevice_SetScreenOrientation (Const Env : TEnv; orientation : TScreen_Orientation);
 Const
  _cFuncName = 'systemSetScreenOrientation';
  _cFuncSig  = '(I)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParam  : jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParam.i := LongInt(orientation);
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParam);
 end;

// Get Device Screen Resolution
Function  jDevice_GetScreenWH (Const Env : TEnv ) : TWH;
 Const
  _cFuncName = 'getScreenWH';
  _cFuncSig  = '(Landroid/content/Context;)I';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParam  : jValue;
  _wh      : Integer;
 begin
  //
  Result.Height := 0;
  Result.Width  := 0;
  //
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParam.l     := 0;// context;
  _wh           := Env.jEnv^.CallIntMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParam);
  Result.Width  := (_wh shr 16);
  Result.Height := (_wh and $0000FFFF);
  jLog('Screen : ' + IntToStr(Result.Width) + 'x' + IntTostr(Result.Height));
 end;

Function  jDevice_GetModel (Const Env : TEnv) : String;
 Const
  _cFuncName = 'getDevModel';
  _cFuncSig  = '()Ljava/lang/String;';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jString : jString;
  _jBoolean: jBoolean;
  _jStrUTF : PChar;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jString  := Env.jEnv^.CallObjectMethod(Env.jEnv, Env.jControls,_jMethod);
  Case _jString = nil of
  True : Result    := '';
  False: begin
          _jBoolean := JNI_False;
          _jStrUTF  := Env.jEnv^.GetStringUTFChars(Env.jEnv,_jString,@_jBoolean);
          Result    := UTFToStr( _jStrUTF );
          Env.jEnv^.ReleaseStringUTFChars(Env.jEnv,_jString,_jStrUTF);
         end;
 end;
 jLog('Model:'+ Result);
end;

Function  jDevice_GetVersion (Const Env : TEnv) : String;
 Const
  _cFuncName = 'getDevVersion';
  _cFuncSig  = '()Ljava/lang/String;';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jString : jString;
  _jBoolean: jBoolean;
  _jStrUTF : PChar;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jString  := Env.jEnv^.CallObjectMethod(Env.jEnv, Env.jControls,_jMethod);
  Case _jString = nil of
  True : Result    := '';
  False: begin
          _jBoolean := JNI_False;
          _jStrUTF  := Env.jEnv^.GetStringUTFChars(Env.jEnv,_jString,@_jBoolean);
          Result    := UTFtoStr( _jStrUTF );
          Env.jEnv^.ReleaseStringUTFChars(Env.jEnv,_jString,_jStrUTF);
         end;
 end;
 jLog('Version:'+ Result);
end;

Function  jDevice_GetPhoneNumber(Const Env : TEnv) : String;
 Const
  _cFuncName = 'getDevPhoneNumber';
  _cFuncSig  = '()Ljava/lang/String;';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jString : jString;
  _jBoolean: jBoolean;
  _jStrUTF : PChar;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jString  := Env.jEnv^.CallObjectMethod(Env.jEnv, Env.jControls,_jMethod);
  Case _jString = nil of
   True : Result    := '';
   False: begin
           _jBoolean := JNI_False;
           _jStrUTF  := Env.jEnv^.GetStringUTFChars(Env.jEnv,_jString,@_jBoolean);
           Result    := String( _jStrUTF );
           Env.jEnv^.ReleaseStringUTFChars(Env.jEnv,_jString,_jStrUTF);
          end;
  end;
  jLog('PhoneNumber:'+ Result);
 end;

Function  jDevice_GetID(Const Env : TEnv) : String;
 Const
  _cFuncName = 'getDevDeviceID';
  _cFuncSig  = '()Ljava/lang/String;';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jString : jString;
  _jBoolean: jBoolean;
  _jStrUTF : PChar;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jString  := Env.jEnv^.CallObjectMethod(Env.jEnv, Env.jControls,_jMethod);
  Case _jString = nil of
   True : Result    := '';
   False: begin
           _jBoolean := JNI_False;
           _jStrUTF  := Env.jEnv^.GetStringUTFChars(Env.jEnv,_jString,@_jBoolean);
           Result    := String( _jStrUTF );
           Env.jEnv^.ReleaseStringUTFChars(Env.jEnv,_jString,_jStrUTF);
          end;
  end;
  jLog('DeviceID:'+ Result);
 end;

Function  jDevice_GetNetwork(Const Env : TEnv) : TNetwork_Type;
 Const
  _cFuncName = 'getDevNetwork';
  _cFuncSig  = '()I';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  Rst      : Integer;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  Rst := Env.jEnv^.CallIntMethod(Env.jEnv, Env.jControls,_jMethod);
  Result := TNetwork_Type(Rst);
  Case Result of
   Network_Type_None   : jLog('Network:None'  );
   Network_Type_Wifi   : jLog('Network:Wifi'  );
   Network_Type_Mobile : jLog('Network:Mobile');
  end;
 end;

Procedure jClass_setNull (Const Env : TEnv; ClassObj : jClass);
 Const
  _cFuncName = 'classSetNull';
  _cFuncSig  = '(Ljava/lang/Class;)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParam  : jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParam.l := ClassObj;
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParam);
 end;

//
Function  jClass_chkNull (Const Env : TEnv; ClassObj : jClass) : Boolean;
 Const
  _cFuncName = 'classChkNull';
  _cFuncSig  = '(Ljava/lang/Class;)Z';
 Var
  _jMethod  : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParam   : jValue;
  _jBoolean : jBoolean;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParam.l := ClassObj;
  _jBoolean := Env.jEnv^.CallBooleanMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParam);
  Result    := Boolean(_jBoolean);
 end;

// App - Activity

// fix by jmpessoa -> Java Code
Procedure jApp_Finish (Const Env : TEnv);
Const
 _cFuncName = 'appFinish';
 _cFuncSig  = '()V';
Var
 _jMethod   : jMethodID {$IFDef FPC} = nil {$EndIf};
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 Env.jEnv^.CallVoidMethod(Env.jEnv, Env.jControls,_jMethod);
end;

// fix by jmpessoa -> Java Code
Procedure jApp_KillProcess(Const Env : TEnv);
Const
 _cFuncName = 'appKillProcess';
 _cFuncSig  = '()V';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 Env.jEnv^.CallVoidMethod(Env.jEnv, Env.jControls,_jMethod);
end;


Procedure jApp_SetTitleBar(Const Env : TEnv; visible:boolean);
Const
 _cFuncName = 'appSetTitleBar';
 _cFuncSig  = '(Z)V';
Var
 _jMethod : jMethodID = nil;
 _jParam : jValue;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParam.z := Byte(visible);
 Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParam);
end;


// Asset
//  src     'test.txt'
//  outFile '/data/data/com/kredix/files/test.txt'
//            App.Path.Dat+'/image01.png'
Function  jAsset_SaveToFile( Const Env : TEnv; Asset,FileName :String ) : Boolean;
 Const
  _cFuncName = 'assetSaveToFile';
  _cFuncSig  = '(Ljava/lang/String;Ljava/lang/String;)Z';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : Array[0..1] of jValue;
  _jString : jString;
  _jBoolean: jBoolean;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := Env.jEnv^.NewStringUTF(Env.jEnv, pchar(Asset   ) );
  _jParams[1].l := Env.jEnv^.NewStringUTF(Env.jEnv, pchar(FileName) );
  _jBoolean  := Env.jEnv^.CallBooleanMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
  Result     := Boolean(_jBoolean);
  Env.jEnv^.DeleteLocalRef(Env.jEnv,_jParams[0].l);
  Env.jEnv^.DeleteLocalRef(Env.jEnv,_jParams[1].l);
 end;

//
Function jBitmap_getInfo     ( Const Env : TEnv; jbitmap: jobject; info: PAndroid_Bitmap_Info ) : cInt;
 begin
  Result := AndroidBitmap_getInfo    (Env.jEnv,jbitmap,info);
 end;

//
Function jBitmap_lockPixels  ( Const Env : TEnv; jbitmap: jobject; addrPtr : PPointer) : cInt;
 begin
  Result := AndroidBitmap_lockPixels (Env.jEnv,jbitmap,addrPtr);
 end;

//
Function jBitmap_unlockPixels( Const Env : TEnv; jbitmap: jobject) : cInt;
 begin
  Result := AndroidBitmap_unlockPixels(Env.jEnv,jBitmap);
 end;

//------------------------------------------------------------------------------
// Image
//------------------------------------------------------------------------------

Function  jImage_getWH       ( Const Env : TEnv; filename : String ) : TWH;
 Const
  _cFuncName = 'Image_getWH';
  _cFuncSig  = '(Ljava/lang/String;)I';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParam  : jValue;
  _wh      : Integer;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParam.l  := Env.jEnv^.NewStringUTF(Env.jEnv, pchar(filename) );
  _wh        := Env.jEnv^.CallIntMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParam);
  Env.jEnv^.DeleteLocalRef(Env.jEnv,_jParam.l);
  //
  Result.Width  := (_wh shr 16);
  Result.Height := (_wh and $0000FFFF);
  jLog('Image : ' + IntToStr(Result.Width) + 'x' + IntTostr(Result.Height));
 end;

//
Function  jImage_resample              (Const Env : TEnv; filename : String; size : integer ) : jObject;
 Const
  _cFuncName = 'Image_resample';
  _cFuncSig  = '(Ljava/lang/String;I)Landroid/graphics/Bitmap;';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : Array[0..1] of jValue;
  _jObject : jObject;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := Env.jEnv^.NewStringUTF(Env.jEnv, pchar(filename) );
  _jParams[1].i := size;
  _jObject      := Env.jEnv^.CallObjectMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
  Env.jEnv^.DeleteLocalRef(Env.jEnv,_jParams[0].l);
  Result := _jObject;
  jLog('resampling');
 end;

Procedure jImage_save                  (Const Env : TEnv; Bitmap : jObject; filename : String);
 Const
  _cFuncName = 'Image_save';
  _cFuncSig  = '(Landroid/graphics/Bitmap;Ljava/lang/String;)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : Array[0..1] of jValue;
  _jObject : jObject;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := Bitmap;;
  _jParams[1].l := Env.jEnv^.NewStringUTF(Env.jEnv, pchar(filename) );
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
  Env.jEnv^.DeleteLocalRef (Env.jEnv,_jParams[1].l);
 end;


//
Procedure jView_SetVisible (Const Env : TEnv; view : jObject; visible : Boolean);
Const
 _cFuncName = 'view_SetVisible';
 _cFuncSig  = '(Landroid/view/View;I)V';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParams : Array[0..1] of jValue;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParams[0].l := view;
 Case Visible of
   True  : _jParams[1].i := 0; //
   False : _jParams[1].i := 4; //
 end;
 Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
end;

//
Procedure jView_SetBackGroundColor (Const Env : TEnv; view : jObject; color : DWord);
Const
 _cFuncName = 'view_SetBackGroundColor';
 _cFuncSig  = '(Landroid/view/View;I)V';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParams : Array[0..1] of jValue;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParams[0].l := view;
 _jParams[1].i := color;
 Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
end;

//
Procedure jView_Invalidate (Const Env : TEnv; view : jObject);
Const
 _cFuncName = 'view_Invalidate';
 _cFuncSig  = '(Landroid/view/View;)V';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParam  : jValue;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParam.l := view;
 Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParam);
end;

//------------------------------------------------------------------------------
// Form
//------------------------------------------------------------------------------

//
Function  jForm_Create (Const Env : TEnv; pasObj : TObject) : jObject;
 Const
  _cFuncName = 'jForm_Create';
  _cFuncSig  = '(I)Ljava/lang/Object;';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParam  : jValue;
  _jC      : jClass;
 begin
  //
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParam.l := pasObj;

  Result := Env.jEnv^.CallObjectMethodA(Env.jEnv,Env.jControls,_jMethod,@_jParam);
  Result := Env.jEnv^.NewGlobalRef     (Env.jEnv,Result);
 end;

//
Procedure jForm_Free      (Const Env : TEnv; Form    : jObject);
 Const
  _cFuncName = 'jForm_Free';
  _cFuncSig  = '(Ljava/lang/Object;)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams.l := Form;
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
  Env.jEnv^.DeleteGlobalRef(Env.jEnv, Form);
 end;

//
Procedure jForm_Show (Const Env : TEnv; Form : jObject;  Effect : TAnimation_Effect = Animation_Effect_None);
 Const
  _cFuncName = 'jForm_Show';
  _cFuncSig  = '(Ljava/lang/Object;I)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : Array[0..1] of jValue;
 begin
  //
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := Form;
  _jParams[1].i := Integer(Effect);
  Env.jEnv^.CallVoidMethodA(Env.jEnv,Env.jControls,_jMethod,@_jParams);
 end;

//
Procedure jForm_Close     (Const Env : TEnv; Form : jObject; Effect : TAnimation_Effect = Animation_Effect_None);
 Const
  _cFuncName = 'jForm_Close';
  _cFuncSig  = '(Ljava/lang/Object;I)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : Array[0..1] of jValue;
 begin
  //
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := Form;
  _jParams[1].i := Integer(Effect);
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 end;

//
Function  jForm_GetLayoutG (Const Env : TEnv; Form : jObject) : jObject;
 Const
  _cFuncName = 'jForm_GetLayout';
  _cFuncSig  = '(Ljava/lang/Object;)Landroid/widget/RelativeLayout;';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParam  : jValue;
 begin
  //
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParam.l := Form;

  Result := Env.jEnv^.CallObjectMethodA(Env.jEnv,Env.jControls,_jMethod,@_jParam);
  Result := Env.jEnv^.NewGlobalRef     (Env.jEnv,Result);
 end;

//
Procedure jForm_SetVisibility (Const Env : TEnv; Form : jObject; visible : Boolean);
 Const
  _cFuncName = 'jForm_SetVisible';
  _cFuncSig  = '(Ljava/lang/Object;Z)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..1] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := Form;
  _jParams[1].z := Byte(visible);
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 end;

Procedure jForm_SetTitle (Const Env : TEnv; Form : jObject; visible : Boolean);
 Const
  _cFuncName = 'jForm_SetTitle';
  _cFuncSig  = '(Ljava/lang/Object;Z)V';
 Var
  _jMethod : jMethodID = nil;
  _jParams : array[0..1] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := Form;
  _jParams[1].z := Byte(visible);
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 end;

//
Procedure jForm_SetEnabled    (Const Env : TEnv; Form : jObject; enabled : Boolean);
 Const
  _cFuncName = 'jForm_SetEnabled';
  _cFuncSig  = '(Ljava/lang/Object;Z)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..1] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := Form;
  _jParams[1].z := Byte(enabled);
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 end;


//------------------------------------------------------------------------------
// TextView
//------------------------------------------------------------------------------

//
Function jTextView_Create(Const Env : TEnv; pasObj : TObject ) : jObject;
Const
 _cFuncName = 'jTextView_Create';
 _cFuncSig  = '(I)Ljava/lang/Object;';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParams : array[0..0] of jValue;
 cls: jClass;
begin
 //
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParams[0].l := pasObj;
 Result := Env.jEnv^.CallObjectMethodA(Env.jEnv, Env.jControls, _jMethod,@_jParams);
 Result := Env.jEnv^.NewGlobalRef     (Env.jEnv, Result);
end;

//
Procedure jTextView_Free (Const Env : TEnv; TextView : jObject);
 Const
  _cFuncName = 'jTextView_Free';
  _cFuncSig  = '(Ljava/lang/Object;)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams.l := TextView;
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
  Env.jEnv^.DeleteGlobalRef(Env.jEnv, TextView);
 end;

//
Procedure jTextView_setParent(Const Env : TEnv; TextView : jObject;ViewGroup : jObject);
 Const
  _cFuncName = 'jTextView_setParent';
  _cFuncSig  = '(Ljava/lang/Object;Landroid/view/ViewGroup;)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..1] of jValue;
 begin
  //
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := TextView;
  _jParams[1].l := ViewGroup;
  Env.jEnv^.CallVoidMethodA(Env.jEnv,Env.jControls,_jMethod,@_jParams);
 end;

//
Procedure jTextView_setEnabled (Const Env : TEnv; TextView : jObject; enabled : Boolean);
 Const
  _cFuncName = 'jTextView_setEnabled';
  _cFuncSig  = '(Ljava/lang/Object;Z)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..1] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := TextView;
  _jParams[1].z := Byte(enabled);
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 end;

//
Procedure jTextView_setXYWH(Const Env : TEnv; TextView : jObject;x,y,w,h : integer);
 Const
  _cFuncName = 'jTextView_setXYWH';
  _cFuncSig  = '(Ljava/lang/Object;IIII)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..4] of jValue;
 begin
  //
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := TextView;
  _jParams[1].i := x;
  _jParams[2].i := y;
  _jParams[3].i := w;
  _jParams[4].i := h;
  Env.jEnv^.CallVoidMethodA(Env.jEnv,Env.jControls,_jMethod,@_jParams);
 end;

//
Function jTextView_getText(Const Env : TEnv; TextView : jObject) : String;
 Const
  _cFuncName = 'jTextView_getText';
  _cFuncSig  = '(Ljava/lang/Object;)Ljava/lang/String;';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : jValue;
  _jString : jString;
  _jBoolean: jBoolean;
  _jStrUTF : PChar;
 begin
  //
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams.l := TextView;
  _jString   := Env.jEnv^.CallObjectMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
  Case _jString = nil of
   True : Result    := '';
   False: begin
           _jBoolean := JNI_False;
           _jStrUTF  := Env.jEnv^.GetStringUTFChars(Env.jEnv,_jString,@_jBoolean);
           Result    := String( _jStrUTF );
           Env.jEnv^.ReleaseStringUTFChars(Env.jEnv,_jString,_jStrUTF);
          end;
  end;
 end;

//
Procedure jTextView_setText (Const Env : TEnv; TextView : jObject; Str : String);
 Const
  _cFuncName = 'jTextView_setText';
  _cFuncSig  = '(Ljava/lang/Object;Ljava/lang/String;)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..1] of jValue;
 begin
  //
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := TextView;
  _jParams[1].l := Env.jEnv^.NewStringUTF(Env.jEnv, pchar(Str));
  Env.jEnv^.CallVoidMethodA(Env.jEnv,Env.jControls,_jMethod,@_jParams);
  Env.jEnv^.DeleteLocalRef (Env.jEnv,_jParams[1].l);
 end;

//
Procedure jTextView_setTextColor (Const Env : TEnv; TextView : jObject; color : TColor);
 Const
  _cFuncName = 'jTextView_setTextColor';
  _cFuncSig  = '(Ljava/lang/Object;I)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..1] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := TextView;
  _jParams[1].i := color;
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 end;

// Font Height ( Pixel )
Procedure jTextView_setTextSize (Const Env : TEnv; TextView : jObject; size : DWord);
 Const
  _cFuncName = 'jTextView_setTextSize';
  _cFuncSig  = '(Ljava/lang/Object;I)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..1] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := TextView;
  _jParams[1].i := size;
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 end;

// LORDMAN   - 2013-08-12
// Simonsayz - 2015.02.25 convert Type style
Procedure jTextView_setTextAlignment (Const Env : TEnv; TextView : jObject; align : TText_Alignment);
 Const
  _cFuncName = 'jTextView_setTextAlignment';
  _cFuncSig  = '(Ljava/lang/Object;I)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..1] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := TextView;
  _jParams[1].i := Integer(align);
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 end;

//------------------------------------------------------------------------------
// EditText
//------------------------------------------------------------------------------

//
Function jEditText_Create(Const Env : TEnv; pasObj : TObject ) : jObject;
 Const
  _cFuncName = 'jEditText_Create';
  _cFuncSig  = '(I)Ljava/lang/Object;';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..0] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := pasObj;
  Result := Env.jEnv^.CallObjectMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
  Result := Env.jEnv^.NewGlobalRef     (Env.jEnv,Result);
 end;

//
Procedure jEditText_Free (Const Env : TEnv; EditText : jObject);
 Const
  _cFuncName = 'jEditText_Free';
  _cFuncSig  = '(Ljava/lang/Object;)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams.l := EditText;
  Env.jEnv^.CallVoidMethodA(Env.jEnv,Env.jControls,_jMethod,@_jParams);
  Env.jEnv^.DeleteGlobalRef(Env.jEnv,EditText);
 end;

//
Procedure jEditText_setXYWH(Const Env : TEnv; EditText : jObject;x,y,w,h : integer);
 Const
  _cFuncName = 'jEditText_setXYWH';
  _cFuncSig  = '(Ljava/lang/Object;IIII)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..4] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := EditText;
  _jParams[1].i := x;
  _jParams[2].i := y;
  _jParams[3].i := w;
  _jParams[4].i := h;
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 end;

//
Procedure jEditText_setParent(Const Env : TEnv;
                              EditText : jObject;ViewGroup : jObject);
 Const
  _cFuncName = 'jEditText_setParent';
  _cFuncSig  = '(Ljava/lang/Object;Landroid/view/ViewGroup;)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..1] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := EditText;
  _jParams[1].l := ViewGroup;
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 end;

// LORDMAN 2013-08-27
Procedure jEditText_SetEnabled (Const Env : TEnv;
                                EditText : jObject; enabled : Boolean);
Const
 _cFuncName = 'jEditText_setEnabled';
 _cFuncSig  = '(Ljava/lang/Object;Z)V';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParams : array[0..1] of jValue;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParams[0].l := EditText;
 _jParams[1].z := jBool(enabled);
 Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
end;

// Simonsayz 20150225 - Fixed Memory leak
Function jEditText_getText(Const Env : TEnv; EditText : jObject) : String;
 Const
  _cFuncName = 'jEditText_getText';
  _cFuncSig  = '(Ljava/lang/Object;)Ljava/lang/String;';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : jValue;
  _jString : jString;
  _jBoolean: jBoolean;
  _jStrUTF : PChar;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams.l := EditText;
  _jString   := Env.jEnv^.CallObjectMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
  Case _jString = nil of
   True : Result    := '';
   False: begin
           _jBoolean := JNI_False;
           _jStrUTF  := Env.jEnv^.GetStringUTFChars(Env.jEnv,_jString,@_jBoolean);
           Result    := String( _jStrUTF );
           Env.jEnv^.ReleaseStringUTFChars(Env.jEnv,_jString,_jStrUTF);
          end;
  end;
 end;

//
Procedure jEditText_setText(Const Env : TEnv;
                            EditText : jObject; Str : String);
 Const
  _cFuncName = 'jEditText_setText';
  _cFuncSig  = '(Ljava/lang/Object;Ljava/lang/String;)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..1] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := EditText;
  _jParams[1].l := Env.jEnv^.NewStringUTF(Env.jEnv, pchar(Str) );
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
  Env.jEnv^.DeleteLocalRef (Env.jEnv,_jParams[1].l);
 end;

//
Procedure jEditText_setTextColor (Const Env : TEnv;
                                  EditText : jObject; color : DWord);
Const
 _cFuncName = 'jEditText_setTextColor';
 _cFuncSig  = '(Ljava/lang/Object;I)V';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParams : array[0..1] of jValue;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParams[0].l := EditText;
 _jParams[1].i := color;
 Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
end;

// Font Height ( Pixel )
Procedure jEditText_setTextSize (Const Env : TEnv;
                                 EditText : jObject; size : DWord);
Const
 _cFuncName = 'jEditText_setTextSize';
 _cFuncSig  = '(Ljava/lang/Object;I)V';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParams : array[0..1] of jValue;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParams[0].l := EditText;
 _jParams[1].i := size;
 Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
end;

// Hint, PlaceHolder
Procedure jEditText_setHint     (Const Env : TEnv; EditText : jObject;
                                 Str : String);
Const
 _cFuncName = 'jEditText_setHint';
 _cFuncSig  = '(Ljava/lang/Object;Ljava/lang/String;)V';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParams : array[0..1] of jValue;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParams[0].l := EditText;
 _jParams[1].l := Env.jEnv^.NewStringUTF(Env.jEnv, pchar(Str) );
 Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 Env.jEnv^.DeleteLocalRef (Env.jEnv,_jParams[1].l);
end;

// LORDMAN - 2013-07-26
Procedure jEditText_SetFocus (Const Env : TEnv; EditText : jObject );
Const
 _cFuncName = 'jEditText_SetFocus';
 _cFuncSig  = '(Ljava/lang/Object;)V';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParams : array[0..1] of jValue;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParams[0].l := EditText;
 Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
end;

// LORDMAN - 2013-07-26
Procedure jEditText_immShow (Const Env : TEnv; EditText : jObject );
Const
 _cFuncName = 'jEditText_immShow';
 _cFuncSig  = '(Ljava/lang/Object;)V';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParams : array[0..1] of jValue;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParams[0].l := EditText;
 Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
end;

// LORDMAN - 2013-07-26
Procedure jEditText_immHide (Const Env : TEnv; EditText : jObject );
Const
 _cFuncName = 'jEditText_immHide';
 _cFuncSig  = '(Ljava/lang/Object;)V';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParams : array[0..1] of jValue;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParams[0].l := EditText;
 Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
end;

// 2015.03.04 DonAlfredo added Property
Procedure jEditText_setEditStyle (Const Env : TEnv; EditText : jObject; editStyle : TEdit_Style);
Const
 _cFuncName = 'jEditText_setEditStyle';
 _cFuncSig  = '(Ljava/lang/Object;I)V';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParams : array[0..1] of jValue;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParams[0].l := EditText;
 _jParams[1].i := Integer(editStyle);
 Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
end;

// 2013.07.26 LORDMAN    added Property
// 2015.02.28 SimonSayz  convert int to type
Procedure jEditText_setEditType (Const Env : TEnv; EditText : jObject; editType : TEdit_Type);
Const
 _cFuncName = 'jEditText_setEditType';
 _cFuncSig  = '(Ljava/lang/Object;I)V';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParams : array[0..1] of jValue;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParams[0].l := EditText;
 _jParams[1].i := Integer(editType);
 Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
end;

// LORDMAN - 2013-07-26
Procedure jEditText_maxLength(Const Env : TEnv; EditText : jObject; size  : DWord);
Const
 _cFuncName = 'jEditText_maxLength';
 _cFuncSig  = '(Ljava/lang/Object;I)V';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParams : array[0..1] of jValue;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParams[0].l := EditText;
 _jParams[1].i := size;
 Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
end;

// LORDMAN - 2013-07-26
Procedure jEditText_GetCursorPos       (Const Env : TEnv; EditText : jObject; Var x,y : Integer);
Const
 _cFuncName = 'jEditText_GetCursorPos';
 _cFuncSig  = '(Ljava/lang/Object;)[I';
Var
 _jMethod   : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParam    : jValue;
 _jIntArray : jintArray;
 _jBoolean  : jBoolean;
 //
 PInt    : PInt32;
 PIntSav : PInt32;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParam.l  := EditText;
 _jIntArray := Env.jEnv^.CallObjectMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParam);
 //
 _jBoolean  := JNI_False;
 PInt       := Env.jEnv^.GetIntArrayElements(Env.jEnv,_jIntArray,_jBoolean);
 PIntSav    := PInt;
 x := PInt^; Inc(PInt);
 y := PInt^; Inc(PInt);
 Env.jEnv^.ReleaseIntArrayElements(Env.jEnv,_jIntArray,PIntSav,0);
end;

// LORDMAN - 2013-07-26
Procedure jEditText_SetCursorPos(Const Env : TEnv; EditText : jObject; startPos,endPos : Integer);
Const
 _cFuncName = 'jEditText_SetCursorPos';
 _cFuncSig  = '(Ljava/lang/Object;II)V';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParams : array[0..2] of jValue;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParams[0].l := EditText;
 _jParams[1].i := startPos;
 _jParams[2].i := endPos;
 Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
end;

// LORDMAN - 2013-08-12
Procedure jEditText_setTextAlignment (Const Env : TEnv; EditText : jObject; align : DWord);
Const
 _cFuncName = 'jEditText_setTextAlignment';
 _cFuncSig  = '(Ljava/lang/Object;I)V';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParams : array[0..1] of jValue;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParams[0].l := EditText;
 _jParams[1].i := align;
 Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
end;

// LORDMAN 2013-08-27
Procedure jEditText_SetEditable (Const Env : TEnv;
                                 EditText : jObject; enabled : Boolean);
Const
 _cFuncName = 'jEditText_setEditable';
 _cFuncSig  = '(Ljava/lang/Object;Z)V';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParams : array[0..1] of jValue;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParams[0].l := EditText;
 _jParams[1].z := jBool(enabled);
 Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
end;

//------------------------------------------------------------------------------
// Button
//------------------------------------------------------------------------------

//
Function jButton_Create(Const Env : TEnv;  pasObj: TObject) : jObject;
 Const
  _cFuncName = 'jButton_Create';
  _cFuncSig  = '(I)Ljava/lang/Object;';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..0] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := pasObj;
  Result := Env.jEnv^.CallObjectMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
  Result := Env.jEnv^.NewGlobalRef     (Env.jEnv, Result);
 end;

//
Procedure jButton_Free (Const Env : TEnv; Button : jObject);
  Const
   _cFuncName = 'jButton_Free';
   _cFuncSig  = '(Ljava/lang/Object;)V';
  Var
   _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
   _jParams : jValue;
  begin
   jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
   _jParams.l := Button;
   Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
   Env.jEnv^.DeleteGlobalRef(Env.jEnv,Button);
  end;

//
Procedure jButton_setXYWH(Const Env : TEnv;
                          Button : jObject;x,y,w,h : integer);
 Const
  _cFuncName = 'jButton_setXYWH';
  _cFuncSig  = '(Ljava/lang/Object;IIII)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..4] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := Button;
  _jParams[1].i := x;
  _jParams[2].i := y;
  _jParams[3].i := w;
  _jParams[4].i := h;
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 end;

//
Procedure jButton_setParent(Const Env : TEnv;
                            Button : jObject;ViewGroup : jObject);
 Const
  _cFuncName = 'jButton_setParent';
  _cFuncSig  = '(Ljava/lang/Object;Landroid/view/ViewGroup;)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..1] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := Button;
  _jParams[1].l := ViewGroup;
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 end;

//
Procedure jButton_setText(Const Env : TEnv;
                          Button : jObject; Str : String);
 Const
  _cFuncName = 'jButton_setText';
  _cFuncSig  = '(Ljava/lang/Object;Ljava/lang/String;)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..1] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := Button;
  _jParams[1].l := Env.jEnv^.NewStringUTF(Env.jEnv, pchar(Str) );

  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
  Env.jEnv^.DeleteLocalRef (Env.jEnv,_jParams[1].l);
 end;

//
Function jButton_getText(Const Env : TEnv;
                         Button : jObject) : String;
 Const
  _cFuncName = 'jButton_getText';
  _cFuncSig  = '(Ljava/lang/Object;)Ljava/lang/String;';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : jValue;
  _jString : jString;
  _jBoolean: jBoolean;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams.l := Button;
  _jString   := Env.jEnv^.CallObjectMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
  Case _jString = nil of
   True : Result    := '';
   False: begin
           _jBoolean := JNI_False;
           Result    := String( Env.jEnv^.GetStringUTFChars(Env.jEnv,_jString,@_jBoolean) );
          end;
  end;
 end;

//
Procedure jButton_setTextColor (Const Env : TEnv;
                                Button : jObject; color : DWord);
Const
 _cFuncName = 'jButton_setTextColor';
 _cFuncSig  = '(Ljava/lang/Object;I)V';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParams : array[0..1] of jValue;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParams[0].l := Button;
 _jParams[1].i := color;
 Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
end;

// Font Height ( Pixel )
Procedure jButton_setTextSize (Const Env : TEnv;
                               Button : jObject; size : DWord);
Const
 _cFuncName = 'jButton_setTextSize';
 _cFuncSig  = '(Ljava/lang/Object;I)V';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParams : array[0..1] of jValue;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParams[0].l := Button;
 _jParams[1].i := size;
 Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
end;

//
Procedure jButton_setOnClick(Const Env : TEnv;
                             Button : jObject);
 Const
  _cFuncName = 'Button_setOnClick';
  _cFuncSig  = '(Landroid/widget/Button;)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams.l := Button;
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 end;

//------------------------------------------------------------------------------
// CheckBox
//------------------------------------------------------------------------------

//
Function jCheckBox_Create(Const Env : TEnv; pasObj : TObject ) : jObject;
 Const
  _cFuncName = 'jCheckBox_Create';
  _cFuncSig  = '(I)Ljava/lang/Object;';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..1] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := pasObj;
  Result := Env.jEnv^.CallObjectMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
  Result := Env.jEnv^.NewGlobalRef     (Env.jEnv, Result);
 end;

//
Procedure jCheckBox_Free (Const Env : TEnv; CheckBox : jObject);
  Const
   _cFuncName = 'jCheckBox_Free';
   _cFuncSig  = '(Ljava/lang/Object;)V';
  Var
   _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
   _jParams : jValue;
  begin
   jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
   _jParams.l := CheckBox;
   Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
   Env.jEnv^.DeleteGlobalRef(Env.jEnv,CheckBox);
  end;

//
Procedure jCheckBox_setXYWH(Const Env : TEnv;
                            CheckBox : jObject;x,y,w,h : integer);
 Const
  _cFuncName = 'jCheckBox_setXYWH';
  _cFuncSig  = '(Ljava/lang/Object;IIII)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..4] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := CheckBox;
  _jParams[1].i := x;
  _jParams[2].i := y;
  _jParams[3].i := w;
  _jParams[4].i := h;
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 end;

//
Procedure jCheckBox_setParent(Const Env : TEnv;
                              CheckBox : jObject;ViewGroup : jObject);
 Const
  _cFuncName = 'jCheckBox_setParent';
  _cFuncSig  = '(Ljava/lang/Object;Landroid/view/ViewGroup;)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..1] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := CheckBox;
  _jParams[1].l := ViewGroup;
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 end;


// Java Function
Function jCheckBox_getText(Const Env : TEnv; CheckBox : jObject) : String;
 Const
  _cFuncName = 'jCheckBox_getText';
  _cFuncSig  = '(Ljava/lang/Object;)Ljava/lang/String;';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : jValue;
  _jString : jString;
  _jBoolean: jBoolean;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams.l := CheckBox;
  _jString   := Env.jEnv^.CallObjectMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
  Case _jString = nil of
   True : Result    := '';
   False: begin
           _jBoolean := JNI_False;
           Result    := String( Env.jEnv^.GetStringUTFChars(Env.jEnv,_jString,@_jBoolean) );
          end;
  end;
 end;

// Java Function
Procedure jCheckBox_setText(Const Env : TEnv;
                            CheckBox : jObject; Str : String);
 Const
  _cFuncName = 'jCheckBox_setText';
  _cFuncSig  = '(Ljava/lang/Object;Ljava/lang/String;)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..1] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := CheckBox;
  _jParams[1].l := Env.jEnv^.NewStringUTF(Env.jEnv, pchar(Str) );
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
  Env.jEnv^.DeleteLocalRef (Env.jEnv,_jParams[1].l);
 end;

//
Procedure jCheckBox_setTextColor (Const Env : TEnv;
                                  CheckBox : jObject; color : DWord);
Const
 _cFuncName = 'jCheckBox_setTextColor';
 _cFuncSig  = '(Ljava/lang/Object;I)V';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParams : array[0..1] of jValue;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParams[0].l := CheckBox;
 _jParams[1].i := color;
 Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
end;

//
Function  jCheckBox_isChecked          (Const Env : TEnv; CheckBox : jObject) : Boolean;
Const
 _cFuncName = 'jCheckBox_isChecked';
 _cFuncSig  = '(Ljava/lang/Object;)Z';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParams : jValue;
 _jBool   : jBoolean;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParams.l := CheckBox;
 _jBool     := Env.jEnv^.CallBooleanMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 Result     := Boolean(_jBool);
end;

//
Procedure jCheckBox_setChecked         (Const Env : TEnv;
                                        CheckBox : jObject; value : Boolean);
Const
 _cFuncName = 'jCheckBox_setChecked';
 _cFuncSig  = '(Ljava/lang/Object;Z)V';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParams : array[0..1] of jValue;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParams[0].l := CheckBox;
 _jParams[1].z := Byte(value);
 Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
end;

// Font Height ( Pixel )
Procedure jCheckBox_setTextSize (Const Env : TEnv;
                                  CheckBox : jObject; size : DWord);
Const
 _cFuncName = 'jCheckBox_setTextSize';
 _cFuncSig  = '(Ljava/lang/Object;I)V';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParams : array[0..1] of jValue;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParams[0].l := CheckBox;
 _jParams[1].i := size;
 Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
end;

//------------------------------------------------------------------------------
// RadioButton
//------------------------------------------------------------------------------

//
Function jRadioButton_Create(Const Env : TEnv; pasObj : TObject ) : jObject;
 Const
  _cFuncName = 'jRadioButton_Create';
  _cFuncSig  = '(I)Ljava/lang/Object;';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..0] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := pasObj;
  Result := Env.jEnv^.CallObjectMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
  Result := Env.jEnv^.NewGlobalRef     (Env.jEnv, Result);
 end;

//
Procedure jRadioButton_Free (Const Env : TEnv; RadioButton : jObject);
  Const
   _cFuncName = 'jRadioButton_Free';
   _cFuncSig  = '(Ljava/lang/Object;)V';
  Var
   _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
   _jParams : jValue;
  begin
   jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
   _jParams.l := RadioButton;
   Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
   Env.jEnv^.DeleteGlobalRef(Env.jEnv,RadioButton);
  end;

//
Procedure jRadioButton_setXYWH(Const Env : TEnv;
                            RadioButton : jObject;x,y,w,h : integer);
 Const
  _cFuncName = 'jRadioButton_setXYWH';
  _cFuncSig  = '(Ljava/lang/Object;IIII)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..4] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := RadioButton;
  _jParams[1].i := x;
  _jParams[2].i := y;
  _jParams[3].i := w;
  _jParams[4].i := h;
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 end;

//
Procedure jRadioButton_setParent(Const Env : TEnv;
                              RadioButton : jObject;ViewGroup : jObject);
 Const
  _cFuncName = 'jRadioButton_setParent';
  _cFuncSig  = '(Ljava/lang/Object;Landroid/view/ViewGroup;)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..1] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := RadioButton;
  _jParams[1].l := ViewGroup;
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 end;


// Java Function
Function jRadioButton_getText(Const Env : TEnv; RadioButton : jObject) : String;
 Const
  _cFuncName = 'jRadioButton_getText';
  _cFuncSig  = '(Ljava/lang/Object;)Ljava/lang/String;';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : jValue;
  _jString : jString;
  _jBoolean: jBoolean;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams.l := RadioButton;
  _jString   := Env.jEnv^.CallObjectMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
  Case _jString = nil of
   True : Result    := '';
   False: begin
           _jBoolean := JNI_False;
           Result    := String( Env.jEnv^.GetStringUTFChars(Env.jEnv,_jString,@_jBoolean) );
          end;
  end;
 end;

// Java Function
Procedure jRadioButton_setText(Const Env : TEnv;
                            RadioButton : jObject; Str : String);
 Const
  _cFuncName = 'jRadioButton_setText';
  _cFuncSig  = '(Ljava/lang/Object;Ljava/lang/String;)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..1] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := RadioButton;
  _jParams[1].l := Env.jEnv^.NewStringUTF(Env.jEnv, pchar(Str) );
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
  Env.jEnv^.DeleteLocalRef (Env.jEnv,_jParams[1].l);
 end;

//
Procedure jRadioButton_setTextColor (Const Env : TEnv;
                                  RadioButton : jObject; color : DWord);
Const
 _cFuncName = 'jRadioButton_setTextColor';
 _cFuncSig  = '(Ljava/lang/Object;I)V';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParams : array[0..1] of jValue;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParams[0].l := RadioButton;
 _jParams[1].i := color;
 Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
end;

//
Function  jRadioButton_isChecked          (Const Env : TEnv; RadioButton : jObject) : Boolean;
Const
 _cFuncName = 'jRadioButton_isChecked';
 _cFuncSig  = '(Ljava/lang/Object;)Z';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParams : jValue;
 _jBool   : jBoolean;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParams.l := RadioButton;
 _jBool     := Env.jEnv^.CallBooleanMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 Result     := Boolean(_jBool);
end;

//
Procedure jRadioButton_setChecked         (Const Env : TEnv;
                                        RadioButton : jObject; value : Boolean);
Const
 _cFuncName = 'jRadioButton_setChecked';
 _cFuncSig  = '(Ljava/lang/Object;Z)V';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParams : array[0..1] of jValue;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParams[0].l := RadioButton;
 _jParams[1].z := Byte(value);
 Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
end;

// Font Height ( Pixel )
Procedure jRadioButton_setTextSize (Const Env : TEnv;
                                  RadioButton : jObject; size : DWord);
Const
 _cFuncName = 'jRadioButton_setTextSize';
 _cFuncSig  = '(Ljava/lang/Object;I)V';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParams : array[0..1] of jValue;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParams[0].l := RadioButton;
 _jParams[1].i := size;
 Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
end;

//------------------------------------------------------------------------------
// ProgressBar
//------------------------------------------------------------------------------

// Java Function
Function jProgressBar_Create(Const Env : TEnv; pasObj : TObject; Style : DWord ) : jObject;
 Const
  _cFuncName = 'jProgressBar_Create';
  _cFuncSig  = '(II)Ljava/lang/Object;';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..1] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := pasObj;
  _jParams[1].i := Style;
  Result := Env.jEnv^.CallObjectMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
  Result := Env.jEnv^.NewGlobalRef     (Env.jEnv,Result);
 end;

//
Procedure jProgressBar_Free (Const Env : TEnv; ProgressBar : jObject);
  Const
   _cFuncName = 'jProgressBar_Free';
   _cFuncSig  = '(Ljava/lang/Object;)V';
  Var
   _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
   _jParams : jValue;
  begin
   jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
   _jParams.l := ProgressBar;
   Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
   Env.jEnv^.DeleteGlobalRef(Env.jEnv, ProgressBar);
  end;

//
Procedure jProgressBar_setXYWH(Const Env : TEnv;
                               ProgressBar : jObject;x,y,w,h : integer);
 Const
  _cFuncName = 'jProgressBar_setXYWH';
  _cFuncSig  = '(Ljava/lang/Object;IIII)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..4] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := ProgressBar;
  _jParams[1].i := x;
  _jParams[2].i := y;
  _jParams[3].i := w;
  _jParams[4].i := h;
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 end;

//
Procedure jProgressBar_setParent(Const Env : TEnv;
                                 ProgressBar : jObject;ViewGroup : jObject);
 Const
  _cFuncName = 'jProgressBar_setParent';
  _cFuncSig  = '(Ljava/lang/Object;Landroid/view/ViewGroup;)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..1] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := ProgressBar;
  _jParams[1].l := ViewGroup;
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 end;

//
Function  jProgressBar_getProgress (Const Env : TEnv; ProgressBar : jObject) : Integer;
Const
 _cFuncName = 'jProgressBar_getProgress';
 _cFuncSig  = '(Ljava/lang/Object;)I';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParams : jValue;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParams.l := ProgressBar;
 Result     := Env.jEnv^.CallIntMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
end;

//
Procedure jProgressBar_setProgress (Const Env : TEnv;
                                    ProgressBar : jObject; value : Integer);
Const
 _cFuncName = 'jProgressBar_setProgress';
 _cFuncSig  = '(Ljava/lang/Object;I)V';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParams : array[0..1] of jValue;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParams[0].l := ProgressBar;
 _jParams[1].i := value;
 Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
end;


//------------------------------------------------------------------------------
// ImageView
//------------------------------------------------------------------------------

//
Function  jImageView_Create  (Const Env : TEnv; pasObj : TObject) : jObject;
 Const
  _cFuncName = 'jImageView_Create';
  _cFuncSig  = '(I)Ljava/lang/Object;';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..0] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := pasObj;
  Result := Env.jEnv^.CallObjectMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
  Result := Env.jEnv^.NewGlobalRef     (Env.jEnv,Result);
 end;

//
Procedure jImageView_Free   (Const Env : TEnv; ImageView : jObject);
  Const
   _cFuncName = 'jImageView_Free';
   _cFuncSig  = '(Ljava/lang/Object;)V';
  Var
   _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
   _jParams : jValue;
  begin
   jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
   _jParams.l := ImageView;
   Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
   Env.jEnv^.DeleteGlobalRef(Env.jEnv,ImageView);
  end;

//
Procedure jImageView_setXYWH(Const Env : TEnv;
                             ImageView : jObject;x,y,w,h : integer);
 Const
  _cFuncName = 'jImageView_setXYWH';
  _cFuncSig  = '(Ljava/lang/Object;IIII)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..4] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := ImageView;
  _jParams[1].i := x;
  _jParams[2].i := y;
  _jParams[3].i := w;
  _jParams[4].i := h;
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 end;

//
Procedure jImageView_setParent(Const Env : TEnv;
                               ImageView : jObject;ViewGroup : jObject);
 Const
  _cFuncName = 'jImageView_setParent';
  _cFuncSig  = '(Ljava/lang/Object;Landroid/view/ViewGroup;)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..1] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := ImageView;
  _jParams[1].l := ViewGroup;
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 end;

//
Procedure jImageView_setImage(Const Env : TEnv;
                              ImageView : jObject; Str : String);
 Const
  _cFuncName = 'jImageView_setImage';
  _cFuncSig  = '(Ljava/lang/Object;Ljava/lang/String;)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..1] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := ImageView;
  _jParams[1].l := Env.jEnv^.NewStringUTF(Env.jEnv, pchar(Str) );
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
  Env.jEnv^.DeleteLocalRef (Env.jEnv,_jParams[1].l);
 end;

//------------------------------------------------------------------------------
// ListView
//------------------------------------------------------------------------------

//
Function  jListView_Create  (Const Env : TEnv; pasObj : TObject) : jObject;
 Const
  _cFuncName = 'jListView_Create';
  _cFuncSig  = '(I)Ljava/lang/Object;';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..5] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := pasObj;
  Result := Env.jEnv^.CallObjectMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
  Result := Env.jEnv^.NewGlobalRef     (Env.jEnv,Result);
 end;

//
Procedure jListView_Free   (Const Env : TEnv; ListView : jObject);
  Const
   _cFuncName = 'jListView_Free';
   _cFuncSig  = '(Ljava/lang/Object;)V';
  Var
   _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
   _jParams : jValue;
  begin
   jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
   _jParams.l := ListView;
   Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
   Env.jEnv^.DeleteGlobalRef(Env.jEnv, ListView);
  end;

//
Procedure jListView_setXYWH(Const Env : TEnv;
                            ListView : jObject;x,y,w,h : integer);
 Const
  _cFuncName = 'jListView_setXYWH';
  _cFuncSig  = '(Ljava/lang/Object;IIII)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..4] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := ListView;
  _jParams[1].i := x;
  _jParams[2].i := y;
  _jParams[3].i := w;
  _jParams[4].i := h;
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 end;

//
Procedure jListView_setParent(Const Env : TEnv;
                              ListView : jObject;ViewGroup : jObject);
 Const
  _cFuncName = 'jListView_setParent';
  _cFuncSig  = '(Ljava/lang/Object;Landroid/view/ViewGroup;)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..1] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := ListView;
  _jParams[1].l := ViewGroup;
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 end;

//
Procedure jListView_setTextColor (Const Env : TEnv;
                                  ListView : jObject; color : DWord);
Const
  _cFuncName = 'jListView_setTextColor';
  _cFuncSig  = '(Ljava/lang/Object;I)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..1] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := ListView;
  _jParams[1].i := color;
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 end;

//
Procedure jListView_setTextSize  (Const Env : TEnv;
                                  ListView : jObject; size  : DWord);
Const
 _cFuncName = 'jListView_setTextSize';
 _cFuncSig  = '(Ljava/lang/Object;I)V';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParams : array[0..1] of jValue;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParams[0].l := ListView;
 _jParams[1].i := size;
 Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
end;

//
Procedure jListView_setItemPosition    (Const Env : TEnv;
                                        ListView : jObject; Pos: integer; y:Integer );

Const
 _cFuncName = 'jListView_setItemPosition';
 _cFuncSig  = '(Ljava/lang/Object;II)V';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParams : array[0..2] of jValue;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParams[0].l := ListView;
 _jParams[1].i := Pos;
 _jParams[2].i := y;
 Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
end;

// Java Function
Procedure jListView_add(Const Env : TEnv; ListView : jObject; Str : String);
 Const
  _cFuncName = 'jListView_add';
  _cFuncSig  = '(Ljava/lang/Object;Ljava/lang/String;)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..1] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := ListView;
  _jParams[1].l := Env.jEnv^.NewStringUTF(Env.jEnv, pchar(Str) );
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
  Env.jEnv^.DeleteLocalRef (Env.jEnv,_jParams[1].l);
 end;

Procedure jListView_clear              (Const Env : TEnv;
                                        ListView : jObject);
Const
 _cFuncName = 'jListView_clear';
 _cFuncSig  = '(Ljava/lang/Object;)V';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParam  : jValue;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParam.l := ListView;
 Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParam);
end;

Procedure jListView_delete             (Const Env : TEnv;
                                        ListView : jObject; index : integer);
Const
 _cFuncName = 'jListView_delete';
 _cFuncSig  = '(Ljava/lang/Object;I)V';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParams : array[0..1] of jValue;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParams[0].l := ListView;
 _jParams[1].i := index;
 Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
end;

//------------------------------------------------------------------------------
// ScrollView
//------------------------------------------------------------------------------

//
Function  jScrollView_Create  (Const Env : TEnv; pasObj : TObject) : jObject;
 Const
  _cFuncName = 'jScrollView_Create';
  _cFuncSig  = '(I)Ljava/lang/Object;';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..0] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := pasObj;
  Result := Env.jEnv^.CallObjectMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
  Result := Env.jEnv^.NewGlobalRef     (Env.jEnv,Result);
 end;

//
Procedure jScrollView_Free   (Const Env : TEnv; ScrollView : jObject);
  Const
   _cFuncName = 'jScrollView_Free';
   _cFuncSig  = '(Ljava/lang/Object;)V';
  Var
   _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
   _jParams : jValue;
  begin
   jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
   _jParams.l := ScrollView;
   Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
   Env.jEnv^.DeleteGlobalRef(Env.jEnv, ScrollView);
  end;

//
Procedure jScrollView_setXYWH(Const Env : TEnv;
                              ScrollView : jObject;x,y,w,h : integer);
 Const
  _cFuncName = 'jScrollView_setXYWH';
  _cFuncSig  = '(Ljava/lang/Object;IIII)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..4] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := ScrollView;
  _jParams[1].i := x;
  _jParams[2].i := y;
  _jParams[3].i := w;
  _jParams[4].i := h;
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 end;

//
Procedure jScrollView_setParent(Const Env : TEnv;
                                ScrollView : jObject;ViewGroup : jObject);
 Const
  _cFuncName = 'jScrollView_setParent';
  _cFuncSig  = '(Ljava/lang/Object;Landroid/view/ViewGroup;)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..1] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := ScrollView;
  _jParams[1].l := ViewGroup;
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 end;

//
Procedure jScrollView_setScrollSize(Const Env : TEnv;
                                    ScrollView : jObject; size : integer);
 Const
  _cFuncName = 'jScrollView_setScrollSize';
  _cFuncSig  = '(Ljava/lang/Object;I)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..1] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := ScrollView;
  _jParams[1].i := size;
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 end;

//
Function jScrollView_getView(Const Env : TEnv;
                             ScrollView : jObject) : jObject;
 Const
  _cFuncName = 'jScrollView_getView';
  _cFuncSig  = '(Ljava/lang/Object;)Landroid/view/ViewGroup;';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParam  : jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParam.l := ScrollView;
  Result := Env.jEnv^.CallObjectMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParam);
 end;


//------------------------------------------------------------------------------
// HorizontalScrollView
// LORDMAN 2013-09-03
//------------------------------------------------------------------------------
//
Function  jHorizontalScrollView_Create  (Const Env : TEnv; pasObj : TObject) : jObject;
 Const
  _cFuncName = 'jHorizontalScrollView_Create';
  _cFuncSig  = '(I)Ljava/lang/Object;';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..0] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := pasObj;
  Result := Env.jEnv^.CallObjectMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
  Result := Env.jEnv^.NewGlobalRef     (Env.jEnv, Result);
 end;

//
Procedure jHorizontalScrollView_Free   (Const Env : TEnv; ScrollView : jObject);
  Const
   _cFuncName = 'jHorizontalScrollView_Free';
   _cFuncSig  = '(Ljava/lang/Object;)V';
  Var
   _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
   _jParams : jValue;
  begin
   jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
   _jParams.l := ScrollView;
   Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
   Env.jEnv^.DeleteGlobalRef(Env.jEnv, ScrollView);
  end;

//
Procedure jHorizontalScrollView_setXYWH(Const Env : TEnv;
                                        ScrollView : jObject;x,y,w,h : integer);
 Const
  _cFuncName = 'jHorizontalScrollView_setXYWH';
  _cFuncSig  = '(Ljava/lang/Object;IIII)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..4] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := ScrollView;
  _jParams[1].i := x;
  _jParams[2].i := y;
  _jParams[3].i := w;
  _jParams[4].i := h;
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 end;

//
Procedure jHorizontalScrollView_setParent(Const Env : TEnv;
                                          ScrollView : jObject;ViewGroup : jObject);
 Const
  _cFuncName = 'jHorizontalScrollView_setParent';
  _cFuncSig  = '(Ljava/lang/Object;Landroid/view/ViewGroup;)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..1] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := ScrollView;
  _jParams[1].l := ViewGroup;
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 end;

//
Procedure jHorizontalScrollView_setScrollSize(Const Env : TEnv;
                                              ScrollView : jObject; size : integer);
 Const
  _cFuncName = 'jHorizontalScrollView_setScrollSize';
  _cFuncSig  = '(Ljava/lang/Object;I)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..1] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := ScrollView;
  _jParams[1].i := size;
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 end;

//
Function jHorizontalScrollView_getView(Const Env : TEnv;
                                       ScrollView : jObject) : jObject;
 Const
  _cFuncName = 'jHorizontalScrollView_getView';
  _cFuncSig  = '(Ljava/lang/Object;)Landroid/view/ViewGroup;';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParam  : jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParam.l := ScrollView;
  Result := Env.jEnv^.CallObjectMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParam);
 end;

//------------------------------------------------------------------------------
// ViewFlipper
//------------------------------------------------------------------------------

// ViewFlipper
Function  jViewFlipper_Create          (Const Env : TEnv; pasObj : TObject ) : jObject;
Const
 _cFuncName = 'jViewFlipper_Create';
 _cFuncSig  = '(I)Ljava/lang/Object;';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParams : array[0..0] of jValue;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParams[0].l := pasObj;
 Result := Env.jEnv^.CallObjectMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 Result := Env.jEnv^.NewGlobalRef     (Env.jEnv, Result);
end;

Procedure jViewFlipper_Free            (Const Env : TEnv; ViewFlipper : jObject);
Const
 _cFuncName = 'jViewFlipper_Free';
 _cFuncSig  = '(Ljava/lang/Object;)V';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParams : jValue;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParams.l := ViewFlipper;
 Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 Env.jEnv^.DeleteGlobalRef(Env.jEnv, ViewFlipper);
end;

//
Procedure jViewFlipper_setXYWH         (Const Env : TEnv;
                                        ViewFlipper : jObject;x,y,w,h : integer);
Const
 _cFuncName = 'jViewFlipper_setXYWH';
 _cFuncSig  = '(Ljava/lang/Object;IIII)V';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParams : array[0..4] of jValue;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParams[0].l := ViewFlipper;
 _jParams[1].i := x;
 _jParams[2].i := y;
 _jParams[3].i := w;
 _jParams[4].i := h;
 Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
end;

Procedure jViewFlipper_setParent       (Const Env : TEnv;
                                        ViewFlipper : jObject;ViewGroup : jObject);
Const
 _cFuncName = 'jViewFlipper_setParent';
 _cFuncSig  = '(Ljava/lang/Object;Landroid/view/ViewGroup;)V';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParams : array[0..1] of jValue;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParams[0].l := ViewFlipper;
 _jParams[1].l := ViewGroup;
 Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
end;

//------------------------------------------------------------------------------
// WebView
//------------------------------------------------------------------------------

//
Function  jWebView_Create  (Const Env : TEnv; pasObj : TObject) : jObject;
 Const
  _cFuncName = 'jWebView_Create';
  _cFuncSig  = '(I)Ljava/lang/Object;';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..0] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := pasObj;
  Result := Env.jEnv^.CallObjectMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
  Result := Env.jEnv^.NewGlobalRef     (Env.jEnv, Result);
 end;

//
Procedure jWebView_Free   (Const Env : TEnv; WebView : jObject);
  Const
   _cFuncName = 'jWebView_Free';
   _cFuncSig  = '(Ljava/lang/Object;)V';
  Var
   _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
   _jParams : jValue;
  begin
   jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
   _jParams.l := WebView;
   Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
   Env.jEnv^.DeleteGlobalRef(Env.jEnv, WebView);
  end;

//
Procedure jWebView_setXYWH(Const Env : TEnv;
                           WebView : jObject;x,y,w,h : integer);
 Const
  _cFuncName = 'jWebView_setXYWH';
  _cFuncSig  = '(Ljava/lang/Object;IIII)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..4] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := WebView;
  _jParams[1].i := x;
  _jParams[2].i := y;
  _jParams[3].i := w;
  _jParams[4].i := h;
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 end;

//
Procedure jWebView_setParent(Const Env : TEnv;
                             WebView : jObject;ViewGroup : jObject);
 Const
  _cFuncName = 'jWebView_setParent';
  _cFuncSig  = '(Ljava/lang/Object;Landroid/view/ViewGroup;)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..1] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := WebView;
  _jParams[1].l := ViewGroup;
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 end;

//
Procedure jWebView_setJavaScript       (Const Env : TEnv;
                                        WebView : jObject; javascript : boolean);
Const
 _cFuncName = 'jWebView_setJavaScript';
 _cFuncSig  = '(Ljava/lang/Object;Z)V';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParams : Array[0..1] of jValue;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParams[0].l := WebView;
 _jParams[1].z := jBool(JavaScript);
 Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
end;

//
Procedure jWebView_loadURL(Const Env : TEnv;
                           WebView : jObject; Str : String);
 Const
  _cFuncName = 'jWebView_loadURL';
  _cFuncSig  = '(Ljava/lang/Object;Ljava/lang/String;)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..1] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := WebView;
  _jParams[1].l := Env.jEnv^.NewStringUTF(Env.jEnv, pchar(Str) );
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
  Env.jEnv^.DeleteLocalRef (Env.jEnv, _jParams[1].l);
 end;

//------------------------------------------------------------------------------
// Canvas
//------------------------------------------------------------------------------

Function  jCanvas_Create               (Const Env : TEnv;
                                        pasObj : TObject) : jObject;
Const
 _cFuncName = 'jCanvas_Create';
 _cFuncSig  = '(I)Ljava/lang/Object;';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParam  : jValue;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParam.l := pasObj;
 Result := Env.jEnv^.CallObjectMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParam);
 Result := Env.jEnv^.NewGlobalRef     (Env.jEnv, Result);
end;

Procedure jCanvas_Free                 (Const Env : TEnv; jCanvas : jObject);
Const
 _cFuncName = 'jCanvas_Free';
 _cFuncSig  = '(Ljava/lang/Object;)V';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParams : jValue;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParams.l := jCanvas;
 Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 Env.jEnv^.DeleteGlobalRef(Env.jEnv, jCanvas);
end;

//
Procedure jCanvas_setStrokeWidth       (Const Env : TEnv;
                                        jCanvas : jObject;width : single);
Const
 _cFuncName = 'jCanvas_setStrokeWidth';
 _cFuncSig  = '(Ljava/lang/Object;F)V';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParams : Array[0..1] of jValue;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParams[0].l := jCanvas;
 _jParams[1].f := width;
 Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
end;

Procedure jCanvas_setStyle             (Const Env : TEnv;
                                        jCanvas : jObject; style : integer);
Const
 _cFuncName = 'jCanvas_setStyle';
 _cFuncSig  = '(Ljava/lang/Object;I)V';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParams : Array[0..1] of jValue;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParams[0].l := jCanvas;
 _jParams[1].i := style;
 Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
end;

Procedure jCanvas_setColor             (Const Env : TEnv;
                                        jCanvas : jObject; color : DWord  );
Const
 _cFuncName = 'jCanvas_setColor';
 _cFuncSig  = '(Ljava/lang/Object;I)V';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParams : Array[0..1] of jValue;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParams[0].l := jCanvas;
 _jParams[1].i := color;
 Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
end;

Procedure jCanvas_setTextSize          (Const Env : TEnv;
                                        jCanvas : jObject; textsize : single );
Const
 _cFuncName = 'jCanvas_setTextSize';
 _cFuncSig  = '(Ljava/lang/Object;F)V';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParams : Array[0..1] of jValue;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParams[0].l := jCanvas;
 _jParams[1].f := textsize;
 Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
end;

Procedure jCanvas_drawLine             (Const Env : TEnv;
                                        jCanvas : jObject; x1,y1,x2,y2 : single);
Const
 _cFuncName = 'jCanvas_drawLine';
 _cFuncSig  = '(Ljava/lang/Object;FFFF)V';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParams : Array[0..4] of jValue;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParams[0].l := jCanvas;
 _jParams[1].F := x1;
 _jParams[2].F := y1;
 _jParams[3].F := x2;
 _jParams[4].F := y2;
 Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
end;

// LORDMAN 2013-08-13
Procedure jCanvas_drawPoint(Const Env : TEnv; jCanvas:jObject; x1,y1:single);
Const
_cFuncName = 'jCanvas_drawPoint';
_cFuncSig  = '(Ljava/lang/Object;FF)V';
Var
_jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
_jParams : Array[0..2] of jValue;
begin
jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
_jParams[0].l := jCanvas;
_jParams[1].F := x1;
_jParams[2].F := y1;
Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
end;

Procedure jCanvas_drawText             (Const Env : TEnv;
                                        jCanvas : jObject; const text : string; x,y : single);
Const
 _cFuncName = 'jCanvas_drawText';
 _cFuncSig  = '(Ljava/lang/Object;Ljava/lang/String;FF)V';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParams : Array[0..3] of jValue;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParams[0].l := jCanvas;
 _jParams[1].l := Env.jEnv^.NewStringUTF(Env.jEnv, pchar(text) );
 _jParams[2].F := x;
 _jParams[3].F := y;
 Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 Env.jEnv^.DeleteLocalRef (Env.jEnv,_jParams[1].l);
end;


Procedure jCanvas_drawBitmap           (Const Env : TEnv;
                                        jCanvas : jObject; bmp : jObject; b,l,r,t : integer);
Const
 _cFuncName = 'jCanvas_drawBitmap';
 _cFuncSig  = '(Ljava/lang/Object;Landroid/graphics/Bitmap;IIII)V';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParams : Array[0..5] of jValue;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParams[0].l := jCanvas;
 _jParams[1].l := bmp;
 _jParams[2].i := b;
 _jParams[3].i := l;
 _jParams[4].i := r;
 _jParams[5].i := t;
 Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
end;

//------------------------------------------------------------------------------
// Bitmap
//------------------------------------------------------------------------------

//
Function  jBitmap_Create  (Const Env : TEnv;
                           pasObj : TObject) : jObject;
 Const
  _cFuncName = 'jBitmap_Create';
  _cFuncSig  = '(I)Ljava/lang/Object;';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParam  : jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParam.l := pasObj;
  Result := Env.jEnv^.CallObjectMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParam);
  Result := Env.jEnv^.NewGlobalRef(Env.jEnv,Result);
 end;

//
Procedure jBitmap_Free   (Const Env : TEnv; jbitmap : jObject);
  Const
   _cFuncName = 'jBitmap_Free';
   _cFuncSig  = '(Ljava/lang/Object;)V';
  Var
   _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
   _jParam  : jValue;
  begin
   jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
   _jParam.l := jbitmap;
   Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParam);
   Env.jEnv^.DeleteGlobalRef(Env.jEnv,jbitmap);
  end;

//
Procedure jBitmap_loadFile(Const Env : TEnv;
                           jbitmap : jObject; filename : String);
 Const
  _cFuncName = 'jBitmap_loadFile';
  _cFuncSig  = '(Ljava/lang/Object;Ljava/lang/String;)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..1] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := jbitmap;
  _jParams[1].l := Env.jEnv^.NewStringUTF(Env.jEnv, pchar(filename) );
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
  Env.jEnv^.DeleteLocalRef (Env.jEnv,_jParams[1].l);
 end;

//
Procedure jBitmap_createBitmap(Const Env : TEnv;
                               jbitmap : jObject; w,h : integer);
 Const
  _cFuncName = 'jBitmap_createBitmap';
  _cFuncSig  = '(Ljava/lang/Object;II)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..2] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := jbitmap;
  _jParams[1].i := w;
  _jParams[2].i := h;
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 end;

//
Procedure jBitmap_getWH(Const Env : TEnv;
                        jbitmap : jObject; var w,h : integer);
 Const
  _cFuncName = 'jBitmap_getWH';
  _cFuncSig  = '(Ljava/lang/Object;)[I';
 Var
  _jMethod   : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParam    : jValue;
  _jIntArray : jintArray;
  _jBoolean  : jBoolean;
  //
  PInt       : PInt32;
  PIntSav    : PInt32;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParam.l  := jbitmap;
  _jIntArray := Env.jEnv^.CallObjectMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParam);
  //
  _jBoolean  := JNI_False;
  PInt       := Env.jEnv^.GetIntArrayElements(Env.jEnv,_jIntArray,_jBoolean);
  PIntSav    := PInt;
  w          := PInt^; Inc(PInt);
  h          := PInt^; Inc(PInt);
  Env.jEnv^.ReleaseIntArrayElements(Env.jEnv,_jIntArray,PIntSav,0);
 end;

Function  jBitmap_getJavaBitmap (Const Env : TEnv;
                                 jbitmap : jObject) : jObject;
Const
 _cFuncName = 'jBitmap_getJavaBitmap';
 _cFuncSig  = '(Ljava/lang/Object;)Landroid/graphics/Bitmap;';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParam  : jValue;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParam.l := jbitmap;
 Result := Env.jEnv^.CallObjectMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParam);
end;

//------------------------------------------------------------------------------
// View
//------------------------------------------------------------------------------

//
Function  jView_Create  (Const Env : TEnv; pasObj : TObject) : jObject;
 Const
  _cFuncName = 'jView_Create';
  _cFuncSig  = '(I)Ljava/lang/Object;';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..0] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := pasObj;
  Result := Env.jEnv^.CallObjectMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
  Result := Env.jEnv^.NewGlobalRef     (Env.jEnv, Result);
 end;

//
Procedure jView_Free   (Const Env : TEnv; View : jObject);
  Const
   _cFuncName = 'jView_Free';
   _cFuncSig  = '(Ljava/lang/Object;)V';
  Var
   _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
   _jParams : jValue;
  begin
   jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
   _jParams.l := View;
   Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
   Env.jEnv^.DeleteGlobalRef(Env.jEnv, View);
  end;

//
Procedure jView_setXYWH(Const Env : TEnv;
                        View : jObject;x,y,w,h : integer);
 Const
  _cFuncName = 'jView_setXYWH';
  _cFuncSig  = '(Ljava/lang/Object;IIII)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..4] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := View;
  _jParams[1].i := x;
  _jParams[2].i := y;
  _jParams[3].i := w;
  _jParams[4].i := h;
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 end;

//
Procedure jView_setParent(Const Env : TEnv; View : jObject;ViewGroup : jObject);
 Const
  _cFuncName = 'jView_setParent';
  _cFuncSig  = '(Ljava/lang/Object;Landroid/view/ViewGroup;)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..1] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := View;
  _jParams[1].l := ViewGroup;
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 end;

//
Procedure jView_setjCanvas(Const Env : TEnv; View : jObject;jCanvas : jObject);
 Const
  _cFuncName = 'jView_setjCanvas';
  _cFuncSig  = '(Ljava/lang/Object;Ljava/lang/Object;)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..1] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := View;
  _jParams[1].l := jCanvas;
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 end;

// LORDMAN  2013-08-14
// Simonsyz 2014.02.25
Procedure jView_SaveToFile (Const Env : TEnv; View : jObject; Filename : String);
Const
 _cFuncName = 'jView_saveView';
 _cFuncSig  = '(Ljava/lang/Object;Ljava/lang/String;)V';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParams : Array[0..1] of jValue;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParams[0].l := View;
 _jParams[1].l := Env.jEnv^.NewStringUTF(Env.jEnv, pchar(Filename) );
 Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 Env.jEnv^.DeleteLocalRef (Env.jEnv,_jParams[1].l);
end;

//------------------------------------------------------------------------------
// jGLSurfaceView
//------------------------------------------------------------------------------

Function  jGLSurfaceView_Create(Const Env : TEnv; pasObj : TObject; version : TGLES_Version) : jObject;
 Const
  _cFuncName = 'jGLSurfaceView_Create';
  _cFuncSig  = '(II)Ljava/lang/Object;';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..1] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := pasObj;
  _jParams[1].i := integer(version);
  Result := Env.jEnv^.CallObjectMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
  Result := Env.jEnv^.NewGlobalRef     (Env.jEnv, Result);
 end;

//
Procedure jGLSurfaceView_Free   (Const Env : TEnv; GLSurfaceView : jObject);
  Const
   _cFuncName = 'jGLSurfaceView_Free';
   _cFuncSig  = '(Ljava/lang/Object;)V';
  Var
   _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
   _jParams : jValue;
  begin
   jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
   _jParams.l := GLSurfaceView;
   Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
   Env.jEnv^.DeleteGlobalRef(Env.jEnv, GLSurfaceView);
  end;

//
Procedure jGLSurfaceView_setXYWH(Const Env : TEnv;
                                 GLSurfaceView : jObject;x,y,w,h : integer);
 Const
  _cFuncName = 'jGLSurfaceView_setXYWH';
  _cFuncSig  = '(Ljava/lang/Object;IIII)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..4] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := GLSurfaceView;
  _jParams[1].i := x;
  _jParams[2].i := y;
  _jParams[3].i := w;
  _jParams[4].i := h;
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 end;

//
Procedure jGLSurfaceView_setParent(Const Env : TEnv;
                                   GLSurfaceView : jObject;ViewGroup : jObject);
 Const
  _cFuncName = 'jGLSurfaceView_setParent';
  _cFuncSig  = '(Ljava/lang/Object;Landroid/view/ViewGroup;)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..1] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := GLSurfaceView;
  _jParams[1].l := ViewGroup;
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 end;

Procedure jGLSurfaceView_SetAutoRefresh(Const Env : TEnv; glView : jObject; Active : Boolean);
 Const
  _cFuncName = 'jGLSurfaceView_SetAutoRefresh';
  _cFuncSig  = '(Ljava/lang/Object;Z)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..1] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := glView;
  _jParams[1].z := jBool(Active);
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 end;

Procedure jGLSurfaceView_Refresh(Const Env : TEnv; glView : jObject);
 Const
  _cFuncName = 'jGLSurfaceView_Refresh';
  _cFuncSig  = '(Ljava/lang/Object;)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParam  : jValue;
 begin
  Env.jVM^.AttachCurrentThread(Env.jVM,@Env.jEnv,nil);
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParam.l := glView;
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParam);
 end;

Procedure jGLSurfaceView_deleteTexture(Const Env : TEnv; glView : jObject; id : Integer);
 Const
  _cFuncName = 'jGLSurfaceView_deleteTexture';
  _cFuncSig  = '(Ljava/lang/Object;I)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : Array[0..1] of jValue;
 begin
  Env.jVM^.AttachCurrentThread(Env.jVM,@Env.jEnv,nil);
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := glView;
  _jParams[1].i := id;
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 end;

Procedure jGLSurfaceView_getBmpArray(Const Env : TEnv;filename : String);
 Const
  _cFuncName = 'getBmpArray';
  _cFuncSig  = '(Ljava/lang/String;)[I';
 Var
  _jMethod   : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParam    : jValue;
  _jIntArray : jintArray;
  _jBoolean  : jBoolean;
  //
  Size : Integer;
  PInt : PInt32;
  PIntS: PInt32;
  i    : Integer;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParam.l  := Env.jEnv^.NewStringUTF( Env.jEnv, pchar(filename));
  _jIntArray := Env.jEnv^.CallObjectMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParam);
  Env.jEnv^.DeleteLocalRef(Env.jEnv,_jParam.l);
  Size := Env.jEnv^.GetArrayLength(Env.jEnv,_jIntArray);
  jLog('Size: ' + IntToStr(Size) );
  _jBoolean  := JNI_False;
  PInt := Env.jEnv^.GetIntArrayElements(Env.jEnv,_jIntArray,_jBoolean);
  PIntS:= PInt;
  Inc(PIntS,Size-2);
  jLog('width:'  + IntToStr(PintS^)); Inc(PintS);
  jLog('height:' + IntToStr(PintS^));
  Env.jEnv^.ReleaseIntArrayElements(Env.jEnv,_jIntArray,PInt,0);
  jLog('Here...');
 end;

Procedure jGLSurfaceView_requestGLThread(Const Env : TEnv; glView : jObject);
 Const
  _cFuncName = 'jGLSurfaceView_glThread';
  _cFuncSig  = '(Ljava/lang/Object;)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParam  : jValue;
 begin
  Env.jVM^.AttachCurrentThread(Env.jVM,@Env.jEnv,nil);
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParam.l := glView;
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParam);
 end;

//------------------------------------------------------------------------------
// jCameraView
//------------------------------------------------------------------------------

//
Function  jCameraView_Create(Const Env : TEnv; pasObj : TObject) : jObject;
 Const
  _cFuncName = 'jCameraView_Create';
  _cFuncSig  = '(I)Ljava/lang/Object;';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..0] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := pasObj;
  Result := Env.jEnv^.CallObjectMethodA(Env.jEnv,Env.jControls,_jMethod,@_jParams);
  Result := Env.jEnv^.NewGlobalRef     (Env.jEnv,Result);
 end;

//
Procedure jCameraView_Free   (Const Env : TEnv; SurfaceView : jObject);
 Const
  _cFuncName = 'jCameraView_Free';
  _cFuncSig  = '(Ljava/lang/Object;)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams.l := SurfaceView;
  Env.jEnv^.CallVoidMethodA(Env.jEnv,Env.jControls,_jMethod,@_jParams);
  Env.jEnv^.DeleteGlobalRef(Env.jEnv,SurfaceView);
 end;

//
Procedure jCameraView_setXYWH(Const Env : TEnv; SurfaceView : jObject;x,y,w,h : integer);
 Const
  _cFuncName = 'jCameraView_setXYWH';
  _cFuncSig  = '(Ljava/lang/Object;IIII)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..4] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := SurfaceView;
  _jParams[1].i := x;
  _jParams[2].i := y;
  _jParams[3].i := w;
  _jParams[4].i := h;
  Env.jEnv^.CallVoidMethodA(Env.jEnv,Env.jControls,_jMethod,@_jParams);
 end;

//
Procedure jCameraView_setParent(Const Env : TEnv; SurfaceView : jObject;ViewGroup : jObject);
 Const
  _cFuncName = 'jCameraView_setParent';
  _cFuncSig  = '(Ljava/lang/Object;Landroid/view/ViewGroup;)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..1] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := SurfaceView;
  _jParams[1].l := ViewGroup;
  Env.jEnv^.CallVoidMethodA(Env.jEnv,Env.jControls,_jMethod,@_jParams);
 end;

//
Procedure jCameraView_saveImage (Const Env : TEnv; SurfaceView : jObject; fileName : String);
 Const
  _cFuncName = 'jCameraView_saveImage';
  _cFuncSig  = '(Ljava/lang/Object;Ljava/lang/String;)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : Array[0..1] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := SurfaceView;
  _jParams[1].l := Env.jEnv^.NewStringUTF(Env.jEnv, pchar(fileName) );
  Env.jEnv^.CallVoidMethodA(Env.jEnv,Env.jControls,_jMethod,@_jParams);
  Env.jEnv^.DeleteLocalRef (Env.jEnv,_jParams[1].l);
 end;

//------------------------------------------------------------------------------
// Timer
//------------------------------------------------------------------------------

Function jTimer_Create (Const Env : TEnv; pasObj : TObject): jObject;
 Const
  _cFuncName = 'jTimer_Create';
  _cFuncSig  = '(I)Ljava/lang/Object;';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParam  : jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParam.l := pasObj;
  Result    := Env.jEnv^.CallObjectMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParam);
  Result    := Env.jEnv^.NewGlobalRef     (Env.jEnv,Result);
 end;

//
Procedure jTimer_Free   (Const Env : TEnv; Timer : jObject);
  Const
   _cFuncName = 'jTimer_Free';
   _cFuncSig  = '(Ljava/lang/Object;)V';
  Var
   _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
   _jParams : jValue;
  begin
   jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
   _jParams.l := Timer;
   Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
   Env.jEnv^.DeleteGlobalRef(Env.jEnv, Timer);
  end;

Procedure jTimer_SetInterval(Const Env : TEnv; Timer  : jObject; Interval : Integer);
 Const
  _cFuncName = 'jTimer_SetInterval';
  _cFuncSig  = '(Ljava/lang/Object;I)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : Array[0..1] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := Timer;
  _jParams[1].i := Interval;
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 end;

Procedure jTimer_SetEnabled (Const Env : TEnv; Timer  : jObject; Active : Boolean);
 Const
  _cFuncName = 'jTimer_SetEnabled';
  _cFuncSig  = '(Ljava/lang/Object;Z)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : Array[0..1] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := Timer;
  _jParams[1].z := jBool(Active);
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 end;

//------------------------------------------------------------------------------
// jDialog YN
//------------------------------------------------------------------------------

Function jDialogYN_Create (Const Env : TEnv; pasObj : TObject;
                           title,msg,y,n : string ): jObject;
 Const
  _cFuncName = 'jDialogYN_Create';
  _cFuncSig  = '(ILjava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)Ljava/lang/Object;';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : Array[0..4] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := pasObj;
  _jParams[1].l := Env.jEnv^.NewStringUTF(Env.jEnv, pchar(title) );
  _jParams[2].l := Env.jEnv^.NewStringUTF(Env.jEnv, pchar(Msg  ) );
  _jParams[3].l := Env.jEnv^.NewStringUTF(Env.jEnv, pchar(y    ) );
  _jParams[4].l := Env.jEnv^.NewStringUTF(Env.jEnv, pchar(n    ) );
  Result := Env.jEnv^.CallObjectMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
  Result := Env.jEnv^.NewGlobalRef(Env.jEnv,Result);
  Env.jEnv^.DeleteLocalRef(Env.jEnv,_jParams[1].l);
  Env.jEnv^.DeleteLocalRef(Env.jEnv,_jParams[2].l);
  Env.jEnv^.DeleteLocalRef(Env.jEnv,_jParams[3].l);
  Env.jEnv^.DeleteLocalRef(Env.jEnv,_jParams[4].l);
 end;

//
Procedure jDialogYN_Free   (Const Env : TEnv; DialogYN : jObject);
  Const
   _cFuncName = 'jDialogYN_Free';
   _cFuncSig  = '(Ljava/lang/Object;)V';
  Var
   _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
   _jParams : jValue;
  begin
   jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
   _jParams.l := DialogYN;
   Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
   Env.jEnv^.DeleteGlobalRef(Env.jEnv, DialogYN);
  end;

Procedure jDialogYN_Show (Const Env : TEnv; DialogYN: jObject);
Const
 _cFuncName = 'jDialogYN_Show';
 _cFuncSig  = '(Ljava/lang/Object;)V';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParam  : jValue;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParam.l := DialogYN;
 Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParam);
end;

//------------------------------------------------------------------------------
// jDialog Progress
//------------------------------------------------------------------------------

Function jDialogProgress_Create (Const Env : TEnv; pasObj : TObject;
                                 title,msg : string ): jObject;
 Const
  _cFuncName = 'jDialogProgress_Create';
  _cFuncSig  = '(ILjava/lang/String;Ljava/lang/String;)Ljava/lang/Object;';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : Array[0..2] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := pasObj;
  _jParams[1].l := Env.jEnv^.NewStringUTF(Env.jEnv, pchar(title) );
  _jParams[2].l := Env.jEnv^.NewStringUTF(Env.jEnv, pchar(Msg  ) );
  Result := Env.jEnv^.CallObjectMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
  Result := Env.jEnv^.NewGlobalRef(Env.jEnv,Result);
  Env.jEnv^.DeleteLocalRef(Env.jEnv,_jParams[1].l);
  Env.jEnv^.DeleteLocalRef(Env.jEnv,_jParams[2].l);
 end;

//
Procedure jDialogProgress_Free (Const Env : TEnv; DialogProgress : jObject);
 Const
  _cFuncName = 'jDialogProgress_Free';
  _cFuncSig  = '(Ljava/lang/Object;)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParam  : jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParam.l  := DialogProgress;
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParam);
  Env.jEnv^.DeleteGlobalRef(Env.jEnv, DialogProgress);
 end;

//------------------------------------------------------------------------------
// MessageBox , Dialog
//------------------------------------------------------------------------------

Procedure jToast (Const Env : TEnv; Str : String);
 Const
  _cFuncName = 'jToast';
  _cFuncSig  = '(Ljava/lang/String;)V';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParam  : jValue;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParam.l := Env.jEnv^.NewStringUTF(Env.jEnv, pchar(Str) );
 Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParam);
 Env.jEnv^.DeleteLocalRef (Env.jEnv,_jParam.l);
end;

//------------------------------------------------------------------------------
// jImageBtn
//------------------------------------------------------------------------------

//
Function  jImageBtn_Create  (Const Env : TEnv; pasObj : TObject) : jObject;
 Const
  _cFuncName = 'jImageBtn_Create';
  _cFuncSig  = '(I)Ljava/lang/Object;';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..0] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := pasObj;
  Result := Env.jEnv^.CallObjectMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
  Result := Env.jEnv^.NewGlobalRef     (Env.jEnv, Result);
 end;

//
Procedure jImageBtn_Free   (Const Env : TEnv; ImageBtn : jObject);
  Const
   _cFuncName = 'jImageBtn_Free';
   _cFuncSig  = '(Ljava/lang/Object;)V';
  Var
   _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
   _jParams : jValue;
  begin
   jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
   _jParams.l := ImageBtn;
   Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
   Env.jEnv^.DeleteGlobalRef(Env.jEnv, ImageBtn);
  end;

//
Procedure jImageBtn_setXYWH(Const Env : TEnv;
                            ImageBtn : jObject;x,y,w,h : integer);
 Const
  _cFuncName = 'jImageBtn_setXYWH';
  _cFuncSig  = '(Ljava/lang/Object;IIII)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..4] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := ImageBtn;
  _jParams[1].i := x;
  _jParams[2].i := y;
  _jParams[3].i := w;
  _jParams[4].i := h;
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 end;

//
Procedure jImageBtn_setParent(Const Env : TEnv;
                               ImageBtn : jObject;ViewGroup : jObject);
 Const
  _cFuncName = 'jImageBtn_setParent';
  _cFuncSig  = '(Ljava/lang/Object;Landroid/view/ViewGroup;)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..1] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := ImageBtn;
  _jParams[1].l := ViewGroup;
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 end;

//
Procedure jImageBtn_setButton(Const Env : TEnv;
                              ImageBtn: jObject; up,dn : string);
 Const
  _cFuncName = 'jImageBtn_setButton';
  _cFuncSig  = '(Ljava/lang/Object;Ljava/lang/String;Ljava/lang/String;)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : array[0..2] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := ImageBtn;
  _jParams[1].l := Env.jEnv^.NewStringUTF(Env.jEnv, pchar(up) );
  _jParams[2].l := Env.jEnv^.NewStringUTF(Env.jEnv, pchar(dn) );
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
  Env.jEnv^.DeleteLocalRef (Env.jEnv,_jParams[1].l);
  Env.jEnv^.DeleteLocalRef (Env.jEnv,_jParams[2].l);
 end;

// LORDMAN 2013-08-16
Procedure jImageBtn_SetEnabled (Const Env : TEnv;
                                ImageBtn : jObject; Active   : Boolean);
 Const
  _cFuncName = 'jImageBtn_setEnabled';
  _cFuncSig  = '(Ljava/lang/Object;Z)V';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParams : Array[0..1] of jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParams[0].l := ImageBtn;
  _jParams[1].z := jBool(Active);
  Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
 end;

//------------------------------------------------------------------------------
// jAsyncTask
//------------------------------------------------------------------------------

//
Function  jAsyncTask_Create  (Const Env : TEnv; pasObj : TObject) : jObject;
 Const
  _cFuncName = 'jAsyncTask_Create';
  _cFuncSig  = '(I)Ljava/lang/Object;';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParam  : jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParam.l := pasObj;
  Result := Env.jEnv^.CallObjectMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParam);
  Result := Env.jEnv^.NewGlobalRef     (Env.jEnv,Result);
  jLog('jAsyncTask_Create');
 end;

//
Procedure jAsyncTask_Free   (Const Env : TEnv; AsyncTask : jObject);
  Const
   _cFuncName = 'jAsyncTask_Free';
   _cFuncSig  = '(Ljava/lang/Object;)V';
  Var
   _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
   _jParams : jValue;
  begin
   jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
   _jParams.l := AsyncTask;
   Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
   Env.jEnv^.DeleteGlobalRef(Env.jEnv, AsyncTask);
   jLog('jAsyncTask_Free');
  end;

//
Procedure jAsyncTask_Execute(Const Env : TEnv; AsyncTask : jObject);
  Const
   _cFuncName = 'jAsyncTask_Execute';
   _cFuncSig  = '(Ljava/lang/Object;)V';
  Var
   _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
   _jParams : jValue;
  begin
   jLog('jAsyncTask_Execute');
   jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
   _jParams.l := AsyncTask;
   Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
  end;

// Warning : AsyncTask is thread,
//           Env.jEnv = AsyncTask's jEnv / Not App.jEnv
//           AsyncTask's jEnv is updated by "Java_Event_pOnAsyncEvent"
Procedure jAsyncTask_setProgress(Const Env : TEnv; AsyncTask : jObject;Progress : Integer);
  Const
   _cFuncName = 'jAsyncTask_setProgress';
   _cFuncSig  = '(Ljava/lang/Object;I)V';
  Var
   _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
   _jParams : Array[0..1] of jValue;
  begin
   jLog('jAsyncTask_setProgress');
   Env.jVM^.AttachCurrentThread(Env.jVM,@Env.jEnv,nil);
   jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
   _jParams[0].l := AsyncTask;
   _jParams[1].i := progress;
   Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
  end;

//------------------------------------------------------------------------------
// jHttp
//------------------------------------------------------------------------------

//
Function  jHttp_Create  (Const Env : TEnv; pasObj : TObject) : jObject;
 Const
  _cFuncName = 'jHttp_Create';
  _cFuncSig  = '(I)Ljava/lang/Object;';
 Var
  _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
  _jParam  : jValue;
 begin
  jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
  _jParam.l := pasObj;
  Result := Env.jEnv^.CallObjectMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParam);
  Result := Env.jEnv^.NewGlobalRef     (Env.jEnv,Result);
  jLog('jHttp_Create');
 end;

//
Procedure jHttp_Free   (Const Env : TEnv; Http : jObject);
  Const
   _cFuncName = 'jHttp_Free';
   _cFuncSig  = '(Ljava/lang/Object;)V';
  Var
   _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
   _jParams : jValue;
  begin
   jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
   _jParams.l := Http;
   Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
   Env.jEnv^.DeleteGlobalRef(Env.jEnv, Http);
   jLog('jHttp_Free');
  end;

//
Procedure jHttp_getText(Const Env : TEnv; Http : jObject; url : String);
  Const
   _cFuncName = 'jHttp_getText';
   _cFuncSig  = '(Ljava/lang/Object;Ljava/lang/String;)V';
  Var
   _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
   _jParams : array[0..1] of jValue;
  begin
   jLog('jHttp_getText');
   jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
   _jParams[0].l := Http;
   _jParams[1].l := Env.jEnv^.NewStringUTF(Env.jEnv, pchar(url) );
   Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
   Env.jEnv^.DeleteLocalRef(Env.jEnv,_jParams[1].l);
  end;

//
Procedure jHttp_DownloadFile(Const Env : TEnv; Http : jObject; url : String; localfile : String);
  Const
   _cFuncName = 'jHttp_downloadFile';
   _cFuncSig  = '(Ljava/lang/Object;Ljava/lang/String;Ljava/lang/String;)V';
  Var
   _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
   _jParams : array[0..2] of jValue;
  begin
   jLog('jHttp_downloadFile');
   jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
   _jParams[0].l := Http;
   _jParams[1].l := Env.jEnv^.NewStringUTF(Env.jEnv, pchar(url      ) );
   _jParams[2].l := Env.jEnv^.NewStringUTF(Env.jEnv, pchar(localFile) );
   Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
   Env.jEnv^.DeleteLocalRef(Env.jEnv,_jParams[1].l);
   Env.jEnv^.DeleteLocalRef(Env.jEnv,_jParams[2].l);
  end;

//
Procedure jHttp_UploadFile(Const Env : TEnv; Http : jObject; url : String; localfile : String);
  Const
   _cFuncName = 'jHttp_uploadFile';
   _cFuncSig  = '(Ljava/lang/Object;Ljava/lang/String;Ljava/lang/String;)V';
  Var
   _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
   _jParams : array[0..2] of jValue;
  begin
   jLog('jHttp_uploadFile');
   jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
   _jParams[0].l := Http;
   _jParams[1].l := Env.jEnv^.NewStringUTF(Env.jEnv, pchar(url      ) );
   _jParams[2].l := Env.jEnv^.NewStringUTF(Env.jEnv, pchar(localFile) );
   Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParams);
   Env.jEnv^.DeleteLocalRef(Env.jEnv,_jParams[1].l);
   Env.jEnv^.DeleteLocalRef(Env.jEnv,_jParams[2].l);
  end;

//------------------------------------------------------------------------------
// jTakePhoto
//------------------------------------------------------------------------------
Procedure jTakePhoto(Const Env : TEnv; FileName : String);
Const
 _cFuncName = 'takePhoto';
 _cFuncSig  = '(Ljava/lang/String;)V';
Var
 _jMethod : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jParam  : jValue;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jParam.l  := Env.jEnv^.NewStringUTF(Env.jEnv, pchar(filename) );
 Env.jEnv^.CallVoidMethodA(Env.jEnv, Env.jControls,_jMethod,@_jParam);
 Env.jEnv^.DeleteLocalRef (Env.jEnv,_jParam.l);
end;

//------------------------------------------------------------------------------
// jBenchMark
//------------------------------------------------------------------------------

Procedure jBenchMark_Java  (Const Env : TEnv;var mSec : Integer;var value : single);
Const
 _cFuncName = 'benchMark';
 _cFuncSig  = '()[F';
Var
 _jMethod      : jMethodID {$IFDef FPC} = nil {$EndIf};
 _jFloatArray  : jFloatArray;
 _jBoolean     : jBoolean;
 PFloat        : PSingle;
 PFloatSav     : PSingle;
begin
 jGetMethodID(Env,_cFuncName,_cFuncSig,_jMethod);
 _jFloatArray := Env.jEnv^.CallObjectMethod(Env.jEnv, Env.jControls,_jMethod);
 _jBoolean    := JNI_False;
 PFloat       := Env.jEnv^.GetFloatArrayElements(Env.jEnv,_jFloatArray,_jBoolean);
 PFloatSav    := PFloat;
 mSec         := Round(PFloat^); Inc(PFloat);
 value        := Round(PFloat^); Inc(PFloat);
 Env.jEnv^.ReleaseFloatArrayElements(Env.jEnv,_jFloatArray,PFloatSav,0);
end;

Procedure jBenchMark_Pascal (Const Env : TEnv;var mSec : Integer;var value : single);
 Var
  StartTime,EndTime : LongInt;
  i                 : Integer;
 begin
  StartTime := jSystem_GetTick(Env);
  value     := 30;
  i         := 0;
  for i := 0 to 100000000-1 do
   value := value * 2/ 3 + 5 - 1;
  EndTime   := jSystem_GetTick(Env);
  jLog( IntToStr( EndTime - StartTime) + ' Result:' + FloatToStr(value) );
  //
  mSec := EndTime - StartTime;
 end;

end.

