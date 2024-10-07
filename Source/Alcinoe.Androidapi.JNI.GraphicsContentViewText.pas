unit Alcinoe.Androidapi.JNI.GraphicsContentViewText;

interface

{$I Alcinoe.inc}

uses
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Hardware,
  Androidapi.JNIBridge;

type

  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if https://quality.embarcadero.com/browse/RSP-44100 has been resolved. If resolved, remove the class definition below.'}
  {$ENDIF}
  JALBitmapClass = interface(JBitmapClass)
    ['{3CB7580B-2DE1-4F79-A5EC-18BAD23884E4}']
    {class} function wrapHardwareBuffer(hardwareBuffer: JHardwareBuffer; colorSpace: JColorSpace): JBitmap; cdecl; //https://quality.embarcadero.com/browse/RSP-44100
  end;
  [JavaSignature('android/graphics/Bitmap')]
  JALBitmap = interface(JBitmap)
    ['{FF2C1C70-F5E1-44AC-A8C6-4326BAA4440B}']
    function getHardwareBuffer: JHardwareBuffer; cdecl;
  end;
  TJALBitmap = class(TJavaGenericImport<JALBitmapClass, JALBitmap>) end; //https://quality.embarcadero.com/browse/RSP-44100


  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if https://quality.embarcadero.com/browse/RSP-44102 has been resolved. If resolved, remove the class definition below.'}
  {$ENDIF}
  JALRecordingCanvasClass = interface(JRecordingCanvasClass)
    ['{BA2A5ADF-60C8-4E8F-A73B-766400D4AAB5}']
  end;
  [JavaSignature('android/graphics/RecordingCanvas')]
  JALRecordingCanvas = interface(JRecordingCanvas)
    ['{F8DABC57-5411-4383-8AA8-22EF9654DC67}']
    procedure drawBitmap(bitmap: JBitmap; matrix: JMatrix; paint: JPaint); cdecl; overload; //https://quality.embarcadero.com/browse/RSP-44102
    procedure drawBitmap(bitmap: JBitmap; src: JRect; dst: JRect; paint: JPaint); cdecl; overload; //https://quality.embarcadero.com/browse/RSP-44102
    procedure drawBitmap(bitmap: JBitmap; left: Single; top: Single; paint: JPaint); cdecl; overload; //https://quality.embarcadero.com/browse/RSP-44102
    procedure drawBitmap(bitmap: JBitmap; src: JRect; dst: JRectF; paint: JPaint); cdecl; overload; //https://quality.embarcadero.com/browse/RSP-44102
  end;
  TJALRecordingCanvas = class(TJavaGenericImport<JALRecordingCanvasClass, JALRecordingCanvas>) end;

implementation

end.
