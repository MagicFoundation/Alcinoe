unit Alcinoe.AndroidApi.Widget;

interface

{$I Alcinoe.inc}

uses
  Androidapi.JNI.Widget,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Util,
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes;

type

  {**************************************}
  JALDatePickerDialogListener = interface;
  JALDatePickerDialog = interface;
  JALKeyPreImeListener = interface;
  JALEditText = interface;
  JProgressBar = interface;
  JStyleSpan = interface;

  JStyleSpanClass = interface(JObjectClass)
    ['{72E179F0-032C-4D92-86A8-4498E8070101}']
    {class} function init(style: Integer): JStyleSpan; cdecl;
  end;
  [JavaSignature('android/text/style/StyleSpan')]
  JStyleSpan = interface(JObject)
    ['{72E179F0-032C-4D92-86A8-4498E8070102}']
  end;
  TJStyleSpan = class(TJavaGenericImport<JStyleSpanClass, JStyleSpan>) end;

  {*****************************************}
  JProgressBarClass = interface(JViewClass)
    ['{51D8E9CA-DB21-4741-9A62-9E2BEEB3C731}']
    {class} function init(context: JContext): JProgressBar; cdecl;
  end;
  [JavaSignature('android/widget/ProgressBar')]
  JProgressBar = interface(JView)
    ['{A498BB20-0336-4180-A7CF-AC26AC54648E}']
    procedure setIndeterminate(indeterminate: Boolean); cdecl;
    function getIndeterminateDrawable: JDrawable; cdecl;
  end;
  TJProgressBar = class(TJavaGenericImport<JProgressBarClass, JProgressBar>) end;

  {******************************************************}
  JALDatePickerDialogListenerClass = interface(IJavaClass)
    ['{3EDC638B-74FD-40D8-A09C-B92919C9D85B}']
  end;
  [JavaSignature('io/magicfoundation/alcinoe/datepicker/ALDatePickerDialogListener')]
  JALDatePickerDialogListener = interface(IJavaInstance)
    ['{9A145783-462B-4E51-AAFC-48F68C79C3EA}']
    procedure onBtnClick(which: integer; year: integer; month: integer; dayOfMonth: integer); cdecl;
  end;
  TJALDatePickerDialogListener = class(TJavaGenericImport<JALDatePickerDialogListenerClass, JALDatePickerDialogListener>) end;

  {************************************************}
  JALDatePickerDialogClass = interface(JObjectClass)
    ['{5CBB555C-9128-492E-BFE9-B0B6AE42F26B}']
    {class} function init(
                       context: JContext;
                       button_positive_text: JCharSequence;
                       button_negative_text: JCharSequence;
                       button_neutral_text: JCharSequence;
                       title: JCharSequence): JALDatePickerDialog; cdecl;
  end;
  [JavaSignature('io/magicfoundation/alcinoe/datepicker/ALDatePickerDialog')]
  JALDatePickerDialog = interface(JObject)
    ['{DF4E7117-15AA-4063-9150-EEEC2356FCD7}']
    procedure setListener(listener: JALDatePickerDialogListener); cdecl;
    procedure setMinDate(
                year: integer;
                month: integer;
                dayOfMonth: integer); cdecl;
    procedure setMaxDate(
                year: integer;
                month: integer;
                dayOfMonth: integer); cdecl;
    procedure show(
                year: integer;
                month: integer;
                dayOfMonth: integer); cdecl;
  end;
  TJALDatePickerDialog = class(TJavaGenericImport<JALDatePickerDialogClass, JALDatePickerDialog>) end;

  {***********************************************}
  JALKeyPreImeListenerClass = interface(IJavaClass)
    ['{E01C70E2-4BBF-47CB-8713-5A73344E9EA9}']
  end;
  [JavaSignature('io/magicfoundation/alcinoe/edittext/ALKeyPreImeListener')]
  JALKeyPreImeListener = interface(IJavaInstance)
    ['{343578E2-962A-461E-ADD7-47A1E4BAA1D9}']
    function onKeyPreIme(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
  end;
  TJALKeyPreImeListener = class(TJavaGenericImport<JALKeyPreImeListenerClass, JALKeyPreImeListener>) end;

  {******************************************}
  JALEditTextClass = interface(JEditTextClass)
    ['{1969D8DA-0870-47A7-8F4D-E556BC10BB41}']
    {class} function init(context: JContext): JALEditText; cdecl; overload;
    {class} function init(context: JContext; attrs: JAttributeSet): JALEditText; cdecl; overload;
    {class} function init(context: JContext; attrs: JAttributeSet; defStyleAttr: Integer): JALEditText; cdecl; overload;
    {class} function init(context: JContext; attrs: JAttributeSet; defStyleAttr: Integer; defStyleRes: Integer): JALEditText; cdecl; overload;
  end;
  [JavaSignature('io/magicfoundation/alcinoe/edittext/ALEditText')]
  JALEditText = interface(JEditText)
    ['{A3E765A1-44EB-45C0-9AA5-19A38C029CE5}']
    procedure setKeyPreImeListener(listener: JALKeyPreImeListener); cdecl;
    procedure setMaxLength(value: integer); cdecl;
  end;
  TJALEditText = class(TJavaGenericImport<JALEditTextClass, JALEditText>) end;

implementation

uses
  Alcinoe.Common;

{**********************}
procedure RegisterTypes;
begin
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Widget.JStyleSpan', TypeInfo(Alcinoe.AndroidApi.Widget.JStyleSpan));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Widget.JProgressBar', TypeInfo(Alcinoe.AndroidApi.Widget.JProgressBar));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Widget.JALDatePickerDialogListener', TypeInfo(Alcinoe.AndroidApi.Widget.JALDatePickerDialogListener));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Widget.JALDatePickerDialog', TypeInfo(Alcinoe.AndroidApi.Widget.JALDatePickerDialog));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Widget.JALKeyPreImeListener', TypeInfo(Alcinoe.AndroidApi.Widget.JALKeyPreImeListener));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Widget.JALEditText', TypeInfo(Alcinoe.AndroidApi.Widget.JALEditText));
end;

initialization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.AndroidApi.Widget','initialization');
  {$ENDIF}
  RegisterTypes;

end.