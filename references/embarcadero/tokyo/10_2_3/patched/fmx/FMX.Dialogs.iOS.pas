{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Dialogs.iOS;

interface

{$SCOPEDENUMS ON}

implementation

uses
  System.Classes, System.Types, System.UITypes, System.Generics.Collections, System.SysUtils, System.Math,
  Macapi.Helpers, Macapi.ObjectiveC, iOSapi.UIKit, iOSapi.CocoaTypes, iOSapi.Foundation, FMX.Platform, FMX.Forms,
  FMX.Dialogs, FMX.Consts, fmx.types;

type

  TFMXAlertViewInputDelegate = class;

  TCocoaDialogsService = class(TInterfacedObject, IFMXDialogService, IFMXDialogServiceSync, IFMXDialogServiceAsync)
  strict private
    class var FCurrent: TCocoaDialogsService;
    class function GetCurrent: TCocoaDialogsService; static;
  private
    FInputDelegates: TList<TFMXAlertViewInputDelegate>;
    constructor Create;
    destructor Destroy; override;
  protected
    class function CreateAlertView(const ACaption: string; const APrompts, ADefaultValues: array of string;
      const Delegate: TFMXAlertViewInputDelegate): UIAlertView;
  public
    class destructor UnInitialize;
    class property Current: TCocoaDialogsService read GetCurrent;
    { IFMXDialogService }
    function DialogOpenFiles(const ADialog: TOpenDialog; var AFiles: TStrings; AType: TDialogType): Boolean;
    function DialogPrint(var ACollate, APrintToFile: Boolean;
      var AFromPage, AToPage, ACopies: Integer; AMinPage, AMaxPage: Integer; var APrintRange: TPrintRange;
      AOptions: TPrintDialogOptions): Boolean;
    function PageSetupGetDefaults(var AMargin, AMinMargin: TRect; var APaperSize: TPointF;
      AUnits: TPageMeasureUnits; AOptions: TPageSetupDialogOptions): Boolean;
    function DialogPageSetup(var AMargin, AMinMargin: TRect; var APaperSize: TPointF; var AUnits: TPageMeasureUnits;
      AOptions: TPageSetupDialogOptions): Boolean;
    function DialogSaveFiles(const ADialog: TOpenDialog; var AFiles: TStrings): Boolean;
    function DialogPrinterSetup: Boolean;
    function MessageDialog(const AMessage: string; const ADialogType: TMsgDlgType; const AButtons: TMsgDlgButtons;
	    const ADefaultButton: TMsgDlgBtn; const AX, AY: Integer; const AHelpCtx: THelpContext;
      const AHelpFileName: string): Integer; overload;
    procedure MessageDialog(const AMessage: string; const ADialogType: TMsgDlgType; const AButtons: TMsgDlgButtons;
      const ADefaultButton: TMsgDlgBtn; const AX, AY: Integer; const AHelpCtx: THelpContext; const AHelpFileName: string;
      const ACloseDialogProc: TInputCloseDialogProc); overload;
    function InputQuery(const ACaption: string; const APrompts: array of string;
	    var AValues: array of string; const ACloseQueryFunc: TInputCloseQueryFunc = nil): Boolean; overload;
    procedure InputQuery(const ACaption: string; const APrompts, ADefaultValues: array of string;
	    const ACloseQueryProc: TInputCloseQueryProc); overload;

    { IFMXDialogServiceSync }
    /// <summary>Show a simple message box with an 'Ok' button to close it.</summary>
    procedure ShowMessageSync(const AMessage: string);
    /// <summary>Shows custom message dialog with specified buttons on it.</summary>
    function MessageDialogSync(const AMessage: string; const ADialogType: TMsgDlgType; const AButtons: TMsgDlgButtons;
      const ADefaultButton: TMsgDlgBtn; const AHelpCtx: THelpContext): Integer;
    /// <summary>Shows an input message dialog with the specified promps and values on it. Values are modified within it.</summary>
    function InputQuerySync(const ACaption: string; const APrompts: array of string; var AValues: array of string): Boolean;

    { IFMXDialogServiceAsync }
    /// <summary>Show a simple message box with an 'Ok' button to close it.</summary>
    procedure ShowMessageAsync(const AMessage: string); overload;
    /// <summary>Show a simple message box with an 'Ok' button to close it.</summary>
    procedure ShowMessageAsync(const AMessage: string; const ACloseDialogProc: TInputCloseDialogProc); overload;
    /// <summary>Show a simple message box with an 'Ok' button to close it.</summary>
    procedure ShowMessageAsync(const AMessage: string; const ACloseDialogEvent: TInputCloseDialogEvent;
      const AContext: TObject = nil); overload;

    /// <summary>Shows custom message dialog with specified buttons on it.</summary>
     procedure MessageDialogAsync(const AMessage: string; const ADialogType: TMsgDlgType; const AButtons: TMsgDlgButtons;
      const ADefaultButton: TMsgDlgBtn; const AHelpCtx: THelpContext; const ACloseDialogProc: TInputCloseDialogProc); overload;
    /// <summary>Shows custom message dialog with specified buttons on it.</summary>
    procedure MessageDialogAsync(const AMessage: string; const ADialogType: TMsgDlgType; const AButtons: TMsgDlgButtons;
      const ADefaultButton: TMsgDlgBtn; const AHelpCtx: THelpContext; const ACloseDialogEvent: TInputCloseDialogEvent;
      const AContext: TObject = nil); overload;

    /// <summary>Shows an input message dialog with the specified promps and values on it. Values are modified within it.</summary>
    procedure InputQueryAsync(const ACaption: string; const APrompts: array of string; const ADefaultValues: array of string;
      const ACloseQueryProc: TInputCloseQueryProc); overload;
    /// <summary>Shows an input message dialog with the specified promps and values on it. Values are modified within it.</summary>
    procedure InputQueryAsync(const ACaption: string; const APrompts: array of string; const ADefaultValues: array of string;
      const ACloseQueryEvent: TInputCloseQueryWithResultEvent; const AContext: TObject = nil); overload;
  end;

  TFMXAlertViewInputDelegate = class(TOCLocal, UIAlertViewDelegate)
  private
    [Weak] FParentList: TList<TFMXAlertViewInputDelegate>;
    FResults: array of Integer;
    FModal: Boolean;
    FModalResult: TModalResult;
    FDefaultValues: array of string;
    FValues: array of string;
    FInputCloseQueryProc: TInputCloseQueryProc;
    procedure DoDialogClosed;
    procedure DoReadAlertView(const alertView: UIAlertView);
    procedure DoDismiss(const alertView: UIAlertView);
    procedure SetParentList(const AList: TList<TFMXAlertViewInputDelegate>);
  public
    constructor Create(const AButtons: TMsgDlgButtons; const AInputCloseQueryProc: TInputCloseQueryProc);
    { UIAlertViewDelegate }
    procedure alertView(alertView: UIAlertView; clickedButtonAtIndex: NSInteger); overload; cdecl;
    [MethodName('alertView:didDismissWithButtonIndex:')]
    procedure alertViewDidDismissWithButtonIndex(alertView: UIAlertView; didDismissWithButtonIndex: NSInteger); cdecl;
    procedure alertViewCancel(alertView: UIAlertView); cdecl;
    procedure didPresentAlertView(alertView: UIAlertView); cdecl;
    property ModalResult: TModalResult read FModalResult;
    property Modal: Boolean read FModal write FModal;
    property ParentList: TList<TFMXAlertViewInputDelegate> read FParentList write SetParentList;
  end;

constructor TCocoaDialogsService.Create;
begin
  inherited;
  FInputDelegates := TList<TFMXAlertViewInputDelegate>.Create;
end;

class destructor TCocoaDialogsService.UnInitialize;
begin
  FCurrent.DisposeOf;
  FCurrent := nil;
end;

class function TCocoaDialogsService.GetCurrent: TCocoaDialogsService;
begin
  if FCurrent = nil then
    FCurrent := TCocoaDialogsService.Create;
  Result := FCurrent;
end;

class function TCocoaDialogsService.CreateAlertView(const ACaption: string; const APrompts, ADefaultValues: array of string;
  const Delegate: TFMXAlertViewInputDelegate): UIAlertView;

  function IsPassword(const Prompt: string): Boolean;
  begin
    Result := (Prompt.Length > 0) and (Prompt.Chars[0] < #32);
  end;

  function StripPrompt(const Prompt: string; const Password: Boolean): String;
  begin
    Result := Prompt.Substring(IfThen(Password, 1, 0));
  end;
var
  Prompt: string;
  TextField: UITextField;
  LPromptsLength: Integer;
begin
  LPromptsLength := Length(APrompts);
  if Length(ADefaultValues) < LPromptsLength then
    raise EInvalidOperation.Create(SPromptArrayTooShort);
  if LPromptsLength = 0 then
    raise EInvalidOperation.Create(SPromptArrayEmpty);

  if (LPromptsLength > 2) or ((LPromptsLength = 2) and not (IsPassword(APrompts[1]) and
    (not IsPassword(APrompts[0])))) then
    raise EInvalidOperation.Create(SUnsupportedInputQuery);

  Result := TUIAlertView.Alloc;
  Prompt := StripPrompt(APrompts[0], IsPassword(APrompts[0]));
  Result.initWithTitle(StrToNSStr(ACaption), StrToNSStr(Prompt), Delegate.GetObjectID,
    StrToNSStr(SMsgDlgOK), nil);
  Result.addButtonWithTitle(StrToNSStr(SMsgDlgCancel));

  if LPromptsLength = 1 then
    Result.setAlertViewStyle(IfThen(IsPassword(APrompts[0]), UIAlertViewStyleSecureTextInput,
      UIAlertViewStylePlainTextInput))
  else if LPromptsLength = 2 then
  begin
    Result.setAlertViewStyle(UIAlertViewStyleLoginAndPasswordInput);
    TUITextField.Wrap(Result.textFieldAtIndex(1)).setText(StrToNSStr(ADefaultValues[1]));
  end;
  TextField := TUITextField.Wrap(Result.textFieldAtIndex(0));
  TextField.setText(StrToNSStr(ADefaultValues[0]));
end;

destructor TCocoaDialogsService.Destroy;
begin
  FInputDelegates.Free;
  inherited;
end;

function TCocoaDialogsService.DialogOpenFiles(const ADialog: TOpenDialog; var AFiles: TStrings; AType: TDialogType): Boolean;
begin
  Result := False;
end;

function TCocoaDialogsService.DialogPrint(var ACollate, APrintToFile: Boolean; var AFromPage, AToPage, ACopies: Integer;
  AMinPage, AMaxPage: Integer; var APrintRange: TPrintRange; AOptions: TPrintDialogOptions): Boolean;
begin
  Result := False;
end;

function TCocoaDialogsService.PageSetupGetDefaults(var AMargin, AMinMargin: TRect; var APaperSize: TPointF;
  AUnits: TPageMeasureUnits; AOptions: TPageSetupDialogOptions): Boolean;
begin
  Result := False;
end;

function TCocoaDialogsService.DialogPageSetup(var AMargin, AMinMargin :TRect; var APaperSize: TPointF;
  var AUnits: TPageMeasureUnits; AOptions: TPageSetupDialogOptions): Boolean;
begin
  Result := False;
end;

function TCocoaDialogsService.DialogPrinterSetup: Boolean;
begin
  Result := False;
end;

function TCocoaDialogsService.DialogSaveFiles(const ADialog: TOpenDialog; var AFiles: TStrings): Boolean;
begin
  Result := False;
end;

function TCocoaDialogsService.MessageDialog(const AMessage: string; const ADialogType: TMsgDlgType;
  const AButtons: TMsgDlgButtons;	const ADefaultButton: TMsgDlgBtn; const AX, AY: Integer; const AHelpCtx: THelpContext;
  const AHelpFileName: string): Integer;
var
  AlertView: UIAlertView;
  B: TMsgDlgBtn;
  Delegate: TFMXAlertViewInputDelegate;
  RunLoop: NSRunLoop;
  DistantFuture: NSDate;
  DefaultRunLoopMode: NSString;
begin
  Delegate := TFMXAlertViewInputDelegate.Create(AButtons, nil);
  Delegate.Modal := True;
  Delegate.ParentList := FInputDelegates;

  AlertView := TUIAlertView.Alloc;
  try
    AlertView := TUIAlertView.Wrap(AlertView.initWithTitle(StrToNSStr(MsgTitles[ADialogType]), StrToNSStr(AMessage),
      Delegate.GetObjectID, nil, nil));
    for B := High(TMsgDlgBtn) downto Low(TMsgDlgBtn) do // https://quality.embarcadero.com/browse/RSP-15971?filter=-2
      if B in AButtons then
        AlertView.addButtonWithTitle(StrToNSStr(translate(ButtonCaptions[B]))); // https://quality.embarcadero.com/browse/RSP-15970?filter=-2
    AlertView.Show;

    RunLoop := TNSRunLoop.Wrap(TNSRunLoop.OCClass.currentRunLoop);
    DistantFuture := TNSDate.Wrap(TNSDate.OCClass.distantFuture);
    DefaultRunLoopMode := NSDefaultRunLoopMode;
    while Delegate.Modal and RunLoop.runMode(DefaultRunLoopMode, DistantFuture) do;

    Result := Delegate.ModalResult;
  finally
    AlertView.release;
  end
end;

procedure TCocoaDialogsService.MessageDialog(const AMessage: string; const ADialogType: TMsgDlgType;
  const AButtons: TMsgDlgButtons; const ADefaultButton: TMsgDlgBtn; const AX, AY: Integer; const AHelpCtx: THelpContext;
  const AHelpFileName: string;	const ACloseDialogProc: TInputCloseDialogProc);
var
  AlertView: UIAlertView;
  B: TMsgDlgBtn;
  Delegate: TFMXAlertViewInputDelegate;
begin
  Delegate := TFMXAlertViewInputDelegate.Create(AButtons,
    procedure (const AResult: TModalResult; const AValues: array of string)
    begin
      if Assigned(ACloseDialogProc) then
        ACloseDialogProc(AResult);
    end);
  Delegate.ParentList := FInputDelegates;
  AlertView := TUIAlertView.Alloc;
  AlertView := TUIAlertView.Wrap(AlertView.initWithTitle(StrToNSStr(MsgTitles[ADialogType]), StrToNSStr(AMessage),
    Delegate.GetObjectID, nil, nil));
  for B := High(TMsgDlgBtn) downto Low(TMsgDlgBtn) do // https://quality.embarcadero.com/browse/RSP-15971?filter=-2
    if B in AButtons then
      AlertView.addButtonWithTitle(StrToNSStr(translate(ButtonCaptions[B]))); // https://quality.embarcadero.com/browse/RSP-15970?filter=-2
  AlertView.Show;
end;

function TCocoaDialogsService.InputQuery(const ACaption: string; const APrompts: array of string;
  var AValues: array of string; const ACloseQueryFunc: TInputCloseQueryFunc): Boolean;
var
  AlertView: UIAlertView;
  Delegate: TFMXAlertViewInputDelegate;
  RunLoop: NSRunLoop;
  DistantFuture: NSDate;
  DefaultRunLoopMode: NSString;
  I: Integer;
begin
  if (Length(AValues) > 0) and (Length(APrompts) > 0) then
  begin
    Delegate := TFMXAlertViewInputDelegate.Create(mbOKCancel, nil);
    Delegate.ParentList := FInputDelegates;
    Delegate.Modal := True;

    AlertView := CreateAlertView(ACaption, APrompts, AValues, Delegate);
    try
      AlertView.show;
      RunLoop := TNSRunLoop.Wrap(TNSRunLoop.OCClass.currentRunLoop);
      DistantFuture := TNSDate.Wrap(TNSDate.OCClass.distantFuture);
      DefaultRunLoopMode := NSDefaultRunLoopMode;
      while Delegate.Modal and RunLoop.runMode(DefaultRunLoopMode, DistantFuture) do;

      Result := Delegate.ModalResult = mrOk;
      if Result then
        for I := 0 to Length(AValues) - 1 do
          AValues[I] := NSStrToStr(TNSString.Wrap(TUITextField.Wrap(AlertView.textFieldAtIndex(I)).text));
    finally
      AlertView.release;
    end;
  end
  else
    Result := False;
end;

procedure TCocoaDialogsService.InputQuery(const ACaption: string; const APrompts, ADefaultValues: array of string;
  const ACloseQueryProc: TInputCloseQueryProc);
var
  Delegate: TFMXAlertViewInputDelegate;
  AlertView: UIAlertView;
begin
  Delegate := TFMXAlertViewInputDelegate.Create(mbOKCancel, ACloseQueryProc);
  Delegate.ParentList := FInputDelegates;
  AlertView := CreateAlertView(ACaption, APrompts, ADefaultValues, Delegate);
  AlertView.show;
end;

{ IFMXDialogServiceSync }
procedure TCocoaDialogsService.ShowMessageSync(const AMessage: string);
begin
  MessageDialogSync(AMessage, TMsgDlgType.mtCustom, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0);
end;

function  TCocoaDialogsService.MessageDialogSync(const AMessage: string; const ADialogType: TMsgDlgType; const AButtons: TMsgDlgButtons;
  const ADefaultButton: TMsgDlgBtn; const AHelpCtx: THelpContext): Integer;
var
  AlertView: UIAlertView;
  B: TMsgDlgBtn;
  Delegate: TFMXAlertViewInputDelegate;
  RunLoop: NSRunLoop;
  DistantFuture: NSDate;
  DefaultRunLoopMode: NSString;
begin
  MessageDialogCheckInUIThread;
  Delegate := TFMXAlertViewInputDelegate.Create(AButtons, nil);
  Delegate.Modal := True;
  Delegate.ParentList := FInputDelegates;

  AlertView := TUIAlertView.Alloc;
  try
    AlertView := TUIAlertView.Wrap(AlertView.initWithTitle(StrToNSStr(MsgTitles[ADialogType]), StrToNSStr(AMessage),
      Delegate.GetObjectID, nil, nil));
    for B := High(TMsgDlgBtn) downto Low(TMsgDlgBtn) do // https://quality.embarcadero.com/browse/RSP-15971?filter=-2
      if B in AButtons then
        AlertView.addButtonWithTitle(StrToNSStr(translate(ButtonCaptions[B]))); // https://quality.embarcadero.com/browse/RSP-15970?filter=-2
    AlertView.Show;

    RunLoop := TNSRunLoop.Wrap(TNSRunLoop.OCClass.currentRunLoop);
    DistantFuture := TNSDate.Wrap(TNSDate.OCClass.distantFuture);
    DefaultRunLoopMode := NSDefaultRunLoopMode;
    while Delegate.Modal and RunLoop.runMode(DefaultRunLoopMode, DistantFuture) do;

    Result := Delegate.ModalResult;
  finally
    AlertView.release;
  end
end;


function TCocoaDialogsService.InputQuerySync(const ACaption: string; const APrompts: array of string; var AValues: array of string): Boolean;
var
  AlertView: UIAlertView;
  Delegate: TFMXAlertViewInputDelegate;
  RunLoop: NSRunLoop;
  DistantFuture: NSDate;
  DefaultRunLoopMode: NSString;
  I: Integer;
begin
  MessageDialogCheckInUIThread;
  if (Length(AValues) > 0) and (Length(APrompts) > 0) then
  begin
    Delegate := TFMXAlertViewInputDelegate.Create(mbOKCancel, nil);
    Delegate.ParentList := FInputDelegates;
    Delegate.Modal := True;

    AlertView := CreateAlertView(ACaption, APrompts, AValues, Delegate);
    try
      AlertView.show;
      RunLoop := TNSRunLoop.Wrap(TNSRunLoop.OCClass.currentRunLoop);
      DistantFuture := TNSDate.Wrap(TNSDate.OCClass.distantFuture);
      DefaultRunLoopMode := NSDefaultRunLoopMode;
      while Delegate.Modal and RunLoop.runMode(DefaultRunLoopMode, DistantFuture) do;

      Result := Delegate.ModalResult = mrOk;
      if Result then
        for I := 0 to Length(AValues) - 1 do
          AValues[I] := NSStrToStr(TNSString.Wrap(TUITextField.Wrap(AlertView.textFieldAtIndex(I)).text));
    finally
      AlertView.release;
    end;
  end
  else
    Result := False;
end;

{ IFMXDialogServiceAsync }
procedure TCocoaDialogsService.ShowMessageAsync(const AMessage: string);
begin
  MessageDialogAsync(AMessage, TMsgDlgType.mtCustom, [TMsgDlgBtn.mbOk], TMsgDlgBtn.mbOk, 0,
    procedure(const AResult: TModalResult)
    begin
    end);
end;

procedure TCocoaDialogsService.ShowMessageAsync(const AMessage: string; const ACloseDialogProc: TInputCloseDialogProc);
begin
  MessageDialogAsync(AMessage, TMsgDlgType.mtCustom, [TMsgDlgBtn.mbOk], TMsgDlgBtn.mbOk, 0, ACloseDialogProc);
end;

procedure TCocoaDialogsService.ShowMessageAsync(const AMessage: string; const ACloseDialogEvent: TInputCloseDialogEvent;
  const AContext: TObject = nil);
begin
  MessageDialogAsync(AMessage, TMsgDlgType.mtCustom, [TMsgDlgBtn.mbOk], TMsgDlgBtn.mbOk, 0, ACloseDialogEvent, AContext);
end;

procedure TCocoaDialogsService.MessageDialogAsync(const AMessage: string; const ADialogType: TMsgDlgType; const AButtons: TMsgDlgButtons;
  const ADefaultButton: TMsgDlgBtn; const AHelpCtx: THelpContext; const ACloseDialogProc: TInputCloseDialogProc);
var
  AlertView: UIAlertView;
  B: TMsgDlgBtn;
  Delegate: TFMXAlertViewInputDelegate;
begin
  MessageDialogCheckInUIThread;
  Delegate := TFMXAlertViewInputDelegate.Create(AButtons,
    procedure (const AResult: TModalResult; const AValues: array of string)
    begin
      if Assigned(ACloseDialogProc) then
        ACloseDialogProc(AResult);
    end);
  Delegate.ParentList := FInputDelegates;
  AlertView := TUIAlertView.Alloc;
  AlertView := TUIAlertView.Wrap(AlertView.initWithTitle(StrToNSStr(MsgTitles[ADialogType]), StrToNSStr(AMessage),
    Delegate.GetObjectID, nil, nil));
  for B := High(TMsgDlgBtn) downto Low(TMsgDlgBtn) do // https://quality.embarcadero.com/browse/RSP-15971?filter=-2
    if B in AButtons then
      AlertView.addButtonWithTitle(StrToNSStr(translate(ButtonCaptions[B]))); // https://quality.embarcadero.com/browse/RSP-15970?filter=-2
  AlertView.Show;
end;

procedure TCocoaDialogsService.MessageDialogAsync(const AMessage: string; const ADialogType: TMsgDlgType; const AButtons: TMsgDlgButtons;
  const ADefaultButton: TMsgDlgBtn; const AHelpCtx: THelpContext; const ACloseDialogEvent: TInputCloseDialogEvent;
  const AContext: TObject = nil);
begin
  MessageDialogAsync(AMessage, ADialogType, AButtons, ADefaultButton, AHelpCtx,
  procedure(const AResult: TModalResult)
  begin
    if Assigned(ACloseDialogEvent) then
      ACloseDialogEvent(AContext, AResult);
  end);
end;

procedure TCocoaDialogsService.InputQueryAsync(const ACaption: string; const APrompts: array of string; const ADefaultValues: array of string;
  const ACloseQueryProc: TInputCloseQueryProc);
var
  Delegate: TFMXAlertViewInputDelegate;
  AlertView: UIAlertView;
begin
  MessageDialogCheckInUIThread;
  Delegate := TFMXAlertViewInputDelegate.Create(mbOKCancel, ACloseQueryProc);
  Delegate.ParentList := FInputDelegates;

  SetLength(Delegate.FDefaultValues, Length(ADefaultValues));
  TArray.Copy<string>(ADefaultValues, Delegate.FDefaultValues, Length(ADefaultValues));

  AlertView := CreateAlertView(ACaption, APrompts, ADefaultValues, Delegate);
  AlertView.show;
end;

procedure TCocoaDialogsService.InputQueryAsync(const ACaption: string; const APrompts: array of string; const ADefaultValues: array of string;
  const ACloseQueryEvent: TInputCloseQueryWithResultEvent; const AContext: TObject = nil);
begin
  InputQueryAsync(ACaption, APrompts, ADefaultValues,
    procedure (const AResult: TModalResult; const AValues: array of string)
    begin
      if Assigned(ACloseQueryEvent) then
        ACloseQueryEvent(AContext, AResult, AValues);
    end);

end;

{ TFMXAlertViewInputDelegate }

procedure TFMXAlertViewInputDelegate.alertView(alertView: UIAlertView; clickedButtonAtIndex: NSInteger);
begin
  if (clickedButtonAtIndex >= 0) and (clickedButtonAtIndex < Length(FResults)) then
    FModalResult := FResults[clickedButtonAtIndex]
  else
    FModalResult := mrCancel;

  DoReadAlertView(alertView);

  TThread.Queue(nil, DoDialogClosed);
  FModal := False;
end;

procedure TFMXAlertViewInputDelegate.didPresentAlertView(alertView: UIAlertView);
var
  TextField: UITextField;
  I1, I2: UITextPosition;
  Range: UITextRange;
begin
  if alertView.alertViewStyle <> UIAlertViewStyleDefault then
  begin
    TextField := TUITextField.Wrap(alertView.textFieldAtIndex(0));
    TextField.becomeFirstResponder;
    I1 := TextField.beginningOfDocument;
    I2 := TextField.endOfDocument;
    Range := TextField.textRangeFromPosition(I1, I2);
    TextField.setSelectedTextRange(Range);
  end;
end;

procedure TFMXAlertViewInputDelegate.alertViewCancel(alertView: UIAlertView);
begin
  FModalResult := mrCancel;
  DoReadAlertView(alertView);
  TThread.Queue(nil, DoDialogClosed);
  FModal := False;
  DoDismiss(alertView);
end;

procedure TFMXAlertViewInputDelegate.alertViewDidDismissWithButtonIndex(alertView: UIAlertView;
  didDismissWithButtonIndex: NSInteger);
begin
  DoDismiss(alertView);
end;

constructor TFMXAlertViewInputDelegate.Create(const AButtons: TMsgDlgButtons; const AInputCloseQueryProc: TInputCloseQueryProc);
var
  B: TMsgDlgBtn;
begin
  inherited Create;
  FParentList := nil;
  FInputCloseQueryProc := AInputCloseQueryProc;
  for B := High(TMsgDlgBtn) downto Low(TMsgDlgBtn) do // https://quality.embarcadero.com/browse/RSP-15971?filter=-2
    if B in AButtons then
    begin
      SetLength(FResults, Length(FResults) + 1);
      FResults[High(FResults)] := ModalResults[B]
    end;
end;

procedure TFMXAlertViewInputDelegate.DoDialogClosed;
begin
  try
    if Assigned(FInputCloseQueryProc) then
      FInputCloseQueryProc(FModalResult, FValues);
  except
    Application.HandleException(Self);
  end;
end;

procedure TFMXAlertViewInputDelegate.DoDismiss(const alertView: UIAlertView);
begin
  if FParentList <> nil then
    FParentList.Remove(Self);
  FParentList := nil;
  alertView.setDelegate(nil);
end;

procedure TFMXAlertViewInputDelegate.DoReadAlertView(const alertView: UIAlertView);
begin
  case alertView.alertViewStyle of
    UIAlertViewStyleDefault: SetLength(FValues, 0);
    UIAlertViewStyleSecureTextInput,
    UIAlertViewStylePlainTextInput:
      begin
        SetLength(FValues, 1);
        if FModalResult = mrOk then
          FValues[0] := NSStrToStr(TUITextView.Wrap(alertView.textFieldAtIndex(0)).text)
        else if Length(FDefaultValues) > 0 then
          TArray.Copy<string>(FDefaultValues, FValues, Length(FDefaultValues));
      end;
    UIAlertViewStyleLoginAndPasswordInput:
      begin
        SetLength(FValues, 2);
        if FModalResult = mrOk then
        begin
          FValues[0] := NSStrToStr(TUITextView.Wrap(alertView.textFieldAtIndex(0)).text);
          FValues[1] := NSStrToStr(TUITextView.Wrap(alertView.textFieldAtIndex(1)).text);
        end
        else if Length(FDefaultValues) > 0 then
          TArray.Copy<string>(FDefaultValues, FValues, Length(FDefaultValues));
      end;
  end;
end;

procedure TFMXAlertViewInputDelegate.SetParentList(const AList: TList<TFMXAlertViewInputDelegate>);
begin
  FParentList := AList;
  if FParentList <> nil then
    FParentList.Add(Self);
end;

initialization
  TPlatformServices.Current.AddPlatformService(IFMXDialogService, TCocoaDialogsService.Create);
  TPlatformServices.Current.AddPlatformService(IFMXDialogServiceSync, TCocoaDialogsService.Current);
  TPlatformServices.Current.AddPlatformService(IFMXDialogServiceAsync, TCocoaDialogsService.Current);
end.
