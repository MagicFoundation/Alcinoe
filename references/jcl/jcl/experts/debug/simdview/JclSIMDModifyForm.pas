{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is: JvSIMDModifyForm.pas, released on 2004-10-11.                              }
{                                                                                                  }
{ The Initial Developer of the Original Code is Florent Ouchet                                     }
{ [ouchet dott florent att laposte dott net]                                                       }
{ Portions created by Florent Ouchet are Copyright (C) 2004 Florent Ouchet.                        }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{ You may retrieve the latest version of this file at the Project JEDI's JCL home page,            }
{ located at https://github.com/project-jedi/jcl                                                   }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclSIMDModifyForm;

interface

{$I jcl.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ToolsApi, Contnrs,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclOtaUtils, JclSysInfo, JclSIMDUtils;

const
  WM_MODIFYCONTINUE = WM_USER + 100;

type
  TJclRegisterType = (rtYMM, rtXMM, rtMM);

  TJclSIMDModifyFrm = class(TForm)
    ComboBoxDisplay: TComboBox;
    ComboBoxFormat: TComboBox;
    LabelDisplay: TLabel;
    LabelFormat: TLabel;
    LabelBlank: TLabel;
    PanelModify: TPanel;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    MemoTip: TMemo;
    procedure ComboBoxDisplayChange(Sender: TObject);
    procedure ComboBoxFormatChange(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
  private
    FRegisterType: TJclRegisterType;
    FXMMRegister: TJclXMMRegister;
    FYMMRegister: TJclYMMRegister;
    FMMRegister: TJclMMRegister;
    FDisplay: TJclPackedContentType;
    FFormat: TJclSIMDFormat;
    FDebuggerServices: IOTADebuggerServices;
    FComboBoxList: TComponentList;
    FLabelList: TComponentList;
    FHistory: TStringList;
    FThread: IOTAThread;
    FTextIndex: Integer;
    FExprStr: string;
    FResultStr: string;
    FReturnCode: Cardinal;
    FCPUInfo: TCpuInfo;
    FYMMEnabled: Boolean;
    FSettings: TJclOTASettings;
    procedure ContinueModify;
    procedure StartModify;
    procedure WMModifyContinue(var Msg: TMessage); message WM_MODIFYCONTINUE;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    property RegisterType: TJclRegisterType read FRegisterType;
    property XMMRegister: TJclXMMRegister read FXMMRegister;
    property YMMRegister: TJclYMMRegister read FYMMRegister;
    property MMRegister: TJclMMRegister read FMMRegister;
    property DebuggerServices: IOTADebuggerServices read FDebuggerServices;
  public
    constructor Create(AOwner: TComponent;
      ADebuggerServices: IOTADebuggerServices; ASettings: TJclOTASettings); reintroduce;
    destructor Destroy; override;
    function Execute(AThread: IOTAThread; ADisplay: TJclPackedContentType;
      AFormat: TJclSIMDFormat; var ARegister: TJclXMMRegister;
      const ACpuInfo: TCpuInfo; AYMMEnabled: Boolean): Boolean; overload;
    function Execute(AThread: IOTAThread; ADisplay: TJclPackedContentType;
      AFormat: TJclSIMDFormat; var AXMMRegister: TJclXMMRegister; var AYMMRegister: TJclYMMRegister;
      const ACpuInfo: TCpuInfo; AYMMEnabled: Boolean): Boolean; overload;
    function Execute(AThread: IOTAThread; ADisplay: TJclPackedContentType;
      AFormat: TJclSIMDFormat; var ARegister: TJclMMRegister;
      const ACpuInfo: TCpuInfo; AYMMEnabled: Boolean): Boolean; overload;
    procedure ThreadEvaluate(const ExprStr, ResultStr: string; ReturnCode: Integer);
    procedure UpdateDisplay;
    procedure UpdateFormat;
    procedure LoadHistory;
    procedure SaveHistory;
    procedure MergeHistory;

    property Display: TJclPackedContentType read FDisplay;
    property Format: TJclSIMDFormat read FFormat;
    property History: TStringList read FHistory;
    property Thread: IOTAThread read FThread;
    property Settings: TJclOTASettings read FSettings;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\experts\debug\simdview';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

{$R *.dfm}

uses
  JclOtaResources;

const
  NbEdits: array [TJclRegisterType, TJclPackedContentType] of Byte =
   (
    (32, 16, 8, 4, 8, 4), // YMM
    (16,  8, 4, 2, 4, 2), // XMM
    ( 8,  4, 2, 1, 2, 1)  // MM
   );

  Texts: array [TJclPackedContentType] of string =
    ('Byte', 'Word', 'DWord', 'QWord', 'Single', 'Double');

  ItemFormat = 'Item%d';
  CountPropertyName = 'Count';

  HistoryListSize = 30;

//=== { TJclSIMDModifyFrm } ==================================================

constructor TJclSIMDModifyFrm.Create(AOwner: TComponent;
  ADebuggerServices: IOTADebuggerServices; ASettings: TJclOTASettings);
var
  ContentType: TJclPackedContentType;
begin
  inherited Create(AOwner);

  for ContentType := Low(TJclPackedContentType) to High(TJClPackedContentType) do
    ComboBoxDisplay.Items.Add(Texts[ContentType] + 's');
  ComboBoxFormat.Items.Add(LoadResString(@RsBinary));
  ComboBoxFormat.Items.Add(LoadResString(@RsSignedDecimal));
  ComboBoxFormat.Items.Add(LoadResString(@RsUnsignedDecimal));
  ComboBoxFormat.Items.Add(LoadResString(@RsHexadecimal));
  LabelDisplay.Caption := LoadResString(@RsDisplay);
  LabelFormat.Caption := LoadResString(@RsFormat);
  LabelBlank.Caption := LoadResString(@RsKeepBlank);
  ButtonOK.Caption := LoadResString(@RsOk);
  ButtonCancel.Caption := LoadResString(@RsCancel);
  MemoTip.Lines.Text := LoadResString(@RsSIMDModificationDescription);

  FDebuggerServices := ADebuggerServices;
  FSettings := ASettings;

  FComboBoxList := TComponentList.Create(False);
  FLabelList := TComponentList.Create(False);
  FHistory := TStringList.Create;
  FHistory.Duplicates := dupIgnore;
end;

procedure TJclSIMDModifyFrm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);

  // Fixing the Window Ghosting "bug"
  Params.Style := params.Style or WS_POPUP;
  if Assigned(Screen.ActiveForm) then
    Params.WndParent := Screen.ActiveForm.Handle
  else if Assigned (Application.MainForm) then
    Params.WndParent := Application.MainForm.Handle
  else
    Params.WndParent := Application.Handle;
end;

destructor TJclSIMDModifyFrm.Destroy;
begin
  FLabelList.Free;
  FComboBoxList.Free;
  FHistory.Free;
  FDebuggerServices := nil;

  inherited Destroy;
end;

function TJclSIMDModifyFrm.Execute(AThread: IOTAThread; ADisplay: TJclPackedContentType;
  AFormat: TJclSIMDFormat; var ARegister: TJclXMMRegister;
  const ACPUInfo: TCPUInfo; AYMMEnabled: Boolean): Boolean;
begin
  FTextIndex := 0;
  FRegisterType := rtXMM;
  FXMMRegister := ARegister;
  FFormat := AFormat;
  FDisplay := ADisplay;
  FThread := AThread;
  FCpuInfo := ACpuInfo;
  FYMMEnabled := AYMMEnabled;

  LoadHistory;

  ComboBoxDisplay.ItemIndex := Integer(Display);
  ComboBoxFormat.Enabled := Display in [pctBytes..pctQWords];
  ComboBoxFormat.ItemIndex := Integer(Format);
  UpdateDisplay;

  Result := ShowModal = mrOk;

  if Result then
    ARegister := XMMRegister;

  MergeHistory;
  SaveHistory;
end;

function TJclSIMDModifyFrm.Execute(AThread: IOTAThread; ADisplay: TJclPackedContentType;
  AFormat: TJclSIMDFormat; var AXMMRegister: TJclXMMRegister; var AYMMRegister: TJclYMMRegister;
  const ACPUInfo: TCPUInfo; AYMMEnabled: Boolean): Boolean;
begin
  FTextIndex := 0;
  FRegisterType := rtYMM;
  FXMMRegister := AXMMRegister;
  FYMMRegister := AYMMRegister;
  FFormat := AFormat;
  FDisplay := ADisplay;
  FThread := AThread;
  FCpuInfo := ACpuInfo;
  FYMMEnabled := AYMMEnabled;

  LoadHistory;

  ComboBoxDisplay.ItemIndex := Integer(Display);
  ComboBoxFormat.Enabled := Display in [pctBytes..pctQWords];
  ComboBoxFormat.ItemIndex := Integer(Format);
  UpdateDisplay;

  Result := ShowModal = mrOk;

  if Result then
  begin
    AXMMRegister := XMMRegister;
    AYMMRegister := YMMRegister;
  end;

  MergeHistory;
  SaveHistory;
end;

function TJclSIMDModifyFrm.Execute(AThread: IOTAThread;
  ADisplay: TJclPackedContentType; AFormat: TJclSIMDFormat;
  var ARegister: TJclMMRegister; const ACpuInfo: TCpuInfo; AYMMEnabled: Boolean): Boolean;
begin
  FTextIndex := 0;
  FRegisterType := rtMM;
  FMMRegister := ARegister;
  FFormat := AFormat;
  FDisplay := ADisplay;
  FThread := AThread;
  FCpuInfo := ACpuInfo;
  FYMMEnabled := AYMMEnabled;

  LoadHistory;

  ComboBoxDisplay.ItemIndex := Integer(Display);
  ComboBoxFormat.Enabled := Display in [pctBytes..pctQWords];
  ComboBoxFormat.ItemIndex := Integer(Format);
  UpdateDisplay;

  Result := ShowModal = mrOk;

  if Result then
    ARegister := MMRegister;

  MergeHistory;
  SaveHistory;
end;

procedure TJclSIMDModifyFrm.UpdateDisplay;
var
  Index: Integer;
  AComboBox: TComboBox;
  ALabel: TLabel;
  X, Y: Integer;
begin
  MergeHistory;
  while PanelModify.ControlCount > 0 do
    PanelModify.Controls[0].Free;
  FComboBoxList.Clear;
  FLabelList.Clear;

  ComboBoxDisplay.ItemIndex := Integer(Display);
  ComboBoxFormat.Enabled := Display in [pctBytes..pctQWords];
  ComboBoxFormat.ItemIndex := Integer(Format);

  X := 0;
  Y := 12;
  for Index := 0 to NbEdits[RegisterType, Display] - 1 do
  begin
    AComboBox := TComboBox.Create(Self);
    AComboBox.Parent := PanelModify;
    AComboBox.SetBounds(X + 130, Y, 90, AComboBox.Height);
    AComboBox.Tag := Index;
    AComboBox.Text := '';
    AComboBox.Items.Assign(History);
    FComboBoxList.Add(AComboBox);
    ALabel := TLabel.Create(Self);
    ALabel.Parent := PanelModify;
    ALabel.SetBounds(X + 5, Y + 2, 60, ALabel.Height);
    ALabel.Tag := Index;
    FLabelList.Add(ALabel);
    if (Index and 7) = 7 then
    begin
      Y := 12;
      Inc(X, 230);
    end
    else
      Inc(Y, 32);
  end;
  UpdateFormat;
end;

procedure TJclSIMDModifyFrm.UpdateFormat;
var
  Index: Integer;
  Value: TJclSIMDValue;
  ALabel: TLabel;
begin
  Value.Display := Display;
  for Index := 0 to FLabelList.Count - 1 do
  begin
    ALabel := FLabelList.Items[Index] as TLabel;
    case RegisterType of
      rtYMM:
        case Display of
          pctBytes:
            if ALabel.Tag >= Low(YMMRegister.Bytes) then
              Value.ValueByte := YMMRegister.Bytes[ALabel.Tag]
            else
              Value.ValueByte := XMMRegister.Bytes[ALabel.Tag];
          pctWords:
            if ALabel.Tag >= Low(YMMRegister.Words) then
              Value.ValueWord := YMMRegister.Words[ALabel.Tag]
            else
              Value.ValueWord := XMMRegister.Words[ALabel.Tag];
          pctDWords:
            if ALabel.Tag >= Low(YMMRegister.DWords) then
              Value.ValueDWord := YMMRegister.DWords[ALabel.Tag]
            else
              Value.ValueDWord := XMMRegister.DWords[ALabel.Tag];
          pctQWords:
            if ALabel.Tag >= Low(YMMRegister.QWords) then
              Value.ValueQWord := YMMRegister.QWords[ALabel.Tag]
            else
              Value.ValueQWord := XMMRegister.QWords[ALabel.Tag];
          pctSingles:
            if ALabel.Tag >= Low(YMMRegister.Singles) then
              Value.ValueSingle := YMMRegister.Singles[ALabel.Tag]
            else
              Value.ValueSingle := XMMRegister.Singles[ALabel.Tag];
          pctDoubles:
            if ALabel.Tag >= Low(YMMRegister.Doubles) then
              Value.ValueDouble := YMMRegister.Doubles[ALabel.Tag]
            else
              Value.ValueDouble := XMMRegister.Doubles[ALabel.Tag];
        end;
      rtXMM:
        case Display of
          pctBytes:
            Value.ValueByte := XMMRegister.Bytes[ALabel.Tag];
          pctWords:
            Value.ValueWord := XMMRegister.Words[ALabel.Tag];
          pctDWords:
            Value.ValueDWord := XMMRegister.DWords[ALabel.Tag];
          pctQWords:
            Value.ValueQWord := XMMRegister.QWords[ALabel.Tag];
          pctSingles:
            Value.ValueSingle := XMMRegister.Singles[ALabel.Tag];
          pctDoubles:
            Value.ValueDouble := XMMRegister.Doubles[ALabel.Tag];
        end;
      rtMM:
        case Display of
          pctBytes:
            Value.ValueByte := MMRegister.Bytes[ALabel.Tag];
          pctWords:
            Value.ValueWord := MMRegister.Words[ALabel.Tag];
          pctDWords:
            Value.ValueDWord := MMRegister.DWords[ALabel.Tag];
          pctQWords:
            Value.ValueQWord := MMRegister.QWords;
          pctSingles:
            Value.ValueSingle := MMRegister.Singles[ALabel.Tag];
          pctDoubles:
            begin
              ALabel.Caption := '';
              Break;
            end;
        end;
    end;
    ALabel.Caption := SysUtils.Format('%s%d = %s', [Texts[Display], Index, FormatValue(Value, Format)]);
  end;
end;

procedure TJclSIMDModifyFrm.ComboBoxDisplayChange(Sender: TObject);
begin
  try
    FDisplay := TJclPackedContentType((Sender as TComboBox).ItemIndex);
    UpdateDisplay;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

procedure TJclSIMDModifyFrm.ComboBoxFormatChange(Sender: TObject);
begin
  try
    FFormat := TJclSIMDFormat((Sender as TComboBox).ItemIndex);
    UpdateFormat;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

procedure TJclSIMDModifyFrm.LoadHistory;
var
  Index, Count: Integer;
begin
  Count := Settings.LoadInteger(CountPropertyName, 0);
  History.Clear;

  for Index := 0 to Count - 1 do
    History.Add(Settings.LoadString(SysUtils.Format(ItemFormat, [Index]), ''));
end;

procedure TJclSIMDModifyFrm.SaveHistory;
var
  Index: Integer;
begin
  Settings.SaveInteger(CountPropertyName, History.Count);
  for Index := 0 to History.Count - 1 do
    Settings.SaveString(SysUtils.Format(ItemFormat, [Index]), History.Strings[Index]);
end;

procedure TJclSIMDModifyFrm.MergeHistory;
var
  I, J: Integer;
begin
  History.Duplicates := dupIgnore;
  for I := 0 to PanelModify.ControlCount - 1 do
    if PanelModify.Controls[I] is TComboBox then
      with TComboBox(PanelModify.Controls[I]) do
  begin
    for J := 0 to Items.Count - 1 do
      if (Items.Strings[J] <> '') and (History.IndexOf(Items.Strings[J]) = -1) then
        History.Add(Items.Strings[J]);
    if (Text <> '') and (History.IndexOf(Text) = -1) then
      History.Add(Text);
  end;
  while History.Count > HistoryListSize do
    History.Delete(0);
end;

procedure TJclSIMDModifyFrm.WMModifyContinue(var Msg: TMessage);
begin
  try
    ContinueModify;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

procedure TJclSIMDModifyFrm.StartModify;
begin
  FTextIndex := -1;
  FResultStr := '';
  FReturnCode := 0;
  ContinueModify;
end;

procedure TJclSIMDModifyFrm.ContinueModify;
const
  ResultBufferSize = 200;
var
  EvaluateResult: TOTAEvaluateResult;
  AValue: TJclSIMDValue;
  AComboBox: TComboBox;
  ResultBuffer: array [0..ResultBufferSize-1] of Char;
  ResultAddr, ResultSize: Cardinal;
  CanModify: Boolean;
  JclContext: TJclContext;
begin
  if (FReturnCode <> 0) then
    EvaluateResult := erError
  else
    EvaluateResult := erOK;
  AValue.Display := Display;
  GetThreadJclContext(Thread, JclContext);
  while (FTextIndex < FComboBoxList.Count) and (EvaluateResult = erOK) do
  begin
    if (FTextIndex >= 0) and (FResultStr <> '') then
    begin
      if (ParseValue(FResultStr,AValue,Format)) then
        case RegisterType of
          rtYMM:
            case AValue.Display of
              pctBytes:
                if FTextIndex >= Low(FYMMRegister.Bytes) then
                  FYMMRegister.Bytes[FTextIndex] := AValue.ValueByte
                else
                  FXMMRegister.Bytes[FTextIndex] := AValue.ValueByte;
              pctWords:
                if FTextIndex >= Low(FYMMRegister.Words) then
                  FYMMRegister.Words[FTextIndex] := AValue.ValueWord
                else
                  FXMMRegister.Words[FTextIndex] := AValue.ValueWord;
              pctDWords:
                if FTextIndex >= Low(FYMMRegister.DWords) then
                  FYMMRegister.DWords[FTextIndex] := AValue.ValueDWord
                else
                  FXMMRegister.DWords[FTextIndex] := AValue.ValueDWord;
              pctQWords:
                if FTextIndex >= Low(FYMMRegister.QWords) then
                  FYMMRegister.QWords[FTextIndex] := AValue.ValueQWord
                else
                  FXMMRegister.QWords[FTextIndex] := AValue.ValueQWord;
              pctSingles:
                if FTextIndex >= Low(FYMMRegister.Singles) then
                  FYMMRegister.Singles[FTextIndex] := AValue.ValueSingle
                else
                  FXMMRegister.Singles[FTextIndex] := AValue.ValueSingle;
              pctDoubles:
                if FTextIndex >= Low(FYMMRegister.Doubles) then
                  FYMMRegister.Doubles[FTextIndex] := AValue.ValueDouble
                else
                  FXMMRegister.Doubles[FTextIndex] := AValue.ValueDouble;
            end;
          rtXMM:
            case AValue.Display of
              pctBytes:
                FXMMRegister.Bytes[FTextIndex] := AValue.ValueByte;
              pctWords:
                FXMMRegister.Words[FTextIndex] := AValue.ValueWord;
              pctDWords:
                FXMMRegister.DWords[FTextIndex] := AValue.ValueDWord;
              pctQWords:
                FXMMRegister.QWords[FTextIndex] := AValue.ValueQWord;
              pctSingles:
                FXMMRegister.Singles[FTextIndex] := AValue.ValueSingle;
              pctDoubles:
                FXMMRegister.Doubles[FTextIndex] := AValue.ValueDouble;
            end;
          rtMM:
            case AValue.Display of
              pctBytes:
                FMMRegister.Bytes[FTextIndex] := AValue.ValueByte;
              pctWords:
                FMMRegister.Words[FTextIndex] := AValue.ValueWord;
              pctDWords:
                FMMRegister.DWords[FTextIndex] := AValue.ValueDWord;
              pctQWords:
                FMMRegister.QWords := AValue.ValueQWord;
              pctSingles:
                FMMRegister.Singles[FTextIndex] := AValue.ValueSingle;
              pctDoubles:
                EvaluateResult := erError;
            end;
          else
            EvaluateResult := erError;
        end
      else
        EvaluateResult := erError;
    end;
    if EvaluateResult = erOK then
    begin
      Inc(FTextIndex);
      if FTextIndex < FComboBoxList.Count then
      begin
        AComboBox := TComboBox(FComboBoxList.Items[FTextIndex]);
        FExprStr := AComboBox.Text;
        if FExprStr <> '' then
        begin
          if not ParseValue(FExprStr, AValue, Format) then
          begin
            if ReplaceSIMDRegisters(FExprStr, FCPUInfo.Is64Bits, FYMMEnabled, JclContext) then
              EvaluateResult := Thread.Evaluate(FExprStr, ResultBuffer,
                ResultBufferSize, CanModify, True, '', ResultAddr, ResultSize, FReturnCode)
            else
              EvaluateResult := erError;
            if (EvaluateResult <> erDeferred) and (FReturnCode <> 0) then
              EvaluateResult := erError;
            if EvaluateResult = erOK then
              FResultStr := ResultBuffer;
            if FResultStr = '' then
              EvaluateResult := erError;
          end
          else
          begin
            FResultStr := FExprStr;
            EvaluateResult := erOK;
          end;
        end
        else
          FResultStr := '';
      end;
    end;
  end;
  if (EvaluateResult = erError) and (FTextIndex < FComboBoxList.Count) then
  begin
    AComboBox := TComboBox(FComboBoxList.Items[FTextIndex]);
    FocusControl(AComboBox);
    AComboBox.SelectAll;
  end
  else
  if (EvaluateResult = erOK) and (FTextIndex >= FComboBoxList.Count) then
    ModalResult := mrOk;
end;

procedure TJclSIMDModifyFrm.ButtonOKClick(Sender: TObject);
begin
  try
    StartModify;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

procedure TJclSIMDModifyFrm.ThreadEvaluate(const ExprStr, ResultStr: string; ReturnCode: Integer);
begin
  if CompareText(FExprStr, ExprStr) = 0 then
  begin
    FResultStr := ResultStr;
    FReturnCode := ReturnCode;
    PostMessage(Handle, WM_MODIFYCONTINUE, 0, 0);
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
