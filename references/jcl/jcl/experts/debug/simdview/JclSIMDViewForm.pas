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
{ The Original Code is: JvSIMDViewForm.pas, released on 2004-10-11.                                }
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

unit JclSIMDViewForm;

{$I jcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ToolsApi, Grids, ExtCtrls, Menus, ActnList,
  DockForm,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclOtaUtils, JclSysInfo, JclSIMDUtils, JclSIMDModifyForm;

type
  TJclSIMDViewFrm = class(TDockableForm)
    Splitter: TSplitter;
    ListBoxRegs: TListBox;           
    ListBoxMXCSR: TListBox;
    PopupMenuRegs: TPopupMenu;
    PopupMenuMXCSR: TPopupMenu;
    MenuItemComplement: TMenuItem;
    MenuItemBinary: TMenuItem;
    MenuItemSigned: TMenuItem;
    MenuItemUnsigned: TMenuItem;
    MenuItemHexa: TMenuItem;
    MenuItemDisplay: TMenuItem;
    MenuItemFormat: TMenuItem;
    MenuItemBytes: TMenuItem;
    MenuItemWords: TMenuItem;
    MenuItemDWords: TMenuItem;
    MenuItemQWords: TMenuItem;
    MenuItemSeparator1: TMenuItem;
    MenuItemSingles: TMenuItem;
    MenuItemDoubles: TMenuItem;
    MenuItemSeparator2: TMenuItem;
    MenuItemStayOnTop: TMenuItem;
    MenuItemModify: TMenuItem;
    MenuItemCpuInfo: TMenuItem;
    ActionListOptions: TActionList;
    ActionStayOnTop: TAction;
    ActionModify: TAction;
    ActionComplement: TAction;
    ActionEmpty: TAction;
    ActionEmptyAll: TAction;
    MenuItemEmptyMM: TMenuItem;
    MenuItemEmptyAll: TMenuItem;
    ActionYMMEnabled: TAction;
    MenuItemYMMEnabled: TMenuItem;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListBoxMXCSRDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure ListBoxMXCSRMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ListBoxRegsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure MenuItemFormatClick(Sender: TObject);
    procedure MenuItemDisplayClick(Sender: TObject);
    procedure MenuItemCpuInfoClick(Sender: TObject);
    procedure ActionStayOnTopUpdate(Sender: TObject);
    procedure ActionStayOnTopExecute(Sender: TObject);
    procedure ActionModifyUpdate(Sender: TObject);
    procedure ActionModifyExecute(Sender: TObject);
    procedure ActionComplementExecute(Sender: TObject);
    procedure ActionComplementUpdate(Sender: TObject);
    procedure ActionEmptyUpdate(Sender: TObject);
    procedure ActionEmptyAllUpdate(Sender: TObject);
    procedure ActionEmptyExecute(Sender: TObject);
    procedure ActionEmptyAllExecute(Sender: TObject);
    procedure ListBoxesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ActionYMMEnabledUpdate(Sender: TObject);
    procedure ActionYMMEnabledExecute(Sender: TObject);
  private
    FDebuggerServices: IOTADebuggerServices;
    FJclContext: TJclContext;
    FDisplay: TJclPackedContentType;
    FFormat: TJclSIMDFormat;
    FCpuInfo: TCpuInfo;
    FEnabledFeatures: TOSEnabledFeatures;
    FSIMDCaption: string;
    FNbMMRegister: Integer;
    FNbXMMRegister: Integer;
    FYMMEnabled: Boolean;
    FOldThreadID: LongWord;
    FOldThreadState: TOTAThreadState;
    FModifyForm: TJclSIMDModifyFrm;
    FMXCSRChanged: array [TMXCSRRange] of Boolean;
    FRegisterChanged: array of Boolean;
    FSettings: TJclOtaSettings;
    procedure SetDisplay(const Value: TJclPackedContentType);
    procedure SetFormat(const Value: TJclSIMDFormat);
    procedure SetYMMEnabled(const Value: Boolean);
  protected
    procedure DoClose(var Action: TCloseAction); override;
    procedure UpdateActions; override;
    // not for dockable windows
    //procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent; ADebuggerServices: IOTADebuggerServices;
      ASettings: TJclOtaSettings); reintroduce;
    destructor Destroy; override;
    procedure ThreadEvaluate(const ExprStr, ResultStr: string; ReturnCode: Integer);
    procedure SetThreadValues;
    procedure GetThreadValues;
    property CpuInfo: TCpuInfo read FCpuInfo;
    property EnabledFeatures: TOSEnabledFeatures read FEnabledFeatures;
    property Format: TJclSIMDFormat read FFormat write SetFormat;
    property Display: TJclPackedContentType read FDisplay write SetDisplay;
    property SIMDCaption: string read FSIMDCaption write FSIMDCaption;
    property DebuggerServices: IOTADebuggerServices read FDebuggerServices;
    property NbMMRegister: Integer read FNbMMRegister;
    property NbXMMRegister: Integer read FNbXMMRegister;
    property YMMEnabled: Boolean read FYMMEnabled write SetYMMEnabled;
    property Settings: TJclOtaSettings read FSettings;
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

uses
  TypInfo,
  JclOtaResources, JclOtaConsts,
  JclSIMDCpuInfo;

{$R *.dfm}

constructor TJclSIMDViewFrm.Create(AOwner: TComponent;
  ADebuggerServices: IOTADebuggerServices; ASettings: TJclOTASettings);
var
  I: TMXCSRRange;
  J: Integer;
begin
  inherited Create(AOwner);

  FDebuggerServices := ADebuggerServices;
  FOldThreadID := 0;
  FOldThreadState := tsNone;
  FSettings := ASettings;

  JclSysInfo.GetCpuInfo(FCpuInfo);
  FEnabledFeatures := GetOSEnabledFeatures;

  // the behaviour of Delphi and C++Builder overrides all changes made on
  // the floating point context of the debugged thread when it is run
  // (even using step into and step over).
  // to be uncommented as soon as Borland changes this behaviour
  {if CpuInfo.MMX or CPUInfo._3DNow then
    FNbMMRegister := 8
  else
    FNbMMRegister := 0;}

  FNbMMRegister := 0;

  if CpuInfo.SSE = [] then
    FNbXMMRegister := 0
  else
  if CpuInfo.Is64Bits then
    FNbXMMRegister := 17
  else
    FNbXMMRegister := 9;

  FYMMEnabled := (avx in CpuInfo.SSE) and (oefAVX in EnabledFeatures);

  ListBoxMXCSR.Items.Clear;
  for I := Low(TMXCSRRange) to High(TMXCSRRange) do
    ListBoxMXCSR.Items.Add('0');

  ListBoxRegs.Items.Clear;
  SetLength(FRegisterChanged,NbMMRegister + NbXMMRegister);
  for J := 0 to NbMMRegister + NbXMMRegister - 1 do
  // MM registers (MMX) + XMM registers (SSE) + 1 cardinal (MXCSR)
    ListBoxRegs.Items.Add('');

  MenuItemBinary.Tag := Integer(sfBinary);
  MenuItemSigned.Tag := Integer(sfSigned);
  MenuItemUnsigned.Tag := Integer(sfUnsigned);
  MenuItemHexa.Tag := Integer(sfHexa);
  MenuItemBytes.Tag := Integer(pctBytes);
  MenuItemWords.Tag := Integer(pctWords);
  MenuItemDWords.Tag := Integer(pctDWords);
  MenuItemQWords.Tag := Integer(pctQWords);
  MenuItemSingles.Tag := Integer(pctSingles);
  MenuItemDoubles.Tag := Integer(pctDoubles);

  Format := sfHexa;
  Display := pctWords;

  GetThreadValues;
end;

// not for dockable windows
{procedure TJclSIMDViewFrm.CreateParams(var Params: TCreateParams);
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
end;}

destructor TJclSIMDViewFrm.Destroy;
begin
  SetLength(FRegisterChanged,0);
  FDebuggerServices := nil;

  inherited Destroy;
end;

procedure TJclSIMDViewFrm.ListBoxMXCSRDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  try
    with (Control as TListBox), Canvas do
    begin
      if not (odFocused in State) then
      begin
        Pen.Color := Brush.Color;
        if odSelected in State then
          Font.Color := clWindow;
      end;
      Rectangle(Rect);
      TextOut(Rect.Left + 2, Rect.Top, LoadResString(MXCSRBitsDescriptions[Index].ShortName));
      if FMXCSRChanged[Index] then
        Font.Color := clRed;
      TextOut(Rect.Left + 2 + TextExtent(LoadResString(MXCSRBitsDescriptions[Index].ShortName)).cx, Rect.Top, Items[Index]);
    end;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

procedure TJclSIMDViewFrm.ListBoxMXCSRMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  AIndex: Integer;
  AText: string;
begin
  try
    if Shift <> [] then
      Application.HideHint
    else
      with Sender as TListBox do
      begin
        AIndex := ItemAtPos(Point(X,Y),True);
        if (AIndex >= 0) and (AIndex < Items.Count) then
        begin
          with MXCSRBitsDescriptions[AIndex] do
          begin
            AText := LoadResString(LongName);
            if AndMask = MXCSR_RC then
              case (FJclContext.ExtendedContext.SaveArea.MXCSR and AndMask) shr Shifting of
                0:
                  AText := SysUtils.Format('%s (%s)', [AText, LoadResString(@RsRoundToNearest)]);
                1:
                  AText := SysUtils.Format('%s (%s)', [AText, LoadResString(@RsRoundDown)]);
                2:
                  AText := SysUtils.Format('%s (%s)', [AText, LoadResString(@RsRoundUp)]);
                3:
                  AText := SysUtils.Format('%s (%s)', [AText, LoadResString(@RsRoundTowardZero)]);
            end;
            if AText <> Hint then
            begin
              Hint := AText;
              Application.HideHint;
              Application.ActivateHint(Point(X, Y));
            end;
          end;
        end
        else
        begin
          Hint := '';
          Application.HideHint;
        end;
      end;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

procedure TJclSIMDViewFrm.ListBoxRegsDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  AText, RegName: string;
begin
  try
    with (Control as TListBox), Canvas do
    begin
      if not (odFocused in State) then
      begin
        Pen.Color := Brush.Color;
        if odSelected in State then
          Font.Color := clWindow;
      end;
      Rectangle(Rect);
      if Index < NbMMRegister then
        AText := SysUtils.Format('MM%d ', [Index])
      else
      if Index < NbMMRegister + NbXMMRegister - 1 then
      begin
        if YMMEnabled then
          RegName := 'YMM'
        else
          RegName := 'XMM';
        if CpuInfo.Is64Bits then
          AText := SysUtils.Format('%s%.2d ', [RegName, Index - NbMMRegister])
        else
          AText := SysUtils.Format('%s%d ', [RegName, Index - NbMMRegister]);
      end
      else
        AText := 'MXCSR ';
      TextOut(Rect.Left + 2, Rect.Top, AText);
      if FRegisterChanged[Index] then
        Font.Color := clRed;
      TextOut(Rect.Left + 2 + TextExtent(AText).cx, Rect.Top, Items[Index]);
    end;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

procedure TJclSIMDViewFrm.GetThreadValues;
var
  NewJclContext: TJclContext;
  NewBitValue, OldBitValue: Cardinal;
  Index: Integer;
  AProcess: IOTAProcess;
  AThread: IOTAThread;

  function ChangedFlag(const Value1, Value2: TJclXMMRegister): Boolean; overload;
  begin
    Result := (Value1.QWords[0] <> Value2.QWords[0]) or (Value1.QWords[1] <> Value2.QWords[1]);
  end;

  function ChangedFlag(const Value1, Value2: TJclYMMRegister): Boolean; overload;
  begin
    Result := (Value1.QWords[2] <> Value2.QWords[2]) or (Value1.QWords[3] <> Value2.QWords[3]);
  end;

  function ChangedFlag(const Value1, Value2: TJclMMRegister): Boolean; overload;
  begin
    Result := Value1.QWords <> Value2.QWords;
  end;

  function FormatReg(const XMMReg: TJclXMMRegister; const YMMReg: TJclYMMRegister): string; overload;
  var
    I: Integer;
    Value: TJclSIMDValue;
  begin
    Result := '';
    Value.Display := Display;
    case Display of
      pctBytes:
        begin
          if YMMEnabled then
            for I := High(YMMReg.Bytes) downto Low(YMMReg.Bytes) do
          begin
            Value.ValueByte := YMMReg.Bytes[I];
            Result := Result + ' ' + FormatValue(Value, Format);
          end;
          for I := High(XMMReg.Bytes) downto Low(XMMReg.Bytes) do
          begin
            Value.ValueByte := XMMReg.Bytes[I];
            Result := Result + ' ' + FormatValue(Value, Format);
          end;
        end;
      pctWords:
        begin
          if YMMEnabled then
            for I := High(YMMReg.Words) downto Low(YMMReg.Words) do
          begin
            Value.ValueWord := YMMReg.Words[I];
            Result := Result + ' ' + FormatValue(Value, Format);
          end;
          for I := High(XMMReg.Words) downto Low(XMMReg.Words) do
          begin
            Value.ValueWord := XMMReg.Words[I];
            Result := Result + ' ' + FormatValue(Value, Format);
          end;
        end;
      pctDWords:
        begin
          if YMMEnabled then
            for I := High(YMMReg.DWords) downto Low(YMMReg.DWords) do
          begin
            Value.ValueDWord := YMMReg.DWords[I];
            Result := Result + ' ' + FormatValue(Value, Format);
          end;
          for I := High(XMMReg.DWords) downto Low(XMMReg.DWords) do
          begin
            Value.ValueDWord := XMMReg.DWords[I];
            Result := Result + ' ' + FormatValue(Value, Format);
          end;
        end;
      pctQWords:
        begin
          if YMMEnabled then
            for I := High(YMMReg.QWords) downto Low(YMMReg.QWords) do
          begin
            Value.ValueQWord := YMMReg.QWords[I];
            Result := Result + ' ' + FormatValue(Value, Format);
          end;
          for I := High(XMMReg.QWords) downto Low(XMMReg.QWords) do
          begin
            Value.ValueQWord := XMMReg.QWords[I];
            Result := Result + ' ' + FormatValue(Value, Format);
          end;
        end;
      pctSingles:
        begin
          if YMMEnabled then
            for I := High(YMMReg.Singles) downto Low(YMMReg.Singles) do
          begin
            Value.ValueSingle := YMMReg.Singles[I];
            Result := Result + ' ' + FormatValue(Value, Format);
          end;
          for I := High(XMMReg.Singles) downto Low(XMMReg.Singles) do
          begin
            Value.ValueSingle := XMMReg.Singles[I];
            Result := Result + ' ' + FormatValue(Value, sfBinary);
          end;
        end;
      pctDoubles:
        begin
          if YMMEnabled then
            for I := High(YMMReg.Doubles) downto Low(YMMReg.Doubles) do
          begin
            Value.ValueDouble := YMMReg.Doubles[I];
            Result := Result + ' ' + FormatValue(Value, Format);
          end;
          for I := High(XMMReg.Doubles) downto Low(XMMReg.Doubles) do
          begin
            Value.ValueDouble := XMMReg.Doubles[I];
            Result := Result + ' ' + FormatValue(Value, sfBinary);
          end;
        end;
    end;
  end;

  function FormatReg(const AReg: TJclFPUData; Index: Cardinal): string; overload;
  var
    I: Integer;
    Value: TJclSIMDValue;
  begin
    Result := '';
    Value.Display := Display;

    if (AReg.Reserved = $FFFF) and ((NewJclContext.ExtendedContext.SaveArea.FTW and (1 shl Index)) <> 0) then
      case Display of
        pctBytes:
          for I := High(AReg.MMRegister.Bytes) downto Low(AReg.MMRegister.Bytes) do
          begin
            Value.ValueByte := AReg.MMRegister.Bytes[I];
            Result := Result + ' ' + FormatValue(Value, Format);
          end;
        pctWords:
          for I := High(AReg.MMRegister.Words) downto Low(AReg.MMRegister.Words) do
          begin
            Value.ValueWord := AReg.MMRegister.Words[I];
            Result := Result + ' ' + FormatValue(Value, Format);
          end;
        pctDWords:
          for I := High(AReg.MMRegister.DWords) downto Low(AReg.MMRegister.DWords) do
          begin
            Value.ValueDWord := AReg.MMRegister.DWords[I];
            Result := Result + ' ' + FormatValue(Value, Format);
          end;
        pctQWords:
          begin
            Value.ValueQWord := AReg.MMRegister.QWords;
            Result := FormatValue(Value, Format);
          end;
        pctSingles:
          for I := High(AReg.MMRegister.Singles) downto Low(AReg.MMRegister.Singles) do
          begin
            Value.ValueSingle := AReg.MMRegister.Singles[I];
            Result := Result + ' ' + FormatValue(Value, sfBinary);
          end;
        pctDoubles:
          Result := LoadResString(@RsNotSupportedFormat);
      end
    else
      Result := LoadResString(@RsNoPackedData);
  end;

begin
  AProcess := nil;
  AThread := nil;
  if DebuggerServices.ProcessCount > 0 then
    AProcess := DebuggerServices.CurrentProcess;
  if (AProcess <> nil) and (AProcess.ThreadCount > 0) then
    AThread := AProcess.CurrentThread;

  if (AThread = nil) or (AThread.State = tsNone) or
    (AThread.GetOSThreadID = 0) or (AThread.Handle = 0) then
  begin
    Close;
    Exit;
  end;

  case AThread.State of
    tsStopped:
      begin
        if DebuggerServices.CurrentProcess.ThreadCount > 1 then
          Caption := SysUtils.Format('%s Thread : %d', [SIMDCaption,AThread.GetOSThreadID])
        else
          Caption := SIMDCaption;

        GetThreadJclContext(AThread,NewJclContext);

        for Index := 0 to ListBoxMXCSR.Items.Count - 1 do
          with ListBoxMXCSR, Items, MXCSRBitsDescriptions[Index] do
          begin
            NewBitValue := NewJclContext.ExtendedContext.SaveArea.MXCSR and AndMask;
            OldBitValue := FJclContext.ExtendedContext.SaveArea.MXCSR and AndMask;
            FMXCSRChanged[Index] := NewBitValue <> OldBitValue;
            Strings[Index] := IntToStr(NewBitValue shr Shifting);
          end;
        ListBoxMXCSR.Invalidate;

        for Index := 0 to NbMMRegister - 1 do
        begin
          FRegisterChanged[Index] := ChangedFlag(NewJclContext.ExtendedContext.SaveArea.FPURegisters[Index].Data.MMRegister,
            FJclContext.ExtendedContext.SaveArea.FPURegisters[Index].Data.MMRegister);
          ListBoxRegs.Items.Strings[Index] := FormatReg(NewJclContext.ExtendedContext.SaveArea.FPURegisters[Index].Data, Index);
        end;

        if FNbXMMRegister > 0 then
        begin
          for Index := 0 to FNbXMMRegister - 2 do
          begin
            FRegisterChanged[Index + NbMMRegister] := ChangedFlag(NewJclContext.ExtendedContext.SaveArea.XMMRegisters.LongXMM[Index],
                                                                  FJclContext.ExtendedContext.SaveArea.XMMRegisters.LongXMM[Index]) or
                                                      (YMMEnabled and ChangedFlag(NewJclContext.ExtendedContext.ExtSaveArea2.LongYMM[Index],
                                                                                  FJclContext.ExtendedContext.ExtSaveArea2.LongYMM[Index]));
            ListBoxRegs.Items.Strings[Index + NbMMRegister] := FormatReg(NewJclContext.ExtendedContext.SaveArea.XMMRegisters.LongXMM[Index],
                                                                         NewJclContext.ExtendedContext.ExtSaveArea2.LongYMM[Index]);
          end;

          FRegisterChanged[NbMMRegister + NbXMMRegister - 1] := NewJclContext.ExtendedContext.SaveArea.MXCSR <> FJclContext.ExtendedContext.SaveArea.MXCSR;
          ListBoxRegs.Items.Strings[NbMMRegister + NbXMMRegister - 1] := IntToHex(NewJclContext.ExtendedContext.SaveArea.MXCSR, 8);
        end;
        ListBoxRegs.Invalidate;

        FJclContext := NewJclContext;
      end;
    tsRunnable:
      Caption := SysUtils.Format('%s <running>', [SIMDCaption]);
    tsBlocked:
      Caption := SysUtils.Format('%s <blocked>', [SIMDCaption]);
  end;
end;

procedure TJclSIMDViewFrm.SetThreadValues;
begin
  if not SetThreadJclContext(DebuggerServices.CurrentProcess.CurrentThread,FJclContext) then
    raise EJclExpertException.CreateRes(@RsECantUpdateThreadContext);
end;

procedure TJclSIMDViewFrm.SetYMMEnabled(const Value: Boolean);
begin
  FYMMEnabled := Value;
  GetThreadValues;
end;

procedure TJclSIMDViewFrm.MenuItemFormatClick(Sender: TObject);
begin
  try
    Format := TJclSIMDFormat((Sender as TMenuItem).Tag);
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

procedure TJclSIMDViewFrm.SetDisplay(const Value: TJclPackedContentType);
var
  AEnabled: Boolean;
begin
  FDisplay := Value;
  MenuItemBytes.Checked := Value = pctBytes;
  MenuItemWords.Checked := Value = pctWords;
  MenuItemDWords.Checked := Value = pctDWords;
  MenuItemQWords.Checked := Value = pctQWords;
  MenuItemSingles.Checked := Value = pctSingles;
  MenuItemDoubles.Checked := Value = pctDoubles;

  AEnabled := not (Value in [pctSingles, pctDoubles]);
  MenuItemBinary.Enabled := AEnabled;
  MenuItemSigned.Enabled := AEnabled;
  MenuItemUnsigned.Enabled := AEnabled;
  MenuItemHexa.Enabled := AEnabled;

  GetThreadValues;
end;

procedure TJclSIMDViewFrm.SetFormat(const Value: TJclSIMDFormat);
begin
  FFormat := Value;
  MenuItemBinary.Checked := Value = sfBinary;
  MenuItemSigned.Checked := Value = sfSigned;
  MenuItemUnsigned.Checked := Value = sfUnsigned;
  MenuItemHexa.Checked := Value = sfHexa;

  GetThreadValues;
end;

procedure TJclSIMDViewFrm.MenuItemDisplayClick(Sender: TObject);
begin
  try
    Display := TJclPackedContentType((Sender as TMenuItem).Tag);
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

procedure TJclSIMDViewFrm.DoClose(var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TJclSIMDViewFrm.FormCreate(Sender: TObject);
begin
  MXCSRBitsDescriptions[0].ShortName := @RsVectorIE;
  MXCSRBitsDescriptions[0].LongName := @RsVectorIEText;
  MXCSRBitsDescriptions[1].ShortName := @RsVectorDE;
  MXCSRBitsDescriptions[1].LongName := @RsVectorDEText;
  MXCSRBitsDescriptions[2].ShortName := @RsVectorZE;
  MXCSRBitsDescriptions[2].LongName := @RsVectorZEText;
  MXCSRBitsDescriptions[3].ShortName := @RsVectorOE;
  MXCSRBitsDescriptions[3].LongName := @RsVectorOEText;
  MXCSRBitsDescriptions[4].ShortName := @RsVectorUE;
  MXCSRBitsDescriptions[4].LongName := @RsVectorUEText;
  MXCSRBitsDescriptions[5].ShortName := @RsVectorPE;
  MXCSRBitsDescriptions[5].LongName := @RsVectorPEText;
  MXCSRBitsDescriptions[6].ShortName := @RsVectorDAZ;
  MXCSRBitsDescriptions[6].LongName := @RsVectorDAZText;
  MXCSRBitsDescriptions[7].ShortName := @RsVectorIM;
  MXCSRBitsDescriptions[7].LongName := @RsVectorIMText;
  MXCSRBitsDescriptions[8].ShortName := @RsVectorDM;
  MXCSRBitsDescriptions[8].LongName := @RsVectorDMText;
  MXCSRBitsDescriptions[9].ShortName := @RsVectorZM;
  MXCSRBitsDescriptions[9].LongName := @RsVectorZMText;
  MXCSRBitsDescriptions[10].ShortName := @RsVectorOM;
  MXCSRBitsDescriptions[10].LongName := @RsVectorOMText;
  MXCSRBitsDescriptions[11].ShortName := @RsVectorUM;
  MXCSRBitsDescriptions[11].LongName := @RsVectorUMText;
  MXCSRBitsDescriptions[12].ShortName := @RsVectorPM;
  MXCSRBitsDescriptions[12].LongName := @RsVectorPMText;
  MXCSRBitsDescriptions[13].ShortName := @RsVectorRC;
  MXCSRBitsDescriptions[13].LongName := @RsVectorRCText;
  MXCSRBitsDescriptions[14].ShortName := @RsVectorFZ;
  MXCSRBitsDescriptions[14].LongName := @RsVectorFZText;

  ActionStayOnTop.Caption := LoadResString(@RsStayOnTop);
  ActionModify.Caption := LoadResString(@RsModify);
  ActionComplement.Caption := LoadResString(@RsComplementBit);
  ActionEmpty.Caption := LoadResString(@RsEmptyMM);
  ActionEmptyAll.Caption := LoadResString(@RsEmptyAllMM);
  ActionYMMEnabled.Caption := LoadResString(@RsViewYMM);
  MenuItemDisplay.Caption := LoadResString(@RsDisplay);
  MenuItemFormat.Caption := LoadResString(@RsFormat);
  MenuItemBinary.Caption := LoadResString(@RsBinary);
  MenuItemSigned.Caption := LoadResString(@RsSignedDecimal);
  MenuItemUnsigned.Caption := LoadResString(@RsUnsignedDecimal);
  MenuItemHexa.Caption := LoadResString(@RsHexadecimal);
  MenuItemCpuInfo.Caption := LoadResString(@RsCPUInfo);

  SetBounds(
    Settings.LoadInteger('Left', Left),
    Settings.LoadInteger('Top', Top),
    Settings.LoadInteger('Width', Width),
    Settings.LoadInteger('Height', Height));

  if Left < 0 then
    Left := 0;
  if Top < 0 then
    Top := 0;
  if Width > Screen.Width then
    Width := Screen.Width;
  if (Left + Width) > Screen.DesktopWidth then
    Left := Screen.DesktopWidth - Width;
  if Height > Screen.Height then
    Height := Screen.Height;
  if (Top + Height) > Screen.DesktopHeight then
    Top := Screen.DesktopHeight - Height;

  Format := TJclSIMDFormat(GetEnumValue(TypeInfo(TJclSIMDFormat),
    Settings.LoadString('Format', GetEnumName(TypeInfo(TJclSIMDFormat), Integer(sfHexa)))));
  Display := TJclPackedContentType(GetEnumValue(TypeInfo(TJclPackedContentType),
    Settings.LoadString('Display', GetEnumName(TypeInfo(TJclPackedContentType), Integer(pctWords)))));
  YMMEnabled := Settings.LoadInteger('YMMEnabled', 0) <> 0;

  if Settings.LoadInteger('StayOnTop', 0) = 1 then
    FormStyle := fsStayOnTop
  else
    FormStyle := fsNormal;
end;

procedure TJclSIMDViewFrm.FormDestroy(Sender: TObject);
begin
  Settings.SaveInteger('Left', Left);
  Settings.SaveInteger('Top', Top);
  Settings.SaveInteger('Width', Width);
  Settings.SaveInteger('Height', Height);
  Settings.SaveString('Display', GetEnumName(TypeInfo(TJclPackedContentType), Integer(Display)));
  Settings.SaveString('Format', GetEnumName(TypeInfo(TJclSIMDFormat), Integer(Format)));
  Settings.SaveInteger('YMMEnabled', Ord(YMMEnabled));
  Settings.SaveInteger('StayOnTop', Ord(FormStyle = fsStayOnTop));
end;

procedure TJclSIMDViewFrm.MenuItemCpuInfoClick(Sender: TObject);
var
  FormCPUInfo: TJclFormCpuInfo;
begin
  try
    FormCPUInfo := TJclFormCpuInfo.Create(Self);
    try
      FormCPUInfo.Execute(CpuInfo, EnabledFeatures);
    finally
      FormCPUInfo.Free;
    end;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

procedure TJclSIMDViewFrm.UpdateActions;
var
  CurrentThreadID: Cardinal;
  AProcess: IOTAProcess;
  AThread: IOTAThread;
  ANewThreadState: TOTAThreadState;
begin
  inherited UpdateActions;

  CurrentThreadID := 0;
  AProcess := nil;
  AThread := nil;

  if DebuggerServices.ProcessCount > 0 then
    AProcess := DebuggerServices.CurrentProcess;
  if (AProcess <> nil) and (AProcess.ThreadCount > 0) then
    AThread := AProcess.CurrentThread;
  if AThread <> nil then
  begin
    ANewThreadState := AThread.State;
    if ANewThreadState in [tsStopped, tsBlocked] then
      CurrentThreadID := AThread.GetOSThreadID;
    if (CurrentThreadID <> 0) and ((CurrentThreadID <> FOldThreadID) or (ANewThreadState <> FOldThreadState)) then
    begin
      FOldThreadID := CurrentThreadID;
      FOldThreadState := ANewThreadState;
      GetThreadValues;
    end;
  end;
end;

procedure TJclSIMDViewFrm.ThreadEvaluate(const ExprStr, ResultStr: string;
  ReturnCode: Integer);
begin
  if Assigned(FModifyForm) then
    FModifyForm.ThreadEvaluate(ExprStr, ResultStr, ReturnCode);
end;

procedure TJclSIMDViewFrm.ActionStayOnTopUpdate(Sender: TObject);
var
  AAction: TAction;
begin
  try
    AAction := Sender as TAction;
    AAction.Checked := FormStyle = fsStayOnTop;
    AAction.Enabled := True;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

procedure TJclSIMDViewFrm.ActionYMMEnabledExecute(Sender: TObject);
begin
  try
    YMMEnabled := not YMMEnabled;
    GetThreadValues;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

procedure TJclSIMDViewFrm.ActionYMMEnabledUpdate(Sender: TObject);
var
  AAction: TAction;
begin
  try
    AAction := Sender as TAction;
    AAction.Checked := YMMEnabled;
    AAction.Enabled := True;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

procedure TJclSIMDViewFrm.ActionStayOnTopExecute(Sender: TObject);
begin
  try
    if FormStyle = fsStayOnTop then
      FormStyle := fsNormal
    else
      FormStyle := fsStayOnTop;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

procedure TJclSIMDViewFrm.ActionModifyUpdate(Sender: TObject);
var
  AProcess: IOTAProcess;
  AThread: IOTAThread;
  AItemIndex: Integer;
begin
  try
    AProcess := DebuggerServices.CurrentProcess;
    AThread := nil;
    AItemIndex := ListBoxRegs.ItemIndex;
    if NbXMMRegister > 0 then
      Inc(AItemIndex);

    if Assigned(AProcess) then
      AThread := AProcess.CurrentThread;

    (Sender as TAction).Enabled := Assigned(AThread) and (AThread.State = tsStopped) and
      (AItemIndex >= 0) and (AItemIndex < (NbMMRegister + NbXMMRegister));
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

procedure TJclSIMDViewFrm.ActionModifyExecute(Sender: TObject);
var
  AItemIndex: Integer;
begin
  try
    AItemIndex := ListBoxRegs.ItemIndex;
    if AItemIndex >= 0 then
    try
      FModifyForm := TJclSIMDModifyFrm.Create(Self, DebuggerServices, Settings);
      FModifyForm.Icon.Assign(Self.Icon);

      if AItemIndex < NbMMRegister then
      begin
        FModifyForm.Caption := SysUtils.Format(LoadResString(@RsModifyMM), [AItemIndex]);
        if FModifyForm.Execute(DebuggerServices.CurrentProcess.CurrentThread, Display,
          Format, FJclContext.ExtendedContext.SaveArea.FPURegisters[AItemIndex].Data.MMRegister ,FCpuInfo, YMMEnabled) then
        begin
          FJclContext.ExtendedContext.SaveArea.FPURegisters[AItemIndex].Data.Reserved := $FFFF;
          FJclContext.ExtendedContext.SaveArea.FTW := FJclContext.ExtendedContext.SaveArea.FTW or (1 shl AItemIndex);
          SetThreadValues;
          GetThreadValues;
          FRegisterChanged[AItemIndex] := True;
          ListBoxRegs.Invalidate;
        end;
      end else
      begin
        if YMMEnabled then
        begin
          if CpuInfo.Is64Bits then
            FModifyForm.Caption := SysUtils.Format(LoadResString(@RsModifyYMM2), [AItemIndex - NbMMRegister])
          else
            FModifyForm.Caption := SysUtils.Format(LoadResString(@RsModifyYMM1), [AItemIndex - NbMMRegister]);
          if FModifyForm.Execute(DebuggerServices.CurrentProcess.CurrentThread, Display,
            Format, FJclContext.ExtendedContext.SaveArea.XMMRegisters.LongXMM[AItemIndex - NbMMRegister],
            FJclContext.ExtendedContext.ExtSaveArea2.LongYMM[AItemIndex - NbMMRegister], FCpuInfo, YMMEnabled) then
        end
        else
        begin
          if CpuInfo.Is64Bits then
            FModifyForm.Caption := SysUtils.Format(LoadResString(@RsModifyXMM2), [AItemIndex - NbMMRegister])
          else
            FModifyForm.Caption := SysUtils.Format(LoadResString(@RsModifyXMM1), [AItemIndex - NbMMRegister]);
          if FModifyForm.Execute(DebuggerServices.CurrentProcess.CurrentThread, Display,
            Format, FJclContext.ExtendedContext.SaveArea.XMMRegisters.LongXMM[AItemIndex - NbMMRegister], FCpuInfo, YMMEnabled) then
        end;
        begin
          SetThreadValues;
          GetThreadValues;
          FRegisterChanged[AItemIndex] := True;
          ListBoxRegs.Invalidate;
        end;
      end;
    finally
      FreeAndNil(FModifyForm);
    end;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

procedure TJclSIMDViewFrm.ActionEmptyUpdate(Sender: TObject);
var
  AProcess: IOTAProcess;
  AThread: IOTAThread;
  AItemIndex: Integer;
begin
  try
    AProcess := DebuggerServices.CurrentProcess;
    AThread := nil;
    AItemIndex := ListBoxRegs.ItemIndex;
    if Assigned(AProcess) then
      AThread := AProcess.CurrentThread;
    (Sender as TAction).Enabled := Assigned(AThread) and (AThread.State = tsStopped) and
      (AItemIndex >= 0) and (AItemIndex < NbMMRegister) and
      ((FJclContext.ExtendedContext.SaveArea.FTW and (1 shl AItemIndex)) <> 0) and
      (FJclContext.ExtendedContext.SaveArea.FPURegisters[AItemIndex].Data.Reserved = $FFFF);
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

procedure TJclSIMDViewFrm.ActionEmptyExecute(Sender: TObject);
var
  AItemIndex: Integer;
begin
  try
    AItemIndex := ListBoxRegs.ItemIndex;
    FJclContext.ExtendedContext.SaveArea.FTW := FJclContext.ExtendedContext.SaveArea.FTW and not (1 shl AItemIndex);
    FJclContext.ExtendedContext.SaveArea.FPURegisters[AItemIndex].Data.FloatValue := 0.0;
    SetThreadValues;
    GetThreadValues;
    FRegisterChanged[AItemIndex] := True;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

procedure TJclSIMDViewFrm.ActionEmptyAllUpdate(Sender: TObject);
var
  AProcess: IOTAProcess;
  AThread: IOTAThread;
  AItemIndex: Integer;
begin
  try
    AProcess := DebuggerServices.CurrentProcess;
    AThread := nil;
    AItemIndex := ListBoxRegs.ItemIndex;
    if Assigned(AProcess) then
      AThread := AProcess.CurrentThread;
    (Sender as TAction).Enabled := (AItemIndex >= 0) and (AItemIndex < NbMMRegister) and
      Assigned(AThread) and (AThread.State = tsStopped);
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

procedure TJclSIMDViewFrm.ActionEmptyAllExecute(Sender: TObject);
var
  Index: Integer;
begin
  try
    FJclContext.ExtendedContext.SaveArea.FTW := 0;
    for Index := Low(FJclContext.ExtendedContext.SaveArea.FPURegisters) to High(FJclContext.ExtendedContext.SaveArea.FPURegisters) do
      FJclContext.ExtendedContext.SaveArea.FPURegisters[Index].Data.FloatValue := 0.0;
    SetThreadValues;
    GetThreadValues;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

procedure TJclSIMDViewFrm.ActionComplementUpdate(Sender: TObject);
begin
  try
    (Sender as TAction).Enabled := ListBoxMXCSR.ItemIndex >= 0;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

procedure TJclSIMDViewFrm.ActionComplementExecute(Sender: TObject);
var
  BitValue: Cardinal;
  OldMXCSRValue: Cardinal;
begin
  try
    if ListBoxMXCSR.ItemIndex >= 0 then
      with MXCSRBitsDescriptions[ListBoxMXCSR.ItemIndex] do
    begin
      OldMXCSRValue := FJclContext.ExtendedContext.SaveArea.MXCSR;
      BitValue := (Cardinal(FJclContext.ExtendedContext.SaveArea.MXCSR) and AndMask) shr Shifting;
      Inc(BitValue);
      FJclContext.ExtendedContext.SaveArea.MXCSR := (FJclContext.ExtendedContext.SaveArea.MXCSR and (not AndMask)) or ((BitValue shl Shifting) and AndMask);
      SetThreadValues;
      FJclContext.ExtendedContext.SaveArea.MXCSR := OldMXCSRValue;
      GetThreadValues;
    end;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

procedure TJclSIMDViewFrm.ListBoxesMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AListBox: TListBox;
begin
  try
    if Button = mbRight then
    begin
      AListBox := Sender as TListBox;
      AListBox.ItemIndex := AListBox.ItemAtPos(Point(X, Y), True);
    end;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
