unit Main;

interface

{$I Alcinoe.inc}
{$SCOPEDENUMS OFF}

uses
  Vcl.Forms, dxBarBuiltInMenu, cxGraphics, dxUIAClasses, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, Vcl.Menus,
  cxStyles, cxCustomData, cxFilter, cxData, cxDataStorage, cxNavigator,
  dxDateRanges, dxScrollbarAnnotations, dxCore, cxClasses, dxSkinsForm,
  cxGridLevel, cxGridCustomTableView, cxGridTableView, cxGridCustomView, cxGrid,
  Vcl.StdCtrls, cxButtons, Vcl.ComCtrls, cxTextEdit, cxMemo, cxLabel, cxPC,
  System.Classes, Vcl.Controls, Alcinoe.StringList, cxGeometry, dxFramedControl,
  dxPanel, System.Generics.Collections, Alcinoe.CodeProfiler, cxDateUtils,
  cxDropDownEdit, cxCalendar, cxMaskEdit, Vcl.ExtCtrls, cxCurrencyEdit,
  dxSkinsCore, dxSkinBasic, dxSkinBlack, dxSkinBlue, dxSkinBlueprint,
  dxSkinCaramel, dxSkinCoffee, dxSkinDarkroom, dxSkinDarkSide,
  dxSkinDevExpressDarkStyle, dxSkinDevExpressStyle, dxSkinFoggy,
  dxSkinGlassOceans, dxSkinHighContrast, dxSkiniMaginary, dxSkinLilian,
  dxSkinLiquidSky, dxSkinLondonLiquidSky, dxSkinMcSkin, dxSkinMetropolis,
  dxSkinMetropolisDark, dxSkinMoneyTwins, dxSkinOffice2007Black,
  dxSkinOffice2007Blue, dxSkinOffice2007Green, dxSkinOffice2007Pink,
  dxSkinOffice2007Silver, dxSkinOffice2010Black, dxSkinOffice2010Blue,
  dxSkinOffice2010Silver, dxSkinOffice2013DarkGray, dxSkinOffice2013LightGray,
  dxSkinOffice2013White, dxSkinOffice2016Colorful, dxSkinOffice2016Dark,
  dxSkinOffice2019Black, dxSkinOffice2019Colorful, dxSkinOffice2019DarkGray,
  dxSkinOffice2019White, dxSkinPumpkin, dxSkinSeven, dxSkinSevenClassic,
  dxSkinSharp, dxSkinSharpPlus, dxSkinSilver, dxSkinSpringtime, dxSkinStardust,
  dxSkinSummer2008, dxSkinTheAsphaltWorld, dxSkinTheBezier, dxSkinValentine,
  dxSkinVisualStudio2013Blue, dxSkinVisualStudio2013Dark,
  dxSkinVisualStudio2013Light, dxSkinVS2010, dxSkinWhiteprint, dxSkinWXI,
  dxSkinXmas2008Blue, cxTL, cxTLdxBarBuiltInMenu, cxInplaceContainer, cxTreeView,
  cxTLData, cxSplitter, IdBaseComponent, IdComponent, IdCustomTCPServer,
  IdCustomHTTPServer, IdHTTPServer, IdContext, dxStatusBar, System.SysUtils,
  System.SyncObjs, cxRadioGroup, cxCheckBox;

type

  TMainForm = class(TForm)
    dxSkinController: TdxSkinController;
    cxStyleRepository: TcxStyleRepository;
    MainPageControl: TcxPageControl;
    InstrumentationTabSheet: TcxTabSheet;
    PerformanceAnalysisTabSheet: TcxTabSheet;
    Panelfilter: TPanel;
    ProcNameFilterEdit: TcxTextEdit;
    ApplyFilterBtn: TcxButton;
    TreeListProcMetrics: TcxTreeList;
    TreeListProcMetricsColumnProcName: TcxTreeListColumn;
    TreeListProcMetricsColumnThreadID: TcxTreeListColumn;
    TreeListProcMetricsColumnTimeTaken: TcxTreeListColumn;
    TreeListProcMetricsColumnExecutionID: TcxTreeListColumn;
    GridProcMetrics: TcxGrid;
    GridTableViewProcMetrics: TcxGridTableView;
    GridTableViewProcMetricsColumnExecutionID: TcxGridColumn;
    GridTableViewProcMetricsColumnProcName: TcxGridColumn;
    GridTableViewProcMetricsColumnThreadID: TcxGridColumn;
    GridTableViewProcMetricsColumnTimeTaken: TcxGridColumn;
    GridTableViewProcMetricsColumnCallCount: TcxGridColumn;
    GridLevelProcMetrics: TcxGridLevel;
    LoadDataBtn: TcxButton;
    InstructionPanel: TdxPanel;
    cxLabel1: TcxLabel;
    cxLabel2: TcxLabel;
    cxLabel3: TcxLabel;
    cxLabel4: TcxLabel;
    cxLabel5: TcxLabel;
    LastInstructionLabel: TcxLabel;
    dxPanel2: TdxPanel;
    SourcesPathMemo: TcxMemo;
    cxLabel9: TcxLabel;
    dxPanel3: TdxPanel;
    cxSplitter1: TcxSplitter;
    cxStyleTreeListProcMetricsBackground: TcxStyle;
    TreeListProcMetricsColumnStartTimeStamp: TcxTreeListColumn;
    TreeListProcMetricsColumnCallCount: TcxTreeListColumn;
    GridTableViewProcMetricsColumnStartTimestamp: TcxGridColumn;
    StartTimeStampMinEdit: TcxMaskEdit;
    Label1: TLabel;
    StartTimeStampMaxEdit: TcxMaskEdit;
    Label2: TLabel;
    IdHTTPServer: TIdHTTPServer;
    cxLabel10: TcxLabel;
    dxPanel1: TdxPanel;
    InsertProfilerMarkersBtn: TcxButton;
    RemoveProfilerMarkersBtn: TcxButton;
    HttpServerPortEdit: TcxMaskEdit;
    cxLabel11: TcxLabel;
    HttpServerNameEdit: TcxMaskEdit;
    cxLabel12: TcxLabel;
    MainStatusBar: TdxStatusBar;
    cxLabel13: TcxLabel;
    cxLabel14: TcxLabel;
    ClearDataBtn: TcxButton;
    ExportToCsvBtn: TcxButton;
    dxPanel4: TdxPanel;
    DoNotGroupRadioButton: TcxRadioButton;
    GroupCallsByProcIDRadioButton: TcxRadioButton;
    GroupCallsByCallStackRadioButton: TcxRadioButton;
    cxLabel15: TcxLabel;
    dxPanel5: TdxPanel;
    BrowseCodeProfilerIncFilenameBtn: TcxButton;
    CodeProfilerIncFilenameEdit: TcxTextEdit;
    dxPanel6: TdxPanel;
    CodeProfilerEnabledCheckBox: TcxCheckBox;
    dxPanel7: TdxPanel;
    IgnoreThreadIDCheckBox: TcxCheckBox;
    cxLabel7: TcxLabel;
    cxLabel16: TcxLabel;
    procedure InsertProfilerMarkersBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LoadDataBtnClick(Sender: TObject);
    procedure ClearDataBtnClick(Sender: TObject);
    procedure GridTableViewProcMetricsCellDblClick(
                Sender: TcxCustomGridTableView;
                ACellViewInfo: TcxGridTableDataCellViewInfo;
                AButton: TMouseButton;
                AShift: TShiftState;
                var AHandled: Boolean);
    procedure TreeListProcMetricsDblClick(Sender: TObject);
    procedure RemoveProfilerMarkersBtnClick(Sender: TObject);
    procedure ApplyFilterBtnClick(Sender: TObject);
    procedure InstrumentationTabSheetResize(Sender: TObject);
    procedure GridTableViewProcMetricsColumnStartTimestampGetDisplayText(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; var AText: string);
    procedure TreeListProcMetricsColumnStartTimeStampGetDisplayText(Sender: TcxTreeListColumn; ANode: TcxTreeListNode; var Value: string);
    procedure PanelfilterResize(Sender: TObject);
    procedure HttpServerPortEditPropertiesChange(Sender: TObject);
    procedure HttpServerNameEditPropertiesChange(Sender: TObject);
    procedure IdHTTPServerCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure IdHTTPServerException(AContext: TIdContext; AException: Exception);
    procedure IdHTTPServerListenException(AThread: TIdListenerThread; AException: Exception);
    procedure IdHTTPServerConnect(AContext: TIdContext);
    procedure ExportToCsvBtnClick(Sender: TObject);
    procedure HistoryGroupModeRadioButtonClick(Sender: TObject);
    procedure CodeProfilerIncFilenameEditPropertiesChange(Sender: TObject);
    procedure BrowseCodeProfilerIncFilenameBtnClick(Sender: TObject);
    procedure CodeProfilerEnabledCheckBoxPropertiesChange(Sender: TObject);
    procedure IgnoreThreadIDCheckBoxPropertiesChange(Sender: TObject);
  private
    const ConfigFilename = 'Config.ini';
    // Default value of ALCodeProfilerHistoryCapacity, used for every history
    // group mode but ALCodeProfilerHistoryGroupByProcID, where the capacity
    // is deduced from ALCodeProfilerProcIDMap.txt instead.
    const DefaultHistoryCapacity = 1000000;
  private
    Type
      TGoBackStackItem = record
        TopRowIndex: Int64;
        FocusedRowIndex: Int64;
        SortColumnIndex: Integer;
        SortOrder: TcxGridSortOrder;
      end;
      // Mirrors the ALCodeProfilerHistoryGroupXXX modes selected via the
      // radio buttons at runtime, so this tool never needs to be recompiled
      // just because Alcinoe.CodeProfiler.inc's active mode changed.
      TALCodeProfilerHistoryGroupMode = (hgmNone, hgmByProcID, hgmByCallStack);
      // Raw, on-disk layouts. These intentionally mirror the field lists of
      // Alcinoe.CodeProfiler.TALProcMetrics for each mode so the compiler
      // computes the exact same size/offsets that the profiled app used
      // when writing the .dat file.
      TALProcMetricsNone = record
        ExecutionID: Cardinal;
        ParentExecutionID: Cardinal;
        ProcID: Cardinal;
        ThreadID: Cardinal;
        StartTimeStamp: Int64;
        ElapsedTicks: Int64;
      end;
      TALProcMetricsByProcID = record
        ProcID: Cardinal;
        ThreadID: Cardinal;
        CallCount: Cardinal;
        ElapsedTicks: Int64;
      end;
      TALProcMetricsByCallStack = record
        HashCode: Integer;
        ProcID: Cardinal;
        MetricsID: Cardinal;
        ParentMetricsID: Cardinal;
        ThreadID: Cardinal;
        CallCount: Cardinal;
        ElapsedTicks: Int64;
      end;
      // In-memory, mode-agnostic representation decoded from whichever raw
      // layout above matches FHistoryGroupMode. Fields not meaningful for
      // the current mode are left at 0.
      TALProcMetrics = record
        ExecutionID: Cardinal;
        ParentExecutionID: Cardinal;
        ProcID: Cardinal;
        MetricsID: Cardinal;
        ParentMetricsID: Cardinal;
        ThreadID: Cardinal;
        StartTimeStamp: Int64;
        ElapsedTicks: Int64;
        CallCount: Cardinal;
      end;
      // CSV columns available across the three modes (CSV export).
      TALProcMetricsColumnKind = (
        colkExecutionID, colkParentExecutionID, colkProcID, colkMetricsID, colkParentMetricsID,
        colkProcName, colkThreadID, colkStartTimeStamp, colkCallCount, colkTimeTaken);
  private
    FDataDir: String;
    // Set while the UI is being populated from Config.ini or from
    // Alcinoe.CodeProfiler.inc, so that the change events raised by that
    // population do not write the settings back to Alcinoe.CodeProfiler.inc.
    FLoadingSettings: Boolean;
    FProcIDSequence: Integer;
    FHistoryGroupMode: TALCodeProfilerHistoryGroupMode;
    FProcMetrics: TArray<TALProcMetrics>;
    FProcIDMap: TALHashedStringListA;
    FTreeListProcMetricsTailNode: TcxTreeListNode;
    FTreeListProcMetricsRootNode: TcxTreeListNode;
    FFilterProcIDs: THashSet<Cardinal>;
    FFilterExecutionIDs: THashSet<Cardinal>;
    FFilterParentExecutionIDs: THashSet<Cardinal>;
    FFilterStartTimeStampMin: Int64;
    FFilterStartTimeStampMax: Int64;
    FOverrideFilterParentExecutionID: Cardinal;
    FOverrideFilterThreadID: Cardinal; // High(Cardinal) means "no thread filter"
    FGoBackStack: TDictionary<Int64{ExecutionID}, TGoBackStackItem>;
    FHttpServerCriticalSection: TCriticalSection;
    procedure ResetFilters;
    procedure ResetTreeListProcMetrics;
    Procedure RemoveMarkers(const AFileName: String);
    Procedure InsertMarkers(const AFileName: String; Const AProcIDMap: TALStringListA);
    procedure Refresh;
    function GetSelectedHistoryGroupModeEnum: TALCodeProfilerHistoryGroupMode;
    function GetSelectedHistoryGroupMode: AnsiString;
    procedure SelectHistoryGroupMode(const AHistoryGroupMode: String);
    procedure UpdateHistoryGroupModeUI;
    function GetSelectedProcMetricsFilename: String;
    function GetProcMetricsRawRecordSize(const AHistoryGroupMode: TALCodeProfilerHistoryGroupMode): Integer;
    procedure DecodeProcMetricsRaw(
                const AHistoryGroupMode: TALCodeProfilerHistoryGroupMode;
                const ARawBuffer: TBytes;
                const AOffset: Integer;
                out ARec: TALProcMetrics);
    function GetProcMetricsColumnsForMode(const AHistoryGroupMode: TALCodeProfilerHistoryGroupMode): TArray<TALProcMetricsColumnKind>;
    function GetProcMetricsColumnName(const AKind: TALProcMetricsColumnKind): AnsiString;
    function GetProcMetricsColumnValue(
               const AKind: TALProcMetricsColumnKind;
               const ARec: TALProcMetrics;
               const AProcNames: TDictionary<Cardinal, AnsiString>): AnsiString;
    function GetSelectedServerName: AnsiString;
    procedure SelectServerName(const AServerName: String);
    function GetCodeProfilerIncFilename: String;
    function GetHistoryCapacity: Integer;
    procedure SaveCodeProfilerIncFile;
    procedure LoadCodeProfilerIncFile;
    procedure SaveConfigFile;
  public
  end;

var
  MainForm: TMainForm;

implementation

uses
  System.Variants,
  System.UITypes,
  System.AnsiStrings,
  System.IOUtils,
  System.Win.Registry,
  system.IniFiles,
  System.Generics.Defaults,
  winapi.Windows,
  DelphiAST,
  DelphiAST.Classes,
  DelphiAST.Consts,
  VCL.Dialogs,
  VCL.CheckLst,
  Alcinoe.StringUtils,
  Alcinoe.FileUtils,
  Alcinoe.Common;

{$R *.dfm}

{******************************************************************************************}
Procedure ExpandSourcesPath(const ASourcesPath: TStrings; Const AFilenames: TALStringListW);
begin
  var LFilenamesToRemove := TALStringListW.Create;
  try
    for var I := 0 to ASourcesPath.Count - 1 do begin
      var LFileName := ExpandFileName(ASourcesPath[i]);
      //--
      var LDstLst: TALStringListW;
      If AlposW('!',LFileName) = 1 then begin
        LDstLst := LFilenamesToRemove;
        delete(LFileName, 1, 1);
      end
      else
        LDstLst := AFilenames;
      //--
      IF Tfile.Exists(LFileName) then LDstLst.Add(LFileName)
      else if TDirectory.Exists(LFileName) then begin
        var Lfilenames := TDirectory.GetFiles(LFileName, '*.pas', TSearchOption.soAllDirectories);
        For var J := low(Lfilenames) to high(Lfilenames) do
          LDstLst.Add(Lfilenames[J]);
        Lfilenames := TDirectory.GetFiles(LFileName, '*.dpr', TSearchOption.soAllDirectories);
        For var J := low(Lfilenames) to high(Lfilenames) do
          LDstLst.Add(Lfilenames[J]);
      end;
    end;
    //--
    for var I := 0 to LFilenamesToRemove.Count - 1 do begin
      var J := AFilenames.IndexOf(LFilenamesToRemove[I]);
      if J >= 0 then AFilenames.Delete(J);
    end;
    //--
    for var I := AFilenames.Count - 1 downto 0 do
      if ALSameTextW(ALExtractFileName(AFilenames[i]), 'Alcinoe.CodeProfiler.pas') then
        AFilenames.Delete(I);
  finally
    ALFreeAndNil(LFilenamesToRemove);
  end;
end;

{*******************************}
procedure TMainForm.ResetFilters;
begin
  FFilterProcIDs.Clear;
  FFilterExecutionIDs.Clear;
  FFilterParentExecutionIDs.Clear;
  FFilterStartTimeStampMin := 0;
  FFilterStartTimeStampMax := 0;
  FOverrideFilterParentExecutionID := 0;
  FOverrideFilterThreadID := High(Cardinal);
end;

{*******************************************}
procedure TMainForm.ResetTreeListProcMetrics;
begin
  TreeListProcMetrics.Clear;
  // Always keep a "..." root node visible so the user has an obvious,
  // clickable way back to the root instead of having to guess that
  // double-clicking empty space resets the drill-down.
  FTreeListProcMetricsTailNode := TreeListProcMetrics.Add;
  FTreeListProcMetricsTailNode.Texts[TreeListProcMetricsColumnProcName.ItemIndex] := '...';
  FTreeListProcMetricsTailNode.Texts[TreeListProcMetricsColumnExecutionID.ItemIndex] := '0';
  // Leave the ThreadID cell blank for the root node (there is no single
  // thread to show); FTreeListProcMetricsRootNode is used instead of this
  // column's value to detect the root node and clear the thread filter.
  FTreeListProcMetricsRootNode := FTreeListProcMetricsTailNode;
end;

{**********************************************************************************}
function TMainForm.GetSelectedHistoryGroupModeEnum: TALCodeProfilerHistoryGroupMode;
begin
  if DoNotGroupRadioButton.Checked then Result := hgmNone
  else if GroupCallsByProcIDRadioButton.Checked then Result := hgmByProcID
  else Result := hgmByCallStack;
end;

{*********************************************************}
function TMainForm.GetSelectedHistoryGroupMode: AnsiString;
begin
  case GetSelectedHistoryGroupModeEnum of
    hgmNone: Result := 'ALCodeProfilerHistoryGroupNone';
    hgmByProcID: Result := 'ALCodeProfilerHistoryGroupByProcID';
    else Result := 'ALCodeProfilerHistoryGroupByCallStack';
  end;
end;

{**************************************************************************}
procedure TMainForm.SelectHistoryGroupMode(const AHistoryGroupMode: String);
begin
  if ALSameTextW(AHistoryGroupMode, 'ALCodeProfilerHistoryGroupNone') then DoNotGroupRadioButton.Checked := True
  else if ALSameTextW(AHistoryGroupMode, 'ALCodeProfilerHistoryGroupByProcID') then GroupCallsByProcIDRadioButton.Checked := True
  else GroupCallsByCallStackRadioButton.Checked := True;
end;

{*******************************************}
procedure TMainForm.UpdateHistoryGroupModeUI;
begin
  case GetSelectedHistoryGroupModeEnum of
    hgmNone: begin
      TreeListProcMetricsColumnStartTimeStamp.Visible := True;
      GridTableViewProcMetricsColumnStartTimestamp.Visible := True;
      TreeListProcMetricsColumnCallCount.Visible := False;
      GridTableViewProcMetricsColumnCallCount.Visible := False;
      // The start-timestamp filter only makes sense for individual calls,
      // which only exist in hgmNone; grouped modes have no StartTimeStamp.
      StartTimeStampMinEdit.Enabled := True;
      StartTimeStampMaxEdit.Enabled := True;
    end;
    else begin
      TreeListProcMetricsColumnStartTimeStamp.Visible := False;
      GridTableViewProcMetricsColumnStartTimestamp.Visible := False;
      TreeListProcMetricsColumnCallCount.Visible := True;
      GridTableViewProcMetricsColumnCallCount.Visible := True;
      StartTimeStampMinEdit.Enabled := False;
      StartTimeStampMaxEdit.Enabled := False;
    end;
  end;
  // ALCodeProfilerIgnoreThreadID is only supported by
  // ALCodeProfilerHistoryGroupByProcID.
  IgnoreThreadIDCheckBox.Enabled := GetSelectedHistoryGroupModeEnum = hgmByProcID;
end;

{********************************************************}
function TMainForm.GetSelectedProcMetricsFilename: String;
begin
  case GetSelectedHistoryGroupModeEnum of
    hgmNone: Result := 'ALCodeProfilerProcMetrics.None.dat';
    hgmByProcID: Result := 'ALCodeProfilerProcMetrics.ByProcID.dat';
    else Result := 'ALCodeProfilerProcMetrics.ByCallStack.dat';
  end;
end;

{****************************************************************************************************************}
function TMainForm.GetProcMetricsRawRecordSize(const AHistoryGroupMode: TALCodeProfilerHistoryGroupMode): Integer;
begin
  case AHistoryGroupMode of
    hgmNone: Result := SizeOf(TALProcMetricsNone);
    hgmByProcID: Result := SizeOf(TALProcMetricsByProcID);
    else Result := SizeOf(TALProcMetricsByCallStack);
  end;
end;

{***************************************}
procedure TMainForm.DecodeProcMetricsRaw(
            const AHistoryGroupMode: TALCodeProfilerHistoryGroupMode;
            const ARawBuffer: TBytes;
            const AOffset: Integer;
            out ARec: TALProcMetrics);
begin
  FillChar(ARec, SizeOf(ARec), 0);
  case AHistoryGroupMode of
    hgmNone: begin
      var LRaw: TALProcMetricsNone;
      Move(ARawBuffer[AOffset], LRaw, SizeOf(LRaw));
      ARec.ExecutionID := LRaw.ExecutionID;
      ARec.ParentExecutionID := LRaw.ParentExecutionID;
      ARec.ProcID := LRaw.ProcID;
      ARec.ThreadID := LRaw.ThreadID;
      ARec.StartTimeStamp := LRaw.StartTimeStamp;
      ARec.ElapsedTicks := LRaw.ElapsedTicks;
    end;
    hgmByProcID: begin
      var LRaw: TALProcMetricsByProcID;
      Move(ARawBuffer[AOffset], LRaw, SizeOf(LRaw));
      ARec.ProcID := LRaw.ProcID;
      ARec.ThreadID := LRaw.ThreadID;
      ARec.CallCount := LRaw.CallCount;
      ARec.ElapsedTicks := LRaw.ElapsedTicks;
    end;
    hgmByCallStack: begin
      var LRaw: TALProcMetricsByCallStack;
      Move(ARawBuffer[AOffset], LRaw, SizeOf(LRaw));
      ARec.ProcID := LRaw.ProcID;
      ARec.MetricsID := LRaw.MetricsID;
      ARec.ParentMetricsID := LRaw.ParentMetricsID;
      ARec.ThreadID := LRaw.ThreadID;
      ARec.CallCount := LRaw.CallCount;
      ARec.ElapsedTicks := LRaw.ElapsedTicks;
    end;
  end;
end;

{******************************************************************************************************************************************}
function TMainForm.GetProcMetricsColumnsForMode(const AHistoryGroupMode: TALCodeProfilerHistoryGroupMode): TArray<TALProcMetricsColumnKind>;
begin
  case AHistoryGroupMode of
    hgmNone: Result := [colkExecutionID, colkParentExecutionID, colkProcID, colkProcName, colkThreadID, colkStartTimeStamp, colkTimeTaken];
    hgmByProcID: Result := [colkProcID, colkProcName, colkThreadID, colkCallCount, colkTimeTaken];
    else Result := [colkProcID, colkMetricsID, colkParentMetricsID, colkProcName, colkThreadID, colkCallCount, colkTimeTaken];
  end;
end;

{*********************************************************************************************}
function TMainForm.GetProcMetricsColumnName(const AKind: TALProcMetricsColumnKind): AnsiString;
begin
  case AKind of
    colkExecutionID: Result := 'ExecutionID';
    colkParentExecutionID: Result := 'ParentExecutionID';
    colkProcID: Result := 'ProcID';
    colkMetricsID: Result := 'MetricsID';
    colkParentMetricsID: Result := 'ParentMetricsID';
    colkProcName: Result := 'ProcName';
    colkThreadID: Result := 'ThreadID';
    colkStartTimeStamp: Result := 'StartTimeStamp';
    colkCallCount: Result := 'CallCount';
    else Result := 'TimeTaken';
  end;
end;

{*******************************************}
function TMainForm.GetProcMetricsColumnValue(
           const AKind: TALProcMetricsColumnKind;
           const ARec: TALProcMetrics;
           const AProcNames: TDictionary<Cardinal, AnsiString>): AnsiString;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _TicksToMillisecondsStr(const ATicks: Int64): AnsiString;
  begin
    // 1 tick equals 0.0001 millisecond (ALCodeProfilerMillisecondsPerTick),
    // so use integer arithmetic to avoid any rounding/locale issue
    Result := ALIntToStrA(ATicks div 10000) + '.' + ALFormatA('%.4d', [ATicks mod 10000]);
  end;

begin
  case AKind of
    colkExecutionID: Result := ALIntToStrA(ARec.ExecutionID);
    colkParentExecutionID: Result := ALIntToStrA(ARec.ParentExecutionID);
    colkProcID: Result := ALIntToStrA(ARec.ProcID);
    colkMetricsID: Result := ALIntToStrA(ARec.MetricsID);
    colkParentMetricsID: Result := ALIntToStrA(ARec.ParentMetricsID);
    colkProcName: begin
      var LProcName: AnsiString;
      if not AProcNames.TryGetValue(ARec.ProcID, LProcName) then LProcName := '';
      Result := LProcName;
    end;
    colkThreadID: Result := ALIntToStrA(ARec.ThreadID);
    colkStartTimeStamp: Result := _TicksToMillisecondsStr(ARec.StartTimeStamp);
    colkCallCount: Result := ALIntToStrA(ARec.CallCount);
    else Result := _TicksToMillisecondsStr(ARec.ElapsedTicks);
  end;
end;

{***************************************************}
function TMainForm.GetSelectedServerName: AnsiString;
begin
  if (ALTrim(HttpServerNameEdit.Text) <> '') and (ALTrim(HttpServerPortEdit.Text) <> '') then
    Result := AnsiString('http://'+ALTrim(HttpServerNameEdit.Text)+':'+ALTrim(HttpServerPortEdit.Text))
  else
    Result := '';
end;

{**************************************************************}
procedure TMainForm.SelectServerName(const AServerName: String);
begin
  var LServerName := AServerName;
  if ALPosW('http://', LServerName) = 1 then Delete(LServerName, 1, Length('http://'));
  var LColonPos := LastDelimiter(':', LServerName);
  if LColonPos > 0 then begin
    HttpServerNameEdit.Text := ALCopyStr(LServerName, 1, LColonPos - 1);
    HttpServerPortEdit.Text := ALCopyStr(LServerName, LColonPos + 1, MaxInt);
  end
  else begin
    HttpServerNameEdit.Text := LServerName;
    HttpServerPortEdit.Text := '8080';
  end;
end;

{********************************************************************}
procedure TMainForm.HistoryGroupModeRadioButtonClick(Sender: TObject);
begin
  UpdateHistoryGroupModeUI;
  SaveCodeProfilerIncFile;
end;

{**************************************************************************}
procedure TMainForm.IgnoreThreadIDCheckBoxPropertiesChange(Sender: TObject);
begin
  SaveCodeProfilerIncFile;
end;

{****************************************************}
function TMainForm.GetCodeProfilerIncFilename: String;
begin
  Result := ALTrim(CodeProfilerIncFilenameEdit.Text);
  if Result = '' then exit;
  // A relative path is relative to the folder of this tool, so that the
  // default value (..\..\Source\Alcinoe.CodeProfiler.inc) keeps working
  // whatever the location of the Alcinoe repository.
  if TPath.IsRelativePath(Result) then Result := ALGetModulePathW + Result;
  Result := ExpandFileName(Result);
end;

{*********************************************}
function TMainForm.GetHistoryCapacity: Integer;
begin
  // With ALCodeProfilerHistoryGroupByProcID the history is a flat array
  // indexed by the procedure ID, so it only needs to be big enough to hold the
  // highest procedure ID of ALCodeProfilerProcIDMap.txt. When that file does
  // not exist (no markers inserted yet, or markers just removed) fall back to
  // the default capacity.
  Result := DefaultHistoryCapacity;
  var LProcIDMapFilename := TPath.Combine(FDataDir, ALCodeProfilerProcIDMapFilename);
  if not TFile.Exists(LProcIDMapFilename) then exit;
  var LProcIDMap := TALStringListA.Create;
  try
    LProcIDMap.LoadFromFile(LProcIDMapFilename);
    if LProcIDMap.Count = 0 then exit;
    var LMaxProcID := 0;
    for var I := 0 to LProcIDMap.Count - 1 do begin
      var LProcID := ALStrToInt(LProcIDMap.Names[I]);
      if LProcID > LMaxProcID then LMaxProcID := LProcID;
    end;
    // The procedure IDs start at 1, so the array must have LMaxProcID + 1
    // items for FArray[LMaxProcID] to be valid.
    Result := LMaxProcID + 1;
  finally
    ALFreeAndNil(LProcIDMap);
  end;
end;

{******************************************}
procedure TMainForm.SaveCodeProfilerIncFile;
var
  LHistoryGroupMode: AnsiString;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _DefineLine(const AHistoryGroupModeName: AnsiString): AnsiString;
  begin
    if ALSameTextA(AHistoryGroupModeName, LHistoryGroupMode) then Result := '{$DEFINE ' + AHistoryGroupModeName + '}'
    else Result := '{.$DEFINE ' + AHistoryGroupModeName + '}';
  end;

begin
  // Never write Alcinoe.CodeProfiler.inc while the UI is still being populated
  // from Config.ini / Alcinoe.CodeProfiler.inc, else the values just loaded
  // would be written back, possibly to a file that is not the selected one yet.
  if FLoadingSettings then exit;
  var LIncFilename := GetCodeProfilerIncFilename;
  if LIncFilename = '' then exit;
  LHistoryGroupMode := GetSelectedHistoryGroupMode;
  var LContent: AnsiString :=
    'var'#10 +
    '  ALCodeProfilerEnabled: Boolean = '+ALIfThenA(CodeProfilerEnabledCheckBox.Checked, 'True', 'False')+';'#10 +
    '  ALCodeProfilerServerName: String = '''+GetSelectedServerName+''';'#10 +
    #10 +
    '// Do not group calls. Each function/procedure call generates one row.'#10 +
    '// The result will be displayed as a call tree, for example:'#10 +
    '// procedure A - 1 call - 310 ms'#10 +
    '//   procedure B - 1 call - 215 ms'#10 +
    '//     procedure C - 1 call - 14 ms'#10 +
    '//     procedure C - 1 call - 11 ms'#10 +
    '//     procedure D - 1 call - 85 ms'#10 +
    '//     procedure C - 1 call - 21 ms'#10 +
    '//   procedure B - 1 call - 35 ms'#10 +
    '//   procedure B - 1 call - 41 ms'#10 +
    '//   procedure C - 1 call - 22 ms'#10 +
    '//'#10 +
    '// NOTE: This option can use a lot of memory. If you enable it, it is recommended'#10 +
    '// to limit profiling to the specific code you want to measure by using'#10 +
    '// ALCodeProfilerStart and ALCodeProfilerStop.'#10 +
    _DefineLine('ALCodeProfilerHistoryGroupNone') + #10 +
    #10 +
    '// Group calls by procedure ID.'#10 +
    '// The result will be displayed as a flat grid, for example:'#10 +
    '// procedure A - 1 call - 310 ms'#10 +
    '// procedure B - 3 calls - 291 ms'#10 +
    '// procedure C - 4 calls - 68 ms'#10 +
    '// procedure D - 1 call - 85 ms'#10 +
    '//'#10 +
    '// NOTE: This is the fastest option and has the lowest impact on each function call.'#10 +
    _DefineLine('ALCodeProfilerHistoryGroupByProcID') + #10 +
    #10 +
    '{$IF defined(ALCodeProfilerHistoryGroupByProcID)}'#10 +
    '// Ignore the thread ID. The calls made from every thread are merged together'#10 +
    '// instead of producing one row per procedure and per thread, for example:'#10 +
    '// procedure A - 3 calls - 310 ms  = 1 call from the main thread + 2 calls from a background thread'#10 +
    '//'#10 +
    '// NOTE: This option is only available with ALCodeProfilerHistoryGroupByProcID.'#10 +
    '// All the threads then share the same metrics, which are updated atomically,'#10 +
    '// so it slightly increases the cost of each function call, but it also greatly'#10 +
    '// reduces the memory usage as only one history is allocated for the whole'#10 +
    '// process instead of one per thread.'#10 +
    ALIfThenA(
      IgnoreThreadIDCheckBox.Checked,
      '{$DEFINE ALCodeProfilerIgnoreThreadID}',
      '{.$DEFINE ALCodeProfilerIgnoreThreadID}') + #10 +
    '{$ENDIF}'#10 +
    #10 +
    '// Group calls by call stack.'#10 +
    '// The result will be displayed as a grouped call tree, for example:'#10 +
    '// procedure A - 1 call - 310 ms'#10 +
    '//   procedure B - 3 calls - 291 ms'#10 +
    '//     procedure C - 3 calls - 46 ms'#10 +
    '//     procedure D - 1 call - 85 ms'#10 +
    '//   procedure C - 1 call - 22 ms'#10 +
    _DefineLine('ALCodeProfilerHistoryGroupByCallStack') + #10 +
    #10 +
    '// Capacity of the history, in number of rows.'#10 +
    '//'#10 +
    '// NOTE: With ALCodeProfilerHistoryGroupByProcID the history is a flat array'#10 +
    '// indexed by the procedure ID, so it only needs to be big enough to hold the'#10 +
    '// highest procedure ID of ' + AnsiString(ALCodeProfilerProcIDMapFilename) + '. The value below is'#10 +
    '// updated by the Alcinoe Code Profiler GUI each time the markers are inserted'#10 +
    '// or removed, so there is no reason to edit it by hand.'#10 +
    '{$IF defined(ALCodeProfilerHistoryGroupByProcID)}'#10 +
    'const'#10 +
    '  ALCodeProfilerHistoryCapacity = '+ALIntToStrA(GetHistoryCapacity)+';'#10 +
    '{$ELSE}'#10 +
    'const'#10 +
    '  ALCodeProfilerHistoryCapacity = '+ALIntToStrA(DefaultHistoryCapacity)+'; {1 000 000 * 32 Bytes = 32 MB or with gap = 2 097 152 * 32 Bytes = 67.11 MB}'#10 +
    '{$ENDIF}'#10;
  ALSaveStringToFile(LContent, LIncFilename);
end;

{******************************************}
procedure TMainForm.LoadCodeProfilerIncFile;
begin
  var LIncFilename := GetCodeProfilerIncFilename;
  var LContent: AnsiString := '';
  if (LIncFilename <> '') and TFile.Exists(LIncFilename) then LContent := ALGetStringFromFile(LIncFilename);
  // Populating the controls below raises their OnChange event, which would
  // otherwise save these very same values back to Alcinoe.CodeProfiler.inc.
  FLoadingSettings := True;
  try
    //--
    if ALPosA('{$DEFINE ALCodeProfilerHistoryGroupNone}', LContent) > 0 then SelectHistoryGroupMode('ALCodeProfilerHistoryGroupNone')
    else if ALPosA('{$DEFINE ALCodeProfilerHistoryGroupByProcID}', LContent) > 0 then SelectHistoryGroupMode('ALCodeProfilerHistoryGroupByProcID')
    else SelectHistoryGroupMode('ALCodeProfilerHistoryGroupByCallStack');
    //--
    IgnoreThreadIDCheckBox.Checked := ALPosA('{$DEFINE ALCodeProfilerIgnoreThreadID}', LContent) > 0;
    //--
    var LServerName: String := '';
    var LMarker: AnsiString := 'ALCodeProfilerServerName: String = ''';
    var LValueStart := ALPosA(LMarker, LContent);
    if LValueStart > 0 then begin
      inc(LValueStart, Length(LMarker));
      var LValueEnd := ALPosA('''', LContent, LValueStart);
      if LValueEnd > LValueStart then
        LServerName := String(ALCopyStr(LContent, LValueStart, LValueEnd - LValueStart));
    end;
    SelectServerName(LServerName);
    //--
    var LEnabled := True;
    LMarker := 'ALCodeProfilerEnabled: Boolean = ';
    LValueStart := ALPosA(LMarker, LContent);
    if LValueStart > 0 then begin
      inc(LValueStart, Length(LMarker));
      LEnabled := not ALSameTextA(ALCopyStr(LContent, LValueStart, Length('False')), 'False');
    end;
    CodeProfilerEnabledCheckBox.Checked := LEnabled;
  finally
    FLoadingSettings := False;
  end;
end;

{*********************************}
procedure TMainForm.SaveConfigFile;
begin
  var LIniFile := TIniFile.Create(TPath.Combine(FDataDir, ConfigFilename));
  try
    LIniFile.WriteString('General','SourcesPath',ALStringReplaceW(ALTrim(SourcesPathMemo.Text), #13#10, ';', [RfReplaceALL]));
    LIniFile.WriteString('General','CodeProfilerIncFilename',ALTrim(CodeProfilerIncFilenameEdit.Text));
  finally
    ALFreeAndNil(LIniFile);
  end;
end;

{*******************************************************************************}
procedure TMainForm.CodeProfilerIncFilenameEditPropertiesChange(Sender: TObject);
begin
  if FLoadingSettings then exit;
  SaveConfigFile;
  // Reload the options only once the typed path points to an existing file,
  // else every intermediate keystroke would reset them to their defaults.
  if TFile.Exists(GetCodeProfilerIncFilename) then LoadCodeProfilerIncFile;
end;

{*************************************************************************}
procedure TMainForm.BrowseCodeProfilerIncFilenameBtnClick(Sender: TObject);
begin
  var LOpenDialog := TOpenDialog.Create(nil);
  try
    LOpenDialog.Title := 'Select Alcinoe.CodeProfiler.inc';
    LOpenDialog.Filter := 'Alcinoe.CodeProfiler.inc|Alcinoe.CodeProfiler.inc|Include files (*.inc)|*.inc|All files (*.*)|*.*';
    LOpenDialog.DefaultExt := 'inc';
    LOpenDialog.Options := LOpenDialog.Options + [ofFileMustExist, ofPathMustExist];
    var LIncFilename := GetCodeProfilerIncFilename;
    if LIncFilename <> '' then begin
      LOpenDialog.InitialDir := ALExtractFilePath(LIncFilename);
      LOpenDialog.FileName := LIncFilename;
    end;
    if LOpenDialog.Execute(Handle) then
      CodeProfilerIncFilenameEdit.Text := LOpenDialog.FileName;
  finally
    ALFreeAndNil(LOpenDialog);
  end;
end;

{*******************************************************************************}
procedure TMainForm.CodeProfilerEnabledCheckBoxPropertiesChange(Sender: TObject);
begin
  SaveCodeProfilerIncFile;
end;

{*********************************************************}
procedure TMainForm.RemoveMarkers(const AFileName: String);
begin
  var LSourceCode := ALGetStringFromFile(AFileName);
  var P1 := AlPosA('{ALCodeProfiler>>}',LSourceCode);
  While P1 > 0 do begin
    var P2 := ALposA('{<<ALCodeProfiler}',LSourceCode,P1);
    If P2 < 0 then raise Exception.Create('Error 206270E5-3304-46BE-9840-E010CC7BF148');
    inc(P2, length('{<<ALCodeProfiler}'));
    delete(LSourceCode, P1, P2 - P1);
    P1 := AlPosA('{ALCodeProfiler>>}',LSourceCode, P1);
  end;
  ALSaveStringToFile(LSourceCode,AFileName);
end;

{*****************************************************************}
procedure TMainForm.RemoveProfilerMarkersBtnClick(Sender: TObject);
begin
  var LSourceFilenames := TALStringListW.Create;
  try
    ExpandSourcesPath(SourcesPathMemo.Lines, LSourceFilenames);
    if LSourceFilenames.Count = 0 then
      Raise Exception.Create('Error: No files have been selected');
    RemoveProfilerMarkersBtn.Cursor := crHourGlass;
    Try
      for var I := 0 to LSourceFilenames.Count - 1 do
        RemoveMarkers(LSourceFilenames[i]);
    finally
      RemoveProfilerMarkersBtn.Cursor := crDefault;
    End;
    var LProcMetricsFilename := TPath.Combine(FDataDir, GetSelectedProcMetricsFilename);
    If TFile.Exists(LProcMetricsFilename) then
      TFile.Delete(LProcMetricsFilename);
    var LProcIDMapFilename := TPath.Combine(FDataDir, ALCodeProfilerProcIDMapFilename);
    If TFile.Exists(LProcIDMapFilename) then
      TFile.Delete(LProcIDMapFilename);
    // ALCodeProfilerHistoryCapacity was deduced from the procedure IDs just
    // deleted, so Alcinoe.CodeProfiler.inc must be reset as well.
    SaveCodeProfilerIncFile;
    MessageDlg('The operation completed successfully', mtInformation, [mbOK], 0);
  Finally
    ALFreeAndNil(LSourceFilenames);
  end;
end;

{*************************************************************************************************************************************}
procedure TMainForm.IdHTTPServerCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  FHttpServerCriticalSection.Acquire;
  try
    // We're only handling POST requests here.
    if ARequestInfo.CommandType = hcPOST then
    begin
      // Define the file path where the POST content will be saved. The
      // filename depends on the currently selected radio button, so it
      // must be read on the main thread.
      var LProcMetricsFilename: String;
      TThread.Synchronize(nil,
        procedure
        begin
          LProcMetricsFilename := TPath.Combine(FDataDir, GetSelectedProcMetricsFilename);
        end);
      var LProcMetricsTmpFilename := LProcMetricsFilename + '~tmp';
      If TFile.Exists(LProcMetricsTmpFilename) then
        TFile.Delete(LProcMetricsTmpFilename);
      if Assigned(ARequestInfo.PostStream) then
      begin
        TThread.Synchronize(nil,
          procedure
          begin
            MainStatusBar.Panels[1].Text := 'Receiving new performance file...';
          end);
        ARequestInfo.PostStream.Position := 0;
        var LFileStream := TFileStream.Create(LProcMetricsTmpFilename, fmCreate);
        try
          // Save the raw POST content to the file.
          LFileStream.CopyFrom(ARequestInfo.PostStream, ARequestInfo.PostStream.Size);
        finally
          LFileStream.Free;
        end;
        AResponseInfo.ResponseNo := 200;
        AResponseInfo.ContentText := '';
        TThread.Synchronize(nil,
          procedure
          begin
            Try
              If TFile.Exists(LProcMetricsFilename) then
                TFile.Delete(LProcMetricsFilename);
              TFile.Move(LProcMetricsTmpFilename, LProcMetricsFilename);
              MainStatusBar.Panels[1].Text := 'New performance file received successfully. Please reload the data.';
            except
              on E: Exception do begin
                MainStatusBar.Panels[1].Text := 'Error: '+E.Message;
              end;
            End;
          end);
      end
      else
      begin
        AResponseInfo.ResponseNo := 400;
        AResponseInfo.ContentText := 'No POST content received.';
        TThread.Synchronize(nil,
          procedure
          begin
            MainStatusBar.Panels[1].Text := 'Error: No POST content received.';
          end);
      end;
    end
    else
    begin
      AResponseInfo.ResponseNo := 405;
      AResponseInfo.ContentText := 'Method not allowed.';
      TThread.Synchronize(nil,
        procedure
        begin
          MainStatusBar.Panels[1].Text := 'Error: Method not allowed.';
        end);
    end;
  finally
    FHttpServerCriticalSection.Release;
  end;
end;

{************************************************************}
procedure TMainForm.IdHTTPServerConnect(AContext: TIdContext);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      MainStatusBar.Panels[1].Text := 'Receiving new performance file...';
    end);
end;

{*************************************************************************************}
procedure TMainForm.IdHTTPServerException(AContext: TIdContext; AException: Exception);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      MainStatusBar.Panels[1].Text := 'Error: '+AException.Message;
    end);
end;

{*************************************************************************************************}
procedure TMainForm.IdHTTPServerListenException(AThread: TIdListenerThread; AException: Exception);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      MainStatusBar.Panels[1].Text := 'Error: '+AException.Message;
    end);
end;

{*******************************************************************************************}
procedure TMainForm.InsertMarkers(const AFileName: String; Const AProcIDMap: TALStringListA);

type
  TMarkerInsertion = record
    Line: Integer;
    Col: Integer;
    Text: AnsiString;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function IsIdentifierChar(const AChar: AnsiChar): Boolean;
  begin
    Result := AChar in ['a'..'z', 'A'..'Z', '0'..'9', '_'];
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function IsKeywordAt(const ALine, AKeyword: AnsiString; const ACol: Integer): Boolean;
  begin
    Result := False;
    if ACol < 1 then
      Exit;
    if ACol + Length(AKeyword) - 1 > Length(ALine) then
      Exit;
    if not ALSameTextA(ALCopyStr(ALine, ACol, Length(AKeyword)), AKeyword) then
      Exit;
    if ACol > 1 then
      if IsIdentifierChar(ALine[ACol - 1]) then
        Exit;
    if ACol + Length(AKeyword) <= Length(ALine) then
      if IsIdentifierChar(ALine[ACol + Length(AKeyword)]) then
        Exit;
    Result := True;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function FindKeywordColumn(const ALine, AKeyword: AnsiString; const APreferredCol: Integer; const ASearchBackwards: Boolean): Integer;
  begin
    if IsKeywordAt(ALine, AKeyword, APreferredCol) then
      Exit(APreferredCol);
    if ASearchBackwards then begin
      for var I := Length(ALine) - Length(AKeyword) + 1 downto 1 do
        if IsKeywordAt(ALine, AKeyword, I) then
          Exit(I);
    end
    else begin
      for var I := 1 to Length(ALine) - Length(AKeyword) + 1 do
        if IsKeywordAt(ALine, AKeyword, I) then
          Exit(I);
    end;
    Result := 0;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function FindBeginInsertionColumn(const ASourceCode: TALStringListA; const ALine, ACol: Integer): Integer;
  begin
    if (ALine < 1) or (ALine > ASourceCode.Count) then
      raise Exception.Create('Invalid AST begin line: ' + ALIntToStrW(ALine) + ' - Filename: ' + AFileName);
    var LLine := ASourceCode[ALine - 1];
    var LBeginCol := FindKeywordColumn(LLine, 'begin', ACol, False);
    if LBeginCol <= 0 then
      Exit(0);
    Result := LBeginCol + Length('begin');
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function FindEndInsertionColumn(const ASourceCode: TALStringListA; const ALine, ACol: Integer): Integer;
  begin
    if (ALine < 1) or (ALine > ASourceCode.Count) then
      raise Exception.Create('Invalid AST end line: ' + ALIntToStrW(ALine) + ' - Filename: ' + AFileName);
    var LLine := ASourceCode[ALine - 1];
    Result := FindKeywordColumn(LLine, 'end', ACol - Length('end'), True);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure AddInsertion(const AInsertions: TList<TMarkerInsertion>; const ASourceCode: TALStringListA; const ALine, ACol: Integer; const AText: AnsiString);
  begin
    if (ALine < 1) or (ALine > ASourceCode.Count) then
      raise Exception.Create('Invalid insertion line: ' + ALIntToStrW(ALine) + ' - Filename: ' + AFileName);
    if (ACol < 1) or (ACol > Length(ASourceCode[ALine - 1]) + 1) then
      raise Exception.Create('Invalid insertion column: ' + ALIntToStrW(ACol) + ' - Line: ' + ALIntToStrW(ALine) + ' - Filename: ' + AFileName);
    var LInsertion: TMarkerInsertion;
    LInsertion.Line := ALine;
    LInsertion.Col := ACol;
    LInsertion.Text := AText;
    AInsertions.Add(LInsertion);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function FindFirstNode(const ANode: TSyntaxNode; const ANodeType: TSyntaxNodeType): TSyntaxNode;
  begin
    Result := nil;
    if not Assigned(ANode) then
      Exit;
    if ANode.Typ = ANodeType then begin
      Result := ANode;
      Exit;
    end;
    for var LChild in ANode.ChildNodes do begin
      Result := FindFirstNode(LChild, ANodeType);
      if Assigned(Result) then
        Exit;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function GetMethodStatements(const ANode: TSyntaxNode): TCompoundSyntaxNode;
  begin
    Result := nil;
    for var LChild in ANode.ChildNodes do
      if (LChild.Typ = ntStatements) and
         (LChild is TCompoundSyntaxNode) then begin
        Result := TCompoundSyntaxNode(LChild);
        Exit;
      end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function GetMethodName(const ANode: TSyntaxNode; var AAnonymousMethodSequence: Integer): AnsiString;
  begin
    Result := ALTrim(AnsiString(ANode.GetAttribute(anName)));
    for var LChild in ANode.ChildNodes do
      if (LChild.Typ = ntName) and
         (LChild is TValuedSyntaxNode) then begin
        var LNamePart := ALTrim(AnsiString(TValuedSyntaxNode(LChild).Value));
        if (LNamePart <> '') and
           ((Result = '') or (ALPosIgnoreCaseA(LNamePart + '.', Result) <> 1)) then begin
          if Result <> '' then
            Result := LNamePart + '.' + Result
          else
            Result := LNamePart;
        end;
      end;
    if Result = '' then begin
      inc(AAnonymousMethodSequence);
      Result := '$AnonymousMethod' + ALIntToStrA(AAnonymousMethodSequence);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure CollectMethodMarkers(const ANode: TSyntaxNode; const AParentProcName: AnsiString; const AUnitName: AnsiString; const ASourceCode: TALStringListA; const AInsertions: TList<TMarkerInsertion>; var AAnonymousMethodSequence: Integer);
  begin
    if not Assigned(ANode) then
      Exit;

    var LParentProcName := AParentProcName;
    if ANode.Typ in [ntMethod, ntAnonymousMethod] then begin
      var LProcName := GetMethodName(ANode, AAnonymousMethodSequence);
      if LParentProcName <> '' then
        LProcName := LParentProcName + '.' + LProcName;
      LParentProcName := LProcName;

      var LStatements := GetMethodStatements(ANode);
      if Assigned(LStatements) then begin
        var LBeginInsertionCol := FindBeginInsertionColumn(ASourceCode, LStatements.Line, LStatements.Col);
        var LEndInsertionCol := FindEndInsertionColumn(ASourceCode, LStatements.EndLine, LStatements.EndCol);
        if (LBeginInsertionCol > 0) and (LEndInsertionCol > 0) then begin
          inc(FProcIDSequence);
          AddInsertion(
            AInsertions,
            ASourceCode,
            LStatements.Line,
            LBeginInsertionCol,
            '{ALCodeProfiler>>}ALCodeProfilerEnterProc('+ALIntToStrA(FProcIDSequence)+'); try{<<ALCodeProfiler}');
          AddInsertion(
            AInsertions,
            ASourceCode,
            LStatements.EndLine,
            LEndInsertionCol,
            '{ALCodeProfiler>>}finally ALCodeProfilerExitProc('+ALIntToStrA(FProcIDSequence)+'); end;{<<ALCodeProfiler}');
          AProcIDMap.Add(ALIntToStrA(FProcIDSequence) + '=' + AUnitName + '.' + LProcName);
        end;
      end;
    end;

    for var LChild in ANode.ChildNodes do
      CollectMethodMarkers(LChild, LParentProcName, AUnitName, ASourceCode, AInsertions, AAnonymousMethodSequence);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure ApplyInsertions(const ASourceCode: TALStringListA; const AInsertions: TList<TMarkerInsertion>);
  begin
    AInsertions.Sort(
      TComparer<TMarkerInsertion>.Construct(
        function(const Left, Right: TMarkerInsertion): Integer
        begin
          if Left.Line <> Right.Line then
            Result := Right.Line - Left.Line
          else
            Result := Right.Col - Left.Col;
        end));

    for var LInsertion in AInsertions do begin
      var LLine := ASourceCode[LInsertion.Line - 1];
      Insert(LInsertion.Text, LLine, LInsertion.Col);
      ASourceCode[LInsertion.Line - 1] := LLine;
    end;
  end;

begin
  RemoveMarkers(AFileName);
  var LIsDPR := ALSameTextW(ALExtractFileExt(AFileName), '.dpr');
  var LUnitName := ALExtractFileName(AnsiString(AFileName), true{RemoveFileExt});
  var LSyntaxTree := TPasSyntaxTreeBuilder.Run(AFileName);
  var LInsertions := TList<TMarkerInsertion>.Create;
  var LSourceCode := TALStringListA.create;
  try
    LSourceCode.LoadFromFile(AFileName);

    var LUsesNode := FindFirstNode(LSyntaxTree, ntUses);
    if Assigned(LUsesNode) then
      AddInsertion(
        LInsertions,
        LSourceCode,
        LUsesNode.Line,
        LUsesNode.Col + Length('uses'),
        '{ALCodeProfiler>>}{$DEFINE ALCodeProfiler}Alcinoe.CodeProfiler,{<<ALCodeProfiler}')
    else if not LIsDPR then begin
      var LInterfaceNode := FindFirstNode(LSyntaxTree, ntInterface);
      if not Assigned(LInterfaceNode) then
        raise Exception.Create('Interface section not found - Filename: ' + AFileName);
      var LInterfaceCol := FindKeywordColumn(LSourceCode[LInterfaceNode.Line - 1], 'interface', LInterfaceNode.Col, False);
      if LInterfaceCol <= 0 then
        raise Exception.Create('Interface keyword not found at line ' + ALIntToStrW(LInterfaceNode.Line) + ' - Filename: ' + AFileName);
      AddInsertion(
        LInsertions,
        LSourceCode,
        LInterfaceNode.Line,
        LInterfaceCol + Length('interface'),
        '{ALCodeProfiler>>}{$DEFINE ALCodeProfiler}uses Alcinoe.CodeProfiler;{<<ALCodeProfiler}');
    end;

    if not LIsDPR then begin
      var LImplementationNode := FindFirstNode(LSyntaxTree, ntImplementation);
      if Assigned(LImplementationNode) then begin
        var LAnonymousMethodSequence := 0;
        CollectMethodMarkers(LImplementationNode, '', LUnitName, LSourceCode, LInsertions, LAnonymousMethodSequence);
      end;
    end;

    ApplyInsertions(LSourceCode, LInsertions);
    LSourceCode.ProtectedSave := true;
    LSourceCode.SaveToFile(AFileName);

  finally
    AlFreeAndNil(LSourceCode);
    ALFreeAndNil(LInsertions);
    ALFreeAndNil(LSyntaxTree);
  end;
end;

{*****************************************************************}
procedure TMainForm.InsertProfilerMarkersBtnClick(Sender: TObject);
begin
  SaveConfigFile;
  var LSourceFilenames := TALStringListW.Create;
  var LFailedFilenames := TALStringListW.Create;
  var LProcIDMap := TALStringListA.Create;
  try
    ExpandSourcesPath(SourcesPathMemo.Lines, LSourceFilenames);
    if LSourceFilenames.Count = 0 then
      Raise Exception.Create('Error: No files have been selected');

    // Ask whether the previously collected data must be cleared. If it is
    // kept, preload the existing proc ID map and continue the ID sequence
    // after the highest existing ProcID so that the IDs already present in
    // the sources and in the performance file stay valid
    var LProcIDMapFilename := TPath.Combine(FDataDir, ALCodeProfilerProcIDMapFilename);
    var LProcMetricsFilenameOnly := GetSelectedProcMetricsFilename;
    var LClearData := MessageDlg(
                        'Do you want to start a fresh profiling session and clear the data collected so far?'+ sLineBreak + sLineBreak +
                        'YES – Start fresh: the procedure IDs ('+ALCodeProfilerProcIDMapFilename+') and the collected performance data ('+LProcMetricsFilenameOnly+') will be discarded, and the ID numbering will restart from 1. '+
                        'Choose this only if the selected files cover ALL the files currently containing profiler markers; any file left instrumented from a previous run would keep old IDs that clash with the new ones.'+ sLineBreak + sLineBreak +
                        'NO – Keep the existing data: the procedures of the selected files will be assigned new IDs, following the existing ones, so previously instrumented files and already collected performance data stay valid.',
                        mtConfirmation, [mbYes, mbNo, mbCancel], 0);
    if (LClearData <> mrYes) and (LClearData <> mrNo) then Exit;
    if LClearData = mrYes then FProcIDSequence := 0
    else if TFile.Exists(LProcIDMapFilename) then begin
      LProcIDMap.LoadFromFile(LProcIDMapFilename);
      for var I := 0 to LProcIDMap.Count - 1 do begin
        var LProcID := ALStrToInt(LProcIDMap.Names[I]);
        if LProcID > FProcIDSequence then FProcIDSequence := LProcID;
      end;
    end;

    InsertProfilerMarkersBtn.Cursor := crHourGlass;
    Try
      for var I := 0 to LSourceFilenames.Count - 1 do
        try
          InsertMarkers(LSourceFilenames[i], LProcIDMap);
        Except
          On E: Exception do
            LFailedFilenames.Add(LSourceFilenames[i]);
        end;
      LProcIDMap.SaveToFile(LProcIDMapFilename);
      // ALCodeProfilerHistoryCapacity depends on the procedure IDs just
      // assigned, so Alcinoe.CodeProfiler.inc must be updated as well.
      SaveCodeProfilerIncFile;
    finally
      InsertProfilerMarkersBtn.Cursor := crDefault;
    End;
    if LClearData = mrYes then begin
      var LProcMetricsFilename := TPath.Combine(FDataDir, LProcMetricsFilenameOnly);
      If TFile.Exists(LProcMetricsFilename) then
        TFile.Delete(LProcMetricsFilename);
    end;
    if LFailedFilenames.Count > 0 then
      MessageDlg(
        'The operation completed except for the following file(s), which are  '+
        'badly formatted and could not be updated. Now, you must recompile your '+
        'project and run it. After you close the application (or move it '+
        'between background and foreground on Android/iOS), a performance '+
        'file (ALCodeProfilerProcMetrics.dat) will be generated in the user''s '+
        'document folder.' + sLineBreak + sLineBreak + LFailedFilenames.Text, mtError, [mbOK], 0)
    else
      MessageDlg(
        'The operation completed successfully. Now, you must recompile your '+
        'project and run it. After you close the application (or move it '+
        'between background and foreground on Android/iOS), a performance '+
        'file (ALCodeProfilerProcMetrics.dat) will be generated in the user''s '+
        'document folder.',
        mtInformation,
        [mbOK], 0);
  Finally
    ALFreeAndNil(LSourceFilenames);
    ALFreeAndNil(LFailedFilenames);
    ALFreeAndNil(LProcIDMap);
  end;
end;

{**************************}
procedure TMainForm.Refresh;
begin
  GridTableViewProcMetrics.BeginUpdate;
  try
    GridTableViewProcMetrics.DataController.RecordCount := 0;
    for var I := low(FProcMetrics) to high(FProcMetrics) do begin
      case FHistoryGroupMode of
        hgmNone: begin
          if FOverrideFilterParentExecutionID <> 0 then begin
            if FProcMetrics[i].ParentExecutionID <> FOverrideFilterParentExecutionID then
              continue;
            //--
            if (FOverrideFilterThreadID <> High(Cardinal)) and
               (FProcMetrics[i].ThreadID <> FOverrideFilterThreadID) then
              continue;
            //--
            if (FFilterStartTimeStampMin > 0) and
               (FProcMetrics[i].StartTimeStamp + FProcMetrics[i].ElapsedTicks < FFilterStartTimeStampMin) then
              continue;
            //--
            if (FFilterStartTimeStampMax > 0) and
               (FProcMetrics[i].StartTimeStamp > FFilterStartTimeStampMax) then
              continue;
          end
          else begin
            if (FFilterParentExecutionIDs.Count > 0) and
               (not FFilterParentExecutionIDs.Contains(FProcMetrics[i].ParentExecutionID)) then
              continue;
            //--
            if (FFilterExecutionIDs.Count > 0) and
               (not FFilterExecutionIDs.Contains(FProcMetrics[i].ExecutionID)) then
              continue;
            //--
            if (FFilterProcIDs.Count > 0) and
               (not FFilterProcIDs.Contains(FProcMetrics[i].ProcID)) then
              continue;
          end;
        end;
        hgmByCallStack: begin
          if FOverrideFilterParentExecutionID <> 0 then begin
            if FProcMetrics[i].ParentMetricsID <> FOverrideFilterParentExecutionID then
              continue;
            //--
            if (FOverrideFilterThreadID <> High(Cardinal)) and
               (FProcMetrics[i].ThreadID <> FOverrideFilterThreadID) then
              continue;
          end
          else begin
            if (FFilterParentExecutionIDs.Count > 0) and
               (not FFilterParentExecutionIDs.Contains(FProcMetrics[i].ParentMetricsID)) then
              continue;
            //--
            if (FFilterProcIDs.Count > 0) and
               (not FFilterProcIDs.Contains(FProcMetrics[i].ProcID)) then
              continue;
          end;
        end;
        hgmByProcID: begin
          if (FFilterProcIDs.Count > 0) and
             (not FFilterProcIDs.Contains(FProcMetrics[i].ProcID)) then
            continue;
        end;
      end;
      var LRecordCount := GridTableViewProcMetrics.DataController.RecordCount;
      inc(LRecordCount);
      GridTableViewProcMetrics.DataController.RecordCount := LRecordCount;
      // No per-call ExecutionID exists in grouped modes, so a mode-specific
      // surrogate is reused as the row identity: ProcID is unique enough in
      // hgmByProcID (there is only ever one row per ProcID), but in
      // hgmByCallStack the same ProcID can appear under different
      // parents, so MetricsID (unique per row, and also the drill-down key
      // matched against ParentMetricsID above) must be used instead.
      case FHistoryGroupMode of
        hgmNone: GridTableViewProcMetrics.DataController.SetValue(LRecordCount-1, GridTableViewProcMetricsColumnExecutionID.Index, FProcMetrics[i].ExecutionID);
        hgmByProcID: GridTableViewProcMetrics.DataController.SetValue(LRecordCount-1, GridTableViewProcMetricsColumnExecutionID.Index, FProcMetrics[i].ProcID);
        hgmByCallStack: GridTableViewProcMetrics.DataController.SetValue(LRecordCount-1, GridTableViewProcMetricsColumnExecutionID.Index, FProcMetrics[i].MetricsID);
      end;
      GridTableViewProcMetrics.DataController.SetValue(LRecordCount-1, GridTableViewProcMetricsColumnThreadID.Index, FProcMetrics[i].ThreadID);
      GridTableViewProcMetrics.DataController.SetValue(LRecordCount-1, GridTableViewProcMetricsColumnProcName.Index, String(FProcIDMap.Values[ALIntToStrA(FProcMetrics[i].ProcID)]));
      if FHistoryGroupMode = hgmNone then
        GridTableViewProcMetrics.DataController.SetValue(LRecordCount-1, GridTableViewProcMetricsColumnStartTimestamp.Index, FProcMetrics[i].StartTimeStamp * ALCodeProfilerMillisecondsPerTick)
      else
        GridTableViewProcMetrics.DataController.SetValue(LRecordCount-1, GridTableViewProcMetricsColumnCallCount.Index, FProcMetrics[i].CallCount);
      GridTableViewProcMetrics.DataController.SetValue(LRecordCount-1, GridTableViewProcMetricsColumnTimeTaken.Index, FProcMetrics[i].ElapsedTicks * ALCodeProfilerMillisecondsPerTick);
    end;
  finally
    GridTableViewProcMetrics.EndUpdate;
  end;
  GridTableViewProcMetrics.Controller.TopRecordIndex := 0;
end;

{****************************************************}
procedure TMainForm.LoadDataBtnClick(Sender: TObject);
begin
  FHistoryGroupMode := GetSelectedHistoryGroupModeEnum;
  var LProcMetricsFilenameOnly := GetSelectedProcMetricsFilename;
  var LProcMetricsFilename := TPath.Combine(FDataDir, LProcMetricsFilenameOnly);
  If not TFile.Exists(LProcMetricsFilename) then
    raise Exception.CreateFmt(
            'The required file "%s" is missing. Please make '+
            'sure it is available in the data subfolder where '+
            'Alcinoe Code Profiler is located before proceeding.',
            [LProcMetricsFilenameOnly]);
  LoadDataBtn.Cursor := crHourGlass;
  Try

    // Load ALCodeProfilerProcMetrics.dat in FProcMetrics
    var LRawRecordSize := GetProcMetricsRawRecordSize(FHistoryGroupMode);
    Var LfileStream := TfileStream.Create(LProcMetricsFilename, fmOpenRead);
    try
      if LfileStream.Size mod LRawRecordSize <> 0 then
        raise Exception.CreateFmt('The file "%s" is corrupted', [LProcMetricsFilenameOnly]);
      var LRawBytes: TBytes;
      SetLength(LRawBytes, LfileStream.Size);
      if Length(LRawBytes) > 0 then
        LfileStream.ReadBuffer(LRawBytes[0], Length(LRawBytes));
      setlength(FProcMetrics, Length(LRawBytes) div LRawRecordSize);
      for var I := low(FProcMetrics) to high(FProcMetrics) do
        DecodeProcMetricsRaw(FHistoryGroupMode, LRawBytes, I * LRawRecordSize, FProcMetrics[i]);
    finally
      ALFreeandNil(LfileStream);
    end;

    // Update orphan node
    case FHistoryGroupMode of
      hgmNone: begin
        Var LDictionary := TDictionary<Cardinal, boolean>.Create;
        try
          For var I := low(FProcMetrics) to High(FProcMetrics) do
            LDictionary.Add(FProcMetrics[i].ExecutionID, true);
          var LBool: Boolean;
          For var I := low(FProcMetrics) to High(FProcMetrics) do
            if not LDictionary.TryGetValue(FProcMetrics[i].ParentExecutionID, LBool) then
              FProcMetrics[i].ParentExecutionID := 0;
        finally
          AlFreeAndNil(LDictionary);
        end;
      end;
      hgmByCallStack: begin
        Var LDictionary := TDictionary<Cardinal, boolean>.Create;
        try
          For var I := low(FProcMetrics) to High(FProcMetrics) do
            LDictionary.AddOrSetValue(FProcMetrics[i].MetricsID, true);
          var LBool: Boolean;
          For var I := low(FProcMetrics) to High(FProcMetrics) do
            if not LDictionary.TryGetValue(FProcMetrics[i].ParentMetricsID, LBool) then
              FProcMetrics[i].ParentMetricsID := 0;
        finally
          AlFreeAndNil(LDictionary);
        end;
      end;
    end;

    // Load ALCodeProfilerProcIDMap.txt in FProcIDMap
    var LProcIDMapFilename := TPath.Combine(FDataDir, ALCodeProfilerProcIDMapFilename);
    If not TFile.Exists(LProcIDMapFilename) then
      raise Exception.CreateFmt('The required file "%s" does not exist. Please ensure it is available before proceeding', [ALCodeProfilerProcIDMapFilename]);
    FProcIDMap.Clear;
    FProcIDMap.LoadFromFile(LProcIDMapFilename);

    // Reset Filter
    ResetFilters;
    FFilterParentExecutionIDs.Add(0);

    // Reset TreeListProcMetrics
    ResetTreeListProcMetrics;
    FGoBackStack.Clear;

    // Reset the grid
    Refresh;

    // Clear the StatusBar
    MainStatusBar.Panels[1].Text := '';

  Finally
    LoadDataBtn.Cursor := crDefault;
  End;

  MessageDlg(ALIntToStrW(Length(FProcMetrics)) + ' records have been loaded successfully.', mtInformation, [mbOK], 0);
end;

{*****************************************************}
procedure TMainForm.ClearDataBtnClick(Sender: TObject);
begin
  if MessageDlg(
       'Do you want to delete all the performance data collected so far? ' +
       'Every performance file of the CodeProfiler data folder will be deleted and the grid will be emptied. ' +
       'The procedure IDs (' + ALCodeProfilerProcIDMapFilename + ') are kept, so the sources already instrumented stay valid ' +
       'and the next run will simply collect fresh data.',
       mtConfirmation, [mbYes, mbNo], 0) <> mrYes then exit;

  var LDeletedCount := 0;
  ClearDataBtn.Cursor := crHourGlass;
  try

    // Delete every performance file, whatever the history group mode they
    // were collected with. This must not run while the HTTP server is
    // receiving a new performance file in FDataDir.
    FHttpServerCriticalSection.Acquire;
    try
      if TDirectory.Exists(FDataDir) then begin
        var LFilenames := TDirectory.GetFiles(FDataDir, '*', TSearchOption.soTopDirectoryOnly);
        for var I := low(LFilenames) to high(LFilenames) do begin
          // '.dat~tmp' is the temporary file the HTTP server writes the
          // incoming performance file to before renaming it to '.dat'.
          var LExtension := TPath.GetExtension(LFilenames[I]);
          if (not ALSameTextW(LExtension, '.dat')) and
             (not ALSameTextW(LExtension, '.dat~tmp')) then continue;
          TFile.Delete(LFilenames[I]);
          inc(LDeletedCount);
        end;
      end;
    finally
      FHttpServerCriticalSection.Release;
    end;

    // Drop the data loaded in memory
    setlength(FProcMetrics, 0);
    FProcIDMap.Clear;

    // Reset Filter
    ResetFilters;
    FFilterParentExecutionIDs.Add(0);

    // Reset TreeListProcMetrics
    ResetTreeListProcMetrics;
    FGoBackStack.Clear;

    // Empty the grid
    Refresh;

    // Clear the StatusBar
    MainStatusBar.Panels[1].Text := '';

  finally
    ClearDataBtn.Cursor := crDefault;
  end;

  MessageDlg(ALIntToStrW(LDeletedCount) + ' performance file(s) have been deleted successfully.', mtInformation, [mbOK], 0);
end;

{*****************************************************}
procedure TMainForm.PanelfilterResize(Sender: TObject);
begin
  ProcNameFilterEdit.Width := Panelfilter.Width - ProcNameFilterEdit.Left - 6;
end;

{*****************************************************************}
procedure TMainForm.InstrumentationTabSheetResize(Sender: TObject);
begin
  InstructionPanel.Height := LastInstructionLabel.Top + LastInstructionLabel.Height + LastInstructionLabel.Margins.Bottom;
end;

{*******************************************************}
procedure TMainForm.ExportToCsvBtnClick(Sender: TObject);

var
  LCsvStream: TFileStream;
  LCsvBuffer: AnsiString;
  LCsvBufferPos: Integer;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _WriteToCsvBuffer(const AStr: AnsiString);
  begin
    if LCsvBufferPos + length(AStr) > length(LCsvBuffer) then begin
      LCsvStream.WriteBuffer(PAnsiChar(LCsvBuffer)^, LCsvBufferPos);
      LCsvBufferPos := 0;
    end;
    ALMove(PAnsiChar(AStr)^, LCsvBuffer[LCsvBufferPos + 1], length(AStr));
    LCsvBufferPos := LCsvBufferPos + length(AStr);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _FlushCsvBuffer;
  begin
    if LCsvBufferPos > 0 then begin
      LCsvStream.WriteBuffer(PAnsiChar(LCsvBuffer)^, LCsvBufferPos);
      LCsvBufferPos := 0;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _AppendToCsvRow(var ARow: AnsiString; var AFirstColumn: Boolean; const AValue: AnsiString);
  begin
    if not AFirstColumn then ARow := ARow + ',';
    AFirstColumn := False;
    ARow := ARow + AValue;
  end;

begin
  var LHistoryGroupMode := GetSelectedHistoryGroupModeEnum;
  var LRawRecordSize := GetProcMetricsRawRecordSize(LHistoryGroupMode);
  var LColumns := GetProcMetricsColumnsForMode(LHistoryGroupMode);
  var LProcMetricsFilenameOnly := GetSelectedProcMetricsFilename;
  var LProcMetricsFilename := TPath.Combine(FDataDir, LProcMetricsFilenameOnly);
  If not TFile.Exists(LProcMetricsFilename) then
    raise Exception.CreateFmt(
            'The required file "%s" is missing. Please make '+
            'sure it is available in the data subfolder where '+
            'Alcinoe Code Profiler is located before proceeding.',
            [LProcMetricsFilenameOnly]);

  // Ask which columns to export
  var LExportColumns: TArray<Boolean>;
  SetLength(LExportColumns, Length(LColumns));
  var LColumnsForm := TForm.CreateNew(nil);
  try
    LColumnsForm.Caption := 'Export to CSV';
    LColumnsForm.BorderStyle := bsDialog;
    LColumnsForm.Position := poScreenCenter;
    LColumnsForm.ClientWidth := 300;
    LColumnsForm.ClientHeight := 233;
    var LColumnsLabel := TLabel.Create(LColumnsForm);
    LColumnsLabel.Parent := LColumnsForm;
    LColumnsLabel.Caption := 'Select the columns to export:';
    LColumnsLabel.SetBounds(8, 8, LColumnsForm.ClientWidth - 16, 15);
    var LColumnsCheckListBox := TCheckListBox.Create(LColumnsForm);
    LColumnsCheckListBox.Parent := LColumnsForm;
    LColumnsCheckListBox.SetBounds(8, 29, LColumnsForm.ClientWidth - 16, 161);
    for var I := 0 to High(LColumns) do begin
      LColumnsCheckListBox.Items.Add(String(GetProcMetricsColumnName(LColumns[I])));
      LColumnsCheckListBox.Checked[I] := True;
    end;
    var LOkBtn := TButton.Create(LColumnsForm);
    LOkBtn.Parent := LColumnsForm;
    LOkBtn.Caption := 'OK';
    LOkBtn.ModalResult := mrOk;
    LOkBtn.Default := True;
    LOkBtn.SetBounds(LColumnsForm.ClientWidth - 170, 198, 75, 27);
    var LCancelBtn := TButton.Create(LColumnsForm);
    LCancelBtn.Parent := LColumnsForm;
    LCancelBtn.Caption := 'Cancel';
    LCancelBtn.ModalResult := mrCancel;
    LCancelBtn.Cancel := True;
    LCancelBtn.SetBounds(LColumnsForm.ClientWidth - 87, 198, 75, 27);
    if LColumnsForm.ShowModal <> mrOk then exit;
    for var I := 0 to High(LColumns) do
      LExportColumns[I] := LColumnsCheckListBox.Checked[I];
  finally
    ALFreeAndNil(LColumnsForm);
  end;
  var LExportAnyColumn := False;
  for var I := 0 to High(LColumns) do
    LExportAnyColumn := LExportAnyColumn or LExportColumns[I];
  if not LExportAnyColumn then
    Raise Exception.Create('Error: No columns have been selected');

  // The proc ID map is only needed to resolve the ProcName column
  var LNeedProcNames := False;
  for var I := 0 to High(LColumns) do
    if (LColumns[I] = colkProcName) and LExportColumns[I] then LNeedProcNames := True;
  var LProcIDMapFilename := TPath.Combine(FDataDir, ALCodeProfilerProcIDMapFilename);
  If LNeedProcNames and (not TFile.Exists(LProcIDMapFilename)) then
    raise Exception.CreateFmt('The required file "%s" does not exist. Please ensure it is available before proceeding', [ALCodeProfilerProcIDMapFilename]);

  // Ask where to save the CSV file
  var LCsvFilename: String;
  var LSaveDialog := TSaveDialog.Create(nil);
  try
    LSaveDialog.Title := 'Export to CSV';
    LSaveDialog.Filter := 'CSV files (*.csv)|*.csv|All files (*.*)|*.*';
    LSaveDialog.DefaultExt := 'csv';
    LSaveDialog.Options := LSaveDialog.Options + [ofOverwritePrompt];
    LSaveDialog.FileName := ALStringReplaceW(LProcMetricsFilenameOnly, '.dat', '.csv', [rfIgnoreCase]);
    if not LSaveDialog.Execute then exit;
    LCsvFilename := LSaveDialog.FileName;
  finally
    ALFreeAndNil(LSaveDialog);
  end;

  ExportToCsvBtn.Cursor := crHourGlass;
  var LExportedRecordCount: Int64 := 0;
  Try

    // Load ALCodeProfilerProcIDMap.txt in LProcNames
    var LProcNames := TDictionary<Cardinal, AnsiString>.Create;
    var LProcMetricsStream: TFileStream := nil;
    LCsvStream := nil;
    try
      if LNeedProcNames then begin
        var LProcIDMap := TALHashedStringListA.Create;
        try
          LProcIDMap.LoadFromFile(LProcIDMapFilename);
          for var I := 0 to LProcIDMap.Count - 1 do
            LProcNames.AddOrSetValue(Cardinal(ALStrToInt(LProcIDMap.Names[I])), LProcIDMap.ValueFromIndex[I]);
        finally
          ALFreeAndNil(LProcIDMap);
        end;
      end;

      // Convert ALCodeProfilerProcMetrics.dat to CSV chunk by chunk as the
      // file can be very huge and can not be fully loaded in memory
      LProcMetricsStream := TFileStream.Create(LProcMetricsFilename, fmOpenRead or fmShareDenyWrite);
      if LProcMetricsStream.Size mod LRawRecordSize <> 0 then
        raise Exception.CreateFmt('The file "%s" is corrupted', [LProcMetricsFilenameOnly]);
      var LTotalRecordCount: Int64 := LProcMetricsStream.Size div LRawRecordSize;
      LCsvStream := TFileStream.Create(LCsvFilename, fmCreate);
      var LRawBuffer: TBytes;
      SetLength(LRawBuffer, 65536 * LRawRecordSize); // ~2 MB chunk
      Setlength(LCsvBuffer, 4194304); // 4 MB
      LCsvBufferPos := 0;
      var LCsvHeader: AnsiString := '';
      var LFirstColumn := True;
      for var I := 0 to High(LColumns) do
        if LExportColumns[I] then
          _AppendToCsvRow(LCsvHeader, LFirstColumn, GetProcMetricsColumnName(LColumns[I]));
      _WriteToCsvBuffer(LCsvHeader + #13#10);
      While True do begin
        var LBytesRead := LProcMetricsStream.Read(LRawBuffer[0], Length(LRawBuffer));
        if LBytesRead <= 0 then break;
        if LBytesRead mod LRawRecordSize <> 0 then
          raise Exception.CreateFmt('The file "%s" is corrupted', [LProcMetricsFilenameOnly]);
        for var I := 0 to (LBytesRead div LRawRecordSize) - 1 do begin
          var LRec: TALProcMetrics;
          DecodeProcMetricsRaw(LHistoryGroupMode, LRawBuffer, I * LRawRecordSize, LRec);
          var LCsvRow: AnsiString := '';
          LFirstColumn := True;
          for var J := 0 to High(LColumns) do
            if LExportColumns[J] then
              _AppendToCsvRow(LCsvRow, LFirstColumn, GetProcMetricsColumnValue(LColumns[J], LRec, LProcNames));
          _WriteToCsvBuffer(LCsvRow + #13#10);
          inc(LExportedRecordCount);
        end;
        if LTotalRecordCount > 0 then begin
          MainStatusBar.Panels[1].Text := 'Exporting to CSV: ' + ALIntToStrW(Round((LExportedRecordCount / LTotalRecordCount) * 100)) + '%';
          MainStatusBar.Update;
        end;
      end;
      _FlushCsvBuffer;
    finally
      ALFreeAndNil(LProcNames);
      ALFreeAndNil(LProcMetricsStream);
      ALFreeAndNil(LCsvStream);
    end;

  Finally
    ExportToCsvBtn.Cursor := crDefault;
    MainStatusBar.Panels[1].Text := '';
  End;

  MessageDlg(ALIntToStrW(LExportedRecordCount) + ' records have been exported successfully.', mtInformation, [mbOK], 0);
end;

{**********************************************}
procedure TMainForm.FormCreate(Sender: TObject);
begin
  FDataDir := ALGetModulePathW + 'data\';
  FLoadingSettings := True;
  FProcIDSequence := 0;
  Setlength(FProcMetrics, 0);
  FProcIDMap := TALHashedStringListA.Create;
  ResetTreeListProcMetrics;
  FFilterProcIDs := THashSet<Cardinal>.Create;
  FFilterExecutionIDs := THashSet<Cardinal>.Create;
  FFilterParentExecutionIDs := THashSet<Cardinal>.Create;
  FFilterParentExecutionIDs.Add(0);
  FFilterStartTimeStampMin := 0;
  FFilterStartTimeStampMax := 0;
  FOverrideFilterParentExecutionID := 0;
  FOverrideFilterThreadID := High(Cardinal);
  FGoBackStack := TDictionary<Int64{ExecutionID}, TGoBackStackItem>.Create;
  FHttpServerCriticalSection := TCriticalSection.Create;
  //--
  TreeListProcMetricsColumnExecutionID.Visible := False;
  GridTableViewProcMetricsColumnExecutionID.Visible := False;
  If not TDirectory.Exists(FDataDir) then TDirectory.CreateDirectory(FDataDir);
  var LRegistry := TRegistry.Create(KEY_WRITE);
  try
    LRegistry.RootKey := HKEY_CURRENT_USER;
    if LRegistry.OpenKey(ALCodeProfilerRegistryPath, True{Cancreate}) then begin
      LRegistry.WriteString(ALCodeProfilerDataStoragePathKey, FDataDir);
      LRegistry.CloseKey;
    end
    else
      raise Exception.Create('Failed to open or create registry key: ' + ALCodeProfilerRegistryPath);
  finally
    LRegistry.Free;
  end;
  var LIniFile := TIniFile.Create(TPath.Combine(FDataDir, ConfigFilename));
  try
    SourcesPathMemo.Text := ALStringReplaceW(LIniFile.ReadString('General','SourcesPath', '..\..\Source\;..\..\Embarcadero\Florence\fmx\;..\..\Demos\ALFmxDynamicListBox\_Source\'), ';', #13#10, [RfReplaceALL]);
    CodeProfilerIncFilenameEdit.Text := LIniFile.ReadString('General','CodeProfilerIncFilename', '..\..\Source\Alcinoe.CodeProfiler.inc');
  finally
    ALFreeAndNil(LIniFile);
  end;
  FLoadingSettings := False;
  // Must be done after Config.ini has been read, as the location of
  // Alcinoe.CodeProfiler.inc is one of its settings.
  LoadCodeProfilerIncFile;
  FHistoryGroupMode := GetSelectedHistoryGroupModeEnum;
  UpdateHistoryGroupModeUI;
  InstrumentationTabSheetResize(nil);
  PanelfilterResize(nil);
end;

{***********************************************}
procedure TMainForm.FormDestroy(Sender: TObject);
begin
  setlength(FProcMetrics, 0);
  ALFreeAndNil(FProcIDMap);
  ALFreeAndNil(FFilterProcIDs);
  ALFreeAndNil(FFilterExecutionIDs);
  ALFreeAndNil(FFilterParentExecutionIDs);
  ALFreeAndNil(FGoBackStack);
  ALFreeAndNil(FHttpServerCriticalSection);
end;

{*******************************************************}
procedure TMainForm.GridTableViewProcMetricsCellDblClick(
            Sender: TcxCustomGridTableView;
            ACellViewInfo: TcxGridTableDataCellViewInfo;
            AButton: TMouseButton;
            AShift: TShiftState;
            var AHandled: Boolean);
begin
  // In hgmByProcID, calls are grouped by ProcID only, with no parent/child
  // relationship recorded, so there is nothing to drill into.
  if FHistoryGroupMode = hgmByProcID then Exit;
  if ACellViewInfo.GridRecord <> nil then begin
    var LGoBackStackItem: TGoBackStackItem;
    LGoBackStackItem.TopRowIndex := GridTableViewProcMetrics.Controller.TopRowIndex;
    LGoBackStackItem.FocusedRowIndex := GridTableViewProcMetrics.Controller.FocusedRowIndex;
    LGoBackStackItem.SortColumnIndex := -1;
    LGoBackStackItem.SortOrder := TcxGridSortOrder.soNone;
    for var i := 0 to GridTableViewProcMetrics.ColumnCount - 1 do begin
      var LColumn := GridTableViewProcMetrics.Columns[i];
      if LColumn.SortIndex <> -1 then begin
        LGoBackStackItem.SortColumnIndex := i;
        LGoBackStackItem.SortOrder := LColumn.SortOrder;
        break;
      end;
    end;
    FGoBackStack.Remove(FOverrideFilterParentExecutionID);
    FGoBackStack.add(FOverrideFilterParentExecutionID, LGoBackStackItem);
    //--
    FOverrideFilterParentExecutionID := ACellViewInfo.GridRecord.Values[GridTableViewProcMetricsColumnExecutionID.Index];
    FOverrideFilterThreadID := ACellViewInfo.GridRecord.Values[GridTableViewProcMetricsColumnThreadID.Index];
    FTreeListProcMetricsTailNode := TreeListProcMetrics.AddChild(FTreeListProcMetricsTailNode);
    FTreeListProcMetricsTailNode.Texts[TreeListProcMetricsColumnExecutionID.ItemIndex] := ACellViewInfo.GridRecord.Values[GridTableViewProcMetricsColumnExecutionID.Index];
    FTreeListProcMetricsTailNode.Texts[TreeListProcMetricsColumnThreadID.ItemIndex] := ACellViewInfo.GridRecord.Values[GridTableViewProcMetricsColumnThreadID.Index];
    FTreeListProcMetricsTailNode.Texts[TreeListProcMetricsColumnProcName.ItemIndex] := ACellViewInfo.GridRecord.Values[GridTableViewProcMetricsColumnProcName.Index];
    if FHistoryGroupMode = hgmNone then
      FTreeListProcMetricsTailNode.Texts[TreeListProcMetricsColumnStartTimeStamp.ItemIndex] := ACellViewInfo.GridRecord.Values[GridTableViewProcMetricsColumnStartTimestamp.Index]
    else
      FTreeListProcMetricsTailNode.Texts[TreeListProcMetricsColumnCallCount.ItemIndex] := ACellViewInfo.GridRecord.Values[GridTableViewProcMetricsColumnCallCount.Index];
    FTreeListProcMetricsTailNode.Texts[TreeListProcMetricsColumnTimeTaken.ItemIndex] := ACellViewInfo.GridRecord.Values[GridTableViewProcMetricsColumnTimeTaken.Index];
    TreeListProcMetrics.FullExpand;
    TreeListProcMetrics.TopVisibleNode := FTreeListProcMetricsTailNode;
    Refresh;
  end;
end;

{**************************************************************************************************************************************************************}
procedure TMainForm.GridTableViewProcMetricsColumnStartTimestampGetDisplayText(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; var AText: string);
begin
  var LVariant := ARecord.Values[Sender.Index];
  If VarIsNull(LVariant) then exit;
  var LMilliseconds: Double := LVariant;
  //var LHours: integer := Trunc(LMilliseconds) div (1000 * 60 * 60);
  //LMilliseconds := LMilliseconds - (LHours * 1000 * 60 * 60);
  var LMinutes: integer := Trunc(LMilliseconds) div (1000 * 60);
  LMilliseconds := LMilliseconds - (LMinutes * 1000 * 60);
  var LSeconds: integer := Trunc(LMilliseconds) div 1000;
  LMilliseconds := LMilliseconds - (LSeconds * 1000);
  var LHundredNanoseconds: integer := Round(Frac(LMilliseconds) * 10000);
  AText := ALFormatW({'%.2d:'+}'%.2d:%.2d:%.3d.%.5d', [{LHours,} LMinutes, LSeconds, Trunc(LMilliseconds), LHundredNanoseconds])
end;

{**********************************************************************}
procedure TMainForm.HttpServerPortEditPropertiesChange(Sender: TObject);
begin
  MainStatusBar.Panels[1].Text := '';
  SaveCodeProfilerIncFile;
  IdHTTPServer.Active := False;
  if GetSelectedServerName <> '' then begin
    IdHTTPServer.DefaultPort := ALStrToInt(ALTrim(HttpServerPortEdit.Text));
    try
      IdHTTPServer.Active := True;
      MainStatusBar.Panels[0].Text := 'Listening on port ' + HttpServerPortEdit.Text;
    except
      MainStatusBar.Panels[0].Text := 'Not listening';
      Raise;
    end;
  end
  else MainStatusBar.Panels[0].Text := 'Not listening';
end;

{**********************************************************************}
procedure TMainForm.HttpServerNameEditPropertiesChange(Sender: TObject);
begin
  SaveCodeProfilerIncFile;
end;

{**********************************************************************************************************************************************}
procedure TMainForm.TreeListProcMetricsColumnStartTimeStampGetDisplayText(Sender: TcxTreeListColumn; ANode: TcxTreeListNode; var Value: string);
begin
  var LVariant := ANode.Values[Sender.ItemIndex];
  If VarIsNull(LVariant) then exit;
  var LMilliseconds: Double := LVariant;
  //var LHours: integer := Trunc(LMilliseconds) div (1000 * 60 * 60);
  //LMilliseconds := LMilliseconds - (LHours * 1000 * 60 * 60);
  var LMinutes: integer := Trunc(LMilliseconds) div (1000 * 60);
  LMilliseconds := LMilliseconds - (LMinutes * 1000 * 60);
  var LSeconds: integer := Trunc(LMilliseconds) div 1000;
  LMilliseconds := LMilliseconds - (LSeconds * 1000);
  var LHundredNanoseconds: integer := Round(Frac(LMilliseconds) * 10000);
  Value := ALFormatW({'%.2d:'+}'%.2d:%.2d:%.3d.%.5d', [{LHours,} LMinutes, LSeconds, Trunc(LMilliseconds), LHundredNanoseconds])
end;

{***************************************************************}
procedure TMainForm.TreeListProcMetricsDblClick(Sender: TObject);
begin
  var LClickedNode := TreeListProcMetrics.HitTest.HitNode;
  var LfocusedNode := TreeListProcMetrics.focusedNode;
  if Assigned(LClickedNode) and Assigned(LfocusedNode) then begin
    FOverrideFilterParentExecutionID := LfocusedNode.Values[TreeListProcMetricsColumnExecutionID.ItemIndex];
    if LfocusedNode = FTreeListProcMetricsRootNode then
      FOverrideFilterThreadID := High(Cardinal)
    else
      FOverrideFilterThreadID := LfocusedNode.Values[TreeListProcMetricsColumnThreadID.ItemIndex];
    FTreeListProcMetricsTailNode := LfocusedNode;
    FTreeListProcMetricsTailNode.DeleteChildren;
  end
  else begin
    FOverrideFilterParentExecutionID := 0;
    FOverrideFilterThreadID := High(Cardinal);
    ResetTreeListProcMetrics;
  end;

  var LGoBackStackItem: TGoBackStackItem;
  if FGoBackStack.TryGetValue(FOverrideFilterParentExecutionID, LGoBackStackItem) then begin
    for var i := 0 to GridTableViewProcMetrics.ColumnCount - 1 do begin
      if i = LGoBackStackItem.SortColumnIndex then begin
        GridTableViewProcMetrics.Columns[i].SortIndex := 0;
        GridTableViewProcMetrics.Columns[i].SortOrder := LGoBackStackItem.SortOrder;
      end
      else begin
        GridTableViewProcMetrics.Columns[i].SortIndex := -1;
        GridTableViewProcMetrics.Columns[i].SortOrder := TcxGridSortOrder.soNone;
      end;
    end;
    Refresh;
    GridTableViewProcMetrics.Controller.TopRowIndex := LGoBackStackItem.TopRowIndex;
    GridTableViewProcMetrics.Controller.FocusedRowIndex := LGoBackStackItem.FocusedRowIndex;
  end
  else
    Refresh;

  if FOverrideFilterParentExecutionID = 0 then
    FGoBackStack.Clear;
  GridProcMetrics.SetFocus;
end;

{*******************************************************}
procedure TMainForm.ApplyFilterBtnClick(Sender: TObject);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function TimeStringToMilliseconds(const ATimeStr: string): int64;
  begin
    if ALTrim(ATimeStr) = '' then exit(0);
    var LParts: TArray<string>;
    LParts := ALTrim(ATimeStr).Split([':', '.']);
    if Length(LParts) <> 4 then
      raise Exception.Create('Invalid time format. Expected "mm:ss:zzz.zzzz"');
    var LMinutes := StrToInt(LParts[0]);
    var LSeconds := StrToInt(LParts[1]);
    var LMilliseconds := StrToInt(LParts[2]);
    var LHundredNanoseconds := StrToInt(LParts[3]);
    Result := (LMinutes * 60 * 1000 * 10000) + (LSeconds * 1000 * 10000) + LMilliseconds * 10000 + LHundredNanoseconds;
  end;

begin
  ApplyFilterBtn.Cursor := crHourGlass;
  Try

    ResetFilters;

    FFilterStartTimeStampMin := TimeStringToMilliseconds(ALTrim(StartTimeStampMinEdit.Text));
    FFilterStartTimeStampMax := TimeStringToMilliseconds(ALTrim(StartTimeStampMaxEdit.Text));
    var LProcNameFilter: AnsiString := ALTrim(AnsiString(ProcNameFilterEdit.Text));

    if LProcNameFilter <> '' then begin
      var LProcNames := TalStringListA.Create;
      Try
        LProcNames.LineBreak := ';';
        LProcNames.Text := LProcNameFilter;
        for var I := 0 to LProcNames.Count - 1 do
          for var J := 0 to FProcIDMap.Count - 1 do
            if ALPosIgnoreCaseA(LProcNames[I], FProcIDMap.ValueFromIndex[J]) > 0 then
              FFilterProcIDs.Add(ALStrToInt(FProcIDMap.Names[J]));
      Finally
        ALFreeAndNil(LProcNames);
      End;
      if FFilterProcIDs.Count = 0 then
        FFilterProcIDs.Add(ALMaxUInt);
    end;

    // The start-timestamp filter only makes sense for individual calls,
    // which only exist in hgmNone; grouped modes have no StartTimeStamp.
    if (FHistoryGroupMode = hgmNone) and
       ((FFilterStartTimeStampMin > 0) or
        (FFilterStartTimeStampMax > 0)) then begin
      var LExecutionDict := TDictionary<Cardinal, Cardinal>.create;
      Try
        for var I := low(FProcMetrics) to high(FProcMetrics) do begin
          if (FFilterStartTimeStampMin > 0) and
             (FProcMetrics[i].StartTimeStamp + FProcMetrics[i].ElapsedTicks < FFilterStartTimeStampMin) then
            continue;
          //--
          if (FFilterStartTimeStampMax > 0) and
             (FProcMetrics[i].StartTimeStamp > FFilterStartTimeStampMax) then
            continue;
          //--
          LExecutionDict.Add(FProcMetrics[i].ExecutionID, FProcMetrics[i].ParentExecutionID);
        end;
        //--
        var LExecutionPair: TPair<Cardinal, Cardinal>;
        if LProcNameFilter = '' then begin
          for LExecutionPair in LExecutionDict do
            If not LExecutionDict.ContainsKey(LExecutionPair.Value) then
              FFilterExecutionIDs.Add(LExecutionPair.Key);
        end
        else begin
          for LExecutionPair in LExecutionDict do
            FFilterExecutionIDs.Add(LExecutionPair.Key);
        end;
      Finally
        ALFreeAndNil(LExecutionDict);
      End;
      if FFilterExecutionIDs.Count = 0 then
        FFilterExecutionIDs.Add(ALMaxUInt);
    end;

    if (FFilterProcIDs.Count = 0) and
       (FFilterExecutionIDs.Count = 0) then
      FFilterParentExecutionIDs.Add(0);

    ResetTreeListProcMetrics;
    FGoBackStack.Clear;

    Refresh;

  Finally
    ApplyFilterBtn.Cursor := crDefault;
  End;
end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.