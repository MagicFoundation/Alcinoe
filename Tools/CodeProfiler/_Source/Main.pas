unit Main;

interface

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
  cxTLData, cxSplitter;

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
    GridLevelProcMetrics: TcxGridLevel;
    LoadDataBtn: TcxButton;
    InstructionPanel: TdxPanel;
    cxLabel1: TcxLabel;
    cxLabel2: TcxLabel;
    cxLabel3: TcxLabel;
    cxLabel4: TcxLabel;
    cxLabel5: TcxLabel;
    cxLabel6: TcxLabel;
    cxLabel8: TcxLabel;
    dxPanel2: TdxPanel;
    SourcesPathMemo: TcxMemo;
    cxLabel9: TcxLabel;
    dxPanel3: TdxPanel;
    InsertProfilerMarkersBtn: TcxButton;
    RemoveProfilerMarkersBtn: TcxButton;
    cxSplitter1: TcxSplitter;
    cxStyleTreeListProcMetricsBackground: TcxStyle;
    TreeListProcMetricsColumnStartTimeStamp: TcxTreeListColumn;
    GridTableViewProcMetricsColumnStartTimestamp: TcxGridColumn;
    cxLabel7: TcxLabel;
    LastInstructionLabel: TcxLabel;
    StartTimeStampMinEdit: TcxMaskEdit;
    Label1: TLabel;
    StartTimeStampMaxEdit: TcxMaskEdit;
    Label2: TLabel;
    procedure InsertProfilerMarkersBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LoadDataBtnClick(Sender: TObject);
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
  private
    const ConfigFilename = 'Config.ini';
  private
    FDataDir: String;
    FProcIDSequence: Integer;
    FTreeListProcMetricsTailNode: TcxTreeListNode;
    FProcMetrics: TArray<TALProcMetrics>;
    FProcIDMap: TALHashedStringListA;
    FFilterProcIDs: THashSet<Cardinal>;
    FFilterExecutionIDs: THashSet<Cardinal>;
    FFilterParentExecutionIDs: THashSet<Cardinal>;
    FOverrideFilterParentExecutionID: Cardinal;
    procedure ResetFilters;
    Procedure RemoveMarkers(const AFileName: String);
    Procedure InsertMarkers(const AFileName: String; Const AProcIDMap: TALStringListA);
    procedure Refresh;
  public
  end;

var
  MainForm: TMainForm;

implementation

uses
  System.Variants,
  System.UITypes,
  System.AnsiStrings,
  System.SysUtils,
  System.IOUtils,
  System.Win.Registry,
  system.IniFiles,
  winapi.Windows,
  VCL.Dialogs,
  Alcinoe.StringUtils,
  Alcinoe.Files,
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
  FOverrideFilterParentExecutionID := 0;
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
  if MessageDlg('⚠ WARNING: Make sure to back up your files before continuing! Do you want to continue?', mtWarning, [mbYes, mbCancel], 0) <> mrYes then Exit;
  var LSourceFilenames := TALStringListW.Create;
  try
    ExpandSourcesPath(SourcesPathMemo.Lines, LSourceFilenames);
    if LSourceFilenames.Count = 0 then
      Raise Exception.Create('Error: No files have been selected');
    if MessageDlg('Are you REALLY sure you want to update all the files listed below?' + sLineBreak + sLineBreak + LSourceFilenames.Text, mtWarning, [mbYes, mbCancel], 0) <> mrYes then Exit;
    RemoveProfilerMarkersBtn.Cursor := crHourGlass;
    Try
      for var I := 0 to LSourceFilenames.Count - 1 do
        RemoveMarkers(LSourceFilenames[i]);
    finally
      RemoveProfilerMarkersBtn.Cursor := crDefault;
    End;
    var LProcMetricsFilename := TPath.Combine(FDataDir, ALCodeProfilerProcMetricsFilename);
    If TFile.Exists(LProcMetricsFilename) then
      TFile.Delete(LProcMetricsFilename);
    var LProcIDMapFilename := TPath.Combine(FDataDir, ALCodeProfilerProcIDMapFilename);
    If TFile.Exists(LProcIDMapFilename) then
      TFile.Delete(LProcIDMapFilename);
    MessageDlg('The operation completed successfully', mtInformation, [mbOK], 0);
  Finally
    ALFreeAndNil(LSourceFilenames);
  end;
end;

{*******************************************************************************************}
procedure TMainForm.InsertMarkers(const AFileName: String; Const AProcIDMap: TALStringListA);

type
  TProcStackEntry = record
    ProcID: cardinal;
    ProcName: AnsiString;
    ProcIndent: AnsiString;
    MarkerAfterBeginAdded: Boolean;
  end;

begin
  RemoveMarkers(AFileName);
  var LUnitName := ALExtractFileName(AnsiString(AFileName), true{RemoveFileExt});
  var LUseAdded := False;
  var LAnonymousMethodSequence := 0;
  var LProcStack := TStack<TProcStackEntry>.Create;
  var LSourceCode := TALStringListA.create;
  try
    LSourceCode.LoadFromFile(AFileName);
    var LCurrentProcIndent: AnsiString := '';
    var LAddMarkerAfterNextProcBegin: Boolean := False;
    var LAddMarkerBeforeNextProcEnd: Boolean := False;
    For var I := 0 to LSourceCode.Count - 1 do begin

      var LLine := LSourceCode[i];
      var LTrimedLine := ALTrim(LSourceCode[i]);

      // Ignore lines like:
      //
      // TALDynamicListBox = class(TALControl)
      // public
      //   procedure Prepare; virtual;
      // end;
      if (LAddMarkerAfterNextProcBegin) and
         (LTrimedLine <> '') and
         (LCurrentProcIndent <> '') and
         (ALPosIgnoreCaseA(LCurrentProcIndent, LLine) <> 1) then begin
        LProcStack.Pop;
        LCurrentProcIndent := '';
        LAddMarkerAfterNextProcBegin := False;
        LAddMarkerBeforeNextProcEnd := False;
      end;

      // Add procedure like:
      //
      // Procedure ALDynamicListBoxMakeBufDrawables(const AControl: TALDynamicListBoxControl; const AEnsureDoubleBuffered: Boolean = True);
      // begin
      // end
      //
      // constructor TALDynamicListBoxControl.Create(const AOwner: TObject);
      // begin
      // end
      //
      // TThread.queue(nil,
      //   procedure
      //   begin
      //     ...
      //   end);
      //
      // TThread.CreateAnonymousThread(
      //   procedure
      //   begin
      //   end).Start;
      If (ALPosIgnoreCaseA('class function ', LTrimedLine) = 1) or
         (ALPosIgnoreCaseA('class procedure ', LTrimedLine) = 1) or
         (ALPosIgnoreCaseA('constructor ', LTrimedLine) = 1) or
         (ALPosIgnoreCaseA('destructor ', LTrimedLine) = 1) or
         (ALPosIgnoreCaseA('function', LTrimedLine) = 1) or
         (ALPosIgnoreCaseA('procedure', LTrimedLine) = 1) then begin
        var J := 1;
        While (J <= High(LLine)) and (LLine[j] = ' ') do inc(J);
        var LProcIndent := ALCopyStr(LLine, 1, J-1);
        // Ignore lines like:
        // function PMSessionValidatePrintSettings: OSStatus; cdecl; external libPrintCore name '_PMSessionValidatePrintSettings';
        // function PMSessionSetDestination: OSStatus; cdecl; external libPrintCore name '_PMSessionSetDestination';
        If (LAddMarkerAfterNextProcBegin) and (LProcIndent = LCurrentProcIndent) then begin
          LProcStack.Pop;
          LCurrentProcIndent := '';
          LAddMarkerAfterNextProcBegin := False;
          LAddMarkerBeforeNextProcEnd := False;
          Continue;
        end;
        LCurrentProcIndent := LProcIndent;
        var LProcName: AnsiString;
        If (ALPosIgnoreCaseA('class function ', LTrimedLine) = 1) then LProcName := ALStringReplaceA(LTrimedLine,'class function', '', [rfIgnoreCase])
        else If (ALPosIgnoreCaseA('class procedure ', LTrimedLine) = 1) then LProcName := ALStringReplaceA(LTrimedLine,'class procedure', '', [rfIgnoreCase])
        else If (ALPosIgnoreCaseA('constructor ', LTrimedLine) = 1) then LProcName := ALStringReplaceA(LTrimedLine,'constructor ', '', [rfIgnoreCase])
        else If (ALPosIgnoreCaseA('destructor ', LTrimedLine) = 1) then LProcName := ALStringReplaceA(LTrimedLine,'destructor ', '', [rfIgnoreCase])
        else If (ALPosIgnoreCaseA('function', LTrimedLine) = 1) then LProcName := ALStringReplaceA(LTrimedLine,'function', '', [rfIgnoreCase])
        else If (ALPosIgnoreCaseA('procedure', LTrimedLine) = 1) then LProcName := ALStringReplaceA(LTrimedLine,'procedure', '', [rfIgnoreCase])
        else Raise Exception.Create('Error C67E3412-6E90-4317-89A5-EED1E0A496B2');
        LProcName := ALTrim(LProcName);
        J := 1;
        While true do begin
          While (J <= High(LProcName)) and (LProcName[j] in ['a'..'z','A'..'Z','0'..'9','_','.']) do inc(J);
          If (J <= High(LProcName)) and (LProcName[j] = '<') then
            While (J <= High(LProcName)) and (LProcName[j] <> '>') do inc(J)
          else
            break;
          inc(J);
        end;
        LProcName := ALCopyStr(LProcName, 1, J-1);
        If LProcName = '' then begin
          inc(LAnonymousMethodSequence);
          LProcName := '$AnonymousMethod' + ALIntToStrA(LAnonymousMethodSequence);
        end
        else LAnonymousMethodSequence := 0;
        if LProcStack.Count > 0 then
          LProcName := LProcStack.Peek.ProcName + '.' + LProcName;
        inc(FProcIDSequence);
        var LProcStackEntry: TProcStackEntry;
        LProcStackEntry.ProcID := FProcIDSequence;
        LProcStackEntry.ProcName := LProcName;
        LProcStackEntry.ProcIndent := LCurrentProcIndent;
        LProcStackEntry.MarkerAfterBeginAdded := False;
        LProcStack.Push(LProcStackEntry);
        LAddMarkerAfterNextProcBegin := True;
        LAddMarkerBeforeNextProcEnd := False;
      end

      // Handle "begin"
      else if (LAddMarkerAfterNextProcBegin) and
              (ALPosIgnoreCaseA(LCurrentProcIndent + 'begin', LLine) = 1) then begin
        if LProcStack.Count = 0 then
          raise Exception.Create(
                  'The source code is not properly formatted. '+
                  'CodeProfiler requires all procedures to be perfectly '+
                  'indented to function correctly - ' +
                  'Line: ' + ALIntToStrW(I+1) + ' - ' +
                  'Filename: ' + AFileName + ' - ' +
                  'Error: 75F32B58-8284-493D-BE75-2F9F3DE2DEF4');
        var LNewLine := LLine;
        Insert('{ALCodeProfiler>>}ALCodeProfilerEnterProc('+ALIntToStrA(LProcStack.Peek.ProcID){$IF defined(debug)}+'{ '+LProcStack.Peek.ProcName+' }'{$ENDIF}+'); try{<<ALCodeProfiler}', LNewLine, length(LCurrentProcIndent + 'begin')+1);
        LSourceCode[i] := LNewLine;
        var LProcStackEntry := LProcStack.Peek;
        LProcStackEntry.MarkerAfterBeginAdded := True;
        LProcStack.Pop;
        LProcStack.Push(LProcStackEntry);
        LAddMarkerAfterNextProcBegin := False;
        LAddMarkerBeforeNextProcEnd := True;
      end

      // Handle "end"
      else if (LAddMarkerBeforeNextProcEnd) and
              (ALPosIgnoreCaseA(LCurrentProcIndent + 'end', LLine) = 1) then begin
        if LProcStack.Count = 0 then
          raise Exception.Create(
                  'The source code is not properly formatted. '+
                  'CodeProfiler requires all procedures to be perfectly '+
                  'indented to function correctly - ' +
                  'Line: ' + ALIntToStrW(I+1) + ' - ' +
                  'Filename: ' + AFileName + ' - ' +
                  'Error: 469093AA-2081-4271-97B9-4218B1E88C83');
        var LNewLine := LLine;
        Insert('{ALCodeProfiler>>}finally ALCodeProfilerExitProc('+ALIntToStrA(LProcStack.Peek.ProcID){$IF defined(debug)}+'{ '+LProcStack.Peek.ProcName+' }'{$ENDIF}+'); end;{<<ALCodeProfiler}', LNewLine, length(LCurrentProcIndent)+1);
        LSourceCode[i] := LNewLine;
        AProcIDMap.Add(ALIntToStrA(LProcStack.Peek.ProcId) + '=' + LUnitName + '.' + LProcStack.Peek.ProcName);
        LProcStack.Pop;
        if LProcStack.Count > 0 then begin
          LCurrentProcIndent := LProcStack.Peek.ProcIndent;
          LAddMarkerAfterNextProcBegin := not LProcStack.Peek.MarkerAfterBeginAdded;
          LAddMarkerBeforeNextProcEnd := LProcStack.Peek.MarkerAfterBeginAdded;
        end
        else begin
          LAddMarkerAfterNextProcBegin := False;
          LAddMarkerBeforeNextProcEnd := False;
        end;
      end

      // Handle "use"
      else if (not LUseAdded) and
              (ALPosIgnoreCaseA(LCurrentProcIndent + 'uses', LLine) = 1) then begin
        var LNewLine := LLine;
        Insert('{ALCodeProfiler>>}{$DEFINE ALCodeProfiler}Alcinoe.CodeProfiler,{<<ALCodeProfiler}', LNewLine, length(LCurrentProcIndent + 'uses')+1);
        LSourceCode[i] := LNewLine;
        LUseAdded := True;
      end;

    end;

    LSourceCode.ProtectedSave := true;
    LSourceCode.SaveToFile(AFileName);

  finally
    AlFreeAndNil(LSourceCode);
    ALFreeAndNil(LProcStack);
  end;
end;

{*****************************************************************}
procedure TMainForm.InsertProfilerMarkersBtnClick(Sender: TObject);
begin
  var LIniFile := TIniFile.Create(TPath.Combine(FDataDir, ConfigFilename));
  try
    LIniFile.WriteString('General','SourcesPath',ALStringReplaceW(ALTrim(SourcesPathMemo.Text), #13#10, ';', [RfReplaceALL]));
  finally
    ALFreeAndNil(LIniFile);
  end;
  if MessageDlg('⚠ WARNING: Make sure to back up your files before continuing! Do you want to continue?', mtWarning, [mbYes, mbCancel], 0) <> mrYes then Exit;
  var LSourceFilenames := TALStringListW.Create;
  var LFailedFilenames := TALStringListW.Create;
  var LProcIDMap := TALStringListA.Create;
  try
    ExpandSourcesPath(SourcesPathMemo.Lines, LSourceFilenames);
    if LSourceFilenames.Count = 0 then
      Raise Exception.Create('Error: No files have been selected');
    if MessageDlg('Are you REALLY sure you want to update all the files listed below?' + sLineBreak + sLineBreak + LSourceFilenames.Text, mtWarning, [mbYes, mbCancel], 0) <> mrYes then Exit;
    InsertProfilerMarkersBtn.Cursor := crHourGlass;
    Try
      for var I := 0 to LSourceFilenames.Count - 1 do
        try
          InsertMarkers(LSourceFilenames[i], LProcIDMap);
        Except
          On E: Exception do
            LFailedFilenames.Add(LSourceFilenames[i]);
        end;
      LProcIDMap.SaveToFile(TPath.Combine(FDataDir, ALCodeProfilerProcIDMapFilename));
    finally
      InsertProfilerMarkersBtn.Cursor := crDefault;
    End;
    var LProcMetricsFilename := TPath.Combine(FDataDir, ALCodeProfilerProcMetricsFilename);
    If TFile.Exists(LProcMetricsFilename) then
      TFile.Delete(LProcMetricsFilename);
    if LFailedFilenames.Count > 0 then
      MessageDlg('The operation completed successfully, except for the following file(s), which are badly formatted and could not be updated:' + sLineBreak + LFailedFilenames.Text, mtError, [mbOK], 0)
    else
      MessageDlg('The operation completed successfully', mtInformation, [mbOK], 0);
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
      if FOverrideFilterParentExecutionID <> 0 then begin
        if FProcMetrics[i].ParentExecutionID <> FOverrideFilterParentExecutionID then continue;
      end
      else begin
        if (FFilterParentExecutionIDs.Count > 0) and
           (not FFilterParentExecutionIDs.Contains(FProcMetrics[i].ParentExecutionID)) then continue;
        //--
        if (FFilterExecutionIDs.Count > 0) and
           (not FFilterExecutionIDs.Contains(FProcMetrics[i].ExecutionID)) then continue;
        //--
        if (FFilterProcIDs.Count > 0) and
           (not FFilterProcIDs.Contains(FProcMetrics[i].ProcID)) then continue;
      end;
      var LRecordCount := GridTableViewProcMetrics.DataController.RecordCount;
      inc(LRecordCount);
      GridTableViewProcMetrics.DataController.RecordCount := LRecordCount;
      GridTableViewProcMetrics.DataController.SetValue(LRecordCount-1, GridTableViewProcMetricsColumnExecutionID.Index, FProcMetrics[i].ExecutionID);
      GridTableViewProcMetrics.DataController.SetValue(LRecordCount-1, GridTableViewProcMetricsColumnThreadID.Index, FProcMetrics[i].ThreadID);
      GridTableViewProcMetrics.DataController.SetValue(LRecordCount-1, GridTableViewProcMetricsColumnProcName.Index, String(FProcIDMap.Values[ALIntToStrA(FProcMetrics[i].ProcID)]));
      GridTableViewProcMetrics.DataController.SetValue(LRecordCount-1, GridTableViewProcMetricsColumnStartTimeStamp.Index, FProcMetrics[i].StartTimeStamp * ALCodeProfilerMillisecondsPerTick);
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
  var LProcMetricsFilename := TPath.Combine(FDataDir, ALCodeProfilerProcMetricsFilename);
  If not TFile.Exists(LProcMetricsFilename) then
    raise Exception.CreateFmt('The required file "%s" does not exist. Please ensure it is available before proceeding', [ALCodeProfilerProcMetricsFilename]);

  LoadDataBtn.Cursor := crHourGlass;
  Try

    // Load ALCodeProfilerProcMetrics.dat in FProcMetrics
    Var LfileStream := TfileStream.Create(LProcMetricsFilename, fmOpenRead);
    try
      setlength(FProcMetrics, LfileStream.Size div SizeOf(TALProcMetrics));
      if length(FProcMetrics) > 0 then
        LfileStream.ReadBuffer(FProcMetrics[0],  length(FProcMetrics) * SizeOf(TALProcMetrics));
    finally
      ALFreeandNil(LfileStream);
    end;

    // Update orphan node
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
    TreeListProcMetrics.Clear;

    // Reset the grid
    Refresh;

  Finally
    LoadDataBtn.Cursor := crDefault;
  End;
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

{**********************************************}
procedure TMainForm.FormCreate(Sender: TObject);
begin
  Setlength(FProcMetrics, 0);
  FProcIDSequence := 0;
  FProcIDMap := TALHashedStringListA.Create;
  FDataDir := ALGetModulePathW + 'data\';
  FFilterProcIDs := THashSet<Cardinal>.Create;
  FFilterExecutionIDs := THashSet<Cardinal>.Create;
  FFilterParentExecutionIDs := THashSet<Cardinal>.Create;
  ResetFilters;
  FFilterParentExecutionIDs.Add(0);
  FTreeListProcMetricsTailNode := nil;
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
    SourcesPathMemo.Text := ALStringReplaceW(LIniFile.ReadString('General','SourcesPath', ''), ';', #13#10, [RfReplaceALL]);
  finally
    ALFreeAndNil(LIniFile);
  end;
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
end;

{*******************************************************}
procedure TMainForm.GridTableViewProcMetricsCellDblClick(
            Sender: TcxCustomGridTableView;
            ACellViewInfo: TcxGridTableDataCellViewInfo;
            AButton: TMouseButton;
            AShift: TShiftState;
            var AHandled: Boolean);
begin
  if ACellViewInfo.GridRecord <> nil then begin
    FOverrideFilterParentExecutionID := ACellViewInfo.GridRecord.Values[GridTableViewProcMetricsColumnExecutionID.Index];
    if FTreeListProcMetricsTailNode = nil then FTreeListProcMetricsTailNode := TreeListProcMetrics.Add
    else FTreeListProcMetricsTailNode := TreeListProcMetrics.AddChild(FTreeListProcMetricsTailNode);
    FTreeListProcMetricsTailNode.Texts[TreeListProcMetricsColumnExecutionID.ItemIndex] := ACellViewInfo.GridRecord.Values[GridTableViewProcMetricsColumnExecutionID.Index];
    FTreeListProcMetricsTailNode.Texts[TreeListProcMetricsColumnThreadID.ItemIndex] := ACellViewInfo.GridRecord.Values[GridTableViewProcMetricsColumnThreadID.Index];
    FTreeListProcMetricsTailNode.Texts[TreeListProcMetricsColumnProcName.ItemIndex] := ACellViewInfo.GridRecord.Values[GridTableViewProcMetricsColumnProcName.Index];
    FTreeListProcMetricsTailNode.Texts[TreeListProcMetricsColumnStartTimeStamp.ItemIndex] := ACellViewInfo.GridRecord.Values[GridTableViewProcMetricsColumnStartTimeStamp.Index];
    FTreeListProcMetricsTailNode.Texts[TreeListProcMetricsColumnTimeTaken.ItemIndex] := ACellViewInfo.GridRecord.Values[GridTableViewProcMetricsColumnTimeTaken.Index];
    TreeListProcMetrics.FullExpand;
    TreeListProcMetrics.TopVisibleNode := FTreeListProcMetricsTailNode;
    Refresh;
  end;
end;

{*****************************************************************************}
procedure TMainForm.GridTableViewProcMetricsColumnStartTimestampGetDisplayText(
            Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord;
            var AText: string);
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
  var LMicroSec: integer := Round(Frac(LMilliseconds) * 100000);
  AText := ALFormatW({'%.2d:'+}'%.2d:%.2d:%.3d.%.5d', [{LHours,} LMinutes, LSeconds, Trunc(LMilliseconds), LMicroSec])
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
  var LMicroSec: integer := Round(Frac(LMilliseconds) * 100000);
  Value := ALFormatW({'%.2d:'+}'%.2d:%.2d:%.3d.%.5d', [{LHours,} LMinutes, LSeconds, Trunc(LMilliseconds), LMicroSec])
end;

{***************************************************************}
procedure TMainForm.TreeListProcMetricsDblClick(Sender: TObject);
begin
  var LClickedNode := TreeListProcMetrics.HitTest.HitNode;
  var LfocusedNode := TreeListProcMetrics.focusedNode;
  if Assigned(LClickedNode) and Assigned(LfocusedNode) then begin
    FOverrideFilterParentExecutionID := LfocusedNode.Values[TreeListProcMetricsColumnExecutionID.ItemIndex];
    FTreeListProcMetricsTailNode := LfocusedNode;
    FTreeListProcMetricsTailNode.DeleteChildren;
  end
  else begin
    FOverrideFilterParentExecutionID := 0;
    FTreeListProcMetricsTailNode := nil;
    TreeListProcMetrics.Clear;
  end;
  Refresh;
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
      raise Exception.Create('Invalid time format. Expected "mm:ss:zzz.zzzzz"');
    var LMinutes := StrToInt(LParts[0]);
    var LSeconds := StrToInt(LParts[1]);
    var LMilliseconds := StrToInt(LParts[2]);
    var LMicroseconds := StrToInt(LParts[3]);
    Result := (LMinutes * 60 * 1000 * 10000) + (LSeconds * 1000 * 10000) + LMilliseconds * 10000 + LMicroseconds;
  end;

begin
  ApplyFilterBtn.Cursor := crHourGlass;
  Try

    ResetFilters;

    var LFilterStartTimeStampMin: Int64 := TimeStringToMilliseconds(ALTrim(StartTimeStampMinEdit.Text));
    var LFilterStartTimeStampMax: Int64 := TimeStringToMilliseconds(ALTrim(StartTimeStampMaxEdit.Text));
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
    end;

    if (LFilterStartTimeStampMin > 0) or
       (LFilterStartTimeStampMax > 0) then begin
      var LExecutionDict := TDictionary<Cardinal, Cardinal>.create;
      Try
        for var I := low(FProcMetrics) to high(FProcMetrics) do begin
          if (LFilterStartTimeStampMin > 0) and
             (FProcMetrics[i].StartTimeStamp + FProcMetrics[i].ElapsedTicks < LFilterStartTimeStampMin) then
            continue;
          //--
          if (LFilterStartTimeStampMax > 0) and
             (FProcMetrics[i].StartTimeStamp > LFilterStartTimeStampMax) then
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
    end;

    if (FFilterProcIDs.Count = 0) and
       (FFilterExecutionIDs.Count = 0) then
      FFilterParentExecutionIDs.Add(0);

    FTreeListProcMetricsTailNode := nil;
    TreeListProcMetrics.Clear;
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
