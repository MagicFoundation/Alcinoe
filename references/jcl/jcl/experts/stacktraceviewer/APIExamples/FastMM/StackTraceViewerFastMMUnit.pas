unit StackTraceViewerFastMMUnit;

interface

uses
  SysUtils, Classes, Forms, Contnrs, FastMMParser, JclStackTraceViewerAPI;

type
  TCustomTreeViewLinkClass = class of TCustomTreeViewLink;

  TCustomTreeViewLink = class(TInterfacedObject, IJclStackTraceViewerTreeViewLink)
  private
    FItems: TInterfaceList;
    FText: string;
  public
    constructor Create(const AText: string);
    destructor Destroy; override;
    function Add(const AText: string; AClass: TCustomTreeViewLinkClass): TCustomTreeViewLink;
    procedure Clear;
    procedure DoShow(AFrame: TCustomFrame); virtual;
    function GetCount: Integer;
    function GetFrameClass: TCustomFrameClass; virtual;
    function GetItems(AIndex: Integer): IJclStackTraceViewerTreeViewLink;
    function GetText: string;
  end;

  TRootTreeViewLink = class(TCustomTreeViewLink);

  TDummyTreeViewLink = class(TCustomTreeViewLink);

  TLeakTreeViewLink = class(TCustomTreeViewLink)
  private
    FLeakData: TFastMMLeak;
  public
    constructor Create(const AText: string);
    procedure DoShow(AFrame: TCustomFrame); override;
    function GetFrameClass: TCustomFrameClass; override;
    property LeakData: TFastMMLeak read FLeakData write FLeakData;
  end;

  TLeakGroupTreeViewLink = class(TCustomTreeViewLink)
  private
    FLeakGroupData: TFastMMLeakGroup;
  public
    constructor Create(const AText: string);
    procedure DoShow(AFrame: TCustomFrame); override;
    function GetFrameClass: TCustomFrameClass; override;
    property LeakData: TFastMMLeakGroup read FLeakGroupData write FLeakGroupData;
  end;

  TLeakSummaryTreeViewLink = class(TCustomTreeViewLink)
  private
    FReport: TFastMMReport;
  public
    constructor Create(const AText: string);
    procedure DoShow(AFrame: TCustomFrame); override;
    function GetFrameClass: TCustomFrameClass; override;
    property Report: TFastMMReport read FReport write FReport;
  end;

  TFreedObjectTreeViewLink = class(TCustomTreeViewLink)
  private
    FFreedObjectData: TFastMMVMOnFreedObject;
  public
    constructor Create(const AText: string);
    procedure DoShow(AFrame: TCustomFrame); override;
    function GetFrameClass: TCustomFrameClass; override;
    property FreedObjectData: TFastMMVMOnFreedObject read FFreedObjectData write FFreedObjectData;
  end;

  TFastMMReportData = class(TObject)
  private
    FReportList: TObjectList;
    FRootLink: TRootTreeViewLink;
    FRootLinkIntf: IJclStackTraceViewerTreeViewLink;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFastMMFile(const AFileName: string);
  end;

implementation

uses
  FastMMLeakFrame, FastMMLeakGroupFrame, FastMMLeakSummaryFrame, FastMMFreedObjectFrame;

{ TTestTreeViewLink }

constructor TCustomTreeViewLink.Create(const AText: string);
begin
  inherited Create;
  FItems := TInterfaceList.Create;
  FText := AText;
end;

destructor TCustomTreeViewLink.Destroy;
begin
  //FStack.Free;
  FItems.Free;
  inherited Destroy;
end;

function TCustomTreeViewLink.Add(const AText: string; AClass: TCustomTreeViewLinkClass): TCustomTreeViewLink;
begin
  Result := AClass.Create(AText);
  FItems.Add(Result);
end;

procedure TCustomTreeViewLink.Clear;
begin
  FItems.Clear;
end;

procedure TCustomTreeViewLink.DoShow(AFrame: TCustomFrame);
begin
end;

function TCustomTreeViewLink.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TCustomTreeViewLink.GetFrameClass: TCustomFrameClass;
begin
  Result := nil;
end;

function TCustomTreeViewLink.GetItems(AIndex: Integer): IJclStackTraceViewerTreeViewLink;
begin
  if FItems[AIndex].QueryInterface(IJclStackTraceViewerTreeViewLink, Result) <> S_OK then
    Result := nil;
end;

function TCustomTreeViewLink.GetText: string;
begin
  Result := FText;
end;

{ TLeakTreeViewLink }

constructor TLeakTreeViewLink.Create(const AText: string);
begin
  inherited Create(AText);
  FLeakData := nil;
end;

procedure TLeakTreeViewLink.DoShow(AFrame: TCustomFrame);
begin
  inherited DoShow(AFrame);
  if (AFrame is TfrmLeak) and Assigned(FLeakData) then
  begin
    if FLeakData.Stack.Count > 0 then
      StackTraceViewerStackProcessorServices.PrepareLocationInfoList(FLeakData.Stack, False);
    TfrmLeak(AFrame).LeakData := FLeakData;
  end;
end;

function TLeakTreeViewLink.GetFrameClass: TCustomFrameClass;
begin
  Result := TfrmLeak;
end;

{ TLeakGroupTreeViewLink }

constructor TLeakGroupTreeViewLink.Create(const AText: string);
begin
  inherited Create(AText);
  FLeakGroupData := nil;
end;

procedure TLeakGroupTreeViewLink.DoShow(AFrame: TCustomFrame);
begin
  inherited DoShow(AFrame);
  if (AFrame is TfrmLeakGroup) and Assigned(FLeakGroupData) then
  begin
    if (FLeakGroupData.Count > 0) and (FLeakGroupData[0].Stack.Count > 0) then
      StackTraceViewerStackProcessorServices.PrepareLocationInfoList(FLeakGroupData[0].Stack, False);
    TfrmLeakGroup(AFrame).LeakGroupData := FLeakGroupData;
  end;
end;

function TLeakGroupTreeViewLink.GetFrameClass: TCustomFrameClass;
begin
  Result := TfrmLeakGroup;
end;

{ TLeakSummaryTreeViewLink }

constructor TLeakSummaryTreeViewLink.Create(const AText: string);
begin
  inherited Create(AText);
  FReport := nil;
end;

procedure TLeakSummaryTreeViewLink.DoShow(AFrame: TCustomFrame);
begin
  inherited DoShow(AFrame);
  if (AFrame is TfrmLeakSummary) and Assigned(FReport) then
    TfrmLeakSummary(AFrame).Report := FReport;
end;

function TLeakSummaryTreeViewLink.GetFrameClass: TCustomFrameClass;
begin
  Result := TfrmLeakSummary;
end;

{ TFreedObjectTreeViewLink }

constructor TFreedObjectTreeViewLink.Create(const AText: string);
begin
  inherited Create(AText);
  FFreedObjectData := nil;
end;

procedure TFreedObjectTreeViewLink.DoShow(AFrame: TCustomFrame);
begin
  inherited DoShow(AFrame);
  if (AFrame is TfrmFreedObject) and Assigned(FFreedObjectData) then
  begin
    if FFreedObjectData.Stack1.Count > 0 then
      StackTraceViewerStackProcessorServices.PrepareLocationInfoList(FFreedObjectData.Stack1, False);
    if FFreedObjectData.Stack2.Count > 0 then
      StackTraceViewerStackProcessorServices.PrepareLocationInfoList(FFreedObjectData.Stack2, False);
    if FFreedObjectData.Stack3.Count > 0 then
      StackTraceViewerStackProcessorServices.PrepareLocationInfoList(FFreedObjectData.Stack3, False);
    TfrmFreedObject(AFrame).FreedObjectData := FFreedObjectData;
  end;
end;

function TFreedObjectTreeViewLink.GetFrameClass: TCustomFrameClass;
begin
  Result := TfrmFreedObject;
end;

{ TFastMMReportData }

constructor TFastMMReportData.Create;
begin
  inherited Create;
  FReportList := TObjectList.Create;
  FRootLink := TRootTreeViewLink.Create('');
  FRootLinkIntf := FRootLink;
end;

destructor TFastMMReportData.Destroy;
begin
  FRootLinkIntf := nil;
  FReportList.Free;
  inherited Destroy;
end;

procedure TFastMMReportData.LoadFastMMFile(const AFileName: string);
var
  I, J, K: Integer;
  FastMMFileParser: TFastMMFileParser;
  FastMMReport: TFastMMReport;
  ReportLink: TDummyTreeViewLink;
  LeakGroup: TLeakGroupTreeViewLink;
  Leak: TLeakTreeViewLink;
  Summary: TLeakSummaryTreeViewLink;
  FreedObject: TFreedObjectTreeViewLink;
begin
  if Assigned(StackTraceViewerStackServices) then
  begin
    StackTraceViewerStackServices.UnregisterFrameClass(TfrmLeak);
    StackTraceViewerStackServices.UnregisterFrameClass(TfrmLeakGroup);
    StackTraceViewerStackServices.UnregisterFrameClass(TfrmLeakSummary);
    StackTraceViewerStackServices.UnregisterFrameClass(TfrmFreedObject);
    StackTraceViewerStackServices.ShowTree(nil);
  end;
  FReportList.Clear;
  FRootLink.Clear;
  FastMMFileParser := TFastMMFileParser.Create;
  try
    FastMMFileParser.ParseFile(AFileName, FReportList);
  finally
    FastMMFileParser.Free;
  end;
  if Assigned(StackTraceViewerStackServices) then
  begin
    for I := 0 to FReportList.Count - 1 do
    begin
      FastMMReport := TFastMMReport(FReportList[I]);
      ReportLink := TDummyTreeViewLink(FRootLink.Add(Format('Report %d', [I + 1]), TDummyTreeViewLink));
      if FastMMReport.LeakSummary.Count > 0 then
      begin
        Summary := TLeakSummaryTreeViewLink(ReportLink.Add('Leak Summary', TLeakSummaryTreeViewLink));
        Summary.Report := FastMMReport;
      end;
      for J := 0 to FastMMReport.LeakGroupCount - 1 do
      begin
        if FastMMReport.LeakGroupItems[J].Count = 1 then
        begin
          Leak := TLeakTreeViewLink(ReportLink.Add(Format('Leak %d', [J + 1]), TLeakTreeViewLink));
          Leak.LeakData := FastMMReport.LeakGroupItems[J][0];
        end
        else
        begin
          LeakGroup := TLeakGroupTreeViewLink(ReportLink.Add(Format('Leak Group %d', [J + 1]), TLeakGroupTreeViewLink));
          LeakGroup.LeakData := FastMMReport.LeakGroupItems[J];
          for K := 0 to FastMMReport.LeakGroupItems[J].Count - 1 do
          begin
            Leak := TLeakTreeViewLink(LeakGroup.Add(Format('Leak %d', [K + 1]), TLeakTreeViewLink));
            Leak.LeakData := FastMMReport.LeakGroupItems[J][K];
          end;
        end;
      end;
      for J := 0 to FastMMReport.VMOnFreedObjectCount - 1 do
      begin
        FreedObject := TFreedObjectTreeViewLink(ReportLink.Add(Format('Freed Object %d', [J + 1]), TFreedObjectTreeViewLink));
        FreedObject.FreedObjectData := FastMMReport.VMOnFreedObjectItems[J];
      end;
    end;
    StackTraceViewerStackServices.ShowTree(FRootLinkIntf);
  end;
end;

initialization

finalization
  if Assigned(StackTraceViewerStackServices) then
  begin
    StackTraceViewerStackServices.UnregisterFrameClass(TfrmLeak);
    StackTraceViewerStackServices.UnregisterFrameClass(TfrmLeakGroup);
    StackTraceViewerStackServices.UnregisterFrameClass(TfrmLeakSummary);
    StackTraceViewerStackServices.UnregisterFrameClass(TfrmFreedObject);
    StackTraceViewerStackServices.ShowTree(nil);
  end;

end.
