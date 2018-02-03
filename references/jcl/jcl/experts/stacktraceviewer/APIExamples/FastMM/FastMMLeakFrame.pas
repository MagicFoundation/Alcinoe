unit FastMMLeakFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, ExtCtrls, Grids, ComCtrls, JclStackTraceViewerAPI, FastMMParser,
  FastMMMemoryFrame, FastMMMemoryVisualizerFrame;

type
  TfrmLeak = class(TFrame, IJclStackTraceViewerPreparableStackFrame, IJclStackTraceViewerStackSelection)
    pnlTop: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lbTimestamp: TLabel;
    lbSize: TLabel;
    lbThread: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lbClass: TLabel;
    lbAllocationNumber: TLabel;
    pg: TPageControl;
    tsMemory: TTabSheet;
    tsStack: TTabSheet;
    tsMemoryVisualized: TTabSheet;
  private
    { Private-Deklarationen }
    FLeakData: TFastMMLeak;
    FStackFrame: TCustomFrame;
    FMemoryFrame: TfrmMemory;
    FMemoryVisualizerFrame: TfrmMemoryVisualizer;
    function GetSelected: IJclLocationInfo;
    function GetPreparableLocationInfoListCount: Integer;
    function GetPreparableLocationInfoList(AIndex: Integer): IJclPreparedLocationInfoList;
    procedure UpdateViews;
    procedure SetLeakData(const Value: TFastMMLeak);
  public
    { Public-Deklarationen }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property LeakData: TFastMMLeak write SetLeakData;
  end;

implementation

{$R *.dfm}

{ TfrmLeak }

constructor TfrmLeak.Create(AOwner: TComponent);
var
  StackFrameClass: TCustomFrameClass;
begin
  inherited Create(AOwner);
  FLeakData := nil;
  if Assigned(StackTraceViewerStackServices) then
  begin
    StackFrameClass := StackTraceViewerStackServices.GetDefaultFrameClass(dfStack);
    if Assigned(StackFrameClass) then
    begin
      FStackFrame := StackFrameClass.Create(Self);
      FStackFrame.Parent := tsStack;
      FStackFrame.Align := alClient;
    end;
  end;
  FMemoryFrame := TfrmMemory.Create(Self);
  FMemoryFrame.Parent := tsMemory;
  FMemoryFrame.Align := alClient;
  FMemoryVisualizerFrame := TfrmMemoryVisualizer.Create(Self);
  FMemoryVisualizerFrame.Parent := tsMemoryVisualized;
  FMemoryVisualizerFrame.Align := alClient;
end;

destructor TfrmLeak.Destroy;
begin
  FMemoryVisualizerFrame.Free;
  FMemoryFrame.Free;
  FStackFrame.Free;
  inherited Destroy;
end;

function TfrmLeak.GetPreparableLocationInfoList(AIndex: Integer): IJclPreparedLocationInfoList;
begin
  Result := FLeakData.Stack;
end;

function TfrmLeak.GetPreparableLocationInfoListCount: Integer;
var
  Dummy: IJclPreparedLocationInfoList;
begin
  if Assigned(FLeakData) and Assigned(FLeakData.Stack) and
    (FLeakData.Stack.QueryInterface(IJclPreparedLocationInfoList, Dummy) = S_OK) then
    Result := 1
  else
    Result := 0;
end;

function TfrmLeak.GetSelected: IJclLocationInfo;
var
  StackTraceViewerStackSelection: IJclStackTraceViewerStackSelection;
begin
  if pg.Visible and (pg.ActivePage = tsStack) and FStackFrame.Visible and
    (FStackFrame.GetInterface(IJclStackTraceViewerStackSelection, StackTraceViewerStackSelection)) and
    Assigned(StackTraceViewerStackSelection.Selected) then
    Result := StackTraceViewerStackSelection.Selected
  else
    Result := nil;
end;

procedure TfrmLeak.SetLeakData(const Value: TFastMMLeak);
var
  StackTraceViewerStackFrame: IJclStackTraceViewerStackFrame;
begin
  FLeakData := Value;
  pnlTop.Visible := Assigned(FLeakData);
  if Assigned(FLeakData) then
  begin
    lbTimestamp.Caption := FLeakData.DateStr;
    lbSize.Caption := IntToStr(FLeakData.LeakSize);
    lbThread.Caption := Format('%x', [FLeakData.ThreadID]);
    lbClass.Caption := FLeakData.BlockClass;
    lbAllocationNumber.Caption := IntToStr(FLeakData.AllocationNumber);
  end;
  tsStack.TabVisible := Assigned(FStackFrame) and Assigned(FLeakData) and (FLeakData.Stack.Count > 0);
  if tsStack.TabVisible and (FStackFrame.GetInterface(IJclStackTraceViewerStackFrame, StackTraceViewerStackFrame)) then
    StackTraceViewerStackFrame.SetStackList(FLeakData.Stack);

  tsMemory.TabVisible := Assigned(FLeakData) and FLeakData.FoundMemory;
  tsMemoryVisualized.TabVisible := tsMemory.TabVisible and Assigned(FLeakData.Parent) and
    IsVisualizable(FLeakData.BlockClass, FLeakData.Parent.ReportCompilerVersion, @FLeakData.Memory, Length(FLeakData.Memory));
  pg.Visible := tsStack.TabVisible or tsMemory.TabVisible or tsMemoryVisualized.TabVisible;
  if pg.Visible then
    pg.TabIndex := 0;
  if Assigned(FLeakData) and FLeakData.FoundMemory then
  begin
    FMemoryFrame.Address := FLeakData.Address;
    FMemoryFrame.MemoryArray := FLeakData.Memory;
  end;
  if tsMemoryVisualized.TabVisible then
  begin
    FMemoryVisualizerFrame.Memory := @FLeakData.Memory;
    FMemoryVisualizerFrame.MemorySize := Length(FLeakData.Memory);
    FMemoryVisualizerFrame.ReportCompilerVersion := FLeakData.Parent.ReportCompilerVersion;
    FMemoryVisualizerFrame.TypeStr := FLeakData.BlockClass;
    FMemoryVisualizerFrame.Decode;
  end;
end;

procedure TfrmLeak.UpdateViews;
var
  StackTraceViewerPreparableStackFrame: IJclStackTraceViewerPreparableStackFrame;
begin
  if FStackFrame.Visible and
    (FStackFrame.GetInterface(IJclStackTraceViewerPreparableStackFrame, StackTraceViewerPreparableStackFrame)) then
    StackTraceViewerPreparableStackFrame.UpdateViews;
end;

end.
