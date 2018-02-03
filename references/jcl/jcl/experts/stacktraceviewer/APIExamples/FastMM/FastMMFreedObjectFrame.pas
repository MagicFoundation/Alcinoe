unit FastMMFreedObjectFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, JclStackTraceViewerAPI, FastMMParser,
  FastMMMemoryFrame;

type
  TfrmFreedObject = class(TFrame, IJclStackTraceViewerPreparableStackFrame, IJclStackTraceViewerStackSelection)
    pnlTop: TPanel;
    pg: TPageControl;
    tsStack1: TTabSheet;
    tsStack2: TTabSheet;
    tsStack3: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lbVM: TLabel;
    lbVMAddr: TLabel;
    lbFreedObjectClass: TLabel;
    lbAllocationNumber: TLabel;
    tsMemory: TTabSheet;
  private
    FFreedObjectData: TFastMMVMOnFreedObject;
    FStackFrame1: TCustomFrame;
    FStackFrame2: TCustomFrame;
    FStackFrame3: TCustomFrame;
    FStackInterfaceList: TInterfaceList;
    FMemoryFrame: TfrmMemory;
    function GetSelected: IJclLocationInfo;
    function GetPreparableLocationInfoListCount: Integer;
    function GetPreparableLocationInfoList(AIndex: Integer): IJclPreparedLocationInfoList;
    procedure UpdateViews;
    procedure SetFreedObjectData(const Value: TFastMMVMOnFreedObject);
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property FreedObjectData: TFastMMVMOnFreedObject write SetFreedObjectData;
  end;

implementation

{$R *.dfm}

{ TfrmFreedObject }

constructor TfrmFreedObject.Create(AOwner: TComponent);
var
  StackFrameClass: TCustomFrameClass;
begin
  inherited Create(AOwner);
  FFreedObjectData := nil;
  if Assigned(StackTraceViewerStackServices) then
  begin
    StackFrameClass := StackTraceViewerStackServices.GetDefaultFrameClass(dfStack);
    if Assigned(StackFrameClass) then
    begin
      FStackFrame1 := StackFrameClass.Create(Self);
      FStackFrame1.Parent := tsStack1;
      FStackFrame1.Align := alClient;
      FStackFrame1.Name := 'StackFrame1';
      FStackFrame2 := StackFrameClass.Create(Self);
      FStackFrame2.Parent := tsStack2;
      FStackFrame2.Align := alClient;
      FStackFrame2.Name := 'StackFrame2';
      FStackFrame3 := StackFrameClass.Create(Self);
      FStackFrame3.Parent := tsStack3;
      FStackFrame3.Align := alClient;
      FStackFrame3.Name := 'StackFrame3';
    end;
  end;
  FMemoryFrame := TfrmMemory.Create(Self);
  FMemoryFrame.Parent := tsMemory;
  FMemoryFrame.Align := alClient;
  FStackInterfaceList := TInterfaceList.Create;
end;

destructor TfrmFreedObject.Destroy;
begin
  FStackInterfaceList := TInterfaceList.Create;
  FStackFrame1.Free;
  FStackFrame2.Free;
  FStackFrame3.Free;
  FMemoryFrame.Free;
  inherited Destroy;
end;

function TfrmFreedObject.GetPreparableLocationInfoList(AIndex: Integer): IJclPreparedLocationInfoList;
begin
  if FStackInterfaceList[AIndex].QueryInterface(IJclPreparedLocationInfoList, Result) <> S_OK then
    Result := nil;
end;

function TfrmFreedObject.GetPreparableLocationInfoListCount: Integer;
begin
  Result := FStackInterfaceList.Count;
end;

function TfrmFreedObject.GetSelected: IJclLocationInfo;
var
  StackTraceViewerStackSelection: IJclStackTraceViewerStackSelection;
begin
  if Assigned(FFreedObjectData) then
  begin
    if pg.Visible and (pg.ActivePage = tsStack1) and FStackFrame1.Visible and
      (FStackFrame1.GetInterface(IJclStackTraceViewerStackSelection, StackTraceViewerStackSelection)) and
      Assigned(StackTraceViewerStackSelection.Selected) then
      Result := StackTraceViewerStackSelection.Selected
    else
    if pg.Visible and (pg.ActivePage = tsStack2) and FStackFrame2.Visible and
      (FStackFrame2.GetInterface(IJclStackTraceViewerStackSelection, StackTraceViewerStackSelection)) and
      Assigned(StackTraceViewerStackSelection.Selected) then
      Result := StackTraceViewerStackSelection.Selected
    else
    if pg.Visible and (pg.ActivePage = tsStack3) and FStackFrame3.Visible and
      (FStackFrame3.GetInterface(IJclStackTraceViewerStackSelection, StackTraceViewerStackSelection)) and
      Assigned(StackTraceViewerStackSelection.Selected) then
      Result := StackTraceViewerStackSelection.Selected
    else
      Result := nil;
  end
  else
    Result := nil;
end;

procedure TfrmFreedObject.SetFreedObjectData(const Value: TFastMMVMOnFreedObject);
var
  StackTraceViewerStackFrame: IJclStackTraceViewerStackFrame;
  PreparedLocationInfoList: IJclPreparedLocationInfoList;
begin
  FStackInterfaceList.Clear;
  FFreedObjectData := Value;
  pnlTop.Visible := Assigned(FFreedObjectData);
  if Assigned(FFreedObjectData) then
  begin
    lbFreedObjectClass.Caption := FFreedObjectData.ObjectClass;
    lbAllocationNumber.Caption := IntToStr(FFreedObjectData.AllocationNumber);
    lbVM.Caption := FFreedObjectData.VirtualMethod;
    lbVMAddr.Caption := Format(HexDigitFmt, [FFreedObjectData.VirtualMethodAddress]);
  end;
  tsStack1.TabVisible := Assigned(FStackFrame1) and Assigned(FFreedObjectData) and (FFreedObjectData.Stack1.Count > 0);
  tsStack1.Caption := Format('Stack (allocated by thread %x)', [FFreedObjectData.Stack1Thread]);
  if tsStack1.TabVisible and FFreedObjectData.Stack1.GetInterface(IJclPreparedLocationInfoList, PreparedLocationInfoList) then
    FStackInterfaceList.Add(PreparedLocationInfoList);
  if tsStack1.TabVisible and (FStackFrame1.GetInterface(IJclStackTraceViewerStackFrame, StackTraceViewerStackFrame)) then
    StackTraceViewerStackFrame.SetStackList(FFreedObjectData.Stack1);
  tsStack2.TabVisible := Assigned(FStackFrame2) and Assigned(FFreedObjectData) and (FFreedObjectData.Stack2.Count > 0);
  tsStack2.Caption := Format('Stack (freed by thread %x)', [FFreedObjectData.Stack2Thread]);
  if tsStack2.TabVisible and FFreedObjectData.Stack2.GetInterface(IJclPreparedLocationInfoList, PreparedLocationInfoList) then
    FStackInterfaceList.Add(PreparedLocationInfoList);
  if tsStack2.TabVisible and (FStackFrame2.GetInterface(IJclStackTraceViewerStackFrame, StackTraceViewerStackFrame)) then
    StackTraceViewerStackFrame.SetStackList(FFreedObjectData.Stack2);
  tsStack3.TabVisible := Assigned(FStackFrame3) and Assigned(FFreedObjectData) and (FFreedObjectData.Stack3.Count > 0);
  tsStack3.Caption := Format('Stack (current thread %x)', [FFreedObjectData.Stack3Thread]);
  if tsStack3.TabVisible and FFreedObjectData.Stack3.GetInterface(IJclPreparedLocationInfoList, PreparedLocationInfoList) then
    FStackInterfaceList.Add(PreparedLocationInfoList);
  if tsStack3.TabVisible and (FStackFrame3.GetInterface(IJclStackTraceViewerStackFrame, StackTraceViewerStackFrame)) then
    StackTraceViewerStackFrame.SetStackList(FFreedObjectData.Stack3);
  tsMemory.TabVisible := Assigned(FFreedObjectData) and FFreedObjectData.FoundMemory;
  if Assigned(FFreedObjectData) and FFreedObjectData.FoundMemory then
  begin
    FMemoryFrame.Address := FFreedObjectData.Address;
    FMemoryFrame.MemoryArray := FFreedObjectData.Memory;
  end;
  pg.Visible := tsStack1.TabVisible or tsStack2.TabVisible or tsStack3.TabVisible or tsMemory.TabVisible;
  if pg.Visible then
    pg.TabIndex := 0;
end;

procedure TfrmFreedObject.UpdateViews;
var
  StackTraceViewerPreparableStackFrame: IJclStackTraceViewerPreparableStackFrame;
begin
  if tsStack1.TabVisible and
    (FStackFrame1.GetInterface(IJclStackTraceViewerPreparableStackFrame, StackTraceViewerPreparableStackFrame)) then
    StackTraceViewerPreparableStackFrame.UpdateViews;
  if tsStack2.TabVisible and
    (FStackFrame2.GetInterface(IJclStackTraceViewerPreparableStackFrame, StackTraceViewerPreparableStackFrame)) then
    StackTraceViewerPreparableStackFrame.UpdateViews;
  if tsStack3.TabVisible and
    (FStackFrame3.GetInterface(IJclStackTraceViewerPreparableStackFrame, StackTraceViewerPreparableStackFrame)) then
    StackTraceViewerPreparableStackFrame.UpdateViews;
end;

end.
