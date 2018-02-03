unit FastMMLeakGroupFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, ExtCtrls, JclStackTraceViewerAPI, FastMMParser;

type
  TfrmLeakGroup = class(TFrame, IJclStackTraceViewerPreparableStackFrame, IJclStackTraceViewerStackSelection)
    pnlTop: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    lbLeakCount: TLabel;
    lbLeakSize: TLabel;
  private
    FLeakGroupData: TFastMMLeakGroup;
    FStackFrame: TCustomFrame;
    function GetSelected: IJclLocationInfo;
    function GetPreparableLocationInfoListCount: Integer;
    function GetPreparableLocationInfoList(AIndex: Integer): IJclPreparedLocationInfoList;
    procedure UpdateViews;
    procedure SetLeakGroupData(const Value: TFastMMLeakGroup);
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property LeakGroupData: TFastMMLeakGroup write SetLeakGroupData;
  end;

implementation

{$R *.dfm}

{ TfrmLeakGroup }

constructor TfrmLeakGroup.Create(AOwner: TComponent);
var
  StackFrameClass: TCustomFrameClass;
begin
  inherited Create(AOwner);
  FLeakGroupData := nil;
  if Assigned(StackTraceViewerStackServices) then
  begin
    StackFrameClass := StackTraceViewerStackServices.GetDefaultFrameClass(dfStack);
    if Assigned(StackFrameClass) then
    begin
      FStackFrame := StackFrameClass.Create(Self);
      FStackFrame.Parent := Self;
      FStackFrame.Align := alClient;
    end;
  end;
end;

destructor TfrmLeakGroup.Destroy;
begin
  FStackFrame.Free;
  inherited Destroy;
end;

function TfrmLeakGroup.GetPreparableLocationInfoList(AIndex: Integer): IJclPreparedLocationInfoList;
begin
  Result := FLeakGroupData[0].Stack;
end;

function TfrmLeakGroup.GetPreparableLocationInfoListCount: Integer;
var
  Dummy: IJclPreparedLocationInfoList;
begin
  if Assigned(FLeakGroupData) and (FLeakGroupData.Count > 0) and
    (FLeakGroupData[0].Stack.QueryInterface(IJclPreparedLocationInfoList, Dummy) = S_OK) then
    Result := 1
  else
    Result := 0;
end;

function TfrmLeakGroup.GetSelected: IJclLocationInfo;
var
  StackTraceViewerStackSelection: IJclStackTraceViewerStackSelection;
begin
  if FStackFrame.Visible and
    (FStackFrame.GetInterface(IJclStackTraceViewerStackSelection, StackTraceViewerStackSelection)) and
    Assigned(StackTraceViewerStackSelection.Selected) then
    Result := StackTraceViewerStackSelection.Selected
  else
    Result := nil;
end;

procedure TfrmLeakGroup.SetLeakGroupData(const Value: TFastMMLeakGroup);
var
  StackTraceViewerStackFrame: IJclStackTraceViewerStackFrame;
begin
  FLeakGroupData := Value;
  pnlTop.Visible := Assigned(FLeakGroupData);
  if Assigned(FLeakGroupData) then
  begin
    lbLeakCount.Caption := IntToStr(FLeakGroupData.Count);
    lbLeakSize.Caption := IntToStr(FLeakGroupData.LeakSize);
  end;
  if Assigned(FStackFrame) then
  begin
    FStackFrame.Visible := Assigned(FLeakGroupData) and (FLeakGroupData.Count > 0) and (FLeakGroupData[0].Stack.Count > 0);
    if FStackFrame.Visible and (FStackFrame.GetInterface(IJclStackTraceViewerStackFrame, StackTraceViewerStackFrame)) then
      StackTraceViewerStackFrame.SetStackList(FLeakGroupData[0].Stack);
  end;
end;

procedure TfrmLeakGroup.UpdateViews;
var
  StackTraceViewerPreparableStackFrame: IJclStackTraceViewerPreparableStackFrame;
begin
  if FStackFrame.Visible and
    (FStackFrame.GetInterface(IJclStackTraceViewerPreparableStackFrame, StackTraceViewerPreparableStackFrame)) then
    StackTraceViewerPreparableStackFrame.UpdateViews;
end;

end.
