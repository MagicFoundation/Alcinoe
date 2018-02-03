unit StackTraceViewerHelloWorldExampleReg;

interface

procedure Register;

implementation

uses
  Classes, Contnrs, Forms, JclStackTraceViewerAPI, HelloWorldFrame;

type
  TTestLocationInfo = class(TInterfacedObject, IJclLocationInfo, IJclPreparedLocationInfo)
  private
    FSourceName: string;
  public
    destructor Destroy; override;
    function GetAddress: Pointer;
    function GetBinaryFileName: string;
    function GetLineNumber: Integer;
    function GetLineNumberOffsetFromProcedureStart: Integer;
    function GetModuleName: string;
    function GetOffsetFromLineNumber: Integer;
    function GetOffsetFromProcName: Integer;
    function GetProcedureName: string;
    function GetSourceName: string;
    function GetSourceUnitName: string;
    function GetUnitVersionDateTime: TDateTime;
    function GetUnitVersionExtra: string;
    function GetUnitVersionLogPath: string;
    function GetUnitVersionRCSfile: string;
    function GetUnitVersionRevision: string;
    function GetVAddress: Pointer;
    function GetValues: Integer;

    function GetFileName: string;
    function GetFoundFile: Boolean;
    function GetProjectName: string;
    function GetRevision: string;
    function GetTranslatedLineNumber: Integer;
    procedure SetFileName(AValue: string);
    procedure SetFoundFile(AValue: Boolean);
    procedure SetProjectName(AValue: string);
    procedure SetRevision(AValue: string);
    procedure SetTranslatedLineNumber(AValue: Integer);
  end;

  TTestLocationInfoList = class(TInterfacedObject, IJclLocationInfoList)
  private
    FItems: TObjectList;
    FInterfaceList: TInterfaceList;
  public
    constructor Create;
    destructor Destroy; override;
    function Add: TTestLocationInfo;

    function GetCount: Integer;
    function GetLocationItems(AIndex: Integer): IJclLocationInfo;
    function GetPrepared: Boolean;
    procedure SetPrepared(AValue: Boolean);
  end;

  TTestTreeViewLink = class(TInterfacedObject, IJclStackTraceViewerTreeViewLink)
  private
    FFrameClass: TCustomFrameClass;
    FItems: TInterfaceList;
    FText: string;
    FStack: TTestLocationInfoList;
  public
    constructor Create(const AText: string; AFrameClass: TCustomFrameClass);
    destructor Destroy; override;
    function Add(const AText: string; AFrameClass: TCustomFrameClass): TTestTreeViewLink;
    procedure DoShow(AFrame: TCustomFrame); virtual;
    function GetCount: Integer;
    function GetFrameClass: TCustomFrameClass;
    function GetItems(AIndex: Integer): IJclStackTraceViewerTreeViewLink;
    function GetText: string;
  end;

{ TTestTreeViewLink }

constructor TTestTreeViewLink.Create(const AText: string; AFrameClass: TCustomFrameClass);
begin
  inherited Create;
  FFrameClass := AFrameClass;
  FItems := TInterfaceList.Create;
  FStack := TTestLocationInfoList.Create;
  FStack._AddRef;
  FStack.Add.FSourceName := 'SysUtils.pas';
  FStack.Add.FSourceName := 'Classes.pas';
  FText := AText;
end;

destructor TTestTreeViewLink.Destroy;
begin
  FStack._Release;
  //FStack.Free;
  FItems.Free;
  inherited Destroy;
end;

function TTestTreeViewLink.Add(const AText: string; AFrameClass: TCustomFrameClass): TTestTreeViewLink;
begin
  Result := TTestTreeViewLink.Create(AText, AFrameClass);
  FItems.Add(Result);
end;

procedure TTestTreeViewLink.DoShow(AFrame: TCustomFrame);
var
  StackTraceViewerStackFrame: IJclStackTraceViewerStackFrame;
begin
  if (FText = 'Stack') and (AFrame.GetInterface(IJclStackTraceViewerStackFrame, StackTraceViewerStackFrame)) then
  begin
    //StackTraceViewerStackProcessorServices.PrepareLocationInfoList(FStack, False);
    StackTraceViewerStackFrame.SetStackList(FStack);
  end;
end;

function TTestTreeViewLink.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TTestTreeViewLink.GetFrameClass: TCustomFrameClass;
begin
  Result := FFrameClass;
end;

function TTestTreeViewLink.GetItems(AIndex: Integer): IJclStackTraceViewerTreeViewLink;
begin
  if FItems[AIndex].QueryInterface(IJclStackTraceViewerTreeViewLink, Result) <> S_OK then
    Result := nil;
end;

function TTestTreeViewLink.GetText: string;
begin
  Result := FText;
end;

procedure Register;
var
  RootLink, Link: TTestTreeViewLink;
  StackFrameClass: TCustomFrameClass;
begin
  if Assigned(StackTraceViewerStackServices) then
  begin
    RootLink := TTestTreeViewLink.Create('', nil);
    Link := RootLink.Add('Hello', nil);
    Link.Add('World', TfrmHelloWorld);
    StackFrameClass := StackTraceViewerStackServices.GetDefaultFrameClass(dfStack);
    if Assigned(StackFrameClass) then
      Link.Add('Stack', StackFrameClass);
    StackTraceViewerStackServices.ShowTree(RootLink);
  end;
end;

{ TTestLocationInfoList }

function TTestLocationInfoList.Add: TTestLocationInfo;
var
  I: IUnknown;
begin
  FItems.Add(TTestLocationInfo.Create);
  FItems.Last.GetInterface(IUnknown, I);
  FInterfaceList.Add(I);
  Result := TTestLocationInfo(FItems.Last);
end;

constructor TTestLocationInfoList.Create;
begin
  inherited Create;
  FItems := TObjectList.Create(False);
  FInterfaceList := TInterfaceList.Create;
end;

destructor TTestLocationInfoList.Destroy;
begin
  FInterfaceList.Free;
  FItems.Free;
  inherited Destroy;
end;

function TTestLocationInfoList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TTestLocationInfoList.GetLocationItems(AIndex: Integer): IJclLocationInfo;
begin
  if not FItems[AIndex].GetInterface(IJclLocationInfo, Result) then
    Result := nil;
end;

function TTestLocationInfoList.GetPrepared: Boolean;
begin
  Result := False;
end;

procedure TTestLocationInfoList.SetPrepared(AValue: Boolean);
begin
//
end;

{ TTestLocationInfo }

destructor TTestLocationInfo.Destroy;
begin
  inherited Destroy;
end;

function TTestLocationInfo.GetAddress: Pointer;
begin
  Result := nil;
end;

function TTestLocationInfo.GetBinaryFileName: string;
begin
  Result := '';
end;

function TTestLocationInfo.GetLineNumber: Integer;
begin
  Result := 0;
end;

function TTestLocationInfo.GetLineNumberOffsetFromProcedureStart: Integer;
begin
  Result := 0;
end;

function TTestLocationInfo.GetModuleName: string;
begin
  Result := '';
end;

function TTestLocationInfo.GetOffsetFromLineNumber: Integer;
begin
  Result := 0;
end;

function TTestLocationInfo.GetOffsetFromProcName: Integer;
begin
  Result := 0;
end;

function TTestLocationInfo.GetProcedureName: string;
begin
  Result := '';
end;

function TTestLocationInfo.GetSourceName: string;
begin
  Result := FSourceName;
end;

function TTestLocationInfo.GetSourceUnitName: string;
begin
  Result := '';
end;

function TTestLocationInfo.GetUnitVersionDateTime: TDateTime;
begin
  Result := 0;
end;

function TTestLocationInfo.GetUnitVersionExtra: string;
begin
  Result := '';
end;

function TTestLocationInfo.GetUnitVersionLogPath: string;
begin
  Result := '';
end;

function TTestLocationInfo.GetUnitVersionRCSfile: string;
begin
  Result := '';
end;

function TTestLocationInfo.GetUnitVersionRevision: string;
begin
  Result := '';
end;

function TTestLocationInfo.GetVAddress: Pointer;
begin
  Result := nil;
end;

function TTestLocationInfo.GetValues: Integer;
begin
  Result := 0;
end;

function TTestLocationInfo.GetFileName: string;
begin
  Result := '';
end;

function TTestLocationInfo.GetFoundFile: Boolean;
begin
  Result := False;
end;

function TTestLocationInfo.GetProjectName: string;
begin
  Result := '';
end;

function TTestLocationInfo.GetRevision: string;
begin
  Result := '';
end;

function TTestLocationInfo.GetTranslatedLineNumber: Integer;
begin
  Result := 0;
end;

procedure TTestLocationInfo.SetFileName(AValue: string);
begin
//
end;

procedure TTestLocationInfo.SetFoundFile(AValue: Boolean);
begin
//
end;

procedure TTestLocationInfo.SetProjectName(AValue: string);
begin
//
end;

procedure TTestLocationInfo.SetRevision(AValue: string);
begin
//
end;

procedure TTestLocationInfo.SetTranslatedLineNumber(AValue: Integer);
begin
//
end;

initialization

finalization
  if Assigned(StackTraceViewerStackServices) then
  begin
    StackTraceViewerStackServices.UnregisterFrameClass(TfrmHelloWorld);
    StackTraceViewerStackServices.ShowTree(nil);
  end;

end.
