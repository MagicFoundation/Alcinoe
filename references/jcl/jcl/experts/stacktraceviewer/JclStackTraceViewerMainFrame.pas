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
{ The Original Code is JclStackTraceViewerMainFrame.pas.                                           }
{                                                                                                  }
{ The Initial Developer of the Original Code is Uwe Schuster.                                      }
{ Portions created by Uwe Schuster are Copyright (C) 2009 Uwe Schuster. All rights reserved.       }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Uwe Schuster (uschuster)                                                                       }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclStackTraceViewerMainFrame;

{$I jcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Docktoolform, StdCtrls, ComCtrls, Menus,
  ActnList, ToolWin, ExtCtrls, IniFiles, ToolsAPI,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclDebug, JclDebugSerialization, Contnrs, JclStackTraceViewerStackFrame, JclStackTraceViewerModuleFrame,
  JclStackTraceViewerClasses, JclStackTraceViewerStackCodeUtils, JclStackTraceViewerExceptInfoFrame, JclStackTraceViewerThreadFrame,
  JclStackTraceViewerOptions,
  JclStackTraceViewerAPIImpl, JclOtaUtils
  , JclStrings, JclDebugXMLDeserializer, JclStackTraceViewerStackUtils, JclStackTraceViewerAPI
  ;

type
  TfrmMain = class(TFrame, IJclStackTraceViewerStackServices)
    ActionList1: TActionList;
    acJumpToCodeLine: TAction;
    acLoadStack: TAction;
    OpenDialog1: TOpenDialog;
    tv: TTreeView;
    acOptions: TAction;
    acUpdateLocalInfo: TAction;
    Splitter2: TSplitter;
    StatusBar: TStatusBar;
    PB: TProgressBar;
    procedure acJumpToCodeLineExecute(Sender: TObject);
    procedure acLoadStackExecute(Sender: TObject);
    procedure tvChange(Sender: TObject; Node: TTreeNode);
    procedure acOptionsExecute(Sender: TObject);
    procedure acUpdateLocalInfoExecute(Sender: TObject);
    procedure acJumpToCodeLineUpdate(Sender: TObject);
    procedure acUpdateLocalInfoUpdate(Sender: TObject);
    procedure FrameResize(Sender: TObject);
  private
    { Private declarations }
    FTreeViewLinkList: TObjectList;
    FRootLink: IJclStackTraceViewerTreeViewLink;
    FThreadInfoList: TJclStackTraceViewerThreadInfoList;
    FExceptionInfo: TJclStackTraceViewerExceptionInfo;
    FStackFrame: TfrmStack;
    FModuleFrame: TfrmModule;
    FExceptionFrame: TfrmException;
    FThreadFrame: TfrmThread;
    FFrameList: TList;
    FLastControl: TControl;
    FLocationInfoProcessor: TJclLocationInfoProcessor;
    FOptions: TExceptionViewerOption;
    FRootDir: string;
    procedure AddItemsToTree(ANode: TTreeNode; ALink: IJclStackTraceViewerTreeViewLink);
    procedure DoProgress(AStatus: TLocationInfoProcessorProgressStatus; APos, AMax: Integer; const AText: string);
    procedure SetOptions(const Value: TExceptionViewerOption);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadWindowState(ADesktop: TCustomIniFile);
    procedure SaveWindowState(ADesktop: TCustomIniFile; AIsProject: Boolean);
    property Options: TExceptionViewerOption read FOptions write SetOptions;
    property RootDir: string read FRootDir write FRootDir;
    { IJclStackTraceViewerStackServices }
    function GetDefaultFrameClass(const AFrameClassID: Integer): TCustomFrameClass;
    procedure ShowTree(ARootLink: IJclStackTraceViewerTreeViewLink);
    procedure UnregisterFrameClass(AFrameClass: TCustomFrameClass);
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\experts\stacktraceviewer';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF BDS8_UP}
  JclOtaAddinOptions,
  {$ENDIF BDS8_UP}
  JclOtaConsts, JclOtaResources,
  JclStackTraceViewerImpl;

{$R *.dfm}

type
  TCustomTreeViewLink = class(TInterfacedObject, IInterface, IJclStackTraceViewerTreeViewLink)
  private
    function GetInternalItems(AIndex: Integer): TCustomTreeViewLink;
  protected
    FData: TObject;
    FItems: TObjectList;
    FOwnsData: Boolean;
    FParent: TCustomTreeViewLink;
  public
    constructor Create(AParent: TCustomTreeViewLink);
    destructor Destroy; override;
    procedure Show(AFrame: TCustomFrame);
    property Data: TObject read FData write FData;
    property OwnsData: Boolean read FOwnsData write FOwnsData;
    property Parent: TCustomTreeViewLink read FParent;
    { IInterface }
    // function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    { IJclStackTraceViewerTreeViewLink }
    procedure DoShow(AFrame: TCustomFrame); virtual;
    function GetCount: Integer;
    function GetFrameClass: TCustomFrameClass; virtual;
    function GetItems(AIndex: Integer): IJclStackTraceViewerTreeViewLink;
    function GetText: string; virtual;
    property Count: Integer read GetCount;
    property FrameClass: TCustomFrameClass read GetFrameClass;
    property Items[AIndex: Integer]: TCustomTreeViewLink read GetInternalItems; default;
    property Text: string read GetText;
  end;

  TThreadData = class(TObject)
  private
    FException: TJclSerializableException;
    FModuleList: TJclStackTraceViewerModuleInfoList;
    FThreadInfo: TJclStackTraceViewerThreadInfo;
  public
    property Exception: TJclSerializableException read FException write FException;
    property ModuleList: TJclStackTraceViewerModuleInfoList read FModuleList write FModuleList;
    property ThreadInfo: TJclStackTraceViewerThreadInfo read FThreadInfo write FThreadInfo;
  end;

  TStackData = class(TObject)
  private
    FModuleList: TJclStackTraceViewerModuleInfoList;
    FStack: TJclStackTraceViewerLocationInfoList;
  public
    property ModuleList: TJclStackTraceViewerModuleInfoList read FModuleList write FModuleList;
    property Stack: TJclStackTraceViewerLocationInfoList read FStack write FStack;
  end;

  TTreeViewLinkKind = (tvlkException, tvlkModuleList, tvlkThread, tvlkThreadStack, tvlkThreadCreationStack, tvlkRoot);

  TDefaultTreeViewLink = class(TCustomTreeViewLink)
  private
    FKind: TTreeViewLinkKind;
  protected
    procedure DoShow(AFrame: TCustomFrame); override;
    function GetFrameClass: TCustomFrameClass; override;
    function GetText: string; override;
  public
    constructor Create(AParent: TCustomTreeViewLink; AKind: TTreeViewLinkKind);
    function Add(AKind: TTreeViewLinkKind): TDefaultTreeViewLink;
    property Kind: TTreeViewLinkKind read FKind;
  end;

  TRootTreeViewLink = class(TDefaultTreeViewLink)
    constructor Create;
  end;

constructor TCustomTreeViewLink.Create(AParent: TCustomTreeViewLink);
begin
  inherited Create;
  FData := nil;
  FItems := TObjectList.Create;
  FOwnsData := False;
  FParent := AParent;
end;

destructor TCustomTreeViewLink.Destroy;
begin
  if FOwnsData then
    FData.Free;
  FItems.Free;
  inherited Destroy;
end;

procedure TCustomTreeViewLink.DoShow(AFrame: TCustomFrame);
begin
//
end;

function TCustomTreeViewLink.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TCustomTreeViewLink.GetFrameClass: TCustomFrameClass;
begin
  Result := nil;
end;

function TCustomTreeViewLink.GetInternalItems(AIndex: Integer): TCustomTreeViewLink;
begin
  Result := TCustomTreeViewLink(FItems[AIndex]);
end;

function TCustomTreeViewLink.GetItems(AIndex: Integer): IJclStackTraceViewerTreeViewLink;
begin
  Result := Items[AIndex];
end;

function TCustomTreeViewLink.GetText: string;
begin
  Result := '';
end;

procedure TCustomTreeViewLink.Show(AFrame: TCustomFrame);
begin
  DoShow(AFrame);
end;

function TCustomTreeViewLink._AddRef: Integer;
begin
  Result := -1;
end;

function TCustomTreeViewLink._Release: Integer;
begin
  Result := -1;
end;

constructor TDefaultTreeViewLink.Create(AParent: TCustomTreeViewLink; AKind: TTreeViewLinkKind);
begin
  inherited Create(AParent);
  FKind := AKind;
end;

function TDefaultTreeViewLink.Add(AKind: TTreeViewLinkKind): TDefaultTreeViewLink;
begin
  FItems.Add(TDefaultTreeViewLink.Create(Self, AKind));
  Result := TDefaultTreeViewLink(FItems.Last);
end;

procedure TDefaultTreeViewLink.DoShow(AFrame: TCustomFrame);
var
  StackData: TStackData;
  ThreadFrame: TfrmThread;
  ThreadInfo: TJclStackTraceViewerThreadInfo;
  ThreadData: TThreadData;
begin
  case FKind of
    tvlkModuleList: if AFrame is TfrmModule then
                      TfrmModule(AFrame).ModuleList := TJclSerializableModuleInfoList(Data);
    tvlkThread: if AFrame is TfrmThread then
                begin
                  ThreadData := TThreadData(Data);
                  ThreadFrame := TfrmThread(AFrame);
                  ThreadInfo := ThreadData.ThreadInfo;
                  StackTraceViewerStackProcessorServices.ModuleList := ThreadData.ModuleList;
                  StackTraceViewerStackProcessorServices.PrepareLocationInfoList(ThreadInfo.CreationStack, False);
                  if tioCreationStack in ThreadInfo.Values then
                    ThreadFrame.CreationStackList := ThreadInfo.CreationStack
                  else
                    ThreadFrame.CreationStackList := nil;
                  ThreadFrame.Exception := ThreadData.Exception;
                  StackTraceViewerStackProcessorServices.PrepareLocationInfoList(ThreadInfo.Stack, False);
                  if tioStack in ThreadInfo.Values then
                    ThreadFrame.StackList := ThreadInfo.Stack
                  else
                    ThreadFrame.StackList := nil;
                end;
    tvlkException: if AFrame is TfrmException then
                     TfrmException(AFrame).Exception := TJclSerializableException(Data);
    tvlkThreadStack, tvlkThreadCreationStack: if AFrame is TfrmStack then
                                              begin
                                                StackData := TStackData(Data);
                                                StackTraceViewerStackProcessorServices.ModuleList := StackData.ModuleList;
                                                StackTraceViewerStackProcessorServices.PrepareLocationInfoList(StackData.Stack, False);
                                                TfrmStack(AFrame).StackList := StackData.Stack;
                                              end;
  end;
end;

function TDefaultTreeViewLink.GetFrameClass: TCustomFrameClass;
begin
  case FKind of
    tvlkModuleList: Result := TfrmModule;
    tvlkThread: Result := TfrmThread;
    tvlkException: Result := TfrmException;
    tvlkThreadStack, tvlkThreadCreationStack: Result := TfrmStack;
    else
      Result := nil;
  end;
end;

function TDefaultTreeViewLink.GetText: string;
begin
  Result := '';
  case FKind of
    tvlkModuleList: if Data is TJclStackTraceViewerModuleInfoList then
                      Result := Format('Module List [%d]', [TJclStackTraceViewerModuleInfoList(Data).GetModuleCount]);
    tvlkThread: if Data is TThreadData then
                begin
                  if tioIsMainThread in TThreadData(Data).ThreadInfo.Values then
                    Result := '[MainThread]'
                  else
                    Result := '';
                  Result := Format('ID: %d %s', [TThreadData(Data).ThreadInfo.ThreadID, Result]);
                end;
    tvlkException: Result := 'Exception';
    tvlkThreadStack: if Data is TStackData then
                       Result := Format('Stack [%d]', [TStackData(Data).Stack.Count]);
    tvlkThreadCreationStack: if Data is TStackData then
                               Result := Format('CreationStack [%d]', [TStackData(Data).Stack.Count]);
  end;
end;

constructor TRootTreeViewLink.Create;
begin
  inherited Create(nil, tvlkRoot);
end;

procedure TfrmMain.LoadWindowState(ADesktop: TCustomIniFile);
begin
  if Assigned(ADesktop) then
  begin
    FStackFrame.LoadState(ADesktop, JclStackTraceViewerDesktopIniSection, 'StackFrameSingle');
    FModuleFrame.LoadState(ADesktop, JclStackTraceViewerDesktopIniSection);
    FThreadFrame.LoadState(ADesktop, JclStackTraceViewerDesktopIniSection);
  end;
end;

procedure TfrmMain.SaveWindowState(ADesktop: TCustomIniFile; AIsProject: Boolean);
begin
  if Assigned(ADesktop) then
  begin
    FStackFrame.SaveState(ADesktop, JclStackTraceViewerDesktopIniSection, 'StackFrameSingle');
    FModuleFrame.SaveState(ADesktop, JclStackTraceViewerDesktopIniSection);
    FThreadFrame.SaveState(ADesktop, JclStackTraceViewerDesktopIniSection);
  end;
end;

procedure TfrmMain.SetOptions(const Value: TExceptionViewerOption);
var
  OldOptions: TExceptionViewerOption;
begin
  OldOptions := TExceptionViewerOption.Create;
  try
    OldOptions.Assign(FOptions);
    FOptions.Assign(Value);
    if FOptions.ModuleVersionAsRevision <> OldOptions.ModuleVersionAsRevision then
    begin
      { TODO -oUSc : Update stack views }
    end;
  finally
    OldOptions.Free;
  end;
end;

procedure TfrmMain.ShowTree(ARootLink: IJclStackTraceViewerTreeViewLink);
begin
  FRootLink := ARootLink;
  FStackFrame.StackList := nil;
  FThreadFrame.CreationStackList := nil;
  FThreadFrame.StackList := nil;
  tv.Selected := nil;
  tv.Items.Clear;
  if Assigned(FLastControl) then
  begin
    FLastControl.Hide;
    FLastControl := nil;
  end;
  if Assigned(ARootLink) then
    AddItemsToTree(nil, ARootLink);
end;

procedure TfrmMain.tvChange(Sender: TObject; Node: TTreeNode);
var
  TreeViewLink: IJclStackTraceViewerTreeViewLink;
  NewControl: TControl;
  I: Integer;
  Frame: TCustomFrame;
begin
  inherited;
  NewControl := nil;
  if Assigned(tv.Selected) and Assigned(tv.Selected.Data) and
    (IUnknown(tv.Selected.Data).QueryInterface(IJclStackTraceViewerTreeViewLink, TreeViewLink) = S_OK) then
  begin
    if Assigned(TreeViewLink.FrameClass) then
    begin
      for I := 0 to FFrameList.Count - 1 do
        if TObject(FFrameList[I]).ClassType = TreeViewLink.FrameClass then
        begin
          NewControl := TControl(FFrameList[I]);
          Break;
        end;
      if not Assigned(NewControl) then
      begin
        FFrameList.Add(TreeViewLink.FrameClass.Create(Self));
        Frame := TCustomFrame(FFrameList.Last);
        Frame.Parent := Self;
        Frame.Align := alClient;
        Frame.Visible := False;
        NewControl := Frame;
      end;
    end;
    if Assigned(NewControl) and (NewControl is TCustomFrame) then
      TreeViewLink.DoShow(TCustomFrame(NewControl));
  end;
  if Assigned(NewControl) then
    NewControl.Show;
  if Assigned(FLastControl) and (FLastControl <> NewControl) then
    FLastControl.Hide;
  if FLastControl <> NewControl then
    FLastControl := NewControl;
end;

procedure TfrmMain.UnregisterFrameClass(AFrameClass: TCustomFrameClass);
var
  I, Idx: Integer;
  Frame: TCustomFrame;
begin
  Idx := -1;
  for I := 0 to FFrameList.Count - 1 do
    if TObject(FFrameList[I]).ClassType = AFrameClass then
    begin
      Idx := I;
      Break;
    end;
  if Idx <> -1 then
  begin
    Frame := TCustomFrame(FFrameList[Idx]);
    Frame.Free;
    FFrameList.Delete(Idx);
  end;
end;

procedure TfrmMain.acJumpToCodeLineExecute(Sender: TObject);
var
  StackTraceViewerStackSelection: IJclStackTraceViewerStackSelection;
begin
  if Assigned(FLastControl) and
    (FLastControl.GetInterface(IJclStackTraceViewerStackSelection, StackTraceViewerStackSelection)) then
    JumpToCode(StackTraceViewerStackSelection.Selected);
end;

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  acJumpToCodeLine.Caption := LoadResString(@RsJumpToCodeLine);
  acLoadStack.Caption := LoadResString(@RsLoadStack);
  acOptions.Caption := LoadResString(@RsOptions);
  acUpdateLocalInfo.Caption := LoadResString(@RsUpdateLocalInfo);

  FExceptionInfo := TJclStackTraceViewerExceptionInfo.Create;
  FThreadInfoList := FExceptionInfo.ThreadInfoList;
  FTreeViewLinkList := TObjectList.Create;
  FRootLink := nil;
  FFrameList := TList.Create;
  FStackFrame := TfrmStack.Create(Self);
  FFrameList.Add(FStackFrame);
  FStackFrame.Name := 'StackFrameSingle';
  FStackFrame.Parent := Self;
  FStackFrame.Align := alClient;
  FStackFrame.Visible := False;

  FModuleFrame := TfrmModule.Create(Self);
  FFrameList.Add(FModuleFrame);
  FModuleFrame.Parent := Self;
  FModuleFrame.Align := alClient;
  FModuleFrame.Visible := False;

  FExceptionFrame := TfrmException.Create(Self);
  FFrameList.Add(FExceptionFrame);
  FExceptionFrame.Name := 'ExceptionFrameSingle';
  FExceptionFrame.Parent := Self;
  FExceptionFrame.Align := alClient;
  FExceptionFrame.Visible := False;

  FThreadFrame := TfrmThread.Create(Self);
  FFrameList.Add(FThreadFrame);
  FThreadFrame.Parent := Self;
  FThreadFrame.Align := alClient;
  FThreadFrame.Visible := False;

  PB.Parent := StatusBar;
  PB.SetBounds(StatusBar.Panels[0].Width + 2, 3, 96, 14);

  FOptions := TExceptionViewerOption.Create;
  if Assigned(StackTraceViewerExpert) then
  begin
    Options := StackTraceViewerExpert.Options;
    RootDir := StackTraceViewerExpert.RootDir;
  end;

  FLocationInfoProcessor := TJclLocationInfoProcessor.Create;
  FLocationInfoProcessor.OnProgress := DoProgress;
  FLocationInfoProcessor.Options := Options;
  FLocationInfoProcessor.RootDir := RootDir;
  StackTraceViewerStackProcessorServices := FLocationInfoProcessor;
  StackTraceViewerStackServices := Self;

  FLastControl := nil;
end;

destructor TfrmMain.Destroy;
begin
  StackTraceViewerStackServices := nil;
  StackTraceViewerStackProcessorServices := nil;
  FStackFrame.StackList := nil;
  FLocationInfoProcessor.Free;
  FOptions.Free;
  FTreeViewLinkList.Free;
  FRootLink := nil;
  FFrameList.Free;
  FExceptionInfo.Free;
  inherited Destroy;
end;

procedure TfrmMain.acJumpToCodeLineUpdate(Sender: TObject);
var
  StackTraceViewerStackSelection: IJclStackTraceViewerStackSelection;
begin
  acJumpToCodeLine.Enabled := Assigned(FLastControl) and
    (FLastControl.GetInterface(IJclStackTraceViewerStackSelection, StackTraceViewerStackSelection)) and
    Assigned(StackTraceViewerStackSelection.Selected);
end;

procedure TfrmMain.acLoadStackExecute(Sender: TObject);
var
  SS: TStringStream;
  {$IFNDEF COMPILER12_UP}
  FS: TFileStream;
  {$ENDIF ~COMPILER12_UP}
  I: Integer;
  ThreadTreeViewLink, TreeViewLink: TDefaultTreeViewLink;
  XMLDeserializer: TJclXMLDeserializer;
  SerializeExceptionInfo: TJclSerializableExceptionInfo;
  RootTreeViewLink: TRootTreeViewLink;
  ThreadData: TThreadData;
  StackData: TStackData;
begin
  inherited;
  if OpenDialog1.Execute then
  begin
    FStackFrame.StackList := nil;
    FThreadFrame.CreationStackList := nil;
    FThreadFrame.StackList := nil;
    FRootLink := nil;
    tv.Selected := nil;
    tv.Items.Clear;
    FTreeViewLinkList.Clear;
    SS := TStringStream.Create('');
    try
      {$IFDEF COMPILER12_UP}
      SS.LoadFromFile(OpenDialog1.FileName);
      {$ELSE ~COMPILER12_UP}
      FS := TFileStream.Create(OpenDialog1.FileName, fmOpenRead or fmShareDenyWrite);
      try
        SS.CopyFrom(FS, 0);
      finally
        FS.Free;
      end;
      {$ENDIF ~COMPILER12_UP}
      SerializeExceptionInfo := TJclSerializableExceptionInfo.Create;
      try
        XMLDeserializer := TJclXMLDeserializer.Create('ExceptInfo');
        try
          XMLDeserializer.LoadFromString(SS.DataString);
          SerializeExceptionInfo.Deserialize(XMLDeserializer);
        finally
          XMLDeserializer.Free;
        end;
        FExceptionInfo.AssignExceptionInfo(SerializeExceptionInfo);
      finally
        SerializeExceptionInfo.Free;
      end;

      RootTreeViewLink := TRootTreeViewLink.Create;

      TreeViewLink := RootTreeViewLink.Add(tvlkModuleList);
      TreeViewLink.Data := FExceptionInfo.Modules;

      if FThreadInfoList.Count > 0 then
      begin
        for I := 0 to FThreadInfoList.Count - 1 do
        begin
          //FTreeViewLinkList.Add(TDefaultTreeViewLink.Create(tvlkThread));//TODO
          ThreadTreeViewLink := RootTreeViewLink.Add(tvlkThread);
          ThreadTreeViewLink.OwnsData := True;
          ThreadTreeViewLink.Data := TThreadData.Create;
          ThreadData := TThreadData(ThreadTreeViewLink.Data);
          ThreadData.Exception := FExceptionInfo.Exception;
          ThreadData.ModuleList := FExceptionInfo.Modules;
          ThreadData.ThreadInfo := FThreadInfoList[I];

          if I = 0 then
          begin
            TreeViewLink := ThreadTreeViewLink.Add(tvlkException);
            TreeViewLink.Data := FExceptionInfo.Exception;
          end;

          if tioStack in FThreadInfoList[I].Values then
          begin
            TreeViewLink := ThreadTreeViewLink.Add(tvlkThreadStack);
            TreeViewLink.OwnsData := True;
            TreeViewLink.Data := TStackData.Create;
            StackData := TStackData(TreeViewLink.Data);
            StackData.ModuleList := FExceptionInfo.Modules;
            StackData.Stack := FThreadInfoList[I].Stack;
          end;

          if tioCreationStack  in FThreadInfoList[I].Values then
          begin
            TreeViewLink := ThreadTreeViewLink.Add(tvlkThreadCreationStack);
            TreeViewLink.OwnsData := True;
            TreeViewLink.Data := TStackData.Create;
            StackData := TStackData(TreeViewLink.Data);
            StackData.ModuleList := FExceptionInfo.Modules;
            StackData.Stack := FThreadInfoList[I].CreationStack;
          end;
        end;
      end;
      FRootLink := RootTreeViewLink;
      AddItemsToTree(nil, RootTreeViewLink);
    finally
      SS.Free;
    end;
  end;
end;

procedure TfrmMain.acOptionsExecute(Sender: TObject);
begin
  inherited;
  {$IFDEF BDS8_UP}
  (BorlandIDEServices as IOTAServices).GetEnvironmentOptions.EditOptions('', StackTraceViewerExpert.GetCaption);
  {$ELSE ~BDS8_UP}
  TJclOTAExpertBase.ConfigurationDialog(LoadResString(@RsStackTraceViewerOptionsPageName));
  {$ENDIF ~BDS8_UP}
end;

procedure TfrmMain.acUpdateLocalInfoExecute(Sender: TObject);
var
  I: Integer;
  PreparableStackFrame: IJclStackTraceViewerPreparableStackFrame;
  PreparedLocationInfoList: IJclPreparedLocationInfoList;
  UpdateView: Boolean;
begin
  inherited;
  if Assigned(StackTraceViewerStackProcessorServices) and Assigned(FLastControl) and
    (FLastControl.GetInterface(IJclStackTraceViewerPreparableStackFrame, PreparableStackFrame)) then
  begin
    UpdateView := False;
    for I := 0 to PreparableStackFrame.PreparableLocationInfoListCount - 1 do
    begin
      PreparedLocationInfoList := PreparableStackFrame.PreparableLocationInfoList[I];
      if Assigned(PreparedLocationInfoList) then
      begin
        StackTraceViewerStackProcessorServices.ModuleList := PreparedLocationInfoList.ModuleInfoList;
        StackTraceViewerStackProcessorServices.PrepareLocationInfoList(PreparedLocationInfoList, True);
        UpdateView := True;
      end;
    end;
    if UpdateView then
      PreparableStackFrame.UpdateViews;
  end;
end;

procedure TfrmMain.acUpdateLocalInfoUpdate(Sender: TObject);
var
  PreparableStackFrame: IJclStackTraceViewerPreparableStackFrame;
begin
  acUpdateLocalInfo.Enabled := Assigned(StackTraceViewerStackProcessorServices) and Assigned(FLastControl) and
    (FLastControl.GetInterface(IJclStackTraceViewerPreparableStackFrame, PreparableStackFrame)) and
    (PreparableStackFrame.PreparableLocationInfoListCount > 0);
end;

procedure TfrmMain.AddItemsToTree(ANode: TTreeNode; ALink: IJclStackTraceViewerTreeViewLink);
var
  I: Integer;
  ChildNode: TTreeNode;
begin
  for I := 0 to ALink.Count - 1 do
  begin
    if ANode = nil then
      ChildNode := tv.Items.Add(nil, ALink[I].Text)
    else
      ChildNode := tv.Items.AddChild(ANode, ALink[I].Text);
    ChildNode.Data := Pointer(ALink[I]);
    if ALink[I].Count > 0 then
      AddItemsToTree(ChildNode, ALink[I]);
  end;
  if FOptions.ExpandTreeView and Assigned(ANode) then
    ANode.Expanded := True;
end;

procedure TfrmMain.DoProgress(AStatus: TLocationInfoProcessorProgressStatus; APos, AMax: Integer; const AText: string);
begin
  if AStatus = lippsStart then
    PB.Visible := True
  else
  if AStatus = lippsFinished then
    PB.Visible := False;
  PB.Max := AMax;
  PB.Position := APos;
  StatusBar.Panels[0].Text := AText;
  StatusBar.Update;
end;

procedure TfrmMain.FrameResize(Sender: TObject);
begin
  StatusBar.Panels[0].Width := Width - 100;
  PB.SetBounds(StatusBar.Panels[0].Width + 2, 3, 96, 14);
end;

function TfrmMain.GetDefaultFrameClass(const AFrameClassID: Integer): TCustomFrameClass;
begin
  case AFrameClassID of
    dfStack: Result := TfrmStack;
    else
      Result := nil;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
