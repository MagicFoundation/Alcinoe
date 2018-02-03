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
{ The Original Code is UnitVersioningTestMain.pas.                                                 }
{                                                                                                  }
{ The Initial Developer of the Original Code is Uwe Schuster.                                      }
{ Portions created by Uwe Schuster are Copyright (C) Uwe Schuster. All rights reserved.            }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Uwe Schuster (uschuster)                                                                       }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ sample for TUnitVersioning                                                                       }
{                                                                                                  }
{ Unit owner: Uwe Schuster                                                                         }
{                                                                                                  }
{**************************************************************************************************}

// Last modified: $Date$

unit UnitVersioningTestMain;

{$I jcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, JclUnitVersioning, JclUnitVersioningProviders,
  JclDebug, JclFileUtils;

type
  TfrmUnitVersioningTestMain = class(TForm)
    tv: TTreeView;
    Panel1: TPanel;
    btnTestDummyProvider: TButton;
    btnTestGetLocationInfoStr: TButton;
    btnShowUVContent: TButton;
    btnTestFindMethods: TButton;
    btnLoadDLL: TButton;
    btnInsertSection: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnTestFindMethodsClick(Sender: TObject);
    procedure btnTestDummyProviderClick(Sender: TObject);
    procedure btnTestGetLocationInfoStrClick(Sender: TObject);
    procedure btnShowUVContentClick(Sender: TObject);
    procedure btnLoadDLLClick(Sender: TObject);
    procedure btnInsertSectionClick(Sender: TObject);
  private
    { Private declarations }
    FFindMethodsInfoPtrs: TList;
    TestDLLHandle: THandle;
    procedure FreeTestDLL;
  public
    { Public declarations }
  end;

var
  frmUnitVersioningTestMain: TfrmUnitVersioningTestMain;

implementation

{$R *.dfm}

const
  TestDLLFileName = 'UnitVersioningTestDLL.dll';

procedure TfrmUnitVersioningTestMain.FormCreate(Sender: TObject);
begin
  FFindMethodsInfoPtrs := TList.Create;
end;

procedure TfrmUnitVersioningTestMain.FormDestroy(Sender: TObject);
var
  I: Integer;
  UnitVersionInfoPtr: PUnitVersionInfo;
begin
  for I := 0 to FFindMethodsInfoPtrs.Count - 1 do
  begin
    UnitVersionInfoPtr := PUnitVersionInfo(FFindMethodsInfoPtrs[I]);
    StrDispose(UnitVersionInfoPtr^.RCSfile);
    StrDispose(UnitVersionInfoPtr^.Revision);
    StrDispose(UnitVersionInfoPtr^.Date);
    StrDispose(UnitVersionInfoPtr^.LogPath);
    StrDispose(UnitVersionInfoPtr^.Extra);
    Dispose(UnitVersionInfoPtr);
  end;
  FFindMethodsInfoPtrs.Free;
  FreeTestDLL;
end;

procedure TfrmUnitVersioningTestMain.FreeTestDLL;
begin
  if TestDLLHandle <> 0 then
  begin
    if FreeLibrary(TestDLLHandle) then
      TestDLLHandle := 0;
  end;
end;

procedure TfrmUnitVersioningTestMain.btnTestFindMethodsClick(Sender: TObject);
const MaxCnt = 1000;
var
  UnitVersioning: TUnitVersioning;
  I, Idx: Integer;
  UnitVersionInfoPtr: PUnitVersionInfo;
begin
  UnitVersioning := GetUnitVersioning;
  for I := 1 to MaxCnt do
  begin
    New(UnitVersionInfoPtr);
    with UnitVersionInfoPtr^ do
    begin
      RCSfile := StrNew(PChar(Format('unit%d.pas', [I])));
      Revision := StrNew('');
      Date := StrNew('');
      LogPath := StrNew('');
      Extra := StrNew('');
      Data := nil;
    end;
    FFindMethodsInfoPtrs.Add(UnitVersionInfoPtr);
    RegisterUnitVersion(HInstance, UnitVersionInfoPtr^);
  end;
  if MaxCnt >= 500 then
  begin
    Idx := UnitVersioning.IndexOf('unit500.pas');
    if Idx <> -1 then
      ShowMessage(Format('IndexOf %s = %d', [UnitVersioning.Items[Idx].RCSfile, Idx]))
    else
      ShowMessage('IndexOf failed');
  end;
  if MaxCnt >= 600 then
  begin
    if Assigned(UnitVersioning.FindUnit('unit600.pas')) then
      ShowMessage('FindUnit ' + UnitVersioning.FindUnit('unit600.pas').RCSfile)
    else
      ShowMessage('FindUnit failed');
  end;
end;

type
  TDummyUnitVersioningProvider = class(TCustomUnitVersioningProvider)
  private
    FUV: PUnitVersionInfo;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure LoadModuleUnitVersioningInfo(Instance: THandle); override;
  end;

constructor TDummyUnitVersioningProvider.Create;
begin
  inherited Create;
  FUV := nil;
end;

destructor TDummyUnitVersioningProvider.Destroy;
begin
  if Assigned(FUV) then
    Dispose(FUV);
  inherited Destroy;
end;

procedure TDummyUnitVersioningProvider.LoadModuleUnitVersioningInfo(Instance: THandle);
begin
  if (Instance = HInstance) and not Assigned(FUV) then
  begin
    New(FUV);
    FUV^.RCSfile := 'DummyUnit.pas';
    FUV^.Revision := '0.12';
    FUV^.Date := '';
    FUV^.LogPath := '';
    FUV^.Extra := '';
    FUV^.Data := nil;
    RegisterUnitVersion(Instance, FUV^);
  end;
end;

procedure TfrmUnitVersioningTestMain.btnTestDummyProviderClick(Sender: TObject);
var
  UnitVersioning: TUnitVersioning;
  Idx: Integer;
begin
  UnitVersioning := GetUnitVersioning;
  UnitVersioning.RegisterProvider(TDummyUnitVersioningProvider);
  UnitVersioning.LoadModuleUnitVersioningInfo(HInstance);
  Idx := UnitVersioning.IndexOf('DummyUnit.pas');
  if Idx <> -1 then
    ShowMessage(Format('IndexOf %s=%d Revision=%s', [UnitVersioning.Items[Idx].RCSfile,
      Idx, UnitVersioning.Items[Idx].Revision]))
  else
    ShowMessage('DummyProvider Test failed');
end;

procedure TfrmUnitVersioningTestMain.btnTestGetLocationInfoStrClick(Sender: TObject);
var
  S: string;
begin
  S := GetLocationInfoStr(@TUnitVersioning.LoadModuleUnitVersioningInfo,
    False, True, True, False);
  ShowMessage(S);
end;

procedure TfrmUnitVersioningTestMain.btnShowUVContentClick(Sender: TObject);
var
  I, J: Integer;
  UnitVersioning: TUnitVersioning;
  tnModule: TTreeNode;
  LongFileName: string;
begin
  UnitVersioning := GetUnitVersioning;
  UnitVersioning.RegisterProvider(TJclDefaultUnitVersioningProvider);
  for I := 0 to Pred(UnitVersioning.ModuleCount) do
    UnitVersioning.LoadModuleUnitVersioningInfo(UnitVersioning.Modules[I].Instance);
  tv.Items.BeginUpdate;
  try
    tv.Items.Clear;
    for I := 0 to Pred(UnitVersioning.ModuleCount) do
    begin
      tnModule := tv.Items.Add(nil, Format('%s [%d units]',
        [GetModulePath(UnitVersioning.Modules[I].Instance), UnitVersioning.Modules[I].Count]));
      for J := 0 to Pred(UnitVersioning.Modules[I].Count) do
        with UnitVersioning.Modules[I][J] do
        begin
          LongFileName := LogPath;
          if LongFileName <> '' then
            LongFileName := PathAddSeparator(LongFileName);
          LongFileName := LongFileName + RCSfile;
          tv.Items.AddChild(tnModule, Format('%s  %s  %s', [LongFileName, Revision, Date]));
        end;
    end;
  finally
    tv.Items.EndUpdate;
  end;
end;

procedure TfrmUnitVersioningTestMain.btnLoadDLLClick(Sender: TObject);
begin
  if TestDLLHandle = 0 then
  begin
    TestDLLHandle := LoadLibrary(TestDLLFileName);
    if TestDLLHandle = 0 then
      ShowMessage(Format('Could not load %s', [TestDLLFileName]));
  end;
end;

procedure TfrmUnitVersioningTestMain.btnInsertSectionClick(Sender: TObject);
var
  TestStream: TMemoryStream;
  UnitList: TJclUnitVersioningList;
  UnitVersionInfo: TUnitVersionInfo;
  I: Integer;
begin
  FreeTestDLL;
  if TestDLLHandle = 0 then
  begin
    TestStream := TMemoryStream.Create;
    try
      UnitList := TJclUnitVersioningList.Create;
      try
        for I := 1 to 20 do
        begin
          with UnitVersionInfo do
          begin
            RCSfile := PChar(Format('unit%d.pas', [I]));
            Revision := PChar(Format('0.%d', [I]));
            Date := '';
            LogPath := '';
            Extra := '';
            Data := nil;
          end;
          UnitList.Add(UnitVersionInfo);
        end;
        if not InsertUnitVersioningSection(TestDLLFileName, UnitList) then
          ShowMessage(Format('Inserting UnitVersion information section into %s failed',
            [TestDLLFileName]));
      finally
        UnitList.Free;
      end;
    finally
      TestStream.Free;
    end;
  end
  else
    ShowMessage('Can''t insert section - DLL still loaded and unload failed...');
end;

end.
