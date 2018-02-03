{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL) - Delphi Tools                                                   }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is MemoryDump.pas.                                                             }
{                                                                                                  }
{ The Initial Developer of the Original Code is Petr Vones. Portions created by Petr Vones are     }
{ Copyright (C) of Petr Vones. All Rights Reserved.                                                }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date$                                                      }
{                                                                                                  }
{**************************************************************************************************}

unit MemoryDump;

{$I JCL.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ToolWin, ActnList, ExtCtrls, ViewTemplate, Menus;

type
  TMemoryInfo = packed record
    MemInfo: TMemoryBasicInformation;
    RepeatedItem, MappedFile: Boolean;
  end;

  TMemoryDumpForm = class(TViewForm)
    StatusBar: TStatusBar;
    PagesListView: TListView;
    Splitter1: TSplitter;
    DumpListView: TListView;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    MemoryTreeView: TTreeView;
    Splitter2: TSplitter;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    Refresh2: TMenuItem;
    N1: TMenuItem;
    Copy2: TMenuItem;
    Save1: TMenuItem;
    N2: TMenuItem;
    Selectall2: TMenuItem;
    ViewAsText1: TAction;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    N3: TMenuItem;
    Viewastext2: TMenuItem;
    SaveData1: TAction;
    ToolButton9: TToolButton;
    Savedata2: TMenuItem;
    SaveDataDialog: TSaveDialog;
    ToolButton10: TToolButton;
    procedure Refresh1Execute(Sender: TObject);
    procedure DumpListViewData(Sender: TObject; Item: TListItem);
    procedure PagesListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure PagesListViewCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure PagesListViewData(Sender: TObject; Item: TListItem);
    procedure MemoryTreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure MemoryTreeViewGetSelectedIndex(Sender: TObject;
      Node: TTreeNode);
    procedure ViewAsText1Execute(Sender: TObject);
    procedure SaveData1Update(Sender: TObject);
    procedure SaveData1Execute(Sender: TObject);
  private
    FDumpBytesPerLine: Integer;
    FProcessID: DWORD;
    FProcess: THandle;
    FFileName: TFileName;
    FMemoryInfo: array of TMemoryInfo;
    FModulesList: TStringList;
    procedure BuildPagesList;
    procedure BuildModulesList;
    procedure UpdateDumpList;
  public
    procedure SetParams(ProcessID: DWORD; const FileName: TFileName);
  end;

var
  MemoryDumpForm: TMemoryDumpForm;

implementation

uses Global, TLHelp32, ToolsUtils, FindDlg, JclBase;

{$R *.DFM}

resourcestring
  sAllocations = 'Allocations';
  sCaption = 'Virtual Memory list - %s';
  sCommited = 'Comitted: %.0n';
  sCount = 'Count: %d';
  sModules = 'Modules';
  sReserved = 'Reserved: %.0n';

function AllocationProtectStr(P: DWORD): string;
begin
  case P of
    PAGE_NOACCESS:
      Result := 'NoAccess';
    PAGE_READONLY:
      Result := 'ReadOnly';
    PAGE_READWRITE:
      Result := 'ReadWrite';
    PAGE_WRITECOPY:
      Result := 'WriteCopy';
    PAGE_EXECUTE:
      Result := 'Exec';
    PAGE_EXECUTE_READ:
      Result := 'ExecRead';
    PAGE_EXECUTE_READWRITE:
      Result := 'ExecReadWrite';
    PAGE_EXECUTE_WRITECOPY:
      Result := 'ExecWriteCopy';
    PAGE_GUARD:
      Result := 'Guard';
    PAGE_NOCACHE:
      Result := 'NoCache';
  else
    Result := '';
  end;
end;

function StateStr(P: DWORD): string;
begin
  case P of
    MEM_COMMIT:
      Result := 'Commit';
    MEM_FREE:
      Result := 'Free';
    MEM_RESERVE:
      Result := 'Reserve';
  else
    Result := Format('%x', [P]);
  end;
end;

function TypeStr(P: DWORD): string;
begin
  case P of
    MEM_IMAGE:
      Result := 'Image';
    MEM_MAPPED:
      Result := 'Mapped';
    MEM_PRIVATE:
      Result := 'Private';
  else
    Result := Format('%x', [P]);
  end;
end;

function ImageIndexFromInfo(MemInfo: TMemoryInfo): Integer;
begin
  with MemInfo do
    if MappedFile then Result := 6 else
      if RepeatedItem then Result := 21 else
        Result := 19;
end;

{ TMemoryDumpForm }

procedure TMemoryDumpForm.FormCreate(Sender: TObject);
begin
  inherited;
  FModulesList := TStringList.Create;
end;

procedure TMemoryDumpForm.FormDestroy(Sender: TObject);
begin
  FModulesList.Free;
  if FProcess <> 0 then CloseHandle(FProcess);
end;

procedure TMemoryDumpForm.BuildPagesList;
var
  AllocationsNode, ModulesNode, TempNode: TTreeNode;
  LastAllocationBase: Pointer;
  LastMappedFile: Boolean;
  I, N, TotalCommit, TotalReserve: Integer;

  procedure EnumAllocations;
var
  P: PChar;
  MI: TMemoryBasicInformation;
  Res: DWORD;
  Count: Integer;
begin
  FMemoryInfo := nil;
  Count := 0;
  P := Pointer(0);
  Res := VirtualQueryEx(FProcess, P, MI, SizeOf(MI));
  if Res <> SizeOf(MI) then RaiseLastOSError;
  while Res = SizeOf(MI) do
  begin
    if MI.AllocationBase <> nil then
    begin
      SetLength(FMemoryInfo, Count + 1);
      FMemoryInfo[Count].MemInfo := MI;
      Inc(Count);
    end;
    Inc(P, MI.RegionSize);
    Res := VirtualQueryEx(FProcess, P, MI, SizeOf(MI));
  end;
end;

begin
  Screen.Cursor := crHourGlass;
  try
    PagesListView.Items.BeginUpdate;
    PagesListView.Items.Count := 0;
    MemoryTreeView.Items.BeginUpdate;
    StatusBar.Panels.BeginUpdate;
    try
      EnumAllocations;
      PagesListView.Items.Count := Length(FMemoryInfo);

      with MemoryTreeView.Items do
      begin
        Clear;
        AllocationsNode := AddFirst(nil, sAllocations);
        AllocationsNode.ImageIndex := 19;
        ModulesNode := Add(nil, sModules);
        ModulesNode.ImageIndex := 6;
        LastAllocationBase := nil;
        LastMappedFile := False;
        for I := 0 to Length(FMemoryInfo) - 1 do
          with FMemoryInfo[I] do
            if LastAllocationBase <> MemInfo.AllocationBase then
            begin
              TempNode := AddChildObject(AllocationsNode, Format('%p', [MemInfo.AllocationBase]), Pointer(I));
              with TempNode do ImageIndex := Parent.ImageIndex;
              LastAllocationBase := MemInfo.AllocationBase;
              RepeatedItem := False;
              N := FModulesList.IndexOfObject(LastAllocationBase);
              if N <> -1 then
              begin
                TempNode := AddChildObject(ModulesNode, FModulesList[N], Pointer(I));
                with TempNode do ImageIndex := Parent.ImageIndex;
                MappedFile := True;
              end else
                MappedFile := False;
              LastMappedFile := MappedFile;
            end else
            begin
              RepeatedItem := True;
              MappedFile := LastMappedFile;
            end;
      end;
      AllocationsNode.AlphaSort;
      ModulesNode.AlphaSort;

      TotalCommit := 0;
      TotalReserve := 0;
      for I := 0 to Length(FMemoryInfo) - 1 do with FMemoryInfo[I].MemInfo do
        case State of
          MEM_COMMIT: Inc(TotalCommit, RegionSize);
          MEM_RESERVE: Inc(TotalReserve, RegionSize);
        end;
      with StatusBar do
      begin
        Panels[0].Text := Format(sCount, [Length(FMemoryInfo)]);
        Panels[1].Text := Format(sCommited, [IntToExtended(TotalCommit)]);
        Panels[2].Text := Format(sReserved, [IntToExtended(TotalReserve)]);
      end;

      ListViewFocusFirstItem(PagesListView);
    finally
      PagesListView.Items.EndUpdate;
      MemoryTreeView.Items.EndUpdate;
      StatusBar.Panels.EndUpdate;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMemoryDumpForm.BuildModulesList;
var
  SnapProcHandle: THandle;
  ModuleEntry: TModuleEntry32;
  Next: Boolean;
begin
  FModulesList.Clear;
  SnapProcHandle := CreateToolhelp32Snapshot(TH32CS_SNAPMODULE, FProcessID);
  if SnapProcHandle <> THandle(-1) then
  begin
    ModuleEntry.dwSize := SizeOf(ModuleEntry);
    Next := Module32First(SnapProcHandle, ModuleEntry);
    while Next do
    begin
      FModulesList.AddObject(ModuleEntry.szModule, Pointer(ModuleEntry.modBaseAddr));
      Next := Module32Next(SnapProcHandle, ModuleEntry);
    end;
    CloseHandle(SnapProcHandle);
  end;
end;

procedure TMemoryDumpForm.SetParams(ProcessID: DWORD; const FileName: TFileName);
begin
  FProcessID := ProcessID;
  FFileName := FileName;
  Caption := Format(sCaption, [FFileName]);
  Refresh1.Execute;
end;

procedure TMemoryDumpForm.UpdateDumpList;
begin
  with DumpListView do
  begin
    if ViewAsText1.Checked then
    begin
      FDumpBytesPerLine := 64;
      Columns[1].Caption := 'Ansi text';
      Columns[2].Caption := 'Unicode text';
    end else
    begin
      FDumpBytesPerLine := 16;
      Columns[1].Caption := 'Data';
      Columns[2].Caption := 'ASCII';
    end;
    Items.Count := Integer(PagesListView.Selected.SubItems.Objects[3]) div FDumpBytesPerLine;
    Invalidate;
  end;
end;

procedure TMemoryDumpForm.Refresh1Execute(Sender: TObject);
begin
  if FProcess <> 0 then CloseHandle(FProcess);
  FProcess := OpenProcess(PROCESS_ALL_ACCESS, False, FProcessID);
  if FProcess = 0 then
  begin
    Close;
    RaiseLastOSError;
  end;
  BuildModulesList;
  BuildPagesList;
end;

procedure TMemoryDumpForm.DumpListViewData(Sender: TObject; Item: TListItem);
var
  Address: Pointer;
  LineData: packed array[0..63] of Byte;
  NR: {$IFDEF RTL230_UP}NativeUInt{$ELSE}DWORD{$ENDIF};
  Hex, Ascii, S: string;
  I: Integer;
  W: PWideChar;
begin
  with TListView(Sender) do
    if PagesListView.Selected <> nil then
    begin
      Address := Pointer(DWORD(FMemoryInfo[PagesListView.Selected.Index].MemInfo.BaseAddress) + DWORD(Item.Index * FDumpBytesPerLine));
      SetLength(Hex, 3 * SizeOf(LineData));
      SetLength(Ascii, 3 * SizeOf(LineData));
      Hex := '';
      Ascii := '';
      if ReadProcessMemory(FProcess, Address, @LineData, SizeOf(LineData), NR) and (NR = SizeOf(LineData)) then
      begin
        if ViewAsText1.Checked then
        begin
          for I := 0 to FDumpBytesPerLine - 1 do
          begin
            if LineData[I] >= 32 then
              Hex := Hex + Chr(LineData[I])
            else
              Hex := Hex + '.';
          end;
          W := PWideChar(@LineData);
          for I := 0 to FDumpBytesPerLine div 2 - 1 do
          begin
            SetLength(S, 1);
            {$IFDEF SUPPORTS_UNICODE}
            S := WideString(W^);
            {$ELSE ~SUPPORTS_UNICODE}
            WideCharToMultiByte(CP_ACP, 0, W, 1, PAnsiChar(S), 1, nil, nil);
            {$ENDIF ~SUPPORTS_UNICODE}
            S := PChar(S);
            if Length(S) = 0 then S := '.';
            Ascii := Ascii + S;
            Inc(W);
          end;
        end else
        begin
          for I := 0 to FDumpBytesPerLine - 1 do
          begin
            Hex := Hex + Format('%.2x ', [LineData[I]]);
            if LineData[I] >= 32 then
              Ascii := Ascii + Chr(LineData[I])
            else
              Ascii := Ascii + '.';
          end;
        end;
      end;
      Item.Caption := Format('%p', [Address]);
      Item.SubItems.Add(Hex);
      Item.SubItems.Add(Ascii);
    end;
end;

procedure TMemoryDumpForm.PagesListViewSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if Selected then
  begin
    if (DWORD(Item.SubItems.Objects[0]) = PAGE_NOACCESS) or
       (DWORD(Item.SubItems.Objects[2]) = 0) then
    begin
      DumpListView.Items.Count := 0;
      DumpListView.Invalidate;
    end else
      UpdateDumpList;
  end;
end;

procedure TMemoryDumpForm.PagesListViewData(Sender: TObject; Item: TListItem);
var
  I: Integer;
begin
  with Item, FMemoryInfo[Item.Index].MemInfo do
  begin
    Caption := Format('%p', [BaseAddress]);
    SubItems.AddObject(AllocationProtectStr(Protect), Pointer(Protect));
    SubItems.AddObject(Format('%p', [AllocationBase]), AllocationBase);
    SubItems.AddObject(AllocationProtectStr(AllocationProtect), Pointer(AllocationProtect));
    SubItems.AddObject(Format('%.0n', [IntToExtended(RegionSize)]), Pointer(RegionSize));
    SubItems.AddObject(StateStr(State), Pointer(State));
    I := FModulesList.IndexOfObject(AllocationBase);
    if I <> - 1 then SubItems.Add(FModulesList[I]) else SubItems.Add('');
    SubItems.AddObject(TypeStr(Type_9), Pointer(Type_9));
  end;
  Item.ImageIndex := ImageIndexFromInfo(FMemoryInfo[Item.Index]);
end;

procedure TMemoryDumpForm.PagesListViewCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if DWORD(Item.SubItems.Objects[0]) = PAGE_NOACCESS then
    Sender.Canvas.Font.Color := clBtnFace;
end;

procedure TMemoryDumpForm.MemoryTreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  if Node.Level = 1 then
    with PagesListView do
    begin
      while Assigned(Selected) do Selected.Selected := False;
      ItemFocused := PagesListView.Items[Integer(Node.Data)];
      ItemFocused.Selected := True;
      ItemFocused.MakeVisible(False);
    end;
end;

procedure TMemoryDumpForm.MemoryTreeViewGetSelectedIndex(Sender: TObject;
  Node: TTreeNode);
begin
  Node.SelectedIndex := Node.ImageIndex;
end;

procedure TMemoryDumpForm.ViewAsText1Execute(Sender: TObject);
begin
  with ViewAsText1 do
    Checked := not Checked;
  UpdateDumpList;  
end;

procedure TMemoryDumpForm.SaveData1Update(Sender: TObject);
begin
  TAction(Sender).Enabled := (ActiveControl = PagesListView) and
    (PagesListView.Selected <> nil) and
    (DWORD(PagesListView.Selected.SubItems.Objects[0]) <> PAGE_NOACCESS);
end;

procedure TMemoryDumpForm.SaveData1Execute(Sender: TObject);
var
  MS: TMemoryStream;
  NR: {$IFDEF RTL230_UP}NativeUInt{$ELSE}DWORD{$ENDIF};
begin
  with SaveDataDialog, FMemoryInfo[PagesListView.Selected.Index].MemInfo do
  begin
    FileName := '';
    if Execute then
    begin
      MS := TMemoryStream.Create;
      try
        MS.Size := RegionSize;
        if ReadProcessMemory(FProcess, BaseAddress, MS.Memory, RegionSize, NR) and
          (NR = RegionSize) then
          MS.SaveToFile(FileName)
        else
          RaiseLastOSError;
      finally
        MS.Free;
      end;
    end;
  end;
end;

end.
