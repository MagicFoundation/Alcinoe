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
{ The Original Code is HeadDump.pas.                                                               }
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

unit HeapDump;

{$I JCL.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, ToolWin, ActnList, ClipBrd, Menus,
  TLHelp32, ViewTemplate;

type
  THeapDumpForm = class(TViewForm)
    StatusBar: TStatusBar;
    Panel1: TPanel;
    HeapListView: TListView;
    Splitter1: TSplitter;
    HeapEntryListView: TListView;
    Splitter2: TSplitter;
    HeapEntryMemo: TMemo;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    Refresh2: TMenuItem;
    N1: TMenuItem;
    Copy2: TMenuItem;
    Save1: TMenuItem;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    N2: TMenuItem;
    Selectall2: TMenuItem;
    ToolButton3: TToolButton;
    procedure HeapListViewColumnClick(Sender: TObject;
      Column: TListColumn);
    procedure HeapListViewCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure StatusBarResize(Sender: TObject);
    procedure Refresh1Execute(Sender: TObject);
    procedure HeapEntryListViewData(Sender: TObject; Item: TListItem);
    procedure HeapEntryListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure HeapListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    FProcessID: DWORD;
    FFileName: TFileName;
    FreeSum, FixedSum, MoveableSum: Integer;
    FHeapEntries: array of THeapEntry32;
    procedure BuildHeapList;
    procedure BuildHeapEntriesList(HeapID: DWORD);
    procedure UpdateStatusLine;
    procedure ReadHeapEntry(Item: TListItem);
  public
    procedure BuildContent; override;
    procedure SetParams(ProcessID: DWORD; const FileName: TFileName);
  end;

var
  HeapDumpForm: THeapDumpForm;

implementation

{$R *.DFM}

uses
  Global, Main, ToolsUtils;

resourcestring
  sCaption = 'HeapList - %s';
  sCountStatus = 'Heap Entries: %d';
  sFixedStatus = 'Fixed: %0.n';
  sFreeStatus = 'Free: %0.n';
  sMoveableStatus = 'Moveable: %0.n';
  sPressEscape = 'Press <ESC> to cancel enumerating heap items ...';

{ THeapDumpForm }

procedure THeapDumpForm.BuildHeapEntriesList(HeapID: DWORD);
var
  Next: Boolean;
  HeapEntry: THeapEntry32;
  EntriesCount: Integer;
begin
  with HeapEntryListView do
  begin
    Items.BeginUpdate;
    Screen.Cursor := crHourGlass;
    try
      HeapEntryMemo.Font.Style := [fsBold];
      HeapEntryMemo.Text := sPressEscape;
      Items.Count := 0;
      EntriesCount := 0;
      SetLength(FHeapEntries, 0);
      FreeSum := 0;
      FixedSum := 0;
      MoveableSum := 0;
      HeapEntry.dwSize := Sizeof(HeapEntry);
      Next := Heap32First(HeapEntry, FProcessID, HeapID);
      while Next do
      begin
        SetLength(FHeapEntries, EntriesCount + 1);
        FHeapEntries[EntriesCount] := HeapEntry;
        with HeapEntry do
          case dwFlags of
            LF32_FIXED:
              Inc(FixedSum, dwBlockSize);
            LF32_FREE:
              Inc(FreeSum, dwBlockSize);
            LF32_MOVEABLE:
              Inc(MoveableSum, dwBlockSize);
          end;
        Inc(EntriesCount);
        if EntriesCount mod 200 = 0 then
        begin
          UpdateStatusLine;
          if GetAsyncKeyState(VK_ESCAPE) and $8000 <> 0 then Break;
        end;
        Next := Heap32Next(HeapEntry);
      end;
      Items.Count := EntriesCount;
      if Items.Count > 0 then
      begin
        AlphaSort;
        ItemFocused := Items[0];
        ItemFocused.Selected := True;
      end;
      UpdateStatusLine;
      HeapEntryMemo.ParentFont := True;
    finally
      Items.EndUpdate;
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure THeapDumpForm.BuildHeapList;
var
  SnapProcHandle: THandle;
  HeapList: THeapList32;
  Next: Boolean;
begin
  with HeapListView do
  begin
    Items.BeginUpdate;
    try
      Items.Clear;
      SnapProcHandle := CreateToolhelp32Snapshot(TH32CS_SNAPHEAPLIST, FProcessID);
      if SnapProcHandle <> THandle(-1) then
      begin
        HeapList.dwSize := Sizeof(HeapList);
        Next := Heap32ListFirst(SnapProcHandle, HeapList);
        while Next do
        begin
          with Items.Add do
          begin
            Caption := Format('%.8x', [HeapList.th32HeapID]);
            Data := Pointer(HeapList.th32HeapID);
            case HeapList.dwFlags of
              HF32_DEFAULT:
                SubItems.Add('Default');
              HF32_SHARED:
                SubItems.Add('Shared');
            else
              SubItems.Add('Normal');
            end;
          end;
          Next := Heap32ListNext(SnapProcHandle, HeapList);
        end;
        CloseHandle(SnapProcHandle);
      end;
      if Items.Count > 0 then
      begin
        AlphaSort;
        ItemFocused := Items[0];
        ItemFocused.Selected := True;
      end else
      begin
        BuildHeapEntriesList(0);
        HeapEntryMemo.Lines.Clear;
      end;  
    finally
      Items.EndUpdate;
    end;
  end;
end;

procedure THeapDumpForm.SetParams(ProcessID: DWORD; const FileName: TFileName);
begin
  FProcessID := ProcessID;
  FFileName := FileName;
  Caption := Format(sCaption, [FFileName]);
  PostBuildContentMessage;
end;

procedure THeapDumpForm.HeapListViewColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  LVColumnClick(Column);
end;

procedure THeapDumpForm.HeapListViewCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
begin
  LVCompare(TListView(Sender), Item1, Item2, Compare);
end;

procedure THeapDumpForm.UpdateStatusLine;
begin
  with StatusBar.Panels do
  begin
    BeginUpdate;
    Items[0].Text := Format(sCountStatus, [High(FHeapEntries) + 1]);
    Items[1].Text := Format(sFixedStatus, [IntToExtended(FixedSum)]);
    Items[2].Text := Format(sMoveableStatus, [IntToExtended(MoveableSum)]);
    Items[3].Text := Format(sFreeStatus, [IntToExtended(FreeSum)]);
    EndUpdate;
    Update;
  end;
end;

procedure THeapDumpForm.StatusBarResize(Sender: TObject);
var
  I: Integer;
begin
  with StatusBar do
    for I := 0 to Panels.Count - 1 do Panels[I].Width := Width div 4;
end;

procedure THeapDumpForm.ReadHeapEntry(Item: TListItem);
var
  BlockSize, BytesRead: {$IFDEF RTL230_UP}NativeUInt{$ELSE}DWORD{$ENDIF};
  Buffer, BufferEnd, P: PChar;
begin
  with HeapEntryMemo do {if DWORD(Item.SubItems.Objects[2]) <> LF32_FREE then}
  begin
    BlockSize := DWORD(Item.SubItems.Objects[1]);
    if BlockSize > 32768 then BlockSize := 32768;
    GetMem(Buffer, BlockSize);
    Lines.BeginUpdate;
    try
      Lines.Clear;
      if Toolhelp32ReadProcessMemory(FProcessID, Item.SubItems.Objects[0],
        Buffer^, BlockSize - 1, BytesRead) then
      begin
        P := Buffer;
        BufferEnd := Buffer + BytesRead - 1;
        while P < BufferEnd do
        begin
          case P^ of
            #0: P^ := '|';
            #1..#31: P^ := '.';
          end;
          Inc(P);
        end;
        Buffer[BytesRead] := #0;
        SetTextBuf(Buffer);
      end;
    finally
      FreeMem(Buffer);
      Lines.EndUpdate;
    end;
  end;
end;

procedure THeapDumpForm.Refresh1Execute(Sender: TObject);
begin
  BuildHeapList;
end;

procedure THeapDumpForm.HeapEntryListViewData(Sender: TObject;
  Item: TListItem);
begin
  with Item, FHeapEntries[Item.Index] do
  begin
    Caption := Format('%.8x', [hHandle]);
    SubItems.AddObject(Format('%.8x', [dwAddress]), Pointer(dwAddress));
    SubItems.AddObject(Format('%.0n', [IntToExtended(dwBlockSize)]), Pointer(dwBlockSize));
    SubItems.AddObject(Format('%.8x', [dwAddress + dwBlockSize]), Pointer(dwAddress + dwBlockSize));
    case dwFlags of
      LF32_FIXED:
        SubItems.AddObject('Fixed', Pointer(dwFlags));
      LF32_FREE:
        SubItems.AddObject('Free', Pointer(dwFlags));
      LF32_MOVEABLE:
        SubItems.AddObject('Moveable', Pointer(dwFlags));
    end;
    SubItems.AddObject(Format('%d', [dwLockCount]), Pointer(dwLockCount));
  end;
end;

procedure THeapDumpForm.HeapEntryListViewSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if Selected then ReadHeapEntry(Item);
end;

procedure THeapDumpForm.HeapListViewSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if Selected then BuildHeapEntriesList(DWORD(Item.Data));
end;

procedure THeapDumpForm.BuildContent;
begin
  BuildHeapList;
end;

end.
