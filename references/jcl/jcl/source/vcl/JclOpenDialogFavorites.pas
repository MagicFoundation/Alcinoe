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
{ The Original Code is OpenDlgFavAdapter.pas.                                                      }
{                                                                                                  }
{ The Initial Developer of the Original Code is Petr Vones.                                        }
{ Portions created by Petr Vones are Copyright (C) Petr Vones. All rights reserved.                }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Salvatore Besso                                                                                }
{   Florent Ouchet (move to JCL runtime)                                                           }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclOpenDialogFavorites;

interface

{$I jcl.inc}

uses
  {$IFDEF HAS_UNITSCOPE}
  Winapi.Windows, Winapi.Messages, Winapi.ShlObj, System.Classes, System.SysUtils, Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls,
  {$ELSE ~HAS_UNITSCOPE}
  Windows, Messages, ShlObj, Classes, SysUtils, Controls, StdCtrls, ExtCtrls,
  {$ENDIF ~HAS_UNITSCOPE}
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclPeImage, JclWin32,
  JclOpenDialogHooks;

// old-style open dialogs are supported by all versions of Delphi
{$DEFINE OLDSTYLE}

// new-style file dialogs are supported by Delphi 2007 and newer
// it is disabled in D2007, because the D2006 RTL does not support it at all
{$IFDEF RTL200_UP}
{$DEFINE NEWSTYLE}
{$ENDIF RTL200_UP}

type
  {$NODEFINE TJclOpenDialogFavoritesHook} // IFileDialogCustomize is badly emitted
  TJclOpenDialogFavoritesHook = class (TJclOpenDialogHook{$IFDEF NEWSTYLE}, IFileDialogControlEvents, IFileDialogEvents, IInterface{$ENDIF})
  private
    FFavoriteFolders: TStrings;
    FTextAdd: string;
    FTextDelete: string;
    FTextVirtual: string;
  {$IFDEF OLDSTYLE}
  private
    FFavoriteComboBox: TComboBox;
    FFavoriteStaticText: TStaticText;
    FFavoritePanel: TPanel;
    procedure FavoriteComboBoxClick(Sender: TObject);
  protected
    procedure DialogAdjustControlPos; override;
    procedure DialogFolderChange; override;
    procedure DialogShow; override;
    procedure DialogClose; override;
  {$ENDIF OLDSTYLE}
  {$IFDEF NEWSTYLE}
  private
    FComboboxCount: Integer;
    FComboboxListItem,
    FComboboxAddItem,
    FComboboxDeleteItem,
    FComboboxVirtualItem: Boolean;
    FTextList: string;
    procedure FileDialogCleanCombobox(const AFileDialogCustomize: IFileDialogCustomize);
    procedure FileDialogFillCombobox(const AFileDialogCustomize: IFileDialogCustomize; ListItem, AddItem, DeleteItem, VirtualItem: Boolean);
  protected
    procedure FileDialogCreate(const AFileDialog: IFileDialog); override;
  public
    { IInterface }
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    { IFileDialogEvents }
    function OnFileOk(const pfd: IFileDialog): HResult; stdcall;
    function OnFolderChanging(const pfd: IFileDialog;
      const psiFolder: IShellItem): HResult; stdcall;
    function OnFolderChange(const pfd: IFileDialog): HResult; stdcall;
    function OnSelectionChange(const pfd: IFileDialog): HResult; stdcall;
    function OnShareViolation(const pfd: IFileDialog; const psi: IShellItem;
      out pResponse: DWORD): HResult; stdcall;
    function OnTypeChange(const pfd: IFileDialog): HResult; stdcall;
    function OnOverwrite(const pfd: IFileDialog; const psi: IShellItem;
      out pResponse: DWORD): HResult; stdcall;
  public
    { IFileDialogControlEvents }
    function OnItemSelected(const pfdc: IFileDialogCustomize; dwIDCtl: DWORD;
      dwIDItem: DWORD): HResult; stdcall;
    function OnButtonClicked(const pfdc: IFileDialogCustomize;
      dwIDCtl: DWORD): HResult; stdcall;
    function OnCheckButtonToggled(const pfdc: IFileDialogCustomize;
      dwIDCtl: DWORD; bChecked: BOOL): HResult; stdcall;
    function OnControlActivating(const pfdc: IFileDialogCustomize;
      dwIDCtl: DWORD): HResult; stdcall;
  {$ENDIF NEWSTYLE}
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure LoadFavorites(const FileName: string);
    property FavoriteFolders: TStrings read FFavoriteFolders;
  end;

{$NODEFINE InitializeOpenDialogFavorites}
function InitializeOpenDialogFavorites: TJclOpenDialogFavoritesHook;
{$NODEFINE FinalizeOpenDialogFavorites}
procedure FinalizeOpenDialogFavorites;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\vcl';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF HAS_UNITSCOPE}
  Winapi.CommDlg, Winapi.Dlgs,
  {$ELSE ~HAS_UNITSCOPE}
  CommDlg, Dlgs,
  {$ENDIF ~HAS_UNITSCOPE}
  JclBase, JclFileUtils, JclStrings, JclSysInfo, JclSysUtils, JclVclResources;

{$IFDEF NEWSTYLE}
const
  CTRLID_COMBOBOX = $2346;
  ITEMID_LIST     = $FFFF;
  ITEMID_ADD      = $FFFE;
  ITEMID_DELETE   = $FFFD;
  ITEMID_VIRTUAL  = $FFFC;
{$ENDIF NEWSTYLE}

function InitializeOpenDialogFavorites: TJclOpenDialogFavoritesHook;
begin
  Result := InitializeOpenDialogHook(TJclOpenDialogFavoritesHook) as TJclOpenDialogFavoritesHook;
end;

procedure FinalizeOpenDialogFavorites;
begin
  FinalizeOpenDialogHook;
end;

//=== { TJclOpenDialogFavoritesHook } ========================================

constructor TJclOpenDialogFavoritesHook.Create;
begin
  inherited Create;
  FFavoriteFolders := TStringList.Create;

  {$IFDEF OLDSTYLE}
  FFavoritePanel := TPanel.Create(nil);
  FFavoritePanel.Name := 'FavoritePanel';
  FFavoritePanel.BevelOuter := bvNone;
  FFavoritePanel.Caption := '';
  FFavoritePanel.FullRepaint := False;

  FFavoriteComboBox := TComboBox.Create(nil);
  FFavoriteComboBox.Parent := FFavoritePanel;
  FFavoriteComboBox.Align := alClient;
  FFavoriteComboBox.Style := csDropDownList;
  FFavoriteComboBox.Sorted := True;
  FFavoriteComboBox.OnClick := FavoriteComboBoxClick;

  FFavoriteStaticText := TStaticText.Create(nil);
  FFavoriteStaticText.SetBounds(6, 18, FFavoriteStaticText.Width, FFavoriteStaticText.Height);
  FFavoriteStaticText.Caption := LoadResString(@RsOpenDialogFavorites);
  FFavoriteStaticText.AutoSize := True;
  FFavoriteStaticText.FocusControl := FFavoriteComboBox;
  {$ENDIF OLDSTYLE}

  FTextAdd := LoadResString(@RsOpenDialogAdd);
  FTextDelete := LoadResString(@RsOpenDialogDelete);
  FTextVirtual := LoadResString(@RsOpenDialogVirtual);

  {$IFDEF NEWSTYLE}
  FTextList := LoadResString(@RsOpenDialogList);
  {$ENDIF NEWSTYLE}
end;

destructor TJclOpenDialogFavoritesHook.Destroy;
begin
  {$IFDEF OLDSTYLE}
  FreeAndNil(FFavoriteComboBox);
  FreeAndNil(FFavoritePanel);
  FreeAndNil(FFavoriteStaticText);
  {$ENDIF OLDSTYLE}
  FreeAndNil(FFavoriteFolders);
  inherited Destroy;
end;

{$IFDEF OLDSTYLE}
procedure TJclOpenDialogFavoritesHook.DialogAdjustControlPos;
var
  FileTypeStaticTextRect, FileTypeEditRect,          // ID = 1136 1089
  FileNameStaticTextRect, FileNameEditRect: TRect;   // ID = 1148 1090

  procedure GetDlgItemRect(ItemID: Integer; var R: TRect);
  begin
    GetWindowRect(GetDlgItem(FParentWnd, ItemID), R);
    MapWindowPoints(0, FParentWnd, R, 2);
  end;

begin
  inherited DialogAdjustControlPos;

  GetDlgItemRect(stc2, FileTypeStaticTextRect);
  GetDlgItemRect(cmb1, FileTypeEditRect);
  GetDlgItemRect(stc3, FileNameStaticTextRect);
  GetDlgItemRect(cmb13, FileNameEditRect);

  FFavoriteStaticText.Left := FileTypeStaticTextRect.Left;
  FFavoriteStaticText.Top := 2 * FileTypeStaticTextRect.Top - FileNameStaticTextRect.Top;

  FFavoritePanel.Left := FileNameEditRect.Left;
  FFavoritePanel.Top := 2 * FileTypeEditRect.Top - FileNameEditRect.Top;
  FFavoritePanel.Width := FileTypeEditRect.Right - FileTypeEditRect.Left;
end;

procedure TJclOpenDialogFavoritesHook.DialogClose;
begin
  inherited DialogClose;
  if not IsOpenPictureDialog then
  begin
    FFavoriteComboBox.Items.Delete(0);
    FavoriteFolders.Assign(FFavoriteComboBox.Items);
  end;
  FFavoritePanel.ParentWindow := 0;
  FFavoriteStaticText.ParentWindow := 0;
  FParentWnd := 0;
end;

procedure TJclOpenDialogFavoritesHook.DialogFolderChange;
var
  Path: string;
begin
  inherited DialogFolderChange;
  Path := CurrentFolder;
  FFavoriteComboBox.ItemIndex := FFavoriteComboBox.Items.IndexOf(Path);
  if FFavoriteComboBox.ItemIndex = -1 then
  begin
    if Path <> '' then
      FFavoriteComboBox.Items[0] := FTextAdd
    else
      FFavoriteComboBox.Items[0] := FTextVirtual;
    FFavoriteComboBox.ItemIndex := 0;
  end
  else
    FFavoriteComboBox.Items[0] := FTextDelete;
  FFavoriteComboBox.Invalidate;
end;

procedure TJclOpenDialogFavoritesHook.DialogShow;
begin
  inherited DialogShow;
  if not IsOpenPictureDialog then
  begin
    FFavoritePanel.ParentWindow := FHandle;
    FFavoriteStaticText.ParentWindow := FHandle;
    FFavoriteComboBox.Items.Assign(FavoriteFolders);
    FFavoriteComboBox.Items.Insert(0, FTextAdd);
  end;
end;

procedure TJclOpenDialogFavoritesHook.FavoriteComboBoxClick(Sender: TObject);
var
  I: Integer;
  Path: string;
begin
  if FFavoriteComboBox.ItemIndex = 0 then
  begin
    Path := CurrentFolder;
    I := FFavoriteComboBox.Items.IndexOf(Path);
    if I > 0 then
    begin
      // delete current folder
      if MessageBox(FHandle,
                    PChar(Format(LoadResString(@RsOpenDialogDelConfirm), [Path])),
                    PChar(LoadResString(@RsOpenDialogConfirmation)),
                    MB_YESNO or MB_ICONQUESTION or MB_DEFBUTTON2) = ID_YES then
      begin
        FFavoriteComboBox.Items.Delete(I);
        FFavoriteComboBox.Items[0] := FTextAdd;
        FFavoriteComboBox.ItemIndex := 0;
      end;
    end
    else
    if Path <> '' then
    begin
      // add current folder
      FFavoriteComboBox.ItemIndex := FFavoriteComboBox.Items.Add(Path);
      FFavoriteComboBox.Items[0] := FTextDelete;
    end;
    FFavoriteComboBox.Invalidate;
  end
  else
  if FFavoriteComboBox.ItemIndex > 0 then
    // switch to selected folder
    CurrentFolder := FFavoriteComboBox.Items[FFavoriteComboBox.ItemIndex];
end;
{$ENDIF OLDSTYLE}

{$IFDEF NEWSTYLE}
procedure TJclOpenDialogFavoritesHook.FileDialogCreate(
  const AFileDialog: IFileDialog);
var
  FileDialogCustomize: IFileDialogCustomize;
  Unused: Cardinal;
begin
  inherited FileDialogCreate(AFileDialog);
  FComboboxListItem := False;
  FComboboxAddItem := False;
  FComboboxDeleteItem := False;
  FComboboxVirtualItem := False;
  FComboboxCount := 0;

  FileDialogCustomize := AFileDialog as IFileDialogCustomize;
  //CheckOSError(FileDialogCustomize.StartVisualGroup(CTRLID_GROUP, PWideChar(WideString(LoadResString(@RsOpenDialogFavorites)))));
  CheckOSError(FileDialogCustomize.AddComboBox(CTRLID_COMBOBOX));
  //CheckOSError(FileDialogCustomize.EndVisualGroup);
  CheckOSError(FileDialogCustomize.MakeProminent(CTRLID_COMBOBOX));
  CheckOSError(AFileDialog.Advise(Self, Unused));
end;

procedure TJclOpenDialogFavoritesHook.FileDialogCleanCombobox(
  const AFileDialogCustomize: IFileDialogCustomize);
var
  I: Integer;
begin
  if FComboboxListItem then
    CheckOSError(AFileDialogCustomize.RemoveControlItem(CTRLID_COMBOBOX, ITEMID_LIST));
  FComboboxListItem := False;
  if FComboboxAddItem then
    CheckOSError(AFileDialogCustomize.RemoveControlItem(CTRLID_COMBOBOX, ITEMID_ADD));
  FComboboxAddItem := False;
  if FComboboxDeleteItem then
    CheckOSError(AFileDialogCustomize.RemoveControlItem(CTRLID_COMBOBOX, ITEMID_DELETE));
  FComboboxDeleteItem := False;
  if FComboboxVirtualItem then
    CheckOSError(AFileDialogCustomize.RemoveControlItem(CTRLID_COMBOBOX, ITEMID_VIRTUAL));
  FComboboxVirtualItem := False;
  for I := 0 to FComboboxCount - 1 do
    CheckOSError(AFileDialogCustomize.RemoveControlItem(CTRLID_COMBOBOX, I));
  FComboboxCount := 0;
end;

procedure TJclOpenDialogFavoritesHook.FileDialogFillCombobox(
  const AFileDialogCustomize: IFileDialogCustomize; ListItem, AddItem, DeleteItem, VirtualItem: Boolean);
var
  I: Integer;
begin
  if ListItem then
    CheckOSError(AFileDialogCustomize.AddControlItem(CTRLID_COMBOBOX, ITEMID_LIST, PWideChar(WideString(FTextList))));
  FComboboxListItem := ListItem;
  if AddItem then
    CheckOSError(AFileDialogCustomize.AddControlItem(CTRLID_COMBOBOX, ITEMID_ADD, PWideChar(WideString(FTextAdd))));
  FComboboxAddItem := AddItem;
  if DeleteItem then
    CheckOSError(AFileDialogCustomize.AddControlItem(CTRLID_COMBOBOX, ITEMID_DELETE, PWideChar(WideString(FTextDelete))));
  FComboboxDeleteItem := DeleteItem;
  if VirtualItem then
    CheckOSError(AFileDialogCustomize.AddControlItem(CTRLID_COMBOBOX, ITEMID_VIRTUAL, PWideChar(WideString(FTextVirtual))));
  FComboboxVirtualItem := VirtualItem;
  FComboboxCount := FFavoriteFolders.Count;
  for I := 0 to FComboboxCount - 1 do
    CheckOSError(AFileDialogCustomize.AddControlItem(CTRLID_COMBOBOX, I, PWideChar(WideString(FFavoriteFolders.Strings[I]))));
end;
{$ENDIF NEWSTYLE}

procedure TJclOpenDialogFavoritesHook.LoadFavorites(const FileName: string);
begin
  if FileExists(FileName) then
    FavoriteFolders.LoadFromFile(FileName)
  else
    FavoriteFolders.Clear;
end;

{$IFDEF NEWSTYLE}
function TJclOpenDialogFavoritesHook.OnButtonClicked(
  const pfdc: IFileDialogCustomize; dwIDCtl: DWORD): HResult;
begin
  Result := S_OK;
end;

function TJclOpenDialogFavoritesHook.OnCheckButtonToggled(
  const pfdc: IFileDialogCustomize; dwIDCtl: DWORD; bChecked: BOOL): HResult;
begin
  Result := S_OK;
end;

function TJclOpenDialogFavoritesHook.OnControlActivating(
  const pfdc: IFileDialogCustomize; dwIDCtl: DWORD): HResult;
begin
  Result := S_OK;
end;

function TJclOpenDialogFavoritesHook.OnFileOk(const pfd: IFileDialog): HResult;
begin
  Result := S_OK;
end;

function TJclOpenDialogFavoritesHook.OnFolderChange(
  const pfd: IFileDialog): HResult;
var
  pfdc: IFileDialogCustomize;
  ppsi: IShellItem;
  Path: PWideChar;
  ItemIndex: Integer;
begin
  Result := S_OK;
  pfdc := pfd as IFileDialogCustomize;
  CheckOSError(pfd.GetFolder(ppsi));
  if not Succeeded(ppsi.GetDisplayName(SIGDN_FILESYSPATH, Path)) then
    Path := nil;
  ItemIndex := FFavoriteFolders.IndexOf(Path);
  if ItemIndex = -1 then
  begin
    if Path <> '' then
    begin
      FileDialogCleanCombobox(pfdc);
      FileDialogFillCombobox(pfdc, True, True, False, False);
      CheckOSError(pfdc.SetSelectedControlItem(CTRLID_COMBOBOX, ITEMID_LIST));
    end
    else
    begin
      FileDialogCleanCombobox(pfdc);
      FileDialogFillCombobox(pfdc, False, False, False, True);
      CheckOSError(pfdc.SetSelectedControlItem(CTRLID_COMBOBOX, ITEMID_VIRTUAL));
    end;
  end
  else
  begin
    FileDialogCleanCombobox(pfdc);
    FileDialogFillCombobox(pfdc, False, False, True, False);
    CheckOSError(pfdc.SetSelectedControlItem(CTRLID_COMBOBOX, ItemIndex));
  end;
end;

function TJclOpenDialogFavoritesHook.OnFolderChanging(const pfd: IFileDialog;
  const psiFolder: IShellItem): HResult;
begin
  Result := S_OK;
end;

function TJclOpenDialogFavoritesHook.OnItemSelected(
  const pfdc: IFileDialogCustomize; dwIDCtl, dwIDItem: DWORD): HResult;
var
  I: Integer;
  pfd: IFileDialog;
  ppsi: IShellItem;
  Path: PWideChar;
begin
  Result := S_OK;
  if dwIDCtl = CTRLID_COMBOBOX then
  begin
    pfd := pfdc as IFileDialog;
    CheckOSError(pfd.GetFolder(ppsi));
    if not Succeeded(ppsi.GetDisplayName(SIGDN_FILESYSPATH, Path)) then
      Path := nil;
    if dwIDItem = ITEMID_DELETE then
    begin
      I := FFavoriteFolders.IndexOf(Path);
      if I >= 0 then
      begin
        // delete current folder
        if MessageBox(0,
                      PChar(Format(LoadResString(@RsOpenDialogDelConfirm), [Path])),
                      PChar(LoadResString(@RsOpenDialogConfirmation)),
                      MB_YESNO or MB_ICONQUESTION or MB_DEFBUTTON2) = ID_YES then
        begin
          FFavoriteFolders.Delete(I);
          FileDialogCleanCombobox(pfdc);
          FileDialogFillCombobox(pfdc, True, True, False, False);
          CheckOSError(pfdc.SetSelectedControlItem(CTRLID_COMBOBOX, ITEMID_LIST));
        end;
      end;
    end
    else
    if dwIDItem = ITEMID_ADD then
    begin
      if Path <> '' then
      begin
        // add current folder
        I := FFavoriteFolders.Add(Path);
        FileDialogCleanCombobox(pfdc);
        FileDialogFillCombobox(pfdc, False, False, True, False);
        CheckOSError(pfdc.SetSelectedControlItem(CTRLID_COMBOBOX, I));
      end;
    end
    else
    //if dwIDItem >= 0 then
    begin
      // switch to selected folder
      CheckOSError(SHCreateItemFromParsingName(PWideChar(WideString(FFavoriteFolders.Strings[dwIDItem])), nil, IShellItem, ppsi));
      CheckOSError(pfd.SetFolder(ppsi));
      FileDialogCleanCombobox(pfdc);
      FileDialogFillCombobox(pfdc, False, False, True, False);
      CheckOSError(pfdc.SetSelectedControlItem(CTRLID_COMBOBOX, dwIDItem));
    end;
  end;
end;

function TJclOpenDialogFavoritesHook.OnOverwrite(const pfd: IFileDialog;
  const psi: IShellItem; out pResponse: DWORD): HResult;
begin
  Result := S_OK;
end;

function TJclOpenDialogFavoritesHook.OnSelectionChange(
  const pfd: IFileDialog): HResult;
begin
  Result := S_OK;
end;

function TJclOpenDialogFavoritesHook.OnShareViolation(const pfd: IFileDialog;
  const psi: IShellItem; out pResponse: DWORD): HResult;
begin
  Result := S_OK;
end;

function TJclOpenDialogFavoritesHook.OnTypeChange(
  const pfd: IFileDialog): HResult;
begin
  Result := S_OK;
end;

function TJclOpenDialogFavoritesHook.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TJclOpenDialogFavoritesHook._AddRef: Integer;
begin
  Result := -1;
end;

function TJclOpenDialogFavoritesHook._Release: Integer;
begin
  Result := -1;
end;
{$ENDIF NEWSTYLE}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
