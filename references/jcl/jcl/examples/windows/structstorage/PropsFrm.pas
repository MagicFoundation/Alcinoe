{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: PropsFrm.PAS, released on 2002-12-29.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

Last Modified: $Date$

You may retrieve the latest version of this file at the Project JEDI's Code Library home page,
located at https://github.com/project-jedi/jcl

Description:
Displays statistics for a TStatStg record

-----------------------------------------------------------------------------}

unit PropsFrm;

{$I jcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ActiveX, StdCtrls, ComCtrls;

type
  TfrmProps = class(TForm)
    TabControl1: TTabControl;
    btnClose: TButton;
    Label1: TLabel;
    edName: TEdit;
    Label2: TLabel;
    edSize: TEdit;
    Label3: TLabel;
    edCreated: TEdit;
    Label4: TLabel;
    edModified: TEdit;
    Label5: TLabel;
    edAccessed: TEdit;
    Label6: TLabel;
    edType: TEdit;
    Label7: TLabel;
    edCLSID: TEdit;
  private
    { Private declarations }
  public
    { Public declarations }
    class procedure ShowProperties(Stat: TStatStg);
  end;

resourcestring
  SError = 'Error';
  SConfirm = 'Confirm';
  SRoot = 'Document Root';
  SConfirmConversion = 'Confirm Conversion';
  SConvertFilePrompt = 'This file doesn''t appear to be a compound file. Would you like to convert it?';
  SConvertSuccess = 'File was converted succesfully.';
  SConvertFailFmt = 'Unable to convert file:'#13#10'%s';
  SBytesFloatFmt = '%0.n bytes';
  SConfirmSaveChanges = 'Do you want to save your changes?';

  SAddFolder = 'Add Folder';
  SFolderNameLabel = '&Name:';
  SErrNameEmpty = 'Name cannot be empty';
  SErrNameDuplicate = 'There is already an item with that name. Use another name.';
  SAddFile = 'Add File';
  SFileNameLabel = '&Name:';
  SDeletePrompt = 'Delete selected item?';
  SErrNodeEdit = 'Cannot edit this node!';
  SErrNodeRename = 'Cannot rename node!';

  SAboutMsg = 'Demo for JCL Structured Storage Class Wrapper.'#13#10#13#10 +
    'Note that all changes made to files with this program will be committed'#13#10 +
    'directly (unless running in Transacted mode) and cannot be undone'#13#10 +
    ' - use backup data for testing!'#13#10#13#10 +
    'The latest version of JCL is always available at https://github.com/project-jedi/jcl';
  SAboutCaption = 'About Compound Document Editor...';
  SStorage = 'Storage';
  SStream = 'Stream';
  SLockBytes = 'Lock bytes';
  SProperty = 'Property';
  SUnknown = 'unknown';
  SNotSet = '(not set)';

implementation
uses
  JclDateTime
{$IFNDEF COMPILER6_UP}
  , ComObj
{$ENDIF}
  ;

{$R *.dfm}

{ TfrmProps }

function StgTypeToStr(dwType: integer): string;
begin
  case dwType of
    STGTY_STORAGE:
      Result := SStorage;
    STGTY_STREAM:
      Result := SStream;
    STGTY_LOCKBYTES:
      Result := SLockBytes;
    STGTY_PROPERTY:
      Result := SProperty;
  else
    Result := SUnknown;
  end;
end;

function LimitedDateTimeToStr(ADateTime: TDateTime): string;
begin
  if ADateTime > EncodeDate(1900, 01, 01) then
    Result := DateTimeToStr(ADateTime)
  else
    Result := SNotSet;
end;

function MyGUIDToString(GUID: TGUID): string;
var EmptyGUID: TGUID;
begin
  FillChar(EmptyGUID, sizeof(EmptyGUID), 0);
  if CompareMem(@GUID, @EmptyGUID, sizeof(GUID)) then
    Result := SNotSet
  else
    Result := GUIDToString(GUID);
end;

class procedure TfrmProps.ShowProperties(Stat: TStatStg);
var
  frmProps: TfrmProps;
  i: integer;
  nSize: double;
begin
  frmProps := self.Create(Application);
  with frmProps, Stat do
  try
    edName.Text := WideCharToString(pwcsName);

    edType.Text := StgTypeToStr(dwType);
    nSize := cbSize;
    edSize.Text := Format(SBytesFloatFmt, [nSize]);
    edCreated.Text := LimitedDateTimeToStr(FileTimeToLocalDateTime(ctime));
    edModified.Text := LimitedDateTimeToStr(FileTimeToLocalDateTime(mtime));
    edAccessed.Text := LimitedDateTimeToStr(FileTimeToLocalDateTime(atime));
    edCLSID.Text := MyGUIDToString(clsid);
    for i := 0 to ComponentCount - 1 do
      if Components[i] is TEdit then
        TEdit(Components[i]).Hint := TEdit(Components[i]).Text;
    ShowModal;
  finally
    Free;
  end;
end;

end.

