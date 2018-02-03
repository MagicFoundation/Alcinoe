unit ClrDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActnList, Menus, ComCtrls, ClrDemoAbstractFrame,
  JclPeImage, JclCLR, ClrDemoMetaDataFrame, ClrDemoCLRFrame;

type
  TfrmMain = class(TForm)
    lstActions: TActionList;
    actFileExit: TAction;
    actFileOpen: TAction;
    actHelpAbout: TAction;
    mnuMain: TMainMenu;
    mnuFile: TMenuItem;
    mnuFileOpen: TMenuItem;
    mnuFileLine0: TMenuItem;
    mnuFileExit: TMenuItem;
    mnuHelp: TMenuItem;
    mnuFileAbout: TMenuItem;
    dlgOpen: TOpenDialog;
    barStatus: TStatusBar;
    PC: TPageControl;
    tsMetadata: TTabSheet;
    frmMetadata: TfrmMetadata;
    popMetadataStream: TPopupMenu;
    actViewStreamData: TAction;
    popViewStreamData: TMenuItem;
    mnuView: TMenuItem;
    mnuViewStreamData: TMenuItem;
    tsCLR: TTabSheet;
    frmCLR: TfrmCLR;
    actFileDump: TAction;
    dlgSave: TSaveDialog;
    mnuFileDump: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actFileExitExecute(Sender: TObject);
    procedure actHelpAboutExecute(Sender: TObject);
    procedure actFileOpenExecute(Sender: TObject);
    procedure PCChange(Sender: TObject);
    procedure actViewStreamDataUpdate(Sender: TObject);
    procedure actViewStreamDataExecute(Sender: TObject);
    procedure frmMetadatalstStreamDblClick(Sender: TObject);
    procedure actFileDumpExecute(Sender: TObject);
    procedure actFileDumpUpdate(Sender: TObject);
  private
    m_Image: TJclPeImage;
    m_CLR: TJclCLRHeaderEx;

    function GetActiveFrame: TfrmAbstract;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.DFM}

uses
  ShellApi, ClrDemoStringsForm, ClrDemoGuidForm, ClrDemoBlobForm,
  ClrDemoTableForm, ClrDemoUserStringsForm;

const
  CRLF = #10#13;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  m_Image := nil;
  m_CLR   := nil;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(m_CLR);
  FreeAndNil(m_Image);
end;

function TfrmMain.GetActiveFrame: TfrmAbstract;
var
  I: Integer;
begin
  if Assigned(PC.ActivePage) then
  with PC.ActivePage do
  for I:=0 to ControlCount-1 do
    if Controls[0].InheritsFrom(TfrmAbstract) then
    begin
      Result := TfrmAbstract(Controls[0]);
      Exit;
    end;

  raise Exception.Create('No frame was active!');
end;

procedure TfrmMain.actFileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.actHelpAboutExecute(Sender: TObject);
begin
  ShellAbout(Handle, PChar(Caption),
    PChar('JEDI Code Library (JCL)' + CRLF + 'http://delphi-jedi.org/'),
    Application.Icon.Handle);
end;

procedure TfrmMain.actFileOpenExecute(Sender: TObject);
var
  Img: TJclPeImage;
begin
  if dlgOpen.Execute then
  begin
    Img := TJclPeImage.Create;
    Img.FileName := dlgOpen.FileName;

    if Img.IsCLR then
    begin
      FreeAndNil(m_Image);
      m_Image := Img;

      FreeAndNil(m_CLR);
      m_CLR := TJclCLRHeaderEx.Create(m_Image);
      GetActiveFrame.ShowInfo(m_CLR);
    end
    else
    begin
      FreeAndNil(Img);
      MessageDlg(Format('The file %s is not a CLR file', [dlgOpen.FileName]), mtWarning, [mbOK], 0);
    end;
  end;
end;

procedure TfrmMain.PCChange(Sender: TObject);
begin
  if Assigned(m_CLR) then
    GetActiveFrame.ShowInfo(m_CLR);
end;

procedure TfrmMain.actViewStreamDataUpdate(Sender: TObject);
begin
  with frmMetadata.lstStream do
  TAction(Sender).Enabled := Assigned(Selected) and
    ((TObject(Selected.Data).ClassType = TJclCLRStringsStream) or
     (TObject(Selected.Data).ClassType = TJclCLRGuidStream) or
     (TObject(Selected.Data).ClassType = TJclCLRUserStringStream) or
     (TObject(Selected.Data).ClassType = TJclCLRBlobStream) or
     (TObject(Selected.Data).ClassType = TJclCLRTableStream));
end;

procedure TfrmMain.actViewStreamDataExecute(Sender: TObject);
begin
  with frmMetadata.lstStream do
  if TObject(Selected.Data).ClassType = TJclCLRStringsStream then
    TfrmStrings.Execute(Selected.Data)
  else if TObject(Selected.Data).ClassType = TJclCLRGuidStream then
    TfrmGuid.Execute(Selected.Data)
  else if TObject(Selected.Data).ClassType = TJclCLRUserStringStream then
    TfrmUserStrings.Execute(Selected.Data)
  else if TObject(Selected.Data).ClassType = TJclCLRBlobStream then
    TfrmBlobs.Execute(Selected.Data)
  else if TObject(Selected.Data).ClassType = TJclCLRTableStream then
    TfrmTable.Execute(Selected.Data);
end;

procedure TfrmMain.frmMetadatalstStreamDblClick(Sender: TObject);
begin
  if actViewStreamData.Enabled then
    actViewStreamDataExecute(Sender);
end;

procedure TfrmMain.actFileDumpExecute(Sender: TObject);
begin
  dlgSave.InitialDir := ExtractFilePath(m_Image.FileName);
  dlgSave.FileName   := ExtractFileName(ChangeFileExt(m_Image.FileName, '.il'));
  if dlgSave.Execute then
  with TStringList.Create do
  try
    Text := m_CLR.DumpIL;
    SaveToFile(dlgSave.FileName);
  finally
    Free;
  end;
end;

procedure TfrmMain.actFileDumpUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(m_CLR);
end;

end.
