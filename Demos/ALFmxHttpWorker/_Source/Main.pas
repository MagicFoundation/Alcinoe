unit Main;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Messaging,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Memo.Types,
  FMX.ScrollBox,
  FMX.Memo,
  Alcinoe.FMX.Controls,
  Alcinoe.FMX.Objects,
  Alcinoe.FMX.StdCtrls,
  Alcinoe.FMX.Layouts;

type
  TMainForm = class(TForm)
    ButtonUpload: TALButton;
    ALText1: TALText;
    LoadingContainer: TALLayout;
    LoadingAnimatedImage: TALAnimatedImage;
    ButtonDownload: TALButton;
    procedure ButtonUploadClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonDownloadClick(Sender: TObject);
  private
    procedure HttpWorkResultHandler(const Sender: TObject; const Msg: System.Messaging.TMessage);
  public
  end;

var
  MainForm: TMainForm;

implementation

uses
  System.Net.URLClient,
  System.IOUtils,
  Alcinoe.FMX.UserPreferences,
  Alcinoe.FMX.Styles,
  Alcinoe.FMX.Snackbar,
  Alcinoe.Http.Worker,
  Alcinoe.Common,
  Alcinoe.FMX.ErrorReporting,
  Alcinoe.Localization,
  Alcinoe.StringUtils;

{$R *.fmx}

{**********************************************}
procedure TMainForm.FormCreate(Sender: TObject);
begin
  ALLog('TMainForm.FormCreate');
  TALErrorReporting.Instance;
  TMessageManager.DefaultManager.SubscribeToMessage(TALHttpWorker.TWorkResultMessage, HttpWorkResultHandler);
  TALStyleManager.Instance.ApplySnackbarManagerStyle('Material3.SnackbarManager', TALSnackbarManager.Instance, 18{AFontSize});
  {$IF defined(MSWindows) or defined(ALMacOS)}
  TALUserPreferences.Instance.SetBoolean('uploading', false);
  TALUserPreferences.Instance.SetBoolean('downloading', false);
  {$ENDIF}
  if TALUserPreferences.Instance.getBoolean('uploading', false) then begin
    ButtonUpload.Enabled := false;
    ButtonUpload.Text := 'Uploading...';
    ButtonDownload.Enabled := False;
    ButtonDownload.Text := 'Download';
    LoadingContainer.Visible := True;
    LoadingAnimatedImage.Enabled := True;
  end
  else if TALUserPreferences.Instance.getBoolean('downloading', false) then begin
    ButtonUpload.Enabled := false;
    ButtonUpload.Text := 'Upload';
    ButtonDownload.Enabled := False;
    ButtonDownload.Text := 'Downloading...';
    LoadingContainer.Visible := True;
    LoadingAnimatedImage.Enabled := True;
  end
  else begin
    ButtonUpload.Enabled := True;
    ButtonUpload.Text := 'Upload';
    ButtonDownload.Enabled := True;
    ButtonDownload.Text := 'Download';
    LoadingContainer.Visible := False;
    LoadingAnimatedImage.Enabled := False;
  end;
end;

{***********************************************}
procedure TMainForm.FormDestroy(Sender: TObject);
begin
  ALLog('TMainForm.FormDestroy');
  TMessageManager.DefaultManager.Unsubscribe(TALHttpWorker.TWorkResultMessage, HttpWorkResultHandler);
end;

{*****************************************************}
procedure TMainForm.ButtonUploadClick(Sender: TObject);
begin
  ButtonUpload.Enabled := false;
  ButtonUpload.Text := 'Uploading...';
  ButtonDownload.Enabled := False;
  ButtonDownload.Text := 'Download';
  LoadingContainer.Visible := True;
  LoadingAnimatedImage.Enabled := True;
  try
    TALUserPreferences.Instance.SetBoolean('uploading', true);
    var Lfilename := TPath.Combine(TPath.GetTempPath, '100mb.tmp');
    if not TFile.Exists(Lfilename) then begin
      var LBytes: TBytes;
      SetLength(Lbytes, 10_000_000);
      FillChar(Lbytes[0],length(LBytes), 32);
      Tfile.WriteAllBytes(Lfilename, LBytes);
    end;
    TALHttpWorker.Instance.enqueue(
      'https://echo-http-requests.appspot.com/echo', // const AUrl: String;
      'POST', // const AMethod: String;
      Lfilename, // const ABodyFilePath: String;
      true, // ADeleteBodyFile
      [TNetHeader.create('User-Agent', 'TALHttpWorker'),
       TNetHeader.create('Content-Type', 'application/octet-stream')]); // const AHeaders: TNetHeaders): String;
  except
    On E: Exception do begin
      TALUserPreferences.Instance.SetBoolean('uploading', false);
      raise;
    end;
  end;
end;

{*******************************************************}
procedure TMainForm.ButtonDownloadClick(Sender: TObject);
begin
  ButtonUpload.Enabled := false;
  ButtonUpload.Text := 'Upload';
  ButtonDownload.Enabled := False;
  ButtonDownload.Text := 'Downloading...';
  LoadingContainer.Visible := True;
  LoadingAnimatedImage.Enabled := True;
  try
    TALUserPreferences.Instance.SetBoolean('downloading', true);
    TALHttpWorker.Instance.enqueue(
      'https://ash-speed.hetzner.com/100MB.bin', // const AUrl: String;
      'GET', // const AMethod: String;
      '', // const ABodyString: String;
      [TNetHeader.create('User-Agent', 'TALHttpWorker')]); // const AHeaders: TNetHeaders): String;
  except
    On E: Exception do begin
      TALUserPreferences.Instance.SetBoolean('downloading', false);
      raise;
    end;
  end;
end;

{*****************************************************************************************************}
procedure TMainForm.HttpWorkResultHandler(const Sender: TObject; const Msg: System.Messaging.TMessage);
begin
  var LWorkResultMessage := TALHttpWorker.TWorkResultMessage(Msg);

  ALLog('TMainForm.HttpWorkResultHandler', 'Request ID: ' + LWorkResultMessage.RequestId + ' | Success: ' + ALBoolToStrW(LWorkResultMessage.Value, 'True', 'False'));
  TALUserPreferences.Instance.SetBoolean('uploading', False);
  TALUserPreferences.Instance.SetBoolean('downloading', False);

  ButtonUpload.Enabled := True;
  ButtonUpload.Text := 'Upload';
  ButtonDownload.Enabled := True;
  ButtonDownload.Text := 'Download';
  LoadingContainer.Visible := False;
  LoadingAnimatedImage.Enabled := False;

  var LMessageText: string;
  if LWorkResultMessage.Value then LMessageText  := 'Job complete'
  else LMessageText  := 'Job failed';
  TALSnackBar.Builder.SetMessageText(LMessageText).Show;
end;

end.