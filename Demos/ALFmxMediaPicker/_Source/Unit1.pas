unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, Alcinoe.FMX.StdCtrls,
  FMX.Effects, FMX.Filter.Effects, FMX.Objects, FMX.Layouts, Alcinoe.FMX.Layouts,
  FMX.Controls.Presentation, FMX.StdCtrls, Alcinoe.FMX.FilterEffects, system.messaging,
  Alcinoe.FMX.Controls, Alcinoe.FMX.Objects, Alcinoe.Fmx.MediaPicker,
  Alcinoe.FMX.VideoPlayer, Alcinoe.FMX.PageController;

type
  TForm1 = class(TForm)
    ALButton1: TALButton;
    MainPageController: TALPageController;
    ALPageIndicator1: TALPageIndicator;
    procedure FormCreate(Sender: TObject);
    procedure ALButton1Click(Sender: TObject);
  private
    procedure OnPickMediaSuccess(const AItems: TArray<TALMediaPicker.TMediaItem>);
    procedure OnPickMediaCancel;
    procedure OnPickMediaError(const AMsg: string);
    procedure ShowRequestPermissionRationale(
                const AToRequestCameraPermission: Boolean;
                const AToRequestMicPermission: Boolean;
                const ACanRequestPermissionProc: TProc;
                const ACanNotRequestPermissionProc: TProc);

  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  Alcinoe.fmx.Styles,
  Alcinoe.FMX.LoadingOverlay,
  Alcinoe.FMX.ErrorReporting,
  Alcinoe.StringUtils,
  Alcinoe.FMX.Common,
  Alcinoe.Fmx.Dialogs,
  Alcinoe.Common;

{***********************************************}
procedure TForm1.ALButton1Click(Sender: TObject);
begin
  TALDialog.Builder
    .SetHeadlineText('Add a photo')
    .SetMessageText('Take a new photo or select one you already have')
    .addButton('<img src="gallery" width="22" color="inherit">&nbsp;&nbsp; Choose from gallery', 1{ATag}, False{AIsFooterButton})
    .addButton('<img src="camera" width="22" color="inherit">&nbsp;&nbsp; Take a photo', 2{ATag}, False{AIsFooterButton})
    .addButton('<img src="video_camera" width="22" color="inherit">&nbsp;&nbsp; Take a video', 3{ATag}, False{AIsFooterButton})
    .addButton('Cancel', 0{ATag})
    .SetOnActionCallback(
       procedure(Const ADialog: TALDialog; const AAction: Integer; var ACanClose: Boolean)
       begin
        If AAction = 1 then
          TALMediaPicker.Instance.PickMedia(
            [TALMediaPicker.TMediaType.image, TALMediaPicker.TMediaType.Video], // const ATypes: TMediaTypes;
            10, // const AMaxCount: Integer;
            OnPickMediaSuccess, // const AOnSuccess: TOnSuccessEvent;
            OnPickMediaCancel, // const AOnCancel: TOnCancelEvent;
            OnPickMediaError) // const AOnError: TOnErrorEvent
        else If AAction = 2 then
          TALMediaPicker.Instance.CaptureFromCamera(
            TALMediaPicker.TMediaType.image, // const ATypes: TMediaTypes;
            OnPickMediaSuccess, // const AOnSuccess: TOnSuccessEvent;
            OnPickMediaCancel, // const AOnCancel: TOnCancelEvent;
            OnPickMediaError, // const AOnError: TOnErrorEvent;
            ShowRequestPermissionRationale) // const AShowRequestPermissionRationaleEvent: TShowRequestPermissionRationaleEvent
        else If AAction = 3 then
          TALMediaPicker.Instance.CaptureFromCamera(
            TALMediaPicker.TMediaType.Video, // const ATypes: TMediaTypes;
            OnPickMediaSuccess, // const AOnSuccess: TOnSuccessEvent;
            OnPickMediaCancel, // const AOnCancel: TOnCancelEvent;
            OnPickMediaError, // const AOnError: TOnErrorEvent;
            ShowRequestPermissionRationale); // const AShowRequestPermissionRationaleEvent: TShowRequestPermissionRationaleEvent
       end)
    .Show;
end;

{*******************************************}
procedure TForm1.FormCreate(Sender: TObject);
begin
  TALErrorReporting.Instance;
  TALStyleManager.Instance.ApplyLoadingOverlayManagerStyle('Material3.LoadingOverlayManager', TALLoadingOverlayManager.Instance);
  TALStyleManager.Instance.ApplyDialogManagerStyle('Material3.DialogManager', TALDialogManager.Instance, 18{AFontSize});
end;

{***********************************************************************************}
procedure TForm1.OnPickMediaSuccess(const AItems: TArray<TALMediaPicker.TMediaItem>);
begin
  ALLog('OnPickMediaSuccess', 'ItemsCount: ' + ALinttostrW(length(AItems)));
  MainPageController.DeleteAllPages;
  for var I := low(AItems) to high(AItems) do begin
    if AItems[I].MediaType = TALMediaPicker.TMediaType.video then begin
      var LPageView := MainPageController.AddPage;
      var LVideoPlayerSurface := TALVideoPlayerSurface.Create(LPageView);
      LVideoPlayerSurface.Parent := LPageView;
      LVideoPlayerSurface.Align := TALAlignLayout.Client;
      LVideoPlayerSurface.Looping := True;
      LVideoPlayerSurface.DataSource := AItems[I].Uri;
      LVideoPlayerSurface.RotateAccordingToMetadataOrientation := true;
      LVideoPlayerSurface.AutoStartMode := TALVideoPlayerSurface.TAutoStartMode.WhenDisplayed;
    end
    else if AItems[I].MediaType = TALMediaPicker.TMediaType.Image then begin
      var LPageView := MainPageController.AddPage;
      var LImage := TALImage.Create(LPageView);
      LImage.Parent := LPageView;
      LImage.Align := TALAlignLayout.Client;
      LImage.RotateAccordingToExifOrientation := True;
      LImage.ResourceStream := AItems[I].ExtractStream;
    end;
  end;
  ALMakeBufDrawables(MainPageController);
end;

{*********************************}
procedure TForm1.OnPickMediaCancel;
begin
  ALLog('OnPickMediaCancel');
end;

{****************************************************}
procedure TForm1.OnPickMediaError(const AMsg: string);
begin
  ALLog('OnPickMediaError', AMsg);
end;

{**********************************************}
procedure TForm1.ShowRequestPermissionRationale(
            const AToRequestCameraPermission: Boolean;
            const AToRequestMicPermission: Boolean;
            const ACanRequestPermissionProc: TProc;
            const ACanNotRequestPermissionProc: TProc);
begin
  var LTitle: String;
  var LMessage: string;
  if AToRequestCameraPermission and AToRequestMicPermission then begin
    LTitle := 'Camera & Microphone Access';
    LMessage := 'To continue, permission is required for the camera and microphone. Please enable them in the app’s settings.';
  end
  else if AToRequestCameraPermission then begin
    LTitle := 'Camera Access';
    LMessage   := 'To continue, permission is required for the camera. Please enable it in the app’s settings.';
  end
  else if AToRequestMicPermission then begin
    LTitle := 'Microphone Access';
    LMessage   := 'To continue, permission is required for the microphone. Please enable it in the app’s settings.';
  end
  else
    ACanRequestPermissionProc();

  TALDialog.Builder
    .SetHeadlineText(LTitle)
    .SetMessageText(LMessage)
    .addButton('Settings', 1{ATag}, True{AIsFooterButton})
    .addButton('Cancel', 0{ATag})
    .SetOnActionCallback(
       procedure(Const ADialog: TALDialog; const AAction: Integer; var ACanClose: Boolean)
       begin
         If AAction = 1 then ACanRequestPermissionProc
         else ACanNotRequestPermissionProc;
       end)
    .Show;
end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.