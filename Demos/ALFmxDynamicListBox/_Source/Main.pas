unit Main;

{$I Alcinoe.inc}

interface

uses
  System.Messaging,
  System.Generics.Collections,
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.ImageList,
  System.Rtti,
  System.Math.Vectors,
  FMX.Platform,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Controls.Presentation,
  FMX.Objects,
  FMX.Layouts,
  FMX.types3D,
  FMX.ImgList,
  FMX.ScrollBox,
  FMX.Edit,
  FMX.Effects,
  FMX.Filter.Effects,
  FMX.Ani,
  Alcinoe.FMX.Styles,
  Alcinoe.FMX.Objects,
  Alcinoe.FMX.Layouts,
  Alcinoe.FMX.Common,
  Alcinoe.FMX.StdCtrls,
  Alcinoe.FMX.PageController,
  Alcinoe.FMX.Edit,
  Alcinoe.FMX.VideoPlayer,
  Alcinoe.FMX.DatePickerDialog,
  Alcinoe.FMX.ErrorReporting,
  Alcinoe.FMX.Dynamic.ListBox,
  Alcinoe.FMX.BreakText,
  Alcinoe.FMX.Graphics,
  Alcinoe.FMX.Ani,
  Alcinoe.FMX.Memo,
  Alcinoe.FMX.Controls,
  Alcinoe.JSONDoc,
  Alcinoe.GuardianThread,
  Alcinoe.FMX.Dynamic.Controls;

type


  {**********************}
  TMainForm = class(TForm)
    MainListBox: TALDynamicListBox;
    procedure FormCreate(Sender: TObject);
    procedure MainListBoxDownloadItems(
                const AContext: TALDynamicListBox.TView.TDownloadItemsContext;
                out AData: TALJSONNodeW;
                var APaginationToken: string;
                var AErrorCode: String);
    function MainListBoxCreateLoadingContent(const AContext: TALDynamicListBox.TView.TContentBuilderContext): TALDynamicListBox.TView.TLoadingContent;
    function MainListBoxCreateItemMainContent(const AContext: TALDynamicListBox.TItem.TContentBuilderContext): TALDynamicListBox.TItem.TMainContent;
    function MainListBoxCreateTopBar(const AContext: TALDynamicListBox.TView.TContentBuilderContext): TALDynamicListBox.TView.TTopBar;
    function MainListBoxCreateBottomBar(const AContext: TALDynamicListBox.TView.TContentBuilderContext): TALDynamicListBox.TView.TBottomBar;
    function MainListBoxCreateItem(const AContext: TALDynamicListBox.TView.TDownloadItemsContext; var AData: TALJSONNodeW): TALDynamicListBox.TItem;
    function MainListBoxCreateLoadMoreIndicator(const AContext: TALDynamicListBox.TView.TContentBuilderContext): TALDynamicListBox.TView.TLoadMoreIndicator;
    function MainListBoxCreateLoadMoreRetryButton(const AContext: TALDynamicListBox.TView.TContentBuilderContext): TALDynamicListBox.TView.TLoadMoreRetryButton;
    function MainListBoxCreatePullToRefreshIndicator(const AContext: TALDynamicListBox.TView.TContentBuilderContext): TALDynamicListBox.TView.TBasePullToRefreshIndicator;
  private
    {$IF defined(ALUIAutomationEnabled)}
    FSimulateInfiniteScrollCurrentPoint: TPointF;
    procedure SimulateInfiniteScroll;
    {$ENDIF}
    {$IF defined(android)}
    procedure UpdateSystemBarsAppearance;
    function ApplicationEventHandler(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;
    {$ENDIF}
    procedure StoriesCarouselDownloadItems(
                const AContext: TALDynamicListBox.TView.TDownloadItemsContext;
                out AData: TALJSONNodeW;
                var APaginationToken: string;
                var AErrorCode: String);
    function StoriesCarouselCreateItemMainContent(const AContext: TALDynamicListBox.TItem.TContentBuilderContext): TALDynamicListBox.TItem.TMainContent;
    procedure SuggestedCarouselDownloadItems(
                const AContext: TALDynamicListBox.TView.TDownloadItemsContext;
                out AData: TALJSONNodeW;
                var APaginationToken: string;
                var AErrorCode: String);
    function SuggestedCarouselCreateItemMainContent(const AContext: TALDynamicListBox.TItem.TContentBuilderContext): TALDynamicListBox.TItem.TMainContent;
    function CarouselCreateLoadMoreIndicator(const AContext: TALDynamicListBox.TView.TContentBuilderContext): TALDynamicListBox.TView.TLoadMoreIndicator;
    function CarouselCreateLoadMoreRetryButton(const AContext: TALDynamicListBox.TView.TContentBuilderContext): TALDynamicListBox.TView.TLoadMoreRetryButton;
    procedure TextEllipsisElementClick(Sender: TObject; const Element: TALTextElement);
    procedure TextEllipsisElementMouseEnter(Sender: TObject; const Element: TALTextElement);
    procedure TextEllipsisElementMouseLeave(Sender: TObject; const Element: TALTextElement);
    procedure PageControllerActivePageChanged(Sender: TObject);
    procedure BottomBarResized(Sender: TObject);
    procedure RetryButtonClick(Sender: TObject);
    procedure FollowButtonClick(Sender: TObject);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  {$IF defined(Android)}
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.Helpers,
  Androidapi.JNI.App,
  {$ENDIF}
  {$IF defined(ios)}
  iOSapi.UIKit,
  iOSapi.CocoaTypes,
  {$ENDIF}
  System.Net.URLClient,
  system.Diagnostics,
  system.threading,
  system.Math,
  system.DateUtils,
  System.Character,
  fmx.DialogService,
  Alcinoe.fmx.Dynamic.Objects,
  Alcinoe.fmx.Dynamic.PageController,
  Alcinoe.fmx.Dynamic.Layouts,
  Alcinoe.fmx.Dynamic.StdCtrls,
  Alcinoe.fmx.Dynamic.VideoPlayer,
  Alcinoe.Cipher,
  Alcinoe.StringUtils,
  Alcinoe.FMX.ScrollEngine,
  Alcinoe.HTTP.Client.Net,
  Alcinoe.Common;

{**********************************************}
procedure TMainForm.FormCreate(Sender: TObject);
begin
  TALErrorReporting.Instance;
  TALGuardianThread.Instance;
  ALGlobalClickSoundEnabled := True;
  {$IF defined(DEBUG)}
  ALInitHasTouchScreen;
  ALHasTouchScreen := true;
  {$ENDIF}

  TALStyleManager.Instance.DarkModeBehavior := TALStyleManager.TDarkModeBehavior.AlwaysLight;

  {$IF defined(ALUIAutomationEnabled)}
  TThread.ForceQueue(nil,
    procedure
    begin
      SimulateInfiniteScroll;
    end, 5000);
  {$ENDIF}

  {$IF defined(android)}

  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-3207 has been resolved. If resolved, remove the code below.'}
  {$ENDIF}
  var LApplicationEventService: IFMXApplicationEventService;
  if TPlatformServices.Current.SupportsPlatformService(IFMXApplicationEventService, IInterface(LApplicationEventService)) then
    LApplicationEventService.SetApplicationEventHandler(ApplicationEventHandler);
  UpdateSystemBarsAppearance;

  {$ENDIF}
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported123}
  {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-3207 has been resolved. If resolved, remove the code below.'}
{$ENDIF}
{$IF defined(android)}
procedure TMainForm.UpdateSystemBarsAppearance;
begin
  if TOSVersion.Check(11{API level 30}) then begin
    var LWindow := TAndroidHelper.Activity.getWindow;
    LWindow.setNavigationBarColor(integer(TAlphaColors.White));
    LWindow.setStatusBarColor(integer(TAlphaColors.White));
    var LInsetsController: JWindowInsetsController := LWindow.getInsetsController;
    if LInsetsController <> nil then
      LInsetsController.setSystemBarsAppearance(
        TJWindowInsetsController.JavaClass.APPEARANCE_LIGHT_STATUS_BARS or
        TJWindowInsetsController.JavaClass.APPEARANCE_LIGHT_NAVIGATION_BARS,
        TJWindowInsetsController.JavaClass.APPEARANCE_LIGHT_STATUS_BARS or
        TJWindowInsetsController.JavaClass.APPEARANCE_LIGHT_NAVIGATION_BARS);
  end
  else begin
    TThread.ForceQueue(nil,
      procedure
      begin
        var LWindow := TAndroidHelper.Activity.getWindow;
        LWindow.setNavigationBarColor(integer(TAlphaColors.White));
        LWindow.setStatusBarColor(integer(TAlphaColors.White));
        LWindow.getDecorView.setSystemUiVisibility(
          TJView.JavaClass.SYSTEM_UI_FLAG_LIGHT_STATUS_BAR or
          TJView.JavaClass.SYSTEM_UI_FLAG_LIGHT_NAVIGATION_BAR);
      end, 500);
  end;
end;
{$ENDIF}

{*************************************}
{$IFNDEF ALCompilerVersionSupported123}
  {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-3207 has been resolved. If resolved, remove the code below.'}
{$ENDIF}
{$IF defined(android)}
function TMainForm.ApplicationEventHandler(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;
begin
  if AAppEvent = TApplicationEvent.BecameActive then UpdateSystemBarsAppearance;
  Result := True;
end;
{$ENDIF}

{*******************************************************************************************}
procedure TMainForm.TextEllipsisElementClick(Sender: TObject; const Element: TALTextElement);
begin
  If Element.Id = 'ellipsis' then
    TALDynamicText(Sender).TextSettings.MaxLines := 65535;
end;

{************************************************************************************************}
procedure TMainForm.TextEllipsisElementMouseEnter(Sender: TObject; const Element: TALTextElement);
begin
  If Element.Id = 'ellipsis' then
    TALDynamicText(Sender).Cursor := crHandPoint;
end;

{************************************************************************************************}
procedure TMainForm.TextEllipsisElementMouseLeave(Sender: TObject; const Element: TALTextElement);
begin
  If Element.Id = 'ellipsis' then
    TALDynamicText(Sender).Cursor := crDefault;
end;

{*******************************************************************}
procedure TMainForm.PageControllerActivePageChanged(Sender: TObject);
begin
  Var LPageController := TALDynamicPageController(Sender);
  TALDynamicText(LPageController['PageCountText']).Text := AlIntToStrW(LPageController.ActivePageIndex+1)+'/'+AlIntToStrW(LPageController.PageCount);
end;

{****************************************************}
procedure TMainForm.BottomBarResized(Sender: TObject);
begin
  with TALDynamicListBox.TView.TBottomBar(Sender) do begin
    var Lbutton1 := Controls[0];
    var Lbutton2 := Controls[1];
    var Lbutton3 := Controls[2];
    var Lbutton4 := Controls[3];
    var Lbutton5 := Controls[4];
    var LMargin: Single := (Width - Lbutton1.Width - Lbutton2.Width - Lbutton3.Width - Lbutton4.Width - Lbutton5.Width) / 10;
    Lbutton1.Margins.Rect := TRectF.Create(LMargin, 0, LMargin, 0);
    Lbutton2.Margins.Rect := TRectF.Create(LMargin, 0, LMargin, 0);
    Lbutton3.Margins.Rect := TRectF.Create(LMargin, 0, LMargin, 0);
    Lbutton4.Margins.Rect := TRectF.Create(LMargin, 0, LMargin, 0);
    Lbutton5.Margins.Rect := TRectF.Create(LMargin, 0, LMargin, 0);
  end;
end;

{********************************************************************************************************************************************************}
function TMainForm.MainListBoxCreateItem(const AContext: TALDynamicListBox.TView.TDownloadItemsContext; var AData: TALJSONNodeW): TALDynamicListBox.TItem;
begin
  if AData.GetChildNodeValueText('type', '') = 'stories' then begin
    var Lview := TALDynamicListBox.TView.Create(nil);
    Lview.Orientation := TOrientation.Horizontal;
    Lview.Height := 132;
    Lview.OnDownloadItems := StoriesCarouselDownloadItems;
    Lview.OnCreateItemMainContent := StoriesCarouselCreateItemMainContent;
    Lview.OnCreateLoadMoreIndicator := CarouselCreateLoadMoreIndicator;
    Lview.OnCreateLoadMoreRetryButton := CarouselCreateLoadMoreRetryButton;
    Result := LView;
  end
  else if AData.GetChildNodeValueText('type', '') = 'suggested' then begin
    var Lview := TALDynamicListBox.TView.Create(nil);
    Lview.Orientation := TOrientation.Horizontal;
    Lview.Height := 255;
    Lview.OnDownloadItems := SuggestedCarouselDownloadItems;
    Lview.OnCreateItemMainContent := SuggestedCarouselCreateItemMainContent;
    Lview.OnCreateLoadMoreIndicator := CarouselCreateLoadMoreIndicator;
    Lview.OnCreateLoadMoreRetryButton := CarouselCreateLoadMoreRetryButton;
    LView.Margins.bottom := 23;
    LView.ScrollEngine.Friction := 0.04;
    Result := LView;
  end
  else begin
    Result := TALDynamicListBox.TItem.Create(nil);
    Result.OnCreateMainContent := MainListBoxCreateItemMainContent;
  end;
end;

{**********************************************************************************************************************************************************}
function TMainForm.MainListBoxCreateLoadingContent(const AContext: TALDynamicListBox.TView.TContentBuilderContext): TALDynamicListBox.TView.TLoadingContent;
begin
  Result := TALDynamicListBox.TView.TLoadingContent.Create(nil);
  Try

    Result.BoundsRect := AContext.TargetRect;
    Result.Animation.Kind := TALDynamicListBox.TItem.TLoadingContent.TAnimation.TAnimationKind.Wave;
    Result.Animation.WaveColor := $1C707070;

    Var LCurrHeight: Single := 0;
    While True do begin

      if LCurrHeight > Result.Height then break;
      var LLayout1 := TALDynamicLayout.Create(Result);
      LLayout1.Align := TALAlignLayout.Top;
      LLayout1.Height := 30;
      LLayout1.Margins.Top := 12;
      LLayout1.Margins.left := 16;
      LCurrHeight := LCurrHeight + LLayout1.Height;

      var LCircle1 := TALDynamicCircle.Create(LLayout1);
      LCircle1.Fill.Color := $1C000000;
      LCircle1.Stroke.Color := TAlphaColors.Null;
      LCircle1.Align := TALAlignLayout.left;
      LCircle1.Width := 30;

      var LLayout2 := TALDynamicLayout.Create(LLayout1);
      LLayout2.Align := TALAlignLayout.client;
      LLayout2.Margins.left := 12;

      var LRectangle1 := TALDynamicRectangle.Create(LLayout2);
      LRectangle1.Fill.Color := $1C000000;
      LRectangle1.Stroke.Color := TAlphaColors.Null;
      LRectangle1.Align := TALAlignLayout.topleft;
      LRectangle1.Height := 12;
      LRectangle1.Width := 100;

      var LRectangle2 := TALDynamicRectangle.Create(LLayout2);
      LRectangle2.Fill.Color := $1C000000;
      LRectangle2.Stroke.Color := TAlphaColors.Null;
      LRectangle2.Align := TALAlignLayout.topleft;
      LRectangle2.Margins.Top := 6;
      LRectangle2.Height := 12;
      LRectangle2.Width := 180;

      if LCurrHeight > Result.Height then break;
      var LRectangle3 := TALDynamicRectangle.Create(Result);
      LRectangle3.Fill.Color := $1C000000;
      LRectangle3.Stroke.Color := TAlphaColors.Null;
      LRectangle3.Align := TALAlignLayout.top;
      LRectangle3.Margins.Top := 12;
      LRectangle3.Height := 300;
      LCurrHeight := LCurrHeight + LRectangle3.Height;

      if LCurrHeight > Result.Height then break;
      var LRectangle4 := TALDynamicRectangle.Create(Result);
      LRectangle4.Fill.Color := $1C000000;
      LRectangle4.Stroke.Color := TAlphaColors.Null;
      LRectangle4.Align := TALAlignLayout.top;
      LRectangle4.Margins.left := 16;
      LRectangle4.Margins.right := 16;
      LRectangle4.Margins.Top := 12;
      LRectangle4.Height := 12;
      LCurrHeight := LCurrHeight + LRectangle4.Height;

      if LCurrHeight > Result.Height then break;
      var LRectangle5 := TALDynamicRectangle.Create(Result);
      LRectangle5.Fill.Color := $1C000000;
      LRectangle5.Stroke.Color := TAlphaColors.Null;
      LRectangle5.Align := TALAlignLayout.top;
      LRectangle5.Margins.left := 16;
      LRectangle5.Margins.right := 16;
      LRectangle5.Margins.Top := 8;
      LRectangle5.Height := 12;
      LCurrHeight := LCurrHeight + LRectangle5.Height;

      if LCurrHeight > Result.Height then break;
      var LRectangle6 := TALDynamicRectangle.Create(Result);
      LRectangle6.Fill.Color := $1C000000;
      LRectangle6.Stroke.Color := TAlphaColors.Null;
      LRectangle6.Align := TALAlignLayout.topleft;
      LRectangle6.Margins.left := 16;
      LRectangle6.Margins.right := 16;
      LRectangle6.Margins.Top := 8;
      LRectangle6.Margins.bottom := 12;
      LRectangle6.Height := 12;
      LRectangle6.width := 150;
      LCurrHeight := LCurrHeight + LRectangle6.Height;

    end;

  Except
    ALFreeAndNil(Result);
    Raise;
  End;
end;

{****************************************************************************************************************************************************************}
function TMainForm.MainListBoxCreateLoadMoreIndicator(const AContext: TALDynamicListBox.TView.TContentBuilderContext): TALDynamicListBox.TView.TLoadMoreIndicator;
begin
  Result := TALDynamicListBox.TView.TLoadMoreIndicator.Create(nil);
  try
    Result.BoundsRect := AContext.TargetRect;
    Result.Margins.Top := -27;

    var LAniIndicator := TALDynamicAniIndicator.Create(Result);
    LAniIndicator.Align := TALAlignLayout.TopCenter;
    LAniIndicator.Margins.Rect := TRectF.Create(0{Left}, 27{Top}, 0{Right}, 27{Bottom});
    LAniIndicator.ResourceName := 'ring_anim_thin';
    LAniIndicator.SetSize(36,36);
    LAniIndicator.Animation.Enabled := True;

  Except
    ALFreeAndNil(Result);
    Raise;
  End;
end;

{********************************************************************************************************************************************************************}
function TMainForm.MainListBoxCreateLoadMoreRetryButton(const AContext: TALDynamicListBox.TView.TContentBuilderContext): TALDynamicListBox.TView.TLoadMoreRetryButton;
begin
  Result := TALDynamicListBox.TView.TLoadMoreRetryButton.Create(nil);
  try
    Result.BoundsRect := AContext.TargetRect;
    Result.Margins.Top := -27;

    var LRetryButton := TALDynamicButton.Create(Result);
    TALStyleManager.Instance.ApplyDynamicButtonIconStyle('Material3.Button.Icon.Outlined', LRetryButton, 50);
    LRetryButton.Align := TALAlignLayout.TopCenter;
    LRetryButton.Fill.ResourceName := 'refresh_thin';
    LRetryButton.Fill.ImageMargins.Rect := TRectF.Create(12,12,12,12);
    LRetryButton.Margins.Rect := TRectF.Create(0{Left}, 20{Top}, 0{Right}, 20{Bottom});
    LRetryButton.TagObject := AContext.Owner;
    LRetryButton.OnClick := RetryButtonClick;

  Except
    ALFreeAndNil(Result);
    Raise;
  End;
end;

{******************************************************************************************************************************************************************************}
function TMainForm.MainListBoxCreatePullToRefreshIndicator(const AContext: TALDynamicListBox.TView.TContentBuilderContext): TALDynamicListBox.TView.TBasePullToRefreshIndicator;
begin
  Result := TALDynamicListBox.TView.TPullToRefreshIndicator.Create(nil);
  try
    Result.Fill.Color := TalphaColors.White;
    Result.Shadow.Color := ALSetColorAlpha(TAlphaColors.Black, 0.18);
    Result.Shadow.blur := 6;
    Result.Shadow.OffsetY := 1;
    with TALDynamicListBox.TView.TPullToRefreshIndicator(Result) do begin
      PullingPhaseAniIndicator.ResourceName := 'refresh_thick';
      PullingPhaseAniIndicator.TintColor := $FF0d1014;
      PullingPhaseAniIndicator.Margins.Rect := TRectF.Create(10,10,10,10);
      RefreshingPhaseAniIndicator.ResourceName := 'ring_anim_thick';
      RefreshingPhaseAniIndicator.Margins.Rect := TRectF.Create(10,10,10,10);
    end;
  Except
    ALFreeAndNil(Result);
    Raise;
  End;
end;

{****************************************************}
procedure TMainForm.RetryButtonClick(Sender: TObject);
begin
  var LRetryButton := TALDynamicButton(Sender);
  TALDynamicListBox.TView(LRetryButton.TagObject).RetryDownloadItems;
end;

{*****************************************************}
procedure TMainForm.FollowButtonClick(Sender: TObject);
begin
  var LFollowButton := TALDynamicButton(Sender);
  if LFollowButton.Text = 'Follow' then begin
    LFollowButton.Fill.Color := $ff66bd2b;
    LFollowButton.Text := 'Following';
    LFollowButton.CacheIndex := 13;
  end
  else begin
    LFollowButton.Fill.Color := $FF4193ef;
    LFollowButton.Text := 'Follow';
    LFollowButton.CacheIndex := 12;
  end;
end;

{******************************************************************************************************************************************}
function TMainForm.MainListBoxCreateTopBar(const AContext: TALDynamicListBox.TView.TContentBuilderContext): TALDynamicListBox.TView.TTopBar;
begin
  Result := TALDynamicListBox.TView.TTopBar.Create(nil);
  try
    Result.BoundsRect := AContext.TargetRect;
    Result.Height := 56;
    Result.Fill.Color := TalphaColors.White;

    var LTitle := TALDynamicText.Create(Result);
    LTitle.Align := TALAlignLayout.LeftCenter;
    LTitle.TextSettings.Font.Size := 22;
    LTitle.TextSettings.Font.weight := TFontWeight.Bold;
    LTitle.TextSettings.Font.Color := $FF0d1014;
    LTitle.AutoSize := TALAutoSizeMode.Both;
    LTitle.Margins.left := 18;
    LTitle.Text := 'For you';

    var LMessageBtn := TALDynamicButton.Create(Result);
    LMessageBtn.Fill.Color := TalphaColors.Null;
    LMessageBtn.Fill.ResourceName := 'message';
    LMessageBtn.Stroke.Color := TalphaColors.Null;
    LMessageBtn.Align := TALAlignLayout.RightCenter;
    LMessageBtn.Margins.right := 24;
    LMessageBtn.Height := 20;
    LMessageBtn.Width := 23;

    var LActivityBtn := TALDynamicButton.Create(Result);
    LActivityBtn.Fill.Color := TalphaColors.Null;
    LActivityBtn.Fill.ResourceName := 'activity';
    LActivityBtn.Stroke.Color := TalphaColors.Null;
    LActivityBtn.Align := TALAlignLayout.RightCenter;
    LActivityBtn.Margins.right := 21;
    LActivityBtn.Height := 22;
    LActivityBtn.Width := 24;

  Except
    ALFreeAndNil(Result);
    Raise;
  End;
end;

{************************************************************************************************************************************************}
function TMainForm.MainListBoxCreateBottomBar(const AContext: TALDynamicListBox.TView.TContentBuilderContext): TALDynamicListBox.TView.TBottomBar;
begin
  Result := TALDynamicListBox.TView.TBottomBar.Create(nil);
  try
    Result.BoundsRect := AContext.TargetRect;

    {$IF defined(IOS)}
    var LhomeIndicatorBottom: CGFloat := 0;
    var LWindow := TUIApplication.Wrap(TUIApplication.OCClass.sharedApplication).keyWindow;
    if LWindow <> nil then begin
      var LInsets := LWindow.safeAreaInsets;
      LhomeIndicatorBottom := LInsets.Bottom;
    end;
    Result.height := 48 + LhomeIndicatorBottom - 8;
    Result.padding.bottom := LhomeIndicatorBottom - 8;
    {$ELSE}
    Result.Height := 48;
    {$ENDIF}
    Result.Fill.Color := TalphaColors.white;

    var LButton1 := TALDynamicButton.Create(Result);
    LButton1.Align := TALAlignLayout.LeftCenter;
    LButton1.Fill.Color := TalphaColors.Null;
    LButton1.Fill.ResourceName := 'home';
    LButton1.Width := 22;
    LButton1.Height := 22;
    LButton1.Stroke.Color := TalphaColors.Null;

    var LButton2 := TALDynamicButton.Create(Result);
    LButton2.Align := TALAlignLayout.LeftCenter;
    LButton2.Fill.Color := TalphaColors.Null;
    LButton2.Fill.ResourceName := 'search';
    LButton2.Width := 22;
    LButton2.Height := 22;
    LButton2.Stroke.Color := TalphaColors.Null;

    var LButton3 := TALDynamicButton.Create(Result);
    LButton3.Align := TALAlignLayout.LeftCenter;
    LButton3.Fill.Color := TalphaColors.Null;
    LButton3.Fill.ResourceName := 'add';
    LButton3.Width := 22;
    LButton3.Height := 22;
    LButton3.Stroke.Color := TalphaColors.Null;

    var LButton4 := TALDynamicButton.Create(Result);
    LButton4.Align := TALAlignLayout.LeftCenter;
    LButton4.Fill.Color := TalphaColors.Null;
    LButton4.Fill.ResourceName := 'video';
    LButton4.Width := 22;
    LButton4.Height := 22;
    LButton4.Stroke.Color := TalphaColors.Null;

    var LButton5 := TALDynamicButton.Create(Result);
    LButton5.Align := TALAlignLayout.LeftCenter;
    LButton5.Fill.Color := TalphaColors.Null;
    LButton5.Fill.ResourceName := 'profile';
    LButton5.Width := 26;
    LButton5.Height := 26;
    LButton5.Stroke.Color := TalphaColors.Null;

    Result.OnResized := BottomBarResized;
    BottomBarResized(Result);

  Except
    ALFreeAndNil(Result);
    Raise;
  End;
end;

{********************************************************************************************************************************************************}
function TMainForm.MainListBoxCreateItemMainContent(const AContext: TALDynamicListBox.TItem.TContentBuilderContext): TALDynamicListBox.TItem.TMainContent;
begin
  Result := TALDynamicListBox.TItem.TMainContent.Create(nil);
  Try

    Result.BoundsRect := AContext.TargetRect;

    Var LLayout1 := TALDynamicLayout.Create(Result);
    LLayout1.Align := TALAlignLayout.Top;
    LLayout1.Height := 38;

    var LRainbowCircle := TALDynamicImage.Create(LLayout1);
    LRainbowCircle.WrapMode := TALImageWrapMode.Fit;
    LRainbowCircle.ResourceName := 'rainbowcircle';
    LRainbowCircle.Align := TALAlignLayout.LeftCenter;
    LRainbowCircle.Margins.Left := 15;
    LRainbowCircle.Height := 38;
    LRainbowCircle.Width := 38;
    LRainbowCircle.CacheIndex := 1;
    LRainbowCircle.CacheEngine := AContext.CacheEngine;

    var LAvatar := TALDynamicImage.Create(LRainbowCircle);
    LAvatar.WrapMode := TALImageWrapMode.FitAndCrop;
    LAvatar.ResourceName := AContext.Owner.Data.GetChildNodeValueText('profile_pic_url', '');
    {$IF defined(debug)}
    LAvatar.TagString := 'Avatar_'+ALIntToStrW(AContext.Owner.index);
    {$ENDIF}
    LAvatar.Align := TALAlignLayout.Center;
    LAvatar.Height := 30;
    LAvatar.Width := 30;
    LAvatar.XRadius := -50;
    LAvatar.yRadius := -50;
    LAvatar.LoadingCacheIndex := 2;
    LAvatar.CacheEngine := AContext.CacheEngine;

    var LMenuBtn := TALDynamicButton.Create(LLayout1);
    LMenuBtn.Fill.Color := TalphaColors.Null;
    LMenuBtn.Fill.ResourceName := 'menu';
    LMenuBtn.Stroke.Color := TalphaColors.Null;
    LMenuBtn.Align := TALAlignLayout.RightCenter;
    LMenuBtn.Margins.right := 14;
    LMenuBtn.Height := 16;
    LMenuBtn.Width := 4;
    LMenuBtn.CacheIndex := 3;
    LMenuBtn.CacheEngine := AContext.CacheEngine;

    Var LLayout2 := TALDynamicLayout.Create(LLayout1);
    LLayout2.Align := TALAlignLayout.LeftCenter;
    LLayout2.Margins.Left := 13;
    LLayout2.AutoSize := TALAutoSizeMode.Both;

    var LUsername := TALDynamicText.Create(LLayout2);
    LUsername.Align := TALAlignLayout.topLeft;
    LUsername.TextSettings.Font.Size := 14;
    LUsername.TextSettings.Font.weight := TFontWeight.Medium;
    LUsername.TextSettings.Font.Color := $FF262626;
    LUsername.AutoSize := TALAutoSizeMode.Both;
    LUsername.Text := {$IF defined(debug)}{'['+ALIntToStrW(AContext.Owner.index)+']'+}{$ENDIF}AContext.Owner.Data.GetChildNodeValueText('username', '') ;

    var LGeotag := TALDynamicText.Create(LLayout2);
    LGeotag.Align := TALAlignLayout.topLeft;
    LGeotag.Margins.Top := 4;
    LGeotag.TextSettings.Font.Size := 12;
    LGeotag.TextSettings.Font.weight := TFontWeight.Regular;
    LGeotag.TextSettings.Font.Color := $FF262626;
    LGeotag.AutoSize := TALAutoSizeMode.Both;
    LGeotag.Text := AContext.Owner.Data.GetChildNodeValueText('geotag', '');

    var LmediaNode := AContext.Owner.Data.GetChildNode('media');
    If LmediaNode.ChildNodes.Count > 1 then begin
      var LPageController := TALDynamicPageController.Create(Result);
      LPageController.Align := TALAlignLayout.Top;
      LPageController.Height := (Result.Width / LmediaNode.ChildNodes[0].GetChildNodeValueInt32('width', 0)) * LmediaNode.ChildNodes[0].GetChildNodeValueInt32('height', 0);
      LPageController.Margins.Top := 11;
      LPageController.OnActivePageChanged := PageControllerActivePageChanged;
      for var I := 0 to LmediaNode.ChildNodes.Count - 1 do begin
        var LPageView := LPageController.AddPage;
        var LMediumNode := LmediaNode.ChildNodes[I];
        if LMediumNode.GetChildNodeValueBool('is_video', false) then begin
          var LVideoPlayerSurface1 := TALDynamicVideoPlayerSurface.Create(LPageView);
          LVideoPlayerSurface1.Align := TALAlignLayout.Client;
          LVideoPlayerSurface1.PreviewResourceName := LMediumNode.GetChildNodeValueText('preview_url', '');
          {$IF defined(debug)}
          LVideoPlayerSurface1.TagString := 'Media_'+ALIntToStrW(AContext.Owner.index) + '_' + ALIntToStrW(I);
          {$ENDIF}
          LVideoPlayerSurface1.Looping := true;
          LVideoPlayerSurface1.AutoStartMode := TALDynamicVideoPlayerSurface.TAutoStartMode.WhenDisplayed;
          LVideoPlayerSurface1.DataSource := LMediumNode.GetChildNodeValueText('url', '');
        end
        else begin
          var LMedia1 := TALDynamicImage.Create(LPageView);
          LMedia1.WrapMode := TALImageWrapMode.FitAndCrop;
          LMedia1.ResourceName := LMediumNode.GetChildNodeValueText('url', '');
          {$IF defined(debug)}
          LMedia1.TagString := 'Media_'+ALIntToStrW(AContext.Owner.index) + '_' + ALIntToStrW(I);
          {$ENDIF}
          LMedia1.Align := TALAlignLayout.client;
        end;
      end;
      var LPageIndicator := TALDynamicPageIndicator.Create(Result);
      LPageIndicator.Align := TALAlignLayout.Top;
      LPageIndicator.Margins.Top := 10;
      LPageIndicator.InactiveIndicator.Width := 6;
      LPageIndicator.InactiveIndicator.Height := 6;
      LpageIndicator.InactiveIndicator.Margins.Rect := TRectF.Create(1.5,1.5,1.5,1.5);
      LpageIndicator.InactiveIndicator.Fill.Color := $FFdcdfe3;
      LpageIndicator.ActiveIndicator.Fill.Color := $FF4193ef;
      LPageIndicator.AnimationType := TALDynamicPageIndicator.TAnimationType.Slide;
      LPageIndicator.CacheIndex := 4;
      LPageIndicator.CacheEngine := AContext.CacheEngine;
      LPageController.PageIndicator := LPageIndicator;
      var LPageCountText := TALDynamicText.Create(LPageController);
      LPageCountText.Name := 'PageCountText';
      LPageCountText.Align := TALAlignLayout.TopRight;
      LPageCountText.Margins.top := 14;
      LPageCountText.Margins.right := 14;
      LPageCountText.padding.Rect := TRectF.Create(10,6,10,6);
      LPageCountText.TextSettings.Font.Size := 13;
      LPageCountText.TextSettings.Font.weight := TFontWeight.regular;
      LPageCountText.TextSettings.Font.Color := $FFffffff;
      LPageCountText.Fill.Color := $B2000000;
      LPageCountText.AutoSize := TALAutoSizeMode.Both;
      LPageCountText.XRadius := -50;
      LPageCountText.YRadius := -50;
      LPageCountText.Text := '1/'+AlIntToStrW(LPageController.PageCount);
    end
    else If LmediaNode.ChildNodes.Count = 1 then begin
      var LMediumNode := LmediaNode.ChildNodes[0];
      if LMediumNode.GetChildNodeValueBool('is_video', false) then begin
        var LVideoPlayerSurface1 := TALDynamicVideoPlayerSurface.Create(Result);
        LVideoPlayerSurface1.Margins.Top := 11;
        LVideoPlayerSurface1.Align := TALAlignLayout.top;
        LVideoPlayerSurface1.Height := (Result.Width / LMediumNode.GetChildNodeValueInt32('width', 0)) * LMediumNode.GetChildNodeValueInt32('height', 0);
        LVideoPlayerSurface1.PreviewResourceName := LMediumNode.GetChildNodeValueText('preview_url', '');
        {$IF defined(debug)}
        LVideoPlayerSurface1.TagString := 'Media_'+ALIntToStrW(AContext.Owner.index);
        {$ENDIF}
        LVideoPlayerSurface1.Looping := true;
        LVideoPlayerSurface1.AutoStartMode := TALDynamicVideoPlayerSurface.TAutoStartMode.WhenDisplayed;
        LVideoPlayerSurface1.DataSource := LMediumNode.GetChildNodeValueText('url', '');
      end
      else begin
        var LMedia1 := TALDynamicImage.Create(Result);
        LMedia1.WrapMode := TALImageWrapMode.FitAndCrop;
        LMedia1.ResourceName := LMediumNode.GetChildNodeValueText('url', '');
        {$IF defined(debug)}
        LMedia1.TagString := 'Media_'+ALIntToStrW(AContext.Owner.index);
        {$ENDIF}
        LMedia1.Margins.Top := 11;
        LMedia1.Align := TALAlignLayout.top;
        LMedia1.Height := (Result.Width / LMediumNode.GetChildNodeValueInt32('width', 0)) * LMediumNode.GetChildNodeValueInt32('height', 0);
        if AContext.Owner.Index = 1 then begin
          LLayout1.Margins.top := 12;
          LMedia1.AddControl(LLayout1);
          LUsername.TextSettings.Font.Color := $FFffffff;
          LGeotag.TextSettings.Font.Color := $FFffffff;
          LMenuBtn.Fill.ResourceName := 'menu_dark';
          LMenuBtn.CacheIndex := 5;
          LMenuBtn.CacheEngine := AContext.CacheEngine;
        end;
      end;
    end;

    Var LLayout3 := TALDynamicLayout.Create(Result);
    LLayout3.Align := TALAlignLayout.Top;
    LLayout3.Height := 23;
    LLayout3.Margins.Top := 12;
    LLayout3.Margins.bottom := 10;

    var LLikeCountBtn := TALDynamicToggleButton.Create(LLayout3);
    LLikeCountBtn.StateStyles.Unchecked.Default.Fill.Inherit := False;
    LLikeCountBtn.StateStyles.Unchecked.Default.Fill.Color := TalphaColors.Null;
    LLikeCountBtn.StateStyles.Unchecked.Default.fill.ResourceName := 'like_outline';
    LLikeCountBtn.StateStyles.checked.Default.Fill.Inherit := False;
    LLikeCountBtn.StateStyles.checked.Default.Fill.Color := TalphaColors.Null;
    LLikeCountBtn.StateStyles.checked.Default.fill.ResourceName := 'like_solid';
    LLikeCountBtn.StateStyles.Unchecked.pressed.Scale := 1.4;
    LLikeCountBtn.StateStyles.checked.pressed.Scale := 1.4;
    LLikeCountBtn.StateStyles.Transition.Duration := 0.5;
    LLikeCountBtn.StateStyles.Transition.InterpolationType := TALInterpolationType.Back;
    LLikeCountBtn.StateStyles.Transition.InterpolationParams.Overshoot := 4;
    LLikeCountBtn.Stroke.Color := TalphaColors.Null;
    LLikeCountBtn.Align := TALAlignLayout.LeftCenter;
    LLikeCountBtn.Margins.Left := 17;
    LLikeCountBtn.Height := 20;
    LLikeCountBtn.Width := 23;
    LLikeCountBtn.TouchTargetExpansion := TRectF.Create(14,14,14,14);
    LLikeCountBtn.CacheIndex := 6;
    LLikeCountBtn.CacheEngine := AContext.CacheEngine;

    var LLikeCountText := TALDynamicText.Create(LLayout3);
    LLikeCountText.Align := TALAlignLayout.LeftCenter;
    LLikeCountText.Margins.Left := 5;
    LLikeCountText.TextSettings.Font.Size := 14;
    LLikeCountText.TextSettings.Font.weight := TFontWeight.Medium;
    LLikeCountText.TextSettings.Font.Color := $FF0d1014;
    LLikeCountText.AutoSize := TALAutoSizeMode.Both;
    LLikeCountText.Text := AlIntToStrW(AContext.Owner.Data.GetChildNodeValueInt32('like_count', 0));

    var LCommentCountBtn := TALDynamicButton.Create(LLayout3);
    LCommentCountBtn.Fill.Color := TalphaColors.Null;
    LCommentCountBtn.Fill.ResourceName := 'comments';
    LCommentCountBtn.Stroke.Color := TalphaColors.Null;
    LCommentCountBtn.Align := TALAlignLayout.LeftCenter;
    LCommentCountBtn.Margins.Left := 14;
    LCommentCountBtn.Height := 22;
    LCommentCountBtn.Width := 22;
    LCommentCountBtn.CacheIndex := 7;
    LCommentCountBtn.CacheEngine := AContext.CacheEngine;

    var LCommentCountText := TALDynamicText.Create(LLayout3);
    LCommentCountText.Align := TALAlignLayout.LeftCenter;
    LCommentCountText.Margins.Left := 5;
    LCommentCountText.TextSettings.Font.Size := 14;
    LCommentCountText.TextSettings.Font.weight := TFontWeight.Medium;
    LCommentCountText.TextSettings.Font.Color := $FF0d1014;
    LCommentCountText.AutoSize := TALAutoSizeMode.Both;
    LCommentCountText.Text := AlIntToStrW(AContext.Owner.Data.GetChildNodeValueInt32('comment_count', 0));

    var LReshareCountsBtn := TALDynamicButton.Create(LLayout3);
    LReshareCountsBtn.Fill.Color := TalphaColors.Null;
    LReshareCountsBtn.Fill.ResourceName := 'message';
    LReshareCountsBtn.Stroke.Color := TalphaColors.Null;
    LReshareCountsBtn.Align := TALAlignLayout.LeftCenter;
    LReshareCountsBtn.Margins.Left := 14;
    LReshareCountsBtn.Height := 19;
    LReshareCountsBtn.Width := 22;
    LReshareCountsBtn.CacheIndex := 8;
    LReshareCountsBtn.CacheEngine := AContext.CacheEngine;

    var LReshareCountText := TALDynamicText.Create(LLayout3);
    LReshareCountText.Align := TALAlignLayout.LeftCenter;
    LReshareCountText.Margins.Left := 5;
    LReshareCountText.TextSettings.Font.Size := 14;
    LReshareCountText.TextSettings.Font.weight := TFontWeight.Medium;
    LReshareCountText.TextSettings.Font.Color := $FF0d1014;
    LReshareCountText.AutoSize := TALAutoSizeMode.Both;
    LReshareCountText.Text := AlIntToStrW(AContext.Owner.Data.GetChildNodeValueInt32('reshare_count', 0));

    var LBookmarkBtn := TALDynamicToggleButton.Create(LLayout3);
    LBookmarkBtn.StateStyles.Unchecked.Default.Fill.Inherit := False;
    LBookmarkBtn.StateStyles.Unchecked.Default.Fill.Color := TalphaColors.Null;
    LBookmarkBtn.StateStyles.Unchecked.Default.fill.ResourceName := 'bookmark_outline';
    LBookmarkBtn.StateStyles.checked.Default.Fill.Inherit := False;
    LBookmarkBtn.StateStyles.checked.Default.Fill.Color := TalphaColors.Null;
    LBookmarkBtn.StateStyles.checked.Default.fill.ResourceName := 'bookmark_solid';
    LBookmarkBtn.StateStyles.Unchecked.pressed.Scale := 1.3;
    LBookmarkBtn.StateStyles.checked.pressed.Scale := 1.3;
    LBookmarkBtn.StateStyles.Transition.Duration := 0.2;
    LBookmarkBtn.StateStyles.Transition.InterpolationType := TALInterpolationType.Quadratic;
    LBookmarkBtn.Stroke.Color := TalphaColors.Null;
    LBookmarkBtn.Align := TALAlignLayout.rightCenter;
    LBookmarkBtn.Margins.right := 19;
    LBookmarkBtn.Height := 20;
    LBookmarkBtn.Width := 18;
    LBookmarkBtn.TouchTargetExpansion := TRectF.Create(14,14,14,14);
    LBookmarkBtn.CacheIndex := 9;
    LBookmarkBtn.CacheEngine := AContext.CacheEngine;

    var LCaption := TALDynamicText.Create(Result);
    LCaption.Align := TALAlignLayout.Top;
    LCaption.Margins.Left := 14;
    LCaption.Margins.Right := 14;
    LCaption.TextSettings.Font.Size := 14;
    LCaption.TextSettings.Font.Color := $FF272727;
    LCaption.TextSettings.LineHeightMultiplier := 1.3;
    LCaption.AutoSize := TALAutoSizeMode.Both;
    LCaption.TextSettings.MaxLines := 2;
    LCaption.TextSettings.IsHtml := True;
    LCaption.TextSettings.Ellipsis := '… more';
    LCaption.TextSettings.EllipsisSettings.Inherit := False;
    LCaption.TextSettings.EllipsisSettings.Font.Size := 14;
    LCaption.TextSettings.EllipsisSettings.Font.Color := $FF70767f;
    var LStr := AContext.Owner.Data.GetChildNodeValueText('caption', '');
    var P1 := AlposW('#',LStr);
    While P1 > 0 do begin
      Insert('<font color="#306dcc">',LStr,P1);
      inc(p1, length('<font color="#306dcc">') + 1);
      while (P1 <= High(LStr)) and (LStr[P1].IsLetterOrDigit) do inc(P1);
      Insert('</font>',LStr,P1);
      P1 := AlposW('#',LStr, P1);
    end;
    P1 := AlposW('@',LStr);
    While P1 > 0 do begin
      Insert('<font color="#306dcc">',LStr,P1);
      inc(p1, length('<font color="#306dcc">') + 1);
      while (P1 <= High(LStr)) and ((LStr[P1].IsLetterOrDigit) or (LStr[P1] = '_')) do inc(P1);
      Insert('</font>',LStr,P1);
      P1 := AlposW('@',LStr, P1);
    end;
    LStr := '<b>'+AContext.Owner.Data.GetChildNodeValueText('username', '')+'</b> ' + LStr;
    LStr := ALStringReplaceW(LStr, #13#10, '<br/>', [RfReplaceAll]);
    LCaption.Text := LStr;
    LCaption.HitTest := True;
    LCaption.OnElementClick := TextEllipsisElementClick;
    LCaption.OnElementMouseEnter := TextEllipsisElementMouseEnter;
    LCaption.OnElementMouseLeave := TextEllipsisElementMouseLeave;

    var LDate := TALDynamicText.Create(Result);
    LDate.Align := TALAlignLayout.TopLeft;
    LDate.Margins.top := 7;
    LDate.Margins.Left := 14;
    LDate.Margins.Right := 14;
    LDate.Margins.bottom := 27;
    LDate.TextSettings.Font.Size := 12.5;
    LDate.TextSettings.Font.weight := TFontWeight.Regular;
    LDate.TextSettings.Font.Color := $FF71757f;
    LDate.AutoSize := TALAutoSizeMode.Both;
    LDate.Text := inttostr(1+ALRandom32(23)) + ' hours ago';
    LDate.TextSettings.IsHtml := True;

  Except
    ALFreeAndNil(Result);
    Raise;
  End;

end;

{************************************************************************************************************************************************************}
function TMainForm.StoriesCarouselCreateItemMainContent(const AContext: TALDynamicListBox.TItem.TContentBuilderContext): TALDynamicListBox.TItem.TMainContent;
begin
  Result := TALDynamicListBox.TItem.TMainContent.Create(nil);
  Try

    Result.BoundsRect := AContext.TargetRect;

    Var LLayout1 := TALDynamicLayout.Create(Result);
    LLayout1.Align := TALAlignLayout.left;
    LLayout1.width := 100;
    LLayout1.Margins.Left := 6;
    LLayout1.Margins.Left := 6;

    var LRainbowCircle := TALDynamicImage.Create(LLayout1);
    LRainbowCircle.WrapMode := TALImageWrapMode.Fit;
    LRainbowCircle.ResourceName := 'bigrainbowcircle';
    LRainbowCircle.Align := TALAlignLayout.TopCenter;
    LRainbowCircle.Margins.Top := 6;
    LRainbowCircle.Margins.bottom := 6;
    LRainbowCircle.Height := 100;
    LRainbowCircle.Width := 100;
    LRainbowCircle.CacheIndex := 20;
    LRainbowCircle.CacheEngine := AContext.CacheEngine;

    var LAvatar := TALDynamicImage.Create(LRainbowCircle);
    LAvatar.WrapMode := TALImageWrapMode.FitAndCrop;
    LAvatar.ResourceName := AContext.Owner.Data.GetChildNodeValueText('profile_pic_url', '');
    {$IF defined(debug)}
    LAvatar.TagString := 'Stories_Avatar_'+ALIntToStrW(AContext.Owner.index);
    {$ENDIF}
    LAvatar.Align := TALAlignLayout.Center;
    LAvatar.Height := 84;
    LAvatar.Width := 84;
    LAvatar.XRadius := -50;
    LAvatar.yRadius := -50;
    LAvatar.LoadingCacheIndex := 21;
    LAvatar.CacheEngine := AContext.CacheEngine;

    var LUsername := TALDynamicText.Create(LLayout1);
    LUsername.Align := TALAlignLayout.top;
    LUsername.TextSettings.Font.Size := 12;
    LUsername.TextSettings.Font.weight := TFontWeight.Regular;
    LUsername.TextSettings.Font.Color := $FF262626;
    LUsername.TextSettings.HorzAlign := TALTextHorzAlign.Center;
    LUsername.TextSettings.MaxLines := 1;
    LUsername.AutoSize := TALAutoSizeMode.Both;
    LUsername.Text := {$IF defined(debug)}{'['+ALIntToStrW(AContext.Owner.index)+']'+}{$ENDIF}AContext.Owner.Data.GetChildNodeValueText('username', '') ;

  Except
    ALFreeAndNil(Result);
    Raise;
  End;

end;

{*************************************************************************************************************************************************************}
function TMainForm.CarouselCreateLoadMoreIndicator(const AContext: TALDynamicListBox.TView.TContentBuilderContext): TALDynamicListBox.TView.TLoadMoreIndicator;
begin
  Result := TALDynamicListBox.TView.TLoadMoreIndicator.Create(nil);
  try
    Result.BoundsRect := AContext.TargetRect;

    var LAniIndicator := TALDynamicAniIndicator.Create(Result);
    LAniIndicator.Align := TALAlignLayout.leftCenter;
    LAniIndicator.Margins.Rect := TRectF.Create(27{Left}, 0{Top}, 27{Right}, 0{Bottom});
    LAniIndicator.ResourceName := 'ring_anim_thin';
    LAniIndicator.SetSize(36,36);

  Except
    ALFreeAndNil(Result);
    Raise;
  End;
end;

{*****************************************************************************************************************************************************************}
function TMainForm.CarouselCreateLoadMoreRetryButton(const AContext: TALDynamicListBox.TView.TContentBuilderContext): TALDynamicListBox.TView.TLoadMoreRetryButton;
begin
  Result := TALDynamicListBox.TView.TLoadMoreRetryButton.Create(nil);
  try
    Result.BoundsRect := AContext.TargetRect;

    var LRetryButton := TALDynamicButton.Create(Result);
    TALStyleManager.Instance.ApplyDynamicButtonIconStyle('Material3.Button.Icon.Outlined', LRetryButton, 50);
    LRetryButton.Align := TALAlignLayout.LeftCenter;
    LRetryButton.Fill.ResourceName := 'refresh_thin';
    LRetryButton.Fill.ImageMargins.Rect := TRectF.Create(12,12,12,12);
    LRetryButton.Margins.Rect := TRectF.Create(20{Left}, 0{Top}, 20{Right}, 0{Bottom});
    LRetryButton.TagObject := AContext.Owner;
    LRetryButton.OnClick := RetryButtonClick;

  Except
    ALFreeAndNil(Result);
    Raise;
  End;
end;

{**************************************************************************************************************************************************************}
function TMainForm.SuggestedCarouselCreateItemMainContent(const AContext: TALDynamicListBox.TItem.TContentBuilderContext): TALDynamicListBox.TItem.TMainContent;
begin
  Result := TALDynamicListBox.TItem.TMainContent.Create(nil);
  Try

    Result.BoundsRect := AContext.TargetRect;

    Var LBackground := TALDynamicRectangle.Create(Result);
    LBackground.Align := TALAlignLayout.left;
    LBackground.width := 220;
    LBackground.Margins.Left := 6;
    LBackground.Margins.Left := 6;
    LBackground.Stroke.Color := $FFeaedf0;
    LBackground.Stroke.Thickness := 1;
    LBackground.XRadius := 3;
    LBackground.yRadius := 3;
    LBackground.Corners := AllCorners;
    LBackground.CacheIndex := 10;
    LBackground.CacheEngine := AContext.CacheEngine;

    var LAvatar := TALDynamicImage.Create(LBackground);
    LAvatar.WrapMode := TALImageWrapMode.FitAndCrop;
    LAvatar.ResourceName := AContext.Owner.Data.GetChildNodeValueText('profile_pic_url', '');
    {$IF defined(debug)}
    LAvatar.TagString := 'Suggested_Avatar_'+ALIntToStrW(AContext.Owner.index);
    {$ENDIF}
    LAvatar.Align := TALAlignLayout.topCenter;
    LAvatar.Height := 148;
    LAvatar.Width := 148;
    LAvatar.XRadius := -50;
    LAvatar.yRadius := -50;
    LAvatar.Margins.top := 16;
    LAvatar.LoadingCacheIndex := 11;
    LAvatar.CacheEngine := AContext.CacheEngine;

    var LUsername := TALDynamicText.Create(LBackground);
    LUsername.Align := TALAlignLayout.topcenter;
    LUsername.TextSettings.Font.Size := 14;
    LUsername.TextSettings.Font.weight := TFontWeight.medium;
    LUsername.TextSettings.Font.Color := $FF0d1014;
    LUsername.AutoSize := TALAutoSizeMode.Both;
    LUsername.TextSettings.MaxLines := 1;
    LUsername.Text := {$IF defined(debug)}{'['+ALIntToStrW(AContext.Owner.index)+']'+}{$ENDIF}AContext.Owner.Data.GetChildNodeValueText('username', '') ;
    LUsername.Margins.top := 12;

    var LFollowButton := TALDynamicButton.Create(LBackground);
    LFollowButton.Fill.Color := $FF4193ef;
    LFollowButton.Stroke.Color := TalphaColors.Null;
    LFollowButton.TextSettings.font.Color := $FFffffff;
    LFollowButton.Align := TALAlignLayout.top;
    LFollowButton.Text := 'Follow';
    LFollowButton.Padding.top := 8;
    LFollowButton.Padding.bottom := 8;
    LFollowButton.Margins.Left := 12;
    LFollowButton.Margins.right := 12;
    LFollowButton.Margins.top := 12;
    LFollowButton.Margins.bottom := 12;
    LFollowButton.XRadius := 6;
    LFollowButton.YRadius := 6;
    LFollowButton.CacheIndex := 12; // and 13 in FollowButtonClick
    LFollowButton.CacheEngine := AContext.CacheEngine;
    LFollowButton.OnClick := FollowButtonClick;

    var LCloseButton := TALDynamicButton.Create(LBackground);
    LCloseButton.Fill.Color := TalphaColors.Null;
    LCloseButton.Fill.ResourceName := 'cross';
    LCloseButton.Stroke.Color := TalphaColors.Null;
    LCloseButton.Top := 11;
    LCloseButton.left := 199;
    LCloseButton.Height := 11;
    LCloseButton.Width := 11;
    LCloseButton.CacheIndex := 14;
    LCloseButton.CacheEngine := AContext.CacheEngine;

  Except
    ALFreeAndNil(Result);
    Raise;
  End;

end;

{*******************************************}
procedure TMainForm.MainListBoxDownloadItems(
            const AContext: TALDynamicListBox.TView.TDownloadItemsContext;
            out AData: TALJSONNodeW;
            var APaginationToken: string;
            var AErrorCode: String);
begin

  // Here, we need to download the JSON data that represents each item.
  // Since we are running in a background thread, we can safely download it from
  // the Internet without worrying about freezing the app.

  AData := nil;
  AErrorCode := '';
  try

    // PaginationToken – A string used to track the position in paginated data,
    // allowing retrieval of the next set of results.
    if APaginationToken = '' then APaginationToken := '0';

    // Here, we retrieve a mocked set of data from the the Internet.
    var LNetHttpClient := ALCreateNetHTTPClient;
    var LResponseContent := TMemoryStream.Create;
    try
      LNetHttpClient.CustomHeaders['Accept-Encoding'] := 'identity';
      var LHTTPResponse := LNetHttpClient.Get('https://www.magicfoundation.io/media/mockup/io.magicfoundation.alcinoe.alfmxdynamiclistboxdemo/posts_'+ALIntToStrW((ALStrtoint(APaginationToken) mod 4) + 1)+'.json', LResponseContent);
      if LHTTPResponse.StatusCode <> 200 then
        raise Exception.CreateFmt('Failed to retrieve JSON data. Server responded with HTTP status code %d.', [LHTTPResponse.StatusCode]);
      AData := TALJsonDocumentW.CreateFromJSONStream(LResponseContent);
    finally
      ALFreeAndNil(LResponseContent);
      ALFreeAndNil(LNetHttpClient);
    end;

    // For demonstration purposes: when the pagination token is greater than 4,
    // randomly sort the nodes and then remove any node whose "type" property is not 'post'.
    if ALStrToInt(APaginationToken) > 4 then begin
      AData.ChildNodes.CustomSort(
        function(List: TALJSONNodeListW; Index1, Index2: Integer): Integer
        begin
          Result := random(MaxInt) - Random(Maxint);
        end);
      for var I := AData.ChildNodes.Count - 1 downto 0 do begin
        if AData.ChildNodes[i].GetChildNodeValueText('type', 'post') <> 'post' then
          AData.ChildNodes.Delete(I);
      end;
    end;

    // For the mock to work correctly, each item needs a unique ID.
    For var I := 0 to AData.ChildNodes.Count - 1 do
      AData.ChildNodes[i].SetChildNodeValueInt64('id', ALRandom64(ALMaxInt64));

    // Increment the pagination token for the next data batch.
    APaginationToken := ALInttoStrW(ALStrtoInt(APaginationToken) + 1);

    // Stories is only on the very first bulk
    if APaginationToken <> '1' then begin
      For var I := AData.ChildNodes.Count - 1 downto 0 do
        if AData.ChildNodes[i].GetChildNodeValueText('type', '') = 'stories' then
          AData.ChildNodes.Delete(i);
    end;

  Except
    On E: Exception do begin
      // In case of an error, assign a custom error code here that
      // can be used later to display an error message to the end user.
      AErrorCode := E.Message;
    end;
  end;

end;

{***********************************************}
procedure TMainForm.StoriesCarouselDownloadItems(
            const AContext: TALDynamicListBox.TView.TDownloadItemsContext;
            out AData: TALJSONNodeW;
            var APaginationToken: string;
            var AErrorCode: String);
begin

  // Here, we need to download the JSON data that represents each item.
  // Since we are running in a background thread, we can safely download it from
  // the Internet without worrying about freezing the app.

  AData := nil;
  AErrorCode := '';
  try

    // PaginationToken – A string used to track the position in paginated data,
    // allowing retrieval of the next set of results.
    if APaginationToken = '' then APaginationToken := '0';

    // Here, we retrieve a mocked set of data from the the Internet.
    var LNetHttpClient := ALCreateNetHTTPClient;
    var LResponseContent := TMemoryStream.Create;
    try
      LNetHttpClient.CustomHeaders['Accept-Encoding'] := 'identity';
      var LHTTPResponse := LNetHttpClient.Get('https://www.magicfoundation.io/media/mockup/io.magicfoundation.alcinoe.alfmxdynamiclistboxdemo/stories_'+ALIntToStrW((ALStrtoint(APaginationToken) mod 3) + 1)+'.json', LResponseContent);
      if LHTTPResponse.StatusCode <> 200 then
        raise Exception.CreateFmt('Failed to retrieve JSON data. Server responded with HTTP status code %d.', [LHTTPResponse.StatusCode]);
      AData := TALJsonDocumentW.CreateFromJSONStream(LResponseContent);
    finally
      ALFreeAndNil(LResponseContent);
      ALFreeAndNil(LNetHttpClient);
    end;

    // For demonstration purposes: when the pagination token is greater than 4,
    // randomly sort the nodes.
    if ALStrToInt(APaginationToken) > 3 then begin
      AData.ChildNodes.CustomSort(
        function(List: TALJSONNodeListW; Index1, Index2: Integer): Integer
        begin
          Result := random(MaxInt) - Random(Maxint);
        end);
    end;

    // For the mock to work correctly, each item needs a unique ID.
    For var I := 0 to AData.ChildNodes.Count - 1 do
      AData.ChildNodes[i].SetChildNodeValueInt64('id', ALRandom64(ALMaxInt64));

    // Increment the pagination token for the next data batch.
    APaginationToken := ALInttoStrW(ALStrtoInt(APaginationToken) + 1);

  Except
    On E: Exception do begin
      // In case of an error, assign a custom error code here that
      // can be used later to display an error message to the end user.
      AErrorCode := E.Message;
    end;
  end;

end;

{*************************************************}
procedure TMainForm.SuggestedCarouselDownloadItems(
            const AContext: TALDynamicListBox.TView.TDownloadItemsContext;
            out AData: TALJSONNodeW;
            var APaginationToken: string;
            var AErrorCode: String);
begin

  // Here, we need to download the JSON data that represents each item.
  // Since we are running in a background thread, we can safely download it from
  // the Internet without worrying about freezing the app.

  AData := nil;
  AErrorCode := '';
  try

    // PaginationToken – A string used to track the position in paginated data,
    // allowing retrieval of the next set of results.
    if APaginationToken = '' then APaginationToken := '0';

    // Here, we retrieve a mocked set of data from the the Internet.
    var LNetHttpClient := ALCreateNetHTTPClient;
    var LResponseContent := TMemoryStream.Create;
    try
      LNetHttpClient.CustomHeaders['Accept-Encoding'] := 'identity';
      var LHTTPResponse := LNetHttpClient.Get('https://www.magicfoundation.io/media/mockup/io.magicfoundation.alcinoe.alfmxdynamiclistboxdemo/suggested_'+ALIntToStrW((ALStrtoint(APaginationToken) mod 3) + 1)+'.json', LResponseContent);
      if LHTTPResponse.StatusCode <> 200 then
        raise Exception.CreateFmt('Failed to retrieve JSON data. Server responded with HTTP status code %d.', [LHTTPResponse.StatusCode]);
      AData := TALJsonDocumentW.CreateFromJSONStream(LResponseContent);
    finally
      ALFreeAndNil(LResponseContent);
      ALFreeAndNil(LNetHttpClient);
    end;

    // For demonstration purposes: when the pagination token is greater than 4,
    // randomly sort the nodes.
    if ALStrToInt(APaginationToken) > 3 then begin
      AData.ChildNodes.CustomSort(
        function(List: TALJSONNodeListW; Index1, Index2: Integer): Integer
        begin
          Result := random(MaxInt) - Random(Maxint);
        end);
    end;

    // For the mock to work correctly, each item needs a unique ID.
    For var I := 0 to AData.ChildNodes.Count - 1 do
      AData.ChildNodes[i].SetChildNodeValueInt64('id', ALRandom64(ALMaxInt64));

    // Increment the pagination token for the next data batch.
    APaginationToken := ALInttoStrW(ALStrtoInt(APaginationToken) + 1);

  Except
    On E: Exception do begin
      // In case of an error, assign a custom error code here that
      // can be used later to display an error message to the end user.
      AErrorCode := E.Message;
    end;
  end;

end;

{$IF defined(ALUIAutomationEnabled)}

type
  TALControlProtectedAccess=class(TControl);

{*****************************************}
procedure TMainForm.SimulateInfiniteScroll;
begin
  TThread.ForceQueue(nil,
    procedure
    begin
      var LControl := TALControlProtectedAccess(MainListBox);
      FSimulateInfiniteScrollCurrentPoint.X := 10;
      FSimulateInfiniteScrollCurrentPoint.Y := LControl.Height - 10;
      LControl.MouseDown(TMouseButton.mbLeft, [TShiftStateItem.ssLeft], FSimulateInfiniteScrollCurrentPoint.X, FSimulateInfiniteScrollCurrentPoint.Y);
    end);

  var LDelay: Integer := 5 + random(10);

  for var I := 0 to 5 + random(5) do begin
    TThread.ForceQueue(nil,
      procedure
      begin
        var LControl := TALControlProtectedAccess(MainListBox);
        FSimulateInfiniteScrollCurrentPoint.X := max(LControl.Width, FSimulateInfiniteScrollCurrentPoint.X + (Random * 5));
        FSimulateInfiniteScrollCurrentPoint.Y := max(0, FSimulateInfiniteScrollCurrentPoint.Y - (Random * 75));
        LControl.MouseMove([TShiftStateItem.ssLeft], FSimulateInfiniteScrollCurrentPoint.X, FSimulateInfiniteScrollCurrentPoint.Y);
      end, LDelay);
    LDelay := LDelay + 5 + random(10);
  end;

  TThread.ForceQueue(nil,
    procedure
    begin
      var LControl := TALControlProtectedAccess(MainListBox);
      FSimulateInfiniteScrollCurrentPoint.X := max(LControl.Width, FSimulateInfiniteScrollCurrentPoint.X + (Random * 5));
      FSimulateInfiniteScrollCurrentPoint.Y := max(0, FSimulateInfiniteScrollCurrentPoint.Y - (Random * 75));
      LControl.MouseUp(TMouseButton.mbLeft, [TShiftStateItem.ssLeft], FSimulateInfiniteScrollCurrentPoint.X, FSimulateInfiniteScrollCurrentPoint.Y);
    end, LDelay);

  LDelay := Ldelay + 3000 + random(1000);

  TThread.ForceQueue(nil,
    procedure
    begin
      SimulateInfiniteScroll;
    end, LDelay);
end;

{$ENDIF}

initialization
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.
