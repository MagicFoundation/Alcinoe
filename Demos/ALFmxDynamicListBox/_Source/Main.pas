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
  Alcinoe.FMX.Themes,
  Alcinoe.FMX.Objects,
  Alcinoe.FMX.Layouts,
  Alcinoe.FMX.Common,
  Alcinoe.FMX.StdCtrls,
  Alcinoe.FMX.TabControl,
  Alcinoe.FMX.Edit,
  Alcinoe.FMX.VideoPlayer,
  Alcinoe.FMX.DatePickerDialog,
  Alcinoe.FMX.ErrorReporting,
  Alcinoe.FMX.DynamicListBox,
  Alcinoe.FMX.BreakText,
  Alcinoe.FMX.Graphics,
  Alcinoe.FMX.Ani,
  Alcinoe.FMX.Memo,
  Alcinoe.FMX.Controls,
  Alcinoe.JSONDoc,
  Alcinoe.GuardianThread,
  System.Rtti,
  System.Math.Vectors;

type


  {**********************}
  TMainForm = class(TForm)
    MainDynamicListBox: TALDynamicListBox;
    BottomBar: TALRectangle;
    Button1: TALButton;
    Button2: TALButton;
    Button3: TALButton;
    Button4: TALButton;
    Button5: TALButton;
    procedure MainDynamicListBoxDownloadItems(
                const AContext: TALDynamicListBoxView.TDownloadItemsContext;
                out AData: TALJSONNodeW;
                var APaginationToken: string;
                var AErrorCode: Integer);
    function MainDynamicListBoxCreateMainViewLoadingContent(const AContext: TALDynamicListBoxItem.TCreateContentContext): TALDynamicListBoxItemLoadingContent;
    function MainDynamicListBoxCreateItemMainContent(const AContext: TALDynamicListBoxItem.TCreateContentContext): TALDynamicListBoxItemMainContent;
    procedure FormCreate(Sender: TObject);
    procedure BottomBarResized(Sender: TObject);
  private
    {$IF defined(ALUIAutomationEnabled)}
    FSimulateInfiniteScrollCurrentPoint: TPointF;
    procedure SimulateInfiniteScroll;
    {$ENDIF}
  protected
    procedure ALTextEllipsisElementClick(Sender: TObject; const Element: TALTextElement);
    procedure ALTextEllipsisElementMouseEnter(Sender: TObject; const Element: TALTextElement);
    procedure ALTextEllipsisElementMouseLeave(Sender: TObject; const Element: TALTextElement);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  {$IF defined(Android)}
  Androidapi.Helpers,
  Androidapi.JNI.App,
  {$ENDIF}
  system.Diagnostics,
  system.threading,
  system.Math,
  system.DateUtils,
  System.Character,
  fmx.DialogService,
  Alcinoe.Cipher,
  Alcinoe.StringUtils,
  Alcinoe.FMX.ScrollEngine,
  Alcinoe.Common;

{*********************************************************************************************}
procedure TMainForm.ALTextEllipsisElementClick(Sender: TObject; const Element: TALTextElement);
begin
  If Element.Id = 'ellipsis' then begin
    TALDynamicListBoxText(Sender).TextSettings.MaxLines := 65535;
    TALDynamicListBoxText(Sender).MaxHeight := 65535;
  end;
end;

{**************************************************************************************************}
procedure TMainForm.ALTextEllipsisElementMouseEnter(Sender: TObject; const Element: TALTextElement);
begin
  If Element.Id = 'ellipsis' then
    TALDynamicListBoxText(Sender).Cursor := crHandPoint;
end;

{**************************************************************************************************}
procedure TMainForm.ALTextEllipsisElementMouseLeave(Sender: TObject; const Element: TALTextElement);
begin
  If Element.Id = 'ellipsis' then
    TALDynamicListBoxText(Sender).Cursor := crDefault;
end;

{****************************************************}
procedure TMainForm.BottomBarResized(Sender: TObject);
begin
  var LMargin: Single := (BottomBar.Width - button1.Width - button2.Width - button3.Width - button4.Width - button5.Width) / 10;
  button1.Margins.Rect := TRectF.Create(LMargin, 0, LMargin, 0);
  button2.Margins.Rect := TRectF.Create(LMargin, 0, LMargin, 0);
  button3.Margins.Rect := TRectF.Create(LMargin, 0, LMargin, 0);
  button4.Margins.Rect := TRectF.Create(LMargin, 0, LMargin, 0);
end;

{******************************************************************************************************************************************************************}
function TMainForm.MainDynamicListBoxCreateMainViewLoadingContent(const AContext: TALDynamicListBoxItem.TCreateContentContext): TALDynamicListBoxItemLoadingContent;
begin
  Result := TALDynamicListBoxItemLoadingContent.Create(nil);
  Try

    Result.BoundsRect := AContext.TargetRect;
    Result.SkeletonAnimation.Kind := TALDynamicListBoxItemLoadingContent.TSkeletonAnimationKind.Wave;
    Result.SkeletonAnimation.WaveColor := $1C707070;

    Var LCurrHeight: Single := 0;
    While True do begin

      if LCurrHeight > Result.Height then break;
      var LLayout1 := TALDynamicListBoxLayout.Create(Result);
      LLayout1.Align := TALAlignLayout.Top;
      LLayout1.Height := 30;
      LLayout1.Margins.Top := 12;
      LLayout1.Margins.left := 16;
      LCurrHeight := LCurrHeight + LLayout1.Height;

      var LCircle1 := TALDynamicListBoxCircle.Create(LLayout1);
      LCircle1.Fill.Color := $1C000000;
      LCircle1.Stroke.Color := TAlphaColors.Null;
      LCircle1.Align := TALAlignLayout.left;
      LCircle1.Width := 30;

      var LLayout2 := TALDynamicListBoxLayout.Create(LLayout1);
      LLayout2.Align := TALAlignLayout.client;
      LLayout2.Margins.left := 12;

      var LRectangle1 := TALDynamicListBoxRectangle.Create(LLayout2);
      LRectangle1.Fill.Color := $1C000000;
      LRectangle1.Stroke.Color := TAlphaColors.Null;
      LRectangle1.Align := TALAlignLayout.topleft;
      LRectangle1.Height := 12;
      LRectangle1.Width := 100;

      var LRectangle2 := TALDynamicListBoxRectangle.Create(LLayout2);
      LRectangle2.Fill.Color := $1C000000;
      LRectangle2.Stroke.Color := TAlphaColors.Null;
      LRectangle2.Align := TALAlignLayout.topleft;
      LRectangle2.Margins.Top := 6;
      LRectangle2.Height := 12;
      LRectangle2.Width := 180;

      if LCurrHeight > Result.Height then break;
      var LRectangle3 := TALDynamicListBoxRectangle.Create(Result);
      LRectangle3.Fill.Color := $1C000000;
      LRectangle3.Stroke.Color := TAlphaColors.Null;
      LRectangle3.Align := TALAlignLayout.top;
      LRectangle3.Margins.Top := 12;
      LRectangle3.Height := 300;
      LCurrHeight := LCurrHeight + LRectangle3.Height;

      if LCurrHeight > Result.Height then break;
      var LRectangle4 := TALDynamicListBoxRectangle.Create(Result);
      LRectangle4.Fill.Color := $1C000000;
      LRectangle4.Stroke.Color := TAlphaColors.Null;
      LRectangle4.Align := TALAlignLayout.top;
      LRectangle4.Margins.left := 16;
      LRectangle4.Margins.right := 16;
      LRectangle4.Margins.Top := 12;
      LRectangle4.Height := 12;
      LCurrHeight := LCurrHeight + LRectangle4.Height;

      if LCurrHeight > Result.Height then break;
      var LRectangle5 := TALDynamicListBoxRectangle.Create(Result);
      LRectangle5.Fill.Color := $1C000000;
      LRectangle5.Stroke.Color := TAlphaColors.Null;
      LRectangle5.Align := TALAlignLayout.top;
      LRectangle5.Margins.left := 16;
      LRectangle5.Margins.right := 16;
      LRectangle5.Margins.Top := 8;
      LRectangle5.Height := 12;
      LCurrHeight := LCurrHeight + LRectangle5.Height;

      if LCurrHeight > Result.Height then break;
      var LRectangle6 := TALDynamicListBoxRectangle.Create(Result);
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

{**********************************************}
procedure TMainForm.FormCreate(Sender: TObject);
begin
  TALErrorReporting.Instance;
  TALGuardianThread.Instance;
  {$IF defined(ALUIAutomationEnabled)}
  TThread.ForceQueue(nil,
    procedure
    begin
      SimulateInfiniteScroll;
    end, 5000);
  {$ENDIF}
  BottomBarResized(nil);
  {$IF defined(android)}
  TAndroidHelper.Activity.getWindow.setNavigationBarColor(integer(TAlphaColors.White));
  {$ENDIF}
end;

{********************************************************************************************************************************************************}
function TMainForm.MainDynamicListBoxCreateItemMainContent(const AContext: TALDynamicListBoxItem.TCreateContentContext): TALDynamicListBoxItemMainContent;
begin
  Result := TALDynamicListBoxItemMainContent.Create(nil);
  Try

    Result.BoundsRect := AContext.TargetRect;

    Var LLayout1 := TALDynamicListBoxLayout.Create(Result);
    LLayout1.Name := 'Layout1';
    LLayout1.Align := TALAlignLayout.Top;
    LLayout1.Height := 38;

    var LRainbowCircle := TALDynamicListBoxImage.Create(LLayout1);
    LRainbowCircle.Name := 'RainbowCircle';
    LRainbowCircle.WrapMode := TALImageWrapMode.Fit;
    LRainbowCircle.ResourceName := 'rainbowcircle';
    LRainbowCircle.Align := TALAlignLayout.LeftCenter;
    LRainbowCircle.Margins.Left := 15;
    LRainbowCircle.Height := 38;
    LRainbowCircle.Width := 38;

    var LAvatar := TALDynamicListBoxImage.Create(LRainbowCircle);
    LAvatar.Name := 'ProfilePicture';
    LAvatar.WrapMode := TALImageWrapMode.FitAndCrop;
    LAvatar.ResourceName := AContext.Owner.Data.GetChildNodeValueText('profile_pic_url', '');
    LAvatar.Align := TALAlignLayout.Center;
    LAvatar.Height := 30;
    LAvatar.Width := 30;
    LAvatar.XRadius := -50;
    LAvatar.yRadius := -50;

    var LMenuBtn := TALDynamicListBoxButton.Create(LLayout1);
    LMenuBtn.Name := 'MenuBtn';
    LMenuBtn.Fill.Color := TalphaColors.Null;
    LMenuBtn.Fill.ResourceName := 'menu';
    LMenuBtn.Stroke.Color := TalphaColors.Null;
    LMenuBtn.CacheIndex := 1;
    LMenuBtn.CacheEngine := AContext.CacheEngine;
    LMenuBtn.Align := TALAlignLayout.RightCenter;
    LMenuBtn.Margins.right := 14;
    LMenuBtn.Height := 16;
    LMenuBtn.Width := 4;

    Var LLayout2 := TALDynamicListBoxLayout.Create(LLayout1);
    LLayout2.Name := 'Layout2';
    LLayout2.Align := TALAlignLayout.LeftCenter;
    LLayout2.Margins.Left := 13;
    LLayout2.AutoSize := True;

    var LUsername := TALDynamicListBoxText.Create(LLayout2);
    LUsername.Name := 'Username';
    LUsername.Align := TALAlignLayout.topLeft;
    LUsername.TextSettings.Font.Size := 14;
    LUsername.TextSettings.Font.weight := TFontWeight.Medium;
    LUsername.TextSettings.Font.Color := $FF262626;
    LUsername.AutoSize := True;
    LUsername.Text := AContext.Owner.Data.GetChildNodeValueText('username', '') ;

    var LGeotag := TALDynamicListBoxText.Create(LLayout2);
    LGeotag.Name := 'Geotag';
    LGeotag.Align := TALAlignLayout.topLeft;
    LGeotag.Margins.Top := 4;
    LGeotag.TextSettings.Font.Size := 12;
    LGeotag.TextSettings.Font.weight := TFontWeight.Regular;
    LGeotag.TextSettings.Font.Color := $FF262626;
    LGeotag.AutoSize := True;
    LGeotag.Text := AContext.Owner.Data.GetChildNodeValueText('geotag', '');

    var LmediaNode := AContext.Owner.Data.GetChildNode('media');
    If LmediaNode.ChildNodes.Count > 1 then begin
      var LMediumNode := LmediaNode.ChildNodes[0];
      if LMediumNode.GetChildNodeValueBool('is_video', false) then begin
        var LVideoPlayerSurface1 := TALDynamicListBoxVideoPlayerSurface.Create(Result);
        LVideoPlayerSurface1.Margins.Top := 11;
        LVideoPlayerSurface1.Align := TALAlignLayout.top;
        LVideoPlayerSurface1.Height := (Result.Width / LMediumNode.GetChildNodeValueInt32('width', 0)) * LMediumNode.GetChildNodeValueInt32('height', 0);
        LVideoPlayerSurface1.PreviewResourceName := LMediumNode.GetChildNodeValueText('preview_url', '');
        LVideoPlayerSurface1.Name := 'VideoPlayer1';
        LVideoPlayerSurface1.Looping := true;
        LVideoPlayerSurface1.AutoStartMode := TALDynamicListBoxVideoPlayerSurface.TAutoStartMode.WhenDisplayed;
        LVideoPlayerSurface1.DataSource := LMediumNode.GetChildNodeValueText('url', '');
      end
      else begin
        var LMedia1 := TALDynamicListBoxImage.Create(Result);
        LMedia1.Name := 'Media1';
        LMedia1.WrapMode := TALImageWrapMode.FitAndCrop;
        LMedia1.ResourceName := LMediumNode.GetChildNodeValueText('url', '');
        LMedia1.Margins.Top := 11;
        LMedia1.Align := TALAlignLayout.top;
        LMedia1.Height := (Result.Width / LMediumNode.GetChildNodeValueInt32('width', 0)) * LMediumNode.GetChildNodeValueInt32('height', 0);
      end;
    end
    else If LmediaNode.ChildNodes.Count = 1 then begin
      var LMediumNode := LmediaNode.ChildNodes[0];
      if LMediumNode.GetChildNodeValueBool('is_video', false) then begin
        var LVideoPlayerSurface1 := TALDynamicListBoxVideoPlayerSurface.Create(Result);
        LVideoPlayerSurface1.Margins.Top := 11;
        LVideoPlayerSurface1.Align := TALAlignLayout.top;
        LVideoPlayerSurface1.Height := (Result.Width / LMediumNode.GetChildNodeValueInt32('width', 0)) * LMediumNode.GetChildNodeValueInt32('height', 0);
        LVideoPlayerSurface1.PreviewResourceName := LMediumNode.GetChildNodeValueText('preview_url', '');
        LVideoPlayerSurface1.Name := 'VideoPlayer1';
        LVideoPlayerSurface1.Looping := true;
        LVideoPlayerSurface1.AutoStartMode := TALDynamicListBoxVideoPlayerSurface.TAutoStartMode.WhenDisplayed;
        LVideoPlayerSurface1.DataSource := LMediumNode.GetChildNodeValueText('url', '');
      end
      else begin
        var LMedia1 := TALDynamicListBoxImage.Create(Result);
        LMedia1.Name := 'Media1';
        LMedia1.WrapMode := TALImageWrapMode.FitAndCrop;
        LMedia1.ResourceName := LMediumNode.GetChildNodeValueText('url', '');
        LMedia1.Margins.Top := 11;
        LMedia1.Align := TALAlignLayout.top;
        LMedia1.Height := (Result.Width / LMediumNode.GetChildNodeValueInt32('width', 0)) * LMediumNode.GetChildNodeValueInt32('height', 0);
      end;
    end;

    Var LLayout3 := TALDynamicListBoxLayout.Create(Result);
    LLayout3.Name := 'Layout3';
    LLayout3.Align := TALAlignLayout.Top;
    LLayout3.Height := 23;
    LLayout3.Margins.Top := 12;
    LLayout3.Margins.bottom := 10;

    var LLikeCountBtn := TALDynamicListBoxButton.Create(LLayout3);
    LLikeCountBtn.Name := 'LikeCountBtn';
    LLikeCountBtn.Fill.Color := TalphaColors.Null;
    LLikeCountBtn.Fill.ResourceName := 'like';
    LLikeCountBtn.Stroke.Color := TalphaColors.Null;
    LLikeCountBtn.CacheIndex := 2;
    LLikeCountBtn.CacheEngine := AContext.CacheEngine;
    LLikeCountBtn.Align := TALAlignLayout.LeftCenter;
    LLikeCountBtn.Margins.Left := 17;
    LLikeCountBtn.Height := 20;
    LLikeCountBtn.Width := 23;

    var LLikeCountText := TALDynamicListBoxText.Create(LLayout3);
    LLikeCountText.Name := 'LikeCountText';
    LLikeCountText.Align := TALAlignLayout.LeftCenter;
    LLikeCountText.Margins.Left := 5;
    LLikeCountText.TextSettings.Font.Size := 14;
    LLikeCountText.TextSettings.Font.weight := TFontWeight.Medium;
    LLikeCountText.TextSettings.Font.Color := $FF0d1014;
    LLikeCountText.AutoSize := True;
    LLikeCountText.Text := AlIntToStrW(AContext.Owner.Data.GetChildNodeValueInt32('like_count', 0));

    var LCommentCountBtn := TALDynamicListBoxButton.Create(LLayout3);
    LCommentCountBtn.Name := 'CommentCountBtn';
    LCommentCountBtn.Fill.Color := TalphaColors.Null;
    LCommentCountBtn.Fill.ResourceName := 'comments';
    LCommentCountBtn.Stroke.Color := TalphaColors.Null;
    LCommentCountBtn.CacheIndex := 3;
    LCommentCountBtn.CacheEngine := AContext.CacheEngine;
    LCommentCountBtn.Align := TALAlignLayout.LeftCenter;
    LCommentCountBtn.Margins.Left := 14;
    LCommentCountBtn.Height := 22;
    LCommentCountBtn.Width := 22;

    var LCommentCountText := TALDynamicListBoxText.Create(LLayout3);
    LCommentCountText.Name := 'CommentCountText';
    LCommentCountText.Align := TALAlignLayout.LeftCenter;
    LCommentCountText.Margins.Left := 5;
    LCommentCountText.TextSettings.Font.Size := 14;
    LCommentCountText.TextSettings.Font.weight := TFontWeight.Medium;
    LCommentCountText.TextSettings.Font.Color := $FF0d1014;
    LCommentCountText.AutoSize := True;
    LCommentCountText.Text := AlIntToStrW(AContext.Owner.Data.GetChildNodeValueInt32('comment_count', 0));

    var LReshareCountsBtn := TALDynamicListBoxButton.Create(LLayout3);
    LReshareCountsBtn.Name := 'ReshareCountsBtn';
    LReshareCountsBtn.Fill.Color := TalphaColors.Null;
    LReshareCountsBtn.Fill.ResourceName := 'message';
    LReshareCountsBtn.Stroke.Color := TalphaColors.Null;
    LReshareCountsBtn.CacheIndex := 4;
    LReshareCountsBtn.CacheEngine := AContext.CacheEngine;
    LReshareCountsBtn.Align := TALAlignLayout.LeftCenter;
    LReshareCountsBtn.Margins.Left := 14;
    LReshareCountsBtn.Height := 19;
    LReshareCountsBtn.Width := 22;

    var LReshareCountText := TALDynamicListBoxText.Create(LLayout3);
    LReshareCountText.Name := 'ReshareCountText';
    LReshareCountText.Align := TALAlignLayout.LeftCenter;
    LReshareCountText.Margins.Left := 5;
    LReshareCountText.TextSettings.Font.Size := 14;
    LReshareCountText.TextSettings.Font.weight := TFontWeight.Medium;
    LReshareCountText.TextSettings.Font.Color := $FF0d1014;
    LReshareCountText.AutoSize := True;
    LReshareCountText.Text := AlIntToStrW(AContext.Owner.Data.GetChildNodeValueInt32('reshare_count', 0));

    var LBookmarkBtn := TALDynamicListBoxButton.Create(LLayout3);
    LBookmarkBtn.Name := 'BookmarkBtn';
    LBookmarkBtn.Fill.Color := TalphaColors.Null;
    LBookmarkBtn.Fill.ResourceName := 'bookmark';
    LBookmarkBtn.Stroke.Color := TalphaColors.Null;
    LBookmarkBtn.CacheIndex := 5;
    LBookmarkBtn.CacheEngine := AContext.CacheEngine;
    LBookmarkBtn.Align := TALAlignLayout.rightCenter;
    LBookmarkBtn.Margins.right := 19;
    LBookmarkBtn.Height := 20;
    LBookmarkBtn.Width := 18;

    var LCaption := TALDynamicListBoxText.Create(Result);
    LCaption.Name := 'Caption';
    LCaption.Align := TALAlignLayout.Top;
    LCaption.Margins.Left := 14;
    LCaption.Margins.Right := 14;
    LCaption.TextSettings.Font.Size := 14;
    LCaption.TextSettings.Font.Color := $FF272727;
    LCaption.TextSettings.LineHeightMultiplier := 1.3;
    LCaption.AutoSize := True;
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
    LCaption.OnElementClick := ALTextEllipsisElementClick;
    LCaption.OnElementMouseEnter := ALTextEllipsisElementMouseEnter;
    LCaption.OnElementMouseLeave := ALTextEllipsisElementMouseLeave;

    var LDate := TALDynamicListBoxText.Create(Result);
    LDate.Name := 'Date';
    LDate.Align := TALAlignLayout.TopLeft;
    LDate.Margins.top := 7;
    LDate.Margins.Left := 14;
    LDate.Margins.Right := 14;
    LDate.Margins.bottom := 27;
    LDate.TextSettings.Font.Size := 12.5;
    LDate.TextSettings.Font.weight := TFontWeight.Regular;
    LDate.TextSettings.Font.Color := $FF71757f;
    LDate.AutoSize := True;
    LDate.Text := inttostr(1+ALRandom32(23)) + ' hours ago';
    LDate.TextSettings.IsHtml := True;

  Except
    ALFreeAndNil(Result);
    Raise;
  End;

end;

{**************************************************}
procedure TMainForm.MainDynamicListBoxDownloadItems(
            const AContext: TALDynamicListBoxView.TDownloadItemsContext;
            out AData: TALJSONNodeW;
            var APaginationToken: string;
            var AErrorCode: Integer);
begin

  // Here, we need to download the JSON data that represents each item.
  // Since we are running in a background thread, we can safely download it from
  // the Internet without worrying about freezing the app.

  // Simulate internet latency
  sleep(1000);

  // PaginationToken – A string used to track the position in paginated data,
  // allowing retrieval of the next set of results.
  APaginationToken := ALRandomStrW(8);

  // In case of an error, assign a custom error code here that
  // can be used later to display an error message to the end user.
  AErrorCode := 0;

  // Here, we retrieve a mocked set of data from the resource.
  // In a real app, you would download the data from the Internet.
  var LStream := TResourceStream.Create(HInstance, 'data', RT_RCDATA);
  try
    AData := TALJsonDocumentW.CreateFromJSONStream(LStream);
  finally
    ALfreeandNil(LStream);
  end;

  // For the mock to work correctly, each item needs a unique ID.
  For var I := 0 to AData.ChildNodes.Count - 1 do
    AData.ChildNodes[i].SetChildNodeValueInt64('id', ALRandom64(ALMaxInt64));

end;

{$IF defined(ALUIAutomationEnabled)}

type
  TALControlAccessProtected=class(TControl);

{*****************************************}
procedure TMainForm.SimulateInfiniteScroll;
begin
  TThread.ForceQueue(nil,
    procedure
    begin
      var LControl := TALControlAccessProtected(MainDynamicListBox);
      FSimulateInfiniteScrollCurrentPoint.X := 10;
      FSimulateInfiniteScrollCurrentPoint.Y := LControl.Height - 10;
      LControl.MouseDown(TMouseButton.mbLeft, [TShiftStateItem.ssLeft], FSimulateInfiniteScrollCurrentPoint.X, FSimulateInfiniteScrollCurrentPoint.Y);
    end);

  var LDelay: Integer := 5 + random(10);

  for var I := 0 to 5 + random(5) do begin
    TThread.ForceQueue(nil,
      procedure
      begin
        var LControl := TALControlAccessProtected(MainDynamicListBox);
        FSimulateInfiniteScrollCurrentPoint.X := max(LControl.Width, FSimulateInfiniteScrollCurrentPoint.X + (Random * 5));
        FSimulateInfiniteScrollCurrentPoint.Y := max(0, FSimulateInfiniteScrollCurrentPoint.Y - (Random * 75));
        LControl.MouseMove([TShiftStateItem.ssLeft], FSimulateInfiniteScrollCurrentPoint.X, FSimulateInfiniteScrollCurrentPoint.Y);
      end, LDelay);
    LDelay := LDelay + 5 + random(10);
  end;

  TThread.ForceQueue(nil,
    procedure
    begin
      var LControl := TALControlAccessProtected(MainDynamicListBox);
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
