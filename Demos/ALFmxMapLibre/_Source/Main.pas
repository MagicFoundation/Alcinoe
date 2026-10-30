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
  Alcinoe.FMX.Edit,
  Alcinoe.fmx.Memo,
  Alcinoe.FMX.FMXOverNativeContainer,
  Alcinoe.FMX.Controls,
  Alcinoe.FMX.Objects,
  Alcinoe.FMX.StdCtrls,
  Alcinoe.FMX.Layouts,
  Alcinoe.FMX.NativeControl,
  Alcinoe.FMX.MapLibre,
  Alcinoe.FMX.WebBrowser;

type

  TRandomMapMarker = record
    ID: Integer;
    Latitude: Double;
    Longitude: Double;
    Icon: Integer;
    ImageURL: String;
  end;

  TMainForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormSafeAreaChanged(Sender: TObject; const AInsets: TRectF);
  private
    MapLibre: TALMapLibre;
    FMarkers: TArray<TRandomMapMarker>;
    FMarkersInitialized: Boolean;
    FMarkersStyleLoaded: Boolean;
    FSelectedMarkerID: Integer;
    FMarkerPopup: TALFMXOverNativeContainer;
    FMapLayersButtonContainer: TALFMXOverNativeContainer;
    procedure UpdateMapMarkers;
    procedure ShowMarkerPopup(const AMarker: TRandomMapMarker);
  public
  published
    procedure MapLibreMapReady(Sender: TObject);
    procedure MapLibreStyleLoaded(Sender: TObject);
    procedure MapLibreCameraDidChange(Sender: TObject);
    procedure MapLibreMapClick(Sender: TObject; const ALatitude: Double; const ALongitude: Double);
    procedure MapLibreSymbolClick(Sender: TObject; const ALayerID: String; const ASymbolID: String);
    procedure MarkerPopupCloseButtonClick(Sender: TObject);
    procedure MapLayersButtonClick(Sender: TObject);
  end;

var
  MainForm: TMainForm;

implementation

uses
  System.Net.URLClient,
  System.IOUtils,
  System.TypInfo,
  System.Math,
  Alcinoe.FMX.UserPreferences,
  Alcinoe.FMX.Styles,
  Alcinoe.FMX.Snackbar,
  Alcinoe.JSONDoc,
  Alcinoe.Common,
  Alcinoe.FMX.Common,
  Alcinoe.fmx.Sheets,
  Alcinoe.FileUtils,
  Alcinoe.FMX.ErrorReporting,
  Alcinoe.Localization,
  Alcinoe.StringUtils;

{$R *.fmx}

{**********************************************}
procedure TMainForm.FormCreate(Sender: TObject);
begin
  ALLog('TMainForm.FormCreate');
  TALErrorReporter.Instance;

  TALStyleManager.Instance.DarkModeBehavior := TALStyleManager.TDarkModeBehavior.AlwaysDark;
  TALStyleManager.Instance.ApplyColorScheme(Self, 'Material3.Color.Surface'{AFormFillColorKey}, 'Material3.Color.Surface'{ASystemStatusBarBackgroundColorKey});
  ALSetSystemBarsColor(
    TALStyleManager.Instance.GetColor('Material3.Color.Surface'), // const AStatusBarColor: TAlphaColor
    TALStyleManager.Instance.GetColor('Material3.Color.Surface'), // const ANavigationBarColor: TAlphaColor
    TALStyleManager.Instance.IsDarkMode, // const AStatusBarUseLightIcons: TAlphaColor
    TALStyleManager.Instance.IsDarkMode); // const ANavigationBarUseLightIcons: TAlphaColor

  Randomize;
  FMarkersInitialized := False;
  FMarkersStyleLoaded := False;
  FSelectedMarkerID := 0;
  SetLength(FMarkers, 0);

  FMarkerPopup := Nil;

  MapLibre := TALMapLibre.Create(Self);
  MapLibre.Parent := Self;
  MapLibre.Align := TALAlignLayout.Client;
  MapLibre.StyleURL := 'https://tiles.openfreemap.org/styles/dark';
  MapLibre.OnMapReady := MapLibreMapReady;
  MapLibre.OnStyleLoaded := MapLibreStyleLoaded;
  MapLibre.OnCameraDidChange := MapLibreCameraDidChange;
  MapLibre.OnMapClick := MapLibreMapClick;
  MapLibre.OnSymbolClick := MapLibreSymbolClick;
end;

{***********************************************}
procedure TMainForm.FormDestroy(Sender: TObject);
begin
  ALLog('TMainForm.FormDestroy');
end;

{******************************************************************************}
procedure TMainForm.FormSafeAreaChanged(Sender: TObject; const AInsets: TRectF);
begin
  Padding.Top := AInsets.Top;
  Padding.Bottom := AInsets.Bottom;
end;

{****************************************************}
procedure TMainForm.MapLibreMapReady(Sender: TObject);
begin
  MapLibre.SetCenterCoordinate(48.8566, 2.3522, 15);
end;

{*******************************************************}
procedure TMainForm.MapLibreStyleLoaded(Sender: TObject);
begin
  MapLibre.SetImage('location_pin_1', ALGetScreenScale, 64, 64, 'location_pin_1');
  MapLibre.SetImage('location_pin_2', ALGetScreenScale, 64, 64, 'location_pin_2');
  MapLibre.SetImage('location_pin_3', ALGetScreenScale, 64, 64, 'location_pin_3');
  MapLibre.SetImage('location_pin_4', ALGetScreenScale, 64, 64, 'location_pin_4');
  MapLibre.SetImage('route_arrow', ALGetScreenScale, 16, 16, 'route_arrow');

  FMarkersStyleLoaded := True;

  if FMapLayersButtonContainer = nil then begin
    FMapLayersButtonContainer := TALFMXOverNativeContainer.Create(MapLibre);
    FMapLayersButtonContainer.Parent := MapLibre;
    FMapLayersButtonContainer.Fill.Color := TalphaColors.Null;
    FMapLayersButtonContainer.Stroke.Color := TalphaColors.Null;
    FMapLayersButtonContainer.Align := TALAlignLayout.TopRight;
    FMapLayersButtonContainer.Margins.Top := 16;
    FMapLayersButtonContainer.Margins.Right := 8;
    FMapLayersButtonContainer.AutoSize := TALAutoSizeMode.Both;

    var LButton := TALButton.Create(FMapLayersButtonContainer);
    LButton.Parent := FMapLayersButtonContainer;
    TALStyleManager.Instance.ApplyButtonIconStyle('Material3.Button.Icon.Filled', LButton, 40);
    LButton.Fill.ImageNoRadius := False;
    LButton.Fill.ResourceName := 'map_layers';
    LButton.OnClick := MapLayersButtonClick;
  end;

  FMapLayersButtonContainer.Visible := True;

  if FMarkersInitialized then begin
    UpdateMapMarkers;
    Exit;
  end;

  MapLibreCameraDidChange(MapLibre);
end;

{***********************************}
procedure TMainForm.UpdateMapMarkers;
begin
  if not FMarkersStyleLoaded then Exit;
  if not FMarkersInitialized then Exit;

  // ---------------------------
  // Create the satellite layer.
  MapLibre.SetRasterLayer(
    'Satellite', // const AId: String;
    'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}?blankTile=false', // const ATileURL: String;
    False, // const AIsTileJSON: Boolean = False;
    256, // const ATileSize: Integer = 256;
    TALMapLibre.TRasterLayerOptions.Create
      .SetRasterOpacity(1.0)
      .SetRasterResampling('linear')
      .SetVisibility('none')); // const AOptions: TALMapLibre.PRasterLayerOptions = nil

  // Create the route connecting all markers.
  if Length(FMarkers) >= 2 then begin

    // ----------------------------------
    // Create the main route as one line.
    var LLineJSON := TALJsonDocumentW.Create;
    try
      LLineJSON.SetChildValueText('type', 'FeatureCollection');

      var LFeatures := LLineJSON.AddChild('features', ntArray);
      var LFeature := LFeatures.AddChild(ntObject);
      LFeature.SetChildValueText('type', 'Feature');

      var LProperties := LFeature.AddChild('properties', ntObject);
      LProperties.SetChildValueText('_line_color', '#' + ALAlphaColorToRGBHex(TALStyleManager.Instance.GetColor('Material3.Color.Outline')));
      LProperties.SetChildValueFloat('_line_width', 3.0);
      LProperties.SetChildValueFloat('_line_opacity', 0.5);

      var LGeometry := LFeature.AddChild('geometry', ntObject);
      LGeometry.SetChildValueText('type', 'LineString');

      var LCoordinates := LGeometry.AddChild('coordinates', ntArray);
      for var LMarker in FMarkers do begin
        var LCoordinate := LCoordinates.AddChild(ntArray);
        LCoordinate.AddChild.Float := LMarker.Longitude;
        LCoordinate.AddChild.Float := LMarker.Latitude;
      end;

      MapLibre.SetLineLayer(
        'MarkerRoute',
        LLineJSON.SaveToJSONString([soStandardJson]),
        TALMapLibre.TLineLayerOptions.Create.SetLineCap('round'));

    finally
      ALFreeAndNil(LLineJSON);
    end;

    // -------------------------------------------
    // Create one arrow for each segment using
    // MapLibre's automatic line-center placement.
    var LArrowJSON := TALJsonDocumentW.Create;
    try
      LArrowJSON.SetChildValueText('type', 'FeatureCollection');

      var LFeatures := LArrowJSON.AddChild('features', ntArray);

      for var I := 0 to High(FMarkers) - 1 do begin
        var LFeature := LFeatures.AddChild(ntObject);
        LFeature.SetChildValueText('type', 'Feature');

        var LProperties := LFeature.AddChild('properties', ntObject);
        LProperties.SetChildValueText('_icon_image', 'route_arrow');

        var LGeometry := LFeature.AddChild('geometry', ntObject);
        LGeometry.SetChildValueText('type', 'LineString');

        var LCoordinates := LGeometry.AddChild('coordinates', ntArray);

        // Starting marker.
        var LCoordinate := LCoordinates.AddChild(ntArray);
        LCoordinate.AddChild.Float := FMarkers[I].Longitude;
        LCoordinate.AddChild.Float := FMarkers[I].Latitude;

        // Ending marker.
        LCoordinate := LCoordinates.AddChild(ntArray);
        LCoordinate.AddChild.Float := FMarkers[I + 1].Longitude;
        LCoordinate.AddChild.Float := FMarkers[I + 1].Latitude;
      end;

      MapLibre.SetSymbolLayer(
        'MarkerRouteArrows',
        LArrowJSON.SaveToJSONString([soStandardJson]),
        TALMapLibre.TSymbolLayerOptions.Create.SetSymbolPlacement('line-center'));

    finally
      ALFreeAndNil(LArrowJSON);
    end;
  end;

  // -------------------
  // Create the markers.
  var LJSON := TALJsonDocumentW.Create;
  try
    LJSON.SetChildValueText('type', 'FeatureCollection');

    var LFeatures := LJSON.AddChild('features', ntArray);

    for var LMarker in FMarkers do begin
      var LFeature := LFeatures.AddChild(ntObject);
      LFeature.SetChildValueText('type', 'Feature');

      var LProperties := LFeature.AddChild('properties', ntObject);
      LProperties.SetChildValueText('id', ALIntToStrW(LMarker.ID));
      LProperties.SetChildValueText('image_url', LMarker.ImageURL);
      LProperties.SetChildValueText('_icon_image', 'location_pin_' + ALIntToStrW(LMarker.Icon));
      LProperties.SetChildValueText('_icon_anchor', 'bottom');
      LProperties.SetChildValueText('_text', '#' + ALIntToStrW(LMarker.ID));
      LProperties.SetChildValueFloat('_text_size', 16);
      LProperties.SetChildValueText('_text_color', '#FFFFFF');
      LProperties.SetChildValueText('_text_anchor', 'top');

      // Enlarge the selected marker by 25%.
      if LMarker.ID = FSelectedMarkerID then begin
        LProperties.SetChildValueFloat('_icon_size', 1.25);
        LProperties.SetChildValueFloat('_symbol_sort_key', 999.0);
      end
      else begin
        LProperties.SetChildValueFloat('_icon_size', 1.00);
        LProperties.SetChildValueFloat('_symbol_sort_key', LMarker.ID);
      end;

      var LGeometry := LFeature.AddChild('geometry', ntObject);
      LGeometry.SetChildValueText('type', 'Point');

      var LCoordinates := LGeometry.AddChild('coordinates', ntArray);
      LCoordinates.AddChild.Float := LMarker.Longitude;
      LCoordinates.AddChild.Float := LMarker.Latitude;
    end;

    MapLibre.SetSymbolLayer(
      'Markers', // const AId: String;
      LJSON.SaveToJSONString([soStandardJson]), // const AGeoJSON: String;
      TALMapLibre.TSymbolLayerOptions.Create
        .SetTextFont(['Noto Sans Regular'])
        .SetIconAllowOverlap(True)
        .SetIconIgnorePlacement(True)
        .SetTextOptional(True));

  finally
    ALFreeAndNil(LJSON);
  end;
end;

{********************************************************}
procedure TMainForm.MapLibreCameraDidChange(Sender: TObject);
const
  CImageIDs: array[0..7] of Integer = (10, 20, 30, 40, 50, 60, 70, 80);
begin
  // Generate the markers only once.
  if not FMarkersStyleLoaded then Exit;
  if FMarkersInitialized then Exit;

  var LBounds := MapLibre.GetViewportBounds;

  var LWest := LBounds.Left;
  var LEast := LBounds.Right;
  var LSouth := EnsureRange(LBounds.Top, -90.0, 90.0);
  var LNorth := EnsureRange(LBounds.Bottom, -90.0, 90.0);

  // Support viewports crossing the antimeridian.
  while LEast < LWest do
    LEast := LEast + 360;

  // Generate exactly 8 random markers.
  SetLength(FMarkers, 8);

  for var I := 0 to High(FMarkers) do begin
    FMarkers[I].ID := I + 1;
    FMarkers[I].Latitude := LSouth + Random * (LNorth - LSouth);
    FMarkers[I].Longitude := LWest + Random * (LEast - LWest);

    // Normalize longitude to [-180, 180].
    while FMarkers[I].Longitude > 180 do
      FMarkers[I].Longitude := FMarkers[I].Longitude - 360;
    while FMarkers[I].Longitude < -180 do
      FMarkers[I].Longitude := FMarkers[I].Longitude + 360;

    FMarkers[I].Icon := Random(4) + 1;
    // Stable image URL, different for each marker.
    FMarkers[I].ImageURL :=
      'https://picsum.photos/id/' + ALIntToStrW(CImageIDs[I]) + '/150';
  end;

  FMarkersInitialized := True;

  UpdateMapMarkers;
end;

{****************************************************************}
procedure TMainForm.ShowMarkerPopup(const AMarker: TRandomMapMarker);
begin

  if FMarkerPopup = nil then begin
    FMarkerPopup := TALFMXOverNativeContainer.Create(MapLibre);
    FMarkerPopup.Parent := MapLibre;
    FMarkerPopup.Padding.Rect := TRectF.Create(16,16,16,16);
    FMarkerPopup.Width := MapLibre.Width - 32;
    FMarkerPopup.Fill.ColorKey := 'Material3.Color.SurfaceContainer';
    FMarkerPopup.Stroke.Color := TalphaColors.Null;
    FMarkerPopup.Margins.Rect := TRectF.Create(16,16,16,16);
    FMarkerPopup.Align := TALAlignLayout.Bottom;
    FMarkerPopup.XRadius := 16;
    FMarkerPopup.YRadius := 16;
    FMarkerPopup.AutoSize := TALAutoSizeMode.Height;

    var LImage := TALImage.Create(FMarkerPopup);
    LImage.Parent := FMarkerPopup;
    LImage.HitTest := False;
    LImage.Height := 150;
    LImage.Width := 150;
    LImage.Margins.Right := 16;
    LImage.Align := TALAlignLayout.Left;
    LImage.LoadingColorKey := 'Material3.Color.SurfaceContainerHighest';
    LImage.WrapMode := TALImageWrapMode.Cover;

    var LText := TALText.Create(FMarkerPopup);
    LText.Parent := FMarkerPopup;
    LText.HitTest := False;
    LText.Align := TALAlignLayout.client;
    LText.TextSettings.Font.Size := 18;
    LText.TextSettings.Font.ColorKey := 'Material3.Color.OnSurface';

    var LCloseButton := TALButton.Create(FMarkerPopup);
    LCloseButton.Parent := FMarkerPopup;
    TALStyleManager.Instance.ApplyButtonIconStyle('Material3.Button.Icon.Standard', LCloseButton, 24);
    LCloseButton.Fill.ImageNoRadius := False;
    LCloseButton.Fill.ResourceName := 'alcinoe_cross';
    LCloseButton.Position.x := FMarkerPopup.Width - 11 - LCloseButton.Width;
    LCloseButton.Position.y := 11;
    LCloseButton.Anchors := [TAnchorKind.akTop, TAnchorKind.akRight];
    LCloseButton.OnClick := MarkerPopupCloseButtonClick;
  end;

  TALImage(FMarkerPopup.Controls[0]).ResourceName := AMarker.ImageURL;

  TALText(FMarkerPopup.controls[1]).Text :=
    'Marker #' + ALIntToStrW(AMarker.ID) + sLineBreak +
    'Latitude: ' + ALFloatToStrW(AMarker.Latitude) + sLineBreak +
    'Longitude: ' + ALFloatToStrW(AMarker.Longitude);

  FMarkerPopup.Visible := True;
end;

{***************************************************************}
procedure TMainForm.MarkerPopupCloseButtonClick(Sender: TObject);
begin
  FMarkerPopup.Visible := False;
end;

{********************************************************}
procedure TMainForm.MapLayersButtonClick(Sender: TObject);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _addIcon(const AParent: TALLayout; const AResName: String);
  begin
    var LIcon := TALRectangle.Create(AParent);
    LIcon.Parent := AParent;
    LIcon.Align := TALAlignLayout.LeftCenter;
    LIcon.Width := 50;
    LIcon.Height := 50;
    LIcon.Fill.ResourceName := AResName;
    LIcon.Fill.Color := TalphaColors.Null;
    LIcon.Stroke.Color := TAlphaColors.Null;
    LIcon.Margins.Left := (width - 150) / 6;
    LIcon.Margins.right := LIcon.Margins.Left;
  end;


begin
(*
  // Build a sample content layout for demonstration purposes.
  // In a real application, it is recommended to use a dedicated TFrame
  // for better structure and reusability.
  Var LContent := TALRectangle.Create(nil);
  LContent.AutoSize := TALAutoSizeMode.Height;
  LContent.Stroke.Color := TAlphaColors.Null;
  LContent.Fill.Color := TAlphaColors.Null;

  var LBar1 := TALLayout.Create(LContent);
  LBar1.Parent := LContent;
  LBar1.AutoSize := TALAutoSizeMode.Both;
  LBar1.Align := TALAlignLayout.MosttopCenter;
  LBar1.Margins.top := 8;
  LBar1.Margins.Bottom := 48;
  _addIcon(LBar1, 'location_pin_1');
  _addIcon(LBar1, 'location_pin_1');
  _addIcon(LBar1, 'location_pin_1');

  var LBar2 := TALLayout.Create(LContent);
  LBar2.Parent := LContent;
  LBar2.AutoSize := TALAutoSizeMode.Both;
  LBar2.Align := TALAlignLayout.topCenter;
  LBar2.Margins.Bottom := 32;
  _addIcon(LBar2, 'location_pin_1');
  _addIcon(LBar2, 'location_pin_1');
  _addIcon(LBar2, 'location_pin_1');
*)
  Maplibre.FreezeNativeView;
(*
  TALBottomSheet.Builder
    .Setcontent(LContent)
    .Show;
*)
end;

{****************************************************************************************************}
procedure TMainForm.MapLibreMapClick(Sender: TObject; const ALatitude: Double; const ALongitude: Double);
begin
  ALLog('MapLibreClick', 'Latitude: ' + ALFloatToStrW(ALatitude) + ' - Longitude: ' + ALFloatToStrW(ALongitude));
end;

{********************************************}
procedure TMainForm.MapLibreSymbolClick(Sender: TObject; const ALayerID: String; const ASymbolID: String);
begin
  var LMarkerID := StrToIntDef(ASymbolID, 0);
  if (LMarkerID < 1) or (LMarkerID > Length(FMarkers)) then Exit;
  if FSelectedMarkerID <> LMarkerID then begin
    FSelectedMarkerID := LMarkerID;
    UpdateMapMarkers;
  end;
  ShowMarkerPopup(FMarkers[LMarkerID - 1]);
end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.
