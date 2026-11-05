unit Main;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Layouts,
  FMX.Graphics,
  Alcinoe.FMX.FMXOverNativeContainer,
  Alcinoe.FMX.Controls,
  Alcinoe.FMX.Objects,
  Alcinoe.FMX.StdCtrls,
  Alcinoe.FMX.Layouts,
  Alcinoe.FMX.NativeControl,
  Alcinoe.FMX.MapLibre,
  Alcinoe.FMX.WebBrowser;

type

  TMapMarker = record
    ID: Integer;
    Name: String;
    Latitude: Double;
    Longitude: Double;
    Icon: Integer;
    ImageURL: String;
  end;

  TMainForm = class(TForm)
    HeaderLayout: TLayout;
    Toolbar: TALRectangle;
    LayerTitleText: TALText;
    LayerDescriptionText: TALText;
    LayerButtonsRow: TALLayout;
    PreviousButton: TALButton;
    NextButton: TALButton;
    StyleButton: TALButton;
    OptionsButtonsRow: TALLayout;
    LookButton: TALButton;
    SpinnerButton: TALButton;
    StatusText: TALText;
    MapLayout: TLayout;
    MapLibre: TALMapLibre;
    MarkerPopup: TALFMXOverNativeContainer;
    MarkerPopupImage: TALImage;
    MarkerPopupText: TALText;
    MarkerPopupCloseButton: TALButton;
    SpinnerTimer: TTimer;
    CameraStatusTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormSafeAreaChanged(Sender: TObject; const AInsets: TRectF);
    procedure MapLibreMapReady(Sender: TObject);
    procedure MapLibreStyleLoaded(Sender: TObject);
    procedure MapLibreCameraWillChange(Sender: TObject);
    procedure MapLibreCameraIsChanging(Sender: TObject);
    procedure MapLibreCameraDidChange(Sender: TObject);
    procedure MapLibreMapClick(Sender: TObject; const ALatitude: Double; const ALongitude: Double);
    procedure MapLibreSymbolClick(Sender: TObject; const ALayerID: String; const ASymbolID: String);
    procedure MarkerPopupCloseButtonClick(Sender: TObject);
    procedure LayerButtonClick(Sender: TObject);
    procedure MapStyleButtonClick(Sender: TObject);
    procedure LookButtonClick(Sender: TObject);
    procedure SpinnerButtonClick(Sender: TObject);
    procedure SpinnerTimerTimer(Sender: TObject);
    procedure CameraStatusTimerTimer(Sender: TObject);
    procedure ToolbarRowResized(Sender: TObject);
  private
    FMarkers: TArray<TMapMarker>;
    FMarkersStyleLoaded: Boolean;
    FRouteLayersInitialized: Boolean;
    FSelectedMarkerID: Integer;
    FMapStyle: Integer;
    FDemoLayer: Integer;
    FAlternateLook: Boolean;
    FUpdatingStatus: Boolean;
    FCameraMoveCount: Integer;
    procedure ScheduleCameraStatusUpdate;
    procedure InitializeMarkers;
    procedure UpdateToolbar;
    procedure InfoButtonClick(Sender: TObject);
    procedure ShowDemoLayer;
    procedure ClearDemoLayers;
    procedure HideRouteLayers;
    function BuildDemoGeoJSON(const AGeometry: String): String;
    procedure UpdateMapMarkers(const ARouteOnly: Boolean = False);
    procedure ShowMarkerPopup(const AMarker: TMapMarker);
  public
  end;

var
  MainForm: TMainForm;

implementation

uses
  System.Math,
  Alcinoe.FMX.Styles,
  Alcinoe.JSONDoc,
  Alcinoe.Common,
  Alcinoe.FMX.Common,
  Alcinoe.Localization,
  Alcinoe.FMX.ErrorReporting,
  Alcinoe.StringUtils;

{$R *.fmx}

const
  // All sample geometry is fixed around central Paris. Coordinates are lon/lat
  // in GeoJSON, and lat/lon in SetCenterCoordinate.
  CDemoLatitude = 48.8566;
  CDemoLongitude = 2.3370;
  CDemoZoom = 12.6;
  CLayerNames: array[0..7] of String = (
    'Symbols + route', 'Line', 'Circle', 'Fill',
    '3D extrusion', 'Heatmap', 'Raster', 'Base map only');
  CLayerDescriptions: array[0..7] of String = (
    'Fixed Paris landmarks. Tap a pin for details; arrows follow the route.',
    'A route through the landmarks. Look switches to a dashed line.',
    'Circles with data-driven radii, colors and white outlines.',
    'Sample polygons around landmarks; these are not real boundaries.',
    'Synthetic towers. Tilt with two fingers to see their height.',
    'Weighted landmark points. Zoom out to see the density blend.',
    'Satellite: Esri, Maxar, Earthstar Geographics. Look changes opacity.',
    'Custom layers removed. Next returns to the fixed landmarks.');
  CStyleNames: array[0..1] of String = ('Dark', 'Bright');
  CEmptyGeoJSON = '{"type":"FeatureCollection","features":[]}';
  // Do not return Esri's 'Data not available' placeholder for missing tiles.
  CRasterURL = 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}?blankTile=false';

{************************************}
procedure TMainForm.InitializeMarkers;
const
  CNames: array[0..6] of String = (
    'Louvre', 'Orsay', 'Notre-Dame', 'Pantheon',
    'Luxembourg', 'Pompidou', 'Concorde');
  CLatitudes: array[0..6] of Double = (
    48.86061, 48.85996, 48.85297, 48.84622,
    48.84622, 48.86064, 48.86563);
  CLongitudes: array[0..6] of Double = (
    2.33764, 2.32656, 2.34990, 2.34641,
    2.33716, 2.35224, 2.32124);
  CImageIDs: array[0..6] of Integer = (10, 20, 30, 40, 50, 60, 80);
begin
  SetLength(FMarkers, Length(CNames));
  for var I := 0 to High(FMarkers) do begin
    FMarkers[I].ID := I + 1;
    FMarkers[I].Name := CNames[I];
    FMarkers[I].Latitude := CLatitudes[I];
    FMarkers[I].Longitude := CLongitudes[I];
    FMarkers[I].Icon := (I mod 4) + 1;
    FMarkers[I].ImageURL := 'https://picsum.photos/id/' + ALIntToStrW(CImageIDs[I]) + '/150';
  end;
end;

{*****************************************************}
procedure TMainForm.ToolbarRowResized(Sender: TObject);
begin
  var LRow := TALLayout(Sender);
  if LRow.ControlsCount = 0 then Exit;
  for var I := 0 to LRow.ControlsCount - 1 do
    LRow.Controls[I].Width := Max(1, LRow.Width / LRow.ControlsCount - 4);
end;

{********************************}
procedure TMainForm.UpdateToolbar;
begin
  LayerTitleText.Text := ALIntToStrW(FDemoLayer + 1) + '/8  ' + CLayerNames[FDemoLayer];
  LayerDescriptionText.Text := CLayerDescriptions[FDemoLayer];
  StyleButton.Text := CStyleNames[FMapStyle];
  if FAlternateLook then LookButton.Text := 'Look: B'
  else LookButton.Text := 'Look: A';
end;

{*******************************************************************}
function TMainForm.BuildDemoGeoJSON(const AGeometry: String): String;
const
  CColors: array[0..3] of String = ('#38BDF8', '#FB7185', '#A78BFA', '#FBBF24');
  // A closed, counterclockwise ring. The footprints are deliberately synthetic.
  COffsetX: array[0..4] of Double = (-0.00065, 0.00065, 0.00065, -0.00065, -0.00065);
  COffsetY: array[0..4] of Double = (-0.00045, -0.00045, 0.00045, 0.00045, -0.00045);
begin
  var LJSON := TALJsonDocumentW.Create;
  try
    LJSON.SetChildValueText('type', 'FeatureCollection');
    var LFeatures := LJSON.AddChild('features', ntArray);
    for var I := 0 to High(FMarkers) do begin
      var LMarker := FMarkers[I];
      var LFeature := LFeatures.AddChild(ntObject);
      LFeature.SetChildValueText('type', 'Feature');
      var LProperties := LFeature.AddChild('properties', ntObject);
      LProperties.SetChildValueText('id', ALIntToStrW(LMarker.ID));
      // Keep the same property schema on every feature and on every update.
      LProperties.SetChildValueText('_circle_color', CColors[I mod 4]);
      LProperties.SetChildValueFloat('_circle_radius', 10 + I * 2);
      LProperties.SetChildValueFloat('_circle_opacity', 0.8);
      LProperties.SetChildValueText('_circle_stroke_color', '#FFFFFF');
      LProperties.SetChildValueFloat('_circle_stroke_width', 2);
      LProperties.SetChildValueText('_fill_color', CColors[I mod 4]);
      LProperties.SetChildValueFloat('_fill_opacity', 0.55);
      LProperties.SetChildValueText('_fill_outline_color', '#FFFFFF');
      LProperties.SetChildValueText('_fill_extrusion_color', CColors[I mod 4]);
      LProperties.SetChildValueFloat('_fill_extrusion_base', 0);
      LProperties.SetChildValueFloat('_fill_extrusion_height', 100 + I * 100);
      LProperties.SetChildValueFloat('_heatmap_weight', (I + 1) / Length(FMarkers));
      if FAlternateLook then begin
        LProperties.SetChildValueFloat('_circle_radius', 18 + I * 3);
        LProperties.SetChildValueFloat('_fill_opacity', 0.85);
        LProperties.SetChildValueFloat('_fill_extrusion_height', 200 + I * 180);
      end;
      var LGeometry := LFeature.AddChild('geometry', ntObject);
      LGeometry.SetChildValueText('type', AGeometry);
      var LCoordinates := LGeometry.AddChild('coordinates', ntArray);
      if AGeometry = 'Polygon' then begin
        var LRing := LCoordinates.AddChild(ntArray);
        for var J := 0 to High(COffsetX) do begin
          var LCoordinate := LRing.AddChild(ntArray);
          LCoordinate.AddChild.Float := LMarker.Longitude + COffsetX[J];
          LCoordinate.AddChild.Float := LMarker.Latitude + COffsetY[J];
        end;
      end
      else begin
        LCoordinates.AddChild.Float := LMarker.Longitude;
        LCoordinates.AddChild.Float := LMarker.Latitude;
      end;
    end;
    Result := LJSON.SaveToJSONString([soStandardJson]);
  finally
    ALFreeAndNil(LJSON);
  end;
end;

{**********************************}
procedure TMainForm.ClearDemoLayers;
const
  CIDs: array[0..6] of String = (
    'Markers', 'MarkerRouteArrows', 'DemoCircle',
    'DemoFill', 'DemoExtrusion', 'DemoHeatmap', 'DemoRaster');
begin
  for var LID in CIDs do MapLibre.RemoveLayer(LID);
  if not (FDemoLayer in [0, 1]) then HideRouteLayers;
  MarkerPopup.Visible := False;
end;

{**********************************}
procedure TMainForm.HideRouteLayers;
begin
  if not FRouteLayersInitialized then Exit;
  // Keep native layers and their dash configuration alive. An empty source
  // hides the route without removing/recreating layers during Look changes.
  MapLibre.SetLineLayer('MarkerRouteSolid', CEmptyGeoJSON);
  MapLibre.SetLineLayer('MarkerRouteDashed', CEmptyGeoJSON);
end;

{********************************}
procedure TMainForm.ShowDemoLayer;
begin
  if not FMarkersStyleLoaded then Exit;
  // Other layer demos are recreated for option changes. Route layers keep
  // fixed solid/dashed options and only update their GeoJSON data.
  ClearDemoLayers;
  case FDemoLayer of
    0: UpdateMapMarkers;
    1: UpdateMapMarkers(True);
    2: MapLibre.SetCircleLayer('DemoCircle', BuildDemoGeoJSON('Point'));
    3: MapLibre.SetFillLayer('DemoFill', BuildDemoGeoJSON('Polygon'));
    4: MapLibre.SetFillExtrusionLayer('DemoExtrusion', BuildDemoGeoJSON('Polygon'));
    5: begin
      var LOptions := TALMapLibre.THeatmapLayerOptions.Create;
      if FAlternateLook then LOptions.SetHeatmapRadius(80).SetHeatmapIntensity(2)
      else LOptions.SetHeatmapRadius(50).SetHeatmapIntensity(1);
      MapLibre.SetHeatmapLayer('DemoHeatmap', BuildDemoGeoJSON('Point'), @LOptions);
    end;
    6: begin
      var LOptions := TALMapLibre.TRasterLayerOptions.Create;
      if FAlternateLook then LOptions.SetRasterOpacity(0.55).SetRasterSaturation(-1)
      else LOptions.SetRasterOpacity(1);
      MapLibre.SetRasterLayer('DemoRaster', CRasterURL, False, 256, @LOptions);
    end;
  end;
  UpdateToolbar;
  InfoButtonClick(nil);
end;

{****************************************************}
procedure TMainForm.LayerButtonClick(Sender: TObject);
begin
  var LStep := TALButton(Sender).Tag;
  if LStep = 0 then FDemoLayer := High(CLayerNames)
  else FDemoLayer := (FDemoLayer + LStep + Length(CLayerNames)) mod Length(CLayerNames);
  FAlternateLook := False;
  UpdateToolbar;
  ShowDemoLayer;
end;

{*******************************************************}
procedure TMainForm.MapStyleButtonClick(Sender: TObject);
begin
  FMapStyle := (FMapStyle + 1) mod Length(CStyleNames);
  FMarkersStyleLoaded := False;
  MarkerPopup.Visible := False;
  UpdateToolbar;
  StatusText.Text := 'Loading ' + CStyleNames[FMapStyle] + ' style...';
  // Each style load destroys custom images/sources/layers. OnStyleLoaded
  // restores the selected demo, its look and the selected marker.
  case FMapStyle of
    0: MapLibre.StyleURL := 'https://tiles.openfreemap.org/styles/dark';
    1: MapLibre.StyleURL := 'https://tiles.openfreemap.org/styles/bright';
  end;
end;

{***************************************************}
procedure TMainForm.LookButtonClick(Sender: TObject);
begin
  FAlternateLook := not FAlternateLook;
  UpdateToolbar;
  ShowDemoLayer;
end;

{******************************************************}
procedure TMainForm.SpinnerButtonClick(Sender: TObject);
begin
  if not FMarkersStyleLoaded then Exit;
  SpinnerTimer.Enabled := False;
  MapLibre.ShowLoadingSpinner(TAlphaColors.Deepskyblue, 48, 48);
  SpinnerButton.Text := 'Loading...';
  SpinnerTimer.Enabled := True;
end;

{*****************************************************}
procedure TMainForm.SpinnerTimerTimer(Sender: TObject);
begin
  SpinnerTimer.Enabled := False;
  MapLibre.HideLoadingSpinner;
  SpinnerButton.Text := 'Spinner';
end;

{*********************************************}
procedure TMainForm.ScheduleCameraStatusUpdate;
begin
  if CameraStatusTimer = nil then Exit;
  // Also settle a start event with no following move/idle event, as can
  // happen when the compass is tapped while already pointing north.
  CameraStatusTimer.Enabled := False;
  CameraStatusTimer.Enabled := True;
end;

{**********************************************************}
procedure TMainForm.CameraStatusTimerTimer(Sender: TObject);
begin
  // Desktop getters process messages. Retry if another read is in progress.
  if FUpdatingStatus then Exit;
  CameraStatusTimer.Enabled := False;
  if FMarkersStyleLoaded then InfoButtonClick(nil);
end;

{***************************************************}
procedure TMainForm.InfoButtonClick(Sender: TObject);
begin
  if not FMarkersStyleLoaded or FUpdatingStatus then Exit;
  // Desktop getters wait for JavaScript; guard against message-loop reentry.
  FUpdatingStatus := True;
  try
    var LCamera := MapLibre.GetCameraPosition;
    var LBounds := MapLibre.GetViewportBounds;
    StatusText.Text := Format(
                         'Zoom %.1f | bearing %.0f | tilt %.0f | moves %d',
                         [LCamera.zoom, LCamera.bearing, LCamera.tilt, FCameraMoveCount]) + sLineBreak +
      Format(
        'Bounds %.3f, %.3f - %.3f, %.3f',
        [LBounds.Left, LBounds.Top, LBounds.Right, LBounds.Bottom]);
  finally
    FUpdatingStatus := False;
  end;
end;

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

  FMarkersStyleLoaded := False;
  FSelectedMarkerID := 0;
  FMapStyle := 0;
  FDemoLayer := 0;
  FAlternateLook := False;
  FCameraMoveCount := 0;
  InitializeMarkers;

  // All visual components (header, toolbar, map, marker popup) and timers are
  // defined in Main.fmx. The form owns two non-overlapping layouts: header
  // above, map below. Header controls never cover the native map or its
  // compass.
  UpdateToolbar;
end;

{***********************************************}
procedure TMainForm.FormDestroy(Sender: TObject);
begin
  SpinnerTimer.Enabled := False;
  CameraStatusTimer.Enabled := False;
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
  MapLibre.SetCenterCoordinate(CDemoLatitude, CDemoLongitude, CDemoZoom);
end;

{*******************************************************}
procedure TMainForm.MapLibreStyleLoaded(Sender: TObject);
begin

  MapLibre.SetImage('location_pin_1', ALGetScreenScale, 64, 64, 'location_pin_1');
  MapLibre.SetImage('location_pin_2', ALGetScreenScale, 64, 64, 'location_pin_2');
  MapLibre.SetImage('location_pin_3', ALGetScreenScale, 64, 64, 'location_pin_3');
  MapLibre.SetImage('location_pin_4', ALGetScreenScale, 64, 64, 'location_pin_4');
  MapLibre.SetImage('route_arrow', ALGetScreenScale, 16, 16, 'route_arrow');

  FRouteLayersInitialized := False;
  FMarkersStyleLoaded := True;

  SpinnerTimerTimer(nil);
  ShowDemoLayer;
end;

{**************************************************************}
procedure TMainForm.UpdateMapMarkers(const ARouteOnly: Boolean);
begin

  if not FMarkersStyleLoaded then Exit;

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
      // Both looks keep the same visible width and opacity on either base style.
      if FMapStyle = 0 then LProperties.SetChildValueText('_line_color', '#38BDF8')
      else LProperties.SetChildValueText('_line_color', '#0369A1');
      LProperties.SetChildValueFloat('_line_width', 4);
      LProperties.SetChildValueFloat('_line_opacity', 1);

      var LGeometry := LFeature.AddChild('geometry', ntObject);
      LGeometry.SetChildValueText('type', 'LineString');

      var LCoordinates := LGeometry.AddChild('coordinates', ntArray);
      for var LMarker in FMarkers do begin
        var LCoordinate := LCoordinates.AddChild(ntArray);
        LCoordinate.AddChild.Float := LMarker.Longitude;
        LCoordinate.AddChild.Float := LMarker.Latitude;
      end;

      var LSolidOptions := TALMapLibre.TLineLayerOptions.Create;
      LSolidOptions.SetLineCap('round').SetLineJoin('round');
      var LDashedOptions := TALMapLibre.TLineLayerOptions.Create;
      LDashedOptions.SetLineCap('butt').SetLineJoin('round').SetLineDasharray([2, 2]);
      var LRouteJSON := LLineJSON.SaveToJSONString([soStandardJson]);
      if not FRouteLayersInitialized then begin
        // Initialize BOTH with real geometry so all data-driven properties
        // are bound, even if that look starts hidden. Each ID has one fixed
        // dash configuration for the entire lifetime of the style.
        MapLibre.SetLineLayer('MarkerRouteSolid', LRouteJSON, @LSolidOptions);
        MapLibre.SetLineLayer('MarkerRouteDashed', LRouteJSON, @LDashedOptions);
        FRouteLayersInitialized := True;
      end;
      if FAlternateLook then begin
        MapLibre.SetLineLayer('MarkerRouteSolid', CEmptyGeoJSON);
        MapLibre.SetLineLayer('MarkerRouteDashed', LRouteJSON, @LDashedOptions);
      end
      else begin
        MapLibre.SetLineLayer('MarkerRouteDashed', CEmptyGeoJSON);
        MapLibre.SetLineLayer('MarkerRouteSolid', LRouteJSON, @LSolidOptions);
      end;

    finally
      ALFreeAndNil(LLineJSON);
    end;

    if ARouteOnly then Exit;

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
      LProperties.SetChildValueText('_text', LMarker.Name);
      if FAlternateLook then LProperties.SetChildValueFloat('_text_size', 20)
      else LProperties.SetChildValueFloat('_text_size', 14);
      LProperties.SetChildValueText('_text_color', '#FFFFFF');
      LProperties.SetChildValueText('_text_anchor', 'top');
      LProperties.SetChildValueFloat('_text_halo_width', 1);
      LProperties.SetChildValueText('_text_halo_color', '#111827');

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
        .SetIconRotationAlignment('viewport')
        .SetIconPitchAlignment('viewport')
        .SetTextOptional(True));

  finally
    ALFreeAndNil(LJSON);
  end;
end;

{************************************************************}
procedure TMainForm.MapLibreCameraWillChange(Sender: TObject);
begin
  StatusText.Text := 'Moving camera...';
  ScheduleCameraStatusUpdate;
end;

{************************************************************}
procedure TMainForm.MapLibreCameraIsChanging(Sender: TObject);
begin
  Inc(FCameraMoveCount);
  ScheduleCameraStatusUpdate;
end;

{***********************************************************}
procedure TMainForm.MapLibreCameraDidChange(Sender: TObject);
begin
  // Read camera state after the native callback has finished.
  ScheduleCameraStatusUpdate;
end;

{*************************************************************}
procedure TMainForm.ShowMarkerPopup(const AMarker: TMapMarker);
begin
  MarkerPopupImage.ResourceName := AMarker.ImageURL;
  MarkerPopupText.Text :=
    AMarker.Name + '<br>' +
    'Latitude: ' + ALFloatToStrW(AMarker.Latitude) + '<br>' +
    'Longitude: ' + ALFloatToStrW(AMarker.Longitude);

  MarkerPopup.Visible := True;
end;

{***************************************************************}
procedure TMainForm.MarkerPopupCloseButtonClick(Sender: TObject);
begin
  MarkerPopup.Visible := False;
end;

{*******************************************************************************************************}
procedure TMainForm.MapLibreMapClick(Sender: TObject; const ALatitude: Double; const ALongitude: Double);
begin
  StatusText.Text := Format('Tap: %.5f, %.5f', [ALatitude, ALongitude]);
end;

{********************************************************************************************************}
procedure TMainForm.MapLibreSymbolClick(Sender: TObject; const ALayerID: String; const ASymbolID: String);
begin
  if (FDemoLayer <> 0) or (ALayerID <> 'Markers') then Exit;
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