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
    FMarkerPopup: TALRectangle;
    procedure UpdateMapMarkers;
    procedure ShowMarkerPopup(const AMarker: TRandomMapMarker);
  public
  published
    procedure MapLibreMapReady(Sender: TObject);
    procedure MapLibreStyleLoaded(Sender: TObject);
    procedure MapLibreCameraDidChange(Sender: TObject);
    procedure MapLibreMapClick(Sender: TObject; const ALatitude: Double; const ALongitude: Double);
    procedure MapLibreMarkerClick(Sender: TObject; const AMarkerID: String);
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
  MapLibre.OnMarkerClick := MapLibreMarkerClick;
end;

{***********************************************}
procedure TMainForm.FormDestroy(Sender: TObject);
begin
  ALLog('TMainForm.FormDestroy');
  MapLibre.HidePopup;
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

  FMarkersStyleLoaded := True;

  // Restore existing markers when the style is reloaded.
  if FMarkersInitialized then begin
    UpdateMapMarkers;
    Exit;
  end;

  MapLibreCameraDidChange(MapLibre);
end;

{*******************************************}
procedure TMainForm.UpdateMapMarkers;
begin
  if not FMarkersStyleLoaded then Exit;
  if not FMarkersInitialized then Exit;

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
      LProperties.SetChildValueText(
        '_icon_image',
        'location_pin_' + ALIntToStrW(LMarker.Icon));

      // Enlarge the selected marker by 25%.
      if LMarker.ID = FSelectedMarkerID then
        LProperties.SetChildValueFloat('_icon_size', 1.25)
      else
        LProperties.SetChildValueFloat('_icon_size', 1.00);

      var LGeometry := LFeature.AddChild('geometry', ntObject);
      LGeometry.SetChildValueText('type', 'Point');

      var LCoordinates := LGeometry.AddChild('coordinates', ntArray);
      LCoordinates.AddChild.Float := LMarker.Longitude;
      LCoordinates.AddChild.Float := LMarker.Latitude;
    end;

    MapLibre.SetMarkers(LJSON.SaveToJSONString([soStandardJson]));
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

    // Create the popup content.
    // Keep it outside the visible form area.
    // ShowPopup will render it into a bitmap.
    FMarkerPopup := TALRectangle.Create(MapLibre);
    FMarkerPopup.Parent := MapLibre;
    FMarkerPopup.Padding.Rect := TRectF.Create(16,16,16,16);
    FMarkerPopup.Position.X := -10000;
    FMarkerPopup.Position.Y := -10000;
    FMarkerPopup.Width := MapLibre.Width - 32;
    FMarkerPopup.Fill.ColorKey := 'Material3.Color.Surface';
    FMarkerPopup.Margins.Bottom := 16;
    FMarkerPopup.Align := TALAlignLayout.Bottom;
    FMarkerPopup.AutoSize := TALAutoSizeMode.Height;

    var LImage := TALImage.Create(FMarkerPopup);
    LImage.Parent := FMarkerPopup;
    LImage.HitTest := False;
    LImage.Height := 150;
    LImage.Width := 150;
    LImage.Margins.Right := 16;
    LImage.Align := TALAlignLayout.Left;
    LImage.LoadingColorKey := 'Material3.Color.SurfaceContainerHigh';
    LImage.WrapMode := TALImageWrapMode.Cover;

    var LText := TALText.Create(FMarkerPopup);
    LText.Parent := FMarkerPopup;
    LText.HitTest := False;
    LText.Align := TALAlignLayout.client;
    LText.TextSettings.Font.Size := 18;
    LText.TextSettings.Font.ColorKey := 'Material3.Color.OnSurface';

  end;

  TALImage(FMarkerPopup.Controls[0]).ResourceName := AMarker.ImageURL;

  TALText(FMarkerPopup.controls[1]).Text :=
    'Marker #' + ALIntToStrW(AMarker.ID) + sLineBreak +
    'Latitude: ' + ALFloatToStrW(AMarker.Latitude) + sLineBreak +
    'Longitude: ' + ALFloatToStrW(AMarker.Longitude);

  MapLibre.ShowPopup(FMarkerPopup);
end;

{****************************************************************************************************}
procedure TMainForm.MapLibreMapClick(Sender: TObject; const ALatitude: Double; const ALongitude: Double);
begin
  ALLog(
    'MapLibreClick',
    'Latitude: ' + ALFloatToStrW(ALatitude) +
    ' - Longitude: ' + ALFloatToStrW(ALongitude));
end;

{********************************************}
procedure TMainForm.MapLibreMarkerClick(
            Sender: TObject;
            const AMarkerID: String);
begin
  var LMarkerID := StrToIntDef(AMarkerID, 0);
  if (LMarkerID < 1) or (LMarkerID > Length(FMarkers)) then Exit;

  // Update marker selection.
  if FSelectedMarkerID <> LMarkerID then begin
    FSelectedMarkerID := LMarkerID;
    UpdateMapMarkers;
  end;

  // Always display the selected marker's location.
  ShowMarkerPopup(FMarkers[LMarkerID - 1]);
end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.
