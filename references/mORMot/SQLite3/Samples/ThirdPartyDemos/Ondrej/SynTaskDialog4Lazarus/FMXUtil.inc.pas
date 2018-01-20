var
  _ScreenDPI_X : Single = 0;

function ScalingByScreenDPI_N( F:TForm = NIL ):Single;
var
  p : TPointF;
  M : TDeviceDisplayMetrics;
  i : integer;
  h : THandle;
begin
  if _ScreenDPI_X > 0 then
  begin
    Result := _ScreenDPI_X;
    Exit;
  end else
    Result := 1;

  {$IFDEF MSWINDOWS}
  if F <> NIL then
  begin
    h := GetWindowDC( WindowHandleToPlatform( F.Handle ).Wnd );
    i := GetDeviceCaps( h, LOGPIXELSX );
    if i >= 72 then
    begin
      Result := i / 96;
      _ScreenDPI_X := Result;
      Exit;
    end;
  end;
 {$ENDIF}

  if TPlatformServices.Current.SupportsPlatformService( IFMXDeviceMetricsService ) then
  begin
    M := (TPlatformServices.Current.GetPlatformService(
        IFMXDeviceMetricsService) as IFMXDeviceMetricsService).GetDisplayMetrics;
    if M.PixelsPerInch >= 72 then
      Result := M.PixelsPerInch / {$IFDEF MACOS}110{$ENDIF}
                                  {$IFDEF MSWINDOWS}96{$ENDIF}
                                  ;
  end;
end;

function ScalingByScreenDPI( F:TForm = NIL ):TPointF;
begin
  Result.X := ScalingByScreenDPI_N(F);
  Result.Y := Result.X;
end;

procedure inc( var F:Single; D:Single ); overload;
begin
  F := F+D;
end;

procedure dec( var F:Single; D:Single ); overload;
begin
  F := F-D;
end;

function FMXMeasureText( s:string; C : TTextControl; MaxWidth : Single; WordWrap : boolean ):TRectF;
var
  R : TRectF;
begin
  if MaxWidth = 0 then
    MaxWidth := 2000;
  R := RectF( 0,0,MaxWidth,2000 );
  C.StyledSettings := C.StyledSettings - [ TStyledSetting.Size ];
  C.Canvas.Font.Assign( C.Font );
  C.Canvas.MeasureText( R, s, WordWrap, [], TTextAlign.Leading, TTextAlign.Leading );
  Result := R;
end;
