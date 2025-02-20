unit Alcinoe.FMX.CacheEngines;

interface

{$I Alcinoe.inc}

uses
  System.Types,
  system.SyncObjs,
  Alcinoe.FMX.Graphics;

type
  TALBufDrawableCacheEngine = class(Tobject)
  public
    type
      TEntry = Record
      public
        Drawable: TALDrawable;
        Rect: TRectF;
      end;
  private
    FLock: TLightweightMREW;
    FRefCount: Integer;
    FEntries: TArray<Tarray<TEntry>>; // [Index][SubIndex]
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function HasEntry(const AIndex, ASubIndex: Integer): Boolean;
    function TryGetEntry(const AIndex, ASubIndex: Integer; out ADrawable: TALDrawable; out ARect: TRectF): Boolean; overload;
    function TryGetEntry(const AIndex, ASubIndex: Integer; out ADrawable: TALDrawable): Boolean; overload;
    function TryGetEntry(const AIndex, ASubIndex: Integer; out ARect: TRectF): Boolean; overload;
    function TrySetEntry(const AIndex, ASubIndex: Integer; Const ADrawable: TALDrawable; const ARect: TRectF): Boolean;
    procedure CLearEntries;
    function IncreaseRefCount: TALBufDrawableCacheEngine;
    procedure DecreaseRefCount;
  end;


implementation

uses
  FMX.Types3D,
  FMX.Graphics,
  {$IF defined(ALSkiaAvailable)}
  System.Skia.API,
  {$ENDIF}
  System.SysUtils;

{*******************************************}
constructor TALBufDrawableCacheEngine.Create;
begin
  inherited;
  //FLock := ??; their is no TLightweightMREW.create but instead an ugly class operator TLightweightMREW.Initialize :(
  FRefCount := 1;
  FEntries := nil;
end;

{*******************************************}
destructor TALBufDrawableCacheEngine.Destroy;
begin
  CLearEntries;
  inherited;
end;

{*************************************************************************************}
function TALBufDrawableCacheEngine.HasEntry(const AIndex, ASubIndex: Integer): Boolean;
begin
  {$IF defined(debug)}
  If AIndex < 0 then raise Exception.Create('AIndex cannot be negative');
  If ASubIndex < 0 then raise Exception.Create('ASubIndex cannot be negative');
  {$ENDIF}
  FLock.BeginRead;
  try
    Result := (AIndex <= high(FEntries)) and
              (ASubIndex <= High(FEntries[AIndex])) and
              (not ALIsDrawableNull(FEntries[AIndex][ASubIndex].Drawable));
  finally
    FLock.EndRead;
  end;
end;

{***************************************************************************************************************************************}
function TALBufDrawableCacheEngine.TryGetEntry(const AIndex, ASubIndex: Integer; out ADrawable: TALDrawable; out ARect: TRectF): Boolean;
begin
  {$IF defined(debug)}
  If AIndex < 0 then raise Exception.Create('AIndex cannot be negative');
  If ASubIndex < 0 then raise Exception.Create('ASubIndex cannot be negative');
  {$ENDIF}
  FLock.BeginRead;
  try
    if (AIndex > high(FEntries)) or
       (ASubIndex > High(FEntries[AIndex])) then begin
      ADrawable := ALNullDrawable;
      ARect := TrectF.Empty;
      Exit(False);
    end;
    ADrawable := FEntries[AIndex][ASubIndex].Drawable;
    ARect := FEntries[AIndex][ASubIndex].Rect;
    Result := not ALIsDrawableNull(ADrawable);
  finally
    FLock.EndRead;
  end;
end;

{********************************************************************************************************************}
function TALBufDrawableCacheEngine.TryGetEntry(const AIndex, ASubIndex: Integer; out ADrawable: TALDrawable): Boolean;
begin
  var LRect: TRectF;
  Result := TryGetEntry(AIndex, ASubIndex, ADrawable, LRect);
end;

{***********************************************************************************************************}
function TALBufDrawableCacheEngine.TryGetEntry(const AIndex, ASubIndex: Integer; out ARect: TRectF): Boolean;
begin
  var LDrawable: TALDrawable;
  Result := TryGetEntry(AIndex, ASubIndex, LDrawable, ARect);
end;

{*******************************************************************************************************************************************}
function TALBufDrawableCacheEngine.TrySetEntry(const AIndex, ASubIndex: Integer; Const ADrawable: TALDrawable; const ARect: TRectF): Boolean;
begin
  {$IF defined(debug)}
  If AIndex < 0 then raise Exception.Create('AIndex cannot be negative');
  If ASubIndex < 0 then raise Exception.Create('ASubIndex cannot be negative');
  If AIndex > 1000 then raise Exception.Create('Invalid value for AIndex: must be less than or equal to 1000');
  If ASubIndex > 100 then raise Exception.Create('Invalid value for ASubIndex: must be less than or equal to 100');
  {$ENDIF}
  FLock.BeginWrite;
  try
    if AIndex > high(FEntries) then begin
      var LOldHigh := high(FEntries);
      setlength(FEntries, AIndex + 1);
      For var I := LOldHigh + 1 to high(FEntries) do
        FEntries[i] := nil;
    end;
    if ASubIndex > High(FEntries[AIndex]) then begin
      var LOldHigh := high(FEntries[AIndex]);
      setlength(FEntries[AIndex], ASubIndex + 1);
      For var I := LOldHigh + 1 to high(FEntries[AIndex]) do
        FEntries[AIndex][i].Drawable := ALNullDrawable;
    end;
    if not ALIsDrawableNull(FEntries[AIndex][ASubIndex].Drawable) then exit(false);
    FEntries[AIndex][ASubIndex].Drawable := ADrawable;
    FEntries[AIndex][ASubIndex].Rect := ARect;
    Result := True;
  finally
    FLock.EndWrite;
  end;
end;

{***********************************************}
procedure TALBufDrawableCacheEngine.CLearEntries;
begin
  FLock.BeginWrite;
  try
    For var I := low(FEntries) to high(FEntries) do
      for var J := Low(FEntries[I]) to High(FEntries[I]) do
        ALFreeAndNilDrawable(FEntries[I][j].Drawable);
    setlength(FEntries, 0);
  finally
    FLock.EndWrite;
  end;
end;

{***************************************************}
procedure TALBufDrawableCacheEngine.DecreaseRefCount;
begin
  If AtomicDecrement(FRefCount) = 0 then
    Free;
end;

{*****************************************************************************}
function TALBufDrawableCacheEngine.IncreaseRefCount: TALBufDrawableCacheEngine;
begin
  AtomicIncrement(FrefCount);
  Result := Self;
end;

end.
