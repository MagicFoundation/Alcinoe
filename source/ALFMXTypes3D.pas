unit ALFMXTypes3D;

interface

uses System.Classes,
     FMX.types,
     FMX.Types3D;

type


  {$IF CompilerVersion <> 31}
    {$MESSAGE WARN 'Check if FMX.Types3D.TTexture still has the exact same fields and adjust the IFDEF'}
  {$ENDIF}
  TALTextureAccessPrivate = class(TInterfacedPersistent)
  public
    FWidth: Integer;
    FHeight: Integer;
    FPixelFormat: TPixelFormat;
    FHandle: TTextureHandle;
    FStyle: TTextureStyles;
    FMagFilter: TTextureFilter;
    FMinFilter: TTextureFilter;
    FTextureScale: Single;  // << i need to access this field
    FRequireInitializeAfterLost: Boolean;
    FBits: Pointer;
    FContextLostId: Integer;
    FContextResetId: Integer;
  protected
  public
  end;

  {**************************}
  TALTexture = class(TTexture)
  private
    fVolatile: Boolean;
  protected
  public
    constructor Create(const aVolatile: Boolean = False); reintroduce;
    procedure Assign(Source: TPersistent); override;
  end;

implementation

uses fmx.graphics,
     fmx.surfaces
     {$IFDEF DEBUG}
     ,
     ALCommon,
     AlString
     {$ENDIF};

{**************************************************************}
constructor TALTexture.Create(const aVolatile: Boolean = False);
begin
   inherited Create;
   fVolatile := aVolatile;
   if fVolatile then Style := Style + [TTextureStyle.Volatile];
end;

{*************************}
{$IF CompilerVersion <> 31}
  {$MESSAGE WARN 'Check if FMX.Types3D.TTexture.assign is still having the same implementation as in previous version and adjust the IFDEF'}
{$ENDIF}
//
// i don't understand the sheet they do in TTexture.assign! they don't copy the Tbitmap data in the TTexture
// if TCanvasStyle.NeedGPUSurface in TBitmap(Source).CanvasClass.GetCanvasStyle !
// http://stackoverflow.com/questions/40095054/can-someone-explain-me-this-strange-behavior-in-ttexture-assign
//
// procedure TTexture.Assign(Source: TPersistent);
// begin
//   ...
//   if not (TCanvasStyle.NeedGPUSurface in TBitmap(Source).CanvasClass.GetCanvasStyle) then
//     begin
//     if TBitmap(Source).Map(TMapAccess.Read, M) then
//     try
//       UpdateTexture(M.Data, M.Pitch);
//     finally
//       TBitmap(Source).Unmap(M);
//     end;
//   end;
//   ...
// end;
//
procedure TALTexture.Assign(Source: TPersistent);
var
  M: TBitmapData;
begin

  Tmonitor.enter(Self); // << because when TTexture is used in multithread then this function can be call from background
                        //    thread when a main thread is calling ContextLostHandler/ContextResetHandler for exemple.
                        //    so you must update the FMX.Types3D and add in ContextLostHandler/ContextResetHandler also
                        //    Tmonitor.enter(Self) ... Tmonitor.exit(Self);
  Try

    if Source is TBitmap then begin
      if Handle <> 0 then TContextManager.DefaultContextClass.FinalizeTexture(Self);
      PixelFormat := TBitmap(Source).PixelFormat;
      Style := [TTextureStyle.Dynamic];
      if fVolatile then Style := Style + [TTextureStyle.Volatile];
      TALTextureAccessPrivate(self).fTextureScale := TBitmap(Source).BitmapScale;
      SetSize(TBitmap(Source).Width, TBitmap(Source).Height);
      if TBitmap(Source).Map(TMapAccess.Read, M) then
      try
        UpdateTexture(M.Data, M.Pitch);
      finally
        TBitmap(Source).Unmap(M);
      end;
    end

    else if Source is TBitmapSurface then begin
      if Handle <> 0 then TContextManager.DefaultContextClass.FinalizeTexture(Self);
      Style := [TTextureStyle.Dynamic];
      if fVolatile then Style := Style + [TTextureStyle.Volatile];
      SetSize(TBitmapSurface(Source).Width, TBitmapSurface(Source).Height);
      UpdateTexture(TBitmapSurface(Source).Bits, TBitmapSurface(Source).Pitch);
    end

    else inherited ;

    {$IFDEF DEBUG}
    if TALTextureAccessPrivate(self).FBits <> nil then
      ALLog('TALTexture.Assign', 'Bits: ' + ALFormatFloatU('0.##',(Width * Height * BytesPerPixel) / 1000, ALDefaultFormatSettingsU) +' kB', TalLogType.Warn);
    {$ENDIF}

  Finally
    Tmonitor.exit(Self);
  End;

end;


end.
