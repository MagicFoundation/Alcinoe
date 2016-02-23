unit ALFmxScrollBox;

interface

uses system.Classes,
     system.types,
     FMX.layouts;

type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  //this class is made to correct this bug: https://quality.embarcadero.com/browse/RSP-13447
  TALVertScrollBox = class(TVertScrollBox)
  private
    fLastRealignRect: TRectF;
  protected
    procedure DoRealignContent(R: TRectF); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  //this class is made to correct this bug: https://quality.embarcadero.com/browse/RSP-13447
  TALFramedVertScrollBox = class(TFramedVertScrollBox)
  private
    fLastRealignRect: TRectF;
  protected
    procedure DoRealignContent(R: TRectF); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

procedure Register;

implementation

uses system.Math,
     system.Math.Vectors,
     fmx.consts;

{*****************************************************}
procedure TALVertScrollBox.DoRealignContent(R: TRectF);
begin
  if sameValue(R.Width,  fLastRealignRect.Width,  Tepsilon.position)  and
     samevalue(R.Height, fLastRealignRect.Height, Tepsilon.position) then exit;
  fLastRealignRect := R;
  inherited;
end;

{******************************************************}
constructor TALVertScrollBox.Create(AOwner: TComponent);
begin
  fLastRealignRect := TrectF.Create(0,0,0,0);
  inherited;
end;

{***********************************************************}
procedure TALFramedVertScrollBox.DoRealignContent(R: TRectF);
begin
  if sameValue(R.Width,  fLastRealignRect.Width,  Tepsilon.position)  and
     samevalue(R.Height, fLastRealignRect.Height, Tepsilon.position) then exit;
  fLastRealignRect := R;
  inherited;
end;

{************************************************************}
constructor TALFramedVertScrollBox.Create(AOwner: TComponent);
begin
  fLastRealignRect := TrectF.Create(0,0,0,0);
  inherited;
end;

procedure Register;
begin
  RegisterComponents('Alcinoe', [TALVertScrollBox, TALFramedVertScrollBox]);
end;

end.
