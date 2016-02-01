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
    fLastWidth: Single;
    fLastHeight: Single;
  protected
    procedure DoRealignContent(R: TRectF); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  //this class is made to correct this bug: https://quality.embarcadero.com/browse/RSP-13447
  TALFramedVertScrollBox = class(TFramedVertScrollBox)
  private
    fLastWidth: Single;
    fLastHeight: Single;
  protected
    procedure DoRealignContent(R: TRectF); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

procedure Register;

implementation

{*****************************************************}
procedure TALVertScrollBox.DoRealignContent(R: TRectF);
begin
  if (R.Width = fLastwidth) and (R.Height = fLastHeight) then exit;
  fLastwidth := R.Width;
  fLastHeight := R.Height;
  inherited;
end;

{******************************************************}
constructor TALVertScrollBox.Create(AOwner: TComponent);
begin
  fLastWidth := 0;
  fLastHeight := 0;
  inherited;
end;

{***********************************************************}
procedure TALFramedVertScrollBox.DoRealignContent(R: TRectF);
begin
  if (R.Width = fLastwidth) and (R.Height = fLastHeight) then exit;
  fLastwidth := R.Width;
  fLastHeight := R.Height;
  inherited;
end;

{************************************************************}
constructor TALFramedVertScrollBox.Create(AOwner: TComponent);
begin
  fLastWidth := 0;
  fLastHeight := 0;
  inherited;
end;

procedure Register;
begin
  RegisterComponents('Alcinoe', [TALVertScrollBox, TALFramedVertScrollBox]);
end;

end.
