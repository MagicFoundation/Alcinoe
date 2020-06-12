unit ServFishShopMain;

{$I Synopse.inc} // define HASINLINE CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  SynCommons,
  mORMot,
  mORMotHttpServer,
  mORMotService,
  ServFishShopTypes;

type
  EFishShop = class(ESynException);

  TFishShopService = class(TInterfacedObject, IFishShop)
  protected
    fFishData: IFishData;
  public
    function GetSpecialOffersPicture: TServiceCustomAnswer;
    procedure GetFishList(out fishes: TFishList);
    procedure GetFishDetails(const id: variant;
      out fish: TFishDetailed);
    procedure Order(const basket: TBasketList;
      const cardnumber, cardcontrol, discountcoupon: RawUTF8);
  end;

  TFishShopSettings = class(TSynDaemonSettings)
  published
  end;

  TFishShopDaemon = class(TSynDaemon)
  protected
    fServer: TSQLRestServer;
    fHttpServer: TSQLHttpServer;
    fFishData: IFishData;
  public
    procedure Start; override;
    procedure Stop; override;
  end;



implementation

{ TFishShopService }

function TFishShopService.GetSpecialOffersPicture: TServiceCustomAnswer;
begin

end;

procedure TFishShopService.GetFishList(out fishes: TFishList);
begin
  if Assigned(fFishData) then
    fFishData.GetFishList(fishes)
  else
    raise EFishShop.Create('Panic Mode');
end;

procedure TFishShopService.GetFishDetails(const id: variant;
  out fish: TFishDetailed);
begin
  if VarIsEmptyOrNull(id) then
    exit;

end;

procedure TFishShopService.Order(const basket: TBasketList; const cardnumber,
  cardcontrol, discountcoupon: RawUTF8);
begin

end;


{ TFishShopDaemon }

procedure TFishShopDaemon.Start;
var
  instance: TFishShopService;
begin
  fFishData := TFishDataLegacyDB.Create;
  fServer := TSQLRestServerFullMemory.CreateWithOwnModel([], False, FISHSHOP_ROOT);
  instance := TFishShopService.Create;
  instance.fFishData := fFishData;
  fServer.ServiceDefine(instance, [IFishShop], FISHSHOP_CONTRACT);
  fHttpServer := TSQLHttpServer.Create(FISHSHOP_PORT, fServer);
end;

procedure TFishShopDaemon.Stop;
begin
  fServer.Free;
  fHttpServer.Free;
  fFishData := nil;
end;

end.
