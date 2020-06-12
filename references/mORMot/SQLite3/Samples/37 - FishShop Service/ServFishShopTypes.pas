// type definitions shared by the Clients and Servers
unit ServFishShopTypes;

{$I Synopse.inc} // define HASINLINE CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  SynCommons,
  mORMot;

type
  TFish = packed record
    Name: RawUTF8;
    Price: currency;
    PictureURI: RawUTF8;
    ID: variant;
  end; // TSQLTable DynArraySave()
  TFishList = array of TFish;

  TFishDetailed = packed record
    Fish: TFish;
    Description: RawUTF8;
    WaterType: RawUTF8;
    WaterTemperature: double;
  end;
  // {"name":...,"price"...,"description":...,}

  TBasketItem = packed record
    ID: variant;
    count: integer;
    gift: boolean;
  end;
  TBasketList = array of TBasketItem;


  IFishShop = interface(IInvokable)
    ['{0A1295BF-9F29-4109-AD3B-84A0070725E4}']
    function GetSpecialOffersPicture: TServiceCustomAnswer;
    procedure GetFishList(out fishes: TFishList);
    procedure GetFishDetails(const id: variant;
      out fish: TFishDetailed);
    procedure Order(const basket: TBasketList;
      const cardnumber, cardcontrol, discountcoupon: RawUTF8);
  end;

const
  FISHSHOP_ROOT = 'v1';
  FISHSHOP_PORT = '888';
  FISHSHOP_CONTRACT = 'FishShop1';


(********************* Data Persistence Layer *************)

type
  IFishData = interface(IInvokable)
    ['{1A7DE36A-AA2F-499D-9F51-AF1EA5A668B2}']
    procedure GetFishList(out fishes: TFishList);
    procedure GetFishDetails(const id: variant;
      out fish: TFishDetailed);
  end;

  TFishDataLegacyDB = class(TInterfacedObject, IFishData)
  protected
    fDetailsCache: TSynDictionary;
  public
    procedure GetFishList(out fishes: TFishList);
    procedure GetFishDetails(const id: variant;
      out fish: TFishDetailed);
  end;

implementation

const
  _TFish = 'name:RawUTF8 price:currency pictureuri:RawUTF8 id:variant';
  _TFishDetailed = _TFish + ' description,watertype:RawUTF8 ' +
    'watertemperature:double';

{ TFishDataLegacyDB }

procedure TFishDataLegacyDB.GetFishDetails(const id: variant;
  out fish: TFishDetailed);
begin
  // SQL here
end;

procedure TFishDataLegacyDB.GetFishList(out fishes: TFishList);
begin
  // SQL here
end;

initialization
  TJSONSerializer.RegisterCustomJSONSerializerFromText([
    TypeInfo(TFish), _TFish,
    TypeInfo(TFishDetailed), _TFishDetailed
    ])
end.
