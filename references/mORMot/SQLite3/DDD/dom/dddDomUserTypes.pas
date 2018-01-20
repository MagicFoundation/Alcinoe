/// shared DDD Domains: User objects definition
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit dddDomUserTypes;

{
    This file is part of Synopse mORMot framework.

    Synopse mORMot framework. Copyright (C) 2018 Arnaud Bouchez
      Synopse Informatique - https://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is Synopse mORMot framework.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2018
  the Initial Developer. All Rights Reserved.

  Contributor(s):

  
  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****

  Version 1.18
  - first public release, corresponding to Synopse mORMot Framework 1.18

}

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  SysUtils,
  Classes,
  SynCommons,
  SynTests,
  mORMot,
  mORMotDDD;


{ *********** Country Modeling }

type
  /// Country identifiers, following ISO 3166-1 standard
  TCountryIdentifier = (ccUndefined,
    ccAF,ccAX,ccAL,ccDZ,ccAS,ccAD,ccAO,ccAI,ccAQ,ccAG,ccAR,ccAM,ccAW,ccAU,ccAT,
    ccAZ,ccBS,ccBH,ccBD,ccBB,ccBY,ccBE,ccBZ,ccBJ,ccBM,ccBT,ccBO,ccBQ,ccBA,ccBW,
    ccBV,ccBR,ccIO,ccBN,ccBG,ccBF,ccBI,ccKH,ccCM,ccCA,ccCV,ccKY,ccCF,ccTD,ccCL,
    ccCN,ccCX,ccCC,ccCO,ccKM,ccCG,ccCD,ccCK,ccCR,ccCI,ccHR,ccCU,ccCW,ccCY,ccCZ,
    ccDK,ccDJ,ccDM,ccDO,ccEC,ccEG,ccSV,ccGQ,ccER,ccEE,ccET,ccFK,ccFO,ccFJ,ccFI,
    ccFR,ccGF,ccPF,ccTF,ccGA,ccGM,ccGE,ccDE,ccGH,ccGI,ccGR,ccGL,ccGD,ccGP,ccGU,
    ccGT,ccGG,ccGN,ccGW,ccGY,ccHT,ccHM,ccVA,ccHN,ccHK,ccHU,ccIS,ccIN,ccID,ccIR,
    ccIQ,ccIE,ccIM,ccIL,ccIT,ccJM,ccJP,ccJE,ccJO,ccKZ,ccKE,ccKI,ccKP,ccKR,ccKW,
    ccKG,ccLA,ccLV,ccLB,ccLS,ccLR,ccLY,ccLI,ccLT,ccLU,ccMO,ccMK,ccMG,ccMW,ccMY,
    ccMV,ccML,ccMT,ccMH,ccMQ,ccMR,ccMU,ccYT,ccMX,ccFM,ccMD,ccMC,ccMN,ccME,ccMS,
    ccMA,ccMZ,ccMM,ccNA,ccNR,ccNP,ccNL,ccNC,ccNZ,ccNI,ccNE,ccNG,ccNU,ccNF,ccMP,
    ccNO,ccOM,ccPK,ccPW,ccPS,ccPA,ccPG,ccPY,ccPE,ccPH,ccPN,ccPL,ccPT,ccPR,ccQA,
    ccRE,ccRO,ccRU,ccRW,ccBL,ccSH,ccKN,ccLC,ccMF,ccPM,ccVC,ccWS,ccSM,ccST,ccSA,
    ccSN,ccRS,ccSC,ccSL,ccSG,ccSX,ccSK,ccSI,ccSB,ccSO,ccZA,ccGS,ccSS,ccES,ccLK,
    ccSD,ccSR,ccSJ,ccSZ,ccSE,ccCH,ccSY,ccTW,ccTJ,ccTZ,ccTH,ccTL,ccTG,ccTK,ccTO,
    ccTT,ccTN,ccTR,ccTM,ccTC,ccTV,ccUG,ccUA,ccAE,ccGB,ccUS,ccUM,ccUY,ccUZ,ccVU,
    ccVE,ccVN,ccVG,ccVI,ccWF,ccEH,ccYE,ccZM,ccZW);

  TCountryIsoAlpha2 = type RawUTF8;
  TCountryIsoAlpha3 = type RawUTF8;
  TCountryIsoNumeric = type word;

  /// Country object
  // - includes conversion methods for ISO 3166-1 alpha-2/alpha-3/numeric codes
  // as explained in http://en.wikipedia.org/wiki/ISO_3166-1
  TCountry = class(TSynPersistent)
  protected
    fIso: TCountryIsoNumeric;
    fCache: packed record
      Identifier: TCountryIdentifier;
      Iso: TCountryIsoNumeric;
    end;
    function GetIdentifier: TCountryIdentifier;
    function GetIsoAlpha2: TCountryIsoAlpha2;
    function GetIsoAlpha3: TCountryIsoAlpha3;
    procedure SetIdentifier(const Value: TCountryIdentifier);
    procedure SetIsoAlpha2(const Value: TCountryIsoAlpha2);
    procedure SetIsoAlpha3(const Value: TCountryIsoAlpha3);
    function GetEnglish: RawUTF8;
  public
    /// built-in simple unit tests
    class procedure RegressionTests(test: TSynTestCase);
    /// returns TRUE if both Country instances have the same content
    // - slightly faster than global function ObjectEquals(self,another)
    function Equals(another: TCountry): boolean; reintroduce;
    /// internal enumerate corresponding to this country
    property Identifier: TCountryIdentifier read GetIdentifier write SetIdentifier;
    /// the ISO 3166-1 alpha-2 codes of this country
    property Alpha2: TCountryIsoAlpha2 read GetIsoAlpha2 write SetIsoAlpha2;
    /// the ISO 3166-1 alpha-3 codes of this countr
    property Alpha3: TCountryIsoAlpha3 read GetIsoAlpha3 write SetIsoAlpha3;
    /// plain English text of this country, e.g. 'France' or 'United States'
    property English: RawUTF8 read GetEnglish;
  published
    /// the stored and transmitted value is this ISO 3166-1 numeric 3-digit code
    property Iso: TCountryIsoNumeric read fIso write fIso;
  end;


{ *********** Address Modeling }

type
  TStreet = type RawUTF8;
  TCityArea = type RawUTF8;
  TCity = type RawUTF8;
  TRegion = type RawUTF8;
  TPostalCode = type RawUTF8;

  /// Address object
  // - we tried to follow a simple but worldwide layout - see
  // http://en.wikipedia.org/wiki/Address_%28geography%29#Address_format
  TAddress = class(TSynAutoCreateFields)
  protected
    fStreet1: TStreet;
    fStreet2: TStreet;
    fCityArea: TCityArea;
    fCity: TCity;
    fRegion: TRegion;
    fCode: TPostalCode;
    fCountry: TCountry;
  public
    function Equals(another: TAddress): boolean; reintroduce;
  published
    property Street1: TStreet read fStreet1 write fStreet1;
    property Street2: TStreet read fStreet2 write fStreet2;
    property CityArea: TCityArea read fCityArea write fCityArea;
    property City: TCity read fCity write fCity;
    property Region: TRegion read fRegion write fRegion;
    property Code: TPostalCode read fCode write fCode;
    property Country: TCountry read fCountry;
  end;

  TAddressObjArray = array of TAddress; 


{ *********** Person / User / Customer Modeling }

type
  TLastName = type RawUTF8;
  TFirstName = type RawUTF8;
  TMiddleName = type RawUTF8;
  TFullName = type RawUTF8;

  /// Person full name
  TPersonFullName = class(TSynPersistent)
  protected
    fFirst: TFirstName;
    fMiddle: TMiddleName;
    fLast: TLastName;
  public
    function Equals(another: TPersonFullName): boolean; reintroduce;
    function FullName(country: TCountryIdentifier=ccUndefined): TFullName; virtual;
  published
    property First: TFirstName read fFirst write fFirst;
    property Middle: TMiddleName read fMiddle write fMiddle;
    property Last: TLastName read fLast write fLast;
  end;

  /// Person birth date
  TPersonBirthDate = class(TSynPersistent)
  protected
    fDate: TDateTime;
  public
    function Equals(another: TPersonBirthDate): boolean; reintroduce;
    function Age: integer; overload;
    function Age(FromDate: TDateTime): integer; overload;
  published
    property Date: TDateTime read fDate write fDate;
  end;
  
  /// Person object
  TPerson = class(TSynAutoCreateFields)
  protected
    fBirthDate: TPersonBirthDate;
    fName: TPersonFullName;
  public
    function Equals(another: TPerson): boolean; reintroduce;
  published
    property Name: TPersonFullName read fName;
    property Birth: TPersonBirthDate read fBirthDate;
  end;

  TPhoneNumber = type RawUTF8;
  TEmailAddress = type RawUTF8;

  /// a Person object, with some contact information
  // - an User is a person, in the context of an application
  TPersonContactable = class(TPerson)
  protected
    fAddress: TAddress;
    fPhone1: TPhoneNumber;
    fPhone2: TPhoneNumber;
    fEmail: TEmailAddress;
  public
    function Equals(another: TPersonContactable): boolean; reintroduce;
    /// built-in simple unit tests
    class procedure RegressionTests(test: TSynTestCase);
  published
    property Address: TAddress read fAddress;
    property Phone1: TPhoneNumber read fPhone1 write fPhone1;
    property Phone2: TPhoneNumber read fPhone2 write fPhone2;
    property Email: TEmailAddress read fEmail write fEmail;
  end;

  TPersonContactableObjArray = array of TPersonContactable;


{ *********** Email Validation Modeling }

type
  /// the status of an email validation process
  TDomUserEmailValidation = (evUnknown, evValidated, evFailed);

  /// how a confirmation email is to be rendered, for email address validation
  // - this information will be available as data context, e.g. to the Mustache
  // template used for rendering of the email body
  TDomUserEmailTemplate = class(TSynPersistent)
  private
    fFileName: RawUTF8;
    fSenderEmail: RawUTF8;
    fSubject: RawUTF8;
    fApplication: RawUTF8;
    fInfo: variant;
  published
    /// the local file name of the Mustache template
    property FileName: RawUTF8 read fFileName write fFileName;
    /// the "sender" field of the validation email
    property SenderEmail: RawUTF8 read fSenderEmail write fSenderEmail;
    /// the "subject" field of the validation email
    property Subject: RawUTF8 read fSubject write fSubject;
    /// the name of the application, currently sending the confirmation
    property Application: RawUTF8 read fApplication write fApplication;
    /// any unstructured additional information, also supplied as data context
    property Info: variant read fInfo write fInfo;
  end;


{ *********** Application User Modeling, with Logon and Email Validation }

type
  TLogonName = type RawUTF8;

  /// an application level-user, whose account would be authenticated per Email
  TUser = class(TPersonContactable)
  private
    fLogonName: TLogonName;
    fEmailValidated: TDomUserEmailValidation;
  published
    /// the logon name would be the main entry point to the application
    property LogonName: TLogonName
      read fLogonName write fLogonName;
    /// will reflect the current state of email validation process for this user
    // - the validation is not handled by this class: this is just a property
    // which reflects the state of TDDDEmailValidationService/IDomUserEmailValidation 
    property EmailValidated: TDomUserEmailValidation
      read fEmailValidated write fEmailValidated;
  end;

  TUserObjArray = array of TUser;

  
implementation

{ TCountry }

const
  COUNTRY_NAME_EN: array[TCountryIdentifier] of RawUTF8 = ('',
    'Afghanistan','Aland Islands','Albania','Algeria','American Samoa',
    'Andorra','Angola','Anguilla','Antarctica','Antigua and Barbuda',
    'Argentina','Armenia','Aruba','Australia','Austria','Azerbaijan',
    'Bahamas','Bahrain','Bangladesh','Barbados','Belarus','Belgium',
    'Belize','Benin','Bermuda','Bhutan','Bolivia, Plurinational State of',
    'Bonaire, Sint Eustatius and Saba','Bosnia and Herzegovina','Botswana',
    'Bouvet Island','Brazil','British Indian Ocean Territory',
    'Brunei Darussalam','Bulgaria','Burkina Faso','Burundi','Cambodia',
    'Cameroon','Canada','Cape Verde','Cayman Islands','Central African Republic',
    'Chad','Chile','China','Christmas Island','Cocos (Keeling) Islands',
    'Colombia','Comoros','Congo','Congo, the Democratic Republic of the',
    'Cook Islands','Costa Rica','Ivory Coast','Croatia','Cuba','Curacao',
    'Cyprus','Czech Republic','Denmark','Djibouti','Dominica',
    'Dominican Republic','Ecuador','Egypt','El Salvador','Equatorial Guinea',
    'Eritrea','Estonia','Ethiopia','Falkland Islands (Malvinas)',
    'Faroe Islands','Fiji','Finland','France','French Guiana',
    'French Polynesia','French Southern Territories','Gabon','Gambia','Georgia',
    'Germany','Ghana','Gibraltar','Greece','Greenland','Grenada','Guadeloupe',
    'Guam','Guatemala','Guernsey','Guinea','Guinea-Bissau','Guyana','Haiti',
    'Heard Island and McDonald Islands','Holy See (Vatican City State)',
    'Honduras','Hong Kong','Hungary','Iceland','India','Indonesia',
    'Iran, Islamic Republic of','Iraq','Ireland','Isle of Man','Israel',
    'Italy','Jamaica','Japan','Jersey','Jordan','Kazakhstan','Kenya',
    'Kiribati','Korea, Democratic People''s Republic of','Korea, Republic of',
    'Kuwait','Kyrgyzstan','Lao People''s Democratic Republic','Latvia',
    'Lebanon','Lesotho','Liberia','Libyan Arab Jamahiriya','Liechtenstein',
    'Lithuania','Luxembourg','Macao','Macedonia, the former Yugoslav Republic of',
    'Madagascar','Malawi','Malaysia','Maldives','Mali','Malta','Marshall Islands',
    'Martinique','Mauritania','Mauritius','Mayotte','Mexico',
    'Micronesia, Federated States of','Moldova, Republic of','Monaco',
    'Mongolia','Montenegro','Montserrat','Morocco','Mozambique','Myanmar',
    'Namibia','Nauru','Nepal','Netherlands','New Caledonia','New Zealand',
    'Nicaragua','Niger','Nigeria','Niue','Norfolk Island',
    'Northern Mariana Islands','Norway','Oman','Pakistan','Palau',
    'Palestinian Territory','Panama','Papua New Guinea','Paraguay','Peru',
    'Philippines','Pitcairn','Poland','Portugal','Puerto Rico','Qatar',
    'Reunion','Romania','Russian Federation','Rwanda','Saint Barthelemy',
    'Saint Helena, Ascension and Tristan da Cunha','Saint Kitts and Nevis',
    'Saint Lucia','Saint Martin (French part)','Saint Pierre and Miquelon',
    'Saint Vincent and the Grenadines','Samoa','San Marino',
    'Sao Tome and Principe','Saudi Arabia','Senegal','Serbia',
    'Seychelles','Sierra Leone','Singapore','Sint Maarten (Dutch part)',
    'Slovakia','Slovenia','Solomon Islands','Somalia','South Africa',
    'South Georgia and the South Sandwich Islands','South Sudan','Spain',
    'Sri Lanka','Sudan','Suriname','Svalbard and Jan Mayen','Swaziland',
    'Sweden','Switzerland','Syrian Arab Republic','Taiwan, Province of China',
    'Tajikistan','Tanzania, United Republic of','Thailand','Timor-Leste',
    'Togo','Tokelau','Tonga','Trinidad and Tobago','Tunisia','Turkey',
    'Turkmenistan','Turks and Caicos Islands','Tuvalu','Uganda','Ukraine',
    'United Arab Emirates','United Kingdom','United States',
    'United States Minor Outlying Islands','Uruguay','Uzbekistan','Vanuatu',
    'Venezuela, Bolivarian Republic of','Viet Nam','Virgin Islands, British',
    'Virgin Islands, U.S.','Wallis and Futuna','Western Sahara','Yemen',
    'Zambia','Zimbabwe');

  COUNTRY_ISO3: array[TCountryIdentifier] of array[0..3] of AnsiChar = ('',
    'AFG','ALA','ALB','DZA','ASM','AND','AGO','AIA','ATA','ATG','ARG','ARM',
    'ABW','AUS','AUT','AZE','BHS','BHR','BGD','BRB','BLR','BEL','BLZ','BEN',
    'BMU','BTN','BOL','BES','BIH','BWA','BVT','BRA','IOT','BRN','BGR','BFA',
    'BDI','KHM','CMR','CAN','CPV','CYM','CAF','TCD','CHL','CHN','CXR','CCK',
    'COL','COM','COG','COD','COK','CRI','CIV','HRV','CUB','CUW','CYP','CZE',
    'DNK','DJI','DMA','DOM','ECU','EGY','SLV','GNQ','ERI','EST','ETH','FLK',
    'FRO','FJI','FIN','FRA','GUF','PYF','ATF','GAB','GMB','GEO','DEU','GHA',
    'GIB','GRC','GRL','GRD','GLP','GUM','GTM','GGY','GIN','GNB','GUY','HTI',
    'HMD','VAT','HND','HKG','HUN','ISL','IND','IDN','IRN','IRQ','IRL','IMN',
    'ISR','ITA','JAM','JPN','JEY','JOR','KAZ','KEN','KIR','PRK','KOR','KWT',
    'KGZ','LAO','LVA','LBN','LSO','LBR','LBY','LIE','LTU','LUX','MAC','MKD',
    'MDG','MWI','MYS','MDV','MLI','MLT','MHL','MTQ','MRT','MUS','MYT','MEX',
    'FSM','MDA','MCO','MNG','MNE','MSR','MAR','MOZ','MMR','NAM','NRU','NPL',
    'NLD','NCL','NZL','NIC','NER','NGA','NIU','NFK','MNP','NOR','OMN','PAK',
    'PLW','PSE','PAN','PNG','PRY','PER','PHL','PCN','POL','PRT','PRI','QAT',
    'REU','ROU','RUS','RWA','BLM','SHN','KNA','LCA','MAF','SPM','VCT','WSM',
    'SMR','STP','SAU','SEN','SRB','SYC','SLE','SGP','SXM','SVK','SVN','SLB',
    'SOM','ZAF','SGS','SSD','ESP','LKA','SDN','SUR','SJM','SWZ','SWE','CHE',
    'SYR','TWN','TJK','TZA','THA','TLS','TGO','TKL','TON','TTO','TUN','TUR',
    'TKM','TCA','TUV','UGA','UKR','ARE','GBR','USA','UMI','URY','UZB','VUT',
    'VEN','VNM','VGB','VIR','WLF','ESH','YEM','ZMB','ZWE');

  COUNTRY_ISONUM: array[TCountryIdentifier] of word = (0,
    4,248,8,12,16,20,24,660,10,28,32,51,533,36,40,31,44,48,50,52,112,56,84,
    204,60,64,68,535,70,72,74,76,86,96,100,854,108,116,120,124,132,136,140,
    148,152,156,162,166,170,174,178,180,184,188,384,191,192,531,196,203,208,
    262,212,214,218,818,222,226,232,233,231,238,234,242,246,250,254,258,260,
    266,270,268,276,288,292,300,304,308,312,316,320,831,324,624,328,332,334,
    336,340,344,348,352,356,360,364,368,372,833,376,380,388,392,832,400,398,
    404,296,408,410,414,417,418,428,422,426,430,434,438,440,442,446,807,450,
    454,458,462,466,470,584,474,478,480,175,484,583,498,492,496,499,500,504,
    508,104,516,520,524,528,540,554,558,562,566,570,574,580,578,512,586,585,
    275,591,598,600,604,608,612,616,620,630,634,638,642,643,646,652,654,659,
    662,663,666,670,882,674,678,682,686,688,690,694,702,534,703,705,90,706,
    710,239,728,724,144,729,740,744,748,752,756,760,158,762,834,764,626,768,
    772,776,780,788,792,795,796,798,800,804,784,826,840,581,858,860,548,862,
    704,92,850,876,732,887,894,716);

  ccFirst = succ(low(TCountryIdentifier));

var
  COUNTRY_ISO2: array[TCountryIdentifier] of word;
  COUNTRY_ISONUM_ORDERED: record // for fast binary search of the ISO numeric
    Values, Indexes: array[TCountryIdentifier] of integer;
  end;

procedure Initialize;
var c: TCountryIdentifier;
    ps: PAnsiChar; // circumvent FPC compilation issue
begin
  with COUNTRY_ISONUM_ORDERED do begin
    for c := ccFirst to high(c) do begin
      Values[c] := COUNTRY_ISONUM[c];
      ps := pointer(GetEnumName(TypeInfo(TCountryIdentifier),ord(c)));
      COUNTRY_ISO2[c] := PWord(ps+3)^;
    end;
    FillIncreasing(@Indexes,0,length(Indexes));
    QuickSortInteger(@Values,@Indexes,0,length(Values)-1);
  end;
end;

function TCountry.GetEnglish: RawUTF8;
begin
  result := COUNTRY_NAME_EN[GetIdentifier];
end;

function TCountry.GetIdentifier: TCountryIdentifier;
var ndx: integer;
begin
  if Iso=0 then begin
    result := ccUndefined;
    exit;
  end else
  if Iso=fCache.Iso then begin
    result := fCache.Identifier;
    exit;
  end;
  with COUNTRY_ISONUM_ORDERED do begin
    ndx := FastFindIntegerSorted(@Values,length(Values)-1,Iso);
    if ndx<0 then
      result := ccUndefined else
      byte(result) := Indexes[TCountryIdentifier(ndx)];
  end;
  fCache.Iso := Iso;
  fCache.Identifier := result;
end;

function TCountry.GetIsoAlpha2: TCountryIsoAlpha2;
begin
  SetString(result,PAnsiChar(@COUNTRY_ISO2[GetIdentifier]),2);
end;

function TCountry.GetIsoAlpha3: TCountryIsoAlpha3;
begin
  SetString(result,COUNTRY_ISO3[GetIdentifier],3);
end;

procedure TCountry.SetIdentifier(const Value: TCountryIdentifier);
begin
  fIso := COUNTRY_ISONUM[Value];
  if Value<>ccUndefined then begin
    fCache.Iso := fIso;
    fCache.Identifier := Value;
  end;
end;

procedure TCountry.SetIsoAlpha2(const Value: TCountryIsoAlpha2);
var up: RawUTF8;
    ndx: integer;
begin
  up := UpperCaseU(Trim(Value));
  if length(up)=2 then begin
    ndx := WordScanIndex(@COUNTRY_ISO2[ccFirst],length(COUNTRY_ISO2)-1,PWord(up)^);
    if ndx>=0 then begin
      SetIdentifier(TCountryIdentifier(ndx+1));
      exit;
    end;
  end;
  fIso := 0;
end;

procedure TCountry.SetIsoAlpha3(const Value: TCountryIsoAlpha3);
var up: RawUTF8;
    ndx: integer;
begin
  up := UpperCaseU(Trim(Value));
  if length(up)=3 then begin
    ndx := IntegerScanIndex(@COUNTRY_ISO3[ccFirst],length(COUNTRY_ISO3)-1,PCardinal(up)^);
    if ndx>=0 then begin
      SetIdentifier(TCountryIdentifier(ndx+1));
      exit;
    end;
  end;
  fIso := 0;
end;

class procedure TCountry.RegressionTests(test: TSynTestCase);
var c,c2: TCountry;
    i: TCountryIdentifier;
    t: RawUTF8;
begin
  c := TCountry.Create;
  c2 := TCountry.Create;
  with test do
  try
    c.Alpha2 := ' fR ';
    Check(c.Iso=250);
    Check(c.Identifier=ccFR);
    c.Alpha2 := ' zz ';
    Check(c.Iso=0);
    Check(c.Identifier=ccUndefined);
    c.Alpha2 := ' fzz ';
    Check(c.Iso=0);
    Check(c.Identifier=ccUndefined);
    c.Alpha3 := ' frA ';
    Check(c.Iso=250);
    Check(c.Identifier=ccFR);
    c.Alpha3 := ' frz ';
    Check(c.Iso=0);
    Check(c.Identifier=ccUndefined);
    for i := low(i) to high(i) do begin
      c.Iso := COUNTRY_ISONUM[i];
      t := c.Alpha2;
      Check(c.Identifier=i);
      c.Iso := 0;
      c.Alpha2 := t;
      Check(c.Identifier=i);
      Check(c.Iso=COUNTRY_ISONUM[i]);
    end;
    for i := low(i) to high(i) do begin
      c.Identifier := i;
      Check(c.Iso=COUNTRY_ISONUM[i]);
      Check(c.Identifier=i);
    end;
    for i := low(i) to high(i) do begin
      c.Alpha3 := COUNTRY_ISO3[i];
      Check(c.Iso=COUNTRY_ISONUM[i]);
      Check(c.Identifier=i);
      t := c.Alpha3;
      c.Iso := 0;
      c.Alpha3 := t;
      Check(c.Identifier=i);
      Check(c.Iso=COUNTRY_ISONUM[i]);
      CopyObject(c,c2);
      Check(c2.Iso=COUNTRY_ISONUM[i]);
      Check(c2.Alpha3=c.Alpha3);
      Check(ObjectEquals(c,c2,false));
      Check(ObjectEquals(c,c2,true));
    end;
  finally
    c2.Free;
    c.Free;
  end;
end;

function TCountry.Equals(another: TCountry): boolean;
begin
  if (self=nil) or (another=nil) then
    result := another=self else
    result := another.fIso=fIso;
end;


{ TAddress }

function TAddress.Equals(another: TAddress): boolean;
begin
  if (self=nil) or (another=nil) then
    result := another=self else
    result := (another.Street1=Street1) and (another.Street2=Street2) and
      (another.CityArea=CityArea) and (another.City=City) and
      (another.Region=Region) and another.Country.Equals(Country);
end;


{ TPersonFullName }

function TPersonFullName.Equals(another: TPersonFullName): boolean;
begin
  if (self=nil) or (another=nil) then
    result := another=self else
    result := (First=another.First) and (Last=another.Last) and
      (Middle=another.Middle);
end;

function TPersonFullName.FullName(country: TCountryIdentifier): TFullName;
begin // see country-specific http://en.wikipedia.org/wiki/Family_name
  case country of
  ccJP,ccCN,ccTW,ccKP,ccKR,ccVN,ccHU,ccRO:
    // Eastern Order
    result := Trim(Trim(Last+' '+Middle)+' '+First);
  else
    // default Western Order
    result := Trim(Trim(First+' '+Middle)+' '+Last);
  end; 
end;


{ TPersonBirthDate }

function TPersonBirthDate.Age: integer;
begin
  result := Age(SysUtils.Date);
end;

function TPersonBirthDate.Age(FromDate: TDateTime): integer;
var YF,YD,MF,MD,DF,DD: word;
begin
  if (self=nil) or (fDate=0) then
    result := 0 else begin
    DecodeDate(FromDate,YF,MF,DF);
    DecodeDate(fDate,YD,MD,DD);
    result := YF-YD;
    if MF<MD then
      dec(result) else
      if (MF=MD) and (DF<DD) then
        dec(result);
  end;
end;

function TPersonBirthDate.Equals(another: TPersonBirthDate): boolean;
begin
  if (self=nil) or (another=nil) then
    result := another=self else
    result := Date=another.Date;
end;


{ TPerson }

function TPerson.Equals(another: TPerson): boolean;
begin
  if (self=nil) or (another=nil) then
    result := another=self else
    result := Name.Equals(another.Name) and Birth.Equals(another.Birth);
end;


{ TPersonContactable }

function TPersonContactable.Equals(another: TPersonContactable): boolean;
begin
  if (self=nil) or (another=nil) then
    result := another=self else
    result := inherited Equals(Self) and Address.Equals(another.Address) and
      (Phone1=another.Phone1) and (Phone2=another.Phone2) and (Email=another.Email);
end;

class procedure TPersonContactable.RegressionTests(test: TSynTestCase);
var p: TPersonContactable;
    json: RawUTF8;
    valid: boolean;
procedure TestP;
begin
  test.Check(p.Phone2='123456');
  test.Check(p.Name.Last='Smith');
  test.Check(p.Name.First='John');
  test.Check(p.Birth.Age(Iso8601ToDateTime('19821030'))=10);
  test.Check(p.Address.Country.Alpha3='FRA');
end;
begin
  p := TPersonContactable.Create;
  with test do
  try
    p.Phone2 := '123456';
    p.Name.Last := 'Smith';
    p.Name.First := 'John';
    p.Birth.Date := Iso8601ToDateTime('19721029');
    Check(p.Birth.Age>40);
    Check(p.Birth.Age(Iso8601ToDateTime('19821020'))=9);
    Check(p.Birth.Age(Iso8601ToDateTime('19821030'))=10);
    p.Address.Country.Alpha2 := 'FR';
    json := ObjectToJSON(p)+'*';
  finally
    p.Free;
  end;
  p := TPersonContactable.Create;
  with test do
  try
    // FileFromString(JSONReformat(json),'person.json');
    Check(ObjectLoadJSON(p,json));
    TestP;
  finally
    p.Free;
  end;
  p := TPersonContactable.Create;
  with test do
  try
    Check(JSONToObject(p,pointer(json),valid)^='*');
    Check(valid);
    TestP;
  finally
    p.Free;
  end;
end;


initialization
  Initialize;
  {$ifndef ISDELPHI2010}
  {$ifndef HASINTERFACERTTI} // circumvent a old FPC bug
  TTextWriter.RegisterCustomJSONSerializerFromTextSimpleType(TypeInfo(TDomUserEmailValidation));
  TTextWriter.RegisterCustomJSONSerializerFromTextSimpleType(TypeInfo(TCountryIdentifier));
  {$endif}
  {$endif}
  TJSONSerializer.RegisterObjArrayForJSON([
    TypeInfo(TAddressObjArray),TAddress,
    TypeInfo(TPersonContactableObjArray),TPersonContactable,
    TypeInfo(TUserObjArray),TUser]);
end.
