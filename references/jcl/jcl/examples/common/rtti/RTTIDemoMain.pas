unit RTTIDemoMain;

interface

{$I jcl.inc}

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    mmResult: TMemo;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  JclSysUtils, JclRTTI, TypInfo;

type
  TDifficultEvent = procedure(const Sender: TObject; var I: Integer; out Stuff;
    IntArr: array of Integer; const VarArray: array of const) of object;

  TLargeEnum = (
    le001, le002, le003, le004, le005, le006, le007, le008, le009, le010,
    le011, le012, le013, le014, le015, le016, le017, le018, le019, le020,
    le021, le022, le023, le024, le025, le026, le027, le028, le029, le030,
    le031, le032, le033, le034, le035, le036, le037, le038, le039, le040,
    le041, le042, le043, le044, le045, le046, le047, le048, le049, le050,
    le051, le052, le053, le054, le055, le056, le057, le058, le059, le060,
    le061, le062, le063, le064, le065, le066, le067, le068, le069, le070,
    le071, le072, le073, le074, le075, le076, le077, le078, le079, le080,
    le081, le082, le083, le084, le085, le086, le087, le088, le089, le090,
    le091, le092, le093, le094, le095, le096, le097, le098, le099, le100,
    le101, le102, le103, le104, le105, le106, le107, le108, le109, le110,
    le111, le112, le113, le114, le115, le116, le117, le118, le119, le120,
    le121, le122, le123, le124, le125, le126, le127, le128, le129, le130,
    le131, le132, le133, le134, le135, le136, le137, le138, le139, le140,
    le141, le142, le143, le144, le145, le146, le147, le148, le149, le150,
    le151, le152, le153, le154, le155, le156, le157, le158, le159, le160);
    
  TLargeSet = set of TLargeEnum;
  TLargeSubEnum = le019 .. le150;
  TLargeSubSet = set of TLargeSubEnum;

  TIntRange = 0 .. 112;

  TSetNoEnum = set of (st01, st02, st03, st04);
  TSetOfByte = set of Byte;
  TInt2Range = 4..11;
  TSetOfIntRange = set of TInt2Range;

  TUpcaseRange = 'A' .. 'Z';

  TMyDouble = Double;
  TMyDouble2 = type Double;

  TIntArray = array of Integer;
  TIntArray2 = array of array of Integer;
  TEnumArray = array of (ar1, ar2, ar3);
  TRecArray = array of record x1: Integer; x2: Integer; end;
  TSetArray = array of set of (ars1, ars2, ars3);
  TSetArray2 = array of array of array of TSetNoEnum;
  TWideStrArray = array of Widestring;

var
  MyEnum: PTypeInfo;
  MySubRange: PTypeInfo;
  MySet: PTypeInfo;
  MyCutLowerEnum: PTypeInfo;

procedure TForm1.Button1Click(Sender: TObject);
var
  Writer: IJclInfoWriter;

begin
  mmResult.Lines.Clear;
  Writer := TJclInfoStringsWriter.Create(mmResult.Lines, 72);
  JclTypeInfo(TypeInfo(Word)).WriteTo(Writer);
  Writer.Writeln('');
  JclTypeInfo(TypeInfo(TIntRange)).WriteTo(Writer);
  Writer.Writeln('');
  JclTypeInfo(TypeInfo(TLargeEnum)).WriteTo(Writer);
  Writer.Writeln('');
  JclTypeInfo(TypeInfo(TLargeSubEnum)).WriteTo(Writer);
  Writer.Writeln('');
  JclTypeInfo(TypeInfo(TLargeSet)).WriteTo(Writer);
  Writer.Writeln('');
  JclTypeInfo(TypeInfo(TLargeSubSet)).WriteTo(Writer);
  Writer.Writeln('');
  JclTypeInfo(TypeInfo(TSetNoEnum)).WriteTo(Writer);
  Writer.Writeln('');
  JclTypeInfo(TypeInfo(TSetOfByte)).WriteTo(Writer);
  Writer.Writeln('');
  JclTypeInfo(TypeInfo(TSetOfIntRange)).WriteTo(Writer);
  Writer.Writeln('');
  JclTypeInfo(TypeInfo(Single)).WriteTo(Writer);
  Writer.Writeln('');
  JclTypeInfo(TypeInfo(Double)).WriteTo(Writer);
  Writer.Writeln('');
  JclTypeInfo(TypeInfo(Extended)).WriteTo(Writer);
  Writer.Writeln('');
  JclTypeInfo(TypeInfo(Comp)).WriteTo(Writer);
  Writer.Writeln('');
  JclTypeInfo(TypeInfo(Currency)).WriteTo(Writer);
  Writer.Writeln('');
  JclTypeInfo(TypeInfo(Real)).WriteTo(Writer);
  Writer.Writeln('');
  JclTypeInfo(TypeInfo(TDateTime)).WriteTo(Writer);
  Writer.Writeln('');
  JclTypeInfo(TypeInfo(TMyDouble)).WriteTo(Writer);
  Writer.Writeln('');
  JclTypeInfo(TypeInfo(TMyDouble2)).WriteTo(Writer);
  Writer.Writeln('');
  JclTypeInfo(TypeInfo(ShortString)).WriteTo(Writer);
  Writer.Writeln('');
  JclTypeInfo(TypeInfo(TScrollingWinControl)).WriteTo(Writer);
  Writer.Writeln('');
  JclTypeInfo(TypeInfo(TDifficultEvent)).WriteTo(Writer);
  Writer.Writeln('');
  JclTypeInfo(TypeInfo(IJclOrdinalRangeTypeInfo)).WriteTo(Writer);
  Writer.Writeln('');
  JclTypeInfo(TypeInfo(Int64)).WriteTo(Writer);
  Writer.Writeln('');
  JclTypeInfo(TypeInfo(Longword)).WriteTo(Writer);
  Writer.Writeln('');
  JclTypeInfo(TypeInfo(TIntArray)).WriteTo(Writer);
  Writer.Writeln('');
  JclTypeInfo(TypeInfo(TIntArray2)).WriteTo(Writer);
  Writer.Writeln('');
  JclTypeInfo(TypeInfo(TEnumArray)).WriteTo(Writer);
  Writer.Writeln('');
  JclTypeInfo(TypeInfo(TRecArray)).WriteTo(Writer);
  Writer.Writeln('');
  JclTypeInfo(TypeInfo(TSetArray)).WriteTo(Writer);
  Writer.Writeln('');
  JclTypeInfo(TypeInfo(TSetArray2)).WriteTo(Writer);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  Writer: IJclInfoWriter;
  LargeSubSet: TLargeSubSet;
  GUID: TGUID;

begin
  mmResult.Lines.Clear;
  Writer := TJclInfoStringsWriter.Create(mmResult.Lines, 72);
  Writer.Writeln('Set conversions:');
  Writer.Indent;
  try
    Writer.Writeln('StrToSet with string=''[le019..le023, le033, le045..le049]''');
    JclStrToSet(TypeInfo(TLargeSubSet), LargeSubSet, '[le019..le023, le033, le045..le049]');
    Writer.Writeln('SetToStr of StrToSet = ''' + JclSetToStr(TypeInfo(TLargeSubSet), LargeSubSet, True, True) + ''', with WantRanges=True');
    Writer.Writeln('SetToStr of StrToSet = ''' + JclSetToStr(TypeInfo(TLargeSubSet), LargeSubSet, True, False) + ''', with WantRanges=False');
    Writer.Writeln('');
    Writer.Writeln('StrToSet with string=''''');
    JclStrToSet(TypeInfo(TLargeSubSet), LargeSubSet, '');
    Writer.Writeln('SetToStr of StrToSet = ''' + JclSetToStr(TypeInfo(TLargeSubSet), LargeSubSet, True, True) + ''', with WantRanges=True');
    Writer.Writeln('SetToStr of StrToSet = ''' + JclSetToStr(TypeInfo(TLargeSubSet), LargeSubSet, True, False) + ''', with WantRanges=False');
    Writer.Writeln('');
    Writer.Writeln('StrToSet with string=''le019    ..    le023,le033     , le045 ..           le049           ''');
    JclStrToSet(TypeInfo(TLargeSubSet), LargeSubSet, 'le019    ..    le023,le033     , le045 ..           le049           ');
    Writer.Writeln('SetToStr of StrToSet = ''' + JclSetToStr(TypeInfo(TLargeSubSet), LargeSubSet, True, True) + ''', with WantRanges=True');
    Writer.Writeln('SetToStr of StrToSet = ''' + JclSetToStr(TypeInfo(TLargeSubSet), LargeSubSet, True, False) + ''', with WantRanges=False');
    Writer.Writeln('');
  finally
    Writer.Outdent;
  end;
  Writer.Writeln('GUID conversions:');
  Writer.Indent;
  try
    Writer.Writeln('GUIDToStr: ' + JclGUIDToString(IJclTypeInfo));
    GUID := JclStringToGUID(JclGUIDToString(IJclTypeInfo));
    Writer.Writeln('StrToGUID: ' + JclGUIDToString(GUID));
  finally
    Writer.Outdent;
  end;
  {$IFDEF COMPILER5_UP}
  Writer.Writeln('');
  Writer.Writeln('Integer conversions:');
  Writer.Indent;
  try
    Writer.Writeln('TypedIntToStr: ' + JclTypedIntToStr(crArrow, TypeInfo(TCursor)));
    Writer.Writeln('StrToTypedInt: ' + IntToStr(JclStrToTypedInt('crArrow', TypeInfo(TCursor))) + ' (should be ' + IntToStr(crArrow) + ')');
    Writer.Writeln('');
    Writer.Writeln('TypedIntToStr: ' + JclTypedIntToStr(1, TypeInfo(TCursor)));
    Writer.Writeln('StrToTypedInt: ' + IntToStr(JclStrToTypedInt('1', TypeInfo(TCursor))) + ' (should be 1)');
  finally
    Writer.Outdent;
  end;
  {$ENDIF}
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  Writer: IJclInfoWriter;

begin
  mmResult.Lines.Clear;
  Writer := TJclInfoStringsWriter.Create(mmResult.Lines, 80);
  Writer.Writeln('Declarations:');
  Writer.Indent;
  try
    JclTypeInfo(TypeInfo(TLargeEnum)).DeclarationTo(Writer);
    Writer.Writeln('');
    JclTypeInfo(TypeInfo(TLargeSubEnum)).DeclarationTo(Writer);
    Writer.Writeln('');
    JclTypeInfo(TypeInfo(TLargeSet)).DeclarationTo(Writer);
    Writer.Writeln('');
    JclTypeInfo(TypeInfo(TSetNoEnum)).DeclarationTo(Writer);
    Writer.Writeln('');
    JclTypeInfo(TypeInfo(Byte)).DeclarationTo(Writer);
    Writer.Writeln('');
    JclTypeInfo(TypeInfo(TSetOfByte)).DeclarationTo(Writer);
    Writer.Writeln('');
    JclTypeInfo(TypeInfo(Char)).DeclarationTo(Writer);
    Writer.Writeln('');
    JclTypeInfo(TypeInfo(TUpcaseRange)).DeclarationTo(Writer);
    Writer.Writeln('');
    JclTypeInfo(TypeInfo(TDifficultEvent)).DeclarationTo(Writer);
    Writer.Writeln('');
    JclTypeInfo(TypeInfo(IJclBaseInfo)).DeclarationTo(Writer);
    Writer.Writeln('');
    JclTypeInfo(TypeInfo(IJclTypeInfo)).DeclarationTo(Writer);
    Writer.Writeln('');
    JclTypeInfo(TypeInfo(TDateTime)).DeclarationTo(Writer);
    Writer.Writeln('');
    JclTypeInfo(TypeInfo(TMyDouble)).DeclarationTo(Writer);
    Writer.Writeln('');
    JclTypeInfo(TypeInfo(TMyDouble2)).DeclarationTo(Writer);
    Writer.Writeln('');
    JclTypeInfo(TypeInfo(TScrollingWinControl)).DeclarationTo(Writer);
    Writer.Writeln('');
    JclTypeInfo(TypeInfo(TJclInfoWriter)).DeclarationTo(Writer);
    Writer.Writeln('');
    JclTypeInfo(TypeInfo(TPersistent)).DeclarationTo(Writer);
    Writer.Writeln('');
    JclTypeInfo(TypeInfo(TIntArray)).DeclarationTo(Writer);
    Writer.Writeln('');
    JclTypeInfo(TypeInfo(TIntArray2)).DeclarationTo(Writer);
    Writer.Writeln('');
    JclTypeInfo(TypeInfo(TEnumArray)).DeclarationTo(Writer);
    Writer.Writeln('');
    JclTypeInfo(TypeInfo(TRecArray)).DeclarationTo(Writer);
    Writer.Writeln('');
    JclTypeInfo(TypeInfo(TSetArray)).DeclarationTo(Writer);
    Writer.Writeln('');
    JclTypeInfo(TypeInfo(TSetArray2)).DeclarationTo(Writer);
    Writer.Writeln('');
    JclTypeInfo(TypeInfo(TWideStrArray)).DeclarationTo(Writer);
  finally
    Writer.Outdent;
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  Writer: IJclInfoWriter;

begin
  mmResult.Lines.Clear;
  Writer := TJclInfoStringsWriter.Create(mmResult.Lines, 80);
  Writer.Writeln('JclGenerateEnumType with literals:');
  Writer.Indent;
  try
    JclTypeInfo(MyEnum).WriteTo(Writer);
    Writer.Writeln;
    JclTypeInfo(MyEnum).DeclarationTo(Writer);
  finally
    Writer.Outdent;
  end;
  Writer.Writeln;
  Writer.Writeln('JclGenerateSubRange:');
  Writer.Indent;
  try
    JclTypeInfo(MySubRange).WriteTo(Writer);
    Writer.Writeln;
    JclTypeInfo(MySubRange).DeclarationTo(Writer);
  finally
    Writer.Outdent;
  end;
  Writer.Writeln;
  Writer.Writeln('JclGenerateSetType:');
  Writer.Indent;
  try
    JclTypeInfo(MySet).WriteTo(Writer);
    Writer.Writeln;
    JclTypeInfo(MySet).DeclarationTo(Writer);
  finally
    Writer.Outdent;
  end;
  Writer.Writeln;
  Writer.Writeln('JclGenerateEnumType based on TLargeEnum:');
  Writer.Indent;
  try
    JclTypeInfo(MyCutLowerEnum).WriteTo(Writer);
    Writer.Writeln;
    JclTypeInfo(MyCutLowerEnum).DeclarationTo(Writer);
  finally
    Writer.Outdent;
  end;
end;

initialization
  //JclHookIs(JclIsClassByName);
  MyEnum := JclGenerateEnumType('MyEnum', ['First value', 'Second value',
    'Third value', 'Fourth value', 'Fifth value']);
  MySubRange := JclGenerateSubRange(MyEnum, 'MySubRange', 1, 3);
  MySet := JclGenerateSetType(MyEnum, 'MySet');
  MyCutLowerEnum := JclGenerateEnumTypeBasedOn('MyCutLower', TypeInfo(TLargeEnum),
    PREFIX_CUT_LOWERCASE);

end.
