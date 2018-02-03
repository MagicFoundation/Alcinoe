unit ClrDemoTableForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ComCtrls, JclClr, JclMetadata;

type
  TDumpLineKind = (lkWide, lkThin, lkEmpty);

  TfrmTable = class(TForm)
    lblVer: TLabel;            
    edtVer: TEdit;
    btnOK: TBitBtn;
    lstTables: TListView;
    memDump: TMemo;
    btnDumpIL: TButton;
    procedure lstTablesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure btnDumpILClick(Sender: TObject);
  private
    FStream: TJclClrTableStream;

    procedure Dump(const Msg: string); overload;
    procedure Dump(const FmtMsg: string; const Args: array of const); overload;
    procedure Dump(const Msg: string; const Blob: TJclClrBlobRecord); overload;
    procedure Dump(const LineKind: TDumpLineKind); overload;
    procedure ShowTables(const AStream: TJclClrTableStream);
    procedure DumpTable(const ATable: TJclClrTableAssembly); overload;
    procedure DumpTable(const ATable: TJclClrTableAssemblyRef); overload;
    procedure DumpTable(const ATable: TJclClrTableAssemblyOS); overload;
    procedure DumpTable(const ATable: TJclClrTableAssemblyProcessor); overload;
    procedure DumpTable(const ATable: TJclClrTableModule); overload;
    procedure DumpTable(const ATable: TJclClrTableModuleRef); overload;
    procedure DumpTable(const ATable: TJclClrTableFieldDef); overload;
    procedure DumpTable(const ATable: TJclClrTableMemberRef); overload;
    procedure DumpTable(const ATable: TJclClrTableCustomAttribute); overload;
    procedure DumpTable(const ATable: TJclClrTableMethodDef); overload;
    procedure DumpTable(const ATable: TJclClrTableTypeDef); overload;
    procedure DumpTable(const ATable: TJclClrTableTypeRef); overload;
    procedure DumpTable(const ATable: TJclClrTablePropertyDef); overload;
    procedure DumpTable(const ATable: TJclClrTableManifestResource); overload;
    procedure DumpTable(const ATable: TJclClrTableFile); overload;
    procedure DumpTable(const ATable: TJclClrTableParamDef); overload;
    procedure DumpTable(const ATable: TJclClrTableExportedType); overload;
  public
    class procedure Execute(const AStream: TJclClrTableStream);
  end;

implementation

{$R *.DFM}

{.$DEFINE USE_JWA}

uses
  ComObj, TypInfo, ClrDemoAbstractFrame,
{$IFDEF USE_JWA}
  JwaWinCrypt, JwaWinNT,
{$ENDIF}
  JclStrings, JclSysUtils, ClrDemoMain;

{ TfrmTable }

class procedure TfrmTable.Execute(const AStream: TJclClrTableStream);
begin
  with TfrmTable.Create(nil) do
  try
    ShowTables(AStream);
    ShowModal;
  finally
    Free;
  end;
end;

procedure TfrmTable.Dump(const Msg: string);
begin
  memDump.Lines.Add(Msg);
end;

procedure TfrmTable.Dump(const FmtMsg: string; const Args: array of const);
begin
  Dump(Format(FmtMsg, Args));
end;

procedure TfrmTable.Dump(const Msg: string; const Blob: TJclClrBlobRecord);
begin
  Dump(Msg);
  TfrmAbstract.DumpBuf(Blob, memDump);
end;

procedure TfrmTable.Dump(const LineKind: TDumpLineKind);
begin
  case LineKind of
    lkWide:  Dump('========================================');
    lkThin:  Dump('----------------------------------------');
    lkEmpty: Dump('');
  end;
end;

procedure TfrmTable.ShowTables(const AStream: TJclClrTableStream);
var
  AKind: TJclClrTableKind;
begin
  FStream     := AStream;
  edtVer.Text := AStream.VersionString;
  with lstTables.Items do
  begin
    BeginUpdate;
    try
      Clear;
      for AKind:=Low(TJclClrTableKind) to High(TJclClrTableKind) do
      if Assigned(AStream.Tables[AKind]) then
      with AStream.Tables[AKind], Add do
      begin
        Caption := IntToStr(Count);
        Data    := AStream.Tables[AKind];
        SubItems.Add(IntToStr(RowCount));
        SubItems.Add('$' + IntToHex(Offset, 8));
        SubItems.Add(IntToStr(Size));
        SubItems.Add(Copy(AStream.Tables[AKind].ClassName, StrLen('TJclClrTable')+1, MaxWord));
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TfrmTable.lstTablesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
var
  ATable: TJclClrTable;
begin
  if Selected then
  begin
    ATable := TJclClrTable(Item.Data);
    memDump.Clear;

    if ATable.ClassType = TJclClrTableAssembly then
      DumpTable(TJclClrTableAssembly(ATable))
    else if ATable.ClassType = TJclClrTableAssemblyRef then
      DumpTable(TJclClrTableAssemblyRef(ATable))
    else if ATable.ClassType = TJclClrTableAssemblyOS then
      DumpTable(TJclClrTableAssemblyOS(ATable))
    else if ATable.ClassType = TJclClrTableAssemblyProcessor then
      DumpTable(TJclClrTableAssemblyProcessor(ATable))
    else if ATable.ClassType = TJclClrTableModule then
      DumpTable(TJclClrTableModule(ATable))
    else if ATable.ClassType = TJclClrTableModuleRef then
      DumpTable(TJclClrTableModuleRef(ATable))
    else if ATable.ClassType = TJclClrTableTypeDef then
      DumpTable(TJclClrTableTypeDef(ATable))
    else if ATable.ClassType = TJclClrTableTypeRef then
      DumpTable(TJclClrTableTypeRef(ATable))
    else if ATable.ClassType = TJclClrTableMethodDef then
      DumpTable(TJclClrTableMethodDef(ATable))
    else if ATable.ClassType = TJclClrTableFieldDef then
      DumpTable(TJclClrTableFieldDef(ATable))
    else if ATable.ClassType = TJclClrTableMemberRef then
      DumpTable(TJclClrTableMemberRef(ATable))
    else if ATable.ClassType = TJclClrTableCustomAttribute then
      DumpTable(TJclClrTableCustomAttribute(ATable))
    else if ATable.ClassType = TJclClrTableParamDef then
      DumpTable(TJclClrTableParamDef(ATable))
    else if ATable.ClassType = TJclClrTablePropertyDef then
      DumpTable(TJclClrTablePropertyDef(ATable))
    else if ATable.ClassType = TJclClrTableFile then
      DumpTable(TJclClrTableFile(ATable))
    else if ATable.ClassType = TJclClrTableManifestResource then
      DumpTable(TJclClrTableManifestResource(ATable))
    else if ATable.ClassType = TJclClrTableExportedType then
      DumpTable(TJclClrTableExportedType(ATable));

    memDump.Perform(WM_VSCROLL, SB_TOP, 0);
  end;
end;

procedure TfrmTable.DumpTable(const ATable: TJclClrTableAssembly);
  function GetHashAlgName(const HashAlgId: DWORD): string;
  begin
  {$IFDEF USE_JWA}
    case HashAlgId of
    CALG_MD2:  Result := 'MD2';
    CALG_MD4:  Result := 'MD4';
    CALG_MD5:  Result := 'MD5';
    CALG_SHA1: Result := 'SHA1';
    CALG_MAC:  Result := 'MAC';
    else
      Result := IntToHex(HashAlgId, 8);
    end;
  {$ELSE}
    Result := IntToHex(HashAlgId, 8);
  {$ENDIF}
  end;
var
  AFlag: TJclClrAssemblyFlag;
  FlagMsg: string;
begin
  Assert(ATable.RowCount = 1);
  with ATable[0] do
  begin
    Dump('Name: ' + Name);
    Dump('Version: ' + Version);
    FlagMsg := 'Flag: ';
    for AFlag := Low(TJclClrAssemblyFlag) to High(TJclClrAssemblyFlag) do
      if AFlag in Flags then
        FlagMsg := FlagMsg +
          GetEnumName(TypeInfo(TJclClrAssemblyFlag), Integer(AFlag)) + ' ';
    Dump(FlagMsg);
    if CultureOffset <> 0 then
      Dump('Culture: ' + Culture);
    Dump('Hash Algorithm: ' + GetHashAlgName(HashAlgId));
    if Assigned(PublicKey) then
      Dump('Public Key: ', PublicKey);
  end;
end;

procedure TfrmTable.DumpTable(const ATable: TJclClrTableAssemblyRef);
var
  I: Integer;
  AFlag: TJclClrAssemblyFlag;
  FlagMsg: string;
  Assembly: TJclClrTableAssemblyRefRow;
begin
  for I:=0 to ATable.RowCount-1 do
  begin
    Assembly := ATable[I];
    Dump('Name: ' + Assembly.Name);
    Dump('Version: ' + Assembly.Version);
    FlagMsg := 'Flag: ';
    for AFlag := Low(TJclClrAssemblyFlag) to High(TJclClrAssemblyFlag) do
      if AFlag in Assembly.Flags then
        FlagMsg := FlagMsg +
          GetEnumName(TypeInfo(TJclClrAssemblyFlag), Integer(AFlag)) + ' ';
    Dump(FlagMsg);
    if Assembly.CultureOffset <> 0 then
      Dump('Culture: ' + Assembly.Culture);
    if Assigned(Assembly.PublicKeyOrToken) then
      Dump('Public Key or Token: ', Assembly.PublicKeyOrToken);
    if Assigned(Assembly.HashValue) then
      Dump('Hash Value: ', Assembly.HashValue);
    Dump(lkWide);
  end;
end;

procedure TfrmTable.DumpTable(const ATable: TJclClrTableAssemblyOS);
  function GetOSName(const PlatformID: DWORD): string;
  begin
    case PlatformID of
    VER_PLATFORM_WIN32s:        Result := 'Win32s';
    VER_PLATFORM_WIN32_WINDOWS: Result := 'Windows';
    VER_PLATFORM_WIN32_NT:      Result := 'WinNT';
    else
      Result := IntToHex(PlatformID, 8);
    end;
  end;
var
  I: Integer;
begin
  for I:=0 to ATable.RowCount-1 do
  begin
    Dump('OS : ' + GetOSName(ATable[I].PlatformID));
    Dump('Version: ' + ATable[I].Version);
  end;
end;

procedure TfrmTable.DumpTable(const ATable: TJclClrTableAssemblyProcessor);
  function GetProcessName(const Processor: DWORD): string;
  begin
  {$IFDEF USE_JWA}
    case Processor of
      PROCESSOR_INTEL_386:     Result := 'Intel 386';
      PROCESSOR_INTEL_486:     Result := 'Intel 486';
      PROCESSOR_INTEL_PENTIUM: Result := 'Intel Pentium';
      PROCESSOR_INTEL_IA64:    Result := 'Intel IA64';
      PROCESSOR_MIPS_R4000:    Result := 'MIPS R4000';
      PROCESSOR_ALPHA_21064:   Result := 'Alpha 21064';
      PROCESSOR_PPC_601:       Result := 'Power PC 601';
      PROCESSOR_PPC_603:       Result := 'Power PC 603';
      PROCESSOR_PPC_604:       Result := 'Power PC 604';
      PROCESSOR_PPC_620:       Result := 'Power PC 620';
      PROCESSOR_OPTIL:         Result := 'MS IL';
      else
      Result := IntToStr(Processor) + ' [' + IntToHex(Processor, 8) + ']';
    end;
  {$ELSE}
    Result := IntToStr(Processor) + ' [' + IntToHex(Processor, 8) + ']';
  {$ENDIF}
  end;
var
  I: Integer;
begin
  for I:=0 to ATable.RowCount-1 do
  begin
    Dump('Processor : ' + GetProcessName(ATable[I].Processor));
  end;
end;

procedure TfrmTable.DumpTable(const ATable: TJclClrTableModule);
begin
  Assert(ATable.RowCount = 1);
  with ATable[0] do
  begin
    Dump('Name     : %s', [Name]);
    Dump('Mvid     : %s', [GUIDToString(Mvid)]);
    if HasEncId then
      Dump('EncId    : %s', [GUIDToString(EncId)]);
    if HasEncBaseId then
      Dump('EncBaseId: %s', [GUIDToString(EncBaseId)]);
  end;
end;

procedure TfrmTable.DumpTable(const ATable: TJclClrTableModuleRef);
var
  I: Integer;
begin
   for I:=0 to ATable.RowCount-1 do
    Dump('Name : ' + ATable[I].Name);
end;

procedure TfrmTable.DumpTable(const ATable: TJclClrTableTypeDef);
const
  ClassSemanticsNames: array[TJclClrClassSemantics] of string =
  ('.class', 'interface');
  TypeVisibilityNames: array[TJclClrTypeVisibility] of string =
  ('private', 'public', 'nested public', 'nested private', 'nested family',
   'nested assembly', 'nested famandassem', 'nested famorassem');
  AbstractNames: array[Boolean] of string = ('', 'abstract ');
  ClassLayoutNames: array[TJclClrClassLayout] of string =
  ('auto', 'sequential', 'explicit');
  StringFormattingNames: array[TJclClrStringFormatting] of string =
  ('ansi', 'unicode', 'autochar');
  ImportNames: array[Boolean] of string = ('', 'import ');
  SerializableNames: array[Boolean] of string = ('', 'serializable');
  SealedNames: array[Boolean] of string = ('', 'sealed ');
  SpecialNameNames: array[Boolean] of string = ('', 'specialname ');
  BeforeFieldInitNames: array[Boolean] of string = ('', 'beforefieldinit ');
  RTSpecialNameNames: array[Boolean] of string = ('', 'rtspecialname ');
  HasSecurityNames: array[Boolean] of string = ('', 'HasSecurity ');
var
  I, J: Integer;
begin
  for I:=0 to ATable.RowCount-1 do
  with ATable.Rows[I] do
  begin
    Dump('%s %s %s%s %s %s%s%s%s%s%s%s%s',
      [ClassSemanticsNames[ClassSemantics],
       TypeVisibilityNames[Visibility],
       AbstractNames[taAbstract in Attributes],
       ClassLayoutNames[ClassLayout],
       StringFormattingNames[StringFormatting],
       ImportNames[taImport in Attributes],
       SerializableNames[taSerializable in Attributes],
       SealedNames[taSealed in Attributes],
       SpecialNameNames[taSpecialName in Attributes],
       BeforeFieldInitNames[taBeforeFieldInit in Attributes],
       RTSpecialNameNames[taRTSpecialName in Attributes],
       HasSecurityNames[taHasSecurity in Attributes],
       FullName]);

    if HasField then
    for J:=0 to FieldCount-1 do
      Dump('  .field %s', [Fields[J].Name]);

    if HasMethod then
    for J:=0 to MethodCount-1 do
      Dump('  .method %s', [Methods[J].Name]);
  end;
end;

procedure TfrmTable.DumpTable(const ATable: TJclClrTableTypeRef); 
var
  I: Integer;
begin
  for I:=0 to ATable.RowCount-1 do
    Dump('%s.%s', [ATable.Rows[I].Namespace, ATable.Rows[I].Name])
end;

procedure TfrmTable.DumpTable(const ATable: TJclClrTableMethodDef); 
var
  I, J: Integer;
  AttrStr, ParamStr: string;
begin
  for I:=0 to ATable.RowCount-1 do
  with ATable.Rows[I] do
  begin
    if HasParam then
    begin
      ParamStr := '';
      for J:=0 to ParamCount-1 do
      begin
        if ParamStr <> '' then
          ParamStr := ParamStr + ', ';
        if Params[J].Flags <> [] then
        begin
          AttrStr := '';
          if pkIn in Params[J].Flags then
            AttrStr := AttrStr + 'In';
          if pkOut in Params[J].Flags then
          begin
            if AttrStr <> '' then
              AttrStr := AttrStr + ', ';
            AttrStr := AttrStr + 'Out';
          end;
          if pkOptional in Params[J].Flags then
          begin
            if AttrStr <> '' then
              AttrStr := AttrStr + ', ';
            AttrStr := AttrStr + 'Opt';
          end;
          if pkHasDefault in Params[J].Flags then
          begin
            if AttrStr <> '' then
              AttrStr := AttrStr + ', ';
            AttrStr := AttrStr + 'Default';
          end;
          if pkHasFieldMarshal in Params[J].Flags then
          begin
            if AttrStr <> '' then
              AttrStr := AttrStr + ', ';
            AttrStr := AttrStr + 'Marshal';
          end;
          ParamStr := ParamStr + '[' + AttrStr + '] ';
        end;
        ParamStr := ParamStr + Params[J].Name;
      end;
    end;
    if Assigned(MethodBody) then
    begin
      Dump('%s.%s::%s(%s) @ %p:%d', [ParentToken.Namespace, ParentToken.Name, Name, ParamStr, Pointer(RVA), MethodBody.Size]);
      TfrmAbstract.DumpBuf(MethodBody.Code, MethodBody.Size, memDump, DWORD(MethodBody.Code), False);
    end
    else
    begin
      Dump('%s.%s::%s(%s)', [ParentToken.Namespace, ParentToken.Name, Name, ParamStr]);
    end;
  end;
end;

procedure TfrmTable.DumpTable(const ATable: TJclClrTableFieldDef); 
var
  I: Integer;
begin
  for I:=0 to ATable.RowCount-1 do
    Dump('%s', [ATable.Rows[I].Name])
end;

procedure TfrmTable.DumpTable(const ATable: TJclClrTableMemberRef); 
var
  I: Integer;
begin
  for I:=0 to ATable.RowCount-1 do
    Dump('%s', [ATable.Rows[I].Name])
end;

procedure TfrmTable.DumpTable(const ATable: TJclClrTableCustomAttribute); 
  function GetParent(const Attr: TJclClrTableCustomAttributeRow): string;
  var
    ARow: TJclClrTableRow;
  begin
    ARow   := Attr.Parent;
    if ARow is TJclClrTableAssemblyRow then
      with ARow as TJclClrTableAssemblyRow do
        Result := Name
    else if ARow is TJclClrTableTypeDefRow then
      with ARow as TJclClrTableTypeDefRow do
        Result := Namespace + '.' + Name
    else if ARow is TJclClrTableTypeRefRow then
      with ARow as TJclClrTableTypeRefRow do
        Result := Namespace + '.' + Name
    else if ARow is TJclClrTableMethodDefRow then
      with ARow as TJclClrTableMethodDefRow do
        Result := Name
    else if ARow is TJclClrTableParamDefRow then
      with ARow as TJclClrTableParamDefRow do
        Result := Method.ParentToken.Namespace + '.' + Method.ParentToken.Name + '::' +
                  Method.Name + '(..., ' + Name + ', ...)'
    else
      Result := 'Unknown Parent';

    Result := Result + ' <' + Copy(ARow.ClassName, Length('TJclClrTable')+1,
        Length(ARow.ClassName)-Length('TJclClrTable')-Length('Row')) +
        '> [' + IntToHex(Attr.ParentIdx, 8) + ']';
  end;
  function GetMethod(const Attr: TJclClrTableCustomAttributeRow): string;
    function GetParentClassName(const ParentClass: TJclClrTableRow): string;
    begin
      if ParentClass is TJclClrTableTypeRefRow then
        with ParentClass as TJclClrTableTypeRefRow do
          Result := Namespace + '.' + Name
      else if ParentClass is TJclClrTableModuleRefRow then
        with ParentClass as TJclClrTableModuleRefRow do
          Result := Name
      else if ParentClass is TJclClrTableMethodDefRow then
        with ParentClass as TJclClrTableMethodDefRow do
          Result := Name
      else if ParentClass is TJclClrTableTypeSpecRow then
        Result := ''
      else if ParentClass is TJclClrTableTypeDefRow then
        with ParentClass as TJclClrTableTypeDefRow do
          Result := Namespace + '.' + Name
      else
        Result := 'Unknown Class - ' + ParentClass.ClassName;
    end;
  var
    AMethod: TJclClrTableRow;
  begin
    AMethod := Attr.Method;
    if AMethod is TJclClrTableMethodDefRow then
      with AMethod as TJclClrTableMethodDefRow do
        Result := ParentToken.Namespace + '.' + ParentToken.Name + ' :: ' + Name
    else if AMethod is TJclClrTableMemberRefRow then
      with AMethod as TJclClrTableMemberRefRow do
        Result := GetParentClassName(ParentClass) + '::' + Name
    else
      Result := 'Unknown method type - ' + IntToHex(Attr.ParentIdx, 8);

    Result := Result + ' <' + Copy(AMethod.ClassName, Length('TJclClrTable')+1,
        Length(AMethod.ClassName)-Length('TJclClrTable')-Length('Row')) +
        '> [' + IntToHex(Attr.TypeIdx, 8) + ']';
  end;
var
  I: Integer;
begin
  for I:=0 to ATable.RowCount-1 do
  begin
    Dump('Parent: ' + GetParent(ATable[I]));
    Dump('Method: ' + GetMethod(ATable[I]));
    Dump('Value: ', ATable[I].Value);
    Dump(lkWide);
  end;
end;

procedure TfrmTable.DumpTable(const ATable: TJclClrTableParamDef);
var
  I: Integer;
  Attr: string;
begin
  for I:=0 to ATable.RowCount-1 do
  begin
    Attr := '';
    if pkIn in ATable.Rows[I].Flags then
      Attr := Attr + 'In ';
    if pkOut in ATable.Rows[I].Flags then
      Attr := Attr + 'Out ';
    if pkOptional in ATable.Rows[I].Flags then
      Attr := Attr + 'Opt ';
    if pkHasDefault in ATable.Rows[I].Flags then
      Attr := Attr + 'Default ';
    if pkHasFieldMarshal in ATable.Rows[I].Flags then
      Attr := Attr + 'Marshal ';

    Dump('%s %s', [ATable.Rows[I].Name, Attr]);
  end;
end;

procedure TfrmTable.DumpTable(const ATable: TJclClrTablePropertyDef); 
var
  I: Integer;
begin
  for I:=0 to ATable.RowCount-1 do
    Dump('%s', [ATable.Rows[I].Name])
end;

procedure TfrmTable.DumpTable(const ATable: TJclClrTableManifestResource); 
var
  I: Integer;
begin
  for I:=0 to ATable.RowCount-1 do
    Dump('%s', [ATable.Rows[I].Name])
end;

procedure TfrmTable.DumpTable(const ATable: TJclClrTableFile);
var
  I: Integer;
begin
  for I:=0 to ATable.RowCount-1 do
  begin
    Dump('File Name: ' + ATable[I].Name);
    Dump('Contains Metadata: ' + BooleanToStr(ATable[I].ContainsMetadata));
    Dump('Hash Value: ', ATable[I].HashValue);
  end;
end;

procedure TfrmTable.DumpTable(const ATable: TJclClrTableExportedType);
var
  I: Integer;
begin
  for I:=0 to ATable.RowCount-1 do
    Dump(ATable[I].TypeNamespace + '.' + ATable[I].TypeName);
end;

procedure TfrmTable.btnDumpILClick(Sender: TObject);
begin
  frmMain.actFileDump.Execute;
end;

end.
