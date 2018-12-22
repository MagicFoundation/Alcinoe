{*******************************************************************************
Description:  Functions to interoperate with OpenOffice (basically the
              same way as official documentation describes on Visual Basic).

Explanation:

The official spec of OpenOffice (and LibreOffice) gives many examples how
to automatize the work with OpenOffice using Visual Basic. They are based
on their own runtime library presented by set of functions.

So this unit focuses to provide the maximum of compatibility to these
examples making process of translation from Visual Basic to Delphi to be
more easy and fast. This way it has the functional interface only.

This unit is inspired by OOOTools Delphi library and espesially on this
implementation:
https://github.com/PlanSwift/sdk-examples-2008/blob/master/Delphi%202007/Open%20Office/OOoTools.pas

The current unit was based on the code above but the code was changed to
better follow the standards of Alcinoe's coding and also many other functions
were added.

Warning: This unit is non-thread-safe at all. It uses global instances of
         Open Office manager in the scope of unit so it is unpredictable how it will
         work in multi-thread environment. We recommend you to accumulate all the job
         with this unit inside of one concrete thread.
*******************************************************************************}

unit ALOpenOffice;

interface

uses System.SysUtils,
     System.Variants,
     ALString;

type

  {------------------------------------------}
  EALOpenOfficeException = class(EALException);

//----- Service initialization and interconnection functions
procedure ConnectOpenOffice;
procedure DisconnectOpenOffice(aTerminateOpenOffice: boolean = true);
function IsOpenOfficeConnected: boolean;
function CreateUnoService(const aServiceName: AnsiString): Variant;
function CreateUnoStruct(const aStructureName: AnsiString;                                    // UNO it is an abbreviation, it means Universal Network Objects
                         const aMaxIndex: integer = -1): Variant;                             // and represents the interfaces that are possible to use to create
function HasUnoInterfaces(aObject: Variant; aInterfaceList: array of AnsiString): boolean;    // an object or property of defined structure and behavior
function CreateProperties(aPropertyList: array of Variant): Variant;
function MakePropertyValue(aPropertyName: AnsiString;
                           aPropertyValue: Variant): Variant;

//----- Document functions
function CreateCalcDocument: Variant;
procedure SaveDocument(aDocument: Variant;
                       aFileName: AnsiString;
                       aFileType: AnsiString = '');
procedure CreateSheet(aDocument: Variant;
                      const aSheetName: AnsiString);
procedure SetColumnWidth(aSheet: Variant;
                         const aColumnIndex: integer;
                         const aWidthInCentimetres: integer); overload;
procedure SetColumnWidth(aSheet: Variant;
                         aColumn: Variant;
                         const aWidthInCentimetres: integer); overload;
procedure SetCellBold(aCell: Variant);
procedure SetCellBorder(aCellRange: Variant; const aBorderColor: Longword); // At the moment only border color is supporting to setup border settings

//----- Helper functions and utils
function IsVariantNullOrEmpty(aVariant: Variant): boolean;
function DummyArray: Variant;
function ConvertToURL(aWinAddress: AnsiString): AnsiString;
function ConvertFromURL(aUrlAddress: AnsiString): AnsiString;
function RGB(aRedByte, aGreenByte, aBlueByte: byte): Longword;


implementation

uses System.Classes,
     System.StrUtils,
     System.Win.ComObj,
     ALStringList;

const cALOpenOfficeUrlProtocols: array[1..7] of AnsiString = ('file:///',
                                                              'ftp://',
                                                              'news:',
                                                              'http://',
                                                              'mailto:',
                                                              'macro:',
                                                              'private:');
      aALOpenOfficeCharBold: Single = 150.0;

var vALOpenOfficeService: Variant;
    vALOpenOfficeStarDesktop: Variant;
    vALOpenOfficeIntrospection: Variant;
    vALOpenOfficeReflection: Variant;
    vALOpenOfficeDispatchHelper: Variant;

{**************************}
procedure ConnectOpenOffice;
begin
  if IsOpenOfficeConnected then Exit;
  vALOpenOfficeService := CreateOleObject('com.sun.star.ServiceManager');
  if IsVariantNullOrEmpty(vALOpenOfficeService) then
    raise EALOpenOfficeException.Create('Cannot instantiate Open Office ServiceManager');
  vALOpenOfficeStarDesktop    := CreateUnoService('com.sun.star.frame.Desktop');
  vALOpenOfficeDispatchHelper := CreateUnoService('com.sun.star.frame.DispatchHelper');
  vALOpenOfficeIntrospection  := CreateUnoService('com.sun.star.beans.Introspection');
  vALOpenOfficeReflection     := CreateUnoService('com.sun.star.reflection.CoreReflection');
end;

{*******************************************************************}
procedure DisconnectOpenOffice(aTerminateOpenOffice: boolean = true);
begin
  if aTerminateOpenOffice then vALOpenOfficeStarDesktop.terminate;
  vALOpenOfficeService        := Unassigned;
  vALOpenOfficeStarDesktop    := Unassigned;
  vALOpenOfficeDispatchHelper := Unassigned;
  vALOpenOfficeIntrospection  := Unassigned;
  vALOpenOfficeReflection     := Unassigned;
end;

{**************************************}
function IsOpenOfficeConnected: boolean;
var aDesktop: Variant;
begin
  result := false;
  if IsVariantNullOrEmpty(vALOpenOfficeService) then Exit;
  try
    aDesktop := vALOpenOfficeService.createInstance('com.sun.star.frame.Desktop');
    result := true;
  except
    on E: Exception do begin
      vALOpenOfficeService := Null;
    end;
  end;
end;

{*****************************************************************}
function CreateUnoService(const aServiceName: AnsiString): Variant;
begin
  result := vALOpenOfficeService.CreateInstance(String(aServiceName));
  if IsVariantNullOrEmpty(Result) then
    raise EALOpenOfficeException.CreateFmt('Cannot create the service with name %s', [aServiceName]);
end;

{********************************************************}
function CreateUnoStruct(const aStructureName: AnsiString;
                         const aMaxIndex: integer = -1): Variant;
var i: Integer;
begin
  try
    if aMaxIndex < 0 then begin
      result := vALOpenOfficeService.Bridge_GetStruct(String(aStructureName));
    end
    else begin
      result := VarArrayCreate([0, aMaxIndex], varVariant);
      for i := 0 to aMaxIndex do begin
        result[i] := vALOpenOfficeService.Bridge_GetStruct(String(aStructureName));
      end;
    end;
  except
    on E: Exception do begin
      result := Null;
    end;
  end;

  if IsVariantNullOrEmpty(Result) then
    raise EALOpenOfficeException.CreateFmt('Cannot create a structure %s', [aStructureName]);
end;

{****************************************************************************************}
function HasUnoInterfaces(aObject: Variant; aInterfaceList: array of AnsiString): boolean;
var aObjectInterfacesList: TALStringList;
    aInterfaceName: AnsiString;
    aInspection: Variant;
    aInspectionMethods: Variant;
    aInspectionMethod: Variant;
    aInspectionDeclaringClass: Variant;
    i, j: integer;
begin
  result := false;
  aObjectInterfacesList := TALStringlist.Create;
  try
    try
      aInspection := vALOpenOfficeIntrospection.inspect(aObject);
      aInspectionMethods := aInspection.getMethods(-1);
      for i := 0 to VarArrayHighBound(aInspectionMethods, 1) do begin
        aInterfaceName := '';
        aInspectionMethod := aInspectionMethods[i];
        aInspectionDeclaringClass := aInspectionMethod.DeclaringClass;
        aInterfaceName := AnsiString(aInspectionDeclaringClass.Name);
        if (aInterfaceName <> '')  and
           (aObjectInterfacesList.IndexOf(aInterfaceName) < 0) then aObjectInterfacesList.Add(aInterfaceName);
      end;

      for i := 0 to High(aInterfaceList) do begin
        j := aObjectInterfacesList.IndexOf(AnsiString(aInterfaceList[i]));
        if j < 0 then Exit;
        if aObjectInterfacesList.Strings[j] <> aInterfaceList[i] then Exit;
      end;
      result := true;

    except
      on E: Exception do begin
        raise EALOpenOfficeException.Create('Open Office Inspection error');
      end;
    end;
  finally
    aObjectInterfacesList.Free;
  end;
end;

{******************************************************************}
function CreateProperties(aPropertyList: array of Variant): Variant;
var aNumberOfProperties: integer;
    i, j: integer;
begin
  aNumberOfProperties := High(aPropertyList);
  if (not Odd(aNumberOfProperties)) or (aNumberOfProperties < 1) then
    raise EALOpenOfficeException.CreateFmt('Wrong number of properties: %s', [ALIntToStr(aNumberOfProperties)]);

  result := VarArrayCreate([0, aNumberOfProperties shr 1], varVariant);
  i := 0;
  j := 0;
  repeat
    result[j] := vALOpenOfficeService.Bridge_GetStruct('com.sun.star.beans.PropertyValue');
    case VarType(aPropertyList[i]) of
      varOleStr,
      varStrArg,
      varString: Variant(result[j]).Name := aPropertyList[i];
      else raise EALOpenOfficeException.Create('Non-string property found');
    end;
    Variant(Result[j]).Value:= aPropertyList[i + 1];
    Inc(j);
    Inc(i, 2);
  until i > aNumberOfProperties;
end;

{***************************************************}
function MakePropertyValue(aPropertyName: AnsiString;
                           aPropertyValue: Variant): Variant;
begin
  result       := vALOpenOfficeService.Bridge_GetStruct('com.sun.star.beans.PropertyValue');
  result.Name  := String(aPropertyName);
  result.Value := aPropertyValue;
end;

{***********************************}
function CreateCalcDocument: Variant;
var aArgs: Variant;
begin
  aArgs := VarArrayCreate([0, 0], varVariant);
  result := vALOpenOfficeStarDesktop.LoadComponentFromURL('private:factory/scalc',
                                                          '_blank',
                                                          0,
                                                          aArgs);
end;

{****************************************}
procedure SaveDocument(aDocument: Variant;
                       aFileName: AnsiString;
                       aFileType: AnsiString = '');
var aFileTypeParams: Variant;
    aUrl: AnsiString;
    aFileTypeParam: Variant;
begin

  // This sleep helps to ensure that OpenOffice correctly unassignes now the
  // possibly used resources. OpenOffice is very slow about that.
  Sleep(1500);

  aUrl := ConvertToUrl(aFileName);
  if aFileType = '' then begin
    aFileTypeParams := VarArrayCreate([0, 0], varVariant);
    aDocument.storeAsURL(String(aUrl), aFileTypeParams);
  end
  else begin
    aFileTypeParams := VarArrayCreate([0, 1], varVariant);
    aFileTypeParams[0] := CreateUnoStruct('com.sun.star.beans.PropertyValue');
    aFileTypeParam := aFileTypeParams[0];
    aFileTypeParam.Name := String('FilterName');
    aFileTypeParam.Value := String(aFileType);
    aDocument.storeAsURL(String(aUrl), aFileTypeParams);
  end;
end;

{***************************************}
procedure CreateSheet(aDocument: Variant;
                      const aSheetName: AnsiString);
var aSheet: OLEVariant;
begin
  if not aDocument.Sheets.hasByName(String(aSheetName)) then begin
    aSheet := aDocument.createInstance('com.sun.star.sheet.Spreadsheet');
    aDocument.Sheets.insertByName(aSheetName, aSheet);
  end;
end;

{***************************************}
procedure SetColumnWidth(aSheet: Variant;
                         const aColumnIndex: integer;
                         const aWidthInCentimetres: integer);
var aColumn: OLEVariant;
begin
  aColumn := aSheet.Columns.getByIndex(aColumnIndex);
  SetColumnWidth(aSheet, aColumn, aWidthInCentimetres);
end;

{***************************************}
procedure SetColumnWidth(aSheet: Variant;
                         aColumn: Variant;
                         const aWidthInCentimetres: integer);
begin
  aColumn.Width := aWidthInCentimetres * 100;
end;

{************************************}
procedure SetCellBold(aCell: Variant);
begin
  aCell.CharWeight := aALOpenOfficeCharBold;
end;

{*************************************************************************}
procedure SetCellBorder(aCellRange: Variant; const aBorderColor: Longword);
var aNewBorder: Variant;
begin
  aNewBorder := CreateUnoStruct('com.sun.star.table.BorderLine');
  try
    aNewBorder.Color := aBorderColor;
    aNewBorder.OuterLineWidth := 0;
    aNewBorder.InnerLineWidth:= 2;
    aNewBorder.LineDistance := 0;

    aCellRange.TopBorder := aNewBorder;
    aCellRange.BottomBorder := aNewBorder;
    aCellRange.LeftBorder := aNewBorder;
    aCellRange.RightBorder := aNewBorder;
  finally
    aNewBorder := Unassigned;
  end;
end;

{********************************************************}
function IsVariantNullOrEmpty(aVariant: Variant): boolean;
begin
  result := (VarIsEmpty(aVariant)) or
            (VarIsNull(aVariant)) or
            (VarIsClear(aVariant));
end;

{***************************}
function DummyArray: Variant;
begin
  result := VarArrayCreate([0, -1], varVariant);
end;

{*********************************************************}
function ConvertToURL(aWinAddress: AnsiString): AnsiString;
var aFileContentService: Variant;
    aLowWinAddress: AnsiString;
    aUTF8Address: AnsiString;
    aPrefix: AnsiString;
    i: integer;
begin
  aLowWinAddress := ALLowerCase(aWinAddress);
  aPrefix := '';
  for i := 1 to High(cALOpenOfficeUrlProtocols) do begin
    if ALPos(cALOpenOfficeUrlProtocols[i], aLowWinAddress) = 1 then begin
      aWinAddress := ALCopyStr(aWinAddress, Length(cALOpenOfficeUrlProtocols[i]) + 1, maxint);
      if i > 1 then aPrefix := cALOpenOfficeUrlProtocols[i];  // because prefix file:/// is possible to drop
      break;
    end;
  end;

  if (Length(aPrefix) = 0) and
     (ALPos('@', aLowWinAddress) > 0) then begin
    result := 'mailto:' + aWinAddress;
  end
  else begin
    aFileContentService := CreateUnoService('com.sun.star.ucb.FileContentProvider');
    aUTF8Address := AnsiString(aFileContentService.getFileURLFromSystemPath('', String(aWinAddress)));
    if Length(aUTF8Address) = 0 then
      raise EALOpenOfficeException.CreateFmt('Cannot get URL from address %s', [aWinAddress]);
    result := aPrefix + aUTF8Address;
  end;
end;

{***********************************************************}
function ConvertFromURL(aUrlAddress: AnsiString): AnsiString;
var aFileContentService: Variant;
    aLowUrlAddress: AnsiString;
    aWinAddress: AnsiString;
    aPrefix: AnsiString;
    i: integer;
begin
  aLowUrlAddress := ALLowerCase(aUrlAddress);
  aPrefix := '';
  for i := 1 to High(cALOpenOfficeUrlProtocols) do begin
    if ALPos(cALOpenOfficeUrlProtocols[i], aLowUrlAddress) = 1 then begin
      if i > 1 then begin
        aUrlAddress := ALCopyStr(aUrlAddress, Length(cALOpenOfficeUrlProtocols[i]) + 1, maxint);
        aPrefix := cALOpenOfficeUrlProtocols[i];
      end;
      Break;
    end;
  end;

  aFileContentService := CreateUnoService('com.sun.star.ucb.FileContentProvider');
  aWinAddress := AnsiString(aFileContentService.getSystemPathFromFileURL(String(aUrlAddress)));
  if Length(aPrefix) <> 0 then // backslash only with file:///
    aWinAddress := ALStringReplace(aWinAddress, '\', '/', [rfReplaceAll]);
  if Length(aWinAddress) = 0 then
    raise EALOpenOfficeException.CreateFmt('Cannot get address from URL %s', [aUrlAddress]);
  result := aPrefix + aWinAddress;
end;

{************************************************************}
function RGB(aRedByte, aGreenByte, aBlueByte: byte): Longword;
begin
  result := (aRedByte shl 16) +
            (aGreenByte shl 8) +
            (aBlueByte);
end;

end.
