{*******************************************************************************
Functions to interoperate with OpenOffice (basically the
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

Warning:
This unit is non-thread-safe at all. It uses global instances of
Open Office manager in the scope of unit so it is unpredictable how it will
work in multi-thread environment. We recommend you to accumulate all the job
with this unit inside of one concrete thread.
*******************************************************************************}

unit ALOpenOffice;

interface

uses
  System.SysUtils,
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
function OpenCalcDocument(const aFileName: AnsiString): Variant;
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

uses
  System.Classes,
  System.StrUtils,
  System.Win.ComObj,
  ALStringList;

const
  cALOpenOfficeUrlProtocols: array[1..7] of AnsiString = ('file:///',
                                                          'ftp://',
                                                          'news:',
                                                          'http://',
                                                          'mailto:',
                                                          'macro:',
                                                          'private:');
  aALOpenOfficeCharBold: Single = 150.0;

var
  vALOpenOfficeService: Variant;
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
var LDesktop: Variant;
begin
  result := false;
  if IsVariantNullOrEmpty(vALOpenOfficeService) then Exit;
  try
    LDesktop := vALOpenOfficeService.createInstance('com.sun.star.frame.Desktop');
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
var I: Integer;
begin
  try
    if aMaxIndex < 0 then begin
      result := vALOpenOfficeService.Bridge_GetStruct(String(aStructureName));
    end
    else begin
      result := VarArrayCreate([0, aMaxIndex], varVariant);
      for I := 0 to aMaxIndex do begin
        result[I] := vALOpenOfficeService.Bridge_GetStruct(String(aStructureName));
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
var LObjectInterfacesList: TALStringList;
    LInterfaceName: AnsiString;
    LInspection: Variant;
    LInspectionMethods: Variant;
    LInspectionMethod: Variant;
    LInspectionDeclaringClass: Variant;
    i, j: integer;
begin
  result := false;
  LObjectInterfacesList := TALStringlist.Create;
  try
    try
      LInspection := vALOpenOfficeIntrospection.inspect(aObject);
      LInspectionMethods := LInspection.getMethods(-1);
      for i := 0 to VarArrayHighBound(LInspectionMethods, 1) do begin
        LInterfaceName := '';
        LInspectionMethod := LInspectionMethods[i];
        LInspectionDeclaringClass := LInspectionMethod.DeclaringClass;
        LInterfaceName := AnsiString(LInspectionDeclaringClass.Name);
        if (LInterfaceName <> '')  and
           (LObjectInterfacesList.IndexOf(LInterfaceName) < 0) then LObjectInterfacesList.Add(LInterfaceName);
      end;

      for i := 0 to High(aInterfaceList) do begin
        j := LObjectInterfacesList.IndexOf(AnsiString(aInterfaceList[i]));
        if j < 0 then Exit;
        if LObjectInterfacesList.Strings[j] <> aInterfaceList[i] then Exit;
      end;
      result := true;

    except
      on E: Exception do begin
        raise EALOpenOfficeException.Create('Open Office Inspection error');
      end;
    end;
  finally
    LObjectInterfacesList.Free;
  end;
end;

{******************************************************************}
function CreateProperties(aPropertyList: array of Variant): Variant;
var LNumberOfProperties: integer;
    i, j: integer;
begin
  LNumberOfProperties := High(aPropertyList);
  if (not Odd(LNumberOfProperties)) or (LNumberOfProperties < 1) then
    raise EALOpenOfficeException.CreateFmt('Wrong number of properties: %s', [ALIntToStr(LNumberOfProperties)]);

  result := VarArrayCreate([0, LNumberOfProperties shr 1], varVariant);
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
  until i > LNumberOfProperties;
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
var LArgs: Variant;
begin
  LArgs := VarArrayCreate([0, 0], varVariant);
  result := vALOpenOfficeStarDesktop.LoadComponentFromURL('private:factory/scalc',
                                                          '_blank',
                                                          0,
                                                          LArgs);
end;

{**************************************************************}
function OpenCalcDocument(const aFileName: AnsiString): Variant;
var LArgs: Variant;
begin
  LArgs := VarArrayCreate([0, 0], varVariant);
  result := vALOpenOfficeStarDesktop.LoadComponentFromURL(String('file:///' + ALStringReplace(aFileName, '\', '/', [rfReplaceAll])),
                                                          '_blank',
                                                          0,
                                                          LArgs);
end;

{****************************************}
procedure SaveDocument(aDocument: Variant;
                       aFileName: AnsiString;
                       aFileType: AnsiString = '');
var LFileTypeParams: Variant;
    LUrl: AnsiString;
    LFileTypeParam: Variant;
begin

  // This sleep helps to ensure that OpenOffice correctly unassignes now the
  // possibly used resources. OpenOffice is very slow about that.
  Sleep(1500);

  LUrl := ConvertToUrl(aFileName);
  if aFileType = '' then begin
    LFileTypeParams := VarArrayCreate([0, 0], varVariant);
    aDocument.storeAsURL(String(LUrl), LFileTypeParams);
  end
  else begin
    LFileTypeParams := VarArrayCreate([0, 1], varVariant);
    LFileTypeParams[0] := CreateUnoStruct('com.sun.star.beans.PropertyValue');
    LFileTypeParam := LFileTypeParams[0];
    LFileTypeParam.Name := String('FilterName');
    LFileTypeParam.Value := String(aFileType);
    aDocument.storeAsURL(String(LUrl), LFileTypeParams);
  end;
end;

{***************************************}
procedure CreateSheet(aDocument: Variant;
                      const aSheetName: AnsiString);
var LSheet: OLEVariant;
begin
  if not aDocument.Sheets.hasByName(String(aSheetName)) then begin
    LSheet := aDocument.createInstance('com.sun.star.sheet.Spreadsheet');
    aDocument.Sheets.insertByName(aSheetName, LSheet);
  end;
end;

{***************************************}
procedure SetColumnWidth(aSheet: Variant;
                         const aColumnIndex: integer;
                         const aWidthInCentimetres: integer);
var LColumn: OLEVariant;
begin
  LColumn := aSheet.Columns.getByIndex(aColumnIndex);
  SetColumnWidth(aSheet, LColumn, aWidthInCentimetres);
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
var LNewBorder: Variant;
begin
  LNewBorder := CreateUnoStruct('com.sun.star.table.BorderLine');
  try
    LNewBorder.Color := aBorderColor;
    LNewBorder.OuterLineWidth := 0;
    LNewBorder.InnerLineWidth:= 2;
    LNewBorder.LineDistance := 0;

    aCellRange.TopBorder := LNewBorder;
    aCellRange.BottomBorder := LNewBorder;
    aCellRange.LeftBorder := LNewBorder;
    aCellRange.RightBorder := LNewBorder;
  finally
    LNewBorder := Unassigned;
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
var LFileContentService: Variant;
    LLowWinAddress: AnsiString;
    LUTF8Address: AnsiString;
    LPrefix: AnsiString;
    I: integer;
begin
  LLowWinAddress := ALLowerCase(aWinAddress);
  LPrefix := '';
  for I := 1 to High(cALOpenOfficeUrlProtocols) do begin
    if ALPos(cALOpenOfficeUrlProtocols[I], LLowWinAddress) = 1 then begin
      aWinAddress := ALCopyStr(aWinAddress, Length(cALOpenOfficeUrlProtocols[I]) + 1, maxint);
      if I > 1 then LPrefix := cALOpenOfficeUrlProtocols[I];  // because prefix file:/// is possible to drop
      break;
    end;
  end;

  if (Length(LPrefix) = 0) and
     (ALPos('@', LLowWinAddress) > 0) then begin
    result := 'mailto:' + aWinAddress;
  end
  else begin
    LFileContentService := CreateUnoService('com.sun.star.ucb.FileContentProvider');
    LUTF8Address := AnsiString(LFileContentService.getFileURLFromSystemPath('', String(aWinAddress)));
    if Length(LUTF8Address) = 0 then
      raise EALOpenOfficeException.CreateFmt('Cannot get URL from address %s', [aWinAddress]);
    result := LPrefix + LUTF8Address;
  end;
end;

{***********************************************************}
function ConvertFromURL(aUrlAddress: AnsiString): AnsiString;
var LFileContentService: Variant;
    LLowUrlAddress: AnsiString;
    LWinAddress: AnsiString;
    LPrefix: AnsiString;
    I: integer;
begin
  LLowUrlAddress := ALLowerCase(aUrlAddress);
  LPrefix := '';
  for I := 1 to High(cALOpenOfficeUrlProtocols) do begin
    if ALPos(cALOpenOfficeUrlProtocols[I], LLowUrlAddress) = 1 then begin
      if I > 1 then begin
        aUrlAddress := ALCopyStr(aUrlAddress, Length(cALOpenOfficeUrlProtocols[I]) + 1, maxint);
        LPrefix := cALOpenOfficeUrlProtocols[I];
      end;
      Break;
    end;
  end;

  LFileContentService := CreateUnoService('com.sun.star.ucb.FileContentProvider');
  LWinAddress := AnsiString(LFileContentService.getSystemPathFromFileURL(String(aUrlAddress)));
  if Length(LPrefix) <> 0 then // backslash only with file:///
    LWinAddress := ALStringReplace(LWinAddress, '\', '/', [rfReplaceAll]);
  if Length(LWinAddress) = 0 then
    raise EALOpenOfficeException.CreateFmt('Cannot get address from URL %s', [aUrlAddress]);
  result := LPrefix + LWinAddress;
end;

{************************************************************}
function RGB(aRedByte, aGreenByte, aBlueByte: byte): Longword;
begin
  result := (aRedByte shl 16) +
            (aGreenByte shl 8) +
            (aBlueByte);
end;

end.
