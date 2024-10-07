unit Alcinoe.Macapi.CoreText;

interface

{$I Alcinoe.inc}

uses
  Macapi.CoreText,
  Macapi.CocoaTypes,
  Macapi.CoreFoundation;

{$M+}

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1929 has been resolved. If resolved, remove the functions below.'}
{$ENDIF}
function CTFontManagerRegisterGraphicsFont(font: CGFontRef; error: PCFErrorRef): Integer; cdecl; external libCoreText name _PU + 'CTFontManagerRegisterGraphicsFont'; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1929

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1934 has been resolved. If resolved, remove the functions below.'}
{$ENDIF}
function kCTFontSymbolicTrait: CFStringRef; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1934
function kCTFontTraitsAttribute: CFStringRef; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1934
function kCTFontNameAttribute: CFStringRef; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1934
function kCTFontFamilyNameAttribute: CFStringRef; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1934
function kCTFontWeightTrait: CFStringRef; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1934
function kCTFontSlantTrait: CFStringRef; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1934
function kCTFontURLAttribute: CFStringRef; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1934
function kCTFontCascadeListAttribute: CFStringRef; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1934
function kCTFontSizeAttribute: CFStringRef; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1934

implementation

uses
  Macapi.Foundation;

// https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1934
function kCTFontSymbolicTrait: CFStringRef;
begin
  Result := CocoaObjectIdConst(libCoreText, 'kCTFontSymbolicTrait');
end;

// https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1934
function kCTFontTraitsAttribute: CFStringRef;
begin
  Result := CocoaObjectIdConst(libCoreText, 'kCTFontTraitsAttribute');
end;

// https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1934
function kCTFontNameAttribute: CFStringRef;
begin
  Result := CocoaObjectIdConst(libCoreText, 'kCTFontNameAttribute');
end;

// https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1934
function kCTFontFamilyNameAttribute: CFStringRef;
begin
  Result := CocoaObjectIdConst(libCoreText, 'kCTFontFamilyNameAttribute');
end;

// https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1934
function kCTFontWeightTrait: CFStringRef;
begin
  Result := CocoaObjectIdConst(libCoreText, 'kCTFontWeightTrait');
end;

// https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1934
function kCTFontSlantTrait: CFStringRef;
begin
  Result := CocoaObjectIdConst(libCoreText, 'kCTFontSlantTrait');
end;

// https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1934
function kCTFontURLAttribute: CFStringRef;
begin
  Result := CocoaObjectIdConst(libCoreText, 'kCTFontURLAttribute');
end;

// https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1934
function kCTFontCascadeListAttribute: CFStringRef;
begin
  Result := CocoaObjectIdConst(libCoreText, 'kCTFontCascadeListAttribute');
end;

// https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1934
function kCTFontSizeAttribute: CFStringRef;
begin
  Result := CocoaObjectIdConst(libCoreText, 'kCTFontSizeAttribute');
end;

end.
