unit Alcinoe.iOSapi.CoreText;

interface

{$I Alcinoe.inc}

uses
  Macapi.CoreFoundation;

{$M+}

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1938 has been resolved. If resolved, remove the functions below.'}
{$ENDIF}
function kCTFontSymbolicTrait: CFStringRef; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1938
function kCTFontTraitsAttribute: CFStringRef; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1938
function kCTFontNameAttribute: CFStringRef; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1938
function kCTFontFamilyNameAttribute: CFStringRef; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1938
function kCTFontWeightTrait: CFStringRef; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1938
function kCTFontSlantTrait: CFStringRef; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1938
function kCTFontURLAttribute: CFStringRef; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1938
function kCTFontCascadeListAttribute: CFStringRef; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1938
function kCTFontSizeAttribute: CFStringRef; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1938

implementation

uses
  iOSapi.CoreText,
  iOSapi.Foundation;

// https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1938
function kCTFontSymbolicTrait: CFStringRef;
begin
  Result := CocoaObjectIdConst(libCoreText, 'kCTFontSymbolicTrait');
end;

// https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1938
function kCTFontTraitsAttribute: CFStringRef;
begin
  Result := CocoaObjectIdConst(libCoreText, 'kCTFontTraitsAttribute');
end;

// https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1938
function kCTFontNameAttribute: CFStringRef;
begin
  Result := CocoaObjectIdConst(libCoreText, 'kCTFontNameAttribute');
end;

// https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1938
function kCTFontFamilyNameAttribute: CFStringRef;
begin
  Result := CocoaObjectIdConst(libCoreText, 'kCTFontFamilyNameAttribute');
end;

// https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1938
function kCTFontWeightTrait: CFStringRef;
begin
  Result := CocoaObjectIdConst(libCoreText, 'kCTFontWeightTrait');
end;

// https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1938
function kCTFontSlantTrait: CFStringRef;
begin
  Result := CocoaObjectIdConst(libCoreText, 'kCTFontSlantTrait');
end;

// https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1938
function kCTFontURLAttribute: CFStringRef;
begin
  Result := CocoaObjectIdConst(libCoreText, 'kCTFontURLAttribute');
end;

// https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1938
function kCTFontCascadeListAttribute: CFStringRef;
begin
  Result := CocoaObjectIdConst(libCoreText, 'kCTFontCascadeListAttribute');
end;

// https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1938
function kCTFontSizeAttribute: CFStringRef;
begin
  Result := CocoaObjectIdConst(libCoreText, 'kCTFontSizeAttribute');
end;

end.
