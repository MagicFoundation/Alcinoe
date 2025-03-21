unit Alcinoe.iOSapi.CoreImage;

interface

{$I Alcinoe.inc}

uses
  iOSapi.Foundation,
  iOSapi.CoreImage;

{$M+}

{*************************************}
{$IFNDEF ALCompilerVersionSupported123}
  {$MESSAGE WARN 'Check if https://quality.embarcadero.com/browse/RSP-19878 has been resolved. If resolved, remove the functions below.'}
{$ENDIF}
function kCIInputImageKey: NSString; // https://quality.embarcadero.com/browse/RSP-19878
function kCIInputRadiusKey: NSString; // https://quality.embarcadero.com/browse/RSP-19878

{*************************************}
{$IFNDEF ALCompilerVersionSupported123}
  {$MESSAGE WARN 'Check if https://quality.embarcadero.com/browse/RSP-44130 has been resolved. If resolved, remove the functions below.'}
{$ENDIF}
function kCIFormatRGBA8: CIFormat; // https://quality.embarcadero.com/browse/RSP-44130
function kCIFormatBGRA8: CIFormat; // https://quality.embarcadero.com/browse/RSP-44130

implementation

// https://quality.embarcadero.com/browse/RSP-19878
function kCIInputImageKey: NSString;
begin
  result := CocoaNSStringConst(libCoreImage, 'kCIInputImageKey');
end;

// https://quality.embarcadero.com/browse/RSP-19878
function kCIInputRadiusKey: NSString;
begin
  result := CocoaNSStringConst(libCoreImage, 'kCIInputRadiusKey');
end;

// https://quality.embarcadero.com/browse/RSP-44130
function kCIFormatRGBA8: CIFormat;
begin
  Result := CocoaIntegerConst(libCoreImage, 'kCIFormatRGBA8');
end;

// https://quality.embarcadero.com/browse/RSP-44130
function kCIFormatBGRA8: CIFormat;
begin
  Result := CocoaIntegerConst(libCoreImage, 'kCIFormatBGRA8');
end;

end.
