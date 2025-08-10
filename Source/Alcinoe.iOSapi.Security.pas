unit Alcinoe.iOSapi.Security;

interface

{$I Alcinoe.inc}

uses
  iOSapi.Foundation,
  IosApi.Security;

{*************************************}
{$IFNDEF ALCompilerVersionSupported123}
  {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-3922 has been resolved. If resolved, remove the definitions below.'}
{$ENDIF}
function kSecValueData: NSString;

implementation

{*******************************}
function kSecValueData: NSString;
begin
  result := CocoaNSStringConst(libSecurity, 'kSecValueData');
end;

end.
