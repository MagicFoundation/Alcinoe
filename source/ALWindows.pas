unit ALWindows;

interface

uses Windows,
     sysutils;

Var ALGetTickCount64: function: int64; stdcall;

const cALINVALID_SET_FILE_POINTER = DWORD(-1);

implementation

{******************************************}
function ALGetTickCount64XP: int64; stdcall;
begin
  Result := GetTickCount;
end;

{***************************}
procedure ALInitWindowsFunct;
var kernel32: HModule;
begin
  // Kernel32 is always loaded already, so use GetModuleHandle
  // instead of LoadLibrary
  kernel32 := GetModuleHandle('kernel32');
  if kernel32 = 0 then RaiseLastOSError;
  @ALGetTickCount64 := GetProcAddress(kernel32, 'GetTickCount64');
  if not Assigned(ALGetTickCount64) then ALGetTickCount64 := ALGetTickCount64XP;
end;

initialization
  ALInitWindowsFunct;

end.
