unit StackTrackDLLsComUnit;

interface

uses
  Windows, ActiveX, Classes, ComObj, StackTrackDLLsComLibrary_TLB, StdVcl;

type
  TStackTrackDllsTest = class(TTypedComObject, IStackTrackDllsTest)
  protected
    function Error1: HResult; stdcall;
    function Error2: HResult; stdcall;
  end;

implementation

uses ComServ, SysUtils;

procedure Error1_1;
begin
  StrToInt('x');
end;

function TStackTrackDllsTest.Error1: HResult;
begin
  Error1_1;
  Result := S_FALSE;
end;

function TStackTrackDllsTest.Error2: HResult;
begin
  raise Exception.Create('Exception from IDllExceptTestObject.Error2');
end;

initialization
  TTypedComObjectFactory.Create(ComServer, TStackTrackDllsTest, Class_StackTrackDllsTest,
    ciMultiInstance, tmApartment);
end.
