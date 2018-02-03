{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{ DUnit Test                                                                                  }
{                                                                                                  }
{ Last Update: 14-10-2004
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{**************************************************************************************************}
unit TestJclDateTime;

interface
uses
  TestFramework,
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF MSWINDOWS}
  JclDateTime;

type
   TDateTransformationTest=class(TTestCase)
   published
      procedure _DateTimeToUnixTime;
      procedure _UnixTimeToDateTime;
   end;
implementation

{ TDateTransformationTest }

procedure TDateTransformationTest._DateTimeToUnixTime;
begin
   //TODO: DateTimeToUnixTime
   CheckEquals(DateTimeToUnixTime(EncodeDate(1970,1,1)),0,'DateTimeToUnixTime');
end;

procedure TDateTransformationTest._UnixTimeToDateTime;
begin
   //TODO: UnixTimeToDateTime
   CheckEquals(UnixTimeToDateTime(0),EncodeDate(1970,1,1),'UnixTimeToDateTime');
end;

initialization
   RegisterTest('JCLDateTime', TDateTransformationTest.Suite);

end.
 