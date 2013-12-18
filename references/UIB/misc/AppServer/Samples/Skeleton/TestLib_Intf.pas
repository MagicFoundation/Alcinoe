{ This file is generated automaticaly, do not modify }
unit TestLib_Intf;
{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}
interface
uses Classes;

// Class identifiers
const
    CLSID_MyObject: TGUID = '{913672C2-80C9-4821-9E87-A09E123A3879}';
    CLSID_MyObject2: TGUID = '{2EB27E27-017B-4479-9FCE-609449187A59}';

// Objects interfaces
type
  IMyObject = interface
  ['{913672C2-80C9-4821-9E87-A09E123A3879}']
    procedure ExecuteScript(const script: string; out data: TMemoryStream); stdcall;
  end;

  IMyObject2 = interface
  ['{2EB27E27-017B-4479-9FCE-609449187A59}']
    function GetString: string; stdcall;
  end;

implementation

end.
