/// some common definitions shared by both client and server side 
unit Project16Interface;

interface

uses
  SynCommons,
  mORMot;
  
type
  TRemoteSQLEngine = (rseOleDB, rseODBC, rseOracle, rseSQlite3, rseJet, rseMSSQL);
  
  IRemoteSQL = interface(IInvokable)
    ['{9A60C8ED-CEB2-4E09-87D4-4A16F496E5FE}']
    procedure Connect(aEngine: TRemoteSQLEngine; const aServerName, aDatabaseName,
      aUserID, aPassWord: RawUTF8);
    function GetTableNames: TRawUTF8DynArray;
    function Execute(const aSQL: RawUTF8; aExpectResults, aExpanded: Boolean): RawJSON;
  end;

const
  ROOT_NAME = 'root';
  PORT_NAME = '888';

implementation

end.
