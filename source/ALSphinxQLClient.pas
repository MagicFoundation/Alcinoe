{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
svn:          svn checkout svn://svn.code.sf.net/p/alcinoe/code/ alcinoe-code
Author(s):    Stéphane Vander Clock (skype/email: svanderclock@yahoo.fr)

product:      ALSphinxQLClient
Version:      4.00

Description:  An Object to query Sphinx full text search engine using
              SphinxQL protocol (MySql Clone Protocol)

Know bug :

History :     26/06/2012: Add xe2 support

Link :        

**************************************************************}
unit ALSphinxQLClient;

interface

{$IF CompilerVersion >= 25} {Delphi XE4}
  {$LEGACYIFEND ON} // http://docwiki.embarcadero.com/RADStudio/XE4/en/Legacy_IFEND_(Delphi)
{$IFEND}

uses AlMySqlClient,
     ALMySqlWrapper;

Type

  {---------------------------------------}
  TalSphinxQlClient = Class(TalMySqlClient)
  Public
    Procedure Connect(const Host: AnsiString;
                      Port: integer;
                      Const Options: TALMySQLOptions = nil); reintroduce;
  end;

  {-------------------------------------------------------------------}
  TalSphinxQlConnectionPoolClient = Class(TalMySqlConnectionPoolClient)
  Private
  Public
    Constructor Create(const aHost: AnsiString;
                       aPort: integer;
                       aApiVer: TALMySqlVersion_API;
                       const alib: AnsiString = 'libmysql.dll';
                       const aOpenConnectionOptions: TALMySQLOptions = nil); overload; virtual;
    Constructor Create(const aHost: AnsiString;
                       aPort: integer;
                       alib: TALMySqlLibrary;
                       const aOpenConnectionOptions: TALMySQLOptions = nil); overload; virtual;
  end;

{---------------------------------------------------------------}
function AlSphinxEscapeString(const src: ansiString): ansiString;


implementation

uses alString;

{*********************************************************}
procedure TalSphinxQlClient.Connect(const Host: AnsiString;
                                    Port: integer;
                                    Const Options: TALMySQLOptions = nil);
begin

  inherited Connect(Host,
                    Port,
                    '',       // DataBaseName
                    '',       // Login
                    '',       // Password
                    '',       // CharSet
                    0,        // ClientFlag
                    Options); // Options

end;

{*************************************************************************}
constructor TalSphinxQlConnectionPoolClient.Create(const aHost: AnsiString;
                                                   aPort: integer;
                                                   aApiVer: TALMySqlVersion_API;
                                                   const alib: AnsiString = 'libmysql.dll';
                                                   const aOpenConnectionOptions: TALMySQLOptions = nil);
begin

  inherited Create(aHost,
                   aPort,
                   '', // aDataBaseName,
                   '', // aLogin,
                   '', // aPassword,
                   '', // aCharSet: String;
                   aApiVer,
                   alib,
                   0,  // aOpenConnectionClientFlag: Cardinal = 0);
                   aOpenConnectionOptions); // aOpenConnectionOptions
  

end;

{*************************************************************************}
constructor TalSphinxQlConnectionPoolClient.Create(const aHost: AnsiString;
                                                   aPort: integer;
                                                   alib: TALMySqlLibrary;
                                                   const aOpenConnectionOptions: TALMySQLOptions = nil);
begin

  inherited Create(aHost,
                   aPort,
                   '', // aDataBaseName,
                   '', // aLogin,
                   '', // aPassword,
                   '', // aCharSet: String;
                   alib,
                   0,  // aOpenConnectionClientFlag: Cardinal = 0);
                   aOpenConnectionOptions); // aOpenConnectionOptions

end;

{***********************************************}
//http://sphinxsearch.com/forum/view.html?id=9558
function AlSphinxEscapeString(const src: ansiString): ansiString;
var i, l: integer;
    Buf, P: PAnsiChar;
    ch: ansiChar;
begin
  Result := '';
  L := Length(src);
  if L = 0 then exit;
  GetMem(Buf, L * 3); // to be on the *very* safe side
  try
    P := Buf;
    for i := low(Src) to High(Src) do begin
      ch := src[i];
      case ch of

        // EscapeString
        '(': begin
               ALStrMove('\\(', P, 3);
               Inc(P, 3);
             end;
        ')': begin
               ALStrMove('\\)', P, 3);
               Inc(P, 3);
             end;
        '|': begin
               ALStrMove('\\|', P, 3);
               Inc(P, 3);
             end;
        '-': begin
               ALStrMove('\\-', P, 3);
               Inc(P, 3);
             end;
        '!': begin
               ALStrMove('\\!', P, 3);
               Inc(P, 3);
             end;
        '@': begin
               ALStrMove('\\@', P, 3);
               Inc(P, 3);
             end;
        '~': begin
               ALStrMove('\\~', P, 3);
               Inc(P, 3);
             end;
        '"': begin
               ALStrMove('\\"', P, 3);
               Inc(P, 3);
             end;
        '&': begin
               ALStrMove('\\&', P, 3);
               Inc(P, 3);
             end;
        '/': begin
               ALStrMove('\\/', P, 3);
               Inc(P, 3);
             end;

        //mysql-real-escape-string
        //Strictly speaking, MySQL requires only that backslash and the quote character used to quote the string in the query be escaped
        '\': begin
               ALStrMove('\\', P, 2);
               Inc(P, 2);
             end;
        '''': begin
                ALStrMove('\''', P, 2);
                Inc(P, 2);
              end;

        else Begin
          P^:= ch;
          Inc(P);
        end;
      end;
    end;
    SetString(Result, Buf, P - Buf);
  finally
    FreeMem(Buf);
  end;
end;

end.
