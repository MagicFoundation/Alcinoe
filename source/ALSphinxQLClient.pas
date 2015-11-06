{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
svn:          svn checkout svn://svn.code.sf.net/p/alcinoe/code/ alcinoe-code
Author(s):    Stéphane Vander Clock (alcinoe@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)

product:      ALSphinxQLClient
Version:      4.00

Description:  An Object to query Sphinx full text search engine using
              SphinxQL protocol (MySql Clone Protocol)

Legal issues: Copyright (C) 1999-2013 by Arkadia Software Engineering

              This software is provided 'as-is', without any express
              or implied warranty.  In no event will the author be
              held liable for any  damages arising from the use of
              this software.

              Permission is granted to anyone to use this software
              for any purpose, including commercial applications,
              and to alter it and redistribute it freely, subject
              to the following restrictions:

              1. The origin of this software must not be
                 misrepresented, you must not claim that you wrote
                 the original software. If you use this software in
                 a product, an acknowledgment in the product
                 documentation would be appreciated but is not
                 required.

              2. Altered source versions must be plainly marked as
                 such, and must not be misrepresented as being the
                 original software.

              3. This notice may not be removed or altered from any
                 source distribution.

              4. You must register this software by sending a picture
                 postcard to the author. Use a nice stamp and mention
                 your name, street address, EMail address and any
                 comment you like to say.

Know bug :

History :     26/06/2012: Add xe2 support

Link :        

* Please send all your feedback to alcinoe@arkadia.com
* If you have downloaded this source from a website different from
  sourceforge.net, please get the last version on http://sourceforge.net/projects/alcinoe/
* Please, help us to keep the development of these components free by 
  promoting the sponsor on http://static.arkadia.com/html/alcinoe_like.html
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


implementation

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

end.
