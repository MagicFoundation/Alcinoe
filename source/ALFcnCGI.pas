{*****************************************************************
www:          http://sourceforge.net/projects/alcinoe/
svn:          https://alcinoe.svn.sourceforge.net/svnroot/alcinoe
Author(s):    St�phane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)

product:      AlFcnCGI
Version:      4.00

Description:  Function to run CGI application like PHP-CGI.exe or
              Perl.exe

Legal issues: Copyright (C) 1999-2007 by Arkadia Software Engineering

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

* Please send all your feedback to svanderclock@arkadia.com
* If you have downloaded this source from a website different from 
  sourceforge.net, please get the last version on http://sourceforge.net/projects/alcinoe/
* Please, help us to keep the development of these components free by 
  promoting the sponsor on http://www.arkadia.com/html/alcinoe_like.html
**************************************************************}

unit ALFcnCGI;

interface

uses Windows,
     Classes,
     HttpApp,
     ALIsapiHTTP,
     AlHttpCommon,
     AlStringList;

Procedure AlCGIInitDefaultServerVariablesFromWebRequest(WebRequest: TwebRequest; ServerVariables: TALStrings); overload;
Procedure AlCGIInitDefaultServerVariablesFromWebRequest(WebRequest: TwebRequest;
                                                        ServerVariables: TALStrings;
                                                        ScriptName,
                                                        ScriptFileName: AnsiString;
                                                        Url: AnsiString); overload;
Procedure ALCGIInitDefaultServerVariables(ServerVariables: TALStrings); overload;
Procedure AlCGIInitDefaultServerVariables(ServerVariables: TALStrings;
                                          ScriptName,
                                          ScriptFileName: AnsiString;
                                          Url: AnsiString); overload;
Procedure AlCGIInitServerVariablesFromWebRequest(WebRequest: TwebRequest;
                                                 ServerVariables: TALStrings;
                                                 ScriptName,
                                                 ScriptFileName: AnsiString;
                                                 Url: AnsiString); overload;
Procedure AlCGIExec(InterpreterFilename: AnsiString;
                    ServerVariables: TALStrings;
                    RequestContentStream: Tstream;
                    ResponseContentStream: Tstream;
                    ResponseHeader: TALHTTPResponseHeader); overload;
Procedure AlCGIExec(ScriptName,
                    ScriptFileName,
                    Url,
                    X_REWRITE_URL,
                    InterpreterFilename: AnsiString;
                    WebRequest: TALIsapiRequest;
                    overloadedCookies: AnsiString;
                    overloadedQueryString: AnsiString;
                    overloadedReferer: AnsiString;
                    overloadedRequestContentStream: Tstream;
                    Var ResponseContentString: AnsiString;
                    ResponseHeader: TALHTTPResponseHeader); overload;
Procedure AlCGIExec(ScriptName,
                    ScriptFileName,
                    Url,
                    X_REWRITE_URL,
                    InterpreterFilename: AnsiString;
                    WebRequest: TALIsapiRequest;
                    Var ResponseContentString: AnsiString;
                    ResponseHeader: TALHTTPResponseHeader); overload;

implementation

uses sysutils,
     AlFcnExecute,
     AlFcnString;

{************************************************************************************************************}
Procedure AlCGIInitDefaultServerVariablesFromWebRequest(WebRequest: TwebRequest; ServerVariables: TALStrings);
Begin
  ServerVariables.Clear;
  {----------}
  ServerVariables.Add('HTTPS=off');                                                                     //HTTPS=off
  ServerVariables.Add('HTTP_UA_CPU='+           WebRequest.GetFieldByName('HTTP_UA_CPU'));              //HTTP_UA_CPU=x86
  ServerVariables.Add('HTTP_CONNECTION=Keep-Alive');                                                    //HTTP_CONNECTION=Keep-Alive | Whether to close the connection when done
  ServerVariables.Add('SERVER_PROTOCOL=HTTP/1.1');                                                      //SERVER_PROTOCOL=HTTP/1.1 | Server Protocol
  ServerVariables.Add('SERVER_SOFTWARE=Microsoft-IIS/7.0');                                             //SERVER_SOFTWARE=Microsoft-IIS/5.1 | Server Software
  ServerVariables.Add('SERVER_NAME='+           WebRequest.GetFieldByName('SERVER_NAME'));              //SERVER_NAME=www.mywebsite.com  | Server Host Name
  ServerVariables.Add('SERVER_PORT='+           WebRequest.GetFieldByName('SERVER_PORT'));              //SERVER_PORT=80 | Server Port Number
  ServerVariables.Add('REMOTE_HOST='+           WebRequest.GetFieldByName('REMOTE_HOST'));              //REMOTE_HOST=88.167.177.200 | Client Host Name
  ServerVariables.Add('REMOTE_ADDR='+           WebRequest.GetFieldByName('REMOTE_ADDR'));              //REMOTE_ADDR=88.167.177.200 | Client IP Number
  ServerVariables.Add('HTTP_HOST='+             WebRequest.GetFieldByName('HTTP_HOST'));                //HTTP_HOST=www.mywebsite.com | Requested Host
  {----------}
  ServerVariables.Add('HTTP_USER_AGENT='+       WebRequest.GetFieldByName('HTTP_USER_AGENT'));          //HTTP_USER_AGENT=Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; SV1) | Client Software you are using
  ServerVariables.Add('HTTP_ACCEPT=*/*');                                                               //HTTP_ACCEPT=*/* | List of acceptable content type
end;

{******************************************************************************}
Procedure AlCGIInitDefaultServerVariablesFromWebRequest(WebRequest: TwebRequest;
                                                        ServerVariables: TALStrings;
                                                        ScriptName,
                                                        ScriptFileName: AnsiString;
                                                        Url: AnsiString);
Begin
  AlCGIInitDefaultServerVariablesFromWebRequest(WebRequest, ServerVariables);
  {----------}
  ServerVariables.Values['URL']             := Url;                //URL=/vbseo.php?vbseourl=forum4/discussion98851/showthread.php&bleubleu=24
  ServerVariables.Values['PATH_INFO']       := ScriptName;         //PATH_INFO=/vbseo.php
  ServerVariables.Values['PATH_TRANSLATED'] := ScriptFileName;     //PATH_TRANSLATED=D:\wwwroot\www.mywebsite.com\vbseo.php
  ServerVariables.Values['PHP_SELF']        := ScriptName;         //PHP_SELF=/vbseo.php
  ServerVariables.Values['SCRIPT_NAME']     := ScriptName;         //SCRIPT_NAME=/vbseo.php | Script Name
  ServerVariables.Values['SCRIPT_FILENAME'] := ScriptFileName;     //SCRIPT_FILENAME=D:\wwwroot\www.mywebsite.com\vbseo.php
  ServerVariables.Values['REQUEST_URI']     := URL;                //REQUEST_URI=/vbseo.php?vbseourl=forum4/discussion98851/showthread.php&bleubleu=24
end;

{*********************************************************************}
Procedure ALCGIInitDefaultServerVariables(ServerVariables: TALStrings);
Begin
  ServerVariables.Clear;
  {----------}
  ServerVariables.Add('HTTPS=off');                          //HTTPS=off
  ServerVariables.Add('HTTP_UA_CPU=x86');                    //HTTP_UA_CPU=x86
  ServerVariables.Add('HTTP_CONNECTION=Keep-Alive');         //HTTP_CONNECTION=Keep-Alive | Whether to close the connection when done
  ServerVariables.Add('SERVER_PROTOCOL=HTTP/1.1');           //SERVER_PROTOCOL=HTTP/1.1 | Server Protocol
  ServerVariables.Add('SERVER_SOFTWARE=Microsoft-IIS/7.0');  //SERVER_SOFTWARE=Microsoft-IIS/5.1 | Server Software
  ServerVariables.Add('SERVER_NAME=127.0.0.1');              //SERVER_NAME=www.mywebsite.com  | Server Host Name
  ServerVariables.Add('SERVER_PORT=80');                     //SERVER_PORT=80 | Server Port Number
  ServerVariables.Add('REMOTE_HOST=127.0.0.1');              //REMOTE_HOST=88.167.177.200 | Client Host Name
  ServerVariables.Add('REMOTE_ADDR=127.0.0.1');              //REMOTE_ADDR=88.167.177.200 | Client IP Number
  ServerVariables.Add('HTTP_HOST=127.0.0.1');                //HTTP_HOST=www.mywebsite.com | Requested Host
  {----------}
  ServerVariables.Add('HTTP_USER_AGENT=Mozilla/4.0 (compatible)');  //HTTP_USER_AGENT=Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; SV1) | Client Software you are using
  ServerVariables.Add('HTTP_ACCEPT=*/*');                           //HTTP_ACCEPT=*/* | List of acceptable content type
end;

{********************************************************************}
Procedure AlCGIInitDefaultServerVariables(ServerVariables: TALStrings;
                                          ScriptName,
                                          ScriptFileName: AnsiString;
                                          Url: AnsiString);
Begin
  AlCGIInitDefaultServerVariables(ServerVariables);
  {----------}
  ServerVariables.Values['URL']             := Url;                //URL=/vbseo.php?vbseourl=forum4/discussion98851/showthread.php&bleubleu=24
  ServerVariables.Values['PATH_INFO']       := ScriptName;         //PATH_INFO=/vbseo.php
  ServerVariables.Values['PATH_TRANSLATED'] := ScriptFileName;     //PATH_TRANSLATED=D:\wwwroot\www.mywebsite.com\vbseo.php
  ServerVariables.Values['PHP_SELF']        := ScriptName;         //PHP_SELF=/vbseo.php
  ServerVariables.Values['SCRIPT_NAME']     := ScriptName;         //SCRIPT_NAME=/vbseo.php | Script Name
  ServerVariables.Values['SCRIPT_FILENAME'] := ScriptFileName;     //SCRIPT_FILENAME=D:\wwwroot\www.mywebsite.com\vbseo.php
  ServerVariables.Values['REQUEST_URI']     := URL;                //REQUEST_URI=/vbseo.php?vbseourl=forum4/discussion98851/showthread.php&bleubleu=24
end;

{***********************************************************************}
Procedure AlCGIInitServerVariablesFromWebRequest(WebRequest: TwebRequest;
                                                 ServerVariables: TALStrings;
                                                 ScriptName,
                                                 ScriptFileName: AnsiString;
                                                 Url: AnsiString);
Begin
  AlCGIInitDefaultServerVariablesFromWebRequest(WebRequest, ServerVariables, ScriptName, ScriptFileName, Url);
  {----------}
  ServerVariables.Add('REQUEST_METHOD='+        WebRequest.GetFieldByName('REQUEST_METHOD'));           //REQUEST_METHOD=GET | Either GET (normal for HTML docs) or POST (normal for forms)
  ServerVariables.Add('CONTENT_TYPE='+          WebRequest.GetFieldByName('CONTENT_TYPE'));             //CONTENT_TYPE=application/x-www-form-urlencoded
  ServerVariables.Add('CONTENT_LENGTH='+        WebRequest.GetFieldByName('CONTENT_LENGTH'));           //CONTENT_LENGTH=207
  ServerVariables.Add('HTTP_CONTENT_ENCODING='+ WebRequest.GetFieldByName('HTTP_CONTENT_ENCODING'));    //HTTP_CONTENT_ENCODING=
  ServerVariables.Add('HTTP_CONTENT_VERSION='+  WebRequest.GetFieldByName('HTTP_CONTENT_VERSION'));     //HTTP_CONTENT_VERSION=
  ServerVariables.Add('QUERY_STRING='+          WebRequest.GetFieldByName('QUERY_STRING'));             //QUERY_STRING=goto=newpost&t=1 | Query String: The characters appended to the URL after a question mark.
  ServerVariables.Add('HTTP_COOKIE='+           WebRequest.GetFieldByName('HTTP_COOKIE'));              //HTTP_COOKIE=bblastvisit=1155301716; bblastactivity=0 | Cookies: information that has been stored on your machine (in the file cookies.txt or MagicCookie) by our server
  {----------}
  ServerVariables.Add('HTTP_REFERER='+          WebRequest.GetFieldByName('HTTP_REFERER'));             //HTTP_REFERER=http://127.0.0.1/usa/forum/ | Address of the document you linked from
  {----------}
  //ServerVariables.Add('HTTP_ACCEPT_ENCODING='+  WebRequest.GetFieldByName('HTTP_ACCEPT_ENCODING'));   //HTTP_ACCEPT_ENCODING=gzip, deflate
  //ServerVariables.Add('HTTP_ACCEPT_LANGUAGE='+  WebRequest.GetFieldByName('HTTP_ACCEPT_LANGUAGE'));   //HTTP_ACCEPT_LANGUAGE=en-us
  //ServerVariables.Add('HTTP_CACHE_CONTROL='+    WebRequest.GetFieldByName('HTTP_CACHE_CONTROL'));     //HTTP_CACHE_CONTROL=
  //ServerVariables.Add('HTTP_IF_MODIFIED_SINCE='+WebRequest.GetFieldByName('HTTP_IF_MODIFIED_SINCE')); //HTTP_IF_MODIFIED_SINCE= | Don't send the item unless it has been changed since the given date
  //ServerVariables.Add('HTTP_DATE='+             WebRequest.GetFieldByName('HTTP_DATE'));              //HTTP_DATE=
  //ServerVariables.Add('HTTP_FROM='+             WebRequest.GetFieldByName('HTTP_FROM'));              //HTTP_FROM= | E-mail address of requesting user
  //ServerVariables.Add('HTTP_DERIVED_FROM='+     WebRequest.GetFieldByName('HTTP_DERIVED_FROM'));      //HTTP_DERIVED_FROM=
  //ServerVariables.Add('HTTP_EXPIRES='+          WebRequest.GetFieldByName('HTTP_EXPIRES'));           //HTTP_EXPIRES=
  //ServerVariables.Add('HTTP_TITLE='+            WebRequest.GetFieldByName('HTTP_TITLE'));             //HTTP_TITLE=
  //ServerVariables.Add('AUTH_TYPE='+             WebRequest.GetFieldByName('AUTH_TYPE'));              //AUTH_TYPE=
  //ServerVariables.Add('AUTH_USER='+             WebRequest.GetFieldByName('AUTH_USER'));              //AUTH_USER=
  //ServerVariables.Add('AUTH_PASSWORD='+         WebRequest.GetFieldByName('AUTH_PASSWORD'));          //AUTH_PASSWORD=
  //ServerVariables.Add('HTTP_AUTHORIZATION='+    WebRequest.GetFieldByName('HTTP_AUTHORIZATION'));     //HTTP_AUTHORIZATION= | Authorization Information
  //ServerVariables.Add('REMOTE_USER='+           WebRequest.GetFieldByName('REMOTE_USER'));            //REMOTE_USER= | Client's User Name (if using Authentification)
  //ServerVariables.Add('REMOTE_IDENT='+          WebRequest.GetFieldByName('REMOTE_IDENT'));           //REMOTE_IDENT= | Client's User Name (using RFC931 protocol)
  //ServerVariables.Add('LOGON_USER='+            WebRequest.GetFieldByName('LOGON_USER'));             //LOGON_USER=
end;

{**************************************************}
Procedure AlCGIExec(InterpreterFilename: AnsiString;
                    ServerVariables: TALStrings;
                    RequestContentStream: Tstream;
                    ResponseContentStream: Tstream;
                    ResponseHeader: TALHTTPResponseHeader);

const EnvironmentTemplate = '%s=%s'#0;

Var ScriptFileName: AnsiString;
    Environment: AnsiString;
    aStream: TALStringStream;
    FreeRequestContentStream: Boolean;
    S1: AnsiString;
    P1: Integer;
    i: integer;

begin

  aStream := TALStringStream.Create('');
  If not assigned(RequestContentStream) then begin
    RequestContentStream := TmemoryStream.Create;
    FreeRequestContentStream := True;
  end
  else FreeRequestContentStream := False;
  Try

    {----------}
    ScriptFileName := ServerVariables.Values['SCRIPT_FILENAME'];

    {For securty issue... if content_length badly set then cpu can go to 100%}
    ServerVariables.Values['CONTENT_LENGTH']  := ALIntToStr(RequestContentStream.Size);

    {init GATEWAY_INTERFACE}
    ServerVariables.Values['GATEWAY_INTERFACE'] := 'CGI/1.1';

    {----------}
    Environment := AlGetEnvironmentString;  //=C:=C:\Program Files\Borland\Delphi7\Projects
                                            //ALLUSERSPROFILE=C:\Documents and Settings\All Users
                                            //APPDATA=C:\Documents and Settings\loki\Application Data
                                            //CLASSPATH=.;C:\Program Files\Java\jre1.5.0_06\lib\ext\QTJava.zip
                                            //CLIENTNAME=Console
                                            //CommonProgramFiles=C:\Program Files\Common Files
                                            //COMPUTERNAME=LOKIMOBILE
                                            //ComSpec=C:\WINDOWS\system32\cmd.exe
                                            //DELPHI=c:\program files\borland\delphi7
                                            //DXVCL=C:\Dev\Library\DevExpress
                                            //FP_NO_HOST_CHECK=NO
                                            //HOMEDRIVE=C:
                                            //HOMEPATH=\Documents and Settings\loki
                                            //LOGONSERVER=\\LOKIMOBILE
                                            //NUMBER_OF_PROCESSORS=1
                                            //OS=Windows_NT
                                            //Path=C:\Program Files\Borland\Delphi7\Bin;C:\Program Files\Borland\Delphi7\Projects\Bpl\;C:\WINDOWS\system32;C:\WINDOWS;C:\WINDOWS\System32\Wbem;C:\Program Files\ATI Technologies\ATI Control Panel;C:\Program Files\QuickTime\QTSystem\;C:\Program Files\MySQL\MySQL Server 5.0\bin
                                            //PATHEXT=.COM;.EXE;.BAT;.CMD;.VBS;.VBE;.JS;.JSE;.WSF;.WSH
                                            //PROCESSOR_ARCHITECTURE=x86
                                            //PROCESSOR_IDENTIFIER=x86 Family 15 Model 2 Stepping 7, GenuineIntel
                                            //PROCESSOR_LEVEL=15
                                            //PROCESSOR_REVISION=0207
                                            //ProgramFiles=C:\Program Files
                                            //QTJAVA=C:\Program Files\Java\jre1.5.0_06\lib\ext\QTJava.zip
                                            //SESSIONNAME=Console
                                            //SystemDrive=C:
                                            //SystemRoot=C:\WINDOWS
                                            //TEMP=C:\DOCUME~1\SVANDE~1\LOCALS~1\Temp
                                            //TMP=C:\DOCUME~1\SVANDE~1\LOCALS~1\Temp
                                            //USERDOMAIN=LOKIMOBILE
                                            //USERNAME=loki
                                            //USERPROFILE=C:\Documents and Settings\loki
                                            //windir=C:\WINDOWS

    {----------}
    For i := 0 to serverVariables.Count - 1 do
      Environment := Environment + ALFormat(EnvironmentTemplate,[ServerVariables.Names[i], ServerVariables.ValueFromIndex[i]]);

    {----------}
    Environment := Environment + #0;

    {----------}
    ALWinExec32(ALQuotedStr(InterpreterFilename,'"') + ' ' + ScriptFileName,
                ALExtractFileDir(InterpreterFilename),
                Environment,
                RequestContentStream,
                aStream);

    {----------}
    S1 := aStream.DataString;
    P1 := AlPos(#13#10#13#10,S1);
    ResponseHeader.RawHeaderText := AlCopyStr(S1,1,P1-1);
    S1 := AlCopyStr(S1,P1+4,MaxInt);
    ResponseContentStream.Write(S1[1], length(S1));

  finally
    aStream.Free;
    If FreeRequestContentStream then RequestContentStream.Free;
  end;
end;

{*****************************}
Procedure AlCGIExec(ScriptName,
                    ScriptFileName,
                    Url,
                    X_REWRITE_URL,
                    InterpreterFilename: AnsiString;
                    WebRequest: TALIsapiRequest;
                    overloadedCookies: AnsiString;
                    overloadedQueryString: AnsiString;
                    overloadedReferer: AnsiString;
                    overloadedRequestContentStream: Tstream;
                    Var ResponseContentString: AnsiString;
                    ResponseHeader: TALHTTPResponseHeader);

Var ServerVariables: TALStrings;
    RequestContentStream: Tstream;
    ResponsecontentStream: TALStringStream;

begin
  ServerVariables := TALStringList.Create;
  ResponsecontentStream := TALStringStream.Create('');
  Try

    If overloadedRequestContentStream <> nil then RequestContentStream := overloadedRequestContentStream
    else RequestContentStream := WebRequest.ContentStream;

    alCGIInitServerVariablesFromWebRequest(WebRequest, ServerVariables, ScriptName, ScriptFileName, Url);
    ServerVariables.Values['HTTP_X_REWRITE_URL'] := X_REWRITE_URL;                                         //HTTP_X_REWRITE_URL=/forum4/discussion98851/showthread.php?bleubleu=24
    If OverloadedCookies <> #0 then ServerVariables.Values['HTTP_COOKIE'] := OverloadedCookies;            //HTTP_COOKIE=bblastvisit=1155301716; bblastactivity=0; bbthreadedmode=linear; sessionid=2025726278; __utmc=96992031; CurrencyCode=EUR; MeasurementSystem=2; language=USA; Source=127.0.0.1; __utma=96992031.751139461.1154095149.1155298085.1155307847.37; __utmz=96992031.1154095149.1.1.utmccn=(direct)|utmcsr=(direct)|utmcmd=(none); vbulletin_collapse=; __utmb=96992031
    If overloadedQueryString <> #0 then ServerVariables.Values['QUERY_STRING'] := overloadedQueryString;   //QUERY_STRING=goto=newpost&t=1
    If overloadedReferer <> #0 then ServerVariables.Values['HTTP_REFERER'] := overloadedReferer;           //HTTP_REFERER=http://www.yahoo.fr

    AlCGIExec(InterpreterFilename,
              ServerVariables,
              RequestContentStream,
              ResponsecontentStream,
              ResponseHeader);

    ResponsecontentString := ResponsecontentStream.DataString;

  finally
    ServerVariables.free;
    ResponsecontentStream.Free;
  end;
end;

{*****************************}
Procedure AlCGIExec(ScriptName,
                    ScriptFileName,
                    Url,
                    X_REWRITE_URL,
                    InterpreterFilename: AnsiString;
                    WebRequest: TALIsapiRequest;
                    Var ResponseContentString: AnsiString;
                    ResponseHeader: TALHTTPResponseHeader);
begin
  AlCGIExec(ScriptName,
            ScriptFileName,
            Url,
            X_REWRITE_URL,
            InterpreterFilename,
            WebRequest,
            #0,
            #0,
            #0,
            nil,
            ResponseContentString,
            ResponseHeader);
end;


end.
