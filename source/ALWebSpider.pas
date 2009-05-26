{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)
							
product:      ALWebSpider
Version:      3.50

Description:  The function in this unit allows you to download a
              World Wide Web site from the Internet to a local directory,
              building recursively all directories, getting HTML, images,
              and other files from the server to your computer. The functions
              arranges the original site's relative link-structure. Simply
              open a page of the "mirrored" website in your browser, and you
              can browse the site from link to link, as if you were viewing it
              online.

Legal issues: Copyright (C) 1999-2009 by Arkadia Software Engineering

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

Know bug :    Link like :
              <td><img src="/imgmix/situation.php?dept=Corse du Sud&coordx=1149.00&coordy=1657.60&t=1133520053 width="200" height="200" border="0"></td>
              Will be not handle corretly because one " is missed.
              it's not an valide HTML document but unfortunatly ie work correctly
              with this kind of error... mean that webmaster can make this error
              without seeing it ! so we need to find a way to handle this error

History :     27/10/2005: Replace LstPageDownloaded and LstPageleft
                          TstringList by binary tree for better performance
                          when crawl huge site with more than 100.000 Url
              16/11/2005: Few improuvement;
              28/11/2005: Move procedure to Component;
              10/09/2007: Move ALCompactHtmlTagParams in ALFcnHTML


Link :

Please send all your feedback to svanderclock@arkadia.com
**************************************************************}
unit ALWebSpider;

interface

uses Windows,
     classes,
     AlHTTPCommon,
     AlHTTPClient;

Type

  {-----------------------------------------------------------------}
  TAlWebSpiderCrawlDownloadSuccessEvent = procedure (Sender: TObject;
                                                     Url: String;
                                                     HTTPResponseHeader: TALHTTPResponseHeader;
                                                     HttpResponseContent: TStream;
                                                     Var StopCrawling: Boolean) of object;

  {------------------------------------------------------------------}
  TAlWebSpiderCrawlDownloadRedirectEvent = procedure (Sender: TObject;
                                                      Url: String;
                                                      RedirectedTo: String;
                                                      HTTPResponseHeader: TALHTTPResponseHeader;
                                                      Var StopCrawling: Boolean) of object;

  {---------------------------------------------------------------}
  TAlWebSpiderCrawlDownloadErrorEvent = procedure (Sender: TObject;
                                                   URL: String;
                                                   ErrorMessage: String;
                                                   HTTPResponseHeader: TALHTTPResponseHeader;
                                                   Var StopCrawling: Boolean) of object;

  {-------------------------------------------------------------}
  TAlWebSpiderCrawlGetNextLinkEvent = procedure (Sender: TObject;
                                                 Var Url: String) of object;

  {----------------------------------------------------------}
  TAlWebSpiderCrawlFindLinkEvent = Procedure (Sender: TObject;
                                              HtmlTagString: String;
                                              HtmlTagParams: TStrings;
                                              URL: String) of object;

  {----------------------------------------------------------------}
  TAlWebSpiderCrawlEndEvent = Procedure (Sender: TObject) of object;

  {-----------------------------------------------------------------------------}
  TAlWebSpiderUpdateLinkToLocalPathGetNextFileEvent = procedure (Sender: TObject;
                                                                 Var FileName: String;
                                                                 Var BaseHref: String) of object;

  {--------------------------------------------------------------------------}
  TAlWebSpiderUpdateLinkToLocalPathFindLinkEvent = Procedure (Sender: TObject;
                                                              HtmlTagString: String;
                                                              HtmlTagParams: TStrings;
                                                              URL: String;
                                                              Var LocalPath: String) of object;

  {--------------------------------------------------------------------------------}
  TAlWebSpiderUpdateLinkToLocalPathEndEvent = Procedure (Sender: TObject) of object;

  {------------------------------}
  TAlWebSpider = Class(TComponent)
  Private
    fOnUpdateLinkToLocalPathEnd: TAlWebSpiderUpdateLinkToLocalPathEndEvent;
    fOnUpdateLinkToLocalPathFindLink: TAlWebSpiderUpdateLinkToLocalPathFindLinkEvent;
    fOnUpdateLinkToLocalPathGetNextFile: TAlWebSpiderUpdateLinkToLocalPathGetNextFileEvent;
    fOnCrawlDownloadError: TAlWebSpiderCrawlDownloadErrorEvent;
    fOnCrawlDownloadRedirect: TAlWebSpiderCrawlDownloadRedirectEvent;
    fOnCrawlDownloadSuccess: TAlWebSpiderCrawlDownloadSuccessEvent;
    FOnCrawlFindLink: TAlWebSpiderCrawlFindLinkEvent;
    fOnCrawlGetNextLink: TAlWebSpiderCrawlGetNextLinkEvent;
    FOnCrawlEnd: TAlWebSpiderCrawlEndEvent;
    FHttpClient: TalHttpClient;
  Protected
  Public
    Procedure Crawl; {Launch the Crawling of the page}
    Procedure UpdateLinkToLocalPath; {Update the link of downloaded page to local path}
  published
    Property  OnCrawlDownloadSuccess: TAlWebSpiderCrawlDownloadSuccessEvent read fOnCrawlDownloadSuccess write fOnCrawlDownloadSuccess; {When a page is successfully downloaded}
    Property  OnCrawlDownloadRedirect: TAlWebSpiderCrawlDownloadRedirectEvent read fOnCrawlDownloadRedirect write fOnCrawlDownloadRedirect; {When a page is redirected}
    Property  OnCrawlDownloadError: TAlWebSpiderCrawlDownloadErrorEvent read fOnCrawlDownloadError write fOnCrawlDownloadError; {When the download of a page encounter an error}
    Property  OnCrawlGetNextLink: TAlWebSpiderCrawlGetNextLinkEvent read fOnCrawlGetNextLink Write FOnCrawlGetNextLink; {When we need another url to download}
    Property  OnCrawlFindLink: TAlWebSpiderCrawlFindLinkEvent read FOnCrawlFindLink Write FOnCrawlFindLink;  {When we find a link in url just downloaded}
    Property  OnCrawlEnd: TAlWebSpiderCrawlEndEvent read FOnCrawlEnd write fOnCrawlEnd; {When their is no more url to crawl}
    Property  OnUpdateLinkToLocalPathGetNextFile: TAlWebSpiderUpdateLinkToLocalPathGetNextFileEvent read fOnUpdateLinkToLocalPathGetNextFile write fOnUpdateLinkToLocalPathGetNextFile;  {When we need another file to update link to local path}
    property  OnUpdateLinkToLocalPathFindLink: TAlWebSpiderUpdateLinkToLocalPathFindLinkEvent read fOnUpdateLinkToLocalPathFindLink write fOnUpdateLinkToLocalPathFindLink; {When we find a link and we need the local path for the file}
    property  OnUpdateLinkToLocalPathEnd: TAlWebSpiderUpdateLinkToLocalPathEndEvent read fOnUpdateLinkToLocalPathEnd write fOnUpdateLinkToLocalPathEnd; {When their is no more local file to update link}
    Property  HttpClient: TalHttpClient Read FHttpClient write FHttpClient; {http client use to crawl the web}
  end;

procedure Register;

implementation

{$R ..\resource\ALWebSpider.dcr}

uses sysutils,
     WinInet,
     UrlMon,
     AlFcnHTML,
     AlFcnMime,
     AlFcnString;

{*****************}
procedure Register;
begin
  RegisterComponents('Alcinoe', [TALWebSpider]);
end;


type

  {*****************************************}
  _TAlWebSpiderHandleTagfunctExtData = record
    WebSpiderObj: TAlWebSpider;
    CurrentBaseHref: String;
  end;

{********************************************************************}
Function _AlWebSpiderExtractUrlHandleTagfunct(const TagString: string;
                                              TagParams: TStrings;
                                              ExtData: pointer;
                                              Var Handled: Boolean): string;

  {-----------------------------------------}
  Procedure FindUrl(aUrl, aBaseHref: String);
  Begin
    {do not work with anchor in self document}
    If (aUrl <> '') and (AlCharPos('#',aUrl) <> 1) then begin

      {make url full path}
      aUrl := AlCombineUrl(
                           aUrl,
                           aBaseHref
                          );

      {exit if it's not a http sheme}
      IF (AlExtractShemeFromUrl(aUrl) in [INTERNET_SCHEME_HTTP, INTERNET_SCHEME_HTTPS]) then

       {fire findlink Event}
        with _TAlWebSpiderHandleTagfunctExtData(ExtData^) do
          WebSpiderObj.FOnCrawlFindLink(
                                        WebSpiderObj,
                                        TagString,
                                        TagParams,
                                        aUrl
                                       );
    end;
  end;

Var Str: String;
    LowerTagString: String;
begin
  Handled := False;
  Result := '';
  ALCompactHtmlTagParams(TagParams);
  LowerTagString := AlLowerCase(TagString);

  with _TAlWebSpiderHandleTagfunctExtData(ExtData^) do begin

    If LowerTagString = 'a' then FindUrl(trim(TagParams.Values['href']),CurrentBaseHref)
    else If LowerTagString = 'applet' then Begin
      str := trim(TagParams.Values['codebase']); //The CODEBASE parameter specifies where the jar and cab files are located.
      {make str full path}
      If str <> '' then Str := AlCombineUrl(
                                            Str,
                                            CurrentBaseHref
                                           )
      else Str := CurrentBaseHref;
      FindUrl(trim(TagParams.Values['code']), Str); //The URL specified by code might be relative to the codebase attribute.
      FindUrl(trim(TagParams.Values['archive']), Str); //The URL specified by code might be relative to the codebase attribute.
    end
    else if LowerTagString = 'area' then FindUrl(Trim(TagParams.Values['href']),CurrentBaseHref)
    else if LowerTagString = 'bgsound' then FindUrl(Trim(TagParams.Values['src']),CurrentBaseHref)
    else if LowerTagString = 'blockquote' then FindUrl(Trim(TagParams.Values['cite']),CurrentBaseHref)
    else if LowerTagString = 'body' then FindUrl(Trim(TagParams.Values['background']),CurrentBaseHref)
    else if LowerTagString = 'del' then FindUrl(Trim(TagParams.Values['cite']),CurrentBaseHref)
    else if LowerTagString = 'embed' then FindUrl(Trim(TagParams.Values['src']),CurrentBaseHref)
    else if LowerTagString = 'frame' then begin
      FindUrl(Trim(TagParams.Values['longdesc']),CurrentBaseHref);
      FindUrl(Trim(TagParams.Values['src']),CurrentBaseHref);
    end
    else if LowerTagString = 'head' then FindUrl(Trim(TagParams.Values['profile']),CurrentBaseHref)
    else if LowerTagString = 'iframe' then begin
      FindUrl(Trim(TagParams.Values['longdesc']),CurrentBaseHref);
      FindUrl(Trim(TagParams.Values['src']),CurrentBaseHref);
    end
    else if LowerTagString = 'ilayer' then begin
      FindUrl(Trim(TagParams.Values['background']),CurrentBaseHref);
      FindUrl(Trim(TagParams.Values['src']),CurrentBaseHref);
    end
    else If LowerTagString = 'img' then Begin
      FindUrl(trim(TagParams.Values['longdesc']),CurrentBaseHref);
      FindUrl(trim(TagParams.Values['src']),CurrentBaseHref);
      FindUrl(trim(TagParams.Values['usemap']),CurrentBaseHref);
      FindUrl(trim(TagParams.Values['dynsrc']),CurrentBaseHref);
      FindUrl(trim(TagParams.Values['lowsrc']),CurrentBaseHref);
    end
    else if LowerTagString = 'input' then begin
      FindUrl(Trim(TagParams.Values['src']),CurrentBaseHref);
      FindUrl(trim(TagParams.Values['usemap']),CurrentBaseHref);
      FindUrl(trim(TagParams.Values['dynsrc']),CurrentBaseHref);
      FindUrl(trim(TagParams.Values['lowsrc']),CurrentBaseHref);
    end
    else if LowerTagString = 'ins' then FindUrl(Trim(TagParams.Values['cite']),CurrentBaseHref)
    else if LowerTagString = 'layer' then begin
      FindUrl(Trim(TagParams.Values['background']),CurrentBaseHref);
      FindUrl(Trim(TagParams.Values['src']),CurrentBaseHref);
    end
    else if LowerTagString = 'link' then FindUrl(Trim(TagParams.Values['href']),CurrentBaseHref)
    else If LowerTagString = 'object' then Begin
      str := trim(TagParams.Values['codebase']); //The CODEBASE parameter specifies where the jar and cab files are located.
      {make str full path}
      If str <> '' then Str := AlCombineUrl(
                                            Str,
                                            CurrentBaseHref
                                           )
      else Str := CurrentBaseHref;
      FindUrl(trim(TagParams.Values['classid']), Str); //The URL specified by code might be relative to the codebase attribute.
      FindUrl(trim(TagParams.Values['data']), Str); //The URL specified by code might be relative to the codebase attribute.
      FindUrl(trim(TagParams.Values['archive']), Str); //The URL specified by code might be relative to the codebase attribute.
      FindUrl(trim(TagParams.Values['usemap']), CurrentBaseHref);
    end
    else if LowerTagString = 'q' then FindUrl(Trim(TagParams.Values['cite']),CurrentBaseHref)
    else if LowerTagString = 'script' then FindUrl(Trim(TagParams.Values['src']),CurrentBaseHref)
    else if LowerTagString = 'table' then FindUrl(Trim(TagParams.Values['background']),CurrentBaseHref)
    else if LowerTagString = 'td' then FindUrl(Trim(TagParams.Values['background']),CurrentBaseHref)
    else if LowerTagString = 'th' then FindUrl(Trim(TagParams.Values['background']),CurrentBaseHref)
    else if LowerTagString = 'xml' then FindUrl(Trim(TagParams.Values['src']),CurrentBaseHref)
    else if LowerTagString = 'base' then Begin
      Str := Trim(TagParams.Values['href']);
      If str <> '' then CurrentBaseHref := Str;
    end;

  end;
end;

{*******************************************************************************}
Function _AlWebSpiderUpdateLinkToLocalPathHandleTagfunct(const TagString: string;
                                                         TagParams: TStrings;
                                                         ExtData: pointer;
                                                         Var Handled: Boolean): string;

  {-----------------------------------------------}
  Procedure FindUrl(aParamName, aBaseHref: String);
  Var aUrl: String;
      aLocalPathValue : string;
  Begin
    {extract Url}
    aUrl := Trim(TagParams.Values[aParamName]);

    {do not work with anchor in self document}
    If (aUrl <> '') and (AlCharPos('#',aUrl) <> 1) then begin

      {make url full path}
      aUrl := AlCombineUrl(
                           aUrl,
                           aBaseHref
                          );

      {exit if it's not a http sheme}
      IF (AlExtractShemeFromUrl(aUrl) in [INTERNET_SCHEME_HTTP, INTERNET_SCHEME_HTTPS]) then begin

        {init local path value}
        aLocalPathValue := '';

        {fire findlink Event}
        with _TAlWebSpiderHandleTagfunctExtData(ExtData^) do
          WebSpiderObj.fOnUpdateLinkToLocalPathFindLink(
                                                        WebSpiderObj,
                                                        TagString,
                                                        TagParams,
                                                        aUrl,
                                                        aLocalPathValue
                                                       );

        {update tagParams}
        If (aLocalPathValue <> '') then begin
          Handled := True;
          TagParams.Values[aParamName] := aLocalPathValue; // 1234.htm#foo
        end;
      end;
    end;
  end;

Var Str: String;
    LowerTagString: String;
    i: Integer;
begin
  Handled := False;
  Result := '';
  ALCompactHtmlTagParams(TagParams);
  LowerTagString := AlLowerCase(TagString);

  with _TAlWebSpiderHandleTagfunctExtData(ExtData^) do begin

    If LowerTagString = 'a' then FindUrl('href',CurrentBaseHref)
    else If LowerTagString = 'applet' then Begin
      str := trim(TagParams.Values['codebase']); //The CODEBASE parameter specifies where the jar and cab files are located.
      {make str full path}
      If str <> '' then Str := AlCombineUrl(
                                            Str,
                                            CurrentBaseHref
                                           )
      else Str := CurrentBaseHref;
      FindUrl('code', Str); //The URL specified by code might be relative to the codebase attribute.
      FindUrl('archive', Str); //The URL specified by code might be relative to the codebase attribute.
    end
    else if LowerTagString = 'area' then FindUrl('href',CurrentBaseHref)
    else if LowerTagString = 'bgsound' then FindUrl('src',CurrentBaseHref)
    else if LowerTagString = 'blockquote' then FindUrl('cite',CurrentBaseHref)
    else if LowerTagString = 'body' then FindUrl('background',CurrentBaseHref)
    else if LowerTagString = 'del' then FindUrl('cite',CurrentBaseHref)
    else if LowerTagString = 'embed' then FindUrl('src',CurrentBaseHref)
    else if LowerTagString = 'frame' then begin
      FindUrl('longdesc',CurrentBaseHref);
      FindUrl('src',CurrentBaseHref);
    end
    else if LowerTagString = 'head' then FindUrl('profile',CurrentBaseHref)
    else if LowerTagString = 'iframe' then begin
      FindUrl('longdesc',CurrentBaseHref);
      FindUrl('src',CurrentBaseHref);
    end
    else if LowerTagString = 'ilayer' then begin
      FindUrl('background',CurrentBaseHref);
      FindUrl('src',CurrentBaseHref);
    end
    else If LowerTagString = 'img' then Begin
      FindUrl('longdesc',CurrentBaseHref);
      FindUrl('src',CurrentBaseHref);
      FindUrl('usemap',CurrentBaseHref);
      FindUrl('dynsrc',CurrentBaseHref);
      FindUrl('lowsrc',CurrentBaseHref);
    end
    else if LowerTagString = 'input' then begin
      FindUrl('src',CurrentBaseHref);
      FindUrl('usemap',CurrentBaseHref);
      FindUrl('dynsrc',CurrentBaseHref);
      FindUrl('lowsrc',CurrentBaseHref);
    end
    else if LowerTagString = 'ins' then FindUrl('cite',CurrentBaseHref)
    else if LowerTagString = 'layer' then begin
      FindUrl('background',CurrentBaseHref);
      FindUrl('src',CurrentBaseHref);
    end
    else if LowerTagString = 'link' then FindUrl('href',CurrentBaseHref)
    else If LowerTagString = 'object' then Begin
      str := trim(TagParams.Values['codebase']); //The CODEBASE parameter specifies where the jar and cab files are located.
      {make str full path}
      If str <> '' then Str := AlCombineUrl(
                                            Str,
                                            CurrentBaseHref
                                           )
      else Str := CurrentBaseHref;
      FindUrl('classid', Str); //The URL specified by code might be relative to the codebase attribute.
      FindUrl('data', Str); //The URL specified by code might be relative to the codebase attribute.
      FindUrl('archive', Str); //The URL specified by code might be relative to the codebase attribute.
      FindUrl('usemap', CurrentBaseHref);
    end
    else if LowerTagString = 'q' then FindUrl('cite',CurrentBaseHref)
    else if LowerTagString = 'script' then FindUrl('src',CurrentBaseHref)
    else if LowerTagString = 'table' then FindUrl('background',CurrentBaseHref)
    else if LowerTagString = 'td' then FindUrl('background',CurrentBaseHref)
    else if LowerTagString = 'th' then FindUrl('background',CurrentBaseHref)
    else if LowerTagString = 'xml' then FindUrl('src',CurrentBaseHref)
    else if LowerTagString = 'base' then begin
      Handled := True;
      exit;
    end;

    {update the html source code}
    If handled then begin
      Result := '<'+TagString;
      for i := 0 to TagParams.Count - 1 do
        If TagParams.Names[i] <> '' then Result := Result + ' ' + TagParams.Names[i] + '="'+ alStringReplace(
                                                                                                             TagParams.ValueFromIndex[i],
                                                                                                             '"',
                                                                                                             '&#34;',
                                                                                                             [rfReplaceAll]
                                                                                                            ) + '"'
        else Result := Result + ' ' + TagParams[i];
      Result := result + '>';
    end;
  end;
end;

{***************************}
procedure TAlWebSpider.Crawl;
Var currentUrl: String;
    StopCrawling: Boolean;
    UrlRedirect: Boolean;
    DownloadError: Boolean;
    CurrentHttpResponseHeader: TALHTTPResponseHeader;
    CurrentHttpResponseContent: TStream;
    aExtData: _TAlWebSpiderHandleTagfunctExtData;
    pMimeTypeFromData: LPWSTR;
    Str: String;
Begin

  Try

    StopCrawling := False;
    If not assigned(FOnCrawlGetNextLink) then exit;

    {start the main loop}
    While True do begin

      CurrentUrl := '';
      FOnCrawlGetNextLink(self, CurrentUrl); {Get the current url to process}
      CurrentUrl := Trim(CurrentUrl);
      If CurrentUrl = '' then Exit; {no more url to download then exit}

      UrlRedirect := False;
      DownloadError := False;
      CurrentHttpResponseContent := TmemoryStream.Create;
      CurrentHttpResponseHeader:= TALHTTPResponseHeader.Create;
      Try

        Try

          {download the page}
          FHttpClient.Get(
                         CurrentURL,
                         CurrentHttpResponseContent,
                         CurrentHttpResponseHeader
                        );

        except
          on E: Exception do begin

            {in case of url redirect}
            If Alpos('3',CurrentHttpResponseHeader.StatusCode)=1 then begin
              UrlRedirect := True;
              If assigned(FOnCrawlDownloadRedirect) then fOnCrawlDownloadRedirect(
                                                                                  Self,
                                                                                  CurrentUrl,
                                                                                  AlCombineUrl(
                                                                                               trim(CurrentHttpResponseHeader.Location),
                                                                                               CurrentUrl
                                                                                              ),
                                                                                  CurrentHttpResponseHeader,
                                                                                  StopCrawling
                                                                                 );
            end

            {in case of any other error}
            else begin
              DownloadError := True;
              If assigned(FOnCrawlDownloadError) then fOnCrawlDownloadError(
                                                                            Self,
                                                                            CurrentUrl,
                                                                            E.Message,
                                                                            CurrentHttpResponseHeader,
                                                                            StopCrawling
                                                                           );
            end;

          end;
        end;

        {download OK}
        If (not UrlRedirect) and (not DownloadError) then begin

          {if size = 0 their is nothing to do}
          if CurrentHTTPResponseContent.Size > 0 then begin

            {read the content in Str}
            CurrentHTTPResponseContent.Position := 0;
            SetLength(Str, CurrentHTTPResponseContent.size);
            CurrentHTTPResponseContent.ReadBuffer(Str[1],CurrentHTTPResponseContent.Size);

            {check the mime content type because some server send wrong mime content type}
            IF (FindMimeFromData(
                                 nil, // bind context - can be nil
                                 nil, // url - can be nil
                                 pchar(str), // buffer with data to sniff - can be nil (pwzUrl must be valid)
                                 length(str), // size of buffer
                                 PWidechar(WideString(CurrentHttpResponseHeader.ContentType)), // proposed mime if - can be nil
                                 0, // will be defined
                                 pMimeTypeFromData, // the suggested mime
                                 0 // must be 0
                                ) <> NOERROR) then pMimeTypeFromData := PWidechar(WideString(CurrentHttpResponseHeader.ContentType));

            {lanche the analyze of the page if content type = text/html}
            If sametext(pMimeTypeFromData,'text/html') and
               assigned(FOnCrawlFindLink) then begin

              {init the CurrentBaseHref of the aExtData object}
              aExtData.WebSpiderObj := self;
              aExtData.CurrentBaseHref := CurrentURL;

              {extract the list of url to download}
              ALHideHtmlUnwantedTagForHTMLHandleTagfunct(Str, False, #1);
              ALFastTagReplace(
                               Str,
                               '<',
                               '>',
                               _AlWebSpiderExtractUrlHandleTagfunct,
                               true,
                               @aExtData,
                               [rfreplaceall]
                              );
            end;

          end;

          {trigger the event OnCrawlDownloadSuccess}
          if assigned(FOnCrawlDownloadSuccess) then begin
            CurrentHTTPResponseContent.Position := 0;
            fOnCrawlDownloadSuccess(
                                    self,
                                    CurrentUrl,
                                    CurrentHttpResponseHeader,
                                    CurrentHttpResponseContent,
                                    StopCrawling
                                   );
          end;

        end;

        {if StopCrawling then exit}
        If StopCrawling then exit;

      finally
        CurrentHTTPResponseContent.free;
        CurrentHTTPResponseHeader.free;
      end;
    end;

  finally
    If assigned(FOnCrawlEnd) then FOnCrawlEnd(self);
  end;
end;

{*******************************************}
procedure TAlWebSpider.UpdateLinkToLocalPath;
Var currentFileName: String;
    CurrentBaseHref: String;
    aExtData: _TAlWebSpiderHandleTagfunctExtData;
    Str: String;
Begin

  Try

    If not assigned(FOnUpdateLinktoLocalPathGetNextFile) or not assigned(OnUpdateLinkToLocalPathFindLink) then exit;

    {start the main loop}
    While True do begin

      CurrentFileName := '';
      CurrentBaseHref := '';
      FOnUpdateLinktoLocalPathGetNextFile(self, CurrentFileName, CurrentBaseHref); {Get the current html file to process}
      CurrentFileName := trim(CurrentFileName);
      CurrentBaseHref := trim(CurrentBaseHref);
      If CurrentFileName = '' then Exit; {no more file to update}

      iF FileExists(CurrentFileName) then begin
        {DOWNLOAD THE BODY}
        Str := AlGetStringFromFile(CurrentFileName);

        {init the CurrentBaseHref of the aExtData object}
        aExtData.WebSpiderObj := self;
        aExtData.CurrentBaseHref := CurrentBaseHref;

        {Update the link}
        ALHideHtmlUnwantedTagForHTMLHandleTagfunct(str, False, #1);
        str := ALFastTagReplace(
                                str,
                                '<',
                                '>',
                                _AlWebSpiderUpdateLinkToLocalPathHandleTagfunct,
                                true,
                                @aExtData,
                                [rfreplaceall]
                               );

        {restore the page to it's original format}
        str := AlStringReplace(
                               str,
                               #1,
                               '<',
                               [rfReplaceAll]
                              );

        {save the result string}
        AlSaveStringToFile(Str,CurrentFileName);
      end;
    end;

  finally
    If assigned(FOnUpdateLinkToLocalPathEnd) then FOnUpdateLinkToLocalPathEnd(self);
  end;
end;


end.
