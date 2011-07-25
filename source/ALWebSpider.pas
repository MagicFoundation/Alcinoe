{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
svn:          https://alcinoe.svn.sourceforge.net/svnroot/alcinoe              
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

Legal issues: Copyright (C) 1999-2010 by Arkadia Software Engineering

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

* Please send all your feedback to svanderclock@arkadia.com
* If you have downloaded this source from a website different from 
  sourceforge.net, please get the last version on http://sourceforge.net/projects/alcinoe/
* Please, help us to keep the development of these components free by 
  voting on http://www.arkadia.com/html/alcinoe_like.html
**************************************************************}
unit ALWebSpider;

interface

uses Windows,
     classes,
     AlAvlBinaryTree,
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

  {----------------------------------------------------------------------------------------}
  TAlWebSpiderCrawlBeforeDownloadEvent = Procedure (Sender: TObject; Url: String) of object;

  {---------------------------------------------------------------}
  TAlWebSpiderCrawlAfterDownloadEvent = Procedure (Sender: TObject;
                                                   Url: String;
                                                   HTTPResponseHeader: TALHTTPResponseHeader;
                                                   HttpResponseContent: TStream;
                                                   Var StopCrawling: Boolean) of object;

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
    fOnCrawlBeforeDownload: TAlWebSpiderCrawlBeforeDownloadEvent;
    fOnCrawlAfterDownload: TAlWebSpiderCrawlAfterDownloadEvent;
  Protected
  Public
    Procedure Crawl; {Launch the Crawling of the page}
    Procedure UpdateLinkToLocalPath; {Update the link of downloaded page to local path}
  published
    Property  OnCrawlBeforeDownload: TAlWebSpiderCrawlBeforeDownloadEvent read fOnCrawlBeforeDownload write fOnCrawlBeforeDownload; {When a page is successfully downloaded}
    Property  OnCrawlAfterDownload: TAlWebSpiderCrawlAfterDownloadEvent read fOnCrawlAfterDownload write fOnCrawlAfterDownload; {When a page is successfully downloaded}
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

  {---------------------------------------------------------------------------------------------------------------------------------------}
  TAlTrivialWebSpiderCrawlProgressEvent = Procedure (Sender: TObject; UrltoDownload, UrlDownloaded: Integer; CurrentUrl: String) of object;

  {---------------------------------------------------------------------------------------------------------------}
  TAlTrivialWebSpiderUpdateLinkToLocalPathProgressEvent = Procedure (Sender: TObject; aFileName: String) of object;

  {-----------------------------------------------------------------}
  TAlTrivialWebSpiderCrawlFindLinkEvent = Procedure (Sender: TObject;
                                                     HtmlTagString: String;
                                                     HtmlTagParams: TStrings;
                                                     URL: String;
                                                     Var Ignore: Boolean) of object;

  {----------------------------------}
  TAlTrivialWebSpider = Class(Tobject)
  Private
    FWebSpider: TalWebSpider;
    fStartUrl: String;
    fLstUrlCrawled: Tstrings;
    fLstErrorEncountered: Tstrings;
    FPageDownloadedBinTree: TAlStringKeyAVLBinaryTree;
    FPageNotYetDownloadedBinTree: TAlStringKeyAVLBinaryTree;
    FCurrentDeepLevel: Integer;
    FCurrentLocalFileNameIndex: Integer;
    fMaxDeepLevel: Integer;
    fOnCrawlBeforeDownload: TAlWebSpiderCrawlBeforeDownloadEvent;
    fUpdateLinkToLocalPath: boolean;
    fExcludeMask: String;
    fStayInStartDomain: Boolean;
    fSaveDirectory: String;
    fSplitDirectoryAmount: integer;
    FHttpClient: TalHttpClient;
    fIncludeMask: String;
    fOnCrawlAfterDownload: TAlWebSpiderCrawlAfterDownloadEvent;
    fOnCrawlFindLink: TAlTrivialWebSpiderCrawlFindLinkEvent;
    fDownloadImage: Boolean;
    fOnUpdateLinkToLocalPathProgress: TAlTrivialWebSpiderUpdateLinkToLocalPathProgressEvent;
    fOnCrawlProgress: TAlTrivialWebSpiderCrawlProgressEvent;
    procedure WebSpiderCrawlDownloadError(Sender: TObject; URL, ErrorMessage: String; HTTPResponseHeader: TALHTTPResponseHeader; var StopCrawling: Boolean);
    procedure WebSpiderCrawlDownloadRedirect(Sender: TObject; Url, RedirectedTo: String; HTTPResponseHeader: TALHTTPResponseHeader; var StopCrawling: Boolean);
    procedure WebSpiderCrawlDownloadSuccess(Sender: TObject; Url: String; HTTPResponseHeader: TALHTTPResponseHeader; HttpResponseContent: TStream; var StopCrawling: Boolean);
    procedure WebSpiderCrawlFindLink(Sender: TObject; HtmlTagString: String; HtmlTagParams: TStrings; URL: String);
    procedure WebSpiderCrawlGetNextLink(Sender: TObject; var Url: String);
    procedure WebSpiderUpdateLinkToLocalPathFindLink(Sender: TObject; HtmlTagString: String; HtmlTagParams: TStrings; URL: String; var LocalPath: String);
    procedure WebSpiderUpdateLinkToLocalPathGetNextFile(Sender: TObject; var FileName, BaseHref: String);
    function GetNextLocalFileName(aContentType: String): String;
  Protected
  Public
    Constructor Create;
    Destructor Destroy; override;
    Procedure Crawl(aUrl: String); overload; {Launch the Crawling of the page}
    procedure Crawl(aUrl: String; LstUrlCrawled: Tstrings; LstErrorEncountered: Tstrings); overload;
    Property HttpClient: TalHttpClient Read FHttpClient write FHttpClient;
    Property DownloadImage: Boolean read fDownloadImage write fDownloadImage default false;
    Property StayInStartDomain: Boolean read fStayInStartDomain write fStayInStartDomain default true;
    Property UpdateLinkToLocalPath: boolean read fUpdateLinkToLocalPath write fUpdateLinkToLocalPath default True;
    Property MaxDeepLevel: Integer read fMaxDeepLevel write fMaxDeepLevel default -1;
    Property ExcludeMask: String read fExcludeMask write fExcludeMask;
    Property IncludeMask: String read fIncludeMask write fIncludeMask;
    Property SaveDirectory: String read fSaveDirectory write fSaveDirectory;
    Property SplitDirectoryAmount: integer read fSplitDirectoryAmount write fSplitDirectoryAmount default 5000;
    Property OnCrawlBeforeDownload: TAlWebSpiderCrawlBeforeDownloadEvent read fOnCrawlBeforeDownload write fOnCrawlBeforeDownload; {When a page is successfully downloaded}
    Property OnCrawlAfterDownload: TAlWebSpiderCrawlAfterDownloadEvent read fOnCrawlAfterDownload write fOnCrawlAfterDownload; {When a page is successfully downloaded}
    Property OnCrawlFindLink: TAlTrivialWebSpiderCrawlFindLinkEvent read fOnCrawlFindLink write fOnCrawlFindLink; {When a a link is found}
    Property OnCrawlProgress: TAlTrivialWebSpiderCrawlProgressEvent read fOnCrawlProgress write fOnCrawlProgress;
    Property OnUpdateLinkToLocalPathProgress: TAlTrivialWebSpiderUpdateLinkToLocalPathProgressEvent read fOnUpdateLinkToLocalPathProgress write fOnUpdateLinkToLocalPathProgress;
  end;

  {----------------------------------------------------------------------------------}
  TAlTrivialWebSpider_PageDownloadedBinTreeNode = Class(TALStringKeyAVLBinaryTreeNode)
  Private
  Protected
  Public
    Data: String;
  end;

  {----------------------------------------------------------------------------------------}
  TAlTrivialWebSpider_PageNotYetDownloadedBinTreeNode = Class(TALStringKeyAVLBinaryTreeNode)
  Private
  Protected
  Public
    DeepLevel: Integer;
  end;

procedure Register;

implementation

{$R ..\resource\ALWebSpider.dcr}

uses sysutils,
     WinInet,
     UrlMon,
     Masks,
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

          {the onbeforedownloadevent}
          if assigned(fOnCrawlBeforeDownload) then fOnCrawlBeforeDownload(Self,CurrentURL);
          Try
            {download the page}
            FHttpClient.Get(
                            CurrentURL,
                            CurrentHttpResponseContent,
                            CurrentHttpResponseHeader
                           );
          Finally
            {the onAfterdownloadevent}
            if assigned(fOnCrawlAfterDownload) then fOnCrawlAfterDownload(Self,CurrentURL, CurrentHttpResponseHeader, CurrentHttpResponseContent, StopCrawling);
          End;

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



///////////////////////////////
///// TAlTrivialWebSpider /////
///////////////////////////////

{************************************************}
procedure TAlTrivialWebSpider.Crawl(aUrl: String);
begin
  Crawl(aUrl, nil, nil);
end;

{********************************************************************************************************}
procedure TAlTrivialWebSpider.Crawl(aUrl: String; LstUrlCrawled: Tstrings; LstErrorEncountered: Tstrings);
Var aNode: TAlTrivialWebSpider_PageNotYetDownloadedBinTreeNode;
Begin
  {check the SaveDirectory}
  if (SaveDirectory <> '') and not directoryExists(SaveDirectory) then Raise Exception.Create('The directory: '+ SaveDirectory + ' not exist!');
  if fHttpClient = nil then Raise Exception.Create('The HttpClient cannot be empty!');
  
  {init private var}
  fStartUrl := trim(aUrl);
  FCurrentDeepLevel := 0;
  FCurrentLocalFileNameIndex := 0;
  fLstUrlCrawled := LstUrlCrawled;
  fLstErrorEncountered := LstErrorEncountered;
  FPageDownloadedBinTree:= TAlStringKeyAVLBinaryTree.Create;
  FPageNotYetDownloadedBinTree:= TAlStringKeyAVLBinaryTree.Create;
  Try

    {add editURL2Crawl.text to the fPageNotYetDownloadedBinTree}
    aNode:= TAlTrivialWebSpider_PageNotYetDownloadedBinTreeNode.Create;
    aNode.ID := fStartUrl;
    aNode.DeepLevel := 0;
    FPageNotYetDownloadedBinTree.AddNode(aNode);

    {start the crawl}
    fWebSpider.HttpClient := fHttpClient;
    FWebSpider.onCrawlBeforeDownload := onCrawlBeforeDownload;
    FWebSpider.onCrawlAfterDownload := onCrawlAfterDownload;
    fWebSpider.Crawl;

    {update the link on downloaded page to local path}
    if fUpdateLinkToLocalPath then fWebSpider.UpdateLinkToLocalPath;

  finally
    FPageDownloadedBinTree.Free;
    FPageNotYetDownloadedBinTree.Free;
    fStartUrl := '';
    FCurrentDeepLevel := 0;
    FCurrentLocalFileNameIndex := 0;
    fLstUrlCrawled := nil;
    fLstErrorEncountered := nil;
    fWebSpider.HttpClient := nil;
  end;
end;

{*************************************}
constructor TAlTrivialWebSpider.Create;
begin
  FWebSpider := TalWebSpider.Create(nil);
  fStartUrl := '';
  fLstUrlCrawled := nil;
  fLstErrorEncountered := nil;
  FPageDownloadedBinTree := nil;
  FPageNotYetDownloadedBinTree := nil;
  FCurrentDeepLevel := 0;
  FCurrentLocalFileNameIndex := 0;
  fMaxDeepLevel := -1;
  fOnCrawlBeforeDownload := nil;
  fUpdateLinkToLocalPath := True;
  fExcludeMask := '';
  fStayInStartDomain := True;
  fSaveDirectory := '';
  fSplitDirectoryAmount := 5000;
  FHttpClient := nil;
  fIncludeMask := '*';
  fOnCrawlAfterDownload := nil;
  fOnCrawlFindLink := nil;
  fDownloadImage := False;
  fOnUpdateLinkToLocalPathProgress:=nil;
  fOnCrawlProgress:=nil;

  FWebSpider.onCrawlDownloadError := WebSpiderCrawlDownloadError;
  FWebSpider.onCrawlDownloadRedirect := WebSpiderCrawlDownloadRedirect;
  FWebSpider.onCrawlDownloadSuccess := WebSpiderCrawlDownloadSuccess;
  FWebSpider.onCrawlFindLink := WebSpiderCrawlFindLink;
  FWebSpider.onCrawlGetNextLink := WebSpiderCrawlGetNextLink;
  FWebSpider.onUpdateLinkToLocalPathFindLink := WebSpiderUpdateLinkToLocalPathFindLink;
  FWebSpider.onUpdateLinkToLocalPathGetNextFile := WebSpiderUpdateLinkToLocalPathGetNextFile;
end;

{*************************************}
destructor TAlTrivialWebSpider.Destroy;
begin
  FWebSpider.Free;
  inherited;
end;

{******************************************************************************}
function TAlTrivialWebSpider.GetNextLocalFileName(aContentType: String): String;
Var aExt: String;

  {-------------------------------------}
  Function SplitPathMakeFilename: String;
  begin
    Result := fSaveDirectory + inttostr((FCurrentLocalFileNameIndex div fSplitDirectoryAmount) * fSplitDirectoryAmount + fSplitDirectoryAmount) + '\';
    If (not DirectoryExists(Result)) and (not createDir(Result)) then raise exception.Create('cannot create dir: ' + Result);
    Result := Result + inttostr(FCurrentLocalFileNameIndex) + aExt;
    inc(FCurrentLocalFileNameIndex);
  end;

Begin
  if fSaveDirectory = '' then result := ''
  else begin
    aExt := ALlowercase(ALGetDefaultFileExtFromMimeContentType(aContentType)); // '.htm'

    If FCurrentLocalFileNameIndex = 0 then Begin
      result := fSaveDirectory + 'Start' + aExt;
      inc(FCurrentLocalFileNameIndex);
    end
    else result := SplitPathMakeFilename;
  end;
end;

{************************************************************************}
procedure TAlTrivialWebSpider.WebSpiderCrawlDownloadError(Sender: TObject;
                                                          URL, ErrorMessage: String;
                                                          HTTPResponseHeader: TALHTTPResponseHeader;
                                                          var StopCrawling: Boolean);
Var aNode: TAlTrivialWebSpider_PageDownloadedBinTreeNode;
begin
  {add the url to downloaded list}
  aNode:= TAlTrivialWebSpider_PageDownloadedBinTreeNode.Create;
  aNode.ID := Url;
  aNode.data := '!';
  If not FPageDownloadedBinTree.AddNode(aNode) then aNode.Free;

  {delete the url from the not yet downloaded list}
  FPageNotYetDownloadedBinTree.DeleteNode(url);

  {update label}
  if assigned(fLstErrorEncountered) then fLstErrorEncountered.Add(ErrorMessage);
  if assigned(fOnCrawlProgress) then fOnCrawlProgress(self,FPageNotYetDownloadedBinTree.nodeCount,FPageDownloadedBinTree.nodeCount, Url);
end;

{***************************************************************************}
procedure TAlTrivialWebSpider.WebSpiderCrawlDownloadRedirect(Sender: TObject;
                                                             Url, RedirectedTo: String;
                                                             HTTPResponseHeader: TALHTTPResponseHeader;
                                                             var StopCrawling: Boolean);
Var aNode: TALStringKeyAVLBinaryTreeNode;
begin
  {add the url to downloaded list}
  aNode:= TAlTrivialWebSpider_PageDownloadedBinTreeNode.Create;
  aNode.ID := Url;
  TAlTrivialWebSpider_PageDownloadedBinTreeNode(aNode).data := '=>'+RedirectedTo;
  If not FPageDownloadedBinTree.AddNode(aNode) then aNode.Free;

  {delete the url from the not yet downloaded list}
  FPageNotYetDownloadedBinTree.DeleteNode(url);

  {Stay in start site}
  If not fStayInStartDomain or
     (ALlowercase(AlExtractHostNameFromUrl(trim(fStartUrl))) = ALlowercase(AlExtractHostNameFromUrl(RedirectedTo))) then begin

    {remove the anchor}
    RedirectedTo := AlRemoveAnchorFromUrl(RedirectedTo);

    {add the redirectTo url to the not yet downloaded list}
    If FPageDownloadedBinTree.FindNode(RedirectedTo) = nil then begin
      aNode:= TAlTrivialWebSpider_PageNotYetDownloadedBinTreeNode.Create;
      aNode.ID := RedirectedTo;
      TAlTrivialWebSpider_PageNotYetDownloadedBinTreeNode(aNode).DeepLevel := FCurrentDeepLevel;
      If not FPageNotYetDownloadedBinTree.AddNode(aNode) then aNode.Free;
    end;

  end;

  {update label}
  if assigned(fOnCrawlProgress) then fOnCrawlProgress(self,FPageNotYetDownloadedBinTree.nodeCount,FPageDownloadedBinTree.nodeCount, Url);
end;


{**************************************************************************}
procedure TAlTrivialWebSpider.WebSpiderCrawlDownloadSuccess(Sender: TObject;
                                                            Url: String;
                                                            HTTPResponseHeader: TALHTTPResponseHeader;
                                                            HttpResponseContent: TStream;
                                                            var StopCrawling: Boolean);
Var aNode: TAlTrivialWebSpider_PageDownloadedBinTreeNode;
    Str: String;
    AFileName: String;
    pMimeTypeFromData: LPWSTR;
begin

  {put the content in str}
  HttpResponseContent.Position := 0;
  SetLength(Str, HttpResponseContent.size);
  HttpResponseContent.ReadBuffer(Str[1],HttpResponseContent.Size);

  {we add a check here to be sure that the file is an http file (text file}
  {Some server send image with text/htm content type}
  IF (FindMimeFromData(
                       nil, // bind context - can be nil
                       nil, // url - can be nil
                       pchar(str), // buffer with data to sniff - can be nil (pwzUrl must be valid)
                       length(str), // size of buffer
                       PWidechar(WideString(HTTPResponseHeader.ContentType)), // proposed mime if - can be nil
                       0, // will be defined
                       pMimeTypeFromData, // the suggested mime
                       0 // must be 0
                      ) <> NOERROR) then pMimeTypeFromData := PWidechar(WideString(HTTPResponseHeader.ContentType));

  {Get the FileName where to save the responseContent}
  aFileName := GetNextLocalFileName(pMimeTypeFromData);

  {If html then add <!-- saved from '+ URL +' -->' at the top of the file}
  if aFileName <> '' then begin
    If sametext(pMimeTypeFromData,'text/html') then begin
      Str := '<!-- saved from '+ URL+' -->' +#13#10 + Str;
      AlSaveStringToFile(str,aFileName);
    end
    {Else Save the file without any change}
    else TmemoryStream(HttpResponseContent).SaveToFile(aFileName);
  end;

  {delete the Url from the PageNotYetDownloadedBinTree}
  FPageNotYetDownloadedBinTree.DeleteNode(Url);

  {add the url to the PageDownloadedBinTree}
  aNode:= TAlTrivialWebSpider_PageDownloadedBinTreeNode.Create;
  aNode.ID := Url;
  aNode.data := AlCopyStr(AFileName,length(fSaveDirectory) + 1,maxint);
  If not FPageDownloadedBinTree.AddNode(aNode) then aNode.Free;

  {update label}
  if assigned(fLstUrlCrawled) then fLstUrlCrawled.add(Url);
  if assigned(fOnCrawlProgress) then fOnCrawlProgress(self,FPageNotYetDownloadedBinTree.nodeCount,FPageDownloadedBinTree.nodeCount, Url);
end;

{*******************************************************************}
procedure TAlTrivialWebSpider.WebSpiderCrawlFindLink(Sender: TObject;
                                                     HtmlTagString: String;
                                                     HtmlTagParams: TStrings;
                                                     URL: String);
Var aNode: TAlTrivialWebSpider_PageNotYetDownloadedBinTreeNode;
    Lst: TstringList;
    I: integer;
    Flag1 : Boolean;
    S1: String;
begin
  {If Check BoxDownload Image}
  IF not fDownloadImage and
     (
      sametext(HtmlTagString,'img') or
      (
       sametext(HtmlTagString,'input') and
       sametext(Trim(HtmlTagParams.Values['type']),'image')
      )
     )
    then Exit;

  {Stay in start site}
  If fStayInStartDomain and
     (ALlowercase(AlExtractHostNameFromUrl(trim(fStartUrl))) <> ALlowercase(AlExtractHostNameFromUrl(Url))) then exit;

  {DeepLevel}
  If (fMaxDeepLevel >= 0) and (FCurrentDeepLevel + 1 > fMaxDeepLevel) then exit;

  {include link(s)}
  If fIncludeMask <> '' then begin
    Lst := TstringList.Create;
    Try
      Lst.Text := Trim(AlStringReplace(FIncludeMask,';',#13#10,[RfReplaceall]));
      Flag1 := True;
      For i := 0 to Lst.Count - 1 do begin
        S1 := Trim(Lst[i]);
        If S1 <> '' then begin
          Flag1 := MatchesMask(URL, S1);
          If Flag1 then Break;
        end;
      end;
      If not flag1 then Exit;
    Finally
      Lst.Free;
    end;
  end;

  {Exclude link(s)}
  If fExcludeMask <> '' then begin
    Lst := TstringList.Create;
    Try
      Lst.Text := Trim(AlStringReplace(fExcludeMask,';',#13#10,[RfReplaceall]));
      Flag1 := False;
      For i := 0 to Lst.Count - 1 do begin
        S1 := Trim(Lst[i]);
        If S1 <> '' then begin
          Flag1 := MatchesMask(URL, S1);
          If Flag1 then Break;
        end;
      end;
      If flag1 then Exit;
    Finally
      Lst.Free;
    end;
  end;

  {remove the anchor}
  URL := AlRemoveAnchorFromUrl(URL);

  {call OnCrawlFindLink}
  Flag1 := False;
  if assigned(fOnCrawlFindLink) then fOnCrawlFindLink(
                                                      Sender,
                                                      HtmlTagString,
                                                      HtmlTagParams,
                                                      URL,
                                                      Flag1
                                                     );
  if Flag1 then exit;

  {If the link not already downloaded then add it to the FPageNotYetDownloadedBinTree}
  If FPageDownloadedBinTree.FindNode(url) = nil then begin
    aNode:= TAlTrivialWebSpider_PageNotYetDownloadedBinTreeNode.Create;
    aNode.ID := Url;
    aNode.DeepLevel := FCurrentDeepLevel + 1;
    If not FPageNotYetDownloadedBinTree.AddNode(aNode) then aNode.Free;
  end;
end;

{****************************************************************************************}
procedure TAlTrivialWebSpider.WebSpiderCrawlGetNextLink(Sender: TObject; var Url: String);

    {-----------------------------------------------------------------------------}
    function InternalfindNextUrlToDownload(aNode: TAlTrivialWebSpider_PageNotYetDownloadedBinTreeNode;
                                           alowDeepLevel: Integer): TAlTrivialWebSpider_PageNotYetDownloadedBinTreeNode;
    Var aTmpNode1, aTmpNode2: TAlTrivialWebSpider_PageNotYetDownloadedBinTreeNode;
    Begin
      If (not assigned(Anode)) or (aNode.DeepLevel <= alowDeepLevel) then result := aNode
      else begin

        if aNode.ChildNodes[true] <> nil then begin
          aTmpNode1 := InternalfindNextUrlToDownload(TAlTrivialWebSpider_PageNotYetDownloadedBinTreeNode(aNode.ChildNodes[true]), alowDeepLevel);
          If (assigned(aTmpNode1)) and (aTmpNode1.DeepLevel <= alowDeepLevel) then begin
            result := aTmpNode1;
            exit;
          end;
        end
        else aTmpNode1 := nil;

        if aNode.ChildNodes[false] <> nil then begin
          aTmpNode2 := InternalfindNextUrlToDownload(TAlTrivialWebSpider_PageNotYetDownloadedBinTreeNode(aNode.ChildNodes[false]), alowDeepLevel);
          If (assigned(aTmpNode2)) and (aTmpNode2.DeepLevel <= alowDeepLevel) then begin
            result := aTmpNode2;
            exit;
          end;
        end
        else aTmpNode2 := nil;

        result := aNode;
        If assigned(aTmpNode1) and (result.deepLevel > aTmpNode1.deeplevel) then result := aTmpNode1;
        If assigned(aTmpNode2) and (result.deepLevel > aTmpNode2.deeplevel) then result := aTmpNode2;

      end;
    end;

Var aNode: TAlTrivialWebSpider_PageNotYetDownloadedBinTreeNode;
begin
  {If theire is more url to download}
  IF FPageNotYetDownloadedBinTree.NodeCount > 0 then begin

    {Find next url with deeplevel closer to FCurrentDeepLevel}
    If fMaxDeepLevel >= 0 then aNode := InternalfindNextUrlToDownload(
                                                                      TAlTrivialWebSpider_PageNotYetDownloadedBinTreeNode(FPageNotYetDownloadedBinTree.head),
                                                                      FCurrentDeepLevel
                                                                     )

    {Find next url without take care of FCurrentDeepLevel}
    else aNode := TAlTrivialWebSpider_PageNotYetDownloadedBinTreeNode(FPageNotYetDownloadedBinTree.head);

    Url := aNode.ID;
    FCurrentDeepLevel := TAlTrivialWebSpider_PageNotYetDownloadedBinTreeNode(aNode).DeepLevel;
  end

  {If their is no more url to download then exit}
  else begin
    Url := '';
    FCurrentDeepLevel := -1;
  end;
end;


{***********************************************************************************}
procedure TAlTrivialWebSpider.WebSpiderUpdateLinkToLocalPathFindLink(Sender: TObject;
                                                                     HtmlTagString: String;
                                                                     HtmlTagParams: TStrings;
                                                                     URL: String;
                                                                     var LocalPath: String);
Var aNode: TALStringKeyAVLBinaryTreeNode;
    aAnchorValue: String;
begin
  LocalPath := '';

  If Url <> '' then begin

    {Find the local Path}
    While True Do begin
      Url := AlRemoveAnchorFromUrl(Url, aAnchorValue);
      aNode := FPageDownloadedBinTree.FindNode(URL);
      If (aNode <> nil) then begin
        LocalPath := TAlTrivialWebSpider_PageDownloadedBinTreeNode(aNode).Data;
        If AlPos('=>',LocalPath) = 1 then Begin
          Url := AlCopyStr(LocalPath,3,MaxInt);
          LocalPath := '';
        end
        else Break;
      end
      else Break;
    end;

    If LocalPath = '!' then localpath := ''
    else If LocalPath <> '' then begin
      LocalPath := AlStringReplace(
                                   LocalPath,
                                   '\',
                                   '/',
                                   [RfReplaceall]
                                  ) + aAnchorValue;
      If (FCurrentLocalFileNameIndex >= 0) then LocalPath := '../' + LocalPath;
    end;
  end;
end;

{**************************************************************************************}
procedure TAlTrivialWebSpider.WebSpiderUpdateLinkToLocalPathGetNextFile(Sender: TObject;
                                                                        var FileName, BaseHref: String);
  {-------------------------------------}
  Function SplitPathMakeFilename: String;
  begin
    If FCurrentLocalFileNameIndex < 0 then result := ''
    else If FCurrentLocalFileNameIndex = 0 then result := fSaveDirectory + 'Start.htm'
    else Result := fSaveDirectory + inttostr((FCurrentLocalFileNameIndex div SplitDirectoryAmount) * SplitDirectoryAmount + SplitDirectoryAmount) + '\' + inttostr(FCurrentLocalFileNameIndex) + '.htm';
    dec(FCurrentLocalFileNameIndex);
  end;

Begin
  if fSaveDirectory = '' then FileName := ''
  else begin

    {Find FileName}
    FileName := SplitPathMakeFilename;
    While (FileName <> '') and not fileExists(FileName) do
      Filename := SplitPathMakeFilename;

  end;

  {if filename found}
  If FileName <> '' then Begin

    {Extract the Base Href}
    BaseHref := AlGetStringFromFile(FileName);
    BaseHref := Trim(
                     AlCopyStr(
                               BaseHref,
                               17,                        // '<!-- saved from ' + URL
                               AlPos(#13,BaseHref) - 21    // URL + ' -->' +#13#10
                              )
                    );

    {update label}
    if assigned(fOnUpdateLinkToLocalPathProgress) then fOnUpdateLinkToLocalPathProgress(self, FileName);

  end
  else BaseHref := '';

end;

end.
