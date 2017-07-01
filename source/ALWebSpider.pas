{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
svn:          svn checkout svn://svn.code.sf.net/p/alcinoe/code/ alcinoe-code              
Author(s):    Stéphane Vander Clock (skype/email: svanderclock@yahoo.fr)
							
product:      ALWebSpider
Version:      4.00

Description:  The function in this unit allows you to download a
              World Wide Web site from the Internet to a local directory,
              building recursively all directories, getting HTML, images,
              and other files from the server to your computer. The functions
              arranges the original site's relative link-structure. Simply
              open a page of the "mirrored" website in your browser, and you
              can browse the site from link to link, as if you were viewing it
              online.

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
              26/06/2012: Add xe2 support

Link :

**************************************************************}
unit ALWebSpider;

interface

{$IF CompilerVersion >= 25} {Delphi XE4}
  {$LEGACYIFEND ON} // http://docwiki.embarcadero.com/RADStudio/XE4/en/Legacy_IFEND_(Delphi)
{$IFEND}

Uses {$IF CompilerVersion >= 23} {Delphi XE2}
     System.classes,
     {$ELSE}
     classes,
     {$IFEND}
     AlAvlBinaryTree,
     AlHTTPClient,
     AlStringList;

Type

  {-----------------------------------------------------------------}
  TAlWebSpiderCrawlDownloadSuccessEvent = procedure (Sender: TObject;
                                                     const Url: AnsiString;
                                                     HTTPResponseHeader: TALHTTPResponseHeader;
                                                     HttpResponseContent: TStream;
                                                     Var StopCrawling: Boolean) of object;

  {------------------------------------------------------------------}
  TAlWebSpiderCrawlDownloadRedirectEvent = procedure (Sender: TObject;
                                                      const Url: AnsiString;
                                                      const RedirectedTo: AnsiString;
                                                      HTTPResponseHeader: TALHTTPResponseHeader;
                                                      Var StopCrawling: Boolean) of object;

  {---------------------------------------------------------------}
  TAlWebSpiderCrawlDownloadErrorEvent = procedure (Sender: TObject;
                                                   const URL: AnsiString;
                                                   const ErrorMessage: AnsiString;
                                                   HTTPResponseHeader: TALHTTPResponseHeader;
                                                   Var StopCrawling: Boolean) of object;

  {-------------------------------------------------------------}
  TAlWebSpiderCrawlGetNextLinkEvent = procedure (Sender: TObject;
                                                 Var Url: AnsiString) of object;

  {----------------------------------------------------------}
  TAlWebSpiderCrawlFindLinkEvent = Procedure (Sender: TObject;
                                              const HtmlTagString: AnsiString;
                                              HtmlTagParams: TALStrings;
                                              const URL: AnsiString) of object;

  {----------------------------------------------------------------}
  TAlWebSpiderCrawlEndEvent = Procedure (Sender: TObject) of object;

  {--------------------------------------------------------------------------------------------------}
  TAlWebSpiderCrawlBeforeDownloadEvent = Procedure (Sender: TObject; const Url: AnsiString) of object;

  {---------------------------------------------------------------}
  TAlWebSpiderCrawlAfterDownloadEvent = Procedure (Sender: TObject;
                                                   const Url: AnsiString;
                                                   HTTPResponseHeader: TALHTTPResponseHeader;
                                                   HttpResponseContent: TStream;
                                                   Var StopCrawling: Boolean) of object;

  {-----------------------------------------------------------------------------}
  TAlWebSpiderUpdateLinkToLocalPathGetNextFileEvent = procedure (Sender: TObject;
                                                                 Var FileName: AnsiString;
                                                                 Var BaseHref: AnsiString) of object;

  {--------------------------------------------------------------------------}
  TAlWebSpiderUpdateLinkToLocalPathFindLinkEvent = Procedure (Sender: TObject;
                                                              const HtmlTagString: AnsiString;
                                                              HtmlTagParams: TALStrings;
                                                              const URL: AnsiString;
                                                              Var LocalPath: AnsiString) of object;

  {--------------------------------------------------------------------------------}
  TAlWebSpiderUpdateLinkToLocalPathEndEvent = Procedure (Sender: TObject) of object;

  {---------------------------}
  TAlWebSpider = Class(TObject)
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
  Public
    Procedure Crawl; {Launch the Crawling of the page}
    Procedure UpdateLinkToLocalPath; {Update the link of downloaded page to local path}
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

  {-------------------------------------------------------------------------------------------------------------------------------------------------}
  TAlTrivialWebSpiderCrawlProgressEvent = Procedure (Sender: TObject; UrltoDownload, UrlDownloaded: Integer; const CurrentUrl: AnsiString) of object;

  {-------------------------------------------------------------------------------------------------------------------------}
  TAlTrivialWebSpiderUpdateLinkToLocalPathProgressEvent = Procedure (Sender: TObject; const aFileName: AnsiString) of object;

  {-----------------------------------------------------------------}
  TAlTrivialWebSpiderCrawlFindLinkEvent = Procedure (Sender: TObject;
                                                     const HtmlTagString: AnsiString;
                                                     HtmlTagParams: TALStrings;
                                                     const URL: AnsiString;
                                                     Var Ignore: Boolean) of object;

  {----------------------------------}
  TAlTrivialWebSpider = Class(Tobject)
  Private
    FWebSpider: TalWebSpider;
    fStartUrl: AnsiString;
    fLstUrlCrawled: TALStrings;
    fLstErrorEncountered: TALStrings;
    FPageDownloadedBinTree: TAlStringKeyAVLBinaryTree;
    FPageNotYetDownloadedBinTree: TAlStringKeyAVLBinaryTree;
    FCurrentDeepLevel: Integer;
    FCurrentLocalFileNameIndex: Integer;
    fMaxDeepLevel: Integer;
    fOnCrawlBeforeDownload: TAlWebSpiderCrawlBeforeDownloadEvent;
    fUpdateLinkToLocalPath: boolean;
    fExcludeMask: AnsiString;
    fStayInStartDomain: Boolean;
    fSaveDirectory: AnsiString;
    fSplitDirectoryAmount: integer;
    FHttpClient: TalHttpClient;
    fIncludeMask: AnsiString;
    fOnCrawlAfterDownload: TAlWebSpiderCrawlAfterDownloadEvent;
    fOnCrawlFindLink: TAlTrivialWebSpiderCrawlFindLinkEvent;
    fDownloadImage: Boolean;
    fOnUpdateLinkToLocalPathProgress: TAlTrivialWebSpiderUpdateLinkToLocalPathProgressEvent;
    fOnCrawlProgress: TAlTrivialWebSpiderCrawlProgressEvent;
    procedure WebSpiderCrawlDownloadError(Sender: TObject; const URL, ErrorMessage: AnsiString; HTTPResponseHeader: TALHTTPResponseHeader; var StopCrawling: Boolean);
    procedure WebSpiderCrawlDownloadRedirect(Sender: TObject; const Url, RedirectedTo: AnsiString; HTTPResponseHeader: TALHTTPResponseHeader; var StopCrawling: Boolean);
    procedure WebSpiderCrawlDownloadSuccess(Sender: TObject; const Url: AnsiString; HTTPResponseHeader: TALHTTPResponseHeader; HttpResponseContent: TStream; var StopCrawling: Boolean);
    procedure WebSpiderCrawlFindLink(Sender: TObject; const HtmlTagString: AnsiString; HtmlTagParams: TALStrings; const URL: AnsiString);
    procedure WebSpiderCrawlGetNextLink(Sender: TObject; var Url: AnsiString);
    procedure WebSpiderUpdateLinkToLocalPathFindLink(Sender: TObject; const HtmlTagString: AnsiString; HtmlTagParams: TALStrings; const URL: AnsiString; var LocalPath: AnsiString);
    procedure WebSpiderUpdateLinkToLocalPathGetNextFile(Sender: TObject; var FileName, BaseHref: AnsiString);
    function GetNextLocalFileName(const aContentType: AnsiString): AnsiString;
  Protected
  Public
    Constructor Create;
    Destructor Destroy; override;
    Procedure Crawl(const aUrl: AnsiString); overload; {Launch the Crawling of the page}
    procedure Crawl(const aUrl: AnsiString; LstUrlCrawled: TALStrings; LstErrorEncountered: TALStrings); overload;
    Property HttpClient: TalHttpClient Read FHttpClient write FHttpClient;
    Property DownloadImage: Boolean read fDownloadImage write fDownloadImage default false;
    Property StayInStartDomain: Boolean read fStayInStartDomain write fStayInStartDomain default true;
    Property UpdateLinkToLocalPath: boolean read fUpdateLinkToLocalPath write fUpdateLinkToLocalPath default True;
    Property MaxDeepLevel: Integer read fMaxDeepLevel write fMaxDeepLevel default -1;
    Property ExcludeMask: AnsiString read fExcludeMask write fExcludeMask;
    Property IncludeMask: AnsiString read fIncludeMask write fIncludeMask;
    Property SaveDirectory: AnsiString read fSaveDirectory write fSaveDirectory;
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
    Data: AnsiString;
  end;

  {----------------------------------------------------------------------------------------}
  TAlTrivialWebSpider_PageNotYetDownloadedBinTreeNode = Class(TALStringKeyAVLBinaryTreeNode)
  Private
  Protected
  Public
    DeepLevel: Integer;
  end;

implementation

Uses {$IF CompilerVersion >= 23} {Delphi XE2}
     Winapi.Windows,
     System.sysutils,
     Winapi.WinInet,
     Winapi.UrlMon,
     {$ELSE}
     Windows,
     sysutils,
     WinInet,
     UrlMon,
     {$IFEND}
     AlHTML,
     AlMime,
     ALString;

type

  {*****************************************}
  _TAlWebSpiderHandleTagfunctExtData = record
    WebSpiderObj: TAlWebSpider;
    CurrentBaseHref: AnsiString;
  end;

{************************************************************************}
Function _AlWebSpiderExtractUrlHandleTagfunct(const TagString: AnsiString;
                                              TagParams: TALStrings;
                                              ExtData: pointer;
                                              Var Handled: Boolean): AnsiString;

  {---------------------------------------------------------------}
  Procedure FindUrl(aUrl: ansiString; const aBaseHref: AnsiString);
  Begin
    {do not work with anchor in self document}
    If (aUrl <> '') and (AlPos('#',aUrl) <> 1) then begin

      {make url full path}
      aUrl := AlCombineUrl(aUrl, aBaseHref);

      {exit if it's not a http sheme}
      IF (AlExtractShemeFromUrl(aUrl) in [INTERNET_SCHEME_HTTP, INTERNET_SCHEME_HTTPS]) then

       {fire findlink Event}
        with _TAlWebSpiderHandleTagfunctExtData(ExtData^) do
          WebSpiderObj.FOnCrawlFindLink(WebSpiderObj,
                                        TagString,
                                        TagParams,
                                        aUrl);
    end;
  end;

Var Str: AnsiString;
    LowerTagString: AnsiString;
begin
  Handled := False;
  Result := '';
  ALCompactHtmlTagParams(TagParams);
  LowerTagString := AlLowerCase(TagString);

  with _TAlWebSpiderHandleTagfunctExtData(ExtData^) do begin

    If LowerTagString = 'a' then FindUrl(ALTrim(TagParams.Values['href']),CurrentBaseHref)
    else If LowerTagString = 'applet' then Begin
      str := ALTrim(TagParams.Values['codebase']); //The CODEBASE parameter specifies where the jar and cab files are located.
      {make str full path}
      If str <> '' then Str := AlCombineUrl(Str, CurrentBaseHref)
      else Str := CurrentBaseHref;
      FindUrl(ALTrim(TagParams.Values['code']), Str); //The URL specified by code might be relative to the codebase attribute.
      FindUrl(ALTrim(TagParams.Values['archive']), Str); //The URL specified by code might be relative to the codebase attribute.
    end
    else if LowerTagString = 'area' then FindUrl(ALTrim(TagParams.Values['href']),CurrentBaseHref)
    else if LowerTagString = 'bgsound' then FindUrl(ALTrim(TagParams.Values['src']),CurrentBaseHref)
    else if LowerTagString = 'blockquote' then FindUrl(ALTrim(TagParams.Values['cite']),CurrentBaseHref)
    else if LowerTagString = 'body' then FindUrl(ALTrim(TagParams.Values['background']),CurrentBaseHref)
    else if LowerTagString = 'del' then FindUrl(ALTrim(TagParams.Values['cite']),CurrentBaseHref)
    else if LowerTagString = 'embed' then FindUrl(ALTrim(TagParams.Values['src']),CurrentBaseHref)
    else if LowerTagString = 'frame' then begin
      FindUrl(ALTrim(TagParams.Values['longdesc']),CurrentBaseHref);
      FindUrl(ALTrim(TagParams.Values['src']),CurrentBaseHref);
    end
    else if LowerTagString = 'head' then FindUrl(ALTrim(TagParams.Values['profile']),CurrentBaseHref)
    else if LowerTagString = 'iframe' then begin
      FindUrl(ALTrim(TagParams.Values['longdesc']),CurrentBaseHref);
      FindUrl(ALTrim(TagParams.Values['src']),CurrentBaseHref);
    end
    else if LowerTagString = 'ilayer' then begin
      FindUrl(ALTrim(TagParams.Values['background']),CurrentBaseHref);
      FindUrl(ALTrim(TagParams.Values['src']),CurrentBaseHref);
    end
    else If LowerTagString = 'img' then Begin
      FindUrl(ALTrim(TagParams.Values['longdesc']),CurrentBaseHref);
      FindUrl(ALTrim(TagParams.Values['src']),CurrentBaseHref);
      FindUrl(ALTrim(TagParams.Values['usemap']),CurrentBaseHref);
      FindUrl(ALTrim(TagParams.Values['dynsrc']),CurrentBaseHref);
      FindUrl(ALTrim(TagParams.Values['lowsrc']),CurrentBaseHref);
    end
    else if LowerTagString = 'input' then begin
      FindUrl(ALTrim(TagParams.Values['src']),CurrentBaseHref);
      FindUrl(ALTrim(TagParams.Values['usemap']),CurrentBaseHref);
      FindUrl(ALTrim(TagParams.Values['dynsrc']),CurrentBaseHref);
      FindUrl(ALTrim(TagParams.Values['lowsrc']),CurrentBaseHref);
    end
    else if LowerTagString = 'ins' then FindUrl(ALTrim(TagParams.Values['cite']),CurrentBaseHref)
    else if LowerTagString = 'layer' then begin
      FindUrl(ALTrim(TagParams.Values['background']),CurrentBaseHref);
      FindUrl(ALTrim(TagParams.Values['src']),CurrentBaseHref);
    end
    else if LowerTagString = 'link' then FindUrl(ALTrim(TagParams.Values['href']),CurrentBaseHref)
    else If LowerTagString = 'object' then Begin
      str := ALTrim(TagParams.Values['codebase']); //The CODEBASE parameter specifies where the jar and cab files are located.
      {make str full path}
      If str <> '' then Str := AlCombineUrl(Str,CurrentBaseHref)
      else Str := CurrentBaseHref;
      FindUrl(ALTrim(TagParams.Values['classid']), Str); //The URL specified by code might be relative to the codebase attribute.
      FindUrl(ALTrim(TagParams.Values['data']), Str); //The URL specified by code might be relative to the codebase attribute.
      FindUrl(ALTrim(TagParams.Values['archive']), Str); //The URL specified by code might be relative to the codebase attribute.
      FindUrl(ALTrim(TagParams.Values['usemap']), CurrentBaseHref);
    end
    else if LowerTagString = 'q' then FindUrl(ALTrim(TagParams.Values['cite']),CurrentBaseHref)
    else if LowerTagString = 'script' then FindUrl(ALTrim(TagParams.Values['src']),CurrentBaseHref)
    else if LowerTagString = 'table' then FindUrl(ALTrim(TagParams.Values['background']),CurrentBaseHref)
    else if LowerTagString = 'td' then FindUrl(ALTrim(TagParams.Values['background']),CurrentBaseHref)
    else if LowerTagString = 'th' then FindUrl(ALTrim(TagParams.Values['background']),CurrentBaseHref)
    else if LowerTagString = 'xml' then FindUrl(ALTrim(TagParams.Values['src']),CurrentBaseHref)
    else if LowerTagString = 'base' then Begin
      Str := ALTrim(TagParams.Values['href']);
      If str <> '' then CurrentBaseHref := Str;
    end;

  end;
end;

{***********************************************************************************}
Function _AlWebSpiderUpdateLinkToLocalPathHandleTagfunct(const TagString: AnsiString;
                                                         TagParams: TALStrings;
                                                         ExtData: pointer;
                                                         Var Handled: Boolean): AnsiString;

  {---------------------------------------------------------}
  Procedure FindUrl(const aParamName, aBaseHref: AnsiString);
  Var aUrl: AnsiString;
      aLocalPathValue : AnsiString;
  Begin
    {extract Url}
    aUrl := ALTrim(TagParams.Values[aParamName]);

    {do not work with anchor in self document}
    If (aUrl <> '') and (AlPos('#',aUrl) <> 1) then begin

      {make url full path}
      aUrl := AlCombineUrl(aUrl, aBaseHref);

      {exit if it's not a http sheme}
      IF (AlExtractShemeFromUrl(aUrl) in [INTERNET_SCHEME_HTTP, INTERNET_SCHEME_HTTPS]) then begin

        {init local path value}
        aLocalPathValue := '';

        {fire findlink Event}
        with _TAlWebSpiderHandleTagfunctExtData(ExtData^) do
          WebSpiderObj.fOnUpdateLinkToLocalPathFindLink(WebSpiderObj,
                                                        TagString,
                                                        TagParams,
                                                        aUrl,
                                                        aLocalPathValue);

        {update tagParams}
        If (aLocalPathValue <> '') then begin
          Handled := True;
          TagParams.Values[aParamName] := aLocalPathValue; // 1234.htm#foo
        end;
      end;
    end;
  end;

Var Str: AnsiString;
    LowerTagString: AnsiString;
    i: Integer;
begin
  Handled := False;
  Result := '';
  ALCompactHtmlTagParams(TagParams);
  LowerTagString := AlLowerCase(TagString);

  with _TAlWebSpiderHandleTagfunctExtData(ExtData^) do begin

    If LowerTagString = 'a' then FindUrl('href',CurrentBaseHref)
    else If LowerTagString = 'applet' then Begin
      str := ALTrim(TagParams.Values['codebase']); //The CODEBASE parameter specifies where the jar and cab files are located.
      {make str full path}
      If str <> '' then Str := AlCombineUrl(Str,CurrentBaseHref)
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
      str := ALTrim(TagParams.Values['codebase']); //The CODEBASE parameter specifies where the jar and cab files are located.
      {make str full path}
      If str <> '' then Str := AlCombineUrl(Str, CurrentBaseHref)
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
        If TagParams.Names[i] <> '' then Result := Result + ' ' + TagParams.Names[i] + '="'+ alStringReplace(TagParams.ValueFromIndex[i],
                                                                                                             '"',
                                                                                                             '&#34;',
                                                                                                             [rfReplaceAll]) + '"'
        else Result := Result + ' ' + TagParams[i];
      Result := result + '>';
    end;
  end;
end;

{***************************}
procedure TAlWebSpider.Crawl;
Var currentUrl: AnsiString;
    StopCrawling: Boolean;
    UrlRedirect: Boolean;
    DownloadError: Boolean;
    CurrentHttpResponseHeader: TALHTTPResponseHeader;
    CurrentHttpResponseContent: TStream;
    aExtData: _TAlWebSpiderHandleTagfunctExtData;
    pMimeTypeFromData: LPWSTR;
    Str: AnsiString;
Begin

  Try

    StopCrawling := False;
    If not assigned(FOnCrawlGetNextLink) then exit;

    {start the main loop}
    While True do begin

      CurrentUrl := '';
      FOnCrawlGetNextLink(self, CurrentUrl); {Get the current url to process}
      CurrentUrl := ALTrim(CurrentUrl);
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
            FHttpClient.Get(CurrentURL,
                            CurrentHttpResponseContent,
                            CurrentHttpResponseHeader);
          Finally
            {the onAfterdownloadevent}
            if assigned(fOnCrawlAfterDownload) then fOnCrawlAfterDownload(Self,CurrentURL, CurrentHttpResponseHeader, CurrentHttpResponseContent, StopCrawling);
          End;

        except
          on E: Exception do begin

            {in case of url redirect}
            If Alpos('3',CurrentHttpResponseHeader.StatusCode)=1 then begin
              UrlRedirect := True;
              If assigned(FOnCrawlDownloadRedirect) then fOnCrawlDownloadRedirect(Self,
                                                                                  CurrentUrl,
                                                                                  AlCombineUrl(ALTrim(CurrentHttpResponseHeader.Location), CurrentUrl),
                                                                                  CurrentHttpResponseHeader,
                                                                                  StopCrawling);
            end

            {in case of any other error}
            else begin
              DownloadError := True;
              If assigned(FOnCrawlDownloadError) then fOnCrawlDownloadError(Self,
                                                                            CurrentUrl,
                                                                            AnsiString(E.Message),
                                                                            CurrentHttpResponseHeader,
                                                                            StopCrawling);
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
            CurrentHTTPResponseContent.ReadBuffer(pointer(Str)^,CurrentHTTPResponseContent.Size);

            {check the mime content type because some server send wrong mime content type}
            IF (FindMimeFromData(
                                 nil, // bind context - can be nil
                                 nil, // url - can be nil
                                 PAnsiChar(str), // buffer with data to sniff - can be nil (pwzUrl must be valid)
                                 length(str), // size of buffer
                                 PWidechar(WideString(CurrentHttpResponseHeader.ContentType)), // proposed mime if - can be nil
                                 0, // will be defined
                                 pMimeTypeFromData, // the suggested mime
                                 0 // must be 0
                                ) <> NOERROR) then pMimeTypeFromData := PWidechar(WideString(CurrentHttpResponseHeader.ContentType));

            {lanche the analyze of the page if content type = text/html}
            If ALSameText(AnsiString(pMimeTypeFromData),'text/html') and
               assigned(FOnCrawlFindLink) then begin

              {init the CurrentBaseHref of the aExtData object}
              aExtData.WebSpiderObj := self;
              aExtData.CurrentBaseHref := CurrentURL;

              {extract the list of url to download}
              ALHideHtmlUnwantedTagForHTMLHandleTagfunct(Str, False, #1);
              ALFastTagReplace(Str,
                               '<',
                               '>',
                               _AlWebSpiderExtractUrlHandleTagfunct,
                               true,
                               @aExtData,
                               [rfreplaceall]);
            end;

          end;

          {trigger the event OnCrawlDownloadSuccess}
          if assigned(FOnCrawlDownloadSuccess) then begin
            CurrentHTTPResponseContent.Position := 0;
            fOnCrawlDownloadSuccess(self,
                                    CurrentUrl,
                                    CurrentHttpResponseHeader,
                                    CurrentHttpResponseContent,
                                    StopCrawling);
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
Var currentFileName: AnsiString;
    CurrentBaseHref: AnsiString;
    aExtData: _TAlWebSpiderHandleTagfunctExtData;
    Str: AnsiString;
Begin

  Try

    If not assigned(FOnUpdateLinktoLocalPathGetNextFile) or not assigned(OnUpdateLinkToLocalPathFindLink) then exit;

    {start the main loop}
    While True do begin

      CurrentFileName := '';
      CurrentBaseHref := '';
      FOnUpdateLinktoLocalPathGetNextFile(self, CurrentFileName, CurrentBaseHref); {Get the current html file to process}
      CurrentFileName := ALTrim(CurrentFileName);
      CurrentBaseHref := ALTrim(CurrentBaseHref);
      If CurrentFileName = '' then Exit; {no more file to update}

      iF FileExists(String(CurrentFileName)) then begin
        {DOWNLOAD THE BODY}
        Str := AlGetStringFromFile(CurrentFileName);

        {init the CurrentBaseHref of the aExtData object}
        aExtData.WebSpiderObj := self;
        aExtData.CurrentBaseHref := CurrentBaseHref;

        {Update the link}
        ALHideHtmlUnwantedTagForHTMLHandleTagfunct(str, False, #1);
        str := ALFastTagReplace(str,
                                '<',
                                '>',
                                _AlWebSpiderUpdateLinkToLocalPathHandleTagfunct,
                                true,
                                @aExtData,
                                [rfreplaceall]);

        {restore the page to it's original format}
        str := AlStringReplace(str,
                               #1,
                               '<',
                               [rfReplaceAll]);

        {save the result string}
        AlSaveStringToFile(Str,CurrentFileName);
      end;
    end;

  finally
    If assigned(FOnUpdateLinkToLocalPathEnd) then FOnUpdateLinkToLocalPathEnd(self);
  end;
end;

{**********************************************************}
procedure TAlTrivialWebSpider.Crawl(const aUrl: AnsiString);
begin
  Crawl(aUrl, nil, nil);
end;

{**********************************************************************************************************************}
procedure TAlTrivialWebSpider.Crawl(const aUrl: AnsiString; LstUrlCrawled: TALStrings; LstErrorEncountered: TALStrings);
Var aNode: TAlTrivialWebSpider_PageNotYetDownloadedBinTreeNode;
Begin
  {check the SaveDirectory}
  if (SaveDirectory <> '') and not directoryExists(String(SaveDirectory)) then Raise Exception.CreateFmt('The directory: "%s" not exist!', [SaveDirectory]);
  if fHttpClient = nil then Raise Exception.Create('The HttpClient cannot be empty!');
  
  {init private var}
  fStartUrl := ALTrim(aUrl);
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
  FWebSpider := TalWebSpider.Create;
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

{********************************************************************************************}
function TAlTrivialWebSpider.GetNextLocalFileName(const aContentType: AnsiString): AnsiString;
Var aExt: AnsiString;

  {-----------------------------------------}
  Function SplitPathMakeFilename: AnsiString;
  begin
    Result := fSaveDirectory + ALIntToStr((FCurrentLocalFileNameIndex div fSplitDirectoryAmount) * fSplitDirectoryAmount + fSplitDirectoryAmount) + '\';
    If (not DirectoryExists(string(Result))) and (not createDir(string(Result))) then raise EALException.CreateFmt('cannot create dir: %s', [Result]);
    Result := Result + ALIntToStr(FCurrentLocalFileNameIndex) + aExt;
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
                                                          const URL, ErrorMessage: AnsiString;
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
                                                             const Url, RedirectedTo: AnsiString;
                                                             HTTPResponseHeader: TALHTTPResponseHeader;
                                                             var StopCrawling: Boolean);
Var aNode: TALStringKeyAVLBinaryTreeNode;
    aRedirectToWithoutAnchor: ansiString;
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
     (ALlowercase(AlExtractHostNameFromUrl(ALTrim(fStartUrl))) = ALlowercase(AlExtractHostNameFromUrl(RedirectedTo))) then begin

    {remove the anchor}
    aRedirectToWithoutAnchor := AlRemoveAnchorFromUrl(RedirectedTo);

    {add the redirectTo url to the not yet downloaded list}
    If FPageDownloadedBinTree.FindNode(aRedirectToWithoutAnchor) = nil then begin
      aNode:= TAlTrivialWebSpider_PageNotYetDownloadedBinTreeNode.Create;
      aNode.ID := aRedirectToWithoutAnchor;
      TAlTrivialWebSpider_PageNotYetDownloadedBinTreeNode(aNode).DeepLevel := FCurrentDeepLevel;
      If not FPageNotYetDownloadedBinTree.AddNode(aNode) then aNode.Free;
    end;

  end;

  {update label}
  if assigned(fOnCrawlProgress) then fOnCrawlProgress(self,FPageNotYetDownloadedBinTree.nodeCount,FPageDownloadedBinTree.nodeCount, Url);
end;


{**************************************************************************}
procedure TAlTrivialWebSpider.WebSpiderCrawlDownloadSuccess(Sender: TObject;
                                                            const Url: AnsiString;
                                                            HTTPResponseHeader: TALHTTPResponseHeader;
                                                            HttpResponseContent: TStream;
                                                            var StopCrawling: Boolean);
Var aNode: TAlTrivialWebSpider_PageDownloadedBinTreeNode;
    Str: AnsiString;
    AFileName: AnsiString;
    pMimeTypeFromData: LPWSTR;
begin

  {put the content in str}
  HttpResponseContent.Position := 0;
  SetLength(Str, HttpResponseContent.size);
  HttpResponseContent.ReadBuffer(pointer(Str)^,HttpResponseContent.Size);

  {we add a check here to be sure that the file is an http file (text file}
  {Some server send image with text/htm content type}
  IF (FindMimeFromData(
                       nil, // bind context - can be nil
                       nil, // url - can be nil
                       PAnsiChar(str), // buffer with data to sniff - can be nil (pwzUrl must be valid)
                       length(str), // size of buffer
                       PWidechar(WideString(HTTPResponseHeader.ContentType)), // proposed mime if - can be nil
                       0, // will be defined
                       pMimeTypeFromData, // the suggested mime
                       0 // must be 0
                      ) <> NOERROR) then pMimeTypeFromData := PWidechar(WideString(HTTPResponseHeader.ContentType));

  {Get the FileName where to save the responseContent}
  aFileName := GetNextLocalFileName(AnsiString(pMimeTypeFromData));

  {If html then add <!-- saved from '+ URL +' -->' at the top of the file}
  if aFileName <> '' then begin
    If ALSameText(AnsiString(pMimeTypeFromData),'text/html') then begin
      Str := '<!-- saved from '+ URL+' -->' +#13#10 + Str;
      AlSaveStringToFile(str,aFileName);
    end
    {Else Save the file without any change}
    else TmemoryStream(HttpResponseContent).SaveToFile(String(aFileName));
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
                                                     const HtmlTagString: AnsiString;
                                                     HtmlTagParams: TALStrings;
                                                     const URL: AnsiString);
Var aNode: TAlTrivialWebSpider_PageNotYetDownloadedBinTreeNode;
    aURLWithoutAnchor: ansiString;
    Lst: TALStringList;
    I: integer;
    Flag1 : Boolean;
    S1: AnsiString;
begin
  {If Check BoxDownload Image}
  IF not fDownloadImage and
     (
      ALSameText(HtmlTagString,'img') or
      (
       ALSameText(HtmlTagString,'input') and
       ALSameText(ALTrim(HtmlTagParams.Values['type']),'image')
      )
     )
    then Exit;

  {Stay in start site}
  If fStayInStartDomain and
     (ALlowercase(AlExtractHostNameFromUrl(ALTrim(fStartUrl))) <> ALlowercase(AlExtractHostNameFromUrl(Url))) then exit;

  {DeepLevel}
  If (fMaxDeepLevel >= 0) and (FCurrentDeepLevel + 1 > fMaxDeepLevel) then exit;

  {include link(s)}
  If fIncludeMask <> '' then begin
    Lst := TALStringList.Create;
    Try
      Lst.Text := ALTrim(AlStringReplace(FIncludeMask,';',#13#10,[RfReplaceall]));
      Flag1 := True;
      For i := 0 to Lst.Count - 1 do begin
        S1 := ALTrim(Lst[i]);
        If S1 <> '' then begin
          Flag1 := ALMatchesMask(URL, S1);
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
    Lst := TALStringList.Create;
    Try
      Lst.Text := ALTrim(AlStringReplace(fExcludeMask,';',#13#10,[RfReplaceall]));
      Flag1 := False;
      For i := 0 to Lst.Count - 1 do begin
        S1 := ALTrim(Lst[i]);
        If S1 <> '' then begin
          Flag1 := ALMatchesMask(URL, S1);
          If Flag1 then Break;
        end;
      end;
      If flag1 then Exit;
    Finally
      Lst.Free;
    end;
  end;

  {remove the anchor}
  aURLWithoutAnchor := AlRemoveAnchorFromUrl(URL);

  {call OnCrawlFindLink}
  Flag1 := False;
  if assigned(fOnCrawlFindLink) then fOnCrawlFindLink(Sender,
                                                      HtmlTagString,
                                                      HtmlTagParams,
                                                      aURLWithoutAnchor,
                                                      Flag1);
  if Flag1 then exit;

  {If the link not already downloaded then add it to the FPageNotYetDownloadedBinTree}
  If FPageDownloadedBinTree.FindNode(aURLWithoutAnchor) = nil then begin
    aNode:= TAlTrivialWebSpider_PageNotYetDownloadedBinTreeNode.Create;
    aNode.ID := aURLWithoutAnchor;
    aNode.DeepLevel := FCurrentDeepLevel + 1;
    If not FPageNotYetDownloadedBinTree.AddNode(aNode) then aNode.Free;
  end;
end;

{********************************************************************************************}
procedure TAlTrivialWebSpider.WebSpiderCrawlGetNextLink(Sender: TObject; var Url: AnsiString);

    {------------------------------------------------------------------------------------------------}
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
    If fMaxDeepLevel >= 0 then aNode := InternalfindNextUrlToDownload(TAlTrivialWebSpider_PageNotYetDownloadedBinTreeNode(FPageNotYetDownloadedBinTree.head), FCurrentDeepLevel)

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
                                                                     const HtmlTagString: AnsiString;
                                                                     HtmlTagParams: TALStrings;
                                                                     const URL: AnsiString;
                                                                     var LocalPath: AnsiString);
Var aNode: TALStringKeyAVLBinaryTreeNode;
    aTmpUrl: ansiString;
    aAnchorValue: AnsiString;
begin
  LocalPath := '';
  If Url <> '' then begin
    aTmpUrl := URL;
    While True Do begin
      aTmpUrl := AlRemoveAnchorFromUrl(aTmpUrl, aAnchorValue);
      aNode := FPageDownloadedBinTree.FindNode(aTmpUrl);
      If (aNode <> nil) then begin
        LocalPath := TAlTrivialWebSpider_PageDownloadedBinTreeNode(aNode).Data;
        If AlPos('=>',LocalPath) = 1 then Begin
          aTmpUrl := AlCopyStr(LocalPath,3,MaxInt);
          LocalPath := '';
        end
        else Break;
      end
      else Break;
    end;

    If LocalPath = '!' then localpath := ''
    else If LocalPath <> '' then begin
      LocalPath := AlStringReplace(LocalPath,
                                   '\',
                                   '/',
                                   [RfReplaceall]) + aAnchorValue;
      If (FCurrentLocalFileNameIndex >= 0) then LocalPath := '../' + LocalPath;
    end;
  end;
end;

{**************************************************************************************}
procedure TAlTrivialWebSpider.WebSpiderUpdateLinkToLocalPathGetNextFile(Sender: TObject;
                                                                        var FileName, BaseHref: AnsiString);
  {-----------------------------------------}
  Function SplitPathMakeFilename: AnsiString;
  begin
    If FCurrentLocalFileNameIndex < 0 then result := ''
    else If FCurrentLocalFileNameIndex = 0 then result := fSaveDirectory + 'Start.htm'
    else Result := fSaveDirectory + ALIntToStr((FCurrentLocalFileNameIndex div SplitDirectoryAmount) * SplitDirectoryAmount + SplitDirectoryAmount) + '\' + ALIntToStr(FCurrentLocalFileNameIndex) + '.htm';
    dec(FCurrentLocalFileNameIndex);
  end;

Begin
  if fSaveDirectory = '' then FileName := ''
  else begin

    {Find FileName}
    FileName := SplitPathMakeFilename;
    While (FileName <> '') and not fileExists(string(FileName)) do
      Filename := SplitPathMakeFilename;

  end;

  {if filename found}
  If FileName <> '' then Begin

    {Extract the Base Href}
    BaseHref := AlGetStringFromFile(FileName);
    BaseHref := ALTrim(AlCopyStr(BaseHref,
                               17,                         // '<!-- saved from ' + URL
                               AlPos(#13,BaseHref) - 21)); // URL + ' -->' +#13#10

    {update label}
    if assigned(fOnUpdateLinkToLocalPathProgress) then fOnUpdateLinkToLocalPathProgress(self, FileName);

  end
  else BaseHref := '';

end;

end.
