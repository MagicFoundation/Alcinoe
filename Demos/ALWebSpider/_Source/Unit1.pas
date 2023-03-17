unit Unit1;

interface

{$WARN UNIT_PLATFORM OFF}

uses
  Windows,
  SysUtils,
  dialogs,
  Forms,
  StdCtrls,
  ComCtrls,
  Controls,
  Classes,
  Alcinoe.StringList,
  Alcinoe.HTTP.Client,
  Alcinoe.HTTP.Client.WinHTTP,
  Alcinoe.WebSpider,
  Alcinoe.AVLBinaryTree,
  ExtCtrls,
  Shellapi;

type

  {-------------------}
  TForm1 = class(TForm)
    StatusBar1: TStatusBar;
    Label1: TLabel;
    editURL2Crawl: TEdit;
    ButtonStart: TButton;
    EditSaveDirectory: TEdit;
    Label2: TLabel;
    EditMaxDeepLevel: TEdit;
    UpDownMaxDeepLevel: TUpDown;
    Label3: TLabel;
    CheckBoxDownloadImage: TCheckBox;
    CheckBoxUpdateHref: TCheckBox;
    CheckBoxStayInStartSite: TCheckBox;
    StatusBar2: TStatusBar;
    BtnChooseSaveDirectory: TButton;
    MemoErrorMsg: TMemo;
    ButtonStop: TButton;
    EditIncludeLink: TEdit;
    Label4: TLabel;
    EditExcludeLink: TEdit;
    Label6: TLabel;
    procedure ButtonStartClick(Sender: TObject);
    procedure BtnChooseSaveDirectoryClick(Sender: TObject);
    procedure MainWebSpiderCrawlDownloadError(Sender: TObject; const URL, ErrorMessage: {$IFDEF UNICODE}AnsiString{$ELSE}String{$ENDIF}; HTTPResponseHeader: TALHTTPResponseHeader; var StopCrawling: Boolean);
    procedure MainWebSpiderCrawlDownloadRedirect(Sender: TObject; const Url, RedirectedTo: {$IFDEF UNICODE}AnsiString{$ELSE}String{$ENDIF}; HTTPResponseHeader: TALHTTPResponseHeader; var StopCrawling: Boolean);
    procedure MainWebSpiderCrawlDownloadSuccess(Sender: TObject; const Url: {$IFDEF UNICODE}AnsiString{$ELSE}String{$ENDIF}; HTTPResponseHeader: TALHTTPResponseHeader; HttpResponseContent: TStream; var StopCrawling: Boolean);
    procedure MainWebSpiderCrawlFindLink(Sender: TObject; const HtmlTagString: {$IFDEF UNICODE}AnsiString{$ELSE}String{$ENDIF}; HtmlTagParams: TALStringsA; const URL: {$IFDEF UNICODE}AnsiString{$ELSE}String{$ENDIF});
    procedure MainWebSpiderCrawlGetNextLink(Sender: TObject; var Url: {$IFDEF UNICODE}AnsiString{$ELSE}String{$ENDIF});
    procedure MainWebSpiderUpdateLinkToLocalPathFindLink(Sender: TObject; const HtmlTagString: {$IFDEF UNICODE}AnsiString{$ELSE}String{$ENDIF}; HtmlTagParams: TALStringsA; const URL: {$IFDEF UNICODE}AnsiString{$ELSE}String{$ENDIF}; var LocalPath: {$IFDEF UNICODE}AnsiString{$ELSE}String{$ENDIF});
    procedure MainWebSpiderUpdateLinkToLocalPathGetNextFile(Sender: TObject; var FileName, BaseHref: {$IFDEF UNICODE}AnsiString{$ELSE}String{$ENDIF});
    procedure ButtonStopClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    FPageDownloadedBinTree: TAlStringKeyAVLBinaryTree;
    FPageNotYetDownloadedBinTree: TAlStringKeyAVLBinaryTree;
    FCurrentDeepLevel: Integer;
    FCurrentLocalFileNameIndex: integer;
    fMainWebSpider: TAlWebSpider;
    fMainHttpClient: TALWinHttpClient;
    function GetNextLocalFileName(const aContentType: AnsiString): AnsiString;
  public
  end;

  {---------------------------------------------------------------}
  TPageDownloadedBinTreeNode = Class(TALStringKeyAVLBinaryTreeNode)
  Private
  Protected
  Public
    Data: AnsiString;
  end;

  {---------------------------------------------------------------------}
  TPageNotYetDownloadedBinTreeNode = Class(TALStringKeyAVLBinaryTreeNode)
  Private
  Protected
  Public
    DeepLevel: Integer;
  end;

var Form1: TForm1;

implementation

{$R *.dfm}

uses
  System.AnsiStrings,
  FileCtrl,
  Masks,
  UrlMon,
  Alcinoe.Files,
  Alcinoe.Mime,
  Alcinoe.StringUtils;

Const SplitDirectoryAmount = 5000;


{*************************************************}
procedure TForm1.ButtonStartClick(Sender: TObject);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure InternalEnableControl;
  Begin
    ButtonStop.Enabled := False;
    buttonStart.Enabled := True;
    editURL2Crawl.Enabled := True;
    EditIncludeLink.Enabled := True;
    EditExcludeLink.Enabled := True;
    EditSaveDirectory.Enabled := True;
    EditMaxDeepLevel.Enabled := True;
    UpDownMaxDeepLevel.Enabled := True;
    CheckBoxDownloadImage.Enabled := True;
    CheckBoxUpdateHref.Enabled := True;
    CheckBoxStayInStartSite.Enabled := True;
    BtnChooseSaveDirectory.Enabled := True;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure InternalDisableControl;
  Begin
    ButtonStop.Enabled := True;
    buttonStart.Enabled := False;
    editURL2Crawl.Enabled := False;
    EditIncludeLink.Enabled := False;
    EditExcludeLink.Enabled := False;
    EditSaveDirectory.Enabled := False;
    EditMaxDeepLevel.Enabled := False;
    UpDownMaxDeepLevel.Enabled := False;
    CheckBoxDownloadImage.Enabled := False;
    CheckBoxUpdateHref.Enabled := False;
    CheckBoxStayInStartSite.Enabled := False;
    BtnChooseSaveDirectory.Enabled := False;
  end;

Var aNode: TPageNotYetDownloadedBinTreeNode;
Begin
  {check the save directory}
  If ALTrim(AnsiString(EditSaveDirectory.Text)) = '' then
    raise Exception.Create('Please select a directory to save the downloaded files!');

  {Handle if we ask to start or stop}
  InternalDisableControl;
  Try
    {clear the label control}
    StatusBar1.Panels[0].Text := '';
    StatusBar1.Panels[1].Text := '';
    StatusBar2.Panels[0].Text := '';
    MemoErrorMsg.Lines.Clear;

    {init private var}
    FCurrentDeepLevel := 0;
    FCurrentLocalFileNameIndex := 0;
    FPageDownloadedBinTree:= TAlStringKeyAVLBinaryTree.Create;
    FPageNotYetDownloadedBinTree:= TAlStringKeyAVLBinaryTree.Create;
    Try

      {add editURL2Crawl.text to the fPageNotYetDownloadedBinTree}
      aNode:= TPageNotYetDownloadedBinTreeNode.Create;
      aNode.ID := ALTrim(AnsiString(editURL2Crawl.text));
      aNode.DeepLevel := 0;
      FPageNotYetDownloadedBinTree.AddNode(aNode);

      {start the crawl}
      fMainWebSpider.Crawl;

      {exit if the user click on the stop button}
      If not ButtonStop.Enabled then exit;

      {update the link on downloaded page to local path}
      if CheckBoxUpdateHref.Checked then fMainWebSpider.UpdateLinkToLocalPath;

    finally
      FPageDownloadedBinTree.Free;
      FPageNotYetDownloadedBinTree.Free;
    end;

  finally
    InternalEnableControl;
    StatusBar2.Panels[0].Text := '';
  end;
end;

{************************************************}
procedure TForm1.ButtonStopClick(Sender: TObject);
begin
  ButtonStop.Enabled := False;
end;

{************************************************************}
procedure TForm1.BtnChooseSaveDirectoryClick(Sender: TObject);
Var aDir: String;
Begin
 aDir := '';
 if SelectDirectory(aDir, [sdAllowCreate, sdPerformCreate, sdPrompt],0) then  EditSaveDirectory.Text := IncludeTrailingPathDelimiter(aDir);
end;

{*******************************************************************************}
function TForm1.GetNextLocalFileName(const aContentType: AnsiString): AnsiString;
Var aExt: AnsiString;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Function SplitPathMakeFilename: AnsiString;
  begin
    Result := AnsiString(EditSaveDirectory.Text) + ALIntToStrA((FCurrentLocalFileNameIndex div SplitDirectoryAmount) * SplitDirectoryAmount + SplitDirectoryAmount) + '\';
    If (not SysUtils.DirectoryExists(String(Result))) and (not createDir(String(Result))) then raise exception.Create('cannot create dir: ' + String(Result));
    Result := Result + ALIntToStrA(FCurrentLocalFileNameIndex) + aExt;
    inc(FCurrentLocalFileNameIndex);
  end;

Begin
  aExt := ALlowercase(ALGetDefaultFileExtFromMimeContentType(aContentType)); // '.htm'

  If FCurrentLocalFileNameIndex = 0 then Begin
    result := AnsiString(EditSaveDirectory.Text) + 'Start' + aExt;
    inc(FCurrentLocalFileNameIndex);
  end
  else result := SplitPathMakeFilename;
end;

{***********************************************}
procedure TForm1.MainWebSpiderCrawlDownloadError(
            Sender: TObject;
            const URL, ErrorMessage: AnsiString;
            HTTPResponseHeader: TALHTTPResponseHeader;
            var StopCrawling: Boolean);
Var aNode: TPageDownloadedBinTreeNode;
begin
  {add the url to downloaded list}
  aNode:= TPageDownloadedBinTreeNode.Create;
  aNode.ID := Url;
  aNode.data := '!';
  If not FPageDownloadedBinTree.AddNode(aNode) then aNode.Free;

  {delete the url from the not yet downloaded list}
  FPageNotYetDownloadedBinTree.DeleteNode(url);

  {update label}
  MemoErrorMsg.Lines.Add('Error: ' + String(ErrorMessage));
  StatusBar1.Panels[0].Text := 'Url Downloaded: ' + IntToStr((FPageDownloadedBinTree.nodeCount));
  StatusBar1.Panels[1].Text := 'Url to Download: ' + IntToStr((FPageNotYetDownloadedBinTree.nodeCount));
  application.ProcessMessages;
  StopCrawling := not ButtonStop.Enabled;
end;

{**************************************************}
procedure TForm1.MainWebSpiderCrawlDownloadRedirect(
            Sender: TObject;
            const Url, RedirectedTo: AnsiString;
            HTTPResponseHeader: TALHTTPResponseHeader;
            var StopCrawling: Boolean);
Var aNode: TALStringKeyAVLBinaryTreeNode;
    RedirectedToWithoutAnchor: ansiString;
begin
  {add the url to downloaded list}
  aNode:= TPageDownloadedBinTreeNode.Create;
  aNode.ID := Url;
  TPageDownloadedBinTreeNode(aNode).data := '=>'+RedirectedTo;
  If not FPageDownloadedBinTree.AddNode(aNode) then aNode.Free;

  {delete the url from the not yet downloaded list}
  FPageNotYetDownloadedBinTree.DeleteNode(url);

  {Stay in start site}
  If not CheckBoxStayInStartSite.Checked or
     (ALlowercase(AlExtractHostNameFromUrl(ALTrim(AnsiString(editURL2Crawl.Text)))) = ALlowercase(AlExtractHostNameFromUrl(RedirectedTo))) then begin

    {remove the anchor}
    RedirectedToWithoutAnchor := AlRemoveAnchorFromUrl(RedirectedTo);

    {add the redirectTo url to the not yet downloaded list}
    If FPageDownloadedBinTree.FindNode(RedirectedToWithoutAnchor) = nil then begin
      aNode:= TPageNotYetDownloadedBinTreeNode.Create;
      aNode.ID := RedirectedToWithoutAnchor;
      TPageNotYetDownloadedBinTreeNode(aNode).DeepLevel := FCurrentDeepLevel;
      If not FPageNotYetDownloadedBinTree.AddNode(aNode) then aNode.Free;
    end;

  end;

  {update label}
  StatusBar1.Panels[0].Text := 'Url Downloaded: ' + IntToStr((FPageDownloadedBinTree.nodeCount));
  StatusBar1.Panels[1].Text := 'Url to Download: ' + IntToStr((FPageNotYetDownloadedBinTree.nodeCount));
  application.ProcessMessages;
  StopCrawling := not ButtonStop.Enabled;
end;

{*************************************************}
procedure TForm1.MainWebSpiderCrawlDownloadSuccess(
            Sender: TObject;
            const Url: AnsiString;
            HTTPResponseHeader: TALHTTPResponseHeader;
            HttpResponseContent: TStream;
            var StopCrawling: Boolean);
Var aNode: TPageDownloadedBinTreeNode;
    Str: AnsiString;
    AFileName: AnsiString;
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
  If ALSameTextA(AnsiString(pMimeTypeFromData),'text/html') then begin
    Str := '<!-- saved from '+ URL+' -->' +#13#10 + Str;
    AlSaveStringToFile(str,aFileName);
  end
  {Else Save the file without any change}
  else TmemoryStream(HttpResponseContent).SaveToFile(String(aFileName));

  {delete the Url from the PageNotYetDownloadedBinTree}
  FPageNotYetDownloadedBinTree.DeleteNode(Url);

  {add the url to the PageDownloadedBinTree}
  aNode:= TPageDownloadedBinTreeNode.Create;
  aNode.ID := Url;
  aNode.data := AlCopyStr(AFileName,length(EditSaveDirectory.Text) + 1,maxint);
  If not FPageDownloadedBinTree.AddNode(aNode) then aNode.Free;

  {update label}
  StatusBar1.Panels[0].Text := 'Url Downloaded: ' + IntToStr((FPageDownloadedBinTree.nodeCount));
  StatusBar1.Panels[1].Text := 'Url to Download: ' + IntToStr((FPageNotYetDownloadedBinTree.nodeCount));
  application.ProcessMessages;
  StopCrawling := not ButtonStop.Enabled;
end;

{******************************************}
procedure TForm1.MainWebSpiderCrawlFindLink(
            Sender: TObject;
            const HtmlTagString: AnsiString;
            HtmlTagParams: TALStringsA;
            const URL: AnsiString);
Var aNode: TPageNotYetDownloadedBinTreeNode;
    aURLWithoutAnchor: AnsiString;
    Lst: TALStringListA;
    I: integer;
    Flag1 : Boolean;
    S1: AnsiString;
begin
  {If Check BoxDownload Image}
  IF not CheckBoxDownloadImage.Checked and
     (
      ALSameTextA(HtmlTagString,'img') or
      (
       ALSameTextA(HtmlTagString,'input') and
       ALSameTextA(ALTrim(HtmlTagParams.Values['type']),'image')
      )
     )
    then Exit;

  {Stay in start site}
  If CheckBoxStayInStartSite.Checked and
     (ALlowercase(AlExtractHostNameFromUrl(ALTrim(AnsiString(editURL2Crawl.Text)))) <> ALlowercase(AlExtractHostNameFromUrl(Url))) then exit;

  {DeepLevel}
  If (UpDownMaxDeepLevel.Position >= 0) and (FCurrentDeepLevel + 1 > UpDownMaxDeepLevel.Position) then exit;

  {include link(s)}
  If EditIncludeLink.Text <> '' then begin
    Lst := TALStringListA.Create;
    Try
      Lst.Text := ALTrim(ALStringReplaceA(AnsiString(EditIncludeLink.Text),';',#13#10,[RfReplaceall]));
      Flag1 := True;
      For i := 0 to Lst.Count - 1 do begin
        S1 := ALTrim(Lst[i]);
        If S1 <> '' then begin
          Flag1 := ALMatchesMaskA(URL, S1);
          If Flag1 then Break;
        end;
      end;
      If not flag1 then Exit;
    Finally
      Lst.Free;
    end;
  end;

  {Exclude link(s)}
  If EditExcludeLink.Text <> '' then begin
    Lst := TALStringListA.Create;
    Try
      Lst.Text := ALTrim(ALStringReplaceA(AnsiString(EditExcludeLink.Text),';',#13#10,[RfReplaceall]));
      Flag1 := False;
      For i := 0 to Lst.Count - 1 do begin
        S1 := ALTrim(Lst[i]);
        If S1 <> '' then begin
          Flag1 := ALMatchesMaskA(URL, S1);
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

  {If the link not already downloaded then add it to the FPageNotYetDownloadedBinTree}
  If FPageDownloadedBinTree.FindNode(aURLWithoutAnchor) = nil then begin
    aNode:= TPageNotYetDownloadedBinTreeNode.Create;
    aNode.ID := aURLWithoutAnchor;
    aNode.DeepLevel := FCurrentDeepLevel + 1;
    If not FPageNotYetDownloadedBinTree.AddNode(aNode) then aNode.Free
    else begin
      StatusBar1.Panels[1].Text := 'Url to Download: ' + IntToStr((FPageNotYetDownloadedBinTree.nodeCount));
      application.ProcessMessages;
    end;
  end;
end;

{*********************************************}
procedure TForm1.MainWebSpiderCrawlGetNextLink(
            Sender: TObject;
            var Url: AnsiString);

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    function InternalfindNextUrlToDownload(
               aNode: TPageNotYetDownloadedBinTreeNode;
               alowDeepLevel: Integer): TPageNotYetDownloadedBinTreeNode;
    Var aTmpNode1, aTmpNode2: TPageNotYetDownloadedBinTreeNode;
    Begin
      If (not assigned(Anode)) or (aNode.DeepLevel <= alowDeepLevel) then result := aNode
      else begin

        if aNode.ChildNodes[true] <> nil then begin
          aTmpNode1 := InternalfindNextUrlToDownload(TPageNotYetDownloadedBinTreeNode(aNode.ChildNodes[true]), alowDeepLevel);
          If (assigned(aTmpNode1)) and (aTmpNode1.DeepLevel <= alowDeepLevel) then begin
            result := aTmpNode1;
            exit;
          end;
        end
        else aTmpNode1 := nil;

        if aNode.ChildNodes[false] <> nil then begin
          aTmpNode2 := InternalfindNextUrlToDownload(TPageNotYetDownloadedBinTreeNode(aNode.ChildNodes[false]), alowDeepLevel);
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

Var aNode: TPageNotYetDownloadedBinTreeNode;
begin
  {If theire is more url to download}
  IF FPageNotYetDownloadedBinTree.NodeCount > 0 then begin

    {Find next url with deeplevel closer to FCurrentDeepLevel}
    If UpDownMaxDeepLevel.Position >= 0 then aNode := InternalfindNextUrlToDownload(
                                                         TPageNotYetDownloadedBinTreeNode(FPageNotYetDownloadedBinTree.head),
                                                         FCurrentDeepLevel
                                                        )

    {Find next url without take care of FCurrentDeepLevel}
    else aNode := TPageNotYetDownloadedBinTreeNode(FPageNotYetDownloadedBinTree.head);

    Url := aNode.ID;
    FCurrentDeepLevel := TPageNotYetDownloadedBinTreeNode(aNode).DeepLevel;
  end

  {If their is no more url to download then exit}
  else begin
    Url := '';
    FCurrentDeepLevel := -1;
  end;

  StatusBar2.Panels[0].Text := 'Downloading Url: ' + String(Url);
  Application.ProcessMessages;
  if not ButtonStop.Enabled then url := '';
end;

{**********************************************************}
procedure TForm1.MainWebSpiderUpdateLinkToLocalPathFindLink(
            Sender: TObject;
            const HtmlTagString: AnsiString;
            HtmlTagParams: TALStringsA;
            const URL: AnsiString;
            var LocalPath: AnsiString);
Var aNode: TALStringKeyAVLBinaryTreeNode;
    aTmpUrl: ansiString;
    aAnchorValue: AnsiString;
begin
  LocalPath := '';
  If Url <> '' then begin
    aTmpUrl := Url;
    While True Do begin
      aTmpUrl := AlRemoveAnchorFromUrl(aTmpUrl, aAnchorValue);
      aNode := FPageDownloadedBinTree.FindNode(aTmpUrl);
      If (aNode <> nil) then begin
        LocalPath := TPageDownloadedBinTreeNode(aNode).Data;
        If ALPosA('=>',LocalPath) = 1 then Begin
          aTmpUrl := AlCopyStr(LocalPath,3,MaxInt);
          LocalPath := '';
        end
        else Break;
      end
      else Break;
    end;

    If LocalPath = '!' then localpath := ''
    else If LocalPath <> '' then begin
      LocalPath := ALStringReplaceA(
                      LocalPath,
                      '\',
                      '/',
                      [RfReplaceall]
                     ) + aAnchorValue;
      If (FCurrentLocalFileNameIndex >= 0) then LocalPath := '../' + LocalPath;
    end;
  end;
end;

{*************************************************************}
procedure TForm1.MainWebSpiderUpdateLinkToLocalPathGetNextFile(
            Sender: TObject;
            var FileName, BaseHref: AnsiString);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Function SplitPathMakeFilename: AnsiString;
  begin
    If FCurrentLocalFileNameIndex < 0 then result := ''
    else If FCurrentLocalFileNameIndex = 0 then result := AnsiString(EditSaveDirectory.Text + 'Start.htm')
    else Result := AnsiString(EditSaveDirectory.Text + IntToStr((FCurrentLocalFileNameIndex div SplitDirectoryAmount) * SplitDirectoryAmount + SplitDirectoryAmount) + '\' + IntToStr(FCurrentLocalFileNameIndex) + '.htm');
    dec(FCurrentLocalFileNameIndex);
  end;

Begin
  {Find FileName}
  FileName := SplitPathMakeFilename;
  While (FileName <> '') and not fileExists(String(FileName)) do
    Filename := SplitPathMakeFilename;

  {if filename found}
  If FileName <> '' then Begin

    {Extract the Base Href}
    BaseHref := AlGetStringFromFile(FileName);
    BaseHref := ALTrim(
                   AlCopyStr(
                      BaseHref,
                      17,                        // '<!-- saved from ' + URL
                      ALPosA(#13,BaseHref) - 21    // URL + ' -->' +#13#10
                     )
                  );

  end
  else BaseHref := '';

  {update label}
  StatusBar2.Panels[0].Text := 'Update Href to Local path for file: ' + String(FileName);
  application.ProcessMessages;
  if not ButtonStop.Enabled then begin
    FileName := '';
    BaseHref := '';
  end
end;

{*******************************************}
procedure TForm1.FormCreate(Sender: TObject);
begin
  fMainHttpClient := TALWinHttpClient.Create;
  fMainHttpClient.InternetOptions := [wHttpIo_REFRESH, wHttpIo_Keep_connection];

  fMainWebSpider := TAlWebSpider.Create;
  fMainWebSpider.OnCrawlDownloadSuccess := MainWebSpiderCrawlDownloadSuccess;
  fMainWebSpider.OnCrawlDownloadRedirect := MainWebSpiderCrawlDownloadRedirect;
  fMainWebSpider.OnCrawlDownloadError := MainWebSpiderCrawlDownloadError;
  fMainWebSpider.OnCrawlGetNextLink := MainWebSpiderCrawlGetNextLink;
  fMainWebSpider.OnCrawlFindLink := MainWebSpiderCrawlFindLink;
  fMainWebSpider.OnUpdateLinkToLocalPathGetNextFile := MainWebSpiderUpdateLinkToLocalPathGetNextFile;
  fMainWebSpider.OnUpdateLinkToLocalPathFindLink := MainWebSpiderUpdateLinkToLocalPathFindLink;
  fMainWebSpider.HttpClient := fMainHttpClient;
end;

{********************************************************************}
procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  fMainHttpClient.Free;
  fMainWebSpider.Free;
end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.
