unit Unit1;

interface

{$WARN UNIT_PLATFORM OFF}

uses Windows,
     SysUtils,
     dialogs,
     Forms,
     StdCtrls,
     ComCtrls,
     Controls,
     Classes,
     ALHttpClient,
     ALWinHttpClient,
     ALWebSpider,
     AlAVLBinaryTree,
     AlHttpCommon,
     ExtCtrls;

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
    MainWebSpider: TAlWebSpider;
    MainHttpClient: TALWinHttpClient;
    ButtonStop: TButton;
    Panel1: TPanel;
    Label5: TLabel;
    EditIncludeLink: TEdit;
    Label4: TLabel;
    EditExcludeLink: TEdit;
    Label6: TLabel;
    procedure ButtonStartClick(Sender: TObject);
    procedure BtnChooseSaveDirectoryClick(Sender: TObject);
    procedure MainWebSpiderCrawlDownloadError(Sender: TObject; URL, ErrorMessage: String; HTTPResponseHeader: TALHTTPResponseHeader; var StopCrawling: Boolean);
    procedure MainWebSpiderCrawlDownloadRedirect(Sender: TObject; Url, RedirectedTo: String; HTTPResponseHeader: TALHTTPResponseHeader; var StopCrawling: Boolean);
    procedure MainWebSpiderCrawlDownloadSuccess(Sender: TObject; Url: String; HTTPResponseHeader: TALHTTPResponseHeader; HttpResponseContent: TStream; var StopCrawling: Boolean);
    procedure MainWebSpiderCrawlFindLink(Sender: TObject; HtmlTagString: String; HtmlTagParams: TStrings; URL: String);
    procedure MainWebSpiderCrawlGetNextLink(Sender: TObject; var Url: String);
    procedure MainWebSpiderUpdateLinkToLocalPathFindLink(Sender: TObject; HtmlTagString: String; HtmlTagParams: TStrings; URL: String; var LocalPath: String);
    procedure MainWebSpiderUpdateLinkToLocalPathGetNextFile(Sender: TObject; var FileName, BaseHref: String);
    procedure ButtonStopClick(Sender: TObject);
  private
    FPageDownloadedBinTree: TAlStringKeyAVLBinaryTree;
    FPageNotYetDownloadedBinTree: TAlStringKeyAVLBinaryTree;
    FCurrentDeepLevel: Integer;
    FCurrentLocalFileNameIndex: integer;
    function GetNextLocalFileName(aContentType: String): String;
  public
  end;

  {---------------------------------------------------------------}
  TPageDownloadedBinTreeNode = Class(TALStringKeyAVLBinaryTreeNode)
  Private
  Protected
  Public
    Data: String;
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

uses FileCtrl,
     Masks,
     UrlMon,
     AlFcnFile,
     AlFcnString,
     AlFcnMime;

Const SplitDirectoryAmount = 5000;




{*************************************************}
procedure TForm1.ButtonStartClick(Sender: TObject);

  {------------------------------}
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

  {-------------------------------}
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
  If trim(EditSaveDirectory.Text) = '' then
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
      aNode.ID := trim(editURL2Crawl.text);
      aNode.DeepLevel := 0;
      FPageNotYetDownloadedBinTree.AddNode(aNode);

      {start the crawl}
      MainWebSpider.Crawl;

      {exit if the user click on the stop button}
      If not ButtonStop.Enabled then exit;

      {update the link on downloaded page to local path}
      if CheckBoxUpdateHref.Checked then MainWebSpider.UpdateLinkToLocalPath;

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
Var aDir: string;
Begin
 aDir := '';
 if SelectDirectory(aDir, [sdAllowCreate, sdPerformCreate, sdPrompt],0) then  EditSaveDirectory.Text := ALMakeGoodEndPath(aDir);
end;

{*****************************************************************}
function TForm1.GetNextLocalFileName(aContentType: String): String;
Var aExt: String;

  {-------------------------------------}
  Function SplitPathMakeFilename: String;
  begin
    Result := EditSaveDirectory.Text + inttostr((FCurrentLocalFileNameIndex div SplitDirectoryAmount) * SplitDirectoryAmount + SplitDirectoryAmount) + '\';
    If (not DirectoryExists(Result)) and (not createDir(Result)) then raise exception.Create('cannot create dir: ' + Result);
    Result := Result + inttostr(FCurrentLocalFileNameIndex) + aExt;
    inc(FCurrentLocalFileNameIndex);
  end;

Begin
  aExt := ALlowercase(ALGetDefaultFileExtFromMimeContentType(aContentType)); // '.htm'

  If FCurrentLocalFileNameIndex = 0 then Begin
    result := EditSaveDirectory.Text + 'Start' + aExt;
    inc(FCurrentLocalFileNameIndex);
  end
  else result := SplitPathMakeFilename;
end;

{***************************************************************}
procedure TForm1.MainWebSpiderCrawlDownloadError(Sender: TObject;
                                                 URL, ErrorMessage: String;
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
  MemoErrorMsg.Lines.Add('Error: ' + ErrorMessage);
  StatusBar1.Panels[0].Text := 'Url Downloaded: ' + inttostr((FPageDownloadedBinTree.nodeCount));
  StatusBar1.Panels[1].Text := 'Url to Download: ' + inttostr((FPageNotYetDownloadedBinTree.nodeCount));
  application.ProcessMessages;
  StopCrawling := not ButtonStop.Enabled;
end;

{******************************************************************}
procedure TForm1.MainWebSpiderCrawlDownloadRedirect(Sender: TObject;
                                                    Url, RedirectedTo: String;
                                                    HTTPResponseHeader: TALHTTPResponseHeader;
                                                    var StopCrawling: Boolean);
Var aNode: TALStringKeyAVLBinaryTreeNode;
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
     (ALlowercase(AlExtractHostNameFromUrl(trim(editURL2Crawl.Text))) = ALlowercase(AlExtractHostNameFromUrl(RedirectedTo))) then begin

    {remove the anchor}
    RedirectedTo := AlRemoveAnchorFromUrl(RedirectedTo);

    {add the redirectTo url to the not yet downloaded list}
    If FPageDownloadedBinTree.FindNode(RedirectedTo) = nil then begin
      aNode:= TPageNotYetDownloadedBinTreeNode.Create;
      aNode.ID := RedirectedTo;
      TPageNotYetDownloadedBinTreeNode(aNode).DeepLevel := FCurrentDeepLevel;
      If not FPageNotYetDownloadedBinTree.AddNode(aNode) then aNode.Free;
    end;

  end;

  {update label}
  StatusBar1.Panels[0].Text := 'Url Downloaded: ' + inttostr((FPageDownloadedBinTree.nodeCount));
  StatusBar1.Panels[1].Text := 'Url to Download: ' + inttostr((FPageNotYetDownloadedBinTree.nodeCount));
  application.ProcessMessages;
  StopCrawling := not ButtonStop.Enabled;
end;

{*****************************************************************}
procedure TForm1.MainWebSpiderCrawlDownloadSuccess(Sender: TObject;
                                                   Url: String;
                                                   HTTPResponseHeader: TALHTTPResponseHeader;
                                                   HttpResponseContent: TStream;
                                                   var StopCrawling: Boolean);
Var aNode: TPageDownloadedBinTreeNode;
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
  If sametext(pMimeTypeFromData,'text/html') then begin
    Str := '<!-- saved from '+ URL+' -->' +#13#10 + Str;
    AlSaveStringToFile(str,aFileName);
  end
  {Else Save the file without any change}
  else TmemoryStream(HttpResponseContent).SaveToFile(aFileName);

  {delete the Url from the PageNotYetDownloadedBinTree}
  FPageNotYetDownloadedBinTree.DeleteNode(Url);

  {add the url to the PageDownloadedBinTree}
  aNode:= TPageDownloadedBinTreeNode.Create;
  aNode.ID := Url;
  aNode.data := AlCopyStr(AFileName,length(EditSaveDirectory.Text) + 1,maxint);
  If not FPageDownloadedBinTree.AddNode(aNode) then aNode.Free;

  {update label}
  StatusBar1.Panels[0].Text := 'Url Downloaded: ' + inttostr((FPageDownloadedBinTree.nodeCount));
  StatusBar1.Panels[1].Text := 'Url to Download: ' + inttostr((FPageNotYetDownloadedBinTree.nodeCount));
  application.ProcessMessages;
  StopCrawling := not ButtonStop.Enabled;
end;

{**********************************************************}
procedure TForm1.MainWebSpiderCrawlFindLink(Sender: TObject;
                                            HtmlTagString: String;
                                            HtmlTagParams: TStrings;
                                            URL: String);
Var aNode: TPageNotYetDownloadedBinTreeNode;
    Lst: TstringList;
    I: integer;
    Flag1 : Boolean;
    S1: String;
begin
  {If Check BoxDownload Image}
  IF not CheckBoxDownloadImage.Checked and
     (
      sametext(HtmlTagString,'img') or
      (
       sametext(HtmlTagString,'input') and
       sametext(Trim(HtmlTagParams.Values['type']),'image')
      )
     )
    then Exit;

  {Stay in start site}
  If CheckBoxStayInStartSite.Checked and
     (ALlowercase(AlExtractHostNameFromUrl(trim(editURL2Crawl.Text))) <> ALlowercase(AlExtractHostNameFromUrl(Url))) then exit;

  {DeepLevel}
  If (UpDownMaxDeepLevel.Position >= 0) and (FCurrentDeepLevel + 1 > UpDownMaxDeepLevel.Position) then exit;

  {include link(s)}
  If EditIncludeLink.Text <> '' then begin
    Lst := TstringList.Create;
    Try
      Lst.Text := Trim(AlStringReplace(EditIncludeLink.Text,';',#13#10,[RfReplaceall]));
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
  If EditExcludeLink.Text <> '' then begin
    Lst := TstringList.Create;
    Try
      Lst.Text := Trim(AlStringReplace(EditExcludeLink.Text,';',#13#10,[RfReplaceall]));
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

  {If the link not already downloaded then add it to the FPageNotYetDownloadedBinTree}
  If FPageDownloadedBinTree.FindNode(url) = nil then begin
    aNode:= TPageNotYetDownloadedBinTreeNode.Create;
    aNode.ID := Url;
    aNode.DeepLevel := FCurrentDeepLevel + 1;
    If not FPageNotYetDownloadedBinTree.AddNode(aNode) then aNode.Free
    else begin
      StatusBar1.Panels[1].Text := 'Url to Download: ' + inttostr((FPageNotYetDownloadedBinTree.nodeCount));
      application.ProcessMessages;
    end;
  end;
end;

{*************************************************************}
procedure TForm1.MainWebSpiderCrawlGetNextLink(Sender: TObject;
                                               var Url: String);

    {-----------------------------------------------------------------------------}
    function InternalfindNextUrlToDownload(aNode: TPageNotYetDownloadedBinTreeNode;
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

  StatusBar2.Panels[0].Text := 'Downloading Url: ' + Url;
  Application.ProcessMessages;
  if not ButtonStop.Enabled then url := '';
end;

{**************************************************************************}
procedure TForm1.MainWebSpiderUpdateLinkToLocalPathFindLink(Sender: TObject;
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
        LocalPath := TPageDownloadedBinTreeNode(aNode).Data;
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

{*****************************************************************************}
procedure TForm1.MainWebSpiderUpdateLinkToLocalPathGetNextFile(Sender: TObject;
                                                               var FileName, BaseHref: String);

  {-------------------------------------}
  Function SplitPathMakeFilename: String;
  begin
    If FCurrentLocalFileNameIndex < 0 then result := ''
    else If FCurrentLocalFileNameIndex = 0 then result := EditSaveDirectory.Text + 'Start.htm'
    else Result := EditSaveDirectory.Text + inttostr((FCurrentLocalFileNameIndex div SplitDirectoryAmount) * SplitDirectoryAmount + SplitDirectoryAmount) + '\' + inttostr(FCurrentLocalFileNameIndex) + '.htm';
    dec(FCurrentLocalFileNameIndex);
  end;

Begin
  {Find FileName}
  FileName := SplitPathMakeFilename;
  While (FileName <> '') and not fileExists(FileName) do
    Filename := SplitPathMakeFilename;

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

  end
  else BaseHref := '';

  {update label}
  StatusBar2.Panels[0].Text := 'Update Href to Local path for file: ' + FileName;
  application.ProcessMessages;
  if not ButtonStop.Enabled then begin
    FileName := '';
    BaseHref := '';
  end
end;

end.

