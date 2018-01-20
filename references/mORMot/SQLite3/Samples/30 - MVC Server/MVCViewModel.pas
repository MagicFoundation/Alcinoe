/// ViewModel/Control interfaces for the MVCServer BLOG sample
unit MVCViewModel;

{$I Synopse.inc} // define HASINLINE WITHLOG ONLYUSEHTTPSOCKET

interface

uses
  SysUtils,
  Contnrs,
  Variants,
  SynCommons,
  SynLog,
  SynTests,
  mORMot,
  mORMotMVC,
  MVCModel;

type
  /// defines the main ViewModel/Controller commands of the BLOG web site
  // - typical URI are:
  // ! blog/main/articleView?id=12 -> view one article
  // ! blog/main/authorView?id=12 -> information about one author
  // ! blog/main/login?name=...&plainpassword=... -> log as author
  // ! blog/main/articlecommit -> article edition commit (ID=0 for new)
  IBlogApplication = interface(IMVCApplication)
    ['{73B27C06-9DB9-45A2-BEDD-2013CFB609D0}']
    procedure ArticleView(ID: TID;
      var WithComments: boolean; Direction: integer; var Scope: variant;
      out Article: TSQLArticle; out Author: variant;
      out Comments: TObjectList);
    procedure AuthorView(
      var ID: TID; out Author: TSQLAuthor; out Articles: variant);
    function Login(
      const LogonName,PlainPassword: RawUTF8): TMVCAction;
    function Logout: TMVCAction;
    function ArticleComment(ID: TID; const Title,Comment: RawUTF8): TMVCAction;
    function ArticleMatch(const Match: RawUTF8): TMVCAction;
    procedure ArticleEdit(var ID: TID; const Title,Content: RawUTF8;
      const ValidationError: variant;
      out Article: TSQLArticle);
    function ArticleCommit(
      ID: TID; const Title,Content: RawUTF8): TMVCAction;
  end;

  /// session information which will be stored on client side within a cookie
  // - TMVCSessionWithCookies is able to store any record on the client side,
  // as optimized base64 encoded binary data, without any storage on the server
  // - before Delphi 2010, TTextWriter.RegisterCustomJSONSerializerFromText() is
  // called in initialization block below, to allow proper JSON serialization
  // as needed for fields injection into the Mustache rendering data context
  TCookieData = packed record
    AuthorName: RawUTF8;
    AuthorID: cardinal;
    AuthorRights: TSQLAuthorRights;
  end;

  /// implements the ViewModel/Controller of this BLOG web site
  TBlogApplication = class(TMVCApplication,IBlogApplication)
  protected
    fBlogMainInfo: variant;
    fTagsLookup: TSQLTags;
    fDefaultData: ILockedDocVariant;
    fDefaultLastID: TID;
    fHasFTS: boolean;
    procedure ComputeMinimalData; virtual;
    procedure FlushAnyCache; override;
    function GetViewInfo(MethodIndex: integer): variant; override;
    function GetLoggedAuthorID(Right: TSQLAuthorRight; ContentToFillAuthor: TSQLContent): TID;
    procedure MonthToText(const Value: variant; out result: variant);
    procedure TagToText(const Value: variant; out result: variant);
  public
    procedure Start(aServer: TSQLRestServer); reintroduce;
  public
    procedure Default(var Scope: variant);
    procedure ArticleView(ID: TID;
      var WithComments: boolean; Direction: integer; var Scope: variant;
      out Article: TSQLArticle; out Author: variant;
      out Comments: TObjectList);
    procedure AuthorView(
      var ID: TID; out Author: TSQLAuthor; out Articles: variant);
    function Login(const LogonName,PlainPassword: RawUTF8): TMVCAction;
    function Logout: TMVCAction;
    function ArticleComment(ID: TID; const Title,Comment: RawUTF8): TMVCAction;
    function ArticleMatch(const Match: RawUTF8): TMVCAction;
    procedure ArticleEdit(var ID: TID; const Title,Content: RawUTF8;
      const ValidationError: variant;
      out Article: TSQLArticle);
    function ArticleCommit(ID: TID; const Title,Content: RawUTF8): TMVCAction;
  end;


implementation

resourcestring
  sErrorInvalidLogin = 'Wrong logging information';
  sErrorNeedValidAuthorSession = 'You need to be logged as a valid Author to perform this action';
  sErrorWriting = 'An error occured during saving the information to the database';


{ TBlogApplication }

procedure TBlogApplication.Start(aServer: TSQLRestServer);
begin
  fDefaultData := TLockedDocVariant.Create;
  inherited Start(aServer,TypeInfo(IBlogApplication));
  fHasFTS := aServer.StaticVirtualTable[TSQLArticle]=nil;
  fTagsLookup.Init(RestModel);
  // publish IBlogApplication using Mustache Views (TMVCRunOnRestServer default)
  fMainRunner := TMVCRunOnRestServer.Create(Self).
    SetCache('Default',cacheRootIfNoSession,15).
    SetCache('ArticleView',cacheWithParametersIfNoSession,60).
    SetCache('AuthorView',cacheWithParametersIgnoringSession,60);
  (TMVCRunOnRestServer(fMainRunner).Views as TMVCViewsMustache).
    RegisterExpressionHelpers(['MonthToText'],[MonthToText]).
    RegisterExpressionHelpers(['TagToText'],[TagToText]);
  ComputeMinimalData;
  aServer.Cache.SetCache(TSQLAuthor);
  aServer.Cache.SetCache(TSQLArticle);
  aServer.Cache.SetCache(TSQLComment);
  aServer.Cache.SetTimeOut(TSQLArticle,60000);
  aServer.Cache.SetTimeOut(TSQLComment,60000);
  with TSQLBlogInfo.Create(RestModel,'') do
  try
    fBlogMainInfo := GetSimpleFieldsAsDocVariant(false);
  finally
    Free;
  end;
end;

procedure TBlogApplication.MonthToText(const Value: variant;
  out result: variant);
const MONTHS: array[0..11] of RawUTF8 = (
  'January','February','March','April','May','June','July','August',
  'September','October','November','December');
var month: integer;
begin
  if VariantToInteger(Value,month) and (month>0) then
    RawUTF8ToVariant(MONTHS[month mod 12]+' '+UInt32ToUTF8(month div 12),result) else
    SetVariantNull(result);
end;

procedure TBlogApplication.TagToText(const Value: variant;
  out result: variant);
var tag: integer;
begin
  if VariantToInteger(Value,tag) then
     RawUTF8ToVariant(fTagsLookup.Get(tag),result) else
     SetVariantNull(result);
end;

const
  // just try with 100000 - and let your WordPress blog engine start to cry...
  // note that it includes FullText indexation if you use SQLite3 as database!
  FAKEDATA_ARTICLESCOUNT = 10000;
  
procedure TBlogApplication.ComputeMinimalData;
var info: TSQLBlogInfo;
    article: TSQLArticle;
    comment: TSQLComment;
    tag: TSQLTag;
    batch: TSQLRestBatch;
    n,t: integer;
    articles,tags,comments: TIDDynArray;
    tmp: RawUTF8;
    auto: IAutoFree; // mandatory only for FPC
begin
  auto := TSQLRecord.AutoFree([ // avoid several try..finally
    @info,TSQLBlogInfo, @article,TSQLArticle, @comment,TSQLComment, @tag,TSQLTag]);
  if not RestModel.Retrieve('',info) then begin // retrieve first item
    info.Title := 'mORMot BLOG';
    info.Language := 'en';
    info.Description := 'Sample Blog Web Application using Synopse mORMot MVC';
    info.Copyright := '&copy;2018 <a href=https://synopse.info>Synopse Informatique</a>';
    info.About := TSynTestCase.RandomTextParagraph(30,'!');
    RestModel.Add(info,true);
  end;
  if RestModel.TableHasRows(TSQLArticle) then
    exit;
  tmp := StringFromFile('d:\download\2014-12-27-a8003957c2ae6bde5be6ea279c9c9ce4-backup.txt');
  if tmp<>'' then begin
    DotClearFlatImport(RestModel,tmp,fTagsLookup,'http://blog.synopse.info',
      (TMVCRunOnRestServer(fMainRunner).Views as TMVCViewsMustache).ViewStaticFolder);
    exit;
  end;  
  SetLength(tags,32);
  for n := 1 to length(tags) do begin
    tag.Ident := 'Tag'+UInt32ToUtf8(n);
    tag.IDValue := n*2; // force test TSQLTags layout
    tags[n-1] := RestModel.Add(tag,true,true);
  end;
  fTagsLookup.Init(RestModel); // reload after initial fill
  batch := TSQLRestBatch.Create(RestModel,TSQLArticle,20000);
  try
    article.Author := TSQLAuthor(1);
    article.AuthorName := 'synopse';
    for n := 1 to FAKEDATA_ARTICLESCOUNT do begin
      article.PublishedMonth := 2014*12+(n div 10);
      article.Title := TSynTestCase.RandomTextParagraph(5,' ');
      article.Abstract := TSynTestCase.RandomTextParagraph(30,'!');
      article.Content := TSynTestCase.RandomTextParagraph(200,'.','https://synopse.info');
      article.Tags := nil;
      for t := 1 to Random(6) do
        article.TagsAddOrdered(tags[random(length(tags))],fTagsLookup);
      batch.Add(article,true);
    end;
    if RestModel.BatchSend(batch,articles)=HTTP_SUCCESS then begin
      fTagsLookup.SaveOccurence(RestModel);
      comment.Author := article.Author;
      comment.AuthorName := article.AuthorName;
      batch.Reset(TSQLComment,20000);
      for n := 1 to FAKEDATA_ARTICLESCOUNT*2 do begin
        comment.Article := Pointer(articles[random(length(articles))]);
        comment.Title := TSynTestCase.RandomTextParagraph(5,' ');
        comment.Content := TSynTestCase.RandomTextParagraph(30,'.','http://mormot.net');
        batch.Add(Comment,true);
      end;
      RestModel.BatchSend(batch,comments)
    end;
  finally
    batch.Free;
  end;
end;

function TBlogApplication.GetLoggedAuthorID(Right: TSQLAuthorRight;
  ContentToFillAuthor: TSQLContent): TID;
var SessionInfo: TCookieData;
    author: TSQLAuthor;
begin
  result := 0;
  if (CurrentSession.CheckAndRetrieve(@SessionInfo,TypeInfo(TCookieData))>0) and
     (Right in SessionInfo.AuthorRights) then
    with TSQLAuthor.AutoFree(author,RestModel,SessionInfo.AuthorID) do
    if Right in author.Rights then begin
      result := SessionInfo.AuthorID;
      if ContentToFillAuthor<>nil then begin
        ContentToFillAuthor.Author := pointer(result);
        ContentToFillAuthor.AuthorName := author.LogonName;
      end;
    end;
end;

function TBlogApplication.GetViewInfo(MethodIndex: integer): variant;
begin
  result := inherited GetViewInfo(MethodIndex);
  _ObjAddProps(['blog',fBlogMainInfo,
    'session',CurrentSession.CheckAndRetrieveInfo(TypeInfo(TCookieData))],result);
  if not fDefaultData.AddExistingProp('archives',result) then
    fDefaultData.AddNewProp('archives',RestModel.RetrieveDocVariantArray(
      TSQLArticle,'','group by PublishedMonth order by PublishedMonth desc limit 100',[],
      'distinct(PublishedMonth),max(RowID)+1 as FirstID'),result);
  if not fDefaultData.AddExistingProp('tags',result) then
    fDefaultData.AddNewProp('tags',fTagsLookup.GetAsDocVariantArray,result);
end;

procedure TBlogApplication.FlushAnyCache;
begin
  inherited FlushAnyCache; // call fMainRunner.NotifyContentChanged
  fDefaultData.Clear;
end;


{ TBlogApplication - Commands }

const
  ARTICLE_FIELDS = 'RowID,Title,Tags,Abstract,ContentHtml,Author,AuthorName,CreatedAt';
  ARTICLE_DEFAULT_LIMIT = ' limit 20';
  ARTICLE_DEFAULT_ORDER: RawUTF8 = 'order by RowID desc'+ARTICLE_DEFAULT_LIMIT;

procedure TBlogApplication.Default(var Scope: variant);
var scop: PDocVariantData;
    lastID: TID;
    tag: integer;
    whereClause,match: RawUTF8;
    articles: variant;
    rank: double;
begin
  lastID := 0;
  tag := 0;
  rank := 0;
  scop := _Safe(Scope);
  if scop^.GetAsRawUTF8('match',match) and fHasFTS then begin
    if scop^.GetAsDouble('lastrank',rank) then
      whereClause := 'and rank<? ';
    whereClause := 'join (select docid,rank(matchinfo(ArticleSearch),1.0,0.7,0.5) as rank '+
      'from ArticleSearch where ArticleSearch match ? '+whereClause+
      'order by rank desc'+ARTICLE_DEFAULT_LIMIT+')as r on (r.docid=Article.id)';
    articles := RestModel.RetrieveDocVariantArray(
      TSQLArticle,'',whereClause,[match,rank],
      'id,title,tags,author,authorname,createdat,abstract,contenthtml,rank');
    with _Safe(articles)^ do
      if (Kind=dvArray) and (Count>0) then
        rank := Values[Count-1].rank else
        rank := 0;
    scope := _ObjFast(['Articles',articles,'lastrank',rank,'match',match]);
    exit;
  end else begin
    if scop^.GetAsInt64('lastID',Int64(lastID)) then
      whereClause := 'RowID<?' else
      whereClause := 'RowID>?'; // will search ID>0 so always true
    if scop^.GetAsInteger('tag',tag) and (tag>0) then
      // uses custom function to search in BLOB
      whereClause := whereClause+' and IntegerDynArrayContains(Tags,?)';
  end;
  SetVariantNull(Scope);
  if (lastID=0) and (tag=0) then begin // use simple cache if no parameters
    if not fDefaultData.AddExistingProp('Articles',Scope) then
      fDefaultData.AddNewProp('Articles',RestModel.RetrieveDocVariantArray(
        TSQLArticle,'',ARTICLE_DEFAULT_ORDER,[],
        ARTICLE_FIELDS,nil,@fDefaultLastID),Scope);
    lastID := fDefaultLastID;
  end else // use more complex request using lastID + tag parameters
    scope := _ObjFast(['Articles',RestModel.RetrieveDocVariantArray(
        TSQLArticle,'',whereClause+ARTICLE_DEFAULT_ORDER,[lastID,tag],
        ARTICLE_FIELDS,nil,@lastID)]);
  if lastID>1 then
    _ObjAddProps(['lastID',lastID, 'tag',tag],Scope);
end;

procedure TBlogApplication.ArticleView(ID: TID;
  var WithComments: boolean; Direction: integer; var Scope: variant;
  out Article: TSQLArticle; out Author: variant; out Comments: TObjectList);
var newID: Int64;
const WHERE: array[1..2] of PUTF8Char = (
  'RowID<? order by id desc','RowID>? order by id');
begin
  if Direction in [1,2] then // allows fast paging using index on ID
    if RestModel.OneFieldValue(TSQLArticle,'RowID',WHERE[Direction],[],[ID],newID) and
      (newID<>0) then
      ID := newID;
  RestModel.Retrieve(ID,Article);
  if Article.ID<>0 then begin
    Author := RestModel.RetrieveDocVariant(
      TSQLAuthor,'RowID=?',[Article.Author.ID],'FirstName,FamilyName');
    if WithComments then begin
      Comments.Free; // we will override the TObjectList created at input
      Comments := RestModel.RetrieveList(TSQLComment,'Article=?',[Article.ID]);
    end;
  end else
    raise EMVCApplication.CreateGotoError(HTTP_NOTFOUND);
end;

procedure TBlogApplication.AuthorView(var ID: TID; out Author: TSQLAuthor;
  out Articles: variant);
begin
  RestModel.Retrieve(ID,Author);
  Author.HashedPassword := ''; // no need to publish it
  if Author.ID<>0 then
    Articles := RestModel.RetrieveDocVariantArray(
      TSQLArticle,'','Author=? order by RowId desc limit 50',[ID],ARTICLE_FIELDS) else
    raise EMVCApplication.CreateGotoError(HTTP_NOTFOUND);
end;

function TBlogApplication.Login(const LogonName, PlainPassword: RawUTF8): TMVCAction;
var Author: TSQLAuthor;
    SessionInfo: TCookieData;
begin
  if CurrentSession.CheckAndRetrieve<>0 then begin
    GotoError(result,HTTP_BADREQUEST);
    exit;
  end;
  Author := TSQLAuthor.Create(RestModel,'LogonName=?',[LogonName]);
  try
    if (Author.ID<>0) and Author.CheckPlainPassword(PlainPassword) then begin
      SessionInfo.AuthorName := Author.LogonName;
      SessionInfo.AuthorID := Author.ID;
      SessionInfo.AuthorRights := Author.Rights;
      CurrentSession.Initialize(@SessionInfo,TypeInfo(TCookieData));
      GotoDefault(result);
    end else
      GotoError(result,sErrorInvalidLogin);
  finally
    Author.Free;
  end;
end;

function TBlogApplication.Logout: TMVCAction;
begin
  CurrentSession.Finalize;
  GotoDefault(result);
end;

function TBlogApplication.ArticleComment(ID: TID;
  const Title,Comment: RawUTF8): TMVCAction;
var comm: TSQLComment;
    AuthorID: TID;
    error: string;
begin
  with TSQLComment.AutoFree(comm) do begin
    AuthorID := GetLoggedAuthorID(canComment,comm);
    if AuthorID=0 then begin
      GotoError(result,sErrorNeedValidAuthorSession);
      exit;
    end;
    if not RestModel.MemberExists(TSQLArticle,ID) then begin
      GotoError(result,HTTP_UNAVAILABLE);
      exit;
    end;
    comm.Title := Title;
    comm.Content := Comment;
    comm.Article := TSQLArticle(ID);
    if comm.FilterAndValidate(RestModel,error) and
       (RestModel.Add(comm,true)<>0) then
      GotoView(result,'ArticleView',['ID',ID,'withComments',true]) else
      GotoView(result,'ArticleView',['ID',ID,'withComments',true,'Scope',_ObjFast([
        'CommentError',error,'CommentTitle',comm.Title,'CommentContent',comm.Content])],
        HTTP_BADREQUEST);
  end;
end;

function TBlogApplication.ArticleMatch(const Match: RawUTF8): TMVCAction;
begin
  if Match='' then
    GotoError(result,HTTP_NOTMODIFIED) else
    GotoView(result,'Default',['scope',_ObjFast(['match',Match])]);
end;

procedure TBlogApplication.ArticleEdit(var ID: TID;
  const Title,Content: RawUTF8; const ValidationError: variant;
  out Article: TSQLArticle);
var AuthorID: PtrUInt;
begin
  AuthorID := GetLoggedAuthorID(canPost,Article);
  if AuthorID=0 then
    raise EMVCApplication.CreateGotoError(sErrorNeedValidAuthorSession);
  if ID<>0 then
    if not RestModel.Retrieve(ID,Article) then
      raise EMVCApplication.CreateGotoError(HTTP_UNAVAILABLE) else
    if Article.Author<>pointer(AuthorID) then
      raise EMVCApplication.CreateGotoError(sErrorNeedValidAuthorSession);
  if Title<>'' then
    Article.Title := Title;
  if Content<>'' then
    Article.Content := Content;
end;

function TBlogApplication.ArticleCommit(ID: TID; const Title,Content: RawUTF8): TMVCAction;
var Article: TSQLArticle;
    AuthorID: TID;
    error: string;
begin
  with TSQLArticle.AutoFree(Article,RestModel,ID) do begin
    AuthorID := GetLoggedAuthorID(canPost,Article);
    if AuthorID=0 then begin
      GotoError(result,sErrorNeedValidAuthorSession);
      exit;
    end;
    FlushAnyCache;
    Article.Title := Title;
    Article.Content := Content;
    if not Article.FilterAndValidate(RestModel,error) then
      GotoView(result,'ArticleEdit',
        ['ValidationError',error,'ID',ID,
         'Title',Article.Title,'Content',Article.Content],HTTP_BADREQUEST) else
      if Article.ID=0 then begin
        Article.PublishedMonth := TSQLArticle.CurrentPublishedMonth;
        if RestModel.Add(Article,true)<>0 then
          GotoView(result,'ArticleView',['ID',Article.ID],HTTP_SUCCESS) else
          GotoError(result,sErrorWriting);
      end else
        RestModel.Update(Article);
  end;
end;

initialization
  {$ifndef DELPHI2010}
  // manual definition mandatory only if Delphi 2010 RTTI is not available
  TTextWriter.RegisterCustomJSONSerializerFromTextSimpleType(TypeInfo(TSQLAuthorRights));
  TTextWriter.RegisterCustomJSONSerializerFromText(TypeInfo(TCookieData),
    'AuthorName RawUTF8 AuthorID cardinal AuthorRights TSQLAuthorRights');
  {$endif}
end.
