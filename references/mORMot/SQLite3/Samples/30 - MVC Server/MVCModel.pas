/// database Model for the MVCServer BLOG sample
unit MVCModel;

{$I Synopse.inc} // define HASINLINE WITHLOG ONLYUSEHTTPSOCKET

interface

uses
  SysUtils,
  SynCommons,
  SynCrypto,
  mORMot;

type
  TSQLBlogInfo = class(TSQLRecord)
  private
    fCopyright: RawUTF8;
    fDescription: RawUTF8;
    fTitle: RawUTF8;
    fLanguage: RawUTF8;
    fAbout: RawUTF8;
  published
    property Title: RawUTF8 index 80 read fTitle write fTitle;
    property Language: RawUTF8 index 3 read fLanguage write fLanguage;
    property Description: RawUTF8 index 120 read fDescription write fDescription;
    property Copyright: RawUTF8 index 80 read fCopyright write fCopyright;
    property About: RawUTF8 read fAbout write fAbout;
  end;

  TSQLRecordTimeStamped = class(TSQLRecord)
  private
    fCreatedAt: TCreateTime;
    fModifiedAt: TModTime;
  published
    property CreatedAt: TCreateTime read fCreatedAt write fCreatedAt;
    property ModifiedAt: TModTime read fModifiedAt write fModifiedAt;
  end;

  TSQLSomeone = class(TSQLRecordTimeStamped)
  private
    fFirstName: RawUTF8;
    fFamilyName: RawUTF8;
    fBirthDate: TDateTime;
    fEmail: RawUTF8;
    fVerified: boolean;
    fHashedPassword: RawUTF8;
    fLogonName: RawUTF8;
  public
    procedure SetPlainPassword(const PlainPassword: RawUTF8);
    function CheckPlainPassword(const PlainPassword: RawUTF8): boolean;
    function Name: RawUTF8; 
  published
    property LogonName: RawUTF8 index 30 read fLogonName write fLogonName stored AS_UNIQUE;
    property FirstName: RawUTF8 index 50 read fFirstName write fFirstName;
    property FamilyName: RawUTF8 index 50 read fFamilyName write fFamilyName;
    property BirthDate: TDateTime read fBirthDate write fBirthDate;
    property Email: RawUTF8 index 40 read fEmail write fEmail;
    property HashedPassword: RawUTF8 index 64 read fHashedPassword write fHashedPassword;
    property Verified: boolean read fVerified write fVerified;
  end;
          
  TSQLAuthorRight = (canComment, canPost, canDelete, canAdministrate);
  TSQLAuthorRights = set of TSQLAuthorRight;

  TSQLAuthor = class(TSQLSomeone)
  private
    fRights: TSQLAuthorRights;
  public
    class procedure InitializeTable(Server: TSQLRestServer; const FieldName: RawUTF8;
      Options: TSQLInitializeTableOptions); override;
  published
    property Rights: TSQLAuthorRights read fRights write fRights;
  end;

  TSQLContent = class(TSQLRecordTimeStamped)
  private
    fContent: RawUTF8;
    fTitle: RawUTF8;
    fAuthor: TSQLAuthor;
    fAuthorName: RawUTF8;
    fContentHtml: boolean;
  published
    property Title: RawUTF8 index 120 read fTitle write fTitle;
    property Content: RawUTF8 read fContent write fContent;
    property ContentHtml: boolean read fContentHtml write fContentHtml;
    property Author: TSQLAuthor read fAuthor write fAuthor;
    property AuthorName: RawUTF8 index 50 read fAuthorName write fAuthorName;
  end;

  TSQLTags = object
    Lock: IAutoLocker;
    Lookup: array of record
      Ident: RawUTF8;
      Occurence: integer;
    end;
    OrderID: TIntegerDynArray;
    procedure Init(aRest: TSQLRest);
    function Get(tagID: integer): RawUTF8;
    procedure SaveOccurence(aRest: TSQLRest);
    procedure SortTagsByIdent(var Tags: TIntegerDynArray);
    function GetAsDocVariantArray: Variant;
  end;

  TSQLArticle = class(TSQLContent)
  private
    fAbstract: RawUTF8;
    fPublishedMonth: Integer;
    fTags: TIntegerDynArray;
  public
    class function CurrentPublishedMonth: Integer;
    class procedure InitializeTable(Server: TSQLRestServer; const FieldName: RawUTF8;
      Options: TSQLInitializeTableOptions); override;
    procedure SetPublishedMonth(FromTime: TTimeLog);
    // note: caller should call Tags.SaveOccurence() to update the DB
    procedure TagsAddOrdered(aTagID: Integer; var aTags: TSQLTags);
  published
    property PublishedMonth: Integer read fPublishedMonth write fPublishedMonth;
    property Abstract: RawUTF8 read fAbstract write fAbstract;
    // "index 1" below to allow writing e.g. aArticle.DynArray(1).Delete(aIndex)
    property Tags: TIntegerDynArray index 1 read fTags write fTags;
  end;

  TSQLArticleSearch = class(TSQLRecordFTS4Porter)
  private
    fContent: RawUTF8;
    fTitle: RawUTF8;
    fAbstract: RawUTF8;
  published
    property Title: RawUTF8 read fTitle write fTitle;
    property Abstract: RawUTF8 read fAbstract write fAbstract;
    property Content: RawUTF8 read fContent write fContent;
  end;

  TSQLComment = class(TSQLContent)
  private
    fArticle: TSQLArticle;
  published
    property Article: TSQLArticle read fArticle write fArticle;
  end;

  TSQLTag = class(TSQLRecord)
  private
    fIdent: RawUTF8;
    fOccurence: integer;
    fCreatedAt: TCreateTime;
  published
    property Ident: RawUTF8 index 80 read fIdent write fIdent;
    property Occurence: Integer read fOccurence write fOccurence;
    property CreatedAt: TCreateTime read fCreatedAt write fCreatedAt;
  end;



function CreateModel: TSQLModel;

procedure DotClearFlatImport(Rest: TSQLRest; const aFlatFile: RawUTF8;
  var aTagsLookup: TSQLTags; const aDotClearRoot: RawUTF8;
  const aStaticFolder: TFileName);


implementation

uses
  SynCrtSock; // for DotClearFlatImport() below
  
function CreateModel: TSQLModel;
begin
  result := TSQLModel.Create([TSQLBlogInfo,TSQLAuthor,
    TSQLTag,TSQLArticle,TSQLComment,TSQLArticleSearch],'blog');
  TSQLArticle.AddFilterNotVoidText(['Title','Content']);
  TSQLComment.AddFilterNotVoidText(['Title','Content']);
  TSQLTag.AddFilterNotVoidText(['Ident']);
  result.Props[TSQLArticleSearch].FTS4WithoutContent(TSQLArticle);
end;


{ TSQLSomeone }

const
  SALT = 'mORMot';

function TSQLSomeone.CheckPlainPassword(const PlainPassword: RawUTF8): boolean;
begin
  result := fHashedPassword=SHA256(SALT+LogonName+PlainPassword);
end;

function TSQLSomeone.Name: RawUTF8;
begin
  result := FirstName+' '+FamilyName;
end;

procedure TSQLSomeone.SetPlainPassword(const PlainPassword: RawUTF8);
begin
  fHashedPassword := SHA256(SALT+LogonName+PlainPassword);
end;


{ TSQLAuthor }

class procedure TSQLAuthor.InitializeTable(Server: TSQLRestServer;
  const FieldName: RawUTF8; Options: TSQLInitializeTableOptions);
var Auth: TSQLAuthor;
begin
  inherited InitializeTable(Server,FieldName,Options);
  if FieldName='' then begin // new table -> create default Author
    Auth := TSQLAuthor.Create;
    try
      Auth.LogonName := 'synopse';
      Auth.SetPlainPassword('synopse');
      Auth.FamilyName := 'Synopse';
      Auth.Verified := true;
      Auth.Rights := [Low(TSQLAuthorRight)..High(TSQLAuthorRight)];
      Server.Add(Auth,true);
    finally
      Auth.Free;
    end;
  end;
end;


{ TSQLArticle }

class function TSQLArticle.CurrentPublishedMonth: Integer;
var Y,M,D: word;
begin
  DecodeDate(NowUTC,Y,M,D);
  result := integer(Y)*12+integer(M)-1;
end;

class procedure TSQLArticle.InitializeTable(Server: TSQLRestServer;
  const FieldName: RawUTF8; Options: TSQLInitializeTableOptions);
begin
  inherited;
  if (FieldName='') or (FieldName='PublishedMonth') then
    Server.CreateSQLIndex(TSQLArticle,'PublishedMonth',false);
end;

procedure TSQLArticle.SetPublishedMonth(FromTime: TTimeLog);
begin
  fPublishedMonth := TTimeLogBits(FromTime).Year*12+TTimeLogBits(FromTime).Month-1;
end;

procedure TSQLArticle.TagsAddOrdered(aTagID: Integer; var aTags: TSQLTags);
begin
  if (aTagID<length(aTags.Lookup)) and
     AddInteger(fTags,aTagID,true) then
    with aTags.Lock.ProtectMethod do begin
      inc(aTags.Lookup[aTagID-1].Occurence);
      aTags.SortTagsByIdent(fTags);
    end;
end;


{ TSQLTags }

function TSQLTags.Get(tagID: integer): RawUTF8;
begin
  if (tagID>0) and (tagID<=Length(Lookup)) then
    result := Lookup[tagID-1].Ident else
    result := '';
end;

function TSQLTags.GetAsDocVariantArray: Variant;
var i,ndx: Integer;
begin
  TDocVariant.NewFast(result);
  with Lock.ProtectMethod do
  for i := 0 to length(OrderID)-1 do begin
    ndx := OrderID[i]-1;
    with Lookup[ndx] do
      if Occurence>0 then
        TDocVariantData(result).AddItem(
          _ObjFast(['tagID',ndx+1,'ident',Ident,'occurence',Occurence]));
  end;
end;

procedure TSQLTags.Init(aRest: TSQLRest);
var tag: TSQLTag;
    ID,count,maxID: integer;
begin
  Finalize(Lookup);
  if Lock=nil then
    Lock := TAutoLocker.Create;
  with Lock.ProtectMethod, TAutoFree.One(tag,TSQLTag.CreateAndFillPrepare(
     aRest,'order by Ident','RowID,Ident,Occurence')) do begin
    count := tag.FillTable.RowCount;
    if count=0 then
      exit;
    SetLength(OrderID,count);
    count := 0;
    maxID := 0;
    while tag.FillOne do begin
      ID := tag.ID;
      OrderID[count] := ID;
      inc(count);
      if ID>maxID then
        maxID := ID;
    end;
    SetLength(Lookup,maxID);
    tag.FillRewind;
    while tag.FillOne do
    with Lookup[tag.ID-1] do begin
      Ident := tag.Ident;
      Occurence := tag.Occurence;
    end;
  end;
end;

procedure TSQLTags.SaveOccurence(aRest: TSQLRest);
var tag: TSQLTag;
    batch: TSQLRestBatch;
begin
  with TAutoFree.Several([
     @tag,TSQLTag.CreateAndFillPrepare(aRest,'','RowID,Occurence'),
     @batch,TSQLRestBatch.Create(aRest,TSQLTag,1000)]), Lock.ProtectMethod do begin
    while tag.FillOne do begin
      if tag.ID<=length(Lookup) then
        if Lookup[tag.ID-1].Occurence<>tag.Occurence then begin
          tag.Occurence := Lookup[tag.ID-1].Occurence;
          batch.Update(tag); // will update only Occurence field
        end;
    end;
    aRest.BatchSend(batch);
  end;
end;

procedure TSQLTags.SortTagsByIdent(var Tags: TIntegerDynArray);
var new: TIntegerDynArray;
    i,n: integer;
begin // Lock.ProtectMethod made by caller
  n := length(Tags);
  if n=1 then
    exit;
  SetLength(new,n);
  QuickSortInteger(pointer(Tags),0,n-1);
  n := 0;
  for i := 0 to length(OrderID)-1 do
    if FastFindIntegerSorted(Tags,OrderID[i])>=0 then begin
      new[n] := OrderID[i];
      inc(n);
    end;
  assert(n=length(Tags));
  Tags := new;
end;

type
  /// used to store a DotClear flat export data section
  TDotClearTable = class(TSQLTable)
  protected
    fText: RawUTF8;
    fFields: TRawUTF8DynArray;
    fJSONResults: array of PUTF8Char;
    fName: RawUTF8;
  public
    /// compute a section content
    constructor Create(var Text: PUTF8Char);
    /// parse a DotClear flat export text file, and create a list of sections
    // - you can later on use aList.GetObjectByName('post') as TDotClearTable
    // to access a given section
    class function Parse(const aFlatExport: RawUTF8): TRawUTF8List;
    /// the name of the section, e.g. 'category' or 'post'
    property Name: RawUTF8 read fName;
  end;

constructor TDotClearTable.Create(var Text: PUTF8Char);
var P,D: PUTF8Char;
    f,r: integer;
begin
  fName := GetNextItem(Text,' ');
  CSVToRawUTF8DynArray(Pointer(GetNextItem(Text,']')),fFields);
  fFieldCount := length(fFields);
  GetNextLineBegin(Text,Text);
  P := pointer(Text);
  while (Text<>nil) and (Text^='"') do begin
    GetNextLineBegin(Text,Text);
    inc(fRowCount);
  end;
  if Text=nil then
    fText := P else
    SetString(fText,PAnsiChar(P),Text-P);
  SetLength(fJSONResults,fFieldCount*(fRowCount+1));
  fResults := pointer(fJSONResults);
  for f := 0 to fFieldCount-1 do begin
    fResults[f] := pointer(fFields[f]);
    SetFieldType(f,sftUTF8Text);
  end;
  for r := 1 to fRowCount do begin
    assert(P^='"');
    inc(P);
    for f := 0 to fFieldCount-1 do begin
      fResults[r*fFieldCount+f] := P;
      D := P;
      while P^<>'"' do
        if P^=#0 then
          exit else begin
          if P^='\' then begin
            inc(P);
            case P^ of
            'r': D^ := #13;
            'n': D^ := #10;
            '\': D^ := '\';
            '"': D^ := '"';
            else begin
              D^ := '\';
              inc(D);
              D^ := P^;
            end;
            end;
          end else
            D^ := P^;
          inc(P);
          inc(D);
        end;
      D^ := #0;
      inc(P);
      if (P[0]=',')and(P[1]='"') then
        inc(P,2);
    end;
    GetNextLineBegin(P,P);
  end;
end;

class function TDotClearTable.Parse(const aFlatExport: RawUTF8): TRawUTF8List;
var P: PUTF8Char;
    T: TDotClearTable;
begin
  result := TRawUTF8List.Create(true);
  P := pointer(aFlatExport);
  repeat
    while (P<>nil) and (P^<>'[') do
      GetNextLineBegin(P,P);
    if P=nil then
      exit;
    inc(P);
    T := TDotClearTable.Create(P);
    result.AddObject(T.Name,T);
    //FileFromString(T.GetODSDocument,TFileName(T.Name)+'.ods');
  until P=nil;
end;

procedure DotClearFlatImport(Rest: TSQLRest; const aFlatFile: RawUTF8;
  var aTagsLookup: TSQLTags; const aDotClearRoot: RawUTF8;
  const aStaticFolder: TFileName);
var T,tagTable,postTable: TDotClearTable;
    data,urls: TRawUTF8List;
    info: TSQLBlogInfo;
    article: TSQLArticle;
    comment: TSQLComment;
    tag: TSQLTag;
    tags: TRawUTF8DynArray;
    tagID: TIDDynArray;
    tagsCount: integer;
    batch: TSQLRestBatch;
    PublicFolder: TFileName;
    r,ndx,post_url,meta_id,meta_type,tag_post_id,postID,post_id: integer;
function FixLinks(P: PUTF8Char): RawUTF8;
var B,H: PUTF8Char;
    url: RawUTF8;
    i,urlLen: integer;
    FN: TFileName;
procedure GetUrl(H: PUTF8Char);
begin
  url := GetNextItem(H,'"');
  urlLen := length(url);
  url := UrlDecode(url);
end;
begin
  with TTextWriter.CreateOwnedStream(16384) do
  try
    B := P;
    while P<>nil do begin
      while P^<>' ' do
        if P^=#0 then
          break else
          inc(P);
      if P^=#0 then
        break;
      inc(P);
      H := P; // makes compiler happy
      if IdemPChar(P,'HREF=') then
        inc(H,5) else
      if IdemPChar(P,'SRC=') then
        inc(H,4) else
        continue;
      if H^='"' then inc(H);
      AddNoJSONEscape(B,H-B);
      P := H;
      if P^='/' then
        if IdemPChar(P+1,'POST/') then begin
          GetUrl(P+6);
          i := PosEx('?',url);
          if i>0 then
            SetLength(url,i-1);
          i := urls.IndexOf(url);
          if i>=0 then begin
            AddShort('articleView?id=');
            Add(i+1);
            inc(P,urlLen+6);
          end else
            AddString(aDotClearRoot);
        end else
        if IdemPChar(P+1,'PUBLIC/') then begin
          if PublicFolder<>'' then begin
            GetUrl(P+8);
            FN := PublicFolder+UTF8ToString(StringReplaceChars(url,'/',PathDelim));
            EnsureDirectoryExists(ExtractFilePath(FN));
            if not FileExists(FN) then
              FileFromString({$ifdef MSWINDOWS}TWinHTTP.Get{$else}HttpGet{$endif}(
                aDotClearRoot+'/public/'+url),FN);
            AddShort('.static');
          end else
            AddString(aDotClearRoot);
        end;
      B := P;
    end;
    AddNoJSONEscape(B);
    SetText(result);
  finally
    Free;
  end;
end;
var auto1,auto2: IAutoFree; // mandatory only for FPC
begin
  if aStaticFolder<>'' then begin
    PublicFolder := IncludeTrailingPathDelimiter(aStaticFolder)+'public'+PathDelim;
    EnsureDirectoryExists(PublicFolder);
  end;
  auto1 := TAutoFree.Several([
    @data,TDotClearTable.Parse(aFlatFile),
    @urls,TRawUTF8ListHashed.Create,
    @batch,TSQLRestBatch.Create(Rest,TSQLTag,5000)]);
  auto2 := TSQLRecord.AutoFree([ // avoid several try..finally
    @info,TSQLBlogInfo, @article,TSQLArticle, @comment,TSQLComment, @tag,TSQLTag]);
  T := data.GetObjectByName('setting') as TDotClearTable;
  Rest.Retrieve('',info);
  info.Copyright := VariantToUTF8(T.GetValue('setting_id','copyright_notice','setting_value'));
  if info.ID=0 then
    Rest.Add(info,true) else
    Rest.Update(info);
  tagTable := data.GetObjectByName('meta') as TDotClearTable;
  tagsCount := 0;
  meta_id := tagTable.FieldIndexExisting('meta_id');
  meta_type := tagTable.FieldIndexExisting('meta_type');
  for r := 1 to tagTable.RowCount do
    if tagTable.GetU(r,meta_type)='tag' then
      AddSortedRawUTF8(tags,tagsCount,tagTable.GetU(r,meta_id),nil,-1,@StrIComp);
  for r := 0 to tagsCount-1 do begin
    tag.Ident := tags[r];
    batch.Add(tag,true);
  end;
  Rest.BatchSend(batch,tagID);
  aTagsLookup.Init(Rest); // reload after initial fill
  batch.Reset(TSQLArticle,5000);
  tag_post_id := tagTable.FieldIndexExisting('post_id');
  T.SortFields(tag_post_id,true,nil,sftInteger);
  postTable := data.GetObjectByName('post') as TDotClearTable;
  postTable.SortFields('post_creadt',true,nil,sftDateTime);
  post_id := postTable.FieldIndexExisting('post_id');
  post_url := postTable.FieldIndexExisting('post_url');
  if postTable.Step(true) then
    repeat
      urls.Add(postTable.FieldBuffer(post_url));
    until not postTable.Step;
  article.Author := TSQLAuthor(1);
  article.AuthorName := 'synopse';
  article.ContentHtml := true;
  for r := 1 to postTable.RowCount do begin
    article.Title := postTable.GetU(r,'post_title');
    article.Abstract := FixLinks(postTable.Get(r,'post_excerpt_xhtml'));
    article.Content := FixLinks(postTable.Get(r,'post_content_xhtml'));
    if article.Abstract='' then begin
      article.Abstract := article.Content;
      article.Content := '';
    end;
    article.CreatedAt := Iso8601ToTimeLog(postTable.GetU(r,'post_creadt'));
    article.ModifiedAt := Iso8601ToTimeLog(postTable.GetU(r,'post_upddt'));
    article.SetPublishedMonth(article.CreatedAt);
    postID := postTable.GetAsInteger(r,post_id);
    article.Tags := nil;
    if tagTable.Step(true) then
      repeat
        if tagTable.FieldAsInteger(tag_post_id)=postID then begin
          ndx := FastFindPUTF8CharSorted(
            pointer(tags),high(tags),tagTable.FieldBuffer(meta_id),@StrIComp);
          if ndx>=0 then
            article.TagsAddOrdered(tagID[ndx],aTagsLookup);
        end;
      until not tagTable.Step;
    batch.Add(article,true,false,[],true);
  end;
  Rest.BatchSend(batch);
  aTagsLookup.SaveOccurence(Rest);
end;


end.

