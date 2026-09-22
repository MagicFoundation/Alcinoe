unit DelphiAST.ProjectIndexer;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  Classes, Generics.Defaults, Generics.Collections,
  SimpleParser.Lexer.Types,
  DelphiAST, DelphiAST.Classes, DelphiAST.Consts;

type
  TProjectIndexer = class
  strict private type
    TParsedUnitsCache = TObjectDictionary<string,TSyntaxNode>;
    TUnitPathsCache = TDictionary<string,string>;

    TIncludeInfo = record
      FileName: string;
      Content : string;
    end;

    TIncludeCache = TDictionary<string,TIncludeInfo>;

  public type
    TOption = (piUseDefinesDefinedByCompiler);
    TOptions = set of TOption;
    TGetUnitSyntaxEvent = procedure (Sender: TObject; const fileName: string;
      var syntaxTree: TSyntaxNode; var doParseUnit, doAbort: boolean) of object;
    TUnitParsedEvent = procedure (Sender: TObject; const unitName: string; const fileName: string;
      var syntaxTree: TSyntaxNode; syntaxTreeFromParser: boolean; var doAbort: boolean) of object;

    TUnitInfo = record
      Name: string;
      Path: string;
      SyntaxTree: TSyntaxNode;
      HasError: boolean;
      ErrorInfo: record
        Line: integer;
        Col: integer;
        Error: string;
      end;
    end;

    TParsedUnits = class(TList<TUnitInfo>)
    protected
      procedure Initialize(parsedUnits: TParsedUnitsCache; unitPaths: TUnitPathsCache);
    end;

    TIncludeFileInfo = record
      Name: string;
      Path: string;
    end;

    TIncludeFiles = class(TList<TIncludeFileInfo>)
    protected
      procedure Initialize(includeCache: TIncludeCache);
    end;

    TProblemType = (ptCantFindFile, ptCantOpenFile, ptCantParseFile);
    TProblemInfo = record
      ProblemType: TProblemType;
      FileName   : string;
      Description: string;
    end;

    TProblems = class(TList<TProblemInfo>)
    protected
      procedure LogProblem(problemType: TProblemType; const fileName, description: string);
    end;

  strict private type
    TIncludeHandler = class(TInterfacedObject, IIncludeHandler)
    strict private
      [weak] FIncludeCache: TIncludeCache;
      [weak] FIndexer     : TProjectIndexer;
      [weak] FProblems    : TProblems;
      FUnitFile           : string;
      FUnitFileFolder     : string;
    public
      constructor Create(indexer: TProjectIndexer; includeCache: TIncludeCache;
        problemList: TProblems; const currentFile: string);
      function  GetIncludeFileContent(const ParentFileName, FileName: string; out Content: string;
        out filePath: string): Boolean;
    end;

  var
    FAborting       : boolean;
    FDefines        : string;
    FDefinesList    : TStringList;
    FIncludeCache   : TIncludeCache;
    FIncludeFiles   : TIncludeFiles;
    FNotFoundUnits  : TStringList;
    FOnGetUnitSyntax: TGetUnitSyntaxEvent;
    FOnUnitParsed   : TUnitParsedEvent;
    FOptions        : TOptions;
    FParsedUnits    : TParsedUnitsCache;
    FParsedUnitsInfo: TParsedUnits;
    FProblems       : TProblems;
    FProjectFolder  : string;
    FSearchPath     : string;
    FSearchPaths    : TStringList;
    FUnitPaths      : TUnitPathsCache;
  strict protected
    procedure AppendUnits(usesNode: TSyntaxNode; const filePath: string; unitList: TStrings);
    procedure BuildUsesList(unitNode: TSyntaxNode; const fileName: string; isProject: boolean;
      unitList: TStringList);
    function  FindType(node: TSyntaxNode; nodeType: TSyntaxNodeType): TSyntaxNode;
    procedure GetUnitSyntax(const fileName: string; var syntaxTree: TSyntaxNode; var
      doParseUnit: boolean);
    procedure NotifyUnitParsed(const unitName, fileName: string; var syntaxTree: TSyntaxNode;
      syntaxTreeFromParser: boolean);
    procedure ParseUnit(const unitName: string; const fileName: string; isProject: boolean);
    procedure PrepareSearchPath;
    procedure PrepareDefines;
    procedure RunParserOnUnit(const fileName: string; var syntaxTree: TSyntaxNode);
    procedure ScanUsedUnits(const unitName, fileName: string; isProject: boolean;
      syntaxTree: TSyntaxNode; syntaxTreeFromParser: boolean);
  protected
    function  FindFile(const fileName: string; relativeToFolder: string; var filePath: string): boolean;
    class function SafeOpenFileStream(const fileName: string; var fileStream: TStringStream;
      var errorMsg: string): boolean;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Index(const fileName: string);
    property Defines: string read FDefines write FDefines;
    property Options: TOptions read FOptions write FOptions default [piUseDefinesDefinedByCompiler];
    property ParsedUnits: TParsedUnits read FParsedUnitsInfo;
    property IncludeFiles: TIncludeFiles read FIncludeFiles;
    property Problems: TProblems read FProblems;
    property NotFoundUnits: TStringList read FNotFoundUnits;
    property SearchPath: string read FSearchPath write FSearchPath;
    property OnGetUnitSyntax: TGetUnitSyntaxEvent read FOnGetUnitSyntax write FOnGetUnitSyntax;
    property OnUnitParsed: TUnitParsedEvent read FOnUnitParsed write FOnUnitParsed;
  end;

implementation

uses
  SysUtils,
  SimpleParser;

{ TProjectIndexer.TParsedUnits }

procedure TProjectIndexer.TParsedUnits.Initialize(parsedUnits: TParsedUnitsCache;
  unitPaths: TUnitPathsCache);
var
  info    : TUnitInfo;
  kv      : TPair<string,TSyntaxNode>;
  unitPath: string;
begin
  Clear;
  Capacity := parsedUnits.Count;

  for kv in parsedUnits do begin
    if not assigned(kv.Value) then
      continue; //for kv
    info.Name := kv.Key;
    info.SyntaxTree := kv.Value;
    if not (unitPaths.TryGetValue(kv.Key + '.pas', unitPath)
            or unitPaths.TryGetValue(kv.Key + '.dpr', unitPath))
    then
      unitPath := '';
    info.Path := unitPath;
    info.HasError := false; // TODO 1 -oPrimoz Gabrijelcic : fix that
    Add(info);
  end;

  TrimExcess;
  Sort(
    TComparer<TUnitInfo>.Construct(
      function(const Left, Right: TUnitInfo): integer
      begin
        Result := TOrdinalIStringComparer(TIStringComparer.Ordinal).Compare(Left.Name, Right.Name);
      end));
end;

{ TProjectIndexer.TIncludeFiles }

procedure TProjectIndexer.TIncludeFiles.Initialize(includeCache: TIncludeCache);
var
  info: TIncludeFileInfo;
  kv  : TPair<string,TIncludeInfo>;
  p   : integer;
begin
  Clear;
  Capacity := includeCache.Count;

  for kv in includeCache do begin
    p := Pos(#13, kv.Key);
    if p = 0 then
      continue; //for kv

    info.Name := Copy(kv.Key, 1, p-1);
    info.Path := kv.Value.FileName;
    Add(info);
  end;

  TrimExcess;
  Sort(
    TComparer<TIncludeFileInfo>.Construct(
      function(const Left, Right: TIncludeFileInfo): integer
      begin
        Result := TOrdinalIStringComparer(TIStringComparer.Ordinal).Compare(Left.Name, Right.Name);
      end));
end;

{ TProjectIndexer.TProblems }

procedure TProjectIndexer.TProblems.LogProblem(problemType: TProblemType; const fileName,
  description: string);
var
  info: TProblemInfo;
begin
  info.ProblemType := problemType;
  info.FileName := fileName;
  info.Description := description;
  Add(info);
end;

{ TProjectIndexer }

procedure TProjectIndexer.AppendUnits(usesNode: TSyntaxNode; const filePath: string;
  unitList: TStrings);
var
  childNode: TSyntaxNode;
  unitName : string;
  unitPath : string;
begin
  for childNode in usesNode.ChildNodes do
    if childNode.Typ = ntUnit then begin
      unitName := childNode.GetAttribute(anName);
      unitList.Add(unitName);
      if not FUnitPaths.ContainsKey(unitName) then begin
        unitPath := childNode.GetAttribute(anPath);
        if unitPath <> '' then begin
          if IsRelativePath(unitPath) then
            unitPath := filePath + unitPath;
          FUnitPaths.Add(unitName + '.pas', unitPath);
        end;
      end;
    end;
end;

procedure TProjectIndexer.BuildUsesList(unitNode: TSyntaxNode; const fileName: string;
  isProject: boolean; unitList: TStringList);
var
  fileFolder: string;
  implNode  : TSyntaxNode;
  intfNode  : TSyntaxNode;
  usesNode  : TSyntaxNode;
begin
  fileFolder := IncludeTrailingPathDelimiter(ExtractFilePath(fileName));
  if isProject then begin
    usesNode := FindType(unitNode, ntUses);
    if assigned(usesNode) then
      AppendUnits(usesNode, fileFolder, unitList);
    usesNode := FindType(unitNode, ntContains);
    if assigned(usesNode) then
      AppendUnits(usesNode, fileFolder, unitList);
  end
  else begin
    intfNode := FindType(unitNode, ntInterface);
    if assigned(intfNode) then begin
      usesNode := FindType(intfNode, ntUses);
      if assigned(usesNode) then
        AppendUnits(usesNode, fileFolder, unitList);
    end;
    implNode := FindType(unitNode, ntImplementation);
    if assigned(implNode) then begin
      usesNode := FindType(implNode, ntUses);
      if assigned(usesNode) then
        AppendUnits(usesNode, fileFolder, unitList);
    end;
  end;
end;

constructor TProjectIndexer.Create;
begin
  inherited Create;
  FOptions := [piUseDefinesDefinedByCompiler];
  FSearchPaths := TStringList.Create;
  FSearchPaths.Delimiter := ';';
  FSearchPaths.StrictDelimiter := true;
  FDefinesList := TStringList.Create;
  FDefinesList.Delimiter := ';';
  FDefinesList.StrictDelimiter := true;
  FParsedUnits := TParsedUnitsCache.Create([doOwnsValues], TIStringComparer.Ordinal);
  FParsedUnitsInfo := TParsedUnits.Create;
  FIncludeFiles := TIncludeFiles.Create;
  FNotFoundUnits := TStringList.Create;
  FNotFoundUnits.Sorted := true;
  FNotFoundUnits.Duplicates := dupIgnore;
  FProblems := TProblems.Create;
end;

destructor TProjectIndexer.Destroy;
begin
  FreeAndNil(FProblems);
  FreeAndNil(FNotFoundUnits);
  FreeAndNil(FIncludeFiles);
  FreeAndNil(FParsedUnitsInfo);
  FreeAndNil(FDefinesList);
  FreeAndNil(FParsedUnits);
  FreeAndNil(FSearchPaths);
  inherited;
end;

function TProjectIndexer.FindFile(const fileName: string; relativeToFolder: string;
  var filePath: string): boolean;
var
  fName     : string;
  searchPath: string;

  function FilePresent(const testFile: string): boolean;
  begin
    Result := FileExists(testFile);
    if Result then begin
      filePath := ExpandFileName(testFile);
      FUnitPaths.Add(fName, filePath);
    end;
  end;

begin
  Result := true;
  fName := fileName.DeQuotedString;

  if FUnitPaths.TryGetValue(fName, filePath) then
    Exit;

  if relativeToFolder <> '' then
    if FilePresent(relativeToFolder + fName) then
      Exit;

  if FilePresent(FProjectFolder + fName) then
    Exit;

  for searchPath in FSearchPaths do
    if FilePresent(searchPath + fName) then
      Exit;

  if SameText(ExtractFileExt(fileName), '.pas') then
    Result := false
  else
    Result := FindFile(fileName + '.pas', relativeToFolder, filePath);

  if (not Result) and (relativeToFolder = '') {ignore include files} then
    FNotFoundUnits.Add(fName);
end;

function TProjectIndexer.FindType(node: TSyntaxNode; nodeType: TSyntaxNodeType):
  TSyntaxNode;
begin
  if node.Typ = nodeType then
    Exit(node)
  else
    Result := node.FindNode(nodeType);
end;

procedure TProjectIndexer.GetUnitSyntax(const fileName: string; var syntaxTree:
  TSyntaxNode; var doParseUnit: boolean);
var
  doAbort: boolean;
begin
  doAbort := false;
  doParseUnit := true;
  syntaxTree := nil;
  if assigned(OnGetUnitSyntax) then begin
    OnGetUnitSyntax(Self, fileName, syntaxTree, doParseUnit, doAbort);
    if doAbort then
      FAborting := true;
  end;
end;

procedure TProjectIndexer.Index(const fileName: string);
var
  filePath   : string;
  projectName: string;
begin
  FAborting := false;
  FParsedUnits.Clear;
  FProjectFolder := IncludeTrailingPathDelimiter(ExtractFilePath(fileName));
  FIncludeCache := TIncludeCache.Create;
  try
    FUnitPaths := TUnitPathsCache.Create(TIStringComparer.Ordinal);
    try
      PrepareDefines;
      PrepareSearchPath;
      FNotFoundUnits.Clear;
      FProblems.Clear;
      filePath := ExpandFileName(fileName);
      projectName := ChangeFileExt(ExtractFileName(fileName), '');
      FUnitPaths.Add(projectName + '.dpr', fileName);
      ParseUnit(projectName, filePath, true);
      FParsedUnitsInfo.Initialize(FParsedUnits, FUnitPaths);
      FIncludeFiles.Initialize(FIncludeCache);
    finally FreeAndNil(FUnitPaths); end;
  finally FreeAndNil(FIncludeCache); end;
end;

procedure TProjectIndexer.NotifyUnitParsed(const unitName, fileName: string; var
  syntaxTree: TSyntaxNode; syntaxTreeFromParser: boolean);
var
  doAbort: boolean;
begin
  if assigned(OnUnitParsed) then begin
    doAbort := false;
    OnUnitParsed(Self, unitName, fileName, syntaxTree, syntaxTreeFromParser, doAbort);
    if doAbort then
      FAborting := true;
  end;
end;

procedure TProjectIndexer.ParseUnit(const unitName: string; const fileName: string; isProject: boolean);
var
  doParseUnit: boolean;
  syntaxTree : TSyntaxNode;
begin
  if FAborting then
    Exit;

  GetUnitSyntax(fileName, syntaxTree, doParseUnit);

  if FAborting then
    Exit;

  if doParseUnit then
    RunParserOnUnit(fileName, syntaxTree);

  FParsedUnits.Add(unitName, syntaxTree);

  if (not FAborting) and assigned(syntaxTree) then
    ScanUsedUnits(unitName, fileName, isProject, syntaxTree, doParseUnit);
end;

procedure TProjectIndexer.PrepareSearchPath;
var
  iPath: integer;
  sPath: string;
begin
  FSearchPaths.DelimitedText := SearchPath;
  for iPath := 0 to FSearchPaths.Count - 1 do begin
    sPath := FSearchPaths[iPath];
    if IsRelativePath(sPath) then
      sPath := FProjectFolder + sPath;
    FSearchPaths[iPath] := IncludeTrailingPathDelimiter(sPath);
  end;
end;

class function TProjectIndexer.SafeOpenFileStream(const fileName: string; var fileStream:
  TStringStream; var errorMsg: string): boolean;
var
  buf       : TBytes;
  encoding  : TEncoding;
  readStream: TStream;
begin
  Result := true;
  try
    readStream := TFileStream.Create(fileName, fmOpenRead or fmShareDenyWrite);
  except
    on E: EFCreateError do begin
      errorMsg := E.Message;
      Result := false;
    end;
    on E: EFOpenError do begin
      errorMsg := E.Message;
      Result := false;
    end;
  end;

  if Result then try
    SetLength(buf, 4);
    SetLength(buf, readStream.Read(buf[0], Length(buf)));
    encoding := nil;
    readStream.Position := TEncoding.GetBufferEncoding(buf, encoding);
    fileStream := TStringStream.Create('', encoding);
    fileStream.CopyFrom(readStream, readStream.Size - readStream.Position);
  finally FreeAndNil(readStream); end;
end;

procedure TProjectIndexer.PrepareDefines;
begin
  FDefinesList.DelimitedText := FDefines;
end;

procedure TProjectIndexer.RunParserOnUnit(const fileName: string; var syntaxTree:
  TSyntaxNode);
var
  builder   : TPasSyntaxTreeBuilder;
  define    : string;
  errorMsg  : string;
  fileStream: TStringStream;
begin
  if not SafeOpenFileStream(fileName, fileStream, errorMsg) then
    FProblems.LogProblem(ptCantOpenFile, fileName, errorMsg)
  else try
    builder := TPasSyntaxTreeBuilder.Create;
    try
      builder.IncludeHandler := TIncludeHandler.Create(Self, FIncludeCache, FProblems, fileName);
      if piUseDefinesDefinedByCompiler in Options then
        builder.InitDefinesDefinedByCompiler;
      for define in FDefinesList do
        TmwSimplePasPar(builder).Lexer.AddDefine(define);
      try
        syntaxTree := builder.Run(fileStream);
      except
        on E: ESyntaxTreeException do begin
          FProblems.LogProblem(ptCantParseFile, fileName,
            Format('Line %d, Column %d: %s', [E.Line, E.Col, E.Message]));
        end;
      end;
    finally FreeAndNil(builder); end;
  finally FreeAndNil(fileStream); end;
end; { TProjectIndexer.RunParserOnUnit }

procedure TProjectIndexer.ScanUsedUnits(const unitName, fileName: string; isProject: boolean;
  syntaxTree: TSyntaxNode; syntaxTreeFromParser: boolean);
var
  unitList: TStringList;
  unitNode: TSyntaxNode;
  usesName: string;
  usesPath: string;
begin
  unitNode := FindType(syntaxTree, ntUnit);
  if not assigned(unitNode) then
    Exit;

  unitList := TStringList.Create;
  try
    BuildUsesList(unitNode, fileName, isProject, unitList);

    NotifyUnitParsed(unitName, fileName, syntaxTree, syntaxTreeFromParser);

    for usesName in unitList do begin
      if FAborting then
        Exit;
      if not FParsedUnits.ContainsKey(usesName) then begin
        if FindFile(usesName + '.pas', '', usesPath) then
          ParseUnit(usesName, usesPath, false)
        else
          FParsedUnits.Add(usesName, nil);
      end;
    end;
  finally FreeAndNil(unitList); end;
end;

{ TProjectIndexer.TIncludeHandler }

constructor TProjectIndexer.TIncludeHandler.Create(indexer: TProjectIndexer;
  includeCache: TIncludeCache; problemList: TProblems; const currentFile: string);
begin
  inherited Create;
  FIndexer := indexer;
  FIncludeCache := includeCache;
  FProblems := problemList;
  FUnitFileFolder := IncludeTrailingPathDelimiter(ExtractFilePath(currentFile));
  FUnitFile := ChangeFileExt(ExtractFileName(currentFile), '');
end;

function TProjectIndexer.TIncludeHandler.GetIncludeFileContent(
  const ParentFileName, fileName: string; out Content: string; out filePath: string): Boolean;
var
  errorMsg   : string;
  fileStream : TStringStream;
  fName      : string;
  includeInfo: TIncludeInfo;
  key        : string;
begin
  if fileName.StartsWith('*.') then
    fName := FUnitFile + fileName.Remove(0 {0-based}, 1)
  else if fileName.Contains('*') then
    fName := fileName.Replace('*', '', [rfReplaceAll])
  else
    fName := fileName;

  key := fName + #13 + FUnitFileFolder;
  if FIncludeCache.TryGetValue(key, includeInfo) then
  begin
    Content := includeInfo.Content;
    filePath := includeInfo.FileName;
    Exit(True);
  end;

  if not FIndexer.FindFile(fName, FUnitFileFolder, filePath) then begin
    FProblems.LogProblem(ptCantFindFile, fName, 'Source folder: ' + FUnitFileFolder);
    includeInfo.FileName := '';
    includeInfo.Content := '';
    FIncludeCache.Add(key, includeInfo);
    Exit(False);
  end;

  if FIncludeCache.TryGetValue(filePath, includeInfo) then
  begin
    Content := includeInfo.Content;
    Exit(True);
  end;

  if not TProjectIndexer.SafeOpenFileStream(filePath, fileStream, errorMsg) then begin
    FProblems.LogProblem(ptCantOpenFile, filePath, errorMsg);
    Result := False;
  end
  else try
    Content := fileStream.DataString;
    Result := True;
  finally FreeAndNil(fileStream); end;

  includeInfo.FileName := filePath;
  includeInfo.Content := Content;
  FIncludeCache.Add(fName + #13 + FUnitFileFolder, includeInfo);
  includeInfo.FileName := '';
  FIncludeCache.Add(filePath, includeInfo);
end;

end.
