unit DelphiAST.SimpleParserEx;

interface

uses
  SysUtils, Generics.Collections, SimpleParser, SimpleParser.Lexer.Types,
  SimpleParser.Lexer, Classes;

type
  TStringEvent = procedure(var s: string) of object;

  TPasLexer = class
  private
    FLexer: TmwPasLex;
    FOnHandleString: TStringEvent;
    function GetToken: string; inline;
    function GetPosXY: TTokenPoint; inline;
    function GetFileName: string;
  public
    constructor Create(const ALexer: TmwPasLex; AOnHandleString: TStringEvent);
    property FileName: string read GetFileName;
    property PosXY: TTokenPoint read GetPosXY;
    property Token: string read GetToken;
    property Lexer: TmwPasLex read FLexer;
  end;

  TmwSimplePasParEx = class(TmwSimplePasPar)
  public type
    TNameListStack = class;
    TNameList = class
    public type
      TNameItem = class
      public type
        TNameItemToken = class
        strict private
          FTokenFileName: string;
          FTokenPoint: TTokenPoint;
          FTokenPos: Integer;
          FTokenLen: Integer;
        public
          constructor Create(const ATokenFileName: string;
            const ATokenPoint: TTokenPoint; const ATokenPos, ATokenLen: Integer);
          property TokenFileName: string read FTokenFileName;
          property TokenPoint: TTokenPoint read FTokenPoint;
          property TokenPos: Integer read FTokenPos;
          property TokenLen: Integer read FTokenLen;
        end;
      strict private
        FTokenList: TObjectList<TNameItemToken>;
        FEndNameCalled: Boolean;
        function GetLastNameItemToken: TNameItemToken;
      public
        constructor Create(const ATokenFileName: string;
          const ATokenPoint: TTokenPoint; const ATokenID: TptTokenKind;
          const ATokenPos, ATokenLen: Integer);
        destructor Destroy; override;
        procedure AddToken(const ATokenFileName: string;
          const ATokenPoint: TTokenPoint; const ATokenID: TptTokenKind;
          const ATokenPos, ATokenLen: Integer);
        property TokenList: TObjectList<TNameItemToken> read FTokenList;
        property LastNameItemToken: TNameItemToken read GetLastNameItemToken;
        property EndNameCalled: Boolean read FEndNameCalled write FEndNameCalled;
      end;
    strict private
      FParser: TmwSimplePasParEx;
      FNameItems: TObjectList<TNameItem>;
      FAutoCreated: Boolean;
      function GetItems(const Index: Integer): TNameItem; inline;
      function GetLastItem: TNameItem; inline;
      function GetOriginalNames(const Index: Integer): string;
      function GetNames(const Index: Integer): string;
      function GetLastOriginalName: string;
      function GetLastName: string;
      function GetCount: Integer; inline;
      function GetLexer: TmwPasLex; inline;
      property Lexer: TmwPasLex read GetLexer;
    public
      constructor Create(const AParser: TmwSimplePasParEx;
        const AAutoCreated: Boolean);
      destructor Destroy; override;
      procedure BeginName;
      procedure EndName;
      procedure AddToken;
      property AutoCreated: Boolean read FAutoCreated;
      property Items[const Index: Integer]: TNameItem read GetItems;
      property LastItem: TNameItem read GetLastItem;
      property OriginalNames[const Index: Integer]: string read GetOriginalNames;
      property Names[const Index: Integer]: string read GetNames; default;
      property LastOriginalName: string read GetLastOriginalName;
      property LastName: string read GetLastName;
      property Count: Integer read GetCount;
    end;
    TNameListStack = class
    strict private
      FParser: TmwSimplePasParEx;
      FNameListStack: TObjectStack<TNameList>;
    public
      constructor Create(const AParser: TmwSimplePasParEx);
      destructor Destroy; override;
      procedure PushNames(const AAutoCreated: Boolean); inline;
      procedure PopNames; inline;
      function ExtractNames: TNameList; inline;
      function PeekNames: TNameList; inline;
      function ToArray: TArray<TNameList>; inline;
      function Count: Integer; inline;
    end;
  strict private
    FNameListStack: TNameListStack;
    FPreviousNames: TNameList;
    FLexer: TPasLexer;
    FLowerCaseNames: Boolean;
    FOnHandleString: TStringEvent;
    function GetCurrentNames: TNameList; inline;
  strict protected
    procedure DoHandleString(var AString: string); inline;
    procedure PushNames; inline;
    procedure PopNames; inline;
    function PeekNames: TNameList; inline;
    procedure BeginName;
    procedure EndName;
    property CurrentNames: TNameList read GetCurrentNames;
    property PreviousNames: TNameList read FPreviousNames;
  protected
    procedure NextToken; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Lexer: TPasLexer read FLexer;
    property LowerCaseNames: Boolean read FLowerCaseNames write FLowerCaseNames;
    property OnHandleString: TStringEvent read FOnHandleString write FOnHandleString;
  end;

implementation

{ TPasLexer }

constructor TPasLexer.Create(const ALexer: TmwPasLex; AOnHandleString: TStringEvent);
begin
  inherited Create;
  FLexer := ALexer;
  FOnHandleString := AOnHandleString;
end;

function TPasLexer.GetFileName: string;
begin
  Result := FLexer.Buffer.FileName;
end;

function TPasLexer.GetPosXY: TTokenPoint;
begin
  Result := FLexer.PosXY;
end;

function TPasLexer.GetToken: string;
begin
  Result := FLexer.Token;
  FOnHandleString(Result);
end;

{ TmwSimplePasParEx.TNameList.TNameItem.TNameItemToken }

constructor TmwSimplePasParEx.TNameList.TNameItem.TNameItemToken.Create(
  const ATokenFileName: string; const ATokenPoint: TTokenPoint;
  const ATokenPos, ATokenLen: Integer);
begin
  FTokenFileName := ATokenFileName;
  FTokenPoint := ATokenPoint;
  FTokenPos := ATokenPos;
  FTokenLen := ATokenLen;
end;

{ TPasNamesBuilder.TNamesList.TNameItem }

constructor TmwSimplePasParEx.TNameList.TNameItem.Create(
  const ATokenFileName: string; const ATokenPoint: TTokenPoint;
  const ATokenID: TptTokenKind; const ATokenPos, ATokenLen: Integer);
begin
  FTokenList := TObjectList<TNameItemToken>.Create(True);
  AddToken(ATokenFileName, ATokenPoint, ATokenID, ATokenPos, ATokenLen);
end;

destructor TmwSimplePasParEx.TNameList.TNameItem.Destroy;
begin
  FTokenList.Free;
  inherited;
end;

procedure TmwSimplePasParEx.TNameList.TNameItem.AddToken(
  const ATokenFileName: string; const ATokenPoint: TTokenPoint;
  const ATokenID: TptTokenKind; const ATokenPos, ATokenLen: Integer);
begin
  if not IsTokenIDJunk(ATokenID) and
      ((FTokenList.Count = 0) or (FTokenList.Last.TokenPos < ATokenPos)) then
    FTokenList.Add(TNameItemToken.Create(
      ATokenFileName, ATokenPoint, ATokenPos, ATokenLen));
end;

function TmwSimplePasParEx.TNameList.TNameItem.GetLastNameItemToken: TNameItemToken;
begin
  Result := TokenList.Last;
end;

{ TPasNamesBuilder.TNamesList }

constructor TmwSimplePasParEx.TNameList.Create(const AParser: TmwSimplePasParEx;
  const AAutoCreated: Boolean);
begin
  FParser := AParser;
  FNameItems := TObjectList<TNameItem>.Create(True);
end;

destructor TmwSimplePasParEx.TNameList.Destroy;
begin
  FNameItems.Free;
  inherited;
end;

function TmwSimplePasParEx.TNameList.GetLexer: TmwPasLex;
begin
  Result := FParser.Lexer.Lexer;
end;

procedure TmwSimplePasParEx.TNameList.BeginName;
begin
  FNameItems.Add(TNameItem.Create(Lexer.FileName, Lexer.PosXY, Lexer.TokenID,
    Lexer.TokenPos, Lexer.TokenLen));
end;

procedure TmwSimplePasParEx.TNameList.EndName;
begin
  FNameItems.Last.EndNameCalled := True;
end;

procedure TmwSimplePasParEx.TNameList.AddToken;
begin
  FNameItems.Last.AddToken(Lexer.FileName, Lexer.PosXY, Lexer.TokenID,
    Lexer.TokenPos, Lexer.TokenLen);
end;

function TmwSimplePasParEx.TNameList.GetItems(const Index: Integer): TNameItem;
begin
  Result := FNameItems[Index];
end;

function TmwSimplePasParEx.TNameList.GetLastItem: TNameItem;
begin
  Result := FNameItems.Last;
end;

function TmwSimplePasParEx.TNameList.GetOriginalNames(const Index: Integer): string;
var
  I: Integer;
  NameItem: TNameItem;
  Token: string;
begin
  Result := '';
  NameItem := Items[Index];
  for I := 0 to NameItem.TokenList.Count - 1 do
  begin
    SetString(Token, Lexer.Buffer.Buf + NameItem.TokenList[I].TokenPos,
      NameItem.TokenList[I].TokenLen);
    Result := Result + Token;
  end;
  FParser.DoHandleString(Result);
end;

function TmwSimplePasParEx.TNameList.GetNames(const Index: Integer): string;
begin
  Result := OriginalNames[Index];
  if FParser.LowerCaseNames then
  begin
    Result := AnsiLowerCase(Result);
    FParser.DoHandleString(Result);
  end;
end;

function TmwSimplePasParEx.TNameList.GetLastOriginalName: string;
begin
  Result := OriginalNames[Count - 1];
end;

function TmwSimplePasParEx.TNameList.GetLastName: string;
begin
  Result := Names[Count - 1];
end;

function TmwSimplePasParEx.TNameList.GetCount: Integer;
begin
  Result := FNameItems.Count;
end;

{ TPasNamesBuilder.TNameListStack }

function TmwSimplePasParEx.TNameListStack.Count: Integer;
begin
  Result := FNameListStack.Count;
end;

constructor TmwSimplePasParEx.TNameListStack.Create(
  const AParser: TmwSimplePasParEx);
begin
  FParser := AParser;
  FNameListStack := TObjectStack<TNameList>.Create(True);
end;

destructor TmwSimplePasParEx.TNameListStack.Destroy;
begin
  FNameListStack.Free;
  inherited;
end;

procedure TmwSimplePasParEx.TNameListStack.PushNames(const AAutoCreated: Boolean);
begin
  FNameListStack.Push(TNameList.Create(FParser, AAutoCreated));
end;

procedure TmwSimplePasParEx.TNameListStack.PopNames;
begin
  FNameListStack.Pop;
end;

function TmwSimplePasParEx.TNameListStack.ExtractNames: TNameList;
begin
  Result := FNameListStack.Extract;
end;

function TmwSimplePasParEx.TNameListStack.PeekNames: TNameList;
begin
  Result := FNameListStack.Peek;
end;

function TmwSimplePasParEx.TNameListStack.ToArray: TArray<TNameList>;
begin
  Result := FNameListStack.ToArray;
end;

{ TmwSimplePasParEx }

constructor TmwSimplePasParEx.Create;
begin
  inherited;
  FNameListStack := TNameListStack.Create(Self);
  FNameListStack.PushNames(True);
  FPreviousNames := TNameList.Create(Self, True);
  FLexer := TPasLexer.Create(inherited Lexer, DoHandleString);
  FLowerCaseNames := True;
end;

destructor TmwSimplePasParEx.Destroy;
begin
  FLexer.Free;
  FPreviousNames.Free;
  FNameListStack.PopNames;
  FNameListStack.Free;
  inherited;
end;

procedure TmwSimplePasParEx.NextToken;
var
  NameList: TNameList;
begin
  if FNameListStack.Count > 0 then
    for NameList in FNameListStack.ToArray do
      if (NameList.Count > 0) and not NameList.LastItem.EndNameCalled then
        NameList.AddToken;
  inherited;
end;

procedure TmwSimplePasParEx.PushNames;
begin
  FNameListStack.PushNames(False);
end;

procedure TmwSimplePasParEx.PopNames;
begin
  FPreviousNames.Free;
  FPreviousNames := FNameListStack.ExtractNames;
end;

function TmwSimplePasParEx.PeekNames: TNameList;
begin
  Result := FNameListStack.PeekNames;
end;

procedure TmwSimplePasParEx.BeginName;
var
  NameList: TNameList;
begin
  NameList := FNameListStack.PeekNames;
  if (NameList.Count > 0) and not NameList.LastItem.EndNameCalled then
  begin
    FNameListStack.PushNames(True);
    NameList := FNameListStack.PeekNames;
  end;
  NameList.BeginName;
end;

procedure TmwSimplePasParEx.EndName;
var
  NameList: TNameList;
begin
  NameList := FNameListStack.PeekNames;
  if NameList.LastItem.EndNameCalled then
  begin
    FNameListStack.PopNames;
    NameList := FNameListStack.PeekNames;
  end;
  NameList.EndName;
end;

procedure TmwSimplePasParEx.DoHandleString(var AString: string);
begin
  if Assigned(FOnHandleString) then
    FOnHandleString(AString);
end;

function TmwSimplePasParEx.GetCurrentNames: TNameList;
begin
  Result := PeekNames;
end;

end.
