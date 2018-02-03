unit JclStringsDonations;

interface

uses
  Classes, JclStrings, JclSysUtils;

const
  AnsiQuoteChars  = AnsiString('"''');

{ TODO : Author: Mario R. Carro <ochnap2@yahoo.com.ar> }
procedure StrToStrings(S, Sep: AnsiString; const List: TStrings;
  const AllowEmptyString: Boolean = True; const QuoteChars: String = '';
  const CaseSensitive: Boolean = True); overload;

{ TODO : Author: Mario R. Carro <ochnap2@yahoo.com.ar> }
procedure StrIToStrings(S, Sep: AnsiString; const List: TStrings;
  const AllowEmptyString: Boolean = True; const QuoteChars: String = '');

{ TODO : Author: Peter Panino <peter-panino@aon.at> }
function MyStrToken(var S: AnsiString; Separator: AnsiString): AnsiString;
{ TODO : Author: Peter Panino <peter-panino@aon.at> }
procedure StrTokenToStrings(S, Separator: AnsiString; const List: TStrings); overload;

implementation

//--------------------------------------------------------------------------------------------------

procedure StrToStrings(S, Sep: AnsiString; const List: TStrings;
  const AllowEmptyString: Boolean = True; const QuoteChars: String = '';
  const CaseSensitive: Boolean = True);
var
  FindNextStr: function (const Substr, S: AnsiString; const Index: Integer): Integer;
  A, B, L, LS, LQ, Q: Integer;

  function FindNextQuote(From: Integer): Integer;
  var
    I, QI: Integer;
  begin
    Result := 0;
    if Q < 0 then
    begin
      // Find the nearest opening quote.
      for I := 1 to LQ do
      begin
        QI := FindNextStr(QuoteChars[I], S, From + 1);
        if (QI > 0) and ((Result = 0) or (QI < Result)) then
          Result := QI;
      end;
    end
    else
    begin
      // Find the next quote of an item (closing o nested).
      QI := FindNextStr(S[Q], S, From + 1);
      if QI > 0 then
        Result := QI;
    end;
  end;

begin
  Assert(List <> nil);

  List.Clear;

  if S = '' then
    Exit;

  if CaseSensitive then
    FindNextStr := StrSearch
  else
    FindNextStr := StrFind;

  L := Length(Sep);
  LS := Length(S);
  LQ := Length(QuoteChars);

  // Q = -1 means no quote found so far,
  // but need to check for them.
  // Q = 0 means don't bother about quotes,
  // or, in the repeat, no more quotes.
  Q := iff(LQ > 0, -1, 0);

  B := -L;
  repeat
    A := B + L + 1;
    B := FindNextStr(Sep, S, A) - 1;
    if B < 0 then B := LS;

    // Need to check for (more) quotes?
    if (Q <> 0) and (Q < B) then
    begin
      if Q < 0 then
        Q := FindNextQuote(A);

      if (Q > 0) and (Q < B) then
      begin
        Q := FindNextQuote(Q);

        // Jump over nested quotes.
        while (Q < LS) and (S[Q + 1] = S[Q]) do
          Q := FindNextQuote(Q);

        B := FindNextStr(Sep, S, Q + 1) - 1;
        if B < 0 then B := LS;
        Q := -1;
      end;
    end;

    List.Add(Copy(S, A, B - A + 1));
  until B = LS;
end;

//--------------------------------------------------------------------------------------------------

procedure StrIToStrings(S, Sep: AnsiString; const List: TStrings;
  const AllowEmptyString: Boolean = True; const QuoteChars: String = '');
begin
  StrToStrings(S, Sep, List, AllowEmptyString, QuoteChars, False);
end;

//--------------------------------------------------------------------------------------------------

function MyStrToken(var S: AnsiString; Separator: AnsiString): AnsiString;
var
  I: Integer;
begin
  I := Pos(Separator, S);
  if I <> 0 then
  begin
    Result := Copy(S, 1, I - 1);
    Delete(S, 1, I + Length(Separator) - 1);
  end
  else
  begin
    Result := S;
    S := '';
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure StrTokenToStrings(S, Separator: AnsiString; const List: TStrings);
var
  Token: AnsiString;
begin
  Assert(List <> nil);

  if List = nil then
    Exit;

  List.Clear;
  while S <> '' do
  begin
    Token := MyStrToken(S, Separator);
    if Token <> '' then {if S starts with Separator}
      List.Add(Token);
  end;
end;

end.
