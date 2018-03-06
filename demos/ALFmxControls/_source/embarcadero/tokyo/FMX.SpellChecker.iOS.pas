{*******************************************************}
{                                                       }
{             Delphi FireMonkey Platform                }
{ Copyright(c) 2013-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.SpellChecker.iOS;

interface

{$SCOPEDENUMS ON}

implementation

uses
  System.SysUtils, Macapi.ObjectiveC, Macapi.Helpers, iOSapi.UIKit, iOSapi.Foundation,
  FMX.Types, FMX.SpellChecker, FMX.Platform;

type
  TCocoaTouchSpellCheckerService = class(TInterfacedObject, IFMXSpellCheckerService)
  private
    FChecker: UITextChecker;
    FCurrentLanguage: NSString;
  public
    constructor Create;
    destructor Destroy; override;
    //IFMXSpellCheckerService
    function CheckSpelling(Word: string): TArray<string>;
    function GuessCompletions(Word: string): TArray<string>;
  end;

var
  CocoaSpellCheckerService: TCocoaTouchSpellCheckerService;

procedure RegisterSpellCheckerService;
begin
  CocoaSpellCheckerService := TCocoaTouchSpellCheckerService.Create;
  TPlatformServices.Current.AddPlatformService(IFMXSpellCheckerService, CocoaSpellCheckerService);
end;

procedure UnregisterSpellCheckerService;
begin
  TPlatformServices.Current.RemovePlatformService(IFMXSpellCheckerService);
  FreeAndNil(CocoaSpellCheckerService);
end;


{ TCocoaSpellCheckerService }

function TCocoaTouchSpellCheckerService.CheckSpelling(Word: string): TArray<string>;
var
  I: Integer;
  SpellWords: NSArray;
begin
  Assert(FChecker <> nil, 'Native bridge was not created');
  Result := nil;
  if FChecker.rangeOfMisspelledWordInString(StrToNSStr(Word), NSMakeRange(0, Word.Length), 0, False, FCurrentLanguage).location <> NSNotFound then
  begin
    SpellWords := FChecker.guessesForWordRange(NSMakeRange(0, Word.Length), StrToNSStr(Word), FCurrentLanguage);
    if SpellWords.count > 0 then
    begin
      SetLength(Result, SpellWords.count);
      for I := 0 to SpellWords.count - 1 do
        Result[I] := UTF8ToUnicodeString(TNSString.Wrap(SpellWords.objectAtIndex(I)).UTF8String);
    end;
  end;
end;

constructor TCocoaTouchSpellCheckerService.Create;
var
  Langs: NSArray;
begin
  inherited;
  FChecker := TUITextChecker.Create;
  Langs := TNSLocale.OCClass.preferredLanguages;
  if (Langs <> nil) and (Langs.count > 0) then
    FCurrentLanguage := TNSString.Wrap(Langs.objectAtIndex(0))
  else
    FCurrentLanguage := StrToNSStr('en_US');
end;

destructor TCocoaTouchSpellCheckerService.Destroy;
begin
  FChecker := nil;
  FCurrentLanguage := nil;
  inherited;
end;

function TCocoaTouchSpellCheckerService.GuessCompletions(
  Word: string): TArray<string>;
var
  I: Integer;
  Words: NSArray;
begin
  Assert(FChecker <> nil, 'Native bridge was not created');
  Result := nil;
  Words := FChecker.completionsForPartialWordRange(NSMakeRange(0, Word.Length), StrToNSStr(Word), FCurrentLanguage);
  if Words.count > 0 then
  begin
    SetLength(Result, Words.count);
    for I := 0 to Words.count - 1 do
      Result[I] := UTF8ToUnicodeString(TNSString.Wrap(Words.objectAtIndex(I)).UTF8String);
  end;
end;


initialization
  RegisterSpellCheckerService;
end.
