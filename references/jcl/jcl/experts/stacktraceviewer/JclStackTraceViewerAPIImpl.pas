{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JclStackTraceViewerAPIImpl.pas.                                             }
{                                                                                                  }
{ The Initial Developer of the Original Code is Uwe Schuster.                                      }
{ Portions created by Uwe Schuster are Copyright (C) 2009 Uwe Schuster. All rights reserved.       }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Uwe Schuster (uschuster)                                                                       }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclStackTraceViewerAPIImpl;

{$I jcl.inc}

interface

uses
  Classes, ActiveX,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclStackTraceViewerAPI;

type
  TJclLineNumberTranslators = class(TObject)
  private
    FIndexList: TList;
    FNextIndex: Integer;
    FTranslators: TInterfaceList;
    function GetCount: Integer;
    function GetItems(AIndex: Integer): IJclLineNumberTranslator;
  public
    constructor Create;
    destructor Destroy; override;
    function RegisterTranslator(const ATranslator: IJclLineNumberTranslator): Integer;
    function TranslateLineNumbers(ARevisionContent, ACurrentContent: IStream;
      ARevisionLineNumbers: TList; ACurrentLineNumbers: TList): Integer;
    procedure UnregisterTranslator(AIndex: Integer);
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: IJclLineNumberTranslator read GetItems; default;
  end;

  TJclRevisionProviders = class(TObject)
  private
    FIndexList: TList;
    FNextIndex: Integer;
    FTranslators: TInterfaceList;
    function GetCount: Integer;
    function GetItems(AIndex: Integer): IJclRevisionProvider;
  public
    constructor Create;
    destructor Destroy; override;
    function RegisterProvider(const ATranslator: IJclRevisionProvider): Integer;
    function GetRevisionContent(const AFileName, ARevision: string; AContent: IStream): Boolean;
    procedure UnregisterProvider(AIndex: Integer);
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: IJclRevisionProvider read GetItems; default;
  end;

var
  LineNumberTranslators: TJclLineNumberTranslators;
  RevisionProviders: TJclRevisionProviders;

function TranslateLineNumbers(ARevisionContent, ACurrentContent: IStream; ARevisionLineNumbers: TList; ACurrentLineNumbers: TList): Integer;
function GetRevisionContent(const AFileName, ARevision: string; AContent: IStream): Boolean;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\experts\stacktraceviewer';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

//=== { TJclLineNumberTranslators } ==========================================

constructor TJclLineNumberTranslators.Create;
begin
  inherited Create;
  FNextIndex := 1;
  FIndexList := TList.Create;
  FTranslators := TInterfaceList.Create;
end;

destructor TJclLineNumberTranslators.Destroy;
begin
  FTranslators.Free;
  FIndexList.Free;
  inherited Destroy;
end;

function TJclLineNumberTranslators.GetCount: Integer;
begin
  Result := FTranslators.Count;
end;

function TJclLineNumberTranslators.GetItems(AIndex: Integer): IJclLineNumberTranslator;
begin
  Result := IJclLineNumberTranslator(FTranslators[AIndex]);
end;

function TJclLineNumberTranslators.RegisterTranslator(const ATranslator: IJclLineNumberTranslator): Integer;
begin
  if Assigned(ATranslator) then
  begin
    Result := FNextIndex;
    Inc(FNextIndex);
    FTranslators.Add(ATranslator);
    FIndexList.Add(Pointer(Result));
  end
  else
    Result := -1;
end;

function TJclLineNumberTranslators.TranslateLineNumbers(ARevisionContent, ACurrentContent: IStream;
  ARevisionLineNumbers: TList; ACurrentLineNumbers: TList): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
  begin
    Result := Items[I].TranslateLineNumbers(ARevisionContent, ACurrentContent, ARevisionLineNumbers, ACurrentLineNumbers);
    if Result > 0 then
      Break;
  end;
end;

procedure TJclLineNumberTranslators.UnregisterTranslator(AIndex: Integer);
var
  Idx: Integer;
begin
  Idx := FIndexList.IndexOf(Pointer(AIndex));
  if Idx <> -1 then
  begin
    FTranslators.Delete(Idx);
    FIndexList.Delete(Idx);
  end;
end;

//=== { TJclRevisionProviders } ==============================================

constructor TJclRevisionProviders.Create;
begin
  inherited Create;
  FNextIndex := 1;
  FIndexList := TList.Create;
  FTranslators := TInterfaceList.Create;
end;

destructor TJclRevisionProviders.Destroy;
begin
  FTranslators.Free;
  FIndexList.Free;
  inherited Destroy;
end;

function TJclRevisionProviders.GetCount: Integer;
begin
  Result := FTranslators.Count;
end;

function TJclRevisionProviders.GetItems(AIndex: Integer): IJclRevisionProvider;
begin
  Result := IJclRevisionProvider(FTranslators[AIndex]);
end;

function TJclRevisionProviders.RegisterProvider(const ATranslator: IJclRevisionProvider): Integer;
begin
  if Assigned(ATranslator) then
  begin
    Result := FNextIndex;
    Inc(FNextIndex);
    FTranslators.Add(ATranslator);
    FIndexList.Add(Pointer(Result));
  end
  else
    Result := -1;
end;

function TJclRevisionProviders.GetRevisionContent(const AFileName, ARevision: string; AContent: IStream): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
    if Items[I].GetRevisionContent(AFileName, ARevision, AContent) then
    begin
      Result := True;
      Break;
    end;
end;

procedure TJclRevisionProviders.UnregisterProvider(AIndex: Integer);
var
  Idx: Integer;
begin
  Idx := FIndexList.IndexOf(Pointer(AIndex));
  if Idx <> -1 then
  begin
    FTranslators.Delete(Idx);
    FIndexList.Delete(Idx);
  end;
end;

function RegisterLineNumberTranslator(const ATranslator: IJclLineNumberTranslator): Integer;
begin
  Result := LineNumberTranslators.RegisterTranslator(ATranslator);
end;

function TranslateLineNumbers(ARevisionContent, ACurrentContent: IStream; ARevisionLineNumbers: TList; ACurrentLineNumbers: TList): Integer;
begin
  Result := LineNumberTranslators.TranslateLineNumbers(ARevisionContent, ACurrentContent, ARevisionLineNumbers, ACurrentLineNumbers);
end;

procedure UnregisterLineNumberTranslator(AIndex: Integer);
begin
  LineNumberTranslators.UnregisterTranslator(AIndex);
end;

function GetRevisionContent(const AFileName, ARevision: string; AContent: IStream): Boolean;
begin
  Result := RevisionProviders.GetRevisionContent(AFileName, ARevision, AContent);
end;

function RegisterRevisionProvider(const ATranslator: IJclRevisionProvider): Integer;
begin
  Result := RevisionProviders.RegisterProvider(ATranslator);
end;

procedure UnregisterRevisionProvider(AIndex: Integer);
begin
  RevisionProviders.UnregisterProvider(AIndex);
end;

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}
  LineNumberTranslators := TJclLineNumberTranslators.Create;
  RevisionProviders := TJclRevisionProviders.Create;
  RegisterLineNumberTranslatorProc := RegisterLineNumberTranslator;
  UnregisterLineNumberTranslatorProc := UnregisterLineNumberTranslator;
  RegisterRevisionProviderProc := RegisterRevisionProvider;
  UnregisterRevisionProviderProc := UnregisterRevisionProvider;

finalization
  LineNumberTranslators.Free;
  RevisionProviders.Free;
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.