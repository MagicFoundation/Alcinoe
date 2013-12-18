(* The contents of this file are subject to the Mozilla Public License
   Version 1.1 (the "License"); you may not use this file except in compliance
   with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
   Software distributed under the License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.
*)

unit ThreadAppLoader;

interface
uses SharedHook, Classes, Activex;

function LoadProgram(const filename: string; const hook, GDS32: string): TTHread;

implementation
uses Windows, SysUtils, Forms, madCodeHook, main, SyncObjs, avl;

type
  TOrderedStreamList = class(TIndexedList)
  private
    FCriticalsection: TCriticalSection;
    FLastIndex: Integer;
  protected
    function doCompare(const item1, item2: Pointer): Integer; override;
  public
    procedure AddStream(stream: TMemoryStream);
    procedure Reinit;
    constructor Create(unique: boolean); override;
    destructor Destroy; override;
  end;

var
  OrderedList: TOrderedStreamList;

function CreateEnvironmentStrings: string;
var
  p1, p2: PChar;
begin
 p1 := GetEnvironmentStrings;
 p2 := p1;
 try
   Result := p2 + #0;
   while true do
   begin
     inc(p2);
     if (p2[0] = #0) then
       if (p2[1] = #0) then
         Break else
         begin
           inc(p2);
           if Uppercase(copy(p2, 0, 5)) = 'PATH=' then
             // add my path
             Result := Result + p2 + ';' + ExtractFilePath(ParamStr(0)) + #0 else
             Result := Result + p2 + #0;
         end;
   end;
   Result := Result + #0
 finally
   FreeEnvironmentStrings(p2);
 end;
end;

{ TThreadAppLoader }

type
  TThreadAppLoader = class(TThread)
  private
    FFilename: string;
    FHookLib: string;
    FGDS32: string;
    FIpcid: string;
    FPi: PROCESS_INFORMATION;
  protected
    procedure Execute; override;
    procedure DoTerminate; override;
    procedure BeginHooking;
    procedure EndHooking;
  end;

procedure TThreadAppLoader.BeginHooking;
begin
  MainForm.acRun.Enabled := false;
end;

procedure TThreadAppLoader.DoTerminate;
begin
  DestroyIpcQueue(PChar(FIpcid));
  UninstallMadCHook;
  CloseHandle(Fpi.hProcess);
  CloseHandle(Fpi.hThread);
  Synchronize(EndHooking);
  inherited;
end;

procedure TThreadAppLoader.EndHooking;
begin
  OrderedList.Reinit;
  MainForm.acRun.Enabled := true;
end;

procedure UIBIpcCallBack(name: pchar; messageBuf: pointer; messageLen: dword;
  answerBuf: pointer; answerLen: dword); stdcall;
var
  stream: TStreamMethod;
begin
  stream := TStreamMethod.Create;
  stream.Size := messageLen;
  move(messageBuf^, stream.Memory^, messageLen);
  OrderedList.AddStream(stream)
end;

procedure TThreadAppLoader.Execute;
var
  si: STARTUPINFO;
  result: boolean;
  environment: string;
  g: TGUID;
begin
  ZeroMemory(@si, sizeof(si));
  si.cb := sizeof(si);
  ZeroMemory(@Fpi, sizeof(pi));
  CreateGUID(g);
  Fipcid := GUIDToString(g);
  SetEnvironmentVariable('UIBHOOKIPC', PChar(Fipcid)); // send unique id
  SetEnvironmentVariable('UIBHOOKLIB', PChar(FGDS32)); // send unique id
  environment := CreateEnvironmentStrings;
  SetEnvironmentVariable('UIBHOOKIPC', nil);
  SetEnvironmentVariable('UIBHOOKLIB', nil);
  Synchronize(BeginHooking);
  CreateIpcQueue(PChar(Fipcid), UIBIpcCallBack);
  result := CreateProcessEx(nil, PChar(FFilename), nil, nil, false, 0, PChar(environment),
    PChar(ExtractFilePath(FFilename)), si, Fpi, FHookLib);
  if not result then
  begin
    Synchronize(EndHooking);
    raise exception.Create('impossible de charger le fichier');
  end;
  WaitForSingleObject(Fpi.hProcess, INFINITE);
end;

function LoadProgram(const filename: string; const hook, GDS32: string): TThread;
begin
  Result := TThreadAppLoader.Create(true);
  TThreadAppLoader(Result).FFilename := filename;
  TThreadAppLoader(Result).FHookLib := hook;
  TThreadAppLoader(Result).FGDS32 := GDS32;
  TThreadAppLoader(Result).FreeOnTerminate := true;
  TThreadAppLoader(Result).Resume;
end;

{ TOrderedStreamList }

procedure TOrderedStreamList.Reinit;
begin
  Clear;
  FLastIndex := 0;
end;

procedure TOrderedStreamList.AddStream(stream: TMemoryStream);
var
  strm: TMemoryStream;
begin
  FCriticalsection.Enter;
  try
    Add(stream, true);
    strm := TMemoryStream(First);
    while (First <> nil) and (Integer(strm.Memory^) = FLastIndex + 1) do
    begin
      Delete(strm);
      inc(FLastIndex);
      MainForm.AddMethodEvent(strm);
      strm := TMemoryStream(First);
    end;
  finally
    FCriticalsection.Leave;
  end;
end;

constructor TOrderedStreamList.Create(unique: boolean);
begin
  inherited;
  FLastIndex := 0;
  FCriticalsection := TCriticalSection.Create;
end;

destructor TOrderedStreamList.Destroy;
begin
  FCriticalsection.Free;
  inherited;
end;

function TOrderedStreamList.doCompare(const item1,
  item2: Pointer): Integer;
begin
  Result := Integer(TMemoryStream(item1).Memory^) - Integer(TMemoryStream(item2).Memory^);
end;

initialization
  OrderedList := TOrderedStreamList.Create(true);

finalization
  OrderedList.Free;

end.
