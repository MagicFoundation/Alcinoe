{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL) extension                                                        }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is RegHelper.dpr.                                                              }
{                                                                                                  }
{ The Initial Developer of the Original Code is Florent Ouchet.                                    }
{ Portions created by Florent Ouchet are Copyright (C) of Florent Ouchet. All Rights Reserved.     }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Jean-Fabien Connault (cycocrew)                                                                }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

program RegHelper;

{$APPTYPE CONSOLE}

uses
  SysUtils, Windows, ActiveX,
  JclAnsiStrings, JclHelpUtils, JclSysUtils;

{$R ..\source\windows\JclNoDepAdmin.res}
{$R RegHelper.res}

type
  TCommandFunc = function (const Parameters: array of string): Boolean;
  TCommandRec = record
    Name: string;
    ParamCount: Integer;
    Func: TCommandFunc;
    Description: string;
  end;

var
  Help2Manager: TJclHelp2Manager;
  ShowCopyright: Boolean = True;
  ResultFileName: string = '';
  RegHelperOutput: TEXT;
  DiscardFollowingCommand: Boolean = False;

function CommandCreate(const Parameters: array of string): Boolean;
begin
  Result := Help2Manager.CreateTransaction;
end;

function CommandCommit(const Parameters: array of string): Boolean;
begin
  Result := Help2Manager.CommitTransaction;
end;

function CommandRegNameSpace(const Parameters: array of string): Boolean;
var
  NameSpace, Collection, Description: WideString;
begin
  NameSpace := Parameters[0];
  Collection := Parameters[1];
  Description := Parameters[2];
  Result := Help2Manager.RegisterNameSpace(NameSpace, Collection, Description);
end;

function CommandUnRegNameSpace(const Parameters: array of string): Boolean;
var
  NameSpace: WideString;
begin
  NameSpace := Parameters[0];
  Result := Help2Manager.UnregisterNameSpace(NameSpace);
end;

function CommandRegHelpFile(const Parameters: array of string): Boolean;
var
  NameSpace, Identifier, HxSFile, HxIFile: WideString;
  LangId, Code: Integer;
begin
  Val(Parameters[2], LangId, Code);
  Result := Code = 0;
  if Result then
  begin
    NameSpace := Parameters[0];
    Identifier := Parameters[1];
    HxSFile := Parameters[3];
    HxIFile := Parameters[4];
    Result := Help2Manager.RegisterHelpFile(NameSpace, Identifier, LangId, HxSFile, HxIFile);
  end;
end;

function CommandUnregHelpFile(const Parameters: array of string): Boolean;
var
  NameSpace, Identifier: WideString;
  LangId, Code: Integer;
begin
  Val(Parameters[2], LangId, Code);
  Result := Code = 0;
  if Result then
  begin
    NameSpace := Parameters[0];
    Identifier := Parameters[1];
    Result := Help2Manager.UnregisterHelpFile(NameSpace, Identifier, LangId);
  end;
end;

function CommandPlugNameSpace(const Parameters: array of string): Boolean;
var
  Source, Target: WideString;
begin
  Source := Parameters[0];
  Target := Parameters[1];
  Result := Help2Manager.PlugNameSpaceIn(Source, Target);
end;

function CommandUnplugNameSpace(const Parameters: array of string): Boolean;
var
  Source, Target: WideString;
begin
  Source := Parameters[0];
  Target := Parameters[1];
  Result := Help2Manager.UnPlugNameSpace(Source, Target);
end;

const
  CommandRecs: array [0..7] of TCommandRec =
    ( (Name: 'Create'; ParamCount: 0; Func: CommandCreate;
       Description: '  %s' + AnsiLineBreak +
       '    Create a new transaction'),
      (Name: 'Commit'; ParamCount: 0; Func: CommandCommit;
       Description: '  %s' + AnsiLineBreak +
       '    Commit previous comands; commands are not applied until committed'),

      (Name: 'RegNameSpace'; ParamCount: 3; Func: CommandRegNameSpace;
       Description: '  %s;<NameSpace>;<Collection>;<Description>' + AnsiLineBreak +
       '    Register a new namespace named <NameSpace> with description set to' + AnsiLineBreak +
       '    <Description>. The <Collection> HxC file contains namespace informations.'),

      (Name: 'UnregNameSpace'; ParamCount: 1; Func: CommandUnRegNameSpace;
       Description: '  %s;<NameSpace>' + AnsiLineBreak +
       '    Unregister namespace <NameSpace>'),

      (Name: 'RegHelpFile'; ParamCount: 5; Func: CommandRegHelpFile;
       Description: '  %s;<NameSpace>;<Identifier>;<LangId>;<HxSFile>;<HxIFile>' + AnsiLineBreak +
       '    Register a new help file <Identifier> for namespace <NameSpace>' + AnsiLineBreak +
       '    The <HxSFile> contains the content for this file in language <LangId>' + AnsiLineBreak +
       '    Its index is contained in <HxIFile>'),

      (Name: 'UnregHelpFile'; ParamCount: 3; Func: CommandUnregHelpFile;
       Description: '  %s;<NameSpace>;<Identifier>;<LangId>' + AnsiLineBreak +
       '    Unregister help file <Identifier> with language <LangId> from namespace' + AnsiLineBreak +
       '    <NameSpace>'),

      (Name: 'PlugNameSpace'; ParamCount: 2; Func: CommandPlugNameSpace;
       Description: '  %s;<Source>;<Target>' + AnsiLineBreak +
       '    Plug namespace <Source> in namespace <Target>'),

      (Name: 'UnplugNameSpace'; ParamCount: 2; Func: CommandUnplugNameSpace;
       Description: '  %s;<Source>;<Target>' + AnsiLineBreak +
       '    Unplug namespace <Source> from namespace <Target>')
    );

type
  TCommand = record
    Func: TCommandFunc;
    FuncName: string;
    Parameters: array of string;
  end;

var
  Commands: array of TCommand;

procedure DisplayCopyright;
begin
  WriteLn(RegHelperOutput,'HTML Help 2.0 registration helper');
  WriteLn(RegHelperOutput,'Copyright (c) 2007-2013 Project JEDI');
  WriteLn(RegHelperOutput,'');
end;

procedure DisplayHelp;
var
  Index: Integer;
begin
  if ShowCopyright then
    DisplayCopyright;
    
  WriteLn(RegHelperOutput,'Usage ', ExtractFileName(ParamStr(0)), ' <options> <command1> <command2> ...');
  WriteLn(RegHelperOutput,'Commands are always sequencially executed');
  WriteLn(RegHelperOutput,'Commands cannot contain spaces or use double quotes: "<command 1>"');
  WriteLn(RegHelperOutput,'');
  WriteLn(RegHelperOutput,'Valid options are:');
  WriteLn(RegHelperOutput,'  -c           do not output copyright');
  WriteLn(RegHelperOutput,'  -o<FileName> filename to store output (defaults to stdout)');
  WriteLn(RegHelperOutput,'  -d           discard following commands on fail');
  WriteLn(RegHelperOutput,'Valid commands are:');

  for Index := Low(CommandRecs) to high(CommandRecs) do
  begin
    WriteLn(RegHelperOutput,'');
    WriteLn(RegHelperOutput,Format(CommandRecs[Index].Description, [CommandRecs[Index].Name]));
  end;
end;

function ParseArguments: Boolean;
  function ParseArgument(const Argument: string): Boolean;
  var
    FuncName, Parameters: string;
    IndexCommand, IndexParam, ParamCount: Integer;
  begin
    if (Length(Argument) > 0) and (Argument[1] = '-') then
    begin
      // option
      Result := True;
      if AnsiSameText('-o', Copy(Argument, 1, 2)) then
      begin
        ResultFileName := Copy(Argument, 3, Length(Argument) - 2);
        Assign(RegHelperOutput, ResultFileName);
        Rewrite(RegHelperOutput);
      end
      else
      if AnsiSameText('-c', Argument) then
        ShowCopyright := False
      else
      if AnsiSameText('-d', Argument) then
        DiscardFollowingCommand := True
      else
        Result := False;
    end
    else
    begin
      // command
      Parameters := Argument;
      FuncName := ListGetItem(Parameters, ';', 0);
      ListDelItem(Parameters, ';', 0);
      Result := False;
      for IndexCommand := Low(CommandRecs) to High(CommandRecs) do
        if AnsiSameText(FuncName, CommandRecs[IndexCommand].Name) then
      begin
        ParamCount := ListItemCount(Parameters, ';');
        if ParamCount = CommandRecs[IndexCommand].ParamCount then
        begin
          SetLength(Commands, Length(Commands) + 1);
          Commands[High(Commands)].Func := CommandRecs[IndexCommand].Func;
          Commands[High(Commands)].FuncName := FuncName;
          SetLength(Commands[High(Commands)].Parameters, ParamCount);
          for IndexParam := 0 to ParamCount - 1 do
            Commands[High(Commands)].Parameters[IndexParam] := ListGetItem(Parameters, ';', IndexParam);
          Result := True;
          Break;
        end
        else
        begin
          WriteLn(RegHelperOutput, 'Error: Number of parameters (' + intToStr(ParamCount) + ' found instead of ' + intToStr(CommandRecs[IndexCommand].ParamCount) + ' expected) is invalid for command: ', Argument);
          Result := False;
          Exit;
        end;
      end;
    end;
  end;
var
  Index: Integer;
begin
  Result := False;
  for Index := 1 to ParamCount do
  begin
    Result := ParseArgument(ParamStr(Index));
    if not Result then
      Exit;
  end;
end;

procedure ExecuteArguments;
var
  Index: Integer;
begin
  if ShowCopyright then
    DisplayCopyright;

  for Index := Low(Commands) to High(Commands) do
  begin
    if Commands[Index].Func(Commands[Index].Parameters) then
      WriteLn(RegHelperOutput, 'Success ', Commands[Index].FuncName)
    else
    begin
      WriteLn(RegHelperOutput,'Error executing command ', Commands[Index].FuncName);
      if DiscardFollowingCommand then
        Exit;
    end;
  end;
end;

procedure FinalizeArguments;
var
  Index: Integer;
begin
  for Index := Low(Commands) to High(Commands) do
    SetLength(Commands[Index].Parameters, 0);
  SetLength(Commands, 0);
end;

begin
  try
    CoInitialize(nil); // Help2 interfaces are COM
    try
      Help2Manager := TJclHelp2Manager.Create(0);
      try
        Assign(RegHelperOutput, ''); // stdout
        Rewrite(RegHelperOutput);
        if ParseArguments then
          ExecuteArguments
        else
          DisplayHelp;
      finally
        FinalizeArguments;
        Help2Manager.Free;
        if ResultFileName <> '' then
          Close(RegHelperOutput);
      end;
    finally
      CoUninitialize;
    end;

  except
    on E:Exception do
      Writeln(E.Classname, ': ', E.Message);
  end;
end.
