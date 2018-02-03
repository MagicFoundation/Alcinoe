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
{ The Original Code is CreateStdDialogs.dpr.                                                       }
{                                                                                                  }
{ The Initial Developer of the Original Code is Florent Ouchet                                     }
{         <outchy att users dott sourceforge dott net>                                             }
{ Portions created by Florent Ouchet are Copyright (C) of Florent Ouchet. All rights reserved.     }
{                                                                                                  }
{ Contributors:                                                                                    }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

program jppExceptionDialogs;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  JclIDEUtils,
  JclPreProcessorExcDlgTemplates,
  JclPreProcessorTemplates;

const
  TemplateDirectory = '..\experts\repository\ExceptionDialog\Templates\';
  StdDlgsDirectory = '..\experts\repository\ExceptionDialog\StandardDialogs\';

function LoadTemplate(const FileName: TFileName): string;
var
  AFileStream: TFileStream;
  Buffer: AnsiString;
begin
  AFileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Buffer, AFileStream.Size);
    AFileStream.ReadBuffer(Buffer[1], AFileStream.Size);
    Result := string(Buffer);
  finally
    AFileStream.Free;
  end;
end;

procedure SaveFile(const FileName: TFileName; const FileContent: string);
var
  AFileStream: TFileStream;
  Buffer: AnsiString;
begin
  AFileStream := TFileStream.Create(FileName, fmOpenWrite or fmShareDenyWrite);
  try
    Buffer := AnsiString(FileContent);
    AFileStream.Size := 0;
    AFileStream.Write(Buffer[1], Length(Buffer));
  finally
    AFileStream.Free;
  end;
end;

var
  Params: TJclExcDlgParams;
begin
  try
    Params := TJclExcDlgParams.Create;
    try
      Params.ActivePersonality := bpDelphi32;
      Params.FormName := 'ExceptionDialog';
      Params.FormAncestor := 'TForm';
      Params.ModalDialog := True;
      Params.SendEMail := False;
      Params.SizeableDialog := True;
      Params.AutoScrollBars := True;
      Params.DelayedTrace := True;
      Params.HookDll := True;
      Params.LogFile := True;
      Params.LogSaveDialog := True;
      Params.LogFileName := '''filename.log''';
      Params.OSInfo := True;
      Params.ModuleList := True;
      Params.ActiveControls := True;
      Params.AllThreads := True;
      Params.TraceAllExceptions := False;
      Params.StackList := True;
      Params.RawData := True;
      Params.ModuleName := True;
      Params.ModuleOffset := True;
      Params.CodeDetails := True;
      Params.VirtualAddress := True;

      SaveFile(StdDlgsDirectory + 'ExceptDlg.pas', GetFinalSourceContent(ApplyTemplate(LoadTemplate(TemplateDirectory + 'ExceptDlg.Delphi32.pas'), Params), 'ExceptDlg', 'ExceptionDialog', 'TForm'));
      SaveFile(StdDlgsDirectory + 'ExceptDlg.dfm', GetFinalSourceContent(ApplyTemplate(LoadTemplate(TemplateDirectory + 'ExceptDlg.Delphi32.dfm'), Params), 'ExceptDlg', 'ExceptionDialog', 'TForm'));

      Params.FormName := 'ExceptionDialogMail';
      Params.SendEMail := True;
      Params.EMailAddress := 'name@domain.ext';
      Params.EMailSubject := 'email subject';

      SaveFile(StdDlgsDirectory + 'ExceptDlgMail.pas', GetFinalSourceContent(ApplyTemplate(LoadTemplate(TemplateDirectory + 'ExceptDlg.Delphi32.pas'), Params), 'ExceptDlgMail', 'ExceptionDialogMail', 'TForm'));
      SaveFile(StdDlgsDirectory + 'ExceptDlgMail.dfm', GetFinalSourceContent(ApplyTemplate(LoadTemplate(TemplateDirectory + 'ExceptDlg.Delphi32.dfm'), Params), 'ExceptDlgMail', 'ExceptionDialogMail', 'TForm'));
    finally
      Params.Free;
    end;
  except
    on E:Exception do
      Writeln(E.Classname, ': ', E.Message);
  end;
end.
