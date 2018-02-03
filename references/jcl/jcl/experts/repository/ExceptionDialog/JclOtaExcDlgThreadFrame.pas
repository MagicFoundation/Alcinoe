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
{ The Original Code is JclOtaExcDlgThreadFrame.pas.                                                }
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

unit JclOtaExcDlgThreadFrame;

interface

{$I jcl.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JclDebug,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclPreProcessorExcDlgTemplates, JclOtaWizardFrame;

type
  TJclOtaExcDlgThreadPage = class(TJclWizardFrame)
    MemoStack: TMemo;
    LabelPreview: TLabel;
    CheckBoxStackList: TCheckBox;
    RadioButtonAllThreads: TRadioButton;
    RadioButtonAllRegisteredThreads: TRadioButton;
    RadioButtonMainExceptionThreads: TRadioButton;
    RadioButtonExceptionThread: TRadioButton;
    RadioButtonMainThread: TRadioButton;
    procedure RadioButtonClick(Sender: TObject);
    procedure CheckBoxStackListClick(Sender: TObject);
  private
    FParams: TJclExcDlgParams;
    FTestThread: TJclDebugThread;
    procedure UpdatePreview;
    procedure UpdateCheckBoxes;
  public
    constructor Create(AOwner: TComponent; AParams: TJclExcDlgParams); reintroduce;
    destructor Destroy; override;

    procedure PageActivated(Direction: TJclWizardDirection); override;
    procedure PageDesactivated(Direction: TJclWizardDirection); override;

    property Params: TJclExcDlgParams read FParams write FParams;
  end;

  // in interface to be exported and have basic debug informations based on exports
  TTestThread = class(TJclDebugThread)
  private
    procedure ExecuteTask;
    procedure ExecuteSubTask;
  protected
    procedure Execute; override;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\experts\repository\ExceptionDialog';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

{$R *.dfm}

uses
  JclOtaResources;

//=== { TTestThread } ========================================================

{$STACKFRAMES ON}

procedure TTestThread.Execute;
begin
  ExecuteTask;
end;

{$IFNDEF STACKFRAMES_ON}
{$STACKFRAMES OFF}
{$ENDIF ~STACKFRAMES_ON}

procedure TTestThread.ExecuteTask;
begin
  ExecuteSubTask;
end;

procedure TTestThread.ExecuteSubTask;
begin
  while not Terminated do
    Sleep(100);
end;

//=== { TJclOtaExcDlgThreadPage } ============================================

procedure TJclOtaExcDlgThreadPage.RadioButtonClick(Sender: TObject);
begin
  UpdatePreview;
end;

procedure TJclOtaExcDlgThreadPage.CheckBoxStackListClick(Sender: TObject);
begin
  UpdateCheckBoxes;
end;

constructor TJclOtaExcDlgThreadPage.Create(AOwner: TComponent;
  AParams: TJclExcDlgParams);
begin
  FParams := AParams;
  inherited Create(AOwner);
  FTestThread := TTestThread.Create(False, 'MyTaskThread');

  Caption := LoadResString(@RsExcDlgThreadOptions);
  CheckBoxStackList.Caption := LoadResString(@RsStackList);
  LabelPreview.Caption := LoadResString(@RsPreview);
  RadioButtonAllThreads.Caption := LoadResString(@RsAllThreads);
  RadioButtonAllRegisteredThreads.Caption := LoadResString(@RsAllRegisteredThreads);
  RadioButtonMainExceptionThreads.Caption := LoadResString(@RsMainExceptionThreads);
  RadioButtonExceptionThread.Caption := LoadResString(@RsExceptionThread);
  RadioButtonMainThread.Caption := LoadResString(@RsMainThread);
end;

destructor TJclOtaExcDlgThreadPage.Destroy;
begin
  FTestThread.Free;
  inherited Destroy;
end;

procedure TJclOtaExcDlgThreadPage.PageActivated(Direction: TJclWizardDirection);
begin
  inherited PageActivated(Direction);

  CheckBoxStackList.Checked := Params.StackList;
  RadioButtonAllThreads.Checked := Params.AllThreads;
  RadioButtonAllRegisteredThreads.Checked := Params.AllRegisterThreads;
  RadioButtonMainExceptionThreads.Checked := Params.MainExceptionThreads;
  RadioButtonExceptionThread.Checked := Params.ExceptionThread;
  RadioButtonMainThread.Checked := Params.MainThread;

  UpdateCheckBoxes;
end;

procedure TJclOtaExcDlgThreadPage.PageDesactivated(
  Direction: TJclWizardDirection);
begin
  inherited PageDesactivated(Direction);

  Params.StackList := CheckBoxStackList.Checked;
  Params.AllThreads := RadioButtonAllThreads.Checked;
  Params.AllRegisterThreads := RadioButtonAllRegisteredThreads.Checked;
  Params.MainExceptionThreads := RadioButtonMainExceptionThreads.Checked;
  Params.ExceptionThread := RadioButtonExceptionThread.Checked;
  Params.MainThread := RadioButtonMainThread.Checked;
end;

procedure TJclOtaExcDlgThreadPage.UpdateCheckBoxes;
var
  AEnabled: Boolean;
begin
  AEnabled := CheckBoxStackList.Enabled;

  RadioButtonAllThreads.Enabled := AEnabled;
end;

procedure TJclOtaExcDlgThreadPage.UpdatePreview;
var
  AStack: TJclStackInfoList;
  Index: Integer;
  ThreadID: DWORD;
begin
  MemoStack.Lines.Clear;

  if RadioButtonAllThreads.Checked or RadioButtonAllRegisteredThreads.Checked or RadioButtonMainExceptionThreads.Checked then
    MemoStack.Lines.Add('Main thread stack trace');

  if RadioButtonAllThreads.Checked or RadioButtonAllRegisteredThreads.Checked or RadioButtonMainExceptionThreads.Checked or RadioButtonMainThread.Checked then
  begin
    AStack := TJclStackInfoList.Create(Params.RawData, 0, nil, False);
    try
      AStack.AddToStrings(MemoStack.Lines, Params.ModuleName, Params.ModuleOffset, Params.CodeDetails, Params.VirtualAddress);
    finally
      AStack.Free;
    end;
  end;

  if RadioButtonAllThreads.Checked or RadioButtonAllRegisteredThreads.Checked or RadioButtonMainExceptionThreads.Checked or RadioButtonExceptionThread.Checked then
    for Index := 0 to JclDebugThreadList.ThreadIDCount - 1 do
  begin
    ThreadID := JclDebugThreadList.ThreadIDs[Index];
    MemoStack.Lines.Add('');
    MemoStack.Lines.Add(Format('Stack trace for thread: "%s"', [JclDebugThreadList.ThreadNames[ThreadID]]));
    AStack := JclCreateThreadStackTrace(Params.RawData, JclDebugThreadList.ThreadHandles[Index]);
    try
      AStack.AddToStrings(MemoStack.Lines, Params.ModuleName, Params.ModuleOffset, Params.CodeDetails, Params.VirtualAddress);
    finally
      AStack.Free;
    end;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
