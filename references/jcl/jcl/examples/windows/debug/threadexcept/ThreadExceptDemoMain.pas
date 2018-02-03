unit ThreadExceptDemoMain;

{$I jcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, JclDebug;

type
  TDemoThread = class(TJclDebugThread)
  private
    procedure SomeErrorHere;
  protected
    procedure Execute; override;
  end;

  TMainForm = class(TForm)
    MessageRichEdit: TRichEdit;
    ThreadsRichEdit: TRichEdit;
    CreateThreadBtn: TButton;
    ThreadNameEdit: TEdit;
    ListThreadsBtn: TButton;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure CreateThreadBtnClick(Sender: TObject);
    procedure ListThreadsBtnClick(Sender: TObject);
  private
    procedure DoThreadSyncException(Thread: TJclDebugThread);
    procedure DoThreadRegistered(ThreadID: DWORD);
    procedure DoThreadUnregistered(ThreadID: DWORD);
  public
    ThreadCnt: Integer;
    function GetNewThreadName: string;
    procedure ScrollDownRichEdit(RichEdit: TRichEdit);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

{ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! }
{                                                                                                  }
{ You have to install ThreadNameExpert package located in "\experts\debug\threadnames"             }
{                                                                                                  }
{ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! }

{$DEFINE IdeThreadStatusWindowExtension}

{$IFDEF IdeThreadStatusWindowExtension}
uses
  JclIdeThreadStatus;
{$ENDIF}

{ TDemoThread }

procedure TDemoThread.Execute;
var
  I: Integer;
begin
  try
    I := 0;
    while not Terminated and (I < 5) do
    begin
      Sleep(2000);
      MessageBeep(0);
      try
        SomeErrorHere;
      except
        HandleException;
      end;
      Inc(I);
      {$IFDEF IdeThreadStatusWindowExtension}
      // You can change Thread Name displayed in Thread Status Window in code. This does not change
      // TDemoThread.ThreadName property value
      // ChangeThreadName(Self, Format('I = %d', [I]));
      {$ENDIF}
    end;
  except
    HandleException;
  end;
end;

procedure TDemoThread.SomeErrorHere;
begin
  // Set Breakpoint on "begin", uncheck "Break" and check "Ingore subsequent exceptions" in
  // dialog advanced breakpoint actions 
  StrToInt('x');
end;

{ TMainForm }

procedure TMainForm.DoThreadRegistered(ThreadID: DWORD);
begin
  ThreadsRichEdit.Lines.Add(Format('Thread registered: %s', [JclDebugThreadList.ThreadInfos[ThreadID]]));
  ScrollDownRichEdit(ThreadsRichEdit);
end;

procedure TMainForm.DoThreadSyncException(Thread: TJclDebugThread);
begin
  MessageRichEdit.Lines.Add(Format('Exception in thread: %s', [Thread.ThreadInfo]));
  // Note: JclLastExceptStackList always returns list for *current* thread ID. To simplify getting the
  // stack of thread where an exception occured JclLastExceptStackList returns stack of the thread instead
  // of current thread when called *within* the JclDebugThreadList.OnSyncException handler. This is the
  // *only* exception to the behavior of JclLastExceptStackList described above.
  JclLastExceptStackList.AddToStrings(MessageRichEdit.Lines, False, True, True);
  ScrollDownRichEdit(MessageRichEdit);
end;

procedure TMainForm.DoThreadUnregistered(ThreadID: DWORD);
begin
  ThreadsRichEdit.Lines.Add(Format('Thread unregistered: %s', [JclDebugThreadList.ThreadInfos[ThreadID]]));
  ScrollDownRichEdit(ThreadsRichEdit);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  with JclDebugThreadList do
  begin
    OnSyncException := DoThreadSyncException;
    OnThreadRegistered := DoThreadRegistered;
    OnThreadUnregistered := DoThreadUnregistered;
  end;
  ThreadNameEdit.Text := GetNewThreadName;
end;

function TMainForm.GetNewThreadName: string;
begin
  Inc(ThreadCnt);
  Result := Format('Thread%d', [ThreadCnt]);
end;

procedure TMainForm.CreateThreadBtnClick(Sender: TObject);
var
  DemoThread: TDemoThread;
begin
  DemoThread := TDemoThread.Create(True, ThreadNameEdit.Text);
  DemoThread.FreeOnTerminate := True;
  {$IFDEF RTL230_UP}
  DemoThread.Start;
  {$ELSE ~RTL230_UP}
  DemoThread.Resume;
  {$ENDIF ~RTL230_UP}
  ThreadNameEdit.Text := GetNewThreadName;
end;

procedure TMainForm.ListThreadsBtnClick(Sender: TObject);
var
  I: Integer;
begin
  ThreadsRichEdit.Lines.Add('List of registered threads:');
  with JclDebugThreadList do
    for I := 0 to ThreadIDCount - 1 do
      ThreadsRichEdit.Lines.Add(ThreadInfos[ThreadIDs[I]]);
  ScrollDownRichEdit(ThreadsRichEdit);
end;

procedure TMainForm.ScrollDownRichEdit(RichEdit: TRichEdit);
begin
  SendMessage(RichEdit.Handle, EM_SCROLLCARET, 0, 0);
end;

initialization
  Include(JclStackTrackingOptions, stRawMode);
  JclStartExceptionTracking;

end.
