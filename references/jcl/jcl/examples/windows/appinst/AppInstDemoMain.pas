unit AppInstDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JclAppInst, ComCtrls, StdCtrls;

type
  TForm1 = class(TForm)
    InstancesListView: TListView;
    Label1: TLabel;
    SwitchBtn: TButton;
    MsgBtn: TButton;
    ColorDialog1: TColorDialog;
    Memo1: TMemo;
    SendBtn: TButton;
    AutoUpdateCheckBox: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure SwitchBtnClick(Sender: TObject);
    procedure MsgBtnClick(Sender: TObject);
    procedure SendBtnClick(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
  private
    procedure BuildInstancesList;
    procedure ApplicationEvents1Message(var Msg: TMsg; var Handled: Boolean);
  public
    procedure WndProc(var Message: TMessage); override;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses
  JclBase;

const
  MaxAllowedInstances = 3;

  MyDataKind = 1;

{ TForm1 }

procedure TForm1.BuildInstancesList;
var
  I, CurrIndex: Integer;
begin
  with InstancesListView, JclAppInstances do
  begin
    Items.BeginUpdate;
    Items.Clear;
    for I := 0 to InstanceCount -1 do
      with Items.Add do
      begin
        Caption := IntToStr(I + 1);
        SubItems.Add(Format('%.8x', [ProcessIDs[I]]));
        SubItems.Add(Format(HexDigitFmt, [AppWnds[I]]));
      end;
    CurrIndex := InstanceIndex[GetCurrentProcessId];
    Selected := Items[CurrIndex];
    Items.EndUpdate;
  end;
  Label1.Caption := IntToStr(CurrIndex + 1);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Application.OnMessage := ApplicationEvents1Message;
  BuildInstancesList;
end;

procedure TForm1.ApplicationEvents1Message(var Msg: TMsg; var Handled: Boolean);
begin
  // AI_* messages handler. These messages are automatically send to all instances
  // of the application.
  with Msg do
    if (hwnd = 0) and (message = JclAppInstances.MessageID) then
    begin
      case wParam of
        AI_INSTANCECREATED, AI_INSTANCEDESTROYED:
          BuildInstancesList;
        AI_USERMSG:
          Label1.Font.Color := TColor(lParam);
      end;
      Handled := True;
    end;
end;

procedure TForm1.SwitchBtnClick(Sender: TObject);
begin
  JclAppInstances.SwitchTo(InstancesListView.Selected.Index);
end;

procedure TForm1.MsgBtnClick(Sender: TObject);
begin
  with ColorDialog1 do
  begin
    Color := Label1.Font.Color;
    if Execute then
      JclAppInstances.UserNotify(Color);
  end;
end;

procedure TForm1.SendBtnClick(Sender: TObject);
begin
  // TForm.ClassName matches window class name. It sends the data to all windows
  // belonging instances of this application. The last parameter identifies the
  // 'TForm1' (ClassName) window of this instance.
  JclAppInstances.SendStrings(ClassName, MyDataKind, Memo1.Lines, Handle);
end;

var
  MemoChanging: Boolean;

procedure TForm1.WndProc(var Message: TMessage);
begin
  // Interprocess communication handler.

  // First check whether we can safely read TForm.Handle property ...
  if HandleAllocated and not (csDestroying in ComponentState) then
  begin
    // ... then whether it is our message. The last paramter tells to ignore the
    // message sent from window of this instance
    case ReadMessageCheck(Message, Handle) of
      MyDataKind: // It is our data
        begin
          MemoChanging := True; // prevent deadlock, TMemo.OnChange is also fired now
          Memo1.Lines.BeginUpdate;
          try
            // Read TStrings from the message
            ReadMessageStrings(Message, Memo1.Lines)
          finally
            Memo1.Lines.EndUpdate;
            MemoChanging := False;
          end;
        end;  
     else
       inherited;
     end;  
  end
  else
    inherited;
end;

procedure TForm1.Memo1Change(Sender: TObject);
begin
  if not MemoChanging and AutoUpdateCheckBox.Checked then
    SendBtnClick(nil);
end;

initialization

  with JclAppInstances do
    // CheckInstance returns False if current instance number is greater than
    // MaxAllowedInstances constant
    if not CheckInstance(MaxAllowedInstances) then
    begin
      // Switch to the first instance of the application
      SwitchTo(0);
      // Close this instance
      KillInstance;
    end;

  // Note: For preventing more than one instance of the application you can put
  // simple JclAppInstances.CheckSingleInstance line to initialization section
  // instead of code above

end.
