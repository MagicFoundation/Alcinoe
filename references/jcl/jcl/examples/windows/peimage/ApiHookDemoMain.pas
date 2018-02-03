unit ApiHookDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm1 = class(TForm)
    HookBtn: TButton;
    UnhookBtn: TButton;
    BeepBtn: TButton;
    Memo1: TMemo;
    procedure HookBtnClick(Sender: TObject);
    procedure UnhookBtnClick(Sender: TObject);
    procedure BeepBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure AddMsg(const S: string);
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses
  JclPeImage, JclSysUtils;

var
  PeImportHooks: TJclPeMapImgHooks;

  OldMessageBeep: function(uType: UINT): BOOL; stdcall = nil;

function NewMessageBeep(uType: UINT): BOOL; stdcall;
begin
  Form1.AddMsg(Format('MessageBeep called, uType = %d', [uType]));
  Result := OldMessageBeep(uType);
end;

{ TForm1 }

procedure TForm1.AddMsg(const S: string);
begin
  Memo1.Lines.Add(S);
end;

procedure TForm1.HookBtnClick(Sender: TObject);
begin
  if PeImportHooks.HookImport(Pointer(HInstance), user32, 'MessageBeep',
    @NewMessageBeep, @OldMessageBeep) then
    AddMsg('MessageBeep hooked ...')
  else
    AddMsg(Format('MessageBeep hooking error - %s', [SysErrorMessage(GetLastError)]));
end;

procedure TForm1.UnhookBtnClick(Sender: TObject);
begin
  if PeImportHooks.UnhookByNewAddress(@NewMessageBeep) then
  begin
    @OldMessageBeep := nil;
    AddMsg('MessageBeep unhooked ...');
  end else
    AddMsg('MessageBeep wasn''t hooked')
end;

procedure TForm1.BeepBtnClick(Sender: TObject);
begin
  MessageBeep(MB_OK);
end;

initialization
  PeImportHooks := TJclPeMapImgHooks.Create;

finalization
  FreeAndNil(PeImportHooks);

end.
