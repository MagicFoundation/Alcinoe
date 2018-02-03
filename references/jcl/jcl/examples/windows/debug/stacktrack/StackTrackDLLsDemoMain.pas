unit StackTrackDLLsDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TMainForm = class(TForm)
    StaticLibError1Btn: TButton;
    StaticLibError2Btn: TButton;
    StaticLibGroupBox: TGroupBox;
    ComObjGroupBox: TGroupBox;
    ComObjErr1Btn: TButton;
    ComObjErr2Btn: TButton;
    DynLibGroupBox: TGroupBox;
    DynamicLibError1Btn: TButton;
    DynamicLibError2Btn: TButton;
    LoadLibBtn: TButton;
    FreeLibBtn: TButton;
    procedure StaticLibError1BtnClick(Sender: TObject);
    procedure StaticLibError2BtnClick(Sender: TObject);
    procedure ComObjErr1BtnClick(Sender: TObject);
    procedure ComObjErr2BtnClick(Sender: TObject);
    procedure DynamicLibError1BtnClick(Sender: TObject);
    procedure DynamicLibError2BtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LoadLibBtnClick(Sender: TObject);
    procedure FreeLibBtnClick(Sender: TObject);
  private
    FLibHandle: THandle;
  public
    procedure UpdateButtons;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  JclBase, StackTrackDLLsComLibrary_TLB;

const
  StaticLibrary  = 'StackTrackDLLsStaticLibrary.dll';
  DynamicLibrary = 'StackTrackDLLsDynamicLibrary.dll';

procedure Error1; stdcall; external StaticLibrary;
procedure Error2; stdcall; external StaticLibrary;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TMainForm.StaticLibError1BtnClick(Sender: TObject);
begin
  Error1;
end;

procedure TMainForm.StaticLibError2BtnClick(Sender: TObject);
begin
  Error2;
end;

procedure TMainForm.ComObjErr1BtnClick(Sender: TObject);
var
  I: IStackTrackDllsTest;
begin
  I := CoStackTrackDllsTest.Create;
  I.Error1;
end;

procedure TMainForm.ComObjErr2BtnClick(Sender: TObject);
var
  I: IStackTrackDllsTest;
begin
  I := CoStackTrackDllsTest.Create;
  I.Error2;
end;

procedure TMainForm.LoadLibBtnClick(Sender: TObject);
begin
  FLibHandle := LoadLibrary(DynamicLibrary);
  UpdateButtons;
  if FLibHandle = 0 then
    RaiseLastOSError;
end;

procedure TMainForm.FreeLibBtnClick(Sender: TObject);
begin
  FreeLibrary(FLibHandle);
  FLibHandle := 0;
  UpdateButtons;
end;

procedure TMainForm.DynamicLibError1BtnClick(Sender: TObject);
var
  _Error1: procedure; stdcall;
begin
  @_Error1 := GetProcAddress(FLibHandle, 'Error1');
  if not Assigned(_Error1) then
    RaiseLastOSError;
  _Error1;
end;

procedure TMainForm.DynamicLibError2BtnClick(Sender: TObject);
var
  _Error2: procedure; stdcall;
begin
  @_Error2 := GetProcAddress(FLibHandle, 'Error2');
  if not Assigned(_Error2) then
    RaiseLastOSError;
  _Error2;
end;

procedure TMainForm.UpdateButtons;
begin
  LoadLibBtn.Enabled := (FLibHandle = 0);
  FreeLibBtn.Enabled := (FLibHandle <> 0);
  DynamicLibError1Btn.Enabled := (FLibHandle <> 0);
  DynamicLibError2Btn.Enabled := (FLibHandle <> 0);
end;

end.
