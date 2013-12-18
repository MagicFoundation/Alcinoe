unit fMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, uPSComponent, uPSCompiler, Menus, uPSRuntime;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Memo2: TMemo;
    Splitter1: TSplitter;
    PSScript: TPSScript;
    PS3DllPlugin: TPSDllPlugin;
    MainMenu1: TMainMenu;
    Program1: TMenuItem;
    Compile1: TMenuItem;
    procedure IFPS3ClassesPlugin1CompImport(Sender: TObject;
      x: TPSPascalCompiler);
    procedure IFPS3ClassesPlugin1ExecImport(Sender: TObject; Exec: TPSExec;
      x: TPSRuntimeClassImporter);
    procedure PSScriptCompile(Sender: TPSScript);
    procedure Compile1Click(Sender: TObject);
    procedure PSScriptExecute(Sender: TPSScript);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses
  uPSR_std,
  uPSC_std,
  uPSR_stdctrls,
  uPSC_stdctrls,
  uPSR_forms,
  uPSC_forms,
  uPSC_graphics,
  uPSC_controls,
  uPSC_classes,
  uPSR_graphics,
  uPSR_controls,
  uPSR_classes,
  uPSI_uiblib,
  uPSI_uibconst,
  uPSI_uibmetadata,
  uPSI_uibsqlparser,
  uPSI_uib;

{$R *.DFM}

procedure TForm1.IFPS3ClassesPlugin1CompImport(Sender: TObject;
  x: TIFPSPascalcompiler);
begin
  SIRegister_Std(x);
  SIRegister_Classes(x, true);
  SIRegister_Graphics(x, true);
  SIRegister_Controls(x);
  SIRegister_stdctrls(x);
  SIRegister_Forms(x);
  SIRegister_uiblib(x);
  SIRegister_uibconst(x);
  SIRegister_uibmetadata(x);
  SIRegister_uibsqlparser(x);
  SIRegister_uib(x);
end;

procedure TForm1.IFPS3ClassesPlugin1ExecImport(Sender: TObject; Exec: TIFPSExec;
  x: TIFPSRuntimeClassImporter);
begin
  RIRegister_Std(x);
  RIRegister_Classes(x, True);
  RIRegister_Graphics(x, True);
  RIRegister_Controls(x);
  RIRegister_stdctrls(x);
  RIRegister_Forms(x);
  RIRegister_uiblib(x);
  RIRegister_uibmetadata(x);
  RIRegister_uibsqlparser(x);
  RIRegister_uib(x);
end;

function ImportTest(S1: string; s2: Longint; s3: Byte; s4: word; var s5: string): string;
begin
  Result := s1 + ' ' + IntToStr(s2) + ' ' + IntToStr(s3) + ' ' + IntToStr(s4) + ' - OK!';
  S5 := s5 + ' '+ result + ' -   OK2!';
end;

procedure MyWriteln(const s: string);
begin
  Form1.Memo2.Lines.Add(s);
end;

function MyReadln(const question: string): string;
begin
  Result := InputBox(question, '', '');
end;

procedure TForm1.PSScriptCompile(Sender: TPSScript);
begin
  Sender.AddFunction(@MyWriteln, 'procedure Writeln(s: string);');
  Sender.AddFunction(@MyReadln, 'function Readln(question: string): string;');
  Sender.AddFunction(@ImportTest, 'function ImportTest(S1: string; s2: Longint; s3: Byte; s4: word; var s5: string): string;');
  Sender.AddRegisteredVariable('Application', 'TApplication');
  Sender.AddRegisteredVariable('Self', 'TForm');
  Sender.AddRegisteredVariable('Memo1', 'TMemo');
  Sender.AddRegisteredVariable('Memo2', 'TMemo');
end;

procedure TForm1.Compile1Click(Sender: TObject);
  procedure OutputMessages;
  var
    l: Longint;
    b: Boolean;
  begin
    b := False;

    for l := 0 to PSScript.CompilerMessageCount - 1 do
    begin
      Memo2.Lines.Add('Compiler: '+ PSScript.CompilerErrorToStr(l));
      if (not b) and (PSScript.CompilerMessages[l] is TIFPSPascalCompilerError) then
      begin
        b := True;
        Memo1.SelStart := PSScript.CompilerMessages[l].Pos;
      end;
    end;
  end;
begin
  Memo2.Lines.Clear;
  PSScript.Script.Assign(Memo1.Lines);
  Memo2.Lines.Add('Compiling');
  if PSScript.Compile then
  begin
    OutputMessages;
    Memo2.Lines.Add('Compiled succesfully');
    if not PSScript.Execute then
    begin
      Memo1.SelStart := PSScript.ExecErrorPosition;
      Memo2.Lines.Add(PSScript.ExecErrorToString +' at '+Inttostr(PSScript.ExecErrorProcNo)+'.'+Inttostr(PSScript.ExecErrorByteCodePosition));
    end else Memo2.Lines.Add('Succesfully executed');
  end else
  begin
    OutputMessages;
    Memo2.Lines.Add('Compiling failed');
  end;
end;

procedure TForm1.PSScriptExecute(Sender: TPSScript);
begin
  PSScript.SetVarToInstance('APPLICATION', Application);
  PSScript.SetVarToInstance('SELF', Self);
  PSScript.SetVarToInstance('MEMO1', Memo1);
  PSScript.SetVarToInstance('MEMO2', Memo2);
end;

end.
