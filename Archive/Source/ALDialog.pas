{*************************************
Description:  Standart dialog function
              - ALInputQuery function
              - ALInputBox function
**************************************}

unit ALDialog;

interface

{$IF CompilerVersion >= 25} {Delphi XE4}
  {$LEGACYIFEND ON} // http://docwiki.embarcadero.com/RADStudio/XE4/en/Legacy_IFEND_(Delphi)
{$ENDIF}

function ALInputQuery(const ACaption, APrompt: Ansistring; var Value: Ansistring; ACancelButton: Boolean): Boolean;
function ALInputBox(const ACaption, APrompt, ADefault: Ansistring; ACancelButton: Boolean): Ansistring;

implementation

uses Winapi.windows,
     Vcl.forms,
     Vcl.Graphics,
     Vcl.StdCtrls,
     Vcl.controls,
     Vcl.Consts;

{*****************************************************************************************************************}
function ALInputQuery(const ACaption, APrompt: Ansistring; var Value: Ansistring; ACancelButton: Boolean): Boolean;
var
  Form: TForm;
  Prompt: TLabel;
  Edit: TEdit;
  DialogUnits: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight: Integer;

    function GetAveCharSize(Canvas: TCanvas): TPoint;
    var
      I: Integer;
      Buffer: array[0..51] of Char;
    begin
      for I := 0 to 25 do Buffer[I] := Chr(I + Ord('A'));
      for I := 0 to 25 do Buffer[I + 26] := Chr(I + Ord('a'));
      GetTextExtentPoint(Canvas.Handle, Buffer, 52, TSize(Result));
      Result.X := Result.X div 52;
    end;

begin
  Result := False;
  Form := TForm.Create(Application);
  with Form do
    try
      Canvas.Font := Font;
      DialogUnits := GetAveCharSize(Canvas);
      BorderStyle := bsDialog;
      Caption := String(ACaption);
      ClientWidth := MulDiv(180, DialogUnits.X, 4);
      Position := poScreenCenter;
      Prompt := TLabel.Create(Form);
      with Prompt do begin
        Parent := Form;
        Caption := String(APrompt);
        Left := MulDiv(8, DialogUnits.X, 4);
        Top := MulDiv(8, DialogUnits.Y, 8);
        Constraints.MaxWidth := MulDiv(164, DialogUnits.X, 4);
        WordWrap := True;
      end;
      Edit := TEdit.Create(Form);
      with Edit do begin
        Parent := Form;
        Left := Prompt.Left;
        Top := Prompt.Top + Prompt.Height + 5;
        Width := MulDiv(164, DialogUnits.X, 4);
        MaxLength := 255;
        Text := String(Value);
        SelectAll;
      end;
      ButtonTop := Edit.Top + Edit.Height + 15;
      ButtonWidth := MulDiv(50, DialogUnits.X, 4);
      ButtonHeight := MulDiv(14, DialogUnits.Y, 8);
      with TButton.Create(Form) do begin
        Parent := Form;
        Caption := SMsgDlgOK;
        ModalResult := mrOk;
        Default := True;
        If ACancelButton then SetBounds(MulDiv(38, DialogUnits.X, 4), ButtonTop, ButtonWidth, ButtonHeight)
        else SetBounds(MulDiv(65, DialogUnits.X, 4), ButtonTop, ButtonWidth, ButtonHeight);
        Form.ClientHeight := Top + Height + 13;
      end;
      If ACancelButton then
        with TButton.Create(Form) do begin
          Parent := Form;
          Caption := SMsgDlgCancel;
          ModalResult := mrCancel;
          Cancel := True;
          SetBounds(MulDiv(92, DialogUnits.X, 4), Edit.Top + Edit.Height + 15, ButtonWidth, ButtonHeight);
        end;
      if ShowModal = mrOk then begin
        Value := AnsiString(Edit.Text);
        Result := True;
      end;
    finally
      Form.Free;
    end;
end;

{*****************************************************************************************************}
function ALInputBox(const ACaption, APrompt, ADefault: Ansistring; ACancelButton: Boolean): Ansistring;
begin
  Result := ADefault;
  ALInputQuery(ACaption, APrompt, Result, ACancelButton);
end;

end.
 