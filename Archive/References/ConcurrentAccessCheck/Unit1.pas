unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, Alcinoe.Common, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, System.Generics.Collections;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
    FRunning: Boolean;
    FlistValues: TList<Double>;
    FDouble1: Double;
    FDouble2: Double;
    FRect1: TALRectD;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  Alcinoe.StringUtils,
  system.Math,
  system.Math.Vectors,
  System.DateUtils;

procedure TForm1.Button1Click(Sender: TObject);
begin

  if not FRunning then begin

    FDouble1 := Now;
    FDouble2 := FlistValues[(Random(FlistValues.Count))];
    FRect1 := TALRectD.Create(FlistValues[(Random(FlistValues.Count))], FlistValues[(Random(FlistValues.Count))], FlistValues[(Random(FlistValues.Count))], FlistValues[(Random(FlistValues.Count))]);

    TThread.CreateAnonymousThread(
      Procedure
      Begin
        while FRunning do begin
          FDouble1 := Now;
          FDouble2 := FlistValues[(Random(FlistValues.Count))];
          FRect1 := TALRectD.Create(FlistValues[(Random(FlistValues.Count))], FlistValues[(Random(FlistValues.Count))], FlistValues[(Random(FlistValues.Count))], FlistValues[(Random(FlistValues.Count))]);
        end;
      End).Start;

    for var I := 0 to 10 do begin
      TThread.CreateAnonymousThread(
        Procedure
        Begin
          while FRunning do begin
            var LRes := SecondsBetween(Now, FDouble1);
            if LRes > 1 then begin
                TThread.Synchronize(nil,
                procedure
                begin
                  Memo1.Lines.Add(ALInttostrW(LRes));
                end);
                sleep(100);
            end;

            var LDouble2 := FDouble2;
            var LFound := False;
            var LMinDelta: Double := ALMaxint;
            for Var I := 0 to FlistValues.Count - 1 do begin
              LMinDelta := Min(LMinDelta, Abs(FlistValues[i] - LDouble2));
              if sameValue(FlistValues[i], LDouble2) then begin
                LFound := True;
                Break;
              end;
            end;
            if not Lfound then begin
                TThread.Synchronize(nil,
                procedure
                begin
                  Memo1.Lines.Add(ALFloatToStrW(LDouble2, ALDefaultFormatSettingsW) + ' | ' + ALFloatToStrW(LMinDelta, ALDefaultFormatSettingsW));
                end);
                sleep(100);
            end;

            Var LRect1 := Frect1;
            LFound := False;
            LMinDelta := ALMaxint;
            for Var I := 0 to FlistValues.Count - 1 do begin
              LMinDelta := Min(LMinDelta, Abs(FlistValues[i] - LRect1.Left));
              if sameValue(FlistValues[i], LRect1.Left) then begin
                LFound := True;
                Break;
              end;
            end;
            if not Lfound then begin
                TThread.Synchronize(nil,
                procedure
                begin
                  Memo1.Lines.Add(ALFloatToStrW(LRect1.Left, ALDefaultFormatSettingsW) + ' | ' + ALFloatToStrW(LMinDelta, ALDefaultFormatSettingsW));
                end);
                sleep(100);
            end;

            LFound := False;
            LMinDelta := ALMaxint;
            for Var I := 0 to FlistValues.Count - 1 do begin
              LMinDelta := Min(LMinDelta, Abs(FlistValues[i] - LRect1.right));
              if sameValue(FlistValues[i], LRect1.right) then begin
                LFound := True;
                Break;
              end;
            end;
            if not Lfound then begin
                TThread.Synchronize(nil,
                procedure
                begin
                  Memo1.Lines.Add(ALFloatToStrW(LRect1.right, ALDefaultFormatSettingsW) + ' | ' + ALFloatToStrW(LMinDelta, ALDefaultFormatSettingsW));
                end);
                sleep(100);
            end;
          end;
        End).Start;
    end;

    FRunning := True;

  end
  else begin

    FRunning := False;

  end;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FRunning := False;
  FlistValues := TList<Double>.create;
  for Var I := 1 to 1000 do begin
    var LValue: Double := Random(maxint) * random;
    Memo1.Lines.Add(ALFloatToStrW(LValue, ALDefaultFormatSettingsW));
    FlistValues.Add(LValue);
  end;
  Memo1.Lines.Add('');
  Memo1.Lines.Add('###########');
  Memo1.Lines.Add('');
end;

end.
