unit ExprEvalExampleLogic;

interface

uses
  SysUtils, Classes, JclExprEval;

procedure Init(Evaluator: TEasyEvaluator; FuncList: TStrings);
function ResultAsText(Evaluator: TEvaluator; const Input: string): string;

implementation

uses
  JclMath;

procedure Init(Evaluator: TEasyEvaluator; FuncList: TStrings);
begin
  with Evaluator do
  begin
    // Constants
    AddConst('Pi', Pi);

    // Functions
    AddFunc('LogBase10', LogBase10);
    AddFunc('LogBase2', LogBase2);
    AddFunc('LogBaseN', LogBaseN);
    AddFunc('ArcCos', ArcCos);
    AddFunc('ArcCot', ArcCot);
    AddFunc('ArcCsc', ArcCsc);
    AddFunc('ArcSec', ArcSec);
    AddFunc('ArcSin', ArcSin);
    AddFunc('ArcTan', ArcTan);
    AddFunc('ArcTan2', ArcTan2);
    AddFunc('Cos', Cos);
    AddFunc('Cot', Cot);
    AddFunc('Coversine', Coversine);
    AddFunc('Csc', Csc);
    AddFunc('Exsecans', Exsecans);
    AddFunc('Haversine', Haversine);
    AddFunc('Sec', Sec);
    AddFunc('Sin', Sin);
    AddFunc('Tan', Tan);
    AddFunc('Versine', Versine);
    AddFunc('ArcCosH', ArcCosH);
    AddFunc('ArcCotH', ArcCotH);
    AddFunc('ArcCscH', ArcCscH);
    AddFunc('ArcSecH', ArcSecH);
    AddFunc('ArcSinH', ArcSinH);
    AddFunc('ArcTanH', ArcTanH);
    AddFunc('CosH', CosH);
    AddFunc('CotH', CotH);
    AddFunc('CscH', CscH);
    AddFunc('SecH', SecH);
    AddFunc('SinH', SinH);
    AddFunc('TanH', TanH);
  end;
  with FuncList do
  begin
    Add('LogBase10');
    Add('LogBase2');
    Add('LogBaseN');
    Add('ArcCos');
    Add('ArcCot');
    Add('ArcCsc');
    Add('ArcSec');
    Add('ArcSin');
    Add('ArcTan');
    Add('ArcTan2');
    Add('Cos');
    Add('Cot');
    Add('Coversine');
    Add('Csc');
    Add('Exsecans');
    Add('Haversine');
    Add('Sec');
    Add('Sin');
    Add('Tan');
    Add('Versine');
    Add('ArcCosH');
    Add('ArcCotH');
    Add('ArcCscH');
    Add('ArcSecH');
    Add('ArcSinH');
    Add('ArcTanH');
    Add('CosH');
    Add('CotH');
    Add('CscH');
    Add('SecH');
    Add('SinH');
    Add('TanH');
  end;
end;

function ResultAsText(Evaluator: TEvaluator; const Input: string): string;
begin
  try
    Result := FloatToStr(Evaluator.Evaluate(Input));
  except
    on E: Exception do
      Result := E.Message;
  end;
end;


end.
