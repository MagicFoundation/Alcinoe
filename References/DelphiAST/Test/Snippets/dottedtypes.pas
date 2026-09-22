unit dottedtypes;

interface

uses 
  MyUnit;

type 
  TSample<T: MyUnit.TItem, MyUnit.MyType.IStuff> = class(MyUnit.TBaseClass, MyUnit.IStuff)                             
  public                     
    function DoStuff<T2: MyUnit.TMyObject>(Obj: MyUnit.TMyObject): MyUnit.TMyObject; 

    property Obj : TObj read FObj implements MyUnit.IStuff;
  end;  

implementation

function TSample<T>.DoStuff<T2>(Obj: MyUnit.TMyObject): MyUnit.TMyObject;
var
  Obj2: MyUnit.TMyObject;   
  Obj3, Obj4: MyUnit.TMyAdditionalObject; 
  Sample: TSample<MyUnit.TSpecialItem>; 
  MyObjectArray: array of MyUnit.TMyObject;
begin
  Sample := TSample<MyUnit.TSpecialItem>.Create;
  
  try 
    Sample.DoOtherStuff<MyUnit.TSpecialObject, MyUnit.TObject>(Obj2);
  except
    on E: MyUnit.MyException do
    begin
      WriteLn(E.Message);
    end;          
  end;
end;

end.