unit genericconstraints;

interface

type
  TFoo<T: TComponent> = class(TObject);                          // T inherits from TComponent
  TFoo<TComma1, TComma2> = class(TObject);                       // TComma1 and TComma2 have no constraints
  TBar<TTwoConstraints: TComponent, IUnknown> = class(TObject);  // TTwoConstraints inherits from TComponent and implements IUnknown
  TBar<T1: TComponent; T2: IUnknown> = class(TObject);           // T1 inherits from TComponent, T2 implements IUnknown
  TBaz<TThreeConstraints: TComponent, IUnknown, constructor> = class(TObject);   // TThreeConstraints inherits from TComponent, implements IUnknown and has a constructor without parameters
  TBaz<TComma1, TComma2: class> = class(TObject);                // TComma1 and TComma2 are classes
  TBax<T1: class; T2: record> = class(TObject)                   // T1 is a class, T2 is a record
  public
    procedure DoStuff;
  end;

implementation

{ TBax<T1, T2> }

procedure TBax<T1, T2>.DoStuff;
begin
//
end;

end.
