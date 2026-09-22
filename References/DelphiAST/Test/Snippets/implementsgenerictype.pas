unit implementsgenerictype;

interface

type
  IFoo<T> = interface
  end;

  TBar = class(TInterfacedObject, IFoo<IInterface>)
  private
    FFoo : IFoo<IInterface>;
  public
    property Foo : IFoo<IInterface> read FFoo implements IFoo<IInterface>;
  end;

implementation

end.