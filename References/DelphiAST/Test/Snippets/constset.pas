unit constset;

interface

type
  TClass = class
  public type
    TInnerEnum = (eOne, eTwo, wThree);
  end;

const
  cConstant: set of TClass.TInnerEnum = [
      TClass.TInnerEnum.eOne,
      TClass.TInnerEnum.eTwo,
      TClass.TInnerEnum.wThree
   ];


implementation

end.
