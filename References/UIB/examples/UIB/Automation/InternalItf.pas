unit InternalItf;

interface

type
  IDataPointer = interface(IUnKnown)
  ['{AB6056C1-C480-4B19-A017-9A726E342BF9}']
    function Data: pointer; stdcall;
  end;

implementation

end.
