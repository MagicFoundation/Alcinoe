unit genericinterfacemethoddelegation;

interface

uses
  SysUtils;

type
  TGenerator<T1, TResult> = class(TInterfacedObject,
    TFunc<T1, IEnumerable<TResult>>)
  private
    function TFunc<T1, IEnumerable<TResult>>.Invoke = Bind;
  public
    constructor Create(const proc: TProc<T1>);
    function Bind(arg1: T1): IEnumerable<TResult>;
  end;

implementation

function TGenerator<T1, TResult>.Bind(arg1: T1): IEnumerable<TResult>;
begin
end;

constructor TGenerator<T1, TResult>.Create(const proc: TProc<T1>);
begin
end;

end.