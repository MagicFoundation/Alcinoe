unit externalfunction;

interface

type
  TMyClass = class
  strict protected
    procedure ProcessMsg(var Msg: TMessage); message WM_USER;
  end;

implementation

end.