unit endtoken;

interface

function BitsHighest(X: Cardinal): Integer;

implementation

// Bit manipulation
function BitsHighest(X: Cardinal): Integer;
asm
  {$IFDEF CPU32}
  // --> EAX X
  // <-- EAX
  MOV     ECX, EAX
  MOV     EAX, -1
  BSR     EAX, ECX
  JNZ     @@End
  MOV     EAX, -1
@@End:
  {$ENDIF CPU32}
  {$IFDEF CPU64}
  // --> ECX X
  // <-- RAX
  MOV     EAX, -1
  MOV     R10D, EAX
  BSR     EAX, ECX
  CMOVZ   EAX, R10D
  {$ENDIF CPU64}
end;

end.
