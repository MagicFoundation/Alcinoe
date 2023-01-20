unit ALMacapiObjCRuntime;

interface

uses Macapi.ObjCRuntime;

//very good article but written in russian: https://habrahabr.ru/post/325204/

function imp_implementationWithBlock(block: pointer): pointer; cdecl; external libobjc name  _PU + 'imp_implementationWithBlock';
function imp_removeBlock(anImp: pointer): Integer; cdecl; external libobjc name _PU + 'imp_removeBlock';

implementation

end.
