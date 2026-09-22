unit Unit1;

interface

uses
  UnitA;

{$I ..\sub1inc\include.inc}
{$I ..\subinc\include.inc}

const
  Unit1Folder = 'sub1:' + UnitA.ID;

implementation

end.
