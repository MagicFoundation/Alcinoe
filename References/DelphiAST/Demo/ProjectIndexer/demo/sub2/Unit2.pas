unit Unit2;

interface

uses
  Unit1,
  UnitA;

{$I ..\sub2inc\include.inc}
{$I ..\subinc\include.inc}

function Unit1FolderIndirect: string;

implementation

function Unit1FolderIndirect: string;
begin
  Result := Unit1Folder + ':' + UnitA.ID;
end;

end.
