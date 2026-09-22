// Dummy implementation of IOUtils in order to be able to compile Delphi AST with FPC
unit IOUtils;

interface

uses
  SysUtils;
  
type 
  TPath = class
  public
    class function Combine(const Path1, Path2: string): string; inline; static;
  end;	

implementation

class function TPath.Combine(const Path1, Path2: string): string; 
begin
  Result := ConcatPaths([Path1, Path2]);	
end;
	
end.