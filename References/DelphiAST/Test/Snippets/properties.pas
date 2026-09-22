unit properties;

interface

type 
  TProps = class
  public  
	property Name: string read FName write FName;
	property ReadableName: string read FName;
	property WriteableName: string write FName;
	property Redeclared;
	property Width: TWidth read GetWidth write SetWidth stored IsWidthStored default 50;
	property Tag: Integer read FTag write FTag default 0;
	property Indexed[Index: integer]: string read GetByIndex write SetByIndexed;
	property ReadableIndexed[Index: integer]: string read GetByIndex;
	property WriteableIndexed[Index: integer]: string write SetByIndexed;
	property DefaultIndexed[Index: integer]: string read GetByIndex write SetByIndexed; default;
	property DefaultReadableIndexed[Index: integer]: string read GetByIndex; default;
	property DefaultWriteableIndexed[Index: integer]: string write SetByIndexed; default;
  end;

implementation

end.
