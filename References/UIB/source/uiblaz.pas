{  Ce fichier est automatiquement cr�� par Lazarus. Ne pas le modifier!
  Ce code source est utilis� seulement pour compiler et installer
  le paquet UIBLaz 1.0.
 }

unit UIBLaz; 

interface

uses
  registeruib, uib, uibdataset, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('registeruib', @registeruib.Register); 
end; 

initialization
  RegisterPackage('UIBLaz', @Register); 
end.
