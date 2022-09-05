{  Ce fichier est automatiquement créé par Lazarus. Ne pas le modifier!
  Ce code source est utilisé seulement pour compiler et installer
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
