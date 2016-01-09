unit GLSceneLCL_BASS; 

interface

uses
  GLSMBASS, Bass, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('GLSMBASS', @GLSMBASS.Register); 
end; 

initialization
  RegisterPackage('GLSceneLCL_BASS', @Register); 
end.
