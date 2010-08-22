unit GLSS_FMOD; 

interface

uses
    GLSMFMOD, fmod, fmodpresets, fmodtypes, fmoderrors, fmoddyn, 
  LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('GLSMFMOD', @GLSMFMOD.Register); 
end; 

initialization
  RegisterPackage('GLSS_FMOD', @Register); 
end.
