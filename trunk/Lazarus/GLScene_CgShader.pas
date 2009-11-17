unit GLScene_CgShader; 

interface

uses
  GLCgRegister, GLCgShader, cg, cgGL, GLCgBombShader, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('GLCgRegister', @GLCgRegister.Register); 
end; 

initialization
  RegisterPackage('GLScene_CgShader', @Register); 
end.
