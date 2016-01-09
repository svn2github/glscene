{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit GLSceneLCL_CgShader; 

interface

uses
   GLCgRegister, GLCgShader, cg, cgGL, GLCgBombShader, 
   cgPostTransformationShader, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('GLCgRegister', @GLCgRegister.Register); 
end; 

initialization
  RegisterPackage('GLSceneLCL_CgShader', @Register); 
end.
