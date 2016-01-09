{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit GLSceneLCL_ODE;

interface

uses
  GLODERegister, GLODECustomColliders, GLODEManager, ODEImport, ODEGL, 
  GLODERagdoll, GLODESkeletonColliders, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('GLODERegister', @GLODERegister.Register);
end;

initialization
  RegisterPackage('GLSceneLCL_ODE', @Register);
end.
