{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit GLS_Computing_DesignTime; 

interface

uses
  GLSComputingRegister, GLSCUDAEditor, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('GLSComputingRegister', @GLSComputingRegister.Register); 
end; 

initialization
  RegisterPackage('GLS_Computing_DesignTime', @Register); 
end.
