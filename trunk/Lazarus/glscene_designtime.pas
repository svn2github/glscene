{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit GLScene_DesignTime; 

interface

uses
GLLazarusRegister, FVectorEditor, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('GLLazarusRegister', @GLLazarusRegister.Register); 
end; 

initialization
  RegisterPackage('GLScene_DesignTime', @Register); 
end.
