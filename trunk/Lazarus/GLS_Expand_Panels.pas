{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit GLS_Expand_Panels; 

interface

uses
  GLExpandPanels, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('GLExpandPanels', @GLExpandPanels.Register); 
end; 

initialization
  RegisterPackage('GLS_Expand_Panels', @Register); 
end.
