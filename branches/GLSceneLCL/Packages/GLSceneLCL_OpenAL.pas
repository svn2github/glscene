unit GLSceneLCL_OpenAL; 

interface

uses
  GLSMOpenAL, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('GLSMOpenAL', @GLSMOpenAL.Register); 
end; 

initialization
  RegisterPackage('GLSceneLCL_OpenAL', @Register); 
end.
