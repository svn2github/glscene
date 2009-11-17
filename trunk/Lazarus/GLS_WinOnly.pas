unit GLS_WinOnly; 

interface

uses
    Joystick, ScreenSaver, GLLCLFullScreenViewer, GLSMWaveOut, 
  NonGLSceneRegisterLCL, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('NonGLSceneRegisterLCL', @NonGLSceneRegisterLCL.Register); 
end; 

initialization
  RegisterPackage('GLS_WinOnly', @Register); 
end.
