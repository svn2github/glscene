unit GLS_WinOnly; 

interface

uses
    Joystick, ScreenSaver, GLSMWaveOut, GLSVfw,
  GLAVIRecorder, GLSceneRegisterWinOnlyLCL, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('GLSceneRegisterWinOnlyLCL', @GLSceneRegisterWinOnlyLCL.Register); 
end; 

initialization
  RegisterPackage('GLS_WinOnly', @Register); 
end.
