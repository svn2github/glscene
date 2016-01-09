{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit GLSceneLCL_WinOnly; 

interface

uses
    GLJoystick, GLScreenSaver, GLSMWaveOut, GLAVIRecorder, 
  GLSceneRegisterWinOnlyLCL, GLSVfw, GLSpaceText, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('GLSceneRegisterWinOnlyLCL', @GLSceneRegisterWinOnlyLCL.Register
    ); 
end; 

initialization
  RegisterPackage('GLSceneLCL_WinOnly', @Register); 
end.
