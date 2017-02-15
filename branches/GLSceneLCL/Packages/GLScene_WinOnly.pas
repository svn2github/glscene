{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit GLScene_WinOnly; 

interface

uses
    GLJoystick, GLScreenSaver, GLSMWaveOut, GLAVIRecorder, 
  GLSceneRegisterWinOnlyLCL, GLSVfw, GLSpaceText, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('GLSceneRegisterWinOnly', @GLSceneRegisterWinOnly.Register
    ); 
end; 

initialization
  RegisterPackage('GLScene_WinOnly', @Register); 
end.
