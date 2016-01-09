{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit GLSceneLCL_SDL;

interface

uses
  GLSDLWindow, GLSDLContext, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('GLSDLWindow', @GLSDLWindow.Register);
  RegisterUnit('GLSDLContext', @GLSDLContext.Register);
end;

initialization
  RegisterPackage('GLSceneLCL_SDL', @Register);
end.
