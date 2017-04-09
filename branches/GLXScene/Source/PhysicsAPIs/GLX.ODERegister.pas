//
// VKScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
  GLX.ODERegister - Design time registration code for the ODE Manager
}

unit GLX.ODERegister;

interface

uses
  System.Classes, GLX.ODEManager;

procedure Register;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// Register
//
procedure Register;
begin
  RegisterClasses([TGLODEManager, TGLODEJointList, TODEJoints, TODEElements]);
  RegisterComponents('GLScene',[TGLODEManager,TGLODEJointList]);
end;

end.
