{
  GLODERegister - Design time registration code for the ODE Manager

  History:

    18/06/03 - SG - Creation.
}
unit GLScene.ODE.Register;

interface

uses
  Classes, GLScene.ODE.Manager;

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
