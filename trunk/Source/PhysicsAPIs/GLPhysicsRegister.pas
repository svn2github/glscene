//
// This unit is part of the GLScene Project, http://glscene.org
//
{
  DesignTime registration code for the Physics Managers

  History:
    12/01/16 - PW - Combined ODE&NGD register procedures
    18/06/03 - SG - Creation.
}

unit GLPhysicsRegister;

interface

uses
  System.Classes,
  GLODEManager,
  GLNGDManager,
  GLPhysics;

procedure Register;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

procedure Register;
begin
  //ODE
  RegisterClasses([TGLODEManager, TGLODEJointList, TODEJoints, TODEElements]);
  RegisterComponents('GLScene',[TGLODEManager, TGLODEJointList]);
  //NGD
  RegisterClasses([TGLNGDManager, TGLNGDDynamic, TGLNGDStatic]);
  RegisterComponents('GLScene',[TGLNGDManager, TGLPhysicsManager]);
end;

end.
