{
  GLODERegister - Design time registration code for the ODE Manager

  History:

    18/06/03 - SG - Creation.
}
unit GLODERegister;

interface

uses
  Classes, GLSceneRegister, GLODEManager;

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
  RegisterClasses([TGLODEManager, TGLODEJointList, TGLODEDummy,
                   TGLODEPlane, TGLODEStaticDummy, TGLODETerrain,
                   TODEJoints, TODEElements]);
  RegisterComponents('GLScene',[TGLODEManager,TGLODEJointList]);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

  {with ObjectManager do begin
    RegisterSceneObject(TGLODEDummy,'GLODEDummy','ODE Objects');
    RegisterSceneObject(TGLODEStaticDummy,'GLODEStaticDummy','ODE Objects');
    RegisterSceneObject(TGLODEPlane,'GLODEPlane','ODE Objects');
    RegisterSceneObject(TGLODETerrain,'GLODETerrain','ODE Objects');
  end;//}

end.
