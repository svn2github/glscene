{
  GLODERegister - Design time registration code for the ODE Manager

  History:

    18/06/03 - SG - Creation.
}
unit GLODERegister;

interface

uses
  Classes, GLODEManager, GLSceneRegister, XCollection;

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
  RegisterClasses([TGLODEManager, TGLODEJointList,
                   TGLODEDummy, TGLODEPlane,
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

//  RegisterXCollectionItemClass(TGLODEDynamicBehaviour);

  with ObjectManager do begin
    RegisterSceneObject(TGLODEDummy,'GLODEDummy','ODE Objects');
    RegisterSceneObject(TGLODEPlane,'GLODEPlane','ODE Objects');
  end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
finalization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

//  UnregisterXCollectionItemClass(TGLODEDynamicBehaviour);

  with ObjectManager do begin
    UnregisterSceneObject(TGLODEDummy);
    UnregisterSceneObject(TGLODEPlane);
  end;

end.
