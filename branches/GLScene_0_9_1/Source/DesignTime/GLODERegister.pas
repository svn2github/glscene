{
  GLODERegister - Design time registration code for the ODE Manager

  History:

    18/06/03 - SG - Creation.
}
unit GLODERegister;

interface

uses
  Classes, GLODEManager, GLSceneRegister;

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
  RegisterComponents('GLScene', [TGLODEManager, TGLODEJointList]);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

  {
  // There is a problem here when the package is installed and then
  // removed, the scene objects remain in the list without the classes
  // there to be linked to. Unregistering them fixes this problem but
  // Delphi7 doesn't seem to like the UnregisterSceneObject. So I'm
  // commenting this registration out until the problem is resolved.
  with ObjectManager do begin
    RegisterSceneObject(TGLODEDummy,'GLODEDummy','ODE Objects');
    RegisterSceneObject(TGLODEPlane,'GLODEPlane','ODE Objects');
  end;
  //}

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
finalization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

  {
  // See the comments above
  with ObjectManager do begin
    UnregisterSceneObject(TGLODEDummy);
    UnregisterSceneObject(TGLODEPlane);
  end;
  //}

end.
