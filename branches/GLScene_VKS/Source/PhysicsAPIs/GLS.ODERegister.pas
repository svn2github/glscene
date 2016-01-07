//
// GLScene on Vulkan, http://glscene.sourceforge.net 
//
{
  GLS.ODERegister - Design time registration code for the ODE Manager
}

unit GLS.ODERegister;

interface

uses
  System.Classes, GLS.ODEManager;

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
  RegisterClasses([TVKODEManager, TVKODEJointList, TODEJoints, TODEElements]);
  RegisterComponents('GLScene',[TVKODEManager,TVKODEJointList]);
end;

end.
