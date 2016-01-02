//
// This unit is part of the GLScene Project   
//
{
  VKS.ODERegister - Design time registration code for the ODE Manager

  History:

    18/06/03 - SG - Creation.
}
unit VKS.ODERegister;

interface

uses
  System.Classes, VKS.ODEManager;

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
