//
// VKScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
  VKS.ODERegister - Design time registration code for the ODE Manager
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

procedure Register;
begin
  RegisterClasses([TVKODEManager, TVKODEJointList, TODEJoints, TODEElements]);
  RegisterComponents('VKScene',[TVKODEManager,TVKODEJointList]);
end;

end.
