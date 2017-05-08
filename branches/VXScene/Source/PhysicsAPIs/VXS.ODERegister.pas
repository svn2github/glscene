//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
  VXS.ODERegister - Design time registration code for the ODE Manager
}

unit VXS.ODERegister;

interface

uses
  System.Classes, VXS.ODEManager;

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
  RegisterClasses([TVXODEManager, TVXODEJointList, TODEJoints, TODEElements]);
  RegisterComponents('VXScene',[TVXODEManager,TVXODEJointList]);
end;

end.
