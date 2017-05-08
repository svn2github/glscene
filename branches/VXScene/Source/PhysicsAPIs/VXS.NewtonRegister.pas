//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
  Design time registration code for the Newton Manager. 
}

unit VXS.NewtonRegister;

interface

uses
  System.Classes, VXS.NGDManager;

procedure register;

//=========================================================
implementation
//=========================================================

procedure register;
begin
  RegisterClasses([TVXNGDManager, TVXNGDDynamic, TVXNGDStatic]);
  RegisterComponents('VXScene', [TVXNGDManager]);
end;

end.
