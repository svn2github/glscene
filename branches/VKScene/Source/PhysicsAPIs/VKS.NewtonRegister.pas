//
// VKScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
  Design time registration code for the Newton Manager. 
}

unit VKS.NewtonRegister;

interface

uses
  System.Classes, VKS.NGDManager;

procedure register;

//=========================================================
implementation
//=========================================================

procedure register;
begin
  RegisterClasses([TVKNGDManager, TVKNGDDynamic, TVKNGDStatic]);
  RegisterComponents('VKScene', [TVKNGDManager]);
end;

end.
