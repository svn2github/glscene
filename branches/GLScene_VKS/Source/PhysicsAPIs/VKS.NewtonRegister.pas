//
// VKScene project based on GLScene library, http://glscene.sourceforge.net 
//
{
  Design time registration code for the Newton Manager. 
}

unit VKS.NewtonRegister;

interface

uses
  System.Classes, VKS.NGDManager;

procedure register;

implementation

// Register
//
procedure register;
begin
  RegisterClasses([TVKNGDManager, TVKNGDDynamic, TVKNGDStatic]);
  RegisterComponents('GLScene', [TVKNGDManager]);
end;

end.
