//
// GLScene on Vulkan, http://glscene.sourceforge.net 
//
{
  Design time registration code for the Newton Manager. 
}

unit GLS.NewtonRegister;

interface

uses
  System.Classes, GLS.NGDManager;

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
