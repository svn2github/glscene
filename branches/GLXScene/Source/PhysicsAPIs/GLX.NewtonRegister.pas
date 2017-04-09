//
// VKScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
  Design time registration code for the Newton Manager. 
}

unit GLX.NewtonRegister;

interface

uses
  System.Classes, GLX.NGDManager;

procedure register;

implementation

// Register
//
procedure register;
begin
  RegisterClasses([TGLNGDManager, TGLNGDDynamic, TGLNGDStatic]);
  RegisterComponents('GLScene', [TGLNGDManager]);
end;

end.
