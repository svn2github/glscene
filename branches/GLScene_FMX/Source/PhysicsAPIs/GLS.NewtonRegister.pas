//
// This unit is part of the GLScene Project   
//
{ : GLS.NewtonRegister<p>

  Design time registration code for the Newton Manager.<p>

  <b>History : </b><font size=-1><ul>
  <li>27/12/14 - PW - Upgraded to support FMX
  <li>04/01/11 - FP - Removed Joint
  <li>15/07/10 - FP - Creation by Franck Papouin
  </ul></font>
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
  RegisterClasses([TGLNGDManager, TGLNGDDynamic, TGLNGDStatic]);
  RegisterComponents('GLScene', [TGLNGDManager]);
end;

end.
