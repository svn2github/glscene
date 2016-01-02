//
// This unit is part of the GLScene Project   
//
{ : VKS.NewtonRegister<p>

  Design time registration code for the Newton Manager.<p>

  <b>History : </b><font size=-1><ul>
  <li>27/12/14 - PW - Upgraded to support FMX
  <li>04/01/11 - FP - Removed Joint
  <li>15/07/10 - FP - Creation by Franck Papouin
  </ul></font>
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
