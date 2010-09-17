//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLNewtonRegister<p>

   Design time registration code for the Newton Manager.<p>

	<b>History : </b><font size=-1><ul>
      <li>15/07/10 - FP - Creation by Franck Papouin
	</ul></font>
}

unit GLNewtonRegister;

interface

uses
  Classes, GLNGDManager;

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
  RegisterClasses([TGLNGDManager, TGLNGDDynamic, TGLNGDStatic, TNGDMaterials,
    TGLNGDJointList, TNGDJoints]);
  RegisterComponents('GLScene', [TGLNGDManager, TGLNGDJointList]);
end;

end.
