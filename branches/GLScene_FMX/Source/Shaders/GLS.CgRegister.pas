//
// This unit is part of the GLScene Project   
//
{: GLS.CgRegister<p>

   Registration unit for CG shader.<p>

   <b>History :</b><font size=-1><ul>
      <li>11/11/09 - DaStr - Improved FPC compatibility
                             (thanks Predator) (BugtrackerID = 2893580)
      <li>23/02/07 - DaStr - Initial version

}
unit GLS.CgRegister;

interface

{$I GLScene.inc}

uses
  System.Classes,
  //DesignIntf, DesignEditors, VCLEditors,
  //GLS
  GLS.SceneRegister,
  GLS.Material,

  // CG
  Cg, CgGL, GLS.CgShader, GLS.CgBombShader;

procedure Register;

implementation

procedure Register;
begin
  // Register components.
  RegisterComponents('GLScene Shaders', [TCgShader, TGLCgBombShader]);

  // Register property editors.
  RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLCgBombShader, '', TGLLibMaterialNameProperty);
end;

end.
