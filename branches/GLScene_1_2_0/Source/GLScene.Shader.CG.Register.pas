//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLCgRegister<p>

   Registration unit for CG shader.<p>

   <b>History :</b><font size=-1><ul>
      <li>11/11/09 - DaStr - Improved FPC compatibility
                             (thanks Predator) (BugtrackerID = 2893580)
      <li>23/02/07 - DaStr - Initial version

}
unit GLScene.Shader.CG.Register;

interface

{$I GLScene.inc}

uses
  // VCL
  Classes,
  {$IFNDEF FPC}
    {$IFDEF GLS_DELPHI_6_UP}
      DesignIntf, DesignEditors, VCLEditors,
    {$ELSE}
      DsgnIntf,
    {$ENDIF}
    GLScene.Register.VCL,
  {$ELSE FPC}
    propedits, GLScene.Register.LCL,
  {$ENDIF FPC}
  // GLScene.Core
  GLScene.Material,

  // GLScene.Shader.CG
  GLScene.Shader.CG, GLScene.Shader.CG.GL, GLScene.Shader.CG.Components, GLScene.Shader.CG.Bomb;

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
