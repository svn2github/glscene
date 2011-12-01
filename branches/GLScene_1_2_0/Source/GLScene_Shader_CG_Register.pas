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
unit GLScene_Shader_CG_Register;

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
    GLScene_Register_VCL,
  {$ELSE FPC}
    propedits, GLScene_Register_LCL,
  {$ENDIF FPC}
  // GLScene_Core
  GLScene_Material,

  // GLScene_Shader_CG
  GLScene_Shader_CG, GLScene_Shader_CG_GL, GLScene_Shader_CG_Components, GLScene_Shader_CG_Bomb;

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
