//
// GLScene on Vulkan, http://glscene.sourceforge.net 
//
{
   Registration unit for CG shader. 
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
  RegisterComponents('GLScene Shaders', [TCgShader, TVKCgBombShader]);

  // Register property editors.
  RegisterPropertyEditor(TypeInfo(TVKLibMaterialName), TVKCgBombShader, '', TVKLibMaterialNameProperty);
end;

end.
