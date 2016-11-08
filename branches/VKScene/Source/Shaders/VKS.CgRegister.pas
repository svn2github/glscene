//
// VKScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
   Registration unit for CG shader. 
}
unit VKS.CgRegister;

interface

{$I VKScene.inc}

uses
  System.Classes,
  //DesignIntf, DesignEditors, VCLEditors,
  //VKS
  VKS.SceneRegister,
  VKS.Material,

  // CG
  Cg, CgGL, VKS.CgShader, VKS.CgBombShader;

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
