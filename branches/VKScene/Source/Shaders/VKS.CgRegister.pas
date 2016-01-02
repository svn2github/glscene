//
// This unit is part of the GLScene Project   
//
{: VKS.CgRegister<p>

   Registration unit for CG shader.<p>

   <b>History :</b><font size=-1><ul>
      <li>11/11/09 - DaStr - Improved FPC compatibility
                             (thanks Predator) (BugtrackerID = 2893580)
      <li>23/02/07 - DaStr - Initial version

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
