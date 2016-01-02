//
// This unit is part of the GLScene Project   
//
{: VKS.MultiMaterialShader<p>

   A shader that applies a render pass for each material in
   its assigned MaterialLibrary.<p>

   <b>History : </b><font size=-1><ul>
      <li>22/04/10 - Yar - Fixes after VKS.State revision
      <li>05/03/10 - DanB - Added more state to TVKStateCache
      <li>03/07/09 - DanB - bug fix to allow multi-pass materials to be used by TVKMultiMaterialShader
      <li>20/01/09 - Mrqzzz - Published property "Shaderstyle"
                             (allows f.ex to have multiple textures using lightmaps)
      <li>25/10/07 - Mrqzzz - commented "glPushAttrib(GL_ALL_ATTRIB_BITS);" in DoApply
                              and "glPopAttrib;" in DoUnapply, which seems to fix
                              issues with other objects and materials in the scene.

      <li>25/02/07 - DaStr - Moved registration to GLSceneRegister.pas
      <li>24/05/04 - Mrqzzz - Re-added design-time rendering option
                          (seems stable now)
      <li>29/07/03 - SG - Removed design-time rendering option
                          (shader unstable at design-time)
      <li>29/07/03 - SG - Creation
   </ul></font>
}
unit VKS.MultiMaterialShader;

interface

uses
  System.Classes, 
  //VKS
  VKS.Material, VKS.RenderContextInfo, VKS.State;

type
   TVKMultiMaterialShader = class(TVKShader)
      private
         FPass : Integer;
         FMaterialLibrary : TVKMaterialLibrary;
         FVisibleAtDesignTime: boolean;
         FShaderActiveAtDesignTime : boolean;
    FShaderStyle: TVKShaderStyle;
    procedure SetVisibleAtDesignTime(const Value: boolean);
    procedure SetShaderStyle(const Value: TVKShaderStyle);
      protected
         procedure SetMaterialLibrary(const val : TVKMaterialLibrary);
         procedure DoApply(var rci : TRenderContextInfo; Sender : TObject); override;
         function DoUnApply(var rci : TRenderContextInfo) : Boolean; override;
      public
         constructor Create(aOwner : TComponent); override;
      published
         property MaterialLibrary : TVKMaterialLibrary read FMaterialLibrary write SetMaterialLibrary;
         property VisibleAtDesignTime : boolean read FVisibleAtDesignTime write SetVisibleAtDesignTime;
         property ShaderStyle:TVKShaderStyle read FShaderStyle write SetShaderStyle;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------
// ------------------ TVKMultiMaterialShader ------------------
// ------------------

// Create
//
constructor TVKMultiMaterialShader.Create(aOwner : TComponent);
begin
   inherited;
   FShaderStyle:=ssReplace;
   FVisibleAtDesignTime := False;
end;

// DoApply
//
procedure TVKMultiMaterialShader.DoApply(var rci: TRenderContextInfo; Sender : TObject);
begin
   if not Assigned(FMaterialLibrary) then exit;

   FShaderActiveAtDesignTime := FVisibleAtDesignTime;

   FPass:=1;
   if (not (csDesigning in ComponentState)) or FShaderActiveAtDesignTime then begin
      rci.ignoreDepthRequests := True;
      rci.GLStates.Enable(stDepthTest);
      rci.GLStates.DepthFunc := cfLEqual;
      if FMaterialLibrary.Materials.Count>0 then
         FMaterialLibrary.Materials[0].Apply(rci);
      rci.ignoreDepthRequests := False;
  end;
end;

// DoUnApply
//
function TVKMultiMaterialShader.DoUnApply(
   var rci: TRenderContextInfo): Boolean;
begin
   Result:=False;
   if not Assigned(FMaterialLibrary) then exit;
   if (not (csDesigning in ComponentState)) or FShaderActiveAtDesignTime then begin
      if FMaterialLibrary.Materials.Count>0 then
         // handle multi-pass materials
         if FMaterialLibrary.Materials[FPass-1].UnApply(rci) then
         begin
           Result:=true;
           Exit;
         end;
      if (FPass >= FMaterialLibrary.Materials.Count) then begin
         rci.GLStates.DepthFunc := cfLess;
         exit;
      end;
      FMaterialLibrary.Materials[FPass].Apply(rci);
      Result:=True;
      Inc(FPass);
   end;
end;

// SetMaterialLibrary
//
procedure TVKMultiMaterialShader.SetMaterialLibrary(
   const val: TVKMaterialLibrary);
begin
   if val<>FMaterialLibrary then begin
      FMaterialLibrary:=val;
      NotifyChange(Self);
   end;
end;

procedure TVKMultiMaterialShader.SetShaderStyle(const Value: TVKShaderStyle);
begin
  FShaderStyle := Value;
  inherited ShaderStyle :=FShaderStyle;
end;

procedure TVKMultiMaterialShader.SetVisibleAtDesignTime(
  const Value: boolean);
begin
  FVisibleAtDesignTime := Value;
  if csDesigning in ComponentState then
     NotifyChange(Self);
end;

end.
