//
// VKScene project based on GLScene library, http://glscene.sourceforge.net 
//
{ 
   A shader that applies a render pass for each material in
   its assigned MaterialLibrary. 
   
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
