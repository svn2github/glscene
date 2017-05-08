//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{ 
   A shader that applies a render pass for each material in
   its assigned MaterialLibrary. 
   
}
unit VXS.MultiMaterialShader;

interface

uses
  System.Classes, 
  
  VXS.Material, 
  VXS.RenderContextInfo, 
  VXS.State;

type
   TVXMultiMaterialShader = class(TVXShader)
      private
         FPass : Integer;
         FMaterialLibrary : TVXMaterialLibrary;
         FVisibleAtDesignTime: boolean;
         FShaderActiveAtDesignTime : boolean;
    FShaderStyle: TVXShaderStyle;
    procedure SetVisibleAtDesignTime(const Value: boolean);
    procedure SetShaderStyle(const Value: TVXShaderStyle);
      protected
         procedure SetMaterialLibrary(const val : TVXMaterialLibrary);
         procedure DoApply(var rci : TVXRenderContextInfo; Sender : TObject); override;
         function DoUnApply(var rci : TVXRenderContextInfo) : Boolean; override;
      public
         constructor Create(aOwner : TComponent); override;
      published
         property MaterialLibrary : TVXMaterialLibrary read FMaterialLibrary write SetMaterialLibrary;
         property VisibleAtDesignTime : boolean read FVisibleAtDesignTime write SetVisibleAtDesignTime;
         property ShaderStyle:TVXShaderStyle read FShaderStyle write SetShaderStyle;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------
// ------------------ TVXMultiMaterialShader ------------------
// ------------------

// Create
//
constructor TVXMultiMaterialShader.Create(aOwner : TComponent);
begin
   inherited;
   FShaderStyle:=ssReplace;
   FVisibleAtDesignTime := False;
end;

// DoApply
//
procedure TVXMultiMaterialShader.DoApply(var rci: TVXRenderContextInfo; Sender : TObject);
begin
   if not Assigned(FMaterialLibrary) then exit;

   FShaderActiveAtDesignTime := FVisibleAtDesignTime;

   FPass:=1;
   if (not (csDesigning in ComponentState)) or FShaderActiveAtDesignTime then begin
      rci.ignoreDepthRequests := True;
      rci.VKStates.Enable(stDepthTest);
      rci.VKStates.DepthFunc := cfLEqual;
      if FMaterialLibrary.Materials.Count>0 then
         FMaterialLibrary.Materials[0].Apply(rci);
      rci.ignoreDepthRequests := False;
  end;
end;

// DoUnApply
//
function TVXMultiMaterialShader.DoUnApply(
   var rci: TVXRenderContextInfo): Boolean;
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
         rci.VKStates.DepthFunc := cfLess;
         exit;
      end;
      FMaterialLibrary.Materials[FPass].Apply(rci);
      Result:=True;
      Inc(FPass);
   end;
end;

// SetMaterialLibrary
//
procedure TVXMultiMaterialShader.SetMaterialLibrary(
   const val: TVXMaterialLibrary);
begin
   if val<>FMaterialLibrary then begin
      FMaterialLibrary:=val;
      NotifyChange(Self);
   end;
end;

procedure TVXMultiMaterialShader.SetShaderStyle(const Value: TVXShaderStyle);
begin
  FShaderStyle := Value;
  inherited ShaderStyle :=FShaderStyle;
end;

procedure TVXMultiMaterialShader.SetVisibleAtDesignTime(
  const Value: boolean);
begin
  FVisibleAtDesignTime := Value;
  if csDesigning in ComponentState then
     NotifyChange(Self);
end;

end.
