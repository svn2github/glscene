//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLTextureSharingShader (originally GlProxMultiMatShader)<p>
    <p>
    This shader allows to apply multiple textures, gathering them from existing materials.
    This allows saving resources, since you can reference the textures of any material in
    any materialLibrary.
    Note that actually the component references a Material (not a texture) but
    it uses that material's texture. The referenced material settings will be ignored,
    but the texture's settings (like TextureMode, ImageGamma, ImageBrightness) will be used.
    Instead the local material settings (listed in the collection) will be used.
    </p>

	<b>History : </b><font size=-1><ul>
      <li>17/03/08 - mrqzzz - Added IGLMaterialLibrarySupported, moved registration
      <li>14/03/08 - Pascal - Initial version (contributed to GLScene)

}

unit GLTextureSharingShader;

interface
Uses
  // VCL
  Classes, SysUtils,
  // GLScene
  GLScene, VectorGeometry, GLMisc, GlColor, GLTexture, OpenGL1x,  GLVectorFileObjects,
  GLStrings;

type
  TMMat = Class (TCollectionItem,IGLMaterialLibrarySupported)
  private
  protected
    fTexMat : TMatrix ;
    ftexMatRec : Boolean ;
    FMat: TGLLibMaterial;
    FTexOffset: TGLCoordinates;
    FTexScale: TGLCoordinates;
    ftexMatUnit : Boolean ;
    FBlendingMode: TBlendingMode;
    FSpecular: TGLColor;
    FAmbient: TGLColor;
    FDiffuse: TGLColor;
    FEmission: TGLColor;
    FShininess: TShininess;
    FMaterialLibrary: TGLMaterialLibrary;
    FLibMaterialName: TGLLibMaterialName;

    // Implementing IGLMaterialLibrarySupported.
    function GetMaterialLibrary: TGLMaterialLibrary; virtual;
    // Implementing IInterface.
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    function _AddRef: Integer; virtual; stdcall;
    function _Release: Integer; virtual; stdcall;

    procedure SetAmbient(const Value: TGLColor);
    procedure SetDiffuse(const Value: TGLColor);
    procedure SetEmission(const Value: TGLColor);
    procedure SetShininess(const Value: TShininess);
    procedure SetSpecular(const Value: TGLColor);
    procedure SetMaterialLibrary(const Value: TGLMaterialLibrary);
    procedure SetLibMaterialName(const Value: TGLLibMaterialName);
    procedure SetBlendingMode(const Value: TBlendingMode);
    procedure SetMat(const Value: TGLLibMaterial);
    procedure SetTexOffset(const Value: TGLCoordinates);
    procedure SetTexScale(const Value: TGLCoordinates);
    function  getTexMat: TMatrix;
    function gettexMatUnit: boolean;
    procedure coordNotifychange (Sender: TObject) ;
    procedure OtherNotifychange (Sender: TObject) ;
    function GetDisplayName: string; override;
  public
    Procedure Apply (var rci: TRenderContextInfo) ;
    Procedure UnApply (var rci: TRenderContextInfo) ;
    constructor Create(Collection: TCollection); override ;
    Destructor Destroy ; override ;
    Property TexMat : TMatrix read getTexMat ;
    property Mat : TGLLibMaterial  read FMat write SetMat;
  published
    property TexOffset : TGLCoordinates  read FTexOffset write SetTexOffset;
    property TexScale : TGLCoordinates  read FTexScale write SetTexScale;
    property BlendingMode : TBlendingMode  read FBlendingMode write SetBlendingMode;
    property texMatUnit : boolean read gettexMatUnit ;
    property Emission : TGLColor  read FEmission write SetEmission;
    property Ambient : TGLColor  read FAmbient write SetAmbient;
    property Diffuse : TGLColor  read FDiffuse write SetDiffuse;
    property Specular : TGLColor  read FSpecular write SetSpecular;
    property Shininess : TShininess  read FShininess write SetShininess;
    property MaterialLibrary : TGLMaterialLibrary read FMaterialLibrary write SetMaterialLibrary;
    property LibMaterialName : TGLLibMaterialName read FLibMaterialName write SetLibMaterialName;
  end ;

  TGLTextureSharingShader = class ;

  TMaterialsCollection = class (TCollection)
  private
  protected
    fOwner : TGLTextureSharingShader ;
    function GetOwner: TPersistent; override;
    function GetItems(index: Integer): TMMat;
    procedure SetItems(index: Integer; const Value: TMMat);
  public
    function Add: TMMat;
    constructor Create(AOwner : TGLTextureSharingShader);
    property Items[index : Integer] : TMMat read GetItems write SetItems; default;
  end ;

  TGLTextureSharingShader = class(TGLShader)
  private
    FMaterials: TMaterialsCollection;
    procedure SetMaterials(const Value: TMaterialsCollection);
  protected
    fpass : integer ;
    procedure DoApply(var rci : TRenderContextInfo; Sender : TObject); override;
    function  DoUnApply(var rci : TRenderContextInfo) : Boolean; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(aOwner: TComponent); override ;
    destructor Destroy ; Override ;
    function AddLibMaterial (mat:TGLLibMaterial) : TMMat ;
    function FindLibMaterial (mat:TGLLibMaterial) : TMMat ;
    function count : integer ;
  published
    property Materials : TMaterialsCollection  read FMaterials write SetMaterials;
  end;



implementation
uses
  XOpenGL,GLState ;

{ TMMat }
const
   cPolygonMode : array [pmFill..pmPoints] of TGLEnum = (GL_FILL, GL_LINE, GL_POINT);

procedure TMMat.Apply(var rci: TRenderContextInfo);
begin
   if not assigned(fmat) then exit ;
   xglBeginUpdate;
   if Assigned(mat.Shader) then
   begin
      case mat.Shader.ShaderStyle of
         ssHighLevel : mat.Shader.Apply(rci, mat);
         ssReplace :
         begin
            mat.Shader.Apply(rci, mat);
            Exit;
         end;
      end;
   end;
   if not mat.Material.Texture.Disabled then
   begin
     if not (texMatUnit) then
     begin
         rci.GLStates.SetGLTextureMatrix(TexMat);
     end ;
   end ;

   if moNoLighting in mat.Material.MaterialOptions then
   begin
     if stLighting in rci.GLStates.States then
     begin
       rci.GLStates.UnSetGLState(stLighting);
       Inc(rci.lightingDisabledCounter);
     end;
   end;
   if stLighting in rci.GLStates.States then
   begin
      rci.GLStates.SetGLMaterialColors(GL_FRONT,
         Emission.Color,Ambient.Color,Diffuse.Color,Specular.Color,Shininess);
      rci.GLStates.SetGLPolygonMode(GL_FRONT, cPolygonMode[mat.Material.FrontProperties.PolygonMode]);
   end
   else
     mat.Material.FrontProperties.ApplyNoLighting(rci, GL_FRONT);
   if (stCullFace in rci.GLStates.States) then
   begin
     case mat.Material.FaceCulling of
       fcBufferDefault : if not rci.bufferFaceCull then
       begin
         rci.GLStates.UnSetGLState(stCullFace);
         mat.Material.BackProperties.Apply(rci, GL_BACK);
       end;
       fcCull : ; // nothing to do
       fcNoCull :
       begin
         rci.GLStates.UnSetGLState(stCullFace);
         mat.Material.BackProperties.Apply(rci, GL_BACK);
       end;
     else
       Assert(False);
     end;
   end
   else
   begin
     // currently NOT culling
     case mat.Material.FaceCulling of
       fcBufferDefault :
       begin
         if rci.bufferFaceCull then
           rci.GLStates.SetGLState(stCullFace)
         else
           mat.Material.BackProperties.Apply(rci, GL_BACK);
       end;
       fcCull : rci.GLStates.SetGLState(stCullFace);
       fcNoCull : mat.Material.BackProperties.Apply(rci, GL_BACK);
     else
       Assert(False);
     end;
   end;

   // Apply Blending mode
   if not rci.ignoreBlendingRequests then
     case BlendingMode of
       bmOpaque :
       begin
         rci.GLStates.UnSetGLState(stBlend);
         rci.GLStates.UnSetGLState(stAlphaTest);
       end;
       bmTransparency :
       begin
         rci.GLStates.SetGLState(stBlend);
         rci.GLStates.SetGLState(stAlphaTest);
         glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
       end;
       bmAdditive :
       begin
         rci.GLStates.SetGLState(stBlend);
         rci.GLStates.SetGLState(stAlphaTest);
         glBlendFunc(GL_SRC_ALPHA, GL_ONE);
       end;
       bmAlphaTest50 :
       begin
         rci.GLStates.UnSetGLState(stBlend);
         rci.GLStates.SetGLState(stAlphaTest);
         glAlphaFunc(GL_GEQUAL, 0.5);
       end;
       bmAlphaTest100 :
       begin
         rci.GLStates.UnSetGLState(stBlend);
         rci.GLStates.SetGLState(stAlphaTest);
         glAlphaFunc(GL_GEQUAL, 1.0);
       end;
       bmModulate :
       begin
         rci.GLStates.SetGLState(stBlend);
         rci.GLStates.SetGLState(stAlphaTest);
         glBlendFunc(GL_DST_COLOR, GL_ZERO);
       end;
     else
       Assert(False);
     end;
    // Fog switch
   if moIgnoreFog in mat.Material.MaterialOptions then
   begin
     if stFog in rci.GLStates.States then
     begin
       rci.GLStates.UnSetGLState(stFog);
       Inc(rci.fogDisabledCounter);
     end;
   end;

   if not Assigned(mat.Material.TextureEx) then
   begin
     if Assigned(mat.Material.Texture) then
       mat.Material.Texture.Apply(rci)
   end
   else
   begin
     if Assigned(mat.Material.Texture) and not mat.Material.TextureEx.IsTextureEnabled(0) then
        mat.Material.Texture.Apply(rci)
     else
     if mat.Material.TextureEx.Count>0 then
        mat.Material.TextureEx.Apply(rci);
   end;

   if Assigned(mat.Shader) then
   begin
      case mat.Shader.ShaderStyle of
         ssLowLevel : mat.Shader.Apply(rci, mat);
      end;
   end;
   xglEndUpdate;
end;

procedure TMMat.coordNotifychange(Sender: TObject);
begin
  ftexMatRec := true ;
  TMaterialsCollection(collection).fOwner.NotifyChange (self) ;
end;

constructor TMMat.Create(Collection: TCollection);
begin
  inherited ;
  FSpecular:= TGLColor.create (self);
  FSpecular.OnNotifyChange := OtherNotifychange ;
  FAmbient:= TGLColor.create (self);
  FAmbient.OnNotifyChange := OtherNotifychange ;
  FDiffuse:= TGLColor.create (self);
  FDiffuse.OnNotifyChange := OtherNotifychange ;
  FEmission:= TGLColor.create (self);
  FEmission.OnNotifyChange := OtherNotifychange ;

  FTexOffset := TGLCoordinates.create(self);
  FTexOffset.OnNotifyChange := coordNotifychange ;
  FTexScale := TGLCoordinates.create(self) ;
  FTexScale.OnNotifyChange := coordNotifychange ;
  ftexMatRec := true ;
end;

destructor TMMat.Destroy;
begin
  FSpecular.free;
  FAmbient.free;
  FDiffuse.free;
  FEmission.free;
  FTexOffset.free;
  FTexScale.free;
  inherited;
end;


function TMMat.GetDisplayName: string;
var
   st:string;
begin
     if Assigned(MaterialLibrary) then
        st:=MaterialLibrary.Name
     else
         st:='';
     result:= '['+st+'.'+self.LibMaterialName+']';
end;

function TMMat.GetMaterialLibrary: TGLMaterialLibrary;
begin
     result:=FMaterialLibrary;
end;

function TMMat.getTexMat: TMatrix;
begin
  if ftexMatRec then
  begin
    ftexMatUnit := false ;
    if not(TexOffset.Equals(NullHmgVector) and TexScale.Equals(XYZHmgVector)) then
       fTexMat:=CreateScaleAndTranslationMatrix(TexScale.AsVector,TexOffset.AsVector)
    else
      ftexMatUnit := true ;
    ftexMatRec := false ;
  end ;
  result := fTexMat ;
end;

function TMMat.gettexMatUnit: boolean;
begin
  if ftexMatRec then
    getTexMat ;
  result := ftexMatUnit ;
end;

procedure TMMat.OtherNotifychange(Sender: TObject);
begin
  TMaterialsCollection(collection).fOwner.NotifyChange (self) ;
end;

function TMMat.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then Result := S_OK else Result := E_NOINTERFACE;
end;

procedure TMMat.SetAmbient(const Value: TGLColor);
begin
  FAmbient := Value;
end;

procedure TMMat.SetBlendingMode(const Value: TBlendingMode);
begin
  FBlendingMode := value ;
end;

procedure TMMat.SetDiffuse(const Value: TGLColor);
begin
  FDiffuse := Value;
end;

procedure TMMat.SetEmission(const Value: TGLColor);
begin
  FEmission := Value;
end;

procedure TMMat.SetLibMaterialName(const Value: TGLLibMaterialName);
begin
  FLibMaterialName := Value;
  if (FLibMaterialName='') or (fMaterialLibrary=nil) then
    mat := nil
  else
  begin
    mat := fMaterialLibrary.LibMaterialByName (FLibMaterialName) ;
  end ;
end;

procedure TMMat.SetMat(const Value: TGLLibMaterial);
begin
  FMat := Value;
  if fmat<>nil then
  begin
    fLibMaterialName := fmat.DisplayName ;
    fMaterialLibrary := TGLMaterialLibrary(TGLLibMaterials(value.Collection).Owner) ;
    if not (csloading in TMaterialsCollection(collection).fowner.ComponentState) then
    begin
      TexOffset := Fmat.TextureOffset ;
      TexScale  := Fmat.TextureScale ;
      FBlendingMode := Fmat.Material.BlendingMode ;
      fEmission.Assign (fmat.Material.FrontProperties.Emission);
      fAmbient.Assign (fmat.Material.FrontProperties.Ambient);
      fDiffuse.Assign (fmat.Material.FrontProperties.Diffuse);
      fSpecular.Assign (fmat.Material.FrontProperties.Specular);
      fShininess := fmat.Material.FrontProperties.Shininess;
    end ;
  end ;
end;



procedure TMMat.SetMaterialLibrary(const Value: TGLMaterialLibrary);
begin
  FMaterialLibrary := Value;
  if (FLibMaterialName='') or (fMaterialLibrary=nil) then
    mat := nil
  else
  begin
    mat := fMaterialLibrary.LibMaterialByName (FLibMaterialName) ;
  end ;
end;

procedure TMMat.SetShininess(const Value: TShininess);
begin
  FShininess := Value;
end;

procedure TMMat.SetSpecular(const Value: TGLColor);
begin
  FSpecular := Value;
end;

procedure TMMat.SetTexOffset(const Value: TGLCoordinates);
begin
  FTexOffset.AsVector := Value.AsVector;
  ftexMatRec := true ;
end;

procedure TMMat.SetTexScale(const Value: TGLCoordinates);
begin
  FTexScale.AsVector := Value.AsVector;
  ftexMatRec := true ;
end;


procedure TMMat.UnApply(var rci: TRenderContextInfo);
begin
   if not assigned(fmat) then exit ;
   if Assigned(mat.Shader) then
   begin
      case mat.Shader.ShaderStyle of
         ssLowLevel : mat.Shader.UnApply(rci);
         ssReplace :
         begin
            mat.Shader.UnApply(rci);
            Exit;
         end;
      end;
   end;
   mat.Material.UnApply(rci);
   if not mat.Material.Texture.Disabled then
     if not(texMatUnit) then
     begin
       rci.GLStates.ResetGLTextureMatrix;
     end ;
   if Assigned(mat.Shader) then
   begin
     case mat.Shader.ShaderStyle of
        ssHighLevel : mat.Shader.UnApply(rci);
     end;
   end;
end;

function TMMat._AddRef: Integer;
begin
  Result := -1; //ignore
end;

function TMMat._Release: Integer;
begin
  Result := -1; //ignore
end;

{ TGLTextureSharingShader }

function TGLTextureSharingShader.AddLibMaterial(mat: TGLLibMaterial): TMMat;
begin
  result := FMaterials.add ;
  result.Mat := mat ;
end;

function TGLTextureSharingShader.count: integer;
begin
  result := Materials.count ;
end;

constructor TGLTextureSharingShader.Create(aOwner: TComponent);
begin
  inherited;
  FMaterials := TMaterialsCollection.Create (self) ;
  ShaderStyle:=ssReplace;
end;

destructor TGLTextureSharingShader.Destroy;
begin
  Materials.free ;
  inherited;
end;

procedure TGLTextureSharingShader.DoApply(var rci: TRenderContextInfo;
  Sender: TObject);
begin
  if Materials.count>0 then
  begin
    glEnable(GL_DEPTH_TEST);
    glDepthFunc(GL_LEQUAL);
    Materials[0].Apply (rci) ;
    fpass := 1 ;
  end ;
end;

function TGLTextureSharingShader.DoUnApply(
  var rci: TRenderContextInfo): Boolean;
begin
  result := false ;
  if Materials.count>0 then
  begin
    Materials[fpass-1].UnApply (rci) ;
    if fpass<Materials.count then
    begin
      Materials[fpass].Apply (rci) ;
      inc(fpass) ;
      result := true ;
    end
    else
    begin
      glDepthFunc(GL_LESS);
      fpass := 0 ;
    end
  end ;
end;

function TGLTextureSharingShader.FindLibMaterial(mat: TGLLibMaterial): TMMat;
var
 i : integer ;
begin
  result := nil ;
  for i := 0 to Materials.count-1 do
    if Materials[i].mat = mat then
    begin
      result := Materials[i] ;
      break ;
    end ;
end;

procedure TGLTextureSharingShader.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  t: Integer;
begin
  inherited;
  if Operation=opRemove then
  begin
       if AComponent is TGLMaterialLibrary then
       begin
            for t:=0 to Materials.Count-1 do
            begin
                 if Materials.Items[t].MaterialLibrary = AComponent then
                    Materials.Items[t].MaterialLibrary:=nil;
            end;
       end;
  end;
end;

procedure TGLTextureSharingShader.SetMaterials(
  const Value: TMaterialsCollection);
begin
  FMaterials := Value;
end;

{ TMaterialsCollection }

{ TMaterialsCollection }

function TMaterialsCollection.Add: TMMat;
begin
  result:=(inherited Add) as TMMat;
end;

constructor TMaterialsCollection.Create(AOwner: TGLTextureSharingShader);
begin
  FOwner:=AOwner;
  inherited Create(TMMat);
end;

function TMaterialsCollection.GetItems(index: Integer): TMMat;
begin
 result:=(inherited Items[index]) as TMMat ;
end;

function TMaterialsCollection.GetOwner: TPersistent;
begin
  result := fOwner ;
end;

procedure TMaterialsCollection.SetItems(index: Integer;
  const Value: TMMat);
begin
 inherited Items[index]:=value;
end;


initialization
  RegisterClasses([TGLTextureSharingShader]);




end.

