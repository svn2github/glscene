unit uMaterials;

interface

uses
  uMesh, uClasses, GLVectorGeometry, GLColor, SysUtils, GLMaterial,
  GLTexture{, uPngCopy};

procedure ResetMaterialData  (var MaterialData: TMaterialData);
procedure ApplyMaterialData  (MatLib: TGLMaterialLibrary; Obj: TMesh);
procedure SetMaterialTexture (fName: AnsiString; mat : TGlMaterial);

implementation

procedure ResetMaterialData(var MaterialData: TMaterialData);
begin
  MaterialData.Texture.Scale  := VectorMake(1,1,1);
  MaterialData.Texture.Offset := VectorMake(1,1,1);
end;

procedure ApplyMaterialData(MatLib: TGLMaterialLibrary; Obj: TMesh);
var
  aa: AnsiString;
begin
  with MatLib.Materials.GetLibMaterialByName(Obj.MaterialData.Name) do
  begin
    Material.Texture.Enabled := Obj.MaterialData.Texture.Enable;
    
    if Trim(Obj.MaterialData.Texture.FileName) <> '' then
       Material.Texture.Image.LoadFromFile(Obj.MaterialData.Texture.FileName)
    else Material.Texture.Enabled:= False;

    //

    TextureScale.AsVector := Obj.MaterialData.Texture.Scale;
    Material.BlendingMode := bmOpaque;

    case Obj.MaterialData.Effect of
      effNone           : Material.Texture.MappingMode := tmmUser;
      effReflect        : Material.Texture.MappingMode := tmmCubeMapReflection;
      effReflect2       : Material.Texture.MappingMode := tmmCubeMapLight0;
{      effAlphaChanelPNG : SetMaterialPngTexture(Obj.MaterialData.Texture.FileName,Material)}
    end;

    if Obj.MaterialData.Effect <> effNone then
       TextureScale.AsVector := Obj.MaterialData.Texture.Scale;

    Material.FrontProperties.Diffuse := Obj.MaterialData.Color;
    Material.Texture.TextureMode     := tmReplace;

    if Obj.MaterialData.Color.Alpha < 1 then
    begin
      Obj.MoveLast;
      Material.BlendingMode := bmTransparency;
    end;
  end;
end;

procedure SetMaterialTexture(fName: AnsiString; mat : TGlMaterial);
begin
  with mat,mat.FrontProperties do
  begin
    Texture.Disabled    := false;
    BlendingMode        := bmOpaque;
    Texture.TextureMode := tmReplace;
    Texture.Image.LoadFromFile(fName);

    Ambient .SetColor(1,1,1,1);
    Diffuse .SetColor(1,1,1,1);
    Emission.SetColor(1,1,1,1);
    Specular.SetColor(1,1,1,1);
  end;
end;

end. 
