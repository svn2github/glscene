unit uDDStex;

interface

uses
    Classes, SysUtils,

    GLCompositeImage, GLFileDDS, GLFileJPEG, GLFileTGA, GLFilePNG,
    GLMaterial, GLTexture;

function libmat( AMatLib:TGLMaterialLibrary; AMatName:string): TGLLibMaterial;
function DDStex( AMatLib:TGLMaterialLibrary; ATexName,AFileName:string;
  ASecondTexName:string=''; ADDSLevel:integer = 0): TGLLibMaterial;


implementation


function libmat( AMatLib:TGLMaterialLibrary; AMatName:string): TGLLibMaterial;
begin

  if AMatLib = nil then exit;

  result := AMatLib.LibMaterialByName( AMatName );
  if result = nil then begin
    result := AMatLib.Materials.Add;
    result.Name := AMatName;
  end;

end;


function DDStex( AMatLib:TGLMaterialLibrary; ATexName,AFileName:string;
  ASecondTexName:string=''; ADDSLevel:integer = 0): TGLLibMaterial;
var
    d: TGLDDSDetailLevels;
begin

  if not fileexists(AFileName) then exit;

  d := vDDSDetailLevel;
  case ADDSLevel of
    1: vDDSDetailLevel := ddsMediumDet;
    2: vDDSDetailLevel := ddsLowDet;
    else vDDSDetailLevel := ddsHighDet;
  end;

  result := libmat( AMatLib, ATexName );
  result.Texture2Name := ASecondTexName;
  with result.Material.Texture do begin
    ImageClassName := 'TGLCompositeImage';
    Image.LoadFromFile( AFileName );
    disabled := false;
  end;

  vDDSDetailLevel := d;

end;

end.
