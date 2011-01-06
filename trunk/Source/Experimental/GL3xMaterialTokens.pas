//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GL3xMaterialTokens<p>

   <b>History : </b><font size=-1><ul>
    <li>23/08/10 - Yar - Creation
 </ul></font>
}

unit GL3xMaterialTokens;

interface

{$I GLScene.inc}
{$IFDEF FPC}
{$mode objfpc}{$h+}
{$ENDIF}

uses
{$IFDEF FPC}
  LResources,
{$ENDIF}
  BaseClasses, GLState;

type
  // TFaceCulling
  //
  TFaceCulling = (fcBufferDefault, fcCull, fcNoCull);

  // TBlendingMode
  //
  TBlendingMode = (bmOpaque, bmTransparency, bmAdditive, bmMasked, bmModulate, bmCustom);

  // TLightingModel
  //
  TLightingModel = (lmPhong, lmEmissiveOnly, lmCustomLighting);

  // TMaterialVariant
  //
  TMaterialVariant = (
    matvarCommon, // Common shader sample
    matvarAllSurfProperies // Export surface properies (normal, camera, light, reflection, position, color)
    );

  TMaterialVariants = set of TMaterialVariant;


  TTextureSampler = record
    SamplerName: IGLName;
    TextureName: IGLName;
    UseCount: Integer;
{$IFNDEF FPC}
    class operator Equal(const a, b: TTextureSampler): Boolean;
{$ENDIF}
  end;

  TTextureSamplerArray = array of TTextureSampler;

const

  cFaceCulling: array[TFaceCulling] of string = ('BufferDefault', 'Cull', 'NoCull');
  cPolygonMode: array[TPolygonMode] of string = ('Fill', 'Lines', 'Points');
  cBlendingMode: array[TBlendingMode] of string = ('Opaque', 'Transparency', 'Additive', 'Masked', 'Modulate', 'Custom');
  cLightingModel: array[TLightingModel] of string = ('Phong', 'EmissiveOnly', 'CustomLighting');
  cMaterialVariant: array[TMaterialVariant] of string = ('', 'AllSurfProperties');

type

  TMatSysVertexCategory = record
    Name: AnsiString;
    GetVertex: AnsiString;
    TransformVertex_O2W: AnsiString;
    TransformVertex_W2S: AnsiString;
    GetLight: AnsiString;
    GetCamera: AnsiString;
    TransformLighting_W2T: AnsiString;
    PassTexCoord: array[0..7] of AnsiString;
    PassVertex: AnsiString;
  end;

  TMatSysFragmentCategory = record
    Name: AnsiString;
    GetLight: AnsiString;
    GetFragment: AnsiString;
    AlphaTest: AnsiString;
    SetEmissive: AnsiString;
    SetDiffuse: AnsiString;
    SetSpecular: AnsiString;
    SetDiffusePower: AnsiString;
    SetSpecularPower: AnsiString;
    SetNormal: AnsiString;
    SetOpacity: AnsiString;
    SetOpacityMask: AnsiString;
    Illuminate: AnsiString;
    SetCustomLighting: AnsiString;
    PassFragmentColor: AnsiString;
  end;

  TMatSysGeometryCategory = record
    Name: AnsiString;
    GetTriangle: AnsiString;
    TransformLighting_W2T: AnsiString;
    Shrink: AnsiString;
    EmitTriangle: AnsiString;
  end;

  TMatSysConstantsCategory = record
    Name: AnsiString;
    Constants_Scalar: AnsiString;
    Constants_Vector2: AnsiString;
    Constants_Vector3: AnsiString;
    Constants_Vector4: AnsiString;
    Constants_VertexColor: AnsiString;
  end;

  TMatSysCoordinatesCategory = record
    Name: AnsiString;
    Coordinates_TexCoord: array[0..7] of AnsiString;
    Coordinates_Panner: AnsiString;
    Coordinates_Rotator: AnsiString;
    Coordinates_ObjectPosition: AnsiString;
    Coordinates_WorldPosition: AnsiString;
    Coordinates_ScreenPosition: AnsiString;
  end;

  TMatSysMathCategory = record
    Name: AnsiString;
    Math_Add: AnsiString;
    Math_Sub: AnsiString;
    Math_Mul: AnsiString;
    Math_Div: AnsiString;
    Math_Normalize: AnsiString;
    Math_DotProduct: AnsiString;
    Math_Power: AnsiString;
    Math_Sine: AnsiString;
    Math_Cosine: AnsiString;
    Math_Floor: AnsiString;
    Math_Abs: AnsiString;
    Math_Fract: AnsiString;
    Math_Phong: AnsiString;
    Math_OneMinus: AnsiString;
    Math_SquareRoot: AnsiString;
    Math_Sign: AnsiString;
    Math_SmoothStep: AnsiString;
  end;

  TMatSysTextureCategory = record
    Name: AnsiString;
    Texture2D_Sampler: array[0..7] of AnsiString;
    TextureCube_Sampler: array[0..7] of AnsiString;
    SNorm: AnsiString;
    SNormDerive: AnsiString;
    YCoCg: AnsiString;
  end;

  TMatSysUtilityCategory = record
    Name: AnsiString;
    Utility_Timer: AnsiString;
    Utility_Clamp: AnsiString;
    Utility_Min: AnsiString;
    Utility_Max: AnsiString;
    Utility_ComponentMask: AnsiString;
    Utility_AppendVector: AnsiString;
  end;

  TMatSysVectorsCategory = record
    Name: AnsiString;
    Vectors_WorldNormal: AnsiString;
    Vectors_LightVector: AnsiString;
    Vectors_CameraVector: AnsiString;
    Vectors_ReflectionVector: AnsiString;
  end;

  TMatSysStrings = record
    Vertex: TMatSysVertexCategory;
    Geometry: TMatSysGeometryCategory;
    Fragment: TMatSysFragmentCategory;
    Constants: TMatSysConstantsCategory;
    Coordinates: TMatSysCoordinatesCategory;
    Math: TMatSysMathCategory;
    Texture: TMatSysTextureCategory;
    Utility: TMatSysUtilityCategory;
    Vectors: TMatSysVectorsCategory;
  end;

const
  MaterialSystem: TMatSysStrings = (

    Vertex: (
    Name: 'Vertex';
    GetVertex: 'GetVertex';
    TransformVertex_O2W: 'TransformVertex_O2W';
    TransformVertex_W2S: 'TransformVertex_W2S';
    GetLight: 'GetLight';
    GetCamera: 'GetCamera';
    TransformLighting_W2T: 'TransformLighting_W2T';
    PassTexCoord: (
     'PassTexCoord0',
     'PassTexCoord1',
     'PassTexCoord2',
     'PassTexCoord3',
     'PassTexCoord4',
     'PassTexCoord5',
     'PassTexCoord6',
     'PassTexCoord7');
    PassVertex: 'PassVertex';
    );

    Geometry: (
    Name: 'Geometry';
    GetTriangle: 'GetTriangle';
    TransformLighting_W2T: 'TransformLighting_W2T';
    Shrink: 'Shrink';
    EmitTriangle: 'EmitTriangle';
    );

    Fragment: (
    Name: 'Fragment';
    GetLight: 'GetLight';
    GetFragment: 'GetFragment';
    AlphaTest: 'AlphaTest';
    SetEmissive: 'SetEmissive';
    SetDiffuse: 'SetDiffuse';
    SetSpecular: 'SetSpecular';
    SetDiffusePower: 'SetDiffusePower';
    SetSpecularPower: 'SetSpecularPower';
    SetNormal: 'SetNormal';
    SetOpacity: 'SetOpacity';
    SetOpacityMask: 'SetOpacityMask';
    Illuminate: 'Illuminate';
    SetCustomLighting: 'SecCustomLighting';
    PassFragmentColor: 'PassFragmentColor';
    );

    Constants: (
    Name: 'Constants';
    Constants_Scalar: 'Constants_Scalar';
    Constants_Vector2: 'Constants_Vector2';
    Constants_Vector3: 'Constants_Vector3';
    Constants_Vector4: 'Constants_Vector4';
    Constants_VertexColor: 'Constants_VertexColor';
    );

    Coordinates: (
    Name: 'Coordinates';
    Coordinates_TexCoord: (
     'Coordinates_TexCoord0',
     'Coordinates_TexCoord1',
     'Coordinates_TexCoord2',
     'Coordinates_TexCoord3',
     'Coordinates_TexCoord4',
     'Coordinates_TexCoord5',
     'Coordinates_TexCoord6',
     'Coordinates_TexCoord7');
    Coordinates_Panner: 'Coordinates_Panner';
    Coordinates_Rotator: 'Coordinates_Rotator';
    Coordinates_ObjectPosition: 'Coordinates_ObjectPosition';
    Coordinates_WorldPosition: 'Coordinates_WorldPosition';
    Coordinates_ScreenPosition: 'Coordinates_ScreenPosition';
    );

    Math: (
    Name: 'Math';
    Math_Add: 'Math_Add';
    Math_Sub: 'Math_Sub';
    Math_Mul: 'Math_Mul';
    Math_Div: 'Math_Div';
    Math_Normalize: 'Math_Normalize';
    Math_DotProduct: 'Math_DotProduct';
    Math_Power: 'Math_Power';
    Math_Sine: 'Math_Sine';
    Math_Cosine: 'Math_Cosine';
    Math_Floor: 'Math_Floor';
    Math_Abs: 'Math_Abs';
    Math_Fract: 'Math_Fract';
    Math_Phong: 'Math_Phong';
    Math_OneMinus: 'Math_OneMinus';
    Math_SquareRoot: 'Math_SquareRoot';
    Math_Sign: 'Math_Sign';
    Math_SmoothStep: 'Math_SmoothStep';
    );

    Texture: (
    Name: 'Texture';
    Texture2D_Sampler: (
      'Texture2D_Sampler0',
      'Texture2D_Sampler1',
      'Texture2D_Sampler2',
      'Texture2D_Sampler3',
      'Texture2D_Sampler4',
      'Texture2D_Sampler5',
      'Texture2D_Sampler6',
      'Texture2D_Sampler7');
    TextureCube_Sampler: (
      'TextureCube_Sampler0',
      'TextureCube_Sampler1',
      'TextureCube_Sampler2',
      'TextureCube_Sampler3',
      'TextureCube_Sampler4',
      'TextureCube_Sampler5',
      'TextureCube_Sampler6',
      'TextureCube_Sampler7');
    SNorm: 'SNorm';
    SNormDerive: 'SNormDerive';
    YCoCg: 'YCoCg';
    );

    Utility: (
    Name: 'Utility';
    Utility_Timer: 'Utility_Timer';
    Utility_Clamp: 'Utility_Clamp';
    Utility_Min: 'Utility_Min';
    Utility_Max: 'Utility_Max';
    Utility_ComponentMask: 'Utility_ComponentMask';
    Utility_AppendVector: 'Utility_AppendVector';
    );

    Vectors: (
    Name: 'Vectors';
    Vectors_WorldNormal: 'Vectors_WorldNormal';
    Vectors_LightVector: 'Vectors_LightVector';
    Vectors_CameraVector: 'Vectors_CameraVector';
    Vectors_ReflectionVector: 'Vectors_ReflectionVector';
    );
    );

  {$IFDEF FPC}
  operator =(const a, b: TTextureSampler): Boolean; inline;
  {$ENDIF}

implementation

{$IFNDEF FPC}
class operator TTextureSampler.Equal(const a, b: TTextureSampler): Boolean;
{$ELSE}
operator =(const a, b: TTextureSampler): Boolean;
{$ENDIF}
begin
  Result := (Pointer(a.TextureName) = Pointer(b.TextureName)) and (Pointer(a.SamplerName) = Pointer(b.SamplerName));
end;

{$IFDEF GLS_EXPERIMENTAL}
{$IFDEF FPC}
initialization
{$I ..\Experimental\GLSceneMaterialSysLCL.lrs}
{$ENDIF}
{$ENDIF GLS_EXPERIMENTAL}

end.

