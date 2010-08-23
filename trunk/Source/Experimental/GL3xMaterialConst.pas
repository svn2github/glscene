//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GL3xMaterialConst<p>

   <b>History : </b><font size=-1><ul>
    <li>23/08/10 - Yar - Creation
 </ul></font>
}

unit GL3xMaterialConst;

interface

type

  TMatSysVertexCategory = record
    Name: AnsiString;
    GetVertex: AnsiString;
    TransformVertex_O2W: AnsiString;
    TransformVertex_W2S: AnsiString;
    GetLight: AnsiString;
    GetCamera: AnsiString;
    TransformLighting_W2T: AnsiString;
    PassTexCoord_V2F: AnsiString;
    PassTexCoord_V2G: AnsiString;
    PassVertex_V2F: AnsiString;
    PassVertex_V2G: AnsiString;
  end;

  TMatSysFragmentCategory = record
    Name: AnsiString;
    PassFragmentColor: AnsiString;
    AlphaTest: AnsiString;
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
    Coordinates_TexCoord0: AnsiString;
    Coordinates_TexCoord1: AnsiString;
    Coordinates_TexCoord2: AnsiString;
    Coordinates_TexCoord3: AnsiString;
    Coordinates_TexCoord4: AnsiString;
    Coordinates_TexCoord5: AnsiString;
    Coordinates_TexCoord6: AnsiString;
    Coordinates_TexCoord7: AnsiString;
    Coordinates_Panner: AnsiString;
    Coordinates_Rotator: AnsiString;
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
  end;

  TMatSysTextureCategory = record
    Name: AnsiString;
    Texture_Sampler0: AnsiString;
    Texture_Sampler1: AnsiString;
    Texture_Sampler2: AnsiString;
    Texture_Sampler3: AnsiString;
    Texture_Sampler4: AnsiString;
    Texture_Sampler5: AnsiString;
    Texture_Sampler6: AnsiString;
    Texture_Sampler7: AnsiString;
  end;

  TMatSysUtilityCategory = record
    Name: AnsiString;
    Utility_Timer: AnsiString;
    Utility_Clamp: AnsiString;
    Utility_Min: AnsiString;
    Utility_Max: AnsiString;
    Utility_ComponentMask: AnsiString;
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
    PassTexCoord_V2F: 'PassTexCoord_V2F';
    PassTexCoord_V2G: 'PassTexCoord_V2G';
    PassVertex_V2F: 'PassVertex_V2F';
    PassVertex_V2G: 'PassVertex_V2G';
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
    PassFragmentColor: 'PassFragmentColor';
    AlphaTest: 'AlphaTest';
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
    Coordinates_TexCoord0: 'Coordinates_TexCoord0';
    Coordinates_TexCoord1: 'Coordinates_TexCoord1';
    Coordinates_TexCoord2: 'Coordinates_TexCoord2';
    Coordinates_TexCoord3: 'Coordinates_TexCoord3';
    Coordinates_TexCoord4: 'Coordinates_TexCoord4';
    Coordinates_TexCoord5: 'Coordinates_TexCoord5';
    Coordinates_TexCoord6: 'Coordinates_TexCoord6';
    Coordinates_TexCoord7: 'Coordinates_TexCoord7';
    Coordinates_Panner: 'Coordinates_Panner';
    Coordinates_Rotator: 'Coordinates_Rotator';
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
    );

    Texture: (
    Name: 'Texture';
    Texture_Sampler0: 'Texture_Sampler0';
    Texture_Sampler1: 'Texture_Sampler1';
    Texture_Sampler2: 'Texture_Sampler2';
    Texture_Sampler3: 'Texture_Sampler3';
    Texture_Sampler4: 'Texture_Sampler4';
    Texture_Sampler5: 'Texture_Sampler5';
    Texture_Sampler6: 'Texture_Sampler6';
    Texture_Sampler7: 'Texture_Sampler7';
    );

    Utility: (
    Name: 'Utility';
    Utility_Timer: 'Utility_Timer';
    Utility_Clamp: 'Utility_Clamp';
    Utility_Min: 'Utility_Min';
    Utility_Max: 'Utility_Max';
    Utility_ComponentMask: 'Utility_ComponentMask';
    );

    Vectors: (
    Name: 'Vectors';
    Vectors_WorldNormal: 'Vectors_WorldNormal';
    Vectors_LightVector: 'Vectors_LightVector';
    Vectors_CameraVector: 'Vectors_CameraVector';
    Vectors_ReflectionVector: 'Vectors_ReflectionVector';
    );
    );

implementation

end.

