//
// This unit is part of the GLScene Project, http://glscene.org
//
{ : GLSDrawTechnique<p>

  <b>History : </b><font size=-1><ul>
  <li>17/06/11 - Yar - Added DrawDynamicBatch
  <li>07/06/11 - Yar - Added Bindless Graphic feature
  <li>29/05/11 - Yar - Added picking: name based for OGL1, color based for other techniques
  <li>25/05/11 - Yar - Added instancing
  <li>18/05/11 - Yar - Added axes drawing
  <li>17/04/11 - Yar - Creation
  </ul></font>
}

unit GLScene.DrawTechnique;

interface

{$I GLScene.inc}
{ .$DEFINE GLS_OPENGL_DEBUG }

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes,
  SysUtils,
  GLScene.Base.Classes,
  GLScene.Platform,
  GLScene.Base.OpenGL.Tokens,
  GLScene.Base.Context.Info,
  GLScene.Base.Transformation,
  GLScene.Base.Context,
  GLScene.Base.GLStateMachine,
  GLScene.Mesh,
  GLScene.Material;

const
  VBO_STATIC_POOL_SIZE: Cardinal = 16 * 1024 * 1024;

type

  TPickCallback = procedure of object;
  TOnCustomDraw = procedure(var ARci: TRenderContextInfo) of object;

  PDrawBatch = ^TDrawBatch;

  TDrawBatch = record
    Mesh: TMeshAtom;
    InstancesChain: TInstancesChain;
    Material: TGLAbstractLibMaterial;
    PickingMaterial: TGLAbstractLibMaterial;
    Transformation: PTransformationRec;
    ShowAxes: Boolean;
    ShowAABB: Boolean;
    Changed: Boolean;
    PickFlag: Boolean;
    PickCallback: TPickCallback;
    CustomDraw: TOnCustomDraw;
    ListIndex: Integer;
  end;

  TDrawBatchArray = array of TDrawBatch;

  PPoolSector = ^TPoolSector;

  TPoolSector = record
    Mesh: TMeshAtom;
    Offset: PtrUInt;
    Size: Cardinal;
  end;

  TPoolMapType = (pmtArray, pmtElement);

  TPoolMap = class(TList)
  private
    FType: TPoolMapType;
    function GetSector(Index: Integer): PPoolSector;
    procedure PutSector(Index: Integer; Item: PPoolSector);
  public
    constructor Create(AType: TPoolMapType);
    function AddSector(const AItem: TPoolSector): Integer;
    procedure InsertSector(Index: Integer; const AItem: TPoolSector);
    procedure DeleteSector(Index: Integer);
    procedure Clear; override;
    property Sectors[Index: Integer]: PPoolSector read GetSector
      write PutSector;
  end;

  // TGLAbstractDrawTechnique
  //
  TGLAbstractDrawTechnique = class(TObject)
  protected
    { Protected Declarations }
    function GetAABBMaterial: TGLAbstractLibMaterial;
    procedure DoBeforeAABBDrawing(var ARci: TRenderContextInfo);
      virtual; abstract;
    procedure DoAfterAABBDrawing(var ARci: TRenderContextInfo);
      virtual; abstract;
    procedure ApplyInstance(var ARci: TRenderContextInfo;
      const AInstance: TInstancesChain; const AID: Integer); virtual; abstract;
    procedure DoBeforePicking(AnObjectCountGuess: Integer); virtual; abstract;
    procedure DoAfterPicking(const AList: TList); virtual; abstract;
  public
    { Public Declarations }
    // Draw batch which geometry stored in display list or array buffer
    procedure DrawBatch(var ARci: TRenderContextInfo; const ABatch: TDrawBatch);
      virtual; abstract;
    // Draw batch directly from operative memory
    procedure DrawDynamicBatch(var ARci: TRenderContextInfo;
      const ABatch: TDrawBatch); virtual; abstract;
    // Draw AABB of batch's mesh
    procedure DrawAABB(var ARci: TRenderContextInfo; const ABatch: TDrawBatch);
      virtual; abstract;
  end;

  TGLAbstractDrawTechniqueClass = class of TGLAbstractDrawTechnique;

  // TGLDrawTechniqueOGL1
  //
  { : Fixed function pipeline draw technique. }
  TGLDrawTechniqueOGL1 = class(TGLAbstractDrawTechnique)
  private
    FBuffer: array of TGLuint;
  protected
    { Protected Declarations }
    procedure DoBeforeAABBDrawing(var ARci: TRenderContextInfo); override;
    procedure DoAfterAABBDrawing(var ARci: TRenderContextInfo); override;
    procedure ApplyInstance(var ARci: TRenderContextInfo;
      const AInstance: TInstancesChain; const AID: Integer); override;
    procedure DoBeforePicking(AnObjectCountGuess: Integer); override;
    procedure DoAfterPicking(const AList: TList); override;
  public
    { Public Declarations }
    procedure DrawBatch(var ARci: TRenderContextInfo;
      const ABatch: TDrawBatch); override;
    procedure DrawDynamicBatch(var ARci: TRenderContextInfo;
      const ABatch: TDrawBatch); override;
    procedure DrawAABB(var ARci: TRenderContextInfo;
      const ABatch: TDrawBatch); override;
  end;

  // TGLDrawTechniqueOGL2
  //
  { : Programable pipeline draw technique. }
  TGLDrawTechniqueOGL2 = class(TGLAbstractDrawTechnique)
  protected
    { Protected Declarations }
    FDrawAsElement: Boolean;
    FIndexType: TGLEnum;
    FIndexSize: Byte;

    FArrayHandle: TGLVBOArrayBufferHandle;
    FElementHandle: TGLVBOElementArrayHandle;
    FArrayBufferMap: TPoolMap;
    FElementBufferMap: TPoolMap;
    FArrayBufferAddress: TGLuint64;
    FElementBufferAddress: TGLuint64;

    procedure AllocateBuffers;
    procedure PlacedInBuffer(AMesh: TMeshAtom);
    function BindStateHandle(var ARci: TRenderContextInfo;
      const AMesh: TMeshAtom): Boolean;

    procedure DoBeforeAABBDrawing(var ARci: TRenderContextInfo); override;
    procedure DoAfterAABBDrawing(var ARci: TRenderContextInfo); override;
    procedure ApplyInstance(var ARci: TRenderContextInfo;
      const AInstance: TInstancesChain; const AID: Integer); override;
    procedure DoBeforePicking(AnObjectCountGuess: Integer); override;
    procedure DoAfterPicking(const AList: TList); override;
    procedure DumpPool(APool: TPoolMap);
  public
    { Public Declarations }
    constructor Create; virtual;
    destructor Destroy; override;

    procedure DrawBatch(var ARci: TRenderContextInfo;
      const ABatch: TDrawBatch); override;
    procedure DrawDynamicBatch(var ARci: TRenderContextInfo;
      const ABatch: TDrawBatch); override;
    procedure DrawAABB(var ARci: TRenderContextInfo;
      const ABatch: TDrawBatch); override;
  end;

  TGLDrawTechniqueOGL3 = class(TGLDrawTechniqueOGL2)
  protected
    { Protected Declarations }
    FCommonVAO: TGLVertexArrayHandle;
    procedure DoBeforeAABBDrawing(var ARci: TRenderContextInfo); override;
    procedure DoAfterAABBDrawing(var ARci: TRenderContextInfo); override;
    // procedure ApplyInstance(var ARci: TRenderContextInfo;
    // const AInstance: TInstancesChain; const AID: Integer); override;
  public
    { Public Declarations }
    constructor Create; override;
    destructor Destroy; override;

    procedure DrawBatch(var ARci: TRenderContextInfo;
      const ABatch: TDrawBatch); override;
    procedure DrawDynamicBatch(var ARci: TRenderContextInfo;
      const ABatch: TDrawBatch); override;
  end;

  TGLDrawTechniqueOGL4 = class(TGLDrawTechniqueOGL3)
  public
    { Public Declarations }
    procedure DrawBatch(var ARci: TRenderContextInfo;
      const ABatch: TDrawBatch); override;
    procedure DrawDynamicBatch(var ARci: TRenderContextInfo;
      const ABatch: TDrawBatch); override;
  end;

  // TGLRenderManager
  //
  TGLRenderManager = class(TGLUpdateAbleObject)
  protected
    FDrawList: TList;
    FAxesBatch: TDrawBatch;
    function GetDrawTechnique: TGLAbstractDrawTechnique; virtual;
  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure DrawAll(var ARci: TRenderContextInfo);

    property DrawList: TList read FDrawList;
    property DrawTechnique: TGLAbstractDrawTechnique read GetDrawTechnique;
  end;

var
  vBindlessGraphicsEnabled: Boolean = False;

function GetOrCreateDummyCubeMaterial: TGLAbstractLibMaterial;
function GetOrCreatePickingMaterial: TGLAbstractLibMaterial;
procedure AxesBuildMesh(AMesh: TMeshAtom; AnAxisLen: Single);

implementation

uses
  GLScene.MaterialEx,
  GLScene.Base.Color,
  GLScene.Base.Strings,
  GLScene.Base.Vector.Geometry,
  GLScene.Base.Vector.Types,
  GLScene.Base.Vector.Lists,
  GLScene.Shader.Parameter,
  GLScene.Base.Log;

const
  cPrimitiveType: array [mpTRIANGLES .. mpPATCHES] of GLenum = (GL_TRIANGLES,
    GL_TRIANGLE_STRIP, GL_TRIANGLE_FAN, GL_POINTS, GL_LINES, GL_LINE_LOOP,
    GL_LINE_STRIP, GL_LINES_ADJACENCY, GL_LINE_STRIP_ADJACENCY,
    GL_TRIANGLES_ADJACENCY, GL_TRIANGLE_STRIP_ADJACENCY, GL_PATCHES);

const
  cAdjacencyPrimitives = [mpTRIANGLES_ADJACENCY, mpTRIANGLE_STRIP_ADJACENCY];

const
  cAABBIndices: array [0 .. 23] of TGLUShort = (0, 1, 1, 2, 2, 3, 3, 0, 4, 5, 5,
    6, 6, 7, 7, 4, 0, 4, 1, 5, 2, 6, 3, 7);

type
  TFriendlyMesh = class(TMeshAtom);
  TFriendlyInstancesChain = class(TInstancesChain);

var
  vDrawTechniques: array [0 .. 3] of TGLAbstractDrawTechnique;

function GetOrCreateDummyCubeMaterial: TGLAbstractLibMaterial;
const
  cDummyCubeMaterialName = 'GLScene_DummyCube_Material';
begin
  Result := GetInternalMaterialLibrary.Materials.GetLibMaterialByName
    (cDummyCubeMaterialName);
  if Result = nil then
  begin
    Result := GetInternalMaterialLibrary.Materials.Add;
    with TGLLibMaterialEx(Result) do
    begin
      Name := cDummyCubeMaterialName;
      FixedFunction.BlendingMode := bmTransparency;
      FixedFunction.MaterialOptions := [moNoLighting];
      FixedFunction.LineProperties.Enabled := True;
      FixedFunction.LineProperties.StippleFactor := 1;
      FixedFunction.LineProperties.Smooth := True;
    end;
  end;
end;

var
  vDefaultPickingMaterial: TGLAbstractLibMaterial;

function GetOrCreatePickingMaterial: TGLAbstractLibMaterial;
const
  cPickingMaterialName = 'GLScene_Picking_Material';
begin
  Result := GetInternalMaterialLibrary.Materials.GetLibMaterialByName
    (cPickingMaterialName);
  if Result = nil then
  begin
    Result := GetInternalMaterialLibrary.Materials.Add;
    with TGLLibMaterialEx(Result) do
    begin
      Name := cPickingMaterialName;
      FixedFunction.MaterialOptions := [moNoLighting];
      FixedFunction.LineProperties.Enabled := True;
      FixedFunction.LineProperties.Width := 3;
    end;
    vDefaultPickingMaterial := Result;
  end;
end;

procedure AxesBuildMesh(AMesh: TMeshAtom; AnAxisLen: Single);
begin
  with AMesh do
  begin
    Lock;
    try
      Clear;
      DeclareAttribute(attrPosition, GLSLType3f);
      DeclareAttribute(attrColor, GLSLType3f);

      BeginAssembly(mpLINES);

      Attribute3f(attrColor, 0.5, 0.0, 0.0);
      Attribute3f(attrPosition, NullVector);
      EmitVertex;
      Attribute3f(attrPosition, -AnAxisLen, 0, 0);
      EmitVertex;

      Attribute3f(attrColor, 1.0, 0.0, 0.0);
      Attribute3f(attrPosition, NullVector);
      EmitVertex;
      Attribute3f(attrPosition, AnAxisLen, 0, 0);
      EmitVertex;

      Attribute3f(attrColor, 0.0, 0.5, 0.0);
      Attribute3f(attrPosition, NullVector);
      EmitVertex;
      Attribute3f(attrPosition, 0, -AnAxisLen, 0);
      EmitVertex;

      Attribute3f(attrColor, 0.0, 1.0, 0.0);
      Attribute3f(attrPosition, NullVector);
      EmitVertex;
      Attribute3f(attrPosition, 0, AnAxisLen, 0);
      EmitVertex;

      Attribute3f(attrColor, 0.0, 0.0, 0.5);
      Attribute3f(attrPosition, NullVector);
      EmitVertex;
      Attribute3f(attrPosition, 0, 0, -AnAxisLen);
      EmitVertex;

      Attribute3f(attrColor, 0.0, 0.0, 1.0);
      Attribute3f(attrPosition, NullVector);
      EmitVertex;
      Attribute3f(attrPosition, 0, 0, AnAxisLen);
      EmitVertex;
      EndAssembly;
    finally
      UnLock;
    end;
  end;
end;

procedure ReleaseDrawTechniques;
var
  I: Integer;
begin
  for I := 0 to High(vDrawTechniques) do
    FreeAndNil(vDrawTechniques[I]);
end;

procedure RoundTo(var Value: Cardinal; Step: Cardinal);
{$IFDEF GLS_INLINE} inline;
{$ENDIF}
var
  L: Cardinal;
begin
  L := Value mod Step;
  if L > 0 then
    Inc(Value, Step - L);
end;

{$IFDEF GLS_REGION}{$REGION 'TGLAbstractDrawTechnique'}{$ENDIF}

function TGLAbstractDrawTechnique.GetAABBMaterial: TGLAbstractLibMaterial;
const
  cAABBMaterialName = 'GLScene_AABB_Material';
  cAABBVertexShader120 = '#version 120'#10#13 + 'attribute vec3 Position;'#10#13
    + 'uniform mat4 ModelViewProjectionMatrix;'#10#13 +
    'void main() { gl_Position = ModelViewProjectionMatrix * vec4(Position,1.0); }';
  cAABBFragmentShader120 = '#version 120'#10#13 +
    'void main() { gl_FragColor = vec4(1.0, 0.0, 0.0, 1.0); }'#10#13;
  cAABBVertexShader330 = '#version 330'#10#13 + 'in vec3 Position;'#10#13 +
    'uniform mat4 ModelViewProjectionMatrix;'#10#13 +
    'void main() { gl_Position = ModelViewProjectionMatrix * vec4(Position,1.0); }';
  cAABBFragmentShader330 = '#version 330'#10#13 + 'out vec4 FragColor;'#10#13 +
    'void main() { FragColor = vec4(1.0, 0.0, 0.0, 1.0); }'#10#13;
var
  LShader: TGLShaderEx;
begin
  Result := GetInternalMaterialLibrary.Materials.GetLibMaterialByName
    (cAABBMaterialName);
  if Result = nil then
  begin
    Result := GetInternalMaterialLibrary.Materials.Add;
    with TGLLibMaterialEx(Result) do
    begin
      Name := cAABBMaterialName;
      // FFP
      FixedFunction.MaterialOptions := [moNoLighting];
      FixedFunction.FrontProperties.Diffuse.DirectColor := clrRed;
      FixedFunction.LineProperties.Enabled := True;
      // GLSL 120
      LShader := GetInternalMaterialLibrary.AddShader(cInternalShader);
      LShader.ShaderType := shtVertex;
      LShader.Source.Add(cAABBVertexShader120);
      ShaderModel3.LibVertexShaderName := LShader.Name;
      LShader := GetInternalMaterialLibrary.AddShader(cInternalShader);
      LShader.ShaderType := shtFragment;
      LShader.Source.Add(cAABBFragmentShader120);
      ShaderModel3.LibFragmentShaderName := LShader.Name;
      ShaderModel3.Enabled := True;
      ShaderModel3.DoOnPrepare(CurrentGLContext);
      if ShaderModel3.IsValid then
        ShaderModel3.Uniforms['ModelViewProjectionMatrix'].AutoSetMethod :=
          cafWorldViewProjectionMatrix;
      // GLSL 330
      LShader := GetInternalMaterialLibrary.AddShader(cInternalShader);
      LShader.ShaderType := shtVertex;
      LShader.Source.Add(cAABBVertexShader330);
      ShaderModel4.LibVertexShaderName := LShader.Name;
      LShader := GetInternalMaterialLibrary.AddShader(cInternalShader);
      LShader.ShaderType := shtFragment;
      LShader.Source.Add(cAABBFragmentShader330);
      ShaderModel4.LibFragmentShaderName := LShader.Name;
      ShaderModel4.Enabled := True;
      ShaderModel4.DoOnPrepare(CurrentGLContext);
      if ShaderModel4.IsValid then
        ShaderModel4.Uniforms['ModelViewProjectionMatrix'].AutoSetMethod :=
          cafWorldViewProjectionMatrix;
    end;
  end;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}
{$IFDEF GLS_REGION}{$REGION 'TGLDrawTechniqueOGL1'}{$ENDIF}
// ------------------
// ------------------ TGLDrawTechniqueOGL1 ------------------
// ------------------

procedure TGLDrawTechniqueOGL1.ApplyInstance(var ARci: TRenderContextInfo;
  const AInstance: TInstancesChain; const AID: Integer);
var
  A: TAttribLocation;
  T: TGLEnum;
  LLink: TFriendlyInstancesChain;
  I: Integer;
begin
  LLink := TFriendlyInstancesChain(AInstance);

  with GL do
  begin
    // Texture coordinates
    if ARB_multisample then
    begin
      T := 7;
      for A := attrTexCoord7 downto attrTexCoord0 do
      begin
        if LLink.FAttributes[A] then
        begin
          ClientActiveTexture(GL_TEXTURE0 + T);
          DisableClientState(GL_TEXTURE_COORD_ARRAY);
          I := AID;
          if LLink.FAttributeDivisor[A] > 1 then
            I := I div LLink.FAttributeDivisor[A];
          case LLink.FType[A] of
            GLSLType1F:
              MultiTexCoord1f(GL_TEXTURE0 + T,
                PSingle(@LLink.FAttributeArrays[A].List[I])^);
            GLSLType2F:
              MultiTexCoord2fv(GL_TEXTURE0 + T,
                @LLink.FAttributeArrays[A].List[2 * I]);
            GLSLType3f:
              MultiTexCoord3fv(GL_TEXTURE0 + T,
                @LLink.FAttributeArrays[A].List[3 * I]);
            GLSLType4F:
              MultiTexCoord4fv(GL_TEXTURE0 + T,
                @LLink.FAttributeArrays[A].List[4 * I]);
            GLSLType1I:
              MultiTexCoord1i(GL_TEXTURE0 + T,
                PInteger(@LLink.FAttributeArrays[A].List[I])^);
            GLSLType2I:
              MultiTexCoord2iv(GL_TEXTURE0 + T,
                @LLink.FAttributeArrays[A].List[2 * I]);
            GLSLType3I:
              MultiTexCoord3iv(GL_TEXTURE0 + T,
                @LLink.FAttributeArrays[A].List[3 * I]);
            GLSLType4I:
              MultiTexCoord4iv(GL_TEXTURE0 + T,
                @LLink.FAttributeArrays[A].List[4 * I]);
          end;
        end;
        Dec(T);
      end;
    end
    else
    begin
      // Only one texture unit avaible
      if LLink.FAttributes[attrTexCoord0] then
      begin
        DisableClientState(GL_TEXTURE_COORD_ARRAY);
        I := AID;
        if LLink.FAttributeDivisor[attrTexCoord0] > 1 then
          I := I div LLink.FAttributeDivisor[attrTexCoord0];
        case LLink.FType[attrTexCoord0] of
          GLSLType1F:
            TexCoord1f(PSingle(@LLink.FAttributeArrays[attrTexCoord0]
              .List[I])^);
          GLSLType2F:
            TexCoord2fv(@LLink.FAttributeArrays[attrTexCoord0].List[2 * I]);
          GLSLType3f:
            TexCoord3fv(@LLink.FAttributeArrays[attrTexCoord0].List[3 * I]);
          GLSLType4F:
            TexCoord4fv(@LLink.FAttributeArrays[attrTexCoord0].List[4 * I]);
          GLSLType1I:
            TexCoord1i(PInteger(@LLink.FAttributeArrays[attrTexCoord0]
              .List[I])^);
          GLSLType2I:
            TexCoord2iv(@LLink.FAttributeArrays[attrTexCoord0].List[2 * I]);
          GLSLType3I:
            TexCoord3iv(@LLink.FAttributeArrays[attrTexCoord0].List[3 * I]);
          GLSLType4I:
            TexCoord4iv(@LLink.FAttributeArrays[attrTexCoord0].List[4 * I]);
        end;
      end;
    end;
    // Colors
    if LLink.FAttributes[attrColor] and (ARci.drawState <> dsPicking) then
    begin
      DisableClientState(GL_COLOR_ARRAY);
      I := AID;
      if LLink.FAttributeDivisor[attrColor] > 1 then
        I := I div LLink.FAttributeDivisor[attrColor];
      case LLink.FType[attrColor] of
        GLSLType3f:
          Color3fv(@LLink.FAttributeArrays[attrColor].List[3 * I]);
        GLSLType4F:
          Color4fv(@LLink.FAttributeArrays[attrColor].List[4 * I]);
        GLSLType3I:
          Color3iv(@LLink.FAttributeArrays[attrColor].List[3 * I]);
        GLSLType4I:
          Color4iv(@LLink.FAttributeArrays[attrColor].List[4 * I]);
      end;
    end;
    // Normals
    if LLink.FAttributes[attrNormal] then
    begin
      DisableClientState(GL_NORMAL_ARRAY);
      I := AID;
      if LLink.FAttributeDivisor[attrNormal] > 1 then
        I := I div LLink.FAttributeDivisor[attrNormal];
      case LLink.FType[attrColor] of
        GLSLType3f:
          Normal3fv(@LLink.FAttributeArrays[attrNormal].List[3 * I]);
        GLSLType3I:
          Normal3iv(@LLink.FAttributeArrays[attrNormal].List[3 * I]);
      end;
    end;
    // Positions
    if LLink.FAttributes[attrPosition] then
    begin
      DisableClientState(GL_VERTEX_ARRAY);
      I := AID;
      if LLink.FAttributeDivisor[attrPosition] > 1 then
        I := I div LLink.FAttributeDivisor[attrPosition];
      case LLink.FType[attrPosition] of
        GLSLType2F:
          Vertex2fv(@LLink.FAttributeArrays[attrPosition].List[2 * I]);
        GLSLType3f:
          Vertex3fv(@LLink.FAttributeArrays[attrPosition].List[3 * I]);
        GLSLType4F:
          Vertex4fv(@LLink.FAttributeArrays[attrPosition].List[4 * I]);
        GLSLType2I:
          Vertex2iv(@LLink.FAttributeArrays[attrPosition].List[2 * I]);
        GLSLType3I:
          Vertex3iv(@LLink.FAttributeArrays[attrPosition].List[3 * I]);
        GLSLType4I:
          Vertex4iv(@LLink.FAttributeArrays[attrPosition].List[4 * I]);
      end;
    end;
  end;

  if LLink.FTransformationEnabled then
    ARci.PipelineTransformation.StackTop :=
      PTransformationRec(LLink.FTransformations[AID])^;
end;

procedure TGLDrawTechniqueOGL1.DoAfterAABBDrawing(var ARci: TRenderContextInfo);
begin
  GetAABBMaterial.UnApply(ARci);
end;

procedure TGLDrawTechniqueOGL1.DoAfterPicking(const AList: TList);
type
  TPickSubObjects = array of TGLuint;
var
  subObj: TPickSubObjects;
  next, current, subObjIndex: Cardinal;
  // szmin, szmax: Single;
  LHits, I: Integer;
  pBatch: PDrawBatch;
begin
  with GL do
  begin
    Flush;
    LHits := RenderMode(GL_RENDER);
  end;
  CurrentGLContext.GLStates.SetGLColorWriting(True);

  if LHits > -1 then
  begin
    for I := 0 to AList.Count - 1 do
    begin
      pBatch := AList[I];
      pBatch^.PickFlag := Assigned(pBatch.PickCallback);
    end;

    next := 0;
    for I := 0 to LHits - 1 do
    begin
      current := next;
      next := current + FBuffer[current] + 3;
      // szmin := (FBuffer[current + 1] shr 1) * (1 / MaxInt);
      // szmax := (FBuffer[current + 2] shr 1) * (1 / MaxInt);
      subObj := nil;
      subObjIndex := current + 4;
      if subObjIndex < next then
      begin
        SetLength(subObj, FBuffer[current] - 1);
        while subObjIndex < next do
        begin
          subObj[subObjIndex - current - 4] := FBuffer[subObjIndex];
          Inc(subObjIndex);
        end;
      end;
      pBatch := AList[FBuffer[current + 3]];
      if pBatch.PickFlag then
      begin
        pBatch.PickCallback();
        pBatch.PickFlag := False;
      end;
    end;
  end;
end;

procedure TGLDrawTechniqueOGL1.DoBeforeAABBDrawing
  (var ARci: TRenderContextInfo);
begin
  GetAABBMaterial.Apply(ARci);
end;

procedure TGLDrawTechniqueOGL1.DoBeforePicking(AnObjectCountGuess: Integer);
begin
  SetLength(FBuffer, AnObjectCountGuess * 4 + 32);
  with GL do
  begin
    SelectBuffer(AnObjectCountGuess * SizeOf(TGLuint), @FBuffer[0]);
    RenderMode(GL_SELECT);
    InitNames;
    PushName(0);
  end;
  CurrentGLContext.GLStates.SetGLColorWriting(False);
end;

procedure TGLDrawTechniqueOGL1.DrawAABB(var ARci: TRenderContextInfo;
  const ABatch: TDrawBatch);
var
  LMesh: TFriendlyMesh;
  LPositions: array [0 .. 7] of TVector3f;
begin
  with GL do
  begin
    LMesh := TFriendlyMesh(ABatch.Mesh);
    with LMesh.AABB do
    begin
      LPositions[2] := max;
      LPositions[4] := min;
    end;
    LPositions[0] := Vector3fMake(LPositions[4][0], LPositions[2][1],
      LPositions[4][2]);
    LPositions[1] := Vector3fMake(LPositions[4][0], LPositions[2][1],
      LPositions[2][2]);
    LPositions[3] := Vector3fMake(LPositions[2][0], LPositions[2][1],
      LPositions[4][2]);
    LPositions[5] := Vector3fMake(LPositions[4][0], LPositions[4][1],
      LPositions[2][2]);
    LPositions[6] := Vector3fMake(LPositions[2][0], LPositions[4][1],
      LPositions[2][2]);
    LPositions[7] := Vector3fMake(LPositions[2][0], LPositions[4][1],
      LPositions[4][2]);

    ARci.PipelineTransformation.Push(ABatch.Transformation);
    try
      EnableClientState(GL_VERTEX_ARRAY);
      VertexPointer(3, GL_FLOAT, 0, @LPositions[0]);
      DrawElements(GL_LINES, 24, GL_UNSIGNED_SHORT, @cAABBIndices);
      DisableClientState(GL_VERTEX_ARRAY);
    finally
      ARci.PipelineTransformation.Pop;
    end;
  end;
end;

procedure TGLDrawTechniqueOGL1.DrawBatch(var ARci: TRenderContextInfo;
  const ABatch: TDrawBatch);
var
  LMesh: TFriendlyMesh;
  LInstanceChain: TInstancesChain;
  LMaterial: TGLAbstractLibMaterial;
  LInstanceID: Integer;
  A: TAttribLocation;
  T: TGLEnum;
  glPrimitive: TGLEnum;
begin
  with GL do
  begin
    LMesh := TFriendlyMesh(ABatch.Mesh);
    LMesh.GetDLO.AllocateHandle;
    if LMesh.FRevisionNum <> LMesh.FBufferRevision then
    begin
      LMesh.GetDLO.NotifyChangesOfData;
      LMesh.FBufferRevision := LMesh.FRevisionNum;
    end;

    if Assigned(ABatch.InstancesChain) and ABatch.InstancesChain.IsValid then
    begin
      LInstanceChain := ABatch.InstancesChain;
      LInstanceID := LInstanceChain.InstanceCount;
    end
    else
    begin
      LInstanceChain := nil;
      LInstanceID := 1;
    end;

    // Upload geometry
    if LMesh.GetDLO.IsDataNeedUpdate then
    begin
      LMesh.GetDLO.NewList(GL_COMPILE);
      // Texture coordinates
      if ARB_multisample then
      begin
        T := 7;
        for A := attrTexCoord7 downto attrTexCoord0 do
        begin
          ClientActiveTexture(GL_TEXTURE0 + T);
          if LMesh.FAttributes[A] then
          begin
            EnableClientState(GL_TEXTURE_COORD_ARRAY);
            TexCoordPointer(GLSLTypeComponentCount(LMesh.FType[A]),
              GLSLTypeEnum(LMesh.FType[A]), 0, LMesh.FAttributeArrays[A].List);
          end
          else if LMesh.FAttributes[attrTexCoord0] then
          begin
            // Share first texture coordinates with others
            EnableClientState(GL_TEXTURE_COORD_ARRAY);
            TexCoordPointer(GLSLTypeComponentCount(LMesh.FType[attrTexCoord0]),
              GLSLTypeEnum(LMesh.FType[attrTexCoord0]), 0,
              LMesh.FAttributeArrays[attrTexCoord0].List);
          end
          else
            DisableClientState(GL_TEXTURE_COORD_ARRAY);
          Dec(T);
        end;
      end
      else
      begin
        // Only one texture unit avaible
        if LMesh.FAttributes[attrTexCoord0] then
        begin
          EnableClientState(GL_TEXTURE_COORD_ARRAY);
          TexCoordPointer(GLSLTypeComponentCount(LMesh.FType[attrTexCoord0]),
            GLSLTypeEnum(LMesh.FType[attrTexCoord0]), 0,
            LMesh.FAttributeArrays[attrTexCoord0].List);
        end
        else
          DisableClientState(GL_TEXTURE_COORD_ARRAY);
      end;
      // Colors
      if LMesh.FAttributes[attrColor] then
      begin
        EnableClientState(GL_COLOR_ARRAY);
        ColorPointer(GLSLTypeComponentCount(LMesh.FType[attrColor]),
          GLSLTypeEnum(LMesh.FType[attrColor]), 0,
          LMesh.FAttributeArrays[attrColor].List);
      end
      else
        DisableClientState(GL_COLOR_ARRAY);
      // Normals
      if LMesh.FAttributes[attrNormal] and
        (GLSLTypeComponentCount(LMesh.FType[attrNormal]) = 3) then
      begin
        EnableClientState(GL_NORMAL_ARRAY);
        NormalPointer(GLSLTypeEnum(LMesh.FType[attrNormal]), 0,
          LMesh.FAttributeArrays[attrNormal].List);
      end
      else
        DisableClientState(GL_NORMAL_ARRAY);
      // Positions
      if LMesh.FAttributes[attrPosition] then
      begin
        EnableClientState(GL_VERTEX_ARRAY);
        VertexPointer(GLSLTypeComponentCount(LMesh.FType[attrPosition]),
          GLSLTypeEnum(LMesh.FType[attrPosition]), 0,
          LMesh.FAttributeArrays[attrPosition].List);
      end
      else
        DisableClientState(GL_VERTEX_ARRAY);
      // Draw
      glPrimitive := cPrimitiveType[LMesh.FPrimitive];
      if LMesh.FHasIndices then
      begin
        DrawElements(glPrimitive, LMesh.FElements.Count, GL_UNSIGNED_INT,
          LMesh.FElements.List);
      end
      else
      begin
        if VERSION_1_4 then
          MultiDrawArrays(glPrimitive, PGLint(LMesh.FRestartVertex.List),
            PGLsizei(LMesh.FStripCounts.List), LMesh.FRestartVertex.Count)
        else
          for T := 0 to LMesh.FRestartVertex.Count - 1 do
            DrawArrays(glPrimitive, LMesh.FRestartVertex.List[T],
              LMesh.FStripCounts.List[T]);
      end;

      LMesh.GetDLO.EndList;
      LMesh.GetDLO.NotifyDataUpdated;
    end;

    if Assigned(ABatch.Transformation) then
      ARci.PipelineTransformation.StackTop := ABatch.Transformation^;

    if ARci.drawState = dsPicking then
    begin
      LoadName(ABatch.ListIndex);
      LMaterial := nil;
    end
    else
      LMaterial := ABatch.Material;

    if Assigned(LMaterial) then
      LMaterial.Apply(ARci);
    ARci.PipelineTransformation.LoadMatrices;

    repeat

      repeat
        Dec(LInstanceID);
        if Assigned(LInstanceChain) then
          ApplyInstance(ARci, LInstanceChain, LInstanceID);

        LMesh.GetDLO.CallList;

      until LInstanceID <= 0;

      if not Assigned(LMaterial) then
        break;
    until not LMaterial.UnApply(ARci);

  end;
end;

procedure TGLDrawTechniqueOGL1.DrawDynamicBatch(var ARci: TRenderContextInfo;
  const ABatch: TDrawBatch);
var
  LMesh: TFriendlyMesh;
  LInstanceChain: TInstancesChain;
  LMaterial: TGLAbstractLibMaterial;
  glPrimitive: TGLEnum;
  glType: TGLEnum;
  A: TAttribLocation;
  LInstanceID, T: Integer;
  LArrayAddress: Pointer;
  storeRci: TRenderContextInfo;
  LCount: Integer;
begin
  if not ABatch.Mesh.IsValid then
    exit;

  LMesh := TFriendlyMesh(ABatch.Mesh);
  if Assigned(ABatch.InstancesChain) and ABatch.InstancesChain.IsValid then
  begin
    LInstanceChain := ABatch.InstancesChain;
    LInstanceID := LInstanceChain.InstanceCount;
  end
  else
  begin
    LInstanceChain := nil;
    LInstanceID := 1;
  end;

  if ARci.drawState = dsPicking then
  begin
    LMaterial := vDefaultPickingMaterial;
  end
  else
    LMaterial := ABatch.Material;

  storeRci := ARci;

  if Assigned(ABatch.Transformation) then
    ARci.PipelineTransformation.StackTop := ABatch.Transformation^;
  with GL do
    try
      if Assigned(LMaterial) then
        LMaterial.Apply(ARci);

      if ARB_multisample then
      begin
        T := 7;
        for A := attrTexCoord7 downto attrTexCoord0 do
        begin
          ClientActiveTexture(GL_TEXTURE0 + T);
          if LMesh.FAttributes[A] then
          begin
            EnableClientState(GL_TEXTURE_COORD_ARRAY);
            TexCoordPointer(GLSLTypeComponentCount(LMesh.FType[A]),
              GLSLTypeEnum(LMesh.FType[A]), 0, LMesh.FAttributeArrays[A].List);
          end
          else if LMesh.FAttributes[attrTexCoord0] then
          begin
            // Share first texture coordinates with others
            EnableClientState(GL_TEXTURE_COORD_ARRAY);
            TexCoordPointer(GLSLTypeComponentCount(LMesh.FType[attrTexCoord0]),
              GLSLTypeEnum(LMesh.FType[attrTexCoord0]), 0,
              LMesh.FAttributeArrays[attrTexCoord0].List);
          end
          else
            DisableClientState(GL_TEXTURE_COORD_ARRAY);
          Dec(T);
        end;
      end
      else
      begin
        // Only one texture unit avaible
        if LMesh.FAttributes[attrTexCoord0] then
        begin
          EnableClientState(GL_TEXTURE_COORD_ARRAY);
          TexCoordPointer(GLSLTypeComponentCount(LMesh.FType[attrTexCoord0]),
            GLSLTypeEnum(LMesh.FType[attrTexCoord0]), 0,
            LMesh.FAttributeArrays[attrTexCoord0].List);
        end
        else
          DisableClientState(GL_TEXTURE_COORD_ARRAY);
      end;
      // Colors
      if LMesh.FAttributes[attrColor] then
      begin
        EnableClientState(GL_COLOR_ARRAY);
        ColorPointer(GLSLTypeComponentCount(LMesh.FType[attrColor]),
          GLSLTypeEnum(LMesh.FType[attrColor]), 0,
          LMesh.FAttributeArrays[attrColor].List);
      end
      else
        DisableClientState(GL_COLOR_ARRAY);
      // Normals
      if LMesh.FAttributes[attrNormal] and
        (GLSLTypeComponentCount(LMesh.FType[attrNormal]) = 3) then
      begin
        EnableClientState(GL_NORMAL_ARRAY);
        NormalPointer(GLSLTypeEnum(LMesh.FType[attrNormal]), 0,
          LMesh.FAttributeArrays[attrNormal].List);
      end
      else
        DisableClientState(GL_NORMAL_ARRAY);
      // Positions
      if LMesh.FAttributes[attrPosition] then
      begin
        EnableClientState(GL_VERTEX_ARRAY);
        VertexPointer(GLSLTypeComponentCount(LMesh.FType[attrPosition]),
          GLSLTypeEnum(LMesh.FType[attrPosition]), 0,
          LMesh.FAttributeArrays[attrPosition].List);
      end
      else
        DisableClientState(GL_VERTEX_ARRAY);

      ARci.PipelineTransformation.LoadMatrices;

      if LMesh.FRestartIndex > $FFFF then
        glType := GL_UNSIGNED_INT
      else
        glType := GL_UNSIGNED_SHORT;
      LArrayAddress := LMesh.FElements.List;
      LCount := LMesh.FElements.Count;

      repeat
        glPrimitive := cPrimitiveType[LMesh.FPrimitive];

        repeat
          Dec(LInstanceID);
          if Assigned(LInstanceChain) then
            ApplyInstance(ARci, LInstanceChain, LInstanceID);

          if LMesh.FHasIndices then
            DrawElements(glPrimitive, LCount, glType, LArrayAddress)
          else
          begin
            if VERSION_1_4 then
              MultiDrawArrays(glPrimitive, PGLint(LMesh.FRestartVertex.List),
                PGLsizei(LMesh.FStripCounts.List), LMesh.FRestartVertex.Count)
            else
              for T := 0 to LMesh.FRestartVertex.Count - 1 do
                DrawArrays(glPrimitive, LMesh.FRestartVertex.List[T],
                  LMesh.FStripCounts.List[T]);
          end;

        until LInstanceID <= 0;

        if not Assigned(LMaterial) then
          break;
      until not LMaterial.UnApply(ARci);

    finally
      ARci := storeRci;
    end;
end;

{$IFDEF GLS_REGION}{$ENDREGION 'TGLDrawTechniqueOGL1'}{$ENDIF}
{$IFDEF GLS_REGION}{$REGION 'TPoolMap'}{$ENDIF}

function TPoolMap.AddSector(const AItem: TPoolSector): Integer;
var
  Ptr: PPoolSector;
begin
  New(Ptr);
  Ptr^ := AItem;
  Result := Add(Ptr);
end;

procedure TPoolMap.Clear;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Get(I) <> nil then
      Dispose(PPoolSector(Get(I)));
  inherited;
end;

constructor TPoolMap.Create(AType: TPoolMapType);
begin
  inherited Create;
  FType := AType;
end;

procedure TPoolMap.DeleteSector(Index: Integer);
var
  Ptr: PPoolSector;
  I: Integer;
begin
  Ptr := Get(Index);
  if Assigned(Ptr) then
    Dispose(Ptr);
  Delete(Index);
  if FType = pmtArray then
  begin
    for I := Index to Count - 1 do
    begin
      Ptr := Get(I);
      if Assigned(Ptr) and Assigned(Ptr^.Mesh) then
        TFriendlyMesh(Ptr^.Mesh).FArraySectorIndex := I;
    end;
  end
  else
  begin
    for I := Index to Count - 1 do
    begin
      Ptr := Get(I);
      if Assigned(Ptr) and Assigned(Ptr^.Mesh) then
        TFriendlyMesh(Ptr^.Mesh).FElementSectorIndex := I;
    end;
  end;
end;

function TPoolMap.GetSector(Index: Integer): PPoolSector;
begin
  Result := Get(Index);
end;

procedure TPoolMap.InsertSector(Index: Integer; const AItem: TPoolSector);
var
  Ptr: PPoolSector;
  I: Integer;
begin
  New(Ptr);
  Ptr^ := AItem;
  Insert(Index, Ptr);
  if FType = pmtArray then
  begin
    for I := Index to Count - 1 do
    begin
      Ptr := Get(I);
      if Assigned(Ptr) and Assigned(Ptr^.Mesh) then
        TFriendlyMesh(Ptr^.Mesh).FArraySectorIndex := I;
    end;
  end
  else
  begin
    for I := Index to Count - 1 do
    begin
      Ptr := Get(I);
      if Assigned(Ptr) and Assigned(Ptr^.Mesh) then
        TFriendlyMesh(Ptr^.Mesh).FElementSectorIndex := I;
    end;
  end;
end;

procedure TPoolMap.PutSector(Index: Integer; Item: PPoolSector);
begin
  Put(Index, Item);
end;

{$IFDEF GLS_REGION}{$ENDREGION 'TPoolMap'}{$ENDIF}
{$IFDEF GLS_REGION}{$REGION 'TGLDrawTechniqueOGL2'}{$ENDIF}
// ------------------
// ------------------ TGLDrawTechniqueOGL2 ------------------
// ------------------

constructor TGLDrawTechniqueOGL2.Create;
begin
  if Self = nil then
    exit;
  if Assigned(Self.FArrayHandle) then
    exit;
  FArrayHandle := TGLVBOArrayBufferHandle.Create;
  FArrayBufferMap := TPoolMap.Create(pmtArray);
  FElementHandle := TGLVBOElementArrayHandle.Create;
  FElementBufferMap := TPoolMap.Create(pmtElement);
end;

destructor TGLDrawTechniqueOGL2.Destroy;
begin
  FArrayHandle.Destroy;
  FElementHandle.Destroy;
  FArrayBufferMap.Destroy;
  FElementBufferMap.Destroy;
end;

procedure TGLDrawTechniqueOGL2.DoBeforeAABBDrawing
  (var ARci: TRenderContextInfo);
var
  L: Integer;
begin
  ARci.GLStates.ArrayBufferBinding := 0;
  ARci.GLStates.ElementBufferBinding := 0;
  with GL do
    for L := 15 downto 0 do
      DisableVertexAttribArray(L);
  GetAABBMaterial.Apply(ARci);
  if ARci.GLStates.CurrentProgram = 0 then
    GL.EnableClientState(GL_VERTEX_ARRAY)
  else
    GL.EnableVertexAttribArray(Ord(attrPosition));
end;

procedure TGLDrawTechniqueOGL2.DoAfterAABBDrawing(var ARci: TRenderContextInfo);
begin
  if ARci.GLStates.CurrentProgram = 0 then
    GL.DisableClientState(GL_VERTEX_ARRAY)
  else
    GL.DisableVertexAttribArray(Ord(attrPosition));
  GetAABBMaterial.UnApply(ARci);
end;

procedure TGLDrawTechniqueOGL2.DoBeforePicking(AnObjectCountGuess: Integer);
var
  storeClearColor: TVector4f;
begin
  with CurrentGLContext do
  begin
    storeClearColor := GLStates.ColorClearValue;
    GLStates.ScissorBox := PipelineTransformation.PickingBox;
    GLStates.Enable(stScissorTest);
    GLStates.ColorClearValue := clrWhite;
    GLStates.DepthWriteMask := True;
    GL.Clear(GL_DEPTH_BUFFER_BIT or GL_COLOR_BUFFER_BIT);
    GLStates.Disable(stScissorTest);
    GLStates.ColorClearValue := storeClearColor;
  end;
end;

procedure TGLDrawTechniqueOGL2.DoAfterPicking(const AList: TList);
var
  LReadBuffer: PIntegerArray;
  LBox: TVector4i;
  LPixels: Integer;
  I, N: Integer;
  pBatch: PDrawBatch;
begin
  for I := 0 to AList.Count - 1 do
  begin
    pBatch := AList[I];
    pBatch^.PickFlag := Assigned(pBatch.PickCallback);
  end;

  LBox := CurrentGLContext.PipelineTransformation.PickingBox;
  LPixels := LBox[2] * LBox[3];
  if LPixels > 0 then
  begin
    GetMem(LReadBuffer, LPixels * 4);
    try
      GL.ReadBuffer(GL_BACK);
      GL.ReadPixels(LBox[0], LBox[1], LBox[2], LBox[3], GL_RGBA,
        GL_UNSIGNED_BYTE, LReadBuffer);

      if GL.GREMEDY_frame_terminator then
        GL.FrameTerminatorGREMEDY;
      for I := LPixels - 1 downto 0 do
      begin
        N := LReadBuffer[I] and $FFFFFF;
        if N < AList.Count then
        begin
          pBatch := AList[N];
          if pBatch.PickFlag then
          begin
            pBatch.PickCallback();
            pBatch.PickFlag := False;
          end;
        end;
      end;
    finally
      FreeMem(LReadBuffer);
    end;
  end;
end;

procedure TGLDrawTechniqueOGL2.AllocateBuffers;
var
  VBOFreeMem: TVector4ui;
  VBOPool: Cardinal;
  ArraySector: TPoolSector;
  ElementSector: TPoolSector;
  BufferType: TGLEnum;
begin
  FArrayHandle.AllocateHandle;
  FElementHandle.AllocateHandle;
  if FArrayHandle.IsDataNeedUpdate or FElementHandle.IsDataNeedUpdate then
    with GL do
    begin
      if (FArrayBufferMap.Count > 0) or (FElementBufferMap.Count > 0) then
      begin
        GLSLogger.LogDebug('Reset static buffers pool');
        FArrayBufferMap.Clear;
        FElementBufferMap.Clear;
      end;

      ArraySector.Offset := 0;
      ArraySector.Size := 0;
      ArraySector.Mesh := nil;
      ElementSector.Offset := 0;
      ElementSector.Size := 0;
      ElementSector.Mesh := nil;

      if IsDesignTime then
      begin
        VBOPool := VBO_STATIC_POOL_SIZE;
      end
      else if ATI_meminfo then
      begin
        GetIntegerv(GL_VBO_FREE_MEMORY_ATI, @VBOFreeMem[0]);
        GLSLogger.LogInfo(Format('Free graphic memory avaible - %dM',
          [VBOFreeMem[1] div 1024]));
        VBOPool := VBOFreeMem[1] * 1024 div 4;
        VBOPool := MinInteger(VBOPool, 8 * VBO_STATIC_POOL_SIZE);
      end
      else if NVX_gpu_memory_info then
      begin
        GetIntegerv(GL_GPU_MEMORY_INFO_TOTAL_AVAILABLE_MEMORY_NVX,
          @VBOFreeMem[1]);
        GLSLogger.LogInfo(Format('Free graphic memory avaible - %dM',
          [VBOFreeMem[1] div 1024]));
        VBOPool := VBOFreeMem[1] * 1024 div 4;
        VBOPool := MinInteger(VBOPool, 8 * VBO_STATIC_POOL_SIZE);
      end
      else
      begin
        VBOPool := VBO_STATIC_POOL_SIZE;
        GLSLogger.LogInfo
          ('Can''t get info about graphic memory. Allocate pool size of 16M');
      end;
      ArraySector.Size := 3 * VBOPool div 4;
      ElementSector.Size := VBOPool - ArraySector.Size;

      if IsDesignTime then
        BufferType := GL_DYNAMIC_DRAW
      else
        BufferType := GL_STATIC_DRAW;

      ArraySector.Size := ArraySector.Size * 2;
      ElementSector.Size := ElementSector.Size * 2;
      repeat
        GL.ClearError;
        ArraySector.Size := ArraySector.Size div 2;
        ElementSector.Size := ElementSector.Size div 2;
        FArrayHandle.BindBufferData(nil, ArraySector.Size, BufferType);
        FElementHandle.BindBufferData(nil, ElementSector.Size, BufferType);
        Finish;
      until GL.GetError = GL_NO_ERROR;

      FArrayHandle.NotifyDataUpdated;
      FElementHandle.NotifyDataUpdated;

      FArrayBufferMap.AddSector(ArraySector);
      FElementBufferMap.AddSector(ElementSector);

      GLSLogger.LogInfo(Format('Allocated static vertex buffer pool - %dM',
        [ArraySector.Size div $100000]));
      GLSLogger.LogInfo(Format('Allocated static element buffer pool - %dM',
        [ElementSector.Size div $100000]));

      // For bindless graphic
      with GL do
      begin
        if NV_vertex_buffer_unified_memory then
        begin
          GetBufferParameterui64vNV(FArrayHandle.VBOTarget,
            GL_BUFFER_GPU_ADDRESS_NV, @FArrayBufferAddress);
          MakeBufferResidentNV(FArrayHandle.VBOTarget, GL_READ_ONLY);
          GetBufferParameterui64vNV(FElementHandle.VBOTarget,
            GL_BUFFER_GPU_ADDRESS_NV, @FElementBufferAddress);
          MakeBufferResidentNV(FElementHandle.VBOTarget, GL_READ_ONLY);
        end;
      end;
    end;
end;

procedure TGLDrawTechniqueOGL2.PlacedInBuffer(AMesh: TMeshAtom);
var
  A: TAttribLocation;
  I, J: Integer;
  LDataSize: array [TAttribLocation] of Cardinal;
  RequestSize, Size, Offset, maxIndexValue, ElementsSize: Cardinal;
  ElementBufferSource: Pointer;
  LMesh: TFriendlyMesh;
  ArraySector: TPoolSector;
  ElementSector: TPoolSector;
  pSector: PPoolSector;
  BD: T4ByteData;
begin
  LMesh := TFriendlyMesh(AMesh);
  // Calculate size of array
  RequestSize := 0;
  for A := High(TAttribLocation) downto Low(TAttribLocation) do
    if LMesh.FAttributes[A] then
    begin
      LDataSize[A] := LMesh.FAttributeArrays[A].Count * SizeOf(T4ByteData);
      Inc(RequestSize, LDataSize[A]);
    end
    else
      LDataSize[A] := 0;

  // Check for empty mesh
  if RequestSize = 0 then
  begin
    LMesh.FValid := False;
    exit;
  end
  else if Cardinal(FArrayHandle.BufferSize) < RequestSize then
  begin
    LMesh.FValid := False;
    GLSLogger.LogWarningFmt('Mesh "%s" too big for static vertex array pool',
      [LMesh.TagName]);
    exit;
  end;

  if IsDesignTime then
  begin
    Offset := 0;
  end
  else
  begin
    // choose place in pool to upload data
    J := LMesh.FArraySectorIndex;
    if J > -1 then
    begin
      // Sector overflow
      pSector := FArrayBufferMap.Sectors[J];
      if pSector.Size < RequestSize then
      begin
        // Lool into next sector, it may be free
        Size := pSector^.Size;
        I := J + 1;
        while (I < FArrayBufferMap.Count) do
        begin
          if FArrayBufferMap.Sectors[I].Mesh = nil then
            Size := Size + FArrayBufferMap.Sectors[I].Size
          else
            break;
          if Size >= RequestSize then
            break;
          Inc(I);
        end;

        if Size >= RequestSize then
        begin
          // Merge sectors
          for I := J + 1 to I do
            FArrayBufferMap.DeleteSector(J + 1);

          // Extract the residue
          if (Size - RequestSize) > 0 then
          begin
            ArraySector.Offset := FArrayBufferMap.Sectors[J].Offset + RequestSize;
            ArraySector.Size := Size - RequestSize;
            ArraySector.Mesh := nil;
            if J < FArrayBufferMap.Count - 1 then
              FArrayBufferMap.InsertSector(J, ArraySector)
            else
              FArrayBufferMap.AddSector(ArraySector);
          end;

          // Set new parameters
          LMesh.FArraySectorIndex := J;
          pSector := FArrayBufferMap.Sectors[J];
          pSector.Size := RequestSize;
          pSector.Mesh := LMesh;
        end
        else
        begin
          // Free sector
          pSector := FArrayBufferMap.Sectors[J];
          pSector.Mesh := nil;
          LMesh.FArraySectorIndex := -1;
          J := -1;
        end;
      end;
    end;

    if J < 0 then
    begin
      // Find free sector
      Size := FArrayHandle.BufferSize;
      for I := 0 to FArrayBufferMap.Count - 1 do
        if (FArrayBufferMap.Sectors[I].Mesh = nil) and
          (FArrayBufferMap.Sectors[I].Size >= RequestSize) and
          (FArrayBufferMap.Sectors[I].Size <= Size) then
        begin
          J := I;
          Size := FArrayBufferMap.Sectors[I].Size;
        end;

      // Check overflow
      if J < 0 then
      begin
        // TODO: defragmentation
        LMesh.FValid := False;
        GLSLogger.LogError('Static vertex array pool is full');
        DumpPool(FArrayBufferMap);
        exit;
      end;

      // Extract the residue
      if (FArrayBufferMap.Sectors[J].Size - RequestSize) > 0 then
      begin
        ArraySector.Offset := FArrayBufferMap.Sectors[J].Offset + RequestSize;
        ArraySector.Size := FArrayBufferMap.Sectors[J].Size - RequestSize;
        ArraySector.Mesh := nil;
        if J < FArrayBufferMap.Count - 1 then
          FArrayBufferMap.InsertSector(J, ArraySector)
        else
          FArrayBufferMap.AddSector(ArraySector);
      end;

      // Set new parameters
      LMesh.FArraySectorIndex := J;
      pSector := FArrayBufferMap.Sectors[J];
      pSector.Size := RequestSize;
      pSector.Mesh := LMesh;
    end;

    Offset := FArrayBufferMap.Sectors[J].Offset;
  end;

  // upload each attribute array one after another
  FArrayHandle.Bind;

  for A := Low(TAttribLocation) to High(TAttribLocation) do
    if LMesh.FAttributes[A] then
    begin
      FArrayHandle.BufferSubData(Offset, LDataSize[A],
        LMesh.FAttributeArrays[A].List);
      Inc(Offset, LDataSize[A]);
    end;

  if LMesh.FHasIndices then
  begin
    LMesh.Lock;
    try
      if LMesh.FTrianglesElements.Revision <> LMesh.FElements.Revision then
        LMesh.MakeTriangleElements;
    finally
      LMesh.UnLock;
    end;
    maxIndexValue := LMesh.FAttributeArrays[attrPosition]
      .Count div GLSLTypeComponentCount(LMesh.FType[attrPosition]);
    // Adjust index type according it's number
    if (maxIndexValue + 1 < $10000) and not IsDesignTime then
    begin
      LMesh.FRestartIndex := $FFFF;
      ElementsSize := LMesh.FElements.Count * SizeOf(TGLUShort);
      RoundTo(ElementsSize, 4);
      GetMem(ElementBufferSource, ElementsSize);
      for I := LMesh.FElements.Count - 1 downto 0 do
      begin
        BD := LMesh.FElements[I];
        PWordVector(ElementBufferSource)[I] := BD.Word.Value[0];
      end;
    end
    else
    begin
      LMesh.FRestartIndex := $FFFFFFFF;
      ElementsSize := LMesh.FElements.Count * SizeOf(TGLuint);
      ElementBufferSource := nil;
    end;
    RequestSize := ElementsSize;
    Inc(RequestSize, LMesh.FTrianglesElements.Count * SizeOf(TGLuint));
    Inc(RequestSize, LMesh.FAdjacencyElements.Count * SizeOf(TGLuint));

    // Check for empty mesh (presumably an incredible situation)
    if RequestSize = 0 then
    begin
      LMesh.FValid := False;
      exit;
    end
    else if Cardinal(FElementHandle.BufferSize) < RequestSize then
    begin
      LMesh.FValid := False;
      GLSLogger.LogWarningFmt('Mesh "%s" too big for static element array pool',
        [LMesh.TagName]);
      exit;
    end;

    if IsDesignTime then
    begin
      Offset := 0;
    end
    else
    begin
      // choose place in pool to upload data
      J := LMesh.FElementSectorIndex;
      if J > -1 then
      begin
        // Sector overflow
        if FElementBufferMap.Sectors[J].Size < RequestSize then
        begin
          // Look into next sector, it may be free
          Size := FElementBufferMap.Sectors[J].Size;
          I := J + 1;
          while (I < FElementBufferMap.Count) do
          begin
            if FElementBufferMap.Sectors[I].Mesh = nil then
              Size := Size + FElementBufferMap.Sectors[I].Size
            else
              break;
            if Size >= RequestSize then
              break;
            Inc(I);
          end;

          if Size >= RequestSize then
          begin
            // Merge sectors
            for I := J + 1 to I do
              FElementBufferMap.Delete(J + 1);

            // Extract the residue
            if (Size - RequestSize) > 0 then
            begin
              ElementSector.Offset := FElementBufferMap.Sectors[J].Offset +
                RequestSize;
              ElementSector.Size := Size - RequestSize;
              ElementSector.Mesh := nil;
              if J < FElementBufferMap.Count - 1 then
                FElementBufferMap.InsertSector(J, ElementSector)
              else
                FElementBufferMap.AddSector(ElementSector);
            end;

            // Set new parameters
            pSector.Size := RequestSize;
            pSector.Mesh := LMesh;
            pSector := FElementBufferMap.Sectors[J];
          end
          else
          begin
            // Free sector
            pSector := FElementBufferMap.Sectors[J];
            pSector.Mesh := nil;
            LMesh.FElementSectorIndex := -1;
            J := -1;
          end;
        end;
      end;

      if J < 0 then
      begin
        // Find free sector
        Size := FElementHandle.BufferSize;
        for I := 0 to FElementBufferMap.Count - 1 do
          if (FElementBufferMap.Sectors[I].Mesh = nil) and
            (FElementBufferMap.Sectors[I].Size >= RequestSize) and
            (FElementBufferMap.Sectors[I].Size <= Size) then
          begin
            J := I;
            Size := FElementBufferMap.Sectors[I].Size;
          end;

        // Check overflow
        if J < 0 then
        begin
          // TODO: defragmentation
          LMesh.FValid := False;
          GLSLogger.LogError('Static element array pool is full');
          DumpPool(FElementBufferMap);
          exit;
        end;

        // Extract the residue
        if (FElementBufferMap.Sectors[J].Size - RequestSize) > 0 then
        begin
          ElementSector.Offset := FElementBufferMap.Sectors[J].Offset +
            RequestSize;
          ElementSector.Size := FElementBufferMap.Sectors[J].Size - RequestSize;
          ElementSector.Mesh := nil;
          if J < FElementBufferMap.Count - 1 then
            FElementBufferMap.InsertSector(J, ElementSector)
          else
            FElementBufferMap.AddSector(ElementSector);
        end;

        // Set new parameters
        LMesh.FElementSectorIndex := J;
        pSector := FElementBufferMap.Sectors[J];
        pSector.Size := RequestSize;
        pSector.Mesh := LMesh;
      end;

      Offset := FElementBufferMap.Sectors[J].Offset;
    end;

    // upload element array
    FElementHandle.Bind;
    if Assigned(ElementBufferSource) then
    begin
      FElementHandle.BufferSubData(Offset, ElementsSize, ElementBufferSource);
      FreeMem(ElementBufferSource);
    end
    else
    begin
      FElementHandle.BufferSubData(Offset, ElementsSize, LMesh.FElements.List);
    end;

    if LMesh.FTrianglesElements.Count > 0 then
    begin
      // Pure triangle elements
      Inc(Offset, ElementsSize);
      FElementHandle.BufferSubData(Offset, LMesh.FTrianglesElements.DataSize,
        LMesh.FTrianglesElements.List);
      if LMesh.FAdjacencyElements.Count > 0 then
      begin
        // Adjacency elements
        Inc(Offset, LMesh.FTrianglesElements.DataSize);
        FElementHandle.BufferSubData(Offset, LMesh.FAdjacencyElements.DataSize,
          LMesh.FAdjacencyElements.List);
      end;
    end;
  end;
end;

procedure TGLDrawTechniqueOGL2.ApplyInstance(var ARci: TRenderContextInfo;
  const AInstance: TInstancesChain; const AID: Integer);
var
  LProgram: TGLint;
  A: TAttribLocation;
  T: TGLEnum;
  LLink: TFriendlyInstancesChain;
  I: Integer;
  L: TGLuint;
begin
  LLink := TFriendlyInstancesChain(AInstance);

  with GL do
  begin
    LProgram := ARci.GLStates.CurrentProgram;

    if LProgram > 0 then
    begin
      // Setup generic attributes
      for A := High(TAttribLocation) downto Low(TAttribLocation) do
      begin
        L := Ord(A);
        if LLink.FAttributes[A] then
        begin
          DisableVertexAttribArray(L);
          I := AID;
          if LLink.FAttributeDivisor[A] > 1 then
            I := I div LLink.FAttributeDivisor[A];
          case LLink.FType[A] of
            GLSLType1F:
              VertexAttrib1fv(L, @LLink.FAttributeArrays[A].List[I]);
            GLSLType2F:
              VertexAttrib2fv(L, @LLink.FAttributeArrays[A].List[2 * I]);
            GLSLType3f:
              VertexAttrib3fv(L, @LLink.FAttributeArrays[A].List[3 * I]);
            GLSLType4F:
              VertexAttrib4fv(L, @LLink.FAttributeArrays[A].List[4 * I]);
            GLSLTypeMat2F:
              begin
                VertexAttrib2fv(L, @LLink.FAttributeArrays[A].List[4 * I]);
                VertexAttrib2fv(L + 1, @LLink.FAttributeArrays[A].List
                  [4 * I + 2]);
              end;
            GLSLTypeMat3F:
              begin
                VertexAttrib3fv(L, @LLink.FAttributeArrays[A].List[9 * I]);
                VertexAttrib3fv(L + 1, @LLink.FAttributeArrays[A].List
                  [9 * I + 3]);
                VertexAttrib3fv(L + 2, @LLink.FAttributeArrays[A].List
                  [9 * I + 6]);
              end;
            GLSLTypeMat4F:
              begin
                VertexAttrib4fv(L, @LLink.FAttributeArrays[A].List[16 * I]);
                VertexAttrib4fv(L + 1, @LLink.FAttributeArrays[A].List
                  [16 * I + 4]);
                VertexAttrib4fv(L + 2, @LLink.FAttributeArrays[A].List
                  [16 * I + 8]);
                VertexAttrib4fv(L + 3, @LLink.FAttributeArrays[A].List
                  [16 * I + 12]);
              end;
            GLSLType1I:
              VertexAttribI1iv(L, @LLink.FAttributeArrays[A].List[I]);
            GLSLType2I:
              VertexAttribI2iv(L, @LLink.FAttributeArrays[A].List[2 * I]);
            GLSLType3I:
              VertexAttribI3iv(L, @LLink.FAttributeArrays[A].List[3 * I]);
            GLSLType4I:
              VertexAttribI4iv(L, @LLink.FAttributeArrays[A].List[4 * I]);
            GLSLType1UI:
              VertexAttribI1uiv(L, @LLink.FAttributeArrays[A].List[I]);
            GLSLType2UI:
              VertexAttribI2uiv(L, @LLink.FAttributeArrays[A].List[2 * I]);
            GLSLType3UI:
              VertexAttribI3uiv(L, @LLink.FAttributeArrays[A].List[3 * I]);
            GLSLType4UI:
              VertexAttribI4uiv(L, @LLink.FAttributeArrays[A].List[4 * I]);
          else
            Assert(False, glsErrorEx + glsUnknownType);
          end;
        end;
      end;
    end
    else
    begin
      // Texture coordinates
      T := 8;
      for A := attrTexCoord7 downto attrTexCoord0 do
      begin
        Dec(T);
        if LLink.FAttributes[A] then
        begin
          ClientActiveTexture(GL_TEXTURE0 + T);
          DisableClientState(GL_TEXTURE_COORD_ARRAY);
          I := AID;
          if LLink.FAttributeDivisor[A] > 1 then
            I := I div LLink.FAttributeDivisor[A];
          case LLink.FType[A] of
            GLSLType1F:
              MultiTexCoord1f(GL_TEXTURE0 + T,
                PSingle(@LLink.FAttributeArrays[A].List[I])^);
            GLSLType2F:
              MultiTexCoord2fv(GL_TEXTURE0 + T,
                @LLink.FAttributeArrays[A].List[2 * I]);
            GLSLType3f:
              MultiTexCoord3fv(GL_TEXTURE0 + T,
                @LLink.FAttributeArrays[A].List[3 * I]);
            GLSLType4F:
              MultiTexCoord4fv(GL_TEXTURE0 + T,
                @LLink.FAttributeArrays[A].List[4 * I]);
            GLSLType1I:
              MultiTexCoord1i(GL_TEXTURE0 + T,
                PInteger(@LLink.FAttributeArrays[A].List[I])^);
            GLSLType2I:
              MultiTexCoord2iv(GL_TEXTURE0 + T,
                @LLink.FAttributeArrays[A].List[2 * I]);
            GLSLType3I:
              MultiTexCoord3iv(GL_TEXTURE0 + T,
                @LLink.FAttributeArrays[A].List[3 * I]);
            GLSLType4I:
              MultiTexCoord4iv(GL_TEXTURE0 + T,
                @LLink.FAttributeArrays[A].List[4 * I]);
          end;
        end
        else if LLink.FAttributes[attrTexCoord0] then
        begin
          // Make first texture coordinates same for other, need for multitexturing
          // No sure is this usable for anyone
          ClientActiveTexture(GL_TEXTURE0 + T);
          DisableClientState(GL_TEXTURE_COORD_ARRAY);
          I := AID;
          if LLink.FAttributeDivisor[attrTexCoord0] > 1 then
            I := I div LLink.FAttributeDivisor[attrTexCoord0];
          case LLink.FType[A] of
            GLSLType1F:
              MultiTexCoord1f(GL_TEXTURE0 + T,
                PSingle(@LLink.FAttributeArrays[attrTexCoord0].List[I])^);
            GLSLType2F:
              MultiTexCoord2fv(GL_TEXTURE0 + T,
                @LLink.FAttributeArrays[attrTexCoord0].List[2 * I]);
            GLSLType3f:
              MultiTexCoord3fv(GL_TEXTURE0 + T,
                @LLink.FAttributeArrays[attrTexCoord0].List[3 * I]);
            GLSLType4F:
              MultiTexCoord4fv(GL_TEXTURE0 + T,
                @LLink.FAttributeArrays[attrTexCoord0].List[4 * I]);
            GLSLType1I:
              MultiTexCoord1i(GL_TEXTURE0 + T,
                PInteger(@LLink.FAttributeArrays[attrTexCoord0].List[I])^);
            GLSLType2I:
              MultiTexCoord2iv(GL_TEXTURE0 + T,
                @LLink.FAttributeArrays[attrTexCoord0].List[2 * I]);
            GLSLType3I:
              MultiTexCoord3iv(GL_TEXTURE0 + T,
                @LLink.FAttributeArrays[attrTexCoord0].List[3 * I]);
            GLSLType4I:
              MultiTexCoord4iv(GL_TEXTURE0 + T,
                @LLink.FAttributeArrays[attrTexCoord0].List[4 * I]);
          end;
        end;
      end;
      // Colors
      if LLink.FAttributes[attrColor] and (ARci.drawState <> dsPicking) then
      begin
        DisableClientState(GL_COLOR_ARRAY);
        I := AID;
        if LLink.FAttributeDivisor[attrColor] > 1 then
          I := I div LLink.FAttributeDivisor[attrColor];
        case LLink.FType[attrColor] of
          GLSLType3f:
            Color3fv(@LLink.FAttributeArrays[attrColor].List[3 * I]);
          GLSLType4F:
            Color4fv(@LLink.FAttributeArrays[attrColor].List[4 * I]);
          GLSLType3I:
            Color3iv(@LLink.FAttributeArrays[attrColor].List[3 * I]);
          GLSLType4I:
            Color4iv(@LLink.FAttributeArrays[attrColor].List[4 * I]);
        end;
      end;
      // Normals
      if LLink.FAttributes[attrNormal] then
      begin
        DisableClientState(GL_NORMAL_ARRAY);
        I := AID;
        if LLink.FAttributeDivisor[attrNormal] > 1 then
          I := I div LLink.FAttributeDivisor[attrNormal];
        case LLink.FType[attrColor] of
          GLSLType3f:
            Normal3fv(@LLink.FAttributeArrays[attrNormal].List[3 * I]);
          GLSLType3I:
            Normal3iv(@LLink.FAttributeArrays[attrNormal].List[3 * I]);
        end;
      end;
      // Positions
      if LLink.FAttributes[attrPosition] then
      begin
        DisableClientState(GL_VERTEX_ARRAY);
        I := AID;
        if LLink.FAttributeDivisor[attrPosition] > 1 then
          I := I div LLink.FAttributeDivisor[attrPosition];
        case LLink.FType[attrPosition] of
          GLSLType2F:
            Vertex2fv(@LLink.FAttributeArrays[attrPosition].List[2 * I]);
          GLSLType3f:
            Vertex3fv(@LLink.FAttributeArrays[attrPosition].List[3 * I]);
          GLSLType4F:
            Vertex4fv(@LLink.FAttributeArrays[attrPosition].List[4 * I]);
          GLSLType2I:
            Vertex2iv(@LLink.FAttributeArrays[attrPosition].List[2 * I]);
          GLSLType3I:
            Vertex3iv(@LLink.FAttributeArrays[attrPosition].List[3 * I]);
          GLSLType4I:
            Vertex4iv(@LLink.FAttributeArrays[attrPosition].List[4 * I]);
        end;
      end;
    end;
  end;

  if LLink.FTransformationEnabled then
  begin
    ARci.PipelineTransformation.StackTop :=
      PTransformationRec(LLink.FTransformations[AID])^;
    ARci.PipelineTransformation.LoadMatrices;
  end;
end;

function TGLDrawTechniqueOGL2.BindStateHandle(var ARci: TRenderContextInfo;
  const AMesh: TMeshAtom): Boolean;
var
  LMesh: TFriendlyMesh;
  LProgram: TGLuint;
  LVAO: TGLVertexArrayHandle;

  A: TAttribLocation;
  L, T: TGLuint;
  Offsets: array [TAttribLocation] of Pointer;
  Offset: PtrUInt;
  Offsets64: array [TAttribLocation] of TGLuint64;
  Offset64: TGLuint64;
begin
  Result := False;
  LMesh := TFriendlyMesh(AMesh);
  LProgram := ARci.GLStates.CurrentProgram;
  if LProgram > 0 then
    LVAO := LMesh.GetVAO_Generic
  else if ARci.GLStates.ForwardContext then
    exit
  else
    LVAO := LMesh.GetVAO_BuildIn;

  if LVAO.IsSupported then
    LVAO.AllocateHandle;

  if vBindlessGraphicsEnabled and GL.NV_vertex_buffer_unified_memory then
  begin
    if IsDesignTime then
      Offset64 := FArrayBufferAddress
    else
      Offset64 := FArrayBufferAddress + FArrayBufferMap.Sectors
        [LMesh.FArraySectorIndex].Offset;
    for A := Low(TAttribLocation) to High(TAttribLocation) do
    begin
      Offsets64[A] := Offset64;
      if LMesh.FAttributes[A] then
        Inc(Offset64, LMesh.FAttributeArrays[A].DataSize);
    end;

    with GL do
    begin
      // Make draw calls use the GPU address VAO state, not the handle VAO state
      ARci.GLStates.ArrayBufferUnified := True;
      if LMesh.FHasIndices then
      begin
        ARci.GLStates.ElementBufferUnified := True;
        BufferAddressRangeNV(GL_ELEMENT_ARRAY_ADDRESS_NV, 0,
          FElementBufferAddress, FElementBufferMap.Sectors
          [LMesh.FElementSectorIndex].Size);
      end;

      if LProgram > 0 then
      begin
        // Setup attribute arrays pointer
        for A := High(TAttribLocation) downto Low(TAttribLocation) do
        begin
          L := Ord(A);
          if LMesh.FAttributes[A] then
          begin
            EnableVertexAttribArray(L);
            case LMesh.FType[A] of
              GLSLType1F:
                VertexAttribFormatNV(L, 1, GL_FLOAT, False, 0);
              GLSLType2F:
                VertexAttribFormatNV(L, 2, GL_FLOAT, False, 0);
              GLSLType3f:
                VertexAttribFormatNV(L, 3, GL_FLOAT, False, 0);
              GLSLType4F:
                VertexAttribFormatNV(L, 4, GL_FLOAT, False, 0);
              GLSLTypeMat2F:
                VertexAttribFormatNV(L, 4, GL_FLOAT, False, 0);
              GLSLTypeMat3F:
                VertexAttribFormatNV(L, 9, GL_FLOAT, False, 0);
              GLSLTypeMat4F:
                VertexAttribFormatNV(L, 16, GL_FLOAT, False, 0);
              GLSLType1I:
                VertexAttribIFormatNV(L, 1, GL_INT, 0);
              GLSLType2I:
                VertexAttribIFormatNV(L, 2, GL_INT, 0);
              GLSLType3I:
                VertexAttribIFormatNV(L, 3, GL_INT, 0);
              GLSLType4I:
                VertexAttribIFormatNV(L, 4, GL_UNSIGNED_INT, 0);
              GLSLType1UI:
                VertexAttribIFormatNV(L, 1, GL_UNSIGNED_INT, 0);
              GLSLType2UI:
                VertexAttribIFormatNV(L, 2, GL_UNSIGNED_INT, 0);
              GLSLType3UI:
                VertexAttribIFormatNV(L, 3, GL_UNSIGNED_INT, 0);
              GLSLType4UI:
                VertexAttribIFormatNV(L, 4, GL_UNSIGNED_INT, 0);
            else
              Assert(False, glsErrorEx + glsUnknownType);
            end;
            BufferAddressRangeNV(GL_VERTEX_ATTRIB_ARRAY_ADDRESS_NV, L,
              Offsets64[A], LMesh.FAttributeArrays[A].DataSize)
          end
          else
            DisableVertexAttribArray(L);
        end;
      end
      else
      begin
        // Build-in attributes
        begin
          // Predisable attributes array to avoid conflict
          for L := 15 downto 0 do
            DisableVertexAttribArray(L);
          T := 8;
          for A := attrTexCoord7 downto attrTexCoord0 do
          begin
            Dec(T);
            ClientActiveTexture(GL_TEXTURE0 + T);
            if LMesh.FAttributes[A] then
            begin
              EnableClientState(GL_TEXTURE_COORD_ARRAY);
              TexCoordFormatNV(GLSLTypeComponentCount(LMesh.FType[A]),
                GLSLTypeEnum(LMesh.FType[A]), 0);
              BufferAddressRangeNV(GL_TEXTURE_COORD_ARRAY_ADDRESS_NV, T,
                Offsets64[A], LMesh.FAttributeArrays[A].DataSize);
            end
            else if LMesh.FAttributes[attrTexCoord0] then
            begin
              // Make first texture coordinates same for other, need for multitexturing
              EnableClientState(GL_TEXTURE_COORD_ARRAY);
              TexCoordFormatNV(GLSLTypeComponentCount(LMesh.FType[attrTexCoord0]
                ), GLSLTypeEnum(LMesh.FType[attrTexCoord0]), 0);
              BufferAddressRangeNV(GL_TEXTURE_COORD_ARRAY_ADDRESS_NV, T,
                Offsets64[attrTexCoord0], LMesh.FAttributeArrays[attrTexCoord0]
                .DataSize);
            end
            else
              DisableClientState(GL_TEXTURE_COORD_ARRAY);
          end;
          // Colors
          if LMesh.FAttributes[attrColor] and (ARci.drawState <> dsPicking) then
          begin
            EnableClientState(GL_COLOR_ARRAY);
            ColorFormatNV(GLSLTypeComponentCount(LMesh.FType[attrColor]),
              GLSLTypeEnum(LMesh.FType[attrColor]), 0);
            BufferAddressRangeNV(GL_COLOR_ARRAY_ADDRESS_NV, 0,
              Offsets64[attrColor], LMesh.FAttributeArrays[attrColor].DataSize);
          end
          else
            DisableClientState(GL_COLOR_ARRAY);
          // Normals
          if LMesh.FAttributes[attrNormal] and
            (GLSLTypeComponentCount(LMesh.FType[attrNormal]) = 3) then
          begin
            EnableClientState(GL_NORMAL_ARRAY);
            NormalFormatNV(GLSLTypeEnum(LMesh.FType[attrNormal]), 0);
            BufferAddressRangeNV(GL_NORMAL_ARRAY_ADDRESS_NV, 0,
              Offsets64[attrNormal], LMesh.FAttributeArrays[attrNormal]
              .DataSize);
          end
          else
            DisableClientState(GL_NORMAL_ARRAY);
          // Positions
          if LMesh.FAttributes[attrPosition] then
          begin
            EnableClientState(GL_VERTEX_ARRAY);
            VertexFormatNV(GLSLTypeComponentCount(LMesh.FType[attrPosition]),
              GLSLTypeEnum(LMesh.FType[attrPosition]), 0);
            BufferAddressRangeNV(GL_VERTEX_ARRAY_ADDRESS_NV, 0,
              Offsets64[attrPosition], LMesh.FAttributeArrays[attrPosition]
              .DataSize);
          end
          else
            DisableClientState(GL_VERTEX_ARRAY);
        end;
      end;
    end;
    if LVAO.IsDataNeedUpdate then
      LVAO.NotifyDataUpdated;
  end
  else
  begin
    if LVAO.IsDataNeedUpdate then
      with GL do
      begin
        // Uniting all states and buffers in one vertex array object
        LVAO.Bind;

        // Need to direct bind array buffer for correctly VertexAttribPointer set up
        if ARci.GLStates.ArrayBufferBinding = FArrayHandle.Handle then
          GL.BindBuffer(GL_ARRAY_BUFFER, ARci.GLStates.ArrayBufferBinding)
        else
          FArrayHandle.Bind;
        if LMesh.FHasIndices then
          FElementHandle.Bind;

        if IsDesignTime then
          Offset := 0
        else
          Offset := FArrayBufferMap.Sectors[LMesh.FArraySectorIndex].Offset;
        for A := Low(TAttribLocation) to High(TAttribLocation) do
        begin
          Offsets[A] := Pointer(Offset);
          if LMesh.FAttributes[A] then
            Inc(Offset, LMesh.FAttributeArrays[A].Count * SizeOf(T4ByteData));
        end;

        if LProgram > 0 then
        begin
          // Setup attribute arrays pointer
          for A := High(TAttribLocation) downto Low(TAttribLocation) do
          begin
            L := Ord(A);
            if LMesh.FAttributes[A] then
            begin
              EnableVertexAttribArray(L);
              case LMesh.FType[A] of
                GLSLType1F:
                  VertexAttribPointer(L, 1, GL_FLOAT, False, 0, Offsets[A]);
                GLSLType2F:
                  VertexAttribPointer(L, 2, GL_FLOAT, False, 0, Offsets[A]);
                GLSLType3f:
                  VertexAttribPointer(L, 3, GL_FLOAT, False, 0, Offsets[A]);
                GLSLType4F:
                  VertexAttribPointer(L, 4, GL_FLOAT, False, 0, Offsets[A]);
                GLSLTypeMat2F:
                  VertexAttribPointer(L, 4, GL_FLOAT, False, 0, Offsets[A]);
                GLSLTypeMat3F:
                  VertexAttribPointer(L, 9, GL_FLOAT, False, 0, Offsets[A]);
                GLSLTypeMat4F:
                  VertexAttribPointer(L, 16, GL_FLOAT, False, 0, Offsets[A]);
                GLSLType1I:
                  VertexAttribIPointer(L, 1, GL_INT, 0, Offsets[A]);
                GLSLType2I:
                  VertexAttribIPointer(L, 2, GL_INT, 0, Offsets[A]);
                GLSLType3I:
                  VertexAttribIPointer(L, 3, GL_INT, 0, Offsets[A]);
                GLSLType4I:
                  VertexAttribIPointer(L, 4, GL_UNSIGNED_INT, 0, Offsets[A]);
                GLSLType1UI:
                  VertexAttribIPointer(L, 1, GL_UNSIGNED_INT, 0, Offsets[A]);
                GLSLType2UI:
                  VertexAttribIPointer(L, 2, GL_UNSIGNED_INT, 0, Offsets[A]);
                GLSLType3UI:
                  VertexAttribIPointer(L, 3, GL_UNSIGNED_INT, 0, Offsets[A]);
                GLSLType4UI:
                  VertexAttribIPointer(L, 4, GL_UNSIGNED_INT, 0, Offsets[A]);
              else
                Assert(False, glsErrorEx + glsUnknownType);
              end;
            end
            else
              DisableVertexAttribArray(L);
          end;
        end // of Generic attributes
        else
        // Build-in attributes
        begin
          T := 8;
          for A := attrTexCoord7 downto attrTexCoord0 do
          begin
            Dec(T);
            ClientActiveTexture(GL_TEXTURE0 + T);
            if LMesh.FAttributes[A] then
            begin
              EnableClientState(GL_TEXTURE_COORD_ARRAY);
              TexCoordPointer(GLSLTypeComponentCount(LMesh.FType[A]),
                GLSLTypeEnum(LMesh.FType[A]), 0, Offsets[A]);
            end
            else if LMesh.FAttributes[attrTexCoord0] then
            begin
              // Make first texture coordinates same for other, need for multitexturing
              EnableClientState(GL_TEXTURE_COORD_ARRAY);
              TexCoordPointer(GLSLTypeComponentCount(LMesh.FType[attrTexCoord0]
                ), GLSLTypeEnum(LMesh.FType[attrTexCoord0]), 0,
                Offsets[attrTexCoord0]);
            end
            else
              DisableClientState(GL_TEXTURE_COORD_ARRAY);
          end;
          // Colors
          if LMesh.FAttributes[attrColor] then
          begin
            EnableClientState(GL_COLOR_ARRAY);
            ColorPointer(GLSLTypeComponentCount(LMesh.FType[attrColor]),
              GLSLTypeEnum(LMesh.FType[attrColor]), 0, Offsets[attrColor]);
          end
          else
            DisableClientState(GL_COLOR_ARRAY);
          // Normals
          if LMesh.FAttributes[attrNormal] and
            (GLSLTypeComponentCount(LMesh.FType[attrNormal]) = 3) then
          begin
            EnableClientState(GL_NORMAL_ARRAY);
            NormalPointer(GLSLTypeEnum(LMesh.FType[attrNormal]), 0,
              Offsets[attrNormal]);
          end
          else
            DisableClientState(GL_NORMAL_ARRAY);
          // Positions
          if LMesh.FAttributes[attrPosition] then
          begin
            EnableClientState(GL_VERTEX_ARRAY);
            VertexPointer(GLSLTypeComponentCount(LMesh.FType[attrPosition]),
              GLSLTypeEnum(LMesh.FType[attrPosition]), 0,
              Offsets[attrPosition]);
          end
          else
            DisableClientState(GL_VERTEX_ARRAY);
        end;

        LVAO.NotifyDataUpdated;
      end
    else
      LVAO.Bind;
  end;

  with ARci.GLStates do
  begin
    EnablePrimitiveRestart := LMesh.FHasIndices;
    PrimitiveRestartIndex := LMesh.FRestartIndex;
  end;

  Result := True;
end;

procedure TGLDrawTechniqueOGL2.DrawAABB(var ARci: TRenderContextInfo;
  const ABatch: TDrawBatch);
var
  LMesh: TFriendlyMesh;
  LPositions: array [0 .. 7] of TVector3f;
begin
  with GL do
  begin
    LMesh := TFriendlyMesh(ABatch.Mesh);
    with LMesh.AABB do
    begin
      LPositions[2] := max;
      LPositions[4] := min;
    end;
    LPositions[0] := Vector3fMake(LPositions[4][0], LPositions[2][1],
      LPositions[4][2]);
    LPositions[1] := Vector3fMake(LPositions[4][0], LPositions[2][1],
      LPositions[2][2]);
    LPositions[3] := Vector3fMake(LPositions[2][0], LPositions[2][1],
      LPositions[4][2]);
    LPositions[5] := Vector3fMake(LPositions[4][0], LPositions[4][1],
      LPositions[2][2]);
    LPositions[6] := Vector3fMake(LPositions[2][0], LPositions[4][1],
      LPositions[2][2]);
    LPositions[7] := Vector3fMake(LPositions[2][0], LPositions[4][1],
      LPositions[4][2]);

    ARci.PipelineTransformation.Push(ABatch.Transformation);
    ARci.PipelineTransformation.LoadMatrices;
    try
      if ARci.GLStates.CurrentProgram = 0 then
      begin
        VertexPointer(3, GL_FLOAT, 0, @LPositions[0]);
        DrawElements(GL_LINES, 24, GL_UNSIGNED_SHORT, @cAABBIndices[0]);
      end
      else
      begin
        VertexAttribPointer(Ord(attrPosition), 3, GL_FLOAT, False, 0,
          @LPositions[0]);
        DrawElements(GL_LINES, 24, GL_UNSIGNED_SHORT, @cAABBIndices[0]);
      end;
    finally
      ARci.PipelineTransformation.Pop;
    end;
  end;
end;

procedure TGLDrawTechniqueOGL2.DrawBatch(var ARci: TRenderContextInfo;
  const ABatch: TDrawBatch);
var
  storeRci: TRenderContextInfo;
  LMesh: TFriendlyMesh;
  LInstanceChain: TInstancesChain;
  LMaterial: TGLAbstractLibMaterial;
  LInstanceID: Integer;
  glPrimitive: TGLEnum;
  glType: TGLEnum;
  LOffset: Pointer;
begin
  AllocateBuffers;

  LMesh := TFriendlyMesh(ABatch.Mesh);
  if IsDesignTime or (LMesh.FRevisionNum <> LMesh.FBufferRevision) then
  begin
    PlacedInBuffer(LMesh);
    if not LMesh.IsValid then
      exit;
    LMesh.FBufferRevision := LMesh.FRevisionNum;
  end;

  if Assigned(ABatch.InstancesChain) and ABatch.InstancesChain.IsValid then
  begin
    LInstanceChain := ABatch.InstancesChain;
    LInstanceID := LInstanceChain.InstanceCount;
  end
  else
  begin
    LInstanceChain := nil;
    LInstanceID := 1;
  end;

  if ARci.drawState = dsPicking then
  begin
    LMaterial := vDefaultPickingMaterial;
  end
  else
    LMaterial := ABatch.Material;

  storeRci := ARci;

  if Assigned(ABatch.Transformation) then
    ARci.PipelineTransformation.StackTop := ABatch.Transformation^;
  with GL do
    try
      glPrimitive := cPrimitiveType[LMesh.FPrimitive];
      if LMesh.FRestartIndex > $FFFF then
        glType := GL_UNSIGNED_INT
      else
        glType := GL_UNSIGNED_SHORT;

      if IsDesignTime or not LMesh.FHasIndices then
        LOffset := nil
      else
        LOffset := Pointer(FElementBufferMap.Sectors
          [LMesh.FElementSectorIndex].Offset);

      if Assigned(LMaterial) then
        LMaterial.Apply(ARci);

      if BindStateHandle(ARci, LMesh) then
        repeat

          if (ARci.drawState = dsPicking) and
            (ARci.GLStates.CurrentProgram = 0) then
          begin
            DisableClientState(GL_COLOR_ARRAY);
            Color3ubv(@ABatch.ListIndex);
          end;

          repeat
            Dec(LInstanceID);
            if Assigned(LInstanceChain) then
              ApplyInstance(ARci, LInstanceChain, LInstanceID);

            if LMesh.FHasIndices then
              DrawElements(glPrimitive, LMesh.FElements.Count, glType, LOffset)
            else
            begin
              MultiDrawArrays(glPrimitive, PGLint(LMesh.FRestartVertex.List),
                PGLsizei(LMesh.FStripCounts.List), LMesh.FRestartVertex.Count);
            end;
          until LInstanceID <= 0;

          if not Assigned(LMaterial) then
            break;
        until not LMaterial.UnApply(ARci);

      // Restore client state
      if (ARci.drawState = dsPicking) and LMesh.FAttributes[attrColor] then
      begin
        if ARci.GLStates.CurrentProgram = 0 then
          EnableClientState(GL_COLOR_ARRAY);
      end;

    finally
      ARci := storeRci;
      ARci.GLStates.VertexArrayBinding := 0;
    end;
end;

procedure TGLDrawTechniqueOGL2.DrawDynamicBatch(var ARci: TRenderContextInfo;
  const ABatch: TDrawBatch);
var
  LMesh: TFriendlyMesh;
  LInstanceChain: TInstancesChain;
  LMaterial: TGLAbstractLibMaterial;
  glPrimitive: TGLEnum;
  glType: TGLEnum;
  A: TAttribLocation;
  L, LInstanceID, T: Integer;
  LArrayAddress: Pointer;
  storeRci: TRenderContextInfo;
  LCount: Integer;
begin
  if not ABatch.Mesh.IsValid then
    exit;

  LMesh := TFriendlyMesh(ABatch.Mesh);
  if Assigned(ABatch.InstancesChain) and ABatch.InstancesChain.IsValid then
  begin
    LInstanceChain := ABatch.InstancesChain;
    LInstanceID := LInstanceChain.InstanceCount;
  end
  else
  begin
    LInstanceChain := nil;
    LInstanceID := 1;
  end;

  ARci.GLStates.VertexArrayBinding := 0;
  ARci.GLStates.ArrayBufferBinding := 0;
  ARci.GLStates.ElementBufferBinding := 0;

  with ARci.GLStates do
  begin
    EnablePrimitiveRestart := LMesh.FHasIndices;
    PrimitiveRestartIndex := LMesh.FRestartIndex;
  end;

  if ARci.drawState = dsPicking then
  begin
    LMaterial := vDefaultPickingMaterial;
  end
  else
    LMaterial := ABatch.Material;

  storeRci := ARci;

  if Assigned(ABatch.Transformation) then
    ARci.PipelineTransformation.StackTop := ABatch.Transformation^;
  with GL do
    try
      if Assigned(LMaterial) then
        LMaterial.Apply(ARci);

      if ARci.GLStates.CurrentProgram > 0 then
      begin
        // Setup generic attribute arrays
        for A := High(TAttribLocation) downto Low(TAttribLocation) do
        begin
          L := Ord(A);
          if LMesh.FAttributes[A] then
          begin
            LArrayAddress := LMesh.FAttributeArrays[A].List;
            EnableVertexAttribArray(L);
            case LMesh.FType[A] of
              GLSLType1F:
                VertexAttribPointer(L, 1, GL_FLOAT, False, 0, LArrayAddress);
              GLSLType2F:
                VertexAttribPointer(L, 2, GL_FLOAT, False, 0, LArrayAddress);
              GLSLType3f:
                VertexAttribPointer(L, 3, GL_FLOAT, False, 0, LArrayAddress);
              GLSLType4F:
                VertexAttribPointer(L, 4, GL_FLOAT, False, 0, LArrayAddress);
              GLSLTypeMat2F:
                VertexAttribPointer(L, 4, GL_FLOAT, False, 0, LArrayAddress);
              GLSLTypeMat3F:
                VertexAttribPointer(L, 9, GL_FLOAT, False, 0, LArrayAddress);
              GLSLTypeMat4F:
                VertexAttribPointer(L, 16, GL_FLOAT, False, 0, LArrayAddress);
              GLSLType1I:
                VertexAttribIPointer(L, 1, GL_INT, 0, LArrayAddress);
              GLSLType2I:
                VertexAttribIPointer(L, 2, GL_INT, 0, LArrayAddress);
              GLSLType3I:
                VertexAttribIPointer(L, 3, GL_INT, 0, LArrayAddress);
              GLSLType4I:
                VertexAttribIPointer(L, 4, GL_UNSIGNED_INT, 0, LArrayAddress);
              GLSLType1UI:
                VertexAttribIPointer(L, 1, GL_UNSIGNED_INT, 0, LArrayAddress);
              GLSLType2UI:
                VertexAttribIPointer(L, 2, GL_UNSIGNED_INT, 0, LArrayAddress);
              GLSLType3UI:
                VertexAttribIPointer(L, 3, GL_UNSIGNED_INT, 0, LArrayAddress);
              GLSLType4UI:
                VertexAttribIPointer(L, 4, GL_UNSIGNED_INT, 0, LArrayAddress);
            else
              Assert(False, glsErrorEx + glsUnknownType);
            end;
          end
          else
            DisableVertexAttribArray(L);
        end;
      end // of Generic attributes
      else
      // Build-in attributes
      begin
        T := 8;
        for A := attrTexCoord7 downto attrTexCoord0 do
        begin
          Dec(T);
          ClientActiveTexture(GL_TEXTURE0 + T);
          if LMesh.FAttributes[A] then
          begin
            EnableClientState(GL_TEXTURE_COORD_ARRAY);
            TexCoordPointer(GLSLTypeComponentCount(LMesh.FType[A]),
              GLSLTypeEnum(LMesh.FType[A]), 0, LMesh.FAttributeArrays[A].List);
          end
          else if LMesh.FAttributes[attrTexCoord0] then
          begin
            // Make first texture coordinates same for other, need for multitexturing
            EnableClientState(GL_TEXTURE_COORD_ARRAY);
            TexCoordPointer(GLSLTypeComponentCount(LMesh.FType[attrTexCoord0]),
              GLSLTypeEnum(LMesh.FType[attrTexCoord0]), 0,
              LMesh.FAttributeArrays[attrTexCoord0].List);
          end
          else
            DisableClientState(GL_TEXTURE_COORD_ARRAY);
        end;
        // Colors
        if LMesh.FAttributes[attrColor] then
        begin
          EnableClientState(GL_COLOR_ARRAY);
          ColorPointer(GLSLTypeComponentCount(LMesh.FType[attrColor]),
            GLSLTypeEnum(LMesh.FType[attrColor]), 0,
            LMesh.FAttributeArrays[attrColor].List);
        end
        else
          DisableClientState(GL_COLOR_ARRAY);
        // Normals
        if LMesh.FAttributes[attrNormal] and
          (GLSLTypeComponentCount(LMesh.FType[attrNormal]) = 3) then
        begin
          EnableClientState(GL_NORMAL_ARRAY);
          NormalPointer(GLSLTypeEnum(LMesh.FType[attrNormal]), 0,
            LMesh.FAttributeArrays[attrNormal].List);
        end
        else
          DisableClientState(GL_NORMAL_ARRAY);
        // Positions
        if LMesh.FAttributes[attrPosition] then
        begin
          EnableClientState(GL_VERTEX_ARRAY);
          VertexPointer(GLSLTypeComponentCount(LMesh.FType[attrPosition]),
            GLSLTypeEnum(LMesh.FType[attrPosition]), 0,
            LMesh.FAttributeArrays[attrPosition].List);
        end
        else
          DisableClientState(GL_VERTEX_ARRAY);
      end; // of Build-in attributes

      ARci.PipelineTransformation.LoadMatrices;

      if LMesh.FRestartIndex > $FFFF then
        glType := GL_UNSIGNED_INT
      else
        glType := GL_UNSIGNED_SHORT;
      LArrayAddress := LMesh.FElements.List;
      LCount := LMesh.FElements.Count;

      if (ARci.drawState = dsPicking) and
        (ARci.GLStates.CurrentProgram = 0) then
      begin
        DisableClientState(GL_COLOR_ARRAY);
        Color3ubv(@ABatch.ListIndex);
      end;

      repeat
        glPrimitive := cPrimitiveType[LMesh.FPrimitive];

        repeat
          Dec(LInstanceID);
          if Assigned(LInstanceChain) then
            ApplyInstance(ARci, LInstanceChain, LInstanceID);

          if LMesh.FHasIndices then
            DrawElements(glPrimitive, LCount, glType, LArrayAddress)
          else
          begin
            MultiDrawArrays(glPrimitive, PGLint(LMesh.FRestartVertex.List),
              PGLsizei(LMesh.FStripCounts.List), LMesh.FRestartVertex.Count);
          end;

        until LInstanceID <= 0;

        if not Assigned(LMaterial) then
          break;
      until not LMaterial.UnApply(ARci);

      // Restore client state
      if (ARci.drawState = dsPicking) and LMesh.FAttributes[attrColor] then
      begin
        if ARci.GLStates.CurrentProgram = 0 then
          EnableClientState(GL_COLOR_ARRAY);
      end;

    finally
      ARci := storeRci;
    end;
end;

procedure TGLDrawTechniqueOGL2.DumpPool(APool: TPoolMap);
var
  I: Integer;
begin
  for I := 0 to APool.Count - 1 do
  begin
    with APool.Sectors[I]^ do
    begin
      if Assigned(Mesh) then
        GLSLogger.LogDebugFmt('Sector of mesh "%s", size %d byte', [Mesh.TagName, Size])
      else
        GLSLogger.LogDebugFmt('Free sector, size %d byte', [Mesh.TagName, Size])
    end;
  end;
end;

{$IFDEF GLS_REGION}{$ENDREGION 'TGLDrawTechniqueOGL2'}{$ENDIF}
{$IFDEF GLS_REGION}{$REGION 'TGLDrawTechniqueOGL3'}{$ENDIF}

constructor TGLDrawTechniqueOGL3.Create;
begin
  inherited;
  FCommonVAO := TGLVertexArrayHandle.Create;
end;

destructor TGLDrawTechniqueOGL3.Destroy;
begin
  FCommonVAO.Destroy;
  inherited;
end;

procedure TGLDrawTechniqueOGL3.DoAfterAABBDrawing(var ARci: TRenderContextInfo);
begin
  if ARci.GLStates.CurrentProgram = 0 then
    GL.DisableClientState(GL_VERTEX_ARRAY)
  else
    GL.DisableVertexAttribArray(Ord(attrPosition));
  GetAABBMaterial.UnApply(ARci);
  if ARci.GLStates.ForwardContext then
    FCommonVAO.UnBind;
end;

procedure TGLDrawTechniqueOGL3.DoBeforeAABBDrawing
  (var ARci: TRenderContextInfo);
var
  L: Integer;
begin
  if ARci.GLStates.ForwardContext then
  begin
    FCommonVAO.AllocateHandle;
    FCommonVAO.Bind;
  end;
  with GL do
    for L := 15 downto 0 do
      DisableVertexAttribArray(L);
  ARci.GLStates.ArrayBufferBinding := 0;
  ARci.GLStates.ElementBufferBinding := 0;
  GetAABBMaterial.Apply(ARci);
  if ARci.GLStates.CurrentProgram = 0 then
    GL.EnableClientState(GL_VERTEX_ARRAY)
  else
    GL.EnableVertexAttribArray(Ord(attrPosition));
end;

procedure TGLDrawTechniqueOGL3.DrawBatch(var ARci: TRenderContextInfo;
  const ABatch: TDrawBatch);
var
  LMesh: TFriendlyMesh;
  LInstanceChain: TInstancesChain;
  LMaterial: TGLAbstractLibMaterial;
  glPrimitive: TGLEnum;
  glType: TGLEnum;
  LShift: PtrUInt;
  LOffset: Pointer;
  LCount, LInstanceID: Integer;
  storeRci: TRenderContextInfo;
begin
  AllocateBuffers;

  LMesh := TFriendlyMesh(ABatch.Mesh);
  if IsDesignTime or (LMesh.FRevisionNum <> LMesh.FBufferRevision) then
  begin
    PlacedInBuffer(LMesh);
    if not LMesh.IsValid then
      exit;
    LMesh.FBufferRevision := LMesh.FRevisionNum;
  end;

  if Assigned(ABatch.InstancesChain) and ABatch.InstancesChain.IsValid then
  begin
    LInstanceChain := ABatch.InstancesChain;
    LInstanceID := LInstanceChain.InstanceCount;
  end
  else
  begin
    LInstanceChain := nil;
    LInstanceID := 1;
  end;

  if ARci.drawState = dsPicking then
  begin
    if Assigned(ABatch.PickingMaterial) then
      LMaterial := ABatch.PickingMaterial
    else
      LMaterial := vDefaultPickingMaterial;
  end
  else
    LMaterial := ABatch.Material;

  storeRci := ARci;

  if Assigned(ABatch.Transformation) then
    ARci.PipelineTransformation.StackTop := ABatch.Transformation^;
  with GL do
    try
      if Assigned(LMaterial) then
        LMaterial.Apply(ARci);

      ARci.PipelineTransformation.LoadMatrices;

      if BindStateHandle(ARci, LMesh) then
      begin
        if LMesh.FRestartIndex > $FFFF then
          glType := GL_UNSIGNED_INT
        else
          glType := GL_UNSIGNED_SHORT;
        LShift := 0;
        LCount := LMesh.FElements.Count;

        repeat
          // Primitive selection
          glPrimitive := cPrimitiveType[LMesh.FPrimitive];
          if (ARci.primitiveMask = cAdjacencyPrimitives) and
            not(LMesh.FPrimitive in cAdjacencyPrimitives) then
          begin
            glPrimitive := GL_TRIANGLES_ADJACENCY;
            if LMesh.FHasIndices and (LMesh.FAdjacencyElements.Count > 0) then
            begin
              LShift := LCount;
              LCount := LMesh.FAdjacencyElements.Count;
              if glType = GL_UNSIGNED_SHORT then
              begin
                LShift := LShift * SizeOf(TGLUShort);
                RoundTo(LShift, 4);
                glType := GL_UNSIGNED_INT;
              end
              else
                LShift := LShift * SizeOf(TGLuint);
              LShift := LShift + Cardinal(LMesh.FTrianglesElements.Count *
                SizeOf(TGLuint));
            end
            else
              continue;
          end
          else if not(LMesh.FPrimitive in ARci.primitiveMask) then
            continue;

          if IsDesignTime or not LMesh.FHasIndices then
            LOffset := Pointer(LShift)
          else
            LOffset := Pointer(LShift + FElementBufferMap.Sectors
              [LMesh.FElementSectorIndex].Offset);

          if (ARci.drawState = dsPicking) and
            (ARci.GLStates.CurrentProgram = 0) then
          begin
            DisableClientState(GL_COLOR_ARRAY);
            Color3ubv(@ABatch.ListIndex);
          end;

          repeat
            Dec(LInstanceID);
            if Assigned(LInstanceChain) then
              ApplyInstance(ARci, LInstanceChain, LInstanceID);

            if LMesh.FHasIndices then
              DrawElements(glPrimitive, LCount, glType, LOffset)
            else
            begin
              MultiDrawArrays(glPrimitive, PGLint(LMesh.FRestartVertex.List),
                PGLsizei(LMesh.FStripCounts.List), LMesh.FRestartVertex.Count);
            end;

          until LInstanceID <= 0;

          if not Assigned(LMaterial) then
            break;
        until not LMaterial.UnApply(ARci);

        // Restore client state
        if (ARci.drawState = dsPicking) and LMesh.FAttributes[attrColor] then
        begin
          if ARci.GLStates.CurrentProgram = 0 then
            EnableClientState(GL_COLOR_ARRAY);
        end;
      end;
    finally
      ARci := storeRci;
      ARci.GLStates.VertexArrayBinding := 0;
    end;
end;

procedure TGLDrawTechniqueOGL3.DrawDynamicBatch(var ARci: TRenderContextInfo;
  const ABatch: TDrawBatch);
var
  LMesh: TFriendlyMesh;
  LInstanceChain: TInstancesChain;
  LMaterial: TGLAbstractLibMaterial;
  glPrimitive: TGLEnum;
  glType: TGLEnum;
  A: TAttribLocation;
  L, LInstanceID, T: Integer;
  LArrayAddress: Pointer;
  storeRci: TRenderContextInfo;
  LCount: Integer;
begin
  if not ABatch.Mesh.IsValid then
    exit;

  LMesh := TFriendlyMesh(ABatch.Mesh);
  if Assigned(ABatch.InstancesChain) and ABatch.InstancesChain.IsValid then
  begin
    LInstanceChain := ABatch.InstancesChain;
    LInstanceID := LInstanceChain.InstanceCount;
  end
  else
  begin
    LInstanceChain := nil;
    LInstanceID := 1;
  end;

  if ARci.GLStates.ForwardContext then
  begin
    FCommonVAO.AllocateHandle;
    FCommonVAO.Bind;
  end
  else
    ARci.GLStates.VertexArrayBinding := 0;
  ARci.GLStates.ArrayBufferBinding := 0;
  ARci.GLStates.ElementBufferBinding := 0;

  with ARci.GLStates do
  begin
    EnablePrimitiveRestart := LMesh.FHasIndices;
    PrimitiveRestartIndex := LMesh.FRestartIndex;
  end;

  if ARci.drawState = dsPicking then
  begin
    if Assigned(ABatch.PickingMaterial) then
      LMaterial := ABatch.PickingMaterial
    else
      LMaterial := vDefaultPickingMaterial;
  end
  else
    LMaterial := ABatch.Material;

  storeRci := ARci;

  if Assigned(ABatch.Transformation) then
    ARci.PipelineTransformation.StackTop := ABatch.Transformation^;
  with GL do
    try
      if Assigned(LMaterial) then
        LMaterial.Apply(ARci);

      if NV_vertex_buffer_unified_memory then
      begin
        ARci.GLStates.ArrayBufferUnified := False;
        ARci.GLStates.ElementBufferUnified := False;
      end;

      if ARci.GLStates.CurrentProgram > 0 then
      begin
        // Setup attribute arrays pointer
        for A := High(TAttribLocation) downto Low(TAttribLocation) do
        begin
          L := Ord(A);
          if LMesh.FAttributes[A] then
          begin
            LArrayAddress := LMesh.FAttributeArrays[A].List;
            EnableVertexAttribArray(L);
            case LMesh.FType[A] of
              GLSLType1F:
                VertexAttribPointer(L, 1, GL_FLOAT, False, 0, LArrayAddress);
              GLSLType2F:
                VertexAttribPointer(L, 2, GL_FLOAT, False, 0, LArrayAddress);
              GLSLType3f:
                VertexAttribPointer(L, 3, GL_FLOAT, False, 0, LArrayAddress);
              GLSLType4F:
                VertexAttribPointer(L, 4, GL_FLOAT, False, 0, LArrayAddress);
              GLSLTypeMat2F:
                VertexAttribPointer(L, 4, GL_FLOAT, False, 0, LArrayAddress);
              GLSLTypeMat3F:
                VertexAttribPointer(L, 9, GL_FLOAT, False, 0, LArrayAddress);
              GLSLTypeMat4F:
                VertexAttribPointer(L, 16, GL_FLOAT, False, 0, LArrayAddress);
              GLSLType1I:
                VertexAttribIPointer(L, 1, GL_INT, 0, LArrayAddress);
              GLSLType2I:
                VertexAttribIPointer(L, 2, GL_INT, 0, LArrayAddress);
              GLSLType3I:
                VertexAttribIPointer(L, 3, GL_INT, 0, LArrayAddress);
              GLSLType4I:
                VertexAttribIPointer(L, 4, GL_UNSIGNED_INT, 0, LArrayAddress);
              GLSLType1UI:
                VertexAttribIPointer(L, 1, GL_UNSIGNED_INT, 0, LArrayAddress);
              GLSLType2UI:
                VertexAttribIPointer(L, 2, GL_UNSIGNED_INT, 0, LArrayAddress);
              GLSLType3UI:
                VertexAttribIPointer(L, 3, GL_UNSIGNED_INT, 0, LArrayAddress);
              GLSLType4UI:
                VertexAttribIPointer(L, 4, GL_UNSIGNED_INT, 0, LArrayAddress);
            else
              Assert(False, glsErrorEx + glsUnknownType);
            end;
          end
          else
            DisableVertexAttribArray(L);
        end;
      end // of Generic attributes
      else
      // Build-in attributes
      begin
        T := 8;
        for A := attrTexCoord7 downto attrTexCoord0 do
        begin
          Dec(T);
          ClientActiveTexture(GL_TEXTURE0 + T);
          if LMesh.FAttributes[A] then
          begin
            EnableClientState(GL_TEXTURE_COORD_ARRAY);
            TexCoordPointer(GLSLTypeComponentCount(LMesh.FType[A]),
              GLSLTypeEnum(LMesh.FType[A]), 0, LMesh.FAttributeArrays[A].List);
          end
          else if LMesh.FAttributes[attrTexCoord0] then
          begin
            // Make first texture coordinates same for other, need for multitexturing
            EnableClientState(GL_TEXTURE_COORD_ARRAY);
            TexCoordPointer(GLSLTypeComponentCount(LMesh.FType[attrTexCoord0]),
              GLSLTypeEnum(LMesh.FType[attrTexCoord0]), 0,
              LMesh.FAttributeArrays[attrTexCoord0].List);
          end
          else
            DisableClientState(GL_TEXTURE_COORD_ARRAY);
        end;
        // Colors
        if LMesh.FAttributes[attrColor] then
        begin
          EnableClientState(GL_COLOR_ARRAY);
          ColorPointer(GLSLTypeComponentCount(LMesh.FType[attrColor]),
            GLSLTypeEnum(LMesh.FType[attrColor]), 0,
            LMesh.FAttributeArrays[attrColor].List);
        end
        else
          DisableClientState(GL_COLOR_ARRAY);
        // Normals
        if LMesh.FAttributes[attrNormal] and
          (GLSLTypeComponentCount(LMesh.FType[attrNormal]) = 3) then
        begin
          EnableClientState(GL_NORMAL_ARRAY);
          NormalPointer(GLSLTypeEnum(LMesh.FType[attrNormal]), 0,
            LMesh.FAttributeArrays[attrNormal].List);
        end
        else
          DisableClientState(GL_NORMAL_ARRAY);
        // Positions
        if LMesh.FAttributes[attrPosition] then
        begin
          EnableClientState(GL_VERTEX_ARRAY);
          VertexPointer(GLSLTypeComponentCount(LMesh.FType[attrPosition]),
            GLSLTypeEnum(LMesh.FType[attrPosition]), 0,
            LMesh.FAttributeArrays[attrPosition].List);
        end
        else
          DisableClientState(GL_VERTEX_ARRAY);
      end; // FFP states

      ARci.PipelineTransformation.LoadMatrices;

      if LMesh.FRestartIndex > $FFFF then
        glType := GL_UNSIGNED_INT
      else
        glType := GL_UNSIGNED_SHORT;
      LArrayAddress := LMesh.FElements.List;
      LCount := LMesh.FElements.Count;

      repeat
        // Primitive selection
        glPrimitive := cPrimitiveType[LMesh.FPrimitive];
        if (ARci.primitiveMask = cAdjacencyPrimitives) and
          not(LMesh.FPrimitive in cAdjacencyPrimitives) then
        begin
          glPrimitive := GL_TRIANGLES_ADJACENCY;
          if LMesh.FHasIndices and (LMesh.FAdjacencyElements.Count > 0) then
          begin
            LArrayAddress := LMesh.FAdjacencyElements.List;
            LCount := LMesh.FAdjacencyElements.Count;
            glType := GL_UNSIGNED_INT;
          end
          else
            continue;
        end
        else if not(LMesh.FPrimitive in ARci.primitiveMask) then
          continue;

        if ARci.drawState = dsPicking then
        begin
          if ARci.GLStates.CurrentProgram = 0 then
          begin
            DisableClientState(GL_COLOR_ARRAY);
            Color3ubv(@ABatch.ListIndex);
          end
          else
            Assert(False);
        end;

        repeat
          Dec(LInstanceID);
          if Assigned(LInstanceChain) then
            ApplyInstance(ARci, LInstanceChain, LInstanceID);

          if LMesh.FHasIndices then
            DrawElements(glPrimitive, LCount, glType, LArrayAddress)
          else
          begin
            MultiDrawArrays(glPrimitive, PGLint(LMesh.FRestartVertex.List),
              PGLsizei(LMesh.FStripCounts.List), LMesh.FRestartVertex.Count);
          end;

        until LInstanceID <= 0;

        if not Assigned(LMaterial) then
          break;
      until not LMaterial.UnApply(ARci);

      // Restore client state
      if (ARci.drawState = dsPicking) and LMesh.FAttributes[attrColor] then
      begin
        if ARci.GLStates.CurrentProgram = 0 then
          EnableClientState(GL_COLOR_ARRAY);
      end;
    finally
      ARci := storeRci;
      ARci.GLStates.VertexArrayBinding := 0;
    end;
end;

{$IFDEF GLS_REGION}{$ENDREGION 'TGLDrawTechniqueOGL3'}{$ENDIF}
{$IFDEF GLS_REGION}{$REGION 'TGLDrawTechniqueOGL4'}{$ENDIF}

procedure TGLDrawTechniqueOGL4.DrawBatch(var ARci: TRenderContextInfo;
  const ABatch: TDrawBatch);
var
  LMesh: TFriendlyMesh;
  LInstanceChain: TInstancesChain;
  LMaterial: TGLAbstractLibMaterial;
  glPrimitive: TGLEnum;
  glType: TGLEnum;
  LShift: PtrUInt;
  LOffset: Pointer;
  LCount, LInstanceID: Integer;
  storeRci: TRenderContextInfo;
begin
  AllocateBuffers;

  LMesh := TFriendlyMesh(ABatch.Mesh);
  if IsDesignTime or (LMesh.FRevisionNum <> LMesh.FBufferRevision) then
  begin
    PlacedInBuffer(LMesh);
    if not LMesh.IsValid then
      exit;
    LMesh.FBufferRevision := LMesh.FRevisionNum;
  end;

  if Assigned(ABatch.InstancesChain) and ABatch.InstancesChain.IsValid then
  begin
    LInstanceChain := ABatch.InstancesChain;
    LInstanceID := LInstanceChain.InstanceCount;
  end
  else
  begin
    LInstanceChain := nil;
    LInstanceID := 1;
  end;

  if ARci.drawState = dsPicking then
  begin
    if Assigned(ABatch.PickingMaterial) then
      LMaterial := ABatch.PickingMaterial
    else
      LMaterial := vDefaultPickingMaterial;
  end
  else
    LMaterial := ABatch.Material;

  storeRci := ARci;

  if Assigned(ABatch.Transformation) then
    ARci.PipelineTransformation.StackTop := ABatch.Transformation^;
  with GL do
    try
      if Assigned(LMaterial) then
        LMaterial.Apply(ARci);

      ARci.PipelineTransformation.LoadMatrices;

      if BindStateHandle(ARci, LMesh) then
      begin

        if LMesh.FRestartIndex > $FFFF then
          glType := GL_UNSIGNED_INT
        else
          glType := GL_UNSIGNED_SHORT;
        LShift := 0;
        LCount := LMesh.FElements.Count;

        repeat
          // Primitive selection
          glPrimitive := cPrimitiveType[LMesh.FPrimitive];
          if ARci.primitiveMask = [mpPATCHES] then
          begin
            glPrimitive := GL_PATCHES;
            if LMesh.FHasIndices and (LMesh.FTrianglesElements.Count > 0) then
            begin
              // Replace triangles to patches
              PatchParameteri(GL_PATCH_VERTICES, 3);
              LShift := LCount;
              LCount := LMesh.FTrianglesElements.Count;
              if glType = GL_UNSIGNED_SHORT then
              begin
                LShift := LShift * SizeOf(TGLUShort);
                RoundTo(LShift, 4);
                glType := GL_UNSIGNED_INT;
              end
              else
                LShift := LShift * SizeOf(TGLuint);
            end
            else if LMesh.FPrimitive <> mpPATCHES then
              continue;
          end
          else if (ARci.primitiveMask = cAdjacencyPrimitives) and
            not(LMesh.FPrimitive in cAdjacencyPrimitives) then
          begin
            glPrimitive := GL_TRIANGLES_ADJACENCY;
            if LMesh.FHasIndices and (LMesh.FAdjacencyElements.Count > 0) then
            begin
              LShift := LCount;
              LCount := LMesh.FAdjacencyElements.Count;
              if glType = GL_UNSIGNED_SHORT then
              begin
                LShift := LShift * SizeOf(TGLUShort);
                RoundTo(LShift, 4);
                glType := GL_UNSIGNED_INT;
              end
              else
                LShift := LShift * SizeOf(TGLuint);
              LShift := LShift + Cardinal(LMesh.FTrianglesElements.Count *
                SizeOf(TGLuint));
            end
            else
              continue;
          end
          else if not(LMesh.FPrimitive in ARci.primitiveMask) then
            continue;

          if IsDesignTime or not LMesh.FHasIndices then
            LOffset := Pointer(LShift)
          else
            LOffset := Pointer(LShift + FElementBufferMap.Sectors
              [LMesh.FElementSectorIndex].Offset);

          if (ARci.drawState = dsPicking) and
            (ARci.GLStates.CurrentProgram = 0) then
          begin
            DisableClientState(GL_COLOR_ARRAY);
            Color3ubv(@ABatch.ListIndex);
          end;

          repeat
            Dec(LInstanceID);
            if Assigned(LInstanceChain) then
              ApplyInstance(ARci, LInstanceChain, LInstanceID);

            if LMesh.FHasIndices then
              DrawElements(glPrimitive, LCount, glType, LOffset)
            else
            begin
              MultiDrawArrays(glPrimitive, PGLint(LMesh.FRestartVertex.List),
                PGLsizei(LMesh.FStripCounts.List), LMesh.FRestartVertex.Count);
            end;

          until LInstanceID <= 0;

          if not Assigned(LMaterial) then
            break;
        until not LMaterial.UnApply(ARci);

        // Restore client state
        if (ARci.drawState = dsPicking) and LMesh.FAttributes[attrColor] then
        begin
          if ARci.GLStates.CurrentProgram = 0 then
            EnableClientState(GL_COLOR_ARRAY);
        end;
      end;
    finally
      ARci := storeRci;
      ARci.GLStates.VertexArrayBinding := 0;
    end;
end;

procedure TGLDrawTechniqueOGL4.DrawDynamicBatch(var ARci: TRenderContextInfo;
  const ABatch: TDrawBatch);
var
  LMesh: TFriendlyMesh;
  LInstanceChain: TInstancesChain;
  LMaterial: TGLAbstractLibMaterial;
  glPrimitive: TGLEnum;
  glType: TGLEnum;
  A: TAttribLocation;
  L, LInstanceID, T: Integer;
  LArrayAddress: Pointer;
  storeRci: TRenderContextInfo;
  LCount: Integer;
begin
  if not ABatch.Mesh.IsValid then
    exit;

  LMesh := TFriendlyMesh(ABatch.Mesh);
  // Choose instances chain
  if Assigned(ABatch.InstancesChain) and ABatch.InstancesChain.IsValid then
  begin
    LInstanceChain := ABatch.InstancesChain;
    LInstanceID := LInstanceChain.InstanceCount;
  end
  else
  begin
    LInstanceChain := nil;
    LInstanceID := 1;
  end;

  // Reset buffers state
  if ARci.GLStates.ForwardContext then
  begin
    FCommonVAO.AllocateHandle;
    FCommonVAO.Bind;
  end
  else
    ARci.GLStates.VertexArrayBinding := 0;
  ARci.GLStates.ArrayBufferBinding := 0;
  ARci.GLStates.ElementBufferBinding := 0;

  with ARci.GLStates do
  begin
    EnablePrimitiveRestart := LMesh.FHasIndices;
    PrimitiveRestartIndex := LMesh.FRestartIndex;
  end;

  if ARci.drawState = dsPicking then
  begin
    if Assigned(ABatch.PickingMaterial) then
      LMaterial := ABatch.PickingMaterial
    else
      LMaterial := vDefaultPickingMaterial;
  end
  else
    LMaterial := ABatch.Material;

  storeRci := ARci;

  if Assigned(ABatch.Transformation) then
    ARci.PipelineTransformation.StackTop := ABatch.Transformation^;
  with GL do
    try
      if Assigned(LMaterial) then
        LMaterial.Apply(ARci);

      // Turn off bindless graphics states
      if NV_vertex_buffer_unified_memory then
      begin
        ARci.GLStates.ArrayBufferUnified := False;
        ARci.GLStates.ElementBufferUnified := False;
      end;

      if ARci.GLStates.CurrentProgram > 0 then
      begin
        // Setup generic attribute arrays
        for A := High(TAttribLocation) downto Low(TAttribLocation) do
        begin
          L := Ord(A);
          if LMesh.FAttributes[A] then
          begin
            LArrayAddress := LMesh.FAttributeArrays[A].List;
            EnableVertexAttribArray(L);
            case LMesh.FType[A] of
              GLSLType1F:
                VertexAttribPointer(L, 1, GL_FLOAT, False, 0, LArrayAddress);
              GLSLType2F:
                VertexAttribPointer(L, 2, GL_FLOAT, False, 0, LArrayAddress);
              GLSLType3f:
                VertexAttribPointer(L, 3, GL_FLOAT, False, 0, LArrayAddress);
              GLSLType4F:
                VertexAttribPointer(L, 4, GL_FLOAT, False, 0, LArrayAddress);
              GLSLTypeMat2F:
                VertexAttribPointer(L, 4, GL_FLOAT, False, 0, LArrayAddress);
              GLSLTypeMat3F:
                VertexAttribPointer(L, 9, GL_FLOAT, False, 0, LArrayAddress);
              GLSLTypeMat4F:
                VertexAttribPointer(L, 16, GL_FLOAT, False, 0, LArrayAddress);
              GLSLType1I:
                VertexAttribIPointer(L, 1, GL_INT, 0, LArrayAddress);
              GLSLType2I:
                VertexAttribIPointer(L, 2, GL_INT, 0, LArrayAddress);
              GLSLType3I:
                VertexAttribIPointer(L, 3, GL_INT, 0, LArrayAddress);
              GLSLType4I:
                VertexAttribIPointer(L, 4, GL_UNSIGNED_INT, 0, LArrayAddress);
              GLSLType1UI:
                VertexAttribIPointer(L, 1, GL_UNSIGNED_INT, 0, LArrayAddress);
              GLSLType2UI:
                VertexAttribIPointer(L, 2, GL_UNSIGNED_INT, 0, LArrayAddress);
              GLSLType3UI:
                VertexAttribIPointer(L, 3, GL_UNSIGNED_INT, 0, LArrayAddress);
              GLSLType4UI:
                VertexAttribIPointer(L, 4, GL_UNSIGNED_INT, 0, LArrayAddress);
            else
              Assert(False, glsErrorEx + glsUnknownType);
            end;
          end
          else
            DisableVertexAttribArray(L);
        end;
      end // of Generic attributes
      else
      // Build-in attributes
      begin
        T := 8;
        for A := attrTexCoord7 downto attrTexCoord0 do
        begin
          Dec(T);
          ClientActiveTexture(GL_TEXTURE0 + T);
          if LMesh.FAttributes[A] then
          begin
            EnableClientState(GL_TEXTURE_COORD_ARRAY);
            TexCoordPointer(GLSLTypeComponentCount(LMesh.FType[A]),
              GLSLTypeEnum(LMesh.FType[A]), 0, LMesh.FAttributeArrays[A].List);
          end
          else if LMesh.FAttributes[attrTexCoord0] then
          begin
            // Make first texture coordinates same for other, need for multitexturing
            EnableClientState(GL_TEXTURE_COORD_ARRAY);
            TexCoordPointer(GLSLTypeComponentCount(LMesh.FType[attrTexCoord0]),
              GLSLTypeEnum(LMesh.FType[attrTexCoord0]), 0,
              LMesh.FAttributeArrays[attrTexCoord0].List);
          end
          else
            DisableClientState(GL_TEXTURE_COORD_ARRAY);
        end;
        // Colors
        if LMesh.FAttributes[attrColor] then
        begin
          EnableClientState(GL_COLOR_ARRAY);
          ColorPointer(GLSLTypeComponentCount(LMesh.FType[attrColor]),
            GLSLTypeEnum(LMesh.FType[attrColor]), 0,
            LMesh.FAttributeArrays[attrColor].List);
        end
        else
          DisableClientState(GL_COLOR_ARRAY);
        // Normals
        if LMesh.FAttributes[attrNormal] and
          (GLSLTypeComponentCount(LMesh.FType[attrNormal]) = 3) then
        begin
          EnableClientState(GL_NORMAL_ARRAY);
          NormalPointer(GLSLTypeEnum(LMesh.FType[attrNormal]), 0,
            LMesh.FAttributeArrays[attrNormal].List);
        end
        else
          DisableClientState(GL_NORMAL_ARRAY);
        // Positions
        if LMesh.FAttributes[attrPosition] then
        begin
          EnableClientState(GL_VERTEX_ARRAY);
          VertexPointer(GLSLTypeComponentCount(LMesh.FType[attrPosition]),
            GLSLTypeEnum(LMesh.FType[attrPosition]), 0,
            LMesh.FAttributeArrays[attrPosition].List);
        end
        else
          DisableClientState(GL_VERTEX_ARRAY);
      end; // Build-in attributes

      ARci.PipelineTransformation.LoadMatrices;

      if LMesh.FRestartIndex > $FFFF then
        glType := GL_UNSIGNED_INT
      else
        glType := GL_UNSIGNED_SHORT;
      LArrayAddress := LMesh.FElements.List;
      LCount := LMesh.FElements.Count;

      repeat
        // Primitive selection
        glPrimitive := cPrimitiveType[LMesh.FPrimitive];
        if ARci.primitiveMask = [mpPATCHES] then
        begin
          glPrimitive := GL_PATCHES;
          if LMesh.FHasIndices and (LMesh.FTrianglesElements.Count > 0) then
          begin
            // Replace triangles to patches
            PatchParameteri(GL_PATCH_VERTICES, 3);
            LArrayAddress := LMesh.FTrianglesElements.List;
            LCount := LMesh.FTrianglesElements.Count;
            glType := GL_UNSIGNED_INT;
          end
          else if LMesh.FPrimitive <> mpPATCHES then
            continue;
        end
        else if (ARci.primitiveMask = cAdjacencyPrimitives) and
          not(LMesh.FPrimitive in cAdjacencyPrimitives) then
        begin
          glPrimitive := GL_TRIANGLES_ADJACENCY;
          if LMesh.FHasIndices and (LMesh.FAdjacencyElements.Count > 0) then
          begin
            LArrayAddress := LMesh.FAdjacencyElements.List;
            LCount := LMesh.FAdjacencyElements.Count;
            glType := GL_UNSIGNED_INT;
          end
          else
            continue;
        end
        else if not(LMesh.FPrimitive in ARci.primitiveMask) then
          continue;

        repeat
          Dec(LInstanceID);
          if Assigned(LInstanceChain) then
            ApplyInstance(ARci, LInstanceChain, LInstanceID);

          if (ARci.drawState = dsPicking) and
            (ARci.GLStates.CurrentProgram = 0) then
          begin
            DisableClientState(GL_COLOR_ARRAY);
            Color3ubv(@ABatch.ListIndex);
          end;

          if LMesh.FHasIndices then
            DrawElements(glPrimitive, LCount, glType, LArrayAddress)
          else
          begin
            MultiDrawArrays(glPrimitive, PGLint(LMesh.FRestartVertex.List),
              PGLsizei(LMesh.FStripCounts.List), LMesh.FRestartVertex.Count);
          end;

        until LInstanceID <= 0;

        if not Assigned(LMaterial) then
          break;
      until not LMaterial.UnApply(ARci);
    finally
      ARci := storeRci;
      ARci.GLStates.VertexArrayBinding := 0;
    end;
end;

{$IFDEF GLS_REGION}{$ENDREGION 'TGLDrawTechniqueOGL4'}{$ENDIF}
{$IFDEF GLS_REGION}{$REGION 'TGLRenderManager'}{$ENDIF}

constructor TGLRenderManager.Create(AOwner: TPersistent);
begin
  inherited;
  FDrawList := TList.Create;
  FDrawList.Capacity := 128;
  FAxesBatch.Mesh := TMeshAtom.Create;
  AxesBuildMesh(FAxesBatch.Mesh, 1000);
end;

destructor TGLRenderManager.Destroy;
begin
  FDrawList.Destroy;
  FAxesBatch.Mesh.Destroy;
  inherited;
end;

procedure TGLRenderManager.DrawAll(var ARci: TRenderContextInfo);
var
  pBatch: PDrawBatch;
  LDrawTech: TGLAbstractDrawTechnique;
  I: Integer;
  LFirst: Boolean;
{$IFDEF GLS_OPENGL_DEBUG}
  LStr: string;
{$ENDIF}
begin
  LDrawTech := GetDrawTechnique;

  if FDrawList.Count > 0 then
  begin
    ARci.PipelineTransformation.Push;

    if ARci.drawState = dsPicking then
    begin
      GetOrCreatePickingMaterial;
      LDrawTech.DoBeforePicking(FDrawList.Count);
    end;

    for I := 0 to FDrawList.Count - 1 do
    begin
      pBatch := FDrawList[I];
      pBatch.ListIndex := I;
{$IFDEF GLS_OPENGL_DEBUG}
      if GL.GREMEDY_string_marker and Assigned(pBatch.Mesh) then
      begin
        LStr := Format('Drawing of mesh "%s"', [pBatch.Mesh.TagName]);
        GL.StringMarkerGREMEDY(Length(LStr), PGLChar(TGLString(LStr)));
      end;
{$ENDIF}
      if Assigned(pBatch^.CustomDraw) then
      begin
        if Assigned(pBatch^.Transformation) then
          ARci.PipelineTransformation.StackTop := pBatch^.Transformation^;
        pBatch^.CustomDraw(ARci);
      end
      else
      begin
        if Assigned(pBatch^.Mesh) and (pBatch^.Mesh.IsValid) then
        begin
          ARci.Mesh := pBatch^.Mesh;
          LDrawTech.DrawBatch(ARci, pBatch^);
        end;
      end;
    end;

    if vBindlessGraphicsEnabled and GL.NV_vertex_buffer_unified_memory then
    begin
      ARci.GLStates.ArrayBufferUnified := False;
      ARci.GLStates.ElementBufferUnified := False;
    end;

    if ARci.drawState = dsPicking then
    begin
      LDrawTech.DoAfterPicking(FDrawList);
    end
    else
    begin

      // Draw AABB
      LFirst := True;
      try
        for I := 0 to FDrawList.Count - 1 do
        begin
          pBatch := FDrawList[I];
          if pBatch^.ShowAABB then
          begin
            if LFirst then
            begin
              LDrawTech.DoBeforeAABBDrawing(ARci);
              LFirst := False;
            end;
            LDrawTech.DrawAABB(ARci, pBatch^);
          end;
        end;
      finally
        if not LFirst then
          LDrawTech.DoAfterAABBDrawing(ARci);
      end;

      // Draw Axes
      LFirst := True;
      try
        for I := 0 to FDrawList.Count - 1 do
        begin
          pBatch := FDrawList[I];
          FAxesBatch.Transformation := pBatch^.Transformation;
          if pBatch^.ShowAxes then
          begin
            if LFirst then
            begin
              GetOrCreateDummyCubeMaterial.Apply(ARci);
              LFirst := False;
            end;
            LDrawTech.DrawBatch(ARci, FAxesBatch);
          end;
        end;
      finally
        if not LFirst then
          GetOrCreateDummyCubeMaterial.UnApply(ARci);
      end;
    end;
    ARci.PipelineTransformation.Pop;
  end;
end;

function TGLRenderManager.GetDrawTechnique: TGLAbstractDrawTechnique;
begin
  if GL.VERSION_4_1 then
  begin
    if vDrawTechniques[3] = nil then
      vDrawTechniques[3] := TGLDrawTechniqueOGL4.Create;
    Result := vDrawTechniques[3];
  end
  else if GL.VERSION_3_3 then
  begin
    if vDrawTechniques[2] = nil then
      vDrawTechniques[2] := TGLDrawTechniqueOGL3.Create;
    Result := vDrawTechniques[2];
  end
  else if GL.VERSION_2_1 then
  begin
    if vDrawTechniques[1] = nil then
      vDrawTechniques[1] := TGLDrawTechniqueOGL2.Create;
    Result := vDrawTechniques[1];
  end
  else
  begin
    if vDrawTechniques[0] = nil then
      vDrawTechniques[0] := TGLDrawTechniqueOGL1.Create;
    Result := vDrawTechniques[0];
  end;
end;

{$IFDEF GLS_REGION}{$ENDREGION 'TGLRenderManager'}{$ENDIF}

initialization

finalization

ReleaseDrawTechniques;

end.
