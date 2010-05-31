unit GL3xMaterial;

interface

{$I GLScene.inc}

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes, SysUtils, Variants,
{$IFNDEF FPC}
  XMLIntf, XMLDoc, XMLDom,
{$ELSE}
  FileUtil, DOM, XMLRead, XMLWrite,
{$ENDIF}
  BaseClasses, OpenGL1x, GLContext, GLRenderContextInfo,
  GLState, GLShadersManager, GLVBOManagers,
  VectorTypes, VectorGeometry, VectorGeometryEXT,
  GLSLShader, GLSLog;

const
  fileMaterialConstructions = 'MaterialConstructions.xml';

type

  // TGL3xMaterial
  //
  TGL3xMaterial = class(TPersistent)
  private
    { Private Declarations }
    FOwner: TComponent;
    FName: string;
    //    FVertObject: string;
    //    FGeomObject: string;
    //    FFragObject: string;
    FProgram: string;
    FChanged: Boolean;
    FObjectsAttached: Boolean;
    FProgramLinked: Boolean;

    FSubVertexCode: AnsiString;
    FSubFragmentCode: AnsiString;
    FMainVertexCode: AnsiString;
    FMainFragmentCode: AnsiString;

    FShader: TGLSLShader;
    FShaderChashed: Boolean;
    procedure SetShader(Value: TGLSLShader);
    procedure SetName(const Value: string);
    procedure SetSubVertexCode(const ACode: AnsiString);
    procedure SetSubFragmentCode(const ACode: AnsiString);
    procedure SetMainVertexCode(const ACode: AnsiString);
    procedure SetMainFragmentCode(const ACode: AnsiString);

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    function GetAttributes(out AttrArray: TGLSLAttributeArray): Boolean;
    procedure Apply(var ARci: TRenderContextInfo);
    function UnApply(var ARci: TRenderContextInfo): Boolean;

    property IsReadyToWork: Boolean read FProgramLinked;

    property SubVertexCode: AnsiString read FSubVertexCode
      write SetSubVertexCode;
    property SubFragmentCode: AnsiString read FSubFragmentCode
      write SetSubFragmentCode;
    property MainVertexCode: AnsiString read FMainVertexCode
      write SetMainVertexCode;
    property MainFragmentCode: AnsiString read FMainFragmentCode
      write SetMainFragmentCode;

  published
    { Published Declarations }
    property Shader: TGLSLShader read FShader write SetShader;
    property Name: string read FName write SetName;
  end;

var
  vDefaultModelMatrix: TMatrix;
  vDefaultViewMatrix: TMatrix;
  vDefaultProjectionMatrix: TMatrix;
  vDefaultLightSourcePosition: TVector;

implementation

uses
  GLStrings, ApplicationFileIO;

{$IFNDEF FPC}
{$R ..\Source\DesignTime\GLSceneMaterialSysVCL.drc}
{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'Default shader'}{$ENDIF}
const
  DefaultShader_vp120: AnsiString =
    '#version 120' + #10#13 +
    'attribute vec3 Position;' + #10#13 +
    'attribute vec3 Normal;' + #10#13 +
    'attribute vec2 TexCoord0;' + #10#13 +
    'varying float diffuse;' + #10#13 +
    'varying vec2 texcoord;' + #10#13 +
    'uniform mat4 ModelMatrix;' + #10#13 +
    'uniform mat4 ViewProjectionMatrix;' + #10#13 +
    'uniform vec4 LightSourcePos;' + #10#13 +
    'void main(void)' + #10#13 +
    '{' + #10#13 +
    '	vec4 vertex    = ModelMatrix * vec4(Position, 1.0);' + #10#13 +
    '	vec4 direction = normalize(LightSourcePos - vertex);' + #10#13 +
    '	vec3 normal = normalize(mat3(ModelMatrix) * Normal);' + #10#13 +
    '	diffuse = clamp(dot(normal, direction.xyz), 0.0, 1.0);' + #10#13 +
    '	texcoord = TexCoord0;' + #10#13 +
    '	gl_Position = ViewProjectionMatrix * vertex;' + #10#13 +
    '}';

  DefaultShader_fp120: AnsiString =
    '#version 120' + #10#13 +
    'varying float diffuse;' + #10#13 +
    'varying vec2 texcoord;' + #10#13 +
    'void main(void)' + #10#13 +
    '{' + #10#13 +
    ' vec2 tc = fract(texcoord);' + #10#13 +
    ' float df = sign(diffuse+0.01);' + #10#13 +
    '	gl_FragColor = vec4(tc.s*df, tc.t*df, 0.0, 1.0);' + #10#13 +
    '}';

  DefaultShader_vp150: AnsiString =
    '#version 150' + #10#13 +
    'in vec3 Position;' + #10#13 +
    'in vec3 Normal;' + #10#13 +
    'in vec2 TexCoord0;' + #10#13 +
    'out float diffuse;' + #10#13 +
    'out vec2 texcoord;' + #10#13 +
    'uniform mat4 ModelMatrix;' + #10#13 +
    'uniform mat4 ViewProjectionMatrix;' + #10#13 +
    'uniform vec4 LightSourcePos;' + #10#13 +
    'void main(void)' + #10#13 +
    '{' + #10#13 +
    '	vec4 vertex    = ModelMatrix * vec4(Position, 1.0);' + #10#13 +
    '	vec4 direction = normalize(LightSourcePos - vertex);' + #10#13 +
    '	vec3 normal = normalize(mat3(ModelMatrix) * Normal);' + #10#13 +
    '	diffuse = clamp(dot(normal, direction.xyz), 0.0, 1.0);' + #10#13 +
    '	texcoord = TexCoord0;' + #10#13 +
    '	gl_Position = ViewProjectionMatrix * vertex;' + #10#13 +
    '}';

  DefaultShader_fp150: AnsiString =
    '#version 150' + #10#13 +
    'in float diffuse;' + #10#13 +
    'in vec2 texcoord;' + #10#13 +
    'out vec4 FragColor;' + #10#13 +
    'void main(void)' + #10#13 +
    '{' + #10#13 +
    ' vec2 tc = fract(texcoord);' + #10#13 +
    ' float df = sign(diffuse+0.01);' + #10#13 +
    '	FragColor = vec4(tc.s*df, tc.t*df, 0.0, 1.0);' + #10#13 +
    '}';
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

type
  TMatSysInitializer = procedure(IsDesignTime: Boolean);

var
  DefaultProgram: string;
  DefaultVertexObject: string;
  DefaultFragmentObject: string;
  DefaultProgramLinked: Boolean;
  InitMaterialSystem: TMatSysInitializer;


{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'Material System Initialization'}{$ENDIF}

procedure InitDefaultShader; forward;

procedure InitCap(IsDesignTime: Boolean);
begin
end;

function GetGLSLVersion: AnsiString;
begin
  Result := '#version 150' + #10#13;
end;

procedure InitDefaultShader;
begin
  with ShadersManager do
  begin
    BeginWork;
    DefaultProgram := MakeUniqueProgramName('DefaultProgram');
    DefaultVertexObject := MakeUniqueObjectName('DefaultVertexObject');
    DefaultFragmentObject :=
      MakeUniqueObjectName('DefaultFragmentObject');
    DefineShaderProgram(DefaultProgram);
    if GL_VERSION_3_2 then
    begin
      DefineShaderObject(DefaultVertexObject, DefaultShader_vp150, [ptVertex]);
      DefineShaderObject(DefaultFragmentObject, DefaultShader_fp150,
        [ptFragment]);
    end
    else
    begin
      DefineShaderObject(DefaultVertexObject, DefaultShader_vp120, [ptVertex]);
      DefineShaderObject(DefaultFragmentObject, DefaultShader_fp120,
        [ptFragment]);
    end;
    AttachShaderObjectToProgram(DefaultVertexObject,
      DefaultProgram);
    AttachShaderObjectToProgram(DefaultFragmentObject,
      DefaultProgram);
    DefaultProgramLinked := LinkShaderProgram(DefaultProgram);
    EndWork;
  end;
end;

procedure MaterialSystemInitializer(IsDesignTime: Boolean);
{$IFNDEF FPC}
var
  sXML: PAnsiString;
  hFind, hRes: THandle;
  vDoc: IXMLDocument;
  XMLMaterial, XMLSubroutines,
    XMLParagraph, XMLSubroutine, XMLOverload: IXMLNode;
  I, J, K: Integer;
  ObjName, ObjType: string;
  ObjCode: AnsiString;
  oType: TGLSLProgramTypes;
  attr: OleVariant;
{$ENDIF}
begin
  InitDefaultShader;
  InitMaterialSystem := InitCap;
{$IFNDEF FPC}
  if IsDesignTime or not FileStreamExists(fileMaterialConstructions) then
  begin
    sXML := nil;
    hFind := FindResource(HInstance, 'MatSys', 'RCData');
    if hFind <> 0 then
    begin
      hRes := LoadResource(HInstance, hFind);
      if hRes <> 0 then
      begin
        sXML := LockResource(hRes);
        if Assigned(sXML) then
        begin
          vDoc := NewXMLDocument();
          vDoc.LoadFromXML(sXML^);
        end;
        UnlockResource(hRes);
      end;
      FreeResource(hFind);
    end;
    if not Assigned(sXML) then
      exit;
  end
  else
  begin
    vDoc := NewXMLDocument();
    vDoc.LoadFromFile(fileMaterialConstructions);
  end;

  try
    XMLMaterial := vDoc.DocumentElement;
    XMLSubroutines := XMLMaterial.ChildNodes.FindNode('Subroutines');
    for I := 0 to XMLSubroutines.ChildNodes.Count - 1 do
    begin
      XMLParagraph := XMLSubroutines.ChildNodes[I];
      for J := 0 to XMLParagraph.ChildNodes.Count - 1 do
      begin
        XMLSubroutine := XMLParagraph.ChildNodes[J];
        attr := XMLSubroutine.Attributes['ObjectType'];
        if not VarIsNull(attr) then
        begin
          ObjType := attr;
          oType := [];
          if Pos('Vertex', ObjType) > 0 then
            Include(oType, ptVertex)
          else if Pos('Fragment', ObjType) > 0 then
            Include(oType, ptFragment)
          else if Pos('Geometry', ObjType) > 0 then
            Include(oType, ptGeometry);
          for K := 0 to XMLSubroutine.ChildNodes.Count - 1 do
            with ShadersManager do
            begin
              BeginWork;
              XMLOverload := XMLSubroutine.ChildNodes[K];
              ObjName := MakeUniqueObjectName(XMLSubroutine.NodeName);
              XMLOverload.Attributes['ObjectName'] := ObjName;
              ObjCode := GetGLSLVersion + AnsiString(XMLOverload.Text);
              DefineShaderObject(ObjName, ObjCode, oType);
              EndWork;
            end;
        end;
      end;
    end;

  except
    GLSLogger.LogFatalError('Bad material constructions file');
  end;
{$ENDIF}
end;
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TGL3xMaterial'}{$ENDIF}
// ------------------
// ------------------ TGL3xMaterial ------------------
// ------------------

constructor TGL3xMaterial.Create(AOwner: TComponent);
begin
  inherited Create;
  FShader := nil;
  FOwner := AOwner;
  FObjectsAttached := False;
  FProgramLinked := False;
  FChanged := False;
  FShaderChashed := False;
end;

destructor TGL3xMaterial.Destroy;
begin
  Shader := nil;
  inherited;
end;

procedure TGL3xMaterial.SetShader(Value: TGLSLShader);
begin
  if FShader <> nil then
    FShader.RemoveFreeNotification(FOwner);
  FShader := Value;
  if FShader <> nil then
    FShader.FreeNotification(FOwner);
  FShaderChashed := False;
end;

procedure TGL3xMaterial.SetName(const Value: string);
begin
  if Value <> FName then
  begin
    FName := Value;
  end;
end;

procedure TGL3xMaterial.SetSubVertexCode(const ACode: AnsiString);
begin
  FSubVertexCode := ACode;
  FChanged := True;
end;

procedure TGL3xMaterial.SetSubFragmentCode(const ACode: AnsiString);
begin
  FSubFragmentCode := ACode;
  FChanged := True;
end;

procedure TGL3xMaterial.SetMainVertexCode(const ACode: AnsiString);
begin
  FMainVertexCode := ACode;
  FChanged := True;
end;

procedure TGL3xMaterial.SetMainFragmentCode(const ACode: AnsiString);
begin
  FMainFragmentCode := ACode;
  FChanged := True;
end;

function TGL3xMaterial.GetAttributes(out AttrArray: TGLSLAttributeArray):
  Boolean;
var
  i, j: Integer;
  Attribs: TGLActiveAttribArray;
begin
  Result := false;
  if FShader = nil then
    exit;
  Attribs := FShader.GetActiveAttribs;
  j := 0;
  for i := 0 to GLS_VERTEX_ATTR_NUM - 1 do
    if (i <= High(Attribs)) then
    begin
      AttrArray[I] := TGLSLAttribute.GetAttribute(Attribs[i].Name);
      Inc(j);
    end
    else
      AttrArray[I] := nil;
  Result := (j > 0);
end;

procedure TGL3xMaterial.Apply(var ARci: TRenderContextInfo);
var
  IsDesignTime: Boolean;
begin
  IsDesignTime := csDesigning in FOwner.ComponentState;
  InitMaterialSystem(IsDesignTime);

  if Assigned(FShader) and not IsDesignTime then
  begin
    // Use GLSL Shader Component
    FShader.Apply(ARci, Self);
    if not FShaderChashed then
    with ShadersManager do
    begin
      BeginWork;
      CashLocationsOfGLSLShader;
      EndWork;
      FShaderChashed := True;
    end;
  end
  else if FProgramLinked then
    // Use Main Material Shader
    ShadersManager.UseProgram(FProgram)
  else
  begin
    // Use Default Shader
    with ShadersManager do
      if DefaultProgramLinked then
      begin
        UseProgram(DefaultProgram);
        UniformMat4f(uniformModelMatrix, vDefaultModelMatrix);
        UniformMat4f(uniformViewProjectionMatrix,
          MatrixMultiply(vDefaultViewMatrix, vDefaultProjectionMatrix));
        Uniform4f(uniformLightSourcePos, vDefaultLightSourcePosition);
      end;
  end;
end;

function TGL3xMaterial.UnApply(var ARci: TRenderContextInfo): Boolean;
begin
  if Assigned(FShader) and not (csDesigning in FOwner.ComponentState) then
  begin
    Result := FShader.UnApply(ARci);
  end
  else
  begin
    Result := False;
    if not ARci.GLStates.ForwardContext then
      ShadersManager.UseFixedFunctionPipeline;
  end;
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

initialization

  InitMaterialSystem := MaterialSystemInitializer;
  vDefaultModelMatrix := IdentityHmgMatrix;
  vDefaultViewMatrix := IdentityHmgMatrix;
  vDefaultProjectionMatrix := IdentityHmgMatrix;
  vDefaultLightSourcePosition := VectorMake(10, 10, 10, 1);

end.

