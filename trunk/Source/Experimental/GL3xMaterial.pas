//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GL3xMaterial<p>

   <b>History : </b><font size=-1><ul>
    <li>07/07/10 - Yar - Creation
 </ul></font>
}

// TODO Improve uniform managment

unit GL3xMaterial;

interface

{$I GLScene.inc}

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes,
  SysUtils,
  Variants,
  Forms,
{$IFDEF FPC}
  FileUtil,
  XMLRead,
  XMLWrite,
  LResources,
{$ENDIF}
  GLSCrossXML,
  GLCrossPlatform,
  ApplicationFileIO,
  BaseClasses,
  OpenGLTokens,
  GLContext,
  GLRenderContextInfo,
  GLState,
  GLShaderManager,
  VectorTypes,
  VectorGeometry,
  VectorGeometryEXT,
  GL3xTexture,
  GLSpecializedUniforms,
  GLSRedBlackTree;

const
  fileMaterialSystem = 'MaterialSystem.xml';

type

  TGL3xMaterialName = string;

  // TFaceCulling
  //
  TFaceCulling =
    (
    fcBufferDefault,
    fcCull,
    fcNoCull
    );

  // TBlendingMode
  //
  TBlendingMode =
    (
    bmOpaque,
    bmTransparency,
    bmAdditive,
    bmMasked,
    bmModulate,
    bmCustom
    );

  // TGL3xMaterial
  //

  TGL3xMaterial = class(TDataFile)
  private
    { Private Declarations }
    FSourceDoc: GLSXMLDocument;
    FName: string;
    FHashCode: Integer;
    FVertObject: string;
    FGeomObject: string;
    FFragObject: string;
    FProgram: string;
    FLoaded: Boolean;
    FProgramLinked: Boolean;
    FFaceCulling: TFaceCulling;
    FBlendingMode: TBlendingMode;
    FPolygonMode: TPolygonMode;
    FTextureUnits: TStringList;

    procedure SetName(const AName: string);
  protected
    FSampleList: TList;
    FUniforms: array of TBaseSpecialUniform;
    procedure CreateSamples;
    procedure CreateUniforms;
    procedure ClearSamples;
    procedure ClearUniforms;
    procedure CreateShader(const VertexMain, FragmentMain: AnsiString);
    procedure Default;

    property Document: GLSXMLDocument read FSourceDoc write FSourceDoc;
    property SampleList: TList read FSampleList;
    property BlendingMode: TBlendingMode read FBlendingMode write FBlendingMode;
    property FaceCulling: TFaceCulling read FFaceCulling write FFaceCulling;
    property PolygonMode: TPolygonMode read FPolygonMode write FPolygonMode;
    property TextureUnits: TStringList read FTextureUnits;
  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(const AName: string); reintroduce;
    procedure AssignFrom(const AName: string);

    procedure LoadFromFile(const fileName: string); override;
    procedure SaveToFile(const fileName: string); override;
    procedure LoadFromStream(stream: TStream); override;
    procedure SaveToStream(stream: TStream); override;
    procedure Initialize; override;
    class function Capabilities: TDataFileCapabilities; override;
    //    function GetAttributes(out AttrArray: TGLSLAttributeArray): Boolean;
    procedure Apply;
    function UnApply: Boolean;

    property Name: string read FName write SetName;
    property HashCode: Integer read FHashCode;
    property IsReadyToWork: Boolean read FProgramLinked;
  end;

  TShaderSamplePurpose = (sspOperation, sspConstant, sspVariable);
  {: Simple shader element, which is a construction shader programs. }
  PShaderSample = ^TShaderSample;
  TShaderSample = {$IFNDEF FPC}record {$ELSE}object{$ENDIF}
  private
    LocalID: Word;
    Processed: Boolean;
    FInputRef: array[0..3] of PShaderSample;
    FTextureInIntput: Boolean;
    FObjectName: string;
    FInputFromMatSys: string;
    function GetInputRef(I: Integer): PShaderSample;
    procedure SetInputRef(I: Integer; Value: PShaderSample);
    function GetDeclaration: AnsiString;
  public
    Category: AnsiString;
      Name: AnsiString;
    Participate: TGLSLProgramTypes;
    Purpose: TShaderSamplePurpose;
    Input: array[0..3] of TGLSLDataType;
    Output: TGLSLDataType;
    UniformClasses: array[0..7] of TSpecialUniformClass;
    ConstantValue: string;
    Mask: TColorComponentMask;
    procedure Clear;
    property Declaration: AnsiString read GetDeclaration;
    function MaskAsString: string;
    function InputAsString: string;
    function IsCanProcessed: Boolean;
    function FindShaderObject: Boolean;
    function TotalOutput: TGLSLDataType;
    property TextureInIntput: Boolean read FTextureInIntput write FTextureInIntput;
    property InputRef[Index: Integer]: PShaderSample read GetInputRef write SetInputRef;
  end;

  TMaterialTree =
{$IFDEF FPC}specialize{$ENDIF}GRedBlackTree < Integer, TGL3xMaterial > ;
  TTextureTree =
{$IFDEF FPC}specialize{$ENDIF}GRedBlackTree < Integer, TGL3xTexture > ;

  TGLSMaterialManager = class(TPersistent)
  private
    FInitialized: Boolean;
    FMatTreeChanged: Boolean;
    FTexTreeChanged: Boolean;
    FMatSysDoc: GLSXMLDocument;
    FMaterialTree: TMaterialTree;
    FTextureTree: TTextureTree;
    FLastMaterial: TGL3xMaterial;
    FStructures: AnsiString;
    FLightsBuffer: TGLUniformBufferHandle;
    FLightsBlock: TGLSLUniformBlock;
    procedure LoadMaterialSystem;
    function GetMaterial(const AName: TGL3xMaterialName): TGL3xMaterial;
    procedure TreeChanged(Sender: TObject);
    procedure LightsChanged(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Initialize;
    function FindCompatibleSample(var ASample: TShaderSample): Boolean;
    function GenerateShaderCode(const SampleList: TList; var vertexProgram, fragmentProgram: TStringList): Boolean;
    procedure FillMaterialNameList(var AList: TStringList);
    procedure FillTextureNameList(var AList: TStringList);

    function CreateMaterial(const AName, AFileName: string): Boolean;
    procedure ApplyMaterial(const AName: TGL3xMaterialName);
    function UnApplyMaterial: Boolean;
    procedure ApplyTexture(const AName: TGL3xTextureName; UnitIndex: TGLuint);
    procedure BindLightsBlock;

    function IsMaterialExist(const AName: TGL3xMaterialName): Boolean;
  end;

function MaterialManager: TGLSMaterialManager;

implementation

uses
  GLSLog,
  GLStrings,
  GL3xMaterialConst,
  GLFileDDS;

const

  cFaceCulling: array[TFaceCulling] of string =
    (
    'BufferDefault',
    'Cull',
    'NoCull'
    );

  cPolygonMode: array[TPolygonMode] of string =
    (
    'Fill',
    'Lines',
    'Points'
    );

  cBlendingMode: array[TBlendingMode] of string =
    (
    'Opaque',
    'Transparency',
    'Additive',
    'Masked',
    'Modulate',
    'Custom'
    );
resourcestring
  sBadMaterialConstr = 'Bad material constructions file';

{$IFNDEF FPC}
{$R ..\Source\Experimental\GLSceneMaterialSysVCL.dcr}
{$ENDIF}

var
  vMaterialManager: TGLSMaterialManager;

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TGLSMaterialManager'}{$ENDIF}
  // ------------------
  // ------------------ TGLSMaterialManager ------------------
  // ------------------

function MaterialManager: TGLSMaterialManager;
begin
  Result := vMaterialManager;
end;

function CompareMaterial(const Item1, Item2: TGL3xMaterial): Integer;
begin
  if Length(Item1.Name) < Length(Item2.Name) then
  begin
    Result := -1;
  end
  else if (Item1.Name = Item2.Name) then
  begin
    Result := 0;
  end
  else
  begin
    Result := 1;
  end
end;

function CompareTexture(const Item1, Item2: TGL3xTexture): Integer;
begin
  if Length(Item1.Name) < Length(Item2.Name) then
  begin
    Result := -1;
  end
  else if (Item1.Name = Item2.Name) then
  begin
    Result := 0;
  end
  else
  begin
    Result := 1;
  end
end;

procedure MaterialInitializer(
  k: Integer; AMaterial: TGL3xMaterial; out Flag: Boolean);
begin
  AMaterial.Initialize;
  Flag := True;
end;

procedure TextureInitializer(
  k: Integer; ATexture: TGL3xTexture; out Flag: Boolean);
begin
  ATexture.Initialize;
  Flag := True;
end;

procedure MaterialDestroyer(
  k: Integer; AMaterial: TGL3xMaterial; out Flag: Boolean);
begin
  AMaterial.Destroy;
  Flag := True;
end;

procedure TextureDestroyer(
  k: Integer; ATexture: TGL3xTexture; out Flag: Boolean);
begin
  ATexture.Destroy;
  Flag := True;
end;

var
  vNameList: TStringList;

procedure GetherMaterialName(
  k: Integer; AMaterial: TGL3xMaterial; out Flag: Boolean);
begin
  vNameList.Add(AMaterial.Name);
  Flag := True;
end;

procedure GetherTextureName(
  k: Integer; ATexture: TGL3xTexture; out Flag: Boolean);
begin
  vNameList.Add(ATexture.Name);
  Flag := True;
end;

constructor TGLSMaterialManager.Create;
var
  defMat: TGL3xMaterial;
  defTex, normTex: TGL3xTexture;
  rStream: TGLSResourceStream;
begin
  inherited;
  FMaterialTree := TMaterialTree.Create(CompareInteger, CompareMaterial);
  FTextureTree := TTextureTree.Create(CompareInteger, CompareTexture);
  FMaterialTree.OnChange := TreeChanged;
  FTextureTree.OnChange := TreeChanged;
  LoadMaterialSystem;
  FInitialized := False;
  FMatTreeChanged := True;

  // Make default material
  defMat := TGL3xMaterial.Create(Self);
  defMat.Name := glsDEFAULTMATERIALNAME;
  defMat.Default;
  FMaterialTree.Add(defMat.FHashCode, defMat);

  // Make default diffuse texture
  defTex := TGL3xTexture.Create(Self);
  defTex.Name := glsDIFFUSEMAP;
  rStream := CreateResourceStream(glsDIFFUSEMAP, GLS_RC_JPG_Type);
  defTex.LoadFromStream(rStream);
  rStream.Free;
  FTextureTree.Add(defTex.HashCode, defTex);

  // Make default normal texture
  normTex := TGL3xTexture.Create(Self);
  normTex.Name := glsNORMALMAP;
  rStream := CreateResourceStream(glsNORMALMAP, GLS_RC_JPG_Type);
  normTex.LoadFromStream(rStream);
  rStream.Free;
  FTextureTree.Add(normTex.HashCode, normTex);

  FLightsBuffer := TGLUniformBufferHandle.Create;
  FLightsBlock := TGLSLUniformBlock.RegisterUniformBlock('LightsBlock');
end;

destructor TGLSMaterialManager.Destroy;
begin
  FMaterialTree.ForEach(MaterialDestroyer);
  FMaterialTree.Destroy;
  FTextureTree.ForEach(TextureDestroyer);
  FTextureTree.Destroy;
  FLightsBuffer.Destroy;
  inherited;
end;

procedure TGLSMaterialManager.LoadMaterialSystem;
var
  rStream: TGLSResourceStream;
begin
  if (csDesigning in Application.ComponentState)
    or not FileStreamExists(fileMaterialSystem) then
  begin
    rStream := nil;
    try
      rStream := CreateResourceStream('MaterialSystem', GLS_RC_XML_Type);
{$IFNDEF FPC}
      FMatSysDoc := GLSNewXMLDocument;
      FMatSysDoc.LoadFromStream(rStream);
{$ELSE}
      ReadXMLFile(FMatSysDoc, rStream);
{$ENDIF}
    finally
      rStream.Free;
    end;
    GLSLogger.LogNotice('Material system loaded from application resource');
  end
  else
  begin
{$IFNDEF FPC}
    FMatSysDoc := GLSNewXMLDocument;
    FMatSysDoc.LoadFromFile(fileMaterialSystem);
{$ELSE}
    ReadXMLFile(FMatSysDoc, fileMaterialSystem);
    if FMatSysDoc = nil then
      exit;
{$ENDIF}
    GLSLogger.LogNotice('Material system loaded from file ' + fileMaterialSystem);
  end;
end;

procedure TGLSMaterialManager.Initialize;
var
  RC: TGLContext;
  XMLMaterial, XMLSamples, XMLParagraph, XMLSample, XMLOverload: GLSXMLNode;
  I, J, K, P: Integer;
  Version: Char;
  sCategory: AnsiString;
  ObjName, ObjType, temp, aInput, aOutput: string;
  Param: AnsiChar;
  ObjCode: AnsiString;
  oType: TGLSLProgramTypes;
begin
  RC := SafeCurrentGLContext;
  RC.GLStates.OnLightsChanged := LightsChanged;
  {: Create shader objects from material system database. }
  if FInitialized then
    exit;
  try
    if FMatSysDoc = nil then
      Abort;
    Version := '0';
    if GL.VERSION_4_0 then
      Version := '4'
    else if GL.VERSION_3_0 then
      Version := '3'
    else if GL.VERSION_2_0 then
      Version := '2'
    else
      Abort;
    ShaderManager.BeginWork;

    XMLMaterial := FMatSysDoc.DocumentElement;
    if FindXMLNode(XMLMaterial, 'Samples', XMLSamples) then
    begin
      GetXMLAttribute(XMLSamples, 'Structures', temp);
      FStructures := AnsiString(temp);
      for I := 0 to XMLSamples.ChildNodes.Count - 1 do
      begin
        XMLParagraph := XMLSamples.ChildNodes[I];
        sCategory := AnsiString(XMLParagraph.NodeName);
        if sCategory[1] = '#' then
          continue;
        for J := 0 to XMLParagraph.ChildNodes.Count - 1 do
        begin
          XMLSample := XMLParagraph.ChildNodes[J];
          if GetXMLAttribute(XMLSample, 'ObjectType', ObjType) then
          begin
            oType := [];
            if Pos('V', ObjType) > 0 then
              Include(oType, ptVertex);
            if Pos('F', ObjType) > 0 then
              Include(oType, ptFragment);
            if Pos('G', ObjType) > 0 then
              Include(oType, ptGeometry);
            if Pos('C', ObjType) > 0 then
              Include(oType, ptControl);
            if Pos('E', ObjType) > 0 then
              Include(oType, ptEvaluation);
            for K := 0 to XMLSample.ChildNodes.Count - 1 do
            begin
              XMLOverload := XMLSample.ChildNodes[K];
              if GetXMLAttribute(XMLOverload, 'Version', temp) then
              begin
                if Pos(Version, temp) = 0 then
                  continue;
              end;

              if GetXMLAttribute(XMLOverload, 'Const', temp) then
                continue;
              if GetXMLAttribute(XMLOverload, 'Mask', temp) then
                continue;

              if GetXMLText(XMLOverload, temp) then
              begin
                ObjName := ShaderManager.MakeUniqueObjectName(XMLSample.NodeName);
                SetXMLAttribute(XMLOverload, 'ObjectName', ObjName);
                ObjCode := GetMaxGLSLVersion + #10#13 + 'precision highp float;' + #10#13;
                if (sCategory = MaterialSystem.Vertex.Name)
                  or (sCategory = MaterialSystem.Geometry.Name) then
                  ObjCode := ObjCode + FStructures + #10#13;

                P := Pos('#', temp);
                if P > 1 then
                  ObjCode := ObjCode +
                    AnsiString(Copy(temp, 1, P - 3)) + #10#13;
                Delete(temp, 1, P);
                GetXMLAttribute(XMLOverload, 'Output', aOutput);
                ObjCode := ObjCode + AnsiString(aOutput) + ' ';
                ObjCode := ObjCode + AnsiString(XMLSample.NodeName);
                GetXMLAttribute(XMLOverload, 'Input', aInput);

                if aInput = 'void;' then
                  ObjCode := ObjCode + '() ' + #10#13
                else
                begin
                  ObjCode := ObjCode + '(';
                  Param := 'A';
                  repeat
                    P := Pos(';', aInput);
                    if P = 0 then
                      break;
                    ObjCode := ObjCode +
                      AnsiString(Copy(aInput, 1, P - 1)) + ' ' + Param + ',';
                    Delete(aInput, 1, P);
                    Inc(Param);
                  until Length(aInput) = 0;
                  ObjCode[Length(ObjCode)] := ')';
                  ObjCode := ObjCode + ' ' + #10#13;
                end;
                ObjCode := ObjCode + AnsiString(temp);
                ShaderManager.DefineShaderObject(ObjName, ObjCode, oType);
              end;

            end;
          end;
        end;
      end;
    end
    else
      Abort;

    FInitialized := True;
    ShaderManager.EndWork;
    GLSLogger.LogNotice('Material system intialized');
    FMaterialTree.ForEach(MaterialInitializer);
    FTextureTree.ForEach(TextureInitializer);
  except
    ShaderManager.EndWork;
    GLSLogger.LogFatalError(sBadMaterialConstr);
  end;
end;

function TGLSMaterialManager.FindCompatibleSample(var ASample: TShaderSample): Boolean;
var
  XMLMaterial, XMLSamples, XMLParagraph, XMLSample, XMLOverload: GLSXMLNode;
  temp: string;
var
  sInputs, aInputs, aOutput, ObjType, aUniforms, sUniforms: string;
  I, U, P: Integer;

  procedure CaseOutput;
  begin
    if ASample.Mask = [] then
    begin
      aOutput := temp;
      if aOutput = 'void' then
        ASample.Output := GLSLTypeVoid
      else if aOutput = 'float' then
        ASample.Output := GLSLType1F
      else if aOutput = 'vec2' then
        ASample.Output := GLSLType2F
      else if aOutput = 'vec3' then
        ASample.Output := GLSLType3F
      else if aOutput = 'vec4' then
        ASample.Output := GLSLType4F
      else if aOutput = 'int' then
        ASample.Output := GLSLType1I
      else if aOutput = 'ivec2' then
        ASample.Output := GLSLType2I
      else if aOutput = 'ivec3' then
        ASample.Output := GLSLType3I
      else if aOutput = 'ivec4' then
        ASample.Output := GLSLType4I;
    end
    else
      ASample.Output := MaskToGLSLType(ASample.Mask);
  end;

begin
  if ASample.Purpose = sspVariable then
    exit(True);

  ASample.Output := GLSLTypeUndefined;
  ASample.Participate := [];
  Result := False;

  if Assigned(FMatSysDoc)
    and (Length(ASample.Category) > 0)
    and (Length(ASample.Name) > 0) then
  begin

    sInputs := ASample.InputAsString;
    try
      XMLMaterial := FMatSysDoc.DocumentElement;
      FindXMLNode(XMLMaterial, 'Samples', XMLSamples);
      if FindXMLNode(XMLSamples, string(ASample.Category), XMLParagraph) then
      begin
        if FindXMLNode(XMLParagraph, string(ASample.Name), XMLSample) then
        begin
          if GetXMLAttribute(XMLSample, 'ObjectType', ObjType) then
          begin
            ASample.Participate := [];
            if Pos('V', ObjType) > 0 then
              Include(ASample.Participate, ptVertex);
            if Pos('F', ObjType) > 0 then
              Include(ASample.Participate, ptFragment);
            if Pos('G', ObjType) > 0 then
              Include(ASample.Participate, ptGeometry);
            if Pos('C', ObjType) > 0 then
              Include(ASample.Participate, ptControl);
            if Pos('E', ObjType) > 0 then
              Include(ASample.Participate, ptEvaluation);
          end;

          for I := 0 to XMLSample.ChildNodes.Count - 1 do
          begin
            XMLOverload := XMLSample.ChildNodes[I];

            if ASample.Purpose = sspConstant then
            begin
              if GetXMLAttribute(XMLOverload, 'Const', temp) then
              begin
                CaseOutput;
                Result := True;
                break;
              end
              else
                continue;
            end;

            if GetXMLAttribute(XMLOverload, 'Mask', temp) then
            begin
              ASample.Output := MaskToGLSLType(ASample.Mask);
              Result := True;
              break;
            end;

            if not GetXMLAttribute(XMLOverload, 'Input', aInputs) then
              Abort;

            if sInputs = RemoveGLSLQualifier(aInputs) then
            begin
              if not GetXMLAttribute(XMLOverload, 'ObjectName', ASample.FObjectName) then
                continue;

              if GetXMLAttribute(XMLOverload, 'Output', temp) then
                CaseOutput
              else
                Abort;

              if GetXMLAttribute(XMLOverload, 'Uniforms', aUniforms) then
              begin
                U := 0;
                repeat
                  P := Pos(';', aUniforms);
                  if P > 0 then
                  begin
                    sUniforms := Copy(aUniforms, 0, P - 1);
                    Delete(aUniforms, 1, P);
                    ASample.UniformClasses[U] := TSpecialUniformClass(FindClass(sUniforms));
                    Inc(U);
                  end
                  else
                    break;
                until Length(aUniforms) = 0;
              end;

              Result := True;
              break;
            end;

          end;
        end;
      end;
    except
      GLSLogger.LogFatalError(sBadMaterialConstr);
    end;
  end;
end;

function TGLSMaterialManager.GenerateShaderCode(const SampleList: TList; var vertexProgram, fragmentProgram: TStringList): Boolean;
const
  sLocal: AnsiString = 'Local';
var
  I, ProcessedCount: Integer;
  LocalCount: Word;
  pSample: PShaderSample;
  vertexHeader, fragmentHeader,
    vertexMain, fragmentMain: TStringList;
  line, dline, CurrLocal: AnsiString;
  withParam: Boolean;

  function NextLocal: AnsiString;
  begin
    Result := sLocal + AnsiString(IntToStr(LocalCount));
    pSample.LocalID := LocalCount;
    Inc(LocalCount);
  end;

begin
  // Delete garbage
  for I := 0 to SampleList.Count - 1 do
  begin
    pSample := SampleList[I];
    pSample.Processed := False;
    if (pSample.Participate = []) or (pSample.Output = GLSLTypeUndefined) then
    begin
      GLSLogger.LogInfo('Deleted unused sample ' + string(pSample.Name));
      Dispose(pSample);
      SampleList[I] := nil;
    end;
  end;
  SampleList.Pack;

  if not Assigned(vertexProgram) then
    vertexProgram := TStringList.Create
  else
    vertexProgram.Clear;
  if not Assigned(fragmentProgram) then
    fragmentProgram := TStringList.Create
  else
    fragmentProgram.Clear;

  vertexHeader := TStringList.Create;
  vertexHeader.Add('// VERTEX SHADER');
  vertexHeader.Add('');
  vertexHeader.Add(string(GetMaxGLSLVersion));
  vertexHeader.Add('precision highp float;');
  vertexHeader.Add('');
  vertexHeader.Add(string(FStructures));

  vertexMain := TStringList.Create;
  vertexMain.Add('void main()');
  vertexMain.Add('{');

  fragmentHeader := TStringList.Create;
  fragmentHeader.Add('// FRAGMENT SHADER');
  fragmentHeader.Add('');
  fragmentHeader.Add(string(GetMaxGLSLVersion));
  fragmentHeader.Add('precision highp float;');
  fragmentHeader.Add('');

  fragmentMain := TStringList.Create;
  fragmentMain.Add('void main()');
  fragmentMain.Add('{');

  pSample := nil;
  LocalCount := 0;
  ProcessedCount := 0;

  try
    if SampleList.Count = 0 then
      Abort;

    repeat
      if not Assigned(pSample) then
        for I := 0 to SampleList.Count - 1 do
        begin
          pSample := SampleList[I];
          if not pSample.Processed then
            break;
        end;

      if pSample.IsCanProcessed then
      begin
        case pSample.Purpose of
          sspOperation:
            begin
              if pSample.Name = MaterialSystem.Utility.Utility_ComponentMask then
              begin
                CurrLocal := sLocal + AnsiString(IntToStr(pSample.FInputRef[0].LocalID));
                line := GLSLTypeToString(MaskToGLSLType(pSample.Mask)) + ' ';
                line := line + NextLocal + ' = ';
                line := line + GetGLSLTypeCast(CurrLocal, pSample.FInputRef[0].Output, pSample.Mask) + ';';
              end
              else
              begin
                if pSample.Output <> GLSLTypeVoid then
                begin
                  line := GLSLTypeToString(pSample.Output) + ' ';
                  CurrLocal := NextLocal;
                  line := line + CurrLocal + ' = ';
                end
                else
                  line := '';
                line := line + pSample.Name + '(';
                withParam := False;
                for I := 0 to High(pSample.FInputRef) do
                  if Assigned(pSample.FInputRef[I]) then
                  begin
                    line := line + sLocal + AnsiString(IntToStr(pSample.FInputRef[I].LocalID)) + ', ';
                    withParam := True;
                  end;
                if withParam then
                begin
                  line[Length(line) - 1] := ')';
                  line[Length(line)] := ';';
                end
                else
                  line := line + ');';
                if pSample.Mask <> [] then
                begin
                  line := line + ' ' + GLSLTypeToString(MaskToGLSLType(pSample.Mask)) + ' ';
                  line := line + NextLocal + ' = ';
                  line := line + GetGLSLTypeCast(CurrLocal, pSample.Output, pSample.Mask) + ';';
                end;
              end;
            end;
          sspConstant:
            begin
              if pSample.Mask = [] then
              begin
                line :=
                  'const ' +
                  GLSLTypeToString(pSample.Output) + ' ' +
                  NextLocal + ' = ' +
                  HexToGLSL(pSample.Output, pSample.ConstantValue) + ';';
              end
              else
              begin
                line :=
                  'const ' +
                  GLSLTypeToString(MaskToGLSLType(pSample.Mask)) + ' ' +
                  NextLocal + ' = ' +
                  HexToGLSL(pSample.Output, pSample.ConstantValue) + ';';
              end;
            end;
          sspVariable:
            begin
              line := GLSLTypeToString(pSample.Output) + ' ' +
                NextLocal + ';';
            end;

        end;
        // Add to vertex program
        if ptVertex in pSample.Participate then
        begin
          if pSample.Purpose = sspOperation then
          begin
            dline := pSample.Declaration;
            if vertexHeader.IndexOf(string(dline)) < 0 then
              vertexHeader.Add(string(dline));
          end;
          vertexMain.Add('  ' + string(line));
        end;
        // Add to fragment program
        if ptFragment in pSample.Participate then
        begin
          if pSample.Purpose = sspOperation then
          begin
            dline := pSample.Declaration;
            if fragmentHeader.IndexOf(string(dline)) < 0 then
              fragmentHeader.Add(string(dline));
          end;
          fragmentMain.Add('  ' + string(line));
        end;

        pSample.Processed := True;
        Inc(ProcessedCount);
        pSample := nil;
      end
      else // input samples are not ready
      begin
        for I := 0 to High(pSample.FInputRef) do
          if Assigned(pSample.FInputRef[I])
            and not pSample.FInputRef[I].Processed then
          begin
            pSample := pSample.FInputRef[I];
            break;
          end;
      end;
    until ProcessedCount = SampleList.Count;

    vertexMain.Add('}');
    fragmentMain.Add('}');
    vertexProgram.AddStrings(vertexHeader);
    vertexProgram.AddStrings(vertexMain);

    fragmentProgram.AddStrings(fragmentHeader);
    fragmentProgram.AddStrings(fragmentMain);

    vertexHeader.Free;
    vertexMain.Free;
    fragmentHeader.Free;
    fragmentMain.Free;
    Result := True;

  except
    vertexHeader.Free;
    vertexMain.Free;
    fragmentHeader.Free;
    fragmentMain.Free;
    Result := False;
  end;

end;

function TGLSMaterialManager.GetMaterial(const AName: TGL3xMaterialName): TGL3xMaterial;
var
  i, n, hash: Integer;
begin
  n := Length(AName);
  hash := n;
  for i := 1 to n do
    hash := (hash shl 1) + Byte(AName[i]);
  if FMaterialTree.Find(hash, Result) then
    exit;
  if AName <> glsDEFAULTMATERIALNAME then
    Result := GetMaterial(glsDEFAULTMATERIALNAME)
  else
  begin
    GLSLogger.LogError('Material not found');
    Abort;
  end;
end;

function TGLSMaterialManager.IsMaterialExist(const AName: TGL3xMaterialName): Boolean;
begin
  if AName <> glsDEFAULTMATERIALNAME then
    Result := GetMaterial(AName).Name <> glsDEFAULTMATERIALNAME
  else
    Result := True;
end;

procedure TGLSMaterialManager.FillMaterialNameList(var AList: TStringList);
begin
  if not Assigned(AList) then
  begin
    AList := TStringList.Create;
    FMatTreeChanged := True;
  end;
  if FMatTreeChanged then
  begin
    vNameList := AList;
    AList.Clear;
    FMaterialTree.ForEach(GetherMaterialName);
    FMatTreeChanged := False;
  end;
end;

procedure TGLSMaterialManager.FillTextureNameList(var AList: TStringList);
begin
  if not Assigned(AList) then
  begin
    AList := TStringList.Create;
    FTexTreeChanged := True;
  end;
  if FTexTreeChanged then
  begin
    vNameList := AList;
    AList.Clear;
    FTextureTree.ForEach(GetherTextureName);
    FTexTreeChanged := False;
  end;
end;

function TGLSMaterialManager.CreateMaterial(const AName, AFileName: string): Boolean;
var
  newMat: TGL3xMaterial;
begin
  Result := not IsMaterialExist(AName);
  if Result then
  begin
    newMat := TGL3xMaterial.Create(Self);
    newMat.Name := AName;
    newMat.Default;
    FMaterialTree.Add(newMat.FHashCode, newMat);
    newMat.SaveToFile(AFileName);
  end;
end;

procedure TGLSMaterialManager.TreeChanged(Sender: TObject);
begin
  if Sender = FMaterialTree then
    FMatTreeChanged := True
  else
    FtexTreeChanged := True;
end;

procedure TGLSMaterialManager.LightsChanged(Sender: TObject);
begin
  FLightsBuffer.NotifyChangesOfData;
end;

procedure TGLSMaterialManager.ApplyMaterial(const AName: TGL3xMaterialName);
var
  vMaterial: TGL3xMaterial;
begin
  Assert(FLastMaterial = nil);
  vMaterial := GetMaterial(AName);
  vMaterial.Apply;
  FLastMaterial := vMaterial;
end;

function TGLSMaterialManager.UnApplyMaterial: Boolean;
begin
  Assert(FLastMaterial <> nil);
  Result := FLastMaterial.UnApply;
  FLastMaterial := nil;
end;

procedure TGLSMaterialManager.ApplyTexture(const AName: TGL3xTextureName; UnitIndex: TGLuint);
var
  Texture: TGL3xTexture;
  i, n, hash: Integer;
begin
  n := Length(AName);
  hash := n;
  for i := 1 to n do
    hash := (hash shl 1) + Byte(AName[i]);
  if FTextureTree.Find(hash, Texture) then
    Texture.Apply(UnitIndex);
  // TODO: Write code for case when the texture by giving name does not exist
end;

procedure TGLSMaterialManager.BindLightsBlock;
begin
  FLightsBuffer.AllocateHandle;
  if FLightsBuffer.IsDataNeedUpdate then
  begin
    FLightsBuffer.BindBufferData(SafeCurrentGLContext.GLStates.GetLightStateAsAddress, SizeOf(TLightSourceState)*MAX_HARDWARE_LIGHT, GL_STATIC_READ);
    FLightsBuffer.Unbind;
  end;
  FLightsBuffer.BindBase(FLightsBlock.Location);
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TShaderSample'}{$ENDIF}
// ------------------
// ------------------ TShaderSample ------------------
// ------------------

procedure TShaderSample.Clear;
begin
  FillChar(Self, SizeOf(TShaderSample), $00);
end;

function TShaderSample.GetInputRef(I: Integer): PShaderSample;
begin
  Result := FInputRef[I];
end;

procedure TShaderSample.SetInputRef(I: Integer; Value: PShaderSample);
begin
  FInputRef[I] := Value;
  if Assigned(Value) then
  begin
    if Value.Mask = [] then
      Input[I] := Value.Output
    else
      Input[I] := MaskToGLSLType(Value.Mask);
    FTextureInIntput := FTextureInIntput or Value.FTextureInIntput;
  end;
end;

function TShaderSample.IsCanProcessed: Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to High(FInputRef) do
    if Assigned(FInputRef[I]) then
      Result := Result and FInputRef[I].Processed;
end;

function TShaderSample.TotalOutput: TGLSLDataType;
begin
  if Mask = [] then
    Result := Output
  else
    Result := MaskToGLSLType(Mask);
end;

function TShaderSample.MaskAsString: string;
begin
  Result := '';
  if ccmWhite in Mask then
    Result := Result + 'rgb'
  else
  begin
    if ccmRed in Mask then
      Result := Result + 'r';
    if ccmGreen in Mask then
      Result := Result + 'g';
    if ccmBlue in Mask then
      Result := Result + 'b';
  end;
  if ccmAlpha in Mask then
    Result := Result + 'a';
end;

function TShaderSample.InputAsString: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(Input) do
    case Input[I] of
      GLSLType1F: Result := Result + 'float;';
      GLSLType2F: Result := Result + 'vec2;';
      GLSLType3F: Result := Result + 'vec3;';
      GLSLType4F: Result := Result + 'vec4;';
      GLSLType1I: Result := Result + 'int;';
      GLSLType2I: Result := Result + 'ivec2;';
      GLSLType3I: Result := Result + 'ivec3;';
      GLSLType4I: Result := Result + 'ivec4;';
      GLSLType1UI: Result := Result + 'uint;';
      GLSLType2UI: Result := Result + 'uvec2;';
      GLSLType3UI: Result := Result + 'uvec3;';
      GLSLType4UI: Result := Result + 'uvec4;';
      GLSLTypeVoid: Result := Result + 'void;';
      GLSLTypeVRec: Result := Result + 'vrec;';
    end;
end;

function TShaderSample.GetDeclaration: AnsiString;
var
  L: Integer;
  vParams: AnsiString;
begin
  Result := '';
  if Self.Name <> MaterialSystem.Utility.Utility_ComponentMask then
  begin
    if Length(FInputFromMatSys)=0 then
      if not FindShaderObject then
      begin
        vParams := '(';
        if Input[0] = GLSLTypeVoid then
        begin
          vParams := vParams + 'void';
        end
        else
        begin
          for L := 0 to High(Input) do
            if Input[L] <> GLSLTypeUndefined then
            begin
              vParams := vParams + GLSLTypeToString(Input[L]) + ' ' + AnsiChar(L + 65) + ',';
            end;
        end;

        if vParams[Length(vParams)] = ',' then
          Delete(vParams, Length(vParams), 1);
        vParams := vParams + ')';
        Result := GLSLTypeToString(Self.Output) + ' ' + Self.Name + vParams + ';';
        exit;
      end;
    Result := GLSLTypeToString(Self.Output) + ' ' +
      Self.Name + '('+AnsiString(FInputFromMatSys)+');';
  end;
end;

function TShaderSample.FindShaderObject: Boolean;
var
  XMLMaterial, XMLSamples, XMLParagraph, XMLSample, XMLOverload: GLSXMLNode;
  I, P, U, L: Integer;
  sInputs, aInputs, sOutput, aOutput, aUniforms, sUniforms: string;
  ins: string;
begin
  if Purpose = sspVariable then
    exit(True);
  Result := False;
  if vMaterialManager.FInitialized
    and (Length(Category) > 0)
    and (Length(Name) > 0) then
  begin
    // Skip sample with no shader object
    if (Purpose = sspConstant) or (Purpose = sspVariable) then
      exit(True);

    if Name = MaterialSystem.Utility.Utility_ComponentMask then
      exit(True);

    sInputs := InputAsString;
    sOutput := string(GLSLTypeToString(Output));
    try
      XMLMaterial := vMaterialManager.FMatSysDoc.DocumentElement;
      FindXMLNode(XMLMaterial, 'Samples', XMLSamples);
      if FindXMLNode(XMLSamples, string(Category), XMLParagraph) then
      begin
        if FindXMLNode(XMLParagraph, string(Name), XMLSample) then
        begin
          for I := 0 to XMLSample.ChildNodes.Count - 1 do
          begin
            XMLOverload := XMLSample.ChildNodes[I];

            if not GetXMLAttribute(XMLOverload, 'Input', aInputs) then
              Abort;

            if not GetXMLAttribute(XMLOverload, 'Output', aOutput) then
              Abort;

            if (sInputs = RemoveGLSLQualifier(aInputs)) and (sOutput = aOutput) then
            begin
              FInputFromMatSys := aInputs;
              if FInputFromMatSys <> 'void;' then
              begin
                L := 65;
                while True do
                begin
                  P := Pos(';', FInputFromMatSys);
                  if P > 0 then
                  begin
                    ins := Char(L);
                    Inc(L);
                    if P < Length(FInputFromMatSys) then
                      ins := ins + ',';
                    FInputFromMatSys[P] := ' ';
                    Insert(ins, FInputFromMatSys, P+1);
                    continue;
                  end;
                  break;
                end;
              end
              else FInputFromMatSys := 'void';

              if not GetXMLAttribute(XMLOverload, 'ObjectName', FObjectName) then
                continue;

              if GetXMLAttribute(XMLOverload, 'Uniforms', aUniforms) then
              begin
                U := 0;
                repeat
                  P := Pos(';', aUniforms);
                  if P > 0 then
                  begin
                    sUniforms := Copy(aUniforms, 0, P - 1);
                    Delete(aUniforms, 1, P);
                    UniformClasses[U] := TSpecialUniformClass(FindClass(sUniforms));
                    Inc(U);
                  end
                  else
                    break;
                until Length(aUniforms) = 0;
              end;

              exit(True);
            end;

          end;
        end;
      end;
    except
      GLSLogger.LogFatalError(sBadMaterialConstr);
    end;
  end;
end;
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TGL3xMaterial'}{$ENDIF}
// ------------------
// ------------------ TGL3xMaterial ------------------
// ------------------

constructor TGL3xMaterial.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);

  FLoaded := False;
  FProgramLinked := False;
  FBlendingMode := bmOpaque;

  FSampleList := TList.Create;
  FTextureUnits := TStringList.Create;
  with ShaderManager do
  begin
    try
      BeginWork;
      FVertObject := MakeUniqueObjectName('MainVertexObj');
      FGeomObject := MakeUniqueObjectName('MainGeometryObj');
      FFragObject := MakeUniqueObjectName('MainFragmentObj');
      FProgram := MakeUniqueProgramName('MatProg');
    finally
      EndWork;
    end;
  end;
end;

destructor TGL3xMaterial.Destroy;
begin
  FSourceDoc := nil;
  ClearSamples;
  FSampleList.Free;
  ClearUniforms;
  inherited;
end;

procedure TGL3xMaterial.Default;
var
  rStream: TGLSResourceStream;
begin
  rStream := CreateResourceStream(glsDEFAULTMATERIALNAME, GLS_RC_XML_Type);
  LoadFromStream(rStream);
  rStream.Free;
end;

procedure TGL3xMaterial.Initialize;
var
  I, J: Integer;
  GLSLType: TGLSLDataType;
  pSample: PShaderSample;
  vpStrings, fpStrings: TStringList;
begin
  if not FLoaded then
    exit;

  for I := 0 to FSampleList.Count - 1 do
  begin
    pSample := FSampleList[I];
    for J := 0 to High(pSample.FInputRef) do
      if Assigned(pSample.FInputRef[J]) then
      begin
        if pSample.FInputRef[J].Mask = [] then
          GLSLType := pSample.FInputRef[J].Output
        else
          GLSLType := MaskToGLSLType(pSample.FInputRef[J].Mask);
        if pSample.Input[J] <> GLSLType then
        begin
          FLoaded := False;
          GLSLogger.LogError(Format('Invalid sample %s in material %s. Wrong samples connection.', [pSample.Name, FName]));
          continue;
        end;
      end;
    if not pSample.FindShaderObject then
    begin
      FLoaded := False;
      GLSLogger.LogError(Format('Sample %s can''t find shader object', [pSample.Name]));
    end;
  end;

  if not FLoaded then
    exit;

  vpStrings := nil;
  fpStrings := nil;
  if not vMaterialManager.GenerateShaderCode(FSampleList, vpStrings, fpStrings) then
  begin
    FLoaded := False;
    GLSLogger.LogError(Format('Material %s can''t generate shader program', [FName]));
    exit;
  end;
{$IFDEF GLS_OPENGL_DEBUG}
  vpStrings.SaveToFile('MaterialVertexProgram.txt');
  fpStrings.SaveToFile('MaterialFragmentProgram.txt');
{$ENDIF}
  CreateUniforms;

  try
    CreateShader(AnsiString(vpStrings.Text), AnsiString(fpStrings.Text));
  finally
    vpStrings.Free;
    fpStrings.Free;
    ClearSamples;
    FLoaded := False;
  end;
end;

procedure TGL3xMaterial.Assign(Source: TPersistent);
var
  Mat: TGL3xMaterial;
  I, J, K: Integer;
  pSample, pSample2: PShaderSample;
  mStream: TMemoryStream;
begin
  if Source is TGL3xMaterial then
  begin
    Mat := TGL3xMaterial(Source);
    FBlendingMode := Mat.FBlendingMode;
    FPolygonMode := Mat.FPolygonMode;
    FFaceCulling := Mat.FFaceCulling;
    ClearSamples;
    ClearUniforms;
    FTextureUnits.Assign(Mat.FTextureUnits);
    (*  be beter to redefine objects and program
        FVertObject := Mat.FVertObject;
        FGeomObject := Mat.FGeomObject;
        FFragObject := Mat.FFragObject;
        FProgram := Mat.FProgram;       *)
    if Mat.FSourceDoc <> nil then
    begin
      mStream := TMemoryStream.Create;
      try
        FSourceDoc := GLSNewXMLDocument;
{$IFNDEF FPC}
        Mat.FSourceDoc.SaveToStream(mStream);
        FSourceDoc.LoadFromStream(mStream);
{$ELSE}
        WriteXMLFile(Mat.FSourceDoc, mStream);
        mStream.Seek(0, soBeginning);
        ReadXMLFile(FSourceDoc, mStream);
{$ENDIF}
      finally
        mStream.Free;
      end;
    end
    else
      FSourceDoc := nil;
    FProgramLinked := False;
    FLoaded := Mat.FLoaded;
    if (Mat.FSampleList.Count = 0) and Assigned(FSourceDoc) then
    begin
      CreateSamples;
    end
    else
    begin
      // Copy samples
      for I := 0 to Mat.FSampleList.Count - 1 do
      begin
        New(pSample);
        pSample^ := PShaderSample(Mat.FSampleList[I])^;
        FSampleList.Add(pSample);
      end;
      // Redefine sample references
      for I := 0 to Mat.FSampleList.Count - 1 do
      begin
        pSample := Mat.FSampleList[I];
        pSample2 := FSampleList[I];
        for J := 0 to High(pSample.FInputRef) do
          if Assigned(pSample.FInputRef[J]) then
          begin
            K := Mat.FSampleList.IndexOf(pSample.FInputRef[J]);
            Assert(K > -1);
            pSample2.FInputRef[J] := FSampleList[K];
          end;
      end;
    end;

    ResourceName := Mat.ResourceName;
  end
  else
    inherited;
end;

procedure TGL3xMaterial.AssignTo(const AName: string);
begin
  vMaterialManager.GetMaterial(AName).Assign(Self);
end;

procedure TGL3xMaterial.AssignFrom(const AName: string);
begin
  Assign(vMaterialManager.GetMaterial(AName));
end;

procedure TGL3xMaterial.SetName(const AName: string);
var
  i, n: Integer;
begin
  if (Length(AName) > 0) and (Length(FName) = 0) then
  begin
    FName := AName;
    n := Length(AName);
    FHashCode := n;
    for i := 1 to n do
      FHashCode := (FHashCode shl 1) + Byte(AName[i]);
  end;
end;

procedure TGL3xMaterial.ClearSamples;
var
  I: Integer;
  pSample: PShaderSample;
begin
  for I := 0 to FSampleList.Count - 1 do
  begin
    pSample := FSampleList[I];
    Dispose(pSample);
  end;
  FSampleList.Clear;
end;

procedure TGL3xMaterial.ClearUniforms;
var
  I: Integer;
begin
  for I := 0 to High(FUniforms) do
    FUniforms[I].Free;
  SetLength(FUniforms, 0);
  FTextureUnits.Clear;
end;

// LoadFromFile
//

procedure TGL3xMaterial.LoadFromFile(const filename: string);
var
  fs: TStream;
begin
  if FileStreamExists(fileName) then
  begin
    fs := CreateFileStream(fileName, fmOpenRead);
    ResourceName := filename;
    try
      LoadFromStream(fs);
    finally
      fs.Free;
    end;
  end
  else
  begin
    GLSLogger.LogError(Format('File %s not found', [filename]));
    FLoaded := False;
  end;
end;

// SaveToFile
//

procedure TGL3xMaterial.SaveToFile(const filename: string);
var
  fs: TStream;
begin
  fs := CreateFileStream(fileName, fmOpenWrite or fmCreate);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
  ResourceName := filename;
end;

// LoadFromStream
//

procedure TGL3xMaterial.LoadFromStream(stream: TStream);
begin
  if not Assigned(FSourceDoc) then
    FSourceDoc := GLSNewXMLDocument;
{$IFNDEF FPC}
  FSourceDoc.LoadFromStream(stream);
{$ELSE}
  ReadXMLFile(FSourceDoc, stream);
{$ENDIF}
  CreateSamples;
end;

procedure TGL3xMaterial.CreateSamples;
var
  XMLMaterial, XMLSamples, XMLSample, XMLRefs, XMLRef: GLSXMLNode;
  err: Integer;
  I, J, K: Integer;
  pSample: PShaderSample;
  temp: string;

  function StrToGLSLType(AType: AnsiString): TGLSLDataType;
  var
    N: TGLSLDataType;
  begin
    for N := Low(TGLSLDataType) to High(TGLSLDataType) do
      if GLSLTypeToString(N) = AType then
        exit(N);
    Assert(False);
    Result := GLSLTypeUndefined;
  end;

begin

  FLoaded := False;
  FProgramLinked := False;

  try
    ClearSamples;
    ClearUniforms;
    XMLMaterial := FSourceDoc.DocumentElement;
    if not FindXMLNode(XMLMaterial, 'Samples', XMLSamples) then
      Abort;

    if GetXMLAttribute(XMLMaterial, 'FaceCulling', temp) then
    begin
      FFaceCulling := fcBufferDefault;
      while CompareText(temp, cFaceCulling[FaceCulling]) <> 0 do
        Inc(FFaceCulling);
    end;

    if GetXMLAttribute(XMLMaterial, 'PolygonMode', temp) then
    begin
      FPolygonMode := pmFill;
      while CompareText(temp, cPolygonMode[FPolygonMode]) <> 0 do
        Inc(FPolygonMode);
    end;

    if GetXMLAttribute(XMLMaterial, 'BlendingMode', temp) then
    begin
      FBlendingMode := bmOpaque;
      while CompareText(temp, cBlendingMode[FBlendingMode]) <> 0 do
        Inc(FBlendingMode);
    end;

    FTextureUnits.Clear;
    if GetXMLAttribute(XMLMaterial, 'TextureUnits', temp) then
    begin
      repeat
        K := Pos(';', temp);
        if K = 0 then
          break;
        FTextureUnits.Add(Copy(temp, 1, K - 1));
        Delete(temp, 1, K);
      until Length(temp) = 0;
    end;

    for I := 0 to XMLSamples.ChildNodes.Count - 1 do
    begin
      New(pSample);
      pSample.Clear;
      FSampleList.Add(pSample);
    end;

    for I := 0 to XMLSamples.ChildNodes.Count - 1 do
    begin
      pSample := FSampleList[I];
      XMLSample := XMLSamples.ChildNodes[I];
      GetXMLAttribute(XMLSample, 'Category', temp);
      pSample.Category := AnsiString(temp);
      GetXMLAttribute(XMLSample, 'Name', temp);
      pSample.Name := AnsiString(temp);
      GetXMLAttribute(XMLSample, 'Output', temp);
      pSample.Output := StrToGLSLType(AnsiString(temp));

      GetXMLAttribute(XMLSample, 'Participate', temp);
      if Pos('V', temp) > 0 then
        Include(pSample.Participate, ptVertex);
      if Pos('F', temp) > 0 then
        Include(pSample.Participate, ptFragment);
      if Pos('G', temp) > 0 then
        Include(pSample.Participate, ptGeometry);
      if Pos('C', temp) > 0 then
        Include(pSample.Participate, ptControl);
      if Pos('E', temp) > 0 then
        Include(pSample.Participate, ptEvaluation);

      if GetXMLAttribute(XMLSample, 'Constant', temp) then
      begin
        pSample.Purpose := sspConstant;
        GetXMLAttribute(XMLSample, 'ConstantValue', pSample.ConstantValue);
      end;

      if GetXMLAttribute(XMLSample, 'Purpose', temp) then
      begin
        if temp = 'C' then
        begin
          pSample.Purpose := sspConstant;
          GetXMLAttribute(XMLSample, 'ConstantValue', pSample.ConstantValue);
        end
        else if temp = 'V' then
        begin
          pSample.Purpose := sspVariable;
        end
      end;

      if GetXMLAttribute(XMLSample, 'Mask', temp) then
      begin
        if Pos('r', temp) > 0 then
          Include(pSample.Mask, ccmRed);
        if Pos('g', temp) > 0 then
          Include(pSample.Mask, ccmGreen);
        if Pos('b', temp) > 0 then
          Include(pSample.Mask, ccmBlue);
        if Pos('a', temp) > 0 then
          Include(pSample.Mask, ccmAlpha);
      end;

      FindXMLNode(XMLSample, 'References', XMLRefs);
      if XMLRefs.ChildNodes.Count = 0 then
        pSample.Input[0] := GLSLTypeVoid
      else
        for J := 0 to XMLRefs.ChildNodes.Count - 1 do
        begin
          XMLRef := XMLRefs.ChildNodes[J];
          GetXMLAttribute(XMLRef, 'Index', temp);
          Val(temp, K, err);
          pSample.FInputRef[J] := FSampleList[K];
          GetXMLAttribute(XMLRef, 'Input', temp);
          pSample.Input[J] := StrToGLSLType(AnsiString(temp));
        end;
    end;
    FLoaded := True;
  except
    if FName = glsDEFAULTMATERIALNAME then
      GLSLogger.LogFatalError('Error when loading default material')
    else
      GLSLogger.LogError('Error when loading material ' + FName);
  end;
end;

procedure TGL3xMaterial.CreateUniforms;
var
  I, J, K, L, N: Integer;
  pSample: PShaderSample;
  Duplicate: Boolean;
begin
  for I := 0 to FSampleList.Count - 1 do
  begin
    pSample := FSampleList[I];
    for J := 0 to High(pSample.UniformClasses) do
      if pSample.UniformClasses[J] <> nil then
      begin
        Duplicate := False;
        for K := 0 to High(FUniforms) do
          if (FUniforms[K].ClassType = pSample.UniformClasses[J]) then
          begin
            Duplicate := True;
            break;
          end;
        if not Duplicate then
        begin
          SetLength(FUniforms, Length(FUniforms) + 1);
          N := High(FUniforms);
          FUniforms[N] := pSample.UniformClasses[J].Create;
          if FUniforms[N] is TUniformSamplers then
            for L := 0 to FTextureUnits.Count - 1 do
              TUniformSamplers(FUniforms[N]).AddTexture(FTextureUnits[L]);
        end;
      end;
  end;
end;

procedure TGL3xMaterial.CreateShader(const VertexMain, FragmentMain: AnsiString);
var
  I: Integer;
  pSample: PShaderSample;
begin
  with ShaderManager do
  begin
    try
      BeginWork;
      DefineShaderProgram(FProgram);
      DefineShaderObject(FVertObject, VertexMain, [ptVertex]);
      DefineShaderObject(FFragObject, FragmentMain, [ptFragment]);
      AttachShaderObjectToProgram(FVertObject, FProgram);
      AttachShaderObjectToProgram(FFragObject, FProgram);
      for I := 0 to FSampleList.Count - 1 do
      begin
        pSample := FSampleList[I];
        if Length(pSample.FObjectName) > 0 then
          AttachShaderObjectToProgram(pSample.FObjectName, FProgram);
      end;
      FProgramLinked := LinkShaderProgram(FProgram);
    finally
      EndWork;
    end;
  end;
end;

// SaveToStream
//

procedure TGL3xMaterial.SaveToStream(stream: TStream);
var
  XMLMaterial, XMLSamples, XMLSample, XMLRefs, XMLRef: GLSDOMNode;
  I, J: Integer;
  pSample: PShaderSample;
  sPart: string;
begin
  if FSourceDoc <> nil then
  begin
    XMLMaterial := FSourceDoc.DOMDocument.DocumentElement;
    for I := 0 to XMLMaterial.childNodes.length - 1 do
    begin
      XMLSamples := XMLMaterial.childNodes.item[I];
      if XMLSamples.nodeName = 'Samples' then
      begin
        XMLMaterial.removeChild(XMLSamples);
        break;
      end;
    end;
    XMLSamples := CreateDOMNode(XMLMaterial, 'Samples');
    SetXMLAttribute(XMLMaterial, 'FaceCulling', cFaceCulling[FFaceCulling]);
    SetXMLAttribute(XMLMaterial, 'PolygonMode', cPolygonMode[FPolygonMode]);
    SetXMLAttribute(XMLMaterial, 'BlendingMode', cBlendingMode[FBlendingMode]);

    for I := 0 to FSampleList.Count - 1 do
    begin
      pSample := FSampleList[I];
      XMLSample := CreateDOMNode(XMLSamples, 'Sample' + IntToStr(I));
      SetXMLAttribute(XMLSample, 'Category', string(pSample.Category));
      SetXMLAttribute(XMLSample, 'Name', string(pSample.Name));
      SetXMLAttribute(XMLSample, 'Output', string(GLSLTypeToString(pSample.Output)));

      sPart := '';
      if ptVertex in pSample.Participate then
        sPart := sPart + 'V';
      if ptFragment in pSample.Participate then
        sPart := sPart + 'F';
      if ptGeometry in pSample.Participate then
        sPart := sPart + 'G';
      SetXMLAttribute(XMLSample, 'Participate', sPart);

      case pSample.Purpose of
        sspConstant:
          begin
            SetXMLAttribute(XMLSample, 'Purpose', 'C');
            SetXMLAttribute(XMLSample, 'ConstantValue', pSample.ConstantValue);
          end;
        sspVariable:
          begin
            SetXMLAttribute(XMLSample, 'Purpose', 'V');
          end;
      end;

      if pSample.Mask <> [] then
        SetXMLAttribute(XMLSample, 'Mask', pSample.MaskAsString);

      XMLRefs := CreateDOMNode(XMLSample, 'References');

      for J := 0 to High(pSample.FInputRef) do
        if Assigned(pSample.FInputRef[J]) then
        begin
          XMLRef := CreateDOMNode(XMLRefs, 'R' + IntToStr(J));
          SetXMLAttribute(XMLRef, 'Index', IntToStr(FSampleList.IndexOf(pSample.FInputRef[J])));
          SetXMLAttribute(XMLRef, 'Input', string(GLSLTypeToString(pSample.Input[J])));
        end;
    end;
{$IFNDEF FPC}
    FSourceDoc.SaveToStream(stream);
{$ELSE}
    WriteXMLFile(FSourceDoc, stream);
{$ENDIF}
  end;
end;

//function TGL3xMaterial.GetAttributes(out AttrArray: TGLSLAttributeArray):
//  Boolean;
//var
//  i, j: Integer;
//  Attribs: TGLActiveAttribArray;
//begin
//  Result := false;
//  if FShader = nil then
//    exit;
//  Attribs := FShader.GetActiveAttribs;
//  j := 0;
//  for i := 0 to GLS_VERTEX_ATTR_NUM - 1 do
//    if (i <= High(Attribs)) then
//    begin
//      AttrArray[I] := TGLSLAttribute.GetAttribute(Attribs[i].Name);
//      Inc(j);
//    end
//    else
//      AttrArray[I] := nil;
//  Result := (j > 0);
//end;

procedure TGL3xMaterial.Apply;
var
  I: Integer;
begin
  if not FProgramLinked and FLoaded then
    Initialize; // On the fly initialization

  if FProgramLinked then
  begin
    // Apply Shader
    ShaderManager.UseProgram(FProgram);

    // Apply Uniforms
    for I := 0 to High(FUniforms) do
      FUniforms[I].Apply;

    // Apply States
    with CurrentGLContext.GLStates do
    begin
      PolygonMode := FPolygonMode;

      case FFaceCulling of
        fcBufferDefault: ;
        fcCull: Enable(stCullFace);
        fcNoCull: Disable(stCullFace);
      end;

      case FBlendingMode of
        bmOpaque:
          begin
            Disable(stBlend);
          end;
        bmTransparency, bmMasked:
          begin
            Enable(stBlend);
            SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
          end;
        bmAdditive:
          begin
            Enable(stBlend);
            SetBlendFunc(bfSrcAlpha, bfOne);
          end;
        bmModulate:
          begin
            Enable(stBlend);
            SetBlendFunc(bfDstColor, bfZero);
          end;
        bmCustom:
          begin
          end;
      end;
    end;
    exit;
  end;

  // Force-major
  GLSLogger.LogError(Format('Applying of material %s aborted.', [FName]));
  Abort;
end;

function TGL3xMaterial.UnApply: Boolean;
begin
  if not CurrentGLContext.GLStates.ForwardContext then
    ShaderManager.UseFixedFunctionPipeline;
  Result := True;
end;

// Capabilities
//

class function TGL3xMaterial.Capabilities: TDataFileCapabilities;
begin
  Result := [dfcRead, dfcWrite];
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

initialization
{$IFDEF FPC}
{$I GLSceneMaterialSysLCL.lrs}
{$ENDIF}

  vMaterialManager := TGLSMaterialManager.Create;

finalization

  vMaterialManager.Destroy;
  vMaterialManager := nil;

end.

