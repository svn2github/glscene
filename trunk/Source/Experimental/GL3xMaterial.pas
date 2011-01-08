//
// This unit is part of the GLScene Project, http://glscene.org
//
{ : GL3xMaterial<p>

  <b>History : </b><font size=-1><ul>
  <li>04/09/10 - Yar - Added materials and textures loading. List of MaterialManager's items saved to application resource
  <li>07/07/10 - Yar - Creation
  </ul></font>
}

// TODO: Shader array program in material

unit GL3xMaterial;

interface

{$I GLScene.inc}

uses
  Classes,
  SysUtils,
  Variants,
{$IFDEF FPC}
  LCLVersion,
  FileUtil,
  XMLRead,
  XMLWrite,
  LResources,
{$ENDIF}
  GLSCrossXML,
  GLSGenerics,
  GLCrossPlatform,
  BaseClasses,
  ApplicationFileIO,
  OpenGLTokens,
  GLContext,
  GLState,
  GLShaderManager,
  VectorGeometry,
  GL3xMaterialTokens,
  GL3xTexture,
  GLSRedBlackTree,
  GLRenderContextInfo
{$IFDEF GLS_DELPHI}
  ,
  VectorTypes
{$ENDIF};

const
  fileMaterialSystem = 'MaterialSystem.xml';

{$IFDEF FPC}
  {$IF (FPC_VERSION = 2) and (FPC_RELEASE < 5)}
    {$DEFINE GLS_GENERIC_PREFIX}
  {$IFEND}
{$ENDIF}

type

  TBaseShaderEnvironment = class(TPersistent)
  protected
    FGLSLUniform: TGLSLUniform;
    FGLSLUniformBlock: TGLSLUniformBlock;
  public
    constructor Create(const AArray: TTextureSamplerArray); virtual; abstract;
    procedure Apply; virtual; abstract;
  end;

  TBaseShaderEnvironmentClass = class of TBaseShaderEnvironment;

  TShaderSamplePurpose = (
    sspOperation,
    sspConstant,
    sspVariable,
    sspBuildIn,
    sspLoopOperation);

  { : Simple shader element, which is a construction shader programs. }
  PShaderSample = ^TShaderSample;
  TInputRefArray = array[0..3] of PShaderSample;
  TShaderSample = {$IFNDEF FPC}record {$ELSE}object{$ENDIF}
  private LocalID: Word;
    Processed: Boolean;
    FInputRef: TInputRefArray;
    FTextureInInput: Boolean;
    FObjectName: array[TMaterialVariant] of IGLName;
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
    UniformClasses: array[0..7] of TBaseShaderEnvironmentClass;
    ConstantValue: string;
    Mask: TGLColorComponentMask;
    procedure Clear;
    function CheckWithMaterialSystem: Boolean;
    function FindShaderObject: Boolean;
    property Declaration: AnsiString read GetDeclaration;
    function MaskAsString: string;
    function InputAsString: string;
    function IsCanProcessed: Boolean;
    function TotalOutput: TGLSLDataType;
    property TextureInInput: Boolean read FTextureInInput write FTextureInInput;
    property InputRef[Index: Integer]: PShaderSample read GetInputRef write SetInputRef;
  end;

  TSampleList = {$IFDEF GLS_GENERIC_PREFIX}specialize{$ENDIF}
  	GList < PShaderSample > ;

  TSampleGatherInfo = {$IFNDEF FPC}record {$ELSE}object{$ENDIF}
    SampleList: TSampleList;
    VertexRecord, FragmentRecord, Master: PShaderSample;
    PassTexCoord: array[0..7] of Boolean;
    ProgramTypes: TGLSLProgramTypes;
    ChainInLoop: Boolean;
    procedure NewMaster;
  end;

  TProgramCodeSet = array[TGLSLProgramType] of TStringList;

  // TGL3xMaterialName
  //

  TGL3xMaterialName = class(TGLAbstractName)
  protected
    function GetInheritorClass: TGLAbstractNameClass; override;
  public
    function GetManager: TGLSAbstractManagerClass; override;
  end;

  TProgramVariantSet = record
    FShaders: array[TGLSLProgramType] of IGLName;
    FProgram: IGLName;
  end;

  // TGL3xMaterial
  //

  TGL3xMaterial = class(TDataFile)
  private
    { Private Declarations }
    FSourceDoc: GLSXMLDocument;
    FName: TGL3xMaterialName;
    FVariants: TMaterialVariants;
    FProgramVariants: array[TMaterialVariant] of TProgramVariantSet;
    FLoaded: Boolean;
    FLightingModel: TLightingModel;
    FFaceCulling: TFaceCulling;
    FBlendingMode: TBlendingMode;
    FPolygonMode: TPolygonMode;
    function GetProgram(AVariant: TMaterialVariant): IGLName;
  protected
    { Protected Declarations }
    FSampleList: TSampleList;
    FUniforms: array of TBaseShaderEnvironment;
    FUnits: TTextureSamplerArray;
    procedure CreateSamples;
    procedure CreateUniforms;
    procedure ClearSamples;
    procedure ClearUniforms;
    procedure ClearUnits;
    procedure CreatePrograms(const AProgramCodeSet: TProgramCodeSet);
    procedure Default;
    procedure DirectUse;

    property Document: GLSXMLDocument read FSourceDoc write FSourceDoc;
    property LightingModel: TLightingModel read FLightingModel write FLightingModel;
    property BlendingMode: TBlendingMode read FBlendingMode write FBlendingMode;
    property FaceCulling: TFaceCulling read FFaceCulling write FFaceCulling;
    property PolygonMode: TPolygonMode read FPolygonMode write FPolygonMode;
    property SampleList: TSampleList read FSampleList;
  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(const AName: IGLName); reintroduce;
    procedure AssignFrom(const AName: IGLName);

    procedure LoadFromFile(const fileName: string); override;
    procedure SaveToFile(const fileName: string); override;
    procedure LoadFromStream(stream: TStream); override;
    procedure SaveToStream(stream: TStream); override;
    procedure Initialize; override;
    class function Capabilities: TDataFileCapabilities; override;
    procedure Apply(var ARci: TRenderContextInfo; AVariant: TMaterialVariant);
    function UnApply(var ARci: TRenderContextInfo): Boolean;

    property Name: TGL3xMaterialName read FName;
    property ProgramName[Variant: TMaterialVariant]: IGLName read GetProgram;
  end;

  MaterialManager = class(TGLSAbstractManager)
  protected
    { Private Declarations }
    class procedure LoadMaterialSystem;
    class procedure SaveResources;
    class procedure LoadResources;
    class procedure ClearResources; stdcall;
    class function GetMaterial(const AName: IGLName): TGL3xMaterial;
    class function GetTexture(const AName: IGLName): TGL3xTexture;
    class function GetSampler(const AName: IGLName): TGL3xSampler;
    class procedure PushMaterial(AMaterial: TGL3xMaterial);
    class procedure PushTexture(ATexture: TGL3xTexture);
    class procedure PushSampler(ASampler: TGL3xSampler);
    class procedure Initialize;
    class procedure Finalize;

    // Design time notifications
    class procedure NotifyProjectOpened; override;
    class procedure NotifyProjectClosed; override;
    class procedure NotifyContextCreated; override;
    class procedure NotifyBeforeCompile; override;
  public
    { Public Declarations }
    class function FillResourceList(AList: TStringList): Boolean; override;
    class procedure MakeUniqueItemName(var AName: string; AClass: TGLAbstractNameClass); override;

    // Design time helper methods
    class function GenerateShaderCode(const ASampleList: TSampleList;
      var AProgramCodeSet: TProgramCodeSet; const AName: AnsiString = 'main'; AOutType: TGLSLDataType = GLSLTypeVoid): Boolean;
    class procedure FillMaterialNameList(var AList: TStringList);
    class procedure FillTextureNameList(var AList: TStringList);
    class procedure FillSamplerNameList(var AList: TStringList);

    class function CreateMaterial(AName: string; const AFileName: string): IGLName;
    class function CreateTexture(AName: string; const AImportFile: string; const AFileName: string): IGLName;
    class function CreateSampler(AName: string; const AFileName: string): IGLName;
    class procedure ApplyMaterial(const AName: IGLName; var ARci: TRenderContextInfo;
      AVariant: TMaterialVariant = matvarCommon);
    class function UnApplyMaterial(var ARci: TRenderContextInfo): Boolean;
    class procedure ApplyTextureSampler(const ATextureName, ASamplerName: IGLName; AUniform: TGLSLUniform);

    class function GetMaterialName(const AName: string): IGLName;
    class function GetMaterialProgramName(const AName: IGLName;
      AVariant: TMaterialVariant = matvarCommon): IGLName;
    class function GetTextureName(const AName: string): IGLName;

    class function GetSamplerName(const AName: string): IGLName;
  end;

implementation

uses
  GLSLog,
  GLStrings,
  GLFileDDS;

resourcestring
  sBadMaterialConstr = 'Bad material constructions file';

{$IFNDEF FPC}
{$R GLSceneMaterialSysVCL.dcr}
{$ENDIF}

var
  Initialized: Boolean;
  MatSysDoc: GLSXMLDocument;
  Materials: array of TGL3xMaterial;
  Textures: array of TGL3xTexture;
  Samplers: array of TGL3xSampler;
  LastMaterial: TGL3xMaterial;
  Structures: AnsiString;
  SampleShaderObjectNames: array of IGLName;

{$REGION 'MaterialManager'}
// ------------------
// ------------------ MaterialManager ------------------
// ------------------

class procedure MaterialManager.Initialize;
begin
  RegisterGLSceneManager(MaterialManager);
  LoadMaterialSystem;
  LoadResources;
  Initialized := False;
end;

class procedure MaterialManager.Finalize;
begin
  AddTaskForServiceContext(ClearResources);
end;

class procedure MaterialManager.ClearResources;
var
  I: Integer;
begin
  for I := 0 to High(Materials) do
    FreeAndNil(Materials[I]);
  for I := 0 to High(Textures) do
    FreeAndNil(Textures[I]);
  for I := 0 to High(Samplers) do
    FreeAndNil(Samplers[I]);
  Materials := nil;
  Textures := nil;
  Samplers := nil;
end;

class procedure MaterialManager.LoadMaterialSystem;
var
  rStream: TGLSResourceStream;
begin
  if IsDesignTime or not FileStreamExists(fileMaterialSystem) then
  begin
    rStream := nil;
    try
      rStream := CreateResourceStream('MaterialSystem', GLS_RC_XML_Type);
{$IFNDEF FPC}
      MatSysDoc := GLSNewXMLDocument;
      MatSysDoc.LoadFromStream(rStream);
{$ELSE}
      ReadXMLFile(MatSysDoc, rStream);
{$ENDIF}
    finally
      rStream.Free;
    end;
    GLSLogger.LogNotice('Material system loaded from application resource');
  end
  else
  begin
{$IFNDEF FPC}
    MatSysDoc := GLSNewXMLDocument;
    MatSysDoc.LoadFromFile(fileMaterialSystem);
{$ELSE}
    ReadXMLFile(MatSysDoc, fileMaterialSystem);
    if MatSysDoc = nil then
      exit;
{$ENDIF}
    GLSLogger.LogNotice('Material system loaded from file ' + fileMaterialSystem);
  end;
end;

class procedure MaterialManager.SaveResources;
var
  I: Integer;

  procedure SaveRes(ARes: TDataFile);
  begin
    if Assigned(ARes) and (Length(ARes.ResourceName) > 0) then
      ARes.SaveToFile(ARes.ResourceName);
  end;

begin
  for I := 1 to High(Materials) do
    SaveRes(Materials[I]);
  // Save textures
  for I := 2 to High(Textures) do
    SaveRes(Textures[I]);
  // Save samples
  for I := 0 to High(Samplers) do
    SaveRes(Samplers[I]);
end;

class procedure MaterialManager.LoadResources;
var
  I: Integer;
  rStream: TGLSResourceStream;
  RT, RT_: TGLSApplicationResource;
  ResList: TStringList;
  line, rName, rFile: string;
  newSmp: TGL3xSampler;
  newTex: TGL3xTexture;
  newMat: TGL3xMaterial;

  procedure GetNameAndFile;
  var
    p: Integer;
  begin
    p := Pos('=', line);
    rName := Copy(line, 1, p - 1);
    rFile := Copy(line, p + 1, Length(line) - p + 1);
  end;

begin
  try
    BeginWork;

    if Length(Materials) = 0 then
    begin
      // Make default material
      newMat := TGL3xMaterial.Create(nil);
      newMat.Name.Value := glsDEFAULTMATERIALNAME;
      newMat.Default;
      PushMaterial(newMat);
      // Make default diffuse texture
      newTex := TGL3xTexture.Create(nil);
      newTex.Name.Value := glsDIFFUSEMAP;
      rStream := CreateResourceStream(glsDIFFUSEMAP, GLS_RC_DDS_Type);
      newTex.ImportFromStream(rStream);
      rStream.Destroy;
      PushTexture(newTex);
      // Make default normal texture
      newTex := TGL3xTexture.Create(nil);
      newTex.Name.Value := glsNORMALMAP;
      rStream := CreateResourceStream(glsNORMALMAP, GLS_RC_DDS_Type);
      newTex.ImportFromStream(rStream);
      newTex.TexelCast := tcNormalXYZ;
      rStream.Destroy;
      PushTexture(newTex);
    end;
    // Load materials and textures info from application resource
    GLSLogger.Enabled := False;
    ResList := TStringList.Create;

    if IsDesignTime then
    begin
      ResList.Text := vManagersResourceList;
    end
    else
    begin
      rStream := CreateResourceStream(glsResourceInfo, GLS_RC_String_Type);
      if Assigned(rStream) then
      begin
        ResList.LoadFromStream(rStream);
        rStream.Destroy;
      end;
    end;

    GLSLogger.Enabled := True;

    RT := aresNone;
    SetExeDirectory;
    for I := 0 to ResList.Count - 1 do
    begin
      line := ResList.Strings[I];
      RT_ := StrToGLSResType(line);
      if (RT_ <> aresNone) and (RT <> RT_) then
      begin
        RT := RT_;
        continue;
      end;
      case RT of
        aresMaterial:
          begin
            GetNameAndFile;
            if FileStreamExists(rFile) then
            begin
              newMat := TGL3xMaterial.Create(nil);
              newMat.Name.Value := rName;
              newMat.LoadFromFile(rFile);
              PushMaterial(newMat);
            end
            else
              GLSLogger.LogWarning(Format(glsMissingResource, [TGL3xMaterial.ClassName, rName]));
          end;
        aresTexture:
          begin
            GetNameAndFile;
            if FileStreamExists(rFile) then
            begin
              newTex := TGL3xTexture.Create(nil);
              newTex.Name.Value := rName;
              newTex.LoadFromFile(rFile);
              PushTexture(newTex);
            end
            else
              GLSLogger.LogWarning(Format(glsMissingResource, [TGL3xTexture.ClassName, rName]));
          end;
        aresSampler:
          begin
            GetNameAndFile;
            if FileStreamExists(rFile) then
            begin
              newSmp := TGL3xSampler.Create(nil);
              newSmp.Name.Value := rName;
              newSmp.LoadFromFile(rFile);
              PushSampler(newSmp);
            end
            else
              GLSLogger.LogWarning(Format(glsMissingResource, [TGL3xSampler.ClassName, rName]));
          end;
      end;
    end;

    ResList.Destroy;
  finally
    EndWork;
  end;
end;

class procedure MaterialManager.NotifyContextCreated;

  procedure InitializeAll;
  var
    I: Integer;
  begin
    for I := 0 to High(Samplers) do
      if Assigned(Samplers[I]) then
        Samplers[I].Initialize;
    for I := 0 to High(Materials) do
      if Assigned(Materials[I]) then
        Materials[I].Initialize;
    for I := 0 to High(Textures) do
      if Assigned(Textures[I]) then
        Textures[I].Initialize;
    GL.Finish;
  end;

var
  XMLMaterial, XMLSamples, XMLParagraph, XMLSample, XMLOverload: GLSXMLNode;
  I, J, K, p, II: Integer;
  sCategory: AnsiString;
  ObjType, temp, aInput, aOutput, aGlobal, aLocal, aVariant: string;
  Param: AnsiChar;
  ObjCode: AnsiString;
  oType: TGLSLProgramTypes;
begin
  // Create shader objects from material system database
  if Initialized then
  begin
    InitializeAll;
    exit;
  end;

  try
    ShaderManager.BeginWork;
    if MatSysDoc = nil then
      Abort;

    XMLMaterial := MatSysDoc.DocumentElement;
    if FindXMLNode(XMLMaterial, 'Samples', XMLSamples) then
    begin
      GetXMLAttribute(XMLSamples, 'Structures', temp);
      Structures := AnsiString(temp);
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
            //            if Pos('C', ObjType) > 0 then
            //              Include(oType, ptControl);
            //            if Pos('E', ObjType) > 0 then
            //              Include(oType, ptEvaluation);
            for K := 0 to XMLSample.ChildNodes.Count - 1 do
            begin
              XMLOverload := XMLSample.ChildNodes[K];

              if GetXMLAttribute(XMLOverload, 'Const', temp) then
                continue;
              if GetXMLAttribute(XMLOverload, 'Mask', temp) then
                continue;

              if GetXMLText(XMLOverload, temp) then
              begin
                SetLength(SampleShaderObjectNames, Length(SampleShaderObjectNames) + 1);
                II := High(SampleShaderObjectNames);
                SetXMLAttribute(XMLOverload, 'ObjectIndex', IntToStr(II));

                ObjCode := GetMaxGLSLVersion;

                // Get variant tag
                if GetXMLAttribute(XMLOverload, 'Variant', aVariant) then
                  aVariant := '_' + aVariant;
                                  
                // Global scope attributes, varyings, uniforms
                if GetXMLAttribute(XMLOverload, 'Global', aGlobal) then
                  ObjCode := ObjCode + AnsiString(aGlobal) + #10#13;

                // Share structure declarations
                ObjCode := ObjCode + Structures + #10#13;

                // Local object declaration like as functions
                if GetXMLAttribute(XMLOverload, 'Local', aLocal) then
                  ObjCode := ObjCode + AnsiString(aLocal) + #10#13;

                // Object function arguments
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
                    p := Pos(';', aInput);
                    if p = 0 then
                      break;
                    ObjCode := ObjCode + AnsiString(Copy(aInput, 1, p - 1)) + ' ' + Param + ',';
                    Delete(aInput, 1, p);
                    Inc(Param);
                  until Length(aInput) = 0;
                  ObjCode[Length(ObjCode)] := ')';
                  ObjCode := ObjCode + ' ' + #10#13;
                end;

                // Concate main object code
                ObjCode := ObjCode + AnsiString(temp);
                ShaderManager.DefineShaderObject(SampleShaderObjectNames[II], 
                  ObjCode, oType, XMLSample.NodeName + aVariant);
              end;
            end;
          end;
        end;
      end;
    end
    else
      Abort;

    Initialized := True;
    ShaderManager.EndWork;
    GLSLogger.LogNotice('Material system intialized');
  except
    ShaderManager.EndWork;
    GLSLogger.LogFatalError(sBadMaterialConstr);
  end;
  if Initialized then
    InitializeAll;
end;

class function MaterialManager.FillResourceList(AList: TStringList): Boolean;
var
  I: Integer;
begin
  Result := False;
  if Length(Samplers) > 0 then
  begin
    AList.Add('[SAMPLERS]');
    for I := 0 to High(Samplers) do
    begin
      if Assigned(Samplers[I]) and (Length(Samplers[I].ResourceName) > 0) then
      begin
        AList.Add(Format('%s=%s', [Samplers[I].Name.Value, ExtractFileName(Samplers[I].ResourceName)]));
        Result := True;
      end;
    end;
  end;
  if Length(Textures) > 2 then
  begin
    AList.Add('[TEXTURES]');
    for I := 2 to High(Textures) do
    begin
      if Assigned(Textures[I]) and (Length(Textures[I].ResourceName) > 0) then
      begin
        AList.Add(Format('%s=%s', [Textures[I].Name.Value, ExtractFileName(Textures[I].ResourceName)]));
        Result := True;
      end;
    end;
  end;
  if Length(Materials) > 1 then
  begin
    AList.Add('[MATERIALS]');
    for I := 1 to High(Materials) do
    begin
      if Assigned(Materials[I]) and (Length(Materials[I].ResourceName) > 0) then
      begin
        AList.Add(Format('%s=%s', [Materials[I].Name.Value, ExtractFileName(Materials[I].ResourceName)]));
        Result := True;
      end;
    end;
  end;
end;

class procedure MaterialManager.NotifyProjectOpened;
begin
  if IsDesignTime then
  begin
    AddTaskForServiceContext(ClearResources);
    LoadResources;
  end;
end;

class procedure MaterialManager.NotifyProjectClosed;
begin
  if IsDesignTime then
  begin
    SaveResources;
    AddTaskForServiceContext(ClearResources);
  end;
end;

class procedure MaterialManager.NotifyBeforeCompile;
begin
  SaveResources;
end;

class function MaterialManager.GenerateShaderCode(const ASampleList: TSampleList;
  var AProgramCodeSet: TProgramCodeSet;
  const AName: AnsiString; AOutType: TGLSLDataType): Boolean;
const
  sLocal: AnsiString = 'Local';
var
  I, J, ProcessedCount: Integer;
  PT: TGLSLProgramType;
  Headers, Bodies: TProgramCodeSet;
  LocalCount: Word;
  pSample: PShaderSample;
  line, dline, CurrLocal: AnsiString;
  withParam: Boolean;

  function NextLocal: AnsiString;
  begin
    Result := sLocal + AnsiString(IntToStr(LocalCount));
    pSample.LocalID := LocalCount;
    Inc(LocalCount);
  end;

begin
  try
    BeginWork;

    ProcessedCount := 0;

    // Delete garbage
    for I := 0 to ASampleList.Count - 1 do
    begin
      pSample := ASampleList[I];
      if pSample = nil then
        continue;
      // Skip build-in samples
      if pSample.Purpose = sspBuildIn then
      begin
        pSample.Processed := True;
        Inc(ProcessedCount);
        continue;
      end;
      pSample.Processed := False;
      if (pSample.Participate = []) or (pSample.Output = GLSLTypeUndefined) then
      begin
        GLSLogger.LogInfo('Deleted unused sample ' + string(pSample.Name));
        Dispose(pSample);
        ASampleList[I] := nil;
      end;
    end;
    // Pack list
    J := 0;
    for I := 0 to ASampleList.Count - 1 do
      if Assigned(ASampleList[I]) then
      begin
        ASampleList[J] := ASampleList[I];
        Inc(J);
      end;
    ASampleList.Count := J;

    for PT := Low(TGLSLProgramType) to High(TGLSLProgramType) do
    begin
      if not Assigned(AProgramCodeSet[PT]) then
        AProgramCodeSet[PT] := TStringList.Create
      else
        AProgramCodeSet[PT].Clear;
      Headers[PT] := TStringList.Create;
      Bodies[PT] := TStringList.Create;
      if AName = 'main' then
        with Headers[PT] do
        begin
          Add(Format('// %s SHADER', [cObjectTypeName[PT]]));
          Add('');
          Add(string(GetMaxGLSLVersion));
          Add(string(Structures));
        end;
      Bodies[PT].Add(string(GLSLTypeToString(AOutType) + ' ' + AName + '()'));
      Bodies[PT].Add('{');
    end;

    pSample := nil;
    LocalCount := 0;

    try
      if ASampleList.Count = 0 then
        Abort;

      repeat
        if not Assigned(pSample) then
          for I := 0 to ASampleList.Count - 1 do
          begin
            pSample := ASampleList[I];
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
                  line := 'const ' + GLSLTypeToString(pSample.Output) + ' ' + NextLocal + ' = ' + HexToGLSL(pSample.Output, pSample.ConstantValue) + ';';
                end
                else
                begin
                  line := 'const ' + GLSLTypeToString(MaskToGLSLType(pSample.Mask)) + ' ' + NextLocal + ' = ' + HexToGLSL(pSample.Output, pSample.ConstantValue) + ';';
                end;
              end;
            sspVariable:
              begin
                line := GLSLTypeToString(pSample.Output) + ' ' + NextLocal + ';';
              end;
          end;

          for PT := Low(TGLSLProgramType) to High(TGLSLProgramType) do
            if PT in pSample.Participate then
            begin
              if pSample.Purpose = sspOperation then
              begin
                dline := pSample.Declaration;
                if Headers[PT].IndexOf(string(dline)) < 0 then
                  Headers[PT].Add(string(dline));
              end;
              Bodies[PT].Add('  ' + string(line));
              break;
            end;

          pSample.Processed := True;
          Inc(ProcessedCount);
          pSample := nil;
        end
        else // input samples are not ready
        begin
          for I := 0 to High(pSample.FInputRef) do
            if Assigned(pSample.FInputRef[I]) and not pSample.FInputRef[I].Processed then
            begin
              pSample := pSample.FInputRef[I];
              break;
            end;
        end;
      until ProcessedCount = ASampleList.Count;

      for PT := Low(TGLSLProgramType) to High(TGLSLProgramType) do
      begin
        if Bodies[PT].Count > 2 then
        begin
          Bodies[PT].Add('}');
          AProgramCodeSet[PT].AddStrings(Headers[PT]);
          AProgramCodeSet[PT].AddStrings(Bodies[PT]);
        end;
        Headers[PT].Destroy;
        Bodies[PT].Destroy;
      end;

      Result := True;

    except

      for PT := Low(TGLSLProgramType) to High(TGLSLProgramType) do
      begin
        Headers[PT].Destroy;
        Bodies[PT].Destroy;
      end;

      Result := False;

    end;
  finally
    EndWork;
  end;
end;

class function MaterialManager.GetMaterial(const AName: IGLName): TGL3xMaterial;
var
  I: Integer;
begin
  if Assigned(AName) and (AName.GetInheritorClass = TGL3xMaterialName) then
  begin
    I := AName.GetIndex;
    if I > -1 then
      exit(Materials[I]);
  end;
  Result := Materials[0];
end;

class function MaterialManager.GetTexture(const AName: IGLName): TGL3xTexture;
var
  I: Integer;
begin
  if Assigned(AName) and (AName.GetInheritorClass = TGL3xTextureName) then
  begin
    I := AName.GetIndex;
    if I > -1 then
      exit(Textures[I]);
  end;
  Result := Textures[0];
end;

class function MaterialManager.GetSampler(const AName: IGLName): TGL3xSampler;
var
  I: Integer;
begin
  if Assigned(AName) and (AName.GetInheritorClass = TGL3xSamplerName) then
  begin
    I := AName.GetIndex;
    if I > -1 then
      exit(Samplers[I]);
  end;
  Result := nil;
end;

class function MaterialManager.GetMaterialName(const AName: string): IGLName;
var
  I, N, H: Integer;
begin
  CheckCall;

  N := Length(AName);
  H := N;
  for I := 1 to N do
    H := (H shl 1) + Integer(AName[I]);

  for I := 0 to High(Materials) do
  begin
    if Assigned(Materials[I]) then
    begin
      if (Materials[I].FName.HashCode = H) and (string(Materials[I].FName.Value) = AName) then
        exit(Materials[I].FName);
    end;
  end;

  Result := Materials[0].FName;
end;

class function MaterialManager.GetMaterialProgramName(const AName: IGLName;
  AVariant: TMaterialVariant): IGLName;
begin
  CheckCall;
  Result := GetMaterial(AName).ProgramName[AVariant];
end;

class function MaterialManager.GetTextureName(const AName: string): IGLName;
var
  I, N, H: Integer;
begin
  CheckCall;

  N := Length(AName);
  H := N;
  for I := 1 to N do
    H := (H shl 1) + Integer(AName[I]);

  for I := 0 to High(Textures) do
  begin
    if Assigned(Textures[I]) then
    begin
      if (Textures[I].Name.HashCode = H) and (string(Textures[I].Name.Value) = AName) then
        exit(Textures[I].Name);
    end;
  end;

  Result := Textures[0].Name;
end;

class function MaterialManager.GetSamplerName(const AName: string): IGLName;
var
  I, N, H: Integer;
begin
  CheckCall;

  N := Length(AName);
  H := N;
  for I := 1 to N do
    H := (H shl 1) + Integer(AName[I]);

  for I := 0 to High(Samplers) do
  begin
    if Assigned(Samplers[I]) then
    begin
      if (Samplers[I].Name.HashCode = H) and (string(Samplers[I].Name.Value) = AName) then
        exit(Samplers[I].Name);
    end;
  end;

  Result := nil;
end;

class procedure MaterialManager.MakeUniqueItemName(var AName: string; AClass: TGLAbstractNameClass);
const
  cMatName = 'RenderMaterial';
  cTexName = 'DiffuseTexture';
  cSmpName = 'TextureSampler';
var
  N: Integer;
begin
  CheckCall;

  if AClass = TGL3xMaterialName then
  begin
    if Length(Materials) < 1 then
      exit;
    if (Length(AName) = 0) or (AName = glsDEFAULTMATERIALNAME) then
      AName := cMatName;
    N := 1;
    while GetMaterialName(AName).GetValue <> glsDEFAULTMATERIALNAME do
    begin
      AName := AName + IntToHex(N, 2);
      Inc(N);
    end;
  end
  else if AClass = TGL3xTextureName then
  begin
    if Length(Textures) < 2 then
      exit;
    if (Length(AName) = 0) or (AName = glsDIFFUSEMAP) or (AName = glsNORMALMAP) then
      AName := cTexName;
    N := 1;
    while GetTextureName(AName).GetValue <> glsDIFFUSEMAP do
    begin
      AName := AName + IntToHex(N, 2);
      Inc(N);
    end;
  end
  else if AClass = TGL3xSamplerName then
  begin
    if Length(AName) = 0 then
      AName := cSmpName;
    N := 1;
    while GetSamplerName(AName) <> nil do
    begin
      AName := AName + IntToHex(N, 2);
      Inc(N);
    end;
  end;
end;

class procedure MaterialManager.FillMaterialNameList(var AList: TStringList);
var
  I: Integer;
begin
  try
    BeginWork;
    if not Assigned(AList) then
      AList := TStringList.Create;
    AList.Clear;
    for I := 0 to High(Materials) do
      if Assigned(Materials[I]) then
        AList.Add(Materials[I].Name.Value);
  finally
    EndWork;
  end;
end;

class procedure MaterialManager.FillTextureNameList(var AList: TStringList);
var
  I: Integer;
begin
  try
    BeginWork;
    if not Assigned(AList) then
      AList := TStringList.Create;
    AList.Clear;
    for I := 0 to High(Textures) do
      if Assigned(Textures[I]) then
        AList.Add(Textures[I].Name.Value);
  finally
    EndWork;
  end;
end;

class procedure MaterialManager.FillSamplerNameList(var AList: TStringList);
var
  I: Integer;
begin
  try
    BeginWork;
    if not Assigned(AList) then
      AList := TStringList.Create;
    AList.Clear;
    for I := 0 to High(Samplers) do
      if Assigned(Samplers[I]) then
        AList.Add(Samplers[I].Name.Value);
  finally
    EndWork;
  end;
end;

class procedure MaterialManager.PushMaterial(AMaterial: TGL3xMaterial);
var
  I: Integer;
begin
  SetLength(Materials, Length(Materials) + 1);
  I := High(Materials);
  AMaterial.Name.SetIndex(I);
  Materials[I] := AMaterial;
end;

type
  TAccessableTextureName = class(TGL3xTextureName)
  public
  end;

class procedure MaterialManager.PushTexture(ATexture: TGL3xTexture);
var
  I: Integer;
begin
  SetLength(Textures, Length(Textures) + 1);
  I := High(Textures);
  TAccessableTextureName(ATexture.Name).SetIndex(I);
  Textures[I] := ATexture;
end;

type
  TAccesableSamplerName = class(TGL3xSamplerName)
  public
  end;

class procedure MaterialManager.PushSampler(ASampler: TGL3xSampler);
var
  I: Integer;
begin
  SetLength(Samplers, Length(Samplers) + 1);
  I := High(Samplers);
  TAccesableSamplerName(ASampler.Name).SetIndex(I);
  Samplers[I] := ASampler;
end;

class function MaterialManager.CreateMaterial(AName: string; const AFileName: string): IGLName;
var
  newMat: TGL3xMaterial;
begin
  CheckCall;
  newMat := TGL3xMaterial.Create(nil);
  newMat.Name.Value := AName;
  newMat.Default;
  PushMaterial(newMat);
  if Length(AFileName) > 0 then
    newMat.SaveToFile(AFileName);
  Result := newMat.Name;
end;

class function MaterialManager.CreateTexture(AName: string; const AImportFile: string; const AFileName: string): IGLName;
var
  newTex: TGL3xTexture;
begin
  CheckCall;
  newTex := TGL3xTexture.Create(nil);
  newTex.Name.Value := AName;
  PushTexture(newTex);
  if Length(AImportFile) > 0 then
    newTex.ImportFromFile(AImportFile);
  if Length(AFileName) > 0 then
    newTex.SaveToFile(AFileName);
  Result := newTex.Name;
end;

class function MaterialManager.CreateSampler(AName: string; const AFileName: string): IGLName;
var
  newSmp: TGL3xSampler;
begin
  CheckCall;
  newSmp := TGL3xSampler.Create(nil);
  newSmp.Name.Value := AName;
  PushSampler(newSmp);
  if Length(AFileName) > 0 then
    newSmp.SaveToFile(AFileName);
  Result := newSmp.Name;
end;

class procedure MaterialManager.ApplyMaterial(const AName: IGLName;
  var ARci: TRenderContextInfo; AVariant: TMaterialVariant);
var
  vMaterial: TGL3xMaterial;
begin
  Assert(LastMaterial = nil);
  vMaterial := GetMaterial(AName);
  vMaterial.Apply(ARci, AVariant);
  LastMaterial := vMaterial;
end;

class function MaterialManager.UnApplyMaterial(var ARci: TRenderContextInfo): Boolean;
begin
  if Assigned(LastMaterial) then
  begin
    Result := LastMaterial.UnApply(ARci);
    LastMaterial := nil;
  end
  else
    Result := True;
end;

class procedure MaterialManager.ApplyTextureSampler(const ATextureName, ASamplerName: IGLName; AUniform: TGLSLUniform);
var
  vTexture: TGL3xTexture;
  vSampler: TGL3xSampler;
  A: TGLuint;
begin
  vTexture := GetTexture(ATextureName);
  A := vTexture.Apply(AUniform, not Assigned(ASamplerName));
  if Assigned(ASamplerName) then
  begin
    vSampler := GetSampler(ASamplerName);
    vSampler.Apply(A, vTexture.Target);
  end;
end;

{$ENDREGION}
{$REGION 'TShaderSample'}
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
    FTextureInInput := FTextureInInput or Value.FTextureInInput;
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
      GLSLType1F:
        Result := Result + 'float;';
      GLSLType2F:
        Result := Result + 'vec2;';
      GLSLType3F:
        Result := Result + 'vec3;';
      GLSLType4F:
        Result := Result + 'vec4;';
      GLSLType1I:
        Result := Result + 'int;';
      GLSLType2I:
        Result := Result + 'ivec2;';
      GLSLType3I:
        Result := Result + 'ivec3;';
      GLSLType4I:
        Result := Result + 'ivec4;';
      GLSLType1UI:
        Result := Result + 'uint;';
      GLSLType2UI:
        Result := Result + 'uvec2;';
      GLSLType3UI:
        Result := Result + 'uvec3;';
      GLSLType4UI:
        Result := Result + 'uvec4;';
      GLSLTypeVoid:
        Result := Result + 'void;';
      GLSLTypeVRec:
        Result := Result + 'vrec;';
      GLSLTypeFRec:
        Result := Result + 'frec;';
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
    if Length(FInputFromMatSys) = 0 then
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
    Result := GLSLTypeToString(Self.Output) + ' ' + Self.Name + '(' + AnsiString(FInputFromMatSys) + ');';
  end;
end;

function TShaderSample.FindShaderObject: Boolean;
var
  XMLMaterial, XMLSamples, XMLParagraph, XMLSample, XMLOverload: GLSXMLNode;
  I, p, U, L: Integer;
  aVariant, sInputs, aInputs, sOutput, aOutput, aUniforms, sUniforms: string;
  ins, temp: string;
  matVariant: TMaterialVariant;

  procedure CaseVariant;
  begin
    if aVariant = 'AllSurfProperties' then
      matVariant := matvarAllSurfProperies
    else
      Abort;
  end;

begin
  if Purpose = sspVariable then
    exit(True);
  Result := False;
  if Initialized and (Length(Category) > 0) and (Length(Name) > 0) then
  begin
    // Skip sample with no shader object
    if (Purpose = sspConstant) or (Purpose = sspVariable) then
      exit(True);

    if Name = MaterialSystem.Utility.Utility_ComponentMask then
      exit(True);

    sInputs := InputAsString;
    sOutput := string(GLSLTypeToString(Output));
    try
      XMLMaterial := MatSysDoc.DocumentElement;
      FindXMLNode(XMLMaterial, 'Samples', XMLSamples);
      if FindXMLNode(XMLSamples, string(Category), XMLParagraph) then
      begin
        if FindXMLNode(XMLParagraph, string(Name), XMLSample) then
        begin
          for I := 0 to XMLSample.ChildNodes.Count - 1 do
          begin
            XMLOverload := XMLSample.ChildNodes[I];

            if GetXMLAttribute(XMLOverload, 'Variant', aVariant) then
              CaseVariant
            else
              matVariant := matvarCommon;

            if not GetXMLAttribute(XMLOverload, 'Input', aInputs) then
              Abort;

            if not GetXMLAttribute(XMLOverload, 'Output', aOutput) then
              Abort;

            if not Result then
            begin
              if (sInputs = RemoveGLSLQualifier(aInputs)) and (sOutput = aOutput) then
              begin
                FInputFromMatSys := aInputs;
                if FInputFromMatSys <> 'void;' then
                begin
                  L := 65;
                  while True do
                  begin
                    p := Pos(';', FInputFromMatSys);
                    if p > 0 then
                    begin
                      ins := Char(L);
                      Inc(L);
                      if p < Length(FInputFromMatSys) then
                        ins := ins + ',';
                      FInputFromMatSys[p] := ' ';
                      Insert(ins, FInputFromMatSys, p + 1);
                      continue;
                    end;
                    break;
                  end;
                end
                else
                  FInputFromMatSys := 'void';
                Result := True;
              end;

              if not GetXMLAttribute(XMLOverload, 'ObjectIndex', temp) then
                continue;
              FObjectName[matVariant] := SampleShaderObjectNames[StrToInt(temp)];

              if GetXMLAttribute(XMLOverload, 'Uniforms', aUniforms) then
              begin
                U := 0;
                repeat
                  p := Pos(';', aUniforms);
                  if p > 0 then
                  begin
                    sUniforms := Copy(aUniforms, 0, p - 1);
                    Delete(aUniforms, 1, p);
                    UniformClasses[U] := TBaseShaderEnvironmentClass(FindClass(sUniforms));
                    Inc(U);
                  end
                  else
                    break;
                until Length(aUniforms) = 0;
              end;
            end; // for I, each overload

          end;
        end;
      end;
    except
      GLSLogger.LogFatalError(sBadMaterialConstr);
    end;
  end;
end;

function TShaderSample.CheckWithMaterialSystem: Boolean;
var
  XMLMaterial, XMLSamples, XMLParagraph, XMLSample, XMLOverload: GLSXMLNode;
  temp: string;
var
  aVariant, sInputs, aInputs, aOutput, ObjType, aUniforms, sUniforms: string;
  I, U, p: Integer;
  matVariant: TMaterialVariant;

  procedure CaseVariant;
  begin
    if aVariant = 'AllSurfProperties' then
      matVariant := matvarAllSurfProperies
    else
      Abort;
  end;

  procedure CaseOutput;
  begin
    if Mask = [] then
    begin
      aOutput := temp;
      if aOutput = 'void' then
        Output := GLSLTypeVoid
      else if aOutput = 'float' then
        Output := GLSLType1F
      else if aOutput = 'vec2' then
        Output := GLSLType2F
      else if aOutput = 'vec3' then
        Output := GLSLType3F
      else if aOutput = 'vec4' then
        Output := GLSLType4F
      else if aOutput = 'int' then
        Output := GLSLType1I
      else if aOutput = 'ivec2' then
        Output := GLSLType2I
      else if aOutput = 'ivec3' then
        Output := GLSLType3I
      else if aOutput = 'ivec4' then
        Output := GLSLType4I
      else if aOutput = 'lrec' then
        Output := GLSLTypeLRec;
    end
    else
      Output := MaskToGLSLType(Mask);
  end;

begin
  with MaterialManager do
    try
      BeginWork;

      if Purpose = sspVariable then
        exit(True);

      Output := GLSLTypeUndefined;
      Participate := [];
      Result := False;

      if Assigned(MatSysDoc) and (Length(Category) > 0) and (Length(Name) > 0) then
      begin

        sInputs := InputAsString;
        try
          XMLMaterial := MatSysDoc.DocumentElement;
          FindXMLNode(XMLMaterial, 'Samples', XMLSamples);
          if FindXMLNode(XMLSamples, string(Category), XMLParagraph) then
          begin
            if FindXMLNode(XMLParagraph, string(Name), XMLSample) then
            begin
              if GetXMLAttribute(XMLSample, 'ObjectType', ObjType) then
              begin
                Participate := [];
                if Pos('V', ObjType) > 0 then
                  Include(Participate, ptVertex);
                if Pos('F', ObjType) > 0 then
                  Include(Participate, ptFragment);
                if Pos('G', ObjType) > 0 then
                  Include(Participate, ptGeometry);
                if Pos('C', ObjType) > 0 then
                  Include(Participate, ptControl);
                if Pos('E', ObjType) > 0 then
                  Include(Participate, ptEvaluation);
              end;

              for I := 0 to XMLSample.ChildNodes.Count - 1 do
              begin
                XMLOverload := XMLSample.ChildNodes[I];

                if Purpose = sspConstant then
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
                  Output := MaskToGLSLType(Mask);
                  Result := True;
                  break;
                end;

                if GetXMLAttribute(XMLOverload, 'Variant', aVariant) then
                  CaseVariant
                else
                  matVariant := matvarCommon;

                if not GetXMLAttribute(XMLOverload, 'Input', aInputs) then
                  Abort;

                if sInputs = RemoveGLSLQualifier(aInputs) then
                begin
                  if not GetXMLAttribute(XMLOverload, 'ObjectIndex', temp) then
                    continue;
                  FObjectName[matVariant] := SampleShaderObjectNames[StrToInt(temp)];

                  if GetXMLAttribute(XMLOverload, 'Output', temp) then
                    CaseOutput
                  else
                    Abort;

                  if GetXMLAttribute(XMLOverload, 'Uniforms', aUniforms) then
                  begin
                    U := 0;
                    repeat
                      p := Pos(';', aUniforms);
                      if p > 0 then
                      begin
                        sUniforms := Copy(aUniforms, 0, p - 1);
                        Delete(aUniforms, 1, p);
                        UniformClasses[U] := TBaseShaderEnvironmentClass(FindClass(sUniforms));
                        Inc(U);
                      end
                      else
                        break;
                    until Length(aUniforms) = 0;
                  end;

                  Result := True;
                end; // for I - each overload

              end;
            end;
          end;
        except
          GLSLogger.LogFatalError(sBadMaterialConstr);
        end;
      end;
    finally
      EndWork;
    end;
end;

procedure TSampleGatherInfo.NewMaster;
begin
  New(Master);
  Master.Clear;
  SampleList.Add(Master);
end;

{$ENDREGION}
{$REGION 'TGL3xMaterial'}
// ------------------
// ------------------ TGL3xMaterial ------------------
// ------------------

function TGL3xMaterialName.GetInheritorClass: TGLAbstractNameClass;
begin
  Result := TGLAbstractNameClass(ClassType);
end;

function TGL3xMaterialName.GetManager: TGLSAbstractManagerClass;
begin
  Result := MaterialManager;
end;

constructor TGL3xMaterial.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FName := TGL3xMaterialName.Create;
  FName._AddRef;
  FLoaded := False;
  FBlendingMode := bmOpaque;
  FSampleList := TSampleList.Create;
  FVariants := [matvarCommon];
  if IsDesignTime then
    Include(FVariants, matvarAllSurfProperies);
end;

destructor TGL3xMaterial.Destroy;
begin
  FName.SetIndex(-1);
  FName._Release;
  FSourceDoc := nil;
  ClearSamples;
  ClearUniforms;
  ClearUnits;
  FSampleList.Destroy;
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

procedure TGL3xMaterial.DirectUse;
begin
  ShaderManager.UseProgram(FProgramVariants[matvarCommon].FProgram);
end;

procedure TGL3xMaterial.Initialize;
var
  I, J: Integer;
  PT: TGLSLProgramType;
  GLSLType: TGLSLDataType;
  pSample: PShaderSample;
  ProgramCodeSet: TProgramCodeSet;
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
          GLSLogger.LogError(Format('Invalid sample %s in material %s. Wrong samples connection.', [pSample.Name, FName.Value]));
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

  FillChar(ProgramCodeSet[ptVertex], SizeOf(TProgramCodeSet), $00);
  if not MaterialManager.GenerateShaderCode(FSampleList, ProgramCodeSet) then
  begin
    FLoaded := False;
    GLSLogger.LogError(Format('Material %s can''t generate shader program', [FName.Value]));
    if Name.GetIndex = 0 then
      Abort;
    Default;
    Initialize;
    exit;
  end;

  CreateUniforms;

  try
    try
      CreatePrograms(ProgramCodeSet);
    finally
      for PT := Low(TGLSLProgramType) to High(TGLSLProgramType) do
        ProgramCodeSet[PT].Destroy;
      if not IsDesignTime then
        ClearSamples;
      FLoaded := False;
    end;
  except
    on EAbort do
      exit;
  else
    raise;
  end;
end;

procedure TGL3xMaterial.Assign(Source: TPersistent);
var
  Mat: TGL3xMaterial;
  mStream: TMemoryStream;
  V: TMaterialVariant;
begin
  if Source is TGL3xMaterial then
  begin
    for V := Low(TMaterialVariant) to High(TMaterialVariant) do
      if Assigned(FProgramVariants[V].FProgram) then
        with ShaderManager do
          try
            BeginWork;
            DeleteShaderProgram(FProgramVariants[V].FProgram);
          finally
            EndWork;
          end;

    Mat := TGL3xMaterial(Source);
    ResourceName := Mat.ResourceName;
    FVariants := Mat.FVariants;
    FLightingModel := Mat.FLightingModel;
    FBlendingMode := Mat.FBlendingMode;
    FPolygonMode := Mat.FPolygonMode;
    FFaceCulling := Mat.FFaceCulling;

    if Mat.FSourceDoc <> nil then
    begin
      mStream := TMemoryStream.Create;
      try
        FSourceDoc := GLSNewXMLDocument;
{$IFNDEF FPC}
        Mat.FSourceDoc.SaveToStream(mStream);
        mStream.Seek(0, soBeginning);
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
    begin
      ClearSamples;
      ClearUniforms;
      ClearUnits;
      Default;
      exit;
    end;

    with MaterialManager do
      try
        BeginWork;
        CreateSamples;
      finally
        EndWork;
      end;
    CreateUniforms;
  end
  else
    inherited;
end;

procedure TGL3xMaterial.AssignTo(const AName: IGLName);
begin
  MaterialManager.GetMaterial(AName).Assign(Self);
end;

procedure TGL3xMaterial.AssignFrom(const AName: IGLName);
begin
  Assign(MaterialManager.GetMaterial(AName));
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
  for I := High(FUniforms) downto 0 do
    FUniforms[I].Free;
  SetLength(FUniforms, 0);
end;

procedure TGL3xMaterial.ClearUnits;
var
  I: Integer;
begin
  for I := High(FUnits) downto 0 do
  begin
    FUnits[I].SamplerName := nil;
    FUnits[I].TextureName := nil;
  end;
  SetLength(FUnits, 0);
end;

// LoadFromFile
//

procedure TGL3xMaterial.LoadFromFile(const fileName: string);
var
  fs: TStream;
begin
  if FileStreamExists(fileName) then
  begin
    fs := CreateFileStream(fileName, fmOpenRead);
    ResourceName := fileName;
    try
      LoadFromStream(fs);
    finally
      fs.Free;
    end;
  end
  else
  begin
    GLSLogger.LogError(Format('File %s not found', [fileName]));
    FLoaded := False;
  end;
end;

// SaveToFile
//

procedure TGL3xMaterial.SaveToFile(const fileName: string);
var
  fs: TStream;
begin
  fs := CreateFileStream(fileName, fmOpenWrite or fmCreate);
  ResourceName := fileName;
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
end;

// SaveToStream
//

procedure TGL3xMaterial.SaveToStream(stream: TStream);
var
  XMLMaterial, XMLSamples, XMLSample, XMLRefs, XMLRef: GLSDOMNode;
  I, J: Integer;
  pSample: PShaderSample;
begin
  if (Length(ResourceName) = 0) and (stream is TFileStream) then
    ResourceName := TFileStream(stream).fileName;
  if Assigned(FSourceDoc) then
  begin
{$IFDEF FPC}
    XMLMaterial := FSourceDoc.DocumentElement;
{$ELSE}
    XMLMaterial := FSourceDoc.DOMDocument.DocumentElement;
{$ENDIF}
    if FSampleList.Count > 0 then
    begin
      for I := 0 to XMLMaterial.ChildNodes.Length - 1 do
      begin
        XMLSamples := XMLMaterial.ChildNodes.item[I];
        if XMLSamples.NodeName = 'Samples' then
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

        SetXMLAttribute(XMLSample, 'Participate', GLSLPartToStr(pSample.Participate));

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
          sspBuildIn:
            begin
              SetXMLAttribute(XMLSample, 'Purpose', 'B');
            end;
        end;

        if pSample.Mask <> [] then
          SetXMLAttribute(XMLSample, 'Mask', pSample.MaskAsString);

        XMLRefs := CreateDOMNode(XMLSample, 'References');

        if pSample.Purpose = sspBuildIn then
        begin
          for J := 0 to High(pSample.FInputRef) do
            if pSample.Input[J] <> GLSLTypeUndefined then
            begin
              XMLRef := CreateDOMNode(XMLRefs, 'R' + IntToStr(J));
              SetXMLAttribute(XMLRef, 'Input', string(GLSLTypeToString(pSample.Input[J])));
            end;
        end
        else
          for J := 0 to High(pSample.FInputRef) do
            if Assigned(pSample.FInputRef[J]) then
            begin
              XMLRef := CreateDOMNode(XMLRefs, 'R' + IntToStr(J));
              SetXMLAttribute(XMLRef, 'Index', IntToStr(FSampleList.IndexOf(pSample.FInputRef[J])));
              SetXMLAttribute(XMLRef, 'Input', string(GLSLTypeToString(pSample.Input[J])));
            end;
      end;
    end;
{$IFNDEF FPC}
    FSourceDoc.SaveToStream(stream);
{$ELSE}
    WriteXMLFile(FSourceDoc, stream);
{$ENDIF}
  end;
end;

// LoadFromStream
//

procedure TGL3xMaterial.LoadFromStream(stream: TStream);
begin
  if (Length(ResourceName) = 0) and (stream is TFileStream) then
    ResourceName := TFileStream(stream).fileName;
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
  temp, rName: string;

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

  try
    ClearSamples;
    ClearUniforms;
    XMLMaterial := FSourceDoc.DocumentElement;
    if not FindXMLNode(XMLMaterial, 'Samples', XMLSamples) then
      Abort;

    if GetXMLAttribute(XMLMaterial, 'LightingModel', temp) then
    begin
      LightingModel := lmPhong;
      while CompareText(temp, cLightingModel[LightingModel]) <> 0 do
        Inc(FLightingModel);
    end;

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

    ClearUnits;
    if GetXMLAttribute(XMLMaterial, 'TextureUnits', temp) then
      repeat
        K := Pos(';', temp);
        if K = 0 then
          break;
        rName := Copy(temp, 1, K - 1);
        Delete(temp, 1, K);
        SetLength(FUnits, Length(FUnits) + 1);
        FUnits[High(FUnits)].TextureName := MaterialManager.GetTextureName(rName);
        K := Pos(';', temp);
        rName := Copy(temp, 1, K - 1);
        Delete(temp, 1, K);
        FUnits[High(FUnits)].SamplerName := MaterialManager.GetSamplerName(rName);
      until Length(temp) = 0;

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
        Include(pSample.Participate, ptVertex)
      else if Pos('F', temp) > 0 then
        Include(pSample.Participate, ptFragment)
      else if Pos('G', temp) > 0 then
        Include(pSample.Participate, ptGeometry)
      else if Pos('C', temp) > 0 then
        Include(pSample.Participate, ptControl)
      else if Pos('E', temp) > 0 then
        Include(pSample.Participate, ptEvaluation)
      else
        Abort;

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
        else if temp = 'B' then
        begin
          pSample.Purpose := sspBuildIn;
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
          if GetXMLAttribute(XMLRef, 'Index', temp) then
          begin
            Val(temp, K, err);
            pSample.FInputRef[J] := FSampleList[K];
          end;
          GetXMLAttribute(XMLRef, 'Input', temp);
          pSample.Input[J] := StrToGLSLType(AnsiString(temp));
        end;

    end;
    FLoaded := True;
  except
    if FName.Value = glsDEFAULTMATERIALNAME then
      GLSLogger.LogFatalError('Error when loading default material')
    else
      GLSLogger.LogError('Error when loading material ' + string(FName.Value));
  end;
end;

procedure TGL3xMaterial.CreateUniforms;
var
  I, J, K, N: Integer;
  pSample: PShaderSample;
  Duplicate: Boolean;
begin
  with MaterialManager do
    try
      BeginWork;
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
              FUniforms[N] := pSample.UniformClasses[J].Create(FUnits);
            end;
          end;
      end;
    finally
      EndWork;
    end;
end;

procedure TGL3xMaterial.CreatePrograms(const AProgramCodeSet: TProgramCodeSet);
const
  cSufixes: array[TGLSLProgramType] of string =
    ('_VP', '_GP', '_FP', '_CP', '_EP');
var
  I: Integer;
  PT: TGLSLProgramType;
  pSample: PShaderSample;
  Mask: TGLSLProgramTypes;
  Code: AnsiString;
  V: TMaterialVariant;

  function GetObject: IGLName;
  begin
    if Assigned(pSample.FObjectName[V]) then
      Result := pSample.FObjectName[V]
    else
      Result := pSample.FObjectName[matvarCommon];
  end;

begin
  with ShaderManager do
    try
      BeginWork;
      for V := Low(TMaterialVariant) to High(TMaterialVariant) do
        if V in FVariants then
        begin
          // Define shder objects
          Mask := [];
          for PT := Low(TGLSLProgramType) to High(TGLSLProgramType) do
          begin
            Code := AnsiString(AProgramCodeSet[PT].Text);
            if Length(Code) > 0 then
            begin
              DefineShaderObject(FProgramVariants[V].FShaders[PT], Code, [PT],
                Name.Value + cSufixes[PT]);
              Include(Mask, PT);
            end;
          end;

          DefineShaderProgram(FProgramVariants[V].FProgram, Mask,
            Name.Value + '_' + cMaterialVariant[V]);

          // Attach shader objects of samples
          // Workaround AMD bug of Catalist 10.12
          if FLightingModel = lmPhong then
          begin
            for I := FSampleList.Count - 1 downto 0 do
            begin
              pSample := FSampleList[I];
              AttachShaderObjectToProgram(GetObject, FProgramVariants[V].FProgram);
            end;
          end
          else
          begin
            for I := 0 to FSampleList.Count - 1 do
            begin
              pSample := FSampleList[I];
              AttachShaderObjectToProgram(GetObject, FProgramVariants[V].FProgram);
            end;
          end;

          // Attach objects to program
          for PT := Low(TGLSLProgramType) to High(TGLSLProgramType) do
            if PT in Mask then
              AttachShaderObjectToProgram(FProgramVariants[V].FShaders[PT],
                FProgramVariants[V].FProgram);

          LinkShaderProgram(FProgramVariants[V].FProgram);
        end;
    finally
      EndWork;
    end;
end;

procedure TGL3xMaterial.Apply(var ARci: TRenderContextInfo;
  AVariant: TMaterialVariant);
var
  I: Integer;
  bReady: Boolean;
  bFaceCull: Boolean;
begin
  if not ShaderManager.IsProgramLinked(FProgramVariants[AVariant].FProgram) then
  begin
    if FLoaded then
    begin
      // On the fly initialization
      Initialize;
      bReady := ShaderManager.IsProgramLinked(FProgramVariants[AVariant].FProgram);
    end
    else
      bReady := False;
  end
  else
    bReady := True;

  if bReady then
  begin
    // Apply Shader
    ShaderManager.UseProgram(FProgramVariants[AVariant].FProgram);

    // Apply Uniforms
    for I := 0 to High(FUniforms) do
      FUniforms[I].Apply;

    // Apply States
    with CurrentGLContext.GLStates do
    begin
      PolygonMode := FPolygonMode;
      // Setup face culling
      case FFaceCulling of
        fcBufferDefault: bFaceCull := ARci.bufferFaceCull;
        fcCull: bFaceCull := True;
      else
        bFaceCull := False;
      end;
      if bFaceCull then
        ARci.GLStates.Enable(stCullFace)
      else
        ARci.GLStates.Disable(stCullFace);
      // Setup blending
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

      Enable(stDepthTest);
      DepthWriteMask := True;
    end;
    exit;
  end;

  // Force-major
  GLSLogger.LogError(Format('Applying of material %s aborted.', [FName.Value]));
  Abort;
end;

function TGL3xMaterial.UnApply(var ARci: TRenderContextInfo): Boolean;
begin
  if not CurrentGLContext.GLStates.ForwardContext then
    ShaderManager.UseFixedFunctionPipeline;
  Result := True;
end;

function TGL3xMaterial.GetProgram(AVariant: TMaterialVariant): IGLName;
begin
  Result := FProgramVariants[AVariant].FProgram;
end;

// Capabilities
//

class function TGL3xMaterial.Capabilities: TDataFileCapabilities;
begin
  Result := [dfcRead, dfcWrite];
end;

{$ENDREGION}

{$IFDEF GLS_EXPERIMENTAL}

initialization

  MaterialManager.Initialize;

finalization

  MaterialManager.Finalize;

{$ENDIF GLS_EXPERIMENTAL}

end.

