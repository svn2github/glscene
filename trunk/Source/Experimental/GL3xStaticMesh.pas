unit GL3xStaticMesh;

interface

{$I GLScene.inc}

uses
  {$IFDEF FPC}
  LCLVersion,
  LResources,
  {$ENDIF}
  Classes,
  SysUtils,
  BaseClasses,
  GLCrossPlatform,
  ApplicationFileIO,
  GLState,
  GLShaderManager,
  GL3xMaterial,
  GL3xMesh,
  GLSGenerics;

const
  HackHackHack = 0;

{$IFDEF FPC}
{$IF (LCL_RELEASE < 31)}
{$DEFINE GLS_GENERIC_PREFIX}
{$IFEND}
{$ENDIF}

type

  // TGL3xStaticMeshBuilder
  //
  TGL3xStaticMeshBuilder = class(TGLAbstractMeshBuilder)
  public
    { Public Declarations }
    destructor Destroy; override;
  end;


  {: Design time class to view information
     about mesh face group. }

  // TGLMeshFaceGroup
  //
  TGLMeshFaceGroup = class(TPersistent)
  protected
    { Protected Declarations }
    FOwner: TPersistent;
    FShadowCasting: Boolean;
    FIndex: Integer;
    FDescRef: PFaceGroupDesc;

    function GetMaterialName: string; virtual;
    procedure SetMaterialName(const Value: string); virtual;
    function GetName: string;
    procedure SetName(const Value: string);
    function GetPrimitiveType: TGLMeshPrimitive;
  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent);
    destructor Destroy; override;
  published
    { Published Declarations }
    property EnabledShadowCasting: Boolean
      read FShadowCasting write FShadowCasting default True;
    property Material: string
      read GetMaterialName write SetMaterialName;
    property Name: string
      read GetName write SetName;
    property PrimitiveType: TGLMeshPrimitive read GetPrimitiveType;
  end;

  TGLMeshFaceGroupList = {$IFDEF GLS_GENERIC_PREFIX}specialize{$ENDIF}
    GList<TGLMeshFaceGroup>;

  {: Design time class to view information
     about mesh level of detail. }

  TGLMeshLOD = class(TPersistent)
  protected
    { Protected Declarations }
    FMesh: TGLAbstractMesh;
    FFaceGroupList: TGLMeshFaceGroupList;
    FFaceGroupIndex: Integer;
    FIndex: Byte;

    function GetFaceGroupName: string;
    procedure SetFaceGroupName(const AValue: string);
    function GetFaceGroup: TGLMeshFaceGroup;
    procedure SetFaceGroup(const AValue: TGLMeshFaceGroup);
  public
    { Public Declarations }
    constructor Create(AOwner: TGLAbstractMesh); reintroduce;
    destructor Destroy; override;
    procedure FillFaceGroupNameList(Proc: TGetStrProc);
  published
    { Published Declarations }
    property __FaceGroup__: string read GetFaceGroupName write SetFaceGroupName;
    property FaceGroup: TGLMeshFaceGroup read GetFaceGroup write SetFaceGroup;
  end;

  TGLMeshLODList = {$IFDEF GLS_GENERIC_PREFIX}specialize{$ENDIF}
    GList<TGLMeshLOD>;

  // TGL3xStaticMesh
  //
  TGL3xStaticMesh = class(TGLAbstractMesh)
  protected
    { Protected Declarations }
    FLODList: TGLMeshLODList;
    FLODChainRevision: Integer;
    FLODIndex: Integer;
    FLODDistanceRatio: Single;
    FLODMaxRange: Single;
    FBuilder: TGL3xStaticMeshBuilder;
    function GetBuilder: TObject; override;
    function GetStaticBuilder: TGL3xStaticMeshBuilder;
    // Paralell tasks
    procedure ComputeNormalsTask; stdcall;
    procedure ComputeTexCoordTask; stdcall;
    procedure ComputeTangensTask; stdcall;
    // LODs
    function GetLODIndex: string;
    procedure SetLODIndex(AValue: string);
    function GetLOD: TGLMeshLOD;
    procedure SetLOD(AValue: TGLMeshLOD);
    procedure CheckLODRevison;
    procedure RebuildLODChain;
    procedure ClearLODChain;
    function GetLODCount: Byte;
    function GetFaceGroupCount(ALOD: Byte): Integer;
    function GetFaceGroupDesc(
      ALOD: Byte; AFaceGroup: Integer): PFaceGroupDesc;
  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure LoadFromFile(const fileName: string); override;
    procedure SaveToFile(const fileName: string); override;
    procedure LoadFromStream(stream: TStream); override;
    procedure SaveToStream(stream: TStream); override;

    procedure ImportFromFile(const fileName: string); override;
    procedure ExportToFile(const fileName: string); override;

    {: Notify mesh about requesting attribute. }
    procedure AttributeRequest(const Attr: TGLSLAttribute); virtual;

    procedure FillLODIndexList(Proc: TGetStrProc);

    property StaticBuilder: TGL3xStaticMeshBuilder read GetStaticBuilder;
  published
    property __LOD__: string
      read GetLODIndex write SetLODIndex;
    property LOD: TGLMeshLOD
      read GetLOD write SetLOD;
    property LODDistanceRatio: Single
      read FLODDistanceRatio write FLODDistanceRatio stored False;
    property LODMaxRange: Single
      read FLODMaxRange write FLODMaxRange stored False;
  end;

  TGL3xInstanceDataBuilder = class(TGL3xStaticMeshBuilder)
  public
    { Public Declarations }
  end;

  TGL3xInstanceData = class(TGLAbstractMesh)
  protected
    { Protected Declarations }
    FBuilder: TGL3xInstanceDataBuilder;
    function GetBuilder: TObject; override;
    function GetInstanceDataBuilder: TGL3xInstanceDataBuilder;

  public
    { Public Declarations }
    destructor Destroy; override;
    property InstanceDataBuilder: TGL3xInstanceDataBuilder read GetInstanceDataBuilder;
  end;

  // TGLXMLMeshFileIO
  //
  {: Common mesh file format just in case. }
  TGLMeshXMLFileIO = class(TGLAbstractMeshFileIO)
  public
    { Public Declarations }
    class function Capabilities: TDataFileCapabilities; override;
    class procedure LoadFromStream(AStream: TStream; ABuilder: TGLAbstractMeshBuilder); override;
    class procedure SaveToStream(AStream: TStream; AMesh: TGLAbstractMesh); override;
  end;

implementation

uses
{$IFDEF FPC}
  FileUtil,
{$ENDIF}
  GLSCrossXML,
  GLStrings,
  GLSLog,
  OpenGLTokens,
  GLContext,
  GeometryBB;

const
  cMeshPrimitive: array[TGLMeshPrimitive] of string = (
    'NOPRIMITIVE',
    'TRIANGLES',
    'TRIANGLE_STRIP',
    'TRIANGLE_FAN',
    'POINTS',
    'LINES',
    'LINE_LOOP',
    'LINE_STRIP',
    'LINES_ADJACENCY',
    'LINE_STRIP_ADJACENCY',
    'TRIANGLES_ADJACENCY',
    'TRIANGLE_STRIP_ADJACENCY',
    'PATCHES'
    );

class function TGLMeshXMLFileIO.Capabilities: TDataFileCapabilities;
begin
  Result := [dfcRead, dfcWrite];
end;

class procedure TGLMeshXMLFileIO.LoadFromStream(AStream: TStream; ABuilder: TGLAbstractMeshBuilder);
var
  lvStaticMesh: TGL3xStaticMesh;

  procedure DoLoad;
  var
    MeshDoc: GLSXMLDocument;
    XMLMesh, XMLGeometry,
    XMLAttribs, XMLAttrib: GLSXMLNode;
    XMLLODs, XMLLOD: GLSXMLNode;
    XMLFaceGroups, XMLFaceGroup: GLSXMLNode;
    A, L, I, C, err: Integer;
    P: TGLMeshPrimitive;
    ADiv: TGLuint;
  	temp: string;
  begin
    err := 0;
    MeshDoc := GLSNewXMLDocument;
    ReadXMLFile(MeshDoc, AStream);
    XMLMesh := MeshDoc.DocumentElement;

  	if not FindXMLNode(XMLMesh, 'Geometry', XMLGeometry) then
    	exit;
  	if not FindXMLNode(XMLGeometry, 'VertexAttributes', XMLAttribs) then
    	exit;
    A := GLS_VERTEX_ATTR_NUM - 1;
    for I := 0 to XMLAttribs.ChildNodes.Count - 1 do
    begin
    	XMLAttrib := XMLAttribs.ChildNodes[I];
      GetXMLAttribute(XMLAttrib, 'Name', temp);
      lvStaticMesh.FAttributes[A] := TGLSLAttribute.GetAttribute(temp);
      GetXMLAttribute(XMLAttrib, 'Type', temp);
      lvStaticMesh.FDataFormat[A] := StrToGLSLType(AnsiString(temp));
      GetXMLAttribute(XMLAttrib, 'Divisor', temp);
      Val(temp, ADiv, err);
      lvStaticMesh.FAttributeDivisor[A] := ADiv;
      GetXMLAttribute(XMLAttrib, 'Count', temp);
      Val(temp, C, err);
      lvStaticMesh.FAttributeArrays[A].Count := C;
      GetXMLText(XMLAttrib, temp);
      HexToBin(PChar(temp), PAnsiChar(lvStaticMesh.FAttributeArrays[A].List),
      	lvStaticMesh.FAttributeArrays[A].DataSize);
      Dec(A);
    end;

    if FindXMLNode(XMLGeometry, 'Elements', XMLAttrib) then
    begin
    	lvStaticMesh.FHasIndices := True;
      GetXMLAttribute(XMLAttrib, 'Count', temp);
      Val(temp, C, err);
      lvStaticMesh.FElementBuffer.Count := C;
      GetXMLText(XMLAttrib, temp);
      HexToBin(PChar(temp), PAnsiChar(lvStaticMesh.FElementBuffer.List),
      	lvStaticMesh.FElementBuffer.DataSize);
    end;

  	if not FindXMLNode(XMLGeometry, 'LODs', XMLLODs) then
    	exit;

    // Clear previous LOD info
    for L := 0 to 255 do
      lvStaticMesh.FLODFaceGroupMap[L] := nil;

    for L := 0 to XMLLODs.ChildNodes.Count - 1 do
    begin
      XMLLOD := XMLLODs.ChildNodes[L];
      if not FindXMLNode(XMLLODs, 'FaceGroups', XMLFaceGroups) then
        continue;
      SetLength(lvStaticMesh.FLODFaceGroupMap[L], XMLFaceGroups.ChildNodes.Count);
      for I := 0 to XMLFaceGroups.ChildNodes.Count - 1 do
      begin
        XMLFaceGroup := XMLFaceGroups.ChildNodes[I];
        GetXMLAttribute(XMLFaceGroup, 'Name', temp);
        lvStaticMesh.FLODFaceGroupMap[L][I].Name := temp;
        GetXMLAttribute(XMLFaceGroup, 'VertexOffset', temp);
        Val(temp, lvStaticMesh.FLODFaceGroupMap[L][I].VertexOffset, err);
        GetXMLAttribute(XMLFaceGroup, 'VertexCount', temp);
        Val(temp, lvStaticMesh.FLODFaceGroupMap[L][I].VertexCount, err);
        GetXMLAttribute(XMLFaceGroup, 'ElementOffset', temp);
        Val(temp, lvStaticMesh.FLODFaceGroupMap[L][I].ElementOffset, err);
        GetXMLAttribute(XMLFaceGroup, 'ElementCount', temp);
        Val(temp, lvStaticMesh.FLODFaceGroupMap[L][I].ElementCount, err);
        GetXMLAttribute(XMLFaceGroup, 'PrimitiveType', temp);
        for P := Low(TGLMeshPrimitive) to High(TGLMeshPrimitive) do
        begin
          if cMeshPrimitive[P] = temp then
          begin
            lvStaticMesh.FLODFaceGroupMap[L][I].PrimitiveType := P;
            break;
          end;
        end;
        GetXMLAttribute(XMLFaceGroup, 'Material', temp);
        with MaterialManager do
        try
          BeginWork;
          lvStaticMesh.FLODFaceGroupMap[L][I].Material := GetMaterialName(temp);
        finally
          EndWork;
        end;
      end;
    end;

    GetXMLAttribute(XMLGeometry, 'RestartIndex', temp);
    Val(temp, lvStaticMesh.FRestartIndex, err);

    ReleaseXMLDocument(MeshDoc);
  end;

begin
  if ABuilder is TGL3xStaticMeshBuilder then
  begin
    lvStaticMesh := TGL3xStaticMesh(TGL3xStaticMeshBuilder(ABuilder).FMesh);
    with ABuilder do
    begin
      BeginMeshAssembly;
      Clear;
      DoLoad;
      EndMeshAssembly;
    end;
  end;
end;

class procedure TGLMeshXMLFileIO.SaveToStream(AStream: TStream; AMesh: TGLAbstractMesh);
var
  lvStaticMesh: TGL3xStaticMesh;
  MeshDoc: GLSXMLDocument;
  XMLMesh, XMLGeometry,
  XMLAttribs, XMLAttrib: GLSDOMNode;
  XMLLODs, XMLLOD: GLSDOMNode;
  XMLFaceGroups, XMLFaceGroup: GLSDOMNode;
  A, I: Integer;
  L: Byte;
  temp: string;
begin
  if AMesh is TGL3xStaticMesh then
  begin
    lvStaticMesh := TGL3xStaticMesh(AMesh);
    MeshDoc := GLSNewXMLDocument;
  {$IFDEF FPC}
    XMLMesh := MeshDoc.CreateElement('TGL3xMesh');
    MeshDoc.AppendChild(XMLMesh);
  {$ELSE}
    XMLMesh := MeshDoc.DOMDocument.CreateElement('TGL3xMesh');
    MeshDoc.DOMDocument.AppendChild(XMLMesh);
  {$ENDIF}
    XMLGeometry := CreateDOMNode(XMLMesh, 'Geometry');
    // Save attributes
    XMLAttribs := CreateDOMNode(XMLGeometry, 'VertexAttributes');
    for A := GLS_VERTEX_ATTR_NUM - 1 downto 0 do
    begin
      if Assigned(lvStaticMesh.FAttributes[A]) then
      begin
        XMLAttrib := CreateDOMNode(XMLAttribs, 'VertexAttribute');
        SetXMLAttribute(XMLAttrib, 'Name',
          lvStaticMesh.FAttributes[A].Name);
        SetXMLAttribute(XMLAttrib, 'Type',
          string(GLSLTypeToString(lvStaticMesh.FDataFormat[A])));
        SetXMLAttribute(XMLAttrib, 'Divisor',
          IntToStr(lvStaticMesh.FAttributeDivisor[A]));
        SetXMLAttribute(XMLAttrib, 'Count',
          IntToStr(lvStaticMesh.FAttributeArrays[A].Count));
        SetLength(temp, 2*lvStaticMesh.FAttributeArrays[A].DataSize);
        BinToHex(PAnsiChar(lvStaticMesh.FAttributeArrays[A].List),
          PChar(temp), lvStaticMesh.FAttributeArrays[A].DataSize);
        SetXMLText(XMLAttrib, temp);
      end;
    end;

    if lvStaticMesh.FHasIndices then
    begin
      // Save element buffer
      XMLAttrib := CreateDOMNode(XMLGeometry, 'Elements');
      SetXMLAttribute(XMLAttrib, 'Count',
        IntToStr(lvStaticMesh.FElementBuffer.Count));
      SetLength(temp, 2*lvStaticMesh.FElementBuffer.DataSize);
      BinToHex(PAnsiChar(lvStaticMesh.FElementBuffer.List),
        PChar(temp), lvStaticMesh.FElementBuffer.DataSize);
      SetXMLText(XMLAttrib, temp);
    end;

    // Save draw portions info
    XMLLODs := CreateDOMNode(XMLGeometry, 'LODs');
    for L := 0 to 255 do
    begin
      if Length(lvStaticMesh.FLODFaceGroupMap[L]) = 0 then
        break;
      XMLLOD := CreateDOMNode(XMLLODs, 'LOD');
      XMLFaceGroups := CreateDOMNode(XMLLODs, 'FaceGroups');
      for I := 0 to High(lvStaticMesh.FLODFaceGroupMap[L]) do
      begin
        XMLFaceGroup := CreateDOMNode(XMLFaceGroups, 'FaceGroup');
        SetXMLAttribute(XMLFaceGroup, 'Name',
          lvStaticMesh.FLODFaceGroupMap[L][I].Name);
        SetXMLAttribute(XMLFaceGroup, 'VertexOffset',
          IntToStr(lvStaticMesh.FLODFaceGroupMap[L][I].VertexOffset));
        SetXMLAttribute(XMLFaceGroup, 'VertexCount',
          IntToStr(lvStaticMesh.FLODFaceGroupMap[L][I].VertexCount));
        SetXMLAttribute(XMLFaceGroup, 'ElementOffset',
          IntToStr(lvStaticMesh.FLODFaceGroupMap[L][I].ElementOffset));
        SetXMLAttribute(XMLFaceGroup, 'ElementCount',
          IntToStr(lvStaticMesh.FLODFaceGroupMap[L][I].ElementCount));
        SetXMLAttribute(XMLFaceGroup, 'PrimitiveType',
          cMeshPrimitive[lvStaticMesh.FLODFaceGroupMap[L][I].PrimitiveType]);
        SetXMLAttribute(XMLFaceGroup, 'Material',
          lvStaticMesh.FLODFaceGroupMap[L][I].Material.GetValue);
      end;
    end;

    SetXMLAttribute(XMLGeometry, 'RestartIndex',
        IntToStr(lvStaticMesh.FRestartIndex));

    WriteXMLFile(MeshDoc, AStream);
    ReleaseXMLDocument(MeshDoc);
  end;
end;

{$REGION 'TGL3xStaticMeshBuilder'}
// ------------------
// ------------------ TGL3xStaticMeshBuilder ------------------
// ------------------

destructor TGL3xStaticMeshBuilder.Destroy;
begin
  if Assigned(FMesh) then
    TGL3xStaticMesh(FMesh).FBuilder := nil;
  inherited;
end;

{$REGION 'TGL3xStaticMeshBuilder'}

{$REGION 'TGLMeshFaceGroup'}
// ------------------
// ------------------ TGLMeshFaceGroup ------------------
// ------------------

constructor TGLMeshFaceGroup.Create(AOwner: TPersistent);
begin
  inherited Create;
  FOwner := AOwner;
  FShadowCasting := True;
  FDescRef := nil;
end;

destructor TGLMeshFaceGroup.Destroy;
begin
  inherited;
end;

function TGLMeshFaceGroup.GetMaterialName: string;
begin
  if Assigned(FDescRef) then
    Result := FDescRef.Material.GetValue
  else
    Result := glsDEFAULTMATERIALNAME;
end;

procedure TGLMeshFaceGroup.SetMaterialName(const Value: string);
begin
  if Assigned(FDescRef) then
    with MaterialManager do
      try
        BeginWork;
        FDescRef.Material := GetMaterialName(Value);
      finally
        EndWork;
      end;
end;

function TGLMeshFaceGroup.GetName: string;
begin
  if Assigned(FDescRef) then
    Result := FDescRef.Name
  else
    Result := Format('FaceGroup%d', [TGLMeshLOD(FOwner).FFaceGroupList.IndexOf(Self)]);
end;

procedure TGLMeshFaceGroup.SetName(const Value: string);
begin
  if Assigned(FDescRef) then
    FDescRef.Name := Value;
end;

function TGLMeshFaceGroup.GetPrimitiveType: TGLMeshPrimitive;
begin
  if Assigned(FDescRef) then
    Result := FDescRef.PrimitiveType
  else
    Result := mpNOPRIMITIVE;
end;
{$REGION 'TGLMeshLOD'}

{$REGION 'TGLMeshLOD'}
// ------------------
// ------------------ TGLMeshLOD ------------------
// ------------------

constructor TGLMeshLOD.Create(AOwner: TGLAbstractMesh);
begin
  inherited Create;
  FMesh := AOwner;
  FFaceGroupList := TGLMeshFaceGroupList.Create;
end;

destructor TGLMeshLOD.Destroy;
var
  I: Integer;
begin
  for I := 0 to FFaceGroupList.Count - 1 do
    FFaceGroupList[I].Free;
  FFaceGroupList.Free;
  inherited;
end;

procedure TGLMeshLOD.FillFaceGroupNameList(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to FFaceGroupList.Count - 1 do
    Proc(FFaceGroupList[I].GetName);
end;

function TGLMeshLOD.GetFaceGroupName: string;
begin
  Result := FFaceGroupList[FFaceGroupIndex].GetName;
end;

procedure TGLMeshLOD.SetFaceGroupName(const AValue: string);
var
  I: Integer;
begin
  FFaceGroupIndex := 0;
  for I := 0 to FFaceGroupList.Count - 1 do
    if FFaceGroupList[I].GetName = AValue then
    begin
      FFaceGroupIndex := I;
      break;
    end;
end;

function TGLMeshLOD.GetFaceGroup: TGLMeshFaceGroup;
begin
  Result := FFaceGroupList[FFaceGroupIndex];
end;

procedure TGLMeshLOD.SetFaceGroup(const AValue: TGLMeshFaceGroup);
begin
  FFaceGroupList[FFaceGroupIndex] := AValue;
end;

{$ENDREGION 'TGLMeshLOD'}

{$REGION 'TGL3xStaticMesh'}
// ------------------
// ------------------ TGL3xStaticMesh ------------------
// ------------------

constructor TGL3xStaticMesh.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FLODChainRevision := -1;
end;

destructor TGL3xStaticMesh.Destroy;
begin
  inherited;
  FBuilder.Free;
  ClearLODChain;
  FLODList.Free;
end;

function TGL3xStaticMesh.GetBuilder: TObject;
begin
  if FBuilder = nil then
  begin
    FBuilder := TGL3xStaticMeshBuilder.Create(Self);
  end;
  Result := FBuilder;
end;

function TGL3xStaticMesh.GetStaticBuilder: TGL3xStaticMeshBuilder;
begin
  Result := TGL3xStaticMeshBuilder(GetBuilder);
end;

procedure TGL3xStaticMesh.LoadFromFile(const fileName: string);
begin
  inherited;
end;

procedure TGL3xStaticMesh.SaveToFile(const fileName: string);
begin
  inherited;
end;

procedure TGL3xStaticMesh.LoadFromStream(stream: TStream);
var
  V, Temp, L, F, A: Integer;
  ws: WideString;
begin
  WaitParallelTask;
  stream.ReadBuffer(V, SizeOf(Integer)); // Version
  if V > 1 then
  begin
    GLSLogger.LogErrorFmt(glsUnknownArchive,
      [Self.ClassName, V]);
    Abort;
  end;

  for L := 0 to 255 do
    FLODFaceGroupMap[L] := nil;
  for A := GLS_VERTEX_ATTR_NUM - 1 downto 0 do
  begin
    FAttributes[A] := nil;
    FDataFormat[A] := GLSLTypeVoid;
    FAttributeArrays[A].Clear;
    FAttributeDivisor[A] := 0;
  end;
  FHasIndices := False;
  FElementBuffer.Clear;
  FElementBufferWithAdjacency.Clear;
  FTriOnlyBufferNeedUpdate := True;

  stream.ReadBuffer(Temp, SizeOf(Integer));
  FBlank := Boolean(Temp);

  stream.ReadBuffer(Temp, SizeOf(Integer));
  for A := GLS_VERTEX_ATTR_NUM - 1 downto GLS_VERTEX_ATTR_NUM - Temp do
  begin
    stream.ReadBuffer(Temp, SizeOf(Integer));
    SetLength(ws, Temp);
    stream.ReadBuffer(ws[1], Temp * SizeOf(WideChar));
    FAttributes[A] := TGLSLAttribute.GetAttribute(string(ws));
    stream.ReadBuffer(Temp, SizeOf(Integer));
    FDataFormat[A] := TGLSLDataType(Temp);
    stream.ReadBuffer(Temp, SizeOf(Integer));
    FAttributeArrays[A].Count := Temp;
    if Temp > 0 then
      stream.ReadBuffer(FAttributeArrays[A].List^, FAttributeArrays[A].DataSize);
    stream.ReadBuffer(FAttributeDivisor[A], SizeOf(TGLuint));
  end;

  if V = 0 then
  begin
    L := 0;
    F := 0;
    SetLength(FLODFaceGroupMap[L], 1);
    stream.ReadBuffer(FLODFaceGroupMap[L][F].VertexOffset, SizeOf(Integer));
    stream.ReadBuffer(FLODFaceGroupMap[L][F].VertexCount, SizeOf(Cardinal));
    stream.ReadBuffer(FLODFaceGroupMap[L][F].ElementOffset, SizeOf(Cardinal));
    stream.ReadBuffer(FLODFaceGroupMap[L][F].ElementCount, SizeOf(Cardinal));
    stream.ReadBuffer(FLODFaceGroupMap[L][F].TriOnlyElementOffset, SizeOf(Cardinal));
    stream.ReadBuffer(FLODFaceGroupMap[L][F].TriOnlyElementCount, SizeOf(Cardinal));
    stream.ReadBuffer(Temp, SizeOf(Integer));
    FLODFaceGroupMap[L][F].PrimitiveType := TGLMeshPrimitive(Temp);
    with MaterialManager do
    try
      BeginWork;
      FLODFaceGroupMap[L][F].Material := GetMaterialName(glsDEFAULTMESHNAME);
    finally
      EndWork;
    end;
  end
  else if V = 1 then
  begin
  // LODs and Face groups
    stream.ReadBuffer(Temp, SizeOf(Integer));
    for L := 0 to Temp - 1 do
    begin
      stream.ReadBuffer(Temp, SizeOf(Integer));
      SetLength(FLODFaceGroupMap[L], Temp);
      for F := 0 to Temp - 1 do
      begin
        stream.ReadBuffer(Temp, SizeOf(Integer));
        SetLength(FLODFaceGroupMap[L][F].Name, Temp);
        if Temp > 0 then
          stream.ReadBuffer(FLODFaceGroupMap[L][F].Name[1], Temp*SizeOf(Char));
        stream.ReadBuffer(FLODFaceGroupMap[L][F].VertexOffset, SizeOf(Integer));
        stream.ReadBuffer(FLODFaceGroupMap[L][F].VertexCount, SizeOf(Cardinal));
        stream.ReadBuffer(FLODFaceGroupMap[L][F].ElementOffset, SizeOf(Cardinal));
        stream.ReadBuffer(FLODFaceGroupMap[L][F].ElementCount, SizeOf(Cardinal));
        stream.ReadBuffer(FLODFaceGroupMap[L][F].TriOnlyElementOffset, SizeOf(Cardinal));
        stream.ReadBuffer(FLODFaceGroupMap[L][F].TriOnlyElementCount, SizeOf(Cardinal));
        stream.ReadBuffer(Temp, SizeOf(Integer));
        FLODFaceGroupMap[L][F].PrimitiveType := TGLMeshPrimitive(Temp);
        stream.ReadBuffer(Temp, SizeOf(Integer));
        SetLength(ws, Temp);
        if Temp > 0 then
          stream.ReadBuffer(ws[1], Temp*SizeOf(Char));
          with MaterialManager do
          try
            BeginWork;
            FLODFaceGroupMap[L][F].Material := GetMaterialName(ws);
          finally
            EndWork;
          end;
      end;
    end;
  end;

  // Elements
  stream.ReadBuffer(Temp, SizeOf(Integer));
  FHasIndices := Boolean(Temp);
  if FHasIndices then
  begin
    stream.ReadBuffer(Temp, SizeOf(Integer));
    FElementBuffer.Count := Temp;
    if Temp > 0 then
      stream.ReadBuffer(FElementBuffer.List^, FElementBuffer.DataSize);
    stream.ReadBuffer(Temp, SizeOf(Integer));
    FElementBufferWithAdjacency.Count := Temp;
    if Temp > 0 then
      stream.ReadBuffer(FElementBufferWithAdjacency.List^, FElementBufferWithAdjacency.DataSize);
    stream.ReadBuffer(Temp, SizeOf(Integer));
    FElementBufferTrianglesOnly.Count := Temp;
    if Temp > 0 then
      stream.ReadBuffer(FElementBufferTrianglesOnly.List^, FElementBufferTrianglesOnly.DataSize);
  end;
  stream.ReadBuffer(FRestartIndex, SizeOf(Cardinal));
  stream.ReadBuffer(FAABB, SizeOf(TAABB));

  Inc(FRevisionNum);
end;

procedure TGL3xStaticMesh.SaveToStream(stream: TStream);
var
  Temp, L, A: Integer;
  F: Byte;
  ws: WideString;
begin
  WaitParallelTask;
  Temp := 1;
  stream.WriteBuffer(Temp, SizeOf(Integer)); // Version
  Temp := Integer(FBlank);
  stream.WriteBuffer(Temp, SizeOf(Integer));
  Temp := GetAttributeCount;
  stream.WriteBuffer(Temp, SizeOf(Integer));
  for A := GLS_VERTEX_ATTR_NUM - 1 downto 0 do
    if Assigned(FAttributes[A]) then
    begin
      ws := FAttributes[A].Name;
      L := Length(ws);
      stream.WriteBuffer(L, SizeOf(Integer));
      stream.WriteBuffer(ws[1], L * SizeOf(WideChar));
      Temp := Integer(FDataFormat[A]);
      stream.WriteBuffer(Temp, SizeOf(Integer));
      Temp := FAttributeArrays[A].Count;
      stream.WriteBuffer(Temp, SizeOf(Integer));
      if Temp > 0 then
        stream.WriteBuffer(FAttributeArrays[A].List^, FAttributeArrays[A].DataSize);
      Temp := FAttributeDivisor[A];
      stream.WriteBuffer(Temp, SizeOf(Integer));
    end;

  Temp := GetLODCount;
  stream.WriteBuffer(Temp, SizeOf(Integer));
  for L := 0 to Temp - 1 do
  begin
    Temp := Length(FLODFaceGroupMap[L]);
    stream.WriteBuffer(Temp, SizeOf(Integer));
    for F := 0 to Temp - 1 do
    begin
      ws := FLODFaceGroupMap[L][F].Name;
      Temp := Length(ws);
      stream.WriteBuffer(Temp, SizeOf(Integer));
      if Temp > 0 then
        stream.WriteBuffer(ws[1], Temp*SizeOf(Char));
      stream.WriteBuffer(FLODFaceGroupMap[L][F].VertexOffset, SizeOf(Integer));
      stream.WriteBuffer(FLODFaceGroupMap[L][F].VertexCount, SizeOf(Cardinal));
      stream.WriteBuffer(FLODFaceGroupMap[L][F].ElementOffset, SizeOf(Cardinal));
      stream.WriteBuffer(FLODFaceGroupMap[L][F].ElementCount, SizeOf(Cardinal));
      stream.WriteBuffer(FLODFaceGroupMap[L][F].TriOnlyElementOffset, SizeOf(Cardinal));
      stream.WriteBuffer(FLODFaceGroupMap[L][F].TriOnlyElementCount, SizeOf(Cardinal));
      Temp := Integer(FLODFaceGroupMap[L][F].PrimitiveType);
      stream.WriteBuffer(Temp, SizeOf(Integer));
      ws := FLODFaceGroupMap[L][F].Material.GetValue;
      Temp := Length(ws);
      stream.WriteBuffer(Temp, SizeOf(Integer));
      if Temp > 0 then
        stream.WriteBuffer(ws[1], Temp*SizeOf(Char));
    end;
  end;

  Temp := Integer(FHasIndices);
  stream.WriteBuffer(Temp, SizeOf(Integer));
  if FHasIndices then
  begin
    Temp := FElementBuffer.Count;
    stream.WriteBuffer(Temp, SizeOf(Integer));
    if Temp > 0 then
      stream.WriteBuffer(FElementBuffer.List^, FElementBuffer.DataSize);
    Temp := FElementBufferWithAdjacency.Count;
    stream.WriteBuffer(Temp, SizeOf(Integer));
    if Temp > 0 then
      stream.WriteBuffer(FElementBufferWithAdjacency.List^, FElementBufferWithAdjacency.DataSize);
    Temp := FElementBufferTrianglesOnly.Count;
    stream.WriteBuffer(Temp, SizeOf(Integer));
    if Temp > 0 then
      stream.WriteBuffer(FElementBufferTrianglesOnly.List^, FElementBufferTrianglesOnly.DataSize);
  end;
  stream.WriteBuffer(FRestartIndex, SizeOf(Cardinal));
  stream.WriteBuffer(FAABB, SizeOf(TAABB));
end;

procedure TGL3xStaticMesh.ImportFromFile(const fileName: string);
var
  stream: TStream;
  importer: TGLAbstractMeshFileIOClass;
  rStream: {$IFNDEF FPC}TResourceStream{$ELSE}TLazarusResourceStream{$ENDIF};

begin
  if fileName = '' then
  begin
    if FName.Value = glsDEFAULTMESHNAME then
    begin
      rStream := CreateResourceStream(glsDEFAULTMESHNAME, GLS_RC_XML_Type);
      TGLMeshXMLFileIO.LoadFromStream(rStream, StaticBuilder);
      rStream.Free;
    end
    else
      GLSLogger.LogWarningFmt('Trying to import with empty file name to mesh "%s"', [Name]);
    exit;
  end;

  try
    stream := nil;
    try
      importer := GetVectorFileFormats.FindFromFileName(fileName);
      stream := CreateFileStream(fileName);
      importer.LoadFromStream(stream, StaticBuilder);
    finally
      stream.Free;
    end;
  except
    GLSLogger.LogError('Error during mesh import');
  end;
end;

procedure TGL3xStaticMesh.ExportToFile(const fileName: string);
var
  stream: TStream;
  exporter: TGLAbstractMeshFileIOClass;
begin
  try
    stream := nil;
    try
      exporter := GetVectorFileFormats.FindFromFileName(fileName);
      stream := CreateFileStream(fileName, fmOpenWrite or fmCreate);
      exporter.SaveToStream(stream, Self);
    finally
      stream.Free;
    end;
  except
    GLSLogger.LogError('Error during mesh export');
  end;
end;

procedure TGL3xStaticMesh.AttributeRequest(const Attr: TGLSLAttribute);
var
  A: Integer;
begin
  if FRequest <> [] then
    exit;
  for A := GLS_VERTEX_ATTR_NUM - 1 downto 0 do
    if Attr = FAttributes[A] then
      exit;
  WaitParallelTask;

  if Attr = attrNormal then
  begin
    Include(FRequest, arqNormal);
    AddTaskForServiceContext(ComputeNormalsTask, NewEvent);
  end
  else if Attr = attrTexCoord0 then
  begin
    Include(FRequest, arqTexCoord);
    AddTaskForServiceContext(ComputeTexCoordTask, NewEvent);
  end
  else if Attr = attrTangent then
  begin
    Include(FRequest, arqTangent);
    AddTaskForServiceContext(ComputeTangensTask, NewEvent);
  end;
end;

procedure TGL3xStaticMesh.ComputeNormalsTask;
begin
  with StaticBuilder do
  begin
    BeginMeshAssembly;
    ComputeNormals;
    EndMeshAssembly;
  end;
  Exclude(FRequest, arqNormal);
end;

procedure TGL3xStaticMesh.ComputeTexCoordTask;
begin
  with StaticBuilder do
  begin
    BeginMeshAssembly;
    ComputeTexCoords;
    EndMeshAssembly;
  end;
  Exclude(FRequest, arqTexCoord);
end;

procedure TGL3xStaticMesh.ComputeTangensTask;
begin
  with StaticBuilder do
  begin
    BeginMeshAssembly;
    ComputeTangents;
    EndMeshAssembly;
  end;
  Exclude(FRequest, arqTangent);
end;

function TGL3xStaticMesh.GetLODIndex: string;
begin
  CheckLODRevison;
  Result := Format('LOD%d',[FLODIndex]);
end;

procedure TGL3xStaticMesh.SetLODIndex(AValue: string);
var
  I, E: Integer;
begin
  CheckLODRevison;
  Delete(AValue, 1, 3);
  Val(AValue, I, E);
  if E = 0 then
  begin
    if (I>=0) or (I<FLODList.Count) then
    begin
      FLODIndex := I;
      exit;
    end;
  end;
  FLODIndex := 0;
end;

function TGL3xStaticMesh.GetLOD: TGLMeshLOD;
begin
  CheckLODRevison;
  Result := FLODList[FLODIndex];
end;

procedure TGL3xStaticMesh.SetLOD(AValue: TGLMeshLOD);
begin
end;

procedure TGL3xStaticMesh.FillLODIndexList(Proc: TGetStrProc);
var
  I: Integer;
begin
  CheckLODRevison;
  for I := 0 to FLODList.Count - 1 do
    Proc(Format('LOD%d',[I]));
end;

function TGL3xStaticMesh.GetLODCount: Byte;
var
  I: Byte;
begin
  Result := 0;
  for I := 0 to 255 do
    if Length(FLODFaceGroupMap[I]) > 0 then
      Inc(Result);
end;

function TGL3xStaticMesh.GetFaceGroupCount(ALOD: Byte): Integer;
begin
  Result := Length(FLODFaceGroupMap[ALOD]);
end;

function TGL3xStaticMesh.GetFaceGroupDesc(
  ALOD: Byte; AFaceGroup: Integer): PFaceGroupDesc;
begin
  Result := @FLODFaceGroupMap[ALOD][AFaceGroup];
end;

procedure TGL3xStaticMesh.ClearLODChain;
var
  I: Integer;
begin
  if Assigned(FLODList) then
  begin
    for I := 0 to FLODList.Count - 1 do
      FLODList[I].Free;
    FLODList.Clear;
  end;
end;

procedure TGL3xStaticMesh.CheckLODRevison;
begin
  if FRevisionNum <> FLODChainRevision then
  begin
    RebuildLODChain;
    FLODChainRevision := FRevisionNum;
  end;
end;

procedure TGL3xStaticMesh.RebuildLODChain;
var
  I: Integer;
  L: Byte;
  vlLOD: TGLMeshLOD;
  vlFG: TGLMeshFaceGroup;
begin
  ClearLODChain;
  if not Assigned(FLODList) then
    FLODList := TGLMeshLODList.Create;

  for L := 0 to GetLODCount - 1 do
  begin
    vlLOD := TGLMeshLOD.Create(Self);
    FLODList.Add(vlLOD);
    vlLOD.FIndex := L;
    for I := 0 to GetFaceGroupCount(L) - 1 do
    begin
      vlFG := TGLMeshFaceGroup.Create(vlLOD);
      vlLOD.FFaceGroupList.Add(vlFG);
      vlFG.FDescRef := GetFaceGroupDesc(L, I);
      if Length(vlFG.FDescRef.Name) = 0 then
        vlFG.SetName(Format('FaceGroup%d', [I]));
      vlFG.FIndex := I;
    end;
  end;

  if FLODList.Count = 0 then
  begin
    vlLOD := TGLMeshLOD.Create(Self);
    FLODList.Add(vlLOD);
    vlLOD.FIndex := 0;
    vlFG := TGLMeshFaceGroup.Create(vlLOD);
    vlLOD.FFaceGroupList.Add(vlFG);
    vlFG.SetName('FaceGroup0');
    vlFG.FIndex := 0;
  end;
end;
{$ENDREGION 'TGL3xStaticMesh'}

{$REGION 'TGL3xInstanceData'}
// ------------------
// ------------------ TGL3xInstanceData ------------------
// ------------------
 destructor TGL3xInstanceData.Destroy;
begin
  inherited;
  FBuilder.Free;
end;

function TGL3xInstanceData.GetBuilder: TObject;
begin
  if FBuilder = nil then
  begin
    FBuilder := TGL3xInstanceDataBuilder.Create(Self);
  end;
  Result := FBuilder;
end;

function TGL3xInstanceData.GetInstanceDataBuilder: TGL3xInstanceDataBuilder;
begin
  Result := TGL3xInstanceDataBuilder(GetBuilder);
end;
{$ENDREGION 'TGL3xInstanceData'}

initialization

  RegisterVectorFileFormat('xml', 'XML model file', TGLMeshXMLFileIO);
  RegisterClasses([TGL3xStaticMesh, TGL3xInstanceData]);

end.
