unit GLDrawTechnique;

// TODO: MultiDraw batch union
// TODO: Complete instancing
// TODO: Complete fixed function pipeline drawing
// TODO: Pseudo instanced array

interface

{$I GLScene.inc}

uses
{$IFDEF FPC}
  LCLVersion,
{$ENDIF}
  Classes,
  SysUtils,
  BaseClasses,
  GLCrossPlatform,
  GLContext,
  GLState,
  GLRenderContextInfo,
  OpenGLTokens,
  GLShaderManager,
  GL3xMaterial,
  GL3xMesh,
  GL3xStaticMesh,
  GLSRedBlackTree,
  GLSGenerics;

const
  VBO_STATIC_POOL_SIZE: Cardinal = 16 * 1024 * 1024;
  // Used when no memory info avaible
{$IFDEF FPC}
{$IF (LCL_RELEASE < 31)}
{$DEFINE GLS_GENERIC_PREFIX}
{$IFEND}
{$ENDIF}

type

  TGLBatchIndices = array of LongWord;

  PDrawInfo = ^TDrawInfo;

  TDrawInfo = record
    LOD: Byte;
    InstanceNumber: LongWord;
    PatchVertices: LongWord;
  end;

const
  cDefaultDrawInfo: TDrawInfo = (LOD: 0; InstanceNumber: 0; PatchVertices: 3);

type

  TGLAbstractDrawTechnique = class(TObject)
  protected
    procedure Initialize; virtual; abstract;
  public
    { Public Declarations }
    class function NewInstance: TObject; override;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure DrawCurrentMesh; virtual; abstract;
  end;

  TGLAbstractDrawTechniqueClass = class of TGLAbstractDrawTechnique;

  // TGLStaticMeshDrawerOGL1
  //
  { : Fixed function pipeline draw technique. }
  TGLStaticMeshDrawerOGL1 = class(TGLAbstractDrawTechnique)
  protected
    { Protected Declarations }
    FCurrentMesh: TGL3xStaticMesh;
    procedure Initialize; override;
  public
    { Public Declarations }
    constructor Create; override;
    procedure DrawCurrentMesh; override;
  end;

  TGLVAOHandleTree = {$IFDEF GLS_GENERIC_PREFIX}specialize{$ENDIF}
  GRedBlackTree < TGLProgramHandle, TGLVertexArrayHandle > ;

  TGLMeshState = record
    PerProgramVAO: TGLVAOHandleTree;
    InstanceData: TGL3xInstanceData;
    LastRevision: Integer;
    ArraySectorIndex: Integer;
    ElementSectorIndex: Integer;
    SortedBatchIndices: TGLBatchIndices;
  end;

  PGLMeshState = ^TGLMeshState;

  TPoolSector = record
    Offset: PtrUInt;
    Size: Cardinal;
    MeshState: PGLMeshState;
  end;

  PPoolSector = ^TPoolSector;

  TPoolMap = {$IFDEF GLS_GENERIC_PREFIX}specialize{$ENDIF}
  GList < TPoolSector > ;

  // TGLStaticMeshDrawerOGL2
  //
  { : Programable pipeline draw technique. }
  TGLStaticMeshDrawerOGL2 = class(TGLAbstractDrawTechnique)
  protected
    { Protected Declarations }
    FMeshState: PGLMeshState;
    FMeshStates: array of TGLMeshState;
    FMaterialOverrider: IGLName;
    FPortion: PFaceGroupDesc;
    FDrawAsElement: Boolean;
    FIndexType: TGLEnum;
    FIndexSize: Byte;

    FArrayHandle: TGLVBOArrayBufferHandle;
    FElementHandle: TGLVBOElementArrayHandle;
    FStateHandle: TGLVertexArrayHandle;
    FArrayBufferMap: TPoolMap;
    FElementBufferMap: TPoolMap;

    procedure Initialize; override;
    procedure PassToDevice(AMesh: TGLAbstractMesh);
    procedure MakeBatchList;
    procedure BindStateHandle;
    procedure DrawBatch;
    procedure DoDrawBatch; virtual;
    procedure Draw;
    procedure FreeBuffers; stdcall;

    procedure OnArrayBufferMapChanged(Sender: TObject; const Item: TPoolSector;
      Action: TListNotification);
    procedure OnElementBufferMapChanged(Sender: TObject;
      const Item: TPoolSector; Action: TListNotification);
  public
    { Public Declarations }
    constructor Create; override;
    destructor Destroy; override;

    procedure DrawCurrentMesh; override;
  end;

  TGLStaticMeshDrawerOGL3 = class(TGLStaticMeshDrawerOGL2)
  protected
    procedure DoDrawBatch; override;
  end;

  // DrawManager
  //
  DrawManager = class(TGLSAbstractManager)
  protected
    { Protected Declarations }
    class procedure Initialize; override;
    class procedure Finalize; override;
    class procedure LoadResourceList;
    class procedure ClearResources;
    // Design time notifications
    class procedure NotifyProjectOpened; override;
    class procedure NotifyProjectClosed; override;
    class procedure NotifyContextCreated; override;
    class procedure NotifyBeforeCompile; override;
    class procedure DoDraw;
  public
    { Public Declarations }
    class function FillResourceList(AList: TStringList): Boolean; override;
    class procedure MakeUniqueItemName(var AName: string;
      AClass: TGLAbstractNameClass); override;

    class procedure Draw(var ARci: TRenderContextInfo;
      const AMeshName: IGLName); overload;
    class procedure Draw(var ARci: TRenderContextInfo;
      const AMeshName: IGLName; const ADi: TDrawInfo); overload;
    class procedure Draw(var ARci: TRenderContextInfo; const AMeshName,
      AIntancesDataName: IGLName; const ADi: TDrawInfo); overload;
    class procedure Draw(var ARci: TRenderContextInfo;
      const AMeshName: IGLName; ABatchIndeces: TGLBatchIndices;
      const ADi: TDrawInfo); overload;
  end;

implementation

uses
  GLStrings,
  VectorGeometry,
  VectorTypes,
  VectorLists,
  GLSLog;

const
  cPrimitiveType: array[mpTRIANGLES..mpPATCHES] of GLenum = (GL_TRIANGLES,
    GL_TRIANGLE_STRIP, GL_TRIANGLE_FAN, GL_POINTS, GL_LINES, GL_LINE_LOOP,
    GL_LINE_STRIP, GL_LINES_ADJACENCY, GL_LINE_STRIP_ADJACENCY,
    GL_TRIANGLES_ADJACENCY, GL_TRIANGLE_STRIP_ADJACENCY, GL_PATCHES);

type
  TAccesableShaderManager = class(ShaderManager);
  TAccesableMeshManager = class(MeshManager);
  TAccesableMesh = class(TGLAbstractMesh);
  TAccesableStaticMesh = class(TGL3xStaticMesh);
  TAccesableInstanceData = class(TGL3xInstanceData);

var
  DrawTechniqueClasses: array of TGLAbstractDrawTechnique;

var
  vCurrentMesh: TGLAbstractMesh;
  vCurrentInstanceData: TGL3xInstanceData;
  vCurrentBatchIndices: TGLBatchIndices;
  vRenderContextInfo: PRenderContextInfo;
  vDrawInfo: PDrawInfo;

procedure SetVertexAttribPointer(AFormat: TGLSLDataType; ASlot: TGLuint;
  AOffset: Pointer; ADivisor: TGLuint);
begin
  case AFormat of
    GLSLType1F:
      GL.VertexAttribPointer(ASlot, 1, GL_FLOAT, false, 0, AOffset);
    GLSLType2F:
      GL.VertexAttribPointer(ASlot, 2, GL_FLOAT, false, 0, AOffset);
    GLSLType3F:
      GL.VertexAttribPointer(ASlot, 3, GL_FLOAT, false, 0, AOffset);
    GLSLType4F:
      GL.VertexAttribPointer(ASlot, 4, GL_FLOAT, false, 0, AOffset);
    GLSLTypeMat2F:
      GL.VertexAttribPointer(ASlot, 4, GL_FLOAT, false, 0, AOffset);
    GLSLTypeMat3F:
      GL.VertexAttribPointer(ASlot, 9, GL_FLOAT, false, 0, AOffset);
    GLSLTypeMat4F:
      GL.VertexAttribPointer(ASlot, 16, GL_FLOAT, false, 0, AOffset);
    GLSLType1I:
      GL.VertexAttribIPointer(ASlot, 1, GL_INT, 0, AOffset);
    GLSLType2I:
      GL.VertexAttribIPointer(ASlot, 2, GL_INT, 0, AOffset);
    GLSLType3I:
      GL.VertexAttribIPointer(ASlot, 3, GL_INT, 0, AOffset);
    GLSLType4I:
      GL.VertexAttribIPointer(ASlot, 4, GL_UNSIGNED_INT, 0, AOffset);
    GLSLType1UI:
      GL.VertexAttribIPointer(ASlot, 1, GL_UNSIGNED_INT, 0, AOffset);
    GLSLType2UI:
      GL.VertexAttribIPointer(ASlot, 2, GL_UNSIGNED_INT, 0, AOffset);
    GLSLType3UI:
      GL.VertexAttribIPointer(ASlot, 3, GL_UNSIGNED_INT, 0, AOffset);
    GLSLType4UI:
      GL.VertexAttribIPointer(ASlot, 4, GL_UNSIGNED_INT, 0, AOffset);
  else
    Assert(false, glsErrorEx + glsUnknownType);
  end;
  if GL.ARB_instanced_arrays then
    GL.VertexAttribDivisor(ASlot, ADivisor);
end;

function IsPrimitiveSupported(APrimitive: TGLMeshPrimitive): Boolean;
begin
  if (APrimitive >= mpLINES_ADJACENCY) and
    (APrimitive <= mpTRIANGLE_STRIP_ADJACENCY) then
    Result := GL.EXT_gpu_shader4
  else if APrimitive = mpPATCHES then
    Result := GL.ARB_tessellation_shader
  else
    Result := True;
end;

procedure StateHandleDestroyer(k: TGLProgramHandle;
  AHandle: TGLVertexArrayHandle; out Flag: Boolean);
begin
  AHandle.Destroy;
  Flag := True;
end;

procedure StateHandleChanger(k: TGLProgramHandle;
  AHandle: TGLVertexArrayHandle; out Flag: Boolean);
begin
  AHandle.NotifyChangesOfData;
  Flag := True;
end;

{$REGION 'TGLAbstractDrawTechnique'}

// ------------------
// ------------------ TGLAbstractDrawTechnique ------------------
// ------------------

class function TGLAbstractDrawTechnique.NewInstance: TObject;

var
  I: Integer;
begin
  for I := 0 to High(DrawTechniqueClasses) do
    if Assigned(DrawTechniqueClasses[I]) and
      (DrawTechniqueClasses[I].ClassName = Self.ClassName) then
      exit(DrawTechniqueClasses[I]);
  I := Length(DrawTechniqueClasses);
  SetLength(DrawTechniqueClasses, I + 1);
  Result := inherited NewInstance;
  DrawTechniqueClasses[I] := TGLAbstractDrawTechnique(Result);
end;

constructor TGLAbstractDrawTechnique.Create;
begin
end;

destructor TGLAbstractDrawTechnique.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(DrawTechniqueClasses) do
    if DrawTechniqueClasses[I] = Self then
    begin
      DrawTechniqueClasses[I] := nil;
      exit;
    end;
end;

{$ENDREGION 'TGLAbstractDrawTechnique'}

{$REGION 'TGLStaticMeshDrawerFFP'}

// ------------------
// ------------------ TGLStaticMeshDrawerFFP ------------------
// ------------------

constructor TGLStaticMeshDrawerOGL1.Create;
begin
  if Self = nil then
    exit;
end;

procedure TGLStaticMeshDrawerOGL1.DrawCurrentMesh;
begin
end;

procedure TGLStaticMeshDrawerOGL1.Initialize;
begin
end;

{$ENDREGION 'TGLStaticMeshDrawerFFP'}

{$REGION 'TGLStaticMeshDrawerOGL2'}

// ------------------
// ------------------ TGLStaticMeshDrawerOGL2 ------------------
// ------------------

constructor TGLStaticMeshDrawerOGL2.Create;
begin
  if Self = nil then
    exit;
  if Assigned(Self.FArrayHandle) then
    exit;
  FArrayHandle := TGLVBOArrayBufferHandle.Create;
  FArrayBufferMap := TPoolMap.Create;
  FArrayBufferMap.OnChange := OnArrayBufferMapChanged;
  FElementHandle := TGLVBOElementArrayHandle.Create;
  FElementBufferMap := TPoolMap.Create;
  FElementBufferMap.OnChange := OnElementBufferMapChanged;
end;

destructor TGLStaticMeshDrawerOGL2.Destroy;
var
  I: Integer;
begin
  AddTaskForServiceContext(FreeBuffers);
  FArrayBufferMap.OnChange := nil;
  FArrayBufferMap.Destroy;
  FElementBufferMap.OnChange := nil;
  FElementBufferMap.Destroy;
  for I := 0 to High(FMeshStates) do
    if Assigned(FMeshStates[I].PerProgramVAO) then
    begin
      FMeshStates[I].PerProgramVAO.ForEach(StateHandleDestroyer);
      FMeshStates[I].PerProgramVAO.Destroy;
    end;
end;

procedure TGLStaticMeshDrawerOGL2.Initialize;

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
  begin
    if FArrayBufferMap.Count > 0 then
    begin
      GLSLogger.LogError(
        'Attempt to allocate static buffer in unshared context');
      exit;
    end;

    ArraySector.Offset := 0;
    ArraySector.Size := 0;
    ArraySector.MeshState := nil;
    ElementSector.Offset := 0;
    ElementSector.Size := 0;
    ElementSector.MeshState := nil;

    if IsDesignTime then
    begin
      VBOPool := VBO_STATIC_POOL_SIZE;
    end
    else if GL.ATI_meminfo then
    begin
      GL.GetIntegerv(GL_VBO_FREE_MEMORY_ATI, @VBOFreeMem[0]);
      GLSLogger.LogInfo(Format('Free graphic memory avaible - %dM',
        [VBOFreeMem[1] div 1024]));
      VBOPool := VBOFreeMem[1] * 1024 div 4;
      VBOPool := MinInteger(VBOPool, 2 * VBO_STATIC_POOL_SIZE);
    end
    else if GL.NVX_gpu_memory_info then
    begin
      GL.GetIntegerv(GL_GPU_MEMORY_INFO_TOTAL_AVAILABLE_MEMORY_NVX,
        @VBOFreeMem[1]);
      GLSLogger.LogInfo(Format('Free graphic memory avaible - %dM',
        [VBOFreeMem[1] div 1024]));
      VBOPool := VBOFreeMem[1] * 1024 div 4;
      VBOPool := MinInteger(VBOPool, 2 * VBO_STATIC_POOL_SIZE);
    end
    else
    begin
      VBOPool := VBO_STATIC_POOL_SIZE;
      GLSLogger.LogInfo(
        'Can''t get info about graphic memory. Allocate pool size of 16M');
    end;
    ArraySector.Size := 3 * VBOPool div 4;
    ElementSector.Size := VBOPool - ArraySector.Size;
    GLSLogger.LogInfo(Format('Allocated static vertex buffer pool - %dM',
      [ArraySector.Size div $100000]));
    GLSLogger.LogInfo(Format('Allocated static element buffer pool - %dM',
      [ElementSector.Size div $100000]));

    if IsDesignTime then
      BufferType := GL_DYNAMIC_DRAW
    else
      BufferType := GL_STATIC_DRAW;
    FArrayHandle.BindBufferData(nil, ArraySector.Size, BufferType);
    FElementHandle.BindBufferData(nil, ElementSector.Size, BufferType);
    FArrayHandle.NotifyDataUpdated;
    FElementHandle.NotifyDataUpdated;

    FArrayBufferMap.Add(ArraySector);
    FElementBufferMap.Add(ElementSector);
  end;
  GL.Finish;
end;

procedure TGLStaticMeshDrawerOGL2.FreeBuffers;
begin
  FArrayHandle.Destroy;
  FElementHandle.Destroy;
end;

procedure TGLStaticMeshDrawerOGL2.PassToDevice(AMesh: TGLAbstractMesh);

var
  a, I, J: Integer;
  lDataSize: array[0..GLS_VERTEX_ATTR_NUM - 1] of Cardinal;
  RequestSize, Size, Offset, maxIndexValue: Cardinal;
  ElementBufferSource: Pointer;
  lvMesh: TAccesableMesh;
  ArraySector: TPoolSector;
  ElementSector: TPoolSector;
  pSector: PPoolSector;
  BD: T4ByteData;
begin
  lvMesh := TAccesableMesh(AMesh);
  // Calculate size of array
  RequestSize := 0;
  for a := GLS_VERTEX_ATTR_NUM - 1 downto 0 do
    if Assigned(lvMesh.FAttributes[a]) then
    begin
      lDataSize[a] := lvMesh.FAttributeArrays[a].Count * SizeOf(T4ByteData);
      Inc(RequestSize, lDataSize[a]);
    end
    else
      lDataSize[a] := 0;

  // Check for empty mesh
  if RequestSize = 0 then
    exit;

  if IsDesignTime then
  begin
    if Cardinal(FArrayHandle.BufferSize) < RequestSize then
      exit;
    Offset := 0;
  end
  else
  begin
    // choose place in pool to upload data
    J := FMeshState.ArraySectorIndex;
    if J > -1 then
    begin
      // Sector overflow
      pSector := FArrayBufferMap.ItemAddress[J];
      if pSector.Size < RequestSize then
      begin
        // Lool into next sector, it may be free
        Size := FArrayBufferMap[J].Size;
        I := J + 1;
        while (I < FArrayBufferMap.Count) do
        begin
          if FArrayBufferMap[I].MeshState = nil then
            Size := Size + FArrayBufferMap[I].Size
          else
            break;
          if Size >= RequestSize then
            break;
          Inc(I);
        end;

        if Size >= RequestSize then
        begin
          // Merge sectors
          pSector := FArrayBufferMap.ItemAddress[J];
          pSector.Size := Size;
          for I := J + 1 to I do
            FArrayBufferMap.Delete(J + 1);
        end
        else
        begin
          // Free sector
          pSector := FArrayBufferMap.ItemAddress[J];
          pSector.MeshState := nil;
          FMeshState.ArraySectorIndex := -1;
          J := -1;
        end;
      end;
    end;

    if J < 0 then
    begin
      // Find free sector
      Size := FArrayHandle.BufferSize;
      for I := 0 to FArrayBufferMap.Count - 1 do
        if (FArrayBufferMap[I].MeshState = nil) and
          (FArrayBufferMap[I].Size >= RequestSize) and
          (FArrayBufferMap[I].Size <= Size) then
        begin
          J := I;
          Size := FArrayBufferMap[I].Size;
        end;

      // Check overflow
      if J < 0 then
      begin
        // TODO: defragmentation
        GLSLogger.LogError('Static vertex array pool is full');
        Abort;
      end;

      // Extract the residue
      if (FArrayBufferMap[J].Size - RequestSize) > 0 then
      begin
        ArraySector.Offset := FArrayBufferMap[J].Offset + RequestSize;
        ArraySector.Size := FArrayBufferMap[J].Size - RequestSize;
        ArraySector.MeshState := nil;
        if J < FArrayBufferMap.Count - 1 then
          FArrayBufferMap.Insert(J, ArraySector)
        else
          FArrayBufferMap.Add(ArraySector);
      end;

      // Set new parameters
      FMeshState.ArraySectorIndex := J;
      pSector := FArrayBufferMap.ItemAddress[J];
      pSector.Size := RequestSize;
      pSector.MeshState := FMeshState;
    end;

    Offset := FArrayBufferMap[J].Offset;
  end;

  // upload each attribute array one after another
  FArrayHandle.Bind;

  // Skip if blank
  if not lvMesh.FBlank then
  begin
    for a := GLS_VERTEX_ATTR_NUM - 1 downto 0 do
      if Assigned(lvMesh.FAttributes[a]) then
      begin
        FArrayHandle.BufferSubData(Offset, lDataSize[a],
          lvMesh.FAttributeArrays[a].List);
        Inc(Offset, lDataSize[a]);
      end;
  end;

  if lvMesh.FHasIndices then
  begin
    maxIndexValue := lvMesh.FAttributeArrays[15]
      .Count div GLSLTypeComponentCount(lvMesh.FDataFormat[15]);
    // Adjust index type according it's number
    if (maxIndexValue + 1 < $10000)
      and not lvMesh.FBlank and not IsDesignTime then
    begin
      lvMesh.FRestartIndex := $FFFF;
      RequestSize := lvMesh.FElementBuffer.Count * SizeOf(TGLushort);
      GetMem(ElementBufferSource, RequestSize);
      for I := lvMesh.FElementBuffer.Count - 1 downto 0 do
      begin
        BD := lvMesh.FElementBuffer[I];
        PWordVector(ElementBufferSource)[I] := BD.Word.Value[0];
      end;
    end
    else
    begin
      lvMesh.FRestartIndex := $FFFFFFFF;
      RequestSize := lvMesh.FElementBuffer.Count * SizeOf(TGLuint);
      ElementBufferSource := nil;
    end;

    if IsDesignTime then
    begin
      if Cardinal(FElementHandle.BufferSize) < RequestSize then
        exit;
      Offset := 0;
    end
    else
    begin
      // choose place in pool to upload data
      J := FMeshState.ElementSectorIndex;
      if J > -1 then
      begin
        // Sector overflow
        if FElementBufferMap[J].Size < RequestSize then
        begin
          // Lool into next sector, it may be free
          Size := FElementBufferMap[J].Size;
          I := J + 1;
          while (I < FElementBufferMap.Count) do
          begin
            if FElementBufferMap[I].MeshState = nil then
              Size := Size + FElementBufferMap[I].Size
            else
              break;
            if Size >= RequestSize then
              break;
            Inc(I);
          end;

          if Size >= RequestSize then
          begin
            // Merge sectors
            pSector := FElementBufferMap.ItemAddress[J];
            pSector.Size := Size;
            for I := J + 1 to I do
              FElementBufferMap.Delete(J + 1);
          end
          else
          begin
            // Free sector
            pSector := FElementBufferMap.ItemAddress[J];
            pSector.MeshState := nil;
            FMeshState.ElementSectorIndex := -1;
            J := -1;
          end;
        end;
      end;

      if J < 0 then
      begin
        // Find free sector
        Size := FElementHandle.BufferSize;
        for I := 0 to FElementBufferMap.Count - 1 do
          if (FElementBufferMap[I].MeshState = nil) and
            (FElementBufferMap[I].Size >= RequestSize) and
            (FElementBufferMap[I].Size <= Size) then
          begin
            J := I;
            Size := FElementBufferMap[I].Size;
          end;

        // Check overflow
        if J < 0 then
        begin
          // TODO: defragmentation
          GLSLogger.LogError('Static vertex array pool is full');
          Abort;
        end;

        // Extract the residue
        if (FElementBufferMap[J].Size - RequestSize) > 0 then
        begin
          ElementSector.Offset := FElementBufferMap[J].Offset + RequestSize;
          ElementSector.Size := FElementBufferMap[J].Size - RequestSize;
          ElementSector.MeshState := nil;
          if J < FElementBufferMap.Count - 1 then
            FElementBufferMap.Insert(J, ElementSector)
          else
            FElementBufferMap.Add(ElementSector);
        end;

        // Set new parameters
        FMeshState.ElementSectorIndex := J;
        pSector := FElementBufferMap.ItemAddress[J];
        pSector.Size := RequestSize;
        pSector.MeshState := FMeshState;
      end;

      Offset := FElementBufferMap[J].Offset;
    end;

    // upload element array
    FElementHandle.Bind;
    if Assigned(ElementBufferSource) then
    begin
      if not lvMesh.FBlank then
        FElementHandle.BufferSubData(Offset, RequestSize,
          ElementBufferSource);
      FreeMem(ElementBufferSource);
    end
    else if not lvMesh.FBlank then
    begin
      FElementHandle.BufferSubData(Offset, RequestSize,
        lvMesh.FElementBuffer.List);
    end;
  end;
end;

procedure TGLStaticMeshDrawerOGL2.MakeBatchList;

var
  lvMesh: TAccesableStaticMesh;
  I, I1, I2, J, O1, O2: Integer;
  bNeedUpdateVAO: Boolean;
begin
  lvMesh := TAccesableStaticMesh(vCurrentMesh);
  I := lvMesh.Name.IndexInManagerArray;
  if High(FMeshStates) < I then
    SetLength(FMeshStates, I + 1);
  FMeshState := @FMeshStates[I];
  bNeedUpdateVAO := false;

  // First time mesh allocation
  if FMeshState.PerProgramVAO = nil then
  begin
    FMeshState.ArraySectorIndex := -1;
    FMeshState.ElementSectorIndex := -1;
    PassToDevice(vCurrentMesh);
    FMeshState.PerProgramVAO := TGLVAOHandleTree.Create(CompareProgram, nil);
    FMeshState.LastRevision := lvMesh.FRevisionNum;
    bNeedUpdateVAO := True;
  end
  else if IsDesignTime then
  begin
    FMeshState.InstanceData := vCurrentInstanceData;
    PassToDevice(vCurrentMesh);
    FMeshState.LastRevision := lvMesh.FRevisionNum;
  end;

  // Chack instance data changing
  if FMeshState.InstanceData <> vCurrentInstanceData then
  begin
    FMeshState.InstanceData := vCurrentInstanceData;
    PassToDevice(vCurrentMesh);
    FMeshState.LastRevision := lvMesh.FRevisionNum;
    bNeedUpdateVAO := True;
  end

    // Check revision (mesh structire changing)
  else if FMeshState.LastRevision <> lvMesh.FRevisionNum then
  begin
    PassToDevice(vCurrentMesh);
    FMeshState.LastRevision := lvMesh.FRevisionNum;
    bNeedUpdateVAO := True;
  end;

  if bNeedUpdateVAO then
  begin
    FMeshState.PerProgramVAO.ForEach(StateHandleChanger);

    // Copy batch's indidices
    if Assigned(vCurrentBatchIndices) then
    begin
      FMeshState.SortedBatchIndices := vCurrentBatchIndices;
    end
    else
    begin
      // Fill array by number's chain
      SetLength(FMeshState.SortedBatchIndices,
        Length(lvMesh.FLODFaceGroupMap[vDrawInfo.LOD]));
      for I := 0 to High(FMeshState.SortedBatchIndices) do
        FMeshState.SortedBatchIndices[I] := I;
    end;
    // Sort batches
    if not Assigned(FMaterialOverrider) then
    with MaterialManager do
    try
      BeginWork;
      for I := 0 to High(FMeshState.SortedBatchIndices) do
        for J := 0 to High(FMeshState.SortedBatchIndices) - 1 do
        begin
          I1 := FMeshState.SortedBatchIndices[J];
          I2 := FMeshState.SortedBatchIndices[J + 1];
          O1 := GetMaterialOrder(lvMesh.FLODFaceGroupMap[vDrawInfo.LOD][I1].Material);
          O2 := GetMaterialOrder(lvMesh.FLODFaceGroupMap[vDrawInfo.LOD][I2].Material);
          if O1 < O2 then
          begin
            FMeshState.SortedBatchIndices[J] := I2;
            FMeshState.SortedBatchIndices[J + 1] := I1;
          end;
        end;
    finally
      EndWork;
    end;
  end;
end;

procedure TGLStaticMeshDrawerOGL2.BindStateHandle;

var
  lvMesh: TAccesableStaticMesh;
  lvInstData: TAccesableInstanceData;
  lvProgram: TGLProgramHandle;

  I, L: Integer;
  Offset, Size: PtrUInt;
  EnabledLocations: array[0..GLS_VERTEX_ATTR_NUM - 1] of Boolean;
begin
  // Check shader program binding
  lvProgram := TAccesableShaderManager.CurrentProgram;
  if lvProgram = nil then
  begin
    GLSLogger.LogError(glsNoShader);
    Abort;
  end;

  // Find VAO associated with current program
  if not FMeshState.PerProgramVAO.Find(lvProgram, FStateHandle) then
  begin
    FStateHandle := TGLVertexArrayHandle.Create;
    FMeshState.PerProgramVAO.Add(lvProgram, FStateHandle);
  end;
  FStateHandle.AllocateHandle;

  lvMesh := TAccesableStaticMesh(vCurrentMesh);

  if FStateHandle.IsDataNeedUpdate then
  begin
    // Check attribute quering by shader
    if attrNormal.Location > -1 then
      lvMesh.AttributeRequest(attrNormal);
    if attrTexCoord0.Location > -1 then
      lvMesh.AttributeRequest(attrTexCoord0);
    if attrTangent.Location > -1 then
      lvMesh.AttributeRequest(attrTangent);

    // Uniting all states and buffers in one vertex array object
    FStateHandle.Bind;

    // Need to direct bind array buffer for correctly VertexAttribPointer set up
    if CurrentGLcontext.GLStates.ArrayBufferBinding = FArrayHandle.Handle then
      GL.BindBuffer(GL_ARRAY_BUFFER,
        CurrentGLcontext.GLStates.ArrayBufferBinding)
    else
      FArrayHandle.Bind;
    FElementHandle.Bind;

    if IsDesignTime then
      Offset := 0
    else
      Offset := FArrayBufferMap[FMeshState.ArraySectorIndex].Offset;

    // Predisable attributes
    for I := 0 to GLS_VERTEX_ATTR_NUM - 1 do
      EnabledLocations[I] := false;

    // Setup attribute arrays pointer
    for I := GLS_VERTEX_ATTR_NUM - 1 downto 0 do
    begin
      if Assigned(lvMesh.FAttributes[I]) then
      begin
        Size := PtrUInt(lvMesh.FAttributeArrays[I].DataSize);
        L := lvMesh.FAttributes[I].Location;
        if (Size > 0) and (L > -1) then
        begin
          EnabledLocations[L] := True;
          SetVertexAttribPointer(lvMesh.FDataFormat[I], L, Pointer(Offset), 0);
        end;
        Offset := Offset + Size;
      end;
    end;

    // Setup instance attribute arrays pointer
    if Assigned(vCurrentInstanceData) and GL.ARB_instanced_arrays then
    begin
      lvInstData := TAccesableInstanceData(vCurrentInstanceData);
      for I := 0 to GLS_VERTEX_ATTR_NUM - 1 do
      begin
        if Assigned(lvInstData.FAttributes[I]) then
        begin
          Size := PtrUInt(lvInstData.FAttributeArrays[I].DataSize);
          L := lvInstData.FAttributes[I].Location;
          if (Size > 0) and (L > -1) then
          begin
            EnabledLocations[L] := True;
            SetVertexAttribPointer(lvInstData.FDataFormat[I], L,
              Pointer(Offset),
              lvInstData.FAttributeDivisor[I]);
          end;
          Offset := Offset + Size;
        end;
      end;
    end;

    // Enable engagement attributes array
    with CurrentGLcontext.GLStates do
    begin
      for I := GLS_VERTEX_ATTR_NUM - 1 downto 0 do
        EnableVertexAttribArray[I] := EnabledLocations[I];
    end;
    FStateHandle.NotifyDataUpdated;
  end
  else
    FStateHandle.Bind;

  with vRenderContextInfo.GLStates do
  begin
    EnablePrimitiveRestart := lvMesh.FHasIndices;
    PrimitiveRestartIndex := lvMesh.FRestartIndex;
  end;
end;

procedure TGLStaticMeshDrawerOGL2.DrawCurrentMesh;
begin
  FMaterialOverrider := MaterialManager.GetCurrentMaterialName;
  MakeBatchList;
  Draw;
  vRenderContextInfo.GLStates.VertexArrayBinding := 0;
end;

procedure TGLStaticMeshDrawerOGL2.DoDrawBatch;
var
  glPrimitive: TGLEnum;
  Offset: Pointer;
  instancedID: GLInt;
  I: Integer;
begin
  glPrimitive := cPrimitiveType[FPortion.PrimitiveType];
  Offset := Pointer(FPortion.ElementOffset * FIndexSize);
  if not IsDesignTime then
    Offset := Pointer(FElementBufferMap[FMeshState.ElementSectorIndex].Offset +
      Cardinal(Offset));

    if vDrawInfo.InstanceNumber > 0 then
    begin
      instancedID := uniformInstanceID.Location;

      if FDrawAsElement then
      begin
        for I := 0 to vDrawInfo.InstanceNumber - 1 do
        begin
          if instancedID > 0 then
            GL.Uniform1i(instancedID, I);
          GL.DrawElements(
            glPrimitive,
            FPortion.ElementCount,
            FIndexType,
            Offset);
        end;
        if instancedID > 0 then
          GL.Uniform1i(instancedID, 0);
      end
      else
      begin
        for I := 0 to vDrawInfo.InstanceNumber - 1 do
        begin
          if instancedID > 0 then
            GL.Uniform1i(instancedID, I);
          GL.DrawArrays(
            glPrimitive,
            FPortion.VertexOffset,
            FPortion.VertexCount);
        end;
        if instancedID > 0 then
          GL.Uniform1i(instancedID, 0);
      end;
    end
    else
    begin
      if FDrawAsElement then
        GL.DrawElements(
          glPrimitive,
          FPortion.ElementCount,
          FIndexType,
          Offset)
      else
        GL.DrawArrays(
          glPrimitive,
          FPortion.VertexOffset,
          FPortion.VertexCount);
    end;
end;

procedure TGLStaticMeshDrawerOGL2.DrawBatch;
begin
  if FPortion.PrimitiveType in vRenderContextInfo.PrimitiveMask then
  begin
    try
      // Check the HW support of primitives
      if not IsPrimitiveSupported(FPortion.PrimitiveType) then
        exit;

      if Assigned(FMaterialOverrider) or vRenderContextInfo.ignoreMaterials
        then
      begin
        BindStateHandle;
        DoDrawBatch;
      end
      else
        repeat
          MaterialManager.ApplyMaterial(
            FPortion.Material,
            vRenderContextInfo^);
          BindStateHandle;
          DoDrawBatch;
        until MaterialManager.UnApplyMaterial(vRenderContextInfo^);
    except
      GLSLogger.LogErrorFmt
        ('Failed on rendering face group "%s" of mesh "%s"',
        [FPortion.Name, vCurrentMesh.Name.Value]);
      raise;
    end;
  end;
end;

procedure TGLStaticMeshDrawerOGL2.Draw;
var
  p: Integer;
  lvMesh: TAccesableMesh;
  batchIndex: Integer;
begin
  lvMesh := TAccesableMesh(vCurrentMesh);

  if lvMesh.FRestartIndex = $FFFFFFFF then
  begin
    FIndexType := GL_UNSIGNED_INT;
    FIndexSize := 4;
  end
  else
  begin
    FIndexType := GL_UNSIGNED_SHORT;
    FIndexSize := 2;
  end;

  FDrawAsElement := lvMesh.FHasIndices;

  for p := 0 to High(FMeshState.SortedBatchIndices) do
  begin
    batchIndex := FMeshState.SortedBatchIndices[p];
    FPortion := @lvMesh.FLODFaceGroupMap[vDrawInfo.LOD][batchIndex];
    DrawBatch;
  end;
end;

procedure TGLStaticMeshDrawerOGL2.OnArrayBufferMapChanged(Sender: TObject;
  const Item: TPoolSector; Action: TListNotification);
begin
  if Assigned(Item.MeshState) then
    Item.MeshState.ArraySectorIndex := FArrayBufferMap.IndexOf(Item);
end;

procedure TGLStaticMeshDrawerOGL2.OnElementBufferMapChanged(Sender: TObject;
  const Item: TPoolSector; Action: TListNotification);
begin
  if Assigned(Item.MeshState) then
    Item.MeshState.ElementSectorIndex := FElementBufferMap.IndexOf(Item);
end;

{$ENDREGION 'TGLStaticMeshDrawerOGL2'}

{$REGION 'TGLStaticMeshDrawerOGL3'}

procedure TGLStaticMeshDrawerOGL3.DoDrawBatch;
var
  glPrimitive: TGLEnum;
  Offset: Pointer;
begin
  glPrimitive := cPrimitiveType[FPortion.PrimitiveType];
  Offset := Pointer(FPortion.ElementOffset * FIndexSize);
  if not IsDesignTime then
    Offset := Pointer(FElementBufferMap[FMeshState.ElementSectorIndex].Offset +
      PtrUint(Offset));

  if vDrawInfo.InstanceNumber > 0 then
  begin
    if FDrawAsElement then
      GL.DrawElementsInstanced(
        glPrimitive,
        FPortion.ElementCount,
        FIndexType,
        Offset,
        vDrawInfo.InstanceNumber)
    else
      GL.DrawArraysInstanced(
        glPrimitive,
        FPortion.VertexOffset,
        FPortion.VertexCount,
        vDrawInfo.InstanceNumber);
  end
  else
  begin
    if FDrawAsElement then
      GL.DrawElements(
        glPrimitive,
        FPortion.ElementCount,
        FIndexType,
        Offset)
    else
      GL.DrawArrays(
        glPrimitive,
        FPortion.VertexOffset,
        FPortion.VertexCount);
  end;
end;

{$ENDREGION}


{$REGION 'DrawManager'}

// ------------------
// ------------------ DrawManager ------------------
// ------------------

class procedure DrawManager.Initialize;
begin
end;

class procedure DrawManager.Finalize;

var
  I: Integer;
begin
  for I := 0 to High(DrawTechniqueClasses) do
    DrawTechniqueClasses[I].Free;
end;

class procedure DrawManager.LoadResourceList;
begin
end;

class procedure DrawManager.ClearResources;
begin
end;

class procedure DrawManager.NotifyProjectOpened;
begin
end;

class procedure DrawManager.NotifyProjectClosed;
begin
end;

class procedure DrawManager.NotifyContextCreated;
var
  I: Integer;
  techClass: TGLAbstractDrawTechniqueClass;
begin
  if GL.VERSION_3_0 then
    techClass := TGLStaticMeshDrawerOGL3
  else if GL.VERSION_2_1 then
    techClass := TGLStaticMeshDrawerOGL2
  else
    techClass := TGLStaticMeshDrawerOGL1;
  techClass.Create;

  for I := 0 to High(DrawTechniqueClasses) do
    DrawTechniqueClasses[I].Initialize;
end;

class procedure DrawManager.NotifyBeforeCompile;
begin
end;

class function DrawManager.FillResourceList(AList: TStringList): Boolean;
begin
  Result := false;
end;

class procedure DrawManager.MakeUniqueItemName(var AName: string;
  AClass: TGLAbstractNameClass);
begin
end;

class procedure DrawManager.DoDraw;
var
  techClass: TGLAbstractDrawTechniqueClass;
  tech: TGLAbstractDrawTechnique;
begin
  tech := nil;
  if vCurrentMesh is TGL3xStaticMesh then
  begin
    if GL.VERSION_3_0 then
      techClass := TGLStaticMeshDrawerOGL3
    else if GL.VERSION_2_1 then
      techClass := TGLStaticMeshDrawerOGL2
    else
      techClass := TGLStaticMeshDrawerOGL1;
    tech := techClass.Create;
  end;

  if Assigned(tech) then
    tech.DrawCurrentMesh;
end;

class procedure DrawManager.Draw(var ARci: TRenderContextInfo;
  const AMeshName: IGLName);
begin
  try
    BeginWork;
    vRenderContextInfo := @ARci;
    vDrawInfo := @cDefaultDrawInfo;
    vCurrentMesh := TAccesableMeshManager.GetMesh(AMeshName);
    vCurrentInstanceData := nil;
    vCurrentBatchIndices := nil;
    DoDraw;
  finally
    EndWork;
  end;
end;

class procedure DrawManager.Draw(var ARci: TRenderContextInfo;
  const AMeshName: IGLName; const ADi: TDrawInfo);
begin
  try
    BeginWork;
    vRenderContextInfo := @ARci;
    vDrawInfo := @ADi;
    vCurrentMesh := TAccesableMeshManager.GetMesh(AMeshName);
    vCurrentInstanceData := nil;
    vCurrentBatchIndices := nil;
    DoDraw;
  finally
    EndWork;
  end;
end;

class procedure DrawManager.Draw(var ARci: TRenderContextInfo;
  const AMeshName, AIntancesDataName: IGLName; const ADi: TDrawInfo);

var
  lMesh: TGLAbstractMesh;
begin
  try
    BeginWork;
    vRenderContextInfo := @ARci;
    vDrawInfo := @ADi;
    vCurrentMesh := TAccesableMeshManager.GetMesh(AMeshName);

    lMesh := TAccesableMeshManager.GetMesh(AIntancesDataName);
    if lMesh is TGL3xInstanceData then
      vCurrentInstanceData := TGL3xInstanceData(lMesh);

    vCurrentBatchIndices := nil;
    DoDraw;
  finally
    EndWork;
  end;
end;

class procedure DrawManager.Draw(var ARci: TRenderContextInfo;
  const AMeshName: IGLName; ABatchIndeces: TGLBatchIndices;
  const ADi: TDrawInfo);
begin
  try
    BeginWork;
    vRenderContextInfo := @ARci;
    vDrawInfo := @ADi;
    vCurrentMesh := TAccesableMeshManager.GetMesh(AMeshName);
    vCurrentInstanceData := nil;
    vCurrentBatchIndices := ABatchIndeces;
    DoDraw;
  finally
    EndWork;
  end;
end;

{$ENDREGION 'DrawManager'}

{$IFDEF GLS_EXPERIMENTAL}

initialization

  RegisterGLSceneManager(DrawManager);

finalization

  DrawManager.Finalize;

{$ENDIF GLS_EXPERIMENTAL}

end.

