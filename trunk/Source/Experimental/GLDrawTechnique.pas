unit GLDrawTechnique;

interface

{$I GLScene.inc}

uses
  Classes,
  SysUtils,
  BaseClasses,
  GLCrossPlatform,
  GLContext,
  GLState,
  OpenGLTokens,
  GLShaderManager,
  GL3xMesh,
  GLSRedBlackTree,
  GLSGenerics;

const
  VBO_STATIC_POOL_SIZE: Cardinal = 16 * 1024 * 1024; // Used when no memory info avaible

type

  TGLBatchIndices = array of LongWord;

  TGLAbstractDrawTechnique = class(TObject)
  public
    { Public Declarations }
    class function NewInstance: TObject; override;
    destructor Destroy; override;
    procedure DrawCurrentMesh; virtual; abstract;
  end;

  TGLAbstractDrawTechniqueClass = class of TGLAbstractDrawTechnique;

  // TGLDrawTechniqueFFP
  //
  {: Fixed function pipeline draw technique. }
  TGLStaticMeshDrawerFFP = class(TGLAbstractDrawTechnique)
  protected
    { Protected Declarations }
    FCurrentMesh: TGL3xStaticMesh;
  public
    { Public Declarations }
    constructor Create;
    procedure DrawCurrentMesh; override;
  end;

  TGLVAOHandleTree = {$IFDEF GLS_GENERIC_PREFIX}specialize{$ENDIF}
    GRedBlackTree < TGLProgramHandle, TGLVertexArrayHandle > ;

  TGLMeshState = record
    PerProgramVAO: TGLVAOHandleTree;
    LastInstData: TGL3xInstanceData;
    LastRevision: Integer;
    ArraySectorIndex: Integer;
    ElementSectorIndex: Integer;
  end;
  PGLMeshState = ^TGLMeshState;

  TPoolSector = record
    Offset: PtrUInt;
    Size: Cardinal;
    MeshState: PGLMeshState;
  end;
  PPoolSector = ^TPoolSector;

  TPoolMap = {$IFDEF GLS_GENERIC_PREFIX}specialize{$ENDIF}
    GList<TPoolSector>;

  // TGLStaticMeshDrawerFFP
  //
  {: Programable pipeline draw technique. }
  TGLStaticMeshDrawerPP = class(TGLAbstractDrawTechnique)
  protected
    { Protected Declarations }
    FMeshState: PGLMeshState;
    FMeshStates: array of TGLMeshState;

    FArrayHandle: TGLVBOArrayBufferHandle;
    FElementHandle: TGLVBOElementArrayHandle;
    FStateHandle: TGLVertexArrayHandle;
    FArrayBufferMap: TPoolMap;
    FElementBufferMap: TPoolMap;

    procedure AllocateBuffers;
    procedure PlaceInBuffers(AMesh: TGLAbstractMesh);
    procedure BindVertexArray;
    procedure Draw;
    procedure FreeBuffers; stdcall;

    procedure OnArrayBufferMapChanged(Sender: TObject; const Item: TPoolSector; Action: TListNotification);
    procedure OnElementBufferMapChanged(Sender: TObject; const Item: TPoolSector; Action: TListNotification);
  public
    { Public Declarations }
    constructor Create;
    destructor Destroy; override;

    procedure DrawCurrentMesh; override;
  end;

  DrawManager = class(TGLSAbstractManager)
  protected
    { Protected Declarations }
    class procedure Initialize;
    class procedure Finalize;
    class procedure LoadResourceList;
    class procedure ClearResources;
    // Design time notifications
    class procedure NotifyProjectOpened; override;
    class procedure NotifyProjectClosed; override;
    class procedure NotifyContextCreated; override;
    class procedure NotifyBeforeCompile; override;
    class function GetPrimitiveMask: TGLMeshPrimitives;
    class function GetInstances: LongWord;
    class function GetPatchSize: LongWord;
    class procedure SetPrimitiveMask(AValue: TGLMeshPrimitives);
    class procedure SetInstances(AValue: LongWord);
    class procedure SetPatchSize(AValue: LongWord);
    class procedure DoDraw;
  public
    { Public Declarations }
    class function FillResourceList(AList: TStringList): Boolean; override;
    class procedure MakeUniqueItemName(var AName: string; AClass: TGLAbstractNameClass); override;

    class procedure Draw(const AMeshName: IGLName); overload;
    class procedure Draw(const AMeshName, AIntancesDataName: IGLName); overload;
    class procedure Draw(const AMeshName: IGLName; const ABatchIndeces: TGLBatchIndices); overload;

    property PrimitiveMask: TGLMeshPrimitives read GetPrimitiveMask write SetPrimitiveMask;
    property InstanceNumber: LongWord read GetInstances write SetInstances;
    property PatchSize: LongWord read GetPatchSize write SetPatchSize;
  end;

implementation

uses
  GLStrings,
  VectorGeometry,
  VectorTypes,
  VectorLists,
  GLSLog;

const
  cPrimitiveType: array[mpTRIANGLES..mpPATCHES] of
    GLenum =
    (
    GL_TRIANGLES,
    GL_TRIANGLE_STRIP,
    GL_TRIANGLE_FAN,
    GL_POINTS,
    GL_LINES,
    GL_LINE_LOOP,
    GL_LINE_STRIP,
    GL_LINES_ADJACENCY,
    GL_LINE_STRIP_ADJACENCY,
    GL_TRIANGLES_ADJACENCY,
    GL_TRIANGLE_STRIP_ADJACENCY,
    GL_PATCHES
    );

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
  vCurrentInstancesData: TGL3xInstanceData;
  vCurrentBatchIndices: TGLBatchIndices;
  vPrimitiveMask: TGLMeshPrimitives = cAllMeshPrimitive;
  vInstanceNumber: LongWord;
  vPatchSize: LongWord;

procedure SetVertexAttribPointer(AFormat: TGLSLDataType; ASlot: TGLuint; AOffset: Pointer; ADivisor: TGLuint);
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
    Assert(False, glsErrorEx + glsUnknownType);
  end;
  if GL.ARB_instanced_arrays then
    GL.VertexAttribDivisor(ASlot, ADivisor);
end;

function IsPromitiveSupported(APrimitive: TGLMeshPrimitive): Boolean;
begin
  if (APrimitive >= mpLINES_ADJACENCY)
    and (APrimitive <= mpTRIANGLE_STRIP_ADJACENCY) then
    Result := GL.EXT_gpu_shader4
  else if APrimitive = mpPATCHES then
    Result := GL.ARB_tessellation_shader
  else
    Result := True;
end;

procedure ArrayHandleDestroyer(
  k: TGLProgramHandle;
  AHandle: TGLVertexArrayHandle;
  out Flag: Boolean);
begin
  AHandle.Destroy;
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
    if Assigned(DrawTechniqueClasses[I])
      and (DrawTechniqueClasses[I].ClassName = Self.ClassName) then
      exit(DrawTechniqueClasses[I]);
  I := Length(DrawTechniqueClasses);
  SetLength(DrawTechniqueClasses, I + 1);
  Result := inherited NewInstance;
  DrawTechniqueClasses[I] := TGLAbstractDrawTechnique(Result);
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

constructor TGLStaticMeshDrawerFFP.Create;
begin
  if Self = nil then
    exit;
end;

procedure TGLStaticMeshDrawerFFP.DrawCurrentMesh;
begin
end;

{$ENDREGION 'TGLStaticMeshDrawerFFP'}

{$REGION 'TGLStaticMeshDrawerPP'}
// ------------------
// ------------------ TGLStaticMeshDrawerPP ------------------
// ------------------

constructor TGLStaticMeshDrawerPP.Create;
begin
  if Self = nil then
    exit;
  FArrayHandle := TGLVBOArrayBufferHandle.Create;
  FArrayBufferMap := TPoolMap.Create;
  FArrayBufferMap.OnChange := OnArrayBufferMapChanged;
  FElementHandle := TGLVBOElementArrayHandle.Create;
  FElementBufferMap := TPoolMap.Create;
  FElementBufferMap.OnChange := OnElementBufferMapChanged;
end;

destructor TGLStaticMeshDrawerPP.Destroy;
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
      FMeshStates[I].PerProgramVAO.ForEach(ArrayHandleDestroyer);
      FMeshStates[I].PerProgramVAO.Destroy;
    end;
end;

procedure TGLStaticMeshDrawerPP.AllocateBuffers;
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
      GLSLogger.LogError('Attempt to allocate static buffer in unshared context');
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
      GLSLogger.LogInfo(Format('Free graphic memory avaible - %dM', [VBOFreeMem[1] div 1024]));
      VBOPool := VBOFreeMem[1] * 1024 div 4;
      VBOPool := MinInteger(VBOPool, 2*VBO_STATIC_POOL_SIZE);
    end
    else if GL.NVX_gpu_memory_info then
    begin
      GL.GetIntegerv(GL_GPU_MEMORY_INFO_TOTAL_AVAILABLE_MEMORY_NVX, @VBOFreeMem[1]);
      GLSLogger.LogInfo(Format('Free graphic memory avaible - %dM', [VBOFreeMem[1] div 1024]));
      VBOPool := VBOFreeMem[1] * 1024 div 4;
      VBOPool := MinInteger(VBOPool, 2*VBO_STATIC_POOL_SIZE);
    end
    else
    begin
      VBOPool := VBO_STATIC_POOL_SIZE;
      GLSLogger.LogInfo('Can''t get info about graphic memory. Allocate pool size of 16M');
    end;
    ArraySector.Size := 3 * VBOPool div 4;
    ElementSector.Size := VBOPool - ArraySector.Size;
    GLSLogger.LogInfo(Format('Allocated static vertex buffer pool - %dM', [ArraySector.Size div $100000]));
    GLSLogger.LogInfo(Format('Allocated static element buffer pool - %dM', [ElementSector.Size div $100000]));

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

procedure TGLStaticMeshDrawerPP.FreeBuffers;
begin
  FArrayHandle.Destroy;
  FElementHandle.Destroy;
end;

procedure TGLStaticMeshDrawerPP.PlaceInBuffers(AMesh: TGLAbstractMesh);
var
  a, I, J: Integer;
  lDataSize: array[0..GLS_VERTEX_ATTR_NUM - 1] of Cardinal;
  RequestSize, Size, Offset, maxIndexValue: Cardinal;
  ElementBufferSource: Pointer;
  lMesh: TAccesableMesh;
  ArraySector: TPoolSector;
  ElementSector: TPoolSector;
  pSector: PPoolSector;
  BD: T4ByteData;
begin
  lMesh := TAccesableMesh(AMesh);
  // Calculate size of array
  RequestSize := 0;
  for a := GLS_VERTEX_ATTR_NUM - 1 downto 0 do
    if Assigned(lMesh.FAttributes[a]) then
    begin
      lDataSize[a] := lMesh.FAttributeArrays[a].Count * SizeOf(T4ByteData);
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
        I := J+1;
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
          for I := J+1 to I do
            FArrayBufferMap.Delete(J+1);
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
        if (FArrayBufferMap[I].MeshState = nil)
          and (FArrayBufferMap[I].Size >= RequestSize)
          and (FArrayBufferMap[I].Size <= Size) then
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
  if not lMesh.FBlank then
  begin
    for a := GLS_VERTEX_ATTR_NUM - 1 downto 0 do
      if Assigned(lMesh.FAttributes[a]) then
      begin
        FArrayHandle.BufferSubData(
          Offset,
          lDataSize[a],
          lMesh.FAttributeArrays[a].List);
        Inc(Offset, lDataSize[a]);
      end;
  end;

  if lMesh.FHasIndices then
  begin
    maxIndexValue := lMesh.FAttributeArrays[15].Count div GLSLTypeComponentCount(lMesh.FDataFormat[15]);
    // Adjust index type according it's number
    if (maxIndexValue + 1 < $10000) and not lMesh.FBlank and not IsDesignTime then
    begin
      lMesh.FRestartIndex := $FFFF;
      RequestSize := lMesh.FElementBuffer.Count * SizeOf(TGLushort);
      GetMem(ElementBufferSource, RequestSize);
      for I := lMesh.FElementBuffer.Count - 1 downto 0 do
      begin
        BD := lMesh.FElementBuffer[I];
        PWordVector(ElementBufferSource)[I] := BD.Word.Value[0];
      end;
    end
    else
    begin
      lMesh.FRestartIndex := $FFFFFFFF;
      RequestSize := lMesh.FElementBuffer.Count * SizeOf(TGLuint);
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
          I := J+1;
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
            for I := J+1 to I do
              FElementBufferMap.Delete(J+1);
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
          if (FElementBufferMap[I].MeshState = nil)
            and (FElementBufferMap[I].Size >= RequestSize)
            and (FElementBufferMap[I].Size <= Size) then
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
      if not lMesh.FBlank then
        FElementHandle.BufferSubData(Offset, RequestSize, ElementBufferSource);
      FreeMem(ElementBufferSource);
    end
    else if not lMesh.FBlank then
    begin
      FElementHandle.BufferSubData(Offset, RequestSize, lMesh.FElementBuffer.List);
    end;
  end;
end;

procedure TGLStaticMeshDrawerPP.BindVertexArray;
var
  lProg: TGLProgramHandle;
  lMesh: TAccesableStaticMesh;
//  lInstData: TAccesableInstanceData;

  I, L: Integer;
  Offset, Size: PtrUInt;
  EnabledLocations: array[0..GLS_VERTEX_ATTR_NUM - 1] of Boolean;
begin
  // Check shader program binding
  lProg := TAccesableShaderManager.CurrentProgram;
  if lProg = nil then
  begin
    GLSLogger.LogError(glsNoShader);
    Abort;
  end;

  lMesh := TAccesableStaticMesh(vCurrentMesh);
  I := lMesh.Name.IndexInManagerArray;
  if High(FMeshStates) < I then
    SetLength(FMeshStates, I+1);
  FMeshState := @FMeshStates[I];

  // First time mesh allocation
  if FMeshState.PerProgramVAO = nil then
  begin
    FMeshState.ArraySectorIndex := -1;
    FMeshState.ElementSectorIndex := -1;
    PlaceInBuffers(vCurrentMesh);
    FMeshState.PerProgramVAO := TGLVAOHandleTree.Create(CompareProgram, nil);
    FMeshState.LastRevision := lMesh.FRevisionNum;
  end
  else if IsDesignTime then
    PlaceInBuffers(vCurrentMesh);

  // Find VAO associated with current program
  if not FMeshStates[I].PerProgramVAO.Find(lProg, FStateHandle) then
  begin
    FStateHandle := TGLVertexArrayHandle.Create;
    FMeshState.PerProgramVAO.Add(lProg, FStateHandle);
  end;
  FStateHandle.AllocateHandle;

  // Check revision (mesh structire changing)
  if FMeshState.LastRevision <> lMesh.FRevisionNum then
  begin
    PlaceInBuffers(vCurrentMesh);
    FMeshState.LastRevision := lMesh.FRevisionNum;
    FStateHandle.NotifyChangesOfData;
  end;

  // Chack instance data changing
  if FMeshState.LastInstData <> vCurrentInstancesData then
  begin
    FMeshState.LastInstData := vCurrentInstancesData;
    FStateHandle.NotifyChangesOfData;
  end;

  if FStateHandle.IsDataNeedUpdate then
  begin
    // Check attribute quering by shader
    if attrNormal.Location > -1 then
      lMesh.AttributeRequest(attrNormal);
    if attrTexCoord0.Location > -1 then
      lMesh.AttributeRequest(attrTexCoord0);
    if attrTangent.Location > -1 then
      lMesh.AttributeRequest(attrTangent);

    // Uniting all states and buffers in one vertex array object
    FStateHandle.Bind;

    // Need to direct bind array buffer for correctly VertexAttribPointer set up
    if CurrentGLcontext.GLStates.ArrayBufferBinding = FArrayHandle.Handle then
      GL.BindBuffer(GL_ARRAY_BUFFER, CurrentGLcontext.GLStates.ArrayBufferBinding)
    else
      FArrayHandle.Bind;
    FElementHandle.Bind;

    if IsDesignTime then
      Offset := 0
    else
      Offset := FArrayBufferMap[FMeshState.ArraySectorIndex].Offset;

    // Predisable attributes
    for I := 0 to GLS_VERTEX_ATTR_NUM - 1 do
      EnabledLocations[I] := False;

    for I := GLS_VERTEX_ATTR_NUM - 1 downto 0 do
    begin
      if Assigned(lMesh.FAttributes[I]) then
      begin
        Size := PtrUint(lMesh.FAttributeArrays[I].DataSize);
        L := lMesh.FAttributes[I].Location;
        if (Size > 0) and (L > -1) then
        begin
          EnabledLocations[L] := True;
          SetVertexAttribPointer(lMesh.FDataFormat[I], L, pointer(Offset), lMesh.FAttributeDivisor[I]);
        end;
        Offset := Offset + Size;
      end;
    end;

//    if Assigned(vCurrentInstancesData) and
//      GL.ARB_instanced_arrays then
//    begin
//      lInstData := TAccesableInstanceData(vCurrentInstancesData);
//      for I := 0 to GLS_VERTEX_ATTR_NUM - 1 do
//      begin
//        if Assigned(lInstData.FAttributes[I]) then
//        begin
//          L := lInstData.FAttributes[I].Location;
//          if (L > -1) then
//          begin
//            EnabledLocations[L] := True;
//            // Setup divisor for instancing
//            GL.VertexAttribDivisor(L, lInstData.FAttributeDivisor[I]);
//            // Setup Client Attributes pointer
//            SetVertexAttribPointer(lInstData.FDataFormat[I], L, pointer(Offset));
//          end;
//          Offset := Offset + Cardinal(lInstData.FAttributeArrays[I].DataSize);
//        end;
//      end;
//    end;

    // Enable engagement attributes array
    with CurrentGLContext.GLStates do
    begin
      for I := GLS_VERTEX_ATTR_NUM - 1 downto 0 do
        EnableVertexAttribArray[I] := EnabledLocations[I];
    end;
    if not IsDesignTime then
      FStateHandle.NotifyDataUpdated;
  end
  else
    FStateHandle.Bind;

  with CurrentGLContext.GLStates do
  begin
    EnablePrimitiveRestart := lMesh.FHasIndices;
    PrimitiveRestartIndex := lMesh.FRestartIndex;
  end;
end;

procedure TGLStaticMeshDrawerPP.DrawCurrentMesh;
begin
  BindVertexArray;
  Draw;
  FStateHandle.UnBind;
end;

procedure TGLStaticMeshDrawerPP.Draw;

  var
    p: Integer;
    IndexType: TGLEnum;
    lMesh: TAccesableStaticMesh;

{$REGION 'Draw subroutines'}

  procedure DoDrawPortion(const APortion: TGLMeshDrawPortion);

    var
      glPrimitive: TGLEnum;
      offset: Pointer;

    procedure HardwareInstancing;
    begin
      if lMesh.FHasIndices then
        GL.DrawElementsInstanced(
          glPrimitive,
          APortion.ElementCount,
          IndexType,
          offset,
          vInstanceNumber)
      else
        GL.DrawArraysInstanced(
          glPrimitive,
          APortion.VertexOffset,
          APortion.VertexCount,
          vInstanceNumber);
    end;

    procedure PseudoInstancing;
    var
      uniform: GLInt;
      i: Integer;
    begin
      uniform := GL.GetUniformLocation(
        CurrentGLContext.GLStates.CurrentProgram,
        PGLChar(AnsiString(uniformInstanceID.Name)));
      if uniform = -1 then
        exit;

      if lMesh.FHasIndices then
      begin
        for i := 0 to vInstanceNumber - 1 do
        begin
          GL.Uniform1i(uniform, i);
          GL.DrawElements(
            glPrimitive,
            APortion.ElementCount,
            IndexType,
            offset);
        end;
        GL.Uniform1i(uniform, 0);
      end
      else
      begin
        for i := 0 to vInstanceNumber - 1 do
        begin
          GL.Uniform1i(uniform, i);
          GL.DrawArrays(
            glPrimitive,
            APortion.VertexOffset,
            APortion.VertexCount);
        end;
        GL.Uniform1i(uniform, 0);
      end;
    end;

  begin
    if APortion.PrimitiveType in vPrimitiveMask then
    begin
      // Check the HW support of primitives
      if not IsPromitiveSupported(APortion.PrimitiveType) then
        exit;

      {: Primitives without adjacency should not be drawn with
         primitives with adjacency }
//      if Assigned(CurrentClient.BuiltProp)
//        and CurrentClient.BuiltProp.TriangleAdjacency
//        and not ((CurrentClient.PrimitiveType[p] = GLVBOM_TRIANGLE_STRIP_ADJACENCY)
//        or (CurrentClient.PrimitiveType[p] = GLVBOM_TRIANGLES_ADJACENCY)) then
//        continue;

//      if CurrentClient.PrimitiveType[p] = GLVBOM_PATCHES then
//        GL.PatchParameteri(GL_PATCH_VERTICES, CurrentClient.VerticesInPatch);

      glPrimitive := cPrimitiveType[APortion.PrimitiveType];
      if lMesh.FHasIndices then
        if IsDesignTime then
          Offset := Pointer(APortion.ElementOffset)
        else
          Offset := Pointer(FElementBufferMap[FMeshState.ElementSectorIndex].Offset +
          APortion.ElementOffset);

      if vInstanceNumber > 0 then
      begin
        if GL.EXT_draw_instanced or GL.ARB_draw_instanced then
          HardwareInstancing
        else
          PseudoInstancing;
      end
      else if lMesh.FHasIndices then
      begin
        GL.DrawElements(
          glPrimitive,
          APortion.ElementCount,
          IndexType,
          offset);
      end
      else
      begin
        GL.DrawArrays(
          glPrimitive,
          APortion.VertexOffset,
          APortion.VertexCount);
      end;
    end;
  end;
{$ENDREGION}

begin
  lMesh := TAccesableStaticMesh(vCurrentMesh);

  if lMesh.FRestartIndex = $FFFFFFFF then
    IndexType := GL_UNSIGNED_INT
  else
    IndexType := GL_UNSIGNED_SHORT;

  if not Assigned(vCurrentBatchIndices) then
  begin
    // Draw all batches
    for p := 0 to High(lMesh.FDrawPortions) do
    begin
      DoDrawPortion(lMesh.FDrawPortions[p]);
    end;
  end
  else
  begin
    // Draw set of batches
    for p := 0 to High(vCurrentBatchIndices) do
    begin
      DoDrawPortion(lMesh.FDrawPortions[vCurrentBatchIndices[p]]);
    end;
  end;
end;

procedure TGLStaticMeshDrawerPP.OnArrayBufferMapChanged(Sender: TObject; const Item: TPoolSector; Action: TListNotification);
begin
  if Assigned(Item.MeshState) then
    Item.MeshState.ArraySectorIndex := FArrayBufferMap.IndexOf(Item);
end;

procedure TGLStaticMeshDrawerPP.OnElementBufferMapChanged(Sender: TObject; const Item: TPoolSector; Action: TListNotification);
begin
  if Assigned(Item.MeshState) then
    Item.MeshState.ElementSectorIndex := FElementBufferMap.IndexOf(Item);
end;

{$ENDREGION 'TGLStaticMeshDrawerPP'}

{$REGION 'DrawManager'}
// ------------------
// ------------------ DrawManager ------------------
// ------------------

class procedure DrawManager.Initialize;
begin
  RegisterGLSceneManager(DrawManager);
  vPrimitiveMask := cAllMeshPrimitive;
  TGLStaticMeshDrawerPP.Create;
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
begin
  for I := 0 to High(DrawTechniqueClasses) do
  begin
    if DrawTechniqueClasses[I].ClassType = TGLStaticMeshDrawerPP then
      TGLStaticMeshDrawerPP(DrawTechniqueClasses[I]).AllocateBuffers;
  end;
end;

class procedure DrawManager.NotifyBeforeCompile;
begin
end;

class function DrawManager.FillResourceList(AList: TStringList): Boolean;
begin
  Result := False;
end;

class procedure DrawManager.MakeUniqueItemName(var AName: string; AClass: TGLAbstractNameClass);
begin
end;

class function DrawManager.GetPrimitiveMask: TGLMeshPrimitives;
begin
  CheckCall;
  Result := vPrimitiveMask;
end;

class function DrawManager.GetInstances: LongWord;
begin
  CheckCall;
  Result := vInstanceNumber;
end;

class function DrawManager.GetPatchSize: LongWord;
begin
  CheckCall;
  Result := vPatchSize;
end;

class procedure DrawManager.SetPrimitiveMask(AValue: TGLMeshPrimitives);
begin
  CheckCall;
  vPrimitiveMask := AValue;
end;

class procedure DrawManager.SetInstances(AValue: LongWord);
begin
  CheckCall;
  vInstanceNumber := AValue;
end;

class procedure DrawManager.SetPatchSize(AValue: LongWord);
begin
  CheckCall;
  vPatchSize := AValue;
end;

class procedure DrawManager.DoDraw;
var
  techClass: TGLAbstractDrawTechniqueClass;
  tech: TGLAbstractDrawTechnique;
begin
  tech := nil;
  if vCurrentMesh is TGL3xStaticMesh then
  begin
    if TAccesableShaderManager.CurrentProgram <> nil then
      techClass := TGLStaticMeshDrawerPP
    else
      techClass := TGLStaticMeshDrawerFFP;
    tech := techClass.Create;
  end;

  if Assigned(tech) then
    tech.DrawCurrentMesh;
end;

class procedure DrawManager.Draw(const AMeshName: IGLName);
begin
  try
    BeginWork;
    vCurrentMesh := TAccesableMeshManager.GetMesh(AMeshName);
    vCurrentInstancesData := nil;
    vCurrentBatchIndices := nil;
    DoDraw;
  finally
    EndWork;
  end;
end;

class procedure DrawManager.Draw(const AMeshName, AIntancesDataName: IGLName);
var
  lMesh: TGLAbstractMesh;
begin
  try
    BeginWork;
    vCurrentMesh := TAccesableMeshManager.GetMesh(AMeshName);

    lMesh := TAccesableMeshManager.GetMesh(AIntancesDataName);
    if lMesh is TGL3xInstanceData then
      vCurrentInstancesData := TGL3xInstanceData(lMesh);

    vCurrentBatchIndices := nil;
    DoDraw;
  finally
    EndWork;
  end;
end;

class procedure DrawManager.Draw(const AMeshName: IGLName; const ABatchIndeces: TGLBatchIndices);
begin
  try
    BeginWork;
    vCurrentMesh := TAccesableMeshManager.GetMesh(AMeshName);
    vCurrentInstancesData := nil;
    vCurrentBatchIndices := ABatchIndeces;
    DoDraw;
  finally
    EndWork;
  end;
end;

{$ENDREGION 'DrawManager'}

{$IFDEF GLS_EXPERIMENTAL}

initialization

  DrawManager.Initialize;

finalization

  DrawManager.Finalize;

{$ENDIF GLS_EXPERIMENTAL}
end.

