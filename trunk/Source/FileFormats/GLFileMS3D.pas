unit GLFileMS3D;

interface

uses
  Classes, SysUtils, GLVectorFileObjects,  VectorTypes, gltexture;

type
   // TGLMS3DVectorFile
   //
   {: The MilkShape vector file.<p>
      By Mattias Fagerlund, mattias@cambrianlabs.com. Yada yada. Eric rules! }
   TGLMS3DVectorFile = class (TVectorFile)
      public
         { Public Declarations }
         procedure LoadFromStream(aStream : TStream); override;
   end;

implementation

uses
  TypesMS3D;



// FOR DEBUGGING!
{procedure AddString(s : string);
begin
  Form1.Memo1.Lines.Add(s);
end;//}


{ TGLMS3DVectorFile }

procedure TGLMS3DVectorFile.LoadFromStream(aStream: TStream);
var
  // GLScene
  i, j, k, itemp: integer;
  wtemp : word;
  NormalID : integer;
  TexCoordID : integer;
  MO : TMeshObject;
  FaceGroup : TFGVertexNormalTexIndexList;

  GroupList : TList;
  GLLibMaterial : TGLLibMaterial;

  // Milkshape 3d
  ms3d_header : ms3d_header_t;
  nNumVertices : word;
  // ms3d_vertex : ms3d_vertex_t;
  ms3d_vertices : Pms3d_vertex_t_array;
  nNumTriangles : word;

  ms3d_triangle : ms3d_triangle_t;
  ms3d_triangles : Pms3d_triangle_t_array;

  nNumGroups : word;
  Group : Tms3d_group;
  nNumMaterials : word;
  ms3d_material : ms3d_material_t;

  fAnimationFPS : single;
  fCurrentTime : single;
  iTotalFrames : integer;

  nNumJoints : word;
  ms3d_joints : Pms3d_joint_t_array;
  ms3d_joint : ms3d_joint_t;


  procedure AddFaceVertex(ID : integer);
  begin
    // Add the normal to the normals list
    NormalID := MO.Normals.Add(ms3d_triangle.vertexNormals[ID].v);

    // Add the texCoord
    TexCoordID := MO.TexCoords.Add(ms3d_triangle.s[ID], -ms3d_triangle.t[ID]);

    // Add the vertex to the vertex list
    FaceGroup.Add(ms3d_triangle.vertexIndices[ID], NormalID, TexCoordID);
  end;//}
begin
  GroupList := TList.Create;
  FaceGroup := nil;
  ms3d_vertices := nil;
  ms3d_triangles := nil;
  ms3d_joints := nil;

  try
    // First comes the header.
    aStream.ReadBuffer(ms3d_header, sizeof(ms3d_header_t));

    //AddString(ms3d_header.id);
    //AddString(Format('Version=%d',[ms3d_header.version]));

    Assert(ms3d_header.version = 4,
      Format('The MilkShape3D importer can only handle MS3D files of version 4, this is version ', [ms3d_header.id]));

    // Then comes the number of vertices
    aStream.ReadBuffer(nNumVertices, sizeof(nNumVertices));
    //AddString(Format('nNumVertices=%d',[nNumVertices]));

    // Create the vertex list
    MO := TMeshObject.CreateOwned(Owner.MeshObjects);
    MO.Mode := momFaceGroups;

    // Then comes nNumVertices * sizeof (ms3d_vertex_t)

    ms3d_vertices := AllocMem(sizeof(ms3d_vertex_t) * nNumVertices);
    aStream.ReadBuffer(ms3d_vertices^, sizeof(ms3d_vertex_t) * nNumVertices);

    for i := 0 to nNumVertices - 1 do
      with ms3d_vertices[i] do
      begin
        //ms3d_vertex := ms3d_vertices[i];
        //AddString(Format('  Vertex[%d]= (%f, %f, %f)',[i, vertex.x, vertex.y, vertex.z]));

        // Add the vertex to the vertexlist
        MO.Vertices.Add(vertex.v);
      end;

    // number of triangles
    aStream.ReadBuffer(nNumTriangles, sizeof(nNumTriangles));
    //AddString(Format('nNumTriangles=%d',[nNumTriangles]));

    // nNumTriangles * sizeof (ms3d_triangle_t)
    ms3d_triangles := AllocMem(sizeof(ms3d_triangle_t) * nNumTriangles);
    aStream.ReadBuffer(ms3d_triangles^, sizeof(ms3d_triangle_t) * nNumTriangles);

    // Don't do anything with the triangles / faces just yet

    // number of groups
    aStream.ReadBuffer(nNumGroups, sizeof(nNumGroups));
    //AddString(Format('nNumGroups=%d',[nNumGroups]));

    // nNumGroups * sizeof (ms3d_group_t)
    for i := 0 to nNumGroups - 1 do
    begin
      // Read the first part of the group
      Group := Tms3d_group.Create;
      GroupList.Add(Group);
      aStream.ReadBuffer(Group.Flags, sizeof(Group.Flags));
      aStream.ReadBuffer(Group.name, sizeof(Group.name));
      aStream.ReadBuffer(Group.numtriangles, sizeof(Group.numtriangles));

      //AddString('Group = ' + Group.Name);

      for j := 0 to Group.numtriangles - 1 do
      begin
        aStream.ReadBuffer(wtemp, sizeof(wtemp));
        itemp := wtemp;
        Group.triangleIndices.Add(pointer(itemp));
        //AddString('  '+inttostr(integer(Group.triangleIndices[j])));
      end;

      aStream.ReadBuffer(Group.materialIndex, sizeof(Group.materialIndex));

      // if materialindex=-1, then there is no material, and all faces should
      // be added to a base VIL
      if Group.materialIndex = char(-1) then
      begin
        // If there's no base VIL, create one!
        if FaceGroup = nil then
          FaceGroup := TFGVertexNormalTexIndexList.CreateOwned(MO.FaceGroups);

        for j := 0 to Group.numtriangles - 1 do
        begin
          ms3d_triangle := ms3d_triangles[integer(Group.triangleIndices[j])];

          AddFaceVertex(0);
          AddFaceVertex(1);
          AddFaceVertex(2);
        end;
      end;
    end;

    // number of materials
    //aStream.ReadBuffer(nNumMaterials, sizeof(nNumMaterials));
    aStream.ReadBuffer(nNumMaterials, sizeof(nNumMaterials));
    //AddString(Format('nNumMaterials=%d',[nNumMaterials]));

    // nNumMaterials * sizeof (ms3d_material_t)
    for i := 0 to nNumMaterials-1 do
    begin
      aStream.ReadBuffer(ms3d_material, sizeof(ms3d_material_t));
      //AddString(Format('Material[%d] = %s, Tex=%s', [i, ms3d_material.name, ms3d_material.texture]));

      // Create the material, if there's a materiallibrary!
      if Assigned(Owner.MaterialLibrary) then
      begin
        GLLibMaterial := Owner.MaterialLibrary.AddTextureMaterial(ms3d_material.name, ms3d_material.texture);

        GLLibMaterial.Material.FrontProperties.Emission.Color := ms3d_material.emissive;
        GLLibMaterial.Material.FrontProperties.Ambient.Color := ms3d_material.ambient;
        GLLibMaterial.Material.FrontProperties.Diffuse.Color := ms3d_material.diffuse;
        GLLibMaterial.Material.FrontProperties.Specular.Color := ms3d_material.specular;

        // Shinintess is 0 to 128 in both MS3D and GLScene. Why not 0 to 127? Odd.
        GLLibMaterial.Material.FrontProperties.Shininess := round(ms3d_material.shininess);

        // ms3d_material.transparency is allready set as alpha channel on all
        // colors above
        if ms3d_material.transparency<1 then
          GLLibMaterial.Material.BlendingMode := bmTransparency;//}

        // Create a new face group and add all triangles for this material
        // here. We must cycle through all groups that have this material
        FaceGroup := TFGVertexNormalTexIndexList.CreateOwned(MO.FaceGroups);
        FaceGroup.MaterialName := GLLibMaterial.Name;//}

        for j := 0 to GroupList.Count-1 do
        begin
          Group := Tms3d_group(GroupList[j]);
          if Group.materialIndex = char(i) then
            for k := 0 to Group.numtriangles - 1 do
            begin
              ms3d_triangle := ms3d_triangles[integer(Group.triangleIndices[k])];

              AddFaceVertex(0);
              AddFaceVertex(1);
              AddFaceVertex(2);
            end;
        end;
      end;
    end;

    // save some keyframer data
    aStream.ReadBuffer(fAnimationFPS, sizeof(fAnimationFPS));
    aStream.ReadBuffer(fCurrentTime, sizeof(fCurrentTime));
    aStream.ReadBuffer(iTotalFrames, sizeof(iTotalFrames));

    // NOTE NOTE NOTE!
    // From here on, the info isn't used at all, it's only read so that a future
    // enhancement of the loader can also use animations!

    // number of joints
    aStream.ReadBuffer(nNumJoints, sizeof(nNumJoints));

    // nNumJoints * sizeof (ms3d_joint_t)
    ms3d_joints := AllocMem(sizeof(ms3d_joint_t) * nNumTriangles);

    // We have to read the joints one by one!
    for i := 0 to nNumJoints-1 do
    begin
      ms3d_joint := ms3d_joints^[i];

      // Read the joint base
      aStream.ReadBuffer(ms3d_joint.Base, sizeof(ms3d_joint_t_base));

      //AddString(Format('Joint[%d]=%s (parent=%s)',[i,ms3d_joint.Base.name, ms3d_joint.Base.parentname]));
      //AddString(Format('  Rots=%d, Transes=%d',[ms3d_joint.base.numKeyFramesRot, ms3d_joint.base.numKeyFramesTrans]));

      if ms3d_joint.base.numKeyFramesRot>0 then
      begin
        //     ms3d_keyframe_rot_t keyFramesRot[numKeyFramesRot];      // local animation matrices
        // Allocate memory for the rotations
        ms3d_joint.keyFramesRot := AllocMem(sizeof(ms3d_keyframe_rot_t) * ms3d_joint.base.numKeyFramesRot);

        // Read the rotations
        aStream.ReadBuffer(ms3d_joint.keyFramesRot, sizeof(ms3d_keyframe_rot_t) * ms3d_joint.base.numKeyFramesRot);
      end else
        ms3d_joint.keyFramesRot := nil;

      if ms3d_joint.base.numKeyFramesTrans>0 then
      begin
        //     ms3d_keyframe_pos_t keyFramesTrans[numKeyFramesTrans];  // local animation matrices
        // Allocate memory for the translations
        ms3d_joint.keyFramesTrans := AllocMem(sizeof(ms3d_keyframe_pos_t) * ms3d_joint.base.numKeyFramesTrans);

        // Read the translations
        aStream.ReadBuffer(ms3d_joint.keyFramesTrans, sizeof(ms3d_keyframe_pos_t) * ms3d_joint.base.numKeyFramesTrans);
      end else
        ms3d_joint.keyFramesTrans := nil;
    end;

    // Right here, it's time to use the joints - this from the ms3d spec;

    // ***
    // Mesh Transformation:
    //
    // 0. Build the transformation matrices from the rotation and position
    // 1. Multiply the vertices by the inverse of local reference matrix (lmatrix0)
    // 2. then translate the result by (lmatrix0 * keyFramesTrans)
    // 3. then multiply the result by (lmatrix0 * keyFramesRot)
    //
    // For normals skip step 2.
    //
    //
    //
    // NOTE:  this file format may change in future versions!
    //
    //
    // - Mete Ciragan
    // ***

  finally
    if Assigned(ms3d_vertices) then
      FreeMem(ms3d_vertices);

    if Assigned(ms3d_triangles) then
      FreeMem(ms3d_triangles);

    if Assigned(ms3d_joints) then
    begin
      // Free the internal storage of the joint
      for i := 0 to nNumJoints-1 do
      begin
        if Assigned(ms3d_joints[i].keyFramesRot) then
          FreeMem(ms3d_joints[i].keyFramesRot);

        if Assigned(ms3d_joints[i].keyFramesTrans) then
          FreeMem(ms3d_joints[i].keyFramesTrans);
      end;

      FreeMem(ms3d_joints);
    end;

    // Finalize
    for i := 0 to GroupList.Count-1 do
      Tms3d_group(GroupList[i]).Free;
      
    GroupList.Free;
  end;
end;
end.
