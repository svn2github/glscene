//
// This unit is part of the GLScene Project, http://glscene.org
//
unit TypesMS3D;

interface

uses
  Classes,  VectorTypes, gltexture;

const
  MAX_VERTICES   = 8192;
  MAX_TRIANGLES  = 16384;
  MAX_GROUPS     = 128;
  MAX_MATERIALS  = 128;
  MAX_JOINTS     = 128;
  MAX_KEYFRAMES  = 216;

type
  // typedef struct
  // {
  //     byte            flags;                              // SELECTED | HIDDEN
  //     char            name[32];                           //
  //     word            numtriangles;                       //
  //     word            triangleIndices[numtriangles];      // the groups group the triangles
  //     char            materialIndex;                      // -1 = no material
  // } ms3d_group_t;
  Tms3d_group = class
    flags : byte;
    name : array[0..31] of char;
    numtriangles : word;
    triangleIndices : TList;
    materialIndex : char;
    constructor Create;
    destructor Destroy; override;
  end;

  // typdef struct
  // {
  //    char    id[10];                                     // always "MS3D000000"
  //    int     version;                                    // 4
  // } ms3d_header_t;
  {$A-}

  ms3d_header_t = record
    id : array[0..9] of char;
    version : integer;
  end;


  // typedef struct
  // {
  //     byte    flags;                                      // SELECTED | SELECTED2 | HIDDEN
  //     float   vertex[3];                                  //
  //     char    boneId;                                     // -1 = no bone
  //     byte    referenceCount;
  // } ms3d_vertex_t;

  ms3d_vertex_t = record
    flags : byte;
    vertex : TD3DVector;
    boneId : char;
    referenceCount : byte;
  end;

  ms3d_vertex_t_array = array[0..MAX_VERTICES-1] of ms3d_vertex_t;
  Pms3d_vertex_t_array = ^ms3d_vertex_t_array;


  // typedef struct
  // {
  //     word    flags;                                      // SELECTED | SELECTED2 | HIDDEN
  //     word    vertexIndices[3];                           //
  //     float   vertexNormals[3][3];                        //
  //     float   s[3];                                       //
  //     float   t[3];                                       //
  //     byte    smoothingGroup;                             // 1 - 32
  //     byte    groupIndex;                                 //
  // } ms3d_triangle_t;

  ms3d_triangle_t = record
    flags : word;
    vertexIndices : array[0..2] of word;
    vertexNormals : array[0..2] of TD3DVector;
    s : array[0..2] of single;
    t : array[0..2] of single;
    smoothingGroup : byte;  // 1 - 32
    groupIndex : byte;
  end;

  ms3d_triangle_t_array = array[0..MAX_TRIANGLES-1] of ms3d_triangle_t;
  Pms3d_triangle_t_array = ^ms3d_triangle_t_array;

  // typedef struct
  // {
  //     char            name[32];                           //
  //     float           ambient[4];                         //
  //     float           diffuse[4];                         //
  //     float           specular[4];                        //
  //     float           emissive[4];                        //
  //     float           shininess;                          // 0.0f - 128.0f
  //     float           transparency;                       // 0.0f - 1.0f
  //     char            mode;                               // 0, 1, 2 is unused now
  //     char            texture[128];                        // texture.bmp
  //     char            alphamap[128];                       // alpha.bmp
  // } ms3d_material_t;
  ms3d_material_t = record
    name : array[0..31] of char;
    ambient : TColorVector;
    diffuse : TColorVector;
    specular : TColorVector;
    emissive : TColorVector;
    shininess : single;
    transparency : single;
    mode : char;
    texture : array[0..127] of char;
    alphamap : array[0..127] of char;
  end;


  // typedef struct
  // {
  //     float           time;                               // time in seconds
  //     float           rotation[3];                        // x, y, z angles
  // } ms3d_keyframe_rot_t;
  ms3d_keyframe_rot_t = record
    time : single;
    rotation : TD3DVector;
  end;

  ms3d_keyframe_rot_t_array = array[0..MAX_KEYFRAMES-1] of ms3d_keyframe_rot_t;
  Pms3d_keyframe_rot_t_array = ^ms3d_keyframe_rot_t_array;

  // typedef struct
  // {
  //     float           time;                               // time in seconds
  //     float           position[3];                        // local position
  // } ms3d_keyframe_pos_t;
  ms3d_keyframe_pos_t = record
    time : single;
    position : TD3DVector;
  end;

  ms3d_keyframe_pos_t_array = array[0..MAX_KEYFRAMES-1] of ms3d_keyframe_pos_t;
  Pms3d_keyframe_pos_t_array = ^ms3d_keyframe_pos_t_array;

  // typedef struct
  // {
  //     byte            flags;                              // SELECTED | DIRTY
  //     char            name[32];                           //
  //     char            parentName[32];                     //
  //     float           rotation[3];                        // local reference matrix
  //     float           position[3];
  //
  //     word            numKeyFramesRot;                    //
  //     word            numKeyFramesTrans;                  //
  //
  //     ms3d_keyframe_rot_t keyFramesRot[numKeyFramesRot];      // local animation matrices
  //     ms3d_keyframe_pos_t keyFramesTrans[numKeyFramesTrans];  // local animation matrices
  // } ms3d_joint_t;

  ms3d_joint_t_base = record
    flags : byte;
    name : array[0..31] of char;
    parentName : array[0..31] of char;
    rotation : TD3DVector;
    position : TD3DVector;
    numKeyFramesRot : word;
    numKeyFramesTrans : word;
  end;

  ms3d_joint_t = record
    Base : ms3d_joint_t_base;

    keyFramesRot        : Pms3d_keyframe_rot_t_array;
    keyFramesTrans   : Pms3d_keyframe_pos_t_array;
  end;

  ms3d_joint_t_array = array[0..MAX_JOINTS] of ms3d_joint_t;
  Pms3d_joint_t_array = ^ms3d_joint_t_array;

  {$A+}

implementation


{ Tms3d_group }

constructor Tms3d_group.Create;
begin
  triangleIndices := TList.Create;
end;

destructor Tms3d_group.Destroy;
begin
  triangleIndices.Free;

  inherited;
end;


end.
