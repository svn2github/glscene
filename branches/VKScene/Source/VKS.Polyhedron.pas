//
// VKScene project, http://glscene.sourceforge.net 
//
{
   Standard polyhedrons
   
}
unit VKS.Polyhedron;

interface

uses
  System.Classes,

  VKS.Scene, VKS.Objects, VKS.VectorGeometry, VKS.RenderContextInfo;

type

  // TVKDodecahedron
  //
  { A Dodecahedron. 
     The dodecahedron has no texture coordinates defined, ie. without using
     a texture generation mode, no texture will be mapped. }
  TVKDodecahedron = class(TVKSceneObject)
  public
    { Public Declarations }
    procedure BuildList(var rci: TVKRenderContextInfo); override;
  end;

  // TVKIcosahedron
  //
  { A Icosahedron. 
     The icosahedron has no texture coordinates defined, ie. without using
     a texture generation mode, no texture will be mapped. }
  TVKIcosahedron = class(TVKSceneObject)
  public
    { Public Declarations }
    procedure BuildList(var rci: TVKRenderContextInfo); override;
  end;

  // TVKOctahedron
  //
  { A Octahedron. 
     The octahedron has no texture coordinates defined, ie. without using
     a texture generation mode, no texture will be mapped. }
  TVKOctahedron = class(TVKSceneObject)
  public
    { Public Declarations }
    procedure BuildList(var rci: TVKRenderContextInfo); override;
  end;

  // TVKTetrahedron
  //
  { A Tetrahedron. 
     The tetrahedron has no texture coordinates defined, ie. without using
     a texture generation mode, no texture will be mapped. }
  TVKTetrahedron = class(TVKSceneObject)
  public
    { Public Declarations }
    procedure BuildList(var rci: TVKRenderContextInfo); override;
  end;


  //-------------------------------------------------------------
  //-------------------------------------------------------------
  //-------------------------------------------------------------
implementation
// ------------------
// ------------------ TVKDodecahedron ------------------
// ------------------

// BuildList
//

procedure TVKDodecahedron.BuildList(var rci: TVKRenderContextInfo);
begin
  DodecahedronBuildList;
end;

// ------------------
// ------------------ TVKIcosahedron ------------------
// ------------------

// BuildList
//

procedure TVKIcosahedron.BuildList(var rci: TVKRenderContextInfo);
begin
  IcosahedronBuildList;
end;

//--------------------
//--------------------  TVKOctahedron ------------------------
//--------------------

// BuildList
//
procedure TVKOctahedron.BuildList(var rci: TVKRenderContextInfo);
begin
  OctahedronBuildList;
end;

//--------------------
//--------------------  TVKTetrahedron ------------------------
//--------------------

// BuildList
//
procedure TVKTetrahedron.BuildList(var rci: TVKRenderContextInfo);
begin
  TetrahedronBuildList;
end;

initialization
  //-------------------------------------------------------------
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  RegisterClasses([TVKDodecahedron, TVKIcosahedron, TVKOctahedron, TVKTetrahedron]);

end.

