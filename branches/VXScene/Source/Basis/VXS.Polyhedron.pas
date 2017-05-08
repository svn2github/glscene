//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
   Standard polyhedrons
   
}
unit VXS.Polyhedron;

interface

uses
  System.Classes,

  VXS.Scene, VXS.Objects, VXS.VectorGeometry, VXS.RenderContextInfo;

type

  // TVXDodecahedron
  //
  { A Dodecahedron. 
     The dodecahedron has no texture coordinates defined, ie. without using
     a texture generation mode, no texture will be mapped. }
  TVXDodecahedron = class(TVXSceneObject)
  public
    
    procedure BuildList(var rci: TVXRenderContextInfo); override;
  end;

  // TVXIcosahedron
  //
  { A Icosahedron. 
     The icosahedron has no texture coordinates defined, ie. without using
     a texture generation mode, no texture will be mapped. }
  TVXIcosahedron = class(TVXSceneObject)
  public
    
    procedure BuildList(var rci: TVXRenderContextInfo); override;
  end;

  // TVXOctahedron
  //
  { A Octahedron. 
     The octahedron has no texture coordinates defined, ie. without using
     a texture generation mode, no texture will be mapped. }
  TVXOctahedron = class(TVXSceneObject)
  public
    
    procedure BuildList(var rci: TVXRenderContextInfo); override;
  end;

  // TVXTetrahedron
  //
  { A Tetrahedron. 
     The tetrahedron has no texture coordinates defined, ie. without using
     a texture generation mode, no texture will be mapped. }
  TVXTetrahedron = class(TVXSceneObject)
  public
    
    procedure BuildList(var rci: TVXRenderContextInfo); override;
  end;


  //-------------------------------------------------------------
  //-------------------------------------------------------------
  //-------------------------------------------------------------
implementation
// ------------------
// ------------------ TVXDodecahedron ------------------
// ------------------

// BuildList
//

procedure TVXDodecahedron.BuildList(var rci: TVXRenderContextInfo);
begin
  DodecahedronBuildList;
end;

// ------------------
// ------------------ TVXIcosahedron ------------------
// ------------------

// BuildList
//

procedure TVXIcosahedron.BuildList(var rci: TVXRenderContextInfo);
begin
  IcosahedronBuildList;
end;

//--------------------
//--------------------  TVXOctahedron ------------------------
//--------------------

// BuildList
//
procedure TVXOctahedron.BuildList(var rci: TVXRenderContextInfo);
begin
  OctahedronBuildList;
end;

//--------------------
//--------------------  TVXTetrahedron ------------------------
//--------------------

// BuildList
//
procedure TVXTetrahedron.BuildList(var rci: TVXRenderContextInfo);
begin
  TetrahedronBuildList;
end;

initialization
  //-------------------------------------------------------------
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  RegisterClasses([TVXDodecahedron, TVXIcosahedron, TVXOctahedron, TVXTetrahedron]);

end.

