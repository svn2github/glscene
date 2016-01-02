//
// This unit is part of the GLScene Project   
//
{: VKS.Polyhedron<p>

   Standard polyhedrons

   <b>History : </b><font size=-1><ul>
      <li>10/03/13 - PW - Added TVKTetrahedron and TVKOctahedron classes
      <li>23/08/10 - Yar - Added VKS.OpenGLTokens to uses
      <li>20/01/04 - SG - Added TVKIcosahedron
      <li>21/07/03 - EG - Creation from VKS.Objects split
   </ul></font>
}
unit VKS.Polyhedron;

interface

uses
  System.Classes,

  VKS.Scene, VKS.Objects, VKS.VectorGeometry, VKS.RenderContextInfo;

type

  // TVKDodecahedron
  //
  {: A Dodecahedron.<p>
     The dodecahedron has no texture coordinates defined, ie. without using
     a texture generation mode, no texture will be mapped. }
  TVKDodecahedron = class(TVKSceneObject)
  public
    { Public Declarations }
    procedure BuildList(var rci: TRenderContextInfo); override;
  end;

  // TVKIcosahedron
  //
  {: A Icosahedron.<p>
     The icosahedron has no texture coordinates defined, ie. without using
     a texture generation mode, no texture will be mapped. }
  TVKIcosahedron = class(TVKSceneObject)
  public
    { Public Declarations }
    procedure BuildList(var rci: TRenderContextInfo); override;
  end;

  // TVKOctahedron
  //
  {: A Octahedron.<p>
     The octahedron has no texture coordinates defined, ie. without using
     a texture generation mode, no texture will be mapped. }
  TVKOctahedron = class(TVKSceneObject)
  public
    { Public Declarations }
    procedure BuildList(var rci: TRenderContextInfo); override;
  end;

  // TVKTetrahedron
  //
  {: A Tetrahedron.<p>
     The tetrahedron has no texture coordinates defined, ie. without using
     a texture generation mode, no texture will be mapped. }
  TVKTetrahedron = class(TVKSceneObject)
  public
    { Public Declarations }
    procedure BuildList(var rci: TRenderContextInfo); override;
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

procedure TVKDodecahedron.BuildList(var rci: TRenderContextInfo);
begin
  DodecahedronBuildList;
end;

// ------------------
// ------------------ TVKIcosahedron ------------------
// ------------------

// BuildList
//

procedure TVKIcosahedron.BuildList(var rci: TRenderContextInfo);
begin
  IcosahedronBuildList;
end;

//--------------------
//--------------------  TVKOctahedron ------------------------
//--------------------

// BuildList
//
procedure TVKOctahedron.BuildList(var rci: TRenderContextInfo);
begin
  OctahedronBuildList;
end;

//--------------------
//--------------------  TVKTetrahedron ------------------------
//--------------------

// BuildList
//
procedure TVKTetrahedron.BuildList(var rci: TRenderContextInfo);
begin
  TetrahedronBuildList;
end;

initialization
  //-------------------------------------------------------------
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  RegisterClasses([TVKDodecahedron, TVKIcosahedron, TVKOctahedron, TVKTetrahedron]);

end.

