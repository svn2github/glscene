{: GLFileQ3BSP<p>

    Support-code to load Q3BSP Files into TGLFreeForm-Components in GLScene.<p>
    Note that you must manually add this unit to one of your project's uses
    to enable support for OBJ & OBJF at run-time.<p>

	<b>History : </b><font size=-1><ul>
      <li>30/01/03 - EG - Creation
   </ul><p>
}
unit GLFileQ3BSP;

interface

uses Classes, GLVectorFileObjects, GLMisc;

type

   // TGLQ3BSPVectorFile
   //
   {: The Q3BSP vector file (Quake III BSP).<p> }
   TGLQ3BSPVectorFile = class(TVectorFile)
      public
         { Public Declarations }
         class function Capabilities : TDataFileCapabilities; override;

         procedure LoadFromStream(aStream: TStream); override;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses Q3BSP, Geometry, VectorLists, SysUtils, GLBSP;

// ------------------
// ------------------ TGLSTLVectorFile ------------------
// ------------------

// Capabilities
//
class function TGLQ3BSPVectorFile.Capabilities : TDataFileCapabilities;
begin
   Result:=[dfcRead];
end;

// LoadFromStream
//
procedure TGLQ3BSPVectorFile.LoadFromStream(aStream : TStream);
var
   bsp : TQ3BSP;
   mo : TBSPMeshObject;
   fg : TFGBSPNode;
   i, j, k : Integer;
   facePtr : PBSPFace;
begin
   bsp:=TQ3BSP.Create(aStream);
   try
      mo:=TBSPMeshObject.CreateOwned(Owner.MeshObjects);
      // import all geometry first
      mo.Vertices.AdjustCapacityToAtLeast(bsp.NumOfVerts);
      mo.Normals.AdjustCapacityToAtLeast(bsp.NumOfVerts);
      mo.TexCoords.AdjustCapacityToAtLeast(bsp.NumOfVerts);
      for i:=0 to bsp.NumOfVerts-1 do begin
         mo.Vertices.Add(bsp.Vertices[i].Position);
         mo.Normals.Add(bsp.Vertices[i].Normal);
         mo.TexCoords.Add(bsp.Vertices[i].TextureCoord);
      end;
      // Q3 BSP separates tree nodes from leafy nodes, we don't,
      // so we place nodes first, then all leafs afterwards
      // now import all nodes
      for i:=0 to bsp.NumOfNodes-1 do begin
         fg:=TFGBSPNode.CreateOwned(mo.FaceGroups);
         fg.SplitPlane:=bsp.Planes[bsp.Nodes[i].plane];
         fg.PositiveSubNodeIndex:=bsp.Nodes[i].Children[0];
         if fg.PositiveSubNodeIndex<0 then
            fg.PositiveSubNodeIndex:=bsp.NumOfNodes-fg.PositiveSubNodeIndex-1;
         Assert(fg.PositiveSubNodeIndex<bsp.NumOfNodes+bsp.NumOfLeaves);
         Assert(fg.PositiveSubNodeIndex>0);
         fg.NegativeSubNodeIndex:=bsp.Nodes[i].Children[1];
         if fg.NegativeSubNodeIndex<0 then
            fg.NegativeSubNodeIndex:=bsp.NumOfNodes-fg.NegativeSubNodeIndex-1;
         Assert(fg.NegativeSubNodeIndex<bsp.NumOfNodes+bsp.NumOfLeaves);
         Assert(fg.NegativeSubNodeIndex>0);
      end;
      // import faces
      for i:=0 to bsp.NumOfLeaves-1 do begin
         fg:=TFGBSPNode.CreateOwned(mo.FaceGroups);
         for j:=0 to bsp.Leaves[i].NumFaces-1 do begin
            facePtr:=@bsp.Faces[bsp.Leaves[i].FirstFace+j];
            if facePtr.FaceType=FACE_POLYGON then begin
               // Q3 Faces are fans, convert them to regular triangles
               // (may be updated later on)
               for k:=2 to facePtr.numOfVerts-1 do
                  fg.VertexIndices.Add(facePtr.startVertIndex,
                                       facePtr.startVertIndex+k-1,
                                       facePtr.startVertIndex+k);
               // there are also per-leaf mesh references... dunno what they
               // are for, did not encounter them so far... If you have a BSP
               // that has some, and if you know how to make use of them, shout!
            end;
         end;
      end;
   finally
      bsp.Free;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterVectorFileFormat('q3bsp', 'Quake3 BSP files', TGLQ3BSPVectorFile);

   // registering this extension too might be a little abusive right now...
   RegisterVectorFileFormat('bsp', 'BSP files', TGLQ3BSPVectorFile);

end.
