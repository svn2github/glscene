//
// This unit is part of the GLScene Project   
//
{: GLS.FileDAE<p>

	Quake2 MD2 vector file format implementation.<p>

	<b>History :</b><font size=-1><ul>
      <li>17/01/14 - PW - Bugfixed for XE5
      <li>16/01/14 - PW - Added $I GLScene.inc
      <li>15/01/14 - PW - Creation
	</ul></font>
}
unit GLS.FileDAE;

interface

{$I GLScene.inc}

uses
  System.Classes, System.SysUtils,
  FMX.DAE.Importer, FMX.DAE.Model,

  GLS.VectorFileObjects, GLS.ApplicationFileIO;

type
   // TGLFileDAE
   //
   {: The DAE vector file (COLLADA actor file).<p>
      Stores a set of "frames" describing the different postures of the actor,
      it may be animated by TGLActor. The "Skin" must be loaded indepentendly
      (the whole mesh uses a single texture bitmap).<p>}
   TGLFileDAE = class(TVectorFile)
      public
         { Public Declarations }
         class function Capabilities : TDataFileCapabilities; override;
         procedure LoadFromStream(aStream : TStream); override;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TGLDAEVectorFile ------------------
// ------------------

// Capabilities
//
class function TGLFileDAE.Capabilities : TDataFileCapabilities;
begin
   Result:=[dfcRead];
end;

// LoadFromStream
//
procedure TGLFileDAE.LoadFromStream(aStream : TStream);
var
   i, j : Integer;
   DAEFile : TGLFileDAE;
   mesh : TMorphableMeshObject;
   faceGroup : TFGIndexTexCoordList;
   morphTarget : TMeshMorphTarget;
begin
   { TODO : E2035 Not enough actual parameters }
   (*DAEFile:=TGLFileDAE.Create();*)
   DAEFile.LoadFromStream(aStream);
   try
      // retrieve mesh data
      mesh:=TMorphableMeshObject.CreateOwned(Owner.MeshObjects);
      with mesh, DAEFile do begin
         Mode:=momFaceGroups;
         faceGroup:=TFGIndexTexCoordList.CreateOwned(FaceGroups);
         with faceGroup do begin
            MaterialName:='';
            { TODO : E2003 Undeclared identifier: 'iTriangles' }
            (*
            VertexIndices.Capacity:=iTriangles*3;
            TexCoords.Capacity:=iTriangles*3;
            // copy the face list
            for i:=0 to iTriangles-1 do with IndexList[i] do begin
               Add(a, a_s, -a_t);
               Add(b, b_s, -b_t);
               Add(c, c_s, -c_t);
            end;
            *)
         end;
         // retrieve frames data (morph targets)
         { TODO : E2003 Undeclared identifier: 'iFrames' }
         (*
         for i:=0 to iFrames-1 do begin
            morphTarget:=TMeshMorphTarget.CreateOwned(MorphTargets);
            with morphTarget do begin
               Name:='Frame'+IntToStr(i);
               Vertices.Capacity:=iVertices;
               for j:=0 to iVertices-1 do
                  Vertices.Add(VertexList[i][j]);
               BuildNormals(faceGroup.VertexIndices, momTriangles);
            end;
         end;
         *)
      end;
      if GetOwner is TGLActor then with TGLActor(GetOwner).Animations do begin
         Clear;
         { TODO : E2003 Undeclared identifier: 'frameNames' }
         (*
         with DAEFile do for i:=0 to frameNames.Count-1 do with Add do begin
            Name:=frameNames[i];
            Reference:=aarMorph;
            StartFrame:=Integer(frameNames.Objects[i]);
            if i<frameNames.Count-1 then
               EndFrame:=Integer(frameNames.Objects[i+1])-1
            else EndFrame:=iFrames-1;
         end;
         *)
      end;
      if mesh.MorphTargets.Count>0 then
         mesh.MorphTo(0);
   finally
      DAEFile.Free;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterVectorFileFormat('dae', 'COLLADA model files', TGLFileDAE);

end.
