// GLMeshOptimizer
{: Mesh optimization for GLScene.<p>

	<b>History : </b><font size=-1><ul>
      <li>21/08/03 - EG - Added basic mooStandardize support
      <li>03/06/03 - EG - Creation
	</ul></font>
}
unit GLMeshOptimizer;

interface

uses VectorGeometry, GLVectorFileObjects;

type

   // TMeshOptimizerOptions
   //
   TMeshOptimizerOption = (mooStandardize, mooVertexCache, mooSortByMaterials,
                           mooMergeObjects);
   TMeshOptimizerOptions = set of TMeshOptimizerOption;

var
   vDefaultMeshOptimizerOptions : TMeshOptimizerOptions =
      [mooStandardize, mooVertexCache, mooSortByMaterials, mooMergeObjects];

procedure OptimizeMesh(aList : TMeshObjectList; options : TMeshOptimizerOptions); overload;
procedure OptimizeMesh(aList : TMeshObjectList); overload;
procedure OptimizeMesh(aMeshObject : TMeshObject; options : TMeshOptimizerOptions); overload;
procedure OptimizeMesh(aMeshObject : TMeshObject); overload;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses PersistentClasses, VectorLists, MeshUtils;

// OptimizeMesh (list, default options)
//
procedure OptimizeMesh(aList : TMeshObjectList);
begin
   OptimizeMesh(aList, vDefaultMeshOptimizerOptions);
end;

// OptimizeMesh (list, with options)
//
procedure OptimizeMesh(aList : TMeshObjectList; options : TMeshOptimizerOptions);
var
   i, k : Integer;
   mob, mo : TMeshObject;
   fg : TFaceGroup;
   fgvi : TFGVertexIndexList;
begin
   // optimize all mesh objects
   for i:=0 to aList.Count-1 do begin
      OptimizeMesh(aList[i], options);
   end;
   if (mooStandardize in options) then begin
      // drop mesh objects that have become empty
      for i:=aList.Count-1 downto 0 do begin
         if (aList[i].Mode=momFaceGroups) and (aList[i].FaceGroups.Count=0) then
            aList[i].Free;
      end;
   end;
   if (aList.Count>0) and (mooMergeObjects in options) then begin
      mob:=aList[0];
      Assert(mob.Mode=momFaceGroups);
      for i:=1 to aList.Count-1 do begin
         mo:=aList[i];
         Assert(mo.Mode=momFaceGroups);
         k:=mob.Vertices.Count;
         mob.Vertices.Add(mo.Vertices);
         mob.Normals.Add(mo.Normals);
         mob.TexCoords.Add(mo.TexCoords);
         while mo.FaceGroups.Count>0 do begin
            fg:=mo.FaceGroups[0];
            fgvi:=(fg as TFGVertexIndexList);
            fgvi.Owner:=mob.FaceGroups;
            mob.FaceGroups.Add(fgvi);
            mo.FaceGroups.Delete(0);
            fgvi.VertexIndices.Offset(k);
         end;
      end;
      for i:=aList.Count-1 downto 1 do
         aList[i].Free;
   end;
end;

// OptimizeMesh (object, default options)
//
procedure OptimizeMesh(aMeshObject : TMeshObject);
begin
   OptimizeMesh(aMeshObject, vDefaultMeshOptimizerOptions);
end;

// OptimizeMesh (object, with options)
//
procedure OptimizeMesh(aMeshObject : TMeshObject; options : TMeshOptimizerOptions);
var
   i : Integer;
   fg : TFaceGroup;
   coords, texCoords, normals : TAffineVectorList;
   il : TIntegerList;
   materialName : String;
begin
   if (mooMergeObjects in options) then begin
      if aMeshObject.Mode=momFaceGroups then begin
         // remove empty facegroups
         for i:=aMeshObject.FaceGroups.Count-1 downto 0 do begin
            fg:=aMeshObject.FaceGroups[i];
            if fg.TriangleCount=0 then
               fg.Free;
         end;
      end;
   end;

   if (mooStandardize in options) then begin
      if (aMeshObject.Mode<>momFaceGroups) or (aMeshObject.FaceGroups.Count<=1) then begin
         if aMeshObject.FaceGroups.Count=1 then
            materialName:=aMeshObject.FaceGroups[0].MaterialName;
         texCoords:=TAffineVectorList.Create;
         normals:=TAffineVectorList.Create;
         coords:=aMeshObject.ExtractTriangles(texCoords, normals);
         try
            il:=BuildVectorCountOptimizedIndices(coords, normals, texCoords);
            try
               aMeshObject.Clear;
               if il.Count>0 then begin
                  RemapReferences(normals, il);
                  RemapReferences(texCoords, il);
                  RemapAndCleanupReferences(coords, il);
                  aMeshObject.Vertices:=coords;
                  aMeshObject.Normals:=normals;
                  aMeshObject.TexCoords:=texCoords;
                  fg:=TFGVertexIndexList.CreateOwned(aMeshObject.FaceGroups);
                  fg.MaterialName:=materialName;
                  TFGVertexIndexList(fg).VertexIndices:=il;
               end;
            finally
               il.Free;
            end;
         finally
            coords.Free;
            normals.Free;
            texCoords.Free;
         end;
      end else Assert(False, 'Standardization with multiple facegroups not supported... yet.');
   end;

   if (mooVertexCache in options) and (aMeshObject.Mode=momFaceGroups) then begin
       for i:=0 to aMeshObject.FaceGroups.Count-1 do begin
         fg:=aMeshObject.FaceGroups[i];
         if fg.ClassType=TFGVertexIndexList then with TFGVertexIndexList(fg) do begin
            if Mode in [fgmmTriangles, fgmmFlatTriangles] then
               IncreaseCoherency(VertexIndices, 12);
         end;
      end;
   end;

   if mooSortByMaterials in options then
      aMeshObject.FaceGroups.SortByMaterial;
end;

end.
