// GLMeshOptimizer
{: Mesh optimization for GLScene.<p>

	<b>History : </b><font size=-1><ul>
      <li>03/06/03 - EG - Creation
	</ul></font>
}
unit GLMeshOptimizer;

interface

uses Geometry, GLVectorFileObjects, MeshUtils;

type

   // TMeshOptimizerOptions
   //
   TMeshOptimizerOption = (mooStandardize, mooVertexCache, mooSortByMaterials);
   TMeshOptimizerOptions = set of TMeshOptimizerOption;

var
   vDefaultMeshOptimizerOptions : TMeshOptimizerOptions =
      [mooStandardize, mooVertexCache, mooSortByMaterials];

procedure OptimizeMesh(aList : TMeshObjectList; options : TMeshOptimizerOptions); overload;
procedure OptimizeMesh(aList : TMeshObjectList); overload;
procedure OptimizeMesh(aMeshObject : TMeshObject; options : TMeshOptimizerOptions); overload;
procedure OptimizeMesh(aMeshObject : TMeshObject); overload;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation

uses PersistentClasses;
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

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
   i : Integer;
begin
   for i:=0 to aList.Count-1 do
      OptimizeMesh(aList[i], options);
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
begin
   if mooStandardize in options then
      Assert(False, 'not supported.. yet.');

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
