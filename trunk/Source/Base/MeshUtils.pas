{: MeshUtils.<p>

   General utilities for mesh manipulations.<p>

	<b>Historique : </b><font size=-1><ul>
      <li>02/11/01 - EG - BuildVectorCountOptimizedIndices three times faster,
                          StripifyMesh slightly faster
	   <li>18/08/01 - EG - Creation
	</ul></font>
}
unit MeshUtils;

interface

uses PersistentClasses, VectorLists, Geometry;

{: Builds a vector-count optimized indices list.<p>
   The returned list (to be freed by caller) contains an "optimized" indices
   list in which duplicates corodinates in the original vertices list are used
   only once (the first available duplicate in the list is used).<br>
   The vertices list is left untouched, to remap/cleanup, you may use the
   RemapAndCleanupReferences function. }
function BuildVectorCountOptimizedIndices(const vertices : TAffineVectorList) : TIntegerList;

{: Alters a reference/indice pair and removes unused reference values.<p>
   This functions scans the reference list and removes all values that aren't
   referred in the indices list, and the indices list is remapped so as to remain
   coherent. }
procedure RemapAndCleanupReferences(reference : TAffineVectorList;
                                    indices : TIntegerList);

{: Builds normals for a triangles list.<p>
   Builds one normal per reference vertex (may be NullVector is reference isn't
   used), which is the averaged for normals of all adjacent triangles.<p>
   Returned list must be freed by caller. }
function BuildNormals(reference : TAffineVectorList;
                      indices : TIntegerList) : TAffineVectorList;

{: Attempts to create as few as possible triangle strips to cobver the mesh.<p>
   The indices parameters define a set of triangles as a set of indices to
   vertices in a vertex pool, free of duplicate vertices (or resulting
   stripification will be of lower quality).<br>
   The function returns a list of TIntegerList, each of these lists hosting
   a triangle strip, returned objects must be freed by caller.<br>
   If agglomerateLoneTriangles is True, the first of the lists actually contains
   an agglomerated lists of the triangles that couldn't be stripified. }
function StripifyMesh(indices : TIntegerList; maxVertexIndex : Integer;
                      agglomerateLoneTriangles : Boolean = False) : TPersistentObjectList;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils;

// BuildVectorCountOptimizedIndices
//
function BuildVectorCountOptimizedIndices(const vertices : TAffineVectorList) : TIntegerList;
var
   i, j : Integer;
   found : Boolean;
   hashSize : Integer;
   hashTable : array of TIntegerlist;
   list : TIntegerList;
const
   cVerticesPerHashKey = 48;

   function HashKey(const v : TAffineVector) : Integer;
   begin
      Result:=(( Integer(PIntegerArray(@v)[0])
                +Integer(PIntegerArray(@v)[1])
                +Integer(PIntegerArray(@v)[2])) shr 8) and hashSize;
   end;

begin
   Result:=TIntegerList.Create;
   Result.Capacity:=vertices.Count;

   if vertices.Count<128 then begin
      // not a lot of stuff, we can go brute force
      // (this method is sluggish when there are thousandth of vertices)
      for i:=0 to vertices.Count-1 do
          Result.Add(vertices.IndexOf(vertices[i]));
   end else begin
      // That may be a big one, prep up the artillery, just in case...
      // This method is very fast, at the price of memory requirement its
      // complexity is only O(n) (it's a kind of bucket-sort hellspawn)

      // Initialize data structures for a hash table
      // (each vertex will only be compared to vertices of similar hash value)
      hashSize:=Trunc(Power(2, Trunc(log2(vertices.Count/cVerticesPerHashKey))))-1;
      if hashSize>65535 then hashSize:=65535;
      SetLength(hashTable, hashSize+1);
      // allocate and fill our hashtable (will store "reference" vertex indices)
      for i:=0 to hashSize do begin
         hashTable[i]:=TIntegerList.Create;
         hashTable[i].GrowthDelta:=cVerticesPerHashKey div 2;
      end;
      // here we go for all vertices
      for i:=0 to vertices.Count-1 do begin
         list:=hashTable[HashKey(vertices.List[i])];
         found:=False;
         // Check each vertex against its hashkey siblings
         for j:=0 to list.Count-1 do begin
            if VectorEquals(vertices.List[list.List[j]], vertices.List[i]) then begin
               // vertex known, just store its index
               Result.Add(list.List[j]);
               found:=True;
               Break;
            end;
         end;
         if not found then begin
            // vertex unknown, store index and add to the hashTable's list
            list.Add(i);
            Result.Add(i);
         end;
      end;
      // free hash data
      for i:=0 to hashSize do
         hashTable[i].Free;
      SetLength(hashTable, 0);
   end;
end;

// RemapAndCleanupReferences
//
procedure RemapAndCleanupReferences(reference : TAffineVectorList;
                                    indices : TIntegerList);
var
   i, n : Integer;
   tag : array of Integer;
begin
   SetLength(tag, reference.Count);
   // 1st step, tag all used references
   for i:=0 to indices.Count-1 do
      tag[indices[i]]:=1;
   // 2nd step, build remap indices and cleanup references
   n:=0;
   for i:=0 to Length(tag)-1 do
      if tag[i]<>0 then begin
         tag[i]:=n;
         if n<>i then
            reference[n]:=reference[i];
         Inc(n);
      end;
   reference.Count:=n;
   // 3rd step, remap indices
   for i:=0 to indices.Count-1 do
      indices[i]:=tag[indices[i]];
end;

// BuildNormals
//
function BuildNormals(reference : TAffineVectorList;
                      indices : TIntegerList) : TAffineVectorList;
var
   i, n : Integer;
   normalsCount : TIntegerList;
   v : TAffineVector;
begin
   Result:=TAffineVectorList.Create;
   normalsCount:=TIntegerList.Create;
   try
      Result.Count:=reference.Count;
      normalsCount.Count:=reference.Count;
      // 1st step, calculate triangle normals and sum
      i:=0; while i<indices.Count do begin
         v:=CalcPlaneNormal(reference[indices[i]],
                            reference[indices[i+1]],
                            reference[indices[i+2]]);
         for n:=i to i+2 do begin
            Result.TranslateItem(indices[n], v);
            normalsCount[indices[n]]:=normalsCount[indices[n]]+1;
         end;
         Inc(i, 3);
      end;
      // 2nd step, average normals
      for i:=0 to reference.Count-1 do if normalsCount[i]>1 then
         ScaleVector(Result.List[i], 1/normalsCount[i]);
   finally
      normalsCount.Free;
   end;
end;

// StripifyMesh
//
function StripifyMesh(indices : TIntegerList; maxVertexIndex : Integer;
                      agglomerateLoneTriangles : Boolean = False) : TPersistentObjectList;
var
   accountedTriangles : array of ByteBool;
   vertexTris : array of TIntegerList;
   indicesList : PIntegerArray;
   indicesCount : Integer;
   currentStrip : TIntegerList;
   nextTriangle, nextVertex : Integer;

   function FindTriangleWithEdge(vertA, vertB : Integer) : Boolean;
   var
      i, n : Integer;
      p : PIntegerArray;
      list : TIntegerList;
   begin
      Result:=False;
      list:=vertexTris[vertA];
      for n:=0 to list.Count-1 do begin
         i:=list.List[n];
         if not (accountedTriangles[i]) then begin
            p:=@indicesList[i];
            if (p[0]=vertA) and (p[1]=vertB) then begin
               Result:=True;
               nextVertex:=p[2];
               nextTriangle:=i;
               Break;
            end else if (p[1]=vertA) and (p[2]=vertB) then begin
               Result:=True;
               nextVertex:=p[0];
               nextTriangle:=i;
               Break;
            end else if (p[2]=vertA) and (p[0]=vertB) then begin
               Result:=True;
               nextVertex:=p[1];
               nextTriangle:=i;
               Break;
            end;
         end;
      end;

   end;

   procedure BuildStrip(vertA, vertB : Integer);
   var
      vertC : Integer;
   begin
      currentStrip.Add(vertA, vertB);
      repeat
         vertC:=nextVertex;
         currentStrip.Add(vertC);
         accountedTriangles[nextTriangle]:=True;
         if not FindTriangleWithEdge(vertB, vertC) then Break;
         currentStrip.Add(nextVertex);
         accountedTriangles[nextTriangle]:=True;
         vertB:=nextVertex;
         vertA:=vertC;
      until not FindTriangleWithEdge(vertB, vertA);
   end;

var
   i, n, triangle : Integer;
   loneTriangles : TIntegerList;
begin
   Assert((indices.Count mod 3)=0, 'indices count is not a multiple of 3!');
   Result:=TPersistentObjectList.Create;
   // direct access and cache vars
   indicesList:=indices.List;
   indicesCount:=indices.Count;
   // Build adjacency lookup table (vertex based, not triangle based)
   SetLength(vertexTris, maxVertexIndex+1);
   for i:=0 to High(vertexTris) do
      vertexTris[i]:=TIntegerList.Create;
   n:=0;
   triangle:=0;
   for i:=0 to indicesCount-1 do begin
      vertexTris[indicesList[i]].Add(triangle);
      if n=2 then begin
         n:=0;
         Inc(triangle, 3);
      end else Inc(n);
   end;
   // Now, we use a greedy algo to build triangle strips
   SetLength(accountedTriangles, indicesCount); // yeah, waste of memory
   if agglomerateLoneTriangles then begin
      loneTriangles:=TIntegerList.Create;
      Result.Add(loneTriangles);
   end else loneTriangles:=nil;
   i:=0; while i<indicesCount do begin
      if not accountedTriangles[i] then begin
         accountedTriangles[i]:=True;
         if FindTriangleWithEdge(indicesList[i+1], indicesList[i]) then begin
            currentStrip:=TIntegerList.Create;
            currentStrip.Add(indicesList[i+2]);
            BuildStrip(indicesList[i], indicesList[i+1]);
         end else if FindTriangleWithEdge(indicesList[i+2], indicesList[i+1]) then begin
            currentStrip:=TIntegerList.Create;
            currentStrip.Add(indicesList[i]);
            BuildStrip(indicesList[i+1], indicesList[i+2]);
         end else if FindTriangleWithEdge(indicesList[i], indicesList[i+2]) then begin
            currentStrip:=TIntegerList.Create;
            currentStrip.Add(indicesList[i+1]);
            BuildStrip(indicesList[i+2], indicesList[i]);
         end else begin
            if agglomerateLoneTriangles then
               currentStrip:=loneTriangles
            else currentStrip:=TIntegerList.Create;
            currentStrip.Add(indicesList[i], indicesList[i+1], indicesList[i+2]);
         end;
         if currentStrip<>loneTriangles then
            Result.Add(currentStrip);
      end;
      Inc(i, 3);
   end;
   // cleanup
   for i:=0 to High(vertexTris) do
      vertexTris[i].Free;
end;

end.
