{: MeshUtils.<p>

   General utilities for mesh manipulations.<p>

	<b>Historique : </b><font size=-1><ul>
      <li>13/01/03 - EG - Added InvertTrianglesWinding, BuildNonOrientedEdgesList,
                          SubdivideTriangles 
      <li>10/03/02 - EG - Added WeldVertices, RemapTrianglesIndices and IncreaseCoherency
      <li>04/11/01 - EG - Optimized RemapAndCleanupReferences and BuildNormals
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
   list in which duplicates coordinates in the original vertices list are used
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
{: Remaps a list of triangles vertex indices and remove degenerate triangles.<p>
   The indicesMap provides newVertexIndex:=indicesMap[oldVertexIndex] }
procedure RemapTrianglesIndices(indices, indicesMap : TIntegerList);

{: Inverts the triangles winding (vertex order). }
procedure InvertTrianglesWinding(indices : TIntegerList);

{: Builds normals for a triangles list.<p>
   Builds one normal per reference vertex (may be NullVector is reference isn't
   used), which is the averaged for normals of all adjacent triangles.<p>
   Returned list must be freed by caller. }
function BuildNormals(reference : TAffineVectorList;
                      indices : TIntegerList) : TAffineVectorList;

{: Builds a list of non-oriented (non duplicated) edges list.<p>
   Each edge is represented by the two integers of its vertices,
   sorted in ascending order.<br>
   If not nil, triangleEdges is filled with the 3 indices of the 3 edges
   of the triangle, the edges ordering respecting the original triangle
   orientation.<p>
   *NOT* optimized, lacks a hash in ProcessEdge }
function BuildNonOrientedEdgesList(triangleIndices : TIntegerList;
                                   triangleEdges : TIntegerList) : TIntegerList;

{: Welds all vertices separated by a distance inferior to weldRadius.<p>
   Any two vertices whose distance is inferior to weldRadius will be merged
   (ie. one of them will be removed, and the other replaced by the barycenter).<p>
   The indicesMap is constructed to allow remapping of indices lists with the
   simple rule: newVertexIndex:=indicesMap[oldVertexIndex].<p>
   The logic is protected from chain welding, and only vertices that were
   initially closer than weldRadius will be welded in the same resulting vertex.<p>
   This procedure can be used for mesh simplification, but preferably at design-time
   for it is not optimized for speed, nor for quality. This is more a "fixing"
   utility for meshes exported from high-polycount CAD tools (remove duplicate
   vertices, quantification errors, etc.) }
procedure WeldVertices(vertices : TAffineVectorList;
                       indicesMap : TIntegerList;
                       weldRadius : Single);

{: Attempts to create as few as possible triangle strips to cover the mesh.<p>
   The indices parameters define a set of triangles as a set of indices to
   vertices in a vertex pool, free of duplicate vertices (or resulting
   stripification will be of lower quality).<br>
   The function returns a list of TIntegerList, each of these lists hosting
   a triangle strip, returned objects must be freed by caller.<br>
   If agglomerateLoneTriangles is True, the first of the lists actually contains
   the agglomerated list of the triangles that couldn't be stripified. }
function StripifyMesh(indices : TIntegerList; maxVertexIndex : Integer;
                      agglomerateLoneTriangles : Boolean = False) : TPersistentObjectList;
{: Increases indices coherency wrt vertex caches.<p>
   The indices parameters is understood as vertex indices of a triangles set,
   the triangles are reordered to maximize coherency (vertex reuse) over the
   cacheSize latest indices. This allows higher rendering performance from
   hardware renderers that implement vertex cache (nVidia GeForce family f.i.),
   allowing reuse of T&amp;L performance (similar to stripification without
   the normals issues of strips).<p>
   This procedure performs a coherency optimization via a greedy hill-climber
   algorithm (ie. not optimal but fast). }
procedure IncreaseCoherency(indices : TIntegerList; cacheSize : Integer);

{: Subdivides mesh triangles.<p>
   Splits along edges, each triangle becomes four. The smoothFactor can be
   used to control subdivision smoothing, zero means no smoothing (tesselation
   only), while 1 means "sphere" subdivision (a low res sphere will be subdivided
   in a higher-res sphere), values outside of the [0..1] range are for, er,
   artistic purposes.<p>
   The procedure is not intended for real-time use. }
procedure SubdivideTriangles(smoothFactor : Single;
                             vertices, normals : TAffineVectorList;
                             triangleIndices : TIntegerList;
                             texCoords : TAffineVectorList = nil);

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
   refList : PAffineVectorArray;
   indicesList : PIntegerArray;
begin
   Assert(reference.Count=indices.Count);
   SetLength(tag, reference.Count);
   indicesList:=indices.List;
   // 1st step, tag all used references
   for i:=0 to indices.Count-1 do
      tag[indicesList[i]]:=1;
   // 2nd step, build remap indices and cleanup references
   n:=0;
   refList:=reference.List;
   for i:=0 to High(tag) do begin
      if tag[i]<>0 then begin
         tag[i]:=n;
         if n<>i then
            refList[n]:=refList[i];
         Inc(n);
      end;
   end;
   reference.Count:=n;
   // 3rd step, remap indices
   for i:=0 to indices.Count-1 do
      indicesList[i]:=tag[indicesList[i]];
end;

// RemapTrianglesIndices
//
procedure RemapTrianglesIndices(indices, indicesMap : TIntegerList);
var
   i, k, a, b, c, n : Integer;
begin
   Assert((indices.Count mod 3)=0); // must be a multiple of 3
   n:=indices.Count;
   i:=0;
   k:=0;
   while i<n do begin
      a:=indicesMap[indices[i]];
      b:=indicesMap[indices[i+1]];
      c:=indicesMap[indices[i+2]];
      if (a<>b) and (b<>c) and (a<>c) then begin
         indices[k]:=a;
         indices[k+1]:=b;
         indices[k+2]:=c;
         Inc(k, 3);
      end;
      Inc(i, 3);
   end;
   indices.Count:=k;
end;

// InvertTrianglesWinding
//
procedure InvertTrianglesWinding(indices : TIntegerList);
var
   i : Integer;
begin
   Assert((indices.Count mod 3)=0);
   i:=indices.Count-3;
   while i>=0 do begin
      indices.Exchange(i, i+2);
      Dec(i, 3);
   end;
end;

// BuildNormals
//
function BuildNormals(reference : TAffineVectorList;
                      indices : TIntegerList) : TAffineVectorList;
var
   i, n, k : Integer;
   normalsCount : TIntegerList;
   v : TAffineVector;
   refList : PAffineVectorArray;
   indicesList : PIntegerArray;
begin
   Result:=TAffineVectorList.Create;
   normalsCount:=TIntegerList.Create;
   try
      Result.Count:=reference.Count;
      normalsCount.Count:=reference.Count;
      refList:=reference.List;
      indicesList:=indices.List;
      // 1st step, calculate triangle normals and sum
      i:=0; while i<indices.Count do begin
         v:=CalcPlaneNormal(refList[indicesList[i]],
                            refList[indicesList[i+1]],
                            refList[indicesList[i+2]]);
         for n:=i to i+2 do begin
            k:=indicesList[n];
            Result.TranslateItem(k, v);
            Inc(normalsCount.List[k]);
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

{: Builds a list of non-oriented (non duplicated) edges list.<p>
   Each edge is represented by the two integers of its vertices,
   sorted in ascending order.<br>
   If not nil, triangleEdges is filled with the 3 indices of the 3 edges
   of the triangle, the edges ordering respecting the original triangle
   orientation.<p>
   *NOT* optimized, lacks a hash in ProcessEdge }
// BuildNonOrientedEdgesList
//
function BuildNonOrientedEdgesList(triangleIndices : TIntegerList;
                                   triangleEdges : TIntegerList) : TIntegerList;

   function ProcessEdge(a, b : Integer; edges : TIntegerList) : Integer;
   var
      n : Integer;
   begin
      if a<=b then begin
         n:=0;
         while n<edges.Count do begin
            if (edges[n]=a) and (edges[n+1]=b) then begin
               Result:=n;
               Exit;
            end;
            Inc(n, 2);
         end;
         Result:=edges.Count;
         edges.Add(a, b);
      end else Result:=ProcessEdge(b, a, edges);
   end;

var
   i, j : Integer;
begin
   Result:=TIntegerList.Create;
   Result.Capacity:=1024;
   Result.GrowthDelta:=1024;
   if Assigned(triangleEdges) then
      triangleEdges.Count:=triangleIndices.Count;
   // collect all edges
   i:=0;
   if Assigned(triangleEdges) then begin
      while i<triangleIndices.Count do begin
         triangleEdges[i  ]:=ProcessEdge(triangleIndices[i  ], triangleIndices[i+1], Result);
         triangleEdges[i+1]:=ProcessEdge(triangleIndices[i+1], triangleIndices[i+2], Result);
         triangleEdges[i+2]:=ProcessEdge(triangleIndices[i+2], triangleIndices[i  ], Result);
         Inc(i, 3);
      end;
   end else begin
      while i<triangleIndices.Count do begin
         ProcessEdge(triangleIndices[i  ], triangleIndices[i+1], Result);
         ProcessEdge(triangleIndices[i+1], triangleIndices[i+2], Result);
         ProcessEdge(triangleIndices[i+2], triangleIndices[i  ], Result);
         Inc(i, 3);
      end;
   end;
   // Cleanup
   i:=0;
   j:=0;
   while i<=Result.Count-2 do begin
      if Result[i]<>Result[i+1] then begin
         Result[j]:=Result[i];
         Result[j+1]:=Result[i+1];
         Inc(j, 2);
      end;
      Inc(i, 2);
   end;
   // Build triangles edges index
      if Assigned(triangleEdges) then
end;

// IncreaseCoherency
//
procedure IncreaseCoherency(indices : TIntegerList; cacheSize : Integer);
var
   i, n, maxVertex, bestCandidate, bestScore, candidateIdx, lastCandidate : Integer;
   trisOfVertex : array of TIntegerList;
   candidates : TIntegerList;
   indicesList : PIntegerArray;
begin
   // Alloc lookup structure
   maxVertex:=indices.MaxInteger;
   SetLength(trisOfVertex, maxVertex+1);
   for i:=0 to High(trisOfVertex) do
      trisOfVertex[i]:=TIntegerList.Create;
   candidates:=TIntegerList.Create;
   indicesList:=PIntegerArray(indices.List);
   // Fillup lookup structure
   i:=0;
   while i<indices.Count do begin
      trisOfVertex[indicesList[i+0]].Add(i);
      trisOfVertex[indicesList[i+1]].Add(i);
      trisOfVertex[indicesList[i+2]].Add(i);
      Inc(i, 3);
   end;
   // Optimize
   i:=0;
   while i<indices.Count do begin
      n:=i-cacheSize;
      if n<0 then n:=0;
      candidates.Count:=0;
      while n<i do begin
         candidates.Add(trisOfVertex[indicesList[n]]);
         Inc(n);
      end;
      bestCandidate:=-1;
      if candidates.Count>0 then begin
         candidateIdx:=0;
         bestScore:=0;
         candidates.Sort;
         lastCandidate:=candidates.List[0];
         for n:=1 to candidates.Count-1 do begin
            if candidates.List[n]<>lastCandidate then begin
               if n-candidateIdx>bestScore then begin
                  bestScore:=n-candidateIdx;
                  bestCandidate:=lastCandidate;
               end;
               lastCandidate:=candidates.List[n];
               candidateIdx:=n;
            end;
         end;
         if candidates.Count-candidateIdx>bestScore then
            bestCandidate:=lastCandidate;
      end;
      if bestCandidate>=0 then begin
         trisOfVertex[indicesList[i+0]].Remove(i);
         trisOfVertex[indicesList[i+1]].Remove(i);
         trisOfVertex[indicesList[i+2]].Remove(i);
         trisOfVertex[indicesList[bestCandidate+0]].Remove(bestCandidate);
         trisOfVertex[indicesList[bestCandidate+1]].Remove(bestCandidate);
         trisOfVertex[indicesList[bestCandidate+2]].Remove(bestCandidate);
         trisOfVertex[indicesList[i+0]].Add(bestCandidate);
         trisOfVertex[indicesList[i+1]].Add(bestCandidate);
         trisOfVertex[indicesList[i+2]].Add(bestCandidate);
         indices.Exchange(bestCandidate+0, i+0);
         indices.Exchange(bestCandidate+1, i+1);
         indices.Exchange(bestCandidate+2, i+2);
      end else begin
         trisOfVertex[indicesList[i+0]].Remove(i);
         trisOfVertex[indicesList[i+1]].Remove(i);
         trisOfVertex[indicesList[i+2]].Remove(i);
      end;
      Inc(i, 3);
   end;
   // Release lookup structure
   candidates.Free;
   for i:=0 to High(trisOfVertex) do
      trisOfVertex[i].Free;
end;

// WeldVertices
//
procedure WeldVertices(vertices : TAffineVectorList;
                       indicesMap : TIntegerList;
                       weldRadius : Single);
var
   i, j, n, k : Integer;
   pivot : PAffineVector;
   sum : TAffineVector;
   wr2 : Single;
   mark : packed array of ByteBool;
begin
   indicesMap.Count:=vertices.Count;
   SetLength(mark, vertices.Count);
   wr2:=Sqr(weldRadius);
   // mark duplicates, compute barycenters and indicesMap
   i:=0;
   k:=0;
   while i<vertices.Count do begin
      if not mark[i] then begin
         pivot:=@vertices.List[i];
         indicesMap[i]:=k;
         n:=0;
         j:=vertices.Count-1;
         while j>i do begin
            if not mark[j] then begin
               if VectorDistance2(pivot^, vertices.List[j])<=wr2 then begin
                  if n=0 then begin
                     sum:=VectorAdd(pivot^, vertices.List[j]);
                     n:=2;
                  end else begin
                     AddVector(sum, vertices.List[j]);
                     Inc(n);
                  end;
                  indicesMap[j]:=k;
                  mark[j]:=True;
               end;
            end;
            Dec(j);
         end;
         if n>0 then
            vertices.List[i]:=VectorScale(sum, 1/n);
         Inc(k);
      end;
      Inc(i);
   end;
   // pack vertices list
   k:=0;
   for i:=0 to vertices.Count-1 do begin
      if not mark[i] then begin
         vertices.List[k]:=vertices.List[i];
         Inc(k);
      end;
   end;
   vertices.Count:=k;
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

// SubdivideTriangles
//
procedure SubdivideTriangles(smoothFactor : Single;
                             vertices, normals : TAffineVectorList;
                             triangleIndices : TIntegerList;
                             texCoords : TAffineVectorList = nil);
var
   i, a, b, c, nv : Integer;
   edges : TIntegerList;
   triangleEdges : TIntegerList;
   p, n : TAffineVector;
   f : Single;
begin
   // build edges list
   triangleEdges:=TIntegerList.Create;
   try
      edges:=BuildNonOrientedEdgesList(triangleIndices, triangleEdges);
      try
         nv:=vertices.Count;
         // split all edges, add corresponding vertex & normal
         i:=0;
         while i<edges.Count do begin
            a:=edges[i];
            b:=edges[i+1];
            p:=VectorLerp(vertices[a], vertices[b], 0.5);
            n:=VectorNormalize(VectorLerp(normals[a], normals[b], 0.5));
            if smoothFactor<>0 then begin
               f:=0.25*smoothFactor*VectorDistance(vertices[a], vertices[b])
                  *(1-VectorDotProduct(normals[a], normals[b]));
               if VectorDotProduct(normals[a], VectorSubtract(vertices[b], vertices[a]))
                  +VectorDotProduct(normals[b], VectorSubtract(vertices[a], vertices[b]))>0 then
                  f:=-f;
               CombineVector(p, n, f);
            end;
            if Assigned(texCoords) then
               texCoords.Add(VectorLerp(texCoords[a], texCoords[b], 0.5));
            vertices.Add(p);
            normals.Add(n);
            Inc(i, 2);
         end;
         // spawn new triangles
         i:=triangleIndices.Count-3;
         while i>=0 do begin
            a:=nv+triangleEdges[i+0] div 2;
            b:=nv+triangleEdges[i+1] div 2;
            c:=nv+triangleEdges[i+2] div 2;
            triangleIndices.Add(triangleIndices[i+0], a, c);
            triangleIndices.Add(a, triangleIndices[i+1], b);
            triangleIndices.Add(b, triangleIndices[i+2], c);
            triangleIndices[i+0]:=a;
            triangleIndices[i+1]:=b;
            triangleIndices[i+2]:=c;
            Dec(i, 3);
         end;
      finally
         edges.Free;
      end;
   finally
      triangleEdges.Free;
   end;
end;

end.
