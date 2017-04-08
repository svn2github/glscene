unit AStarBlitzUnit;

// Degenerated from:
// A* Pathfinder by Patrick Lester. Used by permission.
// ==================================================================
// Last updated 8/4/17
// http://www.policyalmanac.org/games/aStarTutorial.htm
// A* Pathfinding for Beginners
// This version of the aStar library has been modified to handle
// pathfinding around other units.

// This is ALL the Sundry code split from the Pathfinder

interface

uses
  Winapi.OpenGL,
  System.Classes,
  System.Math,
  Vcl.Forms, // application.ProcessMessages;

  OpenGLAdapter,
  GLScene,
  GLObjects,
  GLTexture, // TGLCubeEX
  GLVectorGeometry, // math,
  GLCrossPlatform, // PrecisionTimer
  GLRenderContextInfo,

  AStarGlobals;

type //
  TGLCubeEX = class(TGLCube)
  private

  public
    procedure BuildList(var rci: TGLRenderContextInfo); override;
  end;

Function BlitzReadPathX(pathfinderID, pathLocation: Integer): Integer;
Function BlitzReadPathY(pathfinderID, pathLocation: Integer): Integer;
Procedure BlitzReadPath(pathfinderID {, currentX, currentY, pixelsPerFrame }: Integer);
Procedure CheckPathStepAdvance(pathfinderID: Integer { unit.unit } );
Procedure ClaimNodes(pathfinderID: Integer);
Procedure ClearNodes(pathfinderID: Integer);
Function DetectCollision(pathfinderID: Integer): Integer;
Function ReachedNextPathNode(pathfinderID: Integer { unit.unit } ): Boolean;
Function UnitOnOtherUnitPath(pathfinderID, otherUnit: Integer): Boolean;
Function CheckRedirect(pathfinderID, x, y: Integer): Integer;
Function NodeOccupied(pathfinderID, x, y: Integer): Boolean;
Procedure CreateFootPrints(pathfinderID: Integer);
Procedure IdentifyIslands;
Procedure ChooseGroupLocations;
Function GetDistance(x1, y1, x2, y2: Double): Double;

Procedure UpdateGameClock;
Procedure MoveUnits;
Procedure UpdatePath(pathfinderID: Integer);
Procedure MoveUnit(pathfinderID: Integer);
Function MoveTowardNode(pathfinderID: Integer;
  distanceTravelled: Double): Double;

Procedure DrawClaimedNodes;
Procedure RunUnitLoop;
procedure PaintLinePath(ID: Integer);
Procedure RenderScreen;

// =======================================================================
implementation

// =======================================================================

uses
  XOpenGL, // TGLCubeEX
  ATerrainFrm,
  AStarBlitzCode,
  AStarBlitzCodeH; // recalls into the Search

{ This is basically the same code as the orignal buildlist - I've just
  changed the texture coordinates to map dirrent faces to a different part
  of the same bitmap. }
procedure TGLCubeEX.BuildList(var rci: TGLRenderContextInfo);
var
  hw, hh, hd, nd: single;
  ds: single;
begin
  if NormalDirection = ndInside then
    nd := -1
  else
    nd := 1;
  hw := CubeWidth * 0.5;
  hh := CubeHeight * 0.5;
  hd := CubeDepth * 0.5;
  ds := 1 / 6;
  glBegin(GL_QUADS);
  if cpFront in Parts then
  begin
    glNormal3f(0, 0, nd);
    xgl.TexCoord2f(ds, 1);
    glVertex3f(hw, hh, hd);
    xgl.TexCoord2f(0, 1);
    glVertex3f(-hw, hh, hd);
    xgl.TexCoord2f(0, 0);
    glVertex3f(-hw, -hh, hd);
    xgl.TexCoord2f(ds, 0);
    glVertex3f(hw, -hh, hd);
  end;
  if cpLeft in Parts then
  begin
    glNormal3f(-nd, 0, 0);
    xgl.TexCoord2f(2 * ds, 1);
    glVertex3f(-hw, hh, hd);
    xgl.TexCoord2f(ds, 1);
    glVertex3f(-hw, hh, -hd);
    xgl.TexCoord2f(ds, 0);
    glVertex3f(-hw, -hh, -hd);
    xgl.TexCoord2f(2 * ds, 0);
    glVertex3f(-hw, -hh, hd);
  end;
  if cpBack in Parts then
  begin
    glNormal3f(0, 0, -nd);
    xgl.TexCoord2f(2 * ds, 1);
    glVertex3f(hw, hh, -hd);
    xgl.TexCoord2f(2 * ds, 0);
    glVertex3f(hw, -hh, -hd);
    xgl.TexCoord2f(3 * ds, 0);
    glVertex3f(-hw, -hh, -hd);
    xgl.TexCoord2f(3 * ds, 1);
    glVertex3f(-hw, hh, -hd);
  end;

  if cpRight in Parts then
  begin
    glNormal3f(nd, 0, 0);
    xgl.TexCoord2f(3 * ds, 1);
    glVertex3f(hw, hh, hd);
    xgl.TexCoord2f(3 * ds, 0);
    glVertex3f(hw, -hh, hd);
    xgl.TexCoord2f(4 * ds, 0);
    glVertex3f(hw, -hh, -hd);
    xgl.TexCoord2f(4 * ds, 1);
    glVertex3f(hw, hh, -hd);
  end;
  if cpTop in Parts then
  begin
    glNormal3f(0, nd, 0);
    xgl.TexCoord2f(5 * ds, 1);
    glVertex3f(-hw, hh, -hd);
    xgl.TexCoord2f(4 * ds, 1);
    glVertex3f(-hw, hh, hd);
    xgl.TexCoord2f(4 * ds, 0);
    glVertex3f(hw, hh, hd);
    xgl.TexCoord2f(5 * ds, 0);
    glVertex3f(hw, hh, -hd);
  end;
  if cpBottom in Parts then
  begin
    glNormal3f(0, -nd, 0);
    xgl.TexCoord2f(1, 1);
    glVertex3f(-hw, -hh, -hd);
    xgl.TexCoord2f(5 * ds, 1);
    glVertex3f(hw, -hh, -hd);
    xgl.TexCoord2f(5 * ds, 0);
    glVertex3f(hw, -hh, hd);
    xgl.TexCoord2f(1, 0);
    glVertex3f(-hw, -hh, hd);
  end;
  glEnd;
end;

// ==========================================================
// READ PATH DATA: These functions read the path data and convert
// it to screen pixel coordinates.
// The following two functions read the raw path data from the pathBank.
// You can call these functions directly and skip the readPath function
// above if you want. Make sure you know what your current pathLocation is.
// ---------------------------------------------------------------------------
// Name: ReadPathX
// Desc: Reads the x coordinate of the next path step
// ---------------------------------------------------------------------------
Function BlitzReadPathX(pathfinderID, pathLocation: Integer): Integer;
var
  x: Integer;
Begin
  x := 0;
  if (pathLocation <= UnitRecordArray[pathfinderID].pathLength) then
  begin // Read coordinate from bank
    x := // pathBank[pathfinderID,((pathLocation*2)-2)];
      UnitpathBank[UnitRecordArray[pathfinderID].CurrentpathBank][pathfinderID]
      [((pathLocation * 2) - 2)];
    // Adjust the coordinates so they align with the center
    // of the path square (optional). This assumes that you are using
    // sprites that are centered -- i.e., with the midHandle command.
    // Otherwise you will want to adjust this.
    // and make everything Doubles !!!
    x := (ProjectRecord.tileSize * x); // + (0.5*tileSize);
  end;
  Result := x;
end;

// ---------------------------------------------------------------------------
// Name: ReadPathY
// Desc: Reads the y coordinate of the next path step
// ---------------------------------------------------------------------------
Function BlitzReadPathY(pathfinderID, pathLocation: Integer): Integer;
var
  y: Integer;
Begin
  y := 0;
  if (pathLocation <= UnitRecordArray[pathfinderID].pathLength) then
  begin
    // Read coordinate from bank
    y := // pathBank[pathfinderID] [pathLocation*2-1];
      UnitpathBank[UnitRecordArray[pathfinderID].CurrentpathBank][pathfinderID]
      [((pathLocation * 2) - 1)];

    // Adjust the coordinates so they align with the center
    // of the path square (optional). This assumes that you are using
    // sprites that are centered -- i.e., with the midHandle command.
    // Otherwise you will want to adjust this.
    y := ProjectRecord.tileSize * y; // + .5*tileSize;
  end;
  Result := y;
end;

Procedure BlitzReadPath(pathfinderID { , currentX, currentY,
    pixelsPerFrame } : Integer);
// Note on PixelsPerFrame: The need for this parameter probably isn't
// that obvious, so a little explanation is in order. This
// parameter is used to determine if the pathfinder has gotten close
// enough to the center of a given path square to warrant looking up
// the next step on the path.
// This is needed because the speed of certain sprites can
// make reaching the exact center of a path square impossible.
// In Demo #2, the chaser has a velocity of 3 pixels per frame. Our
// tile size is 50 pixels, so the center of a tile will be at location
// 25, 75, 125, etc. Some of these are not evenly divisible by 3, so
// our pathfinder has to know how close is close enough to the center.
// It calculates this by seeing if the pathfinder is less than
// pixelsPerFrame # of pixels from the center of the square.
//
// This could conceivably cause problems if you have a *really* fast
// sprite and/or really small tiles, in which case you may need to
// adjust the formula a bit. But this should almost never be a problem
// for games with standard sized tiles and normal speeds. Our smiley
// in Demo #4 moves at a pretty fast clip and it isn't even close
// to being a problem.
{ Function ReadPath(unit.unit)
  unit\xPath = ReadPathX(unit.unit,unit\pathLocation)
  unit\yPath = ReadPathY(unit.unit,unit\pathLocation)
  End Function }
var
  ID: Integer;
Begin
  ID := pathfinderID; // redundant, but makes the following easier to read
  // Read the path data. the Blitz version is Different...
  UnitRecordArray[ID].xPath := BlitzReadPathX(ID,
    UnitRecordArray[ID].pathLocation);
  UnitRecordArray[ID].yPath := BlitzReadPathY(ID,
    UnitRecordArray[ID].pathLocation);
  {
    //If a path has been found for the pathfinder	...
    if (pathStatus[ID] = found) then
    begin
    //If path finder is just starting a new path or has reached the
    //center of the current path square (and the end of the path
    //hasn't been reached), look up the next path square.
    if (pathLocation[ID] < pathLength[ID]) then
    begin
    //if just starting or if close enough to center of square
    if (   (pathLocation[ID] = 0)
    or(  (abs(currentX - xPath[ID])< pixelsPerFrame)
    and (abs(currentY - yPath[ID]) < pixelsPerFrame)) )
    then	pathLocation[ID] := (pathLocation[ID] + 1);
    end;
    //Read the path data.
    xPath[ID] := BlitzReadPathX(ID,pathLocation[ID]);
    yPath[ID] := BlitzReadPathY(ID,pathLocation[ID]);
    //If the center of the last path square on the path has been
    //reached then reset.
    if (pathLocation[ID] = pathLength[ID]) then
    begin  //if close enough to center of square
    if ( (abs(currentX - xPath[ID]) < pixelsPerFrame)
    and (abs(currentY - yPath[ID]) < pixelsPerFrame))
    then pathStatus[ID] := notStarted;
    end;
    end
    //If there is no path for this pathfinder,
    //simply stay in the current location.
    else
    begin
    xPath[ID] := currentX;
    yPath[ID] := currentY;
    end; }
End;


// ==========================================================
// COLLISION/NODE CLAIMING FUNCTIONS: These functions handle node claiming
// and collision detection (which occurs when a unit tries to claim a node that
// another unit has already claimed).

// This function checks whether the unit is close enough to the next
// path node to advance to the next one
// or, if it is the last path step, to stop.
// Function
Procedure CheckPathStepAdvance(pathfinderID: Integer { unit.unit } );
begin
  // If starting a new path ...
  If UnitRecordArray[pathfinderID].pathLocation = 0 then
  begin
    If UnitRecordArray[pathfinderID].pathLength > 0 then
    begin
      UnitRecordArray[pathfinderID].pathLocation :=
        (UnitRecordArray[pathfinderID].pathLocation + 1);
      ClaimNodes(pathfinderID);
      BlitzReadPath(pathfinderID) // ;update xPath and yPath
    end
    Else If UnitRecordArray[pathfinderID].pathLength = 0 then
    begin
      BlitzReadPath(pathfinderID); // update xPath and yPath
      If ((UnitRecordArray[pathfinderID].startXLoc = UnitRecordArray
        [pathfinderID].xPath) And
        (UnitRecordArray[pathfinderID].startYLoc = UnitRecordArray[pathfinderID]
        .yPath)) then
      begin
        UnitRecordArray[pathfinderID].pathStatus := notstarted;
        ClearNodes(pathfinderID);
      end;
    end; // End If//End If
  End
  // ;If reaching the next path node.
  Else If ((UnitRecordArray[pathfinderID].startXLoc = UnitRecordArray
    [pathfinderID].xPath) And (UnitRecordArray[pathfinderID]
    .startYLoc = UnitRecordArray[pathfinderID].yPath)) then
  begin
    If UnitRecordArray[pathfinderID].pathLocation = UnitRecordArray
      [pathfinderID].pathLength then
    begin
      UnitRecordArray[pathfinderID].pathStatus := notstarted;
      ClearNodes(pathfinderID);
    end
    Else
    begin // unit\pathLocation = unit\pathLocation + 1
      UnitRecordArray[pathfinderID].pathLocation :=
        (UnitRecordArray[pathfinderID].pathLocation + 1);
      ClaimNodes(pathfinderID);
      BlitzReadPath(pathfinderID); // update xPath and yPath
    end; // End If
  End; // If
End; // Function

// This function claims nodes for a unit.
// It is called by ReadPath() ..No by CheckPathStepAdvance
// Function ClaimNodes(unit.unit)
// currentPathBank is a TEMP worker
Procedure ClaimNodes(pathfinderID: Integer);
var
  x2, y2: Integer;
Begin
  // Clear previously claimed nodes and claim the node the unit is currently occupying.
  ClearNodes(pathfinderID);
  // Check next path node for a collision.
  UnitRecordArray[pathfinderID].unitCollidingWith :=
    DetectCollision(pathfinderID);
  // If no collision is detected, claim the node and
  // figure out the distance to the node.
  If UnitRecordArray[pathfinderID].unitCollidingWith = 0 { Null } then
  begin
    // x2 = PeekShort (unit\pathBank,unit\pathLocation*4)
    x2 := // pathBank[pathfinderID] [pathLocation[pathfinderID]*4];
      UnitpathBank[UnitRecordArray[pathfinderID].CurrentpathBank][pathfinderID]
      [UnitRecordArray[pathfinderID].pathLocation * 4];
    // y2 = PeekShort (unit\pathBank,unit\pathLocation*4+2)
    y2 := // pathBank[pathfinderID] [pathLocation[pathfinderID]*4+2];
      UnitpathBank[UnitRecordArray[pathfinderID].CurrentpathBank][pathfinderID]
      [UnitRecordArray[pathfinderID].pathLocation * 4 + 2];
    claimedNode[x2, y2] := pathfinderID;
    BlitzReadPath(pathfinderID); // update xPath/yPath
    UnitRecordArray[pathfinderID].distanceToNextNode :=
      GetDistance(UnitRecordArray[ActiveUnitNumber].startXLoc,
      UnitRecordArray[ActiveUnitNumber].startYLoc,
      UnitRecordArray[pathfinderID].xPath, UnitRecordArray[pathfinderID].yPath);
  end
  // Otherwise, if a collision has been detected ...
  Else
    // If node is occupied by a unit not moving normally, repath.
    If UnitRecordArray[UnitRecordArray[pathfinderID].unitCollidingWith]
      .pathStatus = tempStopped { stopped } then
      { pathStatus[pathfinderID] :=
        FindPath(pathfinderID,unit\targetX,unit\targetY) }
      UnitRecordArray[pathfinderID].pathStatus :=
        BlitzFindPath(pathfinderID, UnitRecordArray[pathfinderID].startXLoc *
        ProjectRecord.tileSize, UnitRecordArray[pathfinderID].startYLoc *
        ProjectRecord.tileSize, UnitRecordArray[pathfinderID].targetX *
        ProjectRecord.tileSize, UnitRecordArray[pathfinderID].targetY *
        ProjectRecord.tileSize, normal)

      // If there is a pending collision between the two units, repath.
    Else If UnitOnOtherUnitPath(pathfinderID, UnitRecordArray[pathfinderID]
      .unitCollidingWith) then
      // unit\pathStatus = FindPath(unit.unit,unit\targetX,unit\targetY)
      UnitRecordArray[pathfinderID].pathStatus :=
        BlitzFindPath(pathfinderID, UnitRecordArray[pathfinderID].startXLoc *
        ProjectRecord.tileSize, UnitRecordArray[pathfinderID].startYLoc *
        ProjectRecord.tileSize, UnitRecordArray[pathfinderID].targetX *
        ProjectRecord.tileSize, UnitRecordArray[pathfinderID].targetY *
        ProjectRecord.tileSize, normal)


      // ;If the pending collision is not head-on, repathing is optional. Check
      // ;to see if repathing produces a short enough path, and if so, use it.
      // ;Otherwise, tempStop.
      { Else If UnitRecordArray[pathfinderID].gDiagonalBlockage = False
        pathLength = unit\pathLength ;save current path stats
        pathLocation = unit\pathLocation ;save current path stats
        currentPathBank = unit\pathBank ;save current path stats
        currentPathCost = RemainingPathCost(unit)
        If unit\pathBank = unit\pathBank1 ;switch the pathBank
        unit\pathBank = unit\pathBank2
        Else
        unit\pathBank = unit\pathBank1
        End If
        unit\pathStatus = FindPath(unit.unit,unit\targetX,unit\targetY) ;check the path

        //;Is resulting path nonexistent or too long? Then reset back to the
        //;original path info saved above and tempStop. Otherwise, the path
        //;just generated will be used.
        If unit\pathStatus = nonexistent Or gPathCost > currentPathCost+35
        unit\pathLength = pathLength
        unit\pathLocation = pathLocation
        unit\pathBank = currentPathBank
        unit\pathStatus = tempStopped
        End If
      }

      // If the pending collision is with a unit crossing diagonally
      // right in front of the unit, then tempStop.
      // This global variable is set by the DetectCollision() function.
    Else If gDiagonalBlockage = True then
      // unit\pathStatus = tempStopped
      UnitRecordArray[pathfinderID].pathStatus := tempStopped;
  // End If 	End If

End; // Function

// ;This function calculates the remaining cost of the current path. This
// ;is used by the ClaimNodes() function to compare the unit's current
// ;path to a possible new path to determine which is better.
Function RemainingPathCost(pathfinderID: Integer { unit.unit } ): Integer;
var
  lastX, lastY, currentX, currentY, temppathLocation, pathCost: Integer;
begin
  pathCost := 0;
  // lastX = Floor(unit\xLoc/tileSize)
  lastX := UnitRecordArray[pathfinderID].startXLoc;
  // lastY = Floor(unit\yLoc/tileSize)
  lastY := UnitRecordArray[pathfinderID].startYLoc;
  // For pathLocation = unit\pathLocation To unit\pathLength
  For temppathLocation := UnitRecordArray[pathfinderID].pathLocation -
    1 to UnitRecordArray[pathfinderID].pathLength do
  begin
    // currentX = PeekShort (unit\pathBank,pathLocation*4)
    currentX := // pathBank[pathfinderID] [pathLocation[pathfinderID]*4];
      UnitpathBank[UnitRecordArray[pathfinderID].CurrentpathBank][pathfinderID]
      [UnitRecordArray[pathfinderID].pathLocation * 4];
    // currentY = PeekShort (unit\pathBank,pathLocation*4+2)
    currentY := // pathBank[pathfinderID] [pathLocation[pathfinderID]*4+2];
      UnitpathBank[UnitRecordArray[pathfinderID].CurrentpathBank][pathfinderID]
      [UnitRecordArray[pathfinderID].pathLocation * 4 + 2];
    If ((lastX <> currentX) And (lastY <> currentY)) then
      pathCost := pathCost + 14 // cost of going to diagonal squares
    Else
      pathCost := pathCost + 10; // cost of going to non-diagonal squares
    // End If
    lastX := currentX;
    lastY := currentY;
  end; // Next
  Result := pathCost;
End; // Function

// ;This function clears a unit's claimed nodes. This function is
// ;called principally by ClaimNodes() before new nodes are
// ;claimed. It is also called by CheckPathStepAdvance() when the
// ;final path node is reached and by LaunchProgram() to initialize
// ;each unit's initial location.
// Function ClearNodes(unit.unit)
Procedure ClearNodes(pathfinderID: Integer);
var
  x, y, a, b: Integer;
Begin
  // x = Floor(unit\xLoc/tileSize) :
  x := UnitRecordArray[pathfinderID].startXLoc;
  // y = Floor(unit\yLoc/tileSize)
  y := UnitRecordArray[pathfinderID].startYLoc;
  For a := x - 1 To x + 1 do
    For b := y - 1 To y + 1 do
    begin
      If ((a >= 0) And (a < mapWidth) And (b >= 0) And (b < mapHeight)) then
        If (claimedNode[a, b] = pathfinderID) Then
          claimedNode[a, b] := 0 // Null
    End; // If   Next    Next
  // reclaim the one the unit is currently occupying.
  claimedNode[x, y] := pathfinderID;
End; // Function

// ;This function checks to see if the next path step is free.
// ;It is called from ClaimNodes() and by UpdatePath() when the
// ;unit is tempStopped.
// Function DetectCollision.unit(unit.unit)   Return claimedNode(x1,y2)
Function DetectCollision(pathfinderID: Integer): Integer;
var
  x1, y1, x2, y2: Integer;
Begin
  gDiagonalBlockage := False;
  // x2 = PeekShort (unit\pathBank,unit\pathLocation*4)
  x2 := // pathBank[pathfinderID] [pathLocation[pathfinderID]*4];
    UnitpathBank[UnitRecordArray[pathfinderID].CurrentpathBank][pathfinderID]
    [UnitRecordArray[pathfinderID].pathLocation * 4];
  // y2 = PeekShort (unit\pathBank,unit\pathLocation*4+2)
  y2 := // pathBank[pathfinderID] [pathLocation[pathfinderID]*4+2];
    UnitpathBank[UnitRecordArray[pathfinderID].CurrentpathBank][pathfinderID]
    [UnitRecordArray[pathfinderID].pathLocation * 4 + 2];
  Result := claimedNode[x2, y2]; // Compiler shut up
  If claimedNode[x2, y2] = 0 { Null } then
  begin
    x1 := UnitRecordArray[pathfinderID].startXLoc; // Floor(unit\xLoc/tileSize)
    y1 := UnitRecordArray[pathfinderID].startYLoc; // Floor(unit\yLoc/tileSize)
    If ((x1 <> x2) And (y1 <> y2)) then
    begin // ;if next path step is diagonal
      If ((claimedNode[x1, y2] <> 0 { Null } ) and
        (claimedNode[x1, y2] = claimedNode[x2, y1])) then
      begin
        gDiagonalBlockage := True;
        Result := claimedNode[x1, y2];
      end;
    End; // If End If End If
  end
  Else
    Result := claimedNode[x2, y2];
End; // Function

// ;This function checks whether a unit has reached the next
// ;path node (true) or is between nodes (false).
// NOT USED ?? by this??? version
Function ReachedNextPathNode(pathfinderID: Integer { unit.unit } ): Boolean;
begin
  Result := False;
  If UnitRecordArray[pathfinderID].pathStatus <> found Then
    Result := True;
  // If unit\xLoc = unit\xPath And unit\yLoc = unit\yPath
  If ((UnitRecordArray[pathfinderID].startXLoc = UnitRecordArray[pathfinderID]
    .xPath) and (UnitRecordArray[pathfinderID].startYLoc = UnitRecordArray
    [pathfinderID].yPath)) Then
    Result := True;
End; // Function

// ;This function checks to see whether a unit is on another unit's
// ;path. It is called by ClaimNodes().
// Function UnitOnOtherUnitPath(unit.unit,otherUnit.unit)
Function UnitOnOtherUnitPath(pathfinderID, otherUnit: Integer): Boolean;
var
  temppathLocation, unitX, unitY: Integer;
begin
  Result := False;
  // unitX = Floor(unit\xLoc/tileSize)
  unitX := UnitRecordArray[pathfinderID].startXLoc;
  // unitY = Floor(unit\yLoc/tileSize)
  unitY := UnitRecordArray[pathfinderID].startYLoc;
  // For pathLocation = otherUnit\pathLocation To otherUnit\pathLength
  For temppathLocation := UnitRecordArray[otherUnit].pathLocation -
    1 to UnitRecordArray[otherUnit].pathLength do
  begin
    If ((unitX = // PeekShort (otherUnit\pathBank,pathLocation*4)
      UnitpathBank[UnitRecordArray[otherUnit].CurrentpathBank][otherUnit]
      [temppathLocation * 4])
      // pathBank[otherUnit] [temppathLocation*4])
      // If unitY = PeekShort(otherUnit\pathBank,pathLocation*4+2)
      and (unitY = UnitpathBank[UnitRecordArray[otherUnit].CurrentpathBank]
      [otherUnit][temppathLocation * 4 + 2])) then
      Result := True; // End If End If
    // If pathLocation > otherUnit\pathLocation+1 Then Return
    If temppathLocation > UnitRecordArray[otherUnit].pathLocation + 1 Then
      Result := True;
  end;
End; // Function

// ;This function is used by the FindPath() function to
// ;check whether the given target location is walkable.
// ;If not, it finds a new, nearby target location that is
// ;walkable. The new coordinates are written to the
// ;gInt1 and gInt2 global variables.
// Function CheckRedirect(unit.unit, x,y)
Function CheckRedirect(pathfinderID, x, y: Integer): Integer;
var
  radius, option: Integer;
Begin
  Result := CheckRedirectFailed;
  If NodeOccupied(pathfinderID, x, y) = True then
  begin
    For radius := 1 To 10 do
      For option := 1 To 4 do
      begin
        If option = 1 then
        begin
          gInt1 := x;
          gInt2 := y - radius;
        end
        Else If option = 2 then
        begin
          gInt1 := x;
          gInt2 := y + radius;
        end
        Else If option = 3 then
        begin
          gInt1 := x - radius;
          gInt2 := y;
        end
        Else If option = 4 then
        begin
          gInt1 := x + radius;
          gInt2 := y;
        end;
        // End;// If

        If ((gInt1 >= 0) And (gInt1 < mapWidth) And (gInt2 >= 0) And
          (gInt2 < mapHeight)) then
          If NodeOccupied(pathfinderID, gInt1, gInt2) = False then
          begin
            If ((x = UnitRecordArray[pathfinderID].targetX)
              // Floor(unit\targetX/tileSize)
              And (y = UnitRecordArray[pathfinderID].targetY))
            // Floor(unit\targetY/tileSize)
            then
            begin // unit\targetX
              UnitRecordArray[pathfinderID].targetX := gInt1;
              // *tileSize+.5*tileSize
              // unit\targetY
              UnitRecordArray[pathfinderID].targetY := gInt2;
              // *tileSize+.5*tileSize}
              Result := CheckRedirectSucceeded;
            end;
          end; // End If
        { Return succeeded ;1 End If End If Next Next
          Return failed ;unable to find redirect (returns -1). End If }
      end;
  end;
End; // Function     succeeded ;1    failed -1

// ;This function is used by the CheckRedirect() functions. to
// ;determine whether a given node is walkable for a given unit.
// Function NodeOccupied(unit.unit,x,y)
Function NodeOccupied(pathfinderID, x, y: Integer): Boolean;
begin
  Result := False; // compiler shut up
  If walkability[x, y] = unwalkable Then
  begin
    Result := True;
    exit;
  end;
  // Leviathan
  If ProjectRecord.ProcessNoGoIslands then
    If island[x, y] <> island[UnitRecordArray[pathfinderID].startXLoc,
      UnitRecordArray[pathfinderID].startYLoc] Then
    begin
      Result := True;
      exit;
    end;

  If ((claimedNode[x, y] = 0 { Null } ) Or (claimedNode[x, y] = pathfinderID))
  then // ;node is free
  begin
    Result := False;
    exit;
  end

  Else // ;there is another unit there
    If (pathStatus[claimedNode[x, y]] = found) then // ;but if it is moving ...
      If claimedNode[x, y] <> UnitRecordArray[pathfinderID].unitCollidingWith
      then // ;and unit is not colliding with it
        Result := False
      else
        Result := True;
  // End If End If End If Return True}

  If walkability[x, y] <> unwalkable then // node is free
  begin
    If ((claimedNode[x, y] = 0 { Null } ) Or (claimedNode[x, y] = pathfinderID))
    then
    begin
      Result := False;
      exit;
    end // Return False
    Else // ;there is another unit there
      // ;but if it is moving ...
      If (UnitRecordArray[claimedNode[x, y]].pathStatus = found) then
        // ;and unit is not colliding with it
        If claimedNode[x, y] <> UnitRecordArray[pathfinderID].unitCollidingWith
        then
          Result := False
        else
          Result := True;
  end; // End If End If End If End If Return True}
End; // Function

// This function is used by the FindPath() function to lay out
// 'footprints' for other nearby units. A node within 1 node of
// the pathfinding unit that is occupied by another unit is
// treated as unwalkable. This function also lays out the
// current paths of any units within two units of the pathfinding
// unit. These nodes are then penalized within the FindPath()
// function. This encourages paths that do not overlap those
// of nearby units.//;This function is used by the FindPath() function to lay out
// 'footprints' for other units within 1 node. FindPath() treats
// these nodes as unwalkable.
// Function CreateFootPrints(unit.unit)
Procedure CreateFootPrints(pathfinderID: Integer);
var
  temppathLocation, otherUnitunit, a, b, unitX, unitY, x, y: Integer;
begin
  otherUnitunit := 0; // pathfinderID;//Compiler shutup
  tempUnwalkable := onClosedList - 2;
  penalized := onClosedList - 3;
  unitX := UnitRecordArray[pathfinderID].startXLoc;
  // unitX = Floor(unit\xLoc/tileSize) :
  unitY := UnitRecordArray[pathfinderID].startYLoc;
  // unitY = Floor(unit\yLoc/tileSize)
  // For a = unitX-2 To unitX+2
  // For b = unitY-2 To unitY+2
  For a := unitX - 2 To unitX + 2 Do
    For b := unitY - 2 To unitY + 2 DO
    Begin
      { If a >= 0 And a < mapWidth And b>=0 And b < mapHeight
        If claimedNode(a,b) <> Null And claimedNode(a,b) <> unit
        otherUnit.unit = claimedNode(a,b) }
      If ((a >= 0) And (a < mapWidth) And (b >= 0) And (b < mapHeight)) then
      begin
        If ((claimedNode[a, b] <> 0 { Null } ) And
          (claimedNode[a, b] <> pathfinderID)) then
        begin
          tempUnwalkability[a, b] := tempUnwalkable;
          otherUnitunit := claimedNode[a, b];
        end;
        If otherUnitunit > 0 then // Dont do Empty places..Units
        begin
          // ;Lay out penalized paths for units within 2 nodes of
          // ;the pathfinding unit.
          // For pathLocation := otherUnit\pathLocation-1 To otherUnit\pathLength
          For temppathLocation := UnitRecordArray[otherUnitunit].pathLocation -
            1 to UnitRecordArray[otherUnitunit].pathLength do
            If temppathLocation >= 0 then
            begin
              // x = PeekShort (otherUnit\pathBank,pathLocation*4)
              x := // pathBank[otherUnitunit] [temppathLocation*4];
                UnitpathBank[UnitRecordArray[otherUnitunit].CurrentpathBank]
                [otherUnitunit][temppathLocation * 4];
              // y = PeekShort (otherUnit\pathBank,pathLocation*4+2)
              y := // pathBank[otherUnitunit] [temppathLocation*4+2];
                UnitpathBank[UnitRecordArray[otherUnitunit].CurrentpathBank]
                [otherUnitunit][temppathLocation * 4 + 2];
              nearByPath[x, y] := penalized;
            end;

          // Designate nodes occupied by units within 1 node
          // as temporarily unwalkable.
          If ((Abs(a - unitX) <= 1) And (Abs(b - unitY) <= 1)) then
            tempUnwalkability[a, b] := tempUnwalkable;
        end;
      End; // If
    End;
End; // Function

// ;This function identifies nodes on the map that are not accessible from other areas
// ;of the map ("islands"). It assumes that the map does not change during the game.
// ;If so, this function must be called again. It is not a good idea to do this too often
// ;during the game, especially if it is a large map, because the function is a little slow.
// ;The island information is saved to an array called island(x,y).
Procedure IdentifyIslands;
var
  startX, startY, areaID, onOpenList, openListItems, m, squaresChecked, a, b,
    parentXval, parentYVal: Integer;
Begin
  SetLength(island, mapWidth + 1, mapHeight + 1);
  areaID := 0;
  squaresChecked := 0;
  For startX := 0 To mapWidth - 1 Do
    For startY := 0 To mapHeight - 1 Do
      If ((walkability[startX, startY] = walkable) And
        (island[startX, startY] = 0)) then
      begin
        areaID := areaID + 1;
        // changing the values of onOpenList and onClosed list
        // is faster than redimming whichList() array
        onClosedList := onClosedList + 5;
        onOpenList := onClosedList - 1;
        openListItems := 1; // :
        openList[1] := 1; // :
        openX[1] := startX; // :
        openY[1] := startY;

        Repeat
        begin
          parentXval := openX[openList[1]]; // :
          parentYVal := openY[openList[1]];
          // put last item in slot #1
          openList[1] := openList[openListItems];
          // reduce number of open list items by 1
          openListItems := openListItems - 1;
          // add cell to closed list
          whichList[parentXval, parentYVal] := onClosedList;
          // Assign item to areaID
          island[parentXval, parentYVal] := areaID;
          For b := parentYVal - 1 To parentYVal + 1 do
            For a := parentXval - 1 To parentXval + 1 do
            begin
              If ((a <> -1) And (b <> -1) And (a <> mapWidth) And
                (b <> mapHeight)) then
              begin
                If ((whichList[a, b] <> onClosedList) And
                  (whichList[a, b] <> onOpenList)) then
                begin
                  If walkability[a, b] <> unwalkable then
                  begin // not = walkable because could = occupied
                    If (a = parentXval) Or (b = parentYVal) then
                    begin // If an orthogonal square of the right type(s)
                      squaresChecked := squaresChecked + 1;
                      m := openListItems + 1; // m = new item at end of heap
                      openList[m] := squaresChecked;
                      openX[squaresChecked] := a; // :
                      openY[squaresChecked] := b;
                      // add one to number of items on the open list
                      openListItems := openListItems + 1;
                      whichList[a, b] := onOpenList;
                    End; // If ;If an orthogonal square of the right type(s)
                  End; // If ;If walkability(a,b) <> unwalkable
                End; // If ;If not on the open or closed lists
              End; // If ;If not off the map.//Next//Next
            end;
        end;
        Until openListItems = 0;
      End; // If Next Next
End; // Function

// ;This function chooses separate destinations for each member of
// ;of a group of selected units. When we choose a destination
// ;for the group, we don't want them to all try to go that exact
// ;location. Instead we want them to go to separate locations close
// ;to that group target location.
// ;	If the units are all close enough together, the function merely
/// ;returns that each unit should stay in the same place relative to one
// ;another. If the units are spread out, the function chooses a relative
// ;location for the unit.
// Called from Mouse Event
Procedure ChooseGroupLocations;
Begin
  { ;Figure out the group center
    For unit.unit = Each unit
    If unit\selected = True
    totalX = totalX + unit\xLoc#
    totalY = totalY + unit\yLoc#
    numberOfUnitsInGroup = numberOfUnitsInGroup+1
    End If
    Next
    If numberOfUnitsInGroup = 0 Then Return
    groupCenterX# = totalX/numberOfUnitsInGroup
    groupCenterY# = totalY/numberOfUnitsInGroup

    ;Figure out if all of the units in the selected group are close enough to
    ;each other to keep them more or less in the same locations relative
    ;to one another.
    maxDistance = tileSize*Sqr(numberOfUnitsInGroup)
    For unit.unit = Each unit
    If unit\selected = True
    unit\xDistanceFromGroupCenter# = unit\xLoc#-groupCenterX#
    unit\yDistanceFromGroupCenter# = unit\yLoc#-groupCenterY#
    If Abs(unit\xDistanceFromGroupCenter#) > maxDistance
    unitOutsideMaxDistance = True
    Else If Abs(unit\yDistanceFromGroupCenter#) > maxDistance
    unitOutsideMaxDistance = True
    End If
    End If
    Next

    ;If they are all close enough together, we don't need to adjust their relative
    ;locations.
    If unitOutsideMaxDistance = False
    ;do nothing

    ;If one or more group members is too far away, we need to generate a new
    ;set of relative locations for the group members.
    Else If numberOfUnitsInGroup = 2

    For unit.unit = Each unit
    If unit\selected = True
    unit\actualAngleFromGroupCenter = 0
    unit\assignedAngleFromGroupCenter = 0
    unit\xDistanceFromGroupCenter# = Sgn(unit\xDistanceFromGroupCenter#)*tileSize/2
    unit\yDistanceFromGroupCenter# = Sgn(unit\yDistanceFromGroupCenter#)*tileSize/2
    End If
    Next

    Else ;if 3+ units

    ;Figure out the angles between each unit in the group and the group center.
    ;Also, save unit type pointers to an array for sorting purposes
    Dim gGroupUnit.unit(numberOfUnitsInGroup+1)
    For unit.unit = Each unit
    If unit\selected = True
    x = x+1
    gGroupUnit.unit(x) = unit
    unit\actualAngleFromGroupCenter = GetAngle(groupCenterX#,groupCenterY#,unit\xLoc#,unit\yLoc#)
    End If
    Next

    ;Sort the units in the group according to their angle, from lowest to highest
    topItemNotSorted = numberOfUnitsInGroup
    While topItemNotSorted <> 1

    ;Find the highest value in the list
    highestValueItem = 1
    For sortItem = 1 To topItemNotSorted
    If gGroupUnit(sortItem)\actualAngleFromGroupCenter >= gGroupUnit(highestValueItem)\actualAngleFromGroupCenter
    highestValueItem = sortItem
    End If
    Next

    ;Now swap it with the highest item in the list
    temp.unit = gGroupUnit(topItemNotSorted)
    gGroupUnit(topItemNotSorted) = gGroupUnit(highestValueItem)
    gGroupUnit(highestValueItem) = temp

    topItemNotSorted = topItemNotSorted - 1
    Wend

    ;Now assign angles to each of the units in the group
    gGroupUnit(1)\assignedAngleFromGroupCenter = gGroupUnit(1)\actualAngleFromGroupCenter
    addAngle# = 360/numberOfUnitsInGroup
    For x = 2 To numberOfUnitsInGroup
    gGroupUnit(x)\assignedAngleFromGroupCenter = gGroupUnit(x-1)\assignedAngleFromGroupCenter + addAngle
    If gGroupUnit(x)\assignedAngleFromGroupCenter >= 360
    gGroupUnit(x)\assignedAngleFromGroupCenter = gGroupUnit(x)\assignedAngleFromGroupCenter-360
    End If
    Next

    ;Now assign the xDistanceFromGroupCenter and yDistanceFromGroupCenter
    If numberOfUnitsInGroup <= 6
    radius# = Sqr(numberOfUnitsInGroup)*0.8*tileSize
    For unit.unit = Each unit
    If unit\selected = True
    unit\xDistanceFromGroupCenter# = radius*Cos(unit\assignedAngleFromGroupCenter)+(unit\ID Mod(2))
    unit\yDistanceFromGroupCenter# = -radius*Sin(unit\assignedAngleFromGroupCenter)	+(unit\ID Mod(2))
    End If
    Next

    ;If there are more than 6 units in the group, create two rings of units.
    Else
    innerRadius# = Sqr(numberOfUnitsInGroup/2)*0.8*tileSize
    outerRadius# = 2.5*Sqr(numberOfUnitsInGroup/2)*0.8*tileSize
    x = 0
    For unit.unit = Each unit
    If unit\selected = True
    x = x+1
    If x Mod 2 = 0
    unit\xDistanceFromGroupCenter# = innerRadius*Cos(unit\assignedAngleFromGroupCenter)
    unit\yDistanceFromGroupCenter# = -innerRadius*Sin(unit\assignedAngleFromGroupCenter)
    Else
    unit\xDistanceFromGroupCenter# = outerRadius*Cos(unit\assignedAngleFromGroupCenter)
    unit\yDistanceFromGroupCenter# = -outerRadius*Sin(unit\assignedAngleFromGroupCenter)
    End If
    End If
    Next
    End If

    End If  ;If group\numberOfUnitsInGroup = 2

    ;Now that the relative locations have been determined, we use this info
    ;to generate the units' destination locations.
    For unit.unit = Each unit
    If unit\selected = True
    unit\targetX# = MouseX() + unit\xDistanceFromGroupCenter#
    unit\targetY# = MouseY() + unit\yDistanceFromGroupCenter#
    If unit\targetX < 0 Then unit\targetX = 0
    If unit\targetX >= 800 Then unit\targetX = 799
    If unit\targetY < 0 Then unit\targetY = 0
    If unit\targetY >= 600 Then unit\targetY = 599
    End If
    Next
  }
End; // Function

/// ////////////////////////////////////////////////////
// from the common unit
//
// Returns the angle between the first point and the second point.
// Zero degrees is at the 3 o'clock position. Angles proceed in a
// counterclockwise direction. For example, 90 degrees is at
// 12 o'clock. 180 degrees is at 9 o'clock, and 270 degrees
// is at 6 o'clock.
// Also, please note that this function is using screen coordinates,
// where y increases in value as it goes down.
// Note that the Blitz ATan2() function returns -180 to 180 with
// zero being the 12 o'clock position if y increases as you move up
// the screen, and 6'oclock if y increases as you move down the screen.
// This functions adjusts for that.  GetAngle(x1#,y1#,x2#,y2#
{ ArcTan   Calculates the arctangent of a given number.
  Tan(x) = Sin(x) / Cos(x)
  ArcSin(x) = ArcTan (x/sqrt (1-sqr (x)))
  ArcCos(x) = ArcTan (sqrt (1-sqr (x)) /x)
  ArcTan2 calculates ArcTan(Y/X), and returns
  an angle in the correct quadrant.
  The values of X and Y must be between –2^64 and 2^64.
  In addition, the value of X can’t be 0.
  The return value will fall in the range from -Pi to Pi radians.
}
Function GetAngle(x1, y1, x2, y2: Double): Double;
var
  angle: Double;
begin // ATan2
  angle := RadToDeg(ArcTan2(x2 - x1, y2 - y1));
  If ((angle >= 90) And (angle <= 180)) then
    Result := angle - 90
  Else
    Result := angle + 270;
End; // Function

// ;Note: Blitz calculates squares very slowly for some reason,
// ;so it is much faster to multiply the values rather than using
// ;the shorthand "^2".  GetDistance#(x1#,y1#,x2#,y2#)
Function GetDistance(x1, y1, x2, y2: Double): Double;
begin
  Result := Sqr((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2))
End; // Function

// This function calculates the average amount time that has passed
// per loop over the past 20 game loops. This rolling average
// is combined with speed information (expressed in pixels/second) to
// determine how far to move a unit in a given loop.
// We use this time-based approach to ensure consistent unit
// movement speeds. If units instead moved a fixed distance every
// loop, the movement speed would be inconsistent from one PC
// to the next because of different chip speeds and monitor refresh
// ;rates.
// A rolling average is used because the Millisecs() function does
// not always return a reliably accurate time down to the millisecond.
// Using an average over the past 20 game loops is more reliable.
// Dim savedClockTime#(20)
// Global savedClockCount
Procedure UpdateGameClock;
var
  timestep: Int64;
Begin
  // inc(savedClockCount);// = savedClockCount+1
  If savedClockCount >= 20 Then
    savedClockCount := 0;
  timestep := StartPrecisionTimer; // time# = MilliSecs()
  gLoopTime := (timestep - savedClockTime[savedClockCount]);
  /// 20000
  inc(savedClockCount); // = savedClockCount+1
  savedClockTime[savedClockCount] := timestep;
  If gLoopTime > 0.1 Then
    gLoopTime := 0.0167;
End; // Function

// This function performs pathfinding and moves the units.
Procedure MoveUnits;
var
  i: Integer;
Begin
  If gGameStarted then // NumberofUnits
    For i := 1 to ProjectRecord.NumberofActiveUnits do
    begin
      UpdatePath(i);
      If UnitRecordArray[i].pathStatus = found Then
        MoveUnit(i);
      Application.ProcessMessages;
    end;
End;

// This function checks for path updates and calls the
// FindPath() function when needed.
Procedure UpdatePath(pathfinderID: Integer);
var // x1,x2,y1,y2,
  otherUnit: Integer;
begin
  // ;If the unit is tempStopped, keep checking the
  // blocked path node until it is free and the unit is able continue
  // along its path. If the next step is blocked by a stopped unit
  // then repath.
  If UnitRecordArray[pathfinderID].pathStatus = tempStopped then
  begin
    otherUnit := DetectCollision(pathfinderID);
    If otherUnit = 0 { Null } then
    begin
      UnitRecordArray[pathfinderID].pathStatus := found;
      ClaimNodes(pathfinderID)
    end // ;node is blocked by nonmoving unit
    Else If UnitRecordArray[otherUnit].pathStatus <> found then
    begin
      UnitRecordArray[pathfinderID].unitCollidingWith := otherUnit;
      pathStatus[pathfinderID] := BlitzFindPath(pathfinderID,
        UnitRecordArray[pathfinderID].startXLoc,
        UnitRecordArray[pathfinderID].startYLoc,
        UnitRecordArray[pathfinderID].targetX,
        UnitRecordArray[pathfinderID].targetY, normal)
    End; // If
  End
  // If the unit's path is nonexistent, find a path to a random location that
  // is nearby. This will tend to break up units that are locked in place.
  Else If UnitRecordArray[pathfinderID].pathStatus = nonexistent then
    UnitRecordArray[pathfinderID].pathStatus :=
      BlitzFindPath(pathfinderID, UnitRecordArray[pathfinderID].startXLoc,
      UnitRecordArray[pathfinderID].startYLoc, 0, 0, randomMove)

    // If the unit's pathStatus = notStarted, and the unit is not at its target location, then
    // generate a new path to that location. This can be true if a unit has found a path
    // to a random location after experiencing a nonexistent path (see above).
  Else If UnitRecordArray[pathfinderID].pathStatus = notstarted // stopped
  then
  begin
    { x1 := Floor(unit\xLoc/tileSize)
      y1 := Floor(unit\yLoc/tileSize)
      x2 := Floor(unit\targetX/tileSize)
      y2 := Floor(unit\targetY/tileSize)
      If  ((x1 <> x2) Or (y1 <> y2)) then }
    If ((UnitRecordArray[pathfinderID].startXLoc <> UnitRecordArray
      [pathfinderID].targetX) Or (UnitRecordArray[pathfinderID].startYLoc <>
      UnitRecordArray[pathfinderID].targetY)) then
      UnitRecordArray[pathfinderID].pathStatus :=
        BlitzFindPath(pathfinderID, UnitRecordArray[pathfinderID].startXLoc,
        UnitRecordArray[pathfinderID].startYLoc,
        UnitRecordArray[pathfinderID].targetX,
        UnitRecordArray[pathfinderID].targetY, normal)
  end // End If End If
  // from inside UpdatePath
  // If the unit has been selected, trigger new paths using the mouse.
  // There is a delay built in so the new path isn't implemented
  // until the next node is reached.
  { If unit\selected = True
    If gMouseHit2 = True
    If unit\distanceToNextNode = 0
    unit\pathStatus = FindPath(unit.unit,unit\targetX,unit\targetY)
    Else
    unit\startNewPath = True ;wait to trigger path (see below)
    End If
    Else If unit\startNewPath = True And unit\distanceToNextNode = 0
    unit\pathStatus = FindPath(unit.unit,unit\targetX,unit\targetY)
    unit\startNewPath = False
    End If }
  // If pathAI = random, choose a random spot on the screen to pathfind to.
  Else If UnitRecordArray[pathfinderID].pathAI = random then
  begin
    If UnitRecordArray[pathfinderID].pathStatus = stopped then
    begin
      UnitRecordArray[pathfinderID].targetX := random(mapWidth);
      UnitRecordArray[pathfinderID].targetY := random(mapHeight);
      UnitRecordArray[pathfinderID].pathStatus :=
        BlitzFindPath(pathfinderID, UnitRecordArray[pathfinderID].startXLoc,
        UnitRecordArray[pathfinderID].startYLoc,
        UnitRecordArray[pathfinderID].targetX,
        UnitRecordArray[pathfinderID].targetY, randomMove)
    End; // If
  End; // If
End;

// This function moves sprites around on the screen.
Procedure MoveUnit(pathfinderID: Integer);
var
  remainingDistance: Double;
Begin
  // Move toward the next path node
  remainingDistance := MoveTowardNode(pathfinderID,
    { gLoopTime* } UnitRecordArray[pathfinderID].speed);

  // If there is any remaining distance left after moving toward the node, then
  // check for path step advances and move to the next one. This two step
  // process ensures smooth movement from node to node.
  If ((remainingDistance <> 0) And
    (UnitRecordArray[pathfinderID].startNewPath = False)) then
    MoveTowardNode(pathfinderID, remainingDistance);
End;

// This function checks for path step advances and then moves toward the
// next path node. If the next node is reached, the function returns any
// remaining distance left to be travelled.
Function MoveTowardNode(pathfinderID: Integer;
  distanceTravelled: Double): Double;
var
  xVector, yVector, angle, remainingDistance: Double;
Begin
  Result := 0;
  CheckPathStepAdvance(pathfinderID);
  If UnitRecordArray[pathfinderID].pathStatus <> found Then
    exit;
  If distanceTravelled <= UnitRecordArray[pathfinderID].distanceToNextNode then
  begin
    xVector := UnitRecordArray[pathfinderID].xPath - UnitRecordArray
      [pathfinderID].startXLoc; // unit\xLoc
    yVector := UnitRecordArray[pathfinderID].yPath - UnitRecordArray
      [pathfinderID].startYLoc; // unit\yLoc
    angle := ArcTan2(yVector, xVector); // ArcTan2   ATan2
    UnitRecordArray[pathfinderID].startXLoc := UnitRecordArray[pathfinderID]
      .startXLoc + Round(Cos(angle) * distanceTravelled);
    UnitRecordArray[pathfinderID].startYLoc := UnitRecordArray[pathfinderID]
      .startYLoc + Round(Sin(angle) * distanceTravelled);
    UnitRecordArray[pathfinderID].distanceToNextNode :=
      UnitRecordArray[pathfinderID].distanceToNextNode - distanceTravelled;
  end
  Else // ;next path node has been reached
  Begin
    UnitRecordArray[pathfinderID].startXLoc := UnitRecordArray
      [pathfinderID].xPath;
    UnitRecordArray[pathfinderID].startYLoc := UnitRecordArray
      [pathfinderID].yPath;
    remainingDistance := distanceTravelled - UnitRecordArray[pathfinderID]
      .distanceToNextNode;
    UnitRecordArray[pathfinderID].distanceToNextNode := 0;
    Result := remainingDistance;
  End; // If
End;

/// //////////////////////////////////////
// RunUnitLoop;// until ???? [Ctrl] F9 Toggles AstarUnitsRunning
Procedure RunUnitLoop;
var
  timestep: Int64;
  xx, yy, i: Integer;
Begin
  savedClockCount := 0;
  savedClockTime[0] := 0;
  gGameStarted := True;
  RenderScreen;
  For i := 1 to NumberofUnits do
  begin
    UnitRecordArray[i].pathLocation := 0;
    UnitRecordArray[i].xPath :=
      BlitzReadPathX(i, UnitRecordArray[i].pathLocation);
    UnitRecordArray[i].yPath :=
      BlitzReadPathY(i, UnitRecordArray[i].pathLocation);

  end;
  Repeat
  Begin
    // inc(FPSCount);
    timestep := StartPrecisionTimer;
    Application.ProcessMessages;
    MoveUnits;
    Application.ProcessMessages;
    For i := 1 to NumberofUnits do
    begin
      If (UnitRecordArray[i].xPath div tileSize) <> (UnitRecordArray[i].targetX)
      then
      begin
        UnitRecordArray[i].pathLocation :=
          (UnitRecordArray[i].pathLocation + 1);
        UnitRecordArray[i].xPath :=
          BlitzReadPathX(i, UnitRecordArray[i].pathLocation);
        UnitRecordArray[i].yPath :=
          BlitzReadPathY(i, UnitRecordArray[i].pathLocation);
        { AStarImage.Picture.Bitmap.Canvas.Brush.Color :=TargetGoalColorArray[i];
          AStarImage.Picture.Bitmap.Canvas.Ellipse(
          Rect(xPath[i]+(2),yPath[i]+(2),xPath[i]+tileSize-(2),yPath[i]+tileSize-(2)));
        }
      end
      else
        AstarUnitsRunning := False;
    end;
    Application.ProcessMessages;
    If ProjectRecord.ClaimedNodesDisplayed then
      DrawClaimedNodes;
    Application.ProcessMessages;

    UpdateGameClock;
    gGameTime := StopPrecisionTimer(timestep);
    { StatusBar1.Panels[5].Text:=
      Format('%d - %.3f ? %.3f',[FPSCount,gGameTime,gLoopTime*1000]);
    }
    For xx := 0 to 1000000 * (10 - speedArray[1]) do
      Application.ProcessMessages;

    // showmessage('Step: '+Inttostr(FPSCount));
  End;
  Until AstarUnitsRunning = False;
  gGameStarted := False;
  // showmessage('Finished');
  For xx := 0 To mapWidth - 1 Do
    For yy := 0 To mapHeight - 1 Do
      claimedNode[xx, yy] := 0;
  for xx := 1 to 3 do
  begin
    // unit\targetX = unit\xLoc : unit\targetY = unit\yLoc ;required
    UnitRecordArray[xx].xPath := UnitRecordArray[xx].startXLoc;
    UnitRecordArray[xx].yPath := UnitRecordArray[xx].startYLoc;
    // ClearNodes(xx);
    UnitRecordArray[xx].pathStatus := notstarted;
  end;
End;

procedure PaintLinePath(ID: Integer);
Begin
  // Paint lines connecting the path points... For all except Demo
  // Smileys use Dots.. leave their outline as path...

End;

// This function draws unit claimed nodes. It is called by the
// RenderScreen() function.  DrawClaimedNodes(drawSelectedOnly=False)
Procedure DrawClaimedNodes; // (drawSelectedOnly=False)
var
  x, y, i: Integer;
Begin
  // If gDrawMore = True
  For x := 0 to mapWidth - 1 do
    For y := 0 to mapHeight - 1 do
    Begin
      If claimedNode[x, y] <> 0 { Null } then // claimed nodes
      begin
        i := claimedNode[x, y];
        // Lester had 3'real' and 3 'other' units
        // If drawSelectedOnly=False Or unit\ID <= 3
        If UnitRecordArray[i].pathStatus <> tempStopped then
        begin
          // Color unit\red,unit\green,unit\blue
          // Rect x*tileSize+.4*tileSize,y*tileSize+.4*tileSize,.2*tileSize,.2*tileSize,1
          { AStarImage.Picture.Bitmap.Canvas.Brush.Color :=
            BaseStartColorArray[i];
            AStarImage.Picture.Bitmap.Canvas.FrameRect(
            Rect(X,Y,X+tileSize,Y+tileSize)); }
        end;
      end;
    end;
  // draw square when unit is tempStopped
  For i := 1 to NumberofUnits do
  begin // If drawSelectedOnly=False Or unit\ID <= 3
    If UnitRecordArray[i].pathStatus = tempStopped then
    begin
      // Color unit\red,unit\green,unit\blue
      // Rect x*tileSize+.4*tileSize,y*tileSize+.4*tileSize,.2*tileSize,.2*tileSize,1
      { AStarImage.Picture.Bitmap.Canvas.Brush.Color :=
        BaseStartColorArray[i];
        //Rect ReadPathX(unit,unit\pathLocation)-.25*tileSize,
        //ReadPathY(unit,unit\pathLocation)-.25*tileSize,.5*tileSize,.5*tileSize,0
        X:= ReadPathX(i,pathLocation[i]);
        Y:= ReadPathY(i,pathLocation[i]);
        AStarImage.Picture.Bitmap.Canvas.FrameRect(
        Rect(X,Y,X+tileSize,Y+tileSize));
      }
    end;
  end;
End; // Function

// This function draws stuff on the screen.
Procedure RenderScreen;
var
  i: Integer;
  Procedure RunAll;
  var
    // xx,
    AllID: Integer;
    a1, a2, a3, a4, a5: Boolean;
  Begin
    a1 := True; // False  True
    a2 := True;
    a3 := True;
    a4 := True;
    a5 := True;
    // The Findpath calls ReadPath the does +1 so Have to reset back 1
    For AllID := 1 to NumberofUnits Do
    begin
      // pathLocation[AllID] := (pathLocation[AllID] - 1);
      Case AllID of
        1:
          a1 := False;
        2:
          a2 := False;
        3:
          a3 := False;
        4:
          a4 := False;
        5:
          a5 := False;
      end;
    end;
    Repeat
    Begin
      For AllID := 1 to NumberofUnits Do
      Begin
        {
          //StatusBar1.Panels[5].Text:='';
          If PathDisplayed then
          AStarImage.Picture.Bitmap.Canvas.Pen.Color := TargetGoalColorArray[AllID]
          else AStarImage.Picture.Bitmap.Canvas.Pen.Color := BackGroundColor;
          //If  NONE set it back False..Then All done...
          //        For Ii:= 0 to pathLength[AllID]-1 do
          If  (xPath[AllID] div tilesize) <> (targetXLocArray[AllID] ) then
          begin
          //          StatusBar1.Panels[5].Text:='Processing: '+inttostr(AllID);
          pathLocation[AllID] := (pathLocation[AllID] + 1);
          xPath[AllID] := ReadPathX(AllID,pathLocation[AllID]);
          yPath[AllID] := ReadPathY(AllID,pathLocation[AllID]);
          AStarImage.Picture.Bitmap.Canvas.Brush.Color :=TargetGoalColorArray[AllID];
          AStarImage.Picture.Bitmap.Canvas.Ellipse(
          Rect(xPath[AllID]+(2),yPath[AllID]+(2),xPath[AllID]+tileSize-(2),yPath[AllID]+tileSize-(2)));
          //Move the Sprites IAW their Speed
          //Call ____ times to allow time to SEE the Sprite move...
          //          For xx:= 0 to 1000000*(10-speedArray[AllID]) do  Application.ProcessMessages;
          //Paint over the Dot to 'erase'
          AStarImage.Picture.Bitmap.Canvas.Brush.Color :=BackGroundColor;
          AStarImage.Picture.Bitmap.Canvas.Ellipse(
          Rect(xPath[AllID]+(2),yPath[AllID]+(2),xPath[AllID]+tileSize-(2),yPath[AllID]+tileSize-(2)));

          //Repaint Last dot.. since it was 'erased'
          AStarImage.Picture.Bitmap.Canvas.Brush.Color :=TargetGoalColorArray[AllID];
          AStarImage.Picture.Bitmap.Canvas.Ellipse(
          Rect(xPath[AllID]+(2),yPath[AllID]+(2),xPath[AllID]+tileSize-(2),yPath[AllID]+tileSize-(2)));

          end else
          begin
          Case AllID of
          1:a1:=True;
          2:a2:=True;
          3:a3:=True;
          4:a4:=True;
          5:a5:=True;
          end;
          end;
        }
      End;
    End;
    Until ((a1 = True) and (a2 = True) and (a3 = True) and (a4 = True) and
      (a5 = True));
    // StatusBar1.Panels[5].Text:='All Paths Finished';
  End;

Begin
  For i := 1 to NumberofUnits do
    RunAll; // Runit(i);
End;
{
  ;Draw the walls and the grid 	overlay
  DrawMapImage() ;see shared functions.bb include file

  ;Draw paths
  If gDrawMore = True
  For unit.unit = Each unit
  If unit\selected = True
  Color unit\red,unit\green,unit\blue
  For pathLocation = 1 To unit\pathLength
  x1 = ReadPathX(unit.unit,pathLocation-1)
  y1 = ReadPathY(unit.unit,pathLocation-1)
  x2 = ReadPathX(unit.unit,pathLocation)
  y2 = ReadPathY(unit.unit,pathLocation)
  x1=x1+unit\ID*2 : x2=x2+unit\ID*2 : y1=y1+unit\ID : y2=y2+unit\ID
  Line x1,y1,x2,y2
  Next
  End If
  Next
  End If

  ;Draw units
  For unit.unit = Each unit
  DrawImage unit\sprite,unit\xLoc,unit\yLoc
  If unit\selected = True Then DrawImage gSelectedCircle,unit\xLoc,unit\yLoc
  Next

  ;Draw unit claimed nodes
  DrawClaimedNodes(True);see shared functions.bb include file

  ;Draw selection box
  If gBoxSelecting = True
  Color 0,255,0
  Line gBoxSelectX,gBoxSelectY,MouseX(),gBoxSelectY
  Line MouseX(),gBoxSelectY,MouseX(),MouseY()
  Line MouseX(),MouseY(),gBoxSelectX,MouseY()
  Line gBoxSelectX,MouseY(),gBoxSelectX,gBoxSelectY
  End If

  ;Draw text on the screen showing some unit statistics.
  If gDrawText = True
  Color 255,255,255 : x = 50 : y = 50
  For unit.unit = Each unit
  Text x,y+0,unit\ID
  Text x,y+15,unit\selected
  Text x,y+30,unit\pathStatus
  Text x,y+45,unit\xLoc
  Text x,y+60,unit\yLoc
  Text x,y+75,unit\xPath
  Text x,y+90,unit\yPath
  Text x,y+105,unit\pathLocation+"/"+unit\pathLength
  x = x + 100
  If unit\ID Mod 8 = 7 Then y = y+200 : x = 50
  Next
  End If

}

end.
