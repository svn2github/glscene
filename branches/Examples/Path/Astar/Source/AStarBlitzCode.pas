unit AStarBlitzCode;
//Degenerated from:
//A* Pathfinder by Patrick Lester. Used by permission.
//==================================================================
//Last updated 4/7/04
//http://www.policyalmanac.org/games/aStarTutorial.htm
//A* Pathfinding for Beginners
//This version of the aStar library has been modified to handle
//pathfinding around other units.

interface

uses
  AStarGlobals,
  forms;//application.ProcessMessages;


Function BlitzFindPath( pathfinderID, startingX,  startingY,//mode=normal
                                  targetX,  targetY,
                                  mode:Integer):Integer;

implementation

uses AStarBlitzUnit;//for the other procedures

//---------------------------------------------------------------------------
// Name: FindPath
// Desc: Finds a path using A*
{Function FindPath(unit.unit,targetX,targetY,mode=normal)
	;Random move mode is used when a unit's main target is
	;currently unreachable. After the random move, the unit
	;will try to pathfind to its original target again. Random
	;moves can break up units that happen to be approaching
	;each other from opposite directions down tight corridors.
	;See the UpdatePath() function.
	If mode = randomMove Then useDijkstras = True}
//Please note that targetX and targetY
//are pixel-based coordinates relative to the
//upper left corner of the map, which is 0,0.
//---------------------------------------------------------------------------
Function BlitzFindPath ( pathfinderID, startingX,  startingY,//mode=normal
                                  targetX,  targetY, mode:Integer):Integer;
var
  useDijkstras:Boolean;
  dx1,dx2,dy1,dy2,cross,//Tiebreakers
  XDistance,YDistance,HDistance, //Heuristic options
  startX,startY,x,y,
  onOpenList, parentXval, parentYval,
  a, b, m, u, v, temp, corner, numberOfOpenListItems,
  addedGCost, tempGcost,  node,
  //path,//moved to global
  tempx, pathX, pathY, cellPosition,
  newOpenListItemID:Integer;
  //13.If there is no path to the selected target, set the pathfinder's
  //xPath and yPath equal to its current location
  //and return that the path is nonexistent.
procedure noPath;
begin
  UnitRecordArray[pathfinderID].xPath := startingX;
  UnitRecordArray[pathfinderID].yPath := startingY;
  result:= nonexistent;
end;
procedure noPathRedirect;
begin
  UnitRecordArray[pathfinderID].xPath := startingX;
  UnitRecordArray[pathfinderID].yPath := startingY;
  result:= RedirectFailed;
end;
procedure noPathTarget;
begin
  UnitRecordArray[pathfinderID].xPath := startingX;
  UnitRecordArray[pathfinderID].yPath := startingY;
  result:= Targetunwalkable;
end;
Begin
  If mode = randomMove Then useDijkstras := True else
  useDijkstras:=False;
  //onOpenList:=0;
  //New changes made these un-initialized..buggy ?
  parentXval:=0; parentYval:=0; //corner:=0;
  addedGCost:=0;
  //a:=0; b:=0; m:=0; u:=0; v:=0;
  //temp:=0;    numberOfOpenListItems:=0;
  //path := 0;
  //  tempx:=0; pathX:=0; pathY:=0;  cellPosition:=0;
  //tempGcost := 0;
  newOpenListItemID:=0;  node:=0;

  //1. Convert location data (in pixels)
  //to coordinates in the walkability array.
  startX := startingX div ProjectRecord.tileSize;
  startY := startingY div ProjectRecord.tileSize;
  targetX := targetX div ProjectRecord.tileSize;
  targetY := targetY div ProjectRecord.tileSize;

  //2.Quick Path Checks: Under the some circumstances no path needs to
  //	be generated ...

//2.	Check for redirects
	result := CheckRedirect(pathfinderID,targetX,targetY);
        //target is unwalkable and could not find a redirect.
	If result = CheckRedirectFailed Then //noPath;
        begin noPathRedirect; exit;{current procedure} end else
	If result = CheckRedirectsucceeded Then
        begin    //island and Claimed nodes already made!
          targetX := gInt1;  //Actual Target X,Y already reset
          targetY := gInt2;  //by CheckRedirect
        end;//  targetX := gInt1 else targetY := gInt2;

  //If starting location and target are in the same location...
  //Blitz sets other stuff..this does it at end...
  if ((startX = targetX) and (startY = targetY)
      and (UnitRecordArray[pathfinderID].pathLocation > 0))
    then  result:= found;
  if ((startX = targetX) and (startY = targetY)
      and (UnitRecordArray[pathfinderID].pathLocation = 0))
    then  result:= nonexistent;

  //If target square is unwalkable, return that it's a nonexistent path.
  if (walkability[targetX][targetY] = unwalkable)then
    begin noPathTarget; exit;{current procedure} end;//goto noPath;

  //3.Reset some variables that need to be cleared
  if (onClosedList > 1000000)then //reset whichList occasionally
  begin
    for x := 0 to mapWidth-1 do
      for y := 0 to mapHeight-1 do whichList [x][y] := 0;
    onClosedList := 10;
  end;
  //changing the values of onOpenList and onClosed list
  //is faster than redimming whichList() array
  onClosedList := onClosedList+5;    //+2;
  onOpenList := onClosedList-1;
  tempUnwalkable := onClosedList-2;
  penalized := onClosedList-3;
  UnitRecordArray[pathfinderID].pathLength := notStarted;//i.e, = 0
  UnitRecordArray[pathfinderID].pathLocation:= notStarted;//i.e, = 0
  Gcost[startX][startY] := 0; //reset starting square's G value to 0

//b. Create a footprint for any nearby unit that the pathfinding unit
//may be about to collide with. Such nodes are designated as tempUnwalkable.
	CreateFootPrints(pathfinderID{unit.unit});

  //4.Add the starting location to the open list of squares to be checked.
  numberOfOpenListItems := 1;
  //assign it as the top (and currently only) item in the open list,
  // which is maintained as a binary heap (explained below)
  openList[1] := 1;
  openX[1] := startX ;
  openY[1] := startY;
  LostinaLoop:=True;
  //5.Do the following until a path is found or deemed nonexistent.
  while (LostinaLoop)//Do until path is found or deemed nonexistent
  do
  begin
    Application.ProcessMessages;
    // If () then LostinaLoop :=False;
    //6.If the open list is not empty, take the first cell off of the list.
    //This is the lowest F cost cell on the open list.
    if (numberOfOpenListItems <> 0) then
    begin
      //6.7. Pop the first item off the open list.
      parentXval := openX[openList[1]];
      //record cell coordinates of the item
      parentYval := openY[openList[1]];
      //add the item to the closed list
      whichList[parentXval][parentYval] := onClosedList;
      //Open List = Binary Heap: Delete this item from the open list, which
      //is maintained as a binary heap.
      //For more information on binary heaps, see:
      //http://www.policyalmanac.org/games/binaryHeaps.htm
      //reduce number of open list items by 1
      numberOfOpenListItems := numberOfOpenListItems - 1;

      //Delete the top item in binary heap and reorder the heap,
      //with the lowest F cost item rising to the top.
      //move the last item in the heap up to slot #1
      openList[1] := openList[numberOfOpenListItems+1];
      v := 1;

      //Repeat the following until the new item in slot #1
      //sinks to its proper spot in the heap.
      while LostinaLoop//(not KeyDown(27))//reorder the binary heap
      do
      begin
        Application.ProcessMessages; //Allow user to Cancel
	u := v;
        //if both children exist
	if (2*u+1 <= numberOfOpenListItems)then
        begin
          //Check if the F cost of the parent is greater than each child.
          //Select the lowest of the two children.
          if (Fcost[openList[u]] >= Fcost[openList[2*u]])then v := 2*u;
          if (Fcost[openList[v]] >= Fcost[openList[2*u+1]])then v := 2*u+1;
	end
	else
	begin //if only child #1 exists
          if (2*u <= numberOfOpenListItems)then
	  begin
            //Check if the F cost of the parent is greater than child #1
            if (Fcost[openList[u]] >= Fcost[openList[2*u]])then
              v := 2*u;
          end;
        end;
              //!=
	if (u <> v)then //if parent's F is > one of its children, swap them
	begin
          temp := openList[u];
          openList[u] := openList[v];
          openList[v] := temp;
        end else break; //otherwise, exit loop
      end;  //while LostinaLoop

      //7.Check the adjacent squares. (Its "children" -- these path children
      //are similar, conceptually, to the binary heap children mentioned
      //above, but don't confuse them. They are different. Path children
      //are portrayed in Demo 1 with grey pointers pointing toward
      //their parents.) Add these adjacent child squares to the open list
      //for later consideration if appropriate (see various if statements
      //below).
      for b := parentYval-1 to parentYval+1 do
      begin
	for a := parentXval-1 to parentXval+1do
        begin
          //If not off the map
          //(do this first to avoid array out-of-bounds errors)
          if (    (a <> -1) and (b <> -1)
              and (a <> mapWidth) and (b <> mapHeight) )then
          begin
            //If not already on the closed list (items on the closed list have
            //already been considered and can now be ignored).
            if (whichList[a][b] <> onClosedList)then
            begin
              //If not a wall/obstacle square.
              if (walkability [a][b] <> unwalkable) then
              begin
        	//If not an adjacent node that is temporarily unwalkable
	        //as defined by CreateFootprints()
                If tempUnwalkability[a,b] <> tempUnwalkable then
                  node := unwalkable;
                //If not occupied by a stopped unit
                If claimedNode[a,b] = 0{Null} then node := walkable
                  Else If claimedNode[a,b]{\pathStatus} <>tempStopped{ stopped }then
                  node := walkable;//{End If
               If node = walkable then
               begin
                 //Don't cut across corners
                 corner := walkable;
                 if (a = parentXval-1)then
                 begin
                  if (b = parentYval-1)then
                  begin
                    if ( (walkability[parentXval-1][parentYval] = unwalkable)
                      or (walkability[parentXval][parentYval-1] = unwalkable))
                        then corner := unwalkable;
                  end
                  else if (b = parentYval+1)then
		  begin
                    if ((walkability[parentXval][parentYval+1] = unwalkable)
                     or (walkability[parentXval-1][parentYval] = unwalkable))
                      then corner := unwalkable;
	          end;
                end
                else if (a = parentXval+1)then
                begin
                  if (b = parentYval-1)then
                  begin
                    if ((walkability[parentXval][parentYval-1] = unwalkable)
                     or (walkability[parentXval+1][parentYval] = unwalkable))
                    then corner:= unwalkable;
                  end
                  else if (b = parentYval+1)then
                  begin
                    if ((walkability[parentXval+1][parentYval] = unwalkable)
                     or (walkability[parentXval][parentYval+1] = unwalkable))
                    then corner := unwalkable;
                  end;
                end;
                if (corner = walkable)then
                begin
                  //If not already on the open list, add it to the open list.
                  if (whichList[a][b] <> onOpenList)then
                  begin
                    //Create a new open list item in the binary heap.
                    //each new item has a unique ID #
                    newOpenListItemID := newOpenListItemID + 1;
                    m := numberOfOpenListItems+1;
                    //place the new open list item
                    //(actually, its ID#) at the bottom of the heap
                    openList[m] := newOpenListItemID;
                    //record the x and y coordinates of the new item
                    openX[newOpenListItemID] := a;
                    openY[newOpenListItemID] := b;

                    //Figure out its G cost
                    if ((abs(a-parentXval) = 1) and (abs(b-parentYval) = 1))
                      //cost of going to diagonal squares
                      then addedGCost := 14
                      //cost of going to non-diagonal squares
                      else addedGCost := 10;
                    Gcost[a][b] := Gcost[parentXval][parentYval]
                       +Trunc((GCostData[a,b] *ProjectRecord.TerrainValueAlphaD))
                       + addedGCost;

		//If the node lies along the path of a nearby unit, add a penalty G cost.
		If nearByPath[a,b] = penalized then
                Gcost[a,b] := Gcost[a,b]+Trunc(20*ProjectRecord.AdjacentPathPenaltyD)
		Else If ((a<>parentXval) And (b<>parentYval)) then
			If ((nearByPath[a,parentYval] = penalized) or
				(nearByPath[parentXval,b] = penalized)) then
				 Gcost[a,b] := Gcost[a,b]+Trunc(28*ProjectRecord.AdjacentPathPenaltyD);


                    //Figure out its H and F costs and parent
                    If useDijkstras then Hcost[openList[m]] :=Trunc(ProjectRecord.AverageTerrainD){0} else
                    //Hcost[openList[m]] := 10*(abs(a - targetX) + abs(b - targetY));
                    Case UnitRecordArray[pathfinderID].SearchMode of
                    0:Hcost[openList[m]] :=Trunc(ProjectRecord.AverageTerrainD);//0; //Dijkstra
                    1://Diagonal
                      Begin
                        XDistance:=abs(a - targetX);
                        YDistance:=abs(b - targetY);
                        If XDistance > YDistance then
                          HDistance:=( (14*YDistance) + (10*(XDistance-YDistance)))
                          else HDistance:=( (14*XDistance) + (10*(YDistance-XDistance)));
                        Hcost[openList[m]] :=Trunc(HDistance+ProjectRecord.AverageTerrainD);
                      End;
                    2:  //Manhattan
                    Hcost[openList[m]] := Trunc(ProjectRecord.AverageTerrainD+
                                   (10*(abs(a - targetX) + abs(b - targetY))));
                    3: //BestFirst..Overestimated h
                    Hcost[openList[m]] := Trunc(ProjectRecord.AverageTerrainD+
                                   (10*((a - targetX)*(a - targetX)
                                     + (b - targetY)*(b - targetY))));
                    4:
                    Hcost[openList[m]] := Trunc(ProjectRecord.AverageTerrainD+
                                   (10*SQRT(((a - targetX)*(a - targetX)
                                     + (b - targetY)*(b - targetY)))));

                    End;
                    //record the F cost of the new square
                    //Fcost[openList[m]] := Gcost[a][b] + Hcost[openList[m]];

                    Case UnitRecordArray[pathfinderID].TieBreakerMode of
                    0:Fcost[openList[m]] := Gcost[a][b] + Hcost[openList[m]];
                    1:begin //straight
                         dx1:=(a - targetX);
                         dx2:=(b - targetY);
                         dy1:=(UnitRecordArray[pathfinderID].startXLoc-UnitRecordArray[pathfinderID].targetX);
                         dy2:=(UnitRecordArray[pathfinderID].startYLoc-UnitRecordArray[pathfinderID].targetY );
                         cross:=abs((dx1*dy2) - (dx2*dy1));
                                              //Round or Trunc ?                          //*
                         Fcost[openList[m]] := Trunc(Gcost[a][b] + Hcost[openList[m]]+(cross/0.001));
                      end;
                    2:Fcost[openList[m]] := Trunc(Gcost[a][b] + Hcost[openList[m]]-(0.1*Hcost[openList[m]])); //close
                    3:Fcost[openList[m]] := Trunc(Gcost[a][b] + Hcost[openList[m]]+(0.1*Hcost[openList[m]])); //far
                    end;


                    //record the parent of the new square
                    parentX[a][b] := parentXval ;
                    parentY[a][b] := parentYval;

                    //Move the new open list item to the proper place
                    //in the binary heap. Starting at the bottom,
                    //successively compare to parent items,
                    //swapping as needed until
                    //the item finds its place in the heap
                    //or bubbles all the way to the top
                    //(if it has the lowest F cost).
                    //While item hasn't bubbled to the top (m=1)
                    while (m <> 1)do
                    begin
                      //Check if child's F cost is < parent's F cost.
                      //If so, swap them.
                      if (Fcost[openList[m]] <= Fcost[openList[m div 2]])then
                      begin
                        temp := openList[m div 2];//[m/2];
                        openList[m div 2]{[m/2]} := openList[m];
                        openList[m] := temp;
                        m := m div 2;//m/2;
                      end else break;
                    end;
                    //add one to the number of items in the heap
                    numberOfOpenListItems := numberOfOpenListItems+1;

                    //Change whichList to show
                    //that the new item is on the open list.
                    whichList[a][b] := onOpenList;
                  end

                  //8.If adjacent cell is already on the open list,
                  //check to see if this path to that cell
                  //from the starting location is a better one.
                  //If so, change the parent of the cell and its G and F costs
                  else //If whichList(a,b) = onOpenList
                  begin
                    //Figure out the G cost of this possible new path
                    if ((abs(a-parentXval) = 1) and (abs(b-parentYval) = 1))
                      //cost of going to diagonal tiles
                      then addedGCost := 14
                      //cost of going to non-diagonal tiles
                      else addedGCost := 10;
                    //tempGcost := Gcost[parentXval][parentYval] + addedGCost;
                    tempGcost := Gcost[parentXval][parentYval]+Trunc(GCostData[a,b]*ProjectRecord.TerrainValueAlphaD) + addedGCost;

		//;If the node lies along the path of a nearby unit,
                //add a penalty G cost.
		If nearByPath[a,b] = penalized then
			tempGcost  := tempGcost+Trunc(20*ProjectRecord.AdjacentPathPenaltyD)
		Else If ((a<>parentXval) And (b<>parentYval))then
                begin
			If ((nearByPath[a,parentYval] = penalized) or
				 (nearByPath[parentXval,b] = penalized))then
					tempGcost  := tempGcost+Trunc(28*ProjectRecord.AdjacentPathPenaltyD);
		End;
                    //If this path is shorter (G cost is lower) then change
                    //the parent cell, G cost and F cost.
                    if (tempGcost < Gcost[a][b])then //if G cost is less,
                    begin  //change the square's parent
                      parentX[a][b] := parentXval;
                      parentY[a][b] := parentYval;
                      Gcost[a][b] := tempGcost;//change the G cost
                      //Because changing the G cost also changes the F cost,
                      //if the item is on the open list we need to change
                      //the item's recorded F cost
                      //and its position on the open list to make
                      //sure that we maintain a properly ordered open list.
                      //look for the item in the heap
                      for x := 1 to numberOfOpenListItems do
                      begin  //item found
                        if (  (openX[openList[x]] = a)
                          and (openY[openList[x]] = b))then
                        begin   //change the F cost
                          Fcost[openList[x]] :=
                                    Gcost[a][b] + Hcost[openList[x]];
                          //See if changing the F score bubbles the item up
                          // from it's current location in the heap
                          m := x;
                          //While item hasn't bubbled to the top (m=1)
                          while (m <> 1)do
                          begin
                            //Check if child is < parent. If so, swap them.
                            if (Fcost[openList[m]] < Fcost[openList[m div 2]])
                            then
                            begin
                              temp := openList[m div 2]; //
                              openList[m div 2] := openList[m]; //m/2
                              openList[m] := temp;
                              m := m div 2;
                            end else break;
                          end;
                          break; //exit for x = loop
                        end; //If openX(openList(x)) = a
                      end; //For x = 1 To numberOfOpenListItems
                    end;//If tempGcost < Gcost(a,b)
                  end;//else If whichList(a,b) = onOpenList
                end;//If not cutting a corner
{	End If ;If not already on the open list
	End If ;If corner = walkable}
	End;// If ;If not occupied by a stopped unit
//	End;// If ;If not an adjacent, temporarily unwalkable node
{	End If ;If not a wall/obstacle cell.
	End If ;If not already on the closed list
	End If ;If not off the map.}
              end;//If not a wall/obstacle square.
            end;//If not already on the closed list
          end;//If not off the map
        end;//for (a = parentXval-1; a <= parentXval+1; a++){
      end;//for (b = parentYval-1; b <= parentYval+1; b++){
    end//if (numberOfOpenListItems != 0)

    //9.If open list is empty then there is no path.
    else
    begin
      //pathStatus[pathfinderID] := nonexistent;
      UnitRecordArray[pathfinderID].pathStatus  := nonexistent;
      break;
    end;

    //If target is added to open list then path has been found.
    //See 10 below
(*    if (whichList[targetX][targetY] = onOpenList)then
    begin
      //pathStatus[pathfinderID] := found;
      UnitRecordArray[pathfinderID].pathStatus := found;
      break;
    end;
  end; *)

//10.Check to see if desired target has been found.
If mode = normal then// ;exit when target is added to open list.
begin
If whichList[targetX,targetY] = onOpenList Then
 begin
    UnitRecordArray[pathfinderID].pathStatus := found;
    break;//: Exit
 end;
end Else
If mode = randomMove then
If Gcost[parentXval,parentYVal] > (20 + Random(20))then
begin
targetX := parentXval;// :
targetY := parentYval;
pathStatus[pathfinderID] := found;// : Exit
break;
end;
end;
{End If
End If}
  //10.Save the path if it exists.
  if (UnitRecordArray[pathfinderID].pathStatus = found)then
  begin
    //a.Working backwards from the target to the starting location by checking
    //each cell's parent, figure out the length of the path.
    pathX := targetX;
    pathY := targetY;
    while ((pathX <> startX) or (pathY <> startY))do
    begin
      //Look up the parent of the current cell.
      tempx := parentX[pathX][pathY];
      pathY := parentY[pathX][pathY];
      pathX := tempx;
      //Figure out the path length
      //pathLength[pathfinderID] := pathLength[pathfinderID] + 1;
      UnitRecordArray[pathfinderID].pathLength := UnitRecordArray[pathfinderID].pathLength + 1;
    end;

{;b. Resize the data bank to the right size (leave room to store step 0,
;which requires storing one more step than the length)
ResizeBank unit\pathBank,(unit\pathLength+1)*4}
    //b.Resize the data bank to the right size in bytes
    {pathBank[pathfinderID] :=
    (int* ) realloc (pathBank[pathfinderID],
		pathLength[pathfinderID]*8);}           //8 ? 4 byte =int ?  +1
    //setlength(pathBank[pathfinderID],(pathLength[pathfinderID])*2);
    setlength(UnitpathBank[UnitRecordArray[pathfinderID].CurrentpathBank,pathfinderID],(UnitRecordArray[pathfinderID].pathLength)*2);

    //c. Now copy the path information over to the databank. Since we are
    //working backwards from the target to the start location, we copy
    //the information to the data bank in reverse order. The result is
    //a properly ordered set of path data, from the first step to the last.
    pathX := targetX;
    pathY := targetY;
    //cellPosition := pathLength[pathfinderID]*2;//start at the end
    cellPosition := ((UnitRecordArray[pathfinderID].pathLength)*2);//start at the end

    while ((pathX <> startX) or (pathY <> startY))do
    begin
      cellPosition := cellPosition - 2;//work backwards 2 integers
      //error C messing with stuffing integers into a byte array type of thing
      //pathBank[pathfinderID] [cellPosition] := pathX;
      //pathBank[pathfinderID] [cellPosition+1] := pathY;
      UnitpathBank[UnitRecordArray[pathfinderID].CurrentpathBank][pathfinderID][cellPosition] := pathX;
      UnitpathBank[UnitRecordArray[pathfinderID].CurrentpathBank][pathfinderID][cellPosition+1] := pathY;

      //d.Look up the parent of the current cell.
      tempx := parentX[pathX][pathY];
      pathY := parentY[pathX][pathY];
      pathX := tempx;

      //e.If we have reached the starting square, exit the loop.
    end;
    //11.Read the first path step into xPath/yPath arrays
    //Change C..Blitz ?
    //BlitzReadPath(pathfinderID,startingX,startingY,1);
    UnitRecordArray[pathfinderID].yPath :=
    ProjectRecord.tileSize* UnitpathBank[UnitRecordArray[pathfinderID].CurrentpathBank][pathfinderID] [UnitRecordArray[pathfinderID].pathLocation*2-1];
    UnitRecordArray[pathfinderID].xPath :=
    ProjectRecord.tileSize*UnitpathBank[UnitRecordArray[pathfinderID].CurrentpathBank][pathfinderID] [UnitRecordArray[pathfinderID].pathLocation*2-2];

  end;
  result:= UnitRecordArray[pathfinderID].pathStatus;
End;




end.
