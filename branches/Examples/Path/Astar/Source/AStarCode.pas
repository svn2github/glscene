unit AStarCode;
//Degenerated from:
//===================================================================
//A* Pathfinder (Version 1.71a) by Patrick Lester. Used by permission.
//===================================================================
//http://www.policyalmanac.org/games/aStarTutorial.htm
//A* Pathfinding for Beginners
interface

uses AStarGlobals,//All data and Init destroy
     forms;//application.ProcessMessages;

Function ReadPathX( pathfinderID, pathLocation:Integer):Integer;
Function ReadPathY( pathfinderID, pathLocation:Integer):Integer;
Procedure ReadPath( pathfinderID, currentX, currentY,
                    pixelsPerFrame:Integer);
Function FindPath ( pathfinderID, startingX,  startingY,
                                  targetX,  targetY:Integer):Integer;

implementation
 

//==========================================================
//READ PATH DATA: These functions read the path data and convert
//it to screen pixel coordinates.
//The following two functions read the raw path data from the pathBank.
//You can call these functions directly and skip the readPath function
//above if you want. Make sure you know what your current pathLocation is.
//---------------------------------------------------------------------------
// Name: ReadPathX
// Desc: Reads the x coordinate of the next path step
//---------------------------------------------------------------------------
Function ReadPathX( pathfinderID, pathLocation:Integer):Integer;
var x:Integer;
Begin   x :=0;
  if (pathLocation <= pathLength[pathfinderID]) then
  begin   //Read coordinate from bank
    x := pathBank[pathfinderID,((pathLocation*2)-2)];
//Adjust the coordinates so they align with the center
//of the path square (optional). This assumes that you are using
//sprites that are centered -- i.e., with the midHandle command.
//Otherwise you will want to adjust this.
                      //and make everything Doubles !!!
    x := (tileSize*x) ;//+ (0.5*tileSize);
  end;
  Result:=x;
end;


//---------------------------------------------------------------------------
// Name: ReadPathY
// Desc: Reads the y coordinate of the next path step
//---------------------------------------------------------------------------
Function ReadPathY( pathfinderID, pathLocation:Integer):Integer;
var y:Integer;
Begin   y :=0;
  if (pathLocation <= pathLength[pathfinderID])then
  begin
    //Read coordinate from bank
    y := pathBank[pathfinderID] [pathLocation*2-1];
    //Adjust the coordinates so they align with the center
    //of the path square (optional). This assumes that you are using
    //sprites that are centered -- i.e., with the midHandle command.
    //Otherwise you will want to adjust this.
    y := tileSize*y ;//+ .5*tileSize;
  end;
  Result:=y;
end;


Procedure ReadPath( pathfinderID, currentX, currentY,
                    pixelsPerFrame:Integer);
//Note on PixelsPerFrame: The need for this parameter probably isn't
//that obvious, so a little explanation is in order. This
//parameter is used to determine if the pathfinder has gotten close
//enough to the center of a given path square to warrant looking up
//the next step on the path.
//This is needed because the speed of certain sprites can
//make reaching the exact center of a path square impossible.
//In Demo #2, the chaser has a velocity of 3 pixels per frame. Our
//tile size is 50 pixels, so the center of a tile will be at location
//25, 75, 125, etc. Some of these are not evenly divisible by 3, so
//our pathfinder has to know how close is close enough to the center.
//It calculates this by seeing if the pathfinder is less than
//pixelsPerFrame # of pixels from the center of the square.
//
//This could conceivably cause problems if you have a *really* fast
//sprite and/or really small tiles, in which case you may need to
//adjust the formula a bit. But this should almost never be a problem
//for games with standard sized tiles and normal speeds. Our smiley
//in Demo #4 moves at a pretty fast clip and it isn't even close
//to being a problem.
var ID:Integer;
Begin
  ID := pathfinderID; //redundant, but makes the following easier to read
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
    xPath[ID] := ReadPathX(ID,pathLocation[ID]);
    yPath[ID] := ReadPathY(ID,pathLocation[ID]);
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
  end;
End;




//---------------------------------------------------------------------------
// Name: FindPath
// Desc: Finds a path using A*
//---------------------------------------------------------------------------
Function FindPath ( pathfinderID, startingX,  startingY,
                                  targetX,  targetY:Integer):Integer;
var
  startX,startY,x,y,
  onOpenList, parentXval, parentYval,
  a, b, m, u, v, temp, corner, numberOfOpenListItems,
  addedGCost, tempGcost,
  //path,//moved to global
  tempx, pathX, pathY, cellPosition,
  newOpenListItemID:Integer;
  //13.If there is no path to the selected target, set the pathfinder's
  //xPath and yPath equal to its current location
  //and return that the path is nonexistent.
procedure noPath;
begin
  xPath[pathfinderID] := startingX;
  yPath[pathfinderID] := startingY;
  result:= nonexistent;
end;
Begin
  //onOpenList:=0; parentXval:=0; parentYval:=0;
  //a:=0; b:=0; m:=0; u:=0; v:=0;
  //temp:=0;  corner:=0;  numberOfOpenListItems:=0;
  addedGCost:=0; tempGcost := 0;
  //path := 0;
  //  tempx:=0; pathX:=0; pathY:=0;  cellPosition:=0;
  newOpenListItemID:=0;

  //1. Convert location data (in pixels)
  //to coordinates in the walkability array.
  startX := startingX div tileSize;
  startY := startingY div tileSize;
  targetX := targetX div tileSize;
  targetY := targetY div tileSize;

  //2.Quick Path Checks: Under the some circumstances no path needs to
  //	be generated ...

  //If starting location and target are in the same location...
  if ((startX = targetX) and (startY = targetY)
      and (pathLocation[pathfinderID] > 0))
    then  result:= found;
  if ((startX = targetX) and (startY = targetY)
      and (pathLocation[pathfinderID] = 0))
    then  result:= nonexistent;

  //If target square is unwalkable, return that it's a nonexistent path.
  if (walkability[targetX][targetY] = unwalkable)then
    begin noPath;exit;{current procedure} end;//goto noPath;

  //3.Reset some variables that need to be cleared
  if (onClosedList > 1000000)then //reset whichList occasionally
  begin
    for x := 0 to mapWidth-1 do
      for y := 0 to mapHeight-1 do whichList [x][y] := 0;
    onClosedList := 10;
  end;
  //changing the values of onOpenList and onClosed list
  //is faster than redimming whichList() array
  onClosedList := onClosedList+2;
  onOpenList := onClosedList-1;
//	tempUnwalkable := onClosedList-2;
  pathLength [pathfinderID] := notStarted;//i.e, = 0
  pathLocation [pathfinderID] := notStarted;//i.e, = 0
  Gcost[startX][startY] := 0; //reset starting square's G value to 0

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
      //7. Pop the first item off the open list.
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
        Application.ProcessMessages;
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
      end;

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
              and (a <> mapWidth) and (b <> mapHeight))then
          begin



            //If not already on the closed list (items on the closed list have
            //already been considered and can now be ignored).
            if (whichList[a][b] <> onClosedList)then
            begin
              //If not a wall/obstacle square.
              if (walkability [a][b] <> unwalkable) then
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
                      
                    Gcost[a][b] := Gcost[parentXval][parentYval] + addedGCost;

                    //Figure out its H and F costs and parent
                    //If useDijkstras then Hcost[openList[m]] :=0 else
                    Hcost[openList[m]] :=
                                   10*(abs(a - targetX) + abs(b - targetY));
                    Fcost[openList[m]] := Gcost[a][b] + Hcost[openList[m]];
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
                    tempGcost := Gcost[parentXval][parentYval] + addedGCost;

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
              end;//If not a wall/obstacle square.
            end;//If not already on the closed list
          end;//If not off the map
        end;//for (a = parentXval-1; a <= parentXval+1; a++){
      end;//for (b = parentYval-1; b <= parentYval+1; b++){
    end//if (numberOfOpenListItems != 0)

    //9.If open list is empty then there is no path.
    else
    begin   //path
     pathStatus[pathfinderID]  := nonexistent;
      break;
    end;

    //If target is added to open list then path has been found.
    if (whichList[targetX][targetY] = onOpenList)then
    begin   //path
      pathStatus[pathfinderID] := found;
      break;
    end;
  end;

  //10.Save the path if it exists.
  if (pathStatus[pathfinderID] = found)then
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
      pathLength[pathfinderID] := pathLength[pathfinderID] + 1;
    end;

		{;b. Resize the data bank to the right size (leave room to store step 0,
		;which requires storing one more step than the length)
		ResizeBank unit\pathBank,(unit\pathLength+1)*4}
    //b.Resize the data bank to the right size in bytes
    {pathBank[pathfinderID] :=
    (int* ) realloc (pathBank[pathfinderID],
		pathLength[pathfinderID]*8);}           //8 ? 4 byte =int ?  +1
    setlength(pathBank[pathfinderID],(pathLength[pathfinderID])*2);
    //c. Now copy the path information over to the databank. Since we are
    //working backwards from the target to the start location, we copy
    //the information to the data bank in reverse order. The result is
    //a properly ordered set of path data, from the first step to the last.
    pathX := targetX;
    pathY := targetY;
    cellPosition := pathLength[pathfinderID]*2;//start at the end
    while ((pathX <> startX) or (pathY <> startY))do
    begin
      cellPosition := cellPosition - 2;//work backwards 2 integers
      pathBank[pathfinderID] [cellPosition] := pathX;
      pathBank[pathfinderID] [cellPosition+1] := pathY;

      //d.Look up the parent of the current cell.
      tempx := parentX[pathX][pathY];
      pathY := parentY[pathX][pathY];
      pathX := tempx;

      //e.If we have reached the starting square, exit the loop.
    end;
    //11.Read the first path step into xPath/yPath arrays
   // ReadPath(pathfinderID,startingX,startingY,1);
    yPath[pathfinderID] :=tileSize*pathBank[pathfinderID] [pathLocation[pathfinderID]*2-1];
    xPath[pathfinderID] :=tileSize*pathBank[pathfinderID] [pathLocation[pathfinderID]*2-2];
   
  end;
  result:= pathStatus[pathfinderID];
End;



end.
