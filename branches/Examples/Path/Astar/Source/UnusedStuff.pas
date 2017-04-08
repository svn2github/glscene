unit UnusedStuff;

interface

  {
  650x500
800x600
640x640
1280x1280
2560x2560
256x256
512x512
1024x1024
2048x2048
4096x4096

32x32
16x16
8x8
4x4
2x2
1x1

  Dijkstra
   Diagonal
   Manhattan
   BestFirst
   Euclidean

   None
   Straight
   Close
   Far }
   
procedure MoveChaser(ID:Integer);
procedure MoveSmiley(ID:Integer);
procedure MoveSprite(ID:Integer);

implementation

//-----------------------------------------------------------------------------
// Name: MoveSmiley
// Desc: This subroutine moves the smiley around on the screen. In
//	this case the findPath function is accessed via mouse clicks.
//-----------------------------------------------------------------------------
procedure MoveSmiley(ID:Integer);
//int ID = 1; //ID of smiley sprite
Begin
  //1.Find Path: If smiley is active, any left or right click
  //on the map will find a path and make him go there.
(*
	if (MouseHit(1) || MouseHit(2))
	{
		//Call the findPath function.
		int time1 = timeGetTime();
		pathStatus[ID] = FindPath(ID,xLoc[ID],yLoc[ID],MouseX(),MouseY());
		int time2 = timeGetTime();
		searchTime = time2-time1;
	}

//2.Move smiley.
	if (pathStatus[ID] == found) MoveSprite(ID);
*)
End;
{procedure TAStarForm.PaintAStarBase;
Begin
// Come Get Some
//Hail to the King, Baby
Retrieves a specified image as a bitmap.
procedure GetBitmap(Index: Integer; Image: TBitmap);
Description
Use the GetBitmap method to obtain a particular image
in the image list as a bitmap object.
Index denotes the index of the image.
The bitmap is returned as the Image parameter.
var
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  Bitmap.Assign()
    Bitmap.Free;
  end;
End;}
//-----------------------------------------------------------------------------
// Name: MoveChaser
// Desc: This subroutine moves the chasers/ghosts around on the screen.
//In this case the findPath function is accessed automatically when
//a chaser reaches the end of his current path.
//The path info is also updated occasionally.
//-----------------------------------------------------------------------------
procedure MoveChaser(ID:Integer);
Begin
(*
  //int targetID = 1; //ID of target (the smiley)
  //1. Find Path: If smiley and chaser are not at the same location on the
  //screen and no path is currently active, find a new path.
      startXLocArray[ActiveUnitNumber]:=AStarx;
      startYLocArray[ActiveUnitNumber]:=AStary;
  if ( (xLoc[ID] <> xLoc[targetID])
    or (yLoc[ID] <> yLoc[targetID]))then
  begin
    //If no path has been generated, generate one. Update it when
    //the chaser reaches its fifth step on the current path.
    if (pathStatus[ID] = notStarted)
      or(pathLocation[ID] = 5))then
    begin
      //Generate a new path.
      //Enter coordinates of smiley sprite
      //(xLoc(1)/yLoc(1)) as the target.
      pathStatus[ID] = FindPath(ID,xLoc[ID],yLoc[ID],
			xLoc[targetID],yLoc[targetID]);
    end;
  end;
  //2.Move chaser.
  if (pathStatus[ID] = found) then MoveSprite(ID);
*)
end;

//-----------------------------------------------------------------------------
// Name: MoveSprite
// Desc: Moves the sprites around on the screen.
//-----------------------------------------------------------------------------
procedure MoveSprite(ID:Integer);
Begin
  //1.Read path information
{	ReadPath(ID,xLoc[ID],yLoc[ID],speed[ID]);}
//2.Move sprite. xLoc/yLoc = current location of sprite. xPath and
//	yPath = coordinates of next step on the path that were/are
//	read using the readPath function.
{	if (xLoc[ID] > xPath[ID])then xLoc[ID] := xLoc[ID] - speed[ID];
	if (xLoc[ID] < xPath[ID])then xLoc[ID] := xLoc[ID] + speed[ID];
	if (yLoc[ID] > yPath[ID])then yLoc[ID] := yLoc[ID] - speed[ID];
	if (yLoc[ID] < yPath[ID])then yLoc[ID] := yLoc[ID] + speed[ID];}
//3.When sprite reaches the end location square	(end of its current path) ...
 //	if (pathLocation[ID] = pathLength[ID])then
        begin
//		Center the chaser in the square (not really necessary, but
//		it looks a little better for the chaser, which moves in 3 pixel
//		increments and thus isn't always centered when it reaches its
//		target).
{		if (abs(xLoc[ID] - xPath[ID]) < speed[ID]) xLoc[ID] = xPath[ID];
		if (abs(yLoc[ID] - yPath[ID]) < speed[ID]) yLoc[ID] = yPath[ID];}
	end;
End;



(*

;This subroutine copies the map image into one large image for
;faster rendering. This isn't really necessary, and it doesn't effect
;pathfinding at all. It just makes map drawing faster because drawing
;one big image is a lot faster than separately drawing each of the wall
;images and grids.
Function CopyMapImage()
	FreeImage gMap
	gMap = CreateImage(800,600) ;create a new map image.
	SetBuffer ImageBuffer(gMap)
	For x = 0 To 800/tileSize-1
	For y = 0 To 600/tileSize-1
		If walkability(x,y) = unwalkable Then DrawBlock wallBlock,x*tileSize,y*tileSize
		DrawImage grid,x*tileSize,y*tileSize
	Next
	Next
	SetBuffer BackBuffer()
End Function

;This function draws the blue walls and grid.
Function DrawMapImage()
	If gGameStarted = False
		Cls
		For x = 0 To 800/tileSize-1
		For y = 0 To 600/tileSize-1
			If walkability(x,y) = unwalkable Then DrawBlock wallBlock,x*tileSize,y*tileSize
			DrawImage grid,x*tileSize,y*tileSize
		Next
		Next
	Else
		DrawBlock gMap,0,0
	End If
End Function
*)

end.
