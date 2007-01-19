// GLHeightTileFileHDS
{: HeightDataSource for the HTF (HeightTileFile) format.<p>

	<b>History : </b><font size=-1><ul>
      <li>19/01/04 - LIN -Bug fix/workaround: Added 'Inverted' property to GLHeightTileFieHDS
                          Set Inverted to false, if you DONT want your rendered
                          terrain to be a mirror image of your height data.
                          (Defaults to true, so it doesnt affect existing apps);
      <li>29/01/03 - EG - Creation
	</ul></font>
}
unit GLHeightTileFileHDS;

interface

uses Classes, GLHeightData, HeightTileFile;

type

	// TGLHeightTileFileHDS
	//
   {: An Height Data Source for the HTF format.<p> }
	TGLHeightTileFileHDS = class (THeightDataSource)
	   private
	      { Private Declarations }
         FInfiniteWrap : Boolean;
         FInverted     : Boolean;
         FHTFFileName : String;
         FHTF : THeightTileFile;
         FMinElevation : Integer;

	   protected
	      { Protected Declarations }
         procedure SetHTFFileName(const val : String);
         procedure SetInfiniteWrap(val : Boolean);
         procedure SetInverted(val : Boolean);
         procedure SetMinElevation(val : Integer);

         procedure StartPreparingData(heightData : THeightData); override;

	   public
	      { Public Declarations }
	      constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

	   published
	      { Published Declarations }

         {: FileName of the HTF file.<p>
            Note that it is accessed via the services of ApplicationFileIO,
            so this may not necessarily be a regular file on a disk... }
         property HTFFileName : String read FHTFFileName write SetHTFFileName;
         {: If true the height field is wrapped indefinetely. }
         property InfiniteWrap : Boolean read FInfiniteWrap write SetInfiniteWrap default True;
         {: If true the height data is inverted.(Top to bottom) }
         property Inverted : Boolean read FInverted write SetInverted default True;
         {: Minimum elevation of the tiles that are considered to exist.<p>
            This property can typically be used to hide underwater tiles. }
         property MinElevation : Integer read FMinElevation write SetMinElevation default -32768;

         property MaxPoolSize;
         property DefaultHeight;
         //property HeightTileFile :THeightTileFile read FHTF;   //Used for inverting the terrain
	end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils;

// ------------------
// ------------------ TGLHeightTileFileHDS ------------------
// ------------------

// Create
//
constructor TGLHeightTileFileHDS.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
   FInfiniteWrap:=True;
   FInverted:=True;
   FMinElevation:=-32768;
end;

// Destroy
//
destructor TGLHeightTileFileHDS.Destroy;
begin
   FHTF.Free;
	inherited Destroy;
end;

// SetHTFFileName
//
procedure TGLHeightTileFileHDS.SetHTFFileName(const val : String);
begin
   if FHTFFileName<>val then begin
      MarkDirty;
      FreeAndNil(FHTF);
      FHTFFileName:=val;
   end;
end;

// SetInfiniteWrap
//
procedure TGLHeightTileFileHDS.SetInfiniteWrap(val : Boolean);
begin
  if FInfiniteWrap=val then exit;
  FInfiniteWrap:=val;
  MarkDirty;
end;

// SetInfiniteWrap
//
procedure TGLHeightTileFileHDS.SetInverted(val : Boolean);
begin
  if FInverted=Val then exit;
  FInverted:=val;
  MarkDirty;
end;


// SetMinElevation
//
procedure TGLHeightTileFileHDS.SetMinElevation(val : Integer);
begin
   if FMinElevation<>val then begin
      FMinElevation:=val;
      MarkDirty;
   end;
end;

// StartPreparingData
//
procedure TGLHeightTileFileHDS.StartPreparingData(heightData : THeightData);
var
   oldType : THeightDataType;
   htfTile : PHeightTile;
   htfTileInfo : PHeightTileInfo;
   x, y : Integer;
   YPos:integer;
   //YInx,XInx:integer;
   //Yinx,Yinv:integer;
   inY,outY:integer;
   Yinx:integer;


   PLineIn, PLineOut : ^PSmallIntArray;

   LineDataSize:integer;
begin
   // access htf data
   if not Assigned(FHTF) then begin
      if FHTFFileName='' then begin
         heightData.DataState:=hdsNone;
         Exit;
      end;
      FHTF:=THeightTileFile.Create(FHTFFileName);
      Assert(FHTF.TileSize=heightData.Size,
             'HTF TileSize and HeightData size don''t match.('+IntToStr(FHTF.TileSize)+' , '+Inttostr(heightData.Size)+')');
   end;

   // retrieve data and place it in the heightData
   with heightData do begin
      if Inverted then YPos:=YTop
                  else YPos:=-YTop;
      if InfiniteWrap then begin
         x:=XLeft mod FHTF.SizeX;
         if x<0 then x:=x+FHTF.SizeX;
         y:=YPos mod FHTF.SizeY;
         if y<0 then y:=y+FHTF.SizeY;
         htfTile:=FHTF.GetTile(x, y, @htfTileInfo);
      end else begin
         htfTile:=FHTF.GetTile(XLeft, YPos, @htfTileInfo);
      end;
      if (htfTile=nil) or (htfTileInfo.max<=FMinElevation) then begin
         // non-aligned tiles aren't handled (would be slow anyway)
         DataState:=hdsNone;
      end else begin
         oldType:=DataType;
         Allocate(hdtSmallInt);

         if Inverted then Move(htfTile.data[0], SmallIntData^, DataSize)
         else begin // invert the terrain (top to bottom) To compensate for the inverted terrain renderer
           LineDataSize:=DataSize div size;
           for y:=0 to size-1 do begin
             inY:=y*HeightData.Size;
             outY:=((size-1)-y)*HeightData.Size;
             PLineIn :=@htfTile.data[inY];
             PLineOut:=@heightData.SmallIntData[outY];
             Move(PLineIn^,PLineOut^,LineDataSize);
           end;
         end;
         //Move(htfTile.data[0], SmallIntData^, DataSize);

         if oldType<>hdtSmallInt then
            DataType:=oldType;
         DataState:=hdsReady;
         HeightMin:=htfTileInfo.min;
         HeightMax:=htfTileInfo.max;
      end;
   end;







end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

	// class registrations
   Classes.RegisterClasses([TGLHeightTileFileHDS]);

end.
