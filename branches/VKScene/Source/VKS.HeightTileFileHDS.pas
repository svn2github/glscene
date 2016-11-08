//
// VKScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
   HeightDataSource for the HTF (HeightTileFile) format. 
}
unit VKS.HeightTileFileHDS;

interface

{$I VKScene.inc}

uses
  System.Classes, System.SysUtils,
   
  VKS.HeightData, VKS.HeightTileFile;

type

	// TVKHeightTileFileHDS
	//
   { An Height Data Source for the HTF format.  }
	TVKHeightTileFileHDS = class (TVKHeightDataSource)
	   private
	      { Private Declarations }
         FInfiniteWrap : Boolean;
         FInverted     : Boolean;
         FHTFFileName : String;
         FHTF : TVKHeightTileFile;
         FMinElevation : Integer;

	   protected
	      { Protected Declarations }
         procedure SetHTFFileName(const val : String);
         procedure SetInfiniteWrap(val : Boolean);
         procedure SetInverted(val : Boolean);
         procedure SetMinElevation(val : Integer);

	   public
	      { Public Declarations }
	        constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;
         procedure StartPreparingData(HeightData : TVKHeightData); override;
         function Width :integer;    override;
         function Height:integer;    override;
         function OpenHTF:TVKHeightTileFile; //gives you direct access to the HTF object

	   published
	      { Published Declarations }

         { FileName of the HTF file. 
            Note that it is accessed via the services of VKS.ApplicationFileIO,
            so this may not necessarily be a regular file on a disk... }
         property HTFFileName : String read FHTFFileName write SetHTFFileName;
         { If true the height field is wrapped indefinetely. }
         property InfiniteWrap : Boolean read FInfiniteWrap write SetInfiniteWrap default True;
         { If true the height data is inverted.(Top to bottom) }
         property Inverted : Boolean read FInverted write SetInverted default True;
         { Minimum elevation of the tiles that are considered to exist. 
            This property can typically be used to hide underwater tiles. }
         property MinElevation : Integer read FMinElevation write SetMinElevation default -32768;

         property MaxPoolSize;
         property DefaultHeight;
	end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------
// ------------------ TVKHeightTileFileHDS ------------------
// ------------------

// Create
//
constructor TVKHeightTileFileHDS.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
   FInfiniteWrap:=True;
   FInverted:=True;
   FMinElevation:=-32768;
end;

// Destroy
//
destructor TVKHeightTileFileHDS.Destroy;
begin
   FHTF.Free;
	inherited Destroy;
end;

// SetHTFFileName
//
procedure TVKHeightTileFileHDS.SetHTFFileName(const val : String);
begin
   if FHTFFileName<>val then begin
      MarkDirty;
      FreeAndNil(FHTF);
      FHTFFileName:=val;
   end;
end;

// SetInfiniteWrap
//
procedure TVKHeightTileFileHDS.SetInfiniteWrap(val : Boolean);
begin
  if FInfiniteWrap=val then exit;
  FInfiniteWrap:=val;
  MarkDirty;
end;

// SetInverted
//
procedure TVKHeightTileFileHDS.SetInverted(val : Boolean);
begin
  if FInverted=Val then exit;
  FInverted:=val;
  MarkDirty;
end;


// SetMinElevation
//
procedure TVKHeightTileFileHDS.SetMinElevation(val : Integer);
begin
   if FMinElevation<>val then begin
      FMinElevation:=val;
      MarkDirty;
   end;
end;

// OpenHTF
// Tries to open the assigned HeightTileFile.
//
function TVKHeightTileFileHDS.OpenHTF:TVKHeightTileFile;
begin
  if not Assigned(FHTF) then begin
    if FHTFFileName='' then FHTF:=nil
      else FHTF:=TVKHeightTileFile.Create(FHTFFileName);
  end;
  result:=FHTF;
end;

// StartPreparingData
//
procedure TVKHeightTileFileHDS.StartPreparingData(HeightData : TVKHeightData);
var
   oldType : TVKHeightDataType;
   htfTile : PHeightTile;
   htfTileInfo : PHeightTileInfo;
   x, y : Integer;
   YPos:integer;
   inY,outY:integer;
   PLineIn, PLineOut : ^PSmallIntArray;
   LineDataSize:integer;
begin
   // access htf data
   if OpenHTF=nil then begin
     HeightData.DataState:=hdsNone;
     Exit;
   end else Assert(FHTF.TileSize=HeightData.Size,
                   'HTF TileSize and HeightData size don''t match.('+IntToStr(FHTF.TileSize)+' and '+Inttostr(HeightData.Size)+')');
   heightdata.DataState := hdsPreparing;
   // retrieve data and place it in the HeightData
   with HeightData do begin
      if Inverted then YPos:=YTop
                  else YPos:=FHTF.SizeY-YTop-size+1;
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
             PLineOut:=@HeightData.SmallIntData[outY];
             Move(PLineIn^,PLineOut^,LineDataSize);
           end;
         end;
         //---Move(htfTile.data[0], SmallIntData^, DataSize);---
         if oldType<>hdtSmallInt then DataType:=oldType;

         TextureCoordinates(HeightData);
         inherited;
         HeightMin:=htfTileInfo.min;
         HeightMax:=htfTileInfo.max;
      end;
   end;
end;

function TVKHeightTileFileHDS.Width :integer;
begin
  if OpenHTF=nil then result:=0
                 else result:=FHTF.SizeX;
end;

function TVKHeightTileFileHDS.Height:integer;
begin
  if OpenHTF=nil then result:=0
                 else result:=FHTF.SizeY;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

	// class registrations
   RegisterClasses([TVKHeightTileFileHDS]);

end.
