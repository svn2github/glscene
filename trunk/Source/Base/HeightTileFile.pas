// HeightTileFile
{: Access to large tiled height data files.<p>

   Performance vs Raw file accesses (for perfect tile match):<ul>
   <li>Cached data:<ul>
      <li>"Smooth" terrain   1:2 to 1:10
      <li>Random terrain     1:1
      </ul>
   <li>Non-cached data:<ul>
      <li>"Smooth" terrain   1:100 to 1:1000
      <li>Random terrain     1:100
      </ul>
   </ul><p>

   <b>Historique : </b><font size=-1><ul>
      <li>21/12/01 - Egg - Creation
   </ul></font>
}
unit HeightTileFile;

interface

uses Classes;

type

   TSmallIntArray = array [0..MaxInt shr 2] of SmallInt;
   PSmallIntArray = ^TSmallIntArray;

   TShortIntArray = array [0..MaxInt shr 2] of ShortInt;
   PShortIntArray = ^TShortIntArray;

   // THeightTileInfo
   //
   THeightTileInfo = packed record
      left, top, width, height : Integer;
      min, max, average : SmallInt;
      fileOffset : Integer;   // offset to tile data in the file
   end;
   PHeightTileInfo = ^THeightTileInfo;

   // THeightTile
   //
   THeightTile = packed record
      info : THeightTileInfo;
      data : array of SmallInt;
   end;
   PHeightTile = ^THeightTile;

   // THTFHeader
   //
   THTFHeader = packed record
      FileVersion : array [0..5] of Char;
      TileIndexOffset : Integer;
      SizeX, SizeY : Integer;
      TileSize : Integer;
   end;

   // THeightTileFile
   //
   {: Interfaces a Tiled file }
   THeightTileFile = class (TObject)
      private
         { Private Declarations }
         FFile : TFileStream;
         FHeader : THTFHeader;
         FTileIndex : array of THeightTileInfo;
         FHashTable : array [0..1023] of array of Integer;
         FCreating : Boolean;
         FHeightTile : THeightTile;
         FInBuf : array of ShortInt;

      protected
         { Protected Declarations }
         procedure PackTile(aWidth, aHeight : Integer; src : PSmallIntArray);
         procedure UnPackTile(source : PShortIntArray);

         property TileIndexOffset : Integer read FHeader.TileIndexOffset write FHeader.TileIndexOffset;

      public
         { Public Declarations }
         {: Creates a new HTF file.<p>
            Read and data access methods are not available when creating. }
         constructor CreateNew(const fileName : String;
                               aSizeX, aSizeY, aTileSize : Integer);
         constructor Create(const fileName : String);
         destructor Destroy; override;

         {: Returns tile index for corresponding left/top. }
         function GetTileIndex(aLeft, aTop : Integer) : Integer;
         {: Returns tile of corresponding left/top.<p> }
         function GetTile(aLeft, aTop : Integer) : PHeightTile;

         {: Stores and compresses give tile data.<p>
            aLeft and top MUST be a multiple of TileSize, aWidth and aHeight
            MUST be lower or equal to TileSize. }
         procedure CompressTile(aLeft, aTop, aWidth, aHeight : Integer;
                                aData : PSmallIntArray);

         {: Extract a single row from the HTF file.<p>
            This is NOT the fastest way to access HTF data. All of the row must
            be contained in the world, otherwise result is undefined. }
         procedure ExtractRow(x, y, len : Integer; dest : PSmallIntArray);
         {: Returns the tile that contains x and y. }
         function XYTileInfo(anX, anY : Integer) : PHeightTileInfo;

         property SizeX : Integer read FHeader.SizeX;
         property SizeY : Integer read FHeader.SizeY;
         property TileSize : Integer read FHeader.TileSize;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils, Dialogs;

const
   cFileVersion = 'HTF100';

// ------------------
// ------------------ THeightTileFile ------------------
// ------------------

// CreateNew
//
constructor THeightTileFile.CreateNew(const fileName : String;
                                aSizeX, aSizeY, aTileSize : Integer);
begin
   with FHeader do begin
      FileVersion:=cFileVersion;
      SizeX:=aSizeX;
      SizeY:=aSizeY;
      TileSize:=aTileSize;
   end;
   FFile:=TFileStream.Create(fileName, fmCreate);
   FFile.Write(FHeader, SizeOf(FHeader));
   FCreating:=True;
   SetLength(FHeightTile.data, aTileSize*aTileSize);
end;

// Create
//
constructor THeightTileFile.Create(const fileName : String);
var
   n, i, key : Integer;
begin
   FFile:=TFileStream.Create(fileName, fmOpenRead+fmShareDenyNone);
   // Read Header
   FFile.Read(FHeader, SizeOf(FHeader));
   if FHeader.FileVersion<>cFileVersion then
      raise Exception.Create('Invalid file type');
   // Read TileIndex
   FFile.Position:=TileIndexOffset;
   FFile.Read(n, 4);
   SetLength(FTileIndex, n);
   FFile.Read(FTileIndex[0], SizeOf(THeightTileInfo)*n);
   // Prepare HasTable
   for n:=0 to High(FTileIndex) do begin
      with FTileIndex[n] do begin
         key:=Left+(Top shl 4);
         key:=((key and 1023)+(key shr 10)+(key shr 20)) and 1023;
      end;
      i:=Length(FHashTable[key]);
      SetLength(FHashTable[key], i+1);
      FHashTable[key][i]:=n;
   end;
   FHeightTile.info.left:=MaxInt; // mark as not loaded
   SetLength(FHeightTile.data, TileSize*TileSize);
   SetLength(FInBuf, TileSize*(TileSize+1));
end;

// Destroy
//
destructor THeightTileFile.Destroy;
var
   n : Integer;
begin
   if FCreating then begin
      TileIndexOffset:=FFile.Position;
      // write tile index
      n:=Length(FTileIndex);
      FFile.Write(n, 4);
      FFile.Write(FTileIndex[0], SizeOf(THeightTileInfo)*n);
      // write data size
      FFile.Position:=0;
      FFile.Write(FHeader, SizeOf(FHeader));
   end;
   FFile.Free;
   inherited Destroy;
end;

// PackTile
//
procedure THeightTileFile.PackTile(aWidth, aHeight : Integer; src : PSmallIntArray);

   function DiffEncode(src : PSmallIntArray; dest : PShortIntArray) : Integer;
   var
      i : Integer;
      v, delta : SmallInt;
   begin
      Result:=Integer(dest);
      v:=src[0];
      PSmallIntArray(dest)[0]:=v;
      dest:=PShortIntArray(Integer(dest)+2);
      i:=1;
      while i<aWidth do begin
         delta:=src[i]-v;
         v:=src[i];
         if Abs(delta)<=127 then begin
            dest[0]:=ShortInt(delta);
            dest:=PShortIntArray(Integer(dest)+1);
         end else begin
            dest[0]:=-128;
            dest:=PShortIntArray(Integer(dest)+1);
            PSmallIntArray(dest)[0]:=v;
            dest:=PShortIntArray(Integer(dest)+2);
         end;
         Inc(i);
      end;
      Result:=Integer(dest)-Result;
   end;

   function RLEEncode(src : PSmallIntArray; dest : PChar) : Integer;
   var
      v : SmallInt;
      i, n : Integer;
   begin
      i:=0;
      Result:=Integer(dest);
      while (i<aWidth) do begin
         v:=src[i];
         Inc(i);
         n:=0;
         PSmallIntArray(dest)[0]:=v;
         Inc(dest, 2);
         while (src[i]=v) and (i<aWidth) do begin
            Inc(n);
            if n=255 then begin
               dest[0]:=#255;
               Inc(dest);
               n:=0;
            end;
            Inc(i);
         end;
         if (i<aWidth) or (n>0) then begin
            dest[0]:=Char(n);
            Inc(dest);
         end;
      end;
      Result:=Integer(dest)-Result;
   end;

var
   y : Integer;
   p : PSmallIntArray;
   buf, bestBuf : array of Byte;
   bestLength, len : Integer;
   bestMethod : Byte;   // 0=RAW, 1=Diff, 2=RLE
   av : Int64;
   v : SmallInt;
begin
   SetLength(buf, TileSize*4);     // worst case situation
   SetLength(bestBuf, TileSize*4); // worst case situation

   with FHeightTile.info do begin
      min:=src[0];
      max:=src[0];
      av:=src[0];
      for y:=1 to TileSize*TileSize-1 do begin
         v:=Src[y];
         if v<min then min:=v else if v>max then max:=v;
         average:=average+v;
      end;
      average:=av div (TileSize*TileSize);

      if min=max then Exit; // no need to store anything

   end;

   for y:=0 to TileSize-1 do begin
      p:=@src[TileSize*y];
      // Default encoding = RAW
      bestLength:=TileSize*2;
      bestMethod:=0;
      Move(src[0], bestBuf[0], bestLength);
      // Diff encoding
      len:=DiffEncode(p, PShortIntArray(@buf[0]));
      if len<bestLength then begin
         bestLength:=len;
         bestMethod:=1;
         Move(buf[0], bestBuf[0], bestLength);
      end;
      // RLE encoding
      len:=RLEEncode(p, PChar(@buf[0]));
      if len<bestLength then begin
         bestLength:=len;
         bestMethod:=2;
         Move(buf[0], bestBuf[0], bestLength);
      end;
      // Write to file
      FFile.Write(bestMethod, 1);
      FFile.Write(bestBuf[0], bestLength);
   end;
end;

// UnPackTile
//
procedure THeightTileFile.UnPackTile(source : PShortIntArray);
var
   aWidth : Integer;
   src : PShortInt;
   dest : PSmallInt;

   procedure DiffDecode;
   var
      v : SmallInt;
      delta : SmallInt;
      locSrc : PShortInt;
      destEnd, locDest : PSmallInt;
   begin
      locSrc:=PShortInt(Integer(src)-1);
      locDest:=dest;
      destEnd:=PSmallInt(Integer(dest)+aWidth*2);
      while Integer(locDest)<Integer(destEnd) do begin
         Inc(locSrc);
         v:=PSmallInt(locSrc)^;
         Inc(locSrc, 2);
         locDest^:=v;
         Inc(locDest);
         while (Integer(locDest)<Integer(destEnd)) do begin
            delta:=locSrc^;
            if delta<>-128 then begin
               v:=v+delta;
               Inc(locSrc);
               locDest^:=v;
               Inc(locDest);
            end else Break;
         end;
      end;
      src:=locSrc;
      dest:=locDest;
   end;

   procedure RLEDecode;
   var
      n, j : Integer;
      v : SmallInt;
      locSrc : PShortInt;
      destEnd, locDest : PSmallInt;
   begin
      locSrc:=src;
      locDest:=dest;
      destEnd:=PSmallInt(Integer(dest)+aWidth*2);
      while Integer(locDest)<Integer(destEnd) do begin
         v:=PSmallIntArray(locSrc)[0];
         Inc(locSrc, 2);
         repeat
            if Integer(locDest)=Integer(destEnd)-2 then begin
               locDest^:=v;
               Inc(locDest);
               n:=0;
            end else begin
               n:=Integer(locSrc^ and 255);
               Inc(locSrc);
               for j:=0 to n do begin
                  locDest^:=v;
                  Inc(locDest);
               end;
            end;
         until (n<255) or (Integer(locDest)>=Integer(destEnd));
      end;
      src:=locSrc;
      dest:=locDest;
   end;

var
   y, s2 : Integer;
   method : Byte;
begin
   src:=PShortInt(source);
   dest:=@FHeightTile.Data[0];
   
   with FHeightTile.info do begin
      aWidth:=width;
      if min=max then begin
         for y:=0 to width*height-1 do
            PSmallIntArray(dest)[y]:=min;
         Exit;
      end;
   end;

   s2:=FHeightTile.info.width*2;
   for y:=0 to FHeightTile.info.height-1 do begin
      method:=src^;
      Inc(src);
      case method of
         1 : DiffDecode;
         2 : RLEDecode;
      else
         Move(src^, dest^, s2);
         Inc(src, s2);
         Inc(dest, s2 shr 1);
      end;
   end;
end;

// GetTileIndex
//
function THeightTileFile.GetTileIndex(aLeft, aTop : Integer) : Integer;
var
   i, n, key : Integer;
begin
   Result:=-1;
   key:=aLeft+(aTop shl 4);
   key:=((key and 1023)+(key shr 10)+(key shr 20)) and 1023;
   for i:=0 to High(FHashTable[key]) do begin
      n:=FHashTable[key][i];
      with FTileIndex[n] do begin
         if (left=aLeft) and (top=aTop) then begin
            Result:=n;
            Break;
         end;
      end;
   end;
end;

// GetTile
//
function THeightTileFile.GetTile(aLeft, aTop : Integer) : PHeightTile;
var
   i, n : Integer;
   tileInfo : PHeightTileInfo;
begin
   with FHeightTile.info do
      if (left=aLeft) and (top=aTop) then begin
         Result:=@FHeightTile;
         Exit;
      end;
   i:=GetTileIndex(aLeft, aTop);
   if i>=0 then begin
      tileInfo:=@FTileIndex[i];
      if i<High(FTileIndex) then
         n:=FTileIndex[i+1].fileOffset-tileInfo.fileOffset
      else n:=TileIndexOffset-tileInfo.fileOffset;
      Result:=@FHeightTile;
      FHeightTile.info:=tileInfo^;
      FFile.Position:=tileInfo.fileOffset;
      FFile.Read(FInBuf[0], n);
      UnPackTile(@FInBuf[0]);
   end else Result:=nil;
end;

// CompressTile
//
procedure THeightTileFile.CompressTile(aLeft, aTop, aWidth, aHeight : Integer;
                                       aData : PSmallIntArray);
begin
   with FHeightTile.info do begin
      left:=aLeft;
      top:=aTop;
      width:=aWidth;
      height:=aHeight;
      fileOffset:=FFile.Position;
   end;
   PackTile(aWidth, aHeight, aData);
   SetLength(FTileIndex, Length(FTileIndex)+1);
   FTileIndex[High(FTileIndex)]:=FHeightTile.info
end;

// ExtractRow
//
procedure THeightTileFile.ExtractRow(x, y, len : Integer; dest : PSmallIntArray);
var
   n, rx : Integer;
   tileInfo : PHeightTileInfo;
   tile : PHeightTile;
begin
   while len>0 do begin
      tileInfo:=XYTileInfo(x, y);
      if not Assigned(tileInfo) then Exit;
      rx:=x-tileInfo.left;
      n:=tileInfo.width-rx;
      if n>len then n:=len;
      tile:=GetTile(tileInfo.left, tileInfo.top);
      Move(tile.data[(y-tileInfo.top)*tileInfo.width+rx], dest^, n*2);
      Dec(len, n);
      Inc(x, n);
   end;
end;

// TileInfo
//
function THeightTileFile.XYTileInfo(anX, anY : Integer) : PHeightTileInfo;
var
   i : Integer;
begin
   Result:=nil;
   for i:=Low(FTileIndex) to High(FTileIndex) do with FTileIndex[i] do begin
      if (left<=anX) and (top<=anY) and (anX<left+width) and (anY<top+height) then begin
         Result:=@FTileIndex[i];
         Break;
      end;
   end;
end;

end.

