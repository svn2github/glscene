// HeightTileFile
{: Access to large tiled height data files<p>

   **** UNDER CONSTRUCTION *****

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
      seaTile : Boolean;      // true if contains exclusively sea
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

   // THeightTileFile
   //
   {: Interfaces a Tiled file }
   THeightTileFile = class (TObject)
      private
         { Private Declarations }
         FFile : TFileStream;
         FSizeX, FSizeY, FTileSize : Integer;
         FTileIndex : array of THeightTileInfo;
         FFileVersion : String;
         FDataSize : Integer;
         FCreating : Boolean;
         FHeightTile : THeightTile;

      protected
         { Protected Declarations }
         procedure PackTile(aWidth, aHeight : Integer; src : PSmallIntArray);
         procedure UnPackTile(src : PShortIntArray);

      public
         { Public Declarations }
         constructor CreateNew(const fileName : String;
                               aSizeX, aSizeY, aTileSize : Integer);
         constructor Create(const fileName : String);
         destructor Destroy; override;

         {: Returns tile of corresponding left/top.<p>
            aLeft and aTop MUST be a multiple of TileSize. }
         function GetTile(aLeft, aTop : Integer) : PHeightTile;

         {: Stores and compresses give tile data.<p>
            aLeft and top MUST be a multiple of TileSize, aWidth and aHeight
            MUST be lower or equal to TileSize. }
         procedure CompressTile(aLeft, aTop, aWidth, aHeight : Integer;
                                aData : PSmallIntArray);

         property SizeX : Integer read FSizeX;
         property SizeY : Integer read FSizeY;
         property TileSize : Integer read FTileSize;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils;

const
   cFileVersion = 'HTF1.0';
   cHeaderSize = 10;

// ------------------
// ------------------ THeightTileFile ------------------
// ------------------

// CreateNew
//
constructor THeightTileFile.CreateNew(const fileName : String;
                                aSizeX, aSizeY, aTileSize : Integer);
begin
   FTileSize:=aTileSize;
   FSizeX:=aSizeX;
   FSizeY:=aSizeY;
   FFile:=TFileStream.Create(fileName, fmCreate);
   FFile.Write(cFileVersion[1], 6);
   FFileVersion:=cFileVersion;
   FDataSize:=0;
   FFile.Write(FDataSize, 8);
   FCreating:=True;
   SetLength(FHeightTile.data, aTileSize*aTileSize);
end;

// Create
//
constructor THeightTileFile.Create(const fileName : String);
var
   n : Integer;
begin
   FFile:=TFileStream.Create(fileName, fmOpenRead+fmShareDenyNone);
   SetLength(FFileVersion, 6);
   FFile.Read(FFileVersion[1], 6);
   if FFileVersion<>cFileVersion then
      raise Exception.Create('Invalid file type');
   FFile.Read(FDataSize, 8);
   FFile.Seek(FDataSize, soFromCurrent);
   FFile.Read(n, 4);
   SetLength(FTileIndex, n);
   FFile.Read(FTileIndex[0], SizeOf(THeightTileInfo)*n);
end;

// Destroy
//
destructor THeightTileFile.Destroy;
var
   n : Integer;
begin
   if FCreating then begin
      // write tile index
      n:=Length(FTileIndex);
      FFile.Write(n, 4);
      FFile.Write(FTileIndex[0], SizeOf(THeightTileInfo)*n);
      FFile.Seek(6, soFromBeginning);
      // write data size
      FFile.Write(FDataSize, 8);
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
      Result:=2;
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
            Inc(Result);
         end else begin
            dest[0]:=-128;
            dest:=PShortIntArray(Integer(dest)+1);
            PSmallIntArray(dest)[0]:=v;
            dest:=PShortIntArray(Integer(dest)+2);
            Inc(Result, 3);
         end;
         Inc(i);
      end;
   end;

   function RLEEncode(src : PSmallIntArray; dest : PChar) : Integer;
   var
      v : SmallInt;
      i, n : Integer;
   begin
      i:=1;
      n:=0;
      v:=src[0];
      PSmallIntArray(dest)[0]:=v;
      dest:=PChar(Integer(dest)+2);
      Result:=3;
      while (i<aWidth) do begin
         if src[i]=v then begin
            Inc(n);
            if n=255 then begin
               dest[0]:=Char(n);
               dest:=PChar(Integer(dest)+1);
               n:=0;
               Inc(Result);
            end;
         end else begin
            dest[0]:=Char(n);
            dest:=PChar(Integer(dest)+1);
            n:=0;
            PSmallIntArray(dest)[0]:=v;
            dest:=PChar(Integer(dest)+2);
            Inc(Result, 3);
         end;
         Inc(i);
      end;
      dest[0]:=Char(n);
   end;

var
   y : Integer;
   p : PSmallIntArray;
   buf, bestBuf : array of Byte;
   bestLength, len : Integer;
   bestMethod : Byte;   // 0=RAW, 1=Diff, 2=RLE
begin
   SetLength(buf, FTileSize*4);     // worst case situation
   SetLength(bestBuf, FTileSize*2); // worst case situation

   with FHeightTile.info do begin
      seaTile:=False;
   end;

   bestLength:=FTileSize*2;
   Move(src[0], bestBuf[0], bestLength);
   bestMethod:=0;
   for y:=0 to FTileSize-1 do begin
      p:=@src[FTileSize*y];
      // Diff encoding
      len:=DiffEncode(p, PShortIntArray(@buf[0]));
      if len<bestLength then begin
         bestLength:=len;
         bestMethod:=1;
         Move(p[0], bestBuf[0], bestLength);
      end;
      // RLE encoding
      len:=RLEEncode(p, PChar(@buf[0]));
      if len<bestLength then begin
         bestLength:=len;
         bestMethod:=2;
         Move(p[0], bestBuf[0], bestLength);
      end;
      // Write to file
      FFile.Write(bestMethod, 1);
      FFile.Write(bestBuf[0], bestLength);
      Inc(FDataSize, bestLength+1);
   end;
end;

// UnPackTile
//
procedure THeightTileFile.UnPackTile(src : PShortIntArray);
var
   width : Integer;

   procedure DiffDecode(var src : PShortIntArray; dest : PSmallIntArray);
   var
      i, k : Integer;
      v : SmallInt;
      delta : ShortInt;
   begin
      i:=0;
      k:=0;
      while i<width do begin
         v:=PSmallIntArray(@src[k])[0];
         Inc(k, 2);
         dest[i]:=v;
         Inc(i);
         while (i<width) and (src[k]<>-128) do begin
            delta:=src[k];
            Inc(k);
            v:=v+delta;
            dest[i]:=v;
            Inc(i);
         end;
      end;
      src:=@src[k];
   end;

   procedure RLEDecode(var src : PShortIntArray; dest : PSmallIntArray);
   var
      i, k, n, j : Integer;
      v : SmallInt;
   begin
      i:=0;
      k:=0;
      while i<width do begin
         v:=PSmallIntArray(@src[k])[0];
         Inc(k, 2);
         dest[i]:=v;
         Inc(i);
         repeat
            n:=Integer(src[k]);
            Inc(k);
            for j:=1 to n do begin
               dest[i]:=v;
               Inc(i);
            end;
         until n<>255;
      end;
      src:=@src[k];
   end;

var
   y, s2 : Integer;
   method : Byte;
   dest : PSmallIntArray;
begin
   width:=FHeightTile.info.width;
   SetLength(FHeightTile.Data, width*FHeightTile.info.height);
   dest:=@FHeightTile.Data[0];
   s2:=FHeightTile.info.width*2;
   for y:=0 to FHeightTile.info.height-1 do begin
      method:=src[0];
      src:=PShortIntArray(Integer(src)+1);
      case method of
         1 : DiffDecode(src, dest);
         2 : RLEDecode(src, dest);
      else
         Move(src[0], dest[0], s2);
         src:=PShortIntArray(Integer(src)+s2);
      end;
      dest:=PSmallIntArray(Integer(dest)+s2);
   end;
end;

// GetTile
//
function THeightTileFile.GetTile(aLeft, aTop : Integer) : PHeightTile;
var
   i, n : Integer;
   tileInfo : PHeightTileInfo;
   buf : array of ShortInt;
begin
   tileInfo:=nil;
   n:=0;
   for i:=Low(FTileIndex) to High(FTileIndex) do with FTileIndex[i] do
      if (left=aLeft) and (top=aTop) then begin
         tileInfo:=@FTileIndex[i];
         if i<High(FTileIndex) then
            n:=FTileIndex[i+1].fileOffset-tileInfo.fileOffset
         else n:=FDataSize+cHeaderSize-tileInfo.fileOffset;
         Break;
      end;
   if Assigned(tileInfo) then begin
      Result:=@FHeightTile;
      FHeightTile.info:=tileInfo^;
      SetLength(Result.Data, tileInfo.width*tileInfo.height);
      SetLength(buf, n);
      FFile.Seek(tileInfo.fileOffset, soFromBeginning);
      FFile.Read(buf[0], n);
      UnPackTile(@buf[0]);
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

end.

