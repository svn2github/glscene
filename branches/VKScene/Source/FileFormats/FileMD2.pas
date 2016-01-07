//
// VKScene project based on GLScene library, http://glscene.sourceforge.net 
//
{
  MD2 file loader 
       
}
unit FileMD2;

interface

{$R-}
{$I VKScene.inc}

uses
  System.Classes, System.SysUtils,
  VKS.VectorGeometry, VKS.VectorTypes, VKS.CrossPlatform,
  TypesMD2;

type
  // TFileMD2
  //
  TFileMD2 = class
  private
    FiFrames: longint;
    FiVertices: longint;
    FiTriangles: longint;
    procedure FreeLists;
  public
    fIndexList : TIndexList;
    fVertexList : TVertexList;
    FrameNames : TStrings;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure LoadFromStream(aStream : TStream);
    property iFrames: longInt read FiFrames;
    property iVertices: longInt read FiVertices;
    property iTriangles: longInt read FiTriangles;

    property IndexList: TIndexList read fIndexList;
    property VertexList: TVertexList read fVertexList;
  end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TFileMD2 ------------------
// ------------------

// Create
//
constructor TFileMD2.Create;
begin
  inherited;
  FreeLists;
  FrameNames := TStringList.Create;
end;

// Destroy
//
destructor TFileMD2.Destroy;
begin
  FreeLists;
  FrameNames.Free;
  inherited;
end;

procedure TFileMD2.FreeLists;
begin
  SetLength(fIndexList,0);
  SetLength(fVertexList,0,0);
  FiFrames := 0;
  FiVertices := 0;
  FiTriangles := 0;
end;

// LoadFromStream
//
procedure TFileMD2.LoadFromStream(aStream : TStream);
var
  Skins: array[0..MAX_MD2_SKINS - 1, 0..63] of AnsiChar;
  TextureCoords: array[0..MAX_MD2_VERTICES - 1] of TVector2s;
  Buffer: array[0..MAX_MD2_VERTICES * 4 + 127] of byte;
  Header: TMD2Header;
  Triangle: TMD2Triangle;
  I: integer;
  J: integer;
  Frame: PMD2AliasFrame;
  FrameName : String;

begin
  FreeLists;
  // read the modelinfo
  aStream.Read(Header, SizeOf(Header));
  FiFrames := Header.Num_Frames;
  FiVertices := Header.Num_Vertices;
  FiTriangles := Header.Num_VertexIndices;
  SetLength(fIndexList, FiTriangles);
  SetLength(fVertexList, FiFrames, FiVertices);
  // get the skins...
  aStream.Read(Skins, Header.Num_Skins * MAX_MD2_SKINNAME);
  // ...and the texcoords
  aStream.Read(TextureCoords, Header.Num_TextureCoords * SizeOf(TVector2s));
  for I := 0 to Header.Num_VertexIndices - 1 do begin
    aStream.Read(Triangle, SizeOf(TMD2Triangle));
     with fIndexList[I] do begin
      A := Triangle.VertexIndex.V[2];
      B := Triangle.VertexIndex.V[1];
      C := Triangle.VertexIndex.V[0];
      A_S := TextureCoords[Triangle.TextureCoordIndex.V[2]].V[0] / Header.SkinWidth;
      A_T := TextureCoords[Triangle.TextureCoordIndex.V[2]].V[1] / Header.SkinHeight;
      B_S := TextureCoords[Triangle.TextureCoordIndex.V[1]].V[0] / Header.SkinWidth;
      B_T := TextureCoords[Triangle.TextureCoordIndex.V[1]].V[1] / Header.SkinHeight;
      C_S := TextureCoords[Triangle.TextureCoordIndex.V[0]].V[0] / Header.SkinWidth;
      C_T := TextureCoords[Triangle.TextureCoordIndex.V[0]].V[1] / Header.SkinHeight;
    end;
  end;
  for I := 0 to Header.Num_Frames - 1 do begin
    Frame := PMD2AliasFrame(@Buffer);
    // read animation / frame info
    aStream.Read(Frame^, Header.FrameSize);
    FrameName := Trim(String(Frame^.Name));
    if CharInSet(Copy(FrameName, Length(FrameName) - 1, 1)[1], ['0'..'9']) then
      FrameName := Copy(FrameName, 1, Length(FrameName) - 2)
    else
      FrameName := Copy(FrameName, 1, Length(FrameName) - 1);
    if FrameNames.IndexOf(FrameName) < 0 then
      FrameNames.AddObject(FrameName, TObject(PtrUInt(I)));
    // fill the vertices list
     for J := 0 to FiVertices - 1 do begin
       fVertexList[i][J].V[0] := Frame^.Vertices[J].V[0] * Frame^.Scale.V[0] + Frame^.Translate.V[0];
       fVertexList[i][J].V[1] := Frame^.Vertices[J].V[1] * Frame^.Scale.V[1] + Frame^.Translate.V[1];
       fVertexList[i][J].V[2] := Frame^.Vertices[J].V[2] * Frame^.Scale.V[2] + Frame^.Translate.V[2];
     end;
  end;
end;

end.
