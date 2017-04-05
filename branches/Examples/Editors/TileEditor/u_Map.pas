unit u_Map;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Imaging.jpeg,
   
  GLCadencer,
  GLWin32Viewer,
  GLCrossPlatform,
  GLBaseClasses,
  GLScene,
  GLObjects,
  GLCoordinates,
  GLMesh,
  GLAsyncTimer,
  GLRenderContextInfo,
  GLVectorGeometry,
  uLog, GLFileTGA;


type
  t_cType = (ct_Grass, ct_Tree, ct_Stone, ct_Sand, ct_Coast, ct_Water);

  t_Cell = record
    ctype: t_cType;
    csub: byte;
    end;


  c_Map = class(TComponent)
  private
    f_arr: array of array of t_Cell;
    f_mapWidth,f_mapHeight: integer;
    function _getCell(i,j:integer): t_Cell;
    procedure _loadMap(m:string);
  public
    constructor Create(a_Owner:TComponent; a_MapFileName:string);
    procedure redraw(a_Mesh:TGLMesh; a_Point:TPoint);
    property cell[i,j:integer]: t_Cell read _getCell;
    property mapWidth: integer read f_mapWidth;
    property mapHeight: integer read f_mapHeight;
  end;



implementation


//
// Create
//
constructor c_Map.Create;
begin

  inherited Create(a_Owner);

  _loadMap(a_MapFileName);

end;


//
// getCell
//
function c_Map._getCell(i,j:integer): t_Cell;
begin

  if (i >= 0) and (i < f_mapWidth) and (j >= 0) and (j < f_mapHeight) then
    result := f_arr[i][j];

end;


//
// loadMap
//
procedure c_Map._loadMap(m:String);
var
    i,j: integer;
    bmp: TBitmap;
    p: PByteArray;

  procedure cell(ct: t_cType; cs:byte);
  begin
    f_arr[i][j].ctype := ct;
    f_arr[i][j].csub := cs;
    end;

begin

  if not fileexists(m) then exit;

  bmp := TBitmap.Create;
  bmp.LoadFromFile(m);

  f_mapWidth := bmp.Width;
  f_mapHeight := bmp.Height;
  setLength(f_arr, bmp.Width, bmp.Height);

  randomize;
  for j := 0 to bmp.Height - 1 do begin

    p := bmp.ScanLine[bmp.Height - j - 1];

    for i := 0 to bmp.Width - 1 do
      case p[i] of
        1..4,6..8,10,12,14: cell(ct_Grass, random(16));
        5,9,11,13,15: cell(ct_Tree, random(8));
        16..17: cell(ct_Stone, (p[i] - 16)*4 + random(4));
        19: cell(ct_Sand, random(8));
        22: cell(ct_Water, random(16));
        23: cell(ct_Coast, random(8));
        end;

    end;

  bmp.Free;

  log('"' + m + '" loaded');

end;


//
// redraw
//
procedure c_Map.redraw(a_Mesh:TGLMesh; a_Point:TPoint);
const
    c = 1 / 8;
    d = 1 / 256;

var
    i,j: integer;
    ts,tt: single;

  function getTCoords: boolean;
  var
      _i,_j: integer;

    procedure st(s,t:single);
    begin
      ts := s; tt := t;
      end;

  begin

    _i := a_Point.X + i;
    _j := a_Point.Y + j;

    result := false;
    if (_i < 0) or (_i >= f_mapWidth) or (_j < 0) or (_j >= f_mapHeight) then
      exit;

    ts := 0; tt := 0;
    with f_arr[_i][_j] do
      case ctype of

        ct_Grass: st( c * (csub mod 8), 1 - c * (1 + csub div 8) );
        ct_Tree: st( c * csub, 1 - c * 3 );
        ct_Stone: st( c * csub, 1 - c * 4 );
        ct_Sand: st( c * csub, 1 - c * 5 );
        ct_Coast: st( c * csub, 1 - c * 6 );
        ct_Water: st( c * (csub mod 8), 1 - c * (7 + csub div 8) );

        end;

    result := true;

    end;

  function v(_x,_y,_s,_t:single): TGLVertexData;
  begin
    setVector(result.coord, _x, _y, 0);
    setVector(result.normal, 0, 0, 1);
    result.textCoord.S := _s;
    result.textCoord.T := _t;
    end;

begin

  if f_arr = nil then
    exit;

  a_Mesh.Vertices.Clear;

  for i := -50 to 49 do
    for j := -25 to 24 do
      with a_Mesh.Vertices do
        if getTCoords then begin
          AddVertex(v(i, j, ts + d, tt + d));
          AddVertex(v(i + 1, j, ts + c - d, tt + d));
          AddVertex(v(i + 1, j + 1, ts + c - d, tt + c - d));
          AddVertex(v(i, j + 1, ts, tt + c - d));
          end
        else begin
          AddVertex(v(i, j, 0,0));
          AddVertex(v(i + 1, j, 0,0));
          AddVertex(v(i + 1, j + 1, 0,0));
          AddVertex(v(i, j + 1, 0,0));
          end;

end;

end.
