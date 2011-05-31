//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLTeapot<p>

   Implements the standard Utah Teapot, builded using Bezier Surface
   http://people.sc.fsu.edu/~jburkardt/data/bezier_surface/bezier_surface.html.

 <b>History : </b><font size=-1><ul>
      <li>30/05/11 - Yar - Transition to indirect rendering objects
      <li>23/08/10 - Yar - Added OpenGLTokens to uses, replaced OpenGL1x functions to OpenGLAdapter
      <li>22/04/10 - Yar - Fixes after GLState revision
      <li>05/03/10 - DanB - More state added to TGLStateCache
      <li>30/03/07 - DaStr - Added $I GLScene.inc
      <li>21/07/03 - EG - Creation from GLObjects split
   </ul></font>
}
unit GLTeapot;

interface

{$I GLScene.inc}

uses
  Classes,
  GLScene,
  GLObjects,
  VectorGeometry,
  OpenGLTokens,
  GLContext,
  GLRenderContextInfo
{$IFDEF GLS_DELPHI},
  VectorTypes{$ENDIF};

type

  // TGLTeapot
  //
  {: The classic teapot.<p>
     The only use of this object is as placeholder for testing... }
  TGLTeapot = class(TGLSceneObjectEx)
  private
    { Private Declarations }
    FDivision: Integer;
    FNormalDirection: TNormalDirection;
    FNormals: TNormalSmoothing;
    procedure SetDivision(Value: Integer);
    procedure SetNormalDirection(const Value: TNormalDirection);
    procedure SetNormals(const Value: TNormalSmoothing);
  protected
    procedure BuildMesh; override; stdcall;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
  published
    property Division: Integer read FDivision write SetDivision default 9;
    property NormalDirection: TNormalDirection read FNormalDirection
      write SetNormalDirection default ndOutside;
    property Normals: TNormalSmoothing read FNormals write SetNormals
      default nsSmooth;
  end;

implementation

uses
  GLState, GLS_ShaderParameter, GLS_Mesh;

constructor TGLTeapot.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDivision := 9;
  FNormals := nsSmooth;
  FNormalDirection := ndOutside;
end;

procedure TGLTeapot.SetDivision(Value: Integer);
begin
  Value := ClampValue(Value, 1, 32);
  if FDivision <> Value then
  begin
    FDivision := Value;
    StructureChanged;
  end;
end;

procedure TGLTeapot.SetNormalDirection(const Value: TNormalDirection);
begin
  if Value <> FNormalDirection then
  begin
    FNormalDirection := Value;
    StructureChanged;
  end;
end;

procedure TGLTeapot.SetNormals(const Value: TNormalSmoothing);
begin
  if Value <> FNormals then
  begin
    FNormals := Value;
    StructureChanged;
  end;
end;

const
  CTeapotIndices: array[0..511] of Integer =
    (
    1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
    4, 17, 18, 19, 8, 20, 21, 22, 12, 23, 24, 25, 16, 26, 27, 28,
    19, 29, 30, 31, 22, 32, 33, 34, 25, 35, 36, 37, 28, 38, 39, 40,
    31, 41, 42, 1, 34, 43, 44, 5, 37, 45, 46, 9, 40, 47, 48, 13,
    13, 14, 15, 16, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
    16, 26, 27, 28, 52, 61, 62, 63, 56, 64, 65, 66, 60, 67, 68, 69,
    28, 38, 39, 40, 63, 70, 71, 72, 66, 73, 74, 75, 69, 76, 77, 78,
    40, 47, 48, 13, 72, 79, 80, 49, 75, 81, 82, 53, 78, 83, 84, 57,
    57, 58, 59, 60, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96,
    60, 67, 68, 69, 88, 97, 98, 99, 92, 100, 101, 102, 96, 103, 104, 105,
    69, 76, 77, 78, 99, 106, 107, 108, 102, 109, 110, 111, 105, 112, 113, 114,
    78, 83, 84, 57, 108, 115, 116, 85, 111, 117, 118, 89, 114, 119, 120, 93,
    121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136,
    124, 137, 138, 121, 128, 139, 140, 125, 132, 141, 142, 129, 136, 143, 144, 133,
    133, 134, 135, 136, 145, 146, 147, 148, 149, 150, 151, 152, 69, 153, 154, 155,
    136, 143, 144, 133, 148, 156, 157, 145, 152, 158, 159, 149, 155, 160, 161, 69,
    162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177,
    165, 178, 179, 162, 169, 180, 181, 166, 173, 182, 183, 170, 177, 184, 185, 174,
    174, 175, 176, 177, 186, 187, 188, 189, 190, 191, 192, 193, 194, 195, 196, 197,
    177, 184, 185, 174, 189, 198, 199, 186, 193, 200, 201, 190, 197, 202, 203, 194,
    204, 204, 204, 204, 207, 208, 209, 210, 211, 211, 211, 211, 212, 213, 214, 215,
    204, 204, 204, 204, 210, 217, 218, 219, 211, 211, 211, 211, 215, 220, 221, 222,
    204, 204, 204, 204, 219, 224, 225, 226, 211, 211, 211, 211, 222, 227, 228, 229,
    204, 204, 204, 204, 226, 230, 231, 207, 211, 211, 211, 211, 229, 232, 233, 212,
    212, 213, 214, 215, 234, 235, 236, 237, 238, 239, 240, 241, 242, 243, 244, 245,
    215, 220, 221, 222, 237, 246, 247, 248, 241, 249, 250, 251, 245, 252, 253, 254,
    222, 227, 228, 229, 248, 255, 256, 257, 251, 258, 259, 260, 254, 261, 262, 263,
    229, 232, 233, 212, 257, 264, 265, 234, 260, 266, 267, 238, 263, 268, 269, 242,
    270, 270, 270, 270, 279, 280, 281, 282, 275, 276, 277, 278, 271, 272, 273, 274,
    270, 270, 270, 270, 282, 289, 290, 291, 278, 286, 287, 288, 274, 283, 284, 285,
    270, 270, 270, 270, 291, 298, 299, 300, 288, 295, 296, 297, 285, 292, 293, 294,
    270, 270, 270, 270, 300, 305, 306, 279, 297, 303, 304, 275, 294, 301, 302, 271
    );

  cTeapotVertices: array[0..305] of TVector3f =
    (
    (1.4000, 2.400, 0.0000),
    (1.4000, 2.400, -0.7840),
    (0.7840, 2.400, -1.4000),
    (0.0000, 2.400, -1.4000),
    (1.3375, 2.5312, 0.0000),
    (1.3375, 2.5312, -0.7490),
    (0.7490, 2.5312, -1.3375),
    (0.0000, 2.5312, -1.3375),
    (1.4375, 2.5312, 0.0000),
    (1.4375, 2.5312, -0.8050),
    (0.8050, 2.5312, -1.4375),
    (0.0000, 2.5312, -1.4375),
    (1.5000, 2.400, 0.0000),
    (1.5000, 2.400, -0.8400),
    (0.8400, 2.400, -1.5000),
    (0.0000, 2.400, -1.5000),
    (-0.7840, 2.400, -1.4000),
    (-1.4000, 2.400, -0.7840),
    (-1.4000, 2.400, 0.0000),
    (-0.7490, 2.5312, -1.3375),
    (-1.3375, 2.5312, -0.7490),
    (-1.3375, 2.5312, 0.0000),
    (-0.8050, 2.5312, -1.4375),
    (-1.4375, 2.5312, -0.8050),
    (-1.4375, 2.5312, 0.0000),
    (-0.8400, 2.400, -1.5000),
    (-1.5000, 2.400, -0.8400),
    (-1.5000, 2.400, 0.0000),
    (-1.4000, 2.400, 0.7840),
    (-0.7840, 2.400, 1.4000),
    (0.0000, 2.400, 1.4000),
    (-1.3375, 2.5312, 0.7490),
    (-0.7490, 2.5312, 1.3375),
    (0.0000, 2.5312, 1.3375),
    (-1.4375, 2.5312, 0.8050),
    (-0.8050, 2.5312, 1.4375),
    (0.0000, 2.5312, 1.4375),
    (-1.5000, 2.400, 0.8400),
    (-0.8400, 2.400, 1.5000),
    (0.0000, 2.400, 1.5000),
    (0.7840, 2.400, 1.4000),
    (1.4000, 2.400, 0.7840),
    (0.7490, 2.5312, 1.3375),
    (1.3375, 2.5312, 0.7490),
    (0.8050, 2.5312, 1.4375),
    (1.4375, 2.5312, 0.8050),
    (0.8400, 2.400, 1.5000),
    (1.5000, 2.400, 0.8400),
    (1.7500, 1.875, 0.0000),
    (1.7500, 1.875, -0.9800),
    (0.9800, 1.875, -1.7500),
    (0.0000, 1.875, -1.7500),
    (2.0000, 1.350, 0.0000),
    (2.0000, 1.350, -1.1200),
    (1.1200, 1.350, -2.0000),
    (0.0000, 1.350, -2.0000),
    (2.0000, 0.900, 0.0000),
    (2.0000, 0.900, -1.1200),
    (1.1200, 0.900, -2.0000),
    (0.0000, 0.900, -2.0000),
    (-0.9800, 1.875, -1.7500),
    (-1.7500, 1.875, -0.9800),
    (-1.7500, 1.875, 0.0000),
    (-1.1200, 1.350, -2.0000),
    (-2.0000, 1.350, -1.1200),
    (-2.0000, 1.350, 0.0000),
    (-1.1200, 0.900, -2.0000),
    (-2.0000, 0.900, -1.1200),
    (-2.0000, 0.900, 0.0000),
    (-1.7500, 1.875, 0.9800),
    (-0.9800, 1.875, 1.7500),
    (0.0000, 1.875, 1.7500),
    (-2.0000, 1.350, 1.1200),
    (-1.1200, 1.350, 2.0000),
    (0.0000, 1.350, 2.0000),
    (-2.0000, 0.900, 1.1200),
    (-1.1200, 0.900, 2.0000),
    (0.0000, 0.900, 2.0000),
    (0.9800, 1.875, 1.7500),
    (1.7500, 1.875, 0.9800),
    (1.1200, 1.350, 2.0000),
    (2.0000, 1.350, 1.1200),
    (1.1200, 0.900, 2.0000),
    (2.0000, 0.900, 1.1200),
    (2.0000, 0.450, 0.0000),
    (2.0000, 0.450, -1.1200),
    (1.1200, 0.450, -2.0000),
    (0.0000, 0.450, -2.0000),
    (1.5000, 0.225, 0.0000),
    (1.5000, 0.225, -0.8400),
    (0.8400, 0.225, -1.5000),
    (0.0000, 0.225, -1.5000),
    (1.5000, 0.150, 0.0000),
    (1.5000, 0.150, -0.8400),
    (0.8400, 0.150, -1.5000),
    (0.0000, 0.150, -1.5000),
    (-1.1200, 0.450, -2.0000),
    (-2.0000, 0.450, -1.1200),
    (-2.0000, 0.450, 0.0000),
    (-0.8400, 0.225, -1.5000),
    (-1.5000, 0.225, -0.8400),
    (-1.5000, 0.225, 0.0000),
    (-0.8400, 0.150, -1.5000),
    (-1.5000, 0.150, -0.8400),
    (-1.5000, 0.150, 0.0000),
    (-2.0000, 0.450, 1.1200),
    (-1.1200, 0.450, 2.0000),
    (0.0000, 0.450, 2.0000),
    (-1.5000, 0.225, 0.8400),
    (-0.8400, 0.225, 1.5000),
    (0.0000, 0.225, 1.5000),
    (-1.5000, 0.150, 0.8400),
    (-0.8400, 0.150, 1.5000),
    (0.0000, 0.150, 1.5000),
    (1.1200, 0.450, 2.0000),
    (2.0000, 0.450, 1.1200),
    (0.8400, 0.225, 1.5000),
    (1.5000, 0.2250, 0.8400),
    (0.8400, 0.150, 1.5000),
    (1.5000, 0.150, 0.8400),
    (-1.6000, 2.025, 0.0000),
    (-1.6000, 2.025, -0.3000),
    (-1.5000, 2.250, -0.3000),
    (-1.5000, 2.250, 0.0000),
    (-2.3000, 2.025, 0.0000),
    (-2.3000, 2.025, -0.3000),
    (-2.5000, 2.250, -0.3000),
    (-2.5000, 2.250, 0.0000),
    (-2.7000, 2.025, 0.0000),
    (-2.7000, 2.025, -0.3000),
    (-3.0000, 2.250, -0.3000),
    (-3.0000, 2.250, 0.0000),
    (-2.7000, 1.800, 0.0000),
    (-2.7000, 1.800, -0.3000),
    (-3.0000, 1.800, -0.3000),
    (-3.0000, 1.800, 0.0000),
    (-1.5000, 2.250, 0.3000),
    (-1.6000, 2.025, 0.3000),
    (-2.5000, 2.250, 0.3000),
    (-2.3000, 2.025, 0.3000),
    (-3.0000, 2.250, 0.3000),
    (-2.7000, 2.025, 0.3000),
    (-3.0000, 1.800, 0.3000),
    (-2.7000, 1.800, 0.3000),
    (-2.7000, 1.575, 0.0000),
    (-2.7000, 1.575, -0.3000),
    (-3.0000, 1.350, -0.3000),
    (-3.0000, 1.350, 0.0000),
    (-2.5000, 1.125, 0.0000),
    (-2.5000, 1.125, -0.3000),
    (-2.6500, 0.937, -0.3000),
    (-2.6500, 0.937, 0.0000),
    (-2.0000, 0.900, -0.3000),
    (-1.9000, 0.600, -0.3000),
    (-1.9000, 0.600, 0.0000),
    (-3.0000, 1.350, 0.3000),
    (-2.7000, 1.575, 0.3000),
    (-2.6500, 0.937, 0.3000),
    (-2.5000, 1.125, 0.3000),
    (-1.9000, 0.600, 0.3000),
    (-2.0000, 0.900, 0.3000),
    (1.7000, 1.425, 0.0000),
    (1.7000, 1.425, -0.6600),
    (1.7000, 0.600, -0.6600),
    (1.7000, 0.600, 0.0000),
    (2.6000, 1.425, 0.0000),
    (2.6000, 1.425, -0.6600),
    (3.1000, 0.825, -0.6600),
    (3.1000, 0.825, 0.0000),
    (2.3000, 2.100, 0.0000),
    (2.3000, 2.100, -0.2500),
    (2.4000, 2.025, -0.2500),
    (2.4000, 2.025, 0.0000),
    (2.7000, 2.400, 0.0000),
    (2.7000, 2.400, -0.2500),
    (3.3000, 2.400, -0.2500),
    (3.3000, 2.400, 0.0000),
    (1.7000, 0.600, 0.6600),
    (1.7000, 1.425, 0.6600),
    (3.1000, 0.825, 0.6600),
    (2.6000, 1.425, 0.6600),
    (2.4000, 2.025, 0.2500),
    (2.3000, 2.100, 0.2500),
    (3.3000, 2.400, 0.2500),
    (2.7000, 2.400, 0.2500),
    (2.8000, 2.475, 0.0000),
    (2.8000, 2.475, -0.2500),
    (3.5250, 2.4937, -0.2500),
    (3.5250, 2.4937, 0.0000),
    (2.9000, 2.475, 0.0000),
    (2.9000, 2.475, -0.1500),
    (3.4500, 2.512, -0.1500),
    (3.4500, 2.512, 0.0000),
    (2.8000, 2.400, 0.0000),
    (2.8000, 2.400, -0.1500),
    (3.2000, 2.400, -0.1500),
    (3.2000, 2.400, 0.0000),
    (3.5250, 2.4937, 0.2500),
    (2.8000, 2.475, 0.2500),
    (3.4500, 2.512, 0.1500),
    (2.9000, 2.475, 0.1500),
    (3.2000, 2.400, 0.1500),
    (2.8000, 2.400, 0.1500),
    (0.0000, 3.150, 0.0000),
    (0.0000, 3.150, -0.0020),
    (0.0020, 3.150, 0.0000),
    (0.8000, 3.150, 0.0000),
    (0.8000, 3.150, -0.4500),
    (0.4500, 3.150, -0.8000),
    (0.0000, 3.150, -0.8000),
    (0.0000, 2.850, 0.0000),
    (0.2000, 2.700, 0.0000),
    (0.2000, 2.700, -0.1120),
    (0.1120, 2.700, -0.2000),
    (0.0000, 2.700, -0.2000),
    (-0.0020, 3.150, 0.0000),
    (-0.4500, 3.150, -0.8000),
    (-0.8000, 3.150, -0.4500),
    (-0.8000, 3.150, 0.0000),
    (-0.1120, 2.700, -0.2000),
    (-0.2000, 2.700, -0.1120),
    (-0.2000, 2.700, 0.0000),
    (0.0000, 3.150, 0.0020),
    (-0.8000, 3.150, 0.4500),
    (-0.4500, 3.150, 0.8000),
    (0.0000, 3.150, 0.8000),
    (-0.2000, 2.700, 0.1120),
    (-0.1120, 2.700, 0.2000),
    (0.0000, 2.700, 0.2000),
    (0.4500, 3.150, 0.8000),
    (0.8000, 3.150, 0.4500),
    (0.1120, 2.700, 0.2000),
    (0.2000, 2.700, 0.1120),
    (0.4000, 2.550, 0.0000),
    (0.4000, 2.550, -0.2240),
    (0.2240, 2.550, -0.4000),
    (0.0000, 2.550, -0.4000),
    (1.3000, 2.550, 0.0000),
    (1.3000, 2.550, -0.7280),
    (0.7280, 2.550, -1.3000),
    (0.0000, 2.550, -1.3000),
    (1.3000, 2.400, 0.0000),
    (1.3000, 2.400, -0.7280),
    (0.7280, 2.400, -1.3000),
    (0.0000, 2.400, -1.3000),
    (-0.2240, 2.550, -0.4000),
    (-0.4000, 2.550, -0.2240),
    (-0.4000, 2.550, 0.0000),
    (-0.7280, 2.550, -1.3000),
    (-1.3000, 2.550, -0.7280),
    (-1.3000, 2.550, 0.0000),
    (-0.7280, 2.400, -1.3000),
    (-1.3000, 2.400, -0.7280),
    (-1.3000, 2.400, 0.0000),
    (-0.4000, 2.550, 0.2240),
    (-0.2240, 2.550, 0.4000),
    (0.0000, 2.550, 0.4000),
    (-1.3000, 2.550, 0.7280),
    (-0.7280, 2.550, 1.3000),
    (0.0000, 2.550, 1.3000),
    (-1.3000, 2.400, 0.7280),
    (-0.7280, 2.400, 1.3000),
    (0.0000, 2.400, 1.3000),
    (0.2240, 2.550, 0.4000),
    (0.4000, 2.550, 0.2240),
    (0.7280, 2.550, 1.3000),
    (1.3000, 2.550, 0.7280),
    (0.7280, 2.400, 1.3000),
    (1.3000, 2.400, 0.7280),
    (0.0000, 0.000, 0.0000),
    (1.5000, 0.150, 0.0000),
    (1.5000, 0.150, 0.8400),
    (0.8400, 0.150, 1.5000),
    (0.0000, 0.150, 1.5000),
    (1.5000, 0.075, 0.0000),
    (1.5000, 0.075, 0.8400),
    (0.8400, 0.075, 1.5000),
    (0.0000, 0.075, 1.5000),
    (1.4250, 0.000, 0.0000),
    (1.4250, 0.000, 0.7980),
    (0.7980, 0.000, 1.4250),
    (0.0000, 0.000, 1.4250),
    (-0.8400, 0.150, 1.5000),
    (-1.5000, 0.150, 0.8400),
    (-1.5000, 0.150, 0.0000),
    (-0.8400, 0.075, 1.5000),
    (-1.5000, 0.075, 0.8400),
    (-1.5000, 0.075, 0.0000),
    (-0.7980, 0.000, 1.4250),
    (-1.4250, 0.000, 0.7980),
    (-1.4250, 0.000, 0.0000),
    (-1.5000, 0.150, -0.8400),
    (-0.8400, 0.150, -1.5000),
    (0.0000, 0.150, -1.5000),
    (-1.5000, 0.075, -0.8400),
    (-0.8400, 0.075, -1.5000),
    (0.0000, 0.075, -1.5000),
    (-1.4250, 0.000, -0.7980),
    (-0.7980, 0.000, -1.4250),
    (0.0000, 0.000, -1.4250),
    (0.8400, 0.150, -1.5000),
    (1.5000, 0.150, -0.8400),
    (0.8400, 0.075, -1.5000),
    (1.5000, 0.075, -0.8400),
    (0.7980, 0.000, -1.4250),
    (1.4250, 0.000, -0.7980)
    );

function GetPatchValue(N: Integer): TVector3f;
{$IFDEF GLS_INLINE} inline;
{$ENDIF}
begin
  Result := cTeapotVertices[CTeapotIndices[N]-1];
end;

procedure TGLTeapot.BuildMesh;
type
  TVertex = record
    Position: TVector3f;
    Tangent: TVector3f;
    Binormal: TVector3f;
    Normal: TVector3f;
    TexCoord: TTexPoint;
  end;
var
  P, P1, P2, I, J, L: Integer;
  invDiv, u, v, u1, v1: Single;
  B0, B1, B2, B3, T0, T1, T2: TTexPoint;
  vp, dv, du: array[0..3] of TVector3f;
  LVertices: array of array of TVertex;
begin
  with FBatch.Mesh do
  begin
    Lock;
    try
      Clear;
      DeclareAttribute(attrPosition, GLSLType3f);
      DeclareAttribute(attrNormal, GLSLType3f);
      DeclareAttribute(attrTangent, GLSLType3f);
      DeclareAttribute(attrBinormal, GLSLType3f);
      DeclareAttribute(attrTexCoord0, GLSLType2f);

      BeginAssembly(mpTRIANGLES);

      SetLength(LVertices, FDivision + 1, FDivision + 1);
      invDiv := 1 / FDivision;
      for P := 0 to 31 do
      begin
        P1 := P * 16;
        for I := 0 to FDivision do
        begin
          v := I * invDiv;
          v1 := 1 - v;
          for J := 0 to FDivision do
          begin
            u := J * invDiv;
            u1 := 1 - u;
            B0.S := u1 * u1 * u1;
            B0.T := v1 * v1 * v1;
            B1.S := 3 * u * u1 * u1;
            B1.T := 3 * v * v1 * v1;
            B2.S := 3 * u * u * u1;
            B2.T := 3 * v * v * v1;
            B3.S := u * u * u;
            B3.T := v * v * v;

            for L := 0 to 3 do
            begin
              P2 := P1 + L * 4;
              SetVector(vp[L], VectorScale(GetPatchValue(P2 + 0), B0.S));
              AddVector(vp[L], VectorScale(GetPatchValue(P2 + 1), B1.S));
              AddVector(vp[L], VectorScale(GetPatchValue(P2 + 2), B2.S));
              AddVector(vp[L], VectorScale(GetPatchValue(P2 + 3), B3.S));
            end;
            ScaleVector(vp[0], B0.T);
            ScaleVector(vp[1], B1.T);
            ScaleVector(vp[2], B2.T);
            ScaleVector(vp[3], B3.T);
            AddVector(vp[0], vp[1]);
            AddVector(vp[0], vp[2]);
            AddVector(vp[0], vp[3]);
            LVertices[I, J].Position := vp[0];

            T0.S := u1 * u1;
            T0.T := v1 * v1;
            T1.S := 2 * u * u1;
            T1.T := 2 * v * v1;
            T2.S := u * u;
            T2.T := v * v;

            SetVector(dv[0], VectorScale(VectorSubtract(GetPatchValue(P1 + 4), GetPatchValue(P1 + 0)), B0.S));
            AddVector(dv[0], VectorScale(VectorSubtract(GetPatchValue(P1 + 5), GetPatchValue(P1 + 1)), B1.S));
            AddVector(dv[0], VectorScale(VectorSubtract(GetPatchValue(P1 + 6), GetPatchValue(P1 + 2)), B2.S));
            AddVector(dv[0], VectorScale(VectorSubtract(GetPatchValue(P1 + 7), GetPatchValue(P1 + 3)), B3.S));
            ScaleVector(dv[0], T0.T);

            SetVector(dv[1], VectorScale(VectorSubtract(GetPatchValue(P1 + 8), GetPatchValue(P1 + 4)), B0.S));
            AddVector(dv[1], VectorScale(VectorSubtract(GetPatchValue(P1 + 9), GetPatchValue(P1 + 5)), B1.S));
            AddVector(dv[1], VectorScale(VectorSubtract(GetPatchValue(P1 + 10), GetPatchValue(P1 + 6)), B2.S));
            AddVector(dv[1], VectorScale(VectorSubtract(GetPatchValue(P1 + 11), GetPatchValue(P1 + 7)), B3.S));
            ScaleVector(dv[1], T1.T);

            SetVector(dv[2], VectorScale(VectorSubtract(GetPatchValue(P1 + 12), GetPatchValue(P1 + 8)), B0.S));
            AddVector(dv[2], VectorScale(VectorSubtract(GetPatchValue(P1 + 13), GetPatchValue(P1 + 9)), B1.S));
            AddVector(dv[2], VectorScale(VectorSubtract(GetPatchValue(P1 + 14), GetPatchValue(P1 + 10)), B2.S));
            AddVector(dv[2], VectorScale(VectorSubtract(GetPatchValue(P1 + 15), GetPatchValue(P1 + 11)), B3.S));
            ScaleVector(dv[2], T2.T);

            AddVector(dv[0], dv[1]);
            AddVector(dv[0], dv[2]);
            LVertices[I, J].Tangent := VectorNormalize(dv[0]);

            SetVector(du[0], VectorScale(VectorSubtract(GetPatchValue(P1 + 1), GetPatchValue(P1 + 0)), T0.S));
            AddVector(du[0], VectorScale(VectorSubtract(GetPatchValue(P1 + 2), GetPatchValue(P1 + 1)), T1.S));
            AddVector(du[0], VectorScale(VectorSubtract(GetPatchValue(P1 + 3), GetPatchValue(P1 + 2)), T2.S));
            ScaleVector(du[0], B0.T);

            SetVector(du[1], VectorScale(VectorSubtract(GetPatchValue(P1 + 5), GetPatchValue(P1 + 4)), T0.S));
            AddVector(du[1], VectorScale(VectorSubtract(GetPatchValue(P1 + 6), GetPatchValue(P1 + 5)), T1.S));
            AddVector(du[1], VectorScale(VectorSubtract(GetPatchValue(P1 + 7), GetPatchValue(P1 + 6)), T2.S));
            ScaleVector(du[1], B1.T);

            SetVector(du[2], VectorScale(VectorSubtract(GetPatchValue(P1 + 9), GetPatchValue(P1 + 8)), T0.S));
            AddVector(du[2], VectorScale(VectorSubtract(GetPatchValue(P1 + 10), GetPatchValue(P1 + 9)), T1.S));
            AddVector(du[2], VectorScale(VectorSubtract(GetPatchValue(P1 + 11), GetPatchValue(P1 + 10)), T2.S));
            ScaleVector(du[2], B2.T);

            SetVector(du[3], VectorScale(VectorSubtract(GetPatchValue(P1 + 13), GetPatchValue(P1 + 12)), T0.S));
            AddVector(du[3], VectorScale(VectorSubtract(GetPatchValue(P1 + 14), GetPatchValue(P1 + 13)), T1.S));
            AddVector(du[3], VectorScale(VectorSubtract(GetPatchValue(P1 + 15), GetPatchValue(P1 + 14)), T2.S));
            ScaleVector(du[3], B3.T);

            AddVector(du[0], du[1]);
            AddVector(du[0], du[2]);
            AddVector(du[0], du[3]);
            LVertices[I, J].Binormal := VectorNormalize(du[0]);
            LVertices[I, J].Normal := VectorNormalize(VectorCrossProduct(dv[0], du[0]));
            LVertices[I, J].TexCoord := TexPointMake(u, v);
          end;
        end;

        for I := 0 to FDivision - 1 do
          for J := 0 to FDivision - 1 do
          begin
            Attribute3f(attrPosition, LVertices[I, J+1].Position);
            Attribute3f(attrNormal, LVertices[I, J+1].Normal);
            Attribute3f(attrTangent, LVertices[I, J+1].Tangent);
            Attribute3f(attrBinormal, LVertices[I, J+1].Binormal);
            Attribute2f(attrTexCoord0, LVertices[I, J+1].TexCoord);
            EmitVertex;
            Attribute3f(attrPosition, LVertices[I, J].Position);
            Attribute3f(attrNormal, LVertices[I, J].Normal);
            Attribute3f(attrTangent, LVertices[I, J].Tangent);
            Attribute3f(attrBinormal, LVertices[I, J].Binormal);
            Attribute2f(attrTexCoord0, LVertices[I, J].TexCoord);
            EmitVertex;
            Attribute3f(attrPosition, LVertices[I+1, J].Position);
            Attribute3f(attrNormal, LVertices[I+1, J].Normal);
            Attribute3f(attrTangent, LVertices[I+1, J].Tangent);
            Attribute3f(attrBinormal, LVertices[I+1, J].Binormal);
            Attribute2f(attrTexCoord0, LVertices[I+1, J].TexCoord);
            EmitVertex;
            EmitVertex;
            Attribute3f(attrPosition, LVertices[I+1, J+1].Position);
            Attribute3f(attrNormal, LVertices[I+1, J+1].Normal);
            Attribute3f(attrTangent, LVertices[I+1, J+1].Tangent);
            Attribute3f(attrBinormal, LVertices[I+1, J+1].Binormal);
            Attribute2f(attrTexCoord0, LVertices[I+1, J+1].TexCoord);
            EmitVertex;
            Attribute3f(attrPosition, LVertices[I, J+1].Position);
            Attribute3f(attrNormal, LVertices[I, J+1].Normal);
            Attribute3f(attrTangent, LVertices[I, J+1].Tangent);
            Attribute3f(attrBinormal, LVertices[I, J+1].Binormal);
            Attribute2f(attrTexCoord0, LVertices[I, J+1].TexCoord);
            EmitVertex;
          end;
      end;

      EndAssembly;
      Rescale(0.5);

      case FNormals of
        nsSmooth:
          begin
            if FNormalDirection = ndInside then
            begin
              FBatch.Mesh.Triangulate;
              FBatch.Mesh.FlipFaces;
            end;
            ApplyExtras;
          end;
        nsFlat:
          begin
            if FNormalDirection = ndInside then
            begin
              FBatch.Mesh.Triangulate;
              FBatch.Mesh.FlipFaces;
            end;
            FBatch.Mesh.ComputeNormals(False);
            ApplyExtras;
          end;
        nsNone:
          begin
            FBatch.Mesh.Attributes[attrNormal] := False;
            FBatch.Mesh.Validate;
            ApplyExtras;
          end;
        end;

    finally
      UnLock;
    end;
  end;

  inherited;
end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
initialization
  //-------------------------------------------------------------
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  RegisterClasses([TGLTeapot]);

end.

