unit Noise;

interface

{ Perlin noise class.  ( by Tom Nuydens (tom@delphi3d.net) )

  ******************************************************************************

  I used the following references for my implementation:
    http://students.vassar.edu/mazucker/code/perlin-noise-math-faq.html
    Darwin Peachey's chapter in "Texturing & Modeling: A Procedural Approach"
  Another good resource is
    http://freespace.virgin.net/hugo.elias/models/m_perlin.htm

  ******************************************************************************

  This class generates 3D Perlin noise. The demo that comes with this is 2D, but
  uses the 3rd dimension to create animated noise. The noise does not tile,
  although it could be made to do so with a few small modifications to the
  algorithm.

  Perlin noise can be used as a starting point for all kinds of things,
  including terrain generation, cloud rendering, procedural textures, and more.
  Most of these techniques involve rendering multiple "octaves" of noise. This
  means you generate multiple noise values for every pixel (each with different
  X, Y and/or Z coordinates), and then sum them. There's an example of this in
  the accompanying demo.
}

uses
  Winapi.Windows, System.Math;

const
  GRADIENT_TABLE_SIZE = 256;

type
  TNoise = class
  protected
    FGradients: array [0..GRADIENT_TABLE_SIZE*3 - 1] of Single;
    procedure InitGradients;
    function Permutate(x: Integer): Integer;
    function Index(ix, iy, iz: Integer): Integer;
    function Lattice(ix, iy, iz: Integer; fx, fy, fz: Single): Single;
    function Lerp(t, x0, x1: Single): Single;
    function Smooth(x: Single): Single;
  public
    // Only two public functions, so it should be easy enough to use!
    constructor Create(seed: Integer);
    function Noise(x, y, z: Single): Single;
  end;

implementation

procedure TNoise.InitGradients;
var
  i: Integer;
  z, r, theta: Single;
begin
  // Generate random gradient vectors.
  for i := 0 to GRADIENT_TABLE_SIZE - 1 do
  begin
    z := 1 - 2*Random;
    r := sqrt(1 - z*z);
    theta := 2*PI*Random;
    FGradients[i*3] := r*cos(theta);
    FGradients[i*3 + 1] := r*sin(theta);
    FGradients[i*3 + 2] := z;
  end;

end;

const
  { Borrowed from Darwyn Peachey (see references above).
    The gradient table is indexed with an XYZ triplet, which is first turned
    into a single random index using a lookup in this table. The table simply
    contains all numbers in [0..255] in random order. }
  PERM: array [0..GRADIENT_TABLE_SIZE-1] of Byte = (
      225,155,210,108,175,199,221,144,203,116, 70,213, 69,158, 33,252,
        5, 82,173,133,222,139,174, 27,  9, 71, 90,246, 75,130, 91,191,
      169,138,  2,151,194,235, 81,  7, 25,113,228,159,205,253,134,142,
      248, 65,224,217, 22,121,229, 63, 89,103, 96,104,156, 17,201,129,
       36,  8,165,110,237,117,231, 56,132,211,152, 20,181,111,239,218,
      170,163, 51,172,157, 47, 80,212,176,250, 87, 49, 99,242,136,189,
      162,115, 44, 43,124, 94,150, 16,141,247, 32, 10,198,223,255, 72,
       53,131, 84, 57,220,197, 58, 50,208, 11,241, 28,  3,192, 62,202,
       18,215,153, 24, 76, 41, 15,179, 39, 46, 55,  6,128,167, 23,188,
      106, 34,187,140,164, 73,112,182,244,195,227, 13, 35, 77,196,185,
       26,200,226,119, 31,123,168,125,249, 68,183,230,177,135,160,180,
       12,  1,243,148,102,166, 38,238,251, 37,240,126, 64, 74,161, 40,
      184,149,171,178,101, 66, 29, 59,146, 61,254,107, 42, 86,154,  4,
      236,232,120, 21,233,209, 45, 98,193,114, 78, 19,206, 14,118,127,
       48, 79,147, 85, 30,207,219, 54, 88,234,190,122, 95, 67,143,109,
      137,214,145, 93, 92,100,245,  0,216,186, 60, 83,105, 97,204, 52
    );

constructor TNoise.Create(seed: Integer);
begin

  inherited Create;

  // Initialize the random gradients before we start.
  RandSeed := seed;
  InitGradients;

end;

function TNoise.Permutate(x: Integer): Integer;
const
  MASK = GRADIENT_TABLE_SIZE - 1;
begin

  // Do a lookup in the permutation table.
  Result := PERM[x and MASK];
end;

function TNoise.Index(ix, iy, iz: Integer): Integer;
begin

  // Turn an XYZ triplet into a single gradient table index.
  Result := Permutate(ix + Permutate(iy + Permutate(iz)));

end;

function TNoise.Lattice(ix, iy, iz: Integer; fx, fy, fz: Single): Single;
var
  g: Integer;
begin

  // Look up a random gradient at [ix,iy,iz] and dot it with the [fx,fy,fz] vector.
  g := Index(ix, iy, iz)*3;
  Result := FGradients[g]*fx + FGradients[g+1]*fy + FGradients[g+2]*fz;

end;

function TNoise.Lerp(t, x0, x1: Single): Single;
begin
  // Simple linear interpolation.
  Result := x0 + t*(x1-x0);
end;

function TNoise.Smooth(x: Single): Single;
begin

  { Smoothing curve. This is used to calculate interpolants so that the noise
    doesn't look blocky when the frequency is low. }
  Result := x*x*(3 - 2*x);

end;

function TNoise.Noise(x, y, z: Single): Single;
var
  ix, iy, iz: Integer;
  fx0, fx1, fy0, fy1, fz0, fz1: Single;
  wx, wy, wz: Single;
  vx0, vx1, vy0, vy1, vz0, vz1: Single;
begin

  { The main noise function. Looks up the pseudorandom gradients at the nearest
    lattice points, dots them with the input vector, and interpolates the
    results to produce a single output value in [0, 1] range. }

  ix := Floor(x);
  fx0 := x - ix;
  fx1 := fx0 - 1;
  wx := Smooth(fx0);
  iy := Floor(y);
  fy0 := y - iy;
  fy1 := fy0 - 1;
  wy := Smooth(fy0);

  iz := Floor(z);
  fz0 := z - iz;
  fz1 := fz0 - 1;
  wz := Smooth(fz0);

  vx0 := Lattice(ix, iy, iz, fx0, fy0, fz0);
  vx1 := Lattice(ix+1, iy, iz, fx1, fy0, fz0);
  vy0 := Lerp(wx, vx0, vx1);

  vx0 := Lattice(ix, iy+1, iz, fx0, fy1, fz0);
  vx1 := Lattice(ix+1, iy+1, iz, fx1, fy1, fz0);
  vy1 := Lerp(wx, vx0, vx1);

  vz0 := Lerp(wy, vy0, vy1);

  vx0 := Lattice(ix, iy, iz+1, fx0, fy0, fz1);
  vx1 := Lattice(ix+1, iy, iz+1, fx1, fy0, fz1);
  vy0 := Lerp(wx, vx0, vx1);

  vx0 := Lattice(ix, iy+1, iz+1, fx0, fy1, fz1);
  vx1 := Lattice(ix+1, iy+1, iz+1, fx1, fy1, fz1);
  vy1 := Lerp(wx, vx0, vx1);

  vz1 := Lerp(wy, vy0, vy1);

  Result := Lerp(wz, vz0, vz1);

end;

end.
