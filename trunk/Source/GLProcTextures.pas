// GLProcTextures
{: Procedural textures.<p>

	<b>History : </b><font size=-1><ul>
      <li>11/12/02 - ??? - Initial, procedural perlin noise texture
                           code by Tobias Peirick
	</ul></font><p>

  I used the following references for my implementation:<p>

  http://freespace.virgin.net/hugo.elias/models/m_perlin.htm<br>
  http://freespace.virgin.net/hugo.elias/models/m_clouds.htm<br>
  http://www.delphi3d.net<p>

  Tobias Peirick
}
unit GLProcTextures;

interface

uses Classes, GLTexture, GLGraphics, OpenGL1x, GLCrossPlatform, SysUtils;

const
  GRADIENT_TABLE_SIZE = 256;
  DAMP_SHIFT = 20;      

type
 TGLProcTextureNoise = class(TGLTextureImage)
 private
   FNoiseMap       : TGLBitmap32;
   FWidth, FHeight : Integer;
   FMinCut         : Byte;
   //FMaxCut         : Byte;
   FNoiseSharpness : Single;
   FNoiseAnimate   : Single;
   FSeamless       : Boolean;
 protected
   FGradients: array [0..GRADIENT_TABLE_SIZE*3 - 1] of Single;
   function GetWidth: Integer; override;
   function GetHeight: Integer; override;
   function  Noise(x, y: Single): Single;
   procedure SetMinCut(const val : Byte);
   procedure SetNoiseSharpness(const val : Single);
   procedure SetWidth(const val : Integer);
   procedure SetHeight(const val : Integer);
   procedure SetSeamless(const val : Boolean);
   procedure UpdateNoise;
 public
   constructor Create(AOwner : TPersistent); override;
   destructor Destroy; override;
   class function FriendlyName : String; override;
   class function FriendlyDescription : String; override;
   procedure Assign(Source: TPersistent); override;
   function GetBitmap32(target: TGLuint): TGLBitmap32; override;
   procedure ReleaseBitmap32; override;
   procedure SaveToFile(const fileName : String); override;
   procedure LoadFromFile(const fileName : String); override;
   procedure NoiseAnimate( speed : Single );
 published
   property Width : Integer read GetWidth write SetWidth default 128;
   property Height : Integer read GetHeight write SetHeight default 128;
   property MinCut : Byte read FMinCut write SetMinCut;
   property NoiseSharpness : Single read FNoiseSharpness write SetNoiseSharpness;
   property Seamless : Boolean read FSeamless write SetSeamless;
 end;

   // TGLBlankTIE
   //
   TGLProcTextureNoiseTIE = class(TGLTextureImageEditor)
		public
         { Public Properties }
			class function Edit(aTexImage : TGLTextureImage) : Boolean; override;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses VectorGeometry, GLUtils;

constructor TGLProcTextureNoise.Create(AOwner: TPersistent);
var
  i: Integer;
  z, r, theta: Single;
begin
  inherited;
  FWidth:=128;
  FHeight:=128;
  FMinCut := 0;
  FNoiseSharpness := 0.99;
  FSeamless := False;

  Randomize;
  RandSeed := Random(10000);

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

destructor TGLProcTextureNoise.Destroy;
begin
  ReleaseBitmap32;
  inherited;
end;


procedure TGLProcTextureNoise.UpdateNoise;
var X,Y,C : Integer;
    Line  : PGLPixel32Array;
    nf    : Single;
    n     : Byte;

function NoiseSeamless( Scale : Single): Single;
begin
  Result :=( Noise(x/Scale, y/Scale) * (Width - x) * (Height - y)
           + Noise((x - width)/Scale, y/Scale) * x * (Height - y)
           + Noise((x - width)/Scale, (y-Height)/Scale) * x * y
           + Noise(x/Scale, (y-Height)/Scale) * (Width - x) * y)
           / (Width *Height);
end;

begin
  // Update the noise texture.
  for y := 0 to FNoiseMap.Height-1 do
  begin
    Line := FNoiseMap.ScanLine[y];
    for x := 0 to FNoiseMap.Width-1 do
    begin
       nf := 0;

       case FSeamless of
           // Take 4 octaves of noise and add them weighted for seamless.
           // uses much Ghz
           True:  begin
                    nf := NoiseSeamless(16)
                        + NoiseSeamless(8)/2
                        + NoiseSeamless(4)/4
                        + NoiseSeamless(2)/8;
                  end;
           // Take 4  octaves of noise and add them.
           False: begin
                    nf := Noise(x/16, y/16)
                        + Noise(x/8, y/8)/2
                        + Noise(x/4, y/4)/4
                        + Noise(x/2, y/2)/8;
                  end;
      end;


      // Range between 0 and 255
      n := Round(255 * (nf+1)/2);
      if MinCut > 0 then begin
        // Min Cut
        C := n - FMinCut;
        if C < 0 then n:=0
        else
        // Noise Sharpness
        n := 255 - Round(IntPower(FNoiseSharpness, C) * 255);
      end;
      // Write the result to the texture image.
      Line[x].r := n;
      Line[x].g := n;
      Line[x].b := n;
      Line[x].a := n;
    end;
  end;
end;

function TGLProcTextureNoise.GetBitmap32(target: TGLuint): TGLBitmap32;
begin
  if not Assigned(FNoiseMap) then begin
      FNoiseMap:=TGLBitmap32.Create;
      FNoiseMap.Width:=FWidth;
      FNoiseMap.Height:=FHeight;
      UpdateNoise;
  end;
  Result:=FNoiseMap;
end;

// FriendlyName
//
class function TGLProcTextureNoise.FriendlyName : String;
begin
   Result:='Procedural Noise';
end;

// FriendlyDescription
//
class function TGLProcTextureNoise.FriendlyDescription : String;
begin
   Result:='Procedural Noise (Animated)';
end;


procedure TGLProcTextureNoise.SetSeamless(const val: Boolean);
begin
  if val<>FSeamless then begin
      FSeamless:=val;
      Invalidate;
   end;
end;


procedure TGLProcTextureNoise.LoadFromFile(const fileName: String);
begin
   Assert(False, 'TGLProcTextureNoise.LoadFromFile not implemented');
end;

procedure TGLProcTextureNoise.ReleaseBitmap32;
begin
  if Assigned(FNoiseMap) then begin
    FNoiseMap.Free;
    FNoiseMap:=nil;
  end;
end;

procedure TGLProcTextureNoise.SaveToFile(const fileName: String);
begin

end;

function TGLProcTextureNoise.GetHeight: Integer;
begin
  Result := FHeight;
end;

function TGLProcTextureNoise.GetWidth: Integer;
begin
  Result := FWidth;
end;

procedure TGLProcTextureNoise.SetHeight(const val: Integer);
begin
   if val<>FHeight then begin
      FHeight:=val;
      if FHeight<1 then FHeight:=1;
      Invalidate;
   end;
end;

procedure TGLProcTextureNoise.SetWidth(const val: Integer);
begin
   if val<>FWidth then begin
      FWidth:=val;
      if FWidth<1 then FWidth:=1;
      Invalidate;
   end;
end;

procedure TGLProcTextureNoise.SetMinCut(const val: Byte);
begin
   if val<>FMinCut then begin
      FMinCut:=val;
      Invalidate;
   end;
end;

procedure TGLProcTextureNoise.SetNoiseSharpness(const val: Single);
begin
  if val<>FNoiseSharpness then begin
      FNoiseSharpness:=val;
      if FNoiseSharpness>1 then FNoiseSharpness:=1;
      Invalidate;
   end;
end;


procedure TGLProcTextureNoise.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TGLProcTextureNoise) then
  begin
     FWidth:=TGLProcTextureNoise(Source).FWidth;
     FHeight:=TGLProcTextureNoise(Source).FHeight;
     FMinCut := TGLProcTextureNoise(Source).FMinCut;
     FNoiseSharpness := TGLProcTextureNoise(Source).FNoiseSharpness;
     Invalidate;
  end else inherited;
end;

procedure TGLProcTextureNoise.NoiseAnimate(speed : Single);
begin
  FNoiseAnimate := FNoiseAnimate + speed;
  Invalidate;
end;


function TGLProcTextureNoise.Noise(x, y : Single): Single;
const
  { Borrowed from Darwyn Peachey.
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
var
  ix, iy, iz: Integer;
  fx0, fx1, fy0, fy1, fz0, fz1: Single;
  wx, wy, wz: Single;
  vx0, vx1, vy0, vy1, vz0, vz1: Single;

  function Smooth(x: Single): Single;
  begin
      { Smoothing curve. This is used to calculate interpolants so that the noise
        doesn't look blocky when the frequency is low. }
      Result := x*x*(3 - 2*x);
  end;

  function Permutate(x: Integer): Integer;
  const
    MASK = GRADIENT_TABLE_SIZE - 1;
  begin
    // Do a lookup in the permutation table.
    Result := PERM[x and MASK];
  end;
  
  function Index(ix, iy, iz: Integer): Integer;
  begin
    // Turn an XYZ triplet into a single gradient table index.
    Result := Permutate(ix + Permutate(iy + Permutate(iz)));
  end;

  function Lattice(lx, ly, lz: Integer; fx, fy, fz: Single): Single;
  var
    g: Integer;
  begin
    // Look up a random gradient at [ix,iy,iz] and dot it with the [fx,fy,fz] vector.
    g := Index(lx, ly, lz)*3;
    Result := FGradients[g]*fx + FGradients[g+1]*fy + FGradients[g+2]*fz;
  end;
  
  function Lerp(t, x0, x1: Single): Single;
  begin
    // Simple linear interpolation.
    Result := x0 + t*(x1-x0);
  end;

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

  iz := Floor(FNoiseAnimate);
  fz0 := FNoiseAnimate - iz;
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

// Edit
//
class function TGLProcTextureNoiseTIE.Edit(aTexImage : TGLTextureImage) : Boolean;
var
   p : Integer;
   buf : String;
begin
   with aTexImage as TGLProcTextureNoise do begin
      buf:=InputDlg(TGLProcTextureNoise.FriendlyName, 'Enter size', Format('%d x %d', [Width, Height]));
      p:=Pos('x', buf);
      if p>0 then begin
         Width:=StrToIntDef(Trim(Copy(buf, 1, p-1)), 256);
         Height:=StrToIntDef(Trim(Copy(buf, p+1, MaxInt)), 256);
         buf:=InputDlg(TGLProcTextureNoise.FriendlyName, 'Minimum Cut', IntToStr(FMinCut));
         FMinCut := StrToIntDef(buf, 0);
         buf:=InputDlg(TGLProcTextureNoise.FriendlyName, 'Noise Sharpness', FloatToStr(FNoiseSharpness));
         FNoiseSharpness := StrToFloatDef(buf, 0.9);
         buf := InputDlg(TGLProcTextureNoise.FriendlyName, 'Generate Seamless Texture (0,1)', IntToStr(Ord(FSeamless)));
         FSeamless := (buf<>'0');
         Result:=True;
         Invalidate;
      end else begin
         InformationDlg('Invalid size');
         Result:=False;
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

   RegisterGLTextureImageClass(TGLProcTextureNoise);
   RegisterGLTextureImageEditor(TGLProcTextureNoise, TGLProcTextureNoiseTIE);

finalization

   UnRegisterGLTextureImageEditor(TGLProcTextureNoiseTIE);

end.
