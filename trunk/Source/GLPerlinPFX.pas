{: GLPerlinPFX<p>

   PFX particle effects revolving around the use of Perlin noise.<p>

   <b>History : </b><font size=-1><ul>
      <li>15/04/04 - EG - Creation
   </ul></font>
}
unit GLPerlinPFX;

interface

uses Classes, GLParticleFX, GLGraphics;

type

   // TGLPerlinPFXManager
   //
   {: A sprite-based particles FX manager using perlin-based sprites.<p>
      This PFX manager is more suited for smoke or fire effects, and with proper
      tweaking of the texture and perlin parameters, may help render a convincing
      effect with less particles.<p>
      The sprite generate by this manager is the composition of a distance-based
      intensity and a perlin noise. }
   TGLPerlinPFXManager = class (TGLBaseSpritePFXManager)
      private
         { Private Declarations }
         FTexMapSize : Integer;
         FNoiseSeed : Integer;
         FNoiseScale : Integer;
         FNoiseAmplitude : Integer;
         FSmoothness : Single;

      protected
         { Protected Declarations }
         procedure PrepareImage(bmp32 : TGLBitmap32; var texFormat : Integer); override;

         procedure SetTexMapSize(const val : Integer);
         procedure SetNoiseSeed(const val : Integer);
         procedure SetNoiseScale(const val : Integer);
         procedure SetNoiseAmplitude(const val : Integer);
         procedure SetSmoothness(const val : Single);

      public
         { Public Declarations }
         constructor Create(aOwner : TComponent); override;
         destructor Destroy; override;

	   published
	      { Published Declarations }
         {: Underlying texture map size, as a power of two.<p>
            Min value is 3 (size=8), max value is 9 (size=512). }
         property TexMapSize : Integer read FTexMapSize write SetTexMapSize default 5;
         {: Smoothness of the distance-based intensity.<p>
            This value is the exponent applied to the intensity in the texture,
            basically with a value of 1 (default) the intensity decreases linearly,
            with higher values, it will remain 'constant' in the center then
            fade-off more abruptly, and with values below 1, there will be a
            sharp spike in the center. }
         property Smoothness : Single read FSmoothness write SetSmoothness;
         {: Random seed to use for the perlin noise. }
         property NoiseSeed : Integer read FNoiseSeed write SetNoiseSeed default 0;
         {: Scale applied to the perlin noise (stretching). }
         property NoiseScale : Integer read FNoiseScale write SetNoiseScale default 100;
         {: Amplitude applied to the perlin noise (intensity).<p>
            This value represent the percentage of the sprite luminance affected by
            the perlin texture. }
         property NoiseAmplitude : Integer read FNoiseAmplitude write SetNoiseAmplitude default 50;

         property ColorMode default scmInner;
         property ParticleSize;
         property ColorInner;
         property ColorOuter;
         property LifeColors;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses PerlinNoise, OpenGL1x, VectorGeometry;

// ------------------
// ------------------ TGLPerlinPFXManager ------------------
// ------------------

// Create
//
constructor TGLPerlinPFXManager.Create(aOwner : TComponent);
begin
   inherited;
   FTexMapSize:=5;
   FNoiseScale:=100;
   FNoiseAmplitude:=50;
   FSmoothness:=1;
   ColorMode:=scmInner;
end;

// Destroy
//
destructor TGLPerlinPFXManager.Destroy;
begin
   inherited Destroy;
end;

// SetTexMapSize
//
procedure TGLPerlinPFXManager.SetTexMapSize(const val : Integer);
begin
   if val<>FTexMapSize then begin
      FTexMapSize:=val;
      if FTexMapSize<3 then FTexMapSize:=3;
      if FTexMapSize>9 then FTexMapSize:=9;
      NotifyChange(Self);
   end;
end;

// SetNoiseSeed
//
procedure TGLPerlinPFXManager.SetNoiseSeed(const val : Integer);
begin
   if val<>FNoiseSeed then begin
      FNoiseSeed:=val;
      NotifyChange(Self);
   end;
end;

// SetNoiseScale
//
procedure TGLPerlinPFXManager.SetNoiseScale(const val : Integer);
begin
   if val<>FNoiseScale then begin
      FNoiseScale:=val;
      NotifyChange(Self);
   end;
end;

// SetNoiseAmplitude
//
procedure TGLPerlinPFXManager.SetNoiseAmplitude(const val : Integer);
begin
   if val<>FNoiseAmplitude then begin
      FNoiseAmplitude:=val;
      if FNoiseAmplitude<0 then FNoiseAmplitude:=0;
      if FNoiseAmplitude>100 then FNoiseAmplitude:=100;
      NotifyChange(Self);
   end;
end;

// SetSmoothness
//
procedure TGLPerlinPFXManager.SetSmoothness(const val : Single);
begin
   if FSmoothness<>val then begin
      FSmoothness:=ClampValue(val, 1e-3, 1e3);
      NotifyChange(Self);
   end;
end;

// BindTexture
//
procedure TGLPerlinPFXManager.PrepareImage(bmp32 : TGLBitmap32; var texFormat : Integer);
var
   s, s2 : Integer;
   x, y, d : Integer;
   is2, f, fy, pf, nBase, nAmp : Single;
   scanLine : PGLPixel32Array;
   noise : TPerlin3DNoise;
begin
   s:=(1 shl TexMapSize);
   bmp32.Width:=s;
   bmp32.Height:=s;
   texFormat:=GL_LUMINANCE_ALPHA;
   noise:=TPerlin3DNoise.Create(0);
   try
      s2:=s shr 1;
      is2:=1/s2;
      pf:=FNoiseScale*0.05*is2;
      nAmp:=FNoiseAmplitude*(0.01*0.5);
      nBase:=1-nAmp;
      for y:=0 to s-1 do begin
         fy:=Sqr((y+0.5-s2)*is2);
         scanLine:=bmp32.ScanLine[y];
         for x:=0 to s-1 do begin
            f:=Sqr((x+0.5-s2)*is2)+fy;
            if f<1 then begin
               d:=Trunc( Power((1-Sqrt(f)), FSmoothness)
                        *(nBase+nAmp*noise.Noise(x*pf, y*pf))
                        *255);
               d:=d+(d shl 8)+(d shl 16)+(d shl 24);
            end else d:=0;
            PInteger(@scanLine[x])^:=d;
         end;
      end;
   finally
      noise.Free;
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
   RegisterClasses([TGLPerlinPFXManager]);

end.
