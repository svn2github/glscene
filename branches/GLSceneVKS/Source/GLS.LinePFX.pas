//
// GLScene on Vulkan, http://glscene.sourceforge.net 
//
{
  A PFX whose particles are lines
    
}
unit GLS.LinePFX;

interface

{$I GLScene.inc}

uses
  System.Classes, System.SysUtils,

  GLS.PersistentClasses, GLS.VectorGeometry,
  GLS.ParticleFX, GLS.Texture, GLS.Color, GLS.RenderContextInfo,
  GLS.OpenGLTokens, GLS.Context , GLS.VectorTypes;

type

   // TVKLineParticle
   //
   { Linear particle.  }
   TVKLineParticle = class (TVKParticle)
      private
         { Private Declarations }
         FDirection : TAffineVector;
         FLength : Single;

      protected
         { Protected Declarations }

      public
         { Public Declarations }
         procedure WriteToFiler(writer : TVirtualWriter); override;
         procedure ReadFromFiler(reader : TVirtualReader); override;

         { Direction of the line. }
         property Direction : TAffineVector read FDirection write FDirection;
         { Length of the line }
         property Length : Single read FLength write FLength;
   end;

   // TVKLinePFXManager
   //
   { Polygonal particles FX manager. 
      The particles of this manager are made of N-face regular polygon with
      a core and edge color. No texturing is available. 
      If you render large particles and don't have T&L acceleration, consider
      using TVKPointLightPFXManager. }
   TVKLinePFXManager = class (TVKLifeColoredPFXManager)
      private
         { Private Declarations }
         Fvx, Fvy : TAffineVector;        // NOT persistent
         FNvx, FNvy : TAffineVector;        // NOT persistent
         FDefaultLength : Single;

      protected
         { Protected Declarations }
         function StoreDefaultLength : Boolean;

         function TexturingMode : Cardinal; override;
         procedure InitializeRendering(var rci: TVKRenderContextInfo); override;
         procedure BeginParticles(var rci: TVKRenderContextInfo); override;
         procedure RenderParticle(var rci: TVKRenderContextInfo; aParticle : TVKParticle); override;
         procedure EndParticles(var rci: TVKRenderContextInfo); override;
         procedure FinalizeRendering(var rci: TVKRenderContextInfo); override;

      public
         { Public Declarations }
         constructor Create(aOwner : TComponent); override;
         destructor Destroy; override;

         class function ParticlesClass : TVKParticleClass; override;
         function CreateParticle : TVKParticle; override;

	   published
	      { Published Declarations }
         property DefaultLength : Single read FDefaultLength write FDefaultLength stored StoreDefaultLength;

         property ParticleSize;
         property ColorInner;
         property ColorOuter;
         property LifeColors;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------
// ------------------ TVKLinePFXManager ------------------
// ------------------

// Create
//
constructor TVKLinePFXManager.Create(aOwner : TComponent);
begin
   inherited;
   FDefaultLength:=1;
end;

// Destroy
//
destructor TVKLinePFXManager.Destroy;
begin
   inherited Destroy;
end;

// ParticlesClass
//
class function TVKLinePFXManager.ParticlesClass : TVKParticleClass;
begin
   Result:=TVKLineParticle;
end;

// CreateParticle
//
function TVKLinePFXManager.CreateParticle : TVKParticle;
begin
   Result:=inherited CreateParticle;
   TVKLineParticle(Result).FLength:=DefaultLength;
end;

// TexturingMode
//
function TVKLinePFXManager.TexturingMode : Cardinal;
begin
   Result:=0;
end;

// InitializeRendering
//
procedure TVKLinePFXManager.InitializeRendering(var rci: TVKRenderContextInfo);
var
   i : Integer;
   matrix : TMatrix;
begin
   inherited;
   glGetFloatv(GL_MODELVIEW_MATRIX, @matrix);
   for i:=0 to 2 do begin
      Fvx.V[i]:=matrix.V[i].V[0];
      Fvy.V[i]:=matrix.V[i].V[1];
   end;
   FNvx:=VectorNormalize(Fvx);
   FNvy:=VectorNormalize(Fvy);
end;

// BeginParticles
//
procedure TVKLinePFXManager.BeginParticles(var rci: TVKRenderContextInfo);
begin
   ApplyBlendingMode(rci);
end;

// RenderParticle
//
procedure TVKLinePFXManager.RenderParticle(var rci: TVKRenderContextInfo; aParticle : TVKParticle);
var
   lifeTime, sizeScale, fx, fy, f : Single;
   inner, outer : TColorVector;
   pos, dir, start, stop, dv : TAffineVector;
begin
   lifeTime:=CurrentTime-aParticle.CreationTime;
   ComputeColors(lifeTime, inner, outer);
   if ComputeSizeScale(lifeTime, sizeScale) then
      sizeScale:=sizeScale*ParticleSize
   else sizeScale:=ParticleSize;

   pos:=aParticle.Position;

   with TVKLineParticle(aParticle) do begin
      dir:=VectorNormalize(aParticle.Velocity);
      f:=Length*0.5;
   end;

   start:=VectorCombine(pos, dir, 1, f);
   stop:=VectorCombine(pos, dir, 1, -f);

   fx:=VectorDotProduct(dir, FNvy)*sizeScale;
   fy:=-VectorDotProduct(dir, FNvx)*sizeScale;

   dv:=VectorCombine(Fvx, Fvy, fx, fy);

   glBegin(GL_TRIANGLE_FAN);
      glColor4fv(@inner);
      glVertex3fv(@start);
      glColor4fv(@outer);
      glVertex3f(start.V[0]+dv.V[0], start.V[1]+dv.V[1], start.V[2]+dv.V[2]);
      glVertex3f(stop.V[0]+dv.V[0], stop.V[1]+dv.V[1], stop.V[2]+dv.V[2]);
      glColor4fv(@inner);
      glVertex3fv(@stop);
      glColor4fv(@outer);
      glVertex3f(stop.V[0]-dv.V[0], stop.V[1]-dv.V[1], stop.V[2]-dv.V[2]);
      glVertex3f(start.V[0]-dv.V[0], start.V[1]-dv.V[1], start.V[2]-dv.V[2]);
   glEnd;
end;

// EndParticles
//
procedure TVKLinePFXManager.EndParticles(var rci: TVKRenderContextInfo);
begin
   UnapplyBlendingMode(rci);
end;

// FinalizeRendering
//
procedure TVKLinePFXManager.FinalizeRendering(var rci: TVKRenderContextInfo);
begin
   inherited;
end;

// StoreDefaultLength
//
function TVKLinePFXManager.StoreDefaultLength : Boolean;
begin
   Result:=(FDefaultLength<>1);
end;

// ------------------
// ------------------ TVKLineParticle ------------------
// ------------------

// WriteToFiler
//
procedure TVKLineParticle.WriteToFiler(writer : TVirtualWriter);
begin
   inherited WriteToFiler(writer);
   with writer do begin
      WriteInteger(0); // Archive Version 0
      Write(FDirection, SizeOf(FDirection));
      WriteFloat(FLength);
   end;
end;

// ReadFromFiler
//
procedure TVKLineParticle.ReadFromFiler(reader : TVirtualReader);
var
   archiveVersion : integer;
begin
   inherited ReadFromFiler(reader);
   archiveVersion:=reader.ReadInteger;
   if archiveVersion=0 then with reader do begin
      Read(FDirection, SizeOf(FDirection));
      FLength:=ReadFloat;
   end else RaiseFilerException(archiveVersion);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   // class registrations
   RegisterClasses([TVKLineParticle, TVKLinePFXManager]);

end.
