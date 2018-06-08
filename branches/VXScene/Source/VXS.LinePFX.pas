//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net
//
{
  A PFX whose particles are lines

}
unit VXS.LinePFX;

interface

{$I VXScene.inc}

uses
  System.Classes,
  System.SysUtils,

  VXS.OpenGL,
  VXS.PersistentClasses,
  VXS.VectorGeometry,
  VXS.ParticleFX,
  VXS.Texture,
  VXS.Color,
  VXS.RenderContextInfo,
  VXS.Context,
  VXS.VectorTypes;

type

   { Linear particle.  }
   TVXLineParticle = class (TVXParticle)
      private
         FDirection : TAffineVector;
         FLength : Single;
      protected
      public
         procedure WriteToFiler(writer : TVirtualWriter); override;
         procedure ReadFromFiler(reader : TVirtualReader); override;
         { Direction of the line. }
         property Direction : TAffineVector read FDirection write FDirection;
         { Length of the line }
         property Length : Single read FLength write FLength;
   end;

   { Polygonal particles FX manager.
      The particles of this manager are made of N-face regular polygon with
      a core and edge color. No texturing is available.
      If you render large particles and don't have T&L acceleration, consider
      using TVXPointLightPFXManager. }
   TVXLinePFXManager = class (TVXLifeColoredPFXManager)
      private
         Fvx, Fvy : TAffineVector;        // NOT persistent
         FNvx, FNvy : TAffineVector;        // NOT persistent
         FDefaultLength : Single;
      protected
         function StoreDefaultLength : Boolean;
         function TexturingMode : Cardinal; override;
         procedure InitializeRendering(var rci: TVXRenderContextInfo); override;
         procedure BeginParticles(var rci: TVXRenderContextInfo); override;
         procedure RenderParticle(var rci: TVXRenderContextInfo; aParticle : TVXParticle); override;
         procedure EndParticles(var rci: TVXRenderContextInfo); override;
         procedure FinalizeRendering(var rci: TVXRenderContextInfo); override;
      public
         constructor Create(aOwner : TComponent); override;
         destructor Destroy; override;
         class function ParticlesClass : TVXParticleClass; override;
         function CreateParticle : TVXParticle; override;
	   published
         property DefaultLength : Single read FDefaultLength write FDefaultLength stored StoreDefaultLength;
         property ParticleSize;
         property ColorInner;
         property ColorOuter;
         property LifeColors;
   end;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

// ------------------
// ------------------ TVXLinePFXManager ------------------
// ------------------

constructor TVXLinePFXManager.Create(aOwner : TComponent);
begin
   inherited;
   FDefaultLength:=1;
end;

destructor TVXLinePFXManager.Destroy;
begin
   inherited Destroy;
end;

class function TVXLinePFXManager.ParticlesClass : TVXParticleClass;
begin
   Result:=TVXLineParticle;
end;

function TVXLinePFXManager.CreateParticle : TVXParticle;
begin
   Result:=inherited CreateParticle;
   TVXLineParticle(Result).FLength:=DefaultLength;
end;

function TVXLinePFXManager.TexturingMode : Cardinal;
begin
   Result:=0;
end;

procedure TVXLinePFXManager.InitializeRendering(var rci: TVXRenderContextInfo);
var
   i : Integer;
   matrix : TMatrix;
begin
   inherited;
   glGetFloatv(GL_MODELVIEW_MATRIX, @matrix);
   for i:=0 to 2 do begin
      Fvx.V[i]:=matrix.V[i].X;
      Fvy.V[i]:=matrix.V[i].Y;
   end;
   FNvx:=VectorNormalize(Fvx);
   FNvy:=VectorNormalize(Fvy);
end;

procedure TVXLinePFXManager.BeginParticles(var rci: TVXRenderContextInfo);
begin
   ApplyBlendingMode(rci);
end;

procedure TVXLinePFXManager.RenderParticle(var rci: TVXRenderContextInfo; aParticle : TVXParticle);
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

   with TVXLineParticle(aParticle) do begin
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
      glVertex3f(start.X+dv.X, start.Y+dv.Y, start.Z+dv.Z);
      glVertex3f(stop.X+dv.X, stop.Y+dv.Y, stop.Z+dv.Z);
      glColor4fv(@inner);
      glVertex3fv(@stop);
      glColor4fv(@outer);
      glVertex3f(stop.X-dv.X, stop.Y-dv.Y, stop.Z-dv.Z);
      glVertex3f(start.X-dv.X, start.Y-dv.Y, start.Z-dv.Z);
   glEnd;
end;

procedure TVXLinePFXManager.EndParticles(var rci: TVXRenderContextInfo);
begin
   UnapplyBlendingMode(rci);
end;

procedure TVXLinePFXManager.FinalizeRendering(var rci: TVXRenderContextInfo);
begin
   inherited;
end;

function TVXLinePFXManager.StoreDefaultLength : Boolean;
begin
   Result:=(FDefaultLength<>1);
end;

// ------------------
// ------------------ TVXLineParticle ------------------
// ------------------

procedure TVXLineParticle.WriteToFiler(writer : TVirtualWriter);
begin
   inherited WriteToFiler(writer);
   with writer do begin
      WriteInteger(0); // Archive Version 0
      Write(FDirection, SizeOf(FDirection));
      WriteFloat(FLength);
   end;
end;

procedure TVXLineParticle.ReadFromFiler(reader : TVirtualReader);
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
initialization
// ------------------------------------------------------------------

   // class registrations
   RegisterClasses([TVXLineParticle, TVXLinePFXManager]);

end.
