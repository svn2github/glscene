//
// This unit is part of the GLScene Project   
//
{: VKS.LinePFX<p>

   A PFX whose particles are lines

   <b>History : </b><font size=-1><ul>
      <li>10/11/12 - PW - Added CPP compatibility: changed vector arrays to records
      <li>23/08/10 - Yar - Added VKS.OpenGLTokens to uses, replaced OpenGL1x functions to OpenGLAdapter
      <li>22/04/10 - Yar - Fixes after VKS.State revision
      <li>05/03/10 - DanB - More state added to TVKStateCache
      <li>12/10/08 - DanB - updated to use RCI
      <li>06/06/07 - DaStr - Added VKS.Color to uses (BugtrackerID = 1732211)
      <li>30/03/07 - DaStr - Added $I GLScene.inc
      <li>20/02/05 - EG - Creation
   </ul></font>
}
unit VKS.LinePFX;

interface

{$I VKScene.inc}

uses
  System.Classes, System.SysUtils,

  VKS.PersistentClasses, VKS.VectorGeometry,
  VKS.ParticleFX, VKS.Texture, VKS.Color, VKS.RenderContextInfo,
  VKS.OpenGLTokens, VKS.Context , VKS.VectorTypes;

type

   // TVKLineParticle
   //
   {: Linear particle.<p> }
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

         {: Direction of the line. }
         property Direction : TAffineVector read FDirection write FDirection;
         {: Length of the line }
         property Length : Single read FLength write FLength;
   end;

   // TVKLinePFXManager
   //
   {: Polygonal particles FX manager.<p>
      The particles of this manager are made of N-face regular polygon with
      a core and edge color. No texturing is available.<br>
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
         procedure InitializeRendering(var rci: TRenderContextInfo); override;
         procedure BeginParticles(var rci: TRenderContextInfo); override;
         procedure RenderParticle(var rci: TRenderContextInfo; aParticle : TVKParticle); override;
         procedure EndParticles(var rci: TRenderContextInfo); override;
         procedure FinalizeRendering(var rci: TRenderContextInfo); override;

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
procedure TVKLinePFXManager.InitializeRendering(var rci: TRenderContextInfo);
var
   i : Integer;
   matrix : TMatrix;
begin
   inherited;
   GL.GetFloatv(GL_MODELVIEW_MATRIX, @matrix);
   for i:=0 to 2 do begin
      Fvx.V[i]:=matrix.V[i].V[0];
      Fvy.V[i]:=matrix.V[i].V[1];
   end;
   FNvx:=VectorNormalize(Fvx);
   FNvy:=VectorNormalize(Fvy);
end;

// BeginParticles
//
procedure TVKLinePFXManager.BeginParticles(var rci: TRenderContextInfo);
begin
   ApplyBlendingMode(rci);
end;

// RenderParticle
//
procedure TVKLinePFXManager.RenderParticle(var rci: TRenderContextInfo; aParticle : TVKParticle);
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

   GL.Begin_(GL_TRIANGLE_FAN);
      GL.Color4fv(@inner);
      GL.Vertex3fv(@start);
      GL.Color4fv(@outer);
      GL.Vertex3f(start.V[0]+dv.V[0], start.V[1]+dv.V[1], start.V[2]+dv.V[2]);
      GL.Vertex3f(stop.V[0]+dv.V[0], stop.V[1]+dv.V[1], stop.V[2]+dv.V[2]);
      GL.Color4fv(@inner);
      GL.Vertex3fv(@stop);
      GL.Color4fv(@outer);
      GL.Vertex3f(stop.V[0]-dv.V[0], stop.V[1]-dv.V[1], stop.V[2]-dv.V[2]);
      GL.Vertex3f(start.V[0]-dv.V[0], start.V[1]-dv.V[1], start.V[2]-dv.V[2]);
   GL.End_;
end;

// EndParticles
//
procedure TVKLinePFXManager.EndParticles(var rci: TRenderContextInfo);
begin
   UnapplyBlendingMode(rci);
end;

// FinalizeRendering
//
procedure TVKLinePFXManager.FinalizeRendering(var rci: TRenderContextInfo);
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
