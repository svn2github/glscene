// GLLensFlare
{: Lens flare object.<p>

	<b>History : </b><font size=-1><ul>
      <li>18/08/03 - SG - Added TGLTextureLensFlare (Tobias Peirick)
      <li>26/03/03 - EG - Framerate independant glow transitions (Tobias Peirick)
      <li>08/12/02 - EG - Added AutoZTest
      <li>29/10/02 - EG - Initial, added defaults and encapsulation,
                          fixed positionning, RandSeed now preserved,
                          minor speedup
	</ul></font><p>
   
   Author  : Tobias Peirick<br>
   eMail   : peirick@onlinehome.de<br>
   Homepage: http://www.TobSoft.de
}
unit GLLensFlare;

interface

uses
   Classes, GLScene, Geometry, GLObjects, GLTexture, OpenGL1x, GLMisc;

type

   // TFlareElement
   //
   TFlareElement = (feGlow, feRing, feStreaks, feRays, feSecondaries);
   TFlareElements = set of TFlareElement;

   {: The actual gradients between two colors are, of course, calculated by OpenGL.<p>
      The start and end colors of a gradient are stored to represent the color of
      lens flare elements. }
   TGradient = record
      CFrom, CTo: TColorVector;
   end;

const
   cDefaultFlareElements = [feGlow, feRing, feStreaks, feRays, feSecondaries];

type
   // TGLLensFlare
   //
  TGLLensFlare = class(TGLBaseSceneObject)
      private
         { Private Declarations }
         FSize        : Integer;
         FDeltaTime   : Double;
         FCurrSize    : Double;
         FSeed        : Integer;
         FSqueeze     : Single;
         FNumStreaks  : Integer;
         FStreakWidth : Single;
         FNumSecs     : Integer;
         FResolution  : Integer;
         FAutoZTest   : Boolean;
         FElements    : TFlareElements;

      protected
         { Protected Declarations }
         procedure SetSize(aValue : Integer);
         procedure SetSeed(aValue : Integer);
         procedure SetSqueeze(aValue : Single);
         function  StoreSqueeze : Boolean;
         procedure SetNumStreaks(aValue : Integer);
         procedure SetStreakWidth(aValue : Single);
         function  StoreStreakWidth : Boolean;
         procedure SetNumSecs(aValue : Integer);
         procedure SetResolution(aValue : Integer);
         procedure SetAutoZTest(aValue : Boolean);
         procedure SetElements(aValue : TFlareElements);

      public
         { Public Declarations }
         Gradients: array [TFlareElement] of TGradient;  // And what color should they have?
         constructor Create(AOwner: TComponent); override;

         procedure BuildList(var rci : TRenderContextInfo); override;
         procedure DoProgress(const progressTime: TProgressTimes); override;

      published
         { Public Declarations }
         //: MaxRadius of the flare.
         property Size : Integer read FSize write SetSize default 50;
         //: Random seed
         property Seed : Integer read FSeed write SetSeed;
         //: To create elliptic flares.
         property Squeeze : Single read FSqueeze write SetSqueeze stored StoreSqueeze;
         //: Number of streaks.
         property NumStreaks : Integer read FNumStreaks write SetNumStreaks default 4;
         //: Width of the streaks.
         property StreakWidth : Single read FStreakWidth write SetStreakWidth stored StoreStreakWidth;
         //: Number of secondary flares.
         property NumSecs : Integer read FNumSecs write SetNumSecs default 8;
         //: Number of segments used when rendering circles.
         property Resolution : Integer read FResolution write SetResolution default 64;
         property AutoZTest : Boolean read FAutoZTest write SetAutoZTest default True;
         //: Which elements should be rendered?
         property Elements : TFlareElements read FElements write SetElements default cDefaultFlareElements;
         
         property ObjectsSorting;
         property Position;
         property Visible;
         property OnProgress;
         property Behaviours;
         property Effects;
   end;

   // TGLTextureLensFlare
   //
   TGLTextureLensFlare = class(TGLBaseSceneObject)
      private
         { Private Declarations }
         FSize: integer;
         FCurrSize: Single;
         FNumSecs: integer;
         FAutoZTest: boolean;
         //used for internal calculation
         FDeltaTime : Double;
         FImgSecondaries: TGLTexture;
         FImgRays: TGLTexture;
         FImgRing: TGLTexture;
         FImgGlow: TGLTexture;
         FSeed: Integer;
         procedure SetImgGlow(const Value: TGLTexture);
         procedure SetImgRays(const Value: TGLTexture);
         procedure SetImgRing(const Value: TGLTexture);
         procedure SetImgSecondaries(const Value: TGLTexture);
         procedure SetSeed(const Value: Integer);
      protected
         { Protected Declarations }
         procedure SetSize(aValue: integer);
         procedure SetNumSecs(aValue: integer);
         procedure SetAutoZTest(aValue: boolean);
      public
         { Public Declarations }    
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;
         procedure BuildList(var rci: TRenderContextInfo); override;
         procedure DoProgress(const progressTime : TProgressTimes); override;
      published
         { Public Declarations }
         //: MaxRadius of the flare.
         property Size: integer read FSize write SetSize default 50;
         //: Random seed
         property Seed : Integer read FSeed write SetSeed;
         //: Number of secondary flares.
         property NumSecs: integer read FNumSecs write SetNumSecs default 8;
         //: Number of segments used when rendering circles.
         //property Resolution: integer read FResolution write SetResolution default 64;
         property AutoZTest: boolean read FAutoZTest write SetAutoZTest default True;
         // The Textures
         property ImgGlow: TGLTexture read FImgGlow write SetImgGlow;
         property ImgRays: TGLTexture read FImgRays write SetImgRays;
         property ImgRing: TGLTexture read FImgRing write SetImgRing;
         property ImgSecondaries: TGLTexture read FImgSecondaries write SetImgSecondaries;

         property ObjectsSorting;
         property Position;
         property Visible;
         property OnProgress;
         property Behaviours;
         property Effects;
   end;


// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TLensFlare ------------------
// ------------------

constructor TGLLensFlare.Create;

    function lfGradient(c1, c2: TColorVector): TGradient;
    begin
      with Result do
      begin
        CFrom := c1;
        CTo := c2;
      end;
    end;

begin
  inherited;
  // Set default parameters:
  ObjectStyle:=ObjectStyle+[osDirectDraw, osNoVisibilityCulling];
  FSize := 50;
  FSeed := 1465;
  FSqueeze := 1;
  FNumStreaks := 4;
  FStreakWidth := 2;
  FNumSecs := 8;
  FResolution := 64;
  FAutoZTest := True;
  // Render all elements by default.
  FElements := [feGlow, feRing, feStreaks, feRays, feSecondaries];
  // Setup default gradients:
  Gradients[feGlow] := lfGradient(VectorMake(1, 1, 0.8, 0.3), VectorMake(1, 0.2, 0, 0));
  Gradients[feRing] := lfGradient(VectorMake(0.5, 0.2, 0, 0.1), VectorMake(0.5, 0.4, 0, 0.1));
  Gradients[feStreaks] := lfGradient(VectorMake(1, 1, 1, 0.2), VectorMake(0.2, 0, 1, 0));
  Gradients[feRays] := lfGradient(VectorMake(1, 0.8, 0.5, 0.05), VectorMake(0.5, 0.2, 0, 0));
  Gradients[feSecondaries] := lfGradient(VectorMake(0.5, 0.5, 0.5, 0.5), VectorMake(0.5, 0.5, 0.5, 0.5));
  Gradients[feSecondaries] := lfGradient(VectorMake(0, 0.2, 1, 0), VectorMake(0, 0.8, 0.2, 0.15));
end;

procedure TGLLensFlare.SetSize( aValue : Integer);
begin
  FSize := aValue;
  StructureChanged;
end;

procedure TGLLensFlare.SetSeed( aValue : Integer);
begin
  FSeed := aValue;
  StructureChanged;
end;

procedure TGLLensFlare.SetSqueeze( aValue : Single);
begin
  FSqueeze := aValue;
  StructureChanged;
end;

// StoreSqueeze
//
function TGLLensFlare.StoreSqueeze : Boolean;
begin
   Result:=(FSqueeze<>1);
end;

procedure TGLLensFlare.SetNumStreaks(aValue : Integer);
begin
  FNumStreaks := aValue;
  StructureChanged;
end;

procedure TGLLensFlare.SetStreakWidth(aValue : Single);
begin
  FStreakWidth := aValue;
  StructureChanged;
end;

// StoreStreakWidth
//
function TGLLensFlare.StoreStreakWidth : Boolean;
begin
   Result:=(FStreakWidth<>2);
end;

procedure TGLLensFlare.SetNumSecs(aValue : Integer);
begin
  FNumSecs := aValue;
  StructureChanged;
end;

procedure TGLLensFlare.SetResolution(aValue : Integer);
begin
  FResolution := aValue;
  StructureChanged;
end;

// SetAutoZTest
//
procedure TGLLensFlare.SetAutoZTest(aValue : Boolean);
begin
   if FAutoZTest<>aValue then begin
      FAutoZTest:=aValue;
      StructureChanged;
   end;
end;

// SetElements
//
procedure TGLLensFlare.SetElements(aValue : TFlareElements);
begin
   if FElements<>aValue then begin
      FElements:=aValue;
      StructureChanged;
   end;
end;

// BuildList
//
procedure TGLLensFlare.BuildList(var rci : TRenderContextInfo);
var
   i, j : Integer;
   rW   : Single;
   a, s, c, f : Single;
   rnd  : Single;
   posVector, v, rv : TAffineVector;
   screenPos : TAffineVector;
   depth     : Single;
   flag      : Boolean;
   oldSeed : LongInt;
begin
   SetVector(v, AbsolutePosition);
   // are we looking towards the flare?
   rv:=VectorSubtract(v, PAffineVector(@rci.cameraPosition)^);
   if VectorDotProduct(rci.cameraDirection, rv)>0 then begin
      // find out where it is on the screen.
      screenPos:=Scene.CurrentBuffer.WorldToScreen(v);
      if     (screenPos[0]<rci.viewPortSize.cx) and (screenPos[0]>=0)
         and (screenPos[1]<rci.viewPortSize.cy) and (screenPos[1]>=0) then begin
         if FAutoZTest then begin
            depth:=Scene.CurrentBuffer.GetPixelDepth(Round(ScreenPos[0]),
                                                     Round(rci.viewPortSize.cy-ScreenPos[1]));
            // but is it behind something?
            if screenPos[2]>=1 then
               flag:=(depth>=1)
            else flag:=(depth>=screenPos[2]);
         end else flag:=True;
      end else flag:=False;
   end else flag:=False;

   MakeVector(posVector,
              screenPos[0]-rci.viewPortSize.cx*0.5,
              screenPos[1]-rci.viewPortSize.cy*0.5,
              0);

   // make the glow appear/disappear progressively
   if Flag then begin
      if FCurrSize<Size then
         FCurrSize:=FCurrSize+FDeltaTime*500;
   end else begin
      if FCurrSize>0 then
         FCurrSize:=FCurrSize-FDeltaTime*500;
   end;
   if FCurrSize>0 then begin

      // Random seed must be backed up, could be used for other purposes
      // (otherwise we essentially reset the random generator at each frame)
      oldSeed:=RandSeed;
      RandSeed:=Seed;

      // Prepare matrices
      glMatrixMode(GL_MODELVIEW);
      glPushMatrix;
      glLoadMatrixf(@Scene.CurrentBuffer.BaseProjectionMatrix);

      glMatrixMode(GL_PROJECTION);
      glPushMatrix;
      glLoadIdentity;
      glPushAttrib(GL_ENABLE_BIT);
      glDisable(GL_LIGHTING);
      glDisable(GL_DEPTH_TEST);
      glDepthMask(False);
      glEnable(GL_BLEND);
      glScalef(2/rci.viewPortSize.cx, 2/rci.viewPortSize.cy, 1);

      glPushMatrix;
      glTranslatef(posVector[0], posVector[1], posVector[2]);

      // Glow (a circle with transparent edges):
      if feGlow in Elements then begin
         glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
         glBegin(GL_TRIANGLE_FAN);
         glColor4fv(@Gradients[feGlow].CFrom);
         glVertex2f(0, 0);
         glColor4fv(@Gradients[feGlow].CTo);
         f:=2*pi/Resolution;
         for i:=0 to Resolution do begin
            SinCos(i*f, s, c);
            glVertex2f(FCurrSize*c, Squeeze*FCurrSize*s);
         end;
         glEnd;
      end;

      // Use additive blending from now on.
      glBlendFunc(GL_SRC_ALPHA, GL_ONE);
      // Streaks (randomly oriented, but evenly spaced, antialiased lines from the origin):
      if feStreaks in Elements then begin
         glEnable(GL_LINE_SMOOTH);
         glLineWidth(StreakWidth);
         a:=2*pi/NumStreaks;
         rnd:=Random*(pi/NumStreaks);
         f:=1.5*FCurrSize;
         glBegin(GL_LINES);
         for i:=0 to NumStreaks-1 do begin
            SinCos(rnd+a*i, f, s, c);
            glColor4fv(@Gradients[feStreaks].CFrom);
            glVertex2f(0, 0);
            glColor4fv(@Gradients[feStreaks].CTo);
            glVertex2f(c, Squeeze*s);
         end;
         glEnd;
         glDisable(GL_LINE_SMOOTH);
      end;

      // Rays (random-length lines from the origin):
      if feRays in Elements then begin
         glLineWidth(1);
         glBegin(GL_LINES);
         f:=2*pi/(Resolution*20);
         for i:=1 to Resolution*20 do begin
            if Odd(i) then
               rnd:=1.5*Random
            else rnd:=Random;
            SinCos(i*f, FCurrSize*rnd, s, c);
            glColor4fv(@Gradients[feRays].CFrom);
            glVertex2f(0, 0);
            glColor4fv(@Gradients[feRays].CTo);
            glVertex2f(c, s*Squeeze);
         end;
         glEnd;
      end;
      glPopMatrix;

      // Ring (Three circles, the outer two are transparent):
      if feRing in Elements then begin
         rW := FCurrSize / 15;  // Ring width
         glPushMatrix;
         glTranslatef(PosVector[0],PosVector[1],PosVector[2]);
         glScalef(0.6, 0.6, 1);
         glBegin(GL_QUADS);
         for i:=0 to Resolution - 1 do begin
            SinCos(2*i*pi/Resolution, s, c);
            glColor4fv(@Gradients[feGlow].CTo);
            glVertex2f((FCurrSize-rW)*c, Squeeze*(FCurrSize-rW)*s);
            glColor4fv(@Gradients[feRing].CFrom);
            glVertex2f(FCurrSize*c, Squeeze*FCurrSize*s);

            SinCos(2*(i+1)*pi/Resolution, s, c);
            glVertex2f(FCurrSize*c, Squeeze*FCurrSize*s);
            glColor4fv(@Gradients[feGlow].CTo);
            glVertex2f((FCurrSize-rW)*c, Squeeze*(FCurrSize-rW)*s);

            glColor4fv(@Gradients[feRing].CFrom);
            glVertex2f(FCurrSize * cos(2*i*pi/Resolution),
                       Squeeze * FCurrSize * sin(2*i*pi/Resolution));
            glVertex2f(FCurrSize * cos(2*(i+1)*pi/Resolution),
                       Squeeze * FCurrSize * sin(2*(i+1)*pi/Resolution));
            glColor4fv(@Gradients[feGlow].CTo);
            glVertex2f((FCurrSize+rW) * cos(2*(i+1)*pi/Resolution),
                       Squeeze * (FCurrSize+rW) * sin(2*(i+1)*pi/Resolution));
            glVertex2f((FCurrSize+rW) * cos(2*i*pi/Resolution),
                       Squeeze * (FCurrSize+rW) * sin(2*i*pi/Resolution));
         end;
         glEnd;
         glPopMatrix;
      end;

      if feSecondaries in Elements then begin
         // Other secondaries (plain gradiented circles, like the glow):
         for j:=1 to NumSecs do begin
            rnd := 2 * Random - 1;
            { If rnd < 0 then the secondary glow will end up on the other side of the
              origin. In this case, we can push it really far away from the flare. If
              the secondary is on the flare's side, we pull it slightly towards the
              origin to avoid it winding up in the middle of the flare. }
            v:=PosVector;
            if rnd<0 then
               ScaleVector(V, rnd)
            else ScaleVector(V, 0.8*rnd);
            glPushMatrix;
            glTranslatef(v[0], v[1],v[2]);
            glBegin(GL_TRIANGLE_FAN);
            if j mod 3 = 0 then begin
               glColor4fv(@Gradients[feGlow].CFrom);
               glVertex2f(0, 0);
               glColor4fv(@Gradients[feGlow].CTo);
            end else begin
               glColor4fv(@Gradients[feSecondaries].CFrom);
               glVertex2f(0, 0);
               glColor4fv(@Gradients[feSecondaries].CTo);
            end;
            rnd:=(Random+0.1)*FCurrSize*0.25;
            f:=2*pi/Resolution;
            for i:=0 to Resolution do begin
               SinCos(i*f, rnd, s, c);
               glVertex2f(c, s);
            end;
            glEnd;
            glPopMatrix;
         end;
      end;

      // restore state
      glDepthMask(True);
      glPopAttrib;
      glPopMatrix;
      glMatrixMode(GL_MODELVIEW);
      glPopMatrix;

      RandSeed:=oldSeed;

   end else FCurrSize:=0;

   if Count>0 then
      Self.RenderChildren(0, Count-1, rci);
end;

// DoProgress
//
procedure TGLLensFlare.DoProgress(const progressTime: TProgressTimes);
begin
  inherited;
  if AutoZTest then
     FDeltaTime := progressTime.deltaTime;
end;


// ------------------
// ------------------ TGLTextureLensFlare ------------------
// ------------------

constructor TGLTextureLensFlare.Create;
begin
  inherited;
  Randomize;
  FSeed := Random(2000)+465;
  
  // Set default parameters:
  ObjectStyle := ObjectStyle + [osDirectDraw, osNoVisibilityCulling];
  FSize := 50;
  FCurrSize := FSize;
  FNumSecs := 8;
  FAutoZTest := True;

  FImgRays := TGLTexture.Create(Self);
  FImgSecondaries := TGLTexture.Create(Self);
  FImgRing := TGLTexture.Create(Self);
  FImgGlow := TGLTexture.Create(Self);
end;

procedure TGLTextureLensFlare.SetSize(aValue: integer);
begin
  if FSize <> aValue then
  begin
    FSize := aValue;
    FCurrSize := FSize;
    StructureChanged;
  end;
end;


procedure TGLTextureLensFlare.SetNumSecs(aValue: integer);
begin
  if FNumSecs <> aValue then
  begin
    FNumSecs := aValue;
    StructureChanged;
  end;
end;

// SetAutoZTest
//
procedure TGLTextureLensFlare.SetAutoZTest(aValue: boolean);
begin
  if FAutoZTest <> aValue then
  begin
    FAutoZTest := aValue;
    StructureChanged;
  end;
end;

// BuildList
//
procedure TGLTextureLensFlare.BuildList(var rci: TRenderContextInfo);
var
  v, rv, screenPos, posVector : TAffineVector;
  depth, rnd : Single;
  flag : Boolean;
  i : Integer;
begin
  SetVector(v, AbsolutePosition);
  // are we looking towards the flare?
  rv := VectorSubtract(v, PAffineVector(@rci.cameraPosition)^);
  if VectorDotProduct(rci.cameraDirection, rv) > 0 then
  begin
    // find out where it is on the screen.
    screenPos := Scene.CurrentBuffer.WorldToScreen(v);
    if (screenPos[0] < rci.viewPortSize.cx) and (screenPos[0] >= 0)
      and (screenPos[1] < rci.viewPortSize.cy) and (screenPos[1] >= 0) then
    begin
      if FAutoZTest then
      begin
        depth := Scene.CurrentBuffer.GetPixelDepth(Round(ScreenPos[0]),
          Round(rci.viewPortSize.cy - ScreenPos[1]));
        // but is it behind something?
        if screenPos[2] >= 1 then
          flag := (depth >= 1)
        else
          flag := (depth >= screenPos[2]);
      end
      else
        flag := True;
    end
    else
      flag := False;
  end
  else
    flag := False;

  MakeVector(posVector,
    screenPos[0] - rci.viewPortSize.cx / 2,
    screenPos[1] - rci.viewPortSize.cy / 2,0);

  // make the glow appear/disappear progressively

  if Flag then
    if FCurrSize < FSize then
       FCurrSize := FCurrSize + FDeltaTime * 200{FSize * 4};
  if not Flag then
    if FCurrSize > 0 then
       FCurrSize := FCurrSize - FDeltaTime * 200{FSize * 4};
  if FCurrSize <= 0 then Exit;


  // Prepare matrices
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix;
  glLoadMatrixf(@Scene.CurrentBuffer.BaseProjectionMatrix);

  glMatrixMode(GL_PROJECTION);
  glPushMatrix;
  glLoadIdentity;
  glScalef(2 / rci.viewPortSize.cx, 2 / rci.viewPortSize.cy, 1);


  glPushAttrib(GL_ENABLE_BIT);
  glDisable(GL_LIGHTING);
  glDisable(GL_DEPTH_TEST);  
  glEnable(GL_BLEND);
  glBlendFunc(GL_ONE,GL_ONE);

  //Rays and Glow on Same Position
  glPushMatrix;
  glTranslatef(posVector[0], posVector[1], posVector[2]);

  if not ImgGlow.Disabled and Assigned(ImgGlow.Image) then
  begin
        ImgGlow.Apply(rci);
        glbegin(GL_QUADS);
          glTexCoord2f(0,0);glVertex3f(-FCurrSize,-FCurrSize,0);
          glTexCoord2f(1,0);glVertex3f( FCurrSize,-FCurrSize,0);
          glTexCoord2f(1,1);glVertex3f( FCurrSize, FCurrSize,0);
          glTexCoord2f(0,1);glVertex3f(-FCurrSize, FCurrSize,0);
        glend;
        ImgGlow.UnApply(rci);
  end;

  if not ImgRays.Disabled and Assigned(ImgRays.Image) then
  begin
        ImgRays.Apply(rci);
        glbegin(GL_QUADS);
          glTexCoord2f(0,0);glVertex3f(-FCurrSize,-FCurrSize,0);
          glTexCoord2f(1,0);glVertex3f( FCurrSize,-FCurrSize,0);
          glTexCoord2f(1,1);glVertex3f( FCurrSize, FCurrSize,0);
          glTexCoord2f(0,1);glVertex3f(-FCurrSize, FCurrSize,0);
        glend;
        ImgRays.UnApply(rci);
  end;
  glPopMatrix;

  if not ImgRing.Disabled and Assigned(ImgRing.Image) then
  begin
        glPushMatrix;
          glTranslatef(posVector[0]*1.1, posVector[1]*1.1, posVector[2]);
          ImgRing.Apply(rci);
          glbegin(GL_QUADS);
            glTexCoord2f(0,0);glVertex3f(-FCurrSize,-FCurrSize,0);
            glTexCoord2f(1,0);glVertex3f( FCurrSize,-FCurrSize,0);
            glTexCoord2f(1,1);glVertex3f( FCurrSize, FCurrSize,0);
            glTexCoord2f(0,1);glVertex3f(-FCurrSize, FCurrSize,0);
          glend;
          ImgRing.UnApply(rci);
        glPopMatrix;
  end;

  if not ImgSecondaries.Disabled and Assigned(ImgSecondaries.Image) then
  begin
        RandSeed := FSeed;
        glPushMatrix;
          ImgSecondaries.Apply(rci);
          for i := 1 to FNumSecs do
          begin
             rnd := 2 * Random - 1;
             v:=PosVector;
             if rnd<0 then
                ScaleVector(V, rnd)
             else ScaleVector(V, 0.8*rnd);
             glPushMatrix;
               glTranslatef(v[0], v[1],v[2]);

               rnd := random*0.5+0.1;
               glbegin(GL_QUADS);
                 glTexCoord2f(0,0);glVertex3f(-FCurrSize*rnd,-FCurrSize*rnd,0);
                 glTexCoord2f(1,0);glVertex3f( FCurrSize*rnd,-FCurrSize*rnd,0);
                 glTexCoord2f(1,1);glVertex3f( FCurrSize*rnd, FCurrSize*rnd,0);
                 glTexCoord2f(0,1);glVertex3f(-FCurrSize*rnd, FCurrSize*rnd,0);
               glend;
             glPopMatrix
          end;
          ImgSecondaries.UnApply(rci);
        glPopMatrix;
  end;

   // restore state

  glPopAttrib;
  glPopMatrix;
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix;

  if Count > 0 then
    Self.RenderChildren(0, Count - 1, rci);
end;


// DoProgress
//
procedure TGLTextureLensFlare.DoProgress(const progressTime: TProgressTimes);
begin
  if AutoZTest then
     FDeltaTime := progressTime.deltaTime;
  inherited;  
end;

procedure TGLTextureLensFlare.SetImgGlow(const Value: TGLTexture);
begin
  FImgGlow.Assign(Value);
  StructureChanged;
end;

procedure TGLTextureLensFlare.SetImgRays(const Value: TGLTexture);
begin
  FImgRays.Assign(Value);
  StructureChanged;
end;

procedure TGLTextureLensFlare.SetImgRing(const Value: TGLTexture);
begin
  FImgRing.Assign(Value);
  StructureChanged;
end;

procedure TGLTextureLensFlare.SetImgSecondaries(const Value: TGLTexture);
begin
  FImgSecondaries.Assign(Value);
  StructureChanged;
end;


destructor TGLTextureLensFlare.Destroy;
begin   
  FImgRays.Free;
  FImgSecondaries.Free;
  FImgRing.Free;
  FImgGlow.Free;
  inherited;
end;

procedure TGLTextureLensFlare.SetSeed(const Value: Integer);
begin
  FSeed := Value;
  StructureChanged;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterClasses([TGLLensFlare, TGLTextureLensFlare]);

end.
