// GLImposter
{: Imposter building and rendering implementation for GLScene.<p>

   <b>History : </b><font size=-1><ul>
      <li>14/04/04 - SG - Fixed texture clamping for old cards and 
                          switched to GL_NEAREST texture sampling.
      <li>24/03/04 - SG - Initial.
   </ul></font><p>
}
unit GLImposter;

interface

uses
  Classes, GLScene, GLContext, GLTexture, VectorGeometry, GeometryBB;

type
  TGLImposter = class;

  TGLImposterBuilder = class(TGLBaseSceneObject)
    private
      FMinTexSize,
      FMaxTexSize : Integer;
      FMinDistance,
      FTolerance : Single;
      FImposterRegister : TList;
      FEnabled,
      FUseMatrixError : Boolean;

    protected
      procedure SetMinDistance(const Value : Single);
      procedure SetEnabled(const Value : Boolean);

    public
      constructor Create(AOwner : TComponent); override;
      destructor Destroy; override;
      procedure DoRender(var rci : TRenderContextInfo;
        renderSelf, renderChildren : Boolean); override;
      procedure RegisterImposter(anImposter : TGLImposter);
      procedure UnregisterImposter(anImposter : TGLImposter);

    published
      property MinTexSize : Integer read FMinTexSize write FMinTexSize;
      property MaxTexSize : Integer read FMaxTexSize write FMaxTexSize;
      property MinDistance : Single read FMinDistance write SetMinDistance;
      property Tolerance : Single read FTolerance write FTolerance;
      property Enabled : Boolean read FEnabled write SetEnabled;
      property UseMatrixError : Boolean read FUseMatrixError write FUseMatrixError;

  end;

  TGLImposter = class(TGLImmaterialSceneObject)
    private
      FTextureHandle : Cardinal;
      FBuilder : TGLImposterBuilder;
      FOldMatrix : TMatrix;
      FDrawImposter : Boolean;
      FSize : Single;
      FTexSize : Integer;
      FLastTexSize : Integer;
      FInvalidated,
      FUseAlphaTest : Boolean;

    protected
      procedure SetBuilder(const val : TGLImposterBuilder);
      function CalcError(NewMatrix : TMatrix) : Single;
      function GetTextureHandle : Cardinal;

    public
      constructor Create(AOwner : TComponent); override;
      destructor Destroy; override;
      procedure DoRender(var rci : TRenderContextInfo;
        renderSelf, renderChildren : Boolean); override;
      procedure Invalidate;
      function AxisAlignedDimensionsUnscaled : TVector; override;

      property TextureHandle : Cardinal read GetTextureHandle;

    published
      property Builder : TGLImposterBuilder read FBuilder write SetBuilder;
      property AlphaTest : Boolean read FUseAlphaTest write FUseAlphaTest;
  end;

implementation

uses
  OpenGL1x, GLUtils;

// ----------
// ---------- TGLImposterBuilder ----------
// ----------

// Create
//
constructor TGLImposterBuilder.Create(AOwner : TComponent);
begin
  inherited;
  FImposterRegister:=TList.Create;
  FTolerance:=0.1;
  FUseMatrixError:=True;
  FMinTexSize:=16;
  FMaxTexSize:=64;
  ObjectStyle:=ObjectStyle+[osDirectDraw];
end;

// Destroy
//
destructor TGLImposterBuilder.Destroy;
var
  i : Integer;
begin
  for i:=FImposterRegister.Count-1 downto 0 do
    TGLImposter(FImposterRegister[i]).Builder:=nil;
  FImposterRegister.Free;
  inherited;
end;

// DoRender
//
procedure TGLImposterBuilder.DoRender(var rci : TRenderContextInfo;
  renderSelf, renderChildren : Boolean);
var
  i, size, Left, Top, Width, Height : Integer;
  imposter : TGLImposter;
  mat, projection, modelview : TMatrix;
  BackColor, pos, temp : TVector;
  rad : Single;
  AABB : TAABB;
begin
  if (csDesigning in ComponentState) or not FEnabled then exit;

  // Store the current clear color
  glGetFloatv(GL_COLOR_CLEAR_VALUE, @BackColor[0]);

  // Get the projection matrix
  if UseMatrixError then
    glGetFloatv(GL_PROJECTION_MATRIX, @projection);

  // Render and save each imposter as required
  for i:=0 to FImposterRegister.Count-1 do begin
    imposter:=TGLImposter(FImposterRegister[i]);
    if (imposter.Count = 0) or not imposter.Visible then Continue;
    imposter.FDrawImposter:=True;

    if VectorDistance(imposter.AbsolutePosition, rci.cameraPosition)<FMinDistance then begin
      imposter.FDrawImposter:=False;
      Continue;
    end;

    glMatrixMode(GL_MODELVIEW);
    glPushMatrix;
    glMultMatrixf(@imposter.AbsoluteMatrixAsAddress[0]);
    glGetFloatv(GL_MODELVIEW_MATRIX, @modelview);

    // Get imposters dimensions
    AABB:=imposter.AxisAlignedBoundingBox;
    rad:=MaxFloat(AABB.max[0],AABB.max[1],AABB.max[2]);
    pos:=imposter.AbsolutePosition;
    temp:=Scene.CurrentBuffer.Camera.AbsoluteEyeSpaceVector(0,1,0);
    temp:=VectorAdd(pos, VectorScale(temp,rad));
    pos:=Scene.CurrentBuffer.WorldToScreen(pos);
    temp:=Scene.CurrentBuffer.WorldToScreen(temp);
    size:=RoundUpToPowerOf2(Round(2*VectorDistance(pos,temp)));
    if size<FMinTexSize then size:=FMinTexSize;
    if size>FMaxTexSize then begin
      imposter.FDrawImposter:=False;
      glPopMatrix;
      Continue;
    end;
    temp:=pos;
    temp[0]:=temp[0]+size;
    temp:=Scene.CurrentBuffer.ScreenToWorld(temp);
    Imposter.FSize:=VectorDistance(imposter.AbsolutePosition,temp);
    imposter.FTexSize:=size;
    pos[0]:=pos[0]-size/2;
    pos[1]:=pos[1]-size/2;

    // Calculate error
    if UseMatrixError then begin
      mat:=MatrixMultiply(modelview, projection);
      if (imposter.CalcError(mat)>FTolerance) or (imposter.FInvalidated) then
        imposter.FOldMatrix:=mat
      else begin
        glPopMatrix;
        Continue;
      end;
    end;

    // Clear to transparent black
    glClearColor(0,0,0,0);

    // Determine size by color (for debug purposes)
    {case size of
      16 : glClearColor(0,0,1,0.1);
      32 : glClearColor(0,1,0,0.1);
      64 : glClearColor(1,0,0,0.1);
      128 : glClearColor(1,1,0,0.1);
      256 : glClearColor(1,0,1,0.1);
    end;//}

    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);

    // Render the imposter's children
    imposter.RenderChildren(0, imposter.Count-1, rci);
    glPopMatrix;

    // Select the imposters texture (will create the handle if null)
    glBindTexture(GL_TEXTURE_2D,imposter.TextureHandle);

    // Check for resize or invalidation
    if (imposter.FTexSize <> imposter.FLastTexSize)
    or (imposter.FInvalidated) then begin
      glTexImage2d(GL_TEXTURE_2D, 0, GL_RGBA, size, size, 0, GL_RGBA, GL_UNSIGNED_BYTE, nil);
      imposter.FLastTexSize:=imposter.FTexSize;
      imposter.FInvalidated:=False;
      imposter.NotifyChange(self);
    end;

    // Get the region to be copied from the frame buffer
    Left:=Floor(pos[0]); Top:=Floor(pos[1]);
    Width:=Size; Height:=Size;
    // ... Perhaps some region clamping here?

    // Copy the frame buffer pixels to the imposter texture
    glCopyTexSubImage2d(GL_TEXTURE_2D, 0, 0, 0,
                        Left, Top, Width, Height);
  end;

  // Reset the clear color and clear color, depth and stencil buffers
  glClearColor(BackColor[0],BackColor[1],BackColor[2],BackColor[3]);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);
end;

// RegisterImposter
//
procedure TGLImposterBuilder.RegisterImposter(anImposter: TGLImposter);
begin
  if FImposterRegister.IndexOf(anImposter) = -1 then
    FImposterRegister.Add(anImposter);
end;

// UnregisterImposter
//
procedure TGLImposterBuilder.UnregisterImposter(anImposter: TGLImposter);
begin
  FImposterRegister.Remove(anImposter);
end;

// SetMinDistance
//
procedure TGLImposterBuilder.SetMinDistance(const Value : Single);
begin
  if Value<>FMinDistance then begin
    FMinDistance:=Value;
    StructureChanged;
  end;
end;

// SetEnabled
//
procedure TGLImposterBuilder.SetEnabled(const Value : Boolean);
var
  i : Integer;
begin
  if Value <> FEnabled then begin
    FEnabled:=Value;
    for i:=0 to FImposterRegister.Count-1 do begin
      if not FEnabled then
        TGLImposter(FImposterRegister[i]).FDrawImposter:=False;
      TGLImposter(FImposterRegister[i]).NotifyChange(Self);
    end;
  end;
end;


// ----------
// ---------- TGLImposter ----------
// ----------

// Create
//
constructor TGLImposter.Create(AOwner : TComponent);
begin
  inherited;
  FTextureHandle:=0;
  FDrawImposter:=False;
  FInvalidated:=False;
  FUseAlphaTest:=False;
  FSize:=1;
  FTexSize:=0;
  FLastTexSize:=-1;
  ObjectStyle:=ObjectStyle+[osDirectDraw];
end;

// Destroy
//
destructor TGLImposter.Destroy;
begin
  Builder:=nil;
  if FTextureHandle<>0 then
    glDeleteTextures(1, @FTextureHandle);
  inherited;
end;

// DoRender
//
procedure TGLImposter.DoRender(var rci : TRenderContextInfo;
  renderSelf, renderChildren : Boolean);
var
  vx, vy : TAffineVector;
  s : Single;
  mat : TMatrix;
begin
  if (not (csDesigning in ComponentState))
  and FDrawImposter and (FTextureHandle<>0) then begin
    // Render the imposter sprite
    glPushAttrib(GL_ENABLE_BIT or GL_COLOR_BUFFER_BIT);
    glDisable(GL_LIGHTING);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    if AlphaTest then begin
      glEnable(GL_ALPHA_TEST);
      glAlphaFunc(GL_GEQUAL, 0.99);
    end;

    glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, FTextureHandle);

    if GL_VERSION_1_2 or GL_EXT_texture_edge_clamp then begin
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    end else begin
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
    end;

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glGetFloatv(GL_MODELVIEW_MATRIX, @mat);
    glBegin(GL_QUADS);
      s:=FSize*0.5;
      vx[0]:=mat[0][0];  vy[0]:=mat[0][1];
      vx[1]:=mat[1][0];  vy[1]:=mat[1][1];
      vx[2]:=mat[2][0];  vy[2]:=mat[2][1];
      ScaleVector(vx, s/VectorLength(vx));
      ScaleVector(vy, s/VectorLength(vy));
      glTexCoord2f(1,1);  glVertex3f( vx[0]+vy[0], vx[1]+vy[1], vx[2]+vy[2]);
      glTexCoord2f(0,1);  glVertex3f(-vx[0]+vy[0],-vx[1]+vy[1],-vx[2]+vy[2]);
      glTexCoord2f(0,0);  glVertex3f(-vx[0]-vy[0],-vx[1]-vy[1],-vx[2]-vy[2]);
      glTexCoord2f(1,0);  glVertex3f( vx[0]-vy[0], vx[1]-vy[1], vx[2]-vy[2]);
    glEnd;
    glPopAttrib;
  end else begin
    Self.RenderChildren(0,Count-1,rci);
  end;
end;

// SetBuilder
//
procedure TGLImposter.SetBuilder(const val : TGLImposterBuilder);
begin
  if val<>FBuilder then begin
    if Assigned(FBuilder) then
      FBuilder.UnregisterImposter(Self);
    FBuilder:=val;
    if Assigned(FBuilder) then
      FBuilder.RegisterImposter(Self);
  end;
end;

// AxisAlignedDimensionsUnscaled
//
function TGLImposter.AxisAlignedDimensionsUnscaled : TVector;
begin
   Result:=NullHMGVector;
end;

// CalcDifference
//
function TGLImposter.CalcError(NewMatrix : TMatrix) : Single;
var
  i : Integer;
  mat : TMatrix;
  err : Single;
begin
  err:=0;
  mat:=NewMatrix;
  InvertMatrix(mat);
  mat:=MatrixMultiply(FOldMatrix, mat);
  for i:=0 to 3 do mat[i][i]:=mat[i][i]-1;
  for i:=0 to 15 do err:=err+Abs(mat[i div 4][i mod 4]);
  Result:=err;
end;

// GetTextureHandle
//
function TGLImposter.GetTextureHandle: Cardinal;
begin
  if FTextureHandle = 0 then
    glGenTextures(1, @FTextureHandle);
  Result:=FTextureHandle;
end;

// Invalidate
//
procedure TGLImposter.Invalidate;
begin
  FInvalidated:=True;
end;

initialization

  RegisterClasses([TGLImposterBuilder, TGLImposter]);

end.
