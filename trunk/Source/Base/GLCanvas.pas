{: GLCanvas<p>

	Implements a basic Canvas-like interface over for OpenGL.<p>
   This class can be used for generic OpenGL applications and has no dependencies
   to the GLScene core units (only to base units).<p>

	<b>History : </b><font size=-1><ul>
      <li>19/01/02 - EG - Creation
	</ul></font>
}
unit GLCanvas;

interface

uses Classes, Geometry;

type

   TColor = Integer;

	// TGLCanvas
	//
   {: A simple Canvas-like interface for OpenGL.<p>
      This class implements a small "shell" for 2D operations in OpenGL,
      it operates over the current OpenGL context and provides methods
      for drawing lines, ellipses and points.<br>
      This class is typically used by creating an instance, using it for drawing,
      and freeing the instance. When drawing (0, 0) is the top left corner.<br>
      All coordinates are internally maintained with floating point precisoion.<p>
      Several states are cached and it is of primary importance not to invoke
      OpenGL directly throughout the life of an instance (at the cost of
      unespected behaviour). }
	TGLCanvas = class
	   private
	      { Private Declarations }
         FColorBackup : TVector;
         FPointSizeBackup, FLineWidthBackup : Single;

         FLastPrimitive : Integer;
         FCurrentPos : TAffineVector;
         FPenColor : TColor;
         FPenWidth : Integer;
         FCurrentPenColorVector : TVector;

	   protected
	      { Protected Declarations }
	      procedure BackupOpenGLStates;
	      procedure RestoreOpenGLStates;

	      procedure StartPrimitive(const primitiveType : Integer);
         procedure StopPrimitive;

         procedure SetPenColor(const val : TColor);
         procedure SetPenWidth(const val : Integer);


      public
	      { Public Declarations }
	      constructor Create(bufferSizeX, bufferSizeY : Integer;
                            const baseTransform : TMatrix); overload;
	      constructor Create(bufferSizeX, bufferSizeY : Integer); overload;
	      destructor Destroy; override;

         {: Current Pen Color. }
         property PenColor : TColor read FPenColor write SetPenColor;
         {: Current Pen Width. }
         property PenWidth : Integer read FPenWidth write SetPenWidth;

         {: Updates the current position. }
	      procedure MoveTo(const x, y : Integer); overload;
	      procedure MoveTo(const x, y : Single); overload;

         {: Draws a line from current position to given coordinate.<p>
            Current position is updated. }
	      procedure LineTo(const x, y : Integer); overload;
	      procedure LineTo(const x, y : Single); overload;
         {: Draws a line from (x1, y1) to (x2, y2).<p>
            The current position is NOT updated. }
	      procedure Line(const x1, y1, x2, y2 : Integer); overload;
	      procedure Line(const x1, y1, x2, y2 : Single); overload;

         {: Plots a pixel at given coordinate.<p>
            PenWidth affects pixel size.<br>
            The current position is NOT updated. }
  	      procedure PlotPixel(const x, y : Integer); overload;
	      procedure PlotPixel(const x, y : Single); overload;

         {: Draws an ellipse with (x1,y1)-(x2, y2) bounding rectangle. }
	      procedure Ellipse(const x1, y1, x2, y2 : Integer); overload;
         {: Draws and ellipse centered at (x, y) with given radiuses. }
	      procedure Ellipse(const x, y : Integer; const xRadius, yRadius : Single); overload;
	      procedure Ellipse(const x, y, xRadius, yRadius : Single); overload;
	end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
implementation
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

uses OpenGL12;

const
   cNoPrimitive = 0;

// ConvertColorVector
//
function ConvertColorVector(const aColor: TVector): TColor;
begin
  Result:=   (Round(255 * AColor[2]) shl 16)
          or (Round(255 * AColor[1]) shl 8)
          or  Round(255 * AColor[0]);
end;

// ConvertWinColor
//
function ConvertWinColor(aColor : TColor; alpha : Single = 1) : TVector;
var
   winColor : Integer;
begin
	// Delphi color to Windows color
   winColor:=ColorToRGB(AColor);
   // convert 0..255 range into 0..1 range
   Result[0]:=(winColor and $FF)*(1/255);
   Result[1]:=((winColor shr 8) and $FF)*(1/255);
   Result[2]:=((winColor shr 16) and $FF)*(1/255);
   Result[3]:=alpha;
end;

// ------------------
// ------------------ TGLCanvas ------------------
// ------------------

// Create
//
constructor TGLCanvas.Create(bufferSizeX, bufferSizeY : Integer;
                             const baseTransform : TMatrix);
begin
   glMatrixMode(GL_PROJECTION);
   glPushMatrix;
   glLoadIdentity;
   gluOrtho2D(0, bufferSizeX, bufferSizeY, 0);

   glMatrixMode(GL_MODELVIEW);
   glPushMatrix;
   glLoadMatrixf(@baseTransform);

   BackupOpenGLStates;

   FLastPrimitive:=cNoPrimitive;
end;

// Create
//
constructor TGLCanvas.Create(bufferSizeX, bufferSizeY : Integer);
begin
   Create(bufferSizeX, bufferSizeY, IdentityHmgMatrix);
end;

// Destroy
//
destructor TGLCanvas.Destroy;
begin
   StopPrimitive;

   RestoreOpenGLStates;

   glMatrixMode(GL_PROJECTION);
   glPopMatrix;

   glMatrixMode(GL_MODELVIEW);
   glPopMatrix;
end;

// BackupOpenGLStates
//
procedure TGLCanvas.BackupOpenGLStates;
begin
   glPushAttrib(GL_ENABLE_BIT);

   glDisable(GL_LIGHTING);
   glDisable(GL_CULL_FACE);
   glDisable(GL_COLOR_MATERIAL);
   glDisable(GL_DEPTH_TEST);
   glDisable(GL_TEXTURE_1D);
   glDisable(GL_TEXTURE_2D);
   glDisable(GL_TEXTURE_3D);
   glDisable(GL_LINE_SMOOTH);

   // Setup and backup pen stuff
   glGetFloatv(GL_CURRENT_COLOR, @FColorBackup);
   FPenColor:=clBlack;
   SetVector(FCurrentPenColorVector, NullHmgPoint);
   glColor4fv(@FCurrentPenColorVector);
   glGetFloatv(GL_LINE_WIDTH, @FLineWidthBackup);
   glGetFloatv(GL_POINT_SIZE, @FPointSizeBackup);
   FPenWidth:=1;
   glLineWidth(1);
   glPointSize(1);
end;

// RestoreOpenGLStates
//
procedure TGLCanvas.RestoreOpenGLStates;
begin
   glColor4fv(@FColorBackup);
   glLineWidth(FLineWidthBackup);
   glPointSize(FPointSizeBackup);

   glPopAttrib;
end;

// StartPrimitive
//
procedure TGLCanvas.StartPrimitive(const primitiveType : Integer);
begin
   if primitiveType<>FLastPrimitive then begin
      if FLastPrimitive<>cNoPrimitive then
         glEnd;
      if primitiveType<>cNoPrimitive then
         glBegin(primitiveType);
      FLastPrimitive:=primitiveType;
   end;
end;

// StopPrimitive
//
procedure TGLCanvas.StopPrimitive;
begin
   StartPrimitive(cNoPrimitive);
end;

// SetPenColor
//
procedure TGLCanvas.SetPenColor(const val : TColor);
begin
   if val<>FPenColor then begin
      SetVector(FCurrentPenColorVector, ConvertWinColor(val, FCurrentPenColorVector[3]));
      FPenColor:=val;
      glColor4fv(@FCurrentPenColorVector);
   end;
end;

// SetPenWidth
//
procedure TGLCanvas.SetPenWidth(const val : Integer);
begin
   if val<1 then Exit;
   if val<>FPenWidth then begin
      FPenWidth:=val;
      glLineWidth(val);
      glPointSize(val);
   end;
end;

// MoveTo
//
procedure TGLCanvas.MoveTo(const x, y : Integer);
begin
   FCurrentPos[0]:=x;
   FCurrentPos[1]:=y;
end;

// MoveTo
//
procedure TGLCanvas.MoveTo(const x, y : Single);
begin
   FCurrentPos[0]:=x;
   FCurrentPos[1]:=y;
end;

// LineTo
//
procedure TGLCanvas.LineTo(const x, y : Integer);
begin
   StartPrimitive(GL_LINES);
   glVertex2fv(@FCurrentPos);
   MoveTo(x, y);
   glVertex2fv(@FCurrentPos);
end;

// LineTo
//
procedure TGLCanvas.LineTo(const x, y : Single);
begin
   StartPrimitive(GL_LINES);
   glVertex2fv(@FCurrentPos);
   MoveTo(x, y);
   glVertex2fv(@FCurrentPos);
end;

// Line
//
procedure TGLCanvas.Line(const x1, y1, x2, y2 : Integer);
begin
   StartPrimitive(GL_LINES);
   glVertex2i(x1, y1);
   glVertex2i(x2, y2);
end;

// Line
//
procedure TGLCanvas.Line(const x1, y1, x2, y2 : Single);
begin
   StartPrimitive(GL_LINES);
   glVertex2f(x1, y1);
   glVertex2f(x2, y2);
end;

// PlotPixel
//
procedure TGLCanvas.PlotPixel(const x, y : Integer);
begin
   StartPrimitive(GL_POINTS);
   glVertex2i(x, y);
end;

// PlotPixel
//
procedure TGLCanvas.PlotPixel(const x, y : Single);
begin
   StartPrimitive(GL_POINTS);
   glVertex2f(x, y);
end;

// Ellipse
//
procedure TGLCanvas.Ellipse(const x1, y1, x2, y2 : Integer);
begin
   Ellipse((x1+x2)*0.5, (y1+y2)*0.5, Abs(x2-x1)*0.5, Abs(y2-y1)*0.5);
end;

// Ellipse
//
procedure TGLCanvas.Ellipse(const x, y : Integer; const xRadius, yRadius : Single);
var
   sx, sy : Single;
begin
   sx:=x; sy:=y;
   Ellipse(sx, sy, xRadius, yRadius);
end;

// Ellipse
//
procedure TGLCanvas.Ellipse(const x, y, xRadius, yRadius : Single);
var
   i, n : Integer;
   s, c : array of Single;
begin
   n:=Round(MaxFloat(xRadius, yRadius)*0.1)+6;
   SetLength(s, n);
   SetLength(c, n);
   Dec(n);
   PrepareSinCosCache(s, c, 0, 90);
   ScaleFloatArray(s, yRadius);
   ScaleFloatArray(c, xRadius);
   StartPrimitive(GL_LINE_STRIP);
   // first quadrant (top right)
   for i:=0 to n do
      glVertex2f(x+c[i], y-s[i]);
   // second quadrant (top left)
   for i:=n-1 downto 0 do
      glVertex2f(x-c[i], y-s[i]);
   // third quadrant (bottom left)
   for i:=1 to n do
      glVertex2f(x-c[i], y+s[i]);
   // fourth quadrant (bottom right)
   for i:=n-1 downto 0 do
      glVertex2f(x+c[i], y+s[i]);
   StopPrimitive;
end;

end.
