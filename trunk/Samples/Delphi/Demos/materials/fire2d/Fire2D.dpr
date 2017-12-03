{
  A demo for using Alex Denissov's Graphics32 library (http://www.graphics32.org)
  to generate 2D texture for use with GLScene.
  By Nelson Chu

  Try lighting the white line near the bottom of the window with your mouse
  pointer and see the fire spreads. Press ESC to quit.

  To use Graphics32 with GLScene:

  1. Make sure USE_GRAPHICS32 is defined in GLSCene.inc. Recompile GLScene if
     needed.
  2. In your program to assign the Bitmap32 to your GLScene texture and notify GLScene
     use code like:

       ..Texture.Image.GetBitmap32(0).Assign(Bitmap32);
       ..Texture.Image.NotifyChange(self);

  To get fast assignment, remember to make the dimensions of your Bitmap32 equal
  to a power of two, so that GLScene doesn't need to do conversion internally.<p>

  In this sample program, a 256 x 256 Graphics32 TByteMap is used to generate a
  "fire" image. At each frame, the fire image is first "visualized" in a
  Graphics32 Bitmap32. Then, the TBitmap32 is copied to the texture of a Cube.
}
program Fire2D;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
