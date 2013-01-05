//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BaseClasses"
#pragma link "GLBitmapFont"
#pragma link "GLCadencer"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLDCE"
#pragma link "GLHeightData"
#pragma link "GLHUDObjects"
#pragma link "GLMaterial"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLTerrainRenderer"
#pragma link "GLVectorFileObjects"
#pragma link "GLWin32Viewer"
#pragma link "GLWindowsFont"
#pragma link "GLFile3DS"
#pragma link "GLFileMD2"

#pragma resource "*.dfm"
TForm1 *Form1;

const
  Single cForce = 250;
  int cSpread = 200;
  int cNbMushrooms = 20;

float random(void)
{
  return (float)(rand() & 0x1FFF) / (float)0x1FFF;
}

//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------

void TForm1::Load(void)
{
  SetGLSceneMediaDir();
  //Load Materials
  GLMaterialLibrary1->AddTextureMaterial("Terrain", "snow512.jpg", true);
  GLMaterialLibrary1->AddTextureMaterial("Terrain", "snow512.jpg", true);

//  GLMaterialLibrary1->Materials->Items[0]->Material->Texture->Image->
//	LoadFromFile("waste.jpg");

  GLMaterialLibrary1->AddTextureMaterial("Actor", "waste.jpg", true);

  //Load Terrain
  GLBitmapHDS1->MaxPoolSize = 8 * 1024 * 1024;
  GLBitmapHDS1->Picture->LoadFromFile("terrain.bmp");
  Terrain->Direction->SetVector(0, 1, 0);
  Terrain->Material->LibMaterialName = "Terrain";
  Terrain->TilesPerTexture = 256 / Terrain->TileSize;
  Terrain->Scale->SetVector(1, 1, 0.02);

  Ground->Material->LibMaterialName = "Terrain";

  // Load mushroom mesh
  //Always use AutoScaling property or you may get some problems
  moMushroom->AutoScaling->SetPoint(0.1, 0.1, 0.1);
  moMushroom->LoadFromFile("Mushroom.3ds");
  moMushroom->Direction->SetVector(0, 1, 0);
  moMushroom->BuildOctree();

  //Load player
  Player->Position->SetPoint(0, 3, 0);
  //Actor
  GLActor1->LoadFromFile("Waste.md2");
  GLActor1->Direction->SetVector(0, 1, 0);
  GLActor1->Up->SetVector(1, 0, 0);
  GLActor1->Scale->SetVector(0.05, 0.05, 0.05);
  GLActor1->Material->LibMaterialName = "Actor";
  GLActor1->Animations->LoadFromFile("Quake2Animations.aaf");
  // Define animation properties
  GLActor1->AnimationMode = aamLoop;
  GLActor1->SwitchToAnimation("stand");
  GLActor1->FrameInterpolation = afpLinear;

  //DCE Behaviour
  GLSphere1->Scale->Assign(GetOrCreateDCEDynamic(Player)->Size);
//  GetOrCreateDCEDynamic(Player)->OnCollision = PlayerBehaviours0Collision();
}

void TForm1::HandleKeys(void)
{
  TAffineVector *Force;
  Force = &NullVector;
  if (IsKeyDown('W') || IsKeyDown('Z'))
	Force->V[2] = cForce;
  if (IsKeyDown('S'))
	Force->V[2] = -cForce;
  if (IsKeyDown('A') || IsKeyDown('Q'))
	Force->V[0] = cForce;
  if (IsKeyDown('D'))
	Force->V[0] = -cForce;

  GetOrCreateDCEDynamic(Player)->ApplyAccel(*Force);
}

void TForm1::HandleAnimation(void)
{
  String anim;
  if (VectorNorm(GetOrCreateDCEDynamic(Player)->Speed) > 0.1)
	anim = "run";
  else
	anim = "stand";

  if (Jumped==true) {
	if (!GetOrCreateDCEDynamic(Player)->InGround)
	  anim = "jump";
	else
	  Jumped = False;
  }
  if (anim == "jump")
	GLActor1->Interval = 500;
  else
	GLActor1->Interval = 100;

  if (GLActor1->CurrentAnimation() != anim)
	GLActor1->SwitchToAnimation(anim);
}

void TForm1::AddBall()
{
  TGLSphere *Ball;
  Single S;
  Single Px,Py,Pz;
  Single cR, cG, cB;

  Ball = (TGLSphere *)(Balls->AddNewChild(__classid(TGLSphere)));
  //with Ball do
	Ball->Tag = 1; //set the identifier of a ball
	Ball->Radius = 1;
	S = random(900.0);
	S = (100.0 + random(900.0)/ 500.0);
	Ball->Scale->SetVector(S, S, S);
	Px = random(40.0) - random(40.0);
	Py = 4.0 + random(10.0);
	Pz = random(40.0) - random(40.0);
	Ball->Position->SetPoint(Px, Py, Pz);
	cR = (100.0 + random(900.0))/1000.0;
	cG = (100.0 + random(900.0))/1000.0;
	cB = (100.0 + random(900.0))/1000.0;
	Ball->Material->FrontProperties->Diffuse->SetColor(cR, cG, cB);
//  with GetOrCreateDCEDynamic(Ball) do
	GetOrCreateDCEDynamic(Ball)->Manager = GLDCEManager1;
	GetOrCreateDCEDynamic(Ball)->BounceFactor = 0.75;
	GetOrCreateDCEDynamic(Ball)->Friction = 0.1;
	GetOrCreateDCEDynamic(Ball)->SlideOrBounce = csbBounce;
	GetOrCreateDCEDynamic(Ball)->Size->Assign(Ball->Scale);
}
void TForm1::AddMushrooms(void)
{
  int i;
  TGLFreeFormProxy *Proxy;
  Vectorgeometry::TVector s;
  Single f;
  Single Px, Py, Pz;

  // spawn some more mushrooms using proxy objects
  for (i = 0; i < cNbMushrooms - 1;i++) {
	// create a new Proxy and set its MasterObject property
	Proxy = (TGLFreeFormProxy *)(Mushrooms->AddNewChild(__classid(TGLFreeFormProxy)));
	//with Proxy do
	  Proxy->ProxyOptions = Proxy->ProxyOptions << pooObjects;
	  Proxy->MasterObject = moMushroom;
	  // retrieve reference attitude
	  Proxy->Direction = moMushroom->Direction;
	  Proxy->Up = moMushroom->Up;
	  // randomize scale
	  s = moMushroom->Scale->AsVector;
	  f = (2 * random() + 1);
	  ScaleVector(s, f);
	  Proxy->Scale->AsVector = s;
	  // randomize position
	  Px = random(cSpread) - (cSpread / 2);
	  Py = moMushroom->Position->Z + 1.5 * f;
	  Pz = random(cSpread) - (cSpread / 2);
	  Proxy->Position->SetPoint(Px, Py, Pz);
	  // randomize orientation
	  Proxy->RollAngle = random(360);
	  Proxy->TransformationChanged();

	//with GetOrCreateDCEStatic(Proxy) do
	  GetOrCreateDCEStatic(Proxy)->Manager = GLDCEManager1;
	  GetOrCreateDCEStatic(Proxy)->BounceFactor = 0.75;
	  GetOrCreateDCEStatic(Proxy)->Friction = 10;
	  GetOrCreateDCEStatic(Proxy)->Shape = csFreeform;
  }
}


void __fastcall TForm1::GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift,
		  int X, int Y)
{
  //Mouse look
  if (Shift.Contains(ssLeft)) {
	GLCamera1->MoveAroundTarget((my - Y), 0);
	Player->Turn(-(mx - X));
  }
  mx = X;
  my = Y;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
		  const double newTime)
{
  HandleKeys();
  HandleAnimation();
  //This shows the manual progress, don't need this if you use the automatic mode
  if (GLDCEManager1->ManualStep)
	GLDCEManager1->Step(deltaTime);

  Help->ModulateColor->Alpha = Help->ModulateColor->Alpha - (deltaTime * 0.05);
  if (Help->ModulateColor->Alpha < 0.25)
	Help->ModulateColor->Alpha = 0.25;
  HelpShadow->ModulateColor->Alpha = Help->ModulateColor->Alpha;
  HelpShadow->Text = Help->Text;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormShow(TObject *Sender)
{
  Load();
  GLCadencer1->Enabled = true;
  Help->Text =
   "Mouse Drag - Look\r\
	A,W,S,D - movement\r\
	SPACE - Jump\r\
	F1 - Add one ball\r\
	F2 - Add 10 balls\r\
	F3 - Add 20 mushrooms\r\
	F4 - Change ground to box\r\
	F5 - Toggle step mode\r\
	RETURN - Reset";
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Timer1Timer(TObject *Sender)
{
  String s;
  if (GLDCEManager1->ManualStep)
	s = "Manual";
  else
	s = "Automatic";
  GLHUDText1->Text = Format("FPS: %.1f - Dynamics: %d - Statics: %d - Step mode: %s",
	 ARRAYOFCONST((GLSceneViewer1->FramesPerSecond(),GLDCEManager1->DynamicCount,GLDCEManager1->StaticCount,s)));
  GLSceneViewer1->ResetPerformanceMonitor();

}
//---------------------------------------------------------------------------

void __fastcall TForm1::PlayerBehaviours0Collision(TObject *Sender,
	  TGLBaseSceneObject *ObjectCollided, TDCECollision &CollisionInfo)
{
  //Use some kind of identifier to know what object you are colliding
  //You can use the Tag, TagFloat, Name, Class
  TAffineVector v;
  if (ObjectCollided->Tag == 1) {
	v = AffineVectorMake(VectorSubtract(ObjectCollided->AbsolutePosition, Player->AbsolutePosition));
	NormalizeVector(v);
	ScaleVector(v, 400);
	GetOrCreateDCEDynamic(ObjectCollided)->StopAbsAccel();
	GetOrCreateDCEDynamic(ObjectCollided)->ApplyAbsAccel(v);
  }
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormKeyDown(TObject *Sender, WORD &Key, TShiftState Shift)

{
  int i;
  if (Key == VK_F1)
	AddBall();
  if (Key == VK_F2)
	for (i = 1; i < 10; i++)
	  AddBall();
  if (Key == VK_F3)
	AddMushrooms();
  if (Key == VK_SPACE) {
	GetOrCreateDCEDynamic(Player)->Jump(1, 20);
	Jumped = true;
  }
  if (Key == VK_F4) {
	Terrain->Visible = false;
	Ground->Visible = true;
	GetOrCreateDCEStatic(Terrain)->Active = false;
	GetOrCreateDCEStatic(Ground)->Active = true;
  }
  if (Key == VK_F5)
	GLDCEManager1->ManualStep = !GLDCEManager1->ManualStep;

  if (Key = VK_RETURN) {
	Player->Position->SetPoint(0, 3, 0);
	Balls->DeleteChildren();
	Mushrooms->DeleteChildren();
	Help->ModulateColor->Alpha = 1;
	Terrain->Visible = true;
	Ground->Visible = false;
	GetOrCreateDCEStatic(Terrain)->Active = true;
	GetOrCreateDCEStatic(Ground)->Active = false;
  }
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLDirectOpenGL1Render(TObject *Sender, TRenderContextInfo &rci)

{
  int i;
  TAffineVector p, n;

  //To use this you will need to enable the debug define in the
  //GLEllipseCollision.pas, if you do, don't forget to clear the
  //triangle list! -> SetLength(debug_tri,0);

/*
  rci->GLStates->PointSize = 5.0;
  GL->Color3f(0, 1, 0);

  for (i = 0; i < High(debug_tri); i++) {
  //	with debug_tri[i] do
	  GL->Color3f(0, 0, 0);
	  GL.Begin_(GL_LINE_STRIP);
	  GL.Vertex3f(p1.V[0], p1.V[1], p1.V[2]);
	  GL.Vertex3f(p2.V[0], p2.V[1], p2.V[2]);
	  GL.Vertex3f(p3.V[0], p3.V[1], p3.V[2]);
	  GL.End_;
	  CalcPlaneNormal(p1, p2, p3, n);
	  ScaleVector(n, 0.25);
	  p.V[0] := (p1.V[0] + p2.V[0] + p3.V[0]) / 3;
	  p.V[1] := (p1.V[1] + p2.V[1] + p3.V[1]) / 3;
	  p.V[2] := (p1.V[2] + p2.V[2] + p3.V[2]) / 3;
	  GL.Color3f(0, 0, 1);
	  GL.Begin_(GL_LINE_STRIP);
	  GL.Vertex3f(p.V[0], p.V[1], p.V[2]);
	  GL.Vertex3f(p.V[0] + n.V[0], p.V[1] + n.V[1], p.V[2] + n.V[2]);
	  GL.End_;
	  GL.Begin_(GL_POINTS);
	  GL.Vertex3f(p.V[0] + n.V[0], p.V[1] + n.V[1], p.V[2] + n.V[2]);
	  GL.End_;

	}
  SetLength(debug_tri, 0);
*/
}
//---------------------------------------------------------------------------

