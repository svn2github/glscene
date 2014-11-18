//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>

#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLCadencer"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLGraph"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLWin32Viewer"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
  Randomize;
  BoxScale = XYZVector;
  SphereRadius = 1;
  BoxMatrix = IdentityHmgMatrix;

}
//---------------------------------------------------------------------------
void __fastcall TForm1::CheckBox04Click(TObject *Sender)
{
  GLCube1->Visible = CheckBox04->Checked;
  GLLines1->Visible = CheckBox05->Checked;
  GLXYZGrid1->Visible = CheckBox06->Checked;
  GLSphere1->Visible = CheckBox07->Checked;
}

//---------------------------------------------------------------------------

// Generates random rotation for matrix. It remains a scale.
Glvectorgeometry::TMatrix RandomRotation(Glvectorgeometry::TMatrix const &aMatrix)
{
  TAffineVector aScale;
  Glvectorgeometry::TMatrix mat;
  int I;

  // Save scale.
  for (I = 0; I < 2; I++)
	aScale.V[I] = VectorLength(aMatrix.V[I]);
  mat.V[3] = aMatrix.V[3];
  // Generate two not equal random vectors.
  while (VectorNorm(VectorSubtract(mat.V[0], mat.V[1])) < 10e-6)
  {
	while (VectorNorm(mat.V[0]) < 10e-6)
	  mat.V[0] = VectorMake(Random() * 2 - 1, Random() * 2 - 1, Random() * 2 - 1);
	while (VectorNorm(mat.V[1]) < 10e-6)
	  mat.V[1] = VectorMake(Random() * 2 - 1, Random() * 2 - 1, Random() * 2 - 1);
  }
  // Calculate two perpendicular vectors.
  mat.V[2] = VectorCrossProduct(mat.V[0], mat.V[1]);
  mat.V[1] = VectorCrossProduct(mat.V[0], mat.V[2]);
  // Restore scale.
  for (I = 0; I < 2; I++)
  {
	NormalizeVector(mat.V[I]);
	ScaleVector(mat.V[I], aScale.V[I]);
  }
  return mat;
}

void __fastcall TForm1::Button4Click(TObject *Sender)
{
  BoxMatrix = RandomRotation(BoxMatrix);
  Edit1Change(Sender);

}
//---------------------------------------------------------------------------
void __fastcall TForm1::Edit1Change(TObject *Sender)
{
  const float EditorsScale = 0.1;
  bool Res1;

  if (!Form1->Visible)
	exit;
  GLLines3->Nodes->Clear();
  // Calc data.
  BoxMatrix.V[3].V[0] = UpDown1->Position * EditorsScale;
  BoxMatrix.V[3].V[1] = UpDown2->Position * EditorsScale;
  BoxMatrix.V[3].V[2] = UpDown3->Position * EditorsScale;
  BoxMatrix.V[3].V[3] = 1;

  BoxScale.V[0] = UpDown4->Position * EditorsScale;
  BoxScale.V[1] = UpDown5->Position * EditorsScale;
  BoxScale.V[2] = UpDown6->Position * EditorsScale;

  SpherePos.V[0] = UpDown7->Position * EditorsScale;
  SpherePos.V[1] = UpDown8->Position * EditorsScale;
  SpherePos.V[2] = UpDown9->Position * EditorsScale;

  SphereRadius = UpDown10->Position * EditorsScale;


  // dCollideSphereBox function !
  Res1 = IntersectSphereBox(VectorMake(SpherePos, 1), SphereRadius, BoxMatrix,
	BoxScale, &intersPoint, &ResNormal);

  if (Res1)
  {
	// Intersected.
	Label1->Caption = "Intersected";
	DCCamTarget->Position->SetPoint(intersPoint);

	// Draw normal
	GLLines3->Nodes->AddNode(intersPoint);
	GLLines3->Nodes->AddNode(VectorAdd(intersPoint, VectorScale(
	  ResNormal, SphereRadius * 3)));

  }
  else
  {
	// Not intersected.
	Beep();
	Label1->Caption = "";
  }
  DCCamTarget->Visible = Res1;
  // Draw GLCube1 and GLSphere1.
  GLCube1->Matrix = BoxMatrix;
  GLCube1->CubeWidth = BoxScale.V[0];
  GLCube1->CubeHeight = BoxScale.V[1];
  GLCube1->CubeDepth = BoxScale.V[2];
  DCCube1->Matrix = GLCube1->Matrix;
  DCCube1->Scale->SetVector(BoxScale);
  GLSphere1->Position->SetPoint(SpherePos);
  GLSphere1->Radius = SphereRadius;
  GLSphere2->Position->SetPoint(SpherePos);
  GLSphere2->Radius = SphereRadius;

}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button3Click(TObject *Sender)
{
  Edit1Change(Sender);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ViewerMouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y)
{
  Viewer->SetFocus();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLCadencerProgress(TObject *Sender, const double deltaTime,
          const double newTime)
{
  if (Form1->Active)
	Viewer->Invalidate();

}
//---------------------------------------------------------------------------
void __fastcall TForm1::ViewerMouseMove(TObject *Sender, TShiftState Shift, int X,
          int Y)
{
  if (Shift.Contains(ssLeft))
	GLCamera1->MoveAroundTarget(mdy - Y, mdx - X);
  mdx = X;
  mdy = Y;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
          TPoint &MousePos, bool &Handled)
{
  if (Viewer->Focused())
	GLCamera1->AdjustDistanceToTarget(Power(1.02, WheelDelta / 120));
}
//---------------------------------------------------------------------------
