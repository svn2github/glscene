//---------------------------------------------------------------------------

#include <vcl.h>
#include <math.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLGeomObjects"
#pragma link "GLWin32Viewer"
#pragma link "GLSMBASS"
#pragma link "GLSMFMOD"
#pragma link "GLSound"
#pragma link "GLMisc"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLCadencer"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent * Owner):TForm(Owner)
{
// Load our sound sample
  GLSoundLibrary->Samples->AddFile("..\\..\\media\\drumloop.wav","");
  GLSoundLibrary->Samples->AddFile("..\\..\\media\\chimes.wav","");
  GLSoundLibrary->Samples->AddFile("..\\..\\media\\howl.mp3","");
}

//---------------------------------------------------------------------------

void __fastcall TForm1::SphereProgress(TObject * Sender,
                                       const double deltaTime,
                                       const double newTime)
{
  float alpha;

  // Move the red sphere (sound source) along an elliptic path
  alpha = 60.0 * DegToRad(newTime);
  ((TGLSphere *) Sender)->Position->SetPoint(sin(alpha) * 2, 0.5,
                                             cos(alpha) * 5);
}

//---------------------------------------------------------------------------
void __fastcall TForm1::TrackBarChange(TObject * Sender)
{
  // Rotate the listener around the vertical axis
  DummyCube->TurnAngle = TrackBar->Position;
  Application->ProcessMessages();
}

//---------------------------------------------------------------------------
void __fastcall TForm1::TrackBar1Change(TObject * Sender)
{
  // Move the listener forward/back
  Mickey->Position->Z = (float)TrackBar1->Position / 10.0;
  Application->ProcessMessages();
}

//---------------------------------------------------------------------------
void __fastcall TForm1::TimerTimer(TObject * Sender)
{
  String mngName;
  TGLSoundManager *sm;
  sm = ActiveSoundManager();

  // some stats
  if(dynamic_cast < TGLSMBASS * >(sm))
    mngName = "BASS";
  else if(dynamic_cast < TGLSMFMOD * >(sm))
    mngName = "FMOD";
  else
    mngName = "";
  if(sm)
    Caption = Format("%.2f FPS, %s CPU use : %.2f%%",
                     ARRAYOFCONST((GLSceneViewer->FramesPerSecond(), mngName,
                                   ActiveSoundManager()->CPUUsagePercent())));
  else
    Caption = "No active sound manager.";
  GLSceneViewer->ResetPerformanceMonitor();
}

//---------------------------------------------------------------------------
void __fastcall TForm1::RBFMODClick(TObject * Sender)
{
  TGLSoundManager *newManager, *sm;

  // This method switches managers. On a real world project, this would never
  // happen: you would choose and API and then cling to it, but the GLSS
  // completely wraps the underlying complexity and makes it a snap
  if(RBFMOD->Checked)
    newManager = GLSMFMOD;
  else
    newManager = GLSMBASS;

  sm = ActiveSoundManager();
  if(newManager != sm)
  {
    // shut down current one, and activate the new one
    if(sm)
    {
      sm->Active = False;
      newManager->Active = True;
      // restart sound
      GetOrCreateSoundEmitter(Sphere)->Playing = True;
    }
  }
}

//---------------------------------------------------------------------------
void __fastcall TForm1::Button1Click(TObject * Sender)
{
  TGLBSoundEmitter *se = new TGLBSoundEmitter(Mickey->Behaviours);
  se->Source->SoundLibrary = GLSoundLibrary;
  se->Source->SoundName = "chimes.wav";
  se->Playing = True;
}

//---------------------------------------------------------------------------
void __fastcall TForm1::Button2Click(TObject * Sender)
{
  TGLBSoundEmitter *se = new TGLBSoundEmitter(Mickey->Behaviours);
  se->Source->SoundLibrary = GLSoundLibrary;
  se->Source->SoundName = "howl.mp3";
  se->Playing = True;
}

//---------------------------------------------------------------------------

