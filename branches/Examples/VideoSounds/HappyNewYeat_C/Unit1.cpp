//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
#include <JPEG.hpp>
#include <stdlib.h>
#include <GLKeyboard.hpp>
#include "math.h"
#include "Unit1.h"
#include "GLFPSMovement.hpp"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLGeomObjects"
#pragma link "GLNavigator"
#pragma link "GLFile3DS"
#pragma link "GLVectorGeometry"
#pragma link "GLKeyboard"

#pragma link "GLBitmapFont"
#pragma link "GLCadencer"
#pragma link "GLGeomObjects"
#pragma link "GLHUDObjects"
#pragma link "GLLensFlare"
#pragma link "GLObjects"
#pragma link "GLParticleFX"
#pragma link "GLScene"
#pragma link "GLShadowPlane"
#pragma link "GLSMBASS"
#pragma link "GLSound"
#pragma link "GLTexture"
#pragma link "GLVectorFileObjects"
#pragma link "GLWin32Viewer"
#pragma link "GLWindowsFont"
#pragma link "GLScreenSaver"
#pragma link "GLFireFX"
#pragma link "GLBaseClasses"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLMaterial"
#pragma resource "*.dfm"
TMain *Main;
  int  mx, my;
  double  fireLight;
  bool inPreview, inSaver;
  Cardinal  bStream;

//---------------------------------------------------------------------------
__fastcall TMain::TMain(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TMain::FormCreate(TObject *Sender)
{
    GLMaterialLibrary->Materials->Items[2]->TextureScale->X=20;
    GLMaterialLibrary->Materials->Items[2]->TextureScale->Y=10;

  GL->LoadFromFile("fireplace.3ds");
  FFFirTree->LoadFromFile("firtree.3ds");
   Randomize();
    fireLight=0.5;

}
//---------------------------------------------------------------------------


void __fastcall TMain::TimerTimer(TObject *Sender)
{
  int i, l;
  TDateTime t;
  AnsiString buf,v,s;
  Word y, m, d, hours, min, sec, msec, tt;

 Caption=Format("%.2f FPS", ARRAYOFCONST((Viewer->FramesPerSecond())));
  Viewer->ResetPerformanceMonitor();


   TGLBSoundEmitter *be;
  if(!GLSMBASS->Active)
    return;

   DecodeDate(Now(), y, m, d);
   DecodeTime(Now(),hours, min, sec, msec);
    if ((m==12 && d==31) || (m==1 && d==1))
   {
    DCGifts->Visible=true;
    if (d==1) { FTCountDown->Text="Happy New Year!"; }
    else
    {
      if (24-hours>1)
       {
          buf=IntToStr(23-hours)+" chacov...";
          buf=buf+IntToStr(60-min)+" minut...";
          FTCountDown->Text=buf;
        }
       else
        {
          buf=IntToStr(59-min)+" minut...";
          buf=buf+IntToStr(60-sec)+"  secund...";
          FTCountDown->Text=buf;
        }
     }
    }
   else
   {
    l=(12-m)*30.5+(30-StrToInt(d));
    buf=IntToStr(l+1)+" dney, ";
    buf=buf+IntToStr(24-hours)+" chasov...";
    FTCountDown->Text=buf;
   }
}
//---------------------------------------------------------------------------

void __fastcall TMain::GLCadencerProgress(TObject *Sender,
      const double deltaTime, const double newTime)
{
   Viewer->Invalidate();
   fireLight=ClampValue(fireLight+(1.0*rand()/RAND_MAX)*0.4-0.2, 0, 1);
   LSFire->Diffuse->Color=VectorLerp(clrYellow,
            VectorMake(0.5, 0, 0, 1), fireLight);
   LSFire->Position->Y=fireLight*0.1;

   if (inPreview==true){HSGLScene->Visible=false;}
   if (HSGLScene->Visible==true)
   {
    HSGLScene->Material->FrontProperties->Diffuse->Alpha=
    HSGLScene->Material->FrontProperties->Diffuse->
    Alpha-deltaTime*0.15;
    if (HSGLScene->Material->FrontProperties->Diffuse->Alpha<0.01)
        {HSGLScene->Visible=false;}
   }

}
//---------------------------------------------------------------------------
void __fastcall TMain::FormResize(TObject *Sender)
{
   GLCamera->SceneScale=Width/640;

}
//---------------------------------------------------------------------------

void __fastcall TMain::ViewerDblClick(TObject *Sender)
{
   if (inPreview==false && inSaver==false && Application->Terminated==false &&
       BorderStyle!=bsNone)
      {
      BorderStyle=bsNone;
      FormStyle=fsStayOnTop;
      Align=alClient;
      }
}
//---------------------------------------------------------------------------

void __fastcall TMain::ScreenSaverCloseQuery(TObject *Sender,
      bool &CanClose)
{
   Application->Terminate();
   CanClose=true;
}
//---------------------------------------------------------------------------

void __fastcall TMain::ScreenSaverPreview(TObject *Sender,
      HWND previewHwnd)
{
   inPreview=true;
}
//---------------------------------------------------------------------------
void __fastcall TMain::ScreenSaverExecute(TObject *Sender)
{
   inSaver=true;
   ShowMessage("New Year 2012");
}
//---------------------------------------------------------------------------

