//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "GLBitmapFont.hpp"
#include "GLCadencer.hpp"
#include "GLHeightData.hpp"
#include "GLHeightTileFileHDS.hpp"
#include "GLHUDObjects.hpp"
#include "GLMisc.hpp"
#include "GLObjects.hpp"
#include "GLScene.hpp"
#include "GLSkydome.hpp"
#include "GLTerrainRenderer.hpp"
#include "GLTexture.hpp"
#include "GLVectorFileObjects.hpp"
#include "GLWin32Viewer.hpp"
#include "GLWindowsFont.hpp"
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include <JPeg.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
        TGLSceneViewer *GLSceneViewer;
        TPanel *PAProgress;
        TLabel *Label1;
        TProgressBar *ProgressBar;
        TGLScene *GLScene1;
        TGLSkyDome *SkyDome;
        TGLDummyCube *DCCamera;
        TGLCamera *GLCamera;
        TGLTerrainRenderer *TerrainRenderer;
        TGLDirectOpenGL *DOWake;
        TGLFreeForm *FFSailBoat;
        TGLLightSource *LSSun;
        TGLHUDText *HTFPS;
        TGLHUDText *HTHelp;
        TTimer *Timer1;
        TGLCadencer *GLCadencer;
        TGLMaterialLibrary *MaterialLibrary;
        TGLHeightTileFileHDS *GLHeightTileFileHDS1;
        TGLWindowsBitmapFont *BFSmall;
        TGLCustomHDS *GLCustomHDS1;
        TGLMemoryViewer *GLMemoryViewer1;
        TGLMaterialLibrary *MLSailBoat;
        TGLWindowsBitmapFont *BFLarge;
        void __fastcall FormCreate(TObject *Sender);
        void __fastcall GLCadencerProgress(TObject *Sender,
          const double deltaTime, const double newTime);
        void __fastcall Timer1Timer(TObject *Sender);
        void __fastcall FormKeyPress(TObject *Sender, char &Key);
        void __fastcall GLCustomHDS1MarkDirtyEvent(const TRect &area);
        void __fastcall GLCustomHDS1StartPreparingData(
          THeightData *heightData);
        void __fastcall GLSceneViewerBeforeRender(TObject *Sender);
        void __fastcall TerrainRendererHeightDataPostRender(
          TRenderContextInfo &rci, const TList *heightDatas);
        void __fastcall DOWakeProgress(TObject *Sender,
          const double deltaTime, const double newTime);
        void __fastcall DOWakeRender(TObject *Sender,
          TRenderContextInfo &rci);
private:	// User declarations
public:		// User declarations
        bool FullScreen;
        float CamHeight;
        int WaterPolyCount;
        bool WaterPlane;
        bool WasAboveWater;
        float HelpOpacity;

        TAffineVectorList *WakeVertices;
        TAffineVectorList *WakeStretch;
        TSingleList *WakeTime;
        __fastcall TForm1(TComponent* Owner);
        void ResetMousePos(void);
        float WaterPhase(const float px, const float py);
        float WaterHeight(const float px, const float py);
        void IssuePoint(THeightData *hd, int x, int y, int s2, float t, int rx, int ry);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
