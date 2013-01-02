//---------------------------------------------------------------------------
/*
   This demo illustrates several GLScene components:
   - TerrainRenderer, used with a material library
   - TerrainRenderer's OnHeightDataPostRender, used to render sea surface
   - HeightTileFileHDS, used as primary elevation datasource
   - CustomHDS, used to attach texturing information to the elevation samples
   - DirectOpenGL, used to render the sailboat's wake

   Note that both custom OpenGL rendering sections are interrelated, the sea
   surface rendering code also setups the stencil buffer, which is used by
   the wake rendering code.

   Credits:
   - Terrain elevation map and textures : Mattias Fagerlund
     (http://www.cambrianlabs.com/Mattias/)
   - Sailboat model and textures : Daniel Polli / Daniel@dansteph.com
     (http://virtualsailor.dansteph.com)

   Eric Grange (http://glscene.org)
*/
//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("Archipelago.res");
USEFORM("Unit1.cpp", Form1);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TForm1), &Form1);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
