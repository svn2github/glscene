//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("FurBall.res");
USEFORM("fFurBall.cpp", frmFurBall);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TfrmFurBall), &frmFurBall);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
