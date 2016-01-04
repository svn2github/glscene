@echo off
del DGLEngineVCL.dcr
del DGLEngineObjects.dcr
BRCC32 -r -foDGLEngineVCL.dcr DGLEngineVCL.rc
BRCC32 -r -foDGLEngineObjects.dcr DGLEngineObjects.rc
pause