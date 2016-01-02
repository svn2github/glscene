@echo off
del *.dcr
BRCC32 -r -foGLSceneFMX.dcr GLSceneFMX.rc
BRCC32 -r -foGLSceneObjects.dcr GLSceneObjects.rc
BRCC32 -r -fononGLSceneFMX.dcr nonGLSceneFMX.rc
BRCC32 -r -foGLSceneFMXBass.dcr GLSceneFMXBass.rc
BRCC32 -r -foGLSceneFMXFMod.dcr GLSceneFMXFMod.rc
BRCC32 -r -foGLSceneFMXOpenAL.dcr GLSceneFMXOpenAL.rc
BRCC32 -r -foGLSceneFMXODE.dcr GLSceneFMXODE.rc
BRCC32 -r -foGLSceneFMXNGD.dcr GLSceneFMXNGD.rc
BRCC32 -r -foGLSceneFMXSDL.dcr GLSceneFMXSDL.rc
BRCC32 -r -foGLSceneFMXCg.dcr GLSceneFMXCg.rc
BRCC32 -r -foGLSceneFMXCUDA.dcr GLSceneFMXCUDA.rc
BRCC32 -r -foGLSceneRunTime.dcr GLSceneRunTime.rc
pause