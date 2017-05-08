@echo off
del *.dcr
BRCC32 -r -foVXScene.dcr VXScene.rc
BRCC32 -r -foVXSceneObjects.dcr VXSceneObjects.rc
BRCC32 -r -fononVXScene.dcr nonVXScene.rc
BRCC32 -r -foVXSceneBass.dcr VXSceneBass.rc
BRCC32 -r -foVXSceneFMod.dcr VXSceneFMod.rc
BRCC32 -r -foVXSceneOpenAL.dcr VXSceneOpenAL.rc
BRCC32 -r -foVXSceneODE.dcr VXSceneODE.rc
BRCC32 -r -foVXSceneNGD.dcr VXSceneNGD.rc
BRCC32 -r -foVXSceneSDL.dcr VXSceneSDL.rc
BRCC32 -r -foVXSceneCg.dcr VXSceneCg.rc
BRCC32 -r -foVXSceneCUDA.dcr VXSceneCUDA.rc
BRCC32 -r -foVXSceneDWS.dcr VXSceneDWS.rc
BRCC32 -r -foVXSceneRunTime.dcr VXSceneRunTime.rc
pause