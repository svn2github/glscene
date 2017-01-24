@echo off
del *.dcr
BRCC32 -r -foVKScene.dcr VKScene.rc
BRCC32 -r -foVKSceneObjects.dcr VKSceneObjects.rc
BRCC32 -r -fononVKScene.dcr nonVKScene.rc
BRCC32 -r -foVKSceneBass.dcr VKSceneBass.rc
BRCC32 -r -foVKSceneFMod.dcr VKSceneFMod.rc
BRCC32 -r -foVKSceneOpenAL.dcr VKSceneOpenAL.rc
BRCC32 -r -foVKSceneODE.dcr VKSceneODE.rc
BRCC32 -r -foVKSceneNGD.dcr VKSceneNGD.rc
BRCC32 -r -foVKSceneSDL.dcr VKSceneSDL.rc
BRCC32 -r -foVKSceneCg.dcr VKSceneCg.rc
BRCC32 -r -foVKSceneCUDA.dcr VKSceneCUDA.rc
BRCC32 -r -foVKSceneDWS.dcr VKSceneDWS.rc
BRCC32 -r -foVKSceneRunTime.dcr VKSceneRunTime.rc
pause