@echo off
echo Copying DLLs to the windows system32 directory
copy cg.dll %SystemRoot%\System32\
copy cgGL.dll %SystemRoot%\System32\
copy sdl.dll %SystemRoot%\System32\
copy SDL.dll %SystemRoot%\System32\

echo Copying Sound's dll to the windows system32 directory
copy bass.dll %SystemRoot%\System32\
copy fmod.dll %SystemRoot%\System32\
copy OpenAL64.dll %SystemRoot%\System32\


echo Copying ODE's dll to the windows system32 directory
copy ode_single.dll %SystemRoot%\System32\
copy ode_singled.dll %SystemRoot%\System32\
copy ode_double.dll %SystemRoot%\System32\
copy ode_doubled.dll %SystemRoot%\System32\

echo Copying Newton's dll to the windows system32 directory
copy ode_single.dll %SystemRoot%\System32\
copy ode_singled.dll %SystemRoot%\System32\
copy newton.dll %SystemRoot%\System32\
copy newton_d.dll %SystemRoot%\System32\
copy dJointLibrary.dll %SystemRoot%\System32\
copy dJointLibrary_d.dll %SystemRoot%\System32\


copy zlib1.dll %SystemRoot%\System32\

pause