@echo off
echo Copying DLLs to the windows system32 directory

copy cg.dll %SystemRoot%\System32\
copy cg.lib %SystemRoot%\System32\
copy cgGL.dll %SystemRoot%\System32\
copy cgGL.lib %SystemRoot%\System32\

copy SDL.dll %SystemRoot%\System32\
copy sdl.lib %SystemRoot%\System32\

echo Copying Sound's dll to the windows system32 directory
copy bass.dll %SystemRoot%\System32\
copy bass.lib %SystemRoot%\System32\
copy fmod.dll %SystemRoot%\System32\
copy fmod.lib %SystemRoot%\System32\

echo Copying ODE's dll to the windows system32 directory
copy ode.dll %SystemRoot%\System32\
copy ode.lib %SystemRoot%\System32\


pause