@echo off
echo Copying cg.dll to the windows system32 directory

@echo Deploying for NT
copy cg.dll %SystemRoot%\System32\

@echo off
echo Copying cgGL.dll to the windows system32 directory

@echo Deploying for NT
copy cgGL.dll %SystemRoot%\System32\

@echo off
echo Copying SDL.dll to the windows system32 directory

@echo Deploying for NT
copy SDL.dll %SystemRoot%\System32\

@echo off
echo Copying bass.dll to the windows system32 directory

@echo Deploying for NT
copy bass.dll %SystemRoot%\System32\

@echo off
echo Copying fmod.dll to the windows system32 directory

@echo Deploying for NT
copy fmod.dll %SystemRoot%\System32\

@echo off
echo Copying ODE's dll to the windows system32 directory

@echo Deploying for NT
copy ode_single.dll %SystemRoot%\System32\
copy ode_singled.dll %SystemRoot%\System32\
copy ode_double.dll %SystemRoot%\System32\
copy ode_doubled.dll %SystemRoot%\System32\

pause