@echo off
echo Copying DLLs to the Windows SysWOW64 directory

echo Copying CG DLLs
copy cg.dll %SystemRoot%\SysWOW64\
copy cgGL.dll %SystemRoot%\SysWOW64\
copy sdl.dll %SystemRoot%\SysWOW64\
copy SDL.dll %SystemRoot%\SysWOW64\

echo Copying Sound's DLLs
copy bass.dll %SystemRoot%\SysWOW64\
copy fmod.dll %SystemRoot%\SysWOW64\
copy OpenAL32.dll %SystemRoot%\SysWOW64\
copy OpenAL64.dll %SystemRoot%\SysWOW64\


echo Copying ODE DLLs
copy ode_single.dll %SystemRoot%\SysWOW64\
copy ode_singled.dll %SystemRoot%\SysWOW64\
copy ode_double.dll %SystemRoot%\SysWOW64\
copy ode_doubled.dll %SystemRoot%\SysWOW64\

echo Copying Newton DLLs
copy newton.dll %SystemRoot%\SysWOW64\
copy newton_d.dll %SystemRoot%\SysWOW64\
copy dJointLibrary.dll %SystemRoot%\SysWOW64\
copy dJointLibrary_d.dll %SystemRoot%\SysWOW64\

echo Copying zlib1 DLL
copy zlib1.dll %SystemRoot%\SysWOW64\

pause