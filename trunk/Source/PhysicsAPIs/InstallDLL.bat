@echo off
echo Copying ode.dll to the windows system32 directory

@echo Deploying for NT
copy ode.dll %SystemRoot%\System32\

@echo Deploying for Windows95 / Windows98 / WindowsME
copy ode.dll %Windir%\System\


pause