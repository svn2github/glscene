@echo off
del *.dcu /s
del *.ddp /s
del *.ppu /s
del *.o /s
del *.~* /s
del *.log /s
del *.dsk /s
del *.dof /s
del *.bk? /s
del *.mps /s
del *.rst /s
del *.s /s
del *.a /s
del *.map /s
del *.rsm /s
del *.drc /s
del *.2007 /s
del *.local /s
del *.cvsignore /s
del *.identcache /s
del *.otares /s
del *.tvsconfig /s
del *.stat /s
del *.hpp /s
del *.bpi /s
del *.lib /s

echo _
echo ************************************************
echo             Don't delete some files
echo ************************************************
echo _

attrib +R "AdvDemos/Q3Demo/Model/animation.cfg"
attrib +R "Source/DesignTime/Resources/lazres.exe"

rem del *.res /s
rem del *.cur /s
rem del *.exe /s
rem del *.cfg /s

attrib -R "AdvDemos/Q3Demo/Model/animation.cfg"
attrib -R "Source/DesignTime/Resources/lazres.exe"

for /r %1 %%R in (Win32) do if exist "%%R" (rd /s /q "%%R")
for /r %1 %%R in (Win64) do if exist "%%R" (rd /s /q "%%R")
for /r %1 %%R in (iOS) do if exist "%%R" (rd /s /q "%%R")
for /r %1 %%R in (Android) do if exist "%%R" (rd /s /q "%%R")
for /r %1 %%R in (Debug_Build) do if exist "%%R" (rd /s /q "%%R")
for /r %1 %%R in (Release_Build) do if exist "%%R" (rd /s /q "%%R")

for /r %1 %%R in (__history) do if exist "%%R" (rd /s /q "%%R")
for /r %1 %%R in (__recovery) do if exist "%%R" (rd /s /q "%%R")