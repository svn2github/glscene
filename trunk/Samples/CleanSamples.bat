echo off
del *.exe /s  
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

rem delete more files

del *.bak /s
del *.xml /s
del *.cvsignore /s
del *.identcache /s
del *.otares /s
del *.tvsconfig /s

rem delete cpp builder files

del *.hpp /s
rem del *.obj /s 
del *.#00 /s
del *.pch /s
del *.tds /s
del *.ilc /s
del *.ild /s
del *.ilf /s
del *.ils /s
del *.pdi /s
del *.vlb /s


echo _
echo ************************************************
echo             Don't delete some files
echo ************************************************
echo _


rem del *.cfg /s  animations


echo --------------------------------------------------------
echo delete all .svn directories with subdirectories and files 
for /r %1 %%R in (.svn) do if exist "%%R" (rd /s /q "%%R")
echo---------------------------------------------------------
echo delete debug and Platform directories with all subdirectories and files 
for /r %1 %%R in (Win32) do if exist "%%R" (rd /s /q "%%R")
for /r %1 %%R in (Win64) do if exist "%%R" (rd /s /q "%%R")
for /r %1 %%R in (Debug_Build) do if exist "%%R" (rd /s /q "%%R")
for /r %1 %%R in (Release_Build) do if exist "%%R" (rd /s /q "%%R")