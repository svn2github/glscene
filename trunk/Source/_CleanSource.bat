del *.dcu /s
del *.ppu /s
del *.o /s
del *.~* /s
del *.cfg /s
del *.dsk /s
del *.dof /s
del *.bk? /s
del *.mps /s
del *.rst /s
del *.s /s
del *.a /s
echo **************************************
echo   DELETE AUTO-GENERATED CPP FILES 
echo **************************************
rem del *.hpp /s
rem del *.bpi /s
rem del *.lib /s
for /r %1 %%R in (__history) do if exist "%%R" (rd /s /q "%%R")