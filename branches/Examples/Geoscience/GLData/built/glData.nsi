; glData NSIS 2.0b3 Script
; http://gldata.sourceforge.net
; Written by Aaron Hochwimmer

; Configuration
; ---------------------------------------------------------
!include "MUI.nsh"
!define MUI_PRODUCT "glData"

SetDateSave on
SetDataBlockOptimize on
ShowInstDetails show
ShowUninstDetails show
CRCCheck on

InstType "Full"
InstType "Normal"
InstType "Minimal"

OutFile "output\gldata033.exe"

InstallDir "$PROGRAMFILES\${MUI_PRODUCT}"
; Remember Install Dir
InstallDirRegKey HKCU "Software\${MUI_PRODUCT}" ""

; Modern UI Configuration
!define MUI_VERSION "0.3.3"
!define MUI_WELCOMEPAGE

!define MUI_LICENSEPAGE
!define MUI_COMPONENTSPAGE
!define MUI_COMPONENTSPAGE_SMALLDESC
!define MUI_DIRECTORYPAGE

!define MUI_FINISHPAGE
!define MUI_FINISHPAGE_SHOWREADME "$INSTDIR\readme-gldata.txt"
!define MUI_FINISHPAGE_RUN "$INSTDIR\glData.exe"
 
!define MUI_ABORTWARNING

!define MUI_UNINSTALLER
!define MUI_UNCONFIRMPAGE

!define MUI_HEADERBITMAP "${NSISDIR}\Contrib\Icons\modern-header.bmp"

;--------------------------------
;Languages

  !insertmacro MUI_LANGUAGE "English"
;Language Strings

;Description
LangString DESC_SecCopyApp ${LANG_ENGLISH} "Install glData application."
LangString DESC_SecCopySampleFiles ${LANG_ENGLISH} "Install sample files."
LangString DESC_SecCopySampleData ${LANG_ENGLISH} "Install sample data."
Langstring DESC_SecCopySampleSurfer ${LANG_ENGLISH} "Install sample Surfer files."
Langstring DESC_SecCopySampleArcInfo ${LANG_ENGLISH} "Install sample ESRI ArcInfo files."
LangString DESC_SecCopySampleVTK ${LANG_ENGLISH} "Install vtk data files."
LangString DESC_SecCopySampleGeo ${LANG_ENGLISH} "Install sample TOUGH (geothermal simulation) grid."
LangString DESC_SecCopySpectrums ${LANG_ENGLISH} "Install colour spectrum (.clr) files."
LangString DESC_SecCopyHelp ${LANG_ENGLISH} "Install online help files."
LangString DESC_SecIcons ${LANG_ENGLISH} "Adds icons to your start menu and your desktop for easy access."

;--------------------------------

;Data
LicenseData MPL-1_1.txt 

Section "glData" SecCopyApp
  SectionIn 1 2 3 RO
  SetOutPath "$INSTDIR"
  File "glData.exe"
  File "readme-gldata.txt"
  File "changelog.txt"

;Tips:
  SetOutPath "$INSTDIR\data"
  File "data\tips.txt"	
  
  SetOutPath "$INSTDIR\photos"
  File "photos\readme-photos.txt"

;Create uninstaller
  WriteUninstaller "$INSTDIR\uninst-gldata.exe"
SectionEnd

Section "Colour Spectrum Files" SecCopySpectrums
  SectionIn 1 2
  SetOutPath "$INSTDIR\spectrums"
  File "spectrums\balt.clr"
  File "spectrums\base1.clr"
  File "spectrums\base2.clr" 
  File "spectrums\base3.clr"
  File "spectrums\blackband.clr"
  File "spectrums\brownblue.clr"
  File "spectrums\brownyellow.clr"
  File "spectrums\bywaves.clr"
  File "spectrums\carnival.clr"
  File "spectrums\chromadepth.clr"
  File "spectrums\colours1.clr"
  File "spectrums\colours2.clr"
  File "spectrums\cottoncandy.clr"
  File "spectrums\cret.clr"
  File "spectrums\cyanbands.clr"
  File "spectrums\desert.clr"
  File "spectrums\dirt.clr"
  File "spectrums\electric.clr"
  File "spectrums\forest.clr"
  File "spectrums\gr.clr"
  File "spectrums\grasswhite.clr"
  File "spectrums\grayscale.clr"
  File "spectrums\greens.clr"
  File "spectrums\gyr.clr"
  File "spectrums\highpoints1.clr"
  File "spectrums\highpoints2.clr"
  File "spectrums\icy.clr"
  File "spectrums\jura.clr"
  File "spectrums\land.clr"
  File "spectrums\landarid.clr"
  File "spectrums\landsea.clr"
  File "spectrums\landbw.clr"
  File "spectrums\lowpaleoz.clr"
  File "spectrums\magnit.clr"
  File "spectrums\merged1.clr"
  File "spectrums\merged2.clr"
  File "spectrums\midnight.clr"
  File "spectrums\mist.clr"
  File "spectrums\newyear.clr"
  File "spectrums\out.clr"
  File "spectrums\pastel1.clr"
  File "spectrums\pastel2.clr"
  File "spectrums\perm1.clr"
  File "spectrums\perm2.clr" 
  File "spectrums\pinkpea.clr"
  File "spectrums\purplebands.clr"
  File "spectrums\purplegreen.clr"
  File "spectrums\rainbow.clr"
  File "spectrums\rainbow2.clr"
  File "spectrums\redhot.clr"
  File "spectrums\redyellow_shaded.clr"
  File "spectrums\redzebra.clr"
  File "spectrums\relief_improved.clr"
  File "spectrums\relief_shade.clr"
  File "spectrums\simple1.clr"
  File "spectrums\simple2.clr"
  File "spectrums\steel.clr"
  File "spectrums\swamp.clr"
  File "spectrums\taffy.clr"
  File "spectrums\tanbrown.clr"
  File "spectrums\terrain.clr"
  File "spectrums\terrain2.clr"
  File "spectrums\tiedie.clr"
  File "spectrums\usflag.clr"
  File "spectrums\zebra.clr"
  File "spectrums\zoom.clr"
SectionEnd

Section "Online Help" SecCopyHelp
  SectionIn 1 2
  SetOutPath "$INSTDIR"
  File "..\docs\glData.chm"
SectionEnd

SubSection "Sample Files" SecCopySampleFiles

  Section "Data" SecCopySampleData
    SectionIn 1 2
    SetOutPath "$INSTDIR\samples"
    File "samples\blowmolding.csv"
    File "samples\glass.csv"
  SectionEnd

  Section "Surfer Grids" SecCopySampleSurfer
    SectionIn 1 2
    SetOutPath "$INSTDIR\samples\surfer"
    File "samples\surfer\demogrid.dat"
    File "samples\surfer\demogrid.grd"
  SectionEnd

  Section "ArcInfo Grids" SecCopySampleArcInfo
    SectionIn 1 2
    SetOutPath "$INSTDIR\samples\arcinfo"
    File "samples\arcinfo\kapiti.grd"
  SectionEnd

  Section "VTK Datasets" SecCopySampleVTK
    SectionIn 1 2
    SetOutPath "$INSTDIR\samples\vtk"
    File "samples\vtk\heart.vtk"
  SectionEnd

  Section "TOUGH Simulation Grids" SecCopySampleGeo
    SectionIn 1 2
    SetOutPath "$INSTDIR\samples\geogrids"
    File "samples\geogrids\blockpts.csv"
    File "samples\geogrids\blocks.csv"
    File "samples\geogrids\layers.csv"
    File "samples\geogrids\partial.csv"
    File "samples\geogrids\rockdist.csv"
    File "samples\geogrids\rocks.csv"
    File "samples\geogrids\vertexorder.csv"
    File "samples\geogrids\vertices.csv"
    File "samples\geogrids\welltemp.csv"
  SectionEnd

SubSectionEnd

Section "Start Menu + Desktop Shortcuts" SecIcons
  SectionIn 1 2
  SetOutPath "$INSTDIR"

  CreateDirectory "$SMPROGRAMS\${MUI_PRODUCT}"
  
  IfFileExists "$INSTDIR\glData.exe" "" +3
    CreateShortCut "$SMPROGRAMS\${MUI_PRODUCT}\glData.lnk" "$INSTDIR\glData.exe" ""
    CreateShortCut "$QUICKLAUNCH\glData.lnk" "$INSTDIR\glData.exe" ""
  
  WriteIniStr "$SMPROGRAMS\glData\glData Website.url" "InternetShortcut" "URL" "http://gldata.sourceforge.net/"
  
 IfFileExists "$INSTDIR\uninst-gldata.exe" "" +2
  CreateShortCut "$SMPROGRAMS\glData\Uninstall glData.lnk" "$INSTDIR\uninst-gldata.exe"
  
  IfFileExists "$INSTDIR\glData.exe" "" +2
    CreateShortCut "$DESKTOP\glData.lnk" "$INSTDIR\glData.exe"
SectionEnd

;Descriptions

!insertmacro MUI_FUNCTIONS_DESCRIPTION_BEGIN
  !insertmacro MUI_DESCRIPTION_TEXT ${SecCopyApp} $(DESC_SecCopyApp)
  !insertmacro MUI_DESCRIPTION_TEXT ${SecCopySampleFiles} $(DESC_SecCopySampleFiles)
  !insertmacro MUI_DESCRIPTION_TEXT ${SecCopySampleSurfer} $(DESC_SecCopySampleSurfer)
  !insertmacro MUI_DESCRIPTION_TEXT ${SecCopySampleArcInfo} $(DESC_SecCopySampleArcInfo)
  !insertmacro MUI_DESCRIPTION_TEXT ${SecCopySampleData} $(DESC_SecCopySampleData)
  !insertmacro MUI_DESCRIPTION_TEXT ${SecCopySampleVTK} $(DESC_SecCopySampleVTK) 
  !insertmacro MUI_DESCRIPTION_TEXT ${SecCopySampleGeo} $(DESC_SecCopySampleGeo)
  !insertmacro MUI_DESCRIPTION_TEXT ${SecCopySpectrums} $(DESC_SecCopySpectrums)
  !insertmacro MUI_DESCRIPTION_TEXT ${SecCopyHelp} $(DESC_SecCopyHelp)
  !insertmacro MUI_DESCRIPTION_TEXT ${SecIcons} $(DESC_SecIcons) 
!insertmacro MUI_FUNCTIONS_DESCRIPTION_END

;--------------------------------
;Uninstaller Section

Section "Uninstall"

  RMDir /r "$SMPROGRAMS\${MUI_PRODUCT}" ;force empty directory
  Delete "$QUICKLAUNCH\glData.lnk"
  Delete "$DESKTOP\glData.lnk"



; Delete the sample files

  Delete "$INSTDIR\samples\surfer\demogrid.dat"
  Delete "$INSTDIR\samples\surfer\demogrid.grd"
  Rmdir  "$INSTDIR\samples\surfer\" ;only delete if empty
  
  Delete "$INSTDIR\samples\arcinfo\kapiti.grd"
  RMDir  "$INSTDIR\samples\arcinfo" ;only delete if empty  

  Delete "$INSTDIR\samples\vtk\heart.vtk"
  RMDir  "$INSTDIR\samples\vtk" ;only delete if empty

  Delete "$INSTDIR\samples\geogrids\blockpts.csv" 
  Delete "$INSTDIR\samples\geogrids\blocks.csv"
  Delete "$INSTDIR\samples\geogrids\layers.csv"
  Delete "$INSTDIR\samples\geogrids\partial.csv"
  Delete "$INSTDIR\samples\geogrids\rockdist.csv"
  Delete "$INSTDIR\samples\geogrids\rocks.csv"
  Delete "$INSTDIR\samples\geogrids\vertexorder.csv"
  Delete "$INSTDIR\samples\geogrids\vertices.csv"
  Delete "$INSTDIR\samples\geogrids\welltemp.csv"
  RmDir  "$INSTDIR\samples\geogrids" ;only if empty
  
  Delete "$INSTDIR\samples\glass.csv"
  Delete "$INSTDIR\samples\blowmolding.csv"
  RMDir  "$INSTDIR\samples" ;only delete if empty

  Delete "$INSTDIR\spectrums\balt.clr"
  Delete "$INSTDIR\spectrums\base1.clr"
  Delete "$INSTDIR\spectrums\base2.clr" 
  Delete "$INSTDIR\spectrums\base3.clr"
  Delete "$INSTDIR\spectrums\blackband.clr"
  Delete "$INSTDIR\spectrums\brownblue.clr"
  Delete "$INSTDIR\spectrums\brownyellow.clr"
  Delete "$INSTDIR\spectrums\bywaves.clr"
  Delete "$INSTDIR\spectrums\carnival.clr"
  Delete "$INSTDIR\spectrums\chromadepth.clr"
  Delete "$INSTDIR\spectrums\colours1.clr"
  Delete "$INSTDIR\spectrums\colours2.clr"
  Delete "$INSTDIR\spectrums\cottoncandy.clr"
  Delete "$INSTDIR\spectrums\cret.clr"
  Delete "$INSTDIR\spectrums\cyanbands.clr"
  Delete "$INSTDIR\spectrums\desert.clr"
  Delete "$INSTDIR\spectrums\dirt.clr"
  Delete "$INSTDIR\spectrums\electric.clr"
  Delete "$INSTDIR\spectrums\forest.clr"
  Delete "$INSTDIR\spectrums\gr.clr"
  Delete "$INSTDIR\spectrums\grasswhite.clr"
  Delete "$INSTDIR\spectrums\grayscale.clr"
  Delete "$INSTDIR\spectrums\greens.clr"
  Delete "$INSTDIR\spectrums\gyr.clr"
  Delete "$INSTDIR\spectrums\highpoints1.clr"
  Delete "$INSTDIR\spectrums\highpoints2.clr"
  Delete "$INSTDIR\spectrums\icy.clr"
  Delete "$INSTDIR\spectrums\jura.clr"
  Delete "$INSTDIR\spectrums\land.clr"
  Delete "$INSTDIR\spectrums\landarid.clr"
  Delete "$INSTDIR\spectrums\landsea.clr"
  Delete "$INSTDIR\spectrums\landbw.clr"
  Delete "$INSTDIR\spectrums\lowpaleoz.clr"
  Delete "$INSTDIR\spectrums\magnit.clr"
  Delete "$INSTDIR\spectrums\merged1.clr"
  Delete "$INSTDIR\spectrums\merged2.clr"
  Delete "$INSTDIR\spectrums\midnight.clr"
  Delete "$INSTDIR\spectrums\mist.clr"
  Delete "$INSTDIR\spectrums\newyear.clr"
  Delete "$INSTDIR\spectrums\out.clr"
  Delete "$INSTDIR\spectrums\pastel1.clr"
  Delete "$INSTDIR\spectrums\pastel2.clr"
  Delete "$INSTDIR\spectrums\perm1.clr"
  Delete "$INSTDIR\spectrums\perm2.clr" 
  Delete "$INSTDIR\spectrums\pinkpea.clr"
  Delete "$INSTDIR\spectrums\purplebands.clr"
  Delete "$INSTDIR\spectrums\purplegreen.clr"
  Delete "$INSTDIR\spectrums\rainbow.clr"
  Delete "$INSTDIR\spectrums\rainbow2.clr"
  Delete "$INSTDIR\spectrums\redhot.clr"
  Delete "$INSTDIR\spectrums\redyellow_shaded.clr"
  Delete "$INSTDIR\spectrums\redzebra.clr"
  Delete "$INSTDIR\spectrums\relief_improved.clr"
  Delete "$INSTDIR\spectrums\relief_shade.clr"
  Delete "$INSTDIR\spectrums\simple1.clr"
  Delete "$INSTDIR\spectrums\simple2.clr"
  Delete "$INSTDIR\spectrums\steel.clr"
  Delete "$INSTDIR\spectrums\swamp.clr"
  Delete "$INSTDIR\spectrums\taffy.clr"
  Delete "$INSTDIR\spectrums\tanbrown.clr"
  Delete "$INSTDIR\spectrums\terrain.clr"
  Delete "$INSTDIR\spectrums\terrain2.clr"
  Delete "$INSTDIR\spectrums\tiedie.clr"
  Delete "$INSTDIR\spectrums\usflag.clr"
  Delete "$INSTDIR\spectrums\zebra.clr"
  Delete "$INSTDIR\spectrums\zoom.clr"
  RMDir  "$INSTDIR\spectrums" ;only delete if empty

  Delete "$INSTDIR\photos\readme-photos.txt"
  RmDir "$INSTDIR\photos" ;only if empty

  Delete "$INSTDIR\data\tips.txt"
  RmDir "$INSTDIR\data" ; only if empty

  Delete "$INSTDIR\glData.exe"
  Delete "$INSTDIR\glData.chm"
  Delete "$INSTDIR\changelog.txt"
  Delete "$INSTDIR\MPL-1_1.txt"
  Delete "$INSTDIR\readme-gldata.txt"

  Delete "$INSTDIR\uninst-gldata.exe"
  RMDir  "$INSTDIR" ;only delete if empty

  DeleteRegKey HKCU "Software\${MUI_PRODUCT}"
  
  ;Display the Finish header
  !insertmacro MUI_UNFINISHHEADER

SectionEnd

Function .onInit
  FindWindow $0 "TformMain" "glData"
    IsWindow $0 0 done
      MessageBox MB_OKCANCEL|MB_TOPMOST|MB_ICONEXCLAMATION "glData is currently running!$\nClick OK to have glData closed, or Cancel to quit the install" IDOK killgldata IDCANCEL quitme
  quitme:
    abort
  killgldata:
    SendMessage $0 16 0 0 
  done:
FunctionEnd


Function un.onInit
  FindWindow $0 "TformMain" "glData"
    IsWindow $0 0 done
      MessageBox MB_OKCANCEL|MB_TOPMOST|MB_ICONEXCLAMATION "glData is currently running!$\nClick OK to have glData closed, or Cancel to quit the uninstall" IDOK killgldata IDCANCEL quitme
  quitme:
    abort
  killgldata:
    SendMessage $0 16 0 0 
  done:
FunctionEnd