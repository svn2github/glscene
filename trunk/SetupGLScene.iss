;-----------------------------------------------------------------------------
; Setup GLSceneVCL script for Inno Setup Compiler
;-----------------------------------------------------------------------------
; GLSTeam (c) 2000-2016

; !!! TODO Registry parameters !!!

#define GLSceneName "GLSceneVCL"
#define GLSceneVersion "1.5"
#define GLScenePublisher "GLSteam Inc."
#define GLSceneURL "http://www.glscene.org/"

[Setup]
AppId={{8CF5F54E-C1FC-4716-BC82-908867D36AD6}
AppName={#GLSceneName}
AppVersion={#GLSceneVersion}
AppVerName=GLScene for Win32
AppCopyright=Copyright © 2016 GLSteam, Inc.
AppPublisher={#GLScenePublisher}
AppPublisherURL={#GLSceneURL}
AppSupportURL={#GLSceneURL}
AppUpdatesURL={#GLSceneURL}
DefaultDirName={pf}\{#GLSceneName}
DefaultGroupName={#GLSceneName}
DisableProgramGroupPage=yes
OutputBaseFilename=SetupGLScene

; Source directory of files
; SourceDir=D:\Library\GLSceneVCL
; Output directory for setup program
OutputDir=D:\GLS\Installation   

InfoBeforeFile=Help\en\Introduction.txt
InfoAfterFile=Samples\Readme.txt

Compression=lzma
SetupIconFile=Samples\media\gls.ico
SolidCompression=yes

;welcome image
WizardImageFile=Samples\media\GLSlogo.bmp  
WizardImageBackColor= clMaroon 
WizardImageStretch=yes
WizardSmallImageFile=Samples\media\GLS.bmp
WizardSmallImageBackColor=clNavy  

;background
WindowVisible=yes 
BackColor=clPurple
BackColor2=clMaroon
;BackColorDirection= lefttoright

;full screen installer
WindowShowCaption=no 
WindowStartMaximized=yes 

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"; LicenseFile: "Help\en\License.txt"
Name: "french"; MessagesFile: "compiler:Languages\French.isl"; 
Name: "russian"; MessagesFile: "compiler:Languages\Russian.isl"; LicenseFile: "Help\ru\License.txt"
Name: "german"; MessagesFile: "compiler:Languages\German.isl"
Name: "spanish"; MessagesFile: "compiler:Languages\Spanish.isl"

[Types]
Name: "Full"; Description: "All comps"
Name: "Custom"; Description: "Choose comps"; Flags: iscustom

[Components]
;Name: "Samples"; Description: "Samples for Delphi&C++Builder"; Types: Full Custom 
;Name: "Utilities"; Description: "Utilities for GLScene"; Types: Full Custom 

[Files]
Source: "CleanForRelease.bat"; DestDir: "{app}"; Flags: ignoreversion
;Source: "external\*"; DestDir: "{app}\external"; Flags: ignoreversion
Source: "Help\*"; DestDir: "{app}\Help"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "Packages\*"; DestDir: "{app}\Packages"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "Resources\*"; DestDir: "{app}\Resources"; Flags: ignoreversion recursesubdirs createallsubdirs
;Source: "Samples\*"; Components: Samples; DestDir: "{app}\Samples"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "Source\*"; DestDir: "{app}\Source"; Flags: ignoreversion recursesubdirs createallsubdirs
;Source: "Utilities\*"; Components: Utilities; DestDir: "{app}\Utilities"; Flags: ignoreversion recursesubdirs createallsubdirs

[Registry]
; Parameters for GLScene
Root: HKCU; Subkey: "Software\GLScene\GLSceneVCL"; ValueType: string; ValueName: "Version"; ValueData: "1.5"; Flags: createvalueifdoesntexist uninsdeletekey 
Root: HKCU; Subkey: "Software\GLScene\GLSceneVCL"; ValueType: string; ValueName: InslallSettings; ValueData: "{src}\SetupGLScene.exe"; Flags: createvalueifdoesntexist uninsdeletekey 
Root: HKCU; Subkey: "Software\GLScene\GLSceneVCL"; ValueType: string; ValueName: LibraryDir; ValueData: "{app}"; Flags: createvalueifdoesntexist uninsdeletekey 

; Parameters for RAD Studio                        
; Environmental Variables, the ValueData needs to be changed from SourceDir to {app}   

;Root: HKCU; Subkey: "Software\Embarcadero\BDS\17.0\Environment Variables"; ValueType: string; ValueName: GLSVCLDIR; ValueData: "D:\GLScene\GLSceneVCL"; Flags: uninsdeletekey                                                                                                        
;Root: HKLM; Subkey: "Software\Company\Program\Settings"; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"

; it's necessary to add environmental variables GLSVCLDIR and GLSVKSDIR to registry in sections
; "Software\Embarcadero\BDS\17.0\Environment Variables"; then to add for Delphi
; "Software\Embarcadero\BDS\17.0\Library\Win32\Search Path" for $(GKSVCLDIR)\Source; + all subdirs and
; "Software\Embarcadero\BDS\17.0\Library\Win64\Search Path" for $(GKSVKSDIR)\Source; + all subdirs and
; and for C++Builder
;
;Root: HKLM; Subkey: "Software\Embarcadero\BDS\15.0\Known Packages";"REG_SZ"; C:\Users\Public\Documents\Embarcadero\Studio\15.0\Bpl\GLSceneVCL_DesignTime.bpl; ValueData: "{app}"
;Root: HKLM; Subkey: "Software\Embarcadero\BDS\16.0\Known Packages";"REG_SZ"; C:\Users\Public\Documents\Embarcadero\Studio\16.0\Bpl\GLSceneVCL_DesignTime.bpl; ValueData: "{app}"
;Root: HKLM; Subkey: "Software\Embarcadero\BDS\17.0\Known Packages";"REG_SZ"; C:\Users\Public\Documents\Embarcadero\Studio\17.0\Bpl\GLSceneVCL_DesignTime.bpl; ValueData: "{app}"

[Code]
function IsRegularUser(): Boolean;
begin
  Result := not (IsAdminLoggedOn or IsPowerUserLoggedOn)
end;
 
function GetDefRoot(Param: String): String;
begin
  if IsRegularUser then
    Result := ExpandConstant('{localappdata}')
  else
    Result := ExpandConstant('{pf}')
end;

[Run]
Filename: "{app}\SetupDLLs.bat"
