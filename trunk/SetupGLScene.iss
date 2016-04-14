;-----------------------------------------------------------------------------
; Setup GLSceneVCL script for Inno Setup Compiler
;-----------------------------------------------------------------------------

#define GLSceneName "GLSceneVCL"
#define GLSceneVersion "1.4.1"
#define GLScenePublisher "GLSteam"
#define GLSceneURL "http://www.glscene.org/"

[Setup]
AppId={{8CF5F54E-C1FC-4716-BC82-908867D36AD6}
AppName={#GLSceneName}
AppVersion={#GLSceneVersion}
AppVerName=GLSceneVCL for Win32
AppCopyright=Copyright © 2000,2016 GLSteam
AppPublisher={#GLScenePublisher}
AppPublisherURL={#GLSceneURL}
AppSupportURL={#GLSceneURL}
AppUpdatesURL={#GLSceneURL}
;DefaultDirName={pf}\{#GLSceneName}
DefaultDirName=D:\GLScene\{#GLSceneName}
DefaultGroupName={#GLSceneName}
DisableProgramGroupPage=yes
OutputBaseFilename=SetupGLSceneVCL_v.1.4.1_for_RAD10_Seattle

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
Source: "Bpl\*"; DestDir: "{app}\Bpl"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "Dcp\*"; DestDir: "{app}\Dcp"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "external\*"; DestDir: "{app}\external"; Flags: ignoreversion
Source: "Help\*"; DestDir: "{app}\Help"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "hpp\*"; DestDir: "{app}\hpp"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "Packages\*"; DestDir: "{app}\Packages"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "Resources\*"; DestDir: "{app}\Resources"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "Samples\*"; DestDir: "{app}\Samples"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "Source\*"; DestDir: "{app}\Source"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "Utilities\*"; DestDir: "{app}\Utilities"; Flags: ignoreversion recursesubdirs createallsubdirs

[Registry]
; Parameters for GLScene
;Root: HKLM; Subkey: "Software\GLScene"; ValueType: string; ValueName: userGUID; ValueData: "0123456789"; Flags: createvalueifdoesntexist uninsdeletekey 
Root: HKCU; Subkey: "Software\GLScene\GLSceneVCL"; ValueType: string; ValueName: "Version"; ValueData: "1.5"; Flags: createvalueifdoesntexist uninsdeletekey 
Root: HKCU; Subkey: "Software\GLScene\GLSceneVCL"; ValueType: string; ValueName: InslallSettings; ValueData: "{src}\SetupGLSceneVCL.exe"; Flags: createvalueifdoesntexist uninsdeletekey 
Root: HKCU; Subkey: "Software\GLScene\GLSceneVCL"; ValueType: string; ValueName: LibraryDir; ValueData: "{app}"; Flags: createvalueifdoesntexist uninsdeletekey 

; Parameters for RAD Studio   
; Auto Save
Root: HKCU; Subkey: "Software\Embarcadero\BDS\17.0\Auto Save"; ValueType: string; ValueName: Desktop; ValueData: "True"; 
Root: HKCU; Subkey: "Software\Embarcadero\BDS\17.0\Auto Save"; ValueType: string; ValueName: Editor Files; ValueData: "True"; 
                     
; Environmental Variables, the ValueData needs to be changed from SourceDir to {app}   
; New user variable GLSVCLDIR
Root: HKCU; Subkey: "Software\Embarcadero\BDS\17.0\Environment Variables"; ValueType: string; ValueName: GLSVCLDIR; ValueData: "{app}"; Flags: deletevalue 
; Path to Bpl packages
Root: HKCU; Subkey: "Software\Embarcadero\BDS\17.0\Environment Variables"; ValueType: string; ValueName: Path; ValueData: "{olddata};{app}\Bpl\Win32\;C:\Users\Public\Documents\Embarcadero\Studio\17.0\Bpl\"; 

; Delphi Options
; Library Paths to sources
Root: HKCU; Subkey: "Software\Embarcadero\BDS\17.0\Library\Win32"; ValueType: string; ValueName: Search Path; ValueData: "{olddata};$(GLSVCLDIR)\Dcp\$(Platform);$(GLSVCLDIR)\Source;$(GLSVCLDIR)\Source\Basis;$(GLSVCLDIR)\Source\DesignTime;$(GLSVCLDIR)\Source\FileFormats;$(GLSVCLDIR)\Source\GameAPIs;$(GLSVCLDIR)\Source\ParallelAPIs;$(GLSVCLDIR)\Source\PhysicsAPIs;$(GLSVCLDIR)\Source\ScriptingAPIs;$(GLSVCLDIR)\Source\Shaders;$(GLSVCLDIR)\Source\SoundVideoAPIs";

; BPL Output Directory
; Package DPL Output
Root: HKCU; Subkey: "Software\Embarcadero\BDS\17.0\Library\Win32"; ValueType: string; ValueName: Package DPL Output; ValueData: "$(GLSVCLDIR)\Bpl\$(Platform)"; Flags: deletevalue
; DCP Output Directory
Root: HKCU; Subkey: "Software\Embarcadero\BDS\17.0\Library\Win32"; ValueType: string; ValueName: Package DCP Output; ValueData: "$(GLSVCLDIR)\Dcp\$(Platform)"; Flags: deletevalue
; HPP Output Directory
Root: HKCU; Subkey: "Software\Embarcadero\BDS\17.0\Library\Win32"; ValueType: string; ValueName: HPP Output Directory; ValueData: "$(GLSVCLDIR)\hpp\$(Platform)"; Flags: deletevalue
; Package BPL Search Path
Root: HKCU; Subkey: "Software\Embarcadero\BDS\17.0\Library\Win32"; ValueType: string; ValueName: Package Search Path; ValueData: "$(GLSVCLDIR)\Bpl\$(Platform)"; Flags: deletevalue

; C++Builder Options
; Include Path to hpp headers
Root: HKCU; Subkey: "Software\Embarcadero\BDS\17.0\C++\Paths\Win32"; ValueType: string; ValueName: IncludePath; ValueData: "{olddata};$(GLSVCLDIR)\hpp\$(Platform)"; 
Root: HKCU; Subkey: "Software\Embarcadero\BDS\17.0\C++\Paths\Win32"; ValueType: string; ValueName: UserIncludePath; ValueData: "$(GLSVCLDIR)\hpp\$(Platform)"; Flags: deletevalue
; Library Path to LIB/BPI files
Root: HKCU; Subkey: "Software\Embarcadero\BDS\17.0\C++\Paths\Win32"; ValueType: string; ValueName: LibraryPath; ValueData: "{olddata};$(GLSVCLDIR)\Dcp\$(Platform)";
; Package BPL output directory 
Root: HKCU; Subkey: "Software\Embarcadero\BDS\17.0\C++\Paths\Win32"; ValueType: string; ValueName: BPLOutput; ValueData: "$(GLSVCLDIR)\Bpl\$(Platform)"; Flags: deletevalue
; Package BPI/LIB output directory 
Root: HKCU; Subkey: "Software\Embarcadero\BDS\17.0\C++\Paths\Win32"; ValueType: string; ValueName: BPIOutput; ValueData: "$(GLSVCLDIR)\Dcp\$(Platform)"; Flags: deletevalue
; Search path
Root: HKCU; Subkey: "Software\Embarcadero\BDS\17.0\C++\Paths\Win32"; ValueType: string; ValueName: SearchPath; ValueData: "{olddata};$(GLSVCLDIR)\Bpl\$(Platform)"; 

; Known Packages
Root: HKCU; Subkey: "Software\Embarcadero\BDS\17.0\Known Packages"; ValueType: string; ValueName: $(GLSVCLDIR)\Bpl\Win32\GLSceneVCL_Cg_DesignTime.bpl; ValueData: "GLSceneVCL Cg Shaders"; Flags: createvalueifdoesntexist uninsdeletevalue
Root: HKCU; Subkey: "Software\Embarcadero\BDS\17.0\Known Packages"; ValueType: string; ValueName: $(GLSVCLDIR)\Bpl\Win32\GLSceneVCL_Parallel_DesignTime.bpl; ValueData: "GLSceneVCL GPU Computing"; Flags: createvalueifdoesntexist uninsdeletevalue
Root: HKCU; Subkey: "Software\Embarcadero\BDS\17.0\Known Packages"; ValueType: string; ValueName: $(GLSVCLDIR)\Bpl\Win32\GLSceneVCL_DesignTime.bpl; ValueData: "GLSceneVCL OpenGL 3D library"; Flags: createvalueifdoesntexist uninsdeletevalue
Root: HKCU; Subkey: "Software\Embarcadero\BDS\17.0\Known Packages"; ValueType: string; ValueName: $(GLSVCLDIR)\Bpl\Win32\GLSceneVCL_Physics_DesignTime.bpl; ValueData: "GLSceneVCL Physics Managers"; Flags: createvalueifdoesntexist uninsdeletevalue
Root: HKCU; Subkey: "Software\Embarcadero\BDS\17.0\Known Packages"; ValueType: string; ValueName: $(GLSVCLDIR)\Bpl\Win32\GLSceneVCL_Sounds_DesignTime.bpl; ValueData: "GLSceneVCL Sound Managers"; Flags: createvalueifdoesntexist uninsdeletevalue

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
; Installation of DLLs in System32 and SysWOW64 directories 
Filename: "{app}\external\SetupDLLs.bat"
