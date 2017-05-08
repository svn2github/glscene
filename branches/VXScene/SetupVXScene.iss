;-----------------------------------------------------------------------------
; Setup VXScene script for Inno Setup Compiler
;-----------------------------------------------------------------------------

#define VXSceneName "VXScene"
#define VXSceneVersion "v.1.0.0_for_Win64"
#define VXScenePublisher "VXSteam"
#define VXSceneURL "http://glscene.sourceforge.net"

[Setup]
AppId={{8CF5F54E-C1FC-4716-BC82-908867D36AD6}
AppName={#VXSceneName}
AppVersion={#VXSceneVersion}
AppVerName=VXScene for Win64
AppCopyright=Copyright © 2000,2017 VXSteam
AppPublisher={#VXScenePublisher}
AppPublisherURL={#VKSceneURL}
AppSupportURL={#VXSceneURL}
AppUpdatesURL={#VXSceneURL}
;DefaultDirName={pf}\{#VXSceneName}
DefaultDirName=D:\Program Files\{#VXSceneName}
DefaultGroupName={#VXSceneName}
DisableProgramGroupPage=yes
OutputBaseFilename=SetupVXScene_{#VXSceneVersion}

; Source directory of files
; SourceDir=D:\VXScene
; Output directory for setup program
OutputDir=D:\GLS\Installation   

InfoBeforeFile=Help\en\Introduction.txt
InfoAfterFile=Samples\Readme.txt

Compression=lzma
SetupIconFile=Samples\media\vks.ico
SolidCompression=yes

;welcome image
WizardImageFile=Samples\media\VKSlogo.bmp  
WizardImageBackColor= clMaroon 
WizardImageStretch=yes
WizardSmallImageFile=Samples\media\VXScene.bmp
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
Name: "german"; MessagesFile: "compiler:Languages\German.isl"; 
Name: "spanish"; MessagesFile: "compiler:Languages\Spanish.isl"; 

[Types]
Name: "Full"; Description: "All comps"
Name: "Custom"; Description: "Choose comps"; Flags: iscustom

[Components]
;Name: "Samples"; Description: "Samples"; Types: Full Custom 
;Name: "Utilities"; Description: "Utilities"; Types: Full Custom 

[Code]
function IsPackageDir: Boolean;
begin
//  if DirExist()
//  then
//  begin
//  end;
end;

[Files]
Source: "CleanForRelease.bat"; DestDir: "{app}"; Flags: ignoreversion
Source: "CleanForRelease.bat"; DestDir: "{app}"; Flags: ignoreversion
Source: "C:\Users\Public\Documents\Embarcadero\Studio\18.0\Bpl\*"; DestDir: "{app}\bpl"; Flags: ignoreversion
Source: "external\*"; DestDir: "{app}\external"; Flags: ignoreversion
Source: "Help\*"; DestDir: "{app}\Help"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "include\*"; DestDir: "{app}\include"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "lib\*"; DestDir: "{app}\lib"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "Packages\*"; DestDir: "{app}\Packages"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "Resources\*"; DestDir: "{app}\Resources"; Flags: ignoreversion recursesubdirs createallsubdirs
;Source: "Samples\*"; DestDir: "{app}\Samples"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "Source\*"; DestDir: "{app}\Source"; Flags: ignoreversion recursesubdirs createallsubdirs
;Source: "Utilities\*"; DestDir: "{app}\Utilities"; Flags: ignoreversion recursesubdirs createallsubdirs

[Code]
function IsDadRegistryExist: Boolean;
begin
  if RegKeyExists(HKEY_CURRENT_USER, 'Software\Embarcadero\BDS\16.0') or    
     RegKeyExists(HKEY_CURRENT_USER, 'Software\Embarcadero\BDS\17.0') or
     RegKeyExists(HKEY_CURRENT_USER, 'Software\Embarcadero\BDS\18.0')
  then
  begin
    /// "Yes". Update 
     
  end  
  else
    begin
      if MsgBox('Do you really want to install GLScene?', mbError, MB_YESNO) = idYes
      then 
        /// Full installation
      else 
    end;
end;

[Registry]
; Parameters for VXScene
Root: HKCU; Subkey: "Software\VXScene"; ValueType: string; ValueName: "Version"; ValueData: {#VXSceneVersion}; Flags: createvalueifdoesntexist uninsdeletekey 
Root: HKCU; Subkey: "Software\VXScene"; ValueType: string; ValueName: InslallSettings; ValueData: "{src}\SetupVXScene.exe"; Flags: createvalueifdoesntexist uninsdeletekey 
Root: HKCU; Subkey: "Software\VXScene"; ValueType: string; ValueName: LibraryDir; ValueData: "{app}"; Flags: createvalueifdoesntexist uninsdeletekey 

; Parameters for RAD Studio   
; Auto Save
Root: HKCU; Subkey: "Software\Embarcadero\BDS\18.0\Auto Save"; ValueType: string; ValueName: Desktop; ValueData: "True"; 
Root: HKCU; Subkey: "Software\Embarcadero\BDS\18.0\Auto Save"; ValueType: string; ValueName: Editor Files; ValueData: "True"; 
                     
; Environmental Variables, the ValueData needs to be changed from SourceDir to {app}   
; New user variable VXSceneDIR
Root: HKCU; Subkey: "Software\Embarcadero\BDS\18.0\Environment Variables"; ValueType: string; ValueName: VXSceneDIR; ValueData: "{app}"; Flags: deletevalue 

; Delphi Options
; Library Paths to sources
Root: HKCU; Subkey: "Software\Embarcadero\BDS\18.0\Library\Win32"; ValueType: string; ValueName: Search Path; ValueData: "{olddata};$(VXSceneDIR)\Source;$(VXSceneDIR)\Source\Basis;$(VXSceneDIR)\Source\DesignTime;$(VXSceneDIR)\Source\FileFormats;$(VXSceneDIR)\Source\GameAPIs;$(VXSceneDIR)\Source\ParallelAPIs;$(VXSceneDIR)\Source\PhysicsAPIs;$(VXSceneDIR)\Source\ScriptingAPIs;$(VXSceneDIR)\Source\Shaders;$(VXSceneDIR)\Source\SoundVideoAPIs";

; C++Builder Options
; Include Path to hpp headers
Root: HKCU; Subkey: "Software\Embarcadero\BDS\18.0\C++\Paths\Win32"; ValueType: string; ValueName: IncludePath; ValueData: "{olddata};$(VXSceneDIR)\include"; 
; Library Path to LIB/BPI files
Root: HKCU; Subkey: "Software\Embarcadero\BDS\18.0\C++\Paths\Win32"; ValueType: string; ValueName: LibraryPath; ValueData: "{olddata};$(VXSceneDIR)\lib";

; Known Packages
Root: HKCU; Subkey: "Software\Embarcadero\BDS\18.0\Known Packages"; ValueType: string; ValueName: $(BDSCOMMONDIR)\Bpl\Win32\VXScene_Cg_DesignTime.bpl; ValueData: "VXScene Cg Shaders"; Flags: createvalueifdoesntexist uninsdeletevalue
Root: HKCU; Subkey: "Software\Embarcadero\BDS\18.0\Known Packages"; ValueType: string; ValueName: $(BDSCOMMONDIR)\Bpl\Win32\VXScene_Parallel_DesignTime.bpl; ValueData: "VXScene GPU Computing"; Flags: createvalueifdoesntexist uninsdeletevalue
Root: HKCU; Subkey: "Software\Embarcadero\BDS\18.0\Known Packages"; ValueType: string; ValueName: $(BDSCOMMONDIR)\Bpl\Win32\VXScene_DesignTime.bpl; ValueData: "VXScene OpenGL 3D library"; Flags: createvalueifdoesntexist uninsdeletevalue
Root: HKCU; Subkey: "Software\Embarcadero\BDS\18.0\Known Packages"; ValueType: string; ValueName: $(BDSCOMMONDIR)\Bpl\Win32\VXScene_Physics_DesignTime.bpl; ValueData: "VXScene Physics Managers"; Flags: createvalueifdoesntexist uninsdeletevalue
Root: HKCU; Subkey: "Software\Embarcadero\BDS\18.0\Known Packages"; ValueType: string; ValueName: $(BDSCOMMONDIR)\Bpl\Win32\VXScene_Sounds_DesignTime.bpl; ValueData: "VXScene Sound Managers"; Flags: createvalueifdoesntexist uninsdeletevalue

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
