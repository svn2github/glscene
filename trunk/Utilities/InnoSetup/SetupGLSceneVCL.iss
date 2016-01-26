;-----------------------------------------------------------------------------
; Setup GLSceneVCL script for Inno Setup Compiler
;-----------------------------------------------------------------------------
; GLSTeam (c) 2000-2016

#define GLSceneName "GLSceneVCL"
#define GLSceneVersion "1.4"
#define GLScenePublisher "GLSTeam Inc."
#define GLSceneURL "http://www.glscene.org/"

[Setup]
; NOTE: The value of AppId uniquely identifies this application.
; Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, click Tools | Generate GUID inside the IDE.)
AppId={{8CF5F54E-C1FC-4716-BC82-908867D36AD6}
AppName={#GLSceneName}
AppVersion={#GLSceneVersion}
;AppVerName={#GLSceneName} {#GLSceneVersion}
AppPublisher={#GLScenePublisher}
AppPublisherURL={#GLSceneURL}
AppSupportURL={#GLSceneURL}
AppUpdatesURL={#GLSceneURL}
DefaultDirName=D:\GLScene\{#GLSceneName}
DefaultGroupName={#GLSceneName}
DisableProgramGroupPage=yes
InfoBeforeFile=D:\Library\GLSceneVCL\Docs\Introduction.txt
InfoAfterFile=D:\Library\GLSceneVCL\Samples\Readme.txt
OutputBaseFilename=SetupGLSceneVCL
SetupIconFile=D:\Library\GLSceneVCL\Samples\media\gls.ico
Compression=lzma
SolidCompression=yes

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"; LicenseFile: "D:\Library\GLSceneVCL\Docs\License.txt"
Name: "french"; MessagesFile: "compiler:Languages\French.isl"; 
Name: "russian"; MessagesFile: "compiler:Languages\Russian.isl"; LicenseFile: "D:\Library\GLSceneVCL\Docs\License_rus.txt"
Name: "german"; MessagesFile: "compiler:Languages\German.isl"
Name: "spanish"; MessagesFile: "compiler:Languages\Spanish.isl"

[Files]
Source: "D:\Library\GLSceneVCL\CleanForRelease.bat"; DestDir: "{app}"; 
Source: "D:\Library\GLSceneVCL\external\*"; DestDir: "{app}\external"; 
Source: "D:\Library\GLSceneVCL\Docs\*"; DestDir: "{app}\Docs"; Flags: recursesubdirs createallsubdirs
Source: "D:\Library\GLSceneVCL\Help\*"; DestDir: "{app}\Help"; Flags: recursesubdirs createallsubdirs
Source: "D:\Library\GLSceneVCL\Locale\*"; DestDir: "{app}\Locale"; Flags: recursesubdirs createallsubdirs
Source: "D:\Library\GLSceneVCL\Packages\*"; DestDir: "{app}\Packages"; Flags: recursesubdirs createallsubdirs
Source: "D:\Library\GLSceneVCL\Resources\*"; DestDir: "{app}\Resources"; Flags: recursesubdirs createallsubdirs
Source: "D:\Library\GLSceneVCL\Samples\*"; DestDir: "{app}\Samples"; Flags: recursesubdirs createallsubdirs
Source: "D:\Library\GLSceneVCL\Source\*"; DestDir: "{app}\Source"; Flags: recursesubdirs createallsubdirs
Source: "D:\Library\GLSceneVCL\Utilities\*"; DestDir: "{app}\Utilities"; Flags: recursesubdirs createallsubdirs
Source: "D:\Library\GLSceneVCL\_Installation\*"; DestDir: "{app}\_Installation"; Flags: ignoreversion recursesubdirs createallsubdirs
; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Registry]
Root: HKLM; Subkey: "Software\GLScene\GLSceneVCL"; Flags: uninsdeletekey
;{reg:HKxx\SubkeyName,ValueName|DefaultValue} 
;{reg:HKLM\Software\GLScene\GLSceneVCL,C:\Users\Public\Documents\Embarcadero\Studio\17.0\Bpl\GLSceneVCL_DesignTime.bpl|GLSceneVCL OpenGL 3D library} 
;Root: HKLM; Subkey: "Software\Company\Program\Settings"; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"
;Root: HKLM; Subkey: "Software\Embarcadero\BDS\17.0\Known Packages";"REG_SZ"; C:\Users\Public\Documents\Embarcadero\Studio\17.0\Bpl\GLSceneVCL_DesignTime.bpl;                      GLSceneVCL OpenGL 3D library

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