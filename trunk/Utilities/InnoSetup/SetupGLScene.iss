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
InfoBeforeFile=D:\Library\GLSceneVCL\Help\en\Introduction.txt
InfoAfterFile=D:\Library\GLSceneVCL\Samples\Readme.txt
OutputBaseFilename=SetupGLScene
SetupIconFile=D:\Library\GLSceneVCL\Samples\media\gls.ico
Compression=lzma
SolidCompression=yes

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"; LicenseFile: "D:\Library\GLSceneVCL\Help\en\License.txt"
Name: "french"; MessagesFile: "compiler:Languages\French.isl"; 
Name: "russian"; MessagesFile: "compiler:Languages\Russian.isl"; LicenseFile: "D:\Library\GLSceneVCL\Help\ru\License.txt"
Name: "german"; MessagesFile: "compiler:Languages\German.isl"
Name: "spanish"; MessagesFile: "compiler:Languages\Spanish.isl"

[Files]
Source: "D:\Library\GLSceneVCL\CleanForRelease.bat"; DestDir: "{app}"; Flags: ignoreversion
Source: "D:\Library\GLSceneVCL\external\*"; DestDir: "{app}\external"; Flags: ignoreversion
Source: "D:\Library\GLSceneVCL\Help\*"; DestDir: "{app}\Help"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "D:\Library\GLSceneVCL\Packages\*"; DestDir: "{app}\Packages"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "D:\Library\GLSceneVCL\Resources\*"; DestDir: "{app}\Resources"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "D:\Library\GLSceneVCL\Samples\*"; DestDir: "{app}\Samples"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "D:\Library\GLSceneVCL\Source\*"; DestDir: "{app}\Source"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "D:\Library\GLSceneVCL\Utilities\*"; DestDir: "{app}\Utilities"; Flags: ignoreversion recursesubdirs createallsubdirs
; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Registry]
Root: HKLM; Subkey: "Software\GLScene\GLSceneVCL"; Flags: uninsdeletekey
;{reg:HKxx\SubkeyName,ValueName|DefaultValue} 

;Root: HKLM; Subkey: "Software\Company\Program\Settings"; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"
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
