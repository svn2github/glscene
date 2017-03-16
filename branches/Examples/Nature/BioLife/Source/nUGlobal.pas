{*******************************************************}
{                                                       }
{                      Tachyon Unit                     }
{    Vector Raster Geographic Information Synthesis     }
{                     VOICE  ..  Tracer                 }
{                     GRIP ICE .. Tongs                 }
{                Digital Terrain Mapping                }
{               Image Locatable Holographics            }
{                          SOS MAP                      }
{  Surreal Object Synthesis Multimedia Analysis Product }
{                   Fractal3D  Life MOW                 }
{       Copyright (c) 1995,2006  Ivan Lee Herring       }
{                                                       }
{*******************************************************}
unit nUGlobal;

interface

uses Dialogs, Messages, Graphics,
  ShellAPI,
  Windows, SysUtils, Classes, Controls, Forms,
  Grids, StdCtrls, ColorGrd, Mask, Buttons,
  Menus, ComCtrls, ExtCtrls;

const
  dPI: Double = 3.14159265358979323846; // Pi
  d2PI: Double = 6.28318530717958623200; // 2 * Pi
  dPiDiv2: Double = 1.57079632679489655800; // Pi / 2
  dPiDiv4: Double = 0.78539816339744827900; // Pi / 4
  dInvPi: Double = 0.31830988618379069122; // 1 / Pi
  dDegToRad: Double = 0.01745329251994329547; // Degrees to Radians
  dRadToDeg: Double = 57.29577951308232286465; // Radians to Degrees
  dHuge: Double = 1.0E+38; {1.0e+38;} // Huge number for
  dEpsilon: Double = 1.0E-5; {1.0e-5;} // Tolerance for s
{  Double	5.0 x 10^–324 .. 1.7 x 10^308	15–16	8
  Extended	3.6 x 10^–4951 .. 1.1 x 10^4932	19–20	10}

type
  PrefRecord = record
    PLifeDir: string[255];
    PProLoaded: string[255];
    PDesolate,
      PGrowing,
      PFull,
      POverpopulated: TColor;
  {This is my secret string ? as text?}
    PHiddenString: string[255]; {array [1..256] of Char;}
    PColorreg: Integer;
    PStarted: TDateTime;
    PStartedNameNumber: string[25];
    PNumberEditStored,
    PMaxRandomCellsEditStored,
  {X Y Location of forms}
  Plife3dFormX,Plife3dFormY,
  PaUniversesFormX,PaUniversesFormY,
    PLifeMainFormX,
      PAboutFormX,
      PLifeFormX,
      PSystemInfoFormX,
      PSystemInfoFormY,
      PEditorFormX,
      PEPFormX,
      PColorsFormX,
      PNewRulesFormX,
      PFamilyFormX,
      PLifeFilesFormX,
      PGRFFormX,
      PGSRShowFormX,
      PTermiteFormX,
      PTermiteMoundFormX,
      PAntsFarmX,
      PAntMoundX,
      PDXTorsoFormX,
      PBirdsFormX,  {TAAABirdForm}
      PBirdBrainX,
      PBirdDXFormX,
      PFrenzyFormY, {TFrenzyForm}
      PfrmBoidsY,   {TAAADemoForm}
      PMessageX,
      PMessageY,
      PfrmBoidsX,
      PFrenzyFormX,
      PBirdDXFormY,
      PGSRShowFormY,
      PLifeFilesFormY,
      PFamilyFormY,
      PBirdBrainY,
      PAntMoundY,
      PTermiteMoundFormY,
      PLifeMainFormY,
      PLifeFormY,
      PEPFormY,
      PColorsFormY,
      PEditorFormY,
      PAboutFormY,
      PGRFFormY,
      PTermiteFormY,
      PAntsFarmY,
      PBirdsFormY,
      PDXTorsoFormY,
      PNewRulesFormY
      : Integer;
    {PDisplayButtonsOn, PDisplayBoth: Boolean;}
  end;
  PrefFile = file of PrefRecord;



var
  nLifeDir,
  BirdLifeDir,
    ProLoaded, HiddenString: string;
  Desolate, Growing, Full, Overpopulated: TColor;
  PreRcd: PrefRecord;
  Colorreg: Integer;
  Started: TDateTime;
  StartedNameNumber: string;
    NumberEditStored,
    MaxRandomCellsEditStored,
  life3dFormX,life3dFormY,
  aUniversesFormX,aUniversesFormY,
  LifeMainFormX,
    LifeFormX,
    EPFormX,
    ColorsFormX,
    EditorFormX,
    AboutFormX,
    GRFFormX,
    TermiteFormX,
    AntsFarmX,
    BirdsFormX,
    DXTorsoFormX,
    NewRulesFormX,
    TermiteMoundFormX,
    AntMoundX,
    BirdBrainX,
    FamilyFormX,
    LifeFilesFormX,
    GSRShowFormX,
    BirdDXFormX,
    FrenzyFormY,
    frmBoidsY,
    frmBoidsX,
    FrenzyFormX,
    SystemInfoFormX,
    MessageX,
    MessageY,
    SystemInfoFormY,
    BirdDXFormY,
    GSRShowFormY,
    LifeFilesFormY,
    FamilyFormY,
    BirdBrainY,
    AntMoundY,
    TermiteMoundFormY,
    LifeMainFormY,
    LifeFormY,
    EPFormY,
    ColorsFormY,
    EditorFormY,
    AboutFormY,
    GRFFormY,
    TermiteFormY,
    AntsFarmY,
    BirdsFormY,
    DXTorsoFormY,
    NewRulesFormY    : Integer;
    LifeSize, CellularRulesStyle : Integer;
bSaveThatThing,    
    FrenzyFilesLoaded,
{    DisplayButtonsOn, DisplayBoth,}
    HasTrueColors,
    BirdLandDX,
    NotYetShown,
    ReallyGone: Boolean;
    Universal:Array of Array of Boolean;{Life 3D}
{var
Bitmap: TBitmap;}
function ExecuteFile(const FileName, Params, DefaultDir: string;
  ShowCmd: Integer): THandle;
procedure Codefx(WhatString: string; Codein: Integer);
function myLoadImage: Boolean;

procedure DoLoader;
procedure SetPreferences;
procedure DoSaver;
procedure GetPreferences;

procedure DoMessages(i: Integer);

implementation

//uses nAbout;

function ExecuteFile(const FileName, Params, DefaultDir: string;
  ShowCmd: Integer): THandle;
var
  zFileName, zParams, zDir: array[0..79] of Char;
begin
  Result := ShellExecute(Application.MainForm.Handle, nil,
    StrPCopy(zFileName, FileName), StrPCopy(zParams, Params),
    StrPCopy(zDir, DefaultDir), ShowCmd);
end;

procedure Codefx(WhatString: string; Codein: Integer);
var CodeS: string;
begin
  str(Codein, CodeS);
  ShowMessage('Error in data Number: ' + #13#10 +
    CodeS + #13#10 +
    WhatString);
end;

function myLoadImage: Boolean;
var
{temp:DWord;}
  dc: HDC;
begin
  dc := GetDc(0);
  if ((GetDeviceCaps(dc, BITSPIXEL) = 24)
    or (GetDeviceCaps(dc, BITSPIXEL) = 32)) then begin
  {dc := } ReleaseDc(0, dc); {Give back the screen dc}
    myLoadImage := True;
  end else
  begin
{    MessageDlg('Requires True Color (24 or 32 bit) Colors Setting',
                 mtInformation,[mbOk], 0); }
{    dc := } ReleaseDc(0, dc);
    myLoadImage := False;
  end; {of if 24 bit}
end; {of this}




procedure DoLoader;
var P_File: PrefFile;
var PathS: string;
begin {}
  PathS := ExtractFilePath(ParamStr(0)) + 'nlife.pof';
  if FileExists(PathS) then
  begin
    AssignFile(P_File, PathS);
    {$I-}Reset(P_File); {$I+}
    if IoResult <> 0 then DoMessages(39984);
    Read(P_File, PreRcd);
    CloseFile(P_File);
    SetPreferences;
  end else {set defaults}DoMessages(39985);
end;

procedure SetPreferences;
begin {after loading}
  with PreRcd do begin
    nLifeDir := PLifeDir;
    ProLoaded := PProLoaded;
    Desolate := PDesolate;
    Growing := PGrowing;
    Full := PFull;
    Overpopulated := POverpopulated;
    Started := PStarted;
    StartedNameNumber := PStartedNameNumber;
    HiddenString := PHiddenString;
    Colorreg := PColorreg;
    NumberEditStored:=PNumberEditStored;
    MaxRandomCellsEditStored:=PMaxRandomCellsEditStored;
    life3dFormX:=Plife3dFormX;
    life3dFormY:=Plife3dFormY;
    aUniversesFormX:=PaUniversesFormX;
    aUniversesFormY:=PaUniversesFormY;
    LifeMainFormX := PLifeMainFormX;
    LifeFormX := PLifeFormX;
    EPFormX := PEPFormX;
    ColorsFormX := PColorsFormX;
    EditorFormX := PEditorFormX;
    AboutFormX := PAboutFormX;
    GRFFormX := PGRFFormX;
    TermiteFormX := pTermiteFormX;
    AntsFarmX := pAntsFarmX;
    BirdsFormX := pBirdsFormX;
    DXTorsoFormX := pDXTorsoFormX;
    NewRulesFormX := pNewRulesFormX;
    TermiteMoundFormX := PTermiteMoundFormX;
    AntMoundX := PAntMoundX;
    BirdBrainX := PBirdBrainX;
    FamilyFormX := PFamilyFormX;
    LifeFilesFormX := PLifeFilesFormX;
    GSRShowFormX := PGSRShowFormX;
    BirdDXFormX := PBirdDXFormX;
    SystemInfoFormX := PSystemInfoFormX;
    SystemInfoFormY := PSystemInfoFormY;
    FrenzyFormY := PFrenzyFormY;
    frmBoidsY := PfrmBoidsY;
    frmBoidsX := PfrmBoidsX;
    FrenzyFormX := PFrenzyFormX;
    MessageX := PMessageX;
    MessageY := PMessageY;
    BirdDXFormY := PBirdDXFormY;
    GSRShowFormY := PGSRShowFormY;
    LifeFilesFormY := PLifeFilesFormY;
    FamilyFormY := PFamilyFormY;
    BirdBrainY := PBirdBrainY;
    AntMoundY := PAntMoundY;
    TermiteMoundFormY := PTermiteMoundFormY;
    LifeMainFormY := PLifeMainFormY;
    LifeFormY := pLifeFormY;
    EPFormY := pEPFormY;
    ColorsFormY := pColorsFormY;
    EditorFormY := pEditorFormY;
    AboutFormY := pAboutFormY;
    GRFFormY := pGRFFormY;
    TermiteFormY := pTermiteFormY;
    AntsFarmY := pAntsFarmY;
    BirdsFormY := pBirdsFormY;
    DXTorsoFormY := pDXTorsoFormY;
    NewRulesFormY := pNewRulesFormY;
    {DisplayButtonsOn := pDisplayButtonsOn;
    DisplayBoth := pDisplayBoth;}
  end;
end;
{---------------------------------------------------------------------}

procedure DoSaver;
var P_File: PrefFile;
var PathS: string;
begin
  PathS := ExtractFilePath(ParamStr(0)) + 'nlife.pof';
  if (not FileExists(PathS)) then  DoMessages(39985);
  GetPreferences;
  AssignFile(P_File, PathS);
  {$I-}Rewrite(P_File);{$I+}
  if IoResult <> 0 then
  begin
    DoMessages(39986);
  end;
  write(P_File, PreRcd);
  CloseFile(P_File);
end;
{---------------------------------------------------------------------}

procedure GetPreferences;
begin {before saving}
  with PreRcd do begin
    PLifeDir := nLifeDir;
    PProLoaded := ProLoaded;
    PDesolate := Desolate;
    PGrowing := Growing;
    PFull := Full;
    POverpopulated := Overpopulated;
{This is my secret string ? as text?}
    PHiddenString := 'Life Copyright, 2003: Ivan Lee Herring';
    PColorreg := Colorreg;
    PStarted := Started;
    PStartedNameNumber := StartedNameNumber;
    PNumberEditStored:=NumberEditStored;
    PMaxRandomCellsEditStored:=MaxRandomCellsEditStored;
    Plife3dFormX:=life3dFormX;
    Plife3dFormY:=life3dFormY;
    PaUniversesFormX:=aUniversesFormX;
    PaUniversesFormY:=aUniversesFormY;
    PLifeMainFormX := LifeMainFormX;
    PLifeFormX := LifeFormX;
    PEPFormX := EPFormX;
    PColorsFormX := ColorsFormX;
    PEditorFormX := EditorFormX;
    PAboutFormX := AboutFormX;
    PGRFFormX := GRFFormX;
    PTermiteFormX := TermiteFormX;
    PAntsFarmX := AntsFarmX;
    PBirdsFormX := BirdsFormX;
    PDXTorsoFormX := DXTorsoFormX;
    PNewRulesFormX := NewRulesFormX;
    PTermiteMoundFormX := TermiteMoundFormX;
    PAntMoundX := AntMoundX;
    PBirdBrainX := BirdBrainX;
    PFamilyFormX := FamilyFormX;
    PLifeFilesFormX := LifeFilesFormX;
    PGSRShowFormX := GSRShowFormX;
    PBirdDXFormX := BirdDXFormX;
    PSystemInfoFormX := SystemInfoFormX;
    PMessageX := MessageX;
    PMessageY := MessageY;
    PSystemInfoFormY := SystemInfoFormY;
    PfrmBoidsY := frmBoidsY;
    PfrmBoidsX := frmBoidsX;
    PFrenzyFormY := FrenzyFormY;
    PFrenzyFormX := FrenzyFormX;
    PBirdDXFormY := BirdDXFormY;
    PGSRShowFormY := GSRShowFormY;
    PLifeFilesFormY := LifeFilesFormY;
    PFamilyFormY := FamilyFormY;
    PBirdBrainY := BirdBrainY;
    PAntMoundY := AntMoundY;
    PTermiteMoundFormY := TermiteMoundFormY;
    PLifeMainFormY := LifeMainFormY;
    PLifeFormY := LifeFormY;
    PEPFormY := EPFormY;
    PColorsFormY := ColorsFormY;
    PEditorFormY := EditorFormY;
    PAboutFormY := AboutFormY;
    PGRFFormY := GRFFormY;
    PTermiteFormY := TermiteFormY;
    PAntsFarmY := AntsFarmY;
    PBirdsFormY := BirdsFormY;
    PDXTorsoFormY := DXTorsoFormY;
    PNewRulesFormY := NewRulesFormY;
    {PDisplayButtonsOn := DisplayButtonsOn;
    PDisplayBoth := DisplayBoth;}
  end;
end;
{---------------------------------------------------------------------}

(*
Do_Messages(
{1..9999 mtInformation	A message box containing a blue "i".}
{10000..19999 mtConfirmation	A message box containing a green question mark.}
{20000..29999 mtWarning   A message box containing a yellow exclamation point symbol.}
{30000..40000 mtError  A message box containing a red stop sign.}

Codefx(WhatString:String;Codein:Integer);
Do_Messages(39979);
{12..9998 mtInformation	A message box containing a blue "i".}
12:Messagestr:= 'Image not loaded';
{10001..19999 mtConfirmation	A message box containing a green question mark.}
{20001..29999 mtWarning   A message box containing a yellow exclamation point symbol.}
{30001..39989 mtError  A message box containing a red stop sign.}
39989:Messagestr:='Image less than 1024x1024';
*)

procedure DoMessages(i: Integer);
var
  Messagestr: string;
  HelpContextl: Longint;
{  s: array[0..250] of char;  }
begin
  HelpContextl := 0;
  case i of {Do_Messages(11}
{1..9999 mtInformation	A message box containing a blue "i".}
    1: Messagestr := 'Resistance is futile';
    2: Messagestr := 'the Life program options file' + #13#10 +
      ExtractFilePath(ParamStr(0)) + 'Life.pof' + #13#10 +
        'Does NOT Exist:... Will Make it' + #13#10 +
        'Please set Options and Directories in' + #13#10 +
        'Investigate Menu: Options... ';
    9: Messagestr := 'Under Construction: nothing happened';
    10: Messagestr := 'Image not loaded';
    11: Messagestr := 'Requires True Color (24 bit) Colors Setting';
    12: Messagestr := 'Requires TRUE COLOR (24 bit) Image';
    13: Messagestr := 'Improper Choices: nothing happened';
    14: Messagestr := 'I found one';
    15: Messagestr := 'Number input wrong,' + #13 + #10 +
      'Please try again';
    16: Messagestr := 'Registration Number invalid';
    17: Messagestr := 'Not Registered!' + #13 + #10 +
      'Please register Life';
    18: Messagestr := 'Gotta have a Distance Value';
    19: Messagestr := 'Invalid Distance Values';
    20: Messagestr := 'Cannot Change unless Loaded';
    21: Messagestr := 'Only Rotate has more than 2 Stages';
    22: Messagestr := 'Only Sobel and Rotate have more than 1 Stage';
    23: Messagestr := 'Cannot Change unless Clear';
    24: Messagestr := 'Top Center Square value wrong';
    25: Messagestr := 'First Load an image to display';
    26: Messagestr := 'no font';
    27: Messagestr := 'The DIS name MUST be Different from DIH' +
      #13#10 +
        'else many .B?? will have the same name';
    28: Messagestr := '3. Gotta have a PNG File name';
    29: Messagestr := 'Band 1 File does not Exist!';
    30: Messagestr := '1. Header record needed';
    31: Messagestr := '2. Gotta have a Band 1 File name';
    32: Messagestr := '2. Gotta have a Band 2 File name';
    33: Messagestr := '2. Gotta have a Band 3 File name';
    34: Messagestr := 'Band 2 File does not Exist!';
    35: Messagestr := 'Band 3 File does not Exist!';
    36: Messagestr := 'Some size is wrong';
    37: Messagestr := 'Already Printing';
    38: Messagestr := 'Printing aborted';
    39: Messagestr := 'RGB Boolean set to Somethingthing!';
    40: Messagestr := 'Converts 32 to 24 bit Color images';
    41: Messagestr := '4bit to 24 bit bitmapped';
    42: Messagestr := '8bit to 24 bit bitmapped';
    43: Messagestr := '15bit to 24 bit bitmapped';
    44: Messagestr := '16bit to 24 bit bitmapped';
    45: Messagestr := '32bit to 24 bit bitmapped';
    46: Messagestr := 'There is no image currently loaded to save.';
{  Do_Messages(40 }

    9999: Messagestr := 'Resistance is futile'
      + #13#10 + 'All Messages end here';
{10000..19999 mtConfirmation	A message box containing a green question mark.}
    10000: Messagestr := 'Do you wanna dance?';
    10001: Messagestr :=
      'a DIH Header exists, use+append+overwrite instead?';
{20000..29999 mtWarning   A message box containing a yellow exclamation point symbol.}
    20000: Messagestr := 'Danger Danger';
    20001: Messagestr := 'Order.txt Files not found';
    20002: Messagestr := 'License.txt Files not found';
    20003: Messagestr := 'Readme.txt Files not found';
    20004: Messagestr := 'Life will fail without Registration' +
      #13#10 +
        'Registration Warning' + #13#10 +
        'See Life Register';
{Do_Messages( 20004}
{30000..40000 mtError  A message box containing a red stop sign.}
    30000: Messagestr := 'There is an Error in the system';
    30001: Messagestr := 'Life failed without Registration' + #13#10
      +
        'Registration Error' + #13#10 +
        'See Life Register';

{Do_Messages( 39977  39998}
    39967: Messagestr :=
      'Header Crashed: Cancel or fix DIS Subset Width';
    39968: Messagestr := 'File unknown';
    39969: Messagestr := 'Not a Radar DAT file';
    39970: Messagestr :=
      'Header Crashed: Cancel or fix Number of Bands';
    39971: Messagestr := 'Header Crashed: Cancel or fix Coordinates';
    39972: Messagestr := 'NO HDR FILE NAME!?! ... Bad File Error';
    39973: Messagestr := 'Error - no files in the Zip file';
    39974: Messagestr := 'Disk full!?! ... Bad File Error';
    39975: Messagestr := 'Image Undone or crashed' +
      #13#10 + 'Restart Line Profile or exit';
    39976: Messagestr := ' An error was detected when'
      + #13 + 'trying to fetch registration info.';
    39977: Messagestr := 'You Crashed ! OUT of Resources!';
    39978: Messagestr := 'Image less than 1024x1024';
    39979: Messagestr := 'Image less than 512x512';
    39980: Messagestr := 'Image less than 256x256';
    39981: Messagestr := 'Stats Error.. Image buggered';
    39982: Messagestr := 'Filter File unknown';
    39983: Messagestr := 'Filename has reached 999';
    39984: Messagestr := 'Failed reading Life.pof File';
    39985: Messagestr := 'Life.pof Does NOT Exist';
    39986: Messagestr := 'Failed writing Life.pof File';
    39987: Messagestr := 'Stat error';
    39988: Messagestr := 'Height not equal to BR';
    39989: Messagestr := 'Width not equal to RC';
    39990: Messagestr :=
      'Horizontal height: 1and2, 3and4 must be same height'
        + #13#10 + 'Vertical width: 1and3, 2and4 must be same width';
    39991: Messagestr := 'Horizontal height not same = equal';
    39992: Messagestr := 'Vertical Width not same = equal';
    39993: Messagestr :=
      'Variables blank, files invalid, or page turned';
    39994: Messagestr := 'Not a DIH file';
    39995: Messagestr := 'Not a DISP file';
    39996: Messagestr := 'Not a DIP file';
    39997: Messagestr := 'NO HDR FILE NAME!?!';
    39998: Messagestr := 'Error opening file';
    39999: Messagestr := 'There is an Error in the system';
    40000: Messagestr := 'FATAL Error creating bitmap... byebye';
  end;

  case i of {–2,147,483,648..2,147,483,647}
{mtInformation	A message box containing a blue "i".}
    1..9999: MessageDlgPos(Messagestr,
        mtInformation,
        [mbOK], {mbYesNoCancel,}
        HelpContextl,
        MessageX,
        MessageY);
{mtConfirmation	A message box containing a green question mark.}
    10000..19999: MessageDlgPos(Messagestr,
        mtConfirmation,
                {[mbOK],} mbYesNoCancel,
        HelpContextl,
        MessageX,
        MessageY);
{mtWarning   A message box containing a yellow exclamation point symbol.}
    20000..29999: MessageDlgPos(Messagestr,
        mtWarning,
        [mbOK], {mbYesNoCancel,}
        HelpContextl,
        MessageX,
        MessageY);
{mtError  A message box containing a red stop sign.}
    30000..40000: begin
(*
        if ErrorBeepOn then Beep(10000, 1000); {Beep;}
        if (ErrorVoiceOn) then
        begin
          StrPCopy(s, ExtractFilePath(ParamStr(0)) + 'ERRORV.WAV');
                       {sndPlaySoundA(s, 0);}
          if (MMSysHandle <> 0) then PlaySound(s, {SND_ASYNC} 0);
        end;
*)
        MessageDlgPos(Messagestr,
          mtError,
          [mbOK], {mbYesNoCancel,}
          HelpContextl,
          MessageX,
          MessageY);
      end;
  end;
end;
(*TMsgDlgBtn = (mbYes, mbNo, mbOK, mbCancel,
             mbAbort, mbRetry, mbIgnore,
             mbAll, mnNoToAll, mbYesToAll, mbHelp);
const
  mbYesNoCancel = [mbYes, mbNo, mbCancel];
  mbOKCancel = [mbOK, mbCancel];
  mbAbortRetryIgnore = [mbAbort, mbRetry, mbIgnore];

mbYes	A button with 'Yes' on its face.
mbNo	A button the text 'No' on its face.
mbOK	A button the text 'OK' on its face.
mbCancel	A button with the text 'Cancel' on its face.
mbAbort	A button with the text 'Abort' on its face
mbRetry	A button with the text 'Retry' on its face
mbIgnore	A button the text 'Ignore' on its face
mbAll	A button with the text 'All' on its face
mbNoToAll	A button with the text 'No to All' on its face
mbYesToAll	A button with the text 'Yes to All' on its face
mbHelp	A button with the text 'Help' on its face

RETURN VALUES
mrNone	 mrAbort	mrYes
mrOk	 mrRetry	mrNo
mrCancel mrIgnore	mrAll
*)
{---------------------------------------------------------------------}
(*      Begin
StrPCopy(s, 'DONE.WAV');
{Play the sound asynchronously}
sndPlaySound(s, 0); {see mmsystem.hlp for other values}
{
If you specify the SND_MEMORY flag, lpszSoundName must point to an
in-memory image of a waveform sound.
If the sound is stored as a resource, use
LoadResource and LockResource to load and lock the resource and get a
pointer to it.
}
       {setimage calls the ImageDone}
       {Whistle is a for i:= 100 do
       go up then down then around }
               { Beep( Pitch, Duration );}
{      Beep(1000,100);}{Order of units must make difference?}
      end;*)


end.
