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
unit nLifeFiles;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs,
  Math, StdCtrls, FileCtrl, Buttons, ExtCtrls;

type
  TLifeFilesForm = class(TForm)
    DirectoryListBox1: TDirectoryListBox;
    DriveComboBox1: TDriveComboBox;
    FileListBox1: TFileListBox;
    LifeFileEdit: TEdit;
    Memo1: TMemo;
    FileLabel: TLabel;
    SpokesLabel: TLabel;
    LifeCyclesLabel: TLabel;
    Panel1: TPanel;
    Label1: TLabel;
    HelpBtn: TSpeedButton;
    LifeFileBtn: TSpeedButton;
    Do3DBtn: TSpeedButton;
    Do2DBtn: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ReallyClose;

    procedure LoadFileBtnClick(Sender: TObject);
procedure DoFileLoader(FileName: string);
    procedure HelpBtnClick(Sender: TObject);
    procedure Do2DBtnClick(Sender: TObject);
{procedure Do2DCellLoader; }
    procedure Do3DBtnClick(Sender: TObject);
procedure Do3DCellLoader;
    procedure FormShow(Sender: TObject);
  private
   MinX, MinY, MaxX, MaxY,
   PatternWide, PatternHigh:Integer;
  public
     
  end;

var
  LifeFilesForm: TLifeFilesForm;

implementation
uses ncLife, nUGlobal, nlife3d;
{$R *.DFM}

procedure TLifeFilesForm.FormCreate(Sender: TObject);
begin
  top := LifeFilesFormY;
  left := LifeFilesFormX;
  PatternWide := 0;
  PatternHigh := 0;
end;

procedure TLifeFilesForm.ReallyClose;
begin
  Setlength(TempArray, 0, 0);
  Close;
end;

procedure TLifeFilesForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  LifeFilesFormY := LifeFilesForm.top;
  LifeFilesFormX := LifeFilesForm.left;
  if ReallyGone then Action := caFree else Action := caHide;
end;
procedure TLifeFilesForm.FormShow(Sender: TObject);
var driveletter:string;
begin
  driveletter:= extractfiledrive(nLifeDir);
  DriveComboBox1.Drive:=driveletter[1];
  DirectoryListBox1.Directory:=nLifeDir;
end;
procedure TLifeFilesForm.HelpBtnClick(Sender: TObject);
begin
  Application.HelpContext(2001);
end;

procedure TLifeFilesForm.LoadFileBtnClick(Sender: TObject);
var {FileListBox1Drive,}FileListBox1Directory:String;
begin
{  FileListBox1Drive := DriveComboBox1.Drive;}
  FileListBox1Directory := DirectoryListBox1.Directory;
{  showmessage(FileListBox1Directory +'\'+LifeFileEdit.Text);}
  If FileExists(FileListBox1Directory +'\'+LifeFileEdit.Text) then
     DoFileLoader(FileListBox1Directory +'\'+LifeFileEdit.Text);
end;
procedure TLifeFilesForm.DoFileLoader(FileName: string);
var
  LifeText: TextFile;
  {CheckString2,} CheckString, DataString, Instring: string;
      {private  MinX, MinY, MaxX, MaxY,
       PatternWide, PatternHigh:Integer;}
  CurrentX, CurrentY,
  TotalWidth, {MissingWidth,}
  TotalLines, {MissingLines,}
  OneY, OneX, {The X,Y read from file for each block of life cells}
  CheckCode, {checks that val worked}
  Ruler,{flips live or die of /   23/3}
  DataCount, {LineCount,} {in processing counts of data}
  i,j:Integer;
  CellList:Array of TPoint;
begin
  nLifeDir:=extractfilepath(FileName);
  AssignFile(LifeText, FileName);
  Reset(LifeText);
  Readln(LifeText, Instring);
  if (Instring = '#Life 1.05') then
  begin
    {showmessage(Instring);}
    Memo1.Lines.Append(Instring);
    DataString := '';
    Memo1.Clear;
    Memo1.Lines.Clear;
    FileLabel.Caption := FileName;
    DataCount := 0;
    CurrentX:= 0;
    CurrentY:= 0;
    OneX:= 0;
    OneY:= 0;
    Setlength(TempArray, 0, 0);
    Setlength(CellList, 0);
    MaxX :=0;{ 0 - 2147;}
    MaxY :=0; {0 - 2147;}
    MinX :=0; {2147;}
    MinY :=0; {2147;}
    while (not eof(LifeText)) do
    begin
      Readln(LifeText, Instring);
      If Length(Instring)>0 then
      begin {skip blank lines}
        If Length(Instring)=1 then
        begin {cover for single data lines}
          begin
            if Instring[1] = '*' then
            begin
              inc(DataCount);
              Setlength(CellList, DataCount+1);
              CellList[DataCount].X := OneX+CurrentX;
              CellList[DataCount].Y := OneY+CurrentY;
              inc(CurrentX);
            end else inc(CurrentX);
          end;
          inc(CurrentY);
        end else
        if (Instring[2] = 'D') then
        begin
          Instring[1] := ' ';  Instring[2] := ' ';
          Instring := trim(Instring);
          {FileLabel.Caption := Instring;}
          Memo1.Lines.Append(Instring);
        end else
        if ((Instring[2] = 'C')) then
        begin
          Instring[1] := ' '; Instring[2] := ' '; trim(Instring);
          Memo1.Lines.Append(Instring);
        end else
        if (Instring[2] = 'N') then
        begin
          Memo1.Lines.Append(Instring+' Means Conway Rule');
          {Normal.... 23/3 ...}
          alife3dForm.Conway1Click(self);
        end else
        if (Instring[2] = 'R') then
        begin
          Memo1.Lines.Append(Instring);
          {Rulestring.... ...}
          Instring[1] := ' ';  Instring[2] := ' ';
          Instring := trim(Instring);
          for i:= 0 to 26 do Universal[0,i]:=False;
          for i:= 0 to 26 do Universal[1,i]:=False;
          Ruler:=0;
          for i := 1 to Length(Instring) do
          begin
            If (Instring[i] = '/') then Ruler:=1 else
            Universal[Ruler,strtoint(Instring[i])]:=True;
          end;
          alife3dForm.CellularRulesOff;
          alife3dForm.Universes.Checked:=True;
          CellularRulesStyle:=1;
          alife3dForm.StatusBarred.Panels[0].Text :=
          alife3dForm.RunBooleanRules;
        end else
        if (Instring[2] = 'P') then
        begin
          {Get the X,Y offsets and read in the next line,
           so the other lines can be read incremented}
          Instring[1] := ' '; Instring[2] := ' '; trim(Instring);
          {Get the X,Y and place the array
           ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ',', '-'];}
          {Checkstring:=Copy(Instring,0,Pos(' ',Instring));}
          Checkstring := '';
          for i := 1 to Pos(' ', Instring) do
          begin
            if ((Instring[i] in TDigitsSet)) then
            Checkstring := Checkstring + Instring[i];
          end;
          val(Checkstring, OneX, CheckCode);
          Checkstring := Copy(Instring, Pos(' ', Instring),Length(Instring));
          val(Checkstring, OneY, CheckCode);
          CurrentX:= 0;
          CurrentY:= 0;
        end else
        begin    {Read in to the current P Block}
          for I := 1 to Length(Instring) do
          begin
            if Instring[I] = '*' then
            begin
              inc(DataCount);
              Setlength(CellList, DataCount+1);
              CellList[DataCount].X := OneX+CurrentX;
              CellList[DataCount].Y := OneY+CurrentY;
              inc(CurrentX);
            end else inc(CurrentX);
          end;
          inc(CurrentY);
        end;
      end;
    end;
    for I := 1 to DataCount do
    begin
      OneX:=CellList[i].X;
      OneY:=CellList[i].Y;
      if (MinX > OneX) then MinX := OneX;
      if (MinY > OneY) then MinY := OneY;
      if (MaxX < OneX) then MaxX := OneX;
      if (MaxY < OneY) then MaxY := OneY;
    end;
    TotalWidth:=(MaxX-MinX);
    TotalLines:=(MaxY-MinY);
    PatternWide:=TotalWidth;
    PatternHigh:=TotalLines;
    {showmessage(inttostr(TotalWidth));}
    Setlength(TempArray, TotalWidth+1, TotalLines+1);
    for j := 0 to TotalLines do
      for I := 0 to TotalWidth do
        TempArray[I, J]:= 0;
    for I := 1 to DataCount do
      TempArray[CellList[i].X+MinX, CellList[i].Y+MinY] := 1;
    {Display Counts}
    str(TotalWidth, CheckString);
    SpokesLabel.Caption := CheckString;
    Memo1.Lines.Append('Total Width: '+CheckString);
    str(TotalLines, CheckString);
    LifeCyclesLabel.Caption := CheckString;
    Memo1.Lines.Append('Total Lines: '+CheckString);
    str(MaxX, CheckString);
    Memo1.Lines.Append('MaxX: '+CheckString);
    str(MaxY, CheckString);
    Memo1.Lines.Append('MaxY: '+CheckString);
    str(MinX, CheckString);
    Memo1.Lines.Append('MinX: '+CheckString);
    str(MinY, CheckString);
    Memo1.Lines.Append('MinY: '+CheckString);
    {Do2DBtn.Enabled:=True;}
    Do3DBtn.Enabled:=True;
  end else
  begin {Bad file }
    FileLabel.Caption := 'FileName';
    Memo1.Lines.Clear;
    Memo1.Lines.Append(Instring);
    Setlength(TempArray, 0, 0);
    Setlength(CellList, 0);
  end;
  CloseFile(LifeText);
    {Setlength(TempArray, 0, 0);}
  Setlength(CellList, 0);
end; {Procedure}



procedure TLifeFilesForm.Do3DBtnClick(Sender: TObject);
begin
  alife3dForm.ShowDown(FileLabel.Caption);
end;
procedure TLifeFilesForm.Do3DCellLoader;
var
  OffsetX, OffsetY,
  LifeSizeHalf, Sizer, I, J {, II,JJ}: Integer;
begin
  {Place the already loaded Life file into the Array}
  Sizer:=max(PatternHigh, PatternWide);
  If Sizer > 511 then {Error}exit;
  Case Sizer of
  1..9:    If LifeSize < 9 then alife3dForm.lifesize10Click(self){LifeSize:=9} ;
  10..24:  If LifeSize < 24 then alife3dForm.lifesize25Click(self){LifeSize:=24};
  25..49:  If LifeSize < 49 then alife3dForm.lifesize50Click(self){LifeSize:=49};
  50..99:  If LifeSize < 99 then alife3dForm.lifesize100Click(self){LifeSize:=99};
  100..149:If LifeSize < 149 then alife3dForm.lifesize150Click(self){LifeSize:=149};
  150..199:If LifeSize < 199 then alife3dForm.lifesize200Click(self){LifeSize:=199};
  200..254:If LifeSize < 254 then alife3dForm.lifesize256Click(self){LifeSize:=254};
  255..299:If LifeSize < 299 then alife3dForm.lifesize300Click(self){LifeSize:=511};
  300..399:If LifeSize < 399 then alife3dForm.lifesize400Click(self){LifeSize:=511};
  400..511:If LifeSize < 511 then alife3dForm.lifesize512Click(self){LifeSize:=511};
  end;
{    JJ := 1; II := 1;}
    OffsetX:=  (LifeSize-PatternWide)div 2;
    OffsetY:= (LifeSize-PatternHigh)div 2;
    LifeSizeHalf:=LifeSize div 2;
    for J := 0 to PatternHigh-1 do begin
      for I := 0 to PatternWide-1 do begin
        BeforeaLifeMatrix[I+OffsetX, J+OffsetY, LifeSizeHalf]:=
          TempArray[i, j];
        {inc(II);}
      end; {inc(JJ);}
    end;
end;

procedure TLifeFilesForm.Do2DBtnClick(Sender: TObject);
begin  {Lif Life 2D  2010}
{  Do2DCellLoader;}

{Use 3D code to paint a GR32 bitmap ... BUT ONLY the area Visible
Speed optimization... ??? the hard part ?}
{Record to store data  into a 2*XxY array  ... X and Y EXPAND as needed
Thus SWITCH which is Active instead of Replacing ALL data every cycle
Count:Byte or Integer
Geez it is too complicated for ME
Display the Max X-Y and Displayed X-Y
Use the GR32 Zoom and Scroll and IGNORE it
Access like a HTF in DTM ?}
end;
{(LoadFileName:String;InWidth:Integer)}
(*
procedure TLifeFilesForm.Do2DCellLoader;
var I, II, J, JJ: Integer;
begin
{  ChangeRow := ARow; ChangeCol := ACol;}
{Place the already loaded Life file into the Array}
  if bEditor39 then
  begin
    JJ := 1; II := 1;
    for J := ChangeRow to PatternHigh do begin
      for I := ChangeCol to PatternWide do begin
        TV[i{ChangeCol}, j{ChangeRow}] := TempArray[II, JJ];
        inc(II);
      end; inc(JJ);
    end;
  end else if bEditor78 then
  begin
  {TV7848[ChangeCol,ChangeRow]}
    JJ := 1; II := 1;
    for J := ChangeRow to PatternHigh do begin
      for I := ChangeCol to PatternWide do begin
        TV7848[i{ChangeCol}, j{ChangeRow}] := TempArray[II, JJ];
        inc(II);
      end; inc(JJ);
    end;
  end;
end;
*)





end.
