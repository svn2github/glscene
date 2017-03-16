{
@abstract(provides a component for exporting data to clipboard and text files)
@author(Aaron Hochwimer <aaron@graphic-edge.co.nz>)
@created(June 30, 2003)
@lastmod(June 30, 2003)
This unit provides the TGEExportFile component component. This is a general
purpose export module to export data to comma, tab, or space separated data
files, clipboard also xml and LaTeX
}
unit geExportFile;

// $Id: geExportFile.pas,v 1.3 2003/07/01 01:42:02 hochwimmera Exp $

interface

uses
  classes,controls,db,dialogs,forms,menus,registry,stdctrls,sysutils,windows,
  shellapi,clipbrd,frmExport;

const
  sACCEPTDATEBOUNDS = 'acceptdatebounds';
  sEXPORT = 'Export';
  sFIELDNAMES = 'fnames';
  sHIDDEN = 'hidden';
  sHIGHTIME = 'hightime';
  sLATEXLONGTABLE = 'longtable';
  sLATEXHLINES = 'hlines';
  sLATEXTABULAR = 'tabular';
  sLATEXVERTRULE = 'vrule';
  sLOWTIME = 'lowtime';
  sOPEN = 'open';
  sOVERRIDE = 'override';
{clipboard}
  sCLIPBOARD = 'clipboard';
{comma separated value file}
  sCSV = 'comma separated value ';
{space separated value file}
  sSSV = 'space separated value';
{tab separated value file}
  sTSV = 'tab separated value';
{xml}
  sXML = 'xml';
{latex}
  sLATEX = 'LaTeX';

type
{Enumeration type providing a list of export file types. Current types include:
efCSV,efTSV,efSSV,efClipboard,efXML,efLatex}
  TExportFileType = (efCSV,efTSV,efClipboard,efSSV,efXML,efLatex);

  TGEExportFile = class(TComponent)
  private
    fAcceptDateBounds:boolean;
    fDateTimeFilterField:string;
    fDelimitEverything:boolean;
    fDefaultFileType:TExportFileType;
    fDelimiter:string;
    fExporter : TformExporter;
    fExportFileType: TExportFileType;
    fFileName: string;
    fFilePath: string;
    fHighTime : boolean;
    fHighDateTime : TDateTime;
    fIdentifier : string;
    fLatexEnvironment : string;
    fLatexHline:boolean;
    fLatexVRule:boolean;
    fLatexColType:string;
    fLowDateTime : TDateTime;
    fLowTime : boolean;
    fOnTypeChange: TNotifyEvent;
{runs shellapi command to open}
    fOpenFileAfterExport : boolean;
{outputs the fieldnames in the header}
    fOutputFieldNames : boolean;
    fPrecision:integer;
    fReg: TRegIniFile;
    fRegPath: string;
    fSubRegPath:string;
    fSaveDialog: TSaveDialog;
{allow access to hidden fields}
    fShowHiddenFields: boolean;
    fSource: TDataSet;
    fTextFile:Textfile;
    function AppendDelimiter(sDelimiter,sItem:string):string;
    function GetHint:string;
    procedure LoadExportFileType;
    procedure SaveNodalSettings;
    procedure SetDelimiter(sDelimiter:string);
    procedure SetExportFileType(aFileType:TExportFileType);
    procedure SetFilePath(aFilePath:string);
    procedure SetExportFile(exporter:TformExporter);
    procedure SetIdentifier(sIdentifier:string);
  protected
    procedure DoOnTypeChange;virtual;
    procedure Loaded;override;
    procedure LoadFilePath;
    procedure LoadRegistrySettings(fsl:TStringList);
    procedure SaveRegistrySettings(fsl:TStringList);
    procedure WriteFileHeaderInfo(sl:TStringList;sSep:string);
    procedure WriteLaTexHeader(sl:TStringList);
  public
    constructor Create(aOwner:TComponent);override;
    destructor Destroy;override;
    procedure Execute;
    procedure SaveTableToClipboard(sl:TStringList);
    procedure SaveTableToFile(sl:TStringList);
    property AcceptDateBounds : boolean read fAcceptDateBounds write fAcceptDateBounds;
    property DateTimeFilterField : string read fDateTimeFilterField write fDateTimeFilterField;
    property DelimitEverything : boolean read fDelimitEverything write fDelimitEverything;
    property Delimiter : string read fDelimiter write SetDelimiter;
    property Exporter : TformExporter read fExporter write fExporter;
    property ExportTextFile : TextFile read fTextFile write fTextFile;
    property FileName : string read fFileName write fFileName;
    property FilePath : string read fFilePath write SetFilePath;
    property Hint : string read GetHint;
    property HighDateTime : TDateTime read fHighDateTime write fHighDateTime;
    property HighTime : boolean read fHighTime write fHighTime;
    property Identifier : string read fIdentifier write SetIdentifier;
    property LatexColType : string read fLatexColType write fLatexColType;
    property LatexEnvironment : string read fLatexEnvironment write fLatexEnvironment;
    property LatexHline : boolean read fLatexHLine write fLatexHline;
    property LatexVRule : boolean read fLatexVRule write fLatexVRule;
    property LowDateTime : TDateTime read fLowDateTime write fLowDateTime;
    property LowTime : boolean read fLowTime write fLowTime;
    property OpenFileAfterExport : boolean read fOpenFileAfterExport write fOpenFileAfterExport;
    property OutputFieldNames : boolean read fOutputFieldNames write fOutputFieldNames;
    property Reg: TRegIniFile read fReg write fReg;
    property SaveDialog: TSaveDialog read fSaveDialog write fSaveDialog;
    property ShowHiddenFields: boolean read fShowHiddenFields write fShowHiddenFields;
  published
    property OnTypeChange: TNotifyEvent read fOnTypeChange write fOnTypeChange;
    property DefaultFileType: TExportFileType read fDefaultFileType write fDefaultFileType;
    property ExportFileType: TExportFileType read fExportFileType write SetExportFileType;
    property RegPath: string read fRegPath write fRegPath;
    property Precision : integer read fPrecision write fPrecision;
    property Source: TDataSet read fSource write fSource;
    property SubRegPath:string read fSubRegPath write fSubRegpath;
  end;

  procedure Register;


implementation
//------ Register --------------------------------------------------------------
procedure Register;

begin
  RegisterComponents('Graphic Edge IO', [TGEExportFile]);
end;
// ------ TGEExportFile.AppendDelimiter --------------------------------------
function TGEExportFile.AppendDelimiter(sDelimiter,sItem:string):string;

begin
  if (sItem <> '') then
    result := sItem + sDelimiter
  else
    result := '';
end;
//------ TGEExportFile.GetHint -------------------------------------------------
{** provides a hint string based on the Export file type}
function TGEExportFile.GetHint:string;

begin
  if (ExportFileType = efCSV) then
    result := sCSV
  else if (ExportFileType = efTSV) then
    result := sTSV
  else if (ExportFileType = efClipboard) then
    result := sCLIPBOARD
  else if (ExportFileType = efSSV) then
    result := sSSV
  else if (ExportFileType = efXML) then
    result := sXML
  else if (ExportFileType = efLatex) then
    result := sLATEX
  else
    result := '';
end;
//------ TGEExportFile.LoadExportFileType ------------------------------------
procedure TGEExportFile.LoadExportFileType;

var
  sType:string;

begin
  Reg.OpenKey(RegPath,true);
  sType := LowerCase(Reg.ReadString(SubRegPath,Identifier+'.DefaultFileType',
    ''));
  Reg.CloseKey;

  if (sType = sCSV) then
    ExportFileType := efCSV
  else if (sType = sTSV) then
    ExportFileType := efTSV
  else if (sType = sCLIPBOARD) then
    ExportFileType := efClipboard
  else if (sType = sSSV) then
    ExportFileType := efSSV
  else if (sType = sXML) then
    ExportFileType := efXML
  else if (sType = sLATEX) then
    ExportFileType := efLaTeX
  else
    ExportFileType := DefaultFileType;
end;
//------ TGEExportFile.SaveNodalSettings ---------------------------------------
procedure TGEExportFile.SaveNodalSettings;

begin
  FilePath := FilePath;
  ExportFileType := ExportFileType;
end;
// ----- TFEExportFile.SaveTableToClipboard ----------------------------------
procedure TGeExportFile.SaveTableToClipboard(sl:TStringList);

const
  sDELIMIT = #9; {** clipboard is tab delimited}

var
  i,iPos,j,k : integer;
  sField,sFieldEntry,sTemp : string;
  checkdatetime : TDateTime;
  clip : TStringList;

begin
  Source.DisableControls;
  screen.Cursor := crHourGlass;

  clip:=TStringList.Create;

{** headers}
  j:=0;
  sFieldEntry := '';

  if OutputFieldNames then
  begin
{** this is the preferred name}
    for i:=0 to sl.Count-1 do
    begin
{**  need to get the real name - only works if there is a one-to-one with
preferred}
      sField := sl.Strings[i];
      if (Source.FieldDefs.IndexOf(sField) <> -1) then
      begin
        sFieldEntry := sFieldEntry + sField;
        inc(j);
        if (j<sl.count) then
          sFieldEntry := sFieldEntry + sDELIMIT;
      end;
    end;
    clip.Add(sFieldEntry);
  end; {output field names}

// now loop over all records
  Source.First;

  for k:= 0 to Source.RecordCount-1 do
  begin
    j:=0;
    sFieldEntry:='';

{** date filtering - reject records that fail}
    if ((LowTime or HighTime) and (DateTimeFilterField <> '')) then
    begin
      checkdateTime := Source.FieldByName(DateTimeFilterField).AsDateTime;
      if AcceptDateBounds then
      begin
        if ((LowTime and (checkdatetime < LowDateTime)) or
          (HighTime and (checkdatetime > HighDateTime))) then
        begin
          Source.Next;
          Continue;
        end;
      end else
      begin
        if ((LowTime and (checkdatetime <= LowDateTime)) or
          (HighTime and (checkdateTime >= HighDateTime))) then
        begin
          Source.Next;
          continue;
        end;
      end;
    end;

    for i:=0 to sl.Count-1 do
    begin
      sField := sl.Strings[i];
      iPos :=Source.FieldDefs.IndexOf(sField);
      if (iPos<>-1) then
      begin
// need formating here with precision [float fields]
        sTemp := Source.FieldByName(sField).AsString;

{** delimiter code - make this subroutine to share with file code}
        if (DelimitEverything and (Delimiter<>'')) then
          stemp := DELIMITER + sTemp + DELIMITER
        else if ((Delimiter <> '') and ((Source.FieldByName(sField)
          is TStringField) or
          (Source.FieldByName(sField) is TWideSTringField))) then
          sTemp := DELIMITER + sTemp + DELIMITER;

        sFieldEntry := sFieldEntry + sTemp;
        inc(j);
        if (j < sl.Count) then
          sFieldEntry := sFieldEntry + sDELIMIT;
      end;
    end;
    clip.Add(sFieldEntry);
    Source.Next;
  end;
  
  Source.EnableControls;
  screen.Cursor :=crDefault;

// the actual copy to clipboard
  ClipBoard.AsText := clip.Text;
end;
// ----- TGEExportFile.SaveTableToFile -----------------------------------------
procedure TGEExportFile.SaveTableToFile(sl:TStringList);

var
  i,iPos,j,k : integer;
  sField,sFieldEntry,sSep,sTemp : string;
  bNoEntry: boolean;
  checkdateTime : TDateTime;

begin

  if (ExportFileType = efCSV) then
    ssep := ','
  else if (ExportFileType = efTSV) then
    ssep := #9
  else if (ExportFileType = efSSV) then
    ssep := ' '
  else if (ExportFileType = efLatex) then
    ssep := ' & ';

  source.DisableControls;
  screen.Cursor := crHourGlass;
  Rewrite(ExportTextFile);

{** writes header info (LaTeX syntax) for LaTeX modes}
  if (ExportFileType = efLaTeX) then
    WriteLaTeXHeader(sl)
  else if (ExportFileType = efXML) then
  begin
{** writes}
    WriteLn(ExportTextFile,'<?xml version="1.0" encoding="UTF-8"?>');
    WriteLn(ExportTextFile,'<records>');
  end else
{** writes field names and units if required}
    WriteFileHeaderInfo(sl,sSep);

{** prep the table}
  Source.First;

{** force output of table units in a specified precision}
  bNoEntry := true;

{** loop over records}
  for k:= 0 to Source.RecordCount-1 do
  begin
    j:=0;

{** date filtering - reject records that fail}
    if ((LowTime or HighTime) and (DateTimeFilterField <> '')) then
    begin
      checkdateTime :=
        Source.FieldByName(DateTimeFilterField).AsDateTime;
      if AcceptDateBounds then
      begin
        if ((LowTime and (checkdatetime < LowDateTime)) or
          (HighTime and (checkdatetime > HighDateTime))) then
        begin
          Source.Next;
          Continue;
        end;
      end else
      begin
        if ((LowTime and (checkdatetime <= LowDateTime)) or
          (HighTime and (checkdateTime >= HighDateTime))) then
        begin
          Source.Next;
          continue;
        end;
      end;
    end;

{** initialise the field entry}
    if (ExportFileType = efXML) then
      sFieldEntry := '<record>'
    else
      sFieldEntry := '';

    bNoEntry := false;

    for i:=0 to sl.Count-1 do
    begin
      sField := sl.Strings[i];
      iPos := Source.FieldDefs.IndexOf(sField);

      if (iPos <> -1) then
      begin
// need formating here with precision...
        sTemp := Source.FieldByName(sField).AsString;
        if (DelimitEverything and (Delimiter<>'')) then
          stemp := DELIMITER + sTemp + DELIMITER
        else if ((Delimiter <> '') and
          (Source.Fields[iPos] is TStringField) or
          (Source.Fields[iPos] is TWideSTringField)) then
          sTemp := DELIMITER + sTemp + DELIMITER;

        if (ExportFileType = efXML) then
        begin
          sFieldEntry := sFieldEntry + '<field name="'+sField+'">'+sTemp+'</field>';
          inc(j);
        end else
        begin
          sFieldEntry := sFieldEntry + sTemp;
          inc(j);
          if (j < sl.Count) then
            sFieldEntry := sFieldEntry + ssep;
        end;
      end;
    end;


    if (ExportFileType = efLatex) then
      sFieldEntry := sFieldEntry + ' \\';

    WriteLn(ExportTextFile,sFieldEntry);

    if (ExportFileType = efLatex) and LatexHLine then
      WriteLn(ExportTextFile,'\hline');
    if (ExportFileType = efXML) then
      WriteLn(ExportTextFile,'</record>');

    Source.Next;
  end; {** for k = 0 to RecordCount-1 - loop over all records}

  if (ExportFileType = efLatex) then
  begin
    if (LatexEnvironment = sLATEXTABULAR) then
      Write(ExportTextFile,'\end{tabular}')
    else if (LatexEnvironment = sLATEXLONGTABLE) then
      Write(ExportTextFile,'\end{longtable}');
  end else if (ExportFileType = efXML) then
    Write(ExportTextFile,'</records>');

  CloseFile(ExportTextFile);
  Source.EnableControls;
  screen.Cursor :=crDefault;

  if (bNoentry) and (DateTimeFilterField <> '') then
    MessageDlg('Date filtering caused all records to be rejected!',
      mtInformation, [mbOK], 0);

  if (OpenFileAfterExport and (not bNoEntry)) then
    ShellExecute(TForm(self.owner).handle,pchar('open'),
      pchar(FileName),nil,nil,SW_NORMAL);
end;
// ------ TGEExportFile.SetDelimiter -----------------------------------------
procedure TGEExportFile.SetDelimiter(sDelimiter:string);

begin
  if (fDelimiter <> sDelimiter) then
  begin
    fDelimiter := sDelimiter;
    Reg.OpenKey(RegPath,true);
    Reg.WriteString(SubRegPath,Identifier+'.Delimiter',sDelimiter);
    Reg.CloseKey;
  end;
end;
//------ TGEExportFile.SetExportFileTYpe -------------------------------------
procedure TGEExportFile.SetExportFileType(aFileType:TExportFileType);

var
  sType:string;

begin
  fExportFileType := aFileType;

  if (fExportFileType = efCSV) then
    sType := sCSV
  else if (fExportFileType = efTSV) then
    sType := STSV
  else if (fExportFileType = efClipboard) then
    sType := sCLIPBOARD
  else if (fExportFileType = efSSV) then
    sType := sSSV
  else if (fExportFileType = efXML) then
    sType :=  sXML
  else if (fExportFileType = efLatex) then
    sType := sLATEX;

{** enable the tooltips of the applications export button to update if req.}
  DoOnTypeChange;

  Reg.OpenKey(RegPath,true);
  Reg.WriteString(SubRegPath,Identifier+'.DefaultFileType',sType);
  Reg.CloseKey;
end;

//------ TGEExportFile.SetFilePath -------------------------------------------
procedure TGEExportFile.SetFilePath(aFilePath:string);

begin
  fFilePath := aFilePath;
  with Reg do
  begin
    OpenKey(RegPath,true);
    WriteString(SubRegPath,Identifier+'.FilePath',aFilePath);
    CloseKey;
  end;
end;
// ------ TGEExportFile.SetExportFile ------------------------------------------
procedure TGEExportFile.SetExportFile(exporter:TformExporter);

var
  SaveDialog : TSaveDialog;
  sFileName:string;

begin
  SaveDialog := TSaveDialog.Create(nil);
  with SaveDialog do
  begin
    Filter := 'Comma Separated Value files (*.csv)|*.csv|'+
      'Text files (*.txt)|*.txt|'+
      'XML files (*.xml)|*.xml|'+
      'TeX files (*.tex)|*.tex|'+
      'VTK files (*.vtk)|*.vtk|'+
      'All files|*.*';
    Options := [ofOverwritePrompt];

    InitialDir := FilePath; {** needs checking}
    if (ExportFileType = efCSV) then
    begin
      Title := 'Save Comma Separated Value file';
      FilterIndex  := 1;
      DefaultExt := '.csv';
    end else if (ExportFileType = efTSV) then
    begin
      Title := 'Save Tab Separated Value file';
      FilterIndex := 2;
      DefaultExt := '.txt';
    end else if (ExportFileType = efSSV) then
    begin
      Title := 'Save Space Separated Value file';
      FilterIndex := 2;
      DefaultExt := '.txt';
    end else if (ExportFileType = efXML) then
    begin
      Title := 'Save XML File';
      FilterIndex := 3;
      DefaultExt := '.xml';
    end else if (ExportFileType = efLatex) then
    begin
      Title := 'Save LaTeX File';
      FilterIndex := 4;
      DefaultExt := '.tex';
    end;

    if Execute then
    begin
      sFileName := FileName;
      self.FileName := sFileName;
      AssignFile(ExportTextFile,sFileName);
      SaveTableToFile(exporter.storedfields);
    end;

    Free;
  end;
end;
//------ TUnitExportFile.SetIdentifier -----------------------------------------
procedure TGEExportFile.SetIdentifier(sIdentifier:string);

begin
  fIdentifier := sIdentifier; // always load
  LoadExportFileType;
  LoadFilePath;
end;
//------ TGEExportFile.DoOnTypeChange ------------------------------------------
procedure TGEExportFile.DoOnTypeChange;

begin
 if Assigned(fOnTypeChange) then
   fOnTypechange(self);
end;
//------ TGEExportFile.Loaded ------------------------------------------------
procedure TGEExportFile.Loaded;

begin
  inherited Loaded;
end;
//------ TGEExportFile.LoadFilePath --------------------------------------------
procedure TGEExportFile.LoadFilePath;

begin
  Reg.OpenKey(RegPath,true);
  FilePath := Reg.ReadString(SubRegPath,Identifier+'.FilePath',
    ExtractFilePath(Application.ExeName));
  Reg.CloseKey;
end;
//------ TGEExportFile.LoadRegistrySettings ----------------------------------
{** this imports the stored fieldnames from the registry}
procedure TGEExportFile.LoadRegistrySettings(fsl:TStringList);

const
  iDELIMITER = 1;
  sDELIMITER = ',';

var
{** s1,s2 are temp string variables}
  s1,s2,sPath,sStored : string;
  bBlankLow:boolean;

begin
  fsl.Clear;

  Reg.OpenKey(RegPath,true);
  sStored := Reg.ReadString(SubRegPath,Identifier+'.FieldSelection','');

  if (sStored <> '') then
  begin
    s1 := sStored;
    while (s1 <> '') do
    begin
      if (Pos(sDELIMITER,s1) > 0) then
      begin
        s2 := Copy(s1,1,Pos(sDELIMITER,s1)-1);
        s1 := Copy(s1,Pos(sDELIMITER,s1) + iDELIMITER,Length(s1));
      end else
      begin
        s2 := s1;
        s1 := '';
      end;
      fsl.Add(s2);
    end;
  end;

{** default settings}
  sStored := Reg.ReadString(SubRegPath,Identifier+'.Options','');

  OpenFileAfterExport := (Pos(sOPEN,sStored) > 0);
  OutputFieldNames :=  ((Pos(sFIELDNAMES,sStored) > 0) or (sStored = ''));
  ShowHiddenFields := (Pos(sHIDDEN,sStored) > 0);
  AcceptDateBounds := (Pos(sACCEPTDATEBOUNDS,sStored) > 0);

  LowTime := (Pos(sLOWTIME,sStored) > 0);
  HighTime := (Pos(sHIGHTIME,sStored) > 0);
  DateTimeFilterField := Reg.ReadString(SubRegPath,Identifier+'.DateTimeField','');
  Precision := Reg.ReadInteger(SubRegPath,Identifier+'.Precision',15);
  sStored := Reg.ReadString(SubRegPath,Identifier+'.LaTeXOptions','');
  Reg.CloseKey;

{** check for environment string - default to normal tabular}
  LatexEnvironment := sLATEXTABULAR;
  if (Pos(sLATEXLONGTABLE,sStored) > 0) then
    LatexEnvironment := sLATEXLONGTABLE;
  if (pos(sLATEXTABULAR,sStored) > 0) then
    LatexEnvironment := sLATEXTABULAR;

  Reg.OpenKey(RegPath + '\' + SubRegPath,false);

  sPath := Identifier+'.LowDateTime';
  if Reg.ValueExists(sPath) then
  begin
    LowDateTime := Reg.ReadFloat(sPath);
    bBlankLow := false;
  end else
  begin
    LowDateTime := Now;
    bBlankLow := true;
  end;

  sPath := Identifier+'.HighDateTime';
  if Reg.ValueExists(sPath) then
    HighDateTime := Reg.ReadFloat(sPath)
  else
    HighDateTime := Now;

  if bBlankLow then
    HighDateTime := Now + 1.0;
  Reg.CloseKey;
end;
//------ TGEExportFile.SaveRegistrySettings ----------------------------------
{** this saves the stored fieldnames into registry}
procedure TGEExportFile.SaveRegistrySettings(fsl:TStringList);

const
  sDELIMITER = ',';

var
  i,iCount : integer;
  sPath,sRegEntry : string;

begin
  Reg.OpenKey(RegPath,true);
  sRegEntry := '';
  iCount := fsl.Count-1;
  for i:=0 to iCount do
  begin
    sRegEntry := sRegEntry + fsl.Strings[i];
    if (i<iCount) then
      sRegEntry := sRegEntry + sDELIMITER;
  end;
  Reg.WriteString(SubRegPath,Identifier + '.FieldSelection',sRegEntry);

{** write export options}
  sRegEntry:='';
  if OpenFileAfterExport then
    sRegEntry := sOPEN;

  if OutputFieldNames then
  begin
    sRegEntry := AppendDelimiter(sDELIMITER,sRegEntry);
    sRegEntry := sRegEntry + sFIELDNAMES;
  end;

  if ShowHiddenFields then
  begin
    sRegEntry := AppendDelimiter(sDELIMITER,sRegEntry);
    sRegEntry := sRegEntry + sHIDDEN;
  end;

  if AcceptDateBounds then
  begin
    sRegEntry := AppendDelimiter(sDELIMITER,sRegEntry);
    sRegEntry := sRegEntry + sACCEPTDATEBOUNDS;
  end;

  if LowTime then
  begin
    sRegEntry := AppendDelimiter(sDELIMITER,sRegEntry);
    sRegEntry := sRegEntry + sLOWTIME;
  end;

  if HighTime then
  begin
    sRegEntry := AppendDelimiter(sDELIMITER,sRegEntry);
    sRegEntry := sRegEntry + sHIGHTIME;
  end;

  Reg.WriteString(SubRegPath,Identifier+'.Options',sRegEntry);
  sRegEntry := LatexEnvironment;

{** save registry settings for this node}
  sRegEntry := LatexEnvironment;
  Reg.WriteString(SubRegPath,Identifier+'.LatexOptions',sRegEntry);
  if LatexHLine then
    sRegEntry := sRegEntry + sLATEXHLINES;
  if LatexVRule then
    sRegEntry := sRegEntry + sLATEXVERTRULE;

  Reg.WriteSTring(SubRegPath,Identifier+'.DateTimeField',DateTimeFilterField);

  Reg.WriteInteger(SubRegPath,Identifier+'.Precision',Precision);
  Reg.CloseKey;

  Reg.OpenKey(RegPath + '\'+SubRegPath,true);
  sPath := Identifier+'.LowDateTime';

  Reg.WriteFloat(sPath,LowDateTime);

  sPath := Identifier+'.HighDateTime';
  Reg.WriteFloat(sPath,HighDateTime);
  Reg.CloseKey;
end;
// ------ TGEExportFile.WriteFileHeaderInfo ------------------------------------
procedure TGEExportFile.WriteFileHeaderInfo(sl:TstringList;sSep:string);

var
  i,j:integer;
  sField,sFieldEntry:string;

begin
  j:=0;
  sFieldEntry := '';

  if OutputFieldNames then
  begin
    for i:=0 to sl.Count-1 do
    begin
      sField := sl.Strings[i];
      if (Source.FieldDefs.IndexOf(sField) <> -1) then
      begin
        sFieldEntry := sFieldEntry + sField;
        inc(j);
        if (j<sl.count) then
          sFieldEntry := sFieldEntry + ssep;
      end;
    end;
    if ExportFIleType = efLatex then
      sFieldEntry := sFieldEntry + ' \\';
    WriteLn(ExportTextFile,sFieldEntry);
    if (ExportFileType = efLatex) and LatexHLine then
      WriteLn(ExportTextFile,'\hline');
  end;
end;
// ------ TGEExportFile.WriteLaTexHeader -------------------------------------
procedure TGEExportFile.WriteLaTexHeader(sl:TStringList);

var
  sTemp : string;
  i : integer;

begin
  if (LaTeXEnvironment = sLATEXTABULAR) then
  begin
    sTemp := '\begin{tabular}{';
    if LatexVRule then
      sTemp := sTemp + '|';
{** set up the column definitions}
    for i := 0 to sl.Count-1 do
    begin
      sTemp := sTemp + LateXColType;
      if LatexVRule then
        sTemp := sTemp + '|';
    end;
    sTemp := sTemp + '}';
    Write(ExportTextFile,sTemp);
    Write(ExportTextFile,#13);
    if LatexHLine then
    begin
      Write(ExportTextFile,'\hline');
      Write(ExportTextFile,#13);
    end;
{** LONGTABLE Mode}
  end else if (LatexEnvironment = sLATEXLONGTABLE) then
  begin
    sTemp := '\begin{longtable}{';
    if LatexVRule then
      sTemp := sTemp + '|';
{** set up the column definitions}
    for i := 0 to sl.Count-1 do
    begin
      sTemp := sTemp + 'c';
      if LatexVRule then
        sTemp := sTemp + '|';
    end;
    sTemp := sTemp + '}';
    Write(ExportTextFile,sTemp + #13);
{** first caption}
    Write(ExportTextFile,
      '\caption[Short first caption]{Long first caption} \\'+#13);
    if LatexHLine then
      Write(ExportTextFile,'\hline'+#13);
    Write(ExportTextFile,'\endfirsthead'+#13);
{** repeated captions/headers}
    Write(ExportTextFile,'\caption[]{(continued)} \\'+#13);
    if LatexHLine then
      Write(ExportTextFile,'\hline'+#13);
    Write(ExportTextFile,'\endhead'+#13);
{** first footer}
    if LatexHLine then
      Write(ExportTextFile,'\hline'+#13);
    Write(ExportTextFile,'\endfoot'+#13);
{** last footer}
    if LatexHLine then
      Write(ExportTextFile,'\hline'+#13);
    Write(ExportTextFile,'\endlastfoot'+#13);
  end;
end;
//------ TGEExportFile.Create --------------------------------------------------
constructor TGEExportFile.Create(aOwner:TComponent);

begin
  inherited Create(aOwner);
  Reg:= TRegIniFile.Create;
  fDefaultFileType := efCSV;
  fDelimiter:= '';
  fDelimitEverything:= false;
  fLowDateTime := Now;
  fHighDateTime := Now+1;
  fAcceptDateBounds := false;
  fSubRegPath:=sEXPORT;
  LowTime := false;
  HighTime := false;
  fIdentifier := 'Untitled';
end;
//------ TGEExportFile.Destroy -------------------------------------------------
destructor TGEExportFile.Destroy;

begin
  Reg.Free;
  inherited Destroy;
end;
// ----- TGEExportFile.Execute -------------------------------------------------
procedure TGEExportFile.Execute;

var
  bAcceptDateBounds,bDelimitEverything,bFields,bHidden,bHighTime,bHLine,
    bKeepExporting,bLowTime,bOpen,bVRule : boolean;
  i : integer;
  sDateTimeField,sDelimiter,sLateXEnv : string;
  aHighDateTime, aLowDateTime : TDateTime;
  dsl : TStringList;
  iPrecision :integer;

begin
  bKeepExporting := true;

  exporter := TformExporter.Create(self);
  exporter.Source := Source;

  dsl := TStringList.Create;
  for i:=0 to Source.FieldCount-1 do
  begin
    if ((Source.Fields[i] is TDateField) or
        (Source.Fields[i] is TTimeField) or
        (Source.Fields[i] is TDateTimeField)) then
      dsl.Add(Source.Fields[i].FieldName);
  end;
  exporter.pnlLatex.visible := (ExportFileType = efLaTeX);

  exporter.PrepCaption(ord(ExportFileType),LaTeXEnvironment,LaTeXHLine,LaTeXVRule);

  while bKeepExporting do
  begin
{** load registry settings here - i.e. which items to check etc.}
    LoadRegistrySettings(exporter.storedfields);
    exporter.PrepFilters(DateTimeFilterField,dsl);

    exporter.CopySettings(OpenFileAfterExport,OutputFieldNames,ShowHiddenFields,
      LowTime,HighTime,DelimitEverything,AcceptDateBounds,
      LowDateTime,HighDateTime,Delimiter,Precision);

    if (exporter.ShowModal = mrOK) then
    begin
{** this saves the settings from the exporter form to the properties here}
      exporter.SaveSettings(bOpen,bFields,bHidden,
        bLowTime,bHighTime,bDelimitEverything,bAcceptDateBounds,
        aLowDateTime,aHighDateTime,sDateTimeField,
        sDelimiter,iPrecision);

      Precision := iPrecision;
      OpenFileAfterExport:=bOpen;
      OutputFieldNames:=bFields;
      ShowHiddenFields:=bHidden;
      LowTime := bLowTime;
      HighTime := bHighTime;
      LowDateTime := aLowDateTime;
      HighDateTime := aHighDateTime;
      Delimiter := sDelimiter;
      DelimitEverything:= bDelimitEverything;
      AcceptDateBoundS := bAcceptDateBounds;
      if (sDateTimeField <> '') then
        DateTimeFilterField := sDateTimeField;

{** save LaTex Settings}
      if (ExportFileType = efLatex) then
        exporter.SaveLaTexSettings(sLatexEnv,bHline,bVrule);

      LatexEnvironment := sLatexEnv;
      LatexHLine := bhline;
      LatexVRule := bVrule;

{** firstly save the current selection to the registry}
      SaveRegistrySettings(exporter.storedfields);
      SaveNodalSettings;

      if (exporter.storedfields.Count > 0) then
      begin
        if (ExportFileType = efClipboard) then
          SaveTableToClipboard(exporter.storedfields)
        else
          SetExportFile(exporter);
      end;
    end else
      bKeepExporting := false; {** cancel action stops the exporting cycle}
  end;
  dsl.Free;
  exporter.Release;
end;

end.

