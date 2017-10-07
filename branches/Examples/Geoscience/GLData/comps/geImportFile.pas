{
@abstract(provides a component for importing from clipboard and text files)
@author(Aaron Hochwimer <aaron@graphic-edge.co.nz>)
@created(June 27, 2003)
@lastmod(June 29, 2003)
This unit provides the TGEImportFile component. This is a general
purpose importing module to import data from comma, tab, or space separated data
files or from the clipboard.
}
unit geImportFile;

// $Id: geImportFile.pas,v 1.3 2003/09/11 05:25:52 hochwimmera Exp $

interface

uses
  Classes, Clipbrd, controls, db, Dialogs, Forms,
  Registry,Sysutils, Windows, Variants,
// Third Party
  {kbmMemTable,}
// local
  frmImport;

const
{clipboard}
  sCLIPBOARD = 'clipboard';
{comma separated value file}
  sCSV = 'comma separated value ';
{fixed format value file - very similar to SSV}
  sFFS = 'fixed format space';
{ignore this column}
  sIGNORE = 'Ignore';
{import identifier}
  sIMPORT = 'import';
{space separated value file}
  sSSV = 'space separated value';
{tab separated value file}
  sTSV = 'tab separated value';

type
{Enumeration type providing a list of import file types. Current types include:
ifCSV,ifTSV,ifSSV,ifFixedFormatSpace,ifClipboard}
  TImportFileType = (ifCSV,ifTSV,ifSSV,ifFixedFormatSpace,ifClipboard);

{Component to handle importing. Simply connect it to a kbmMemTable - the
destination - called by the Execute method}
  TGEImportFile = class(TComponent)
  private
    fAllDelimiters : boolean;
    fAllowFields : TStringList;
    fDefaultFileType : TImportFileType;
    fDelimiter : string;
    {fDestination : TkbmMemTable;}
    fEFileName : string;
    fFilePath : string;
    fGroupDelimiters : boolean;
    fHeaderSkip : integer;
    fIdentifier : string;
    fImporter : TformGEImporter;
    fImportFileType : TImportFileType;
    fOnAfterImport : TNotifyEvent;
    fOnTypeChange : TNotifyEvent;
    fReg : TRegIniFile;
    fRegPath : string;
    fTextFile : TextFile;
{strips a string of its delimiter(s), behaviour varies based on AllDelimiters}
    procedure CheckStripDelimiter(var sValue:string);
{obtains the index of the last delimiter in sLine}
    procedure GetMaxDelimiters(sLine:string;var id2:integer);
{generates a hint string based on the current ImportFileType}
    function GetHint:string;
{get next valid item based on the separator sSEP}
    function GetNextItem(sSEP:string;var Sepstring:string):string;
{returns the position of the last instance of Substr in S}
    function LastPos(Substr,S: string): integer;
{loads the default ImportFileType from registry}
    procedure LoadImportFileType;
{saves current settings such as header rows to skip and also the import file
type}
    procedure SaveIdentifierSettings;
{sets the AllDelimiters property and stores in registry}
    procedure SetAllDelimiters(bAllDelimiters:boolean);
{sets the current Delimiter string and stores in registry}
    procedure SetDelimiter(sDelimiter:string);
{sets the last file path and stores in registry}
    procedure SetFilePath(sFilePath:string);
{sets the GroupDelimiters property and stores in registry}
    procedure SetGroupdelimiters(bGroupDelimiters:boolean);
{sets the HeaderSkip property and stores in registry}
    procedure SetHeaderSkip(iHeaderSkip:integer);
{sets the Identifier property}
    procedure SetIdentifier(sIdentifier:string);
{sets the ImportFileType}
    procedure SetImportFileType(aFileType:TImportFileType);
{writes a string to the importer form grid}
    procedure WriteTableLine(sSep:string;var sLine:string);
  protected
{event handler run after a successful import}
    procedure DoOnAfterImport;virtual;
{event handler run after an import type change}
    procedure DoOnTypeChange;virtual;
{generates a stringlist of allowable fields based on the field definitions
 of the destination dataset}
    procedure GenerateAllowFields;
{loaded method - should check at some future date for valid published properties}
    procedure Loaded;override;
{load the last file path from registry}
    procedure LoadFilePath;
{load the headerskip property from registry}
    procedure LoadHeaderSkip;
{save the current field selection and delimiter to registry}
    procedure SaveRegistrySettings(fsl:TStringList);
{examines contents of clipboard for the column number}
    procedure SetImportClipboard(importer:TformGEImporter;var bContinue:boolean;
      var sl:TStringList;var clip:TStringList);
{examines the text file for the maximum number of columns. Prompts for
the file name}
    procedure SetImportFile(importer:TformGEImporter;var bContinue:boolean;
      var sl:TStringList);
  public
{Class constructor. Creates Reg property for use in reading and writing to
and from the registry}
    constructor Create(aOwner:TComponent);override;
{Class destructor. Cleans up}
    destructor Destroy;override;
{Call execute to intitiate an import - returns true if successful}
    function Execute:boolean;
{Imports data from clipboard}
    procedure ImportDataFromClipboard(clip:TStringList);
{Imports data from text file}
    function ImportDataFromFile(iHeader:integer;bPrompt:boolean):boolean;
{Load current settings from registry}
    procedure LoadRegistrySettings(fsl:TStringList);
{If AllDelimiters is true importing will remove all instances of Delimiter from
the current line}

    property AllDelimiters: boolean read fAllDelimiters write SetAllDelimiters;
{AllowFields is a stringlist detailing the available fields that may be
populated by the import procedure}
    property AllowFields : TStringList read fAllowFields write fAllowFields;
{The current delimiter string. May be blank, single quote ('), or double quote (")}
    property Delimiter : string read fDelimiter write SetDelimiter;
{Name of the last file that was imported}
    property EFileName : string read fEFileName write fEFileName;
{The file path (including name) of the the last file that was imported}
    property FilePath : string read fFilePath write SetFilePath;
{Number of header lines to skip in the importing procedure}
    property HeaderSkip : integer read fHeaderSkip write SetHeaderSkip;
{Hint string - based on the current ImportFileType}
    property Hint : string read GetHint;
{Current Identifier. This may be altered to store unique settings in the registry}
    property Identifier : string read fIdentifier write SetIdentifier;
{Importer - a form for the user to set up the various import parameters. It also
provides a visual preview of the data}
    property Importer : TformGEImporter read fImporter write fImporter;
{Text file variable use for file based importing}
    property ImportTextFile : TextFile read fTextFile write fTextFile;
{Registry variable used for loading and saving settings}
    property Reg:TRegIniFile read fReg write fReg;
  published
{Default file type}
    property DefaultFileType : TImportFileType read fDefaultFileType write
      fDefaultFileType;
{Destination dataset - at present a TkbmMemTable but could be expanded in the
future}
{    property Destination : TkbmMemTable read fDestination write fDestination;}
{GroupDelimeters property will ignore separators (e.g. comma in CSV) if they
are within a matching pair of delimiters. Useful if you wish to import a string
containing a comma - e.g. "thats all she said, mate","no way"}
    property GroupDelimiters : boolean read fGroupDelimiters write
      SetGroupDelimiters;
{ImportFileType - either ifCSV,ifTSV,ifSSV,ifFixedFormatSpace,ifClipboard}
    property ImportFileType : TImportFileType read fImportFileType write
      SetImportFileType;
{Event handler called after a successful import procedure}
    property OnAfterImport : TNotifyEvent read fOnAfterImport write fOnAfterImport;
{Event handler called after a type change}
    property OnTypeChange : TNotifyEvent read fOnTypeChange write fOnTypeChange;
{Top level registry path}
    property RegPath : string read fRegPath write fRegPath;
  end;

  procedure Register;

implementation
// ----- TGEImportFile.CheckStripDelimiter -------------------------------------
procedure TGEImportFile.CheckStripDelimiter(var sValue:string);

begin
  if (Delimiter <> '') then
  begin
    if AllDelimiters then
      sValue := StringReplace(sValue,Delimiter,'',[rfReplaceAll,rfIgnoreCase])
    else
    begin
// delete the matching pair around a string
      if ((sValue[1]=Delimiter) and (sValue[Length(sValue)]=Delimiter)) then
      begin
        Delete(sValue,Length(sValue),1);
        Delete(sValue,1,1);
      end;
    end;
  end;
end;
// ----- TGEImportFile.GetMaxDelimiters ----------------------------------------
procedure TGEImportFile.GetMaxDelimiters(sLine:string;var id2:integer);

var
  sCopy,sSep : string;
  iPos : integer;
  bContinue : boolean;

begin
  bContinue := true;

  sCopy := sLine;

  if (ImportFileType = ifCSV) then
    sSEP := ','
  else if (ImportFileType = ifTSV) then
    sSEP := #9
  else if ( (ImportFileType = ifSSV) or
            (ImportFileType = ifFixedFormatSpace)) then
    sSEP := ' ';

  id2 := 0;
  while bContinue do
  begin
    iPos := Pos(sSEP,sCopy);
    if (iPos > 0) then
    begin
      Inc(id2);
      Delete(sCopy,1,iPos);
    end else
    begin
      bContinue := false;
    end;
  end;
end;
// ----- TGEImportFile.GetHint -------------------------------------------------
function TGEImportFile.GetHint:string;

begin
  if (ImportFileType = ifCSV) then
    result := 'comma separated value file'
  else if (ImportFileType = ifTSV) then
    result := 'tab separated value file'
  else if (ImportFileType = ifClipboard) then
    result := 'clipboard'
  else if (ImportFileType = ifSSV) then
    result := 'space separated value file'
  else if (ImportFileType = ifFixedFormatSpace) then
    result := 'fixed format file (space separated value)'
  else
    result := '';
end;
// ----- TGEImportFile.GetNextItem ---------------------------------------------
function TGEImportFile.GetNextItem(sSEP:string;var SepString:string):string;

var
  DelPos,SepPos: integer; // position of 2nd delimiter,separator string
  sTest: string;  // tempstring to locate DelPos

begin
// for the FixedFormat type start be trimming to left
  if (ImportFileType = ifFixedFormatSpace) then
    SepString := TrimLeft(SepString);

  if (SepString <> '') then
  begin
// now eval DelPos if required for delimiters like ".." and '..'
    if (GroupDelimiters and (SepString[1]=Delimiter)) then
    begin
// strip the first Delimiter using sTest
      sTest := Copy(SepString,2,Length(SepString));
// locates second matching delimiter - add one for the missing first one
      DelPos := Pos(Delimiter,sTest) + 1;
    end else
      DelPos := 0;

    SepPos := Pos(sSEP,SepString);
    if (SepPos > 0) then
    begin
// matching delimiter code - i.e. grouping delimiters
      if (DelPos > 0) then
      begin
        result := Copy(SepString,1,DelPos);
        Delete(SepString,1,DelPos);
      end else
      begin
// normal instance
        result := Copy(SepString,1,SepPos-1);
        Delete(SepString,1,SepPos);
      end;
    end else
    begin
// the last item in the string
      result := SepString;
      SepString := '';
    end;
  end else
    result := '';
end;
// ----- TGEImportFile.LastPos -------------------------------------------------
function TGEImportFile.LastPos(Substr,S: string): integer;

var
  Found: Boolean;
  TempS: string;
  StrPos, LastStrPos,NumRemoved,SubStrLength: integer;
begin
  TempS:=S;
  SubStrLength:=Length(Substr);
  LastStrPos := 0;
  StrPos:=0;
  NumRemoved:=0;
  Found:=True;
  while Found do
    begin
      LastStrPos:=StrPos;
      StrPos:=Pos(Substr,TempS);
      if StrPos>0 then
        begin
          Delete(TempS,StrPos,SubStrLength);
          Inc(NumRemoved);
        end
      else
        Found:=False;
    end;
  if NumRemoved>0 then
    Result:=LastStrPos+(NumRemoved-1)*SubStrLength
  else
    Result:=0;
end;

// ----- TGEImportFile.LoadImportFileType --------------------------------------
procedure TGEImportFile.LoadImportFileType;

var
  stype:string;

begin
  Reg.OpenKey(RegPath,true);
  stype:=LowerCase(Reg.ReadString(sIMPORT,Identifier+'.DefaultFileType',''));
  Reg.CloseKey;

  if (stype = sCSV) then
    ImportFileType := ifCSV
  else if (stype = sTSV) then
    ImportFileType := ifTSV
  else if (stype = sCLIPBOARD) then
    ImportFileType := ifClipBoard
  else if (sType = sSSV) then
    ImportFileType := ifSSV
  else if (sType = sFFS) then
    ImportFileType := ifFixedFormatSpace
  else
    ImportFileType := DefaultFileType;
end;

// ----- TGEImportFile.SaveIdentifierSettings ----------------------------------
procedure TGEImportFile.SaveIdentifierSettings;

begin
  HeaderSkip := HeaderSkip;
  ImportFileType := ImportFileType;
end;

// ----- TGEImportFile.SetAllDelimiters ----------------------------------------
procedure TGEImportFile.SetAllDelimiters(bAllDelimiters:boolean);

begin
  if (fAllDelimiters <> bAllDelimiters) then
  begin
    fAllDelimiters := bAllDelimiters;
    Reg.OpenKey(RegPath,true);
    Reg.WriteBool(sIMPORT,Identifier+'.alldelimiters',fAllDelimiters);
    Reg.CloseKey;
  end;
end;
// ----- TGEImportFile.SetDelimiter --------------------------------------------
procedure TGEImportFile.SetDelimiter(sDelimiter:string);

begin
  if (fDelimiter <> sDelimiter) then
  begin
    fDelimiter := sDelimiter;
    Reg.OpenKey(RegPath,true);
    Reg.WriteString(sIMPORT,Identifier+'.delimiter',fDelimiter);
    Reg.CloseKey;
  end;
end;

// ----- TGEImportFile.SetFilePath ---------------------------------------------
procedure TGEImportFile.SetFilePath(sFilePath:string);

begin
  if (fFilePath <> sFilePath) then
  begin
    fFilePath := sFilePath;
    Reg.OpenKey(RegPath,true);
    Reg.WriteString(sIMPORT,Identifier+'.FilePath',sFilePath);
    Reg.CloseKey;
  end;
end;

// ----- TGEImportFile.SetGroupdelimiters --------------------------------------
procedure TGEImportFile.SetGroupdelimiters(bGroupDelimiters:boolean);

begin
  if (fGroupDelimiters <> bGroupDelimiters) then
  begin
    fGroupDelimiters := bGroupDelimiters;
    Reg.OpenKey(RegPath,true);
    Reg.WriteBool(sIMPORT,Identifier+'.GroupDelimiters',bGroupDelimiters);
    Reg.CloseKey;
  end;
end;

// ----- TGEImportFile.SetHeaderSkip -------------------------------------------
procedure TGEImportFile.SetHeaderSkip(iHeaderSkip:integer);

begin
  if (fHeaderSkip <> iHeaderSkip) then
  begin
    fHeaderSkip := iHeaderSkip;
    Reg.OpenKey(RegPath,true);
    Reg.WriteInteger(sIMPORT,Identifier+'.HeaderSkip',iHeaderSkip);
    Reg.CloseKey;
  end;
end;
// ----- TGEImportFile.SetIdentifier -------------------------------------------
procedure TGEImportFile.SetIdentifier(sIdentifier:string);

begin
  fIdentifier := sIdentifier;
 LoadImportFileType;
 LoadHeaderSkip;
 LoadFilePath;
end;
// ----- TGEImportFile.SetImportFileType ---------------------------------------
procedure TGEImportFile.SetImportFileType(aFileType:TImportFileType);

var
 sType :string;

begin
  fImportFileType := aFileType;

  if (fImportFileType = ifCSV) then
    sType := sCSV
  else if (fImportFileType = ifTSV) then
    sType := STSV
  else if (fImportFileType = ifClipboard) then
    sType := sCLIPBOARD
  else if (fImportFileType = ifSSV) then
    sType := sSSV
  else if (fImportFileType = ifFixedFormatSpace) then
    sTYpe := sFFS;

  if (RegPath <> '') then
  begin
    Reg.OpenKey(RegPath,true);
    Reg.WriteString(sIMPORT,Identifier+'.DefaultFileType',sType);
    Reg.CloseKey;
  end;
end;

// ----- TGEImportFile.WriteTableLine ------------------------------------------
procedure TGEImportFile.WriteTableLine(sSep:string;var sLine:string);

var
 j:integer;
 sFieldName:string;

begin
{
  with Importer.kbmImport do
    for j:=0 to FieldDefs.Count-1 do
    begin
      sFieldName := FieldDefs[j].Name;
      FieldByName(sFieldName).AsString := GetNextItem(sSEP,sLine);
    end;
}
end;
// ----- TGEImportFile.DoOnAfterImport -----------------------------------------
procedure TGEImportFile.DoOnAfterImport;

begin
  if Assigned(fOnAfterImport) then
    fOnAfterImport(self);
end;
// ----- TGEImportFile.DoOnTypeChange ------------------------------------------
procedure TGEImportFile.DoOnTypeChange;

begin
  if Assigned(fOnTypeChange) then
    fOnTypeChange(self);
end;
// ----- TGEImportFile.GenerateAllowFields -------------------------------------
procedure TGEImportFile.GenerateAllowFields;

var
 i:integer;
 sFieldName:string;

begin
  AllowFields.Clear;
  {
  with Destination do
  begin
    if (FieldCount > 0) then
    begin
      for i:=0 to FieldCount-1 do
      begin
        sFieldName:=Fields[i].FieldName;
// only allow visible and writeable import columns
        if ((Fields[i].ReadOnly=false) and (Fields[i].Visible=true))  then
          AllowFields.Add(sFieldName);
      end;
    end;
  end;
  }
end;
// ----- TGEImportFile.Loaded --------------------------------------------------
procedure TGEImportFile.Loaded;

begin
  inherited Loaded;
// additional checks for published properties are required
end;
// ----- TGEImportFile.LoadFilePath --------------------------------------------
procedure TGEImportFile.LoadFilePath;

begin
  Reg.OpenKey(RegPath,true);
  fFilePath := Reg.ReadString(sIMPORT,Identifier+'.FilePath',
    ExtractFilePath(ParamStr(0)));
  Reg.CloseKey;
end;
// ----- TGEImportFile.LoadHeaderSkip ------------------------------------------
procedure TGEImportFile.LoadHeaderSkip;

begin
  Reg.OpenKey(RegPath,true);
  fHeaderSkip := Reg.ReadInteger(sIMPORT,Identifier+'.HeaderSkip',0);
  Reg.CloseKey;
end;
// ----- TGEImportFile.SaveRegistrySettings ------------------------------------
procedure TGEImportFile.SaveRegistrySettings(fsl:TStringList);

var
 i:integer;
 sRegEntry:string;

begin
  sRegEntry := '';
  for i:=0 to fsl.Count-1 do
  begin
    sRegEntry := sRegEntry+fsl.STrings[i];
    if (i<fsl.Count-1) then
      sRegEntry := sRegEntry + ',';
  end;

  Reg.OpenKey(RegPath,true);
  Reg.WriteString(sIMPORT,Identifier+'.FieldSelection',sRegEntry);
  Reg.WriteString(sIMPORT,Identifier+'.Delimiter',Delimiter);
  Reg.CloseKey;
end;
// ----- TGEImportFile.SetImportClipboard --------------------------------------
procedure TGEImportFile.SetImportClipboard(importer:TformGEImporter;
  var bContinue:boolean;var sl:TStringList;var clip:TStringList);

var
  i,j,nEntries : integer;
  sCaption,sItem,sLine : string;

begin
  sCaption := 'Import From Clipboard';
  try
    clip.Text := ClipBoard.AsText;
  except

  end;

  nEntries := 0;

//  search for the biggest number of elements
  for i:= 0 to clip.Count-1 do
  begin
    bContinue := true;
    sLine := clip.Strings[i];
    j:=0;
    while bContinue do
    begin
      sItem := GetNextItem(#9,sLine);
      if (sLine = '') then
      begin
        if (sItem <> '') then
          Inc(j);
        if (j>nEntries) then
          nEntries := j;
        bContinue := false;
      end else
        Inc(j);
    end;
  end;

  if (clip.Count>0) then
  begin
    bContinue := true;
    for i:=0 to nEntries-1 do
      sl.Add('Item'+IntToStr(i));

    Importer.Caption := sCaption;
    Importer.sBarImport.Panels[1].Text := 'Clipboard';
  end else
    bContinue := false;
end;
// ----- TGEImportFile.SetImportFile -------------------------------------------
procedure TGEImportFile.SetImportFile(importer:TformGEImporter;var bContinue:boolean;
  var sl:TStringList);

var
  sBiggestLine,sCaption,sFileName,sItem,sLine : string;
  id,id2:integer;
  OpenFile : TOpenDialog;

begin
  OpenFile := TOpenDialog.Create(nil);
  with OpenFile do
  begin
    Filter := 'Comma Separated Value files (*.csv)|*.csv|'+
              'Tab Separated Value files (*.tsv)|*.tsv|'+
              'Space Separated Value files (*.txt)|*.txt|'+
              'All files|*.*';

// set default file path, initial directory - check it out the validity first.
// Think the default Windows behaviour is to default to "My Documents"
// if the FileName and/or InitialDir haven't been set yet}
    if FileExists(fFilePath) then
    begin
      FileName := fFilePath;
      InitialDir := Copy(fFilePath,1,LastPos('/',fFilepath)-1);
// shouldn't happen but ...
    end else if DirectoryExists(fFilePath) then
      InitialDir := fFilePath;
      
// set captions, filter indices
    if (ImportFileType = ifCSV) then
    begin
      Title := 'Open Comma Separated Value File';
      sCaption := 'Import Comma Separated Value File';
      FilterIndex := 1;
    end else if (ImportFileType = ifTSV) then
    begin
      Title := 'Open Tab Separated Value File';
      sCaption := 'Import Tab Separated Value File';
      FilterIndex := 2;
    end else if (ImportFileType = ifSSV) then
    begin
      Title := 'Open Space Separated Value File';
      sCaption := 'Import Space Separated Value File';
      FilterIndex := 3;
    end else if (ImportFileType = ifFixedFormatSpace) then
    begin
      Title := 'Open Fixed Format (Space Separated) Value File';
      sCaption := 'Import Fixed Format (Space Separated) Value File';
      FilterIndex := 4;
    end;

    if Execute then
    begin
      sFileName := FileName;
// calculates the maximum size of the file
      AssignFile(ImportTextFile,sFileName);
      Reset(ImportTextFile);
      id := 0;
      while not eof(ImportTextFile) do
      begin
        ReadLn(ImportTextFile,sLine);
        GetMaxDelimiters(sLine,id2);
        if (id2 > id) then
        begin
          sBiggestLine := sLine;
          id := id2;
        end;
      end;
      Close(ImportTextFile);

      bContinue:=true;
      while (bContinue) do
      begin
        if (ImportFileType = ifCSV) then
          sItem := GetNextItem(',',sBiggestLine)
        else if (ImportFileType = ifTSV) then
          sItem := GetNextItem(#9,sBiggestLine)
        else if ( (ImportFileType = ifSSV) or
                  (ImportFileType = ifFixedFormatSpace)) then
          sItem := GetNextItem(' ',sBiggestLine);

// don't add a blank value at the end
        if (not bContinue) and (sItem = '') then
          continue
        else begin
          if (sItem = '') then
          begin
            bContinue := false;
            continue
          end;
          sl.Add(sItem);
        end;
      end;

      Importer.Caption := sCaption;
      Importer.sBarImport.Panels[1].Text := sFileName;
      bContinue := true;
      FilePath := FileName;
    end else
      bContinue := false;
  end;

  if bContinue then
    eFileName := OpenFile.FileName;
  OpenFile.Free;
end;

// ----- TGEImportFile.Create --------------------------------------------------
constructor TGEImportFile.Create(aOwner:TComponent);

begin
  inherited Create(aOwner);
  fReg := TRegIniFile.Create;
  fAllowFields := TStringList.Create;
  fDefaultFileType := ifCSV;
  fIdentifier := 'Untitled';
  fAllDelimiters := false;
  fGroupDelimiters := false;
end;
// ----- TGEImportFile.Destroy -------------------------------------------------
{Class destructor.}
destructor TGEImportFile.Destroy;

begin
  fReg.Free;
  fAllowFields.Free;
  inherited Destroy;
end;
// ----- TGEImportFile.Execute -------------------------------------------------
function TGEImportFile.Execute:boolean;

var
  bEntryFailed,bContinue,bIgnoreAll,bSkipLine : boolean;
  currentrecord,i,iIndex,j,k : integer;
  sDateFormat,sFieldName,sValue : string;
  thevalues : array of variant;
  cols,clip,sl,temp : TStringList;  // cols = not ignore columns

begin
(*
  result := false;
  sDateFormat := ShortDateFormat; //preserve this

  sl := TStringList.Create;
  importer := TformGEImporter.Create(self);
  importer.bEditing := false;

// Step 1. Obtain the 'size' of the import data - i.e the number of columns
  if (ImportFileType = ifClipboard) then
  begin
    clip := TStringList.Create;
    SetImportClipboard(importer,bContinue,sl,clip);
    if (not bContinue) then
      clip.Free;
  end else
// Importing a text file - CSV,TSV,SSV,Fixed Format
    SetImportFile(importer,bContinue,sl);

// terminate if required
  if (not bContinue) then
  begin
    sl.Free;
    importer.Release;
    exit;
  end;
// terminate if there are no valid columns
  if (sl.count = 0) then
  begin
    MessageDlg('Data file does not contain any columns in the specified format!',
      mtInformation, [mbOK], 0);
    sl.free;
    importer.Release;
    exit;
  end;

// Step 2. - generate a list of assignable field names based on the structure
//of the destination memory table}
  GenerateAllowFields;
  LoadRegistrySettings(Importer.fsl);

// Step 3 - prepare the importing form
  with Importer do
  begin
    allowsl.Clear;
    allowsl.Assign(AllowFields);
    SetupFieldDefs(sl); {** assigns fields to data}
    SetHeaderSkip(HeaderSkip);
    SetDelimiters(Delimiter,AllDelimiters,GroupDelimiters);
  end;

// Step 4 - prepare additional importing information
  if (ImportFileType = ifClipboard) then
  begin
    ImportDataFromClipboard(clip);
    clip.Free;
  end else
// file based - e.g. CSV,TSV,SSV,Fixed Format
    ImportDataFromFile(0,false);
  sl.Free;

// Step 5. Continue Importing until we are done!
  bContinue := true;
  while bContinue do
  begin
    with Importer do
    begin
      if (ShowModal = mrOK) then
      begin
        ShortDateFormat := ebDateFormat.Text;  {** local date format}
        result := true;

        Screen.Cursor := crHourGlass;
        SaveRegistrySettings(fsl);
        SaveIdentifierSettings;

// prepare the destination - empty if required

        with (Destination) do
        begin
          DisableControls;
          Edit;
          if cbxOverWrite.Checked then
            EmptyTable;
          Active := true;
          Last;
          currentrecord := RecNo+1; // have to remember to add one
        end;

// prepare source table (in the importer)}

        with kbmImport do
        begin
          Active := true;
          First;
          DisableControls;
        end;

        SetLength(thevalues,kbmImport.FieldCount);

//check to ensure we're not ignoring everything}
        bIgnoreAll:=true;
        CheckAllIgnore(bIgnoreAll,fsl);

        if (not bIgnoreAll) then
        begin

          cols := TSTringList.Create;
{ store the non ignore column indices in cols}

          for j:=0 to kbmImport.FieldCount-1 do
          begin
            if (fsl.Strings[j] = sIGNORE) then
              thevalues[j] := sIGNORE
            else
              cols.Add(IntToStr(j));
          end;

          for i :=0 to kbmImport.RecordCount-1 do
          begin
            bSkipLine := false;
            if (i < HeaderSkip) then
            begin
              kbmImport.Delete;
              Continue;
            end;

            for k := 0 to cols.Count-1 do
            begin
              j := StrToInt(cols.Strings[k]);
// nil values
              sValue := Trim(kbmImport.Fields[j].AsString);
              if (sValue = '') then
                thevalues[j] := null
              else
// non-nil values
              begin
                CheckStripDelimiter(sValue);
                thevalues[j] := sValue;
                if (thevalues[j] = null) then
                  bSkipLine:=true;
                if bSkipLine then
                  break;
              end; {if sValue = ''}
            end; {for k := 0 to cols.Count-1};

            if bSkipLine then
            begin
              kbmImport.Next;
              continue;
            end;

// write to the destination - could be subroutined - pass "thevalues"
            with Destination do
            begin
              Edit;
              Append;
              bEntryFailed := false;
// loop over desination fields
              for j:=0 to FieldCount-1 do
              begin
                Edit;
                sFieldName := Fields[j].FieldName;
                iIndex := fsl.IndexOf(sFieldName);
                if ((iIndex = -1) or (thevalues[iIndex] = null)) then
                  Fields[j].Clear
                else
                begin
                  if (Fields[j] is TFloatField) then
                  begin
                    try
                      Fields[j].AsFloat := thevalues[iIndex]
                    except
                      bEntryFailed := true;
                    end;
                  end else if (Fields[j] is TStringField) then
                    Fields[j].AsString := thevalues[iIndex]
                  else if ( (Fields[j] is TIntegerField) or
                            (Fields[j] is TSmallIntField) or
                            (Fields[j] is TWordField)) then
                  begin
                    try
                      Fields[j].AsInteger := thevalues[iIndex]
                    except
                      bEntryFailed := true;
                    end;
                  end else if ( (Fields[j] is TDateField) or
                            (Fields[j] is TDateTimeField)) then
                  begin
                    try
                      Fields[j].AsDateTime := StrToDateTime(thevalues[iIndex]);
                    except
                      bEntryFailed := true;
                    end;
                  end else if (Fields[j] is TMemoField) then
                  begin
// memo -> string conversion
                    temp := TStringList.Create;
                    temp.Add(thevalues[iIndex]);
                    Fields[j].Assign(temp);
                    temp.Free;
                  end;
                end;

                if bEntryFailed then
                  break;
              end; {fieldcount}

              if bEntryFailed then
              begin
                Cancel;
                kbmImport.Next;
              end else
              begin
                try
                  Post;
                  kbmImport.Delete;
                except
                  Cancel;
                  kbmImport.Next;
                end;
              end;
            end; // with Destination
          end; // for i:=0 to kbmImport.RecordCount}

          cols.Free;

          kbmImport.EnableControls;

// jump to the start of the imported data
          with Destination do
          begin
            MoveBy(currentrecord - RecNo);
            EnableControls;
          end;

// records left - failed the importing
          if (kbmImport.RecordCount > 0) then
          begin
            Importer.Caption := 'Errors detected! Displaying rejected records.';
            kbmImport.First;
          end else
            bContinue := false;
        end else
          bContinue := false;

      end else
        bContinue := false; //modal cancel

      Screen.Cursor := crDefault;
    end; // with Importer
  end; // while bContinue


  Importer.Release;
  ShortDateFormat := sDateFormat;
  DoOnAfterImport;
*)
end;

// ----- TGEImportFile.ImportDataFromClipboard ---------------------------------
procedure TGEImportFile.ImportDataFromClipboard(clip:TStringList);

const
  sDELIMITER = #9;

var
  i:integer;
  sLine:string;

begin
  importer.PrepImportTable;

  for i := 0 to clip.Count-1 do
  begin
    sLine:= clip.Strings[i];
    ///importer.kbmImport.Append;
    WriteTableLine(sDELIMITER,sLine);
    ///importer.kbmImport.Post;
  end;

  with importer do
  begin
    ///kbmImport.First;
    ///kbmImport.EnableControls;
    DisplayRecCount;
  end;
end;
// ----- TGEImportFile.ImportDataFromFile --------------------------------------
function TGEImportFile.ImportDataFromFile(iHeader:integer;bPrompt:boolean):boolean;
const
  sPROMPT = 'Replacing existing data by import?';

var
  bCancelled: boolean;
  sLine: string;
  i: integer;

begin
(*
  screen.Cursor := crHourGlass;
  result := true;
{ control whether we want a prompt}
  if bprompt then
    bCancelled := (MessageDlg(sPROMPT,mtConfirmation,[mbOK,mbCancel],0) =mrCancel)
  else
    bCancelled := false;

  if (not bCancelled) then
  begin
    AssignFile(ImportTextFile,eFileName);
    Reset(ImportTextFile);
    Importer.PrepImportTable;

{ skips these lines (useful if the file has a header)}
    if (iheader > 0) then
      for i:=0 to iheader-1 do
        ReadLn(ImportTextFile,sLine);

{ now reads the file}
    while not eof(ImportTextFile) do
    begin
      begin
        ReadLn(ImportTextFile,sLine);
        importer.kbmImport.Append;
        if (ImportFileType = ifCSV) then
          WriteTableLine(',',sLine)
        else if (ImportFileType = ifTSV) then
          WriteTableLine(#9,sLine)
        else if (
          (ImportFileType = ifSSV) or
          (ImportFileType = ifFixedFormatSpace)) then
          WriteTableLine(' ',sLine);
        importer.kbmImport.Post;
      end;
    end;

    with importer do
    begin
      kbmImport.First;
      kbmImport.EnableControls;
      DisplayRecCount;
    end;

    CloseFile(ImportTextFile);
  end
  else
    result:=false;
  Screen.Cursor:=crDefault;
*)
end;
// ----- TGEImportFile.LoadRegistrySettings ------------------------------------
procedure TGEImportFile.LoadRegistrySettings(fsl:TStringList);

const
  iDELIMITER = 1;
  sDELIMITER = ',';

var
  s1,s2,sStored:string;

begin
  fsl.Clear;

  Reg.OpenKey(RegPath,true);
  sStored :=Reg.ReadString(sIMPORT,Identifier+'.FieldSelection','');
  fDelimiter := Reg.ReadString(sIMPORT,Identifier+'.Delimiter','');
  fAllDelimiters := Reg.ReadBool(sIMPORT,Identifier+'.AllDelimiters',false);
  fGroupDelimiters:=Reg.ReadBool(sIMPORT,Identifier+'.GroupDelimiters',false);
  Reg.CloseKey;

//  note the stored strings format: fieldname1,fieldname2,fieldname3,...
  if (sStored <> '') then
  begin
    s1:=sStored;
    while (s1 <> '') do
    begin
      if (Pos(sDelimiter,s1) > 0) then
      begin
        s2 := Copy(s1,1,Pos(sDelimiter,s1)-1);
        s1 := Copy(s1,Pos(sDelimiter,s1)+iDELIMITER,Length(s1));
      end else
      begin
        s2 := s1;
        s1 := '';
      end;
      fsl.Add(s2);
    end;
  end;
end;

// ----- Register --------------------------------------------------------------
procedure Register;

begin
  RegisterComponents('Graphic Edge IO',[TGEImportFile]);
end;


// =============================================================================
end.




