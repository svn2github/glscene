unit FDXPCompileLog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, DXPExpertModule;

type
  TDXPCompileLog = class(TForm)
    PageControl: TPageControl;
    TSCooked: TTabSheet;
    TSRaw: TTabSheet;
    MERaw: TMemo;
    LVMessages: TListView;
    TSConfigFile: TTabSheet;
    MECfgFile: TMemo;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure LVMessagesDblClick(Sender: TObject);
  private
    { Déclarations privées }
    procedure AddMessage(const module, msg : String;
                         lineNumber : Integer);
  public
    { Déclarations publiques }
    DXPExpertModule : TDMDXPExpertModule;
    procedure ExecuteOnFPC(const prjFileName, logFileName : String;
                           expertModule : TDMDXPExpertModule);
  end;

function DXPCompileLog : TDXPCompileLog;
procedure ReleaseDXPCompileLog;
function DXPCompileLogVisible : Boolean;

implementation

{$R *.dfm}

uses ToolsAPI, DXPGlobals;

var
   vDXPCompileLog : TDXPCompileLog;

// DXPCompileLog
//
function DXPCompileLog : TDXPCompileLog;
begin
   if vDXPCompileLog=nil then
      vDXPCompileLog:=TDXPCompileLog.Create(nil);
   Result:=vDXPCompileLog;
end;

// ReleaseDXPCompileLog
//
procedure ReleaseDXPCompileLog;
begin
   if Assigned(vDXPCompileLog) then begin
      if vDXPCompileLog.Visible then
         vDXPCompileLog.Close
      else vDXPCompileLog.Free;
      vDXPCompileLog:=nil;
   end;
end;

// DXPCompileLogVisible
//
function DXPCompileLogVisible : Boolean;
begin
   if Assigned(vDXPCompileLog) then
      Result:=vDXPCompileLog.Visible
   else Result:=False;
end;

// FormClose
//
procedure TDXPCompileLog.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   vDXPCompileLog:=nil;
   Action:=caFree;   
end;

// AddMessage
//
procedure TDXPCompileLog.AddMessage(const module, msg : String;
                                    lineNumber : Integer);
begin
   with LVMessages.Items.Add do begin
      Caption:=module;
      SubItems.Add(IntToStr(lineNumber));
      SubItems.Add(msg);
   end;
end;

// ExecuteOnFPC
//
procedure TDXPCompileLog.ExecuteOnFPC(const prjFileName, logFileName : String;
                                      expertModule : TDMDXPExpertModule);
var
   i, pOpen, pClose, pComma, pDots, colNb, lineNb : Integer;
   line, fName, msgText, location, msgType : String;
   msgKind : TOTAMessageKind;
   msgServices : IOTAMessageServices;
   lineRef : Pointer;
begin
   DXPExpertModule:=expertModule;
   msgServices:=(BorlandIDEServices as IOTAMessageServices);
   msgServices.ClearToolMessages;
   msgServices.ClearCompilerMessages;
   LVMessages.Clear;
   if FileExists(logFileName) then begin
      MERaw.Lines.LoadFromFile(logFileName);
   end else begin
      MERaw.Lines.Text:='No compiler error output.';
      AddMessage('', MERaw.Lines.Text, 0);
      msgServices.AddCompilerMessage(prjFileName, 'No compiler output', 'FPC',
                                     otamkFatal, 0, 0, nil, lineRef);
   end;
   if FileExists(vFPC_BinaryPath+'\fpc.cfg') then
      MECfgFile.Lines.LoadFromFile(vFPC_BinaryPath+'\fpc.cfg')
   else MECfgFile.Clear;
   MERaw.Lines.Insert(0, DateTimeToStr(Now));
   // parse
   for i:=0 to MERaw.Lines.Count-1 do begin
      line:=MERaw.Lines[i];
      pOpen:=Pos('(', line);
      pClose:=Pos(') ', line);
      pComma:=Pos(',', line);
      if (pOpen>0) and (pClose>0) and (pComma>pOpen) and (pComma<pClose) then begin
         fName:=Copy(line, 1, pOpen-1);
         location:=Copy(line, pOpen+1, pClose-pOpen-1);
         msgText:=Copy(line, pClose+2, MaxInt);

         pDots:=Pos(':', msgText);
         msgType:=LowerCase(Copy(msgText, 1, pDots-1));

         if msgType='hint' then msgKind:=otamkHint
         else if msgType='warn' then msgKind:=otamkWarn
         else if msgType='error' then msgKind:=otamkError
         else if msgType='fatal' then msgKind:=otamkFatal
         else msgKind:=otamkInfo;

         pComma:=Pos(',', location);
         lineNb:=StrToIntDef(Copy(location, 1, pComma-1), 1);
         colNb:=StrToIntDef(Copy(location, pComma+1, MaxInt), 1);

         with LVMessages.Items.Add do begin
            Caption:=fName;
            SubItems.Add(location);
            SubItems.Add(msgText);
         end;
         msgServices.AddCompilerMessage(fName, msgText, 'FPC',
                                        msgKind, lineNb, colNb, nil, lineRef);
      end else if CompareText(Copy(line, 1, 6), 'Fatal:')=0 then begin
         msgServices.AddCompilerMessage(prjFileName, line, 'FPC',
                                        otamkFatal, 0, 0, nil, lineRef);
      end;
   end;
   msgServices.ShowMessageView(nil);
   if vFPC_ShowCompileLog then
      Show;
end;

procedure TDXPCompileLog.LVMessagesDblClick(Sender: TObject);
var
   fName, buf : String;
   p, col, line : Integer;
begin
   if LVMessages.Selected<>nil then begin
      fName:=LVMessages.Selected.Caption;
      buf:=LVMessages.Selected.SubItems[0];
      p:=Pos(',', buf);
      line:=StrToIntDef(Copy(buf, 1, p-1), 1);
      col:=StrToIntDef(Copy(buf, p+1, MaxInt), 1);
      DXPExpertModule.WarpTo(fName, col, line);
   end;
end;

end.
