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
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure LVMessagesDblClick(Sender: TObject);
  private
    { Déclarations privées }
    procedure AddMessage(const module, msg : String;
                         lineNumber : Integer);
  public
    { Déclarations publiques }
    DXPExpertModule : TDMDXPExpertModule;
    procedure ExecuteOnFPC(const logFileName : String;
                           expertModule : TDMDXPExpertModule);
  end;

function DXPCompileLog : TDXPCompileLog;
procedure ReleaseDXPCompileLog;
function DXPCompileLogVisible : Boolean;

implementation

{$R *.dfm}

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
      vDXPCompileLog.Release;
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
procedure TDXPCompileLog.ExecuteOnFPC(const logFileName : String;
                                      expertModule : TDMDXPExpertModule);
var
   i, pOpen, pClose, pComma : Integer;
   line : String;
begin
   DXPExpertModule:=expertModule;
   LVMessages.Clear;
   if FileExists(logFileName) then begin
      MERaw.Lines.LoadFromFile(logFileName);
   end else begin
      MERaw.Lines.Text:='No compiler error output.';
      AddMessage('', MERaw.Lines.Text, 0);
   end;
   MERaw.Lines.Insert(0, DateTimeToStr(Now));
   // parse
   for i:=3 to MERaw.Lines.Count-1 do begin
      line:=MERaw.Lines[i];
      pOpen:=Pos('(', line);
      pClose:=Pos(') ', line);
      pComma:=Pos(',', line);
      if (pOpen<=0) or (pClose<=0) or (pComma<=pOpen) or (pComma>=pClose) then continue;
      with LVMessages.Items.Add do begin
         Caption:=Copy(line, 1, pOpen-1);
         SubItems.Add(Copy(line, pOpen+1, pClose-pOpen-1));
         SubItems.Add(Copy(line, pClose+2, MaxInt));
      end;
   end;
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
