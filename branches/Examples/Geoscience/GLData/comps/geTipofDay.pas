unit geTipofDay;

interface

uses
  classes,controls,dialogs,forms,graphics,messages,registry,stdctrls,
  sysutils, windows, frmTip;

const
  sTIPOFDAY = 'TipOfDay';
  sSHOWATSTART = 'ShowAtStart';
  sLASTTIP = 'LastTip';

type
  TGETipOfDay = class(TComponent)
  private
    fDisableShowOnStart : boolean;
    fLastTip : integer;
    fReg : TRegIniFile;
    fRegPath : string;
    fRelativeTipFile:string;
    fShowAtStart : boolean;
    fTips: TStringList;
    procedure SetLastTip(Value:integer);
    procedure SetShowAtStart(Value:boolean);
    procedure SetTips(Value: TStringList);
  protected
    procedure Loaded;override;
    procedure LoadFromRegistry;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: boolean;
    property LastTip : integer read fLastTip write SetLastTip;
    property Reg: TRegIniFile read fReg write fReg;
  published
    property DisableShowAtStart : boolean read fDisableShowOnStart write fDisableShowOnStart;
    property ShowAtStart: boolean read FShowAtStart write SetShowAtStart;
    property RelativeTipFile:string read fRelativeTipFile write fRelativeTipFile;
    property RegPath:string read fRegPath write fRegPath;
    property Tips: TStringList read FTips write SetTips;
  end;

procedure Register;

implementation

// ------ TGETipOfDay.Loaded ---------------------------------------------------
procedure TGETipOfDay.Loaded;

var
  sTipFile:string;

begin
  inherited Loaded;
  if not (csDesigning in ComponentState) then
  begin
    LoadFromRegistry;
    if (fRelativeTipFile <> '') then
    begin
      sTipFile := ExtractFilePath(Application.ExeName)+fRelativeTipFile;
      if FileExists(sTipFile) then
        Tips.LoadFromFile(sTipFile);
    end;
  end;
end;

// ------ TGETipOfDay.Create ---------------------------------------------------
constructor TGETipOfDay.Create(AOwner: TComponent);

begin
  inherited Create(AOwner);
  fReg := TRegIniFile.Create;
  fTips := TStringList.Create;
  fRelativeTipFile := '';
end;

// ------ TGETipOfDay.Destroy --------------------------------------------------
destructor TGETipOfDay.Destroy;

begin
  fReg.Free;
  fTips.Free;
  inherited Destroy;
end;

// ------ TGETipOfDay.LoadFromRegistry -----------------------------------------
procedure TGETipOfDay.LoadFromRegistry;

begin
  Reg.OpenKey(RegPath,true);
  fShowAtStart := Reg.ReadBool(STIPOFDAY,SSHOWATSTART,true);
  fLastTip := Reg.ReadInteger(STIPOFDAY,SLASTTIP,-1); // default = first tip;
  Reg.CloseKey;
end;

// ------ TGETipOfDay.Execute --------------------------------------------------
function TGETipOfDay.Execute: boolean;

var
  GETipDialog: TformGETip;

begin
  LoadFromRegistry;
  GETipDialog := TFormGETip.Create(nil);

  GETipDialog.cbxShowTips.Enabled := not DisableShowatStart;
  if DisableShowAtStart then
    GETipDialog.cbxShowTips.Checked := false;

  result := true;
  with GETipDialog do
  begin
    FormTips.Assign(FTips);
    cbxShowTips.Checked := FShowAtStart;
    NumTip := fLastTip+1; {** increment this for the next tip}
    try
      ShowModal;
      ShowAtStart := cbxShowTips.CHecked;
      LastTip := NumTip;
    finally
      Release;
    end;
  end;
end;

// ------ TGETipOfDay.SetLastTip -----------------------------------------------
procedure TGETipOfDay.SetLastTip(value:integer);

begin
  if (Value <> fLastTip) then
  begin
    fLastTip := value;
    Reg.OpenKey(RegPath,true);
    Reg.WriteInteger(STIPOFDAY,SLASTTIP,value);
    Reg.CloseKey;
  end;
end;

// ------ TGETipOfDay.SetShowAtStart -------------------------------------------
procedure TGETipOfDay.SetShowAtStart(value:boolean);

begin
  fShowAtStart := Value;
  Reg.OpenKey(RegPath,true);
  Reg.WriteBool(STIPOFDAY,SSHOWATSTART,value);
  Reg.CloseKey;
end;

// ------ TGETipOfDay.SetTips --------------------------------------------------
procedure TGETipOfDay.SetTips(Value: TStringList);
begin
  if Value <> FTips then
    FTips.Assign(Value);
end;

// ------ Register -------------------------------------------------------------
procedure Register;

begin
  RegisterComponents('Graphic Edge IO', [TGETipOfDay]);
end;

end.
