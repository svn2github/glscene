unit Commasplit;

interface

uses
  System.Sysutils,
  System.Classes,
  Vcl.Controls,
  Graphics;

type
 TCommaSplitter = class(TComponent)
  private
    fText: String;
    fItems: TStringList;
    fDelimiter: String;
    function GetText: String;
    procedure SetItems(Value: TStringList);
    procedure SetText(Value: String);
     
  protected
    procedure Loaded; override;
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IsIn(S: String): Boolean;
     
  published
    property Delimiter: string read fDelimiter write fDelimiter;
    property Items: TStringList read fItems write SetItems;
    property Text: string read GetText write SetText;
     
  end;

procedure Register;

implementation

//------ TCommaSplitter.GetText ------------------------------------------------
function TCommaSplitter.GetText: String;

var
  i: integer;

begin
  Result := '';
  for i := 0 to fItems.Count - 1 do
    Result := Format('%s%s%s',[Result,Delimiter,fItems.Strings[i]]);
  Result := Copy(Result,Length(Delimiter)+1,Length(Result));
end;
//------ TCommaSplitter.SetItems -----------------------------------------------
procedure TCommaSplitter.SetItems(Value:TStringList);
begin
 fItems.Assign(Value);
end;
//------ TCommaSplitter.SetText ------------------------------------------------
procedure TCommaSplitter.SetText(Value: String);

var
 s1,s2: string;

begin
 fText := Value;
 s1 := Value;
 fItems.Clear;
 while (s1 <> '') do
 begin
   if (Pos(Delimiter,s1) > 0) then
   begin
     s2 := Copy(s1,1,Pos(Delimiter,s1)-1);
     s1 := Copy(s1,Pos(Delimiter,s1)+Length(Delimiter),Length(s1));
   end else
   begin
     s2 := s1;
     s1 := '';
   end;
   fItems.Add(s2);
 end;
end;
//------ TCommaSplitter.Loaded -------------------------------------------------
procedure TCommaSplitter.Loaded;
begin
 inherited Loaded;
{** default to comma seperated value}
 if (Delimiter = '') then
   Delimiter := ',';
end;
//------ TCommaSplitter.Create -------------------------------------------------
constructor TCommaSplitter.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 fItems := TStringList.Create;
 fDelimiter := ',';
end;
//------ TCommaSplitter.Destroy ------------------------------------------------
destructor TCommaSplitter.Destroy;
begin
  fItems.Free;
  inherited Destroy;
end;
//------ TCommaSplitter.IsIn ---------------------------------------------------
function TCommaSplitter.IsIn(S: String): Boolean;

var
 i: integer;

begin
 result:=false;
 for i := 0 to fItems.Count - 1 do
 begin
   if (fItems.Strings[i] = S) then
   begin
     result := true;
     exit;
   end;
 end;
end;

//------ Register --------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('PB Power', [TCommaSplitter]);
end;

end.






















