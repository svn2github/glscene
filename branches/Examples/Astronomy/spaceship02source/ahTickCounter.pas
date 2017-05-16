unit ahTickCounter;

interface

uses Windows,SysUtils;

type
  tSection=record
    Time		:double;
    Count	:cardinal;
    Start	:int64;
  end;//record

  tTickCounter=class(tObject)
  private
    fSections	:array of tSection;
    function GetSections(index: integer): tSection;
  public
    ClearLog	:boolean;
    constructor Create(const aCount:integer=10);
    destructor Destroy; override;
    procedure Reset(const aSection:integer);
    procedure ResetAll;
    property Sections[index:integer]:tSection read GetSections;
    procedure Start(const aSection:integer);
    procedure Stop(const aSection:integer);
  end;

implementation

{ tTickCounter }

constructor tTickCounter.Create(const aCount: integer);
var
  i	:integer;
begin
  inherited Create;
  for i:=0 to aCount-1 do SetLength(fSections,aCount);
end;

destructor tTickCounter.Destroy;
var
  i		:integer;
  F		:TextFile;
  Name	:string;
begin
  Name:=ExtractFileName(ParamStr(0));
  Name:=ChangeFileExt(Name,'')+'-TimeLog.txt';
  AssignFile(F,Name);
  try
    if ClearLog or not FileExists(Name) then Rewrite(F) else Append(F);
    Writeln(F,'Section'#9'Count'#9'Total [s]'#9'Mean time [ms]');
    for i:=0 to High(fSections) do with fSections[i] do begin
      if Count>0 then Writeln(F,Format('%d'#9'%d'#9'%f'#9'%f',
                                [i,Count,Time/1000,Time/Count]));
    end;//for i
    Writeln(F);
  finally
    try CloseFile(F) except end;
    fSections:=nil;
    inherited;
  end;//finally
end;

function tTickCounter.GetSections(index: integer): tSection;
begin
  Result:=fSections[index];
end;

procedure tTickCounter.Reset(const aSection: integer);
begin
  with fSections[aSection] do begin
    Count:=0;
    Time:=0;
  end;//with
end;

procedure tTickCounter.ResetAll;
var
  i	:integer;
begin
  for i:=0 to High(fSections) do with fSections[i] do begin
    Count:=0;
    Time:=0;
  end;//with
end;

procedure tTickCounter.Start(const aSection: integer);
var
  t	:int64;
begin
  QueryPerformanceCounter(t);
  fSections[aSection].Start:=t;
end;

procedure tTickCounter.Stop(const aSection: integer);
var
  t,fq	:int64;
begin
  with fSections[aSection] do begin
    QueryPerformanceCounter(t);
    QueryPerformanceFrequency(fq);
    Time:=Time+(t-Start)/fq;
    Inc(Count);
  end;//with
end;

end.
