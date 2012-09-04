unit jnithread;
{$ifdef fpc}
 {$mode delphi}
 {$packrecords c}
{$endif}

interface

uses jni,log, classes,sysutils;

type

   { TThreadJavaVM }
   TEnvArray = array of TJNIEnv;

   TThreadJavaVM = class(TObject)
   private
     FJavaVM: PJavaVM;
     FEnvArray: TEnvArray;
     procedure RegisterEnv(aEnv: TJNIEnv);
     procedure UnRegisterEnv(aEnv: TJNIEnv);
     function GetThreadEnv():TJNIEnv;
   public
     property JavaVM: PJavaVM read FJavaVM write FJavaVM;
     {Get current Env}
     property Env: TJNIEnv read GetThreadEnv;
     constructor Create(AJavaVM: PJavaVM);
     procedure AttachCurrentThread(Args:pointer);
     procedure DetachCurrentThread();
     {Create TJNIEnv and Get Env}
     function GetEnv(var AEnv: TJNIEnv; Version: JInt): JInt;
   end;

   { TJNIThread }

   TJNIThread = class(TThread)
   private
   protected
     procedure Execute; override;
   public
   end;

implementation

{ TJNIThread }

procedure TJNIThread.Execute;
begin
  inherited;
end;


{ TThreadJavaVM }

procedure TThreadJavaVM.RegisterEnv(aEnv: TJNIEnv);
var n : integer;
begin
  n:=  length(FEnvArray);
  setlength(FEnvArray, n+1 );
  FEnvArray[n] := aEnv;

end;

procedure TThreadJavaVM.UnRegisterEnv(aEnv: TJNIEnv);
var i,j: integer;
begin

  //ищем запись в массиве
  for i:=0 to length(FEnvArray)-1 do
  if FEnvArray[i] = aEnv then
  begin
     j := i;
     break;
  end;
  //если не найдена, выходим
  if j<=0 then exit;
  //удаляем запись структуры со здвигом
  for i:=j to length(FEnvArray)-1 do
  if i+1< length(FEnvArray)-1 then
    FEnvArray[i]:= FEnvArray[i+1];
  Setlength(FEnvArray, length(FEnvArray)-1);
  aEnv.Destroy;
  aEnv:=nil;
end;

function TThreadJavaVM.GetThreadEnv: TJNIEnv;
var i: integer;
    ThrID:integer;
begin
  result :=nil;
  ThrID := GetCurrentThreadId;
  //ищем запись в массиве
  for i:=0 to length(FEnvArray)-1 do
  if (FEnvArray[i].ThreadID=ThrID) then
  begin
     Result := FEnvArray[i];
     break;
  end;
end;

constructor TThreadJavaVM.Create(AJavaVM: PJavaVM);
begin
  FJavaVM := AJavaVM;
end;

procedure TThreadJavaVM.AttachCurrentThread(Args:pointer);
var n: integer;
    fjavaEnvRef: PJNIEnv;
    fEnv: TJNIEnv;
begin
  FJavaVM^^.AttachCurrentThread(FJavaVM,@fjavaEnvRef,Args) ;
  fEnv:= TJNIEnv.Create(fjavaEnvRef);
  fEnv.ThreadID := GetCurrentThreadID;
  RegisterEnv(fEnv);

end;

procedure TThreadJavaVM.DetachCurrentThread();
var i: integer;
    fEnvRef: TJNIEnv;
    ThrID:integer;
begin
  ThrID := GetCurrentThreadId;
  //ищем запись в массиве
  for i:=0 to length(FEnvArray)-1 do
  if (FEnvArray[i]<>nil)and (FEnvArray[i].ThreadID=ThrID) then
  begin
     fEnvRef := FEnvArray[i];
     break;
  end;
  FJavaVM^^.DetachCurrentThread(FJavaVM) ;
  UnRegisterEnv(fEnvRef);

end;

function TThreadJavaVM.GetEnv(var AEnv: TJNIEnv; Version: JInt): JInt;
var fjavaEnvRef: PJNIEnv=nil;
    fEnv: TJNIEnv;
    i:  integer;
    n : integer;
begin
  Result:= FJavaVM^^.GetEnv(FJavaVM,@fjavaEnvRef,Version);
  //надеюсь без ошибок
  if Result <> JNI_OK   then exit;
  //Ищем нету ли подобной записи
  for i:=0 to length(FEnvArray)-1 do
  if (FEnvArray[i]<>nil)and (FEnvArray[i].Env=fjavaEnvRef) then
  begin
     AEnv := FEnvArray[i];
     Exit;
  end;

  AEnv:= TJNIEnv.Create(fjavaEnvRef);
  AEnv.ThreadID := GetCurrentThreadID;
  RegisterEnv(AEnv);

end;

end.


