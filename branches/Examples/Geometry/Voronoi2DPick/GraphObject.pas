
constructor TGraphObject.Create(L:TList;C:TCanvas);
begin

  List:=L;
  if List<>nil then index:=L.Add(self);
  orig_index:=index;
  Canvas:=C;

end;

//------------------------------------------------------------------

procedure TGraphObject.MoveToList(L:TList);
begin
  if List<>nil then List.items[index]:=nil;
  index:=L.Add(self);
  List:=L;
end;

//------------------------------------------------------------------


procedure TGraphObject.CopyToList(L:TList);
begin
  L.Add(self);
end;

//------------------------------------------------------------------


function TGraphObject.CloneToList(L:TList):TGraphObject;
begin
  result:=clone;
  result.movetolist(L);

end;

//------------------------------------------------------------------

procedure TGraphObject.Delete;
var z:integer;
begin
  if List<>nil then begin
     List.delete(index);
     List.pack;
     for z:=0 to List.count-1 do TGraphObject(List.items[z]).reindex(orig);
  end;
  free;
end;

//------------------------------------------------------------------

procedure TGraphObject.ReIndex(orig:boolean); // slow reindex by searching List for "self"
begin
  index:=List.IndexOf(self);
  if index<0 then raise ERangeError.Create('An object lost its bounding to a list ! (ReIndex call failed)');
  if orig then orig_index:=index;
end;

//------------------------------------------------------------------

procedure TGraphObject.ReIndex(orig:boolean;i:integer); // fast reindex with validation
begin
  if List.items[i]<>self then raise ERangeError.Create('An object lost its bounding to a list ! (ReIndex call failed)');
  index:=i;
  if orig then orig_index:=index;
end;

//------------------------------------------------------------------

function TGraphObject.GetIndex:integer;
begin
  result:=index;
end;

//------------------------------------------------------------------

function TGraphObject.GetOrigIndex:integer;
begin
  result:=orig_index;
end;


//------------------------------------------------------------------

procedure TGraphObject.SetCanvas(c:TCanvas);
begin
  canvas:=c;
end;

//------------------------------------------------------------------

function  TGraphObject.GetCanvas:TCanvas;
begin
  result:=canvas;
end;




