//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLSCUDAParser <p>

   Helper unit for parsing PTX or CUBIN and get information about.<p>
   kernel's functions and texture.<p>

   <b>History : </b><font size=-1><ul>
      <li>19/03/10 - Yar - Creation
   </ul></font><p>
}
unit GLSCUDAParser;

interface

uses
  Classes;

const
  cModuleInfoSize = 256;

type

  TCUDAPtxNumType = (fpt_U32, fpt_S32, fpt_F32);

  TCUDATexRefInfo = record
    Name: string;
    TType: TCUDAPtxNumType;
  end;

  TCUDAFuncInfo = record
    Name: string;
    ParamCounter: Integer;
    Param: array[0..cModuleInfoSize-1] of TCUDAPtxNumType;
  end;

  TCUDAModuleInfo = record
    bPtx: Boolean;
    bCubin: Boolean;
    TexRefCounter: Integer;
    TexRef: array[0..cModuleInfoSize-1] of TCUDATexRefInfo;
    FuncCounter: Integer;
    Func: array[0..cModuleInfoSize-1] of TCUDAFuncInfo;
  end;

procedure ParseModule(var Info: TCUDAModuleInfo; source: TStrings);

implementation

uses
  GLStrings;

procedure BreakString(const str: string; outlist: TStrings);
var
  i: Integer;
  accum: string;
  next: Boolean;
begin
  outlist.Clear;
  accum := '';
  next := false;
  for i := 1 to Length(str) do
  begin
    if Byte(str[i])<33 then
    begin
      if next and (Length(accum)>0) then
      begin
        outlist.Add(accum);
        accum := '';
        next := false;
      end;
    end
    else begin
      accum := accum + str[i];
      next := true;
    end;
  end;
  if next and (Length(accum)>0) then
    outlist.Add(accum);
end;

procedure ResetInfo(var Info: TCUDAModuleInfo);
var
  i: Integer;
begin
  Info.bPtx := false;
  Info.bCubin := false;
  Info.TexRefCounter := 0;
  Info.FuncCounter := 0;
  for i := 0 to cModuleInfoSize-1  do
    Info.Func[i].ParamCounter := 0;
end;

function GetPtxType(str: string): TCUDAPtxNumType;
begin
  if str = '.u32' then
    Result := fpt_U32
  else if str = '.s32' then
    Result := fpt_S32
  else if str = '.f32' then
    Result := fpt_F32
  else
  begin
    Assert(False, glsErrorEx + glsUnknownType);
    Result := fpt_U32;
  end;
end;

procedure ParseModule(var Info: TCUDAModuleInfo; source: TStrings);
type
  TLastSemantic = (lsNone, lsSampler, lsCode);
var
  i, j: Integer;
  line: TStrings;
  temp: string;
  LastSemantic:  TLastSemantic;
begin
  ResetInfo(Info);
  LastSemantic := lsNone;
  line := TStringList.Create;
  for i := 0 to source.Count-1 do
  begin
    BreakString(source.Strings[i], line);
    if line.Count=0 then
      continue;
    if line.Strings[0]='.version' then
    begin
      Info.bPtx := true;
    end
    else if line.Strings[0]='.tex' then
    begin
      temp := line.Strings[2];
      System.Delete(temp, Length(temp), 1);
      Info.TexRef[Info.TexRefCounter].Name := temp;
      Info.TexRef[Info.TexRefCounter].TType :=
        GetPtxType(line.Strings[1]);
      Inc(Info.TexRefCounter);
    end
    else if line.Strings[0]='.entry' then
    begin
      Info.Func[Info.FuncCounter].Name := line.Strings[1];
      Inc(Info.FuncCounter);
    end
    else if line.Strings[0]='.param' then
    begin
      j := Info.FuncCounter-1;
      Info.Func[j].Param[Info.Func[j].ParamCounter] :=
        GetPtxType(line.Strings[1]);
      Inc(Info.Func[j].ParamCounter);
    end
    else if line.Strings[0]='modname' then
    begin
      Info.bCubin := true;
    end
    else if (line.Strings[0]='sampler') and Info.bCubin then
    begin
      LastSemantic := lsSampler;
    end
    else if (line.Strings[0]='code') and Info.bCubin then
    begin
      LastSemantic := lsCode;
    end
    else if line.Strings[0]='name' then
    begin
      case LastSemantic of
        lsSampler:
        begin
          Info.TexRef[Info.TexRefCounter].Name := line.Strings[2];
          Inc(Info.TexRefCounter);
        end;
        lsCode:
        begin
          Info.Func[Info.FuncCounter].Name := line.Strings[2];
          Inc(Info.FuncCounter);
        end;
      end;
      LastSemantic := lsNone;
    end
  end;

  line.Free;
end;

end.
