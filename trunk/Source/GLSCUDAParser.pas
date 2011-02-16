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

  TCUDAType =
    (
    char1,
    uchar1,
    char2,
    uchar2,
    char3,
    uchar3,
    char4,
    uchar4,
    short1,
    ushort1,
    short2,
    ushort2,
    short3,
    ushort3,
    short4,
    ushort4,
    int1,
    uint1,
    int2,
    uint2,
    int3,
    uint3,
    int4,
    uint4,
    long1,
    ulong1,
    long2,
    ulong2,
    long3,
    ulong3,
    long4,
    ulong4,
    float1,
    float2,
    float3,
    float4,
    longlong1,
    ulonglong1,
    longlong2,
    ulonglong2,
    longlong3,
    ulonglong3,
    longlong4,
    ulonglong4,
    double1,
    double2,
    double3,
    double4
    );

  TCUDATexRefInfo = record
    Name: string;
    DataType: TCUDAType;
    Channels: Byte;
  end;

  TCUDAArgType = (cuatDefalt, cuatConst, cuatVar, cuatRef);

  TCUDAFuncArgInfo = record
    Name: string;
    DataType: TCUDAType;
    ArgType: TCUDAArgType;
  end;

  TCUDAFuncInfo = record
    Name: string;
    Return: TCUDAFuncArgInfo;
    Args: array of TCUDAFuncArgInfo;
  end;

  TCUDAConstInfo = record
    Name: string;
    DataType: TCUDAType;
  end;

  TCUDAModuleInfo = {$IFNDEF FPC}record{$ELSE}object{$ENDIF}
    TexRef: array of TCUDATexRefInfo;
    Func: array of TCUDAFuncInfo;
    ConstMem: array of TCUDAConstInfo;
    procedure Reset;
    procedure ParseModule(ASource: TStrings);
  end;

procedure BreakString(const str: string; outlist: TStrings);

implementation

uses
  GLStrings;

procedure BreakStrings(inlist, outlist: TStrings);
const
  WordDelimiters: set of Char = [#0..#255] - ['a'..'z','A'..'Z','1'..'9','0'];
var
  i: Integer;
  str, accum: string;
  next: Boolean;
begin
  str := inlist.Text;
  outlist.Clear;
  accum := '';
  next := false;

  for i := 1 to Length(str) do
  begin
    if (str[i] in WordDelimiters) and (Length(accum) > 0) then
    begin
      outlist.Add(accum);
      accum := '';
    end
    else
      accum := accum + str[i];
  end;
end;

procedure TCUDAModuleInfo.Reset;
var
  i: Integer;
begin
  TexRef := nil;
  Func := nil;
  ConstMem := nil;
end;

procedure TCUDAModuleInfo.ParseModule(ASource: TStrings);
type
  TLastSemantic = (lsNone, lsSampler, lsCode);
var
  I: Integer;
  line: TStrings;
  temp: string;
  LastSemantic: TLastSemantic;
begin
  Reset;
  LastSemantic := lsNone;
  line := TStringList.Create;
  for I := 0 to ASource.Count - 1 do
  begin

  end;

  line.Free;
end;

end.

