{ 
  Q3MD3 - Helper classes and methods for Quake3 MD3 actors
  
  History :
    01/04/03 - Mrqzzz -  "LEGS_" animations read from .CFG fixed
    17/02/03 - SG - Creation
}

unit Q3MD3;

interface

uses
  Classes,SysUtils,ApplicationFileIO,Geometry,GLVectorFileObjects,
  VectorLists,FileMD3;

type
  // This class is used to extract the tag transform information
  // stored in the MD3 files. The data is used to offset each
  // part of the model based on the parent parts animation state.
  TMD3TagList = class
    private
      FTags : array of TMD3Tag;
      FNumTags,
      FNumFrames : Integer;
    function GetTag(index: integer): TMD3Tag;
    public
      procedure LoadFromFile(FileName:String);
      procedure LoadFromStream(AStream:TStream);
      function GetTransform(TagName:string; Frame:integer):TMatrix;
      property TagCount : integer read FNumTags;
      property FrameCount : integer read FNumFrames;
      property Tags[index:integer]:TMD3Tag read GetTag;
  end;

// These procedures are helpers to load the Quake3 animation file data
// into an animation list. The NamePrefix parameter is used to determine
// which class of animation is extracted. eg NamePrefix='TORSO' will load
// all animations starting with 'TORSO_' like 'TORSO_STAND'
procedure LoadQ3Anims(Animations:TActorAnimations;
            FileName:string; NamePrefix:string); overload;
procedure LoadQ3Anims(Animations:TActorAnimations;
            Strings:TStrings; NamePrefix:string); overload;

implementation

// LoadQ3Anims
//
procedure LoadQ3Anims(Animations:TActorAnimations;
            FileName:string; NamePrefix:string);
var
  AnimStrings:TStrings;
begin
  AnimStrings:=TStringList.Create;
  AnimStrings.LoadFromFile(FileName);
  LoadQ3Anims(Animations,AnimStrings,NamePrefix);
  AnimStrings.Free;
end;

procedure LoadQ3Anims(Animations:TActorAnimations;
            Strings:TStrings; NamePrefix:string);
var
  anim :TStringList;
  val : array[0..3] of integer;
  strindex,valindex,i : integer;
  GotValues:Boolean;
  commatext,str1 : string;
  TorsoStartFrame,LegsStartFrame : integer; // Used to Fix LEGS Frame values red from CFG file

  function StrIsNumber(str:string):boolean;
  var
    i : integer;
  begin
    result:=false;
    for i:=1 to Length(str) do
      if (Ord(str[i])>=Ord('0'))
      and (Ord(str[i])<=Ord('9')) then
        result:=true
      else begin
        result:=false;
        break;
      end;
  end;

begin
  anim:=TStringList.Create;
  TorsoStartFrame := 0;
  LegsStartFrame  := 0;
  for strindex:=0 to Strings.Count-1 do begin
    commatext:=Strings.Strings[strindex];
    while Pos('  ',commatext)>0 do
      commatext:=StringReplace(commatext,'  ',' ',[rfReplaceAll]);
    commatext:=StringReplace(commatext,' ',',',[rfReplaceAll]);
    anim.CommaText:=commatext;
    GotValues:=False;
    valindex:=0;
    str1:='';
    if anim.Count>=5 then begin
      for i:=0 to Anim.Count-1 do begin
        if GotValues then begin

          // Store start values to Fix LEGS
          if (TorsoStartFrame=0) and (pos('TORSO_',Uppercase(Anim.Strings[i]))>0) then
               TorsoStartFrame := val[0];
          if (LegsStartFrame=0) and (pos('LEGS_',Uppercase(Anim.Strings[i]))>0) then
               LegsStartFrame := val[0];

          if (Anim.Strings[i]<>'//')
          and (Pos(NamePrefix+'_',Anim.Strings[i])>0) then begin
            str1:=StringReplace(Anim.Strings[i],'//','',[rfReplaceAll]);
            break;
          end;
        end else begin
          if StrIsNumber(Anim.Strings[i]) then begin
            val[valindex]:=StrToInt(Anim.Strings[i]);
            Inc(valindex);
            if valindex=4 then GotValues:=True;
          end else break;
        end;
      end;
    end;
    if GotValues and (str1<>'') then begin
      // Values ready for new animation.
      with Animations.Add do begin
        // Fix frame value for Legs
        if Uppercase(NamePrefix)='LEGS' then
             val[0] := val[0]-LegsStartFrame+TorsoStartFrame;
             
        Name:=str1;
        StartFrame:=val[0];
        EndFrame:=val[0]+val[1]-1;
        Reference:=aarMorph;
        // Need a way in TActorAnimation to tell whether it is
        // a looping type animation or a play once type and
        // the framerate (interval) it uses. Both of these can
        // be determined here and loaded.
      end;
    end;
  end;
  anim.Free;
end;

// ------------------
// ------------------ TMD3TagList ------------------
// ------------------

// LoadFromFile
//
procedure TMD3TagList.LoadFromFile(FileName:String);
var
  fs : TStream;
begin
  if fileName<>'' then begin
    fs:=CreateFileStream(FileName, fmOpenRead+fmShareDenyWrite);
    try
      LoadFromStream(fs);
    finally
      fs.Free;
    end;
  end;
end;

// LoadFromStream
//
procedure TMD3TagList.LoadFromStream(aStream:TStream);
var
  MD3File     : TFileMD3;
begin
  MD3File:=TFileMD3.Create;
  try
    MD3File.LoadFromStream(aStream);
    FNumTags:=MD3File.ModelHeader.numTags;
    FNumFrames:=MD3File.ModelHeader.numFrames;
    SetLength(FTags,FNumTags*FNumFrames);
    System.Move(MD3File.Tags[0],FTags[0],FNumTags*FNumFrames*SizeOf(TMD3Tag));
  finally
    MD3File.Free;
  end;
end;

// GetTag
//
function TMD3TagList.GetTag(index: integer): TMD3Tag;
begin
  Result:=FTags[index];
end;

// GetTransform
//
function TMD3TagList.GetTransform(TagName: string;
  Frame: integer): TMatrix;
var
  TagIdx,i,j : integer;
  Tag : TMD3Tag;
begin
  Result:=IdentityHMGMatrix;
  TagIdx:=-1;
  for i:=0 to FNumTags do
    if lowercase(trim(TagName))=lowercase(trim(FTags[i].strName)) then begin
      TagIdx:=i;
      Break;
    end;
  if TagIdx=-1 then exit;
  Tag:=FTags[TagIdx+Frame*FNumTags];
  for j:=0 to 2 do
    for i:=0 to 2 do
      Result[i][j]:=Tag.rotation[i][j];
  for i:=0 to 2 do
    Result[3][i]:=Tag.vPosition[i];
end;

end.
