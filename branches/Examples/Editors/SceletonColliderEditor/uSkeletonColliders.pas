unit uSkeletonColliders;

interface

uses
  System.Classes,
  System.SysUtils,
  GLVectorGeometry,
  GLApplicationFileIO,
  GLVectorFileObjects;

const
  cSkeletonColliderID      = 'SMCF';
  cSkeletonColliderVersion = 1;

type
  TSCDParamRange = 0..2;
  TSkeletonColliderType = (sctSphere, sctCapsule, sctBox);

  TSkeletonColliderHeader = record
    ID : array[0..3] of char;
    Version,
    ColliderCount : Integer;
  end;

  TSkeletonColliderData = record
    Collider : TSkeletonColliderType;
    Bone1,
    Bone2    : Integer;
    Params   : array[TSCDParamRange] of Single;
    Matrix   : TMatrix;
  end;

  TSkeletonColliders = class;

  TSkeletonCollider = class
    private
      FData : TSkeletonColliderData;
      function GetMatrix : TMatrix;
      function GetBone1 : Integer;
      function GetBone2 : Integer;
      procedure SetMatrix(const Value : TMatrix);
      procedure SetBone1(const Value : Integer);
      procedure SetBone2(const Value : Integer);
    protected
      function GetParam(Index : TSCDParamRange) : Single;
      function GetCollider : TSkeletonColliderType;
      procedure SetParam(Index : TSCDParamRange; const Value : Single);
      procedure SetCollider(const Value : TSkeletonColliderType);
    public
      constructor Create; virtual;
      constructor CreateOwned(Owner : TSkeletonColliders);
      procedure LoadFromStream(aStream : TStream);
      procedure SaveToStream(aStream : TStream);
      property Bone1 : Integer read GetBone1 write SetBone1;
      property Bone2 : Integer read GetBone2 write SetBone2;
      property Matrix : TMatrix read GetMatrix write SetMatrix;
  end;

  TSkeletonColliderSphere = class(TSkeletonCollider)
    private
      function GetRadius : Single;
      procedure SetRadius(const Value : Single);
    public
      constructor Create; override;
      property Radius : Single read GetRadius write SetRadius;
  end;

  TSkeletonColliderCapsule = class(TSkeletonCollider)
    private
      function GetRadius : Single;
      function GetHeight : Single;
      procedure SetRadius(const Value : Single);
      procedure SetHeight(const Value : Single);
    public
      constructor Create; override;
      property Radius : Single read GetRadius write SetRadius;
      property Height : Single read GetHeight write SetHeight;
  end;

  TSkeletonColliderBox = class(TSkeletonCollider)
    private
      function GetWidth : Single;
      function GetHeight : Single;
      function GetDepth : Single;
      procedure SetWidth(const Value : Single);
      procedure SetHeight(const Value : Single);
      procedure SetDepth(const Value : Single);
    public
      constructor Create; override;
      property Width : Single read GetWidth write SetWidth;
      property Height : Single read GetHeight write SetHeight;
      property Depth : Single read GetDepth write SetDepth;
  end;

  TSkeletonColliders = class (TList)
    private
      FHeader   : TSkeletonColliderHeader;
      FSkeleton : TGLSkeleton;
      function GetItem(Index : Integer) : TSkeletonCollider;
    public
      constructor Create;
      function Remove(item: TSkeletonCollider) : Integer;
      procedure Delete(index : Integer);

      procedure SaveToStream(aStream : TStream);
      procedure LoadFromStream(aStream : TStream);
      procedure SaveToFile(FileName : String);
      procedure LoadFromFile(FileName : String);

      property Skeleton : TGLSkeleton read FSkeleton write FSkeleton;
      property Items[Index : Integer] : TSkeletonCollider read GetItem; default;
  end;

//=========================================================================
implementation
//=========================================================================

//
//----- TSkeletonCollider
//
constructor TSkeletonCollider.Create;
begin
  FData.Bone1:=-1;
  FData.Bone2:=-1;
  FData.Params[0]:=0;
  FData.Params[1]:=0;
  FData.Params[2]:=0;
  FData.Matrix:=IdentityHMGMatrix;
end;

constructor TSkeletonCollider.CreateOwned(Owner: TSkeletonColliders);
begin
  Create;
  Owner.Add(Self);
end;

procedure TSkeletonCollider.LoadFromStream(aStream: TStream);
begin
  aStream.Read(FData, SizeOf(TSkeletonColliderData));
end;

procedure TSkeletonCollider.SaveToStream(aStream: TStream);
begin
  aStream.Write(FData, SizeOf(TSkeletonColliderData));
end;

function TSkeletonCollider.GetBone1: Integer;
begin
  Result:=FData.Bone1;
end;

function TSkeletonCollider.GetBone2: Integer;
begin
  Result:=FData.Bone2;
end;

function TSkeletonCollider.GetCollider: TSkeletonColliderType;
begin
  Result:=FData.Collider;
end;

function TSkeletonCollider.GetMatrix: TMatrix;
begin
  Result:=FData.Matrix;
end;

function TSkeletonCollider.GetParam(Index: TSCDParamRange): Single;
begin
  Result:=FData.Params[Index];
end;

procedure TSkeletonCollider.SetBone1(const Value: Integer);
begin
  FData.Bone1:=Value;
end;

procedure TSkeletonCollider.SetBone2(const Value: Integer);
begin
  FData.Bone2:=Value;
end;

procedure TSkeletonCollider.SetCollider(const Value: TSkeletonColliderType);
begin
  FData.Collider:=Value;
end;

procedure TSkeletonCollider.SetMatrix(const Value: TMatrix);
begin
  FData.Matrix:=Value;
end;

procedure TSkeletonCollider.SetParam(Index: TSCDParamRange;
  const Value: Single);
begin
  FData.Params[Index]:=Value;
end;

{ TSkeletonColliderSphere }

constructor TSkeletonColliderSphere.Create;
begin
  inherited;
  FData.Collider:=sctSphere;
end;

function TSkeletonColliderSphere.GetRadius: Single;
begin
  Result:=GetParam(0);
end;

procedure TSkeletonColliderSphere.SetRadius(const Value: Single);
begin
  SetParam(0,Value);
end;

{ TSkeletonColliderCapsule }

function TSkeletonColliderCapsule.GetRadius: Single;
begin
  Result:=GetParam(0);
end;

function TSkeletonColliderCapsule.GetHeight: Single;
begin
  Result:=GetParam(1);
end;

procedure TSkeletonColliderCapsule.SetRadius(const Value: Single);
begin
  SetParam(0,Value);
end;

procedure TSkeletonColliderCapsule.SetHeight(const Value: Single);
begin
  SetParam(1,Value);
end;

constructor TSkeletonColliderCapsule.Create;
begin
  inherited;
  FData.Collider:=sctCapsule;
end;

{ TSkeletonColliderBox }

function TSkeletonColliderBox.GetWidth: Single;
begin
  Result:=GetParam(0);
end;

function TSkeletonColliderBox.GetHeight: Single;
begin
  Result:=GetParam(1);
end;

function TSkeletonColliderBox.GetDepth: Single;
begin
  Result:=GetParam(2);
end;

procedure TSkeletonColliderBox.SetWidth(const Value: Single);
begin
  SetParam(0,Value);
end;

procedure TSkeletonColliderBox.SetHeight(const Value: Single);
begin
  SetParam(1,Value);
end;

procedure TSkeletonColliderBox.SetDepth(const Value: Single);
begin
  SetParam(2,Value);
end;

constructor TSkeletonColliderBox.Create;
begin
  inherited;
  FData.Collider:=sctBox;
end;

{ TSkeletonColliders }

constructor TSkeletonColliders.Create;
begin
  inherited;

  FHeader.ID:=cSkeletonColliderID;
  FHeader.Version:=cSkeletonColliderVersion;
end;

procedure TSkeletonColliders.Delete(index: Integer);
begin
  GetItem(index).Free;
  inherited Delete(index);
end;

function TSkeletonColliders.Remove(item: TSkeletonCollider) : Integer;
begin
  Result:=(inherited Remove(item));
  item.Free;
end;

function TSkeletonColliders.GetItem(Index: Integer): TSkeletonCollider;
begin
  Result:=TSkeletonCollider(Get(Index));
end;

procedure TSkeletonColliders.LoadFromFile(FileName: String);
var
  Stream : TStream;
begin
  Stream:=CreateFileStream(FileName);
  LoadFromStream(Stream);
  Stream.Free;
end;

procedure TSkeletonColliders.LoadFromStream(aStream: TStream);
var
  i : Integer;
  p : Int64;
  sct : TSkeletonColliderType;
begin
  Clear;
  aStream.Read(FHeader,SizeOf(TSkeletonColliderHeader));
  if FHeader.ID<>cSkeletonColliderID then
    Exception.Create('File ID mismatch ['+FHeader.ID+'].');
  // This version check will need to be updated if the versions
  // change to support older formats.
  if FHeader.Version<>cSkeletonColliderVersion then
    Exception.Create('File version mismatch ['+IntToStr(FHeader.Version)+']. Supported version is '+IntToStr(cSkeletonColliderVersion)+'.');
  for i:=0 to FHeader.ColliderCount-1 do begin
    p:=aStream.Position;
    aStream.Read(sct,SizeOf(TSkeletonColliderType));
    aStream.Position:=p;
    case sct of
      sctSphere  : TSkeletonColliderSphere.CreateOwned(self);
      sctCapsule : TSkeletonColliderCapsule.CreateOwned(self);
      sctBox     : TSkeletonColliderBox.CreateOwned(self);
    end;
    Items[i].LoadFromStream(aStream);
  end;
end;

procedure TSkeletonColliders.SaveToFile(FileName: String);
var
  Stream : TStream;
begin
  Stream:=CreateFileStream(FileName,fmCreate);
  SaveToStream(Stream);
  Stream.Free;
end;

procedure TSkeletonColliders.SaveToStream(aStream: TStream);
var
  i : integer;
begin
  FHeader.ColliderCount:=Count;
  aStream.Write(FHeader,SizeOf(TSkeletonColliderHeader));
  for i:=0 to Count-1 do
    Items[i].SaveToStream(aStream);
end;

end.