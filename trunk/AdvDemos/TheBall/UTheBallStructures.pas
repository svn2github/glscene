// UTheBallStructures
{: Structures you can have in a TheBall game map.<p>

	<b>History : </b><font size=-1><ul>
	   <li>28/10/02 - EG - Creation
	</ul></font>
}
unit UTheBallStructures;

interface

uses Classes, GLScene, GLMisc, GLObjects, Geometry, ODEImport, GLBitmapFont;

type

	// TTheBallStructure
	//
	TTheBallStructure = class (TPersistent)
	   private
	      { Private Declarations }
         FName : String;

	   protected
	      { Protected Declarations }
         function ParentObject : TGLBaseSceneObject; dynamic;

	   public
	      { Public Declarations }
	      constructor Create; virtual;
         destructor Destroy; override;

         property Name : String read FName;

         procedure Parse(const vals : TStringList); dynamic;
         procedure Instantiate; dynamic; abstract;
         procedure Release; dynamic; abstract;
         procedure Progress(const progressTime : TProgressTimes); virtual;
	end;

   TTheBallStructureClass = class of TTheBallStructure;

	// TTBTableText
	//
	TTBTableText = class (TTheBallStructure)
	   private
	      { Private Declarations }
         FFlatText : TGLFlatText;
         FPosition : TAffineVector;
         FOrientation : TAffineVector;
         FSize : Single;
         FColor : Integer;
         FText : String;

	   protected
	      { Protected Declarations }

	   public
	      { Public Declarations }
	      constructor Create; override;
         destructor Destroy; override;

         procedure Parse(const vals : TStringList); override;
         procedure Instantiate; override;
         procedure Release; override;
	end;

	// TTBCubeArea
	//
	TTBCubeArea = class (TTheBallStructure)
	   private
	      { Private Declarations }
         FPosition : TAffineVector;
         FSize : TAffineVector;

	   protected
	      { Protected Declarations }

	   public
	      { Public Declarations }
	      constructor Create; override;
         destructor Destroy; override;

         procedure Parse(const vals : TStringList); override;
         procedure Instantiate; override;
         procedure Release; override;

         property Position : TAffineVector read FPosition;
         property Size : TAffineVector read FSize;
	end;

	// TTBSpawnPoint
	//
	TTBSpawnPoint = class (TTBCubeArea)
   end;

	// TTBBallExit
	//
	TTBBallExit = class (TTBCubeArea)
	   private
	      { Private Declarations }
         FDummy : TGLDummyCube;
         
	   public
	      { Public Declarations }
         procedure Instantiate; override;
         procedure Release; override;
         procedure Progress(const progressTime : TProgressTimes); override;
   end;

   // TTBSpikes
	//
	TTBSpikes = class (TTBCubeArea)
	   private
	      { Private Declarations }
         FNB : Integer;
         FDummy : TGLDummyCube;
         
	   public
	      { Public Declarations }
         procedure Parse(const vals : TStringList); override;
         procedure Instantiate; override;
         procedure Release; override;
         procedure Progress(const progressTime : TProgressTimes); override;
   end;

   // TTBFire
	//
	TTBFire = class (TTBCubeArea)
	   private
	      { Private Declarations }
         FDisk : TGLDisk;
         
	   public
	      { Public Declarations }
         procedure Parse(const vals : TStringList); override;
         procedure Instantiate; override;
         procedure Release; override;
         procedure Progress(const progressTime : TProgressTimes); override;
   end;

	// TTBBlock
	//
	TTBBlock = class (TTBCubeArea)
	   private
	      { Private Declarations }
         FBlock : TGLCube;
         FBlockGeom : PdxGeom;

	   protected
	      { Protected Declarations }

	   public
	      { Public Declarations }
	      constructor Create; override;
         destructor Destroy; override;

         procedure Parse(const vals : TStringList); override;
         procedure Instantiate; override;
         procedure Release; override;
         procedure Progress(const progressTime : TProgressTimes); override;
	end;

	// TTBMarbleBlock
	//
	TTBMarbleBlock = class (TTBBlock)
	   public
	      { Public Declarations }
         procedure Instantiate; override;
	end;

	// TTBTransparentBlock
	//
	TTBTransparentBlock = class (TTBBlock)
	   protected
	      { Protected Declarations }
         function ParentObject : TGLBaseSceneObject; override;
	end;

	// TTBGlassBlock
	//
	TTBGlassBlock = class (TTBTransparentBlock)
	   public
	      { Public Declarations }
         procedure Instantiate; override;
	end;

procedure ParseTheBallMap(const mapData : String; strucList : TList;
                          var mapTitle : String);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils, FMain, GLTexture, ODEGL, GLParticleFX;

// ParseTheBallMap
//
procedure ParseTheBallMap(const mapData : String; strucList : TList;
                          var mapTitle : String);
var
   i, p : Integer;
   line, className : String;
   sl, vals : TStringList;
   struc : TTheBallStructure;
begin
   sl:=TStringList.Create;
   vals:=TStringList.Create;
   try
      sl.Text:=mapData;
      for i:=0 to sl.Count-1 do begin
         line:=Trim(sl[i]);
         if (line='') or (Copy(line, 1, 2)='//') then continue;
         p:=Pos(':', line);
         Assert(p>1);
         className:='TTB'+Trim(Copy(line, 1, p-1));
         if CompareText(className, 'TTBTitle')=0 then
            mapTitle:=Trim(Copy(line, p+1, MaxInt))
         else begin
            struc:=TTheBallStructureClass(FindClass(className)).Create;
            vals.CommaText:=Trim(Copy(line, p+1, MaxInt));
            struc.Parse(vals);
            strucList.Add(struc);
         end;
      end;
   finally
      vals.Free;
      sl.Free;
   end;
end;

// ------------------
// ------------------ TTheBallStructure ------------------
// ------------------

// Create
//
constructor TTheBallStructure.Create;
begin
	inherited Create;
end;

// Destroy
//
destructor TTheBallStructure.Destroy;
begin
	inherited Destroy;
end;

// ParentObject
//
function TTheBallStructure.ParentObject : TGLBaseSceneObject;
begin
   Result:=Main.DCMap;
end;

// Parse
//
procedure TTheBallStructure.Parse(const vals : TStringList);
begin
   FName:=vals.Values['Name'];
end;

// Progress
//
procedure TTheBallStructure.Progress(const progressTime : TProgressTimes);
begin
   // nothing
end;

// ------------------
// ------------------ TTBTableText ------------------
// ------------------

// Create
//
constructor TTBTableText.Create;
begin
	inherited Create;
end;

// Destroy
//
destructor TTBTableText.Destroy;
begin
	inherited Destroy;
end;

// Parse
//
procedure TTBTableText.Parse(const vals : TStringList);
begin
   inherited;
   FPosition[0]:=StrToFloatDef(vals.Values['X'], 0);
   FPosition[1]:=StrToFloatDef(vals.Values['Y'], 0.01);
   FPosition[2]:=StrToFloatDef(vals.Values['Z'], 0);
   FOrientation[0]:=StrToFloatDef(vals.Values['OX'], 1);
   FOrientation[1]:=StrToFloatDef(vals.Values['OY'], 0);
   FOrientation[2]:=StrToFloatDef(vals.Values['OZ'], 0);
   NormalizeVector(FOrientation);
   FSize:=StrToFloatDef(vals.Values['Size'], 1)*0.01;
   FColor:=StrToIntDef(vals.Values['Color'], 0);
   FText:=vals.Values['Text'];
end;

// Instantiate
//
procedure TTBTableText.Instantiate;
begin
   FFlatText:=TGLFlatText(ParentObject.AddNewChild(TGLFlatText));
   FFlatText.Position.AsAffineVector:=FPosition;
   FFlatText.Direction.AsVector:=YHmgVector;
   FFlatText.Up.AsAffineVector:=FOrientation;
   FFlatText.Roll(180);
   FFlatText.Scale.SetVector(FSize, FSize, FSize);
   FFlatText.BitmapFont:=Main.WindowsBitmapFont;
   FFlatText.Text:=FText;
   FFlatText.ModulateColor.AsWinColor:=FColor;
end;

// Release
//
procedure TTBTableText.Release;
begin
   FreeAndNil(FFlatText);
end;

// ------------------
// ------------------ TTBCubeArea ------------------
// ------------------

// Create
//
constructor TTBCubeArea.Create;
begin
	inherited Create;
end;

// Destroy
//
destructor TTBCubeArea.Destroy;
begin
	inherited Destroy;
end;

// Parse
//
procedure TTBCubeArea.Parse(const vals : TStringList);
begin
   inherited;
   FPosition[0]:=StrToFloatDef(vals.Values['X'], 0);
   FPosition[1]:=StrToFloatDef(vals.Values['Y'], 0.5);
   FPosition[2]:=StrToFloatDef(vals.Values['Z'], 0);
   FSize[0]:=StrToFloatDef(vals.Values['SX'], 1);
   FSize[1]:=StrToFloatDef(vals.Values['SY'], 1);
   FSize[2]:=StrToFloatDef(vals.Values['SZ'], 1);
end;

// Instantiate
//
procedure TTBCubeArea.Instantiate;
begin
   // nothing
end;

// Release
//
procedure TTBCubeArea.Release;
begin
   // nothing
end;

// ------------------
// ------------------ TTBBallExit ------------------
// ------------------

// Instantiate
//
procedure TTBBallExit.Instantiate;
var
   src : TGLSourcePFXEffect;
begin
   FDummy:=TGLDummyCube(ParentObject.AddNewChild(TGLDummyCube));
   FDummy.Position.AsAffineVector:=Position;
   
   src:=GetOrCreateSourcePFX(FDummy);
   src.Manager:=Main.PFXExit;
   src.ParticleInterval:=0.02;
   src.PositionDispersion:=VectorLength(FSize)*0.5;
   src.VelocityDispersion:=VectorLength(FSize)*0.1;
end;

// Release
//
procedure TTBBallExit.Release;
begin
   FreeAndNil(FDummy);
end;

// Progress
//
procedure TTBBallExit.Progress(const progressTime : TProgressTimes);
var
   src : TGLSourcePFXEffect;
begin
   if FDummy.DistanceTo(Main.DCBallAbsolute)<VectorLength(FSize)*0.7 then begin
      if not Main.LevelWon then begin
         src:=GetOrCreateSourcePFX(FDummy);
         src.VelocityDispersion:=VectorLength(FSize)*2;
         src.Burst(progressTime.newTime, 150);
      end;
   end;
end;

// ------------------
// ------------------ TTBSpikes ------------------
// ------------------

// Parse
//
procedure TTBSpikes.Parse(const vals : TStringList);
begin
   inherited;
   FNB:=StrToIntDef(vals.Values['NB'], 3);
end;

// Instantiate
//
procedure TTBSpikes.Instantiate;
var
   i : Integer;
   spike : TGLCone;
begin
   FDummy:=TGLDummyCube(ParentObject.AddNewChild(TGLDummyCube));
   FDummy.Position.AsAffineVector:=Position;

   for i:=1 to FNB do begin
      spike:=TGLCone(FDummy.AddNewChild(TGLCone));
      spike.Height:=(Random*0.4+0.6)*FSize[1];
      spike.Position.X:=(Random-0.5)*2*FSize[0];
      spike.Position.Y:=spike.Height*0.5;
      spike.Position.Z:=(Random-0.5)*2*FSize[2];
      spike.Parts:=[coSides];
      spike.BottomRadius:=0.1*VectorLength(FSize[0], FSize[1]);
      spike.Stacks:=1;
      spike.Slices:=Random(4)+6;
      spike.Material.MaterialLibrary:=main.MaterialLibrary;
      spike.Material.LibMaterialName:='chrome';
   end;
end;

// Release
//
procedure TTBSpikes.Release;
begin
   FreeAndNil(FDummy);
end;

// Progress
//
procedure TTBSpikes.Progress(const progressTime : TProgressTimes);
begin
   if FDummy.DistanceTo(Main.DCBallAbsolute)<VectorLength(FSize)*0.7 then begin
      if Main.deflateEnergy=0 then begin
         Main.deflateEnergy:=3;
         Main.deflateVector:=VectorNormalize(Main.DCBallAbsolute.AbsoluteToLocal(YVector));
         if Main.ballBody<>nil then
            dBodyAddForce(Main.ballBody, 0, 1500, 0);
      end;
   end;
end;

// ------------------
// ------------------ TTBFire ------------------
// ------------------

// Parse
//
procedure TTBFire.Parse(const vals : TStringList);
begin
   inherited;
end;

// Instantiate
//
procedure TTBFire.Instantiate;
var
   src : TGLSourcePFXEffect;
begin
   FDisk:=TGLDisk(ParentObject.AddNewChild(TGLDisk));
   FDisk.Direction.AsVector:=YHmgVector;
   FDisk.Position.AsAffineVector:=Position;
   FDisk.Loops:=1;
   FDisk.Slices:=8;
   FDisk.OuterRadius:=VectorLength(FSize)*0.4;
   FDisk.Material.MaterialLibrary:=Main.MaterialLibrary;
   FDisk.Material.LibMaterialName:='chrome';

   src:=GetOrCreateSourcePFX(FDisk);
   src.Manager:=Main.PFXFire;
   src.ParticleInterval:=0.05;
   src.PositionDispersion:=VectorLength(FSize)*0.2;
   src.VelocityDispersion:=VectorLength(FSize)*0.1;
end;

// Release
//
procedure TTBFire.Release;
begin
   FreeAndNil(FDisk);
end;

// Progress
//
procedure TTBFire.Progress(const progressTime : TProgressTimes);
var
   src : TGLSourcePFXEffect;
begin
   if FDisk.DistanceTo(Main.DCBallAbsolute)<VectorLength(FSize)*0.7 then begin
      src:=GetOrCreateSourcePFX(Main.SPHBall);
      if src.Manager=nil then begin
         src.Manager:=Main.PFXFire;
         src.ParticleInterval:=0.01;
         src.PositionDispersion:=0.4;
         src.VelocityDispersion:=0.1;
         Main.burnOut:=3;
      end;
   end;
end;

// ------------------
// ------------------ TTBBlock ------------------
// ------------------

// Create
//
constructor TTBBlock.Create;
begin
	inherited Create;
end;

// Destroy
//
destructor TTBBlock.Destroy;
begin
   Assert(FBlockGeom=nil);
   Assert(FBlock=nil);
	inherited Destroy;
end;

// Parse
//
procedure TTBBlock.Parse(const vals : TStringList);
begin
   inherited;
end;

// Instantiate
//
procedure TTBBlock.Instantiate;
begin
   FBlock:=TGLCube(ParentObject.AddNewChild(TGLCube));
   FBlock.Position.AsAffineVector:=FPosition;
   FBlock.CubeWidth:=FSize[0];
   FBlock.CubeHeight:=FSize[1];
   FBlock.CubeDepth:=FSize[2];
   FBlock.Parts:=FBlock.Parts-[cpBottom];

   FBlockGeom:=dCreateBox(Main.space, FSize[0], FSize[1], FSize[2]);
   CopyPosFromGeomToGL(FBlockGeom, FBlock);
end;

// Release
//
procedure TTBBlock.Release;
begin
   FreeAndNil(FBlock);

   if Assigned(FBlockGeom) then begin
      dGeomDestroy(FBlockGeom);
      FBlockGeom:=nil;
   end;
end;

// Progress
//
procedure TTBBlock.Progress(const progressTime : TProgressTimes);
begin
   CopyPosFromGeomToGL(FBlockGeom, FBlock);
end;

// ------------------
// ------------------ TTBMarbleBlock ------------------
// ------------------

// Instantiate
//
procedure TTBMarbleBlock.Instantiate;
begin
   inherited;
   with FBlock.Material do begin
      MaterialLibrary:=Main.MaterialLibrary;
      LibMaterialName:='marbleblock';
   end;
end;

// ------------------
// ------------------ TTBTransparentBlock ------------------
// ------------------

// Instantiate
//
function TTBTransparentBlock.ParentObject : TGLBaseSceneObject;
begin
   Result:=Main.DCMapTransparent;
end;

// ------------------
// ------------------ TTBGlassBlock ------------------
// ------------------

// Instantiate
//
procedure TTBGlassBlock.Instantiate;
begin
   inherited;
   with FBlock.Material do begin
      MaterialLibrary:=Main.MaterialLibrary;
      LibMaterialName:='glassblock';
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

	// class registrations
   RegisterClasses([TTBMarbleBlock, TTBSpawnPoint, TTBBallExit, TTBSpikes,
                    TTBFire, TTBGlassBlock, TTBTableText]);

end.

