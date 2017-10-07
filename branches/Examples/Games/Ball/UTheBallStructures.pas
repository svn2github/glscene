// UTheBallStructures
{: Structures you can have in a TheBall game map.<p>

	<b>History : </b><font size=-1><ul>
	   <li>28/10/02 - EG - Creation
	</ul></font>
}
unit UTheBallStructures;

interface

uses
  Classes, SysUtils,

  //GLScene
  GLScene, GLObjects, GLVectorGeometry, OdeImport, OdeGL, GLBitmapFont,
  GLGeomObjects, GLBaseClasses, GLColor, GLTexture,  GLParticleFX, GLSound, GLUtils;

type

   TTheBallStructure = class;

   // TTheBallStructures
   //
   TTheBallStructures = class (TList)
	   protected
	      { Protected Declarations }
         function GetItems(index : Integer) : TTheBallStructure;
         procedure SetItems(index : Integer; val : TTheBallStructure);

	   public
	       
         property Items[index : Integer] : TTheBallStructure read GetItems write SetItems; default;

         function StructureByName(const aName : String) : TTheBallStructure;
   end;

	// TTheBallStructure
	//
	TTheBallStructure = class (TPersistent)
	   private
	       
         FOwner : TTheBallStructures;
         FName : String;

	   protected
	      { Protected Declarations }
         function ParentObject : TGLBaseSceneObject; dynamic;

	   public
	       
	      constructor Create(aOwner : TTheBallStructures); virtual;
         destructor Destroy; override;

         property Owner : TTheBallStructures read FOwner;
         property Name : String read FName;

         procedure Parse(const vals : TStringList); dynamic;
         procedure Instantiate; dynamic; abstract;
         procedure Release; dynamic; abstract;
         procedure Progress(const progressTime : TProgressTimes); virtual;
         procedure SetTransparency(alpha : Single); dynamic;
	end;

   TTheBallStructureClass = class of TTheBallStructure;

	// TTBTableText
	//
	TTBTableText = class (TTheBallStructure)
	   private
	       
         FFlatText : TGLFlatText;
         FPosition : TAffineVector;
         FOrientation : TAffineVector;
         FSize : Single;
         FColor : Integer;
         FText : String;

	   protected
	      { Protected Declarations }

	   public
	       
	      constructor Create(aOwner : TTheBallStructures); override;
         destructor Destroy; override;

         procedure Parse(const vals : TStringList); override;
         procedure Instantiate; override;
         procedure Release; override;
	end;

	// TTBCubeArea
	//
	TTBCubeArea = class (TTheBallStructure)
	   private
	       
         FPosition : TAffineVector;
         FSize : TAffineVector;

	   protected
	      { Protected Declarations }

	   public
	       
	      constructor Create(aOwner : TTheBallStructures); override;
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
	       
         FDummy : TGLDummyCube;
         
	   public
	       
         procedure Instantiate; override;
         procedure Release; override;
         procedure Progress(const progressTime : TProgressTimes); override;
   end;

   // TTBSpikes
	//
	TTBSpikes = class (TTBCubeArea)
	   private
	       
         FNB : Integer;
         FDummy : TGLDummyCube;
         
	   public
	       
         procedure Parse(const vals : TStringList); override;
         procedure Instantiate; override;
         procedure Release; override;
         procedure Progress(const progressTime : TProgressTimes); override;
   end;

   // TTBFire
	//
	TTBFire = class (TTBCubeArea)
	   private
	       
         FDisk : TGLDisk;
         
	   public
	       
         procedure Parse(const vals : TStringList); override;
         procedure Instantiate; override;
         procedure Release; override;
         procedure Progress(const progressTime : TProgressTimes); override;
   end;

   // TTBSteam
	//
	TTBSteam = class (TTBCubeArea)
	   private
	       
         FPlane : TGLPlane;
         FTimeOffset, FTimeOn, FTimeOff : Single;
         FStrength : Single;

	   public
	       
         procedure Parse(const vals : TStringList); override;
         procedure Instantiate; override;
         procedure Release; override;
         procedure Progress(const progressTime : TProgressTimes); override;
   end;

   // TTBTrigger
	//
	TTBTrigger = class (TTBCubeArea)
	   private
	       
         FActionStartTime : Double;
         FDisk : TGLDisk;
         FTarget, FAction, FSound : String;

	   public
	       
         procedure Parse(const vals : TStringList); override;
         procedure Instantiate; override;
         procedure Release; override;
         procedure Progress(const progressTime : TProgressTimes); override;
   end;

	// TTBBlock
	//
	TTBBlock = class (TTBCubeArea)
	   private
	       
         FBlock : TGLCube;
         FBlockGeom : PdxGeom;

	   protected
	      { Protected Declarations }

	   public
	       
	      constructor Create(aOwner : TTheBallStructures); override;
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
	       
         procedure Instantiate; override;
	end;

	// TTBTransparentBlock
	//
	TTBTransparentBlock = class (TTBBlock)
	   private
	       
         FInitialAlpha : Single;

	   protected
	      { Protected Declarations }
         function ParentObject : TGLBaseSceneObject; override;
         
	   public
	       
         procedure SetTransparency(alpha : Single); override;
	end;

	// TTBGlassBlock
	//
	TTBGlassBlock = class (TTBTransparentBlock)
	   public
	       
         procedure Instantiate; override;
	end;

procedure ParseTheBallMap(const mapData : String; strucList : TTheBallStructures;
                          var mapTitle : String);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses FMain;

// ParseTheBallMap
//
procedure ParseTheBallMap(const mapData : String; strucList : TTheBallStructures;
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
            struc:=TTheBallStructureClass(FindClass(className)).Create(strucList);
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
// ------------------ TTheBallStructures ------------------
// ------------------

// GetItems
//
function TTheBallStructures.GetItems(index : Integer) : TTheBallStructure;
begin
   Result:=TTheBallStructure(inherited Items[index]);
end;

// SetItems
//
procedure TTheBallStructures.SetItems(index : Integer; val : TTheBallStructure);
begin
   inherited Items[index]:=val;
end;

// StructureByName
//
function TTheBallStructures.StructureByName(const aName : String) : TTheBallStructure;
var
   i : Integer;
begin
   Result:=nil;
   for i:=0 to Count-1 do
      if CompareText(aName, Items[i].Name)=0 then begin
         Result:=Items[i];
         Break;
      end;
end;

// ------------------
// ------------------ TTheBallStructure ------------------
// ------------------

// Create
//
constructor TTheBallStructure.Create(aOwner : TTheBallStructures);
begin
	inherited Create;
   FOwner:=aOwner;
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

// SetTransparency
//
procedure TTheBallStructure.SetTransparency(alpha : Single);
begin
   // nothing
end;

// ------------------
// ------------------ TTBTableText ------------------
// ------------------

// Create
//
constructor TTBTableText.Create(aOwner : TTheBallStructures);
begin
	inherited;
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
   FPosition.X:=StrToFloatDef(vals.Values['X'], 0);
   FPosition.Y:=StrToFloatDef(vals.Values['Y'], 0.01);
   FPosition.Z:=StrToFloatDef(vals.Values['Z'], 0);
   FOrientation.X:=StrToFloatDef(vals.Values['OX'], 1);
   FOrientation.Y:=StrToFloatDef(vals.Values['OY'], 0);
   FOrientation.Z:=StrToFloatDef(vals.Values['OZ'], 0);
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
constructor TTBCubeArea.Create(aOwner : TTheBallStructures);
begin
	inherited;
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
   FPosition.X:=StrToFloatDef(vals.Values['X'], 0);
   FPosition.Y:=StrToFloatDef(vals.Values['Y'], 0.5);
   FPosition.Z:=StrToFloatDef(vals.Values['Z'], 0);
   FSize.X:=StrToFloatDef(vals.Values['SX'], 1);
   FSize.Y:=StrToFloatDef(vals.Values['SY'], 1);
   FSize.Z:=StrToFloatDef(vals.Values['SZ'], 1);
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
      spike.Height:=(Random*0.4+0.6)*FSize.Y;
      spike.Position.X:=(Random-0.5)*2*FSize.X;
      spike.Position.Y:=spike.Height*0.5;
      spike.Position.Z:=(Random-0.5)*2*FSize.Z;
      spike.Parts:=[coSides];
      spike.BottomRadius:=0.1*VectorLength(FSize.X, FSize.Y);
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
// ------------------ TTBSteam ------------------
// ------------------

// Parse
//
procedure TTBSteam.Parse(const vals : TStringList);
begin
   inherited;
   FTimeOffset:=StrToFloatDef(vals.Values['TimeOffset'], 0);
   FTimeOn:=StrToFloatDef(vals.Values['TimeOn'], 3);
   FTimeOff:=StrToFloatDef(vals.Values['TimeOff'], 3);
   FStrength:=StrToFloatDef(vals.Values['Strength'], 20);
end;

// Instantiate
//
procedure TTBSteam.Instantiate;
var
   src : TGLSourcePFXEffect;
begin
   FPlane:=TGLPlane(ParentObject.AddNewChild(TGLPlane));
   FPlane.Direction.AsVector:=YHmgVector;
   FPlane.Position.AsAffineVector:=Position;
   FPlane.Width:=FSize.X;
   FPlane.Height:=FSize.Z;
   FPlane.Material.MaterialLibrary:=Main.MaterialLibrary;
   FPlane.Material.LibMaterialName:='chrome';

   src:=GetOrCreateSourcePFX(FPlane);
   src.Manager:=Main.PFXSteam;
   src.ParticleInterval:=0.03;
   src.PositionDispersion:=VectorLength(FSize)*0.2;
   src.VelocityDispersion:=VectorLength(FSize)*0.7;
   src.InitialVelocity.Y:=Sqrt(FStrength);
end;

// Release
//
procedure TTBSteam.Release;
begin
   FreeAndNil(FPlane);
end;

// Progress
//
procedure TTBSteam.Progress(const progressTime : TProgressTimes);
var
   t : Single;
   src : TGLSourcePFXEffect;
   v : TVector;
begin
   src:=GetOrCreateSourcePFX(FPlane);
   t:=Frac((progressTime.newTime+FTimeOffset)/(FTimeOn+FTimeOff))*(FTimeOn+FTimeOff);
   if t<=FTimeOn then begin
      src.ParticleInterval:=0.03;
      v:=FPlane.AbsoluteToLocal(Main.DCBallAbsolute.Position.AsVector);
      if (Abs(v.X)<FSize.X) and (Abs(v.Y)<FSize.Z) and (v.Z>=0) then begin
         if v.Z<FStrength*0.3 then
            Main.verticalForce:=Main.verticalForce+FStrength*(1-(v.Z/(FStrength*0.3)))*0.7;
      end;
   end else src.ParticleInterval:=0.5;
end;

// ------------------
// ------------------ TTBTrigger ------------------
// ------------------

// Parse
//
procedure TTBTrigger.Parse(const vals : TStringList);
begin
   inherited;
   FTarget:=vals.Values['Target'];
   FAction:=vals.Values['Action'];
   FSound:=vals.Values['Sound'];
end;

// Instantiate
//
procedure TTBTrigger.Instantiate;
begin
   FActionStartTime:=-1;
   FDisk:=TGLDisk(ParentObject.AddNewChild(TGLDisk));
   FDisk.Direction.AsVector:=YHmgVector;
   FDisk.Position.AsAffineVector:=Position;
   FDisk.Loops:=1;
   FDisk.Slices:=6;
   FDisk.OuterRadius:=VectorLength(FSize)*0.4;
   FDisk.Material.MaterialLibrary:=Main.MaterialLibrary;
   FDisk.Material.LibMaterialName:='wood';
end;

// Release
//
procedure TTBTrigger.Release;
begin
   FreeAndNil(FDisk);
end;

// Progress
//
procedure TTBTrigger.Progress(const progressTime : TProgressTimes);
var
   trg : TTheBallStructure;
   d : Single;
begin
   if FActionStartTime=-1 then begin
      if FDisk.DistanceTo(Main.DCBallAbsolute)<VectorLength(FSize)*0.7 then begin
         FActionStartTime:=progressTime.newTime;
         FDisk.Position.Y:=FDisk.Position.Y-0.05;
         if (FSound<>'') and Main.GLSMBass.Active then begin
            with GetOrCreateSoundEmitter(FDisk) do begin
               Source.SoundLibrary:=Main.SoundLibrary;
               Source.SoundName:=FSound;
               Playing:=True;
            end;
         end;
      end;
   end;
   if FActionStartTime>=0 then begin
      trg:=Owner.StructureByName(FTarget);
      if Assigned(trg) then begin
         if CompareText(FAction, 'Vanish')=0 then begin
            d:=FActionStartTime+3-progressTime.newTime;
            if (d>=0) and (d<=3) then
               trg.SetTransparency(Sqr(d)*0.5)
            else begin
               Owner.Items[Owner.IndexOf(trg)]:=nil;
               trg.Release;
               FActionStartTime:=-2;
            end;
         end;
      end;
   end;
end;

// ------------------
// ------------------ TTBBlock ------------------
// ------------------

// Create
//
constructor TTBBlock.Create(aOwner : TTheBallStructures);
begin
	inherited;
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
   FBlock.CubeWidth:=FSize.X;
   FBlock.CubeHeight:=FSize.Y;
   FBlock.CubeDepth:=FSize.Z;
   FBlock.Parts:=FBlock.Parts-[cpBottom];

   FBlockGeom:=dCreateBox(Main.space, FSize.X, FSize.Y, FSize.Z);
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

// SetTransparency
//
procedure TTBTransparentBlock.SetTransparency(alpha : Single);
var
   n : String;
begin
   n:=FBlock.Material.LibMaterialName;
   if n<>'' then begin
      // clone material locally
      FBlock.Material:=Main.MaterialLibrary.Materials.GetLibMaterialByName(n).Material;
      FBlock.Material.FrontProperties.Emission.Color:=clrGray50;
   end;
   if FInitialAlpha=0 then
      FInitialAlpha:=FBlock.Material.FrontProperties.Diffuse.Alpha;
   FBlock.Material.FrontProperties.Diffuse.Alpha:=alpha*FInitialAlpha;
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
                    TTBFire, TTBGlassBlock, TTBTableText, TTBTrigger, TTBSteam]);

end.

