// GLWaterPlane
{: A plane simulating animated water<p>

	<b>History : </b><font size=-1><ul>
      <li>01/04/03 - EG - Cleanup and optimizations
      <li>14/11/03 - Mrqzzz - Tried "CreateRippleAtWorldPos" to work at any position/rotation, but need expert's help.. :(
      <li>13/11/03 - Mrqzzz - Tried to add timing indipendence (quite not precise yet)
      <li>12/11/03 - Mrqzzz - Added some properties & small optims added
      <li>01/01/03 - Sternas Stefanos - Original code
   </ul></font>

   <p>The Original Code is part of Cosmos4D<br>
   http://users.hol.gr/~sternas/<br>
   Sternas Stefanos 2003
}
unit GLWaterPlane;

interface

uses Classes, Types, VectorTypes, VectorGeometry, GLScene, GLTexture,
   GLMisc, OpenGL1x, VectorLists, GLCrossPlatform, PersistentClasses;

type

   // TGLWaterPlane
   //
   TGLWaterPlane = class (TGLSceneObject)
		private
         { Private Declarations }
         FRainTimeInterval : Integer;
         FRainForce : Single;
         FViscosity : Single;
         FActive : Boolean;
         FMaxWaveAmp : Integer;
         FElastic : Single;
         FResolution, FResolutionSqr : Integer;
         FResolutionInv : Single;
         FSimulationFrequency, FTimeToNextUpdate : Single;
         FTimeToNextRainDrop : Single;
         FMaximumCatchupIterations : Integer;
         FPositions, FVelocity : packed array of Single;
         FLocks : packed array of ByteBool;
         FPlaneQuadIndices : TPersistentObjectList;
         FPlaneQuadTexCoords : TTexPointList;
         FPlaneQuadVertices : TAffineVectorList;
         FPlaneQuadNormals : TAffineVectorList;
         FLastIterationStepTime : Single;
         FMask : TGLPicture;

      protected
         { Protected Declarations }
         procedure SetMaxWaveAmp(const value : Integer);
         procedure SetElastic(const value : Single);
         procedure SetResolution(const value : Integer);
         procedure SetRainTimeInterval(const val : Integer);
         procedure SetViscosity(const val : Single);
         procedure SetRainForce(const val : Single);
         procedure SetSimulationFrequency(const val : Single);
         procedure SetMask(val : TGLPicture);

         procedure DoMaskChanged(Sender : TObject);
         procedure InitResolution;

         procedure Iterate;

      public
         { Public Declarations }
         constructor Create(AOwner : TComponent); override;
         destructor Destroy; override;

         procedure DoProgress(const progressTime : TProgressTimes); override;
         procedure BuildList(var rci : TRenderContextInfo); override;
         procedure Assign(Source: TPersistent); override;
         
         procedure CreateRippleAtGridPos(X,Y:integer);
         procedure CreateRippleAtWorldPos(const x, y, z : Single); overload;
         procedure CreateRippleAtWorldPos(const pos : TVector); overload;
         procedure CreateRippleRandom;
         procedure Reset;

         {: CPU time (in seconds) taken by the last iteration step. }
         property LastIterationStepTime : Single read FLastIterationStepTime;

      published
         { Published Declarations }
         
         property Active : Boolean read FActive write FActive default True;

         {: Delay between raindrops in milliseconds (0 = no rain) }
         property RainTimeInterval : Integer read FRainTimeInterval write SetRainTimeInterval default 500;
         property RainForce : Single read FRainForce write SetRainForce;

         property Viscosity : Single read FViscosity write SetViscosity ;//Value from 0 to +1
         property MaxWaveAmp : Integer read FMaxWaveAmp write SetMaxWaveAmp;
         property Elastic : Single read FElastic write SetElastic;
         property Resolution : Integer read FResolution write SetResolution default 64;

         {: A picture whose pixels determine what part of the waterplane is active.<p>
            Pixels with a green/gray component beyond 128 are active, the others
            are not (in short, white = active, black = inactive).<p>
            The picture will automatically be stretched to match the resolution. }
         property Mask : TGLPicture read FMask write SetMask;

         {: Maximum frequency (in Hz) at which simulation iterations happen. }
         property SimulationFrequency : Single read FSimulationFrequency write SetSimulationFrequency;
         {: Maximum number of simulation iterations during catchups.<p>
            Catchups happen when for a reason or another, the DoProgress doesn't
            happen as fast SimulationFrequency. }
         property MaximumCatchupIterations : Integer read FMaximumCatchupIterations write FMaximumCatchupIterations default 1;
   end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
implementation
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

// Create
//
constructor TGLWaterPlane.Create(AOwner : TComponent);
begin
   inherited Create(AOwner);
   ObjectStyle:=ObjectStyle+[osDirectDraw];

   FMaxWaveAmp:=900000;
   FElastic:=10;
   FActive:=True;
   FRainTimeInterval:=500;
   FRainForce:=5000;
   FViscosity:=0.99;
   FSimulationFrequency:=100; // 100 Hz
   FMaximumCatchupIterations:=1;

   FPlaneQuadIndices:=TPersistentObjectList.Create;
   FPlaneQuadTexCoords:=TTexPointList.Create;
   FPlaneQuadVertices:=TAffineVectorList.Create;
   FPlaneQuadNormals:=TAffineVectorList.Create;
   FMask:=TGLPicture.Create;
   FMask.OnChange:=DoMaskChanged;

   SetResolution(64);
end;

// Destroy
//
destructor TGLWaterPlane.Destroy;
begin
   FMask.Free;
   FPlaneQuadNormals.Free;
   FPlaneQuadVertices.Free;
   FPlaneQuadTexCoords.Free;
   FPlaneQuadIndices.CleanFree;
   inherited;
end;

// DoProgress
//
procedure TGLWaterPlane.DoProgress(const progressTime : TProgressTimes);
var
   i : Integer;
begin
   inherited;
   if Active and Visible then begin
      // new raindrops
      if FRainTimeInterval>0 then begin
         FTimeToNextRainDrop:=FTimeToNextRainDrop-progressTime.deltaTime;
         i:=FMaximumCatchupIterations;
         while FTimeToNextRainDrop<=0 do begin
            CreateRippleRandom;
            FTimeToNextRainDrop:=FTimeToNextRainDrop+FRainTimeInterval*0.001;
            Dec(i);
            if i<0 then begin
               if FTimeToNextRainDrop<0 then FTimeToNextRainDrop:=FRainTimeInterval*0.001;
               Break;
            end;
         end;
      end;
      // iterate simulation
      FTimeToNextUpdate:=FTimeToNextUpdate-progressTime.deltaTime;
      if FTimeToNextUpdate<=0 then begin
         i:=FMaximumCatchupIterations;
         while FTimeToNextUpdate<=0 do begin
            Iterate;
            FTimeToNextUpdate:=FTimeToNextUpdate+1/FSimulationFrequency;
            Dec(i);
            if i<0 then begin
               if FTimeToNextUpdate<0 then FTimeToNextUpdate:=1/FSimulationFrequency;
               Break;
            end;
         end;
         StructureChanged;
      end;
   end;
end;

// CreateRippleAtGridPos
//
procedure TGLWaterPlane.CreateRippleAtGridPos(x, y : Integer);
begin
   if (x>0) and (y>0) and (x<Resolution-1) and (y<Resolution-1) then
      FVelocity[x+y*Resolution]:=FRainForce;
end;

// CreateRippleAtWorldPos
//
procedure TGLWaterPlane.CreateRippleAtWorldPos(const x, y, z : Single);
var
   vv : TVector;
begin
   vv:=AbsoluteToLocal(PointMake(x, y, z));
   CreateRippleAtGridPos(Round((vv[0]+0.5)*Resolution),
                         Round((vv[2]+0.5)*Resolution));
end;

// CreateRippleAtWorldPos
//
procedure TGLWaterPlane.CreateRippleAtWorldPos(const pos : TVector);
var
   vv : TVector;
begin
   vv:=AbsoluteToLocal(PointMake(pos));
   CreateRippleAtGridPos(Round((vv[0]+0.5)*Resolution),
                         Round((vv[2]+0.5)*Resolution));
end;

// CreateRippleRandom
//
procedure TGLWaterPlane.CreateRippleRandom;
begin
   CreateRippleAtGridPos(Random(Resolution-3)+2, Random(Resolution-3)+2);
end;

// InitResolution
//
procedure TGLWaterPlane.InitResolution;
var
   i, j : Integer;
   v : TAffineVector;
begin
   FPlaneQuadIndices.Capacity:=FResolutionSqr*2;
   FPlaneQuadTexCoords.Clear;
   FPlaneQuadTexCoords.Capacity:=FResolutionSqr;
   FPlaneQuadVertices.Clear;
   FPlaneQuadVertices.Capacity:=FResolutionSqr;

   for j:=0 to Resolution-1 do begin
      for i:=0 to Resolution-1 do begin
         FPlaneQuadTexCoords.Add(i*FResolutionInv, j*FResolutionInv);
         FPlaneQuadVertices.Add((i-Resolution*0.5)*FResolutionInv,
                                0,
                                (j-Resolution*0.5)*FResolutionInv);
      end;
   end;

   FPlaneQuadNormals.Count:=FResolutionSqr;
   v[0]:=0;
   v[1]:=2048;
   v[2]:=0;
   for i:=0 to FPlaneQuadNormals.Count-1 do
      FPlaneQuadNormals.List[i]:=v;

   SetLength(FPositions, FResolutionSqr);
   SetLength(FVelocity, FResolutionSqr);
   SetLength(FLocks, FResolutionSqr);

   Reset;
   Iterate;

   StructureChanged;
end;

// Reset
//
procedure TGLWaterPlane.Reset;
var
   i, j, ij : Integer;
   maskBmp : TGLBitmap;
   scanLine : PIntegerArray;
   il : TIntegerList;
   locked : Boolean;
begin
   for i:=0 to FResolutionSqr-1 do begin
      FPositions[i]:=0;
      FVelocity[i]:=0;
      FLocks[i]:=False;
   end;
   if FMask.Width>0 then begin
      maskBmp:=TGLBitmap.Create;
      try
         maskBmp.PixelFormat:=glpf32bit;
         maskBmp.Width:=Resolution;
         maskBmp.Height:=Resolution;
         maskBmp.Canvas.StretchDraw(Rect(0, 0, Resolution, Resolution), FMask.Graphic);
         for j:=0 to Resolution-1 do begin
            scanLine:=maskBmp.ScanLine[Resolution-1-j];
            for i:=0 to Resolution-1 do
               FLocks[i+j*Resolution]:=(((scanLine[i] shr 8) and $FF)<128);
         end;
      finally
         maskBmp.Free;
      end;
   end;

   FPlaneQuadIndices.Clean;
   for j:=0 to Resolution-2 do begin
      il:=TIntegerList.Create;
      for i:=0 to Resolution-1 do begin
         ij:=i+j*Resolution;
         if (il.Count and 2)<>0 then
            locked:=False
         else begin
            locked:=FLocks[ij] and FLocks[ij+Resolution];
            if locked and (i<Resolution-1) then
               locked:=FLocks[ij+1] and FLocks[ij+Resolution+1];
         end;
         if not locked then
            il.Add(ij, ij+Resolution)
         else if il.Count>0 then begin
            FPlaneQuadIndices.Add(il);
            il:=TIntegerList.Create;
         end;
      end;
      if il.Count>0 then
         FPlaneQuadIndices.Add(il)
      else il.Free;
   end;
end;

// Iterate
//
procedure TGLWaterPlane.Iterate;
var
   i, j, ij : Integer;
   f, f1, f2 : Single;
   coeff : Single;
   t : Int64;
   pv : PAffineVector;
begin
   if not Visible then Exit;

   t:=StartPrecisionTimer;

   f1:=0.05;
   f2:=0.01*FElastic;

   // Calculate new velocity
   for i:=1 to Resolution-2 do begin
      for j:=1 to Resolution-2 do begin
         ij:=i+j*Resolution;
         if FLocks[ij] then continue;
         FVelocity[ij]:= FVelocity[ij]
                        +f2*( FPositions[ij]
                             -f1*(4*( FPositions[ij-1]         +FPositions[ij+1]
                                     +FPositions[ij-Resolution]+FPositions[ij+Resolution])
                                  +FPositions[ij-1-Resolution] +FPositions[ij+1-Resolution]
                                  +FPositions[ij-1+Resolution] +FPositions[ij+1+Resolution]));
      end;
   end;
   
   // Calculate the new ripple positions and update vertex coordinates
   coeff:=0.02*Resolution;
   f:=1e-4*FResolutionInv;
   for ij:=0 to FResolutionSqr-1 do begin
      if not FLocks[ij] then begin
         FPositions[ij]:=FPositions[ij]-coeff*FVelocity[ij];
         FVelocity[ij]:=FVelocity[ij]*FViscosity;
         while Abs(FPositions[ij])>FMaxWaveAmp do
            FPositions[ij]:=FPositions[ij]*0.8;
      end;
      FPlaneQuadVertices.List[ij][1]:=FPositions[ij]*f;
   end;

   // Calculate the new vertex normals (not normalized, the hardware will handle that)
   for i:=1 to Resolution-2 do begin
      for j:=1 to Resolution-2 do begin
         ij:=i+j*Resolution;
         pv:=@FPlaneQuadNormals.List[ij];
         pv[0]:=FPositions[ij+1]-FPositions[ij-1];
         pv[2]:=FPositions[ij+Resolution]-FPositions[ij-Resolution];
      end;
   end;

   FLastIterationStepTime:=StopPrecisionTimer(t);
end;

// BuildList
//
procedure TGLWaterPlane.BuildList(var rci : TRenderContextInfo);
var
   i : Integer;
   il : TIntegerList;
begin
   glPushClientAttrib(GL_CLIENT_VERTEX_ARRAY_BIT);

   glEnableClientState(GL_VERTEX_ARRAY);
   glVertexPointer(3, GL_FLOAT, 0, FPlaneQuadVertices.List);
   glEnableClientState(GL_NORMAL_ARRAY);
   glNormalPointer(GL_FLOAT, 0, FPlaneQuadNormals.List);
   glEnableClientState(GL_TEXTURE_COORD_ARRAY);
   glTexCoordPointer(2, GL_FLOAT, 0, FPlaneQuadTexCoords.List);

   for i:=0 to FPlaneQuadIndices.Count-1 do begin
      il:=TIntegerList(FPlaneQuadIndices[i]);
      glDrawElements(GL_QUAD_STRIP, il.Count, GL_UNSIGNED_INT, il.List);
   end;

   glPopClientAttrib;
end;

// Assign
//
procedure TGLWaterPlane.Assign(Source: TPersistent);
begin
   if Assigned(Source) and (Source is TGLWaterPlane) then begin
      Active:=TGLWaterPlane(Source).Active;
      RainTimeInterval:=TGLWaterPlane(Source).RainTimeInterval;
      RainForce:=TGLWaterPlane(Source).RainForce;
      Viscosity:=TGLWaterPlane(Source).Viscosity;
   end;
   inherited Assign(Source);
end;

// SetMaxWaveAmp
//
procedure TGLWaterPlane.SetMaxWaveAmp(const value : Integer);
begin
   FMaxWaveAmp:=value;
   if FMaxWaveAmp<1 then FMaxWaveAmp:=1;
end;

// SetElastic
//
procedure TGLWaterPlane.SetElastic(const Value: single);
begin
   FElastic:=Value;
end;

// SetResolution
//
procedure TGLWaterPlane.SetResolution(const value : Integer);
begin
   if value<>FResolution then begin
      FResolution:=Value;
      if FResolution<16 then FResolution:=16;
      FResolutionSqr:=FResolution*FResolution;
      FResolutionInv:=1/FResolution;
      InitResolution;
   end;
end;

// SetRainTimeInterval
//
procedure TGLWaterPlane.SetRainTimeInterval(Const val:integer);
begin
   if (val>=0) and (Val<=1000000) then
      fRainTimeInterval:=val;
end;

// SetViscosity
//
Procedure TGLWaterPlane.SetViscosity(const val : Single);
begin
   if (val>=0) and (val<=1) then
      FViscosity:=val;
end;

// SetRainForce
//
procedure TGLWaterPlane.SetRainForce(const val : Single);
begin
   if (val>=0) and (val<=1000000) then
      FRainForce:=val;
end;

// SetSimulationFrequency
//
procedure TGLWaterPlane.SetSimulationFrequency(const val : Single);
begin
   if FSimulationFrequency<>val then begin
      FSimulationFrequency:=val;
      if FSimulationFrequency<1 then FSimulationFrequency:=1;
      FTimeToNextUpdate:=0;
   end;
end;

// SetMask
//
procedure TGLWaterPlane.SetMask(val : TGLPicture);
begin
   FMask.Assign(val);
end;

// DoMaskChanged
//
procedure TGLWaterPlane.DoMaskChanged(Sender : TObject);
begin
   Reset;
   StructureChanged;
end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
initialization
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

   RegisterClasses([TGLWaterPlane]);

end.
