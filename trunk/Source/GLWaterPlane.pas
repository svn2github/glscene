// GLWaterPlane
{: A plane simulating animated water<p>

	<b>History : </b><font size=-1><ul>
      <li>14/11/03 - Mrqzzz - Tried "CreateRippleAtWorldPos" to work at any position/rotation, but need expert's help.. :(
      <li>13/11/03 - Mrqzzz - Tried to add timing indipendence (quite not precise yet)
      <li>12/11/03 - Mrqzzz - Added some properties & small optims added
      <li>1/1/03 - Sternas Stefanos - Original code
   </ul></font>

  "  "

   The Original Code is part of Cosmos4D
   http://users.hol.gr/~sternas/
   Sternas Stefanos 2003
}
unit GLWaterPlane;

interface

uses Classes, Types, Vectortypes, VectorGeometry, GLScene, GLTexture, GLMisc, OpenGL1x;

type

   TGLWaterPlane = class (TGLSceneObject)
		private
         fRainTimeInterval:integer;
         fRainForce:single;
         FViscosity: single;
         fActive:Boolean;
         FMaxWaveAmp: integer;
         FElastic: single;
         FSurfaceWave: Single;
         FResolution: integer;
         ResolutionInv : single;
         FSimulationFrequency, FTimeToNextUpdate : Single;
         FTimeToNextRainDrop : Single;

      private
         procedure SetMaxWaveAmp(const Value: integer);
         procedure SetElastic(const Value: single);
         procedure SetSurfaceWave(const Value: Single);
         procedure SetResolution(const Value: integer);

         procedure InitResolution(Res: integer);

      protected
         Positions : array of array of Single;
         Velocity : array of array of Single;
         Vertex : array of array of TAffineVector;
         Normals : array of array of TAffineVector;
         SinCache, CosCache : array of Single;

         procedure SetRainTimeInterval(Const val:integer);
         procedure SetViscosity(Const val:single);
         procedure SetRainForce(Const val:single);
         procedure SetSimulationFrequency(const val : Single);
         procedure InitWater;
         procedure BuildData;

      public
         constructor Create(AOwner: TComponent); override;
         procedure DoProgress(const progressTime : TProgressTimes); override;
         procedure BuildList(var rci : TRenderContextInfo); override;
         procedure Assign(Source: TPersistent); override;
         procedure CreateRippleAtGridPos(X,Y:integer);
         procedure CreateRippleAtWorldPos(X,Y,Z:single);
         procedure CreateRippleRandom;
         procedure Reset;

      published
         property Active : Boolean read FActive write FActive default True;
         property RainTimeInterval : Integer read FRainTimeInterval write SetRainTimeInterval default 500;
                   //Value at Miliseconds from 0 to +1000000 , if value=0 then no rain
         property RainForce : Single read FRainForce write SetRainForce;
                   //Value at Miliseconds from 0 to +1000000

         property Viscosity : Single read FViscosity write SetViscosity ;//Value from 0 to +1
         property MaxWaveAmp : Integer read FMaxWaveAmp write SetMaxWaveAmp;
         property Elastic : Single read FElastic write SetElastic;
         property SurfaceWave : Single read FSurfaceWave write SetSurfaceWave;
         property Resolution : Integer read FResolution write SetResolution default 64;

         property SimulationFrequency : Single read FSimulationFrequency write SetSimulationFrequency;
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
constructor TGLWaterPlane.Create(AOwner:Tcomponent);
begin
   inherited Create(AOwner);
   ObjectStyle:=ObjectStyle+[osDirectDraw];
   FMaxWaveAmp := 900000;
   FElastic := 10;
   FSurfaceWave := 1;
   FActive:=True;
   fRainTimeInterval:=500;
   fRainForce:=5000;
   FViscosity:=0.99;
   FResolution:=64;
   Resolutioninv:=1/FResolution;
   FSimulationFrequency:=100; // 100 Hz
   InitResolution(FResolution);
   InitWater;
end;

procedure TGLWaterPlane.DoProgress(const progressTime : TProgressTimes);
begin
   inherited;
   if (fActive=false) or (visible=false) then exit;

   FTimeToNextRainDrop:=FTimeToNextRainDrop-progressTime.deltaTime;
   while FTimeToNextRainDrop<=0 do begin
      CreateRippleRandom;
      FTimeToNextRainDrop:=FTimeToNextRainDrop+fRainTimeInterval*0.001;
   end;

   FTimeToNextUpdate:=FTimeToNextUpdate-progressTime.deltaTime;
   while FTimeToNextUpdate<=0 do begin
      BuildData;
      FTimeToNextUpdate:=FTimeToNextUpdate+1/FSimulationFrequency;
      StructureChanged;
   end;
end;

procedure TGLWaterPlane.InitWater;
begin
   BuildData;
end;

procedure TGLWaterPlane.SetRainTimeInterval(Const val:integer);
begin
   if (val>=0) and (Val<=1000000) then
      fRainTimeInterval:=val;
end;

Procedure TGLWaterPlane.SetViscosity(Const val:single);
 begin
   if (val>=0) and (Val<=1) then
    fViscosity:=val;
 end;

Procedure TGLWaterPlane.SetRainForce(Const val:single);
  begin
   if (val>=0) and (Val<=1000000) then
    fRainForce:=val;
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


procedure TGLWaterPlane.CreateRippleAtGridPos(X,Y: integer);
begin
  if (X>=0) and (Y>=0) and (x<=Resolution) and (Y<=Resolution) then
   Velocity[x, y] :=fRainForce;
end;

procedure TGLWaterPlane.CreateRippleAtWorldPos(X,Y,Z:single);
 var
    vv : tvector;
    ax,ay:integer;
    Sc : TVector3F;

    procedure CalcAbsScale(O:TGLBaseSceneObject);
    begin
         Sc[0] := Sc[0] * O.Scale.X;
         Sc[1] := Sc[1] * O.Scale.Y;
         Sc[2] := Sc[2] * O.Scale.Z;
         if O<>Scene.Objects then
         begin
              O := O.Parent;
              CalcAbsScale(O);
         end;
    end;

 begin
     vv:=vectormake(x,y,z,0);

     vV := vectorSubtract(vv,Self.AbsolutePosition);

     sc := Self.Scale.AsAffineVector;
     CalcAbsScale(Self.Parent);

     //CellsizeInv := Resolution/(sc[0]);

     vv[0] := vv[0] * Resolution/(sc[0]);
     vv[1] := vv[1] * Resolution/(sc[1]);
     vv[2] := vv[2] * Resolution/(sc[2]);


     ax:= round((vv[0])  + (Resolution * 0.5));
     aY:= round((vv[2])  + (Resolution * 0.5));
     CreateRippleAtGridPos(ax,ay);

 end;

procedure TGLWaterPlane.CreateRippleRandom;
begin
   Velocity[random(Resolution-3)+2, random(Resolution-3)+2] :=fRainForce;
end;


Procedure TGLWaterPlane.BuildData;
var
   I, J : Integer;
   VectLength : single;
   f1,f2 : Single;
   tkf : Single;
   coeff : Single;
begin
   if not Visible then Exit;

   coeff := 0.01;

   f1 := coeff*5; //(self.Scale.X / Resolution) / 250;
   f2 := coeff*FElastic; //(self.Scale.X / Resolution) / 70;

  // Calculate new velocity
  for I := 2 to Resolution - 2 do
    for J := 2 to Resolution - 2 do
      Velocity[I, J] := Velocity[I, J] + (Positions[I, J] -
        (4 * (Positions[I - 1, J] + Positions[I + 1, J] + Positions[I, J - 1] + Positions[I, J + 1]) + // left, right, above, below
        Positions[I - 1, J - 1] + Positions[I + 1, J - 1] + Positions[I - 1, J + 1] + Positions[I + 1, J + 1]) * f1) * f2; // diagonally across
  // Calculate the new ripple positions
  coeff := 0.02;

  coeff := coeff * Resolution;
  for I := 2 to Resolution - 2 do
    for J := 2 to Resolution - 2 do
    begin
      Positions[I, J] := Positions[I, J] - (Velocity[I, J] * coeff);
      Velocity[I, J] := Velocity[I, J] * FViscosity;
      while abs(Positions[I, J])>FMaxWaveAmp do
         Positions[I, J] := Positions[I, J] * 0.8;
    end;
  // Calculate the new vertex coordinates
  tkf := 1;//Sin(newTime*FSurfaceWave); // MRQZZZ for horizontal texture stretching effect
  for I := 0 to Resolution do
    for J := 0 to Resolution do
    begin
      Vertex[I, J][0] := ((I - Resolution * 0.5) * ResolutionInv )+(SinCache[I]*tkf*0.005);
      Vertex[I, J][1] := ((Positions[I, J] * 0.0009765625) * ResolutionInv );
      Vertex[I, J][2] := ((J - Resolution * 0.5) * ResolutionInv)+(CosCache[J]*tkf*0.005);
    end;
  // Calculate the new vertex normals.
  // Do this by using the points to each side to get the right angle
  for I := 0 to Resolution do
  begin
    for J := 0 to Resolution do
    begin
      if (I > 0) and (J > 0) and (I < Resolution) and (J < Resolution) then
      begin
          Normals[I, J][0] := Positions[I + 1, J] - Positions[I - 1, J];
          Normals[I, J][1] := -2048;
          Normals[I, J][2] := Positions[I, J + 1] - Positions[I, J - 1];
          VectLength :=VectorLength(Normals[I, J]);

          if VectLength <> 0 then
            Normals[I, J]:=VectorScale(Normals[I, J],-1/VectLength);
      end
      else
      begin
        Normals[I, J]:=YVector;
      end;
    end;
  end;

 end;

procedure TGLWaterPlane.BuildList(var rci : TRenderContextInfo);
var
   i, j : Integer;
begin
   for J:=Resolution-1 downto 0 do begin
      glBegin(GL_QUAD_STRIP);
      for I:=Resolution downto 1 do begin
         glTexCoord2f(i*ResolutionInv, (j+1)*ResolutionInv);
         glNormal3fv(@Normals[I, J+1]);
         glVertex3fv(@Vertex[I, J+1]);

         glTexCoord2f(i*ResolutionInv, j*ResolutionInv);
         glNormal3fv(@Normals[I, J]);
         glVertex3fv(@Vertex[I, J]);
      end;
      glEnd;
   end;
end;

procedure TGLWaterPlane.Assign(Source: TPersistent);
begin
   if Assigned(Source) and (Source is TGLWaterPlane) then
   begin
      Active:=TGLWaterPlane(Source).Active;
      RainTimeInterval:=TGLWaterPlane(Source).RainTimeInterval;
      RainForce:=TGLWaterPlane(Source).RainForce;
      Viscosity:=TGLWaterPlane(Source).Viscosity;
   end;
   inherited Assign(Source);
end;

//==========================================================

procedure TGLWaterPlane.SetMaxWaveAmp(const Value: integer);
begin
  FMaxWaveAmp := Value;
end;

procedure TGLWaterPlane.SetElastic(const Value: single);
begin
  FElastic := Value;
end;

procedure TGLWaterPlane.SetSurfaceWave(const Value: Single);
begin
     FSurfaceWave := Value;
end;

procedure TGLWaterPlane.SetResolution(const Value: integer);
begin
  FResolution := Value;
  ResolutionInv := 1/FResolution;

  InitResolution(FResolution);
end;


procedure TGLWaterPlane.InitResolution(Res: integer);
var
   t : integer;
begin
   SetLength(Positions, Res+1);    for t := 0 to Res do SetLength(Positions[t],Res+1);
   SetLength(Velocity, Res+1);     for t := 0 to Res do SetLength(Velocity[t],Res+1);
   SetLength(Vertex, Res+1);       for t := 0 to Res do SetLength(Vertex[t],Res+1);
   SetLength(Normals, Res+1);      for t := 0 to Res do SetLength(Normals[t],Res+1);

   SetLength(SinCache,Res+1);
   SetLength(CosCache,Res+1);
   PrepareSinCosCache(SinCache, CosCache, 0, 2000);
   BuildData;

   StructureChanged;
end;

procedure TGLWaterPlane.Reset;
var
   t,u: integer;
begin
     for t := 0 to FResolution do
         for u := 0 to FResolution do
         begin
              Positions[t,u] := 0;
              Velocity[t,u] :=0;
         end;
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
