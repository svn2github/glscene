unit GLSkyBox;

interface


   {
    NOTE : TO USE IT IN THE IDE YOU MUST ADD THE FOLLOWING LINES IN "GLSCEREGISTER.PAS":

1)  Uses ... GLSkyBox;
2)   ObjectManager.RegisterSceneObject(TGLSkyBox, 'GLSkyBox', 'Environment Objects');
   }



{: GLSkyBox<p>

   A TGLImmaterialSceneObject drawing 6 quads (plus another quad as "Cloud" plane) for use as a skybox always centered on the RCI.camera position.<p>

	<b>History : </b><font size=-1><ul>
        <li>09/11/2003 - MRQZZZ - mandatory changes suggested by Eric.
       <li>02/09/2003 - MRQZZZ - Creation
}


Uses
    Classes,GLScene,GLTexture,VectorTypes,VectorGeometry,OpenGL1X,GLMisc;

type TGLSkyBox = class(TGLImmaterialSceneObject)
  private
    FMatNameTop: string;
    FMatNameRight: string;
    FMatNameFront: string;
    FMatNameLeft: string;
    FMatNameBack: string;
    FMatNameBottom: string;
    FMaterialLibrary: TGLMaterialLibrary;
    FMatNameClouds: string;
    FCloudsPlaneOffset: single;
    FCloudsPlaneSize: single;
    procedure SetMaterialLibrary(const Value: TGLMaterialLibrary);
    procedure SetMatNameBack(const Value: string);
    procedure SetMatNameBottom(const Value: string);
    procedure SetMatNameFront(const Value: string);
    procedure SetMatNameLeft(const Value: string);
    procedure SetMatNameRight(const Value: string);
    procedure SetMatNameTop(const Value: string);
    procedure SetMatNameClouds(const Value: string);
    procedure SetCloudsPlaneOffset(const Value: single);
    procedure SetCloudsPlaneSize(const Value: single);
  public
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
    procedure DoRender(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property MaterialLibrary : TGLMaterialLibrary read FMaterialLibrary write SetMaterialLibrary;
    property MatNameTop : string read FMatNameTop write SetMatNameTop;
    property MatNameBottom : string read FMatNameBottom write SetMatNameBottom;
    property MatNameLeft : string read FMatNameLeft write SetMatNameLeft;
    property MatNameRight : string read FMatNameRight write SetMatNameRight;
    property MatNameFront : string read FMatNameFront write SetMatNameFront;
    property MatNameBack : string read FMatNameBack write SetMatNameBack;
    property MatNameClouds : string read FMatNameClouds write SetMatNameClouds;
    property CloudsPlaneOffset : single read FCloudsPlaneOffset write SetCloudsPlaneOffset;
    property CloudsPlaneSize : single read FCloudsPlaneSize write SetCloudsPlaneSize;
end;


implementation


{ TGLSkyBox }

procedure TGLSkyBox.DoRender(var rci : TRenderContextInfo;
                       renderSelf, renderChildren : Boolean);
var
   dV : TVector4F;
   m0,m1,m2: single;
   p0,p1,p2: single;

   cm0,cm2: single;
   cp0,cp2: single;
   cps,cof1 : single;

   IsDepthTestSet : boolean;
begin
        if FMaterialLibrary<>nil then
        begin
             try
                dV := Self.AbsoluteToLocal(rci.cameraPosition);

                // pre-calculate 6 possible values to speed up
                m0 := -0.5+dV[0];
                m1 := -0.5+dV[1];
                m2 := -0.5+dV[2];
                p0 := 0.5+dV[0];
                p1 := 0.5+dV[1];
                p2 := 0.5+dV[2];


                // paint over
                glDepthMask(false);



                // store current stDepthTest state and disable it here
                IsDepthTestSet := stDepthTest in rci.currentStates;
                if IsDepthTestSet then
                   glDisable(GL_DEPTH_TEST);


                // TOP
                if FMatNameTop<>'' then
                begin

                     FMaterialLibrary.ApplyMaterial(FMatNameTop, rci);
                     glBegin(GL_QUADS);
                        glTexCoord2f(0.002, 0.998);  glVertex3f(m0, p1, p2);
                        glTexCoord2f(0.002, 0.002);  glVertex3f(m0, p1, m2);
                        glTexCoord2f(0.998, 0.002);  glVertex3f(p0, p1, m2);
                        glTexCoord2f(0.998, 0.998);  glVertex3f(p0, p1, p2);
                     glEnd;
                     FMaterialLibrary.UnApplyMaterial(rci);

                end;

                // BOTTOM
                if FMatNameBottom<>'' then
                begin

                     MaterialLibrary.ApplyMaterial(FMatNameBottom, rci);
                     glBegin(GL_QUADS);
                        glTexCoord2f(0.002, 0.998);  glVertex3f(m0, m1, m2);
                        glTexCoord2f(0.002, 0.002);  glVertex3f(m0, m1, p2);
                        glTexCoord2f(0.998, 0.002);  glVertex3f(p0, m1, p2);
                        glTexCoord2f(0.998, 0.998);  glVertex3f(p0, m1, m2);
                     glEnd;
                     FMaterialLibrary.UnApplyMaterial(rci);

                end;

                // LEFT
                if FMatNameLeft<>'' then
                begin

                     MaterialLibrary.ApplyMaterial(FMatNameLeft, rci);
                     glBegin(GL_QUADS);
                        glTexCoord2f(0.002, 0.998);  glVertex3f(m0, p1, p2);
                        glTexCoord2f(0.002, 0.002);  glVertex3f(m0, m1, p2);
                        glTexCoord2f(0.998, 0.002);  glVertex3f(m0, m1, m2);
                        glTexCoord2f(0.998, 0.998);  glVertex3f(m0, p1, m2);
                     glEnd;
                     FMaterialLibrary.UnApplyMaterial(rci);

                end;

                // RIGHT
                if FMatNameRight<>'' then
                begin

                     MaterialLibrary.ApplyMaterial(FMatNameRight, rci);
                     glBegin(GL_QUADS);
                        glTexCoord2f(0.002, 0.998);  glVertex3f(p0, p1, m2);
                        glTexCoord2f(0.002, 0.002);  glVertex3f(p0, m1, m2);
                        glTexCoord2f(0.998, 0.002);  glVertex3f(p0, m1, p2);
                        glTexCoord2f(0.998, 0.998);  glVertex3f(p0, p1, p2);
                     glEnd;
                     FMaterialLibrary.UnApplyMaterial(rci);

                end;

                // FRONT
                if FMatNameFront<>'' then
                begin

                     MaterialLibrary.ApplyMaterial(FMatNameFront, rci);
                     glBegin(GL_QUADS);

                        glTexCoord2f(0.002, 0.998);  glVertex3f(m0, p1, m2);
                        glTexCoord2f(0.002, 0.002);  glVertex3f(m0, m1, m2);
                        glTexCoord2f(0.998, 0.002);  glVertex3f(p0, m1, m2);
                        glTexCoord2f(0.998, 0.998);  glVertex3f(p0, p1, m2);
                     glEnd;
                     FMaterialLibrary.UnApplyMaterial(rci);

                end;

                // BACK
                if FMatNameBack<>'' then
                begin

                     MaterialLibrary.ApplyMaterial(FMatNameBack, rci);
                     glBegin(GL_QUADS);

                        glTexCoord2f(0.002, 0.998);  glVertex3f(p0, p1, p2);
                        glTexCoord2f(0.002, 0.002);  glVertex3f(p0, m1, p2);
                        glTexCoord2f(0.998, 0.002);  glVertex3f(m0, m1, p2);
                        glTexCoord2f(0.998, 0.998);  glVertex3f(m0, p1, p2);
                     glEnd;
                     FMaterialLibrary.UnApplyMaterial(rci);

                end;

                // CLOUDS CAP PLANE
                if FMatNameClouds<>'' then
                begin

                     // pre-calculate possible values to speed up
                     cps := FCloudsPlaneSize*0.5;
                     cm0 := dv[0]-cps;
                     cm2 := dv[2]-cps;
                     cp0 := dv[0]+(cps);
                     cp2 := dv[2]+cps;
                     cof1 := dv[1]+FCloudsPlaneOffset;

                     FMaterialLibrary.ApplyMaterial(FMatNameClouds, rci);
                     glBegin(GL_QUADS);

                        glTexCoord2f(0,1);  glVertex3f(cm0, cof1, cp2);
                        glTexCoord2f(0,0);  glVertex3f(cm0, cof1, cm2);
                        glTexCoord2f(1,0);  glVertex3f(cp0, cof1, cm2);
                        glTexCoord2f(1,1);  glVertex3f(cp0, cof1, cp2);
                     glEnd;
                     FMaterialLibrary.UnApplyMaterial(rci);

                end;
                // process childs
                if Count>0 then
                begin
                     // Translate children to be rendered always centered on the skybox
                     glTranslatef(rci.cameraPosition[0], rci.cameraPosition[1], rci.cameraPosition[2]);
                     Self.RenderChildren(0, Count-1, rci);
                end;

                glDepthMask(true); // restore

             finally

                // restore original Depth Test
                if IsDepthTestSet then
                   glEnable(GL_DEPTH_TEST);

             end;
     end;
end;

constructor TGLSkyBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ObjectStyle:=ObjectStyle+[osDirectDraw, osNoVisibilityCulling];
  FCloudsPlaneOffset := 0.2; // this should be set far enouh to avoid near plane clipping
  FCloudsPlaneSize  := 32;   // the bigger, the more this extends the clouds cap to the horizon
end;

destructor TGLSkyBox.Destroy;
begin
     inherited;
end;

procedure TGLSkyBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
   if Operation=opRemove then
   begin
        if AComponent=FMaterialLibrary then
           MaterialLibrary:=nil;
   end;
  inherited;
end;



procedure TGLSkyBox.SetCloudsPlaneOffset(const Value: single);
begin
  FCloudsPlaneOffset := Value;
  NotifyChange(Self);
end;

procedure TGLSkyBox.SetCloudsPlaneSize(const Value: single);
begin
  FCloudsPlaneSize := Value;
  NotifyChange(Self);
end;

procedure TGLSkyBox.SetMaterialLibrary(
  const Value: TGLMaterialLibrary);
begin
  FMaterialLibrary := Value;
  NotifyChange(Self);
end;

procedure TGLSkyBox.SetMatNameBack(const Value: string);
begin
  FMatNameBack := Value;
  NotifyChange(Self);
end;

procedure TGLSkyBox.SetMatNameBottom(const Value: string);
begin
  FMatNameBottom := Value;
  NotifyChange(Self);
end;

procedure TGLSkyBox.SetMatNameClouds(const Value: string);
begin
  FMatNameClouds := Value;
  NotifyChange(Self);
end;

procedure TGLSkyBox.SetMatNameFront(const Value: string);
begin
  FMatNameFront := Value;
  NotifyChange(Self);
end;

procedure TGLSkyBox.SetMatNameLeft(const Value: string);
begin
  FMatNameLeft := Value;
  NotifyChange(Self);
end;

procedure TGLSkyBox.SetMatNameRight(const Value: string);
begin
  FMatNameRight := Value;
  NotifyChange(Self);
end;

procedure TGLSkyBox.SetMatNameTop(const Value: string);
begin
  FMatNameTop := Value;
  NotifyChange(Self);
end;

initialization
              RegisterClass(TGLSkyBox);

end.
