program sdl_test;

{$APPTYPE gui}

uses Forms, GLSCene, GLSDLContext, SDL, GLTeapot {$IFDEF FPC}, Interfaces{$ENDIF};

var
   sdlViewer : TGLSDLViewer;
   scene : TGLScene;
   camera : TGLCamera;
   teapot : TGLTeapot;
   light : TGLLightSource;
begin
   Application.Initialize;

   scene:=TGLScene.Create(nil);

   teapot:=TGLTeapot(scene.Objects.AddNewChild(TGLTeapot));

   light:=TGLLightSource(scene.Objects.AddNewChild(TGLLightSource));
   light.Position.SetPoint(10, 15, 20);

   camera:=TGLCamera(scene.Cameras.AddNewChild(TGLCamera));
   camera.TargetObject:=teapot;
   camera.SceneScale:=4;
   camera.Position.SetPoint(4, 2, 1);

   sdlViewer:=TGLSDLViewer.Create(nil);
   sdlViewer.Camera:=camera;

   sdlViewer.Render;
   while sdlViewer.Active do begin
      // Message queue is not operational, but there may still be some messages
      Forms.Application.ProcessMessages;
      // Relinquish some of that CPU time
      SDL_Delay(1);
      // Slowly rotate the teapot
//      Teapot1.RollAngle:=4*Frac(Now*24)*3600;
   end;

   sdlViewer.Free;
   
   scene.Free;
end.
