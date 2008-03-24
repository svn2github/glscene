{: Demo for Occlusion Querying (also includes timer query).<p>

   Occlusion querying is useful for counting how many pixels (or samples) of an
   object are visible on screen.<p>

   This demo renders a few objects normally, then queries how many pixels are
   visible of the objects rendered between the start of the query and the
   end of the query (the objects contained inside dcTestObjects dummycube).<p>

   Any objects rendered after the query has finished won't be included in the
   results.<p>

   A timer query is also included to see how long it takes to render the same
   objects.
}
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLGeomObjects, GLObjects, GLMisc, GLCadencer, GLWin32Viewer, GLTexture, opengl1x,
  StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLCube1: TGLCube;
    GLCylinder1: TGLCylinder;
    GLDummyCube1: TGLDummyCube;
    OGLBeginQueries: TGLDirectOpenGL;
    dcTestObjects: TGLDummyCube;
    OGLEndQueries: TGLDirectOpenGL;
    GLTorus1: TGLTorus;
    GLLightSource1: TGLLightSource;
    GLDummyCube2: TGLDummyCube;
    GLCube2: TGLCube;
    Timer1: TTimer;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    GLCone1: TGLCone;
    procedure OGLBeginQueriesRender(Sender: TObject;
      var rci: TRenderContextInfo);
    procedure OGLEndQueriesRender(Sender: TObject;
      var rci: TRenderContextInfo);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

const
  NUM_QUERIES = 2;

var
  Form1: TForm1;
  queries: array[0..NUM_QUERIES-1] of TGLuint;
  available:TGLint;

  // (can use TGLuint64EXT if using Delphi 7+, and require time queries > approx. 4 seconds)
  // since time is measured in nanoseconds (1E-9 s), a 32bit TGLuint can only measure
  // up to 4.3 seconds before it overflows
  //  (2^32)*(1E-9) = 4.2949....

  //timeTaken: TGLuint64EXT;
  timeTaken: TGLuint;
  samplesPassed: TGLint;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Occlusion queries are supported by extensions with lower version of OpenGL.
  // To use them, you'd need to check if GL_NV_occlusion_query or GL_ARB_occlusion_query
  // extensions are present, and makes the appropriate calls to the functions/procedures
  // they provide.
  if (not GL_VERSION_1_5) then
  begin
    Messagedlg('Requires at least OpenGL version 1.5 to run', mtError, [mbOK],0);
    Application.Terminate;
  end;
  // Generate the queries
  glGenQueries(NUM_QUERIES,@queries);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  // Delete the queries
  glDeleteQueries(NUM_QUERIES,@queries);
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  // Move some of the scene objects around
  GLDummyCube1.Position.X:=Sin(newTime);
  dcTestObjects.Turn(DeltaTime*50);
  dcTestObjects.Position.z:=2*Sin(newTime);
  GLDummyCube2.Position.X:=-sin(newTime);
end;

procedure TForm1.OGLBeginQueriesRender(Sender: TObject;
  var rci: TRenderContextInfo);
begin
  // Begin the timer + occlusion queries
  if GL_EXT_timer_query then
    glBeginQuery(GL_TIME_ELAPSED_EXT, queries[0]);
  glBeginQuery(GL_SAMPLES_PASSED, queries[1]);
end;

procedure TForm1.OGLEndQueriesRender(Sender: TObject;
  var rci: TRenderContextInfo);
begin
  // End the timer + occlusion queries
  glEndQuery(GL_SAMPLES_PASSED);
  if GL_EXT_timer_query then
    glEndQuery(GL_TIME_ELAPSED_EXT);

  // Most of the frame rate is lost waiting for results to become available
  //  + updating the captions every frame, but as this is a demo, we want to
  // see what is going on.

  available:=0;
  while available=0 do
    glGetQueryObjectiv(queries[1], GL_QUERY_RESULT_AVAILABLE, @available);
  glGetQueryObjectiv(queries[1], GL_QUERY_RESULT, @samplesPassed);

  if GL_EXT_timer_query then
  begin
    available:=0;
    while available=0 do
      glGetQueryObjectiv(queries[0], GL_QUERY_RESULT_AVAILABLE, @available);
    glGetQueryObjectiv(queries[0], GL_QUERY_RESULT, @timeTaken);
  // Use this line instead of the one above to use 64 bit timer (requires Delphi 7+)
//  glGetQueryObjectui64vEXT(queries[0], GL_QUERY_RESULT, @timeTaken);
  end;

  label2.caption:='Number of test pixels visible: ' + IntToStr(samplesPassed);
  if samplesPassed=0 then
    label3.Visible:=true
  else
    label3.Visible:=false;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  // Convert time taken from ns => ms & display
  if GL_EXT_timer_query then
    label1.caption:='Time taken: '+ FloatToSTr(timeTaken/1000000) +' ms'
  else
    label1.Caption:='Time query unavailable, requires GL_EXT_timer_query';

  caption := GLSceneViewer1.FramesPerSecondText(0);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

end.
