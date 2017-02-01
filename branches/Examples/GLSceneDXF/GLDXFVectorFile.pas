{:   GLDXFVectorFile<p>

  DXF Vector File related objects for GLScene<p>

  <b>History :</b><font size=-1><ul>
    <li>08/02/03 - DA - Added a New() method creating a new DXF
    <li>26/01/03 - DA - Added an accessor to the renderer
    <li>25/01/03 - DA - Added the DXF file as a property of the TGLDXFFile object
    <li>18/01/03 - DA - Better function to draw arcs
    <li>12/01/03 - DA - Now drawing arc elements in polylines
    <li>07/12/02 - SJ - Draw insert
    <li>06/12/02 - SJ - Recentered text
    <li>05/12/02 - DA - Draw text
    <li>04/12/02 - DA - Now frees correctly objects
    <li>03/12/02 - DA - Now correctly draw closed polylines  
    <li>02/12/02 - DA - Polylines drawing
    <li>31/11/02 - DA - Minor fixe with arcs having an end angle lower than their start angle
    <li>30/11/02 - DA - Center the drawing on the sreen, Draw Points
    <li>29/11/02 - DA - arcs are now correctly drawing
    <li>27/11/02 - DA - Quick and dirty hack to draw Lines and arcs
    <li>05/10/02 - DA - Unit creation
  </ul></font>
}
unit GLDXFVectorFile;

interface

uses
  Classes,
  WinProcs, // OutputDebugString()
  SysUtils, // Format()

  GLDXFRenderer,
  FileDXF,
  TypesDXF,

  GLScene,
  GLTexture,
  GLWindowsFont,
  GLBitmapFont;

type

  // TDXFProgressFunc
  //
  {: Function to call when loading or saving from/to DXF }
  TDXFProgressFunc = procedure(const Msg: String; ActualPosition,
    PositionMax: Longint) of object;

   // TGLDXFFile
   //
   {: The DXF  file. }
  TGLDXFFile = class
  private
    FOnProgress : TDXFProgressFunc;
    FDXFReader : TFileDXF;
    FDirectOpenGL: TGLDirectOpenGL;
    FBitmapFont: TGLWindowsBitmapFont; // used for opengl text
    FGLDXFRenderer: TGLDXFRenderer;

    //: concert DXF objects to GLScene
    procedure DXFtoGLObjects;

    //: Frees objects
    procedure FreeObjects;

    //: re-new the DXF
    procedure ReNewDXF;

  public
    { Public Declarations }
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromStream(aStream: TStream);

    //: call the load/save progress function
    procedure DoProgress(const AMsg: string; APos, AMax: LongInt);

    //: create a blank new DXF
    procedure NewDXF;

    //: direct opengl object
    property DirectOpenGL: TGLDirectOpenGL read FDirectOpenGL;

    //: renderer
    property Renderer: TGLDXFRenderer read FGLDXFRenderer;

    //: the DXF file
    property DXF: TFileDXF read FDXFReader;

  published
    //: function called on load/save progress
    property OnProgress: TDXFProgressFunc read FOnProgress write FOnProgress;
  end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
implementation
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

uses
  ObjectsDXF,
  GLUtils;

{ TGLDXFFile }

// Create
//
constructor TGLDXFFile.Create;
begin
  // create the opengl renderer
  FGLDXFRenderer := TGLDXFRenderer.Create;
end;

// Destroy
//
destructor TGLDXFFile.Destroy;
begin
  FreeObjects;
  FGLDXFRenderer.Free;
  inherited;
end;

// DoProgress
//
procedure TGLDXFFile.DoProgress(const AMsg: string; APos, AMax: Integer);
begin
  if Assigned(FOnProgress) then FOnProgress(AMsg, APos, AMax);
end;

// DXFtoGLObjects
//
procedure TGLDXFFile.DXFtoGLObjects;
var
  CntEntity : Integer;

  procedure CreateTextObject(Text: TDXFText);
  var
    glText: TGLFlatText;
  begin
    // use the bitmap font
    if not Assigned(FBitmapFont) then FBitmapFont := TGLWindowsBitmapFont.Create(nil);
    FBitmapFont.MinFilter := miLinear;
    FBitmapFont.MagFilter := maLinear;

    // create a new text
    glText := TGLFlatText.Create(nil);

    // configure the text
    with Text do begin
      glText.BitmapFont := FBitmapFont;
      glText.Text := Value;
      glText.Position.SetPoint(Primary.X, Primary.Y+(Height*1.5), Primary.Z);
      glText.RollAngle := Rotation;
      //glText.Scale.Scale(0.12 * Height);
      glText.Scale.Scale(Height * 10 / FBitmapFont.FontTextureWidth);
      { TODO : supports text color }
      {
      glText.Material.FrontProperties.Diffuse.Red := Color[0];
      glText.Material.FrontProperties.Diffuse.Green := Color[1];
      glText.Material.FrontProperties.Diffuse.Blue := Color[2];
      }
      //OutputDebugString(PAnsiChar('Text Heigth='+FloatToStr(Height)));
    end;

    // add it to the direct opengl object
    FDirectOpenGL.AddChild(glText);
  end;

begin
  with FDXFReader.Entities do
    for CntEntity := 0 to Count - 1 do
      if Entity[CntEntity] is TDXFText then CreateTextObject(TDXFText(Entity[CntEntity])) else
      ;
end;

// FreeObjects
//
procedure TGLDXFFile.FreeObjects;
begin
  // destroy a previous reader
  if Assigned(FDXFReader) then FreeAndNil(FDXFReader);
  // destroy opengl objects
  if Assigned(FDirectOpenGL) then FreeAndNil(FDirectOpenGL);
  if Assigned(FBitmapFont) then FreeAndNil(FBitmapFont);
end;

// LoadFromStream
//
procedure TGLDXFFile.LoadFromStream(aStream: TStream);
begin
  inherited;

  // re-create the DXF
  ReNewDXF;
  // read the DXF file
  FDXFReader.LoadFromStream(aStream);
  // convert to opengl
  DXFtoGLObjects;
end;

// NewDXF
//
procedure TGLDXFFile.NewDXF;
begin
  ReNewDXF;
  DXF.Layers.AddLayer(TDXFLayer.Create);
end;

// ReNewDXF
//
procedure TGLDXFFile.ReNewDXF;
begin
  FreeObjects;

  // create a new reader
  FDXFReader := TFileDXF.Create;
  FDXFReader.OnProgress := DoProgress;
  
  // set some properties of the renderer
  FGLDXFRenderer.Header := FDXFReader.Header;
  FGLDXFRenderer.Entities := FDXFReader.Entities;

  // create the direct opengl object
  FDirectOpenGL := TGLDirectOpenGL.Create(nil);
  FDirectOpenGL.OnRender := FGLDXFRenderer.Render;
end;

end.
