//
// VKScene Component Library, based on GLScene http://glscene.sourceforge.net
//
{
  OpenGL for FireMonkey
  Adapted from https://github.com/LUXOPHIA
}

unit VKS.FMXOpenGL;

interface

uses
  Winapi.Windows,
  Winapi.OpenGL,
  Winapi.OpenGLext,
  FMX.Forms;

type
  TFMXOpenGL = class
  private
    _Form: TCommonCustomForm;
    _WND: HWND;
    _DC: HDC;
  protected
    _PFD: TPixelFormatDescriptor;
    _PFI: Integer;
    _RC: HGLRC;
    procedure SetPFD(const PFD_: TPixelFormatDescriptor);
    procedure SetPFI(const PFI_: Integer);
    procedure CreateWindow;
    procedure DestroyWindow;
    procedure ValidatePFD(const PFD_: TPixelFormatDescriptor);
    procedure ValidatePFI(const PFI_: Integer);
    procedure CreateDC;
    procedure DestroyDC;
    procedure CreateRC;
    procedure DestroyRC;
  public
    constructor Create;
    destructor Destroy; override;
    property PFD: TPixelFormatDescriptor read _PFD write SetPFD;
    property PFI: Integer read _PFI write SetPFI;
    property RC: HGLRC read _RC;
    class function DefaultPFD: TPixelFormatDescriptor;
    procedure BeginGL;
    procedure EndGL;
    procedure InitOpenGL;
    procedure ApplyPixelFormat(const DC_: HDC);
  end;

//-----------------------------------------------------------------------
  TVKShader = class
  private
  protected
    _ID: GLuint;
  public
    constructor Create(const Kind_: GLenum);
    destructor Destroy; override;
    property ID: GLuint read _ID;
    procedure SetSource(const Source_: String);
  end;

//-----------------------------------------------------------------------

  TVKShaderV = class(TVKShader)
  private
  protected
  public
    constructor Create;
    destructor Destroy; override;
  end;

//-----------------------------------------------------------------------

  TVKShaderG = class(TVKShader)
  private
  protected
  public
    constructor Create;
    destructor Destroy; override;
  end;

//-----------------------------------------------------------------------

  TVKShaderF = class(TVKShader)
  private
  protected
  public
    constructor Create;
    destructor Destroy; override;
  end;

//-----------------------------------------------------------------------

  TVKProgram = class
  private
  protected
    _ID: GLuint;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Attach(const Shader_: TVKShader);
    procedure Detach(const Shader_: TVKShader);
    procedure Link;
    procedure Use;
  end;

//-----------------------------------------------------------------------

  TVKBuffer<_TYPE_: record > = class
  public type
    _PValue_ = ^_TYPE_;
  private
  protected
    _ID: GLuint;
    _Kind: GLenum;
    _Count: Integer;
    _Head: _PValue_;
    procedure SetCount(const Count_: Integer);
  public
    constructor Create(const Kind_: GLenum);
    destructor Destroy; override;
    property ID: GLuint read _ID;
    property Count: Integer read _Count write SetCount;
    procedure Bind;
    procedure Unbind;
    procedure Map;
    procedure Unmap;
  end;

//-----------------------------------------------------------------------

  TVKBufferV<_TYPE_: record > = class(TVKBuffer<_TYPE_>)
  private
  protected
  public
    constructor Create;
    destructor Destroy; override;
  end;

//-----------------------------------------------------------------------

  TVKBufferI<_TYPE_: record > = class(TVKBuffer<_TYPE_>)
  private
  protected
  public
    constructor Create;
    destructor Destroy; override;
  end;

//-----------------------------------------------------------------------

  TVKBufferU<_TYPE_: record > = class(TVKBuffer<_TYPE_>)
  private
  protected
  public
    constructor Create;
    destructor Destroy; override;
  end;

//-----------------------------------------------------------------------

  TVKArray = class
  private
  protected
    _ID: GLuint;
  public
    constructor Create;
    destructor Destroy; override;
    property ID: GLuint read _ID;
    procedure BeginBind;
    procedure EndBind;
  end;

var
  FMXOpenGL: TFMXOpenGL;

//-----------------------------------------------------------------------
implementation
//-----------------------------------------------------------------------

uses
  System.SysUtils,
  FMX.Platform.Win;

procedure TFMXOpenGL.SetPFD(const PFD_: TPixelFormatDescriptor);
begin
  DestroyRC;
  DestroyDC;
  CreateDC;
  ValidatePFD(PFD_);
  CreateRC;
end;

procedure TFMXOpenGL.SetPFI(const PFI_: Integer);
begin
  DestroyRC;
  DestroyDC;
  CreateDC;
  ValidatePFI(PFI_);
  CreateRC;
end;

// ------------------------------------------------------------------------------
procedure TFMXOpenGL.CreateWindow;
begin
  _Form := TCommonCustomForm.Create(nil);
  _WND := WindowHandleToPlatform(_Form.Handle).Wnd;
end;

procedure TFMXOpenGL.DestroyWindow;
begin
  _Form.Free;
end;

// ------------------------------------------------------------------------------

procedure TFMXOpenGL.ValidatePFD(const PFD_: TPixelFormatDescriptor);
var
  I: Integer;
begin
  _PFD := PFD_;
  I := ChoosePixelFormat(_DC, @_PFD);
  Assert(I > 0, 'Not found the PixelFormat with a close setting!');
  ValidatePFI(I);
end;

procedure TFMXOpenGL.ValidatePFI(const PFI_: Integer);
begin
  _PFI := PFI_;
  Assert(DescribePixelFormat(_DC, _PFI, SizeOf(TPixelFormatDescriptor), _PFD),
    'Not found the PixelFormat of the index!');
end;

// ------------------------------------------------------------------------------

procedure TFMXOpenGL.CreateDC;
begin
  _DC := GetDC(_WND);
end;

procedure TFMXOpenGL.DestroyDC;
begin
  ReleaseDC(0, _DC);
end;

// ------------------------------------------------------------------------------

procedure TFMXOpenGL.CreateRC;
begin
  ApplyPixelFormat(_DC);

  _RC := wglCreateContext(_DC);
end;

procedure TFMXOpenGL.DestroyRC;
begin
  wglDeleteContext(_RC);
end;

constructor TFMXOpenGL.Create;
begin
  inherited;
  CreateWindow;
  CreateDC;
  ValidatePFD(DefaultPFD);
  CreateRC;
  InitOpenGL;
end;

destructor TFMXOpenGL.Destroy;
begin
  DestroyRC;
  DestroyDC;
  DestroyWindow;
  inherited;
end;

// ------------------------------------------------------------------------------

class function TFMXOpenGL.DefaultPFD: TPixelFormatDescriptor;
begin
  with Result do
  begin
    nSize := SizeOf(TPixelFormatDescriptor);
    nVersion := 1;
    dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
    iPixelType := PFD_TYPE_RGBA;
    cColorBits := 24;
    cRedBits := 0;
    cRedShift := 0;
    cGreenBits := 0;
    cGreenShift := 0;
    cBlueBits := 0;
    cBlueShift := 0;
    cAlphaBits := 0;
    cAlphaShift := 0;
    cAccumBits := 0;
    cAccumRedBits := 0;
    cAccumGreenBits := 0;
    cAccumBlueBits := 0;
    cAccumAlphaBits := 0;
    cDepthBits := 32;
    cStencilBits := 0;
    cAuxBuffers := 0;
    iLayerType := PFD_MAIN_PLANE;
    bReserved := 0;
    dwLayerMask := 0;
    dwVisibleMask := 0;
    dwDamageMask := 0;
  end;
end;

// ------------------------------------------------------------------------------

procedure TFMXOpenGL.BeginGL;
begin
  wglMakeCurrent(_DC, _RC);
end;

procedure TFMXOpenGL.EndGL;
begin
  wglMakeCurrent(_DC, 0);
end;

// ------------------------------------------------------------------------------

procedure TFMXOpenGL.InitOpenGL;
begin
  BeginGL;
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_CULL_FACE);
  EndGL;
end;

// ------------------------------------------------------------------------------

procedure TFMXOpenGL.ApplyPixelFormat(const DC_: HDC);
begin
  Assert(SetPixelFormat(DC_, _PFI, @_PFD), 'SetPixelFormat() is failed!');
end;

// ------------------------------------------------------------------------------

constructor TVKShader.Create(const Kind_: GLenum);
begin
  inherited Create;

  _ID := glCreateShader(Kind_);
end;

destructor TVKShader.Destroy;
begin
  glDeleteShader(_ID);
  inherited;
end;

// ------------------------------------------------------------------------------

procedure TVKShader.SetSource(const Source_: String);
var
  P: PAnsiChar;
  N: GLint;
  E: GLint;
  Cs: array of PGLchar;
  CsN: GLsizei;
begin
  P := PAnsiChar(AnsiString(Source_));
  N := Length(Source_);
  glShaderSource(_ID, 1, @P, @N);
  glCompileShader(_ID);
  glGetShaderiv(_ID, GL_COMPILE_STATUS, @E);
  if E = GL_FALSE then
  begin
    glGetShaderiv(_ID, GL_INFO_LOG_LENGTH, @N);
    SetLength(Cs, N);
    glGetShaderInfoLog(_ID, N, @CsN, @Cs[0]);
    Assert(False, AnsiString(Cs));
  end;
end;

// ------------------------------------------------------------------------------

constructor TVKShaderV.Create;
begin
  inherited Create(GL_VERTEX_SHADER);
end;

destructor TVKShaderV.Destroy;
begin
  inherited;
end;

// ------------------------------------------------------------------------------

constructor TVKShaderG.Create;
begin
  inherited Create(GL_GEOMETRY_SHADER);
end;

destructor TVKShaderG.Destroy;
begin
  inherited;
end;

// ------------------------------------------------------------------------------

constructor TVKShaderF.Create;
begin
  inherited Create(GL_FRAGMENT_SHADER);
end;

destructor TVKShaderF.Destroy;
begin
  inherited;
end;

// ------------------------------------------------------------------------------

constructor TVKProgram.Create;
begin
  inherited;
  _ID := glCreateProgram;
end;

destructor TVKProgram.Destroy;
begin
  glDeleteProgram(_ID);
  inherited;
end;

// ------------------------------------------------------------------------------

procedure TVKProgram.Attach(const Shader_: TVKShader);
begin
  glAttachShader(_ID, Shader_.ID);
end;

procedure TVKProgram.Detach(const Shader_: TVKShader);
begin
  glDetachShader(_ID, Shader_.ID);
end;

// ------------------------------------------------------------------------------

procedure TVKProgram.Link;
begin
  glLinkProgram(_ID);
end;

// ------------------------------------------------------------------------------

procedure TVKProgram.Use;
begin
  glUseProgram(_ID);
end;

// ------------------------------------------------------------------------------

procedure TVKBuffer<_TYPE_>.SetCount(const Count_: Integer);
begin
  _Count := Count_;
  Bind;
  glBufferData(_Kind, SizeOf(_TYPE_) * _Count, nil, GL_DYNAMIC_DRAW);
  Unbind;
end;

// ------------------------------------------------------------------------------

constructor TVKBuffer<_TYPE_>.Create(const Kind_: GLenum);
begin
  inherited Create;
  glGenBuffers(1, @_ID);
  _Kind := Kind_;
  Count := 0;
end;

destructor TVKBuffer<_TYPE_>.Destroy;
begin
  glDeleteBuffers(1, @_ID);
  inherited;
end;

// ------------------------------------------------------------------------------

procedure TVKBuffer<_TYPE_>.Bind;
begin
  glBindBuffer(_Kind, _ID);
end;

procedure TVKBuffer<_TYPE_>.Unbind;
begin
  glBindBuffer(_Kind, 0);
end;

// ------------------------------------------------------------------------------

procedure TVKBuffer<_TYPE_>.Map;
begin
  Bind;
  _Head := glMapBuffer(_Kind, GL_READ_WRITE);
end;

procedure TVKBuffer<_TYPE_>.Unmap;
begin
  glUnmapBuffer(_Kind);
  Unbind;
end;

constructor TVKBufferV<_TYPE_>.Create;
begin
  inherited Create(GL_ARRAY_BUFFER);
end;

destructor TVKBufferV<_TYPE_>.Destroy;
begin
  inherited;
end;

// ------------------------------------------------------------------------------

constructor TVKBufferI<_TYPE_>.Create;
begin
  inherited Create(GL_ELEMENT_ARRAY_BUFFER);
end;

destructor TVKBufferI<_TYPE_>.Destroy;
begin
  inherited;
end;

// ------------------------------------------------------------------------------

constructor TVKBufferU<_TYPE_>.Create;
begin
  inherited Create(GL_UNIFORM_BUFFER);
end;

destructor TVKBufferU<_TYPE_>.Destroy;
begin
  inherited;
end;

// ------------------------------------------------------------------------------

constructor TVKArray.Create;
begin
  inherited Create;
  glGenVertexArrays(1, @_ID);
end;

destructor TVKArray.Destroy;
begin
  glDeleteVertexArrays(1, @_ID);
  inherited;
end;

// ------------------------------------------------------------------------------

procedure TVKArray.BeginBind;
begin
  glBindVertexArray(_ID);
end;

procedure TVKArray.EndBind;
begin
  glBindVertexArray(0);
end;

//==========================================================================
initialization
//==========================================================================

 FMXOpenGL := TFMXOpenGL.Create;
 FMXOpenGL.BeginGL;
InitOpenGLext;

finalization

FMXOpenGL.EndGL;
FMXOpenGL.Free;

end.
