object MainForm: TMainForm
  Left = 313
  Top = 190
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'GLSLWater demo'
  ClientHeight = 480
  ClientWidth = 640
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poDefault
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object SceneViewer: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 640
    Height = 480
    Camera = Camera
    Buffer.FogEnvironment.FogColor.Color = {0000803F0000803F0000803F0AD7233C}
    Buffer.FogEnvironment.FogStart = 300.000000000000000000
    Buffer.FogEnvironment.FogEnd = 501.000000000000000000
    Buffer.FogEnvironment.FogDistance = fdEyeRadial
    Buffer.BackgroundColor = 16772829
    Buffer.AmbientColor.Color = {9A99593F9A99593FCDCCCC3D0000803F}
    Buffer.ContextOptions = [roDoubleBuffer, roRenderToWindow, roDestinationAlpha, roNoColorBufferClear]
    Buffer.FogEnable = True
    Buffer.AntiAliasing = aaNone
    FieldOfView = 134.760269165039100000
    Align = alClient
    TabOrder = 0
  end
  object Scene: TGLScene
    ObjectsSorting = osRenderFarthestFirst
    Left = 64
    Top = 76
    object ground: TGLFreeForm
      Direction.Coordinates = {000000280000803F2EBDBBB300000000}
      PitchAngle = 180.000000000000000000
      Up.Coordinates = {2EBD3B9C2EBDBBB3000080BF00000000}
    end
    object forest: TGLFreeForm
      Direction.Coordinates = {000000000000803F2EBDBBB300000000}
      PitchAngle = 180.000000000000000000
      Up.Coordinates = {000000002EBDBBB3000080BF00000000}
    end
    object mountains: TGLFreeForm
      Direction.Coordinates = {000000000000803F2EBDBBB300000000}
      PitchAngle = 180.000000000000000000
      Up.Coordinates = {000000002EBDBBB3000080BF00000000}
    end
    object FPS: TGLHUDText
      Rotation = 0.000000000000000000
      ModulateColor.Color = {0000803FF8FEFE3E000000000000803F}
    end
    object dScene: TGLDummyCube
      CubeSize = 1.000000000000000000
    end
    object Camera: TGLCamera
      DepthOfView = 500.000000000000000000
      FocalLength = 100.000000000000000000
      TargetObject = ground
      Position.Coordinates = {000000000000A040000016430000803F}
      object LightSource: TGLLightSource
        Ambient.Color = {0000803F0000803F0000803F0000803F}
        ConstAttenuation = 1.000000000000000000
        Specular.Color = {0000803F0000803F0000803F0000803F}
        SpotCutOff = 180.000000000000000000
      end
    end
  end
  object Cadencer: TGLCadencer
    Scene = Scene
    OnProgress = CadencerProgress
    Left = 100
    Top = 70
  end
  object FPSTimer: TTimer
    Interval = 200
    OnTimer = FPSTimerTimer
    Left = 30
    Top = 58
  end
end
