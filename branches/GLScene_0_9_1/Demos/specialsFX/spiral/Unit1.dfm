object Form1: TForm1
  Left = 233
  Top = 95
  BorderStyle = bsDialog
  Caption = 'Form1'
  ClientHeight = 357
  ClientWidth = 437
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer: TGLSceneViewer
    Left = 0
    Top = 25
    Width = 437
    Height = 332
    Camera = GLCamera
    Buffer.BackgroundColor = clBlack
    Align = alClient
    OnDblClick = GLSceneViewerDblClick
    OnMouseMove = GLSceneViewerMouseMove
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 437
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 1
    Caption = 'Spiral / PFX Demo'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold, fsItalic]
    ParentFont = False
    TabOrder = 1
    object SpeedButton1: TSpeedButton
      Left = 363
      Top = 2
      Width = 73
      Height = 21
      Caption = 'FullScreen'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      OnClick = GLSceneViewerDblClick
    end
  end
  object GLScene: TGLScene
    Left = 16
    Top = 16
    object DCBase: TGLDummyCube
      CubeSize = 1
      BehavioursData = {
        0201060B54474C42496E657274696102000200050000000000000080FF3F0200
        080500000000000000F005400500000000000000000000050000000000000000
        000008020008020008}
      EffectsData = {
        0201061254474C536F757263655046584566666563740200060750465852696E
        6702010200080200090000000000002040000000000000000005000000000000
        0000000005000000000000000000000500000000000000FA074002000201}
      object DCSrc: TGLDummyCube
        Position.Coordinates = {0000803F00000040000000000000803F}
        CubeSize = 1
        EffectsData = {
          0201061254474C536F7572636550465845666665637402000609504658537069
          72616C020102000900000000CDCC4CBE00000000000000000200080500000000
          00CDCCCCFB3F0500000000000000000000050000000000CDCCCCFA3F02000200}
      end
    end
    object PFXRenderer: TGLParticleFXRenderer
    end
    object GLCamera: TGLCamera
      DepthOfView = 100
      FocalLength = 50
      TargetObject = DCBase
      Position.Coordinates = {0000C0400000A040000080400000803F}
    end
  end
  object PFXSpiral: TGLPolygonPFXManager
    Cadencer = GLCadencer
    Renderer = PFXRenderer
    Acceleration.Coordinates = {00000000CDCC4CBE0000000000000000}
    NbSides = 9
    ParticleSize = 0.3
    LifeColors = <
      item
        ColorInner.Color = {0000803F0000803F000000000000803F}
        ColorOuter.Color = {00000000000000000000803F00000000}
        LifeTime = 3
      end
      item
        ColorInner.Color = {0AD7A33E48E1FA3E1F85EB3E0000803F}
        ColorOuter.Color = {0000803F000000000000000000000000}
        LifeTime = 6
      end
      item
        LifeTime = 9
      end>
    Left = 88
    Top = 16
  end
  object GLCadencer: TGLCadencer
    Scene = GLScene
    MaxDeltaTime = 0.1
    Left = 16
    Top = 56
  end
  object Timer: TTimer
    Interval = 2000
    OnTimer = TimerTimer
    Left = 16
    Top = 96
  end
  object PFXRing: TGLPolygonPFXManager
    Cadencer = GLCadencer
    Renderer = PFXRenderer
    NbSides = 9
    ParticleSize = 0.2
    ColorInner.Color = {00000000000000001283203F0000803F}
    LifeColors = <
      item
        ColorInner.Color = {0000803F0000803F0000803F9A99193F}
        LifeTime = 2.5
      end
      item
        LifeTime = 3.5
      end>
    Left = 88
    Top = 56
  end
  object GLFullScreenViewer: TGLFullScreenViewer
    Camera = GLCamera
    Width = 800
    Height = 600
    Buffer.BackgroundColor = clBlack
    RefreshRate = 0
    OnKeyPress = GLFullScreenViewerKeyPress
    OnDblClick = GLFullScreenViewerDblClick
    OnMouseMove = GLSceneViewerMouseMove
    Left = 88
    Top = 96
  end
end
