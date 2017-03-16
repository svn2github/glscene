object Form1: TForm1
  Left = 231
  Top = 508
  Caption = 'Weather Effects'
  ClientHeight = 448
  ClientWidth = 654
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  object vp: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 654
    Height = 448
    Camera = GLCamera1
    Buffer.BackgroundColor = clBlack
    Buffer.ShadeModel = smSmooth
    FieldOfView = 142.976699829101600000
    Align = alClient
    OnMouseDown = vpMouseDown
    OnMouseUp = vpMouseUp
    TabOrder = 0
  end
  object GLScene1: TGLScene
    ObjectsSorting = osNone
    Left = 8
    Top = 8
    object txt_gl: TGLSpaceText
      Material.FrontProperties.Diffuse.Color = {0000803F0000003F000000000000803F}
      Scale.Coordinates = {CDCCCC3ECDCCCC3ECDCCCC3E00000000}
      Up.Coordinates = {000000000000803F0000008000000000}
      Extrusion = 0.019999999552965160
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      Lines.Strings = (
        'GL')
      Adjust.Horz = haCenter
      Adjust.Vert = vaBottom
      object txt_scene: TGLSpaceText
        Material.FrontProperties.Diffuse.Color = {0000803F0000003F000000000000803F}
        Up.Coordinates = {000000000000803F0000008000000000}
        Extrusion = 0.019999999552965160
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        Lines.Strings = (
          'Scene')
        Adjust.Horz = haCenter
        Adjust.Vert = vaTop
      end
    end
    object GLParticleFXRenderer1: TGLParticleFXRenderer
      ZTest = False
      ZSortAccuracy = saLow
    end
    object GLDummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
      EffectsData = {
        0458434F4C02010203061254474C536F75726365504658456666656374020202
        001200000000020002000617474C506F696E744C696768745046584D616E6167
        657231050000000000000080FF3F02060200080200080200090000803F000000
        000000803F0000000005000000000000000000000500000000000000A0014005
        00000000006F1283F53F02000200080500000000000000000000080200020002
        0202001200000000020002000617474C506F696E744C696768745046584D616E
        6167657232050000000000000080FF3F02060200080200080200090000803F00
        0000000000803F00000000050000000000000080FF3F0500000000000000A001
        400500000000006F1283F53F0200020008050000000000000000000008020002
        00020202001200000000020002000613474C5065726C696E5046584D616E6167
        657231050000000000000080FF3F0206020009000000000000003F0000000000
        00000002000900000000000040C0000000000000000002000900008040000000
        000000803F00000000050000000000CDCCCCFC3F050000000000000080FF3F05
        00000000000AD7A3F73F0200020008050000000000000080FF3F080200}
      object dc_emitter: TGLDummyCube
        Position.Coordinates = {0000C03F00004040000000000000803F}
        CubeSize = 1.000000000000000000
      end
    end
    object GLHUDText1: TGLHUDText
      Position.Coordinates = {000020410000A040000000000000803F}
      BitmapFont = GLWindowsBitmapFont1
      Rotation = 0.000000000000000000
      ModulateColor.Color = {0000803F0000803F0000803F0000003F}
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 75.000000000000000000
      TargetObject = GLDummyCube1
      Position.Coordinates = {0000000000000040000000410000803F}
      Left = 152
      Top = 104
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        SpotCutOff = 180.000000000000000000
      end
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    FixedDeltaTime = 0.025000000000000000
    SleepLength = 1
    OnProgress = GLCadencer1Progress
    Left = 8
    Top = 40
  end
  object AsyncTimer1: TGLAsyncTimer
    Enabled = True
    OnTimer = AsyncTimer1Timer
    ThreadPriority = tpHigher
    Left = 304
    Top = 8
  end
  object GLPointLightPFXManager1: TGLPointLightPFXManager
    Cadencer = GLCadencer1
    Renderer = GLParticleFXRenderer1
    Friction = 1.000000000000000000
    AspectRatio = 0.050000000745058060
    ParticleSize = 0.100000001490116100
    ColorInner.Color = {0000803F0000803F0000803F9A99993E}
    LifeColors = <
      item
        LifeTime = 3.000000000000000000
        SizeScale = 1.000000000000000000
      end>
    Left = 88
    Top = 8
  end
  object GLPointLightPFXManager2: TGLPointLightPFXManager
    Cadencer = GLCadencer1
    Renderer = GLParticleFXRenderer1
    Friction = 1.000000000000000000
    AspectRatio = 0.500000000000000000
    ParticleSize = 0.100000001490116100
    ColorInner.Color = {0000803F0000803F0000803F0000803F}
    LifeColors = <
      item
        LifeTime = 3.000000000000000000
        SizeScale = 1.000000000000000000
      end>
    Left = 184
    Top = 8
  end
  object GLPerlinPFXManager1: TGLPerlinPFXManager
    Cadencer = GLCadencer1
    Renderer = GLParticleFXRenderer1
    Friction = 1.000000000000000000
    Smoothness = 1.000000000000000000
    Brightness = 0.200000002980232200
    Gamma = 1.000000000000000000
    NoiseAmplitude = 75
    ColorMode = scmFade
    ParticleSize = 1.500000000000000000
    ColorInner.Color = {0000803F0000803F0000803F0000003F}
    LifeColors = <
      item
        LifeTime = 15.000000000000000000
        SizeScale = 1.000000000000000000
        RotateAngle = 1.000000000000000000
      end>
    Left = 160
    Top = 96
  end
  object GLWindowsBitmapFont1: TGLWindowsBitmapFont
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Ranges = <
      item
        StartASCII = ' '
        StopASCII = 'z'
        StartGlyphIdx = 0
      end>
    Left = 8
    Top = 104
  end
end
