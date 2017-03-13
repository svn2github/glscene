object Frm: TFrm
  Left = 394
  Top = 182
  BorderIcons = []
  BorderStyle = bsNone
  ClientHeight = 454
  ClientWidth = 657
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 657
    Height = 454
    Cursor = -1
    Camera = Cam
    Buffer.FogEnvironment.FogColor.Color = {8180003F8180003F8180003F0000803F}
    Buffer.FogEnvironment.FogStart = 15.000000000000000000
    Buffer.FogEnvironment.FogEnd = 200.000000000000000000
    Buffer.BackgroundColor = clGray
    Buffer.AmbientColor.Color = {0000000000000000000000000000803F}
    Buffer.FogEnable = True
    FieldOfView = 148.042541503906300000
    Align = alClient
    TabOrder = 0
  end
  object GLScene: TGLScene
    Left = 136
    Top = 16
    object GLSkyDome1: TGLSkyDome
      Bands = <
        item
          StartColor.Color = {8180003F8180003F8180003F0000803F}
          StopAngle = 15.000000000000000000
          StopColor.Color = {000000001283003F9CC4403F0000803F}
        end
        item
          StartAngle = 15.000000000000000000
          StartColor.Color = {000000001283003F9CC4403F0000803F}
          StopAngle = 90.000000000000000000
          StopColor.Color = {1283003F1283003F1283003F0000803F}
          Stacks = 4
        end>
      Stars = <>
    end
    object Scene: TGLDummyCube
      CubeSize = 1.000000000000000000
      object Map: TGLDummyCube
        CubeSize = 1.000000000000000000
        object FreeForm: TGLFreeForm
          Material.FrontProperties.Diffuse.Color = {E3E2E23EE7E6E63EE3E2E23E0000803F}
          Scale.Coordinates = {3480B7393480B73952491D3900000000}
          MaterialLibrary = GLMaterialLibrary1
          LightmapLibrary = GLMaterialLibrary1
        end
      end
      object CarsBox: TGLDummyCube
        CubeSize = 1.000000000000000000
      end
    end
    object Light: TGLLightSource
      Ambient.Color = {BEC0403FBEC0403FBEC0403F0000803F}
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {000000C100000041000096430000803F}
      LightStyle = lsOmni
      Specular.Color = {BEC0403FBEC0403FBEC0403F0000803F}
      SpotCutOff = 180.000000000000000000
    end
    object CamBox: TGLDummyCube
      CubeSize = 1.000000000000000000
      object Cam: TGLCamera
        DepthOfView = 250.000000000000000000
        FocalLength = 65.000000000000000000
        Position.Coordinates = {000000000000803F000000C00000803F}
        Direction.Coordinates = {00000000000000800000803F00000000}
      end
    end
    object GLHUDText1: TGLHUDText
      BitmapFont = GLWindowsBitmapFont1
      Rotation = 0.000000000000000000
    end
  end
  object GLCadencer: TGLCadencer
    Scene = GLScene
    Enabled = False
    MaxDeltaTime = 0.020000000000000000
    OnProgress = GLCadencerProgress
    Left = 136
    Top = 72
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Left = 232
    Top = 16
  end
  object Timer: TTimer
    OnTimer = TimerTimer
    Left = 344
    Top = 16
  end
  object GLWindowsBitmapFont1: TGLWindowsBitmapFont
    Font.Charset = ANSI_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Arial Black'
    Font.Style = [fsItalic]
    Left = 232
    Top = 72
  end
  object Tkiller: TTimer
    Enabled = False
    Interval = 10
    OnTimer = TkillerTimer
    Left = 344
    Top = 72
  end
end
