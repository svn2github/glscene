object Form1: TForm1
  Left = 196
  Top = 130
  Caption = 'TextDirect'
  ClientHeight = 493
  ClientWidth = 763
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  PixelsPerInch = 96
  TextHeight = 13
  object vp: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 763
    Height = 493
    Camera = GLCamera1
    Buffer.BackgroundColor = 2565927
    Buffer.AntiAliasing = aa4x
    FieldOfView = 146.154220581054700000
    Align = alClient
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 64
    Top = 8
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 75.000000000000000000
      TargetObject = dc_txt3d
      CameraStyle = csPerspectiveKeepFOV
      Position.Coordinates = {000080400000C040000000410000803F}
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        SpotCutOff = 180.000000000000000000
      end
    end
    object dogl: TGLDirectOpenGL
      UseBuildList = False
      OnRender = doglRender
      Blend = False
      object dc_txt3d: TGLDummyCube
        Visible = False
        CubeSize = 1.000000000000000000
        object txt3d: TGLSpaceText
          Material.FrontProperties.Diffuse.Color = {0000803F1283003F000000000000803F}
          Scale.Coordinates = {9A99993E9A99993E9A99993E00000000}
          Extrusion = 0.100000001490116100
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          Lines.Strings = (
            '123')
          allowedDeviation = 1.000000000000000000
          CharacterRange = stcrNumbers
          Adjust.Horz = haCenter
          Adjust.Vert = vaCenter
        end
      end
      object dc_txt2d: TGLDummyCube
        Visible = False
        CubeSize = 1.000000000000000000
        object txt2d: TGLFlatText
          Scale.Coordinates = {0AD7A33C0AD7A33C0AD7A33C00000000}
          BitmapFont = GLWindowsBitmapFont1
          Alignment = taCenter
          Layout = tlCenter
          ModulateColor.Color = {000000000000803F000000000000803F}
          Options = []
        end
      end
      object txthud: TGLHUDText
        Visible = False
        BitmapFont = GLWindowsBitmapFont1
        Rotation = 0.000000000000000000
        Alignment = taCenter
        Layout = tlCenter
        ModulateColor.Color = {1283003F0000803F0000803F0000803F}
      end
    end
  end
  object cad: TGLCadencer
    Scene = GLScene1
    Mode = cmApplicationIdle
    OnProgress = cadProgress
    Left = 168
    Top = 8
  end
  object at: TGLAsyncTimer
    Enabled = True
    Interval = 800
    OnTimer = atTimer
    Left = 248
    Top = 8
  end
  object GLWindowsBitmapFont1: TGLWindowsBitmapFont
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -16
    Font.Name = 'Trebuchet MS'
    Font.Style = []
    MinFilter = miLinearMipmapLinear
    Left = 344
    Top = 8
  end
end
