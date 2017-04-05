object frmMain: TfrmMain
  Left = 192
  Top = 107
  Caption = 'Accumulator of objects'
  ClientHeight = 601
  ClientWidth = 854
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object SceneViewer: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 854
    Height = 601
    Camera = camMain
    FieldOfView = 161.106277465820300000
    Align = alClient
    TabOrder = 0
  end
  object btnInitAccum: TButton
    Left = 8
    Top = 8
    Width = 193
    Height = 25
    Caption = 'Initialize accumulator'
    TabOrder = 1
    OnClick = btnInitAccumClick
  end
  object btnAddObject: TButton
    Left = 8
    Top = 40
    Width = 193
    Height = 25
    Caption = 'Add object'
    TabOrder = 2
    OnClick = btnAddObjectClick
  end
  object btnDelObject: TButton
    Left = 8
    Top = 72
    Width = 193
    Height = 25
    Caption = 'Delete object'
    TabOrder = 3
    OnClick = btnDelObjectClick
  end
  object btnDeinitAccum: TButton
    Left = 8
    Top = 104
    Width = 193
    Height = 25
    Caption = 'Free accumulator'
    TabOrder = 4
    OnClick = btnDeinitAccumClick
  end
  object pnlFPS: TPanel
    Left = 8
    Top = 136
    Width = 193
    Height = 25
    BevelInner = bvLowered
    TabOrder = 5
  end
  object GLScene: TGLScene
    object esdSky: TGLEarthSkyDome
      Bands = <>
      Stars = <>
      SunElevation = 75.000000000000000000
      Turbidity = 15.000000000000000000
      ExtendedOptions = []
      Slices = 48
      Stacks = 24
    end
    object lsLight: TGLLightSource
      Ambient.Color = {0000803F0000803F0000803F0000803F}
      ConstAttenuation = 2.500000000000000000
      Position.Coordinates = {00000000000020C1000000000000803F}
      Specular.Color = {0000803F0000803F0000803F0000803F}
      SpotCutOff = 180.000000000000000000
      SpotDirection.Coordinates = {00000000000000000000000000000000}
    end
    object dscTerrain: TGLDisk
      Material.FrontProperties.Ambient.Color = {CDCC4C3EF7F6F63ECDCC4C3E0000803F}
      Material.FrontProperties.Diffuse.Color = {E7E6E63EEFEEEE3EEBEAEA3E0000803F}
      Position.Coordinates = {000000000000803E000000000000803F}
      OuterRadius = 1.000000000000000000
      Slices = 64
      SweepAngle = 360.000000000000000000
      object dcObjects: TGLDummyCube
        Position.Coordinates = {00000000000000006666663E0000803F}
        CubeSize = 1.000000000000000000
      end
    end
    object camMain: TGLCamera
      DepthOfView = 300.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = dscTerrain
      Position.Coordinates = {000000000000A0BF0000003F0000803F}
    end
  end
  object FPSTimer: TGLAsyncTimer
    Enabled = True
    OnTimer = FPSTimerTimer
    ThreadPriority = tpNormal
    Left = 368
  end
  object Cadencer: TGLCadencer
    Scene = GLScene
    OnProgress = CadencerProgress
    Left = 440
  end
end
