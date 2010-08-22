object Form1: TForm1
  Left = 164
  Top = 112
  Width = 491
  Height = 389
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 60
    Width = 483
    Height = 300
    Camera = GLCamera
    Buffer.BackgroundColor = clGray
    Align = alClient
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 483
    Height = 60
    Align = alTop
    BevelOuter = bvLowered
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 58
      Height = 14
      Caption = 'MultiProxy'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object RBUseLODs: TRadioButton
      Left = 88
      Top = 8
      Width = 129
      Height = 17
      Caption = 'Use 3 Levels of Detail'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = RBUseLODsClick
    end
    object RBHighRes: TRadioButton
      Left = 88
      Top = 24
      Width = 129
      Height = 17
      Caption = 'Force High Resolution'
      TabOrder = 1
      OnClick = RBUseLODsClick
    end
    object CBColorize: TCheckBox
      Left = 240
      Top = 8
      Width = 97
      Height = 17
      Caption = 'Colorize LODs'
      TabOrder = 2
      OnClick = RBUseLODsClick
    end
    object RBLowRes: TRadioButton
      Left = 88
      Top = 40
      Width = 129
      Height = 17
      Caption = 'Force Low Resolution'
      TabOrder = 3
      OnClick = RBUseLODsClick
    end
  end
  object GLScene: TGLScene
    Left = 8
    Top = 72
    object DCTarget: TGLDummyCube
      CubeSize = 1
      object GLParticles: TGLParticles
        Direction.Coordinates = {0000000000000000FFFF7F3F00000000}
        Position.Coordinates = {0000000000000040000000000000803F}
        RollAngle = -5
        Up.Coordinates = {B97EB23D9E067F3F0000000000000000}
        CubeSize = 1
        object MPSphere: TGLMultiProxy
          OnProgress = MPSphereProgress
          MasterObjects = <
            item
              MasterObject = SPHighRes
              DistanceMax = 20
            end
            item
              MasterObject = SPMedRes
              DistanceMin = 20
              DistanceMax = 60
            end
            item
              MasterObject = SPLowRes
              DistanceMin = 60
              DistanceMax = 99999
            end>
        end
      end
    end
    object DCReferences: TGLDummyCube
      Visible = False
      CubeSize = 1
      object SPHighRes: TGLSphere
        Radius = 0.5
        Slices = 32
        Stacks = 32
      end
      object SPMedRes: TGLSphere
        Radius = 0.5
      end
      object SPLowRes: TGLSphere
        Radius = 0.5
        Slices = 8
        Stacks = 8
      end
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1
      Position.Coordinates = {00004842000020420000F0410000803F}
      SpotCutOff = 180
    end
    object GLCamera: TGLCamera
      DepthOfView = 200
      FocalLength = 100
      TargetObject = DCTarget
      Position.Coordinates = {0000F04100004040000000400000803F}
    end
  end
  object GLCadencer: TGLCadencer
    Scene = GLScene
    TimeMultiplier = 0.5
    Left = 88
    Top = 72
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 48
    Top = 72
  end
end
