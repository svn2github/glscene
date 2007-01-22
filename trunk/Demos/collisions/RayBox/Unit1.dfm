object Form1: TForm1
  Left = 281
  Top = 118
  Width = 568
  Height = 442
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  OnMouseWheel = FormMouseWheel
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 14
  object Viewer: TGLSceneViewer
    Left = 0
    Top = 41
    Width = 560
    Height = 374
    Camera = GLCamera1
    FieldOfView = 150.060897827148400000
    Align = alClient
    OnMouseDown = ViewerMouseDown
    OnMouseMove = ViewerMouseMove
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 560
    Height = 41
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      Left = 308
      Top = 14
      Width = 33
      Height = 14
      Caption = 'Result:'
    end
    object Button1: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Test!'
      Default = True
      TabOrder = 0
      OnClick = Button1Click
    end
    object CheckBox1: TCheckBox
      Left = 99
      Top = 12
      Width = 70
      Height = 17
      Caption = 'View box'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = CheckBox1Click
    end
    object CheckBox2: TCheckBox
      Left = 178
      Top = 12
      Width = 112
      Height = 17
      Caption = 'Change pos scale'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
  end
  object GLScene: TGLScene
    Left = 480
    Top = 8
    object DCCamTarg: TGLDummyCube
      CubeSize = 1.000000000000000000
      object DCCube1: TGLDummyCube
        CubeSize = 1.000000000000000000
        object GLCube1: TGLCube
        end
      end
    end
    object GLLines1: TGLLines
      Nodes = <>
      NodesAspect = lnaInvisible
      Division = 1
      SplineMode = lsmSegments
      Options = []
    end
    object GLPoints1: TGLPoints
      NoZWrite = False
      Static = False
      Size = 7.000000000000000000
    end
    object GLDummyCube1: TGLDummyCube
      CubeSize = 0.100000001490116100
      VisibleAtRunTime = True
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {00007A4400004844000016440000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLLightSource2: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000FAC30000C8C3000096C30000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLCamera1: TGLCamera
      DepthOfView = 500.000000000000000000
      FocalLength = 50.000000000000000000
      NearPlaneBias = 0.100000001490116100
      TargetObject = DCCamTarg
      Position.Coordinates = {0000A04000000040000040400000803F}
      Direction.Coordinates = {000000000000803F0000008000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
      Left = 256
      Top = 144
    end
  end
  object GLCadencer: TGLCadencer
    Scene = GLScene
    OnProgress = GLCadencerProgress
    Left = 512
    Top = 8
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 480
    Top = 40
  end
  object GLMaterialLibrary: TGLMaterialLibrary
    Left = 512
    Top = 40
  end
end
