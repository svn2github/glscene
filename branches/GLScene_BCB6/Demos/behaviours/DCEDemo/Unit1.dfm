object Form1: TForm1
  Left = 192
  Top = 107
  Width = 696
  Height = 480
  Caption = 'Dynamic Collision Engine Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnKeyUp = FormKeyUp
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 185
    Top = 0
    Width = 503
    Height = 446
    Camera = GLCamera1
    Align = alClient
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 446
    Align = alLeft
    TabOrder = 1
    object lbCollided: TLabel
      Left = 8
      Top = 176
      Width = 60
      Height = 13
      Caption = '(No collision)'
    end
    object lbFPS: TLabel
      Left = 8
      Top = 200
      Width = 32
      Height = 13
      Caption = 'FPS: 0'
    end
    object cbEnemy: TCheckBox
      Left = 8
      Top = 128
      Width = 97
      Height = 17
      Caption = 'Enemy'
      Enabled = False
      TabOrder = 0
    end
    object cbTerrain: TCheckBox
      Left = 8
      Top = 104
      Width = 97
      Height = 17
      Caption = 'Terrain'
      Enabled = False
      TabOrder = 1
    end
    object cbEllipse: TCheckBox
      Left = 8
      Top = 80
      Width = 97
      Height = 17
      Caption = 'Ellipsoid'
      Enabled = False
      TabOrder = 2
    end
    object cbIce: TCheckBox
      Left = 8
      Top = 56
      Width = 97
      Height = 17
      Caption = 'Ice'
      Enabled = False
      TabOrder = 3
    end
    object cbBeer: TCheckBox
      Left = 8
      Top = 32
      Width = 97
      Height = 17
      Caption = 'Beer'
      Enabled = False
      TabOrder = 4
    end
    object cbMap: TCheckBox
      Left = 8
      Top = 8
      Width = 97
      Height = 17
      Caption = 'Map'
      Enabled = False
      TabOrder = 5
    end
    object Memo1: TMemo
      Left = 8
      Top = 224
      Width = 129
      Height = 161
      Color = clBtnFace
      Enabled = False
      Lines.Strings = (
        'Movement:'
        'W,A,S,D'
        ''
        'Rotation:'
        'MOUSE DRAG'
        'Direction Keys'
        ''
        'Other:'
        'G - No gravity'
        'X - Move down'
        'SPACE - Jump')
      ReadOnly = True
      TabOrder = 6
      WantReturns = False
    end
    object cbBox: TCheckBox
      Left = 8
      Top = 152
      Width = 97
      Height = 17
      Caption = 'Box'
      Enabled = False
      TabOrder = 7
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 64
    Top = 24
  end
  object GLBitmapHDS1: TGLBitmapHDS
    MaxPoolSize = 0
    Left = 104
    Top = 24
  end
  object GLScene1: TGLScene
    Left = 24
    Top = 24
    object Beer: TGLFreeForm
      Material.FrontProperties.Diffuse.Color = {EEED6D3FC1C0403D0000000017D92E3F}
      Position.Coordinates = {000080BF000060C0000040400000803F}
      BehavioursData = {
        0201060B54474C42444345426F64790200060B4443454D616E61676572310202
        020008090F0000803F020008}
    end
    object normal: TGLArrowLine
      Material.FrontProperties.Diffuse.Color = {000000000000803F9998183E0000803F}
      Scale.Coordinates = {CDCC4C3ECDCC4C3ECDCC4C3E00000000}
      BottomRadius = 0.100000001490116100
      Height = 1.000000000000000000
      TopRadius = 0.100000001490116100
      TopArrowHeadHeight = 0.500000000000000000
      TopArrowHeadRadius = 0.200000002980232200
      BottomArrowHeadHeight = 0.500000000000000000
      BottomArrowHeadRadius = 0.200000002980232200
    end
    object Map: TGLFreeForm
      Material.FrontProperties.Diffuse.Color = {CDCC4C3FCDCC4C3FCDCC4C3F1283203F}
      AutoScaling.Coordinates = {0000004000000040000000400000803F}
      BehavioursData = {
        0201060B54474C42444345426F64790200060B4443454D616E61676572310202
        020008090F0000A040020008}
    end
    object Ice: TGLFreeForm
      Material.FrontProperties.Diffuse.Color = {9392923E9594143FF2F1713F0000803F}
      Position.Coordinates = {000000000000F0C0000000000000803F}
      AutoScaling.Coordinates = {00004040CDCC4C3E000040400000803F}
      BehavioursData = {
        0201060B54474C42444345426F64790200060B4443454D616E61676572310202
        020008090F0000803F020008}
    end
    object Terrain: TGLTerrainRenderer
      Direction.Coordinates = {000000000000803F0000000000000000}
      Position.Coordinates = {000000000000A0C0000000000000803F}
      Scale.Coordinates = {0000803F0000803F295C0F3D00000000}
      Up.Coordinates = {00000000000000000000803F00000000}
      HeightDataSource = GLBitmapHDS1
      TilesPerTexture = 1.000000000000000000
      BehavioursData = {
        0201060B54474C42444345426F64790200060B4443454D616E61676572310203
        020008090F0000A040020008}
    end
    object Box: TGLCube
      Direction.Coordinates = {F304353FFFFFFF3EFFFFFF3E00000000}
      Position.Coordinates = {0000C03F000080C0000000000000803F}
      Up.Coordinates = {00000000F304353FF30435BF00000000}
      BehavioursData = {
        0201060B54474C42444345426F64790200060B4443454D616E61676572310201
        020008090F0000C841020009000000400000803F0000C03F00000000}
      CubeSize = {000000400000803F0000C03F}
    end
    object Ellipsoid: TGLSphere
      Material.FrontProperties.Diffuse.Color = {CDCC4C3FCDCC4C3FCDCC4C3F508DD73E}
      Position.Coordinates = {000080C0000080C0000000400000803F}
      Scale.Coordinates = {000040400000803F0000404000000000}
      Radius = 1.000000000000000000
      BehavioursData = {
        0201060B54474C42444345426F64790200060B4443454D616E61676572310200
        020008090F0000A040020009000040400000803F0000404000000000}
    end
    object Enemy: TGLSphere
      Position.Coordinates = {0000A040000000400000A0C00000803F}
      Scale.Coordinates = {0000003FCDCC4C3F0000003F00000000}
      Radius = 1.000000000000000000
      BehavioursData = {
        0201060B54474C42444345426F64790200060B4443454D616E61676572310200
        020009090F0000A0400200090000003FCDCC4C3F0000003F00000000}
    end
    object Player: TGLDummyCube
      Position.Coordinates = {00000000000000000000803F0000803F}
      Up.Coordinates = {000000000000803F0000008000000000}
      CubeSize = 1.000000000000000000
      BehavioursData = {
        0201060B54474C42444345426F64790200060B4443454D616E61676572310200
        020009090F000020410200090000003FCDCC4C3F0000003F00000000}
      object plBody: TGLSphere
        Material.FrontProperties.Diffuse.Color = {0000803F000000000000000046B6F33E}
        Material.BlendingMode = bmTransparency
        Scale.Coordinates = {0000003FCDCC4C3F0000003F00000000}
        Radius = 1.000000000000000000
      end
      object GLCamera1: TGLCamera
        DepthOfView = 100.000000000000000000
        FocalLength = 50.000000000000000000
        TargetObject = Player
        Position.Coordinates = {000000000000803F000000C00000803F}
        object GLLightSource1: TGLLightSource
          ConstAttenuation = 1.000000000000000000
          SpotCutOff = 180.000000000000000000
        end
      end
    end
    object GLDirectOpenGL1: TGLDirectOpenGL
      Visible = False
      UseBuildList = False
      OnRender = GLDirectOpenGL1Render
      Blend = False
    end
  end
  object DCEManager1: TGLDCEManager
    OnCollision = DCEManager1Collision
    Gravity.Coordinates = {00000000000020C10000000000000000}
    MovimentScale = 1.000000000000000000
    Left = 144
    Top = 24
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 128
    Top = 96
  end
end
