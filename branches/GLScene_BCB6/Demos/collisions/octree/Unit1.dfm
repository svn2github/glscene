object Form1: TForm1
  Left = 175
  Top = 104
  Width = 562
  Height = 429
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
  object GLSceneViewer2: TGLSceneViewer
    Left = 0
    Top = 49
    Width = 554
    Height = 351
    Camera = GLCamera2
    Buffer.BackgroundColor = 8404992
    Buffer.ShadeModel = smSmooth
    Align = alClient
    OnMouseDown = GLSceneViewer2MouseDown
    OnMouseMove = GLSceneViewer2MouseMove
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 554
    Height = 49
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      Left = 360
      Top = 0
      Width = 32
      Height = 13
      Caption = 'Label1'
    end
    object Label2: TLabel
      Left = 360
      Top = 16
      Width = 32
      Height = 13
      Caption = 'Label2'
    end
    object Label3: TLabel
      Left = 360
      Top = 32
      Width = 32
      Height = 13
      Caption = 'Label3'
    end
    object Label5: TLabel
      Left = 8
      Top = 8
      Width = 32
      Height = 13
      Caption = 'Label5'
    end
    object LABuild: TLabel
      Left = 8
      Top = 24
      Width = 36
      Height = 13
      Caption = 'LABuild'
    end
    object Label4: TLabel
      Left = 168
      Top = 32
      Width = 32
      Height = 13
      Caption = 'Label4'
    end
    object CheckBox1: TCheckBox
      Left = 168
      Top = 16
      Width = 97
      Height = 17
      Caption = 'Auto collide'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object CBOctree: TCheckBox
      Left = 168
      Top = 0
      Width = 97
      Height = 17
      Caption = 'Octree enabled'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
  end
  object GLScene1: TGLScene
    Left = 112
    Top = 80
    object GLLightSource1: TGLLightSource
      Ambient.Color = {000000001283003F0000803F0000803F}
      ConstAttenuation = 1
      Position.Coordinates = {00004842000016430000C8420000803F}
      LightStyle = lsOmni
      Specular.Color = {0000803F00000000000000000000803F}
      SpotCutOff = 180
    end
    object DummyCube1: TGLDummyCube
      CubeSize = 1
      object FreeForm1: TGLFreeForm
        Direction.Coordinates = {000000000000803F0000000000000000}
        Scale.Coordinates = {CDCCCC3DCDCCCC3DCDCCCC3D00000000}
        Up.Coordinates = {00000000000000000000803F00000000}
        Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
      end
    end
    object Sphere1: TGLSphere
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {E9DC72BF000000009BE8A13E00000000}
      Material.FrontProperties.Emission.Color = {0000803F0000803F000000000000803F}
      Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
      Radius = 0.300000011920929
      Slices = 6
      Stacks = 6
      object ArrowLine1: TGLArrowLine
        Position.Coordinates = {0000000000000000CDCCCC3D0000803F}
        Material.FrontProperties.Diffuse.Color = {CDCC4C3FCDCC4C3FCDCC4C3F9A99193F}
        Material.FrontProperties.Emission.Color = {1283803E1283803E000000000000803F}
        Material.BlendingMode = bmTransparency
        Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
        BottomRadius = 0.0500000007450581
        Height = 1
        TopRadius = 0.100000001490116
        TopArrowHeadHeight = 0.5
        TopArrowHeadRadius = 0.200000002980232
        BottomArrowHeadHeight = 0.5
        BottomArrowHeadRadius = 0.200000002980232
      end
    end
    object GLCamera2: TGLCamera
      DepthOfView = 100
      FocalLength = 50
      TargetObject = DummyCube1
      Position.Coordinates = {0000A040000010C10000A0410000803F}
      Direction.Coordinates = {00000000000000000000803F00000000}
      Up.Coordinates = {67C57BBF3B5B393E0000000000000000}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 16
    Top = 80
  end
  object Timer1: TTimer
    Interval = 300
    OnTimer = Timer1Timer
    Left = 64
    Top = 80
  end
end
