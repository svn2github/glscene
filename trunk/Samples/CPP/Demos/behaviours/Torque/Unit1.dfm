object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Torque'
  ClientHeight = 179
  ClientWidth = 683
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 78
    Width = 683
    Height = 101
    Camera = GLCamera1
    FieldOfView = 53.587551116943360000
    Align = alClient
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 683
    Height = 78
    Align = alTop
    TabOrder = 1
    object Label2: TLabel
      Left = 8
      Top = 8
      Width = 125
      Height = 26
      Alignment = taCenter
      Caption = 'Cube has a small constant'#13#10'and linear damping'
    end
    object Label3: TLabel
      Left = 152
      Top = 8
      Width = 99
      Height = 39
      Alignment = taCenter
      Caption = 'Dodecahedron has a'#13#10'small constant and'#13#10'quadratic damping'
    end
    object Label4: TLabel
      Left = 288
      Top = 8
      Width = 111
      Height = 26
      Alignment = taCenter
      Caption = 'Octahedron has a'#13#10'only quadratic damping'
    end
    object Label1: TLabel
      Left = 443
      Top = 8
      Width = 91
      Height = 39
      Caption = 'Move your mouse'#13#10'over an object and'#13#10'it will start spinning'
    end
    object CheckBox1: TCheckBox
      Left = 8
      Top = 55
      Width = 81
      Height = 17
      Caption = 'Double Mass'
      TabOrder = 0
      OnClick = CheckBox1Click
    end
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 80
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000704100002041000020C10000803F}
      SpotCutOff = 180.000000000000000000
    end
    object DummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
      object Cube: TGLCube
        Material.FrontProperties.Diffuse.Color = {F8FEFE3E0000803F000000000000803F}
        Position.Coordinates = {0000000000000000000000400000803F}
      end
      object Dodecahedron: TGLDodecahedron
        Material.FrontProperties.Diffuse.Color = {0000803F00000000000000000000803F}
      end
      object Octahedron: TGLSphere
        Material.FrontProperties.Diffuse.Color = {ABAA2A3FABAA2A3F0000803F0000803F}
        Position.Coordinates = {0000000000000000000000C00000803F}
        Radius = 0.500000000000000000
        Slices = 4
        Stacks = 2
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 100.000000000000000000
      TargetObject = DummyCube1
      Position.Coordinates = {000020410000A040000000000000803F}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 88
    Top = 80
  end
end
