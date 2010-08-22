object Form1: TForm1
  Left = 125
  Top = 119
  BorderStyle = bsDialog
  BorderWidth = 5
  Caption = 'Form1'
  ClientHeight = 170
  ClientWidth = 540
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
  object Label1: TLabel
    Left = 448
    Top = 88
    Width = 89
    Height = 39
    Caption = 'Move your mouse'#13#10'over an object and'#13#10'it will start spinning'
  end
  object Label2: TLabel
    Left = 8
    Top = 8
    Width = 124
    Height = 26
    Alignment = taCenter
    Caption = 'Cube has a small constant'#13#10'and linear damping'
  end
  object Label3: TLabel
    Left = 168
    Top = 0
    Width = 100
    Height = 39
    Alignment = taCenter
    Caption = 'Dodecahedron has a'#13#10'small constant and'#13#10'quadratic damping'
  end
  object Label4: TLabel
    Left = 328
    Top = 8
    Width = 109
    Height = 26
    Alignment = taCenter
    Caption = 'Octahedron has a'#13#10'only quadratic damping'
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 1
    Top = 48
    Width = 441
    Height = 121
    Camera = GLCamera1
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object CheckBox1: TCheckBox
    Left = 448
    Top = 144
    Width = 81
    Height = 17
    Caption = 'Double Mass'
    TabOrder = 1
    OnClick = CheckBox1Click
  end
  object GLScene1: TGLScene
    Left = 480
    Top = 16
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1
      Position.Coordinates = {0000704100002041000020C10000803F}
      SpotCutOff = 180
    end
    object DummyCube1: TGLDummyCube
      CubeSize = 1
      object Cube: TGLCube
        Position.Coordinates = {0000000000000000000000400000803F}
        Material.FrontProperties.Diffuse.Color = {F8FEFE3E0000803F000000000000803F}
        Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
      end
      object Dodecahedron: TGLDodecahedron
        Material.FrontProperties.Diffuse.Color = {0000803F00000000000000000000803F}
        Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
      end
      object Octahedron: TGLSphere
        Position.Coordinates = {0000000000000000000000C00000803F}
        Material.FrontProperties.Diffuse.Color = {ABAA2A3FABAA2A3F0000803F0000803F}
        Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
        Radius = 0.5
        Slices = 4
        Stacks = 2
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100
      FocalLength = 100
      TargetObject = DummyCube1
      Position.Coordinates = {000020410000A040000000000000803F}
    end
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 480
    Top = 48
  end
end
