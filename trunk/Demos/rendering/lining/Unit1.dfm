object Form1: TForm1
  Left = 224
  Top = 145
  Width = 540
  Height = 353
  Caption = 'Lining Shaders'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 377
    Height = 326
    Camera = GLCamera1
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object Panel1: TPanel
    Left = 377
    Top = 0
    Width = 155
    Height = 326
    Align = alRight
    BevelOuter = bvSpace
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object Label1: TLabel
      Left = 17
      Top = 178
      Width = 50
      Height = 13
      Caption = 'Drag with:'
    end
    object Label2: TLabel
      Left = 17
      Top = 194
      Width = 123
      Height = 13
      Caption = 'LMB - move around scene'
    end
    object Label3: TLabel
      Left = 17
      Top = 210
      Width = 89
      Height = 13
      Caption = 'RMB - rotate torus'
    end
    object Bevel1: TBevel
      Left = 11
      Top = 170
      Width = 131
      Height = 9
      Shape = bsTopLine
    end
    object CheckBox1: TCheckBox
      Left = 9
      Top = 138
      Width = 131
      Height = 17
      Caption = 'Outline Shader Enabled'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = CheckBox1Click
    end
    object GroupBox1: TGroupBox
      Left = 9
      Top = 58
      Width = 137
      Height = 73
      Caption = 'Shader on Torus'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      object CheckBox2: TCheckBox
        Left = 10
        Top = 16
        Width = 97
        Height = 17
        Caption = 'Enabled'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = CheckBox2Click
      end
      object CheckBox3: TCheckBox
        Left = 10
        Top = 32
        Width = 97
        Height = 17
        Caption = 'Solid'
        TabOrder = 1
        OnClick = CheckBox3Click
      end
      object CheckBox4: TCheckBox
        Left = 10
        Top = 48
        Width = 113
        Height = 17
        Caption = 'Dotted Hidden Line'
        Checked = True
        State = cbChecked
        TabOrder = 2
        OnClick = CheckBox4Click
      end
    end
    object Panel2: TPanel
      Left = 1
      Top = 1
      Width = 153
      Height = 43
      Align = alTop
      BevelOuter = bvNone
      Caption = 'Lining Shaders'
      Color = clBtnHighlight
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -19
      Font.Name = 'Trebuchet MS'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
    end
  end
  object GLScene1: TGLScene
    ObjectsSorting = osRenderFarthestFirst
    Left = 8
    Top = 8
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {00002041000000410000E0400000803F}
      SpotCutOff = 180.000000000000000000
    end
    object Torus1: TGLTorus
      Direction.Coordinates = {000000000000803F0000000000000000}
      Up.Coordinates = {0000000000000000000080BF00000000}
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'LibMaterial2'
      MajorRadius = 2.500000000000000000
      MinorRadius = 1.500000000000000000
    end
    object Sphere1: TGLSphere
      ShowAxes = True
      Radius = 0.500000000000000000
    end
    object GLAnnulusOutlined: TGLAnnulus
      Position.Coordinates = {0000E04000000000000000000000803F}
      Scale.Coordinates = {00000040000000400000004000000000}
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'LibMaterial'
      BottomRadius = 0.500000000000000000
      Height = 1.000000000000000000
      BottomInnerRadius = 0.300000011920929000
      TopInnerRadius = 0.300000011920929000
      TopRadius = 0.500000000000000000
    end
    object GLAnnulusPink: TGLAnnulus
      Position.Coordinates = {0000E0C000000000000000000000803F}
      Scale.Coordinates = {00000040000000400000004000000000}
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'LibMaterial4'
      BottomRadius = 0.500000000000000000
      Height = 1.000000000000000000
      BottomInnerRadius = 0.300000011920929000
      TopInnerRadius = 0.300000011920929000
      TopRadius = 0.500000000000000000
    end
    object GLAnnulusDotted: TGLAnnulus
      Position.Coordinates = {00000000000000000000E0C00000803F}
      Scale.Coordinates = {00000040000000400000004000000000}
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'LibMaterial1'
      BottomRadius = 0.500000000000000000
      Height = 1.000000000000000000
      BottomInnerRadius = 0.300000011920929000
      TopInnerRadius = 0.300000011920929000
      TopRadius = 0.500000000000000000
    end
    object GLCubeGreen: TGLCube
      Position.Coordinates = {00000000000000000000E0400000803F}
      Scale.Coordinates = {00000040000000400000004000000000}
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'LibMaterial5'
    end
    object GLCubeTransparent: TGLCube
      Position.Coordinates = {0000000000008040000000000000803F}
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'LibMaterial3'
      CubeSize = {000040400000404000004040}
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = Torus1
      Position.Coordinates = {00006041000020410000C0400000803F}
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'LibMaterial'
        Material.FrontProperties.Ambient.Color = {9F9E1E3FCDCC4C3EE2E1613F0000803F}
        Material.FrontProperties.Diffuse.Color = {9796163F0000803F0000803F0000803F}
        Tag = 0
        Shader = GLOutlineShader1
      end
      item
        Name = 'LibMaterial1'
        Tag = 0
        Shader = GLHiddenLineShader1
      end
      item
        Name = 'LibMaterial2'
        Tag = 0
        Shader = GLHiddenLineShader2
      end
      item
        Name = 'LibMaterial3'
        Tag = 0
        Shader = GLHiddenLineShader3
      end
      item
        Name = 'LibMaterial4'
        Tag = 0
        Shader = GLHiddenLineShader4
      end
      item
        Name = 'LibMaterial5'
        Tag = 0
        Shader = GLHiddenLineShader5
      end>
    Left = 8
    Top = 40
  end
  object GLOutlineShader1: TGLOutlineShader
    LineColor.Color = {0000803F00000000000000000000803F}
    LineSmooth = True
    BlendLine = True
    LineWidth = 4.000000000000000000
    Left = 8
    Top = 72
  end
  object GLHiddenLineShader1: TGLHiddenLineShader
    FrontLine.Width = 1.000000000000000000
    FrontLine.Color.Color = {938C0C3E938C0C3E938E0E3F0000803F}
    FrontLine.Pattern = 10101
    BackLine.Width = 2.000000000000000000
    LineSmooth = True
    BlendLine = True
    Solid = True
    Left = 40
    Top = 8
  end
  object GLHiddenLineShader2: TGLHiddenLineShader
    FrontLine.Width = 2.000000000000000000
    BackLine.Width = 1.000000000000000000
    BackLine.Color.Color = {000000000000003F000000000000803F}
    BackLine.Pattern = 65280
    LineSmooth = True
    BlendLine = True
    Left = 40
    Top = 40
  end
  object GLHiddenLineShader3: TGLHiddenLineShader
    FrontLine.Width = 5.000000000000000000
    FrontLine.Color.Color = {EC51B83E0000803ECDCC4C3E9A99193F}
    BackLine.Width = 5.000000000000000000
    BackLine.Color.Color = {0000003F0000003F0000003F9A99993E}
    LineSmooth = True
    BlendLine = True
    Left = 40
    Top = 72
  end
  object GLHiddenLineShader4: TGLHiddenLineShader
    FrontLine.Width = 2.000000000000000000
    FrontLine.Color.Color = {B81E053F14AEC73E14AEC73E0000803F}
    BackLine.Width = 1.000000000000000000
    BackLine.Color.Color = {E4DB5B3FEBE0E03E9A93133F0000803F}
    LineSmooth = True
    BlendLine = True
    Left = 72
    Top = 8
  end
  object GLHiddenLineShader5: TGLHiddenLineShader
    FrontLine.Width = 2.000000000000000000
    FrontLine.Color.Color = {9A93133FE4DB5B3FEBE0E03E3333333F}
    BackLine.Width = 2.000000000000000000
    BackLine.Color.Color = {000000000000003F000000000000803F}
    BackLine.Pattern = 65280
    LineSmooth = True
    BlendLine = True
    Solid = True
    BackgroundColor.Color = {938C0C3E938E0E3F938C0C3E3333333F}
    Left = 72
    Top = 40
  end
end
