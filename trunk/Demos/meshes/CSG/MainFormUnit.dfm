object Form1: TForm1
  Left = 321
  Top = 128
  Width = 624
  Height = 611
  Caption = 'Constructive Solid Geometry Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnMouseWheelDown = FormMouseWheelDown
  OnMouseWheelUp = FormMouseWheelUp
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 608
    Height = 531
    Camera = GLCamera1
    Buffer.BackgroundColor = clTeal
    Buffer.FaceCulling = False
    FieldOfView = 158.669494628906200000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    OnMouseUp = GLSceneViewer1MouseUp
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 531
    Width = 608
    Height = 42
    Align = alBottom
    TabOrder = 1
    object Button1: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Clear'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 88
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Union'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 168
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Subtract A-B'
      TabOrder = 2
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 248
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Subtract B-A'
      TabOrder = 3
      OnClick = Button4Click
    end
    object Button5: TButton
      Left = 328
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Intersection'
      TabOrder = 4
      OnClick = Button5Click
    end
    object CheckBox1: TCheckBox
      Left = 408
      Top = 12
      Width = 97
      Height = 17
      Caption = 'Solid Result'
      Checked = True
      State = cbChecked
      TabOrder = 5
      OnClick = CheckBox1Click
    end
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 8
    object GLFreeForm3: TGLFreeForm
      MaterialLibrary = GLMaterialLibrary1
    end
    object GLFreeForm1: TGLFreeForm
      Material.FrontProperties.Ambient.Color = {B1A8A83EB1A8A83EB1A8A83E0000803F}
      Material.FrontProperties.Diffuse.Color = {9484843E9484843EDBDEDE3E0000803F}
      Material.FrontProperties.Emission.Color = {EBE0E03EE4DB5B3F9A93133F0000803F}
      Material.FrontProperties.Specular.Color = {9A99593F9A99593FCDCCCC3D0000803F}
      Scale.Coordinates = {00002042000020420000204200000000}
      AutoCentering = [macCenterX, macCenterY, macCenterZ]
    end
    object GLFreeForm2: TGLFreeForm
      Material.FrontProperties.Ambient.Color = {029F1F3FBEBEBE3E999F1F3F0000803F}
      Material.FrontProperties.Specular.Color = {BEBEBE3E999F1F3F999F1F3F0000803F}
      Position.Coordinates = {0000804100000000000000000000803F}
      Scale.Coordinates = {0000A0410000A0410000A04100000000}
      AutoCentering = [macCenterX, macCenterY, macCenterZ]
    end
    object GLCamera1: TGLCamera
      DepthOfView = 5000.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = GLFreeForm3
      Position.Coordinates = {0000000000000000000096C30000803F}
      Direction.Coordinates = {00000000000000000000803F00000000}
      Up.Coordinates = {000000000000803F0000008000000000}
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        Diffuse.Color = {CDCC4C3FCDCC4C3FCDCC4C3F0000803F}
        LightStyle = lsOmni
        SpotCutOff = 180.000000000000000000
      end
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = '1'
        Tag = 0
      end
      item
        Name = '2'
        Material.FrontProperties.Diffuse.Color = {6666E63E6666E63E6666E63E0000803F}
        Tag = 0
      end>
    Left = 48
    Top = 8
  end
end
