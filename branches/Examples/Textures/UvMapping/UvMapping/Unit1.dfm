object Form1: TForm1
  Left = 542
  Top = 141
  Caption = 'UV Mapping'
  ClientHeight = 445
  ClientWidth = 570
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 153
    Height = 445
    Align = alLeft
    Color = 9608036
    TabOrder = 0
    object Image1: TImage
      Left = 8
      Top = 8
      Width = 137
      Height = 137
    end
    object Button3: TButton
      Left = 10
      Top = 249
      Width = 137
      Height = 25
      Caption = 'Load texture'
      TabOrder = 0
      OnClick = Button3Click
    end
    object ComboBox1: TComboBox
      Left = 8
      Top = 152
      Width = 137
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 1
      Text = '0 - X Axis'
      OnChange = ComboBox1Change
      Items.Strings = (
        '0 - X Axis'
        '1 - Y Axis'
        '2 - Z Axis')
    end
    object ComboBox2: TComboBox
      Left = 8
      Top = 184
      Width = 137
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 2
      Text = '0 - Planar mapping'
      OnChange = ComboBox2Change
      Items.Strings = (
        '0 - Planar mapping'
        '1 - Cubic mapping'
        '2 - Cylindrical mapping'
        '3 - Spherical mapping')
    end
    object Button1: TButton
      Left = 10
      Top = 218
      Width = 137
      Height = 25
      Caption = 'Load model'
      TabOrder = 3
      OnClick = Button1Click
    end
    object CheckBox1: TCheckBox
      Left = 10
      Top = 296
      Width = 105
      Height = 17
      Caption = 'Inverted normals'
      TabOrder = 4
      OnClick = CheckBox1Click
    end
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 153
    Top = 0
    Width = 417
    Height = 445
    Camera = GLCamera1
    Buffer.BackgroundColor = 12769969
    FieldOfView = 153.029327392578100000
    PenAsTouch = False
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 1
  end
  object GLScene1: TGLScene
    Left = 48
    Top = 8
    object GLFreeForm1: TGLFreeForm
      AutoCentering = [macCenterX, macCenterY, macCenterZ]
    end
    object GLDummyCube1: TGLDummyCube
      CubeSize = 1.000000000000000000
      object GLCamera1: TGLCamera
        DepthOfView = 100.000000000000000000
        FocalLength = 50.000000000000000000
        SceneScale = 2.000000000000000000
        TargetObject = GLDummyCube1
        Position.Coordinates = {0000A0400000A0400000A0400000803F}
        object GLLightSource1: TGLLightSource
          ConstAttenuation = 1.000000000000000000
          SpotCutOff = 180.000000000000000000
        end
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Filter = '3DS File (*.3ds)|*.3ds'
    Left = 328
    Top = 16
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Filter = 
      'All (*.tga;*.jpg;*.jpeg;*.bmp)|*.tga;*.jpg;*.jpeg;*.bmp|Targa (*' +
      '.tga)|*.tga|JPEG Image File (*.jpg)|*.jpg|JPEG Image File (*.jpe' +
      'g)|*.jpeg|Bitmaps (*.bmp)|*.bmp'
    Left = 416
    Top = 16
  end
end
