object GUISkinEditor: TGUISkinEditor
  Left = 111
  Top = 95
  ActiveControl = Button5
  BorderStyle = bsToolWindow
  Caption = 'Skin Editor'
  ClientHeight = 351
  ClientWidth = 804
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label5: TLabel
    Left = 304
    Top = 228
    Width = 28
    Height = 13
    Caption = 'Width'
  end
  object Label6: TLabel
    Left = 304
    Top = 252
    Width = 31
    Height = 13
    Caption = 'Height'
  end
  object Label7: TLabel
    Left = 8
    Top = 296
    Width = 19
    Height = 13
    Caption = 'X: 0'
  end
  object Label8: TLabel
    Left = 80
    Top = 296
    Width = 19
    Height = 13
    Caption = 'Y: 0'
  end
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 258
    Height = 258
    TabOrder = 0
    object Image1: TImage
      Left = 1
      Top = 1
      Width = 256
      Height = 256
      Align = alClient
      OnMouseDown = Image1MouseDown
      OnMouseMove = Image1MouseMove
      OnMouseUp = Image1MouseUp
    end
  end
  object Panel2: TPanel
    Left = 304
    Top = 8
    Width = 130
    Height = 130
    TabOrder = 1
    object Image2: TImage
      Left = 1
      Top = 1
      Width = 128
      Height = 128
      Align = alClient
    end
  end
  object ScrollBar1: TScrollBar
    Left = 272
    Top = 8
    Width = 17
    Height = 257
    Kind = sbVertical
    LargeChange = 64
    Max = 256
    Min = 1
    PageSize = 256
    Position = 1
    TabOrder = 2
    OnChange = ScrollbarChange
    OnScroll = ScrollBarScroll
  end
  object ScrollBar2: TScrollBar
    Left = 8
    Top = 272
    Width = 257
    Height = 17
    LargeChange = 64
    Max = 256
    Min = 1
    PageSize = 256
    Position = 1
    TabOrder = 3
    OnChange = ScrollbarChange
    OnScroll = ScrollBarScroll
  end
  object Panel3: TPanel
    Left = 304
    Top = 144
    Width = 129
    Height = 27
    BevelOuter = bvLowered
    TabOrder = 4
    object Label2: TLabel
      Left = 80
      Top = 7
      Width = 15
      Height = 13
      Alignment = taRightJustify
      Caption = '1.0'
    end
    object Label1: TLabel
      Left = 12
      Top = 7
      Width = 30
      Height = 13
      Caption = 'Zoom:'
    end
    object Button3: TButton
      Left = 113
      Top = 1
      Width = 15
      Height = 12
      Caption = '+'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 113
      Top = 13
      Width = 15
      Height = 12
      Caption = '-'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      OnClick = Button4Click
    end
  end
  object CheckBox1: TCheckBox
    Left = 304
    Top = 200
    Width = 129
    Height = 17
    Caption = 'Show Preview'
    TabOrder = 5
    OnClick = CheckBox1Click
  end
  object WidthEdit: TEdit
    Left = 344
    Top = 224
    Width = 89
    Height = 21
    TabOrder = 6
    Text = '256'
    OnChange = WidthEditChange
  end
  object HeightEdit: TEdit
    Left = 344
    Top = 248
    Width = 89
    Height = 21
    TabOrder = 7
    Text = '256'
    OnChange = HeightEditChange
  end
  object Button5: TButton
    Left = 624
    Top = 320
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 8
  end
  object Button6: TButton
    Left = 712
    Top = 320
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 9
  end
  object GroupBox1: TGroupBox
    Left = 440
    Top = 8
    Width = 361
    Height = 305
    Caption = 'Element List'
    TabOrder = 10
    object Label3: TLabel
      Left = 160
      Top = 16
      Width = 81
      Height = 13
      Caption = 'Skin Elements'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label4: TLabel
      Left = 8
      Top = 44
      Width = 43
      Height = 13
      Caption = 'Skin Part'
    end
    object Label11: TLabel
      Left = 8
      Top = 80
      Width = 18
      Height = 13
      Caption = 'Left'
    end
    object Label12: TLabel
      Left = 8
      Top = 104
      Width = 19
      Height = 13
      Caption = 'Top'
    end
    object Label13: TLabel
      Left = 8
      Top = 128
      Width = 25
      Height = 13
      Caption = 'Right'
    end
    object Label9: TLabel
      Left = 8
      Top = 180
      Width = 37
      Height = 13
      Caption = 'Scale X'
    end
    object Label10: TLabel
      Left = 8
      Top = 204
      Width = 37
      Height = 13
      Caption = 'Scale Y'
    end
    object Label14: TLabel
      Left = 8
      Top = 152
      Width = 33
      Height = 13
      Caption = 'Bottom'
    end
    object ListBox1: TListBox
      Left = 160
      Top = 40
      Width = 193
      Height = 257
      ItemHeight = 13
      TabOrder = 0
      OnClick = ListBox1Click
      OnKeyDown = ListBox1KeyDown
    end
    object Button1: TButton
      Left = 256
      Top = 16
      Width = 49
      Height = 17
      Caption = '+'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 312
      Top = 16
      Width = 41
      Height = 17
      Caption = '-'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      OnClick = Button2Click
    end
    object ComboBox1: TComboBox
      Left = 72
      Top = 40
      Width = 81
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 3
      OnChange = ComboBox1Change
      Items.Strings = (
        'TopLeft'
        'Top'
        'TopRight'
        'Left'
        'Center'
        'Right'
        'BottomLeft'
        'Bottom'
        'BottomRight')
    end
    object LeftEdit: TEdit
      Left = 72
      Top = 72
      Width = 81
      Height = 21
      TabOrder = 4
      Text = '256'
      OnChange = LeftEditChange
    end
    object TopEdit: TEdit
      Left = 72
      Top = 96
      Width = 81
      Height = 21
      TabOrder = 5
      Text = '256'
      OnChange = TopEditChange
    end
    object RightEdit: TEdit
      Left = 72
      Top = 120
      Width = 81
      Height = 21
      TabOrder = 6
      Text = '256'
      OnChange = RightEditChange
    end
    object BottomEdit: TEdit
      Left = 72
      Top = 144
      Width = 81
      Height = 21
      TabOrder = 7
      Text = '256'
      OnChange = BottomEditChange
    end
    object ScaleXEdit: TEdit
      Left = 72
      Top = 168
      Width = 81
      Height = 21
      TabOrder = 8
      Text = '1'
      OnChange = ScaleXEditChange
    end
    object ScaleYEdit: TEdit
      Left = 72
      Top = 196
      Width = 81
      Height = 21
      TabOrder = 9
      Text = '1'
      OnChange = ScaleYEditChange
    end
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 16
    object HUDSprite1: TGLHUDSprite
      Position.Coordinates = {0000804300008043000000000000803F}
      Visible = False
      Material.FrontProperties.Ambient.Color = {938C0C3E938E0E3F938C0C3E0000803F}
      Material.FrontProperties.Diffuse.Color = {938C0C3E938E0E3F938C0C3E0000803F}
      Material.FrontProperties.Emission.Color = {938C0C3E938E0E3F938C0C3E0000803F}
      Material.FrontProperties.Specular.Color = {938C0C3E938E0E3F938C0C3E0000803F}
      Width = 512
      Height = 512
      NoZWrite = False
    end
    object GLPanel1: TGLPanel
      RedrawAtOnce = False
      NoZWrite = False
      Visible = False
      Width = 256
      Height = 256
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100
      FocalLength = 50
      Position.Coordinates = {0000000000000000000020410000803F}
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1
        LightStyle = lsOmni
        SpotCutOff = 180
      end
    end
  end
  object GLMemoryViewer1: TGLMemoryViewer
    Camera = GLCamera1
    Buffer.ContextOptions = []
    Buffer.DepthTest = False
    Buffer.FaceCulling = False
    Buffer.Lighting = False
    Buffer.ColorDepth = cd8bits
    Left = 16
    Top = 64
  end
end
