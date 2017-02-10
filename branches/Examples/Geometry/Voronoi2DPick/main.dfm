object Form1: TForm1
  Left = 214
  Top = 148
  Caption = 'Voronoi demo'
  ClientHeight = 581
  ClientWidth = 853
  Color = clSilver
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PaintBox1: TPaintBox
    Left = 0
    Top = 53
    Width = 853
    Height = 528
    Cursor = crCross
    Align = alClient
    Color = clBlack
    DragCursor = crSizeAll
    ParentColor = False
    OnMouseDown = PaintBox1MouseDown
    OnMouseMove = PaintBox1MouseMove
    OnMouseUp = PaintBox1MouseUp
    OnPaint = PaintBox1Paint
    ExplicitWidth = 861
    ExplicitHeight = 540
  end
  object GroupBox1: TGroupBox
    Left = 0
    Top = 0
    Width = 853
    Height = 53
    Align = alTop
    Caption = 'Options'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMenu
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    TabOrder = 0
    object Label1: TLabel
      Left = 580
      Top = 24
      Width = 83
      Height = 13
      Caption = '! Wrong Cut !'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'Verdana'
      Font.Style = [fsBold]
      ParentFont = False
      Visible = False
    end
    object CheckBox2: TCheckBox
      Left = 184
      Top = 20
      Width = 145
      Height = 17
      Caption = 'Show Triangulation'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = CheckBox1Click
    end
    object CheckBox3: TCheckBox
      Left = 336
      Top = 20
      Width = 141
      Height = 17
      Caption = 'Show Point Indices'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = CheckBox1Click
    end
    object Button1: TButton
      Left = 488
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Clear'
      TabOrder = 3
      OnClick = Button1Click
    end
    object CheckBox1: TCheckBox
      Left = 24
      Top = 20
      Width = 145
      Height = 17
      Caption = 'Show Convex Hull'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = CheckBox1Click
    end
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 563
    Top = 59
    Width = 100
    Height = 100
    TabOrder = 1
  end
  object GLScene1: TGLScene
    Left = 56
    Top = 88
  end
  object GLCadencer1: TGLCadencer
    Left = 144
    Top = 88
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Left = 248
    Top = 88
  end
end
