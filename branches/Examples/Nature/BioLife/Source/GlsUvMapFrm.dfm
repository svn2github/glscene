object GLS3dUvForm: TGLS3dUvForm
  Left = 145
  Top = 76
  Width = 593
  Height = 459
  Caption = 'GLS 3D UV Mesh'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    00000000000000000000000000000008FFF00000000000000000000000000088
    7777000000000000000000000000008800770F00000000000000000000000880
    88F70FFF0000000000000000000008808880FFFFF00000000000000000000888
    000FFF8FFFF0000000000000000007888800F8088FFF00000000000000000078
    888800F0088F8800000000000000000077700FFFF00888880000000000000000
    000FFFFFFFF088800000000000000000000000FFF8880008BBBBBB8800000000
    000000000800BBBBBBBBBBBBB80000000000000000BBBBBBBBBBBBBBBBB00000
    00000000BBBBBBBBB0000BBBBBB00000000000BB87000000000BB0BBBB800000
    000000008B8BBBBBBBBBB00BB80000000000BBBBBBBBBBBBBBBBBBB000000000
    00BBBBBB0000000000BBB0BB0F000000BBBBB0008888888870BB0B0B0FF0000B
    BBB00888BBBBBBB070BB0BB00FF0000BB0088BBBBBBBBBB070BB0B700000000B
    088BBBBBBBBBBB070BB0BB00000000008BBBBBBBBBBBBB070BB0BB0000000000
    0BBBBBBBBBBBB070BB0BB7000000000008BBBBBBBBBBB070BB0BB00000000000
    008BBBBBBBBB070BB0BB0000000000000000BBBBBBB070BB0BB0000000000000
    0000088888070BB0B0000000000000000000000000BBBB000000000000000000
    00000000BBBB000000000000000000000000000000000000000000000000E07F
    FFFFC01FFFFF8007FFFF8003FFFF0000FFFF00007FFF00001FFF000007FF8000
    03FFC0000007F0000001FC000000FFC00000FFE00000FFC00000FF800000FE00
    0001F8000000F0000000E0000000C0000000C0000009C000000FC000000FF000
    001FF000003FF800007FFC0000FFFF0001FFFF8007FFFFC03FFFFFF0FFFF}
  Menu = MainMenu1
  OldCreateOrder = False
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 188
    Top = 0
    Width = 3
    Height = 394
    Cursor = crHSplit
  end
  object ScrollPanel: TPanel
    Left = 0
    Top = 0
    Width = 188
    Height = 394
    Align = alLeft
    TabOrder = 0
    object PageControl1: TPageControl
      Left = 1
      Top = 1
      Width = 186
      Height = 392
      ActivePage = TabSheet3
      Align = alClient
      TabOrder = 0
      object TabSheet1: TTabSheet
        Caption = 'UV Paint'
        object Panel1: TPanel
          Left = 0
          Top = 0
          Width = 178
          Height = 93
          Align = alTop
          TabOrder = 0
          object MeshSLABtn: TSpeedButton
            Left = 144
            Top = 24
            Width = 23
            Height = 22
          end
          object BrushColorBtn: TSpeedButton
            Left = 144
            Top = 56
            Width = 23
            Height = 22
          end
          object BrushColor: TSpeedButton
            Left = 89
            Top = 56
            Width = 25
            Height = 25
            Glyph.Data = {
              66010000424D6601000000000000760000002800000014000000140000000100
              040000000000F000000000000000000000001000000010000000000000000000
              BF0000BF000000BFBF00BF000000BF00BF00BFBF0000C0C0C000808080000000
              FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00999BBBBFAAAA
              4CCCFFFF0000999BBBBFAAAACCCCFFFF0000999BBBBFAAAA4CCCFFFF0000999B
              BBBFAAAACCCCFFFF0000999BBBBFAAAA4CCCFFFF0000999BBBBFAAAACCCCFFFF
              0000999BBBBFAAAA4CCCFFFF0000999BBBBFAAAACCCCFFFF0000999BBBBFAAAA
              4CCCFFFF0000999BBBBFAAAACCCCFFFF0000999BBBBFAAAA4CCCFFFF0000999B
              BBBFAAAACCCCFFFF0000999BBBBFAAAA4CCCFFFF0000999BBBBFAAAACCCCFFFF
              0000999BBBBFAAAA4CCCFFFF0000999BBBBFAAAACCCCFFFF0000999BBBBFAAAA
              4CCCFFFF0000999BBBBFAAAACCCCFFFF0000999BBBBFAAAA4CCCFFFF0000999B
              BBFFAAAFFCCCFFFF0000}
            OnClick = BrushColorClick
          end
          object PenColor: TSpeedButton
            Left = 57
            Top = 56
            Width = 25
            Height = 25
            Glyph.Data = {
              66010000424D6601000000000000760000002800000014000000140000000100
              040000000000F000000000000000000000001000000010000000000000000000
              BF0000BF000000BFBF00BF000000BF00BF00BFBF0000C0C0C000808080000000
              FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00999BBBBFAAAA
              4CCCFFFF0000999BBBBFAAAACCCCFFFF0000999BBBBFAAAA4CCCFFFF0000999B
              BBBFAAAACCCCFFFF0000999BBBBFAAAA4CCCFFFF0000999BBBBFAAAACCCCFFFF
              0000999BBBBFAAAA4CCCFFFF0000999BBBBFAAAACCCCFFFF0000999BBBBFAAAA
              4CCCFFFF0000999BBBBFAAAACCCCFFFF0000999BBBBFAAAA4CCCFFFF0000999B
              BBBFAAAACCCCFFFF0000999BBBBFAAAA4CCCFFFF0000999BBBBFAAAACCCCFFFF
              0000999BBBBFAAAA4CCCFFFF0000999BBBBFAAAACCCCFFFF0000999BBBBFAAAA
              4CCCFFFF0000999BBBBFAAAACCCCFFFF0000999BBBBFAAAA4CCCFFFF0000999B
              BBFFAAAFFCCCFFFF0000}
            OnClick = PenColorClick
          end
          object CheckBox1: TCheckBox
            Left = 90
            Top = 4
            Width = 71
            Height = 17
            Hint = 'Abbie Normal'
            Caption = 'Inv Norm'
            TabOrder = 0
            OnClick = CheckBox1Click
          end
          object ComboBox2: TComboBox
            Left = 2
            Top = 24
            Width = 135
            Height = 21
            Hint = 'UV Mapping type'
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 1
            OnChange = ComboBox2Change
            Items.Strings = (
              '0 - Planar mapping'
              '1 - Cubic mapping'
              '2 - Cylindrical mapping'
              '3 - Spherical mapping')
          end
          object ComboBox1: TComboBox
            Left = 2
            Top = 2
            Width = 89
            Height = 21
            Hint = 'Display Axis'
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 2
            OnChange = ComboBox1Change
            Items.Strings = (
              '0 - X Axis'
              '1 - Y Axis'
              '2 - Z Axis')
          end
          object PenEdit: TEdit
            Left = 8
            Top = 56
            Width = 25
            Height = 21
            TabOrder = 3
            Text = '1'
            OnChange = PenEditChange
          end
          object PenWidth: TUpDown
            Left = 33
            Top = 56
            Width = 16
            Height = 21
            Associate = PenEdit
            Min = 1
            Position = 1
            TabOrder = 4
            Wrap = False
          end
        end
        object ScrollBox1: TScrollBox
          Left = 0
          Top = 93
          Width = 178
          Height = 271
          Align = alClient
          TabOrder = 1
          object Image: TImage
            Left = 0
            Top = 0
            Width = 128
            Height = 128
            AutoSize = True
            OnMouseDown = ImageMouseDown
            OnMouseMove = ImageMouseMove
            OnMouseUp = ImageMouseUp
          end
        end
      end
      object TabSheet2: TTabSheet
        Caption = 'Mesh Edit'
        ImageIndex = 1
        object Label1: TLabel
          Left = 8
          Top = 8
          Width = 68
          Height = 13
          Caption = 'Polygon Mode'
        end
        object Label6: TLabel
          Left = 8
          Top = 192
          Width = 86
          Height = 13
          Caption = 'Camera X Position'
        end
        object cbPolygonMode: TComboBox
          Left = 8
          Top = 24
          Width = 145
          Height = 21
          Hint = 'Point Line Area'
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
          OnChange = cbPolygonModeChange
          Items.Strings = (
            'Fill'
            'Lines / mesh'
            'Points')
        end
        object chbViewPoints: TCheckBox
          Left = 8
          Top = 56
          Width = 121
          Height = 17
          Hint = 'Toggle node Display'
          Caption = 'View vertex points'
          Checked = True
          State = cbChecked
          TabOrder = 1
          OnClick = chbViewPointsClick
        end
        object GroupBox1: TGroupBox
          Left = 8
          Top = 80
          Width = 169
          Height = 105
          Caption = 'Axis'
          TabOrder = 2
          object Bevel1: TBevel
            Left = 8
            Top = 40
            Width = 153
            Height = 2
            Shape = bsBottomLine
          end
          object Label2: TLabel
            Left = 8
            Top = 48
            Width = 58
            Height = 13
            Caption = 'Axis Legend'
          end
          object Label3: TLabel
            Left = 8
            Top = 72
            Width = 29
            Height = 13
            Caption = 'X Axis'
            Color = clBtnFace
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clRed
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentColor = False
            ParentFont = False
          end
          object Label4: TLabel
            Left = 48
            Top = 72
            Width = 29
            Height = 13
            Caption = 'Y Axis'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clGreen
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
          end
          object Label5: TLabel
            Left = 88
            Top = 72
            Width = 29
            Height = 13
            Caption = 'Z Axis'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clNavy
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
          end
          object chbShowAxis: TCheckBox
            Left = 8
            Top = 16
            Width = 153
            Height = 17
            Hint = 'What is Up Doc'
            Caption = 'Show central axis'
            Checked = True
            State = cbChecked
            TabOrder = 0
            OnClick = chbShowAxisClick
          end
        end
        object tbPos: TTrackBar
          Left = 8
          Top = 208
          Width = 169
          Height = 45
          Hint = 'See me'
          Max = 20
          Orientation = trHorizontal
          Frequency = 1
          Position = 5
          SelEnd = 0
          SelStart = 0
          TabOrder = 3
          TickMarks = tmBottomRight
          TickStyle = tsAuto
          OnChange = tbPosChange
        end
        object GroupBox2: TGroupBox
          Left = 8
          Top = 248
          Width = 169
          Height = 65
          Hint = 'Control'
          Caption = 'Movement Direction'
          TabOrder = 4
          object rbXY: TRadioButton
            Left = 8
            Top = 24
            Width = 113
            Height = 17
            Caption = 'Move on X, Y axis'
            Checked = True
            TabOrder = 0
            TabStop = True
          end
          object rbZY: TRadioButton
            Left = 8
            Top = 40
            Width = 113
            Height = 17
            Caption = 'Move on Z, Y axis'
            TabOrder = 1
          end
        end
      end
      object TabSheet3: TTabSheet
        Caption = 'Mesh Data'
        ImageIndex = 2
        object ScrollBox2: TScrollBox
          Left = 0
          Top = 89
          Width = 178
          Height = 275
          Align = alClient
          TabOrder = 0
          object MeshDataListBox: TListBox
            Left = 0
            Top = 0
            Width = 561
            Height = 233
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Courier'
            Font.Style = []
            ItemHeight = 13
            ParentFont = False
            TabOrder = 0
          end
        end
        object Panel2: TPanel
          Left = 0
          Top = 0
          Width = 178
          Height = 89
          Align = alTop
          TabOrder = 1
          object btnGroups: TBitBtn
            Left = 79
            Top = 56
            Width = 86
            Height = 25
            Caption = 'Triangle Count'
            TabOrder = 0
            OnClick = btnGroupsClick
          end
          object Button2: TButton
            Left = 8
            Top = 4
            Width = 137
            Height = 25
            Caption = 'Show Vector / TexCoord'
            TabOrder = 1
          end
          object btnTextcoords: TBitBtn
            Left = 79
            Top = 32
            Width = 86
            Height = 25
            Caption = 'Tex-Coords'
            TabOrder = 2
            OnClick = btnTextcoordsClick
          end
          object btnVertex: TBitBtn
            Left = 4
            Top = 32
            Width = 75
            Height = 25
            Caption = 'Vertex'
            TabOrder = 3
            OnClick = btnVertexClick
          end
          object btnNormals: TBitBtn
            Left = 4
            Top = 56
            Width = 75
            Height = 25
            Caption = 'Normals'
            TabOrder = 4
            OnClick = btnNormalsClick
          end
        end
      end
    end
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 191
    Top = 0
    Width = 394
    Height = 394
    Camera = GLCamera1
    BeforeRender = GLSceneViewer1BeforeRender
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    OnMouseUp = GLSceneViewer1MouseUp
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 394
    Width = 585
    Height = 19
    Panels = <>
    SimplePanel = False
  end
  object GLScene1: TGLScene
    Left = 288
    Top = 24
    object GLFreeForm1: TGLFreeForm
    end
    object dcModifiers: TGLDummyCube
      CubeSize = 1
      object GLCamera1: TGLCamera
        DepthOfView = 100
        FocalLength = 50
        TargetObject = dcModifiers
        Position.Coordinates = {0000A0400000A0400000A0400000803F}
        object GLLightSource1: TGLLightSource
          ConstAttenuation = 1
          SpotCutOff = 180
        end
      end
    end
  end
  object MainMenu1: TMainMenu
    Left = 232
    Top = 16
    object File1: TMenuItem
      Caption = 'File'
      object Open3ds1: TMenuItem
        Caption = 'Open mesh 3ds..obj'
        OnClick = Open3ds1Click
      end
      object Save3ds1: TMenuItem
        Caption = 'Save mesh as .obj'
        OnClick = Save3ds1Click
      end
      object Savemeshasbmp1: TMenuItem
        Caption = 'Save mesh as .bmp'
        OnClick = Savemeshasbmp1Click
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Mesh1: TMenuItem
        Caption = 'Mesh A Matic'
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
    object Map1: TMenuItem
      Caption = 'Texture'
      object Image2: TMenuItem
        Caption = 'Texture Image'
        object Clear1: TMenuItem
          Caption = 'Clear'
          OnClick = Clear1Click
        end
        object N128x1281: TMenuItem
          Caption = 'Set Size: 128 x 128'
          OnClick = N128x1281Click
        end
        object N256x2561: TMenuItem
          Caption = 'Set Size: 256 x 256'
          OnClick = N256x2561Click
        end
        object N512x5121: TMenuItem
          Caption = 'Set Size: 512 x 512'
          OnClick = N512x5121Click
        end
        object N1024x10241: TMenuItem
          Caption = 'Set Size: 1024 x 1024'
          OnClick = N1024x10241Click
        end
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object LoadTexture1: TMenuItem
        Caption = 'Load Texture'
        OnClick = LoadTexture1Click
      end
      object SaveTexture1: TMenuItem
        Caption = 'Save Texture'
        OnClick = SaveTexture1Click
      end
      object SaveAsTexture1: TMenuItem
        Caption = 'Save As Texture'
        OnClick = SaveAsTexture1Click
      end
      object Cut1: TMenuItem
        Caption = 'Cut'
        OnClick = Cut1Click
      end
      object Copy1: TMenuItem
        Caption = 'Copy'
        OnClick = Copy1Click
      end
      object Paste1: TMenuItem
        Caption = 'Paste'
        OnClick = Paste1Click
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object ApplyTexture1: TMenuItem
        Caption = 'Apply Texture'
        OnClick = ApplyTexture1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object PaintPot1: TMenuItem
        Caption = 'Paint Pot'
        object Pen1: TMenuItem
          Caption = 'Pen Style'
          object SolidPen: TMenuItem
            Caption = 'Solid'
            Checked = True
            GroupIndex = 1
            RadioItem = True
            OnClick = SetPenStyle
          end
          object DashPen: TMenuItem
            Caption = 'Dash'
            GroupIndex = 1
            RadioItem = True
            OnClick = SetPenStyle
          end
          object DotPen: TMenuItem
            Caption = 'Dot'
            GroupIndex = 1
            RadioItem = True
            OnClick = SetPenStyle
          end
          object DashDotPen: TMenuItem
            Caption = 'Dash Dot'
            GroupIndex = 1
            RadioItem = True
            OnClick = SetPenStyle
          end
          object DashDotDotPen: TMenuItem
            Caption = 'Dash Dot Dot'
            GroupIndex = 1
            RadioItem = True
            OnClick = SetPenStyle
          end
          object ClearPen: TMenuItem
            Caption = 'Clear'
            GroupIndex = 1
            RadioItem = True
            OnClick = SetPenStyle
          end
        end
        object Brush1: TMenuItem
          Caption = 'Brush Style'
          object SolidBrush: TMenuItem
            Caption = 'Solid'
            Checked = True
            GroupIndex = 2
            RadioItem = True
            OnClick = SetBrushStyle
          end
          object ClearBrush: TMenuItem
            Caption = 'Clear'
            GroupIndex = 2
            RadioItem = True
            OnClick = SetBrushStyle
          end
          object HorizontalBrush: TMenuItem
            Caption = 'Horizontal'
            GroupIndex = 2
            RadioItem = True
            OnClick = SetBrushStyle
          end
          object VerticalBrush: TMenuItem
            Caption = 'Vertical'
            GroupIndex = 2
            RadioItem = True
            OnClick = SetBrushStyle
          end
          object FDiagonalBrush: TMenuItem
            Caption = 'FDiagonal'
            GroupIndex = 2
            RadioItem = True
            OnClick = SetBrushStyle
          end
          object BDiagonalBrush: TMenuItem
            Caption = 'BDiagonal'
            GroupIndex = 2
            RadioItem = True
            OnClick = SetBrushStyle
          end
          object CrossBrush: TMenuItem
            Caption = 'Cross'
            GroupIndex = 2
            RadioItem = True
            OnClick = SetBrushStyle
          end
          object DiagCrossBrush: TMenuItem
            Caption = 'DiagCross'
            GroupIndex = 2
            RadioItem = True
            OnClick = SetBrushStyle
          end
        end
        object BrushStyle1: TMenuItem
          Caption = 'Brush Shape'
          object Line1: TMenuItem
            Caption = 'Line'
            Checked = True
            GroupIndex = 3
            RadioItem = True
            OnClick = Line1Click
          end
          object Rectangle1: TMenuItem
            Caption = 'Rectangle'
            GroupIndex = 3
            RadioItem = True
            OnClick = Rectangle1Click
          end
          object Ellipse1: TMenuItem
            Caption = 'Ellipse'
            GroupIndex = 3
            RadioItem = True
            OnClick = Ellipse1Click
          end
          object RoundRect1: TMenuItem
            Caption = 'Round Rect'
            GroupIndex = 3
            RadioItem = True
            OnClick = RoundRect1Click
          end
        end
      end
    end
    object Help1: TMenuItem
      Caption = 'Help'
      object Help2: TMenuItem
        Caption = 'Help'
        OnClick = Help2Click
      end
      object About1: TMenuItem
        Caption = 'About'
        OnClick = About1Click
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Filter = '3ds  obj|*.3ds;*.obj'
    Left = 248
    Top = 56
  end
  object SaveDialog1: TSaveDialog
    Filter = 'obj|*.obj'
    Left = 290
    Top = 58
  end
  object OpenPictureDialog1: TOpenPictureDialog
    FilterIndex = 7
    Left = 218
    Top = 90
  end
  object ColorDialog1: TColorDialog
    Ctl3D = True
    Left = 256
    Top = 88
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Left = 232
    Top = 144
  end
end
