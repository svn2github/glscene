object MaterialEditorForm: TMaterialEditorForm
  Left = 141
  Top = 95
  BorderStyle = bsDialog
  Caption = 'Material Editor'
  ClientHeight = 289
  ClientWidth = 560
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 232
    Width = 71
    Height = 13
    Caption = 'Blending Mode'
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 8
    Width = 313
    Height = 217
    ActivePage = TSFront
    Style = tsButtons
    TabOrder = 0
    object TSFront: TTabSheet
      Caption = 'Front'
      inline FEFront: TRFaceEditor
        inherited ImageList: TImageList
          Top = 0
        end
      end
    end
    object TSBack: TTabSheet
      Caption = 'Back'
      ImageIndex = 1
      inline FEBack: TRFaceEditor
      end
    end
    object TSTexture: TTabSheet
      Caption = 'Texture'
      ImageIndex = 2
      inline RTextureEdit: TRTextureEdit
        Width = 305
        Height = 186
        Align = alClient
        inherited SBEditImage: TSpeedButton
          Left = 287
        end
        inherited CBImageClass: TComboBox
          Width = 213
        end
      end
    end
  end
  object GroupBox1: TGroupBox
    Left = 320
    Top = 8
    Width = 233
    Height = 241
    Caption = 'Material Preview'
    TabOrder = 1
    inline MPPreview: TRMaterialPreview
      Left = 16
      Top = 22
      inherited GLScene1: TGLScene
        inherited Cube: TGLCube
          Direction.Coordinates = {FCFAF0B1D8B35D3FFEFFFF3E00000000}
          Up.Coordinates = {D7B35DBFFFFF7F3ED7B3DDBE00000000}
          Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
        end
        inherited Sphere: TGLSphere
          Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
        end
        inherited LightSource: TGLLightSource
          Position.Coordinates = {0000000000004040000020410000803F}
          Specular.Color = {0000803F0000803F0000803F0000803F}
        end
        inherited PlanePattern: TGLPlane
          Position.Coordinates = {0000000000000000000040C00000803F}
          Material.Texture.MagFilter = maNearest
          Material.Texture.MinFilter = miNearest
          Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
        end
        inherited Camera: TGLCamera
          Position.Coordinates = {0000000000000000000020410000803F}
        end
      end
    end
  end
  object BBOk: TBitBtn
    Left = 376
    Top = 256
    Width = 83
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
    Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333333333333333330000333333333333333333333333F33333333333
      00003333344333333333333333388F3333333333000033334224333333333333
      338338F3333333330000333422224333333333333833338F3333333300003342
      222224333333333383333338F3333333000034222A22224333333338F338F333
      8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
      33333338F83338F338F33333000033A33333A222433333338333338F338F3333
      0000333333333A222433333333333338F338F33300003333333333A222433333
      333333338F338F33000033333333333A222433333333333338F338F300003333
      33333333A222433333333333338F338F00003333333333333A22433333333333
      3338F38F000033333333333333A223333333333333338F830000333333333333
      333A333333333333333338330000333333333333333333333333333333333333
      0000}
    NumGlyphs = 2
  end
  object BBCancel: TBitBtn
    Left = 472
    Top = 256
    Width = 83
    Height = 25
    TabOrder = 3
    Kind = bkCancel
  end
  object CBBlending: TComboBox
    Left = 88
    Top = 229
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 4
    OnChange = OnMaterialChanged
    Items.Strings = (
      'bmOpaque'
      'bmTransparency'
      'bmAdditive')
  end
end
