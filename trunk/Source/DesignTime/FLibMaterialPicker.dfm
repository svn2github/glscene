object LibMaterialPicker: TLibMaterialPicker
  Left = 325
  Top = 93
  BorderStyle = bsDialog
  Caption = 'LibMaterial Picker'
  ClientHeight = 234
  ClientWidth = 458
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 152
    Top = 8
    Width = 78
    Height = 13
    Caption = 'Material Preview'
  end
  object Label2: TLabel
    Left = 8
    Top = 8
    Width = 88
    Height = 13
    Caption = 'Available Materials'
  end
  object LBMaterials: TListBox
    Left = 8
    Top = 24
    Width = 137
    Height = 201
    ItemHeight = 13
    TabOrder = 0
    OnClick = LBMaterialsClick
    OnKeyPress = LBMaterialsKeyPress
  end
  object BBOk: TBitBtn
    Left = 376
    Top = 24
    Width = 75
    Height = 25
    TabOrder = 2
    Kind = bkOK
  end
  object BBCancel: TBitBtn
    Left = 376
    Top = 56
    Width = 75
    Height = 25
    TabOrder = 1
    Kind = bkCancel
  end
  inline MPPreview: TRMaterialPreview
    Left = 152
    Top = 22
    TabOrder = 3
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
        Material.Texture.MappingTCoordinates.Coordinates = {000000000000803F0000000000000000}
      end
      inherited Camera: TGLCamera
        Position.Coordinates = {0000000000000000000020410000803F}
      end
    end
  end
end
