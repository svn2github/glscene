object RFaceEditor: TRFaceEditor
  Left = 0
  Top = 0
  Width = 305
  Height = 186
  AutoSize = True
  TabOrder = 0
  object Label1: TLabel
    Left = 0
    Top = 6
    Width = 45
    Height = 13
    Caption = 'Shininess'
  end
  object Label2: TLabel
    Left = 0
    Top = 32
    Width = 67
    Height = 13
    Caption = 'Polygon mode'
  end
  object PageControl: TPageControl
    Left = 0
    Top = 56
    Width = 305
    Height = 130
    ActivePage = TSAmbient
    Images = ImageList
    MultiLine = True
    TabOrder = 0
    object TSAmbient: TTabSheet
      BorderWidth = 3
      Caption = 'Ambient'
      inline CEAmbiant: TRColorEditor
      end
    end
    object TSDiffuse: TTabSheet
      BorderWidth = 3
      Caption = 'Diffuse'
      ImageIndex = 1
      inline CEDiffuse: TRColorEditor
      end
    end
    object TSEmission: TTabSheet
      BorderWidth = 3
      Caption = 'Emission'
      ImageIndex = 2
      inline CEEmission: TRColorEditor
      end
    end
    object TSSpecular: TTabSheet
      BorderWidth = 3
      Caption = 'Specular'
      ImageIndex = 3
      inline CESpecular: TRColorEditor
      end
    end
  end
  inline TBEShininess: TRTrackBarEdit
    Left = 52
    Width = 201
    Height = 21
    TabOrder = 1
    inherited TrackBar: TTrackBar
      Max = 128
      Frequency = 16
      OnChange = TBEShininessTrackBarChange
    end
    inherited Edit: TEdit
      Left = 160
    end
  end
  object CBPolygonMode: TComboBox
    Left = 80
    Top = 28
    Width = 89
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
    OnChange = CBPolygonModeChange
    Items.Strings = (
      'pmFill'
      'pmLines'
      'pmPoints')
  end
  object ImageList: TImageList
    Left = 264
    Top = 8
  end
end
