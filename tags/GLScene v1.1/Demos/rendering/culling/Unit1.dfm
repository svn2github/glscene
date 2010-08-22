object Form1: TForm1
  Left = 188
  Top = 112
  Width = 544
  Height = 340
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    536
    311)
  PixelsPerInch = 96
  TextHeight = 14
  object Label1: TLabel
    Left = 440
    Top = 8
    Width = 75
    Height = 15
    Anchors = [akTop, akRight]
    Caption = 'Culling Mode:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 440
    Top = 120
    Width = 47
    Height = 15
    Anchors = [akTop, akRight]
    Caption = 'Objects:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Viewer: TGLSceneViewer
    Left = 8
    Top = 8
    Width = 425
    Height = 297
    Camera = GLCamera1
    Buffer.BackgroundColor = 12040119
    Anchors = [akLeft, akTop, akRight, akBottom]
  end
  object RBNone: TRadioButton
    Left = 440
    Top = 32
    Width = 89
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'None'
    Checked = True
    TabOrder = 1
    TabStop = True
    OnClick = RBNoneClick
  end
  object RBObject: TRadioButton
    Left = 440
    Top = 56
    Width = 89
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'Object based'
    TabOrder = 2
    OnClick = RBNoneClick
  end
  object RBHierarchical: TRadioButton
    Left = 440
    Top = 80
    Width = 89
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'Hierarchical'
    TabOrder = 3
    Visible = False
    OnClick = RBNoneClick
  end
  object Panel1: TPanel
    Left = 440
    Top = 144
    Width = 89
    Height = 65
    Anchors = [akTop, akRight]
    BevelOuter = bvNone
    TabOrder = 4
    object RBSpheres: TRadioButton
      Left = 0
      Top = 0
      Width = 81
      Height = 17
      Caption = 'Spheres'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = RBSpheresClick
    end
    object RBActors: TRadioButton
      Left = 0
      Top = 24
      Width = 81
      Height = 17
      Caption = 'Actors'
      TabOrder = 1
      OnClick = RBSpheresClick
    end
  end
  object GLScene: TGLScene
    Left = 16
    Top = 16
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1
      Position.Coordinates = {0000C8420000A042000070420000803F}
      SpotCutOff = 180
    end
    object DCTarget: TGLDummyCube
      CubeSize = 1
      object GLCamera1: TGLCamera
        DepthOfView = 100
        FocalLength = 50
        TargetObject = DCTarget
        Position.Coordinates = {0000A04000000040000040400000803F}
        Left = 256
        Top = 144
      end
    end
    object DCSpheres: TGLDummyCube
      CubeSize = 1
    end
    object DCActors: TGLDummyCube
      Visible = False
      CubeSize = 1
    end
    object ACReference: TGLActor
      Direction.Coordinates = {000000000000803F2EBD3BB300000000}
      Position.Coordinates = {CDCC4C3DCDCC4C3DCDCC4C3D0000803F}
      Scale.Coordinates = {CDCC4C3DCDCC4C3DCDCC4C3D00000000}
      Up.Coordinates = {000000002EBD3BB3000080BF00000000}
      Material.MaterialLibrary = GLMaterialLibrary
      Material.LibMaterialName = 'ActorTexture'
      AnimationMode = aamLoop
      Interval = 100
    end
  end
  object GLCadencer: TGLCadencer
    Scene = GLScene
    OnProgress = GLCadencerProgress
    Left = 48
    Top = 16
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 16
    Top = 48
  end
  object GLMaterialLibrary: TGLMaterialLibrary
    Materials = <
      item
        Name = 'ActorTexture'
        Material.Texture.Disabled = False
        Tag = 0
      end>
    Left = 48
    Top = 48
  end
end
