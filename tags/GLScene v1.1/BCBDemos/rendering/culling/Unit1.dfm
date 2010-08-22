object Form1: TForm1
  Left = 264
  Top = 142
  Width = 577
  Height = 386
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  PixelsPerInch = 110
  TextHeight = 16
  object Label1: TLabel
    Left = 461
    Top = 9
    Width = 97
    Height = 16
    Anchors = [akTop, akRight]
    Caption = 'Culling Mode:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -14
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 461
    Top = 137
    Width = 58
    Height = 16
    Anchors = [akTop, akRight]
    Caption = 'Objects:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -14
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Viewer: TGLSceneViewer
    Left = 5
    Top = 5
    Width = 436
    Height = 339
    Camera = GLCamera1
    Buffer.BackgroundColor = 12040119
    FieldOfView = 147.129440307617
    Anchors = [akLeft, akTop, akRight, akBottom]
  end
  object RBNone: TRadioButton
    Left = 461
    Top = 37
    Width = 102
    Height = 19
    Anchors = [akTop, akRight]
    Caption = 'None'
    Checked = True
    TabOrder = 1
    TabStop = True
    OnClick = RBNoneClick
  end
  object RBObject: TRadioButton
    Left = 461
    Top = 64
    Width = 102
    Height = 19
    Anchors = [akTop, akRight]
    Caption = 'Object based'
    TabOrder = 2
    OnClick = RBNoneClick
  end
  object RBHierarchical: TRadioButton
    Left = 461
    Top = 91
    Width = 102
    Height = 20
    Anchors = [akTop, akRight]
    Caption = 'Hierarchical'
    TabOrder = 3
    OnClick = RBNoneClick
  end
  object Panel1: TPanel
    Left = 461
    Top = 165
    Width = 102
    Height = 56
    Anchors = [akTop, akRight]
    BevelOuter = bvNone
    TabOrder = 4
    object RBSpheres: TRadioButton
      Left = 0
      Top = 0
      Width = 93
      Height = 19
      Caption = 'Spheres'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = RBSpheresClick
    end
    object RBActors: TRadioButton
      Left = 0
      Top = 27
      Width = 93
      Height = 20
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
      Material.MaterialLibrary = GLMaterialLibrary
      Material.LibMaterialName = 'ActorTexture'
      Direction.Coordinates = {000000000000803F2EBD3BB300000000}
      Position.Coordinates = {CDCC4C3DCDCC4C3DCDCC4C3D0000803F}
      Scale.Coordinates = {CDCC4C3DCDCC4C3DCDCC4C3D00000000}
      Up.Coordinates = {000000002EBD3BB3000080BF00000000}
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
