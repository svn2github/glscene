object Form1: TForm1
  Left = 86
  Top = 133
  Width = 696
  Height = 414
  Caption = 'Moving Objects with Mouse'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnKeyPress = FormKeyPress
  OnKeyUp = FormKeyUp
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  object Scn: TGLSceneViewer
    Left = 185
    Top = 0
    Width = 503
    Height = 385
    Camera = GLCamera1
    AfterRender = ScnAfterRender
    Buffer.BackgroundColor = clBlack
    Align = alClient
    OnMouseDown = ScnMouseDown
    OnMouseMove = ScnMouseMove
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 385
    Align = alLeft
    BevelOuter = bvNone
    Color = 2960685
    TabOrder = 1
    object Label2: TLabel
      Left = 8
      Top = 103
      Width = 165
      Height = 26
      Caption = 'Select and move with the mouse any of the two cubes.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Verdana'
      Font.Style = []
      ParentFont = False
      ShowAccelChar = False
      WordWrap = True
    end
    object Label1: TLabel
      Left = 8
      Top = 6
      Width = 134
      Height = 13
      Caption = 'Author: Rado Stoyanov'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Verdana'
      Font.Style = []
      ParentFont = False
      ShowAccelChar = False
      WordWrap = True
    end
    object Label3: TLabel
      Left = 8
      Top = 143
      Width = 163
      Height = 26
      Caption = 'Default movement is on the XY plane.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Verdana'
      Font.Style = []
      ParentFont = False
      ShowAccelChar = False
      WordWrap = True
    end
    object Label4: TLabel
      Left = 8
      Top = 183
      Width = 157
      Height = 26
      Caption = 'Shift + Drag moves on the XZ plane.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Verdana'
      Font.Style = []
      ParentFont = False
      ShowAccelChar = False
      WordWrap = True
    end
    object Label5: TLabel
      Left = 8
      Top = 22
      Width = 163
      Height = 13
      Caption = 'radostoyanov@softhome.net'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Verdana'
      Font.Style = []
      ParentFont = False
      ShowAccelChar = False
      WordWrap = True
    end
    object Button1: TButton
      Left = 618
      Top = 8
      Width = 155
      Height = 25
      Caption = 'Near: (0,0) Eye -> Obj'
      TabOrder = 0
    end
  end
  object GLScene1: TGLScene
    Left = 190
    Top = 6
    object Floor: TGLCube
      Position.Coordinates = {00000000000000005C8F82BF0000803F}
      Material.FrontProperties.Diffuse.Color = {C5C4C43ECDCCCC3E8382023FE3A53B3F}
      CubeSize = {00000040000000400AD7233C}
    end
    object TopLight1: TGLLightSource
      Ambient.Color = {0000003F0000003F0000003F0000803F}
      ConstAttenuation = 0.800000011920929
      Diffuse.Color = {EAE9693FEAE9693FEAE9693F0000803F}
      Position.Coordinates = {0000804100005041000040410000803F}
      SpotCutOff = 180
    end
    object Cube1: TGLCube
      Direction.Coordinates = {0000803F000000000000000000000000}
      Position.Coordinates = {CDCCCC3DCDCCCC3D666666BF0000803F}
      Scale.Coordinates = {CDCC4C3ECDCC4C3ECDCC4C3E00000000}
      Material.BackProperties.Diffuse.Color = {8382023F8584043FB1B0303F8195633F}
      Material.FrontProperties.Diffuse.Color = {8382023F8584043FB1B0303F6DE75B3F}
      Material.BlendingMode = bmTransparency
      Material.Texture.ImageAlpha = tiaAlphaFromIntensity
      Material.Texture.TextureMode = tmModulate
    end
    object Cube2: TGLCube
      Position.Coordinates = {CDCCCCBECDCCCC3E000000BF0000803F}
      CubeSize = {0000803E0000803E0000803E}
    end
    object DummyCube1: TGLDummyCube
      Position.Coordinates = {0000803F0000803F0000003F0000803F}
      TransformationMode = tmParentWithPos
      CubeSize = 0.200000002980232
      EdgeColor.Color = {DEDD5D3FDEDD5D3FE9E8683F0000803F}
    end
    object XArrow: TGLArrowLine
      Direction.Coordinates = {0000803F000000000000000000000000}
      Position.Coordinates = {CDCCCC3D000080BF000080BF0000803F}
      Up.Coordinates = {000000002EBD3BB30000803F00000000}
      BottomRadius = 0.00999999977648258
      Height = 2.20000004768372
      TopRadius = 0.00999999977648258
      TopArrowHeadHeight = 0.200000002980232
      TopArrowHeadRadius = 0.0500000007450581
      BottomArrowHeadHeight = 0.5
      BottomArrowHeadRadius = 0.0500000007450581
    end
    object YArrow: TGLArrowLine
      Direction.Coordinates = {24DE4C320000803F2CBD3B3300000000}
      Position.Coordinates = {000080BFCDCCCC3D000080BF0000803F}
      Up.Coordinates = {2EBD3BB32CBD3BB30000803F00000000}
      BottomRadius = 0.00999999977648258
      Height = 2.20000004768372
      TopRadius = 0.00999999977648258
      TopArrowHeadHeight = 0.200000002980232
      TopArrowHeadRadius = 0.0500000007450581
      BottomArrowHeadHeight = 0.5
      BottomArrowHeadRadius = 0.200000002980232
    end
    object ZArrow: TGLArrowLine
      Position.Coordinates = {000080BF000080BF000000BF0000803F}
      BottomRadius = 0.00999999977648258
      Height = 1
      TopRadius = 0.00999999977648258
      TopArrowHeadHeight = 0.200000002980232
      TopArrowHeadRadius = 0.0500000007450581
      BottomArrowHeadHeight = 0.5
      BottomArrowHeadRadius = 0.0500000007450581
    end
    object TxtX: TGLSpaceText
      Direction.Coordinates = {F30435BFF30435BF0000000000000000}
      Position.Coordinates = {CDCCCC3F000080BF000080BF0000803F}
      Scale.Coordinates = {9A99993E9A99993E9A99993E00000000}
      Up.Coordinates = {00000000000000000000803F00000000}
      Extrusion = 0.300000011920929
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      Text = 'X'
      CharacterRange = stcrAlphaNum
    end
    object TxtY: TGLSpaceText
      Direction.Coordinates = {F40435BFF20435BF0000000000000000}
      Position.Coordinates = {000080BF9A99D93F000080BF0000803F}
      Scale.Coordinates = {9A99993E9A99993E9A99993E00000000}
      Up.Coordinates = {00000000000000000000803F00000000}
      Extrusion = 0.300000011920929
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      Text = 'Y'
      CharacterRange = stcrAlphaNum
    end
    object TxtZ: TGLSpaceText
      Direction.Coordinates = {F304353FF304353F0000000000000000}
      Position.Coordinates = {9A99193ECDCCCCBD0000803F0000803F}
      Scale.Coordinates = {9A99993E9A99993E9A99993E00000000}
      Up.Coordinates = {00000080000000000000803F00000000}
      Extrusion = 0.200000002980232
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      Text = 'Z'
      CharacterRange = stcrAlphaNum
    end
    object GLCamera1: TGLCamera
      DepthOfView = 1000
      FocalLength = 400
      TargetObject = DummyCube1
      Position.Coordinates = {0000B8410000A041000080410000803F}
      Direction.Coordinates = {2EF964BF2EF9E43E0000000000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
    end
  end
end
