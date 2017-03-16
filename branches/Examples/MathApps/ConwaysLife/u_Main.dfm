object Form1: TForm1
  Left = 197
  Top = 426
  Caption = 'Life'
  ClientHeight = 499
  ClientWidth = 817
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object vp: TGLSceneViewer
    Left = 176
    Top = 0
    Width = 608
    Height = 461
    Camera = cam
    FieldOfView = 155.522048950195300000
    Align = alCustom
    Anchors = [akLeft, akTop, akRight, akBottom]
    OnMouseDown = vpMouseDown
    OnMouseMove = vpMouseMove
    TabOrder = 0
  end
  object Panel3: TPanel
    Left = 8
    Top = 8
    Width = 162
    Height = 97
    BevelOuter = bvNone
    BorderWidth = 1
    BorderStyle = bsSingle
    Color = 15790320
    Ctl3D = False
    ParentBackground = False
    ParentCtl3D = False
    TabOrder = 1
    object Panel4: TPanel
      Left = 1
      Top = 1
      Width = 158
      Height = 20
      Align = alTop
      Alignment = taLeftJustify
      BevelOuter = bvNone
      Caption = '  Life menu'
      Color = 14540253
      ParentBackground = False
      TabOrder = 0
      object p_fps: TPanel
        Left = 78
        Top = 0
        Width = 80
        Height = 20
        Align = alRight
        Alignment = taRightJustify
        BevelOuter = bvNone
        BorderWidth = 4
        Color = 14540253
        TabOrder = 0
      end
    end
    object Button1: TButton
      Left = 8
      Top = 33
      Width = 145
      Height = 24
      Caption = 'load Map...'
      TabOrder = 1
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 8
      Top = 63
      Width = 145
      Height = 24
      Caption = 'save Map...'
      TabOrder = 2
      OnClick = Button2Click
    end
  end
  object Panel2: TPanel
    Left = 8
    Top = 111
    Width = 162
    Height = 122
    BevelOuter = bvNone
    BorderWidth = 1
    BorderStyle = bsSingle
    Color = 15790320
    Ctl3D = False
    ParentBackground = False
    ParentCtl3D = False
    TabOrder = 2
    object Label4: TLabel
      Left = 23
      Top = 36
      Width = 39
      Height = 13
      Caption = 'width: '
    end
    object Label5: TLabel
      Left = 18
      Top = 62
      Width = 44
      Height = 13
      Caption = 'height: '
    end
    object Panel5: TPanel
      Left = 1
      Top = 1
      Width = 158
      Height = 20
      Align = alTop
      Alignment = taLeftJustify
      BevelOuter = bvNone
      Caption = '  Life new'
      Color = 14540253
      ParentBackground = False
      TabOrder = 0
    end
    object Edit2: TEdit
      Left = 70
      Top = 33
      Width = 49
      Height = 19
      ReadOnly = True
      TabOrder = 1
      Text = '32'
    end
    object UpDown2: TUpDown
      Left = 119
      Top = 33
      Width = 17
      Height = 19
      Max = 3
      TabOrder = 2
      OnClick = UpDown2Click
    end
    object Edit3: TEdit
      Left = 70
      Top = 58
      Width = 49
      Height = 19
      ReadOnly = True
      TabOrder = 3
      Text = '32'
    end
    object UpDown3: TUpDown
      Left = 119
      Top = 58
      Width = 17
      Height = 19
      Max = 3
      TabOrder = 4
      OnClick = UpDown3Click
    end
    object Button3: TButton
      Left = 8
      Top = 88
      Width = 145
      Height = 24
      Caption = 'create new Map'
      TabOrder = 5
      OnClick = Button3Click
    end
  end
  object Panel7: TPanel
    Left = 8
    Top = 239
    Width = 162
    Height = 138
    BevelOuter = bvNone
    BorderWidth = 1
    BorderStyle = bsSingle
    Color = 15790320
    Ctl3D = False
    ParentBackground = False
    ParentCtl3D = False
    TabOrder = 3
    object Label1: TLabel
      Left = 19
      Top = 35
      Width = 43
      Height = 13
      Caption = 'speed: '
    end
    object but_run: TSpeedButton
      Left = 8
      Top = 98
      Width = 145
      Height = 29
      AllowAllUp = True
      GroupIndex = 1
      Caption = 'RUN'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Verdana'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Panel8: TPanel
      Left = 1
      Top = 1
      Width = 158
      Height = 20
      Align = alTop
      Alignment = taLeftJustify
      BevelOuter = bvNone
      Caption = '  Life options'
      Color = 14540253
      ParentBackground = False
      TabOrder = 0
    end
    object Edit4: TEdit
      Left = 70
      Top = 32
      Width = 49
      Height = 19
      ReadOnly = True
      TabOrder = 1
      Text = '10'
    end
    object UpDown4: TUpDown
      Left = 119
      Top = 32
      Width = 16
      Height = 19
      Associate = Edit4
      Min = 1
      Max = 20
      Position = 10
      TabOrder = 2
      OnClick = UpDown4Click
    end
    object cb: TCheckBox
      Left = 18
      Top = 64
      Width = 117
      Height = 17
      Caption = 'enable borders'
      TabOrder = 3
    end
  end
  object GLScene1: TGLScene
    Left = 184
    Top = 8
    object hudMap: TGLHUDSprite
      Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
      Material.MaterialOptions = [moNoLighting]
      Material.Texture.ImageClassName = 'TGLBlankImage'
      Material.Texture.Image.ColorFormat = 6408
      Material.Texture.MagFilter = maNearest
      Material.Texture.MinFilter = miLinear
      Material.Texture.TextureWrap = twNone
      Material.Texture.FilteringQuality = tfAnisotropic
      Material.Texture.Disabled = False
      Position.Coordinates = {00009C4300007A43000000000000803F}
      Width = 1.000000000000000000
      Height = 1.000000000000000000
      Rotation = 0.000000000000000000
    end
    object cam: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      CameraStyle = csOrthogonal
      Position.Coordinates = {0000000000000000000020410000803F}
    end
  end
  object at: TGLAsyncTimer
    Enabled = True
    Interval = 100
    OnTimer = atTimer
    Left = 216
    Top = 8
  end
  object opendlg: TOpenPictureDialog
    DefaultExt = '*.bmp'
    Filter = 'Array|*.bmp'
    InitialDir = 'maps'
    Left = 128
    Top = 32
  end
  object savedlg: TSavePictureDialog
    DefaultExt = '*.bmp'
    Filter = 'Array|*.bmp'
    InitialDir = 'maps'
    Left = 128
    Top = 64
  end
end
