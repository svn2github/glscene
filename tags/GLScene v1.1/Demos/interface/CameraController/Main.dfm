object Form1: TForm1
  Left = 138
  Top = 176
  Caption = 'CameraController Demo'
  ClientHeight = 588
  ClientWidth = 863
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 533
    Top = 0
    Width = 330
    Height = 553
    Align = alRight
    TabOrder = 0
    object Panel2: TPanel
      Left = 6
      Top = 9
      Width = 155
      Height = 201
      TabOrder = 0
      object Label1: TLabel
        Left = 6
        Top = 57
        Width = 63
        Height = 13
        Caption = 'Destination X'
      end
      object Label2: TLabel
        Left = 6
        Top = 105
        Width = 63
        Height = 13
        Caption = 'Destination Y'
      end
      object Label3: TLabel
        Left = 6
        Top = 153
        Width = 63
        Height = 13
        Caption = 'Destination Z'
      end
      object Label5: TLabel
        Left = 4
        Top = 2
        Width = 141
        Height = 47
        AutoSize = False
        Caption = 'Data for MoveToPos, OrbitToPos and SafeOrbitAndZoomToPos'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
      end
      object eDestX: TEdit
        Left = 6
        Top = 72
        Width = 121
        Height = 21
        TabOrder = 0
        Text = '0'
      end
      object eDestY: TEdit
        Left = 6
        Top = 122
        Width = 121
        Height = 21
        TabOrder = 1
        Text = '5.79'
      end
      object eDestZ: TEdit
        Left = 6
        Top = 168
        Width = 121
        Height = 21
        TabOrder = 2
        Text = '-3.34'
      end
    end
    object Panel3: TPanel
      Left = 166
      Top = 241
      Width = 155
      Height = 72
      TabOrder = 1
      object Label6: TLabel
        Left = 6
        Top = 25
        Width = 41
        Height = 13
        Caption = 'Distance'
      end
      object Label9: TLabel
        Left = 4
        Top = 2
        Width = 129
        Height = 15
        AutoSize = False
        Caption = 'Data for ZoomToDistance'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
      end
      object eDistance: TEdit
        Left = 6
        Top = 40
        Width = 121
        Height = 21
        TabOrder = 0
        Text = '4'
      end
    end
    object Panel4: TPanel
      Left = 166
      Top = 9
      Width = 155
      Height = 224
      TabOrder = 2
      object Label7: TLabel
        Left = 6
        Top = 41
        Width = 74
        Height = 13
        Caption = 'soSafeDistance'
      end
      object Label8: TLabel
        Left = 6
        Top = 137
        Width = 69
        Height = 13
        Caption = 'soTimeToOrbit'
      end
      object Label10: TLabel
        Left = 6
        Top = 89
        Width = 116
        Height = 13
        Caption = 'soTimeToSafePlacement'
      end
      object Label11: TLabel
        Left = 4
        Top = 2
        Width = 141
        Height = 31
        AutoSize = False
        Caption = 'Data for SafeOrbitAndZoomToPos'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
      end
      object Label12: TLabel
        Left = 6
        Top = 177
        Width = 103
        Height = 13
        Caption = 'soTimeToZoomBackIn'
      end
      object eSafeDistance: TEdit
        Left = 6
        Top = 56
        Width = 121
        Height = 21
        TabOrder = 0
        Text = '9'
      end
      object eTimeToSafePlacement: TEdit
        Left = 6
        Top = 106
        Width = 121
        Height = 21
        TabOrder = 1
        Text = '1'
      end
      object eTimeToOrbit: TEdit
        Left = 6
        Top = 152
        Width = 121
        Height = 21
        TabOrder = 2
        Text = '2'
      end
      object eTimeToZoomBackIn: TEdit
        Left = 6
        Top = 192
        Width = 121
        Height = 21
        TabOrder = 3
        Text = '1'
      end
    end
    object Panel5: TPanel
      Left = 6
      Top = 217
      Width = 155
      Height = 96
      TabOrder = 3
      object Label13: TLabel
        Left = 6
        Top = 49
        Width = 22
        Height = 13
        Caption = 'Time'
      end
      object Label14: TLabel
        Left = 4
        Top = 2
        Width = 129
        Height = 47
        AutoSize = False
        Caption = 'Data for MoveToPos, OrbitToPos and ZoomToDistance'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
      end
      object eTime: TEdit
        Left = 6
        Top = 64
        Width = 121
        Height = 21
        TabOrder = 0
        Text = '2'
      end
    end
    object btnMoveToPos: TButton
      Left = 25
      Top = 336
      Width = 280
      Height = 33
      Caption = 'MoveToPos (linear interpolation)'
      TabOrder = 4
      OnClick = btnMoveToPosClick
    end
    object btnZoomToDistance: TButton
      Left = 25
      Top = 375
      Width = 280
      Height = 34
      Caption = 'ZoomToDistance'
      TabOrder = 5
      OnClick = btnZoomToDistanceClick
    end
    object btnOrbitToPos: TButton
      Left = 25
      Top = 415
      Width = 280
      Height = 34
      Caption = 'OrbitToPos (will not zoom in to pos)'
      TabOrder = 6
      OnClick = btnOrbitToPosClick
    end
    object btnSafeOrbitAndZoomToPos: TButton
      Left = 25
      Top = 455
      Width = 280
      Height = 34
      Caption = 'SafeOrbitAndZoomToPos'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 7
      OnClick = btnSafeOrbitAndZoomToPosClick
    end
    object btnStopMovement: TButton
      Left = 25
      Top = 504
      Width = 280
      Height = 33
      Caption = 'StopMovement'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsItalic]
      ParentFont = False
      TabOrder = 8
      OnClick = btnStopMovementClick
    end
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 533
    Height = 553
    Camera = GLCamera
    FieldOfView = 131.517700195312500000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    OnMouseUp = GLSceneViewer1MouseUp
  end
  object pImg: TPanel
    Left = 4
    Top = 4
    Width = 157
    Height = 24
    BevelOuter = bvNone
    TabOrder = 2
    object Image1: TImage
      Left = 0
      Top = 0
      Width = 24
      Height = 24
      Picture.Data = {
        07544269746D6170F6060000424DF60600000000000036000000280000001800
        0000180000000100180000000000C0060000C30E0000C30E0000000000000000
        000082C9F77FC8F786CBF78CCEF792D0F898D3F89FD6F8A5D8F9AADBF9B1DDFA
        B7E0FABCE3FBC2E5FBC7E7FBCDE9FCD2ECFCD7EEFCDCF0FDE1F3FDE6F5FDE9F7
        FEEDF8FEF2F9FEF5FBFE70C3F5C0C0C0C0C0C0C0C0C0C0C0C08BCDF792D0F898
        D3F89ED6F9A6D9F9ACDBFAB2DEFAB8E1FBBFE3FBC4E6FBCAE9FCCFEBFCD5EDFC
        808080808080808080E8F6FEEDF7FEF2F9FE6DC1F60000000000000000000000
        00C0C0C08CCEF891D0F899D3F89FD6F9A6D9F9ACDCFAB3DEFAB8E1FABFE3FBC4
        E6FBCAE9FC808080D5EDFCDAF0FDDFF2FD808080E8F6FEEEF8FE67BEF563BDF5
        000000000000000000000000C0C0C0C0C0C092D0F798D3F89FD6F9A6D9F9ACDC
        FAB2DEFAB9E1FABFE4FB808080CAE8FCCFEBFCD5EDFCDAF0FDDFF2FD808080EA
        F7FE60BBF55EBAF464BDF56BC0F571C2F60000000000000000008CCEF792D1F8
        98D3F89FD6F9A5D9F9ABDCFAB3DEFAB9E1FB808080C4E6FB8080808080808080
        80DAEFFD808080E6F5FD5BB9F557B7F45EBAF464BDF56AC0F571C3F678C6F600
        000085CBF78CCEF792D1F799D4F89FD6F9A6D9F9ACDCFAB3DEFA808080BFE3FB
        C4E6FBCAE9FCD0EAFCD5EDFC808080E2F3FD56B7F451B5F458B8F45EBAF564BD
        F56BC0F572C3F678C5F67EC8F785CBF78CCEF793D1F899D4F89FD6F9A6D9F9AC
        DCFAB2DEFA808080BFE4FBC4E6FBCAE9FC808080D6EEFCDDF1FD50B5F44AB2F3
        51B5F457B7F400000064BDF56BC0F571C3F60000000000000000000000000000
        00000000000000000000000000000000808080808080808080CAE9FCCFEBFCD8
        EFFC49B2F344B0F34BB2F352B5F400000000000064BDF5000000000000000000
        000000000000000000000000000000000000000000000000000000B9E1FBBFE4
        FBC5E6FBCAE9FCD3ECFC45AFF33FADF346AFF34CB2F300000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        000000B3DFFAB9E1FBBFE4FBC5E6FBCEE9FC3FADF339AAF240ADF346B0F30000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000ACDBFAB3DEFAB9E1FBBFE4FBC8E7FB3AABF234A8F2
        39ABF23FADF20000000000000000000000000000000000000000000000000000
        00000000000000000000000000000000000000A6D9F9ADDCFAB3DFFABAE1FBC4
        E6FB36A8F22EA6F134A8F23AAAF20000000000004CB2F3000000000000000000
        000000000000000000000000000000000000000000000000000000A0D6F9A7D9
        F9ADDCFAB4DFFABEE3FB30A7F229A3F12FA6F134A9F200000040ADF346B0F34C
        B2F3000000000000000000000000000000000000000000000000000000000000
        93D1F899D4F8A0D7F9A6D9F9ADDCFAB8E1FA2CA6F124A2F029A4F12FA6F134A8
        F23AABF240AEF245B0F34BB3F300000000000000000066BEF56CC0F500000000
        000000000086CCF78DCEF894D1F89AD4F8A1D7F9A7DAF9B3DEFA28A3F11F9FF0
        24A1F129A3F12FA5F134A9F239ABF240ADF20000000000000000000000000000
        0000000000000000000000000000000086CCF78DCEF894D1F89AD4F9A1D7F9AC
        DBF924A1F11A9DF01FA0F0FFFFFFFFFFFFFFFFFF34A8F23AABF2000000000000
        00000000000000000000000000000000000000000000000080C9F786CBF78DCE
        F893D1F89AD4F8A7D9F920A0F0169BF0FFFFFF1FA0F024A2F12AA3F1FFFFFF34
        A9F23AABF200000000000000000053B5F459B8F400000000000000000072C3F6
        7AC6F6C0C0C0C0C0C08DCFF893D1F8A1D7F81C9DF0FFFFFF169CF01A9EF0FFFF
        FF24A2F129A4F1FFFFFF35A9F23BABF240ADF247B0F34DB3F352B5F459B8F460
        BBF566BEF56CC0F673C3F6000000000000C0C0C08DCFF89AD4F8189CF0FFFFFF
        1299EFFFFFFFFFFFFFFFFFFF25A1F1FFFFFF2FA6F134A8F23AABF241AEF347B0
        F34CB2F352B5F459B8F45FBBF566BEF56DC1F673C3F6000000000000C0C0C094
        D1F8159BF0FFFFFF0E98EF129AEFFFFFFF1B9EF01FA0F0FFFFFF2AA4F12FA7F1
        35A9F23BABF240AEF247B0F34DB3F353B6F459B8F460BBF466BEF56CC0F573C3
        F60000000000008ECFF71299EF0695EFFFFFFF0E99EF1299EF169CF0FFFFFF1F
        A0F025A1F12AA4F12FA6F134A8F23BABF241AEF347B0F34DB3F353B6F459B8F4
        5FBBF566BDF56DC1F600000000000088CCF70C97EF0394EE0795EFFFFFFFFFFF
        FFFFFFFF169CF01B9DF0209FF025A1F12AA4F12FA7F135A9F23AABF240AEF347
        B0F34CB3F352B6F459B8F460BBF566BEF56DC1F500000080C8F61E9FF01098EF
        129AEF159BF0189DF01C9EF020A1F024A1F128A3F12DA6F132A7F237A9F23BAB
        F241AEF346B0F34CB3F452B5F457B7F45DBAF563BCF569BFF66FC2F676C5F683
        CBF7}
    end
    object Label4: TLabel
      Left = 32
      Top = 6
      Width = 102
      Height = 13
      Caption = 'TGLCameraController'
    end
  end
  object Panel6: TPanel
    Left = 0
    Top = 553
    Width = 863
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
    object Label15: TLabel
      Left = 4
      Top = 6
      Width = 853
      Height = 31
      Alignment = taCenter
      AutoSize = False
      Caption = 
        'The main method, SafeOrbitAndZoomToPos, resambles the Google Ear' +
        'th "Fly to..." motion. I developed and used it to fly to differe' +
        'nt views of a car (hood, trunk etc) in 3DCar(TM), which probably' +
        ' was/is the first interactive 3D car configurator, released by B' +
        'luemind Software.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
  end
  object GLScene1: TGLScene
    VisibilityCulling = vcHierarchical
    Left = 16
    Top = 48
    object GLDummyCube1: TGLDummyCube
      ShowAxes = True
      Up.Coordinates = {000000000000803F0000008000000000}
      CubeSize = 4.000000000000000000
      object GLSphere1: TGLSphere
        Material.MaterialLibrary = GLMaterialLibrary1
        Material.LibMaterialName = 'transparent blue'
        Radius = 2.000000000000000000
        Slices = 32
        Stacks = 32
      end
      object GLCylinder1: TGLCylinder
        Material.MaterialLibrary = GLMaterialLibrary1
        Material.LibMaterialName = 'opaque green'
        ObjectsSorting = osNone
        Direction.Coordinates = {00000000FFFFFF3ED7B35D3F00000000}
        PitchAngle = 30.000000000000000000
        Up.Coordinates = {00000000D7B35D3FFFFFFFBE00000000}
        BottomRadius = 0.200000002980232200
        Height = 4.199999809265137000
        TopRadius = 0.050000000745058060
      end
      object GLSphere2: TGLSphere
        Material.MaterialLibrary = GLMaterialLibrary1
        Material.LibMaterialName = 'opaque green'
        Position.Coordinates = {6666E63F00000000000000000000803F}
        Radius = 0.300000011920929000
      end
      object GLSphere3: TGLSphere
        Material.MaterialLibrary = GLMaterialLibrary1
        Material.LibMaterialName = 'opaque red'
        Position.Coordinates = {000000006666E63F000000000000803F}
        Radius = 0.300000011920929000
      end
    end
    object GLCamera: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 120.000000000000000000
      TargetObject = GLDummyCube1
      Position.Coordinates = {0000204100002041000020410000803F}
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        SpotCutOff = 180.000000000000000000
      end
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'transparent blue'
        Material.FrontProperties.Diffuse.Color = {FBFAFA3EF7F6F63E0000803F9A99193F}
        Material.BlendingMode = bmTransparency
        Tag = 0
      end
      item
        Name = 'opaque green'
        Material.FrontProperties.Diffuse.Color = {000000000000803FF3F2F23E0000803F}
        Tag = 0
      end
      item
        Name = 'opaque red'
        Material.FrontProperties.Ambient.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Diffuse.Color = {0000803F00000000000000000000803F}
        Tag = 0
      end>
    Left = 48
    Top = 48
  end
  object GLCadencer1: TGLCadencer
    Left = 16
    Top = 80
  end
  object GLCameraController1: TGLCameraController
    Camera = GLCamera
    Cadencer = GLCadencer1
    soSafeDistance = 10.000000000000000000
    soTimeToSafePlacement = 1.000000000000000000
    soTimeToOrbit = 2.000000000000000000
    soTimeToZoomBackIn = 1.000000000000000000
    Left = 48
    Top = 80
  end
end
