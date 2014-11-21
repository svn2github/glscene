object frmMineSweeper3D: TfrmMineSweeper3D
  Left = 347
  Top = 178
  Caption = 'Cambrian Labs Mine Sweeper 3D 0.5 Beta'
  ClientHeight = 364
  ClientWidth = 453
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  OnMouseWheel = FormMouseWheel
  OnResize = FormResize
  DesignSize = (
    453
    364)
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 8
    Top = 0
    Width = 16
    Height = 16
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Visible = False
  end
  object Label_GameState: TLabel
    Left = 48
    Top = 4
    Width = 352
    Height = 17
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = '(no state)'
    Color = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    OnClick = Label_GameStateClick
  end
  object Label_MinesLeft: TLabel
    Left = 8
    Top = 0
    Width = 34
    Height = 24
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = '000'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label_GameTime: TLabel
    Left = 408
    Top = 0
    Width = 34
    Height = 24
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    Caption = '000'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label_FrameRate: TLabel
    Left = 371
    Top = 368
    Width = 72
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Alignment = taRightJustify
    Anchors = [akRight, akBottom]
    AutoSize = False
    Caption = '0 fps'
  end
  object Label_IntersectTime: TLabel
    Left = 8
    Top = 367
    Width = 51
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Timing info'
  end
  object GLSceneViewer_Main: TGLSceneViewer
    Left = 8
    Top = 25
    Width = 436
    Height = 340
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Camera = GLCamera_MainCamera
    Buffer.FogEnvironment.FogColor.Color = {CDCC4C3DCDCC4C3DCDCC4C3D0000803F}
    Buffer.FogEnvironment.FogStart = 5.000000000000000000
    Buffer.FogEnvironment.FogEnd = 20.000000000000000000
    Buffer.BackgroundColor = clBlack
    Buffer.ContextOptions = [roDoubleBuffer, roTwoSideLighting]
    Buffer.AntiAliasing = aaNone
    Buffer.ShadeModel = smSmooth
    FieldOfView = 147.220916748046900000
    Anchors = [akLeft, akTop, akRight, akBottom]
    OnMouseMove = GLSceneViewer_MainMouseMove
    TabOrder = 0
  end
  object GLScene1: TGLScene
    ObjectsSorting = osRenderFarthestFirst
    VisibilityCulling = vcObjectBased
    Left = 216
    Top = 96
    object GLCamera_MainCamera: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 50.000000000000000000
      TargetObject = Plane_PlayingField
      Position.Coordinates = {0000404000004040000040400000803F}
    end
    object DummyCube_PlayingBox: TGLDummyCube
      CubeSize = 1.000000000000000000
      object Lines1: TGLLines
        Nodes = <>
        Options = []
      end
      object Plane_PlayingField: TGLPlane
        Height = 1.000000000000000000
        Width = 1.000000000000000000
      end
    end
    object GLLightSource1: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Position.Coordinates = {0000A0400000A0400000A0400000803F}
      SpotCutOff = 180.000000000000000000
    end
  end
  object GLCadencer_Main: TGLCadencer
    Scene = GLScene1
    OnProgress = InvalidateProgress
    Left = 72
    Top = 72
  end
  object GLMaterialLibrary_Digits: TGLMaterialLibrary
    Materials = <
      item
        Name = 'Digit_0'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.Texture.Image.Picture.Data = {
          0A544A504547496D61676503030000FFD8FFE000104A46494600010101012C01
          2C0000FFDB004300080606070605080707070909080A0C140D0C0B0B0C191213
          0F141D1A1F1E1D1A1C1C20242E2720222C231C1C2837292C30313434341F2739
          3D38323C2E333432FFDB0043010909090C0B0C180D0D1832211C213232323232
          3232323232323232323232323232323232323232323232323232323232323232
          32323232323232323232323232FFC00011080010001003012200021101031101
          FFC4001F0000010501010101010100000000000000000102030405060708090A
          0BFFC400B5100002010303020403050504040000017D01020300041105122131
          410613516107227114328191A1082342B1C11552D1F02433627282090A161718
          191A25262728292A3435363738393A434445464748494A535455565758595A63
          6465666768696A737475767778797A838485868788898A92939495969798999A
          A2A3A4A5A6A7A8A9AAB2B3B4B5B6B7B8B9BAC2C3C4C5C6C7C8C9CAD2D3D4D5D6
          D7D8D9DAE1E2E3E4E5E6E7E8E9EAF1F2F3F4F5F6F7F8F9FAFFC4001F01000301
          01010101010101010000000000000102030405060708090A0BFFC400B5110002
          0102040403040705040400010277000102031104052131061241510761711322
          328108144291A1B1C109233352F0156272D10A162434E125F11718191A262728
          292A35363738393A434445464748494A535455565758595A636465666768696A
          737475767778797A82838485868788898A92939495969798999AA2A3A4A5A6A7
          A8A9AAB2B3B4B5B6B7B8B9BAC2C3C4C5C6C7C8C9CAD2D3D4D5D6D7D8D9DAE2E3
          E4E5E6E7E8E9EAF2F3F4F5F6F7F8F9FAFFDA000C03010002110311003F00F4FF
          0089F6BAA5C7807529749D6A7D266B389EEE49605F9E548D19BCB0C08299217E
          607B6304122B53C173CD75E05F0F5C5C4B24D3CBA65B3C9248C599D8C4A4924F
          2493CE6A9FC415D66E3C1B7F61A1E8FF00DA9757F14968C9F69483CA49237064
          CBF0D838F978CE7AF147C3E5D66DFC1B6161AE68FF00D97756114768A9F6949F
          CD48E34024CA70B939F979C63AF3401FFFD9}
        Material.Texture.TextureMode = tmModulate
        Material.Texture.Disabled = False
      end
      item
        Name = 'Digit_1'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.Texture.TextureMode = tmModulate
        Material.Texture.Disabled = False
      end
      item
        Name = 'Digit_2'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.Texture.TextureMode = tmModulate
        Material.Texture.Disabled = False
      end
      item
        Name = 'Digit_3'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.Texture.TextureMode = tmModulate
        Material.Texture.Disabled = False
      end
      item
        Name = 'Digit_4'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.Texture.TextureMode = tmModulate
        Material.Texture.Disabled = False
      end
      item
        Name = 'Digit_5'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.Texture.TextureMode = tmModulate
        Material.Texture.Disabled = False
      end
      item
        Name = 'Digit_6'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.Texture.TextureMode = tmModulate
        Material.Texture.Disabled = False
      end
      item
        Name = 'Digit_7'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.Texture.TextureMode = tmModulate
        Material.Texture.Disabled = False
      end
      item
        Name = 'Digit_8'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.Texture.TextureMode = tmModulate
        Material.Texture.Disabled = False
      end
      item
        Name = 'Text_M'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.Texture.TextureMode = tmModulate
        Material.Texture.Disabled = False
      end
      item
        Name = 'NotAMine'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.Texture.Image.Picture.Data = {
          0A544A504547496D616765B4040000FFD8FFE000104A46494600010101012C01
          2C0000FFDB004300080606070605080707070909080A0C140D0C0B0B0C191213
          0F141D1A1F1E1D1A1C1C20242E2720222C231C1C2837292C30313434341F2739
          3D38323C2E333432FFDB0043010909090C0B0C180D0D1832211C213232323232
          3232323232323232323232323232323232323232323232323232323232323232
          32323232323232323232323232FFC00011080020002003012200021101031101
          FFC4001F0000010501010101010100000000000000000102030405060708090A
          0BFFC400B5100002010303020403050504040000017D01020300041105122131
          410613516107227114328191A1082342B1C11552D1F02433627282090A161718
          191A25262728292A3435363738393A434445464748494A535455565758595A63
          6465666768696A737475767778797A838485868788898A92939495969798999A
          A2A3A4A5A6A7A8A9AAB2B3B4B5B6B7B8B9BAC2C3C4C5C6C7C8C9CAD2D3D4D5D6
          D7D8D9DAE1E2E3E4E5E6E7E8E9EAF1F2F3F4F5F6F7F8F9FAFFC4001F01000301
          01010101010101010000000000000102030405060708090A0BFFC400B5110002
          0102040403040705040400010277000102031104052131061241510761711322
          328108144291A1B1C109233352F0156272D10A162434E125F11718191A262728
          292A35363738393A434445464748494A535455565758595A636465666768696A
          737475767778797A82838485868788898A92939495969798999AA2A3A4A5A6A7
          A8A9AAB2B3B4B5B6B7B8B9BAC2C3C4C5C6C7C8C9CAD2D3D4D5D6D7D8D9DAE2E3
          E4E5E6E7E8E9EAF2F3F4F5F6F7F8F9FAFFDA000C03010002110311003F00D2B3
          BCD63E147888D95EAB5D68F72DBBE51859074DE99FBAE38CAE79E01E36B57B2D
          85FDAEA7630DED94EB3DB4CBB9245E847F43D883C83C556D6F44B1F10E97269F
          A845BE17E430E1A36ECCA7B11FE20E4122BC8ECEF358F851E223657AAD75A3DC
          B6EF9461641D37A67EEB8E32B9E78078DAD5C49BC33B3D60FF000FF807D34A34
          F3AA7CD1B2C425AAD94D2EABFBDFD6DB7B2DFDFDAE996335EDECEB05B42BB9E4
          6E807F53D801C93C578D5EDE6B1F15FC442CAC95AD747B66DDF38CAC63A6F7C7
          DE73CE173C72071B9A8BDBCD63E2BF88859592B5AE8F6CDBBE71958C74DEF8FB
          CE79C2E78E40E37357AE689A258F87B4B8F4FD3E2D90A7258F2D2377663DC9FF
          000030001436F12ECB482FC7FE004634F25A7CD2B3C435A2DD413EAFFBDFD6DB
          D9BFBFB5D32C66BDBD9D60B685773C8DD00FEA7B0039278AF1ABDBCD63E2BF88
          859592B5AE8F6CDBBE71958C74DEF8FBCE79C2E78E40E373517B79AC7C57F110
          B2B256B5D1ED9B77CE32B18E9BDF1F79CF385CF1C81C6E6AF5CD1344B1F0F697
          1E9FA7C5B214E4B1E5A46EECC7B93FE006000286DE25D96905F8FF00C008C69E
          4B4F9A567886B45BA827D5FF007BFADB7F23B3BCD63E147888D95EAB5D68F72D
          BBE51859074DE99FBAE38CAE79E01E36B57B2D85FDAEA7630DED94EB3DB4CBB9
          245E847F43D883C83C556D6F44B1F10E97269FA845BE17E430E1A36ECCA7B11F
          E20E4122BC8ECEF358F851E223657AAD75A3DCB6EF9461641D37A67EEB8E32B9
          E78078DAD426F0CECF583FC3FE004A34F3AA7CD1B2C425AAD94D2EABFBDFD6DB
          7FFFD9}
        Material.Texture.TextureMode = tmModulate
        Material.Texture.Disabled = False
      end>
    Left = 88
    Top = 136
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 72
    Top = 24
  end
  object Timer_GameTimer: TTimer
    Interval = 150
    OnTimer = Timer_GameTimerTimer
    Left = 72
    Top = 192
  end
  object ActionList1: TActionList
    Left = 216
    Top = 24
    object Action_Restart: TAction
      Caption = '&Restart'
      ShortCut = 113
      OnExecute = Action_RestartExecute
    end
    object Action_Close: TAction
      Caption = '&Exit'
      OnExecute = Action_CloseExecute
    end
    object Action_Beginner: TAction
      Category = 'GameTypes'
      Caption = '&Beginner'
      OnExecute = Action_BeginnerExecute
    end
    object Action_Intermediate: TAction
      Category = 'GameTypes'
      Caption = '&Intermediate'
      OnExecute = Action_IntermediateExecute
    end
    object Action_Advanced: TAction
      Category = 'GameTypes'
      Caption = '&Advanced'
      Enabled = False
      OnExecute = Action_AdvancedExecute
    end
    object Action_AntiAliasing: TAction
      Category = 'Options'
      AutoCheck = True
      Caption = 'Anti aliasing'
      OnExecute = Action_AntiAliasingExecute
    end
    object Action_DebugEasy: TAction
      Category = 'GameTypes'
      Caption = '&Debug easy'
      OnExecute = Action_DebugEasyExecute
    end
    object Action_About: TAction
      Category = 'Help'
      Caption = '&About'
      OnExecute = Action_AboutExecute
    end
    object Action_Help: TAction
      Category = 'Help'
      Caption = '&Help'
      OnExecute = Action_HelpExecute
    end
    object Action_Sound: TAction
      Category = 'Options'
      AutoCheck = True
      Caption = '&Sound'
      Checked = True
      OnExecute = Action_SoundExecute
    end
    object Action_SafeFirstReveal: TAction
      Category = 'Options'
      AutoCheck = True
      Caption = 'First turn is safe'
      OnExecute = Action_SafeFirstRevealExecute
    end
    object Action_DebugHard: TAction
      Category = 'GameTypes'
      Caption = 'Debug hard'
      OnExecute = Action_DebugHardExecute
    end
  end
  object MainMenu1: TMainMenu
    Left = 216
    Top = 192
    object Game1: TMenuItem
      Caption = '&Game'
      object Restart1: TMenuItem
        Action = Action_Restart
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Beginner1: TMenuItem
        Action = Action_Beginner
      end
      object Advanced1: TMenuItem
        Action = Action_Intermediate
      end
      object Advanced2: TMenuItem
        Action = Action_Advanced
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object Debug1: TMenuItem
        Action = Action_DebugEasy
      end
      object Debughard1: TMenuItem
        Action = Action_DebugHard
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Action = Action_Close
      end
    end
    object Opt: TMenuItem
      Caption = '&Options'
      object Antialiasing1: TMenuItem
        Action = Action_AntiAliasing
        AutoCheck = True
      end
      object Alwaysinvalidate1: TMenuItem
        AutoCheck = True
        Caption = 'Always invalidate'
        Checked = True
        OnClick = Action_AlwaysInvalidateExecute
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object Firstturnissafe1: TMenuItem
        Action = Action_SafeFirstReveal
        AutoCheck = True
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object Sound1: TMenuItem
        Action = Action_Sound
        AutoCheck = True
      end
    end
    object Help1: TMenuItem
      Caption = '&Help'
      object Help2: TMenuItem
        Action = Action_Help
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object About1: TMenuItem
        Action = Action_About
      end
    end
  end
  object GLFireFXManager1: TGLFireFXManager
    FireDir.Coordinates = {00000000000000000000803F00000000}
    InitialDir.Coordinates = {00000000000000000000000000000000}
    Cadencer = GLCadencer_Main
    MaxParticles = 96
    ParticleSize = 0.400000005960464400
    FireDensity = 0.500000000000000000
    FireEvaporation = 0.860000014305114800
    ParticleLife = 2
    FireBurst = 1.000000000000000000
    FireRadius = 0.500000000000000000
    Disabled = False
    Paused = False
    ParticleInterval = 0.001000000047497451
    UseInterval = True
    Left = 72
    Top = 248
  end
end
