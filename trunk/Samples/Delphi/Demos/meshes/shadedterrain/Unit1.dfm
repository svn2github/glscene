object Form1: TForm1
  Left = 165
  Top = 146
  Caption = 'Shaded Terrain'
  ClientHeight = 408
  ClientWidth = 573
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 65
    Width = 573
    Height = 343
    Camera = GLCamera1
    BeforeRender = GLSceneViewer1BeforeRender
    Buffer.FogEnvironment.FogColor.Color = {0000803F0000803F0000803F0000803F}
    Buffer.FogEnvironment.FogStart = 200.000000000000000000
    Buffer.FogEnvironment.FogEnd = 650.000000000000000000
    Buffer.FogEnvironment.FogDistance = fdEyeRadial
    Buffer.BackgroundColor = clGray
    Buffer.FogEnable = True
    Buffer.Lighting = False
    FieldOfView = 147.492416381835900000
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 573
    Height = 65
    Align = alTop
    BevelOuter = bvLowered
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 113
      Height = 13
      Caption = 'Bumpmap Sub-sampling'
    end
    object LASubFactor: TLabel
      Left = 320
      Top = 8
      Width = 19
      Height = 13
      Caption = 'Sub'
    end
    object Label2: TLabel
      Left = 8
      Top = 40
      Width = 109
      Height = 13
      Caption = 'Bumpmapping Intensity'
    end
    object LABumpIntensity: TLabel
      Left = 320
      Top = 40
      Width = 79
      Height = 13
      Caption = 'LABumpIntensity'
    end
    object TBSubSampling: TTrackBar
      Left = 136
      Top = 1
      Width = 177
      Height = 29
      Max = 3
      PageSize = 1
      Position = 1
      TabOrder = 0
      TabStop = False
      ThumbLength = 10
      TickMarks = tmBoth
      OnChange = TBSubSamplingChange
    end
    object TBIntensity: TTrackBar
      Left = 136
      Top = 33
      Width = 177
      Height = 29
      Max = 100
      PageSize = 1
      Frequency = 10
      Position = 40
      TabOrder = 1
      TabStop = False
      ThumbLength = 10
      TickMarks = tmBoth
      OnChange = TBIntensityChange
    end
  end
  object GLBitmapHDS1: TGLBitmapHDS
    MaxPoolSize = 0
    Left = 56
    Top = 96
  end
  object GLScene1: TGLScene
    ObjectsSorting = osNone
    Left = 56
    Top = 136
    object SkyDome1: TGLSkyDome
      Direction.Coordinates = {000000000000803F2EBD3BB300000000}
      Up.Coordinates = {000000002EBD3BB3000080BF00000000}
      Bands = <
        item
          StartAngle = -5.000000000000000000
          StartColor.Color = {0000803F0000803F0000803F0000803F}
          StopAngle = 25.000000000000000000
          Slices = 9
        end
        item
          StartAngle = 25.000000000000000000
          StopAngle = 90.000000000000000000
          StopColor.Color = {938C0C3E938C0C3E938E0E3F0000803F}
          Slices = 9
          Stacks = 4
        end>
      Stars = <>
      Options = [sdoTwinkle]
      object SPSun: TGLSprite
        Material.FrontProperties.Ambient.Color = {0000000000000000000000000000803F}
        Material.FrontProperties.Diffuse.Color = {0000000000000000000000000000803F}
        Material.DepthProperties.DepthWrite = False
        Material.BlendingMode = bmAdditive
        Material.MaterialOptions = [moIgnoreFog]
        Material.Texture.TextureMode = tmReplace
        Material.Texture.TextureFormat = tfLuminance
        Material.Texture.Compression = tcNone
        Material.Texture.Disabled = False
        Position.Coordinates = {00000C430000C842000096420000803F}
        Width = 60.000000000000000000
        Height = 60.000000000000000000
        Rotation = 0.000000000000000000
      end
    end
    object DummyCube1: TGLDummyCube
      Position.Coordinates = {0000000000000041000000000000803F}
      CubeSize = 1.000000000000000000
      object GLCamera1: TGLCamera
        DepthOfView = 650.000000000000000000
        FocalLength = 50.000000000000000000
        NearPlaneBias = 0.100000001490116100
        TargetObject = DummyCube1
        Position.Coordinates = {0000A040000020410000C8410000803F}
        Direction.Coordinates = {0000803F000000000000000000000000}
        Left = 264
        Top = 160
      end
    end
    object TerrainRenderer1: TGLTerrainRenderer
      Material.MaterialLibrary = GLMaterialLibrary1
      Material.LibMaterialName = 'ground'
      Direction.Coordinates = {000000000000803F0000000000000000}
      Scale.Coordinates = {00008040000080400000803E00000000}
      Up.Coordinates = {00000000000000000000803F00000000}
      HeightDataSource = GLBumpmapHDS1
      TileSize = 128
      TilesPerTexture = 1.000000000000000000
      MaterialLibrary = GLMaterialLibrary1
      CLODPrecision = 30
    end
    object GLLensFlare: TGLLensFlare
      Size = 100
      Seed = 978
      FlareIsNotOccluded = True
      Position.Coordinates = {9A620252C9B28B51B743BAD10000803F}
      Visible = False
      object GLDummyCube1: TGLDummyCube
        CubeSize = 100.000000000000000000
        VisibleAtRunTime = True
      end
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 56
    Top = 176
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    OnProgress = GLCadencer1Progress
    Left = 16
    Top = 96
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'ground'
        Tag = 0
        Material.FrontProperties.Ambient.Color = {0000000000000000000000000000803F}
        Material.FrontProperties.Diffuse.Color = {0000000000000000000000000000803F}
        Material.FrontProperties.Emission.Color = {9A99993E9A99993E9A99993E0000803F}
        Material.Texture.TextureMode = tmReplace
        Material.Texture.Compression = tcStandard
        Material.Texture.Disabled = False
        Texture2Name = 'details'
      end
      item
        Name = 'details'
        Tag = 0
        Material.Texture.TextureMode = tmModulate
        Material.Texture.TextureFormat = tfLuminance
        Material.Texture.Compression = tcStandard
        Material.Texture.Disabled = False
        TextureScale.Coordinates = {00000042000000420000004200000000}
      end
      item
        Name = 'texture'
        Tag = 0
        Material.FrontProperties.Diffuse.Color = {CDCC4C3FCDCC4C3F0000003F0000803F}
        Material.Texture.Image.Picture.Data = {
          07544269746D617036180000424D361800000000000036000000280000000008
          0000010000000100180000000000001800000000000000000000000000000000
          0000A87300A97200A97100AA7100AA7000AB6F00AB6E00AC6E00AD6D00AD6C00
          AE6B00AE6B00AF6A00B06900B06800B16700B16700B26600B26500B36400B464
          00B46300B56200B56100B66100B76000B75F00B85E00B85E00B95D00B95C00BA
          5B00BB5A00BB5A00BC5900BC5800BD5700BD5700BE5600BF5500BF5400C05400
          C05300C15200C25100C25000C35000C34F00C44E00C44D00C54D00C64C00C64B
          00C74A00C74A00C84900C84800C94700CA4700CA4600CB4500CB4400CC4300CD
          4300CD4200CE4100CE4000CF4000CF3F00D03E00D13D00D13D00D23C00D23B00
          D33A00D43A00D43900D53800D53700D63600D63600D73500D83400D83300D933
          00D93200DA3100DA3000DB3000DC2F00DC2E00DD2D00DD2C00DE2C00DF2B00DF
          2A00E02900E02900E12800E12700E22600E32600E32500E42400E42300E52300
          E52200E62100E72000E71F00E81F00E81E00E91D00EA1C00EA1C00EB1B00EB1A
          00EC1900EC1900ED1800EE1700EE1600EF1500EF1500F01400F01300F11200F2
          1200F21100F31000F30F00F40F00F50E00F50D00F60C00F60C00F70B00F70A00
          F80900F90800F90800FA0700FA0600FB0500FC0500FC0400FD0300FD0200FE02
          00FE0100FF0000FF0201FF0302FF0503FF0704FF0906FF0A07FF0C08FF0E09FF
          0F0AFF110BFF130CFF140DFF160FFF1810FF1A11FF1B12FF1D13FF1F14FF2015
          FF2216FF2418FF2519FF271AFF291BFF2B1CFF2C1DFF2E1EFF301FFF3120FF33
          22FF3523FF3624FF3825FF3A26FF3C27FF3D28FF3F29FF412BFF422CFF442DFF
          462EFF472FFF4930FF4B31FF4D32FF4E34FF5035FF5236FF5337FF5538FF5739
          FF583AFF5A3BFF5C3CFF5E3EFF5F3FFF6140FF6341FF6442FF6643FF6844FF69
          45FF6B47FF6D48FF6F49FF704AFF724BFF744CFF754DFF774EFF7950FF7A51FF
          7C52FF7E53FF8054FF8155FF8356FF8557FF8658FF885AFF8A5BFF8B5CFF8D5D
          FF8F5EFF905FFF9260FF9461FF9663FF9764FF9965FF9B66FF9C67FF9E68FFA0
          69FFA16AFFA36CFFA56DFFA76EFFA86FFFAA70FFAC71FFAD72FFAF73FFB174FF
          B276FFB477FFB678FFB879FFB97AFFBB7BFFBD7CFFBE7DFFC07FFFC280FFC381
          FFC582FFC783FFC984FFCA85FFCC86FFCE88FFCF89FFD18AFFD38BFFD48CFFD6
          8DFFD88EFFDA8FFFDB90FFDD92FFDF93FFE094FFE295FFE496FFE597FFE798FF
          E999FFEB9BFFEC9CFFEE9DFFF09EFFF19FFFF3A0FFF5A1FFF6A2FFF8A4FFFAA5
          FFFCA6FFFDA7FFFFA8FDFFA7FCFFA6FAFFA5F8FFA4F6FFA2F5FFA1F3FFA0F1FF
          9FF0FF9EEEFF9DECFF9CEBFF9BE9FF99E7FF98E5FF97E4FF96E2FF95E0FF94DF
          FF93DDFF92DBFF90DAFF8FD8FF8ED6FF8DD4FF8CD3FF8BD1FF8ACFFF89CEFF88
          CCFF86CAFF85C9FF84C7FF83C5FF82C3FF81C2FF80C0FF7FBEFF7DBDFF7CBBFF
          7BB9FF7AB8FF79B6FF78B4FF77B2FF76B1FF74AFFF73ADFF72ACFF71AAFF70A8
          FF6FA7FF6EA5FF6DA3FF6CA1FF6AA0FF699EFF689CFF679BFF6699FF6597FF64
          96FF6394FF6192FF6090FF5F8FFF5E8DFF5D8BFF5C8AFF5B88FF5A86FF5885FF
          5783FF5681FF5580FF547EFF537CFF527AFF5179FF5077FF4E75FF4D74FF4C72
          FF4B70FF4A6FFF496DFF486BFF4769FF4568FF4466FF4364FF4263FF4161FF40
          5FFF3F5EFF3E5CFF3C5AFF3B58FF3A57FF3955FF3853FF3752FF3650FF354EFF
          344DFF324BFF3149FF3047FF2F46FF2E44FF2D42FF2C41FF2B3FFF293DFF283C
          FF273AFF2638FF2536FF2435FF2333FF2231FF2030FF1F2EFF1E2CFF1D2BFF1C
          29FF1B27FF1A25FF1924FF1822FF1620FF151FFF141DFF131BFF121AFF1118FF
          1016FF0F14FF0D13FF0C11FF0B0FFF0A0EFF090CFF080AFF0709FF0607FF0405
          FF0303FF0202FF0100FF0000FF0100FF0200FF0300FF0400FF0600FF0700FF08
          00FF0900FF0A00FF0B00FF0C00FF0D00FF0F00FF1000FF1100FF1200FF1300FF
          1400FF1500FF1600FF1800FF1900FF1A00FF1B00FF1C00FF1D00FF1E00FF1F00
          FF2000FF2200FF2300FF2400FF2500FF2600FF2700FF2800FF2900FF2B00FF2C
          00FF2D00FF2E00FF2F00FF3000FF3100FF3200FF3400FF3500FF3600FF3700FF
          3800FF3900FF3A00FF3B00FF3C00FF3E00FF3F00FF4000FF4100FF4200FF4300
          FF4400FF4500FF4700FF4800FF4900FF4A00FF4B00FF4C00FF4D00FF4E00FF50
          00FF5100FF5200FF5300FF5400FF5500FF5600FF5700FF5800FF5A00FF5B00FF
          5C00FF5D00FF5E00FF5F00FF6000FF6100FF6300FF6400FF6500FF6600FF6700
          FF6800FF6900FF6A00FF6C00FF6D00FF6E00FF6F00FF7000FF7100FF7200FF73
          00FF7400FF7600FF7700FF7800FF7900FF7A00FF7B00FF7C00FF7D00FF7F00FF
          8000FF8100FF8200FF8300FF8400FF8500FF8600FF8800FF8900FF8A00FF8B00
          FF8C00FF8D00FF8E00FF8F00FF9000FF9200FF9300FF9400FF9500FF9600FF97
          00FF9800FF9900FF9B00FF9C00FF9D00FF9E00FF9F00FFA000FFA100FFA200FF
          A400FFA500FFA600FFA700FFA801FFA902FFA903FFAA04FFAA06FFAB07FFAB08
          FFAC09FFAD0AFFAD0BFFAE0CFFAE0DFFAF0FFFB010FFB011FFB112FFB113FFB2
          14FFB215FFB316FFB418FFB419FFB51AFFB51BFFB61CFFB71DFFB71EFFB81FFF
          B820FFB922FFB923FFBA24FFBB25FFBB26FFBC27FFBC28FFBD29FFBD2BFFBE2C
          FFBF2DFFBF2EFFC02FFFC030FFC131FFC232FFC234FFC335FFC336FFC437FFC4
          38FFC539FFC63AFFC63BFFC73CFFC73EFFC83FFFC840FFC941FFCA42FFCA43FF
          CB44FFCB45FFCC47FFCD48FFCD49FFCE4AFFCE4BFFCF4CFFCF4DFFD04EFFD150
          FFD151FFD252FFD253FFD354FFD455FFD456FFD557FFD558FFD65AFFD65BFFD7
          5CFFD85DFFD85EFFD95FFFD960FFDA61FFDA63FFDB64FFDC65FFDC66FFDD67FF
          DD68FFDE69FFDF6AFFDF6CFFE06DFFE06EFFE16FFFE170FFE271FFE372FFE373
          FFE474FFE476FFE577FFE578FFE679FFE77AFFE77BFFE87CFFE87DFFE97FFFEA
          80FFEA81FFEB82FFEB83FFEC84FFEC85FFED86FFEE88FFEE89FFEF8AFFEF8BFF
          F08CFFF08DFFF18EFFF28FFFF290FFF392FFF393FFF494FFF595FFF596FFF697
          FFF698FFF799FFF79BFFF89CFFF99DFFF99EFFFA9FFFFAA0FFFBA1FFFCA2FFFC
          A4FFFDA5FFFDA6FFFEA7FFFEA8FFFFA7FFFFA6FFFFA5FFFFA4FFFFA2FFFFA1FF
          FFA0FFFF9FFFFF9EFFFF9DFFFF9CFFFF9BFFFF99FFFF98FFFF97FFFF96FFFF95
          FFFF94FFFF93FFFF92FFFF90FFFF8FFFFF8EFFFF8DFFFF8CFFFF8BFFFF8AFFFF
          89FFFF88FFFF86FFFF85FFFF84FFFF83FFFF82FFFF81FFFF80FFFF7FFFFF7DFF
          FF7CFFFF7BFFFF7AFFFF79FFFF78FFFF77FFFF76FFFF74FFFF73FFFF72FFFF71
          FFFF70FFFF6FFFFF6EFFFF6DFFFF6CFFFF6AFFFF69FFFF68FFFF67FFFF66FFFF
          65FFFF64FFFF63FFFF61FFFF60FFFF5FFFFF5EFFFF5DFFFF5CFFFF5BFFFF5AFF
          FF58FFFF57FFFF56FFFF55FFFF54FFFF53FFFF52FFFF51FFFF50FFFF4EFFFF4D
          FFFF4CFFFF4BFFFF4AFFFF49FFFF48FFFF47FFFF45FFFF44FFFF43FFFF42FFFF
          41FFFF40FFFF3FFFFF3EFFFF3CFFFF3BFFFF3AFFFF39FFFF38FFFF37FFFF36FF
          FF35FFFF34FFFF32FFFF31FFFF30FFFF2FFFFF2EFFFF2DFFFF2CFFFF2BFFFF29
          FFFF28FFFF27FFFF26FFFF25FFFF24FFFF23FFFF22FFFF20FFFF1FFFFF1EFFFF
          1DFFFF1CFFFF1BFFFF1AFFFF19FFFF18FFFF16FFFF15FFFF14FFFF13FFFF12FF
          FF11FFFF10FFFF0FFFFF0DFFFF0CFFFF0BFFFF0AFFFF09FFFF08FFFF07FFFF06
          FFFF04FFFF03FFFF02FFFF01FFFF00FFFF00FEFF00FEFF00FDFF00FDFF00FCFF
          00FCFF00FBFF00FAFF00FAFF00F9FF00F9FF00F8FF00F7FF00F7FF00F6FF00F6
          FF00F5FF00F5FF00F4FF00F3FF00F3FF00F2FF00F2FF00F1FF00F0FF00F0FF00
          EFFF00EFFF00EEFF00EEFF00EDFF00ECFF00ECFF00EBFF00EBFF00EAFF00EAFF
          00E9FF00E8FF00E8FF00E7FF00E7FF00E6FF00E5FF00E5FF00E4FF00E4FF00E3
          FF00E3FF00E2FF00E1FF00E1FF00E0FF00E0FF00DFFF00DFFF00DEFF00DDFF00
          DDFF00DCFF00DCFF00DBFF00DAFF00DAFF00D9FF00D9FF00D8FF00D8FF00D7FF
          00D6FF00D6FF00D5FF00D5FF00D4FF00D4FF00D3FF00D2FF00D2FF00D1FF00D1
          FF00D0FF00CFFF00CFFF00CEFF00CEFF00CDFF00CDFF00CCFF00CBFF00CBFF00
          CAFF00CAFF00C9FF00C8FF00C8FF00C7FF00C7FF00C6FF00C6FF00C5FF00C4FF
          00C4FF00C3FF00C3FF00C2FF00C2FF00C1FF00C0FF00C0FF00BFFF00BFFF00BE
          FF00BDFF00BDFF00BCFF00BCFF00BBFF00BBFF00BAFF00B9FF00B9FF00B8FF00
          B8FF00B7FF00B7FF00B6FF00B5FF00B5FF00B4FF00B4FF00B3FF00B2FF00B2FF
          00B1FF00B1FF00B0FF00B0FF00AFFF00AEFF00AEFF00ADFF00ADFF00ACFF00AB
          FF00ABFF00AAFF00AAFF00A9FF00A9FF00A8FF00A7FF00A6FF00A5FF00A4FF00
          A2FF00A1FF00A0FF009FFF009EFF009DFF009CFF009BFF0099FF0098FF0097FF
          0096FF0095FF0094FF0093FF0092FF0090FF008FFF008EFF008DFF008CFF008B
          FF008AFF0089FF0088FF0086FF0085FF0084FF0083FF0082FF0081FF0080FF00
          7FFF007DFF007CFF007BFF007AFF0079FF0078FF0077FF0076FF0074FF0073FF
          0072FF0071FF0070FF006FFF006EFF006DFF006CFF006AFF0069FF0068FF0067
          FF0066FF0065FF0064FF0063FF0061FF0060FF005FFF005EFF005DFF005CFF00
          5BFF005AFF0058FF0057FF0056FF0055FF0054FF0053FF0052FF0051FF0050FF
          004EFF004DFF004CFF004BFF004AFF0049FF0048FF0047FF0045FF0044FF0043
          FF0042FF0041FF0040FF003FFF003EFF003CFF003BFF003AFF0039FF0038FF00
          37FF0036FF0035FF0034FF0032FF0031FF0030FF002FFF002EFF002DFF002CFF
          002BFF0029FF0028FF0027FF0026FF0025FF0024FF0023FF0022FF0020FF001F
          FF001EFF001DFF001CFF001BFF001AFF0019FF0018FF0016FF0015FF0014FF00
          13FF0012FF0011FF0010FF000FFF000DFF000CFF000BFF000AFF0009FF0008FF
          0007FF0006FF0004FF0003FF0002FF0001FF0000FF0202FF0303FF0505FF0707
          FF0808FF0A0AFF0C0CFF0D0DFF0F0FFF1010FE1212FE1414FE1515FE1717FE19
          19FE1A1AFE1C1CFE1E1EFE1F1FFE2121FE2323FE2424FE2626FE2828FE2929FE
          2B2BFE2C2CFE2E2EFE3030FD3131FD3333FD3535FD3636FD3838FD3A3AFD3B3B
          FD3D3DFD3F3FFD4040FD4242FD4444FD4545FD4747FD4848FD4A4AFD4C4CFD4D
          4DFC4F4FFC5151FC5252FC5454FC5656FC5757FC5959FC5B5BFC5C5CFC5E5EFC
          6060FC6161FC6363FC6464FC6666FC6868FC6969FC6B6BFC6D6DFB6E6EFB7070
          FB7272FB7373FB7575FB7777FB7878FB7A7AFB7C7CFB7D7DFB7F7FFB8080FB82
          82FB8484FB8585FB8787FB8989FB8A8AFB8C8CFA8E8EFA8F8FFA9191FA9393FA
          9494FA9696FA9797FA9999FA9B9BFA9C9CFA9E9EFAA0A0FAA1A1FAA3A3FAA5A5
          FAA6A6FAA8A8FAAAAAFAABABF9ADADF9AFAFF9B0B0F9B2B2F9B3B3F9B5B5F9B7
          B7F9B8B8F9BABAF9BCBCF9BDBDF9BFBFF9C1C1F9C2C2F9C4C4F9C6C6F9C7C7F9
          C9C9F8CBCBF8CCCCF8CECEF8CFCFF8D1D1F8D3D3F8D4D4F8D6D6F8D8D8F8D9D9
          F8DBDBF8DDDDF8DEDEF8E0E0F8E2E2F8E3E3F8E5E5F8E7E7F8E8E8F7EAEAF7EB
          EBF7EDEDF7EFEFF7F0F0F7F2F2F7F4F4F7F5F5F7F7F7F7F7F7F7F7F7F7F7F7F7
          F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F8F8F8F8F8F8F8F8F8F8F8F8F8F8
          F8F8F8F8F8F8F8F8F8F8F8F8F8F8F8F8F8F8F8F8F8F8F8F8F8F8F8F8F8F8F8F8
          F8F8F8F8F8F8F8F8F8F8F8F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9
          F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9
          F9FAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFA
          FAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFBFBFBFBFBFB
          FBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFB
          FBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFCFCFCFCFCFCFCFCFCFCFCFCFC
          FCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFC
          FCFCFCFCFCFCFCFCFCFCFCFCFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
          FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
          FDFDFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFE
          FEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF}
        Material.Texture.TextureMode = tmReplace
        Material.Texture.MappingMode = tmmObjectLinear
        Material.Texture.MappingSCoordinates.Coordinates = {000000000000000082A8FB3700000000}
        Material.Texture.MappingTCoordinates.Coordinates = {00000000000000000000000000000000}
        Material.Texture.Disabled = False
        TextureOffset.Coordinates = {0000003F000000000000000000000000}
      end
      item
        Name = 'contrast'
        Tag = 0
        Material.Texture.MagFilter = maNearest
        Material.Texture.MinFilter = miNearest
        Material.Texture.TextureMode = tmReplace
        Material.Texture.EnvColor.Color = {0000003F0000003F0000003F0000003F}
        Material.Texture.Disabled = False
      end>
    Left = 16
    Top = 136
  end
  object GLTexCombineShader1: TGLTexCombineShader
    Combiners.Strings = (
      'Tex0:=Dot3(Tex0, Col);'
      'Tex1:=Interpolate(Tex0, Tex1, EnvCol);'
      'Tex2:=Tex1*Tex2;'
      'Tex3:=Tex2*Tex3;')
    DesignTimeEnabled = False
    MaterialLibrary = GLMaterialLibrary1
    LibMaterial3Name = 'detail'
    LibMaterial4Name = 'texture'
    Left = 96
    Top = 136
  end
  object GLBumpmapHDS1: TGLBumpmapHDS
    MaxPoolSize = 20
    HeightDataSource = GLBitmapHDS1
    Active = True
    BumpmapLibrary = GLMaterialLibrary1
    OnNewTilePrepared = GLBumpmapHDS1NewTilePrepared
    BumpScale = 0.001000000047497451
    MaxTextures = 0
    Left = 96
    Top = 96
  end
end
