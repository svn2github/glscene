object aUniversesForm: TaUniversesForm
  Left = 258
  Top = 87
  HelpContext = 1856
  Caption = '3D Universe Rules'
  ClientHeight = 299
  ClientWidth = 342
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001001010100000000000280100001600000028000000100000002000
    00000100040000000000C0000000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    09999900033300009440AA900033000944000AA900030094400000AA90000944
    00CCC00AA90094400CCCCC00AA904400CCCCCCC00AA9000CCCC9CCCC0000000C
    CC999CCC0000E00CCCC9CCCC00B0EE00CCCCCCC00BBDDEE00CCCCC00BBD00DEE
    00CCC00BBD0050DEE00000BBD008550DEE000BBD00885550DE000BD00888F838
    0000F11C0000E38E0000C7C700008C6300001831000030180000E00F0000E00F
    0000600D000030180000183100008C63000047C60000238C000013980000}
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 32
    Top = 4
    Width = 276
    Height = 39
    Caption = 
      'Conway'#39's Life is : 23/3  ( Location does NOT matter )'#13#10'2 or 3 ce' +
      'll neighbors retain life        / 3 neighbors create life'#13#10'3D: 3' +
      '45                                          / 5'
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 80
    Width = 161
    Height = 185
    HelpContext = 1856
    Caption = 'Retainer 3x3x3= (27 - self = 26)'
    TabOrder = 0
    object R0: TCheckBox
      Left = 16
      Top = 16
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '0'
      TabOrder = 0
    end
    object R1: TCheckBox
      Left = 16
      Top = 32
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '1'
      TabOrder = 1
    end
    object R2: TCheckBox
      Left = 16
      Top = 48
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '2'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object R3: TCheckBox
      Left = 16
      Top = 64
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '3'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object R4: TCheckBox
      Left = 16
      Top = 80
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '4'
      TabOrder = 4
    end
    object R5: TCheckBox
      Left = 16
      Top = 96
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '5'
      TabOrder = 5
    end
    object R6: TCheckBox
      Left = 16
      Top = 112
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '6'
      TabOrder = 6
    end
    object R7: TCheckBox
      Left = 16
      Top = 128
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '7'
      TabOrder = 7
    end
    object R8: TCheckBox
      Left = 16
      Top = 144
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '8'
      TabOrder = 8
    end
    object R9: TCheckBox
      Left = 16
      Top = 160
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '9'
      TabOrder = 9
    end
    object R19: TCheckBox
      Left = 56
      Top = 160
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '19'
      TabOrder = 10
    end
    object R18: TCheckBox
      Left = 56
      Top = 144
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '18'
      TabOrder = 11
    end
    object R17: TCheckBox
      Left = 56
      Top = 128
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '17'
      TabOrder = 12
    end
    object R16: TCheckBox
      Left = 56
      Top = 112
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '16'
      TabOrder = 13
    end
    object R15: TCheckBox
      Left = 56
      Top = 96
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '15'
      TabOrder = 14
    end
    object R14: TCheckBox
      Left = 56
      Top = 80
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '14'
      TabOrder = 15
    end
    object R13: TCheckBox
      Left = 56
      Top = 64
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '13'
      TabOrder = 16
    end
    object R12: TCheckBox
      Left = 56
      Top = 48
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '12'
      TabOrder = 17
    end
    object R11: TCheckBox
      Left = 56
      Top = 32
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '11'
      TabOrder = 18
    end
    object R10: TCheckBox
      Left = 56
      Top = 16
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '10'
      TabOrder = 19
    end
    object R26: TCheckBox
      Left = 96
      Top = 112
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '26'
      TabOrder = 20
    end
    object R25: TCheckBox
      Left = 96
      Top = 96
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '25'
      TabOrder = 21
    end
    object R24: TCheckBox
      Left = 96
      Top = 80
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '24'
      TabOrder = 22
    end
    object R23: TCheckBox
      Left = 96
      Top = 64
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '23'
      TabOrder = 23
    end
    object R22: TCheckBox
      Left = 96
      Top = 48
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '22'
      TabOrder = 24
    end
    object R21: TCheckBox
      Left = 96
      Top = 32
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '21'
      TabOrder = 25
    end
    object R20: TCheckBox
      Left = 96
      Top = 16
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '20'
      TabOrder = 26
    end
  end
  object BitBtn1: TBitBtn
    Left = 264
    Top = 272
    Width = 75
    Height = 25
    HelpContext = 1856
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 1
    OnClick = BitBtn1Click
  end
  object BitBtn2: TBitBtn
    Left = 8
    Top = 272
    Width = 75
    Height = 25
    HelpContext = 1856
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 2
  end
  object GroupBox2: TGroupBox
    Left = 176
    Top = 80
    Width = 161
    Height = 185
    HelpContext = 1856
    Caption = 'Creators 3x3x3= (27 - self = 26)'
    TabOrder = 3
    object C0: TCheckBox
      Left = 16
      Top = 16
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '0'
      TabOrder = 0
    end
    object C1: TCheckBox
      Left = 16
      Top = 32
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '1'
      TabOrder = 1
    end
    object C2: TCheckBox
      Left = 16
      Top = 48
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '2'
      TabOrder = 2
    end
    object C3: TCheckBox
      Left = 16
      Top = 64
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '3'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object C4: TCheckBox
      Left = 16
      Top = 80
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '4'
      TabOrder = 4
    end
    object C5: TCheckBox
      Left = 16
      Top = 96
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '5'
      TabOrder = 5
    end
    object C6: TCheckBox
      Left = 16
      Top = 112
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '6'
      TabOrder = 6
    end
    object C7: TCheckBox
      Left = 16
      Top = 128
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '7'
      TabOrder = 7
    end
    object C8: TCheckBox
      Left = 16
      Top = 144
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '8'
      TabOrder = 8
    end
    object C9: TCheckBox
      Left = 16
      Top = 160
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '9'
      TabOrder = 9
    end
    object C19: TCheckBox
      Left = 56
      Top = 160
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '19'
      TabOrder = 10
    end
    object C18: TCheckBox
      Left = 56
      Top = 144
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '18'
      TabOrder = 11
    end
    object C17: TCheckBox
      Left = 56
      Top = 128
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '17'
      TabOrder = 12
    end
    object C16: TCheckBox
      Left = 56
      Top = 112
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '16'
      TabOrder = 13
    end
    object C15: TCheckBox
      Left = 56
      Top = 96
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '15'
      TabOrder = 14
    end
    object C14: TCheckBox
      Left = 56
      Top = 80
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '14'
      TabOrder = 15
    end
    object C13: TCheckBox
      Left = 56
      Top = 64
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '13'
      TabOrder = 16
    end
    object C12: TCheckBox
      Left = 56
      Top = 48
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '12'
      TabOrder = 17
    end
    object C11: TCheckBox
      Left = 56
      Top = 32
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '11'
      TabOrder = 18
    end
    object C10: TCheckBox
      Left = 56
      Top = 16
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '10'
      TabOrder = 19
    end
    object C26: TCheckBox
      Left = 96
      Top = 112
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '26'
      TabOrder = 20
    end
    object C25: TCheckBox
      Left = 96
      Top = 96
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '25'
      TabOrder = 21
    end
    object C24: TCheckBox
      Left = 96
      Top = 80
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '24'
      TabOrder = 22
    end
    object C23: TCheckBox
      Left = 96
      Top = 64
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '23'
      TabOrder = 23
    end
    object C22: TCheckBox
      Left = 96
      Top = 48
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '22'
      TabOrder = 24
    end
    object C21: TCheckBox
      Left = 96
      Top = 32
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '21'
      TabOrder = 25
    end
    object C20: TCheckBox
      Left = 96
      Top = 16
      Width = 41
      Height = 17
      HelpContext = 1856
      Caption = '20'
      TabOrder = 26
    end
  end
  object BitBtn3: TBitBtn
    Left = 136
    Top = 272
    Width = 75
    Height = 25
    HelpContext = 1856
    Kind = bkHelp
    NumGlyphs = 2
    TabOrder = 4
    OnClick = BitBtn3Click
  end
  object ComboBox1: TComboBox
    Left = 8
    Top = 48
    Width = 329
    Height = 21
    Hint = 'Select not edit'
    HelpContext = 1856
    TabOrder = 5
    Text = 'Conway'#39's Life      | 23/3'
    OnChange = ComboBox1Change
    Items.Strings = (
      '/2:  Seeds (2)'
      '/234:  Serviettes'
      '01245678/34:  Hensel Artist'
      '012345678/3:  Flakes'
      '1/1:  Gnarl'
      '1234/3:  Mazectric'
      '12345/3:  Maze'
      '12345/37:  Maze Rats!'
      '12345/45:  Mazex'
      '125/36  :2x2'
      '125678/367:  White coagulations'
      '1357/1357:  Replicator'
      '1358/357:  Amoeba'
      '23/3:  Conway'#39's Life'
      '23/36:  HighLife'
      '2345/45678:  WalledCities'
      '235678/3678:  Stains'
      '235678/378:  Coagulations'
      '238/357:  Pseudo life'
      '245/368:  Move'
      '34/34:  34 Life'
      '345/5:  3D Life'
      '34678/0123478: InverseLife'
      '34678/3678:  Day & Night'
      '4567/345:  Assimilation'
      '45678/3:  Coral'
      '5/345:  Long life'
      '5678/35678:  Diamoeba')
  end
end
