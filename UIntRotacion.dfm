object FormRot: TFormRot
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Interface de Rotacion'
  ClientHeight = 458
  ClientWidth = 461
  Color = clGradientActiveCaption
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 20
    Top = 159
    Width = 20
    Height = 18
    Caption = '0 '#176
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 407
    Top = 159
    Width = 31
    Height = 18
    Caption = '359'#176
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label3: TLabel
    Left = 20
    Top = 311
    Width = 13
    Height = 18
    Caption = '0 '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label4: TLabel
    Left = 429
    Top = 311
    Width = 8
    Height = 18
    Caption = '6'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object StaticText1: TStaticText
    Left = 16
    Top = 64
    Width = 422
    Height = 27
    Caption = 'Ingresar un angulo de rotacion en grados (1,359)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
  end
  object TrackBar1: TTrackBar
    Left = 20
    Top = 97
    Width = 433
    Height = 56
    Max = 3590
    Min = 1
    Frequency = 70
    Position = 150
    TabOrder = 1
    ThumbLength = 30
    OnChange = TrackBar1Change
  end
  object BitBtn1: TBitBtn
    Left = 80
    Top = 360
    Width = 97
    Height = 41
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 2
  end
  object BitBtn2: TBitBtn
    Left = 272
    Top = 360
    Width = 97
    Height = 41
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 3
  end
  object StaticText2: TStaticText
    Left = 16
    Top = 200
    Width = 421
    Height = 27
    Caption = 'Ingresar un angulo de rotacion en Radianes (1,6)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
  end
  object TrackBar2: TTrackBar
    Left = 20
    Top = 249
    Width = 433
    Height = 56
    Max = 60
    Min = 1
    Position = 3
    TabOrder = 5
    ThumbLength = 30
    OnChange = TrackBar1Change
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 8
    Width = 153
    Height = 42
    Caption = 'Rotacion En:'
    Columns = 2
    Items.Strings = (
      'Grados'
      'Radianes')
    TabOrder = 6
    OnClick = RadioGroup1Click
  end
end
