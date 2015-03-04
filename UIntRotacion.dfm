object FormRot: TFormRot
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Interface de Rotacion'
  ClientHeight = 179
  ClientWidth = 449
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
    Left = 200
    Top = 120
    Width = 41
    Height = 18
    Caption = '15.0 '#176
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object StaticText1: TStaticText
    Left = 8
    Top = 8
    Width = 433
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
    Left = 8
    Top = 41
    Width = 433
    Height = 56
    Max = 3590
    Min = 1
    Frequency = 36
    Position = 150
    TabOrder = 1
    ThumbLength = 30
    OnChange = TrackBar1Change
  end
  object BitBtn1: TBitBtn
    Left = 56
    Top = 120
    Width = 97
    Height = 41
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 2
  end
  object BitBtn2: TBitBtn
    Left = 288
    Top = 120
    Width = 97
    Height = 41
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 3
  end
end
