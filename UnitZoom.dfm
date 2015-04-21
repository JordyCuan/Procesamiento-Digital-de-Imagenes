object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 426
  ClientWidth = 353
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 48
    Top = 128
    Width = 30
    Height = 13
    Caption = 'Ancho'
  end
  object Label2: TLabel
    Left = 48
    Top = 181
    Width = 19
    Height = 13
    Caption = 'Alto'
  end
  object Label4: TLabel
    Left = 48
    Top = 80
    Width = 46
    Height = 13
    Caption = 'Original : '
  end
  object Label5: TLabel
    Left = 112
    Top = 80
    Width = 3
    Height = 13
  end
  object Label6: TLabel
    Left = 239
    Top = 128
    Width = 60
    Height = 13
    Caption = '(16....3000)'
  end
  object Label7: TLabel
    Left = 239
    Top = 181
    Width = 60
    Height = 13
    Caption = '(16....3000)'
  end
  object Label3: TLabel
    Left = 35
    Top = 232
    Width = 59
    Height = 18
    Caption = 'Porciento'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label8: TLabel
    Left = 32
    Top = 304
    Width = 23
    Height = 16
    Caption = '0 %'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label9: TLabel
    Left = 276
    Top = 307
    Width = 37
    Height = 16
    Caption = '200 %'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object BitBtn3: TBitBtn
    Left = 32
    Top = 376
    Width = 83
    Height = 25
    Caption = '&OK'
    Enabled = False
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 0
    OnClick = BitBtn3Click
  end
  object BitBtn1: TBitBtn
    Left = 224
    Top = 376
    Width = 75
    Height = 25
    Caption = '&Cancelar'
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 1
  end
  object BitBtn2: TBitBtn
    Left = 136
    Top = 331
    Width = 75
    Height = 25
    Caption = '&Valida'
    Kind = bkHelp
    NumGlyphs = 2
    TabOrder = 2
    OnClick = BitBtn2Click
  end
  object Edit1: TEdit
    Left = 112
    Top = 125
    Width = 121
    Height = 21
    TabOrder = 3
  end
  object Edit2: TEdit
    Left = 112
    Top = 181
    Width = 121
    Height = 21
    TabOrder = 4
  end
  object RadioGroup1: TRadioGroup
    Left = 16
    Top = 16
    Width = 329
    Height = 41
    Caption = 'Zoom En:'
    Columns = 2
    Items.Strings = (
      'Tama'#241'o'
      'Porciento')
    TabOrder = 5
    OnClick = RadioGroup1Click
  end
  object TrackBar1: TTrackBar
    Left = 35
    Top = 256
    Width = 264
    Height = 45
    Max = 200
    Frequency = 10
    Position = 50
    TabOrder = 6
    OnChange = TrackBar1Change
  end
end
