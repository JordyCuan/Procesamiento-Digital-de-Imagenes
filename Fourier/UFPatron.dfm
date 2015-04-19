object FFPatron: TFFPatron
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Propiedades del Patr'#243'n'
  ClientHeight = 394
  ClientWidth = 628
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 50
    Top = 16
    Width = 33
    Height = 13
    Caption = 'Angulo'
  end
  object Label2: TLabel
    Left = 50
    Top = 80
    Width = 30
    Height = 13
    Caption = 'Ancho'
  end
  object Label3: TLabel
    Left = 50
    Top = 144
    Width = 36
    Height = 13
    Caption = 'M'#225'ximo'
  end
  object Label4: TLabel
    Left = 117
    Top = 42
    Width = 6
    Height = 13
    Caption = '0'
  end
  object Label5: TLabel
    Left = 249
    Top = 42
    Width = 18
    Height = 13
    Caption = '179'
  end
  object Label6: TLabel
    Left = 181
    Top = 42
    Width = 12
    Height = 13
    Caption = '90'
  end
  object Label7: TLabel
    Left = 274
    Top = 16
    Width = 7
    Height = 16
    Caption = '0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label8: TLabel
    Left = 117
    Top = 106
    Width = 12
    Height = 13
    Caption = '10'
  end
  object Label9: TLabel
    Left = 250
    Top = 106
    Width = 12
    Height = 13
    Caption = '30'
  end
  object Label10: TLabel
    Left = 274
    Top = 80
    Width = 14
    Height = 16
    Caption = '10'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label11: TLabel
    Left = 117
    Top = 170
    Width = 12
    Height = 13
    Caption = '10'
  end
  object Label12: TLabel
    Left = 245
    Top = 170
    Width = 18
    Height = 13
    Caption = '127'
  end
  object Label13: TLabel
    Left = 274
    Top = 144
    Width = 14
    Height = 16
    Caption = '10'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Image1: TImage
    Left = 107
    Top = 203
    Width = 180
    Height = 180
  end
  object Bevel1: TBevel
    Left = 308
    Top = 16
    Width = 9
    Height = 369
    Style = bsRaised
  end
  object Image2: TImage
    Left = 344
    Top = 128
    Width = 256
    Height = 256
    Stretch = True
  end
  object Label14: TLabel
    Left = 344
    Top = 16
    Width = 27
    Height = 13
    Caption = 'Alpha'
  end
  object Label15: TLabel
    Left = 577
    Top = 16
    Width = 23
    Height = 13
    Caption = '50%'
  end
  object Label16: TLabel
    Left = 390
    Top = 47
    Width = 6
    Height = 13
    Caption = '0'
  end
  object Label17: TLabel
    Left = 464
    Top = 47
    Width = 12
    Height = 13
    Caption = '50'
  end
  object Label18: TLabel
    Left = 535
    Top = 47
    Width = 18
    Height = 13
    Caption = '100'
  end
  object Label19: TLabel
    Left = 384
    Top = 66
    Width = 27
    Height = 16
    Caption = 'Peso'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object TrackBar1: TTrackBar
    Left = 112
    Top = 16
    Width = 150
    Height = 20
    Max = 179
    Frequency = 3
    TabOrder = 0
    ThumbLength = 10
    OnChange = TrackBar1Change
  end
  object TrackBar2: TTrackBar
    Left = 114
    Top = 80
    Width = 150
    Height = 20
    Max = 30
    Min = 10
    Position = 10
    TabOrder = 1
    ThumbLength = 10
    OnChange = TrackBar2Change
  end
  object TrackBar3: TTrackBar
    Left = 114
    Top = 144
    Width = 150
    Height = 20
    Max = 127
    Min = 10
    Frequency = 5
    Position = 10
    TabOrder = 2
    ThumbLength = 10
    OnChange = TrackBar3Change
  end
  object Button2: TButton
    Left = 8
    Top = 203
    Width = 75
    Height = 25
    Caption = 'Vista'
    TabOrder = 3
    OnClick = Button2Click
  end
  object BitBtn1: TBitBtn
    Left = 8
    Top = 320
    Width = 75
    Height = 25
    TabOrder = 4
    Kind = bkOK
  end
  object BitBtn2: TBitBtn
    Left = 8
    Top = 358
    Width = 75
    Height = 25
    TabOrder = 5
    Kind = bkCancel
  end
  object Button3: TButton
    Left = 8
    Top = 241
    Width = 75
    Height = 25
    Caption = 'Aplica'
    TabOrder = 6
    OnClick = Button3Click
  end
  object TrackBar4: TTrackBar
    Left = 384
    Top = 16
    Width = 169
    Height = 25
    Max = 100
    Min = 1
    Frequency = 5
    Position = 50
    TabOrder = 7
    ThumbLength = 15
    OnChange = TrackBar4Change
  end
  object Panel1: TPanel
    Left = 365
    Top = 94
    Width = 209
    Height = 25
    Caption = 'Panel1'
    TabOrder = 8
  end
  object RadioButton1: TRadioButton
    Left = 419
    Top = 66
    Width = 57
    Height = 17
    Caption = 'Normal'
    Checked = True
    TabOrder = 9
    TabStop = True
  end
  object RadioButton2: TRadioButton
    Left = 492
    Top = 66
    Width = 61
    Height = 17
    Caption = 'Fuerte'
    TabOrder = 10
  end
  object Button1: TButton
    Left = 8
    Top = 280
    Width = 75
    Height = 25
    Caption = 'Salva Patr'#243'n'
    TabOrder = 11
    OnClick = Button1Click
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'bmp'
    Filter = 'BitMap (*.bmp)|*.bmp'
    InitialDir = '.'
    Title = 'Salvar Patr'#243'n'
    Left = 176
    Top = 276
  end
end
