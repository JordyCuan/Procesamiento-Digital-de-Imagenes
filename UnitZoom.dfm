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
    Width = 30
    Height = 13
    Caption = 'Alto'
  end
  object Label3: TLabel
    Left = 47
    Top = 36
    Width = 30
    Height = 13
    Caption = 'Modo:'
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
    Width = 117
    Height = 13
  end
  object Validar: TBitBtn
    Left = 48
    Top = 352
    Width = 83
    Height = 25
    Caption = '&OK'
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 0
  end
  object BitBtn1: TBitBtn
    Left = 216
    Top = 352
    Width = 75
    Height = 25
    Caption = '&Cancelar'
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 1
  end
  object BitBtn2: TBitBtn
    Left = 136
    Top = 299
    Width = 75
    Height = 25
    Caption = '&Valida'
    Kind = bkHelp
    NumGlyphs = 2
    TabOrder = 2
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
  object RadioButton1: TRadioButton
    Left = 112
    Top = 32
    Width = 89
    Height = 17
    Caption = 'Tama'#241'o'
    Checked = True
    TabOrder = 5
    TabStop = True
  end
  object RadioButton2: TRadioButton
    Left = 216
    Top = 32
    Width = 89
    Height = 17
    Caption = 'Porciento'
    TabOrder = 6
  end
end
