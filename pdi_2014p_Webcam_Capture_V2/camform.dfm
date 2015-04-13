object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Webcam Capture'
  ClientHeight = 273
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object shp1: TShape
    Left = 8
    Top = 8
    Width = 345
    Height = 255
    Brush.Color = clWindowFrame
  end
  object img1: TImage
    Left = 16
    Top = 16
    Width = 329
    Height = 241
    Center = True
    Proportional = True
    Stretch = True
  end
  object lbl1: TLabel
    Left = 371
    Top = 217
    Width = 182
    Height = 13
    Caption = 'Quantidade de Imagens por segundo:'
  end
  object lbl2: TLabel
    Left = 371
    Top = 155
    Width = 135
    Height = 13
    Caption = 'Qualidade das imagens JPG:'
  end
  object lbl4: TLabel
    Left = 371
    Top = 9
    Width = 79
    Height = 13
    Caption = 'Webcam Status:'
  end
  object lbl_camstatus: TLabel
    Left = 453
    Top = 9
    Width = 46
    Height = 13
    Caption = 'Desligada'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object btn1: TButton
    Left = 371
    Top = 86
    Width = 241
    Height = 25
    Caption = 'Pegar Imagem'
    TabOrder = 0
    OnClick = btn1Click
  end
  object btn2: TButton
    Left = 371
    Top = 117
    Width = 241
    Height = 25
    Caption = 'Video Cam'
    TabOrder = 1
    OnClick = btn2Click
  end
  object btn3: TButton
    Left = 371
    Top = 240
    Width = 241
    Height = 25
    Caption = 'Salvar Na Pasta'
    TabOrder = 2
    OnClick = btn3Click
  end
  object edt1: TEdit
    Left = 559
    Top = 213
    Width = 53
    Height = 21
    NumbersOnly = True
    TabOrder = 3
  end
  object trckbr1: TTrackBar
    Left = 371
    Top = 178
    Width = 241
    Height = 29
    Max = 100
    Min = 1
    Position = 100
    TabOrder = 4
    OnChange = trckbr1Change
  end
  object edt2: TEdit
    Left = 512
    Top = 151
    Width = 100
    Height = 21
    NumbersOnly = True
    TabOrder = 5
    OnChange = edt2Change
  end
  object btnligar: TButton
    Left = 371
    Top = 55
    Width = 121
    Height = 25
    Caption = 'Ligar'
    TabOrder = 6
    OnClick = btnligarClick
  end
  object btndesligar: TButton
    Left = 498
    Top = 55
    Width = 114
    Height = 25
    Caption = 'Desligar'
    TabOrder = 7
    OnClick = btndesligarClick
  end
  object cbb1: TComboBox
    Left = 371
    Top = 28
    Width = 241
    Height = 21
    TabOrder = 8
    Text = 'Dispositivos:'
  end
end
