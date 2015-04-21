object Form1: TForm1
  Left = 509
  Top = 334
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Multi Graphic Format I/O'
  ClientHeight = 595
  ClientWidth = 711
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ScrollBox1: TScrollBox
    Left = 16
    Top = 32
    Width = 505
    Height = 529
    TabOrder = 0
    object Image1: TImage
      Left = 0
      Top = 0
      Width = 209
      Height = 217
    end
  end
  object Button1: TButton
    Left = 528
    Top = 32
    Width = 75
    Height = 25
    Caption = 'Abrir Imagen'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 624
    Top = 32
    Width = 75
    Height = 25
    Caption = 'Salvar Imagen'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 624
    Top = 536
    Width = 75
    Height = 25
    Caption = 'Salida'
    TabOrder = 3
    OnClick = Button3Click
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 576
    Width = 711
    Height = 19
    Panels = <
      item
        Width = 300
      end
      item
        Width = 50
      end
      item
        Width = 50
      end
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object Memo1: TMemo
    Left = 528
    Top = 72
    Width = 177
    Height = 449
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssVertical
    TabOrder = 5
  end
  object Button4: TButton
    Left = 536
    Top = 536
    Width = 75
    Height = 25
    Caption = 'Limpia Memo'
    TabOrder = 6
    OnClick = Button4Click
  end
  object OpenDialog1: TOpenDialog
    Left = 392
    Top = 120
  end
  object SaveDialog1: TSaveDialog
    Left = 392
    Top = 176
  end
end
