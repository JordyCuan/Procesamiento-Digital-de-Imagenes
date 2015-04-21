object FFCapZonas: TFFCapZonas
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Captura y Elimina Zonas'
  ClientHeight = 285
  ClientWidth = 466
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 8
    Width = 338
    Height = 16
    Caption = 'De Click Derecho en la TDF para Capturar Zonas a eliminar'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object StringGrid1: TStringGrid
    Left = 24
    Top = 40
    Width = 345
    Height = 234
    DefaultRowHeight = 20
    FixedColor = clSilver
    RowCount = 21
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Button1: TButton
    Left = 383
    Top = 40
    Width = 75
    Height = 25
    Caption = 'Aplica'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 383
    Top = 249
    Width = 75
    Height = 25
    Caption = 'Limpia'
    TabOrder = 2
    OnClick = Button2Click
  end
end
