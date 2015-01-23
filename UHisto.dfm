object FormHisto: TFormHisto
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Histograma'
  ClientHeight = 509
  ClientWidth = 601
  Color = cl3DDkShadow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 8
    Top = 8
    Width = 585
    Height = 346
    OnMouseMove = Image1MouseMove
  end
  object Panel1: TPanel
    Left = 8
    Top = 360
    Width = 585
    Height = 105
    Color = clMoneyGreen
    ParentBackground = False
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 9
      Width = 34
      Height = 13
      Caption = 'Niveles'
    end
    object Button1: TButton
      Left = 16
      Top = 71
      Width = 75
      Height = 25
      Caption = 'Refrescar'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 456
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Salvar Imag.'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 456
      Top = 48
      Width = 75
      Height = 25
      Caption = 'Salvar Datos'
      TabOrder = 2
      OnClick = Button3Click
    end
    object RadioGroup1: TRadioGroup
      Left = 224
      Top = 16
      Width = 105
      Height = 65
      Caption = 'Escala Vertical'
      ItemIndex = 0
      Items.Strings = (
        'Lineal'
        'Log')
      TabOrder = 3
    end
    object RadioGroup2: TRadioGroup
      Left = 344
      Top = 16
      Width = 105
      Height = 65
      Caption = 'M'#225'ximos'
      ItemIndex = 0
      Items.Strings = (
        'Global'
        'Parciales')
      TabOrder = 4
    end
    object GroupBox1: TGroupBox
      Left = 109
      Top = 16
      Width = 106
      Height = 81
      Caption = 'Canales Visibles '
      TabOrder = 5
      object CheckBox1: TCheckBox
        Left = 9
        Top = 14
        Width = 80
        Height = 18
        Caption = 'Rojo'
        Checked = True
        State = cbChecked
        TabOrder = 0
      end
      object CheckBox2: TCheckBox
        Left = 9
        Top = 30
        Width = 80
        Height = 17
        Caption = 'Verde'
        Checked = True
        State = cbChecked
        TabOrder = 1
      end
      object CheckBox3: TCheckBox
        Left = 9
        Top = 46
        Width = 80
        Height = 17
        Caption = 'Azul'
        Checked = True
        State = cbChecked
        TabOrder = 2
      end
      object CheckBox4: TCheckBox
        Left = 9
        Top = 61
        Width = 80
        Height = 17
        Caption = 'Grises'
        TabOrder = 3
      end
    end
    object ComboBox1: TComboBox
      Left = 16
      Top = 30
      Width = 75
      Height = 22
      Style = csOwnerDrawFixed
      ItemIndex = 0
      TabOrder = 6
      Text = '256'
      Items.Strings = (
        '256'
        '512')
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 490
    Width = 601
    Height = 19
    Panels = <
      item
        Width = 70
      end
      item
        Width = 70
      end
      item
        Width = 70
      end
      item
        Width = 70
      end
      item
        Width = 70
      end
      item
        Width = 70
      end
      item
        Width = 70
      end
      item
        Width = 70
      end
      item
        Width = 50
      end>
  end
  object StatusBar2: TStatusBar
    Left = 0
    Top = 471
    Width = 601
    Height = 19
    Color = clSkyBlue
    Panels = <
      item
        Alignment = taCenter
        Bevel = pbRaised
        Text = 'MaxR'
        Width = 70
      end
      item
        Alignment = taCenter
        Bevel = pbRaised
        Text = 'PobR'
        Width = 70
      end
      item
        Alignment = taCenter
        Bevel = pbRaised
        Text = 'MaxG'
        Width = 70
      end
      item
        Alignment = taCenter
        Bevel = pbRaised
        Text = 'PobG'
        Width = 70
      end
      item
        Alignment = taCenter
        Bevel = pbRaised
        Text = 'MaxB'
        Width = 70
      end
      item
        Alignment = taCenter
        Bevel = pbRaised
        Text = 'PobB'
        Width = 70
      end
      item
        Alignment = taCenter
        Bevel = pbRaised
        Text = 'MaxZ'
        Width = 70
      end
      item
        Alignment = taCenter
        Bevel = pbRaised
        Text = 'PobZ'
        Width = 70
      end
      item
        Text = 'k'
        Width = 50
      end>
  end
  object SavePictureDialog1: TSavePictureDialog
    Left = 104
    Top = 64
  end
  object SaveDialog1: TSaveDialog
    Left = 256
    Top = 72
  end
end
