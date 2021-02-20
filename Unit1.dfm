object Form1: TForm1
  Left = 241
  Top = 210
  Caption = 'Delphi Neverwinter Model Viewer'
  ClientHeight = 602
  ClientWidth = 854
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnMouseDown = FormMouseDown
  OnMouseMove = FormMouseMove
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 113
    Height = 577
    TabOrder = 0
    object ListBox1: TListBox
      Left = 0
      Top = 0
      Width = 113
      Height = 289
      ItemHeight = 13
      TabOrder = 0
      OnClick = ListBox1Click
    end
    object Button6: TButton
      Left = 0
      Top = 542
      Width = 58
      Height = 25
      Caption = 'Zoom In'
      TabOrder = 1
      OnClick = Button6Click
    end
    object Button7: TButton
      Left = 55
      Top = 542
      Width = 58
      Height = 25
      Caption = 'Zoom Out'
      TabOrder = 2
      OnClick = Button7Click
    end
    object Button1: TButton
      Left = 16
      Top = 328
      Width = 89
      Height = 25
      Caption = 'test'
      TabOrder = 3
      Visible = False
      OnClick = Button1Click
    end
    object ComboBox1: TComboBox
      Left = 16
      Top = 301
      Width = 89
      Height = 21
      TabOrder = 4
      OnCloseUp = ComboBox1CloseUp
    end
    object Edit1: TEdit
      Left = 16
      Top = 429
      Width = 89
      Height = 21
      TabOrder = 5
      Text = '0'
    end
    object Edit2: TEdit
      Left = 16
      Top = 456
      Width = 89
      Height = 21
      TabOrder = 6
      Text = '0'
    end
    object Edit3: TEdit
      Left = 16
      Top = 483
      Width = 89
      Height = 21
      TabOrder = 7
      Text = '0'
    end
    object Button2: TButton
      Left = 32
      Top = 378
      Width = 57
      Height = 18
      Caption = 'to0'
      TabOrder = 8
      Visible = False
      OnClick = Button2Click
    end
    object ComboBox2: TComboBox
      Left = 16
      Top = 402
      Width = 89
      Height = 21
      TabOrder = 9
      OnCloseUp = ComboBox2CloseUp
    end
    object Edit4: TEdit
      Left = 16
      Top = 510
      Width = 89
      Height = 21
      TabOrder = 10
      Text = '0'
    end
  end
  object ApplicationEvents1: TApplicationEvents
    OnIdle = ApplicationEvents1Idle
    Left = 815
    Top = 5
  end
  object sf: SE_SearchFiles
    SubDirectories = True
    Left = 136
    Top = 208
  end
end
