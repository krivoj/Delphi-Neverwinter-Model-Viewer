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
    Height = 401
    TabOrder = 0
    object ListBox1: TListBox
      Left = 0
      Top = 0
      Width = 113
      Height = 257
      ItemHeight = 13
      TabOrder = 0
      OnClick = ListBox1Click
    end
    object Button6: TButton
      Left = 0
      Top = 358
      Width = 58
      Height = 25
      Caption = 'Zoom In'
      TabOrder = 1
      OnClick = Button6Click
    end
    object Button7: TButton
      Left = 56
      Top = 358
      Width = 58
      Height = 25
      Caption = 'Zoom Out'
      TabOrder = 2
      OnClick = Button7Click
    end
    object Button1: TButton
      Left = 16
      Top = 312
      Width = 75
      Height = 25
      Caption = 'test'
      TabOrder = 3
      OnClick = Button1Click
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
