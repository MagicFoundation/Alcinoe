object TestForm: TTestForm
  Left = 209
  Top = 115
  BorderStyle = bsDialog
  Caption = 'File mapping tester'
  ClientHeight = 271
  ClientWidth = 667
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 12
    Top = 84
    Width = 132
    Height = 13
    Caption = 'Contents of the mapped file:'
  end
  object Label3: TLabel
    Left = 16
    Top = 32
    Width = 81
    Height = 17
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Map Name :'
  end
  object Label5: TLabel
    Left = 276
    Top = 32
    Width = 113
    Height = 17
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Update interval (secs)'
    OnClick = cmdFileClick
  end
  object mmData: TMemo
    Left = 12
    Top = 100
    Width = 425
    Height = 165
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 6
  end
  object edFile: TEdit
    Left = 100
    Top = 4
    Width = 337
    Height = 21
    TabOrder = 1
  end
  object edMap: TEdit
    Left = 100
    Top = 28
    Width = 165
    Height = 21
    TabOrder = 2
  end
  object cmdFile: TButton
    Left = 24
    Top = 4
    Width = 75
    Height = 21
    Caption = 'File Name'
    TabOrder = 0
    OnClick = cmdFileClick
  end
  object cmdGo: TButton
    Left = 284
    Top = 76
    Width = 75
    Height = 21
    Caption = '&Go'
    TabOrder = 4
    OnClick = cmdGoClick
  end
  object cmdWrite: TButton
    Left = 360
    Top = 76
    Width = 75
    Height = 21
    Caption = '&Write'
    Enabled = False
    TabOrder = 5
    OnClick = cmdWriteClick
  end
  object cbTime: TComboBox
    Left = 392
    Top = 28
    Width = 41
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 3
    OnChange = cbTimeChange
    Items.Strings = (
      '1'
      '2'
      '3'
      '4'
      '5'
      '6'
      '7'
      '8'
      '9'
      '10')
  end
  object Memo1: TMemo
    Left = 444
    Top = 4
    Width = 217
    Height = 261
    Color = clBtnFace
    Lines.Strings = (
      'Select a (text) file to share between '
      'applications. (NOTE: This file will be written '
      'to!) If left empty, the pagefile will be used.'
      ''
      'Map Name and Mutex Name must be given '
      'or the file mapping object will be private and '
      'it can not be shared between applications. '
      'When a map is named, a mutex must also be '
      'named.'
      ''
      'Run a few instances of this app '
      'simultaneously and see how data entered in '
      'one app is accessible to the others. Only '
      'one app has to select a file to open, or the '
      'page file will be used. The others should use '
      'the same Map Name and Mutex Name for '
      'this to work.')
    ReadOnly = True
    TabOrder = 7
    WantReturns = False
  end
  object odFile: TOpenDialog
    Left = 408
    Top = 204
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 380
    Top = 204
  end
end
