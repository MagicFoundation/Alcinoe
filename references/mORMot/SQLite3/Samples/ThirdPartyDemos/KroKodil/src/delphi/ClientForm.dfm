object Form1: TForm1
  Left = 231
  Top = 220
  BorderStyle = bsSingle
  ClientHeight = 397
  ClientWidth = 966
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 16
  object StatusBar1: TStatusBar
    Left = 0
    Top = 378
    Width = 966
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object Panel1: TPanel
    Left = 0
    Top = 51
    Width = 966
    Height = 327
    Align = alBottom
    BevelOuter = bvNone
    Caption = 'Panel1'
    TabOrder = 1
    object Label1: TLabel
      Left = 16
      Top = 8
      Width = 67
      Height = 16
      Caption = 'Your name:'
    end
    object Label2: TLabel
      Left = 16
      Top = 56
      Width = 86
      Height = 16
      Caption = 'Your message:'
    end
    object lblA: TLabel
      Left = 312
      Top = 34
      Width = 17
      Height = 16
      Caption = 'A='
    end
    object lblB: TLabel
      Left = 312
      Top = 66
      Width = 16
      Height = 16
      Caption = 'B='
    end
    object lblResult: TLabel
      Left = 604
      Top = 72
      Width = 184
      Height = 16
      Caption = 'Enter numbers, then Call Server'
    end
    object QuestionMemo: TMemo
      Left = 16
      Top = 72
      Width = 257
      Height = 193
      TabOrder = 0
    end
    object NameEdit: TEdit
      Left = 16
      Top = 24
      Width = 81
      Height = 24
      TabOrder = 1
    end
    object AddButton: TButton
      Left = 16
      Top = 280
      Width = 145
      Height = 25
      Caption = 'Add the message'
      TabOrder = 2
      OnClick = AddButtonClick
    end
    object QuitButton: TButton
      Left = 197
      Top = 280
      Width = 75
      Height = 25
      Caption = 'Quit'
      TabOrder = 3
      OnClick = QuitButtonClick
    end
    object FindButton: TButton
      Left = 104
      Top = 24
      Width = 169
      Height = 25
      Caption = 'Find a previous message'
      TabOrder = 4
      OnClick = FindButtonClick
    end
    object edtA: TEdit
      Left = 336
      Top = 32
      Width = 153
      Height = 24
      TabOrder = 5
      Text = '2'
    end
    object edtB: TEdit
      Left = 336
      Top = 64
      Width = 153
      Height = 24
      TabOrder = 6
      Text = '3'
    end
    object btnCall: TButton
      Left = 496
      Top = 32
      Width = 97
      Height = 57
      Caption = 'Call Server'
      TabOrder = 7
      OnClick = btnCallClick
    end
    object Button1: TButton
      Left = 616
      Top = 125
      Width = 75
      Height = 25
      Caption = 'GetFileList'
      TabOrder = 8
      OnClick = Button1Click
    end
    object Edit1: TEdit
      Left = 312
      Top = 125
      Width = 297
      Height = 24
      TabOrder = 9
      Text = '.\'
    end
    object ListView1: TListView
      Left = 312
      Top = 155
      Width = 625
      Height = 151
      Columns = <
        item
          Caption = 'Name'
          Width = 200
        end
        item
          Caption = 'Size'
          Width = 90
        end
        item
          Caption = 'Modified'
          Width = 140
        end
        item
          Caption = 'Version'
          Width = 150
        end>
      TabOrder = 10
      ViewStyle = vsReport
    end
  end
  object Panel2: TPanel
    Left = 8
    Top = 8
    Width = 361
    Height = 41
    BevelOuter = bvNone
    Caption = 'Panel2'
    TabOrder = 2
    object txtUser: TEdit
      Left = 8
      Top = 9
      Width = 121
      Height = 24
      TabOrder = 0
      Text = 'User'
    end
    object txtPassword: TEdit
      Left = 144
      Top = 9
      Width = 121
      Height = 24
      TabOrder = 1
      Text = 'synopse'
    end
    object btnLogin: TButton
      Left = 272
      Top = 7
      Width = 75
      Height = 25
      Caption = 'Login'
      TabOrder = 2
      OnClick = btnLoginClick
    end
  end
end
