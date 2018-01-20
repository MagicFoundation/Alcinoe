object LoginForm: TLoginForm
  Left = 251
  Top = 104
  Width = 249
  Height = 196
  AutoSize = True
  BorderStyle = bsSizeToolWin
  Caption = 'Log-in'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 241
    Height = 169
    TabOrder = 0
    object Label1: TLabel
      Left = 24
      Top = 88
      Width = 191
      Height = 33
      AutoSize = False
      Caption = 
        'Try admin / admin or demo / demo if you don'#39't have an own accoun' +
        't yet'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 4934475
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object LabeledEdit1: TLabeledEdit
      Left = 80
      Top = 32
      Width = 121
      Height = 21
      EditLabel.Width = 52
      EditLabel.Height = 13
      EditLabel.Caption = 'username: '
      LabelPosition = lpLeft
      TabOrder = 0
      OnKeyDown = LabeledEdit2KeyDown
    end
    object LabeledEdit2: TLabeledEdit
      Left = 80
      Top = 56
      Width = 121
      Height = 21
      EditLabel.Width = 51
      EditLabel.Height = 13
      EditLabel.Caption = 'password: '
      LabelPosition = lpLeft
      PasswordChar = '*'
      TabOrder = 1
      OnKeyDown = LabeledEdit2KeyDown
    end
    object Button1: TButton
      Left = 128
      Top = 128
      Width = 75
      Height = 25
      Caption = 'OK'
      ModalResult = 1
      TabOrder = 2
      OnClick = Button1Click
    end
  end
end
