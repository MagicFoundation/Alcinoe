object MainForm: TMainForm
  Left = 245
  Top = 228
  BorderStyle = bsDialog
  Caption = ' mORMot HTTP Client Stress Test'
  ClientHeight = 292
  ClientWidth = 666
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    666
    292)
  PixelsPerInch = 96
  TextHeight = 13
  object lbledtServerAddress: TLabeledEdit
    Left = 24
    Top = 24
    Width = 177
    Height = 21
    EditLabel.Width = 174
    EditLabel.Height = 13
    EditLabel.Caption = 'Remote HTTP server IP (port is 888)'
    TabOrder = 0
    Text = '127.0.0.1'
  end
  object lbledtClientThreadCount: TLabeledEdit
    Left = 24
    Top = 128
    Width = 177
    Height = 21
    EditLabel.Width = 118
    EditLabel.Height = 13
    EditLabel.Caption = 'Number of client threads'
    TabOrder = 2
    Text = '50'
    OnKeyPress = lbledtClientPerThreadInstanceCountKeyPress
  end
  object lbledtClientPerThreadInstanceCount: TLabeledEdit
    Left = 24
    Top = 168
    Width = 177
    Height = 21
    EditLabel.Width = 180
    EditLabel.Height = 13
    EditLabel.Caption = 'Number of client instances per thread'
    TabOrder = 3
    Text = '1'
    OnKeyPress = lbledtClientPerThreadInstanceCountKeyPress
  end
  object lbledtNumberOfObjectAdded: TLabeledEdit
    Left = 24
    Top = 72
    Width = 177
    Height = 21
    EditLabel.Width = 121
    EditLabel.Height = 13
    EditLabel.Caption = 'Number of objects added'
    TabOrder = 1
    Text = '10000'
    OnKeyPress = lbledtClientPerThreadInstanceCountKeyPress
  end
  object btnStart: TButton
    Left = 24
    Top = 216
    Width = 129
    Height = 49
    Caption = 'Start'
    TabOrder = 4
    OnClick = btnStartClick
  end
  object mmoInfo: TMemo
    Left = 224
    Top = 8
    Width = 433
    Height = 273
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      '')
    ScrollBars = ssVertical
    TabOrder = 5
  end
  object chkSocketAPI: TCheckBox
    Left = 24
    Top = 269
    Width = 129
    Height = 17
    Caption = 'Use Socket API'
    TabOrder = 6
  end
end
