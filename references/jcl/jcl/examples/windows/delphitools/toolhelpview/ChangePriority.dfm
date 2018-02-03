object ChangePriorityDlg: TChangePriorityDlg
  Left = 235
  Top = 178
  ActiveControl = PriorityRadioGroup
  BorderStyle = bsDialog
  Caption = 'Change process priority'
  ClientHeight = 111
  ClientWidth = 229
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object OKBtn: TButton
    Left = 148
    Top = 8
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 0
    OnClick = OKBtnClick
  end
  object CancelBtn: TButton
    Left = 148
    Top = 38
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object PriorityRadioGroup: TRadioGroup
    Left = 8
    Top = 3
    Width = 129
    Height = 102
    Caption = 'Priority'
    TabOrder = 2
  end
end
