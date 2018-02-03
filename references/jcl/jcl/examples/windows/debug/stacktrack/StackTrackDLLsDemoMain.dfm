object MainForm: TMainForm
  Left = 555
  Top = 318
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Exceptions in DLLs example'
  ClientHeight = 296
  ClientWidth = 235
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
  object StaticLibGroupBox: TGroupBox
    Left = 8
    Top = 8
    Width = 217
    Height = 65
    Caption = 'Statically linked library'
    TabOrder = 0
    object StaticLibError1Btn: TButton
      Left = 23
      Top = 24
      Width = 75
      Height = 25
      Caption = 'Error1'
      TabOrder = 0
      OnClick = StaticLibError1BtnClick
    end
    object StaticLibError2Btn: TButton
      Left = 119
      Top = 24
      Width = 75
      Height = 25
      Caption = 'Error2'
      TabOrder = 1
      OnClick = StaticLibError2BtnClick
    end
  end
  object ComObjGroupBox: TGroupBox
    Left = 8
    Top = 216
    Width = 217
    Height = 65
    Caption = 'COM object'
    TabOrder = 1
    object ComObjErr1Btn: TButton
      Left = 23
      Top = 24
      Width = 75
      Height = 25
      Caption = 'Error1'
      TabOrder = 0
      OnClick = ComObjErr1BtnClick
    end
    object ComObjErr2Btn: TButton
      Left = 119
      Top = 24
      Width = 75
      Height = 25
      Caption = 'Error2'
      TabOrder = 1
      OnClick = ComObjErr2BtnClick
    end
  end
  object DynLibGroupBox: TGroupBox
    Left = 8
    Top = 88
    Width = 217
    Height = 113
    Caption = 'Dynamically linked library'
    TabOrder = 2
    object DynamicLibError1Btn: TButton
      Left = 23
      Top = 64
      Width = 75
      Height = 25
      Caption = 'Error1'
      TabOrder = 0
      OnClick = DynamicLibError1BtnClick
    end
    object DynamicLibError2Btn: TButton
      Left = 119
      Top = 64
      Width = 75
      Height = 25
      Caption = 'Error2'
      TabOrder = 1
      OnClick = DynamicLibError2BtnClick
    end
    object LoadLibBtn: TButton
      Left = 23
      Top = 24
      Width = 75
      Height = 25
      Caption = 'Load'
      TabOrder = 2
      OnClick = LoadLibBtnClick
    end
    object FreeLibBtn: TButton
      Left = 119
      Top = 24
      Width = 75
      Height = 25
      Caption = 'Free'
      TabOrder = 3
      OnClick = FreeLibBtnClick
    end
  end
end
