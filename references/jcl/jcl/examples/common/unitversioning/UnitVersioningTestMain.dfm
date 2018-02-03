object frmUnitVersioningTestMain: TfrmUnitVersioningTestMain
  Left = 192
  Top = 137
  Caption = 'UnitVersioning Test'
  ClientHeight = 453
  ClientWidth = 695
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object tv: TTreeView
    Left = 0
    Top = 73
    Width = 695
    Height = 380
    Align = alClient
    Indent = 19
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 695
    Height = 73
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object btnTestDummyProvider: TButton
      Left = 152
      Top = 8
      Width = 121
      Height = 25
      Caption = 'Test Dummy Provider'
      TabOrder = 0
      OnClick = btnTestDummyProviderClick
    end
    object btnTestGetLocationInfoStr: TButton
      Left = 280
      Top = 8
      Width = 129
      Height = 25
      Caption = 'Test GetLocationInfoStr'
      TabOrder = 1
      OnClick = btnTestGetLocationInfoStrClick
    end
    object btnShowUVContent: TButton
      Left = 8
      Top = 40
      Width = 153
      Height = 25
      Caption = 'Show UnitVersioning content'
      TabOrder = 2
      OnClick = btnShowUVContentClick
    end
    object btnTestFindMethods: TButton
      Left = 8
      Top = 8
      Width = 137
      Height = 25
      Caption = 'Test IndexOf and FindUnit'
      TabOrder = 3
      OnClick = btnTestFindMethodsClick
    end
    object btnLoadDLL: TButton
      Left = 168
      Top = 40
      Width = 75
      Height = 25
      Caption = 'Load DLL'
      TabOrder = 4
      OnClick = btnLoadDLLClick
    end
    object btnInsertSection: TButton
      Left = 248
      Top = 40
      Width = 137
      Height = 25
      Caption = 'Insert info section into DLL'
      TabOrder = 5
      OnClick = btnInsertSectionClick
    end
  end
end
