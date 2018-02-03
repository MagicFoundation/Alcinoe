object TextFrame: TTextFrame
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  TabOrder = 0
  TabStop = True
  object PanelOptions: TPanel
    Left = 0
    Top = 240
    Width = 320
    Height = 0
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
  end
  object PanelText: TPanel
    Left = 0
    Top = 0
    Width = 320
    Height = 240
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object RichEditText: TRichEdit
      Left = 16
      Top = 16
      Width = 288
      Height = 208
      Anchors = [akLeft, akTop, akRight, akBottom]
      ScrollBars = ssBoth
      TabOrder = 0
    end
  end
end
