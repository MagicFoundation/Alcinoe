object Form1: TForm1
  Left = 228
  Top = 165
  Width = 729
  Height = 540
  ActiveControl = EnvironmentView
  Caption = 'Environment Variables'
  OnCreate = FormCreate
  PixelsPerInch = 96
  object EnvironmentView: TListView
    Left = 0
    Top = 0
    Width = 729
    Height = 540
    Align = alClient
    Columns = <
      item
        AutoSize = True
        Caption = 'Environment variable'
        Tag = 0
        Width = 200
      end
      item
        AutoSize = True
        Caption = 'Value'
        Tag = 0
        Width = 500
      end>
    RowSelect = True
    ReadOnly = True
    TabOrder = 0
    ViewStyle = vsReport
  end
end
