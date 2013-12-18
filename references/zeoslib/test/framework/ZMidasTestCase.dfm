object ZRemoteDM: TZRemoteDM
  OldCreateOrder = False
  OnCreate = RemoteDataModuleCreate
  Left = 348
  Top = 172
  Height = 239
  Width = 417
  object DSProvider: TDataSetProvider
    Constraints = True
    Options = [poAllowCommandText]
    Left = 91
    Top = 8
  end
  object FMasterSource: TDataSource
    Left = 24
    Top = 8
  end
end
