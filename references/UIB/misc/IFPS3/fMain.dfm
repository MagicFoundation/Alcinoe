object Form1: TForm1
  Left = 292
  Top = 240
  Width = 585
  Height = 372
  Caption = 'RemObjects Pascal Script - Test Application'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 234
    Width = 577
    Height = 3
    Cursor = crVSplit
    Align = alBottom
  end
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 577
    Height = 234
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'program dbtest;'
      'var '
      '  db: TUIBDatabase;'
      '  tr: TUIBTransaction;'
      '  qr: TUIBQuery;'
      'begin'
      '  db := TUIBDatabase.create(nil);'
      '  tr := TUIBTransaction.create(nil);'
      '  qr := TUIBQuery.create(nil);'
      '  try'
      '    db.username := '#39'SYSDBA'#39';'
      '    db.password := '#39'masterkey'#39';'
      
        '    db.databasename := '#39'C:\Program Files\Firebird\Firebird_2_0\e' +
        'xamples\empbuild\EMPLOYEE.FDB'#39';'
      '    db.connected := True;'
      '    tr.database := db;'
      '    qr.transaction := tr;'
      '    qr.sql.text := '#39'select * from employee'#39';'
      '    qr.open(true);'
      '    while not qr.eof do'
      '    begin'
      '       writeln(qr.fields.asstring[1]);'
      '       qr.next;'
      '    end;'
      '  finally'
      '    qr.free;'
      '    tr.free;'
      '    db.free;'
      '  end;'
      'end.')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object Memo2: TMemo
    Left = 0
    Top = 237
    Width = 577
    Height = 89
    Align = alBottom
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
    WordWrap = False
  end
  object PSScript: TPSScript
    CompilerOptions = []
    OnCompile = PSScriptCompile
    OnExecute = PSScriptExecute
    OnCompImport = IFPS3ClassesPlugin1CompImport
    OnExecImport = IFPS3ClassesPlugin1ExecImport
    Plugins = <
      item
        Plugin = PS3DllPlugin
      end>
    UsePreProcessor = False
    Left = 240
    Top = 40
  end
  object PS3DllPlugin: TPSDllPlugin
    Left = 240
    Top = 72
  end
  object MainMenu1: TMainMenu
    Left = 240
    Top = 8
    object Program1: TMenuItem
      Caption = '&Program'
      object Compile1: TMenuItem
        Caption = '&Compile'
        ShortCut = 120
        OnClick = Compile1Click
      end
    end
  end
end
