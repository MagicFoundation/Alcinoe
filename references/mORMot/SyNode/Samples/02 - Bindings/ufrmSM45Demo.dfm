object frmSM45Demo: TfrmSM45Demo
  Left = 362
  Height = 516
  Top = 176
  Width = 771
  Caption = 'SyNode (Synopse SpiderMonkey with NodeJS modules) demo'
  ClientHeight = 516
  ClientWidth = 771
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  LCLVersion = '2.0.0.2'
  object mSource: TMemo
    Left = 16
    Height = 414
    Top = 24
    Width = 442
    Lines.Strings = (
      '/*'
      '1) We define a property  global.mainForm = binding to this Deplhi form'
      '2) JavaScript debugger is listeninng on the localhost:6000'
      ' - to debug run Firefox with -chrome flag as below'
      '      "C:\Program Files\Firefox Developer Edition\firefox.exe" -chrome chrome://devtools/content/framework/connect/connect.xhtml'
      'OR'
      ' - enable a remote debugger: https://developer.mozilla.org/ru/docs/Tools/Remote_Debugging#On_the_desktop_2'
      ' - go to chrome://devtools/content/framework/connect/connect.xhtml'
      'FF58 for windows has known bug with remote debugger - use either FF57 or FF59 (developer edition)'
      '*/'
      ''
      'mainForm.caption = new Date().toLocaleString();'
      'mainForm.top = 10;'
      ''
      'const fs = require(''fs'');'
      'const path = require(''path'');'
      'let content = fs.readFileSync(path.join(process.cwd(), ''ExtendDebuggerConsole.js''), ''utf8'');'
      'mainForm.toLog(content);'
      '/*'
      ' * You can evaluate script below if you compiled math-module sample'
      ' * to SyNode\Samples\01 - Dll Modules\math-module\build\mathModule.dll'
      ' */'
      ''
      '/*'
      'const mathModule = require(''''../../Samples/01 - Dll Modules/math-module'''');'
      'mainForm.toLog(''''PI='''' + mathModule.pi);'
      'mainForm.toLog(`(1+ 2)=${mathModule.add(1, 2)}`);'
      'mainForm.toLog(`16/3=${mathModule.div(16, 3)}`);'
      '*/'
      ''
      'const http = require(''http'');'
      'const assert = require(''assert'');'
      '// set global proxy settings if client is behind a proxy'
      '// http.setGlobalProxyConfiguration(''''proxy.main:3249'''', ''''localhost'''');'
      'let resp = http.get(''https://synopse.info/fossil/wiki/Synopse+OpenSource'');'
      '// check we are actually behind a proxy'
      '// assert.ok(resp.headers(''''via'''').startsWith(''''1.1 proxy.main''''), ''''proxy used'''');'
      'let index = resp.read();'
      'mainForm.toLog(index);'
    )
    TabOrder = 0
    WordWrap = False
  end
  object mResult: TMemo
    Left = 464
    Height = 414
    Top = 24
    Width = 249
    TabOrder = 1
  end
  object btnEvaluate: TButton
    Left = 16
    Height = 25
    Top = 444
    Width = 129
    Caption = 'Evaluate selected'
    OnClick = btnEvaluateClick
    TabOrder = 2
  end
end
