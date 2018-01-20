object frmSM45Demo: TfrmSM45Demo
  Left = 362
  Top = 176
  Width = 771
  Height = 516
  Caption = 'SyNode (Synopse SpiderMonkey with NodeJS modules) demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object mSource: TMemo
    Left = 16
    Top = 24
    Width = 442
    Height = 414
    Lines.Strings = (
      '/*'
      
        '1) We define a property  global.mainForm = binding to this Deplh' +
        'i form'
      '2) JavaScript debugger is listeninng on the localhost:6000'
      ' - to debug run Firefox browser '
      
        ' - enable a remote debugger: https://developer.mozilla.org/ru/do' +
        'cs/Tools/Remote_Debugging#On_the_desktop_2'
      
        ' - go to chrome://devtools/content/framework/connect/connect.xht' +
        'ml'
      '*/'
      ''
      'mainForm.caption = new Date().toLocaleString();'
      'mainForm.top = 10;'
      ''
      'const fs = require('#39'fs'#39');'
      'const path = require('#39'path'#39');'
      
        'let content = fs.readFileSync(path.join(process.cwd(), '#39'ExtendDe' +
        'buggerConsole.js'#39'));'
      'mainForm.toLog(content);'
      '/* '
      
        ' * You can evaluate script below if you compiled math-module sam' +
        'ple'
      
        ' * to SyNode\Samples\01 - Dll Modules\math-module\build\mathModu' +
        'le.dl'
      ' */ '
      ''
      '/*'
      
        'const mathModule = require('#39'../../Samples/01 - Dll Modules/math-' +
        'module'#39');'
      'mainForm.toLog('#39'PI='#39' + mathModule.pi);'
      'mainForm.toLog(`(1+ 2)=${mathModule.add(1, 2)}`);'
      'mainForm.toLog(`16/3=${mathModule.div(16, 3)}`);'
      '*/'
      ''
      'const http = require('#39'http'#39');'
      'const assert = require('#39'assert'#39');'
      '// set global proxy settings if client is behind a proxy'
      
        '// http.setGlobalProxyConfiguration('#39'proxy.main:3249'#39', '#39'localhos' +
        't'#39');'
      
        'let resp = http.get('#39'https://synopse.info/fossil/wiki/Synopse+Op' +
        'enSource'#39');'
      '// check we are actually behind a proxy'
      
        '// assert.ok(resp.headers('#39'via'#39').startsWith('#39'1.1 proxy.main'#39'), '#39 +
        'proxy used'#39');'
      'let index = resp.read();'
      'mainForm.toLog(index);')
    TabOrder = 0
    WordWrap = False
  end
  object mResult: TMemo
    Left = 464
    Top = 24
    Width = 249
    Height = 414
    TabOrder = 1
  end
  object btnEvaluate: TButton
    Left = 16
    Top = 444
    Width = 129
    Height = 25
    Caption = 'Evaluate selected'
    TabOrder = 2
    OnClick = btnEvaluateClick
  end
end
