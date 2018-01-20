object MainForm: TMainForm
  Left = 174
  Top = 123
  Width = 1023
  Height = 552
  Caption = 
    ' Test Mustache libraries: native SynMustache vs JavaScript/Spide' +
    'rMonkey'
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 980
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    1007
    514)
  PixelsPerInch = 96
  TextHeight = 13
  object lblTemplate: TLabel
    Left = 16
    Top = 8
    Width = 44
    Height = 13
    Caption = 'Template'
  end
  object lblContext: TLabel
    Left = 16
    Top = 264
    Width = 39
    Height = 13
    Caption = 'Context'
  end
  object lblIteration: TLabel
    Left = 584
    Top = 40
    Width = 102
    Height = 13
    Caption = 'Number of iterations:'
  end
  object mmoTemplate: TMemo
    Left = 8
    Top = 24
    Width = 537
    Height = 233
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Consolas'
    Font.Style = []
    Lines.Strings = (
      
        '{{! taken from http://boilingplastic.com/using-mustache-template' +
        's-for-javascript-practical-examples}}'
      ''
      '<h2>Example 6 : Recursively binding data to templates</h2>'
      ''
      '    <h3>Organization Structure</h3>'
      '    {{> person}}'
      ''
      '{{<person}}'
      '    <div>'
      '        <b>{{name}}</b> ({{title}})'
      '        <div style='#39'padding-left: 15px; padding-top: 5px;'#39'>'
      '            {{#manages}}'
      '                {{>person}}'
      '            {{/manages}}'
      '        </div>'
      '    </div> '
      '{{/person}}')
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
    WordWrap = False
  end
  object mmoContext: TMemo
    Left = 8
    Top = 280
    Width = 537
    Height = 226
    Anchors = [akLeft, akTop, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Consolas'
    Font.Style = []
    Lines.Strings = (
      ' { title : "President", name : "Perry President", manages : ['
      '        { title : "CTO", name : "Janet TechOff", manages : ['
      
        '            { title : "Web Architect", name : "Hari Archie", man' +
        'ages : ['
      
        '                { title : "Senior Developer", name : "Brenda Sen' +
        'ior", manages : []},'
      
        '                { title : "Developer", name : "Roger Develo", ma' +
        'nages : []},'
      
        '                { title : "Junior Developer", name : "Jerry Juni' +
        'or", manages : []}'
      '            ]}'
      '        ]},   '
      '        { title : "HRO", name : "Harold HarOff", manages : ['
      
        '            { title : "HR Officer", name : "Susan McHorror", man' +
        'ages : []},'
      
        '            { title : "HR Auditor", name : "Audrey O'#39'Fae", manag' +
        'es : []}'
      '        ]}'
      ']}')
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 1
    WordWrap = False
  end
  object btnExecSynMustache: TButton
    Left = 576
    Top = 64
    Width = 121
    Height = 49
    Caption = 'Render with SynMustache'
    TabOrder = 2
    WordWrap = True
    OnClick = Render
  end
  object btnExecSpiderMonkey: TButton
    Left = 712
    Top = 64
    Width = 121
    Height = 49
    Caption = 'Render with  mustache.js'
    TabOrder = 3
    WordWrap = True
    OnClick = Render
  end
  object mmoResult: TMemo
    Left = 560
    Top = 128
    Width = 417
    Height = 378
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 4
    WordWrap = False
  end
  object btnOpenBrowser: TButton
    Left = 872
    Top = 102
    Width = 105
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Show in Browser'
    TabOrder = 5
    OnClick = btnOpenBrowserClick
  end
  object edtIteration: TEdit
    Left = 691
    Top = 38
    Width = 70
    Height = 21
    TabOrder = 6
    Text = '10000'
  end
end
