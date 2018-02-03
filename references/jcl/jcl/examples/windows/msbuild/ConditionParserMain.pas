unit ConditionParserMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFormMain = class(TForm)
    LabelProperties: TLabel;
    MemoProperties: TMemo;
    LabelConditions: TLabel;
    MemoConditions: TMemo;
    Label1: TLabel;
    MemoResults: TMemo;
    ButtonEval: TButton;
    procedure ButtonEvalClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  JclSimpleXml,
  JclMsBuild;

procedure TFormMain.ButtonEvalClick(Sender: TObject);
  function GetVariableName(Index: Integer): string;
  begin
    Result := Format('var%d', [Index]);
  end;

  procedure AddCondition(Index: Integer; XmlPropertyGroup: TJclSimpleXmlElem; const Condition: string);
  var
    VariableName: string;
    XmlProperty: TJclSimpleXMLElem;
  begin
    VariableName := GetVariableName(Index);
    XmlPropertyGroup.Items.Add(VariableName, 'False');
    XmlProperty := XmlPropertyGroup.Items.Add(VariableName, 'True');
    XmlProperty.Properties.Add('Condition', Condition);
  end;

  procedure AddResult(Index: Integer; const Condition: string; Properties: TStrings);
  begin
    MemoResults.Lines.Add(Condition + '=' + Properties.Values[GetVariableName(Index)]);
  end;

var
  MsBuildParser: TJclMsBuildParser;
  Xml: TJclSimpleXml;
  XmlPropertyGroup: TJclSimpleXmlElem;
  Index: Integer;
begin
  MemoResults.Lines.Clear;

  Xml := TJclSimpleXML.Create;
  try
    Xml.Root.Name := 'Project';
    Xml.Root.Properties.Add('xmlns', 'http://schemas.microsoft.com/developer/msbuild/2003');
    XmlPropertyGroup := Xml.Root.Items.Add('PropertyGroup');
    for Index := 0 to MemoConditions.Lines.Count - 1 do
      AddCondition(Index, XmlPropertyGroup, MemoConditions.Lines.Strings[Index]);

    MsBuildParser := TJclMsBuildParser.Create('', Xml, False);
    try
      MsBuildParser.Init;
      MsBuildParser.Properties.GlobalProperties.Assign(MemoProperties.Lines);
      MsBuildParser.Parse;

      for Index := 0 to MemoConditions.Lines.Count - 1 do
        AddResult(Index, MemoConditions.Lines.Strings[Index], MsBuildParser.Properties);
    finally
      MsBuildParser.Free;
    end;
  finally
    Xml.Free;
  end;

  (*MsBuildParser := TJclMsBuildParser.Create('MyProject.dproj');
  try
    MsBuildParser.Init;
    MsBuildParser.Properties.GlobalProperties.Values['Configuration'] := 'Release';
    MsBuildParser.Parse;

    MemoResults.Lines.Assign(MsBuildParser.Properties);
  finally
    MsBuildParser.Free;
  end;*)
end;

end.
