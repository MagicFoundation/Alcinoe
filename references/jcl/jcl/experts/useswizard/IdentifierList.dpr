{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is IdentifierList.dpr.                                                         }
{                                                                                                  }
{ The Initial Developer of the Original Code is Florent Ouchet <outchy att users dott sf dott net> }
{ Portions created by Florent Ouchet are Copyright (C) of Florent Ouchet.                          }
{                                                                                                  }
{ Contributors:                                                                                    }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

// note: this program converts the xml output of Doc-o-matic <http://www.doc-o-matic.com/>
// running on the JCL help files https://github.com/project-jedi/jcl/tree/master/help
// to text files for the JCL uses expert

program IdentifierList;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  JclSimpleXml;

var
  UnitList: TStringList;

procedure InitIdentifierList;
begin
  UnitList := TStringList.Create;
  UnitList.CaseSensitive := False;
end;

procedure FinalizeIdentifierList;
var
  Index: Integer;
  IdentifierList: TStringList;
begin
  for Index := 0 to UnitList.Count - 1 do
  begin
    IdentifierList := TStringList(UnitList.Objects[Index]);
    IdentifierList.SaveToFile(Format('%s.txt', [UnitList.Strings[Index]]));
    IdentifierList.Free;
  end;
  UnitList.Free;
end;

procedure AddIdentifier(const IdentifierName, UnitName: string);
var
  IdentifierList: TStringList;
  UnitIndex: Integer;
  Identifier: string;
begin
  if Pos('.', IdentifierName) > 0 then
    Exit;
  if Pos('@', IdentifierName) > 0 then
    Identifier := Copy(IdentifierName, 1, Pos('@', IdentifierName) - 1)
  else
    Identifier := IdentifierName;

  UnitIndex := UnitList.IndexOf(UnitName);
  if UnitIndex = -1 then
  begin
    IdentifierList := TStringList.Create;
    IdentifierList.CaseSensitive := False;
    UnitList.AddObject(UnitName, IdentifierList);
  end
  else
    IdentifierList := TStringList(UnitList.Objects[UnitIndex]);

  IdentifierList.Add(Identifier);
end;

procedure ProcessXML(const FileName: string);
var
  SimpleXML: TJclSimpleXML;

  procedure ProcessNode(const Node: TJclSimpleXMLElem);
  var
    IndexSection, IndexChild: Integer;
    SectionNode, ChildNode, LinkNode: TJclSimpleXMLElem;
    NameProp, IdProp: TJclSimpleXMLProp;
  begin
    if SameText(Node.Name, 'topic') then
    begin
      for IndexSection := 0 to Node.Items.Count - 1 do
      begin
        SectionNode := Node.Items.Item[IndexSection];
        NameProp := SectionNode.Properties.ItemNamed['name'];
        LinkNode := SectionNode.Items.ItemNamed['link'];
        if Assigned(NameProp) and SameText(SectionNode.Name, 'section')
          and SameText(NameProp.Value, 'Unit') then
        begin
          IdProp := Node.Properties.ItemNamed['id'];
          if Assigned(IdProp) then
          begin
            if Assigned(LinkNode) then
              AddIdentifier(IdProp.Value, LinkNode.Value)
            else
              AddIdentifier(IdProp.Value, SectionNode.Value);
          end;
        end;
      end;

    end;
    for IndexChild := 0 to Node.Items.Count - 1 do
    begin
      ChildNode := Node.Items.Item[IndexChild];
      ProcessNode(ChildNode);
    end;
  end;
begin
  SimpleXML := TJclSimpleXML.Create;
  try
    Write('Loading XML...');
    SimpleXML.LoadFromFile(FileName);
    WriteLn('done.');
    SimpleXML.Options := SimpleXML.Options - [sxoAutoCreate];
    Write('Processing XML...');
    ProcessNode(SimpleXML.Root);
    WriteLn('done.');
  finally
    SimpleXML.Free;
  end;
end;

begin
  Write('initializing lists...');
  InitIdentifierList;
  WriteLn('done.');
  try
    ProcessXML(place here the name of the xml generated by Doc-o-matic);
  finally
    Write('Saving lists...');
    FinalizeIdentifierList;
    WriteLn('done.');
  end;
end.
