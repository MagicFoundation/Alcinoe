{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{ DUnit Test Unit                                                                                  }
{                                                                                                  }
{ Covers:      JclContainer Stuff                                                                  }
{ Last Update: 11-Mar-2005                                                                         }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{**************************************************************************************************}

unit TestJclContainer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, JclContainerIntf, JclLinkedLists, JclVectors, JclArrayLists, JclAlgorithms, TestFramework, GUITestRunner;

type
  TJclStrListImplType = (litArray, litLinkedList, litVector);

  TJclContainerTest = class(TTestCase)
  private
    function GetStrList(ImplentationType: TJclStrListImplType = litLinkedList): IJclStrList;
    procedure DoDelimTest(I: IJclStrList);
    procedure InsertAll(x, y, SavedX: IJclStrList);
  published
    procedure IJclStrList_AppendDelim_with_LinkedList;
    procedure IJclStrList_AppendDelim_with_ArrayList;
    procedure IJclStrList_InsertAll_with_LinkedList;
    procedure IJclStrList_InsertAll_with_ArrayList;
    procedure IJclStrList_InsertAll_with_VectorList;
  end;

implementation

//==================================================================================================
// TJclContainerTest
//==================================================================================================

procedure TJclContainerTest.DoDelimTest(I: IJclStrList);
var
  sl: TStringList;
  x: integer;
  s: string;
begin
  sl := TStringList.Create;
  try
    sl.Add('Frank');
    sl.Add('Jack');
    sl.Add('Tim');
    sl.Add('Andrew');
    sl.Add('dade2004');
    sl.Add('Ford');

    I.AppendDelimited(sl.Text);

    for x := 0 to sl.Count - 1 do
      CheckEquals(sl[x], I.Strings[x]);

    s := sl.Text;
    sl.Text := I.GetAsDelimited;
    CheckEquals(sl.Text, s);
  finally
    sl.free;
  end;
end;

procedure TJclContainerTest.IJclStrList_AppendDelim_with_ArrayList;
begin
  DoDelimTest(GetStrList(litArray));
end;

procedure TJclContainerTest.IJclStrList_AppendDelim_with_LinkedList;
begin
  DoDelimTest(GetStrList);
end;

function TJclContainerTest.GetStrList;
begin
  case ImplentationType of
    litArray: Result := TJclStrArrayList.Create;
    litLinkedList: Result := TJclStrLinkedList.Create(nil);
    litVector: Result := TJclStrVector.Create(500);
  end;
end;

procedure TJclContainerTest.InsertAll(x, y, SavedX: IJclStrList);
var
  i: byte;
  c: cardinal;
const
  StartChar = 'A';
  EndChar = 'Z';
  Count = 1000;
begin
  for c := 1 to count do
  begin
    for i := ord(StartChar) to ord(EndChar) do
      x.Add(chr(i));
    for i := ord(StartChar) to ord(EndChar) do
      y.Add(chr(i));
    for i := ord(StartChar) to ord(EndChar) do
      SavedX.Add(chr(i));
  end;

  for c := 0 to Pred(count div 50) do
  begin
    CheckEquals(Count * (ord(EndChar) - ord(StartChar) + 1), x.size, 'x.size[' + IntToStr(c) + ']');
    CheckEquals(Count * (ord(EndChar) - ord(StartChar) + 1), y.size, 'y.size[' + IntToStr(c) + ']');
    CheckEquals(True, x.InsertAll(c, y), 'x.InsertAll(' + IntToStr(c) + ', y)');
    for i := ord(StartChar) to ord(EndChar) do
      CheckEquals(
        Count * 2,
        CountObject(x.First, x.Size, chr(i), StrSimpleCompare),
        'Count char #' + IntToStr(i) + ', c=' + IntToStr(c)
      );
    x.Clear;
    x.AddAll(SavedX);
  end;
end;

procedure TJclContainerTest.IJclStrList_InsertAll_with_ArrayList;
begin
  InsertAll(getStrlist(litArray), getStrlist(litArray), getStrlist(litArray));
end;

procedure TJclContainerTest.IJclStrList_InsertAll_with_LinkedList;
begin
  InsertAll(getStrlist, getstrlist, getstrlist);
end;

procedure TJclContainerTest.IJclStrList_InsertAll_with_VectorList;
begin
  InsertAll(getStrlist(litVector), getstrlist(litVector), getstrlist(litVector));
end;

initialization
  RegisterTest('JclContainers', TJclContainerTest.Suite);
end.

