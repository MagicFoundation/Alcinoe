{******************************************************************************}
{                        UNIFIED INTERBASE (UIB)                               }
{                                                                              }
{ Project JEDI Code Library (JCL)                                              }
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License Version  }
{ 1.0 (the "License"); you may not use this file except in compliance with the }
{ License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{                                                                              }
{ Unit owner:    Olivier GUILBAUD <golivier@free.fr>                           }
{ Last modified: Jun 17, 2003                                                  }
{                                                                              }
{------------------------------------------------------------------------------}
{ Sample for use the components TUIBDatabase,TUIBTransaction et TUIBQuery      }
{ and write the result in TStringGrid component.                               }
{******************************************************************************}
unit frm_selecttogrid;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, Grids, uib;

type
  TForm1 = class(TForm)
    btnOpen: TButton;
    edDataBase: TEdit;
    SG: TStringGrid;
    UIBBase: TUIBDataBase;
    Qry: TUIBQuery;
    UIBTrs: TUIBTransaction;
    Label1: TLabel;
    moSQL: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    btnConnect: TSpeedButton;
    procedure SpeedButton1Click(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  Try
    if UIBBase.Connected then
    begin
      btnConnect.Caption:='Connect';
      SG.Clear;
      SG.ColCount:=5;
      SG.RowCount:=5;
      SG.FixedCols:=1;
      SG.FixedRows:=1;
    end
    else
    begin
      btnConnect.Caption:='Disconnect';
      UIBBase.DatabaseName:=edDataBase.Text;
    end;

    UIBBase.Connected:=not UIBBase.Connected;
    BtnOPen.Enabled:=UIBBase.Connected;
  Except
    On E:Exception do
      ShowMessage(E.Message);
  end;
end;

procedure TForm1.btnOpenClick(Sender: TObject);
Var i,k : Integer;
begin
  try
   try
     Qry.Close;
     Qry.SQL.Text:=UpperCase(Trim(moSQL.Lines.Text));
     moSQL.Lines.Text:=Qry.SQL.Text;

     If Pos('SELECT',Qry.SQL.Text)=1 then
     begin
       Qry.Open;

       //Init
       SG.ColCount:=Qry.Fields.FieldCount+1;

       //Datas
       i:=0;
       while not Qry.Eof do
       begin
         //Header
         if i=0 then
         begin
           SG.ColWidths[0]:=25;
           for k:=0 to Qry.Fields.FieldCount-1 do
             SG.Cells[k+1,0]:=Qry.Fields.AliasName[k];
         end;


         Inc(i);
         If SG.RowCount-1<i then
             SG.RowCount:=i+1;

         SG.Cells[0,i]:=IntToStr(i);
         for k:=0 to Qry.Fields.FieldCount-1 do
           SG.Cells[k+1,i]:=Qry.Fields.AsString[k];

         Qry.Next;
       end;
     end;
   except
     on E:Exception do
       ShowMessage(E.Message);
   end;
 finally
    Qry.Close;
 end;
end;

initialization
  {$I frm_selecttogrid.lrs}

end.

