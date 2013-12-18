unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uib, StdCtrls, uibmetadata;

type
  TForm1 = class(TForm)
    Button1: TButton;
    DataBase: TUIBDataBase;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Déclarations privées }
    procedure CheckTable(t: TMetaTable);
    procedure CheckTableField(f: TMetaTableField);
    procedure CheckForeign(f: TMetaForeign);
    procedure CheckIndex(t: TMetaTable);
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation
   
uses uiblib;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  meta: TMetaDataBase;
  tr: TUIBTransaction;
  i: integer;
begin
  meta := TMetaDataBase.Create(nil, -1);
  try
    tr := TUIBTransaction.Create(nil);
    try
      tr.DataBase := DataBase;
      meta.LoadFromDatabase(tr);
    finally
      tr.Free;
      DataBase.Connected := False;
    end;

    for i := 0 to meta.TablesCount - 1 do
      CheckTable(meta.Tables[i]);

    for i := 0 to meta.TablesCount - 1 do
      if meta.Tables[i].Data = nil then
        Memo1.Lines.Add(format('la table "%s" est isolée.', [meta.Tables[i].name]));
      
    for i := 0 to meta.TablesCount - 1 do
      CheckIndex(meta.Tables[i]);      
  finally
     meta.Free;
  end;
end;

procedure TForm1.CheckTable(t: TMetaTable);
var
  i: integer;
begin
  if t.Name[1] = '"' then
    memo1.Lines.Add(format('La table %s est quotée, cela n''est pas recommandé.', [t.name]));

  if t.PrimaryCount = 0 then
  begin
    if t.ForeignCount <= 1 then
      Memo1.Lines.Add(format('La table "%s" ne possède pas de clé primaire et n''est pas une table de liason.', [t.name]))
  end 
  else
    for i := 0 to t.Primary[0].FieldsCount - 1 do
      if t.Primary[0].Fields[i].FieldType <> uftInteger then
        Memo1.Lines.Add(format('La clé primaire "%s.%s" est de type "%s".', [t.name, t.Primary[0].Fields[i].name, t.Primary[0].Fields[i].AsDDLNode]));

  for i := 0 to t.FieldsCount - 1 do
    CheckTableField(t.Fields[i]);
  
  for i := 0 to t.ForeignCount - 1 do
    CheckForeign(t.Foreign[i]);
end;

procedure TForm1.CheckTableField(f: TMetaTableField);
begin
  if f.Name[1] = '"' then
    memo1.Lines.Add(format('Le champ %s.%s est quoté, cela n''est pas recommandé.', [f.Parent.Name, f.name]));
end;
   
procedure TForm1.CheckForeign(f: TMetaForeign);
var
  i: integer;
  notnull: boolean;
begin
  f.ForTable.Data := Pointer(Integer(f.ForTable.Data) + 1);
  f.Parent.Data := Pointer(Integer(f.Parent.Data) + 1);

  // vérifier l'intégrité des données en cas de suppression.
  notnull := false;
  for i := 0 to f.FieldsCount - 1 do
    if f.Fields[i].NotNull then
    begin
      notnull := true;
      Break;
    end;

  if notnull then
  begin
    if f.OnDelete <> urCascade then
      Memo1.Lines.Add(format('L''évenement ONDELETE sur la Foreign Key "%s" de la table "%s" devrait être du type "CASCADE".', [f.name, f.Parent.Name]))
  end 
  else
  begin
    if f.OnDelete <> urSetNull then
      if f.OnDelete = urCascade then
      begin
        for i := 0 to f.FieldsCount - 1 do
          if not f.Fields[i].NotNull then
            Memo1.Lines.Add(format('Le champ "%s.%s" devrait être "NOT NULL" puisque l''évenement ONDELETE de la foreign "%s" est "CASCADE".', [f.Parent.Name, f.Fields[i].name, f.name]));
      end 
      else
        Memo1.Lines.Add(format('L''évenement ONDELETE sur la Foreign Key "%s" de la table "%s" devrait être du type "SET NULL".', [f.name, f.Parent.Name]));
  end;

  // vérifier que les champs ont le meme type de données
  for i := 0 to f.FieldsCount - 1 do
    if f.Fields[i].FieldType <> f.ForFields[i].FieldType then
      Memo1.Lines.Add(
        format('Le champ "%s.%s" de type "%s" est lié au champ "%s.%s" qui est de type "%s".',
          [f.Parent.Name, f.Fields[i].Name, f.Fields[i].AsDDLNode,
          f.ForTable.Name, f.ForFields[i].Name, f.ForFields[i].AsDDLNode]));
end;  
  
procedure TForm1.CheckIndex(t: TMetaTable);
  
  function equal(a, b: TMetaConstraint): boolean;
  var
    i: Integer;
  begin
    Result := False;
    if a.FieldsCount <> b.FieldsCount then 
      Exit;
    for i := 0 to a.FieldsCount - 1 do
      if a.Fields[i] <> b.Fields[i] then
        Exit;
    Result := True;
  end;

var
  lst: TList;
  i, j: integer;
  idx1, idx2: TMetaConstraint;
begin
  lst := TList.Create;
  try
    for i := 0 to t.ForeignCount - 1 do 
      lst.Add(t.Foreign[i]);
    
    for i := 0 to t.IndicesCount - 1 do 
      lst.Add(t.Indices[i]);
    
    for i := 0 to t.PrimaryCount - 1 do 
      lst.Add(t.Primary[i]);
    
    for i := 0 to t.UniquesCount - 1 do 
      lst.Add(t.Uniques[i]);

    for i := 0 to t.IndicesCount - 1 do
    begin
      idx1 := t.Indices[i];
      for j := 0 to lst.Count - 1 do
      begin
        idx2 := TMetaConstraint(lst[j]);
        if (idx1 <> idx2) and equal(idx1, idx2) then
        begin
          Memo1.Lines.Add(Format('La table %s possede des indexes doublonnés.', [t.Name]));
          Exit;
        end;
      end;
    end;
  finally
    lst.Free;
  end;
end;
 
end.
