This unit allow you to generate object class and list to changes sql result

Exeample of use:

var
  codegen: TUIBCodeGenerator;
begin
  codegen := TUIBCodeGenerator.Create('test', transaction);
  try
    codegen.AddStatement('TESTTABLE', 'select * from TESTTABLE', [],
     'INSERT INTO TESTTABLE (REFTESTTABLE, FINT, FSTR, FBLOB) VALUES (GEN_ID(GEN_TESTTABLE_ID, 1), :FINT, FSTR, :FBLOB) RETURNING REFTESTTABLE;',
     'UPDATE TESTTABLE SET FINT = :FINT, FSTR = :FSTR, FBLOB = :FBLOB WHERE (REFTESTTABLE = :REFTESTTABLE);',
     'DELETE FROM TESTTABLE WHERE (REFTESTTABLE = :REFTESTTABLE);'
    );
    codegen.SaveToFile('test.pas');
  finally
    codegen.Free;
  end;
end;