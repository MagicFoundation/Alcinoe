unit tryexcept;

interface

implementation

procedure DoStuff;
var
  O: MyUnit.TMyObject;
begin
  try
   DoSomething;
  except
    Log('DoSomething failed');
  end; 

  try
    DoSomethingElse
  except
    on E: Exception do
    begin
      LogError(E);
    end;
  end;
  
  try
    DoCrazyStuff;
  except
  
    on EFileNotFound do
    begin
      LogHint('File not found. Does''nt matter.');
    end;  
    
    on MyUnit.EFileNotFound do
    begin
      LogHint('MyUnit file not found. Does''nt matter.');
    end;  
    
    on E: MyUnit.ECriticalError do
    begin
      LogError('MyUnit Critical error: ' + E.Message);
    end;        
  
    on E: Exception do
    begin
      LogError(E);
    end
    else
      LogError('Unknown error');
  end;              
end;

end.
