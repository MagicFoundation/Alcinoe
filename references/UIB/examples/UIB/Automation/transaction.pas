unit transaction;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, UIB_TLB, StdVcl, uib, InternalItf;

type

  TOleTransaction = class(TUIBTransaction)
  private
    FDatabase: IDatabase;
  protected
    procedure SetDataBase(const Database: TUIBDataBase); override;
  end;

  TTransaction = class(TAutoObject, ITransaction, IDataPointer)
  private
    FTransaction: TOleTransaction;
  protected
    function Data: pointer; stdcall;
    function Get_Database: IDatabase; safecall;
    procedure Set_Database(const Value: IDatabase); safecall;
    procedure Commit; safecall;
    function Get_InTransaction: WordBool; safecall;
    procedure CommitRetaining; safecall;
    procedure RollBack; safecall;
    procedure RollBackRetaining; safecall;

  public
    procedure Initialize; override;
    destructor Destroy; override;
  end;

implementation

uses ComServ;

{ TOleTransaction }

procedure TOleTransaction.SetDataBase(const Database: TUIBDataBase);
begin
  inherited;
  if (DataBase = nil) then FDatabase := nil; // if database disconnect transaction
end;

{ TTransaction }

destructor TTransaction.Destroy;
begin
  FTransaction.Free;
  inherited;
end;

procedure TTransaction.Initialize;
begin
  inherited;
  FTransaction := TOleTransaction.Create(nil);
end;

function TTransaction.Get_Database: IDatabase;
begin
  Result := FTransaction.FDatabase;
end;

procedure TTransaction.Set_Database(const Value: IDatabase);
begin
  if (Value = nil) then
    FTransaction.SetDataBase(nil) else
    FTransaction.SetDataBase((value as IDataPointer).Data);
end;

function TTransaction.Data: pointer;
begin
  Result := FTransaction;
end;

procedure TTransaction.Commit;
begin
  FTransaction.Commit;
end;

function TTransaction.Get_InTransaction: WordBool;
begin
  Result := FTransaction.InTransaction;
end;

procedure TTransaction.CommitRetaining;
begin
  FTransaction.CommitRetaining;
end;

procedure TTransaction.RollBack;
begin
  FTransaction.RollBack;
end;

procedure TTransaction.RollBackRetaining;
begin
  FTransaction.RollBackRetaining;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TTransaction, Class_Transaction,
    ciMultiInstance, tmApartment);
end.
