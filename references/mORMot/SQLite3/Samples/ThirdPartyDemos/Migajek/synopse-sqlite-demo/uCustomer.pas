unit uCustomer;

interface

uses
  SynCommons,
  mORMot;

type
  TSQLTasks = class;

  TSQLPerson = class(TSQLRecord)
  private
    fFirstname, fSurname: RawUTF8;
  published
    property FirstName: RawUTF8 read fFirstname write fFirstname;
    property Surname: RawUTF8 read fSurname write fSurname;
  end;

  TSQLCustomer = class(TSQLPerson)
  private
    fTasks: TSQLTasks;
  published
    property Tasks: TSQLTasks read fTasks;
  end;

  TSQLTaskPriority = (tpLow, tpNormal, tpHigh);
  TSQLTask = class(TSQLRecord)
  private
    fPriority: TSQLTaskPriority;
    fText: RawUTF8;
  published
    property Priority: TSQLTaskPriority read fPriority write fPriority;
    property Text: RawUTF8 read fText write fText;
  end;

  TSQLTasks = class(TSQLRecordMany)
  private
    fSource: TSQLCustomer;
    fDest: TSQLTask;
  published
    property Source: TSQLCustomer read fSource;
    property Dest: TSQLTask read fDest;
  end;

  TSQLUser = class;
  TSQLUserRole = class(TSQLRecord)
  private
    fRoleName: RawUTF8;
  public
    // create standard roles: admin & user
    class procedure CreateStandardRoles(const ADatabase: TSQLRest);
  published
    property RoleName: RawUTF8 read fRoleName write fRoleName;
  end;

  TSQLUserRoles = class(TSQLRecordMany)
  private
    fValidUntil: TTimeLog;
    fSource: TSQLUser;
    fDest: TSQLUserRole;
  published
    property ValidUntil: TTimeLog read fValidUntil write fValidUntil;
    property Dest: TSQLUserRole read fDest;
    property Source: TSQLUser read fSource;
  end;

  // user of our system
  TSQLUser = class(TSQLPerson)
  private
    fRoles: TSQLUserRoles;
    fLogin, fPassword: RawUTF8;
    procedure SetPassword(const APwd: RawUTF8);
  public
    function HasRole(const ARoleName: RawUTF8; const ARoleID: integer = -1): boolean; overload;
    // returned aRowID is an ID of row in PIVOT TABLE !!!
    // so that we can access additional data stored in pivot connection
    function HasRole(const ARoleName: RawUTF8; const ARoleID: integer; var ARowID: integer): boolean; overload;
    // returns 0 on fail, UserID on success
    class function SignIn(const ALogin, APassword: RawUTF8): Integer;
  published
    property Roles: TSQLUserRoles read fRoles write fRoles;
    property Login: RawUTF8 read fLogin write fLogin;
    property Password: RawUTF8 read fPassword write SetPassword;
  end;

function CreateSampleModel: TSQLModel;
procedure InitClient();

var
  globalClient: TSQLRestClientURI;
  currentUser: TSQLUser; 
  model: TSQLModel;

implementation

uses SysUtils, uQueryHistory, SynCrypto, mORMotSQLite3, Forms;


procedure InitClient();
begin
  Model:= CreateSampleModel;
  globalClient:= TSQLRestClientDB.Create(Model, CreateSampleModel, ChangeFileExt(Application.ExeName,'.db3'), TSQLRestServerDB);
  TSQLRestClientDB(globalClient).Server.CreateMissingTables;
end;

function CreateSampleModel: TSQLModel;
begin
  result := TSQLModel.Create([TSQLCustomer, TSQLTask, TSQLTasks, TSQLQueryHistory, TSQLUser, TSQLUserRole, TSQLUserRoles]);
end;



class procedure TSQLUserRole.CreateStandardRoles(const ADatabase: TSQLRest);
const
  names: array [0..1] of RawUTF8 = ( 'user', 'admin' );
var
  i: integer;
  role: TSQLUserRole;
begin
  for i:= low(names) to high(names) do
    begin
      role:= TSQLUserRole.Create(ADatabase, 'RoleName = ?', [names[i]]);
      try
        // if the role isn't present yet
        if role.ID = 0 then
          begin
            role.RoleName:= names[i];
            ADatabase.Add(role, true);
          end
      finally
        FreeAndNil(role);
      end;
    end;
end;


procedure TSQLUser.SetPassword(const APwd: RawUTF8);
begin
  fPassword:= SynCrypto.MD5(APwd);
end;


class function TSQLUser.SignIn(const ALogin, APassword: RawUTF8): Integer;
var
  usr: TSQLUser;
begin
  result:= 0;
  usr:= TSQLUser.Create(globalClient, '(User.Login = ? AND User.Password = ?)', [ALogin, SynCrypto.MD5(APassword)]);
  try
    result:= usr.ID;
    // check if is not expired
    if not usr.HasRole('user') then
      result:= 0
    else
      currentUser:= usr;
  finally
    if result = 0 then  // if no valid user found, free it. otherwise we'll keep it until application end.
      FreeAndNil(usr);
  end;
end;

function TSQLUser.HasRole(const ARoleName: RawUTF8; const ARoleID: integer = -1): boolean;
var
  dummy: integer;
begin
  result:= HasRole(ARoleName, ARoleID, dummy);
end;



function TSQLUser.HasRole(const ARoleName: RawUTF8; const ARoleID: integer; var ARowID: integer): boolean;
var
  role: TSQLUserRole;
begin
  result:= false;
  ARowID:= -1;
  // we want the RowID anyway, so skip that part: 'UserRoles.ValidUntil <= date(''now'')'
  fRoles.FillMany(globalClient, fID);

  // loop through the roles
  while fRoles.FillOne do
    begin
      if fRoles.Dest <> nil then
        try
          role:= TSQLUserRole.Create(globalClient, fRoles.Dest.ID);
          if ((ARoleID = -1) and (role.RoleName = ARoleName)) or ((ARoleID > -1) and (role.ID = ARoleID) ) then
            begin
              // if using "admin" account, role validity doesn't expire.
              if fLogin <> 'admin' then
                result:= fRoles.ValidUntil >= TimeLogNow()
              else
                result:= true;
              ARowID:= fRoles.ID;
              break;
            end;
        finally
          FreeAndNil(role);
        end;
    end;
end;

initialization
  currentUser:= nil;
finalization
  FreeAndNil(globalClient);
  FreeAndNil(model);
  FreeAndNil(currentUser);

end.
