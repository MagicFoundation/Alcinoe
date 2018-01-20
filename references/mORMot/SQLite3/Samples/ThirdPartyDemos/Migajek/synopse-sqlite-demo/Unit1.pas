(*
  Simple Almost-Real-Life example of the Synopse mORMot Framework.
  Written by Michal Gajek, http://migajek.com/

  The demo doesn't depend on any 3rd party libraries nor components.

  ToDO:
    [ ] in each ListBox / ComboBox "Object", instead of ID, keep RecordRef ?
*)

unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, CheckLst, Grids,
  
  SynCommons, mORMotUI, mORMot,
  SynSQLite3Static, 

  uCustomer;

type
  TForm1 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    lbCustomers: TListBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    lblName: TLabel;
    lblSurname: TLabel;
    btnAddCustomer: TButton;
    lbTasks: TListBox;
    btnNewTask: TButton;
    cbCustomers: TComboBox;
    lbCustomerTasks: TListBox;
    Label3: TLabel;
    gbEditTask: TGroupBox;
    cbTaskPriority: TComboBox;
    CheckListBox1: TCheckListBox;
    TabSheet3: TTabSheet;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    TabSheet4: TTabSheet;
    dgTable: TDrawGrid;
    edtQuery: TComboBox;
    tbUsers: TTabSheet;
    Label8: TLabel;
    lbUsers: TListBox;
    GroupBox2: TGroupBox;
    clbRoles: TCheckListBox;
    btnAddUser: TButton;
    dtRoleExpires: TDateTimePicker;
    Label9: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnAddCustomerClick(Sender: TObject);
    procedure lbCustomersClick(Sender: TObject);
    procedure btnNewTaskClick(Sender: TObject);
    procedure cbCustomersClick(Sender: TObject);
    procedure lbTasksClick(Sender: TObject);
    procedure cbTaskPriorityChange(Sender: TObject);
    procedure CheckListBox1ClickCheck(Sender: TObject);
    procedure Label7Click(Sender: TObject);
    procedure edtQueryKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnAddUserClick(Sender: TObject);
    procedure lbUsersClick(Sender: TObject);
    procedure clbRolesClickCheck(Sender: TObject);
    procedure clbRolesClick(Sender: TObject);
    procedure dtRoleExpiresChange(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
  private
    { Private declarations }
    function LoadCustomer(const ACustomerID: integer): TSQLCustomer;
    procedure DisplayCustomerInfo(const ACustomer: TSQLCustomer); overload;
    procedure DisplayCustomerInfo(const ACustomerID: integer); overload;
    procedure FillCustomersList(const AList: TStrings; const AClear: boolean = true);

    function LoadTask(const ATaskID: integer): TSQLTask;
    procedure FillTasksList(const AList: TStrings; ATasks: TSQLTask);
    procedure LoadTasksForCustomer(const ACustomer: TSQLCustomer; const AList: TStrings);

    procedure LoadQueryHistory();

    procedure FillRolesList(const AList: TStrings);
    procedure FillUsersList(const AList: TStrings; const AClear: boolean = true);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses ShellApi, DateUtils, uQueryHistory, Math;

{$R *.dfm}

// loads customer data into "Details" box. Pass nil in order to clear the box.
procedure TForm1.DisplayCustomerInfo(const ACustomer: TSQLCustomer);
begin
  if ACustomer <> nil then
    begin
      lblName.Caption:= UTF8ToString(ACustomer.Firstname);
      lblSurname.Caption:= UTF8ToString(ACustomer.Surname);
      LoadTasksForCustomer(ACustomer, lbCustomerTasks.Items);
    end
  else
    begin
      lblName.Caption:= '';
      lblSurname.Caption:= '';
    end;
end;

procedure TForm1.DisplayCustomerInfo(const ACustomerID: integer);
var
  cust: TSQLCustomer;
begin
  cust:= LoadCustomer(ACustomerID);
  try
    DisplayCustomerInfo(cust);
  finally
    FreeAndNil(cust);
  end;
end;

function TForm1.LoadCustomer(const ACustomerID: integer): TSQLCustomer;
begin
   result:= TSQLCustomer.Create(globalClient, ACustomerID);
end;

// loads a list of customers to a AList
procedure TForm1.FillCustomersList(const AList: TStrings; const AClear: boolean = true);
var
 cust: TSQLCustomer;
begin
  // load all the customers
  try
    AList.BeginUpdate();
    if AClear then
      AList.Clear();
    cust:= TSQLCustomer.CreateAndFillPrepare(globalClient, '');
    while cust.FillOne do
      AList.AddObject(Format('%s, %s', [UTF8ToString(cust.Surname), UTF8ToString(cust.Firstname)]), Pointer(cust.ID)); // we keep integer ID as "Data" object

  finally
    AList.EndUpdate();
    FreeAndNil(cust);
  end;

end;

procedure TForm1.FillTasksList(const AList: TStrings; ATasks: TSQLTask);
var
  freeAfter: boolean;
begin
  freeAfter:= ATasks = nil;
  if freeAfter then
    ATasks:= TSQLTask.CreateAndFillPrepare(globalClient, '');

  try
    AList.BeginUpdate();
    AList.Clear();
    while ATasks.FillOne do
      AList.AddObject(Format('%s', [UTF8ToString(ATasks.Text)]), Pointer(ATasks.ID));
  finally
    AList.EndUpdate();
    if freeAfter then
      FreeAndNil(ATasks);
  end;
end;

procedure TForm1.LoadTasksForCustomer(const ACustomer: TSQLCustomer; const AList: TStrings);
var
 task: TSQLTask;
 fIds: TIDDynArray;
begin
  ACustomer.Tasks.DestGet(globalClient, ACustomer.ID, fIds);
  task:= TSQLTask.CreateAndFillPrepare(globalClient, TInt64DynArray(fIds));
  AList.BeginUpdate();
  AList.Clear();
  try
    while task.FillOne do
      AList.AddObject(Format('%s', [UTF8ToString(task.Text)]), Pointer(task.id));
  finally
    AList.EndUpdate();
    FreeAndNil(task);
  end;
end;

function TForm1.LoadTask(const ATaskID: integer): TSQLTask;
begin
  result:= TSQLTask.Create(globalClient, ATaskID);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // clear all the fields.
  DisplayCustomerInfo(nil);
  FillCustomersList(lbCustomers.Items);
  cbCustomers.AddItem('<Any customer>', nil);
  cbCustomers.ItemIndex:= 0;
  FillCustomersList(cbCustomers.Items, false);
  cbCustomersClick(nil);

  LoadQueryHistory();

  FillRolesList(clbRoles.Items);
  FillUsersList(lbUsers.Items);

  dtRoleExpires.DateTime:= IncMonth(Now(), 1);
end;

procedure TForm1.btnAddCustomerClick(Sender: TObject);
var
  cust: TSQLCustomer;
begin

  cust:= TSQLCustomer.Create;
  try
    cust.Firstname:= StringToUTF8(InputBox('First name', 'Customer first name', 'John'));
    cust.Surname:= StringToUTF8(InputBox('Surname', 'Customer surname', 'Doe'));
    if (cust.Firstname <> '') and (cust.Surname <> '') then
      globalClient.Add(cust, true);
  finally
    FreeAndNil(cust);
    FillCustomersList(lbCustomers.Items);
  end;

end;

procedure TForm1.lbCustomersClick(Sender: TObject);
var
  cust: TSQLCustomer;
begin
  if lbCustomers.ItemIndex <> -1 then
    begin
      // since we store record ID as object, now we can load it
      if lbCustomers.Items.Objects[lbCustomers.ItemIndex] <> nil then
        begin
          cust:= LoadCustomer( Integer(lbCustomers.Items.Objects[lbCustomers.ItemIndex]) );
          DisplayCustomerInfo(cust);
          FreeAndNil(cust);
        end
    end
  else
    DisplayCustomerInfo(nil);

end;

procedure TForm1.btnNewTaskClick(Sender: TObject);
var
  task: TSQLTask;
begin

  task:= TSQLTask.Create();
  try
    task.Text:= StringToUTF8(InputBox('Text', 'Task description', ''));
    task.Priority:= tpNormal;
    if (task.Text <> '') then
      globalClient.Add(task, true);
  finally
    FreeAndNil(task);
    FillTasksList(lbTasks.Items, nil);
  end;

end;

procedure TForm1.cbCustomersClick(Sender: TObject);
var
  cust: TSQLCustomer;
begin
  gbEditTask.Visible:= false;
  if cbCustomers.ItemIndex <> -1 then
    begin
      // since we store record ID as object, now we can load it
      if cbCustomers.Items.Objects[cbCustomers.ItemIndex] <> nil then
        begin
          cust:= LoadCustomer( Integer(cbCustomers.Items.Objects[cbCustomers.ItemIndex]) );
          LoadTasksForCustomer(cust, lbTasks.Items);
          FreeAndNil(cust);
        end
      else
        FillTasksList(lbTasks.Items, nil);
    end
  else
    FillTasksList(lbTasks.Items, nil);
end;

procedure TForm1.lbTasksClick(Sender: TObject);
var
  task: TSQLTask;
  cust: TSQLCustomer;
  clientsIds: TIDDynArray;
  i, j: integer;
begin
  gbEditTask.Visible:= lbTasks.ItemIndex <> -1;
  if not gbEditTask.Visible then
    exit;

  gbEditTask.Visible:= lbTasks.Items.Objects[lbTasks.ItemIndex] <> nil;
  if not gbEditTask.Visible then
    exit;

  task:= LoadTask(integer(lbTasks.Items.Objects[lbTasks.ItemIndex]));
  cbTaskPriority.ItemIndex:= Ord(task.Priority);
  cbTaskPriority.Tag:= task.ID;

  FillCustomersList(CheckListBox1.Items, true);
  CheckListBox1.Tag:= task.ID;
  // load list of customers assigned to the given task
  cust:= TSQLCustomer.Create();
  try
    cust.Tasks.SourceGet(globalClient, task.ID, clientsIds);
    for i:= low(clientsIds) to high(clientsIds) do
      begin

        // find the client on the list (by ID)
        for j:= 0 to CheckListBox1.Count -1 do
          if Integer(CheckListBox1.Items.Objects[j]) = clientsIds[i] then
              CheckListBox1.Checked[j]:= true;
      end;
  finally
    cust.Free();
    FreeAndNil(task);
  end;
end;

procedure TForm1.cbTaskPriorityChange(Sender: TObject);
var
 task: TSQLTask;
begin
  if cbTaskPriority.Tag > 0 then
    begin
      task:= LoadTask(cbTaskPriority.Tag);
      if task <> nil then
        begin
          task.Priority:= TSQLTaskPriority(cbTaskPriority.ItemIndex);
          globalClient.Update(task);
          task.Free;
        end;
    end
end;

procedure TForm1.CheckListBox1ClickCheck(Sender: TObject);
var
 task: TSQLTask;
 cust: TSQLCustomer;
begin
  // first, load the Task based on ID (stored in TAG property)
  if (sender as TComponent).Tag > 0 then
    begin
      task:= LoadTask((sender as TComponent).Tag);
      if task <> nil then
        begin

          // now load the customer from the list
          if CheckListBox1.ItemIndex > - 1 then
            begin
              cust:= LoadCustomer(Integer(CheckListBox1.Items.Objects[CheckListBox1.ItemIndex]));
              if cust <> nil then
                begin
                  if CheckListBox1.Checked[CheckListBox1.ItemIndex] then
                    cust.Tasks.ManyAdd(globalClient, cust.ID, task.ID, true)
                  else
                    cust.Tasks.ManyDelete(globalClient, cust.ID, task.ID);

                  FreeAndNil(cust);
                end;
            end;
          FreeAndNil(task);
        end;
    end

end;

procedure TForm1.Label7Click(Sender: TObject);
begin
  ShellExecute(0, 'open', 'http://code.google.com/p/synopse-sqlite-demo/', '', '',  SW_SHOWNORMAL);
end;

procedure TForm1.edtQueryKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
 data: TSQLTable;
 hist: TSQLQueryHistory;
begin
  if Key = VK_RETURN then
  begin
    Key:= 0;

    // we don't have to worry for freeing the data nor the "previously" created instance of TSQLTableToGrid
    // as the Framework takes care of everything.
    data:= globalClient.ExecuteList([], StringToUTF8(edtQuery.Text));
    TSQLTableToGrid.Create(dgTable, data, globalClient);

    hist:= TSQLQueryHistory.Create(globalClient, 'SQL = ?', [edtQuery.Text]);
    try
      if hist.ID = 0 then
        hist.SQL:= StringToUTF8(edtQuery.Text);
      hist.LastUsed:= Now();
      if hist.ID > 0 then
        globalClient.Update(hist)
      else
        globalClient.Add(hist, true);
    finally
      FreeAndNil(hist);
      LoadQueryHistory();
    end;

  end;
end;

procedure TForm1.LoadQueryHistory();
var
 hist: TSQLQueryHistory;
begin
  hist:= TSQLQueryHistory.Create();
  edtQuery.Items.BeginUpdate();
  edtQuery.Items.Clear();
  try
    hist.FillHistory();
    while hist.FillOne do
      edtQuery.AddItem(UTF8ToString(hist.SQL), nil);
  finally
    edtQuery.Items.EndUpdate();
    FreeAndNil(hist);
  end;
end;

procedure TForm1.FillRolesList(const AList: TStrings);
var
  role: TSQLUserRole;
begin
  role:= TSQLUserRole.CreateAndFillPrepare(globalClient, '');
  AList.BeginUpdate();
  AList.Clear();
  try
    while role.FillOne do
      AList.AddObject(UTF8ToString(role.RoleName), Pointer(role.id));
  finally
    FreeAndNil(role);
    AList.EndUpdate();
  end;
end;

procedure TForm1.FillUsersList(const AList: TStrings; const AClear: boolean = true);
var
 user: TSQLUser;
begin
  // load all the customers
  try
    AList.BeginUpdate();
    if AClear then
      AList.Clear();
    user:= TSQLUser.CreateAndFillPrepare(globalClient, '');
    while user.FillOne do
      AList.AddObject(Format('%s (%s, %s)', [UTF8ToString(user.login), UTF8ToString(user.Surname), UTF8ToString(user.Firstname)]), Pointer(user.ID)); // we keep integer ID as "Data" object

  finally
    AList.EndUpdate();
    FreeAndNil(user);
  end;

end;


procedure TForm1.btnAddUserClick(Sender: TObject);
var
  user: TSQLUser;
begin

  user:= TSQLUser.Create;
  try
    user.Firstname:= StringToUTF8( InputBox('First name', 'User first name', '') );
    user.Surname:= StringToUTF8( InputBox('Surname', 'User surname', '') );
    user.Login:= StringToUTF8( InputBox('Login', 'User login', '') );
    user.Password:= StringToUTF8( InputBox('Password', 'User password', '') );
    if (user.Firstname <> '') and (user.Surname <> '') and (user.Password <> '') and (user.Login <> '') then
      globalClient.Add(user, true)
    else
      MessageBox(handle, 'None of the data can be empty!', 'Wrong', MB_ICONEXCLAMATION);
  finally
    FreeAndNil(user);
    FillUsersList(lbUsers.Items);
  end;


end;

procedure TForm1.lbUsersClick(Sender: TObject);
var
  i, rowID: integer;
  user: TSQLUser;
  roles: TSQLUserRoles;
  date: TTimeLog;
begin
  clbRoles.Enabled:= (lbUsers.ItemIndex > -1) and (lbUsers.Items.Objects[lbUsers.ItemIndex] <> nil);
  FillRolesList(clbRoles.Items);  // clear names, dates and selections

  if clbRoles.Enabled then
  begin
    user:= TSQLUser.Create(globalClient, integer(lbUsers.Items.Objects[lbUsers.ItemIndex]));

    try
      for i:= 0 to clbRoles.Items.Count -1 do
        if user.HasRole('', integer(clbRoles.Items.Objects[i]), rowID) then
          begin
            clbRoles.Checked[i]:= true;
            try
              roles:= TSQLUserRoles.Create();
              // load the actual data (ValidUntil field)
              globalClient.Retrieve(rowID, roles);
              date := roles.ValidUntil;
              clbRoles.Items.Strings[i]:= clbRoles.Items.Strings[i] + Format(' [valid until %s]',
                [TTimeLogBits(date).Text(true)]);
            finally
              FreeAndNil(roles);
            end;
          end;
    finally
      FreeAndNil(user);
    end;
  end
end;

procedure TForm1.clbRolesClickCheck(Sender: TObject);
var
  user: TSQLUser;
  roleID: integer;
begin
  if clbRoles.ItemIndex = -1 then
    exit;

  user:= TSQLUser.Create(globalClient, integer(lbUsers.Items.Objects[lbUsers.ItemIndex]));

  try
    roleID:= Integer(clbRoles.Items.Objects[clbRoles.ItemIndex]);
    user.Roles.ValidUntil:= TimeLogFromDateTime( IncMonth(Now(), 1) );
    if user.HasRole('', roleID) then
      user.Roles.ManyDelete(globalClient, user.ID, roleID)
    else
      user.Roles.ManyAdd(globalClient, user.ID, roleID, true);
  finally
    FreeAndNil(user);
  end;

end;

procedure TForm1.clbRolesClick(Sender: TObject);
var
  roles: TSQLUserRoles;
  roleID: integer;
begin
  if clbRoles.ItemIndex = -1 then
    exit;

  roleID:= integer(clbRoles.Items.Objects[clbRoles.ItemIndex]);
  roles:= TSQLUserRoles.Create();
  try
    //globalClient.Retrieve(rolesID, roles);
    roles.ManySelect(globalClient, integer(lbUsers.Items.Objects[lbUsers.ItemIndex]),roleID);
    if roles.id <> 0 then
      dtRoleExpires.DateTime:= TTimeLogBits(roles.ValidUntil).ToDate;
  finally
    FreeAndNil(roles);
  end;
end;

procedure TForm1.dtRoleExpiresChange(Sender: TObject);
var
  roles: TSQLUserRoles;
  roleID: integer;
begin
  if clbRoles.ItemIndex = -1 then
    exit;

  roleID:= integer(clbRoles.Items.Objects[clbRoles.ItemIndex]);
  roles:= TSQLUserRoles.Create();
  try
    // select the record which associates currently selected user with currently selected role
    roles.ManySelect(globalClient, integer(lbUsers.Items.Objects[lbUsers.ItemIndex]), roleID);
    if roles.id <> 0 then
      begin
        roles.ValidUntil:= TimeLogFromDateTime(dtRoleExpires.DateTime);
        globalClient.Update(roles)
      end;
  finally
    FreeAndNil(roles);
  end;

end;

procedure TForm1.PageControl1Change(Sender: TObject);
begin
if PageControl1.ActivePage = tbUsers then
  if currentUser <> nil then
  if not currentUser.HasRole('admin') then
    begin
      PageControl1.ActivePage:= TabSheet3;
      MessageBox(handle, 'You are not allowed to edit this content', 'Please sign-in as admin!', MB_ICONEXCLAMATION)
    end;
end;

end.
