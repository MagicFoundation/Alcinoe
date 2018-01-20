unit RestServerFormUnit;

// mORMot RESTful API test case 1.02

interface

uses
  // RTL
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  {$ifdef FPC}
  LCLType,
  {$endif}
  Messages,
  SysUtils,
  Classes,
  Generics.Collections,
  Forms,
  Dialogs,
  Controls,
  StdCtrls,
  ExtCtrls,
  // mORMot
  mORMot,
  mORMotHttpServer,
  SynLog,
  SynCommons,
  // Custom
  RestServerUnit, ComCtrls;

type
  lServerAction = (Auto, Start, Stop, Restart);

  TForm1 = class(TForm)
    EditPort: TEdit;
    LabelPortCap: TLabel;
    ButtonStartStop: TButton;
    MemoLog: TMemo;
    ButtonCLS: TButton;
    TimerRefreshLogMemo: TTimer;
    CheckBoxAutoScroll: TCheckBox;
    LabelAuthenticationMode: TLabel;
    ComboBoxAuthentication: TComboBox;
    ButtonShowAuthorizationInfo: TButton;
    CheckBoxDisableLog: TCheckBox;
    LabelProtocol: TLabel;
    ComboBoxProtocol: TComboBox;
    ListViewMethodGroups: TListView;
    GroupBoxMethodGroupConfiguration: TGroupBox;
    RadioGroupAuthorizationPolicy: TRadioGroup;
    ButtonSaveRoleConfiguration: TButton;
    EditAllowGroupNames: TEdit;
    EditDenyAllowGroupNames: TEdit;
    GroupBoxUsers: TGroupBox;
    ListViewUsers: TListView;
    EditUserGroup: TEdit;
    ButtonSaveUsers: TButton;
    EditUserName: TEdit;
    ButtonDeleteUser: TButton;
    EditUserPassword: TEdit;
    LabelHTTPSnote: TLabel;
    procedure ButtonStartStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonCLSClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerRefreshLogMemoTimer(Sender: TObject);
    procedure ComboBoxAuthenticationChange(Sender: TObject);
    procedure ButtonShowAuthorizationInfoClick(Sender: TObject);
    procedure CheckBoxDisableLogClick(Sender: TObject);
    procedure ComboBoxProtocolChange(Sender: TObject);
    procedure ListViewMethodGroupsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure ListViewUsersSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure ButtonSaveRoleConfigurationClick(Sender: TObject);
    procedure ButtonSaveUsersClick(Sender: TObject);
    procedure RadioGroupAuthorizationPolicyClick(Sender: TObject);
    procedure ButtonDeleteUserClick(Sender: TObject);
    procedure ListViewUsersClick(Sender: TObject);
    procedure ListViewMethodGroupsClick(Sender: TObject);
  private
    function LogEvent(Sender: TTextWriter; Level: TSynLogInfo; const Text: RawUTF8): Boolean;
    function GetAuthModeDescription(AM: lAuthenticationMode): string;
    function FillMethodAuthorizationRulesFromUI(RestServerSettings: TRestServerSettings): Boolean;
    procedure StartStopServer(ServerAction: lServerAction = Auto);

  public
    { Public declarations }
  end;

  TLocalLog = class
    Level: TSynLogInfo;
    Text: RawUTF8;
  end;

var
  Form1: TForm1;
  {$ifdef FPC}
  LogThreadSafeList: TList<TLocalLog>;
  {$else}
  LogThreadSafeList: TThreadList<TLocalLog>;
  {$endif}

implementation

{$ifdef FPC}
{$R *.lfm}
{$else}
{$R *.dfm}
{$endif}

{ TForm1 }

// On Form1 create
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Create thread safe List with log data class
  {$ifdef FPC}
  LogThreadSafeList := TList<TLocalLog>.Create();
  {$else}
  LogThreadSafeList := TThreadList<TLocalLog>.Create();
  {$endif}
  // Enable logging
  with TSQLLog.Family do
  begin
    Level := LOG_VERBOSE;
    EchoCustom := LogEvent;
    NoFile := True;
  end;
  {$IFNDEF MSWINDOWS}
  // no named pipes on non-windows
  ComboBoxProtocol.Items.Delete(ComboBoxProtocol.Items.Count-1);
  // on Linux, ports below 1000 are priviledged (root only)
  EditPort.Text:='7777';
  {$ENDIF}
end;

// On Form1 destory
procedure TForm1.FormDestroy(Sender: TObject);
var
  i: integer;
  List: TList<TLocalLog>;
begin
  // Clear and destroy LogThreadSafeList
  {$ifdef FPC}
  List := LogThreadSafeList;
  {$else}
  List := LogThreadSafeList.LockList;
  {$endif}
  for i := 0 to List.Count - 1 do
    List.Items[i].Free;
  List.Clear;
  {$ifndef FPC}
  LogThreadSafeList.UnlockList();
  {$endif}
  FreeAndNil(LogThreadSafeList);
end;

{ SERVER EVENTS, START / STOP }

// Fill RestServerSettings object with groups, users and method authorization settings
function TForm1.FillMethodAuthorizationRulesFromUI(RestServerSettings: TRestServerSettings): Boolean;
var
  i, j, l: integer;
  AddedGroups, GroupsToAdd: TStringList;
  AuthGroup: rAuthGroup;
  AuthUser: rAuthUser;
  MethodAuthorizationSettings: rMethodAuthorizationSettings;
  SQLAccessRights: TSQLAccessRights;
  GroupName, MethodName, UserName: string;
begin
  Result := True;
  // Prepare temp objects
  AddedGroups := TStringList.Create();
  GroupsToAdd := TStringList.Create();
  GroupsToAdd.LineBreak := ',';
  GroupsToAdd.Duplicates := dupIgnore;
  // For REST service, only one rule required = reService, will be applyed to all groups
  SQLAccessRights.AllowRemoteExecute := [reService];
  // Fill group names from columns "Allow" and "Deny"
  for i := 0 to ListViewMethodGroups.Items.Count - 1 do
    GroupsToAdd.Text := GroupsToAdd.Text + ListViewMethodGroups.Items[i].SubItems.Strings[0] + ',' + ListViewMethodGroups.Items[i].SubItems.Strings[1];
  // Add groups
  for i := 0 to GroupsToAdd.Count - 1 do
    begin
      GroupName := GroupsToAdd.Strings[i];
      if AddedGroups.IndexOf(GroupName) = -1 then
        begin
          // Ok, new group here, we must add it before continue
          AuthGroup.Name := StringToUTF8(GroupName);
          AuthGroup.SessionTimeout := 10;
          AuthGroup.SQLAccessRights := SQLAccessRights;
          if RestServerSettings.AddGroup(AuthGroup) then
            // Add group to local temp list, to avoid double processing
            AddedGroups.Add(GroupName)
          else
            begin
              Result := False;
              ShowMessage('Group "' + GroupName + '" was not added, for some reason.');
            end;
        end;
    end;
  GroupsToAdd.Clear;
  if Result then
    begin
      // Add method authorization settings
      for i := 0 to ListViewMethodGroups.Items.Count - 1 do
        begin
          MethodName := ListViewMethodGroups.Items[i].Caption;
          MethodAuthorizationSettings.MethodName := StringToUTF8(MethodName);
          SetLength(MethodAuthorizationSettings.AllowedGroups, 0);
          SetLength(MethodAuthorizationSettings.DeniedGroups, 0);
          // Allowed groups
          GroupsToAdd.Text := ListViewMethodGroups.Items[i].SubItems.Strings[0];
          for j := 0 to GroupsToAdd.Count - 1 do
            begin
              GroupName := GroupsToAdd.Strings[j];
              if AddedGroups.IndexOf(GroupName) <> -1 then
                begin
                  l := Length(MethodAuthorizationSettings.AllowedGroups);
                  SetLength(MethodAuthorizationSettings.AllowedGroups, l + 1);
                  MethodAuthorizationSettings.AllowedGroups[l] := StringToUTF8(GroupName);
                end
            end;
          GroupsToAdd.Clear;
          // Denied groups
          GroupsToAdd.Text := ListViewMethodGroups.Items[i].SubItems.Strings[1];
          for j := 0 to GroupsToAdd.Count - 1 do
            begin
              GroupName := GroupsToAdd.Strings[j];
              if AddedGroups.IndexOf(GroupName) <> -1 then
                begin
                  l := Length(MethodAuthorizationSettings.DeniedGroups);
                  SetLength(MethodAuthorizationSettings.DeniedGroups, l + 1);
                  MethodAuthorizationSettings.DeniedGroups[l] := StringToUTF8(GroupName);
                end
            end;
          if not RestServerSettings.AddMethodAuthorizationSettings(MethodAuthorizationSettings) then
            begin
              Result := False;
              ShowMessage('Method authorization settings for method"' + UTF8ToString(MethodAuthorizationSettings.MethodName) + '" was not added, for some reason.');
            end;
        end;
      GroupsToAdd.Clear;
      if Result then
        begin
          // Add users
          for i := 0 to ListViewUsers.Items.Count - 1 do
            begin
              UserName := ListViewUsers.Items[i].Caption;
              AuthUser.LogonName := StringToUTF8(UserName);
              AuthUser.DisplayName := AuthUser.LogonName;
              AuthUser.PasswordPlain := StringToUTF8(ListViewUsers.Items[i].SubItems.Strings[0]);
              AuthUser.Group := StringToUTF8(ListViewUsers.Items[i].SubItems.Strings[1]);
              if not RestServerSettings.AddUser(AuthUser) then
                begin
                  Result := False;
                  ShowMessage('User "' + UserName + '" was not added, for some reason.');
                end;
            end;
        end;
    end;
  // Cleanup
  AddedGroups.Free;
  GroupsToAdd.Free;
end;

// Depends on the server status, start or stop server (create or destroy objects)
procedure TForm1.StartStopServer(ServerAction: lServerAction = Auto);
var
  pServerCreated: Boolean;
  RestServerSettings: TRestServerSettings;
begin
  pServerCreated := RestServer.Initialized;
  // Unload current server if required
  if pServerCreated then
    RestServer.DeInitialize();
  // Create server if required
  if ((ServerAction = lServerAction.Auto) and not pServerCreated) or ((ServerAction = lServerAction.Restart) and pServerCreated) or (ServerAction = lServerAction.Start) then
    begin
      // Create server object with selected Protocol and Auth mode
      RestServerSettings := TRestServerSettings.Create();
      RestServerSettings.Protocol := lProtocol(ComboBoxProtocol.ItemIndex);
      RestServerSettings.Port := EditPort.Text;
      RestServerSettings.AuthenticationMode := lAuthenticationMode(ComboBoxAuthentication.ItemIndex);
      RestServerSettings.AuthorizationPolicy := lAuthorizationPolicy(RadioGroupAuthorizationPolicy.ItemIndex);
      // Fill method authorization rules from UI
      FillMethodAuthorizationRulesFromUI(RestServerSettings);
      // Start server
      RestServer.Initialize(RestServerSettings);
    end;
end;

// Processing mORMot log event
function TForm1.LogEvent(Sender: TTextWriter; Level: TSynLogInfo; const Text: RawUTF8): Boolean;
var
  List: TList<TLocalLog>;
  LogEventData: TLocalLog;
begin
  Result := False;
  if Assigned(LogThreadSafeList) then
    begin
      {$ifdef FPC}
      List := LogThreadSafeList;
      {$else}
      List := LogThreadSafeList.LockList;
      {$endif}
      try
        LogEventData := TLocalLog.Create();
        LogEventData.Level := Level;
        LogEventData.Text := Text;
        List.Add(LogEventData);
        Result := True;
      finally
        {$ifndef FPC}
        LogThreadSafeList.UnlockList();
        {$endif}
      end;
    end;
end;

{ UI }

// Grabbing new events from thread safe list
procedure TForm1.TimerRefreshLogMemoTimer(Sender: TObject);
var
  List: TList<TLocalLog>;
  i: integer;
begin
  if Assigned(LogThreadSafeList) then
    begin
      {$ifdef FPC}
      List := LogThreadSafeList;
      {$else}
      List := LogThreadSafeList.LockList;
      {$endif}
      try
        if Assigned(Form1) and not Application.Terminated and (List.Count > 0) then
          begin
            for i := 0 to List.Count - 1 do
              begin
                Form1.MemoLog.Lines.BeginUpdate();
                Form1.MemoLog.Lines.Add(string(List.Items[i].Text));
                Form1.MemoLog.Lines.EndUpdate();
                List.Items[i].Free;
              end;
            List.Clear();
            {$IFDEF MSWINDOWS}
            if CheckBoxAutoScroll.Checked then
              SendMessage(Form1.MemoLog.Handle, WM_VSCROLL, SB_BOTTOM, 0);
            {$ENDIF}
          end;
      finally
        {$ifndef FPC}
        LogThreadSafeList.UnlockList();
        {$endif}
      end;
    end;
  if RestServer.Initialized then
    ButtonStartStop.Caption := 'Stop server'
  else
    ButtonStartStop.Caption := 'Start server';
end;

// Get description for AuthMode
function TForm1.GetAuthModeDescription(AM: lAuthenticationMode): string;
begin
  case AM of
    NoAuthentication:
      Result := 'Disabled authentication.';
    {
      URI:
      Result := 'Weak authentication scheme using URL-level parameter';
    }
    {
      SignedURI:
      Result := 'Secure authentication scheme using URL-level digital signature - expected format of session_signature is:' + #13 + 'Hexa8(SessionID) + Hexa8(TimeStamp) + ' + #13 +
      'Hexa8(crc32(SessionID + HexaSessionPrivateKey Sha256(salt + PassWord) + Hexa8(TimeStamp) + url))';
    }
    Default:
      Result := 'mORMot secure RESTful authentication scheme, this method will use a password stored via safe SHA-256 hashing in the TSQLAuthUser ORM table';
    None:
      Result := 'mORMot weak RESTful authentication scheme, this method will authenticate with a given username, but no signature' + #13 +
        'on client side, this scheme is not called by TSQLRestClientURI.SetUser() method - so you have to write:' + #13 + 'TSQLRestServerAuthenticationNone.ClientSetUser(Client,''User'','''');';
    HttpBasic:
      Result := 'Authentication using HTTP Basic scheme. This protocol send both name and password as clear (just base-64 encoded) so should only be used over SSL / HTTPS' +
        ', or for compatibility reasons. Will rely on TSQLRestServerAuthenticationNone for authorization, on client side, this scheme is not called by TSQLRestClientURI.SetUser() ' +
        'method - so you have to write: TSQLRestServerAuthenticationHttpBasic.ClientSetUser(Client,''User'',''password'');' + #13 +
        'for a remote proxy-only authentication (without creating any mORMot session), you can write: TSQLRestServerAuthenticationHttpBasic.ClientSetUserHttpOnly(Client,''proxyUser'',''proxyPass'');';
    {$IFDEF MSWINDOWS}
    SSPI:
      Result := 'authentication of the current logged user using Windows Security Support Provider Interface (SSPI)' + #13 +
        '- is able to authenticate the currently logged user on the client side, using either NTLM or Kerberos - it would allow to safely authenticate on a mORMot server without prompting' +
        ' the user to enter its password' + #13 + '- if ClientSetUser() receives aUserName as '''', aPassword should be either '''' if you expect NTLM authentication to take place,' +
        ' or contain the SPN registration (e.g. ''mymormotservice/myserver.mydomain.tld'') for Kerberos authentication.' + #13 +
        '- if ClientSetUser() receives aUserName as ''DomainName\UserName'', then authentication will take place on the specified domain, with aPassword as plain password value.';
    {$ENDIF}
  else
    Result := 'Authentication description';
  end;
end;

// Changing server protocol
procedure TForm1.ComboBoxProtocolChange(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}
  LabelHTTPSnote.Visible := (lProtocol(ComboBoxProtocol.ItemIndex) = HTTPsys_SSL);
  {$ENDIF}
  StartStopServer(Restart);
end;

// Changing server authentication mode
procedure TForm1.ComboBoxAuthenticationChange(Sender: TObject);
begin
  StartStopServer(Restart);
end;

// Button clear log
procedure TForm1.ButtonCLSClick(Sender: TObject);
begin
  MemoLog.Clear;
end;

// Button show authorization mode description
procedure TForm1.ButtonShowAuthorizationInfoClick(Sender: TObject);
var
  AM: lAuthenticationMode;
begin
  AM := lAuthenticationMode(ComboBoxAuthentication.ItemIndex);
  ShowMessage(GetAuthModeDescription(AM));
end;

// Button start stop server
procedure TForm1.ButtonStartStopClick(Sender: TObject);
begin
  StartStopServer();
end;

// Checkbox Enable/Disable logging to memo (slow down performance when enabled)
procedure TForm1.CheckBoxDisableLogClick(Sender: TObject);
begin
  if not CheckBoxDisableLog.Checked then
    TSQLLog.Family.Level := LOG_VERBOSE
  else
    TSQLLog.Family.Level := [];
end;

procedure TForm1.ListViewMethodGroupsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  if Assigned(Item) then
    begin
      EditAllowGroupNames.Text := Item.SubItems.Strings[0];
      EditDenyAllowGroupNames.Text := Item.SubItems.Strings[1];
    end;
end;

procedure TForm1.ButtonSaveRoleConfigurationClick(Sender: TObject);
var
  Item: TListItem;
begin
  Item := ListViewMethodGroups.Selected;
  if Assigned(Item) then
    begin
      Item.SubItems.Strings[0] := EditAllowGroupNames.Text;
      Item.SubItems.Strings[1] := EditDenyAllowGroupNames.Text;
      StartStopServer(lServerAction.Restart);
    end;
end;

procedure TForm1.ListViewUsersSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  if Assigned(Item) then
    begin
      EditUserName.Text := Item.Caption;
      EditUserPassword.Text := Item.SubItems.Strings[0];
      EditUserGroup.Text := Item.SubItems.Strings[1];
    end;
end;

procedure TForm1.ButtonSaveUsersClick(Sender: TObject);
var
  Item: TListItem;
  Done: Boolean;
begin
  Done := False;
  Item := ListViewUsers.Selected;
  if Assigned(Item) then
    begin
      Item.Caption := EditUserName.Text;
      Item.SubItems.Strings[0] := EditUserPassword.Text;
      Item.SubItems.Strings[1] := EditUserGroup.Text;
      Done := True;
    end
  else if (EditUserName.Text <> '') and (EditUserGroup.Text <> '') then
    begin
      Item := ListViewUsers.Items.Add();
      Item.Caption := EditUserName.Text;
      Item.SubItems.Add(EditUserPassword.Text);
      Item.SubItems.Add(EditUserGroup.Text);
      Done := True;
    end
  else
    ShowMessage('User name and Group must be filled.');
  if Done then
    begin
      EditUserName.Text := '';
      EditUserPassword.Text := '';
      EditUserGroup.Text := '';
      StartStopServer(lServerAction.Restart);
    end;
end;

procedure TForm1.RadioGroupAuthorizationPolicyClick(Sender: TObject);
begin
  StartStopServer(lServerAction.Restart);
end;

procedure TForm1.ListViewMethodGroupsClick(Sender: TObject);
begin
  if ListViewMethodGroups.ItemIndex = -1 then
    begin
      EditAllowGroupNames.Text := '';
      EditDenyAllowGroupNames.Text := '';
    end;
end;

procedure TForm1.ListViewUsersClick(Sender: TObject);
begin
  if ListViewUsers.ItemIndex = -1 then
    begin
      EditUserName.Text := '';
      EditUserPassword.Text := '';
      EditUserGroup.Text := '';
    end;
end;

// Delete user
procedure TForm1.ButtonDeleteUserClick(Sender: TObject);
var
  SelUserIndex: integer;
begin
  SelUserIndex := ListViewUsers.ItemIndex;
  if SelUserIndex <> -1 then
    begin
      ListViewUsers.Items.Delete(SelUserIndex);
      EditUserName.Text := '';
      EditUserPassword.Text := '';
      EditUserGroup.Text := '';
      StartStopServer(lServerAction.Restart);
    end;
end;

end.
