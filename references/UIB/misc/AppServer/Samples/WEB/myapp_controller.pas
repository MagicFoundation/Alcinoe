unit myapp_controller;
{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface
uses superobject;

procedure app_controller_initialize(mvc: ISuperObject);

implementation
uses SysUtils, webserver, mypool, PDGDB;

{ HTTP Methods }

//**************************************************************
// getdata
//**************************************************************

procedure application_getdata_controller(const This, Params: ISuperObject;
  var Result: ISuperObject);
var
  cmd: IPDGCommand;
begin
  with pool.GetConnection.newContext do
  begin
    cmd := newCommand(so(['sql', 'select * from ' + Params.S['id'], 'array', true]));
    this['dataset'] := so(['meta', cmd.GetOutputMeta, 'data', Execute(cmd)]);
  end;
end;

//**************************************************************
// Country
//**************************************************************
procedure country_index(const This, Params: ISuperObject; var Result: ISuperObject);
begin
  with pool.GetConnection.newContext do
    this['dataset'] := Execute(newCommand('select country, currency from country order by 1'));
end;

procedure country_add(const This, Params: ISuperObject; var Result: ISuperObject);
begin
  if HTTPIsPost(this) then
  with pool.GetConnection.newContext do
  begin
    Execute(newCommand('INSERT INTO COUNTRY (country, currency) VALUES (?,?)'), Params['[country, currency]']);
    HTTPredirect(this,'/country/index');
  end;
end;

procedure country_del(const This, Params: ISuperObject; var Result: ISuperObject);
begin
  with pool.GetConnection.newContext do
  try
    Execute(newCommand('DELETE FROM COUNTRY WHERE COUNTRY = ?'), Params['id']);
    HTTPredirect(this,'/country/index');
  except
    on E: Exception do
    begin
      Params.S['action'] := 'index';
      This.S['error'] := E.Message;
      country_index(This, Params, Result);
    end;
  end;
end;

procedure country_edit(const This, Params: ISuperObject; var Result: ISuperObject);
var
  obj: ISuperObject;
begin
  try
    with pool.GetConnection.newContext do
    if HTTPIsPost(this) then
    begin
      Execute(newCommand('UPDATE COUNTRY SET CURRENCY = :currency WHERE COUNTRY = :country'), Params['formulaire']);
      HTTPredirect(this,'/country/index');
    end else
    begin
      obj := Execute(newCommand('{sql: "SELECT COUNTRY, CURRENCY FROM COUNTRY WHERE COUNTRY = ?", firstone: true}'), Params['id']);
      if obj <> nil then
      begin
        This['country'] := obj['COUNTRY'];
        This['currency'] := obj['CURRENCY'];
      end else
        raise Exception.Create('country not found');
    end;
  except
    on E: Exception do
    begin
      Params.S['action'] := 'index';
      This.S['error'] := E.Message;
      country_index(This, Params, Result);
    end;
  end;
end;

//**************************************************************
// initialization
//**************************************************************

procedure app_controller_initialize(mvc: ISuperObject);
begin


  mvc.O['application.getdata.validate'] :=
    SO('{type: map, inherit: mvc, mapping: {id: {type: text}}}');

  mvc.M['application.getdata.controller'] := @application_getdata_controller;

  mvc.M['country.index.controller'] := @country_index;
  mvc.O['country.add.validate'] :=
    SO('{type: map, inherit: mvc, mapping: {country: {type: text},currency: {type: text}}}');
  mvc.M['country.add.controller'] := @country_add;
  mvc.M['country.del.controller'] := @country_del;
  mvc.M['country.edit.controller'] := @country_edit;

end;

end.
