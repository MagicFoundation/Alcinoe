program ALLiveVideoChatServer;

//
// This demo is very drafty, normally we must not use http server for this and
// must must use socket client (or similar like notification), but i was lazzy
// to do for this demo
//

{$APPTYPE CONSOLE}

{$R *.res}

uses System.generics.collections,
     System.sysutils,
     IdHTTPServer,
     IdTCPServer,
     IdContext,
     IdCustomHTTPServer,
     alCommon;

Type


  {*********************************}
  TLiveChatCandidate = class(Tobject)
    SdpMid: String;
    SdpMLineIndex: String;
    Sdp: String;
  end;
  TLiveChatCandidates = TobjectList<TLiveChatCandidate>;

  {************************}
  TLiveChat = class(Tobject)
  public
    id_callee: int64;
    id_caller: int64;
    Status: integer;
    Sdp_offer: String;
    Sdp_answer: String;
    candidates_for_callee: TLiveChatCandidates;
    candidates_for_caller: TLiveChatCandidates;
    constructor create;
    destructor Destroy; override;
  End;
  TLiveChats = Tlist<TLiveChat>;

  {****************************}
  TCommandHandler=class(Tobject)
  protected
    class procedure DoCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
  end;

var Server:TIdHTTPServer;
    LiveChats: TLiveChats;

{***************************}
constructor TLiveChat.create;
begin
  id_callee := 0;
  id_caller := 0;
  Status := 1; // waiting
  Sdp_offer := '';
  Sdp_answer := '';
  candidates_for_callee:= TLiveChatCandidates.Create(true);
  candidates_for_caller:= TLiveChatCandidates.Create(true);
end;

{***************************}
destructor TLiveChat.Destroy;
begin
  candidates_for_callee.Free;
  candidates_for_caller.Free;
  inherited;
end;

{**************************************************************************************************************************************}
class procedure TCommandHandler.DoCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var aUserID: int64;
    aLiveChat: TLiveChat;
    aLiveChatCandidate: TLiveChatCandidate;
    i: integer;
    j: integer;
begin

  //init aUserID
  if not TryStrToInt64(ARequestInfo.Params.Values['user_id'], aUserID) then
    raise Exception.Create('user_id can''t be null');

  //init AResponseInfo.ContentType
  AResponseInfo.ContentType := 'text/plain';

  //action = enter
  if ARequestInfo.Params.Values['action'] = 'enter' then begin

    //writeln
    writeln(inttostr(aUserID) + ': enter');

    //look if one guy is waiting a call
    for I := 0 to LiveChats.Count - 1 do begin
      if LiveChats[i].id_caller = 0 then begin
        LiveChats[i].id_caller := aUserID;
        LiveChats[i].Status := 2;
        AResponseInfo.ContentText := inttostr(LiveChats[i].id_caller);
        exit;
      end;
    end;

    //no one is waiting a call, just add me in the waiting queue
    aLiveChat := TLiveChat.create;
    aLiveChat.id_callee := aUserID;
    aLiveChat.Status := 1;
    LiveChats.Add(aLiveChat);
    AResponseInfo.ContentText := '';
    exit;

  end


  //action = set_offer
  else if ARequestInfo.Params.Values['action'] = 'set_offer' then begin

    //writeln
    writeln(inttostr(aUserID) + ': set_offer');

    //look if one guy is waiting a call
    for I := 0 to LiveChats.Count - 1 do begin
      if (LiveChats[i].id_caller = aUserID) and (LiveChats[i].Status = 2) then begin
        LiveChats[i].Sdp_offer := ARequestInfo.Params.Values['sdp_offer'];
        LiveChats[i].Status := 3;
        AResponseInfo.ContentText := 'OK';
        exit;
      end;
    end;

    //----
    raise Exception.Create('can''t set the offer');

  end


  //action = check_offer
  else if ARequestInfo.Params.Values['action'] = 'check_offer' then begin

    //writeln
    writeln(inttostr(aUserID) + ': check_offer');

    //look if we have one offer
    for I := 0 to LiveChats.Count - 1 do begin
      if (LiveChats[i].id_callee = aUserID) and (LiveChats[i].Status = 3) then begin
        LiveChats[i].Status := 4;
        AResponseInfo.ContentText := LiveChats[i].Sdp_offer;
        exit;
      end;
    end;

    //we don't have any offer
    AResponseInfo.ContentText := '';
    exit;

  end


  //action = set_answer
  else if ARequestInfo.Params.Values['action'] = 'set_answer' then begin

    //writeln
    writeln(inttostr(aUserID) + ': set_answer');

    //look for our chat
    for I := 0 to LiveChats.Count - 1 do begin
      if (LiveChats[i].id_callee = aUserID) and (LiveChats[i].Status = 4) then begin
        LiveChats[i].sdp_answer := ARequestInfo.Params.Values['sdp_answer'];
        LiveChats[i].Status := 5;
        AResponseInfo.ContentText := 'OK';
        exit;
      end;
    end;

    //----
    raise Exception.Create('can''t set the answer');

  end


  //action = check_answer
  else if ARequestInfo.Params.Values['action'] = 'check_answer' then begin

    //writeln
    writeln(inttostr(aUserID) + ': check_answer');

    //look if we have one offer
    for I := 0 to LiveChats.Count - 1 do begin
      if (LiveChats[i].id_caller = aUserID) and (LiveChats[i].Status = 5) then begin
        LiveChats[i].Status := 6;
        AResponseInfo.ContentText := LiveChats[i].Sdp_answer;
        exit;
      end;
    end;

    //----
    AResponseInfo.ContentText := '';
    exit;

  end

  //action = set_candidate
  else if ARequestInfo.Params.Values['action'] = 'set_candidate' then begin

    //writeln
    writeln(inttostr(aUserID) + ': set_candidate');

    //look for our chat
    for I := 0 to LiveChats.Count - 1 do begin
      if (LiveChats[i].id_callee = aUserID) or (LiveChats[i].id_caller = aUserID) then begin
        aLiveChatCandidate := TLiveChatCandidate.Create;
        aLiveChatCandidate.SdpMid := ARequestInfo.Params.Values['SdpMid'];
        aLiveChatCandidate.SdpMLineIndex := ARequestInfo.Params.Values['SdpMLineIndex'];
        aLiveChatCandidate.Sdp := ARequestInfo.Params.Values['Sdp'];
        if (LiveChats[i].id_callee = aUserID) then LiveChats[i].candidates_for_caller.Add(aLiveChatCandidate)
        else LiveChats[i].candidates_for_callee.Add(aLiveChatCandidate);
        AResponseInfo.ContentText := '';
        exit;
      end;
    end;

    //----
    raise Exception.Create('can''t set the answer');

  end

  //action = check_answer
  else if ARequestInfo.Params.Values['action'] = 'check_candidate' then begin

    //writeln
    writeln(inttostr(aUserID) + ': check_candidate');

    //look if we have one offer
    AResponseInfo.ContentText := '';
    for I := 0 to LiveChats.Count - 1 do begin
      if (LiveChats[i].id_callee = aUserID) then begin
        for J := 0 to LiveChats[i].candidates_for_callee.Count -1 do begin
          if AResponseInfo.ContentText <> '' then AResponseInfo.ContentText := AResponseInfo.ContentText + #13#10;
          AResponseInfo.ContentText := AResponseInfo.ContentText + LiveChats[i].candidates_for_callee[j].SdpMid + #13#10 +
                                                                   LiveChats[i].candidates_for_callee[j].SdpMLineIndex + #13#10 +
                                                                   LiveChats[i].candidates_for_callee[j].Sdp;
        end;
        LiveChats[i].candidates_for_callee.Clear;
        exit;
      end
      else if (LiveChats[i].id_caller = aUserID) then begin
        for J := 0 to LiveChats[i].candidates_for_caller.Count -1 do begin
          if AResponseInfo.ContentText <> '' then AResponseInfo.ContentText := AResponseInfo.ContentText + #13#10;
          AResponseInfo.ContentText := AResponseInfo.ContentText + LiveChats[i].candidates_for_caller[j].SdpMid + #13#10 +
                                                                   LiveChats[i].candidates_for_caller[j].SdpMLineIndex + #13#10 +
                                                                   LiveChats[i].candidates_for_caller[j].Sdp;
        end;
        LiveChats[i].candidates_for_caller.Clear;
        exit;
      end;
    end;

  end
end;

begin
  LiveChats := TLiveChats.create;
  Server := TIdHTTPServer.Create(nil);
  try
    Server.OnCommandGet := TCommandHandler.DoCommandGet;
    Server.DefaultPort := 3030;
    Server.Active := True;
    Readln;
  finally
    ALFreeAndNil(Server);
    ALFreeAndNil(LiveChats);
  end;
end.
