unit Unit1;

interface

uses
  Vcl.StdCtrls, System.Classes, Vcl.Controls, VCL.Forms;

type
  TForm1 = class(TForm)
    GoButton: TButton;
    ResultMemo: TMemo;
    procedure GoButtonClick(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

Uses
  system.SysUtils,
  Alcinoe.Net.TLS.Client.SChannel,
  Alcinoe.StringUtils,
  Alcinoe.JSONDoc,
  Alcinoe.Common;

{$R *.DFM}

{**********************************************}
procedure TForm1.GoButtonClick(Sender: TObject);
begin
  var LTSLClient := TAlSChannelTlsClient.Create;
  Try

    (*
    // test TSL 1.3 renegociation
    LTSLClient.Connect('tls13.akamai.io', 443);
    var LReqA: AnsiString :=
      'GET / HTTP/1.1'#13#10 +
      'Host: tls13.akamai.io' + #13#10 +
      'Connection: close'#13#10 +
      'User-Agent: Alcinoe-SChannel-Demo'#13#10 +
      #13#10;
    *)

    (*
    // test TSL big file
    LTSLClient.Connect('ash-speed.hetzner.com', 443);
    var LReqA: AnsiString :=
      'GET /100MB.bin HTTP/1.1'#13#10 +
      'Host: ash-speed.hetzner.com' + #13#10 +
      'Connection: close'#13#10 +
      'User-Agent: Alcinoe-SChannel-Demo'#13#10 +
      #13#10;
    *)

    LTSLClient.Connect('www.howsmyssl.com', 443);
    var LReqA: AnsiString :=
      'GET /a/check HTTP/1.1'#13#10 +
      'Host: www.howsmyssl.com' + #13#10 +
      'Connection: close'#13#10 +
      'User-Agent: Alcinoe-SChannel-Demo'#13#10 +
      #13#10;

    LTSLClient.SendAll(LReqA[low(LReqA)], length(LReqA));
    var LRespA: AnsiString;
    LTSLClient.ReceiveAll(LRespA);
    var LBodyA := LRespA;
    var P1 := ALposA(#13#10#13#10, LBodyA);
    If P1 > 0 then delete(LBodyA, 1, P1);
    LBodyA := AlTrim(LBodyA);
    if alposA('{', LBodyA) = 1 then begin
      var LJsonDoc := TALJsonDocumentA.CreateFromJSONString(ALTrim(LBodyA));
      try
        LJsonDoc.SaveToJSONString(LBodyA, [soNodeAutoIndent]);
        ResultMemo.Text := String(LBodyA);
      finally
        ALFreeAndNil(LJsonDoc);
      end;
    end
    else
      ResultMemo.Text := String(LRespA);
  Finally
    ALFreeAndNil(LTSLClient);
  End;
end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.