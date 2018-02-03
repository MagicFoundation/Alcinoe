program ConsoleExamples;

{$APPTYPE CONSOLE}

{$I jcl.inc}
{$I windowsonly.inc}

uses
  SysUtils,
  Windows,
  TypInfo,
  JclBase,
  JclConsole;

{$R ..\..\..\source\windows\JclNoDepAsInvoker.res}

type
  TCPInfoEx = packed record
    MaxCharSize: DWORD;
    DefaultChar: array[0..MAX_DEFAULTCHAR-1] of Byte;
    LeadByte: array[0..MAX_LEADBYTES-1] of Byte;
    UnicodeDefaultChar: WideChar;
    CodePage: DWORD;
    CodePageName: array[0..MAX_PATH-1] of Char;
  end;

function GetCPInfoEx(CodePage, dwFlags: DWORD; var lpCPInfoEx: TCPInfoEx): BOOL; stdcall;
  external 'kernel32.dll' name 'GetCPInfoExA';

procedure ShowConsoleInfo(const Console: TJclConsole);
  function CodePageToName(CodePage: DWORD): string;
  var
    CpInfo: TCPInfoEx;
  begin
    Win32Check(GetCPInfoEx(CodePage, 0, CpInfo));
    Result := CpInfo.CodePageName;
  end;
begin
  Assert(TJclConsole.IsConsole(GetModuleHandle(nil)));
  Assert(TJclConsole.IsConsole(ParamStr(0)));

  Console.ActiveScreen.WriteLn('Old Windows Title : ' + Console.Title);
  Console.Title := 'Information of the Default Output Screen Buffer';
  Console.ActiveScreen.WriteLn('New Windows Title : ' + Console.Title);
  Console.ActiveScreen.WriteLn(Format('Input Code Page : %s', [CodePageToName(Console.InputCodePage)]));
  Console.ActiveScreen.WriteLn(Format('Output Code Page : %s', [CodePageToName(Console.OutputCodePage)]));
end;

procedure ShowScreenInfo(const ScrBuf: TJclScreenBuffer);
  function ModeToString: string;
  var
    AMode: TJclConsoleOutputMode;
  begin
    for AMode:=Low(TJclConsoleOutputMode) to High(TJclConsoleOutputMode) do
    begin
      if AMode in ScrBuf.Mode then
      begin
        if Result <> '' then
          Result := Result + ', ';
        Result := Result + GetEnumName(TypeInfo(TJclConsoleOutputMode), Integer(AMode));
      end;
    end;
  end;
var
  OldMode: TJclConsoleOutputModes;
  Attr: IJclScreenTextAttribute;
begin
  ScrBuf.WriteLn;
  ScrBuf.WriteLn(Format('Handle: ' + HexFmt, [ScrBuf.Handle]));
  ScrBuf.Writeln('Old Mode : ' + ModeToString);
  OldMode := ScrBuf.Mode;
  ScrBuf.Mode := ScrBuf.Mode - [omWrapAtEol];
  ScrBuf.Write('New Mode : ' + ModeToString);
  Readln;
  ScrBuf.Mode := OldMode;

  ScrBuf.WriteLn(Format('Old Size: (%d, %d)', [ScrBuf.Width, ScrBuf.Height]));
  ScrBuf.Width := ScrBuf.Width * 2;
  ScrBuf.Write(Format('New Size: (%d, %d)', [ScrBuf.Width, ScrBuf.Height]));
  Readln;
  ScrBuf.Width := ScrBuf.Width div 2;

  Attr := TJclScreenTextAttribute.Create(fclYellow, bclBlue, True, False,
    [fsGridHorizontal, fsUnderscore]);

  ScrBuf.Write('Top', thaCenter, tvaTop, Attr);
  ScrBuf.Write('Bottom', thaCenter, tvaBottom, Attr);
  ScrBuf.Write('Left', thaLeft, tvaCenter, Attr);
  ScrBuf.Write('Right', thaRight, tvaCenter, Attr);
  ScrBuf.Write('Center', thaCenter, tvaCenter, Attr);
end;

procedure ShowCursorInfo(const ScrBuf: TJclScreenBuffer);
const
  BoolName: array[Boolean] of string = ('Hide', 'Show');
var
  OldPos: TCoord;
  OldSize: TJclScreenCursorSize;
begin
  ScrBuf.WriteLn;
  ScrBuf.WriteLn(Format('Cursor Position: (%d, %d)', [ScrBuf.Cursor.Position.X, ScrBuf.Cursor.Position.Y]));
  OldPos := ScrBuf.Cursor.Position;
  ScrBuf.Cursor.MoveTo(ScrBuf.Window.Left, ScrBuf.Window.Top);
  ScrBuf.Write(Format('(%d, %d)', [ScrBuf.Cursor.Position.X, ScrBuf.Cursor.Position.Y]));
  ScrBuf.Cursor.Position := OldPos;
  Readln;
  ScrBuf.WriteLn('Left-Top corner :' + ScrBuf.ReadLn(0, 0));

  ScrBuf.WriteLn(Format('Old Cursor Size: %d', [ScrBuf.Cursor.Size]));
  OldSize := ScrBuf.Cursor.Size; ScrBuf.Cursor.Size := 100;
  ScrBuf.Write(Format('New Cursor Size: %d', [ScrBuf.Cursor.Size]));
  Readln;
  ScrBuf.Cursor.Size := OldSize;

  ScrBuf.WriteLn('Visible of Cursor: ' + BoolName[ScrBuf.Cursor.Visible]);
  ScrBuf.Cursor.Visible := False;
  ScrBuf.Write('Hidden Cursor: ' + BoolName[ScrBuf.Cursor.Visible]);
  Readln;
  ScrBuf.Cursor.Visible := True;
end;

procedure ShowWindowInfo(const ScrBuf: TJclScreenBuffer);
var
  OldPos, OldSize: TCoord;
begin
  ScrBuf.WriteLn;
  ScrBuf.WriteLn(Format('Largest Console Size : (%d, %d)',
    [ScrBuf.Window.MaxConsoleWindowSize.X, ScrBuf.Window.MaxConsoleWindowSize.Y]));
  ScrBuf.WriteLn(Format('Largest Window Size : (%d, %d)',
    [ScrBuf.Window.MaxWindow.X, ScrBuf.Window.MaxWindow.Y]));

  ScrBuf.WriteLn(Format('Old Window Position : (%d, %d)', [ScrBuf.Window.Left, ScrBuf.Window.Top]));
  OldPos := ScrBuf.Window.Position;
  ScrBuf.Window.Left := 0;
  ScrBuf.Window.Top  := 0;
  ScrBuf.Write(Format('New Window Position : (%d, %d)', [ScrBuf.Window.Left, ScrBuf.Window.Top]));
  Readln;
  ScrBuf.Window.Position := OldPos;

  ScrBuf.WriteLn(Format('Old Window Size : (%d, %d)', [ScrBuf.Window.Width, ScrBuf.Window.Height]));
  OldSize := ScrBuf.Window.Size;
  ScrBuf.Window.Width  := ScrBuf.Window.Width div 2;
  ScrBuf.Window.Height := ScrBuf.Window.Height div 2;
  ScrBuf.Write(Format('New Window Size : (%d, %d)', [ScrBuf.Window.Width, ScrBuf.Window.Height]));
  Readln;
  ScrBuf.Window.Size := OldSize;

  ScrBuf.Write(Format('Scroll up %d line: ', [ScrBuf.Window.Top]));
  Readln;
  OldPos := ScrBuf.Window.Position;
  ScrBuf.Window.Scroll(0, -ScrBuf.Window.Top);
  Readln;
  ScrBuf.Window.Position := OldPos;
end;

procedure ShowTextAttributeInfo(const ScrBuf: TJclScreenBuffer);
  function StyleToString: string;
  var
    AStyle: TJclScreenFontStyle;
  begin
    for AStyle:=Low(TJclScreenFontStyle) to High(TJclScreenFontStyle) do
    begin
      if AStyle in ScrBuf.Font.Style then
      begin
        if Result <> '' then
          Result := Result + ', ';
        Result := Result + GetEnumName(TypeInfo(TJclScreenFontStyle), Integer(AStyle));
      end;
    end;
  end;
const
  HighlightName: array[Boolean] of string = ('', ' [Highlight]');
var
  OldTextAttribute: Word;
begin
  ScrBuf.WriteLn('Old Font Color : ' +
    GetEnumName(TypeInfo(TJclScreenFontColor), Integer(ScrBuf.Font.Color)) +
    HighlightName[ScrBuf.Font.Highlight]);
  ScrBuf.WriteLn('Old Back Color : ' +
    GetEnumName(TypeInfo(TJclScreenBackColor), Integer(ScrBuf.Font.BgColor)) +
    HighlightName[ScrBuf.Font.BgHighlight]);
  ScrBuf.Writeln('Old Font Style : ' + StyleToString);
  OldTextAttribute      := ScrBuf.Font.TextAttribute;
  ScrBuf.Font.Color     := fclYellow;
  ScrBuf.Font.Highlight := True;
  ScrBuf.Font.BgColor   := bclBlue;
  ScrBuf.Font.Style     := ScrBuf.Font.Style + [fsUnderscore];
  ScrBuf.WriteLn('New Font Color : ' +
    GetEnumName(TypeInfo(TJclScreenFontColor), Integer(ScrBuf.Font.Color)) +
    HighlightName[ScrBuf.Font.Highlight]);
  ScrBuf.WriteLn('New Back Color : ' +
    GetEnumName(TypeInfo(TJclScreenBackColor), Integer(ScrBuf.Font.BgColor)) +
    HighlightName[ScrBuf.Font.BgHighlight]);
  ScrBuf.Write('New Font Style : ' + StyleToString);
  ScrBuf.Font.TextAttribute := OldTextAttribute;
  ScrBuf.Writeln;
end;

{ TCtrlEventHandler }

type
  TCtrlEventHandler = class
  private
    FConsole: TJclConsole;
    FTerminated: Boolean;
  protected
    procedure OnCtrlC(Sender: TObject);
    procedure OnCtrlBreak(Sender: TObject);
    procedure OnClose(Sender: TObject);
    procedure OnLogOff(Sender: TObject);
    procedure OnShutdown(Sender: TObject);

    procedure Terminate;
  public
    constructor Create(AConsole: TJclConsole);

    property Console: TJclConsole read FConsole;
    property Terminated: Boolean read FTerminated;
  end;

constructor TCtrlEventHandler.Create(AConsole: TJclConsole);
begin
  FConsole    := AConsole;
  FTerminated := False;

  Console.OnCtrlC     := OnCtrlC;
  Console.OnCtrlBreak := OnCtrlBreak;
  Console.OnClose     := OnClose;
  Console.OnLogOff    := OnLogOff;
  Console.OnShutdown  := OnShutdown;
end;

procedure TCtrlEventHandler.Terminate;
var
  Evt: TInputRecord;
begin
  Sleep(1000);

  FTerminated := True;

  Evt.EventType := FOCUS_EVENT;
  Evt.Event.FocusEvent.bSetFocus := False;
  FConsole.Input.PutEvent(Evt);
end;

procedure TCtrlEventHandler.OnCtrlC(Sender: TObject);
begin
  Console.ActiveScreen.Writeln('Ctrl Event: Ctrl-C');
end;

procedure TCtrlEventHandler.OnCtrlBreak(Sender: TObject);
begin
  Console.ActiveScreen.Writeln('Ctrl Event: Ctrl-Break');
end;

procedure TCtrlEventHandler.OnClose(Sender: TObject);
begin
  Console.ActiveScreen.Writeln('Ctrl Event: Close');
  Terminate;
end;

procedure TCtrlEventHandler.OnLogOff(Sender: TObject);
begin
  Console.ActiveScreen.Writeln('Ctrl Event: Logoff');
  Terminate;
end;

procedure TCtrlEventHandler.OnShutdown(Sender: TObject);
begin
  Console.ActiveScreen.Writeln('Ctrl Event: Shutdown');
  Terminate;
end;

procedure ShowInputInfo(const InputBuf: TJclInputBuffer);
  function ModeToString: string;
  var
    AMode: TJclConsoleInputMode;
  begin
    for AMode:=Low(TJclConsoleInputMode) to High(TJclConsoleInputMode) do
    begin
      if AMode in InputBuf.Mode then
      begin
        if Result <> '' then
          Result := Result + ', ';
        Result := Result + GetEnumName(TypeInfo(TJclConsoleInputMode), Integer(AMode));
      end;
    end;
  end;

  procedure AddEvent;
  var
    ir: TInputRecord;
  begin
    ir.EventType := MENU_EVENT;
    ir.Event.MenuEvent.dwCommandId := 111;
    InputBuf.PutEvent(ir);
  end;
const
  MOUSE_CLICKED = 0;
  MOUSE_WHEELED = 3;
  KeyDownBoolName: array[Boolean] of string = ('released', 'pressed');
  SetFocusBoolName: array[Boolean] of string = ('deactivated', 'activated');
var
  I: DWORD;
  OldPos: TCoord;
  CtrlEvt: TCtrlEventHandler;
  ScrBuf: TJclScreenBuffer;
begin
  ScrBuf := InputBuf.Console.ActiveScreen;
  ScrBuf.WriteLn(Format('Input Event Count : %d', [InputBuf.EventCount]));

  InputBuf.Mode := [imProcessed, imWindow, imMouse];
  ScrBuf.Writeln('Input Mode : ' + ModeToString);

  InputBuf.Clear;

  AddEvent;

  CtrlEvt := TCtrlEventHandler.Create(InputBuf.Console);
  try
    ScrBuf.WriteLn('Press [q] to break the loop...');
    while not CtrlEvt.Terminated and InputBuf.WaitEvent do
    begin
      with InputBuf.GetEvent do
        case EventType of
        KEY_EVENT:
        begin
            ScrBuf.WriteLn(Format('Key (%s)$%.2x is %s %d times',
             [Event.KeyEvent.AsciiChar, Event.KeyEvent.wVirtualKeyCode,
              KeyDownBoolName[Event.KeyEvent.bKeyDown], Event.KeyEvent.wRepeatCount]));

            if Event.KeyEvent.AsciiChar = 'q' then
              Break;
        end;
        _MOUSE_EVENT:
        begin
          case Event.MouseEvent.dwEventFlags of
            MOUSE_CLICKED:
            begin
              for I:= 1 to TJclConsole.MouseButtonCount do
              if (Event.MouseEvent.dwButtonState and (1 shl (I - 1))) <> 0 then
              begin
                ScrBuf.Write(Format('Mouse %d button click at', [I]));
                Break;
              end;
              if I > TJclConsole.MouseButtonCount then
                ScrBuf.Write('Mouse button released at');
            end;
            DOUBLE_CLICK:
              ScrBuf.Write('Mouse double-click at');
            MOUSE_MOVED:
            begin
              if (OldPos.X <> Event.MouseEvent.dwMousePosition.X) or
                 (OldPos.Y <> Event.MouseEvent.dwMousePosition.Y) then
              begin
                ScrBuf.Write('Mouse move to');
                OldPos := Event.MouseEvent.dwMousePosition;
              end
              else
                Continue;
            end;
            MOUSE_WHEELED:
              ScrBuf.Write('Mouse wheeled at');
          else
            ScrBuf.Write('Mouse unknown action at');
          end;

          ScrBuf.WriteLn(Format(' (%d, %d) ', [Event.MouseEvent.dwMousePosition.X, Event.MouseEvent.dwMousePosition.Y]));
       end;
        WINDOW_BUFFER_SIZE_EVENT:
          ScrBuf.WriteLn(Format('Screen buffer size is change to (%d, %d)',
            [Event.WindowBufferSizeEvent.dwSize.X, Event.WindowBufferSizeEvent.dwSize.Y]));
        MENU_EVENT:
          ScrBuf.WriteLn(Format('Menu command %d is selected', [Event.MenuEvent.dwCommandId]));
        FOCUS_EVENT:
          ScrBuf.Writeln('Console window is ' + SetFocusBoolName[Event.FocusEvent.bSetFocus]);
        else
          ScrBuf.WriteLn(Format('Unknown event - %d', [EventType]));
        end;
    end;
  finally
    FreeAndNil(CtrlEvt);
  end;
end;

var
  ScrBuf, NewScrBuf: TJclScreenBuffer;
begin
  ShowConsoleInfo(TJclConsole.Default);

  ScrBuf := TJclConsole.Default.ActiveScreen;

  ShowScreenInfo(ScrBuf);
  ShowCursorInfo(ScrBuf);
  ShowWindowInfo(ScrBuf);

  ScrBuf.Clear;

  NewScrBuf := TJclConsole.Default.Add;
  ShowTextAttributeInfo(NewScrBuf);
  TJclConsole.Default.ActiveScreen := NewScrBuf;

  ShowInputInfo(TJclConsole.Default.Input);

  NewScrBuf.Clear;

  TJclConsole.Default.ActiveScreen := ScrBuf;
end.
