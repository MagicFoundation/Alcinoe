Unit Form_Splash;

{$mode delphi}

interface

uses
  SysUtils,Classes,math,
  And_jni,And_jni_Bridge,
  And_Controls_Types,And_Controls;

Type
 jForm_Splash = class(jForm)
 private
  tvLabel     : jTextView;
  ivImageBG   : jImageView; // Image - BackGround
  ivImageIcon : jImageView; // Image - Cube
  Timer       : jTimer;
  //
 protected 
  Procedure Form_OnActive    (Sender : TObject);
  Procedure Form_OnCloseQuery(Sender : TObject; Var CanClose : Boolean);
  Procedure Form_OnClose     (Sender : TObject);
  //
  Procedure Timer_OnTimer    (Sender : TObject);
 public
  Cnt_Timer  : Integer;
  Constructor Create(Owner : jForm) ; override;
  Destructor  Destroy; override;
 end;

implementation

//
Constructor jForm_Splash.Create(Owner : jForm);
 begin
  inherited;
  dbg('SplashForm.OnCreate');
  // Resource Extract
  Asset_SaveToFile('mormot.jpg'  ,App.Paths.Files+'/mormot.jpg');
  Asset_SaveToFile('Synopse.png',App.Paths.Files+'/Synopse.png');
  // Create Controls ----------------------------------------------------------
  ivImageBG              := jImageView.Create(Self);
  ivImageBG.xyWH         := xyWH( 0,0,App.Device.ScreenWH.width,App.Device.ScreenWH.height); // x,y,w,h
  ivImageBG.Parent       := Self.View;
  ivImageBG.ImageName    := App.Paths.Files+'/mormot.jpg';
  //
  ivImageIcon            := jImageView.Create(Self);
  ivImageIcon.xyWH       := xyWH(100,80,550,190); // x,y,w,h
  ivImageIcon.Parent     := Self.View;
  ivImageIcon.ImageName  := App.Paths.Files+'/Synopse.png';
  //
  tvLabel                := jTextView.Create (Self);
  tvLabel.xyWH           := xyWH(60,100,600,60);
  tvLabel.Parent         := Self.View;
  tvLabel.Text           := utf8encode('Please wait ... initializing the mORMot');
  tvLabel.FontColor      := clWhite;
  tvLabel.FontSize       := 35;
  //
  Timer                  := jTimer.Create(Self);
  Timer.OnTimer          := Timer_OnTimer;
  Timer.Interval         := 20;
  Timer.Enabled          := True;
  Cnt_Timer              := 0;
  //
  Self.Name              := 'Form_Splash';
  Self.Color             := clBlack;
  Self.OnActive          := Form_OnActive;
  Self.OnClose           := Form_OnClose;
  Self.OnCloseQuery      := Form_OnCloseQuery;
  App.SetTitleBar(False);
  App.SetScreenStyle(Screen_Style_Full);
  //
 end;

Destructor jForm_Splash.Destroy;
 begin
  ivImageBG.Free;
  ivImageIcon.Free;
  tvLabel.Free;
  Timer.Free;
  inherited;
 end;

//
Procedure jForm_Splash.Form_OnActive(Sender : TObject);
begin
  dbg('Form_Splash OnActive');
end;

//
Procedure jForm_Splash.Form_OnCloseQuery(Sender : TObject; Var CanClose : Boolean);
begin
  dbg('Form_Splash OnCloseQuery');
  CanClose := True;
end;

//
Procedure jForm_Splash.Form_OnClose     (Sender : TObject);
begin
  dbg('Form_Splash OnClose');
  Free;
  App.SetTitleBar(True);
  App.SetScreenStyle(Screen_Style_Normal);
end;

//
Procedure jForm_Splash.Timer_OnTimer(Sender : TObject);
 begin
  Inc(Cnt_Timer,3);
  If Cnt_timer < ((App.Device.ScreenWH.height DIV 2)-200) then ivImageIcon.xyWH  := xyWH(100,80+Cnt_Timer,550,190);
  If Cnt_timer < (App.Device.ScreenWH.height DIV 2) then Exit;
  Timer.Enabled := False;
  Close;
 end;

end.
