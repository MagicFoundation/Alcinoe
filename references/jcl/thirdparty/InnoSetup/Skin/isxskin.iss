[Files]
Source: {#MyWizardButtonImageFile}; DestDir: {tmp}; Flags: dontcopy
Source: {#MyWizardBottomImageFile}; DestDir: {tmp}; Flags: dontcopy

[Code]
const
  MAINPANELCOLOR = $663300;
  PAGECOLOR = $ab663d;
  BUTTONPANELCOLOR = $603A2B;
  BUTTONWIDTH = 100;
  BUTTONHEIGHT = 27;

  bidBack = 0;
  bidNext = 1;
  bidCancel = 2;
  bidDirBrowse = 3;
  bidGroupBrowse = 4;

var
  ButtonPanels: array [0..4] of TPanel;
  ButtonImages: array [0..4] of TBitmapImage;
  ButtonLabels: array [0..4] of TLabel;

procedure UpdateButton(AButton: TButton; AButtonIndex: integer);
begin
  ButtonLabels[AButtonIndex].Caption := AButton.Caption;
  ButtonPanels[AButtonIndex].Visible := AButton.Visible;
  ButtonLabels[AButtonIndex].Enabled := AButton.Enabled;
end;

procedure ButtonLabelClick(Sender: TObject);
var
  button: TButton;
begin
  ButtonImages[TLabel(Sender).Tag].Left := 0;
  if not ButtonLabels[TLabel(Sender).Tag].Enabled then
    Exit;
  case TLabel(Sender).Tag of
    bidBack: button := WizardForm.BackButton;
    bidNext: button := WizardForm.NextButton;
    bidCancel: button := WizardForm.CancelButton;
    bidDirBrowse: button := WizardForm.DirBrowseButton;
    bidGroupBrowse: button := WizardForm.GroupBrowseButton;
  else
    Exit;
  end;
  button.OnClick(button);
end;

procedure ButtonLabelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button=mbLeft) and (ButtonLabels[TLabel(Sender).Tag].Enabled) then
    ButtonImages[TLabel(Sender).Tag].Left := -BUTTONWIDTH;
end;

procedure ButtonLabelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ButtonImages[TLabel(Sender).Tag].Left := 0;
end;

procedure LoadButtonImage(AButton: TButton; AButtonIndex: integer);
var
  image: TBitmapImage;
  panel: TPanel;
  labl: TLabel;
  FileName: String;
begin
  panel := TPanel.Create(WizardForm);
  with panel do begin
    Parent := AButton.Parent;
    Color := BUTTONPANELCOLOR;
    Tag := AButtonIndex;
    Left := AButton.Left;
    Top := AButton.Top;
    Width := AButton.Width;
    Height := AButton.Height;
  end;
  ButtonPanels[AButtonIndex] := panel;

  image := TBitmapImage.Create(WizardForm);
  FileName := ExtractFileName('{#MyWizardButtonImageFile}');
  with image do begin
    Height := BUTTONHEIGHT;
    Width := BUTTONWIDTH*2;
    Bitmap.LoadFromFile(ExpandConstant('{tmp}\')+FileName);
    Parent := panel;
    Enabled := false;
  end;
  ButtonImages[AButtonIndex] := image;

  with TLabel.Create(WizardForm) do begin
    Tag := AButtonIndex;
    Parent := panel;
    Width := panel.Width;
    Height := panel.Height;
    transparent := true;
    OnClick := @ButtonLabelClick;
    OnDblClick := @ButtonLabelClick;
    OnMouseDown := @ButtonLabelMouseDown;
    OnMouseUp := @ButtonLabelMouseUp;
  end;

  labl := TLabel.Create(WizardForm);
  with labl do begin
    Tag := AButtonIndex;
    Alignment := taCenter;
    Transparent := true;
    AutoSize := false;
    Top := (BUTTONHEIGHT div 4);
    Left := 0;
    Width := panel.ClientWidth;
    Parent := panel;
    if AButtonIndex = bidNext then
      Font.Style := [fsBold];
    Font.Color := clWhite;
    Caption := AButton.Caption;
    OnClick := @ButtonLabelClick;
    OnDblClick := @ButtonLabelClick;
    OnMouseDown := @ButtonLabelMouseDown;
    OnMouseUp := @ButtonLabelMouseUp;
  end;
  ButtonLabels[AButtonIndex] := labl;
end;

procedure LicenceAcceptedRadioOnClick(Sender: TObject);
begin
  ButtonLabels[bidNext].Enabled := true;
end;

procedure LicenceNotAcceptedRadioOnClick(Sender: TObject);
begin
  ButtonLabels[bidNext].Enabled := false;
end;

procedure InitializeSkin;
var
  image: TBitmapImage;
  TypesComboItemIndex: integer;
  FileName: String;
begin
  with WizardForm do
    with OuterNotebook do
      with InnerPage do
        with InnerNotebook do
          with SelectComponentsPage do
            TypesComboItemIndex := TypesCombo.ItemIndex;


  WizardForm.Bevel.Hide;
  WizardForm.Bevel1.Hide;
  WizardForm.SelectDirBitmapImage.Hide;
  WizardForm.SelectGroupBitmapImage.Hide;

  image := TBitmapImage.Create(WizardForm);
  image.Top := 451;
  image.Width := 690;
  image.Height := 45;

  FileName := ExtractFileName('{#MyWizardBottomImageFile}');
  ExtractTemporaryFile(FileName);
  image.Bitmap.LoadFromFile(ExpandConstant('{tmp}\')+FileName);
  image.Parent := WizardForm;
  image.SendToBack;

  with WizardForm do begin
    Position := poScreenCenter;
    ClientWidth := 690
    ClientHeight := 496
    Font.Color := $ffffff
    Font.Name := 'MS Sans Serif'
    Font.Style := []
    with CancelButton do begin
      Left := 555
      Top := 460
      Width := BUTTONWIDTH;
      Height := BUTTONHEIGHT;
    end;
    with NextButton do begin
      Left := 371
      Top := 460
      Width := BUTTONWIDTH;
      Height := BUTTONHEIGHT;
    end;
    with BackButton do begin
      Left := 266
      Top := 460
      Width := BUTTONWIDTH;
      Height := BUTTONHEIGHT;
    end;

    with OuterNotebook do begin
      Left := 190
      Top := 59
      Width := 500
      Height := 392
      with WelcomePage do begin
        Color := PAGECOLOR;
        with WizardBitmapImage do begin
          Parent := WizardForm
          Left := 0
          Top := 60
          Width := 190
          Height := 391
        end;
        with WelcomeLabel2 do begin
          Left := 35
          Top := 171
          Width := 465
          Height := 200
        end;
        with WelcomeLabel1 do begin
          Left := 35
          Top := 131
          Width := 465
          Height := 28
          Font.Size := 8
          Font.Color := $ffffff
        end;
      end;
      with InnerPage do begin
        with InnerNotebook do begin
          Left := 20
          Top := 20
          Width := 465
          Height := 354
          Color := PAGECOLOR;
          with LicensePage do begin
            with LicenseNotAcceptedRadio do begin
              Left := 0
              Top := 338
              Width := 17
              Height := 17
              OnClick := @LicenceNotAcceptedRadioOnClick;
            end;
            with LicenseAcceptedRadio do begin
              Left := 0
              Top := 318
              Width := 17
              Height := 17
              OnClick := @LicenceAcceptedRadioOnClick;
            end;
            with LicenseMemo do begin
              Left := 0
              Top := 38
              Width := 465
              Height := 266
            end;
            with LicenseLabel1 do begin
              Left := 0
              Top := 0
              Width := 465
              Height := 28
            end;
          end;
          with PasswordPage do begin
            with PasswordEdit do begin
              Left := 0
              Top := 50
              Width := 465
              Height := 21
              Color := $ffffff
              Font.Color := $000000
            end;
            with PasswordEditLabel do begin
              Left := 0
              Top := 34
              Width := 465
              Height := 14
            end;
            with PasswordLabel do begin
              Left := 0
              Top := 0
              Width := 465
              Height := 28
            end;
          end;
          with InfoBeforePage do begin
            with InfoBeforeMemo do begin
              Left := 0
              Top := 24
              Width := 465
              Height := 327
            end;
            with InfoBeforeClickLabel do begin
              Left := 0
              Top := 0
              Width := 465
              Height := 14
            end;
          end;
          with UserInfoPage do begin
            with UserInfoSerialEdit do begin
              Left := 0
              Top := 120
              Width := 465
              Height := 21
              Color := $ffffff
              Font.Color := $000000
            end;
            with UserInfoSerialLabel do begin
              Left := 0
              Top := 104
              Width := 465
              Height := 14
            end;
            with UserInfoOrgEdit do begin
              Left := 0
              Top := 68
              Width := 465
              Height := 21
              Color := $ffffff
              Font.Color := $000000
            end;
            with UserInfoOrgLabel do begin
              Left := 0
              Top := 52
              Width := 465
              Height := 14
            end;
            with UserInfoNameEdit do begin
              Left := 0
              Top := 16
              Width := 465
              Height := 21
              Color := $ffffff
              Font.Color := $000000
            end;
            with UserInfoNameLabel do begin
              Left := 0
              Top := 0
              Width := 465
              Height := 14
            end;
          end;
          with SelectDirPage do begin
            with DiskSpaceLabel do begin
              Left := 0
              Top := 340
              Width := 465
              Height := 14
            end;
            with DirBrowseButton do begin
              Left := 368
              Top := 288
              Width := BUTTONWIDTH;
              Height := BUTTONHEIGHT;
            end;
            with DirEdit do begin
              Left := 0
              Top := 290
              Width := 350
              Height := 21
              Color := $ffffff
              Font.Color := $000000
            end;
            with SelectDirBrowseLabel do begin
              Left := 0
              Top := 24
              Width := 465
              Height := 28
            end;
            with SelectDirLabel do begin
              Left := 0
              Top := 0
              Width := 465
              Height := 14
            end;
          end;
          with SelectComponentsPage do begin
            with ComponentsDiskSpaceLabel do begin
              Left := 0
              Top := 340
              Width := 417
              Height := 14
            end;
            with ComponentsList do begin
              Left := 0
              Top := 62
              Width := 465
              Height := 261
              Color := $ffffff
              Font.Color := $000000
            end;
            with TypesCombo do begin
              Left := 0
              Top := 38
              Width := 465
              Height := 21
              Color := $ffffff
              Font.Color := $000000
              ItemIndex := TypesComboItemIndex;
            end;
            with SelectComponentsLabel do begin
              Left := 0
              Top := 0
              Width := 465
              Height := 28
            end;
          end;
          with SelectProgramGroupPage do begin
            with NoIconsCheck do begin
              Left := 0
              Top := 337
              Width := 17
              Height := 17
              Visible := True
            end;
            with GroupBrowseButton do begin
              Left := 368
              Top := 288
              Width := BUTTONWIDTH;
              Height := BUTTONHEIGHT;
            end;
            with GroupEdit do begin
              Left := 0
              Top := 290
              Width := 350
              Height := 21
              Color := $ffffff
              Font.Color := $000000
            end;
            with SelectStartMenuFolderBrowseLabel do begin
              Left := 0
              Top := 24
              Width := 465
              Height := 28
            end;
            with SelectStartMenuFolderLabel do begin
              Left := 0
              Top := 0
              Width := 465
              Height := 14
            end;
          end;
          with SelectTasksPage do begin
            with TasksList do begin
              Left := 0
              Top := 34
              Width := 465
              Height := 317
              Color := PAGECOLOR;
            end;
            with SelectTasksLabel do begin
              Left := 0
              Top := 0
              Width := 465
              Height := 28
            end;
          end;
          with ReadyPage do begin
            with ReadyMemo do begin
              Left := 0
              Top := 34
              Width := 465
              Height := 317
              Color := PAGECOLOR;
              //Color := $ffffff
              //Font.Color := $000000
            end;
            with ReadyLabel do begin
              Left := 0
              Top := 0
              Width := 465
              Height := 28
            end;
          end;
          with InstallingPage do begin
            with FilenameLabel do begin
              Left := 0+10
              Top := 16+10
              Width := 465
              Height := 16
            end;
            with StatusLabel do begin
              Left := 0+10
              Top := 0+10
              Width := 465
              Height := 16
            end;
            with ProgressGauge do begin
              Left := 0+10
              Top := 42+10
              Width := 465
              Height := 21
            end;
            with WebDownloadFilenameLabel do begin
              Left := 0+10
              Top := 16+10+80
              Width := 465
              Height := 16
            end;
            with WebDownloadStatusLabel do begin
              Left := 0+10
              Top := 0+10+80
              Width := 465
              Height := 16
            end;
            with WebDownloadProgressGauge do begin
              Left := 0+10
              Top := 42+10+80
              Width := 465
              Height := 21
            end;
          end;
          with InfoAfterPage do begin
            with InfoAfterMemo do begin
              Left := 0
              Top := 24
              Width := 465
              Height := 327
            end;
            with InfoAfterClickLabel do begin
              Left := 0
              Top := 0
              Width := 465
              Height := 14
            end;
          end;
        end;
        with MainPanel do begin
          Parent := WizardForm;
          color := MAINPANELCOLOR;
          Left := 0
          Top := 0
          Width := 690
          Height := 60
          with WizardSmallBitmapImage do begin
            Left := 0
            Top := 58
            Width := 690
            Height := 2
          end;
          with PageDescriptionLabel do begin
            Left := 25
            Top := 25
            Width := 500
            Height := 14
            Color := MAINPANELCOLOR;
            Font.Color := $ffffff
          end;
          with PageNameLabel do begin
            Left := 15
            Top := 7
            Width := 500
            Height := 14
            Color := MAINPANELCOLOR;
            Font.Color := $ffffff
          end;
        end;
      end;
      with FinishedPage do begin
        Color := PAGECOLOR;
        WizardBitmapImage2.hide;
        with NoRadio do begin
          Left := 35
          Top := 168
          Width := 465
          Height := 17
        end;
        with YesRadio do begin
          Left := 35
          Top := 140
          Width := 465
          Height := 17
        end;
        with RunList do begin
          Left := 35
          Top := 140
          Width := 465
          Height := 149
        end;
        with FinishedLabel do begin
          Left := 35
          Top := 60
          Width := 465
          Height := 53
        end;
        with FinishedHeadingLabel do begin
          Left := 35
          Top := 20
          Width := 465
          Height := 24
          Font.Size := 8
          Font.Color := $ffffff
        end;
      end;
    end;
  end;

  with TLabel.Create(WizardForm) do begin
    Left := 17
    Top := 320
    Width := 445
    Height := 17
    Color := clWhite;
    Transparent := true;
    Caption := WizardForm.LicenseAcceptedRadio.Caption
    Parent := WizardForm.LicensePage
  end;
  with TLabel.Create(WizardForm) do begin
    Left := 17
    Top := 320
    Width := 445
    Height := 17
    Color := clWhite;
    Transparent := true;
    Caption := WizardForm.LicenseAcceptedRadio.Caption
    Parent := WizardForm.LicensePage
  end;
  with TLabel.Create(WizardForm) do begin
    Left := 17
    Top := 340
    Width := 445
    Height := 17
    Color := clWhite;
    Transparent := true;
    Caption := WizardForm.LicenseNotAcceptedRadio.Caption
    Parent := WizardForm.LicensePage
  end;
  with TLabel.Create(WizardForm) do begin
    Left := 17
    Top := 340
    Width := 445
    Height := 17
    Color := clWhite;
    Transparent := true;
    Caption := WizardForm.NoIconsCheck.Caption
    Parent := WizardForm.SelectProgramGroupPage
  end;

  ExtractTemporaryFile(ExtractFileName('{#MyWizardButtonImageFile}'));

  LoadButtonImage(WizardForm.BackButton,bidBack);
  LoadButtonImage(WizardForm.NextButton,bidNext);
  LoadButtonImage(WizardForm.CancelButton,bidCancel);
  LoadButtonImage(WizardForm.DirBrowseButton,bidDirBrowse);
  LoadButtonImage(WizardForm.GroupBrowseButton,bidGroupBrowse);
end;
