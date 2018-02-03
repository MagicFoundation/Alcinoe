unit NtSvcDemoGroups;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, Buttons, JclSvcCtrl;

type
  TfrmServiceGroups = class(TForm)
    treeServices: TTreeView;
    btnOK: TBitBtn;
    procedure treeServicesDblClick(Sender: TObject);
  private
    m_SelectedSvc: TJclNtService;

    procedure ShowGroups(const NtSvc: TJclNtService);
  public
    class function Execute(const NtSvc: TJclNtService): TJclNtService;
  end;

implementation

{$R *.DFM}

{ TfrmServiceGroups }

class function TfrmServiceGroups.Execute(const NtSvc: TJclNtService): TJclNtService;
begin
  with TfrmServiceGroups.Create(nil) do
  try
    ShowGroups(NtSvc);

    m_SelectedSvc := nil;
    ShowModal;
    Result := m_SelectedSvc;
  finally
    Free;
  end;
end;

procedure TfrmServiceGroups.ShowGroups(const NtSvc: TJclNtService);
var
  GrpIdx, SvcIdx: Integer;
  GrpNode, SvcNode: TTreeNode;
  CurGrp: TJclServiceGroup;
  CurNtSvc: TJclNtService;
begin
  with NtSvc.SCManager do
  for GrpIdx:=0 to GroupCount-1 do
  begin
    CurGrp  := Groups[GrpIdx];

    if CurGrp.Name = '' then Continue;

    GrpNode := treeServices.Items.AddChildObject(nil, CurGrp.Name, CurGrp);
    for SvcIdx:=0 to CurGrp.ServiceCount-1 do
    begin
      CurNtSvc := CurGrp.Services[SvcIdx];
      SvcNode  := treeServices.Items.AddChildObject(GrpNode, CurNtSvc.ServiceName, CurNtSvc);
      if NtSvc = CurNtSvc then
        treeServices.Selected := SvcNode;
    end;
  end;
end;

procedure TfrmServiceGroups.treeServicesDblClick(Sender: TObject);
begin
  if Assigned(treeServices.Selected) and (treeServices.Selected.Level = 1) then
  begin
    m_SelectedSvc := TJclNtService(treeServices.Selected.Data);
    Close;
  end;
end;

end.
