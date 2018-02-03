unit NtSvcDemoDependent;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, WinSvc, JclSvcCtrl, ComCtrls;

type
  TfrmDependent = class(TForm)
    boxDependOn: TGroupBox;
    boxDependBy: TGroupBox;
    btnOK: TBitBtn;
    treeDependOn: TTreeView;
    treeDependBy: TTreeView;
    procedure treeDependDblClick(Sender: TObject);
  private
    m_SelectedSvc: TJclNtService;

    procedure ShowDependent(const NtSvc: TJclNtService);
  public
    class function Execute(const NtSvc: TJclNtService): TJclNtService;
  end;

implementation

uses NtSvcDemoGroups;

{$R *.DFM}

{ TfrmDependent }

class function TfrmDependent.Execute(const NtSvc: TJclNtService): TJclNtService;
begin
  with TfrmDependent.Create(nil) do
  try
    ShowDependent(NtSvc);

    m_SelectedSvc := nil;
    ShowModal;
    Result := m_SelectedSvc;
  finally
    Free;
  end;
end;

procedure TfrmDependent.ShowDependent(const NtSvc: TJclNtService);
var
  I, J: Integer;
  Node: TTreeNode;
  SvcGrp: TJclServiceGroup;
begin
  treeDependOn.ShowLines := NtSvc.DependentGroupCount <> 0;
  treeDependOn.ShowRoot  := NtSvc.DependentGroupCount <> 0;

  for I:=0 to NtSvc.DependentGroupCount-1 do
  begin
    SvcGrp := NtSvc.DependentGroups[I];
    Node := treeDependOn.Items.AddObject(nil, SvcGrp.Name, SvcGrp);
    for J:=0 to SvcGrp.ServiceCount-1 do
      treeDependOn.Items.AddChildObject(Node,
        SvcGrp.Services[J].ServiceName, SvcGrp.Services[J]);
  end;

  for I:=0 to NtSvc.DependentServiceCount-1 do
    treeDependOn.Items.AddObject(nil, NtSvc.DependentServices[I].ServiceName,
      NtSvc.DependentServices[I]);

  for I:=0 to NtSvc.DependentByServiceCount-1 do
    treeDependBy.Items.AddObject(nil, NtSvc.DependentByServices[I].ServiceName,
      NtSvc.DependentByServices[I]);

  treeDependOn.FullExpand;
  treeDependBy.FullExpand;
end;

procedure TfrmDependent.treeDependDblClick(Sender: TObject);
begin
  with TTreeView(Sender) do
  if Assigned(Selected) then
    if TObject(Selected.Data).ClassType = TJclNtService then
    begin
      m_SelectedSvc := TJclNtService(Selected.Data);
      Close;
    end;
end;

end.
