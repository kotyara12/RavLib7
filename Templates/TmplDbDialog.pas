unit TmplDbDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDialog, StdCtrls, Buttons, ExtCtrls, DB;

type
  TDbDialogTemplate = class(TDialogTemplate)
    DataSource: TDataSource;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    function  GetEditMode: Boolean;
  protected
    procedure InitFormVariables; override;
    procedure StartForm; override;
    function  CheckOkClose: Boolean; override;
    procedure InitControls(const EditMode: Boolean); virtual;
    procedure InitComponents(const EditMode: Boolean); virtual;
    procedure SetOkBtnState; virtual;
  public
    procedure InitDbDialog;
    procedure UpdateDbControls;
    property  IsEditMode: Boolean read GetEditMode;
  end;

implementation

{$R *.dfm}

uses
  RDbUtils, RDbStatus, RDialogs, RMsgRu;

procedure TDbDialogTemplate.InitFormVariables;
begin
  inherited;
end;

{ == Запуск формы ============================================================== }
procedure TDbDialogTemplate.StartForm;
begin
  try
    inherited;
  finally
    InitDbDialog;
  end;
end;

{ == Инициализация Db - диалога ================================================ }
procedure TDbDialogTemplate.InitControls(const EditMode: Boolean);
begin
end;

procedure TDbDialogTemplate.InitComponents(const EditMode: Boolean);
begin
end;

procedure TDbDialogTemplate.SetOkBtnState;
begin
  // OkBtn.Visible := Assigned(DataSource.DataSet);
  OkBtn.Visible := IsEditMode;
  OkBtn.Enabled := IsEditMode;
end;

procedure TDbDialogTemplate.UpdateDbControls;
begin
  FillDbHints(Self);
  FillDbReadOnly(Self);
  FillDbRequired(Self);
end;

procedure TDbDialogTemplate.InitDbDialog;
begin
  try
    // Caption := GetDataSetStateText(DataSource.DataSet);
    try
      InitControls(IsEditMode);
    finally
      try
        UpdateDbControls;
      finally
        InitComponents(IsEditMode);
      end;
    end;
  finally
    SetOkBtnState;
  end;
end;

procedure TDbDialogTemplate.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := (ModalResult = mrOk) or CancelBtn.Enabled;
  if not CanClose then
    CautionBox(SErrCancelDisabled);
end;

function TDbDialogTemplate.CheckOkClose: Boolean;
begin
  Result := inherited CheckOkClose
              and CheckDbRequired(Self);
end;

function TDbDialogTemplate.GetEditMode: Boolean;
begin
  Result := Assigned(DataSource.DataSet) and (DataSource.State in [dsInsert, dsEdit]);
end;

end.
