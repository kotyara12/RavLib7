unit RptLoadForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDbDialog, DB, StdCtrls, Buttons, ExtCtrls, Grids, DBGrids;

type
  TFormRptLoad = class(TDbDialogTemplate)
    DBGrid: TDBGrid;
    DBGridLabel: TLabel;
    procedure DBGridDblClick(Sender: TObject);
    procedure DataSourceDataChange(Sender: TObject; Field: TField);
  private
  protected
    procedure SetOkBtnState; override;
  public
  end;

implementation

{$R *.dfm}

uses
  ReportsForm;

procedure TFormRptLoad.DataSourceDataChange(Sender: TObject; Field: TField);
begin
  OkBtn.Enabled := (DataSource.DataSet <> nil) and DataSource.DataSet.Active
    and (DataSource.State = dsBrowse) and not DataSource.DataSet.IsEmpty;
end;

procedure TFormRptLoad.DBGridDblClick(Sender: TObject);
begin
  if OkBtn.Enabled then OkBtn.Click;
end;

procedure TFormRptLoad.SetOkBtnState;
begin
  OkBtn.Visible := True;
end;

end.
