unit TmplAuditDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplTabDialog, DB, ComCtrls, StdCtrls, Buttons, ExtCtrls,
  RDbText, Mask, DBCtrls;

type
  TDbAuditDialogTemplate = class(TDbTabDialogTemplate)
    tsProperty: TTabSheet;
    tsAudit: TTabSheet;
    lblCreated: TLabel;
    lblChanged: TLabel;
    deCreated: TDBEdit;
    deCreator: TDBEdit;
    deChanged: TDBEdit;
    deChanger: TDBEdit;
  private
    { Private declarations }
  public
    procedure InitComponents(const EditMode: Boolean); override;
  end;

implementation

uses BaseDbUnit;

{$R *.dfm}

{ TDbAuditDialogTemplate }

procedure TDbAuditDialogTemplate.InitComponents(const EditMode: Boolean);
begin
  inherited;
  tsAudit.TabVisible := DataSource.State <> dsInsert;
end;

end.
