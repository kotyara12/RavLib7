unit OrderDelivsProp;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplAuditDialog, DB, StdCtrls, Mask, DBCtrls, ComCtrls, Buttons,
  ExtCtrls;

type
  TFormOrderDelivsProp = class(TDbAuditDialogTemplate)
    SuppDataSource: TDataSource;
  private
    { Private declarations }
  public
    procedure InitComponents(const EditMode: Boolean); override;
  end;

implementation

{$R *.dfm}

uses
  BaseDbUnit, OprList, OrderDelivsForm;

{ TFormOrderDelivsProp }

procedure TFormOrderDelivsProp.InitComponents(const EditMode: Boolean);
begin
  inherited;

  btnSuppliers.Enabled := EditMode and not deSuppliers.Field.ReadOnly;

  deDelivNum.OnChange := DelivNumChange;
  deDateDeliv.OnChange := DateDelivChange;
  deCntDeliv.OnChange := CntDelivChange;

  dePriceSupp.OnExit := ChangePrice;

  PageControlChange(nil);
end;

end.
