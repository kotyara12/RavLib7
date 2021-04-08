unit RavCtlReg;

interface

procedure Register;

implementation

uses
  Classes, RavClrCombo, RavTreeView, RavTreeView_Old, RavListView,
  RavFloatEdit, RavIpEdit, RavSpin64, RavGrid;

procedure Register;
begin
  RegisterComponents('Rav Soft', [TRFloatEdit, TRIpAddrEdit, TRColorCombo,
    TRSortListView, TRTreeView, TRIDTreeView, TRKindIDTreeView, TSpinEdit64, TROwnerDrawGrid]);
end;

end.
