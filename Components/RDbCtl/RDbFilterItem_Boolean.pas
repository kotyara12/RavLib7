unit RDbFilterItem_Boolean;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, RDbFilterItem, StdCtrls, Buttons, ExtCtrls;

type
  TFormDbFilterItem_Boolean = class(TFormDbFilterItem)
    ModeLabel: TLabel;
    ModeComboBox: TComboBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

end.
