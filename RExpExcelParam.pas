unit RExpExcelParam;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDialog, DB, StdCtrls, Buttons, ExtCtrls, RavFloatEdit;

type
  TFormExpExcelParam = class(TDialogTemplate)
    CaptionEdit: TEdit;
    CaptionEditLabel: TLabel;
    AlignComboBox: TComboBox;
    AlignComboBoxLabel: TLabel;
    WidthEditLabel: TLabel;
    WidthEdit: TRFloatEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

end.
