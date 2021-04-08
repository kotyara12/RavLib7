unit RSelProcMode;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDialog, StdCtrls, Buttons, ExtCtrls;

type
  TFormSelectMode = class(TDialogTemplate)
    ModeComboBox: TComboBox;
    ModeComboBoxLabel: TLabel;
    NumTextLabel: TLabel;
    NumText: TStaticText;
    procedure ModeComboBoxChange(Sender: TObject);
  private
  public
    FAllNum: Integer;
    FSelNum: Integer;
  end;

function SelectProcessMode(const AllCount, SelCount: Integer; var SelMode: Boolean): Boolean;

implementation

{$R *.dfm}

function SelectProcessMode(const AllCount, SelCount: Integer; var SelMode: Boolean): Boolean;
begin
  with TFormSelectMode.Create(Application) do
  begin
    try
      FAllNum := AllCount;
      FSelNum := SelCount;
      if FSelNum > 1
      then ModeComboBox.ItemIndex := 1
      else ModeComboBox.ItemIndex := 0;
      ModeComboBoxChange(nil);
      Result := ShowModal = mrOk;
      SelMode := ModeComboBox.ItemIndex = 1;
    finally
      Free;
    end;
  end;
end;

procedure TFormSelectMode.ModeComboBoxChange(Sender: TObject);
begin
  case ModeComboBox.ItemIndex of
    0: NumText.Caption := IntToStr(FAllNum);
    1: NumText.Caption := IntToStr(FSelNum);
  end;
end;

end.
