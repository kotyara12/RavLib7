unit SettingsProp;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDialog, StdCtrls, Buttons, ExtCtrls, Spin, ComCtrls,
  RavFloatEdit;

type
  TFormSettingsProp = class(TDialogTemplate)
    lblName: TLabel;
    lblDefault: TLabel;
    edDefault: TEdit;
    lblValue: TLabel;
    edInt: TSpinEdit;
    edReal: TRFloatEdit;
    edChar: TEdit;
    edDateTime: TDateTimePicker;
    edBoolean: TComboBox;
    btnOpen: TSpeedButton;
    edName: TMemo;
    procedure btnOpenClick(Sender: TObject);
  protected
    procedure InitFormVariables; override;
  public
  end;

implementation

{$R *.dfm}

uses
  RMsgRu, FileCtrl;

procedure TFormSettingsProp.InitFormVariables;
begin
  inherited;
  edBoolean.Items.BeginUpdate;
  try
    edBoolean.Items.Add(SBooleanOn);
    edBoolean.Items.Add(SBooleanOff);
  finally
    edBoolean.Items.EndUpdate;
  end;
end;

procedure TFormSettingsProp.btnOpenClick(Sender: TObject);
var
  Value: string;
  Dialog: TOpenDialog;
begin
  if btnOpen.Tag > 0 then begin
    if SelectDirectory(SSelectDirectory, EmptyStr, Value)
    then edChar.Text := Value;
  end
  else begin
    Dialog := TOpenDialog.Create(Self);
    try
      Dialog.Title := SSelectFile;
      Dialog.Options := [ofReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing];
      Dialog.Filter := SAllFilesFilter;
      if Dialog.Execute then edChar.Text := Dialog.FileName;
    finally
      Dialog.Free;
    end;
  end;
end;

end.
