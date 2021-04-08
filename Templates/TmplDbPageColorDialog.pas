unit TmplDbPageColorDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDbDialog, DB, StdCtrls, Buttons, ExtCtrls, ComCtrls;

type
  TDbPageColorDialogTemplate = class(TDbDialogTemplate)
    ColorsGroupBox: TGroupBox;
    TestText: TStaticText;
    CellColorCheckBox: TCheckBox;
    CellColorBtn: TBitBtn;
    FontColorBtn: TBitBtn;
    FontColorCheckBox: TCheckBox;
    FontStyleCheckBox: TCheckBox;
    BoldCheckBox: TCheckBox;
    ItalicCheckBox: TCheckBox;
    UnderlineCheckBox: TCheckBox;
    StrikeCheckBox: TCheckBox;
    ColorDialog: TColorDialog;
    PageControl: TPageControl;
    tsProperties: TTabSheet;
    procedure DecodeColors(Sender: TObject);
    procedure SetCellColor(Sender: TObject);
    procedure CellColorBtnClick(Sender: TObject);
    procedure SetFontColor(Sender: TObject);
    procedure FontColorBtnClick(Sender: TObject);
    procedure SetFontStyle(Sender: TObject);
    procedure SetFontStyleProp(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FCellColorField: TField;
    FFontColorField: TField;
    FFontStyleField: TField;
    FCellColor: Integer;
    FFontColor: Integer;
    FFontStyle: Integer;
    procedure PutCellColor;
    procedure PutFontColor;
    procedure PutFontStyle;
    procedure UpdateControls(Sender: TObject);
  protected
    procedure InitFormVariables; override;
  public
    procedure InitComponents(const EditMode: Boolean); override;
  end;

implementation

{$R *.dfm}

uses
  RDbConst, RDialogs;

procedure TDbPageColorDialogTemplate.FormCreate(Sender: TObject);
begin
  inherited;
  TestText.ControlStyle := TestText.ControlStyle - [csParentBackground] + [csOpaque];
end;

{ == Инициализация переменных ================================================== }
procedure TDbPageColorDialogTemplate.InitFormVariables;
begin
  inherited;
  FCellColorField := nil;
  FFontColorField := nil;
  FFontStyleField := nil;
  FCellColor := clWindow;
  FFontColor := clWindowText;
  FFontStyle := 0;
end;

{ == Инициализация компонентов ================================================= }
procedure TDbPageColorDialogTemplate.InitComponents(const EditMode: Boolean);
begin
  try
    inherited;
  finally
    if Assigned(DataSource.DataSet) then
    begin
      FCellColorField := DataSource.DataSet.FindField(fnCELLCOLOR);
      FFontColorField := DataSource.DataSet.FindField(fnFONTCOLOR);
      FFontStyleField := DataSource.DataSet.FindField(fnFONTSTYLE);
    end;
   DecodeColors(Self);
 end;
end;

{ == Установка доступности кнопок ============================================== }
procedure TDbPageColorDialogTemplate.UpdateControls(Sender: TObject);
begin
  inherited;
  CellColorCheckBox.Enabled := IsEditMode and Assigned(FCellColorField) and not FCellColorField.ReadOnly;
  CellColorBtn.Enabled := CellColorCheckBox.Enabled and CellColorCheckBox.Checked;
  FontColorCheckBox.Enabled := IsEditMode and Assigned(FFontColorField) and not FFontColorField.ReadOnly;
  FontColorBtn.Enabled := FontColorCheckBox.Enabled and FontColorCheckBox.Checked;
  FontStyleCheckBox.Enabled := IsEditMode and Assigned(FFontStyleField) and not FFontStyleField.ReadOnly;
  BoldCheckBox.Enabled := FontStyleCheckBox.Enabled and FontStyleCheckBox.Checked;
  ItalicCheckBox.Enabled := FontStyleCheckBox.Enabled and FontStyleCheckBox.Checked;
  UnderlineCheckBox.Enabled := FontStyleCheckBox.Enabled and FontStyleCheckBox.Checked;
  StrikeCheckBox.Enabled := FontStyleCheckBox.Enabled and FontStyleCheckBox.Checked;
end;

{ == Декодирование параметров и запись в элементы управления =================== }
procedure TDbPageColorDialogTemplate.DecodeColors(Sender: TObject);
begin
  FCellColor := clWindow;
  FFontColor := clWindowText;
  FFontStyle := 0;
  try
    if Assigned(FCellColorField) then begin
      CellColorCheckBox.OnClick := nil;
      try
        CellColorCheckBox.Checked := FCellColorField.AsInteger > -1;
        if CellColorCheckBox.Checked then
          FCellColor := FCellColorField.AsInteger;
      finally
        CellColorCheckBox.OnClick := SetCellColor;
      end;
    end;
    if Assigned(FFontColorField) then begin
      FontColorCheckBox.OnClick := nil;
      try
        FontColorCheckBox.Checked := FFontColorField.AsInteger > -1;
        if FontColorCheckBox.Checked then
          FFontColor := FFontColorField.AsInteger;
      finally
        FontColorCheckBox.OnClick := SetFontColor;
      end;
    end;
    if Assigned(FFontStyleField) then begin
      FontStyleCheckBox.OnClick := nil;
      try
        FontStyleCheckBox.Checked := FFontStyleField.AsInteger > -1;
        if FontStyleCheckBox.Checked then
          FFontStyle := FFontStyleField.AsInteger;
      finally
        FontStyleCheckBox.OnClick := SetFontStyle;
      end;
    end;
  finally
    PutCellColor;
    PutFontColor;
    PutFontStyle;
    UpdateControls(Sender);
  end;
end;

{ == Смена цвета фона ========================================================== }
procedure TDbPageColorDialogTemplate.PutCellColor;
begin
  if CellColorCheckBox.Checked
  then TestText.Color := FCellColor
  else TestText.Color := clWindow;
end;

procedure TDbPageColorDialogTemplate.SetCellColor(Sender: TObject);
begin
  if Assigned(FCellColorField) then
  begin
    if IsEditMode and not FCellColorField.ReadOnly then
    begin
      if CellColorCheckBox.Checked
      then FCellColorField.AsInteger := FCellColor
      else FCellColorField.AsInteger := -1;
      PutCellColor;
      UpdateControls(Sender);
    end;
  end;
end;

procedure TDbPageColorDialogTemplate.CellColorBtnClick(Sender: TObject);
begin
  ColorDialog.Color := FCellColor;
  if ColorDialog.Execute then
  begin
    FCellColor := ColorDialog.Color;
    SetCellColor(Sender);
  end;
end;

{ == Смена цвета текста ======================================================== }
procedure TDbPageColorDialogTemplate.PutFontColor;
begin
  if FontColorCheckBox.Checked
  then TestText.Font.Color := FFontColor
  else TestText.Font.Color := clWindowText;
end;

procedure TDbPageColorDialogTemplate.SetFontColor(Sender: TObject);
begin
  if Assigned(FFontColorField) then
  begin
    if IsEditMode and not FFontColorField.ReadOnly then
    begin
      if FontColorCheckBox.Checked
      then FFontColorField.AsInteger := FFontColor
      else FFontColorField.AsInteger := -1;
      PutFontColor;
      UpdateControls(Sender);
    end;
  end;
end;

procedure TDbPageColorDialogTemplate.FontColorBtnClick(Sender: TObject);
begin
  ColorDialog.Color := FFontColor;
  if ColorDialog.Execute then
  begin
    FFontColor := ColorDialog.Color;
    SetFontColor(Sender);
  end;
end;

{ == Смена стиля шрифта ======================================================== }
procedure TDbPageColorDialogTemplate.PutFontStyle;
begin
  TestText.Font.Style := [];
  if FontStyleCheckBox.Checked then
  begin
    BoldCheckBox.OnClick := nil;
    ItalicCheckBox.OnClick := nil;
    UnderlineCheckBox.OnClick := nil;
    StrikeCheckBox.OnClick := nil;
    try
      BoldCheckBox.Checked := (FFontStyle and 1) > 0;
      ItalicCheckBox.Checked := (FFontStyle and 2) > 0;
      UnderlineCheckBox.Checked := (FFontStyle and 4) > 0;
      StrikeCheckBox.Checked := (FFontStyle and 8) > 0;
    finally
      BoldCheckBox.OnClick := SetFontStyleProp;
      ItalicCheckBox.OnClick := SetFontStyleProp;
      UnderlineCheckBox.OnClick := SetFontStyleProp;
      StrikeCheckBox.OnClick := SetFontStyleProp;
    end;
    if BoldCheckBox.Checked then
      TestText.Font.Style := TestText.Font.Style + [fsBold];
    if ItalicCheckBox.Checked then
      TestText.Font.Style := TestText.Font.Style + [fsItalic];
    if UnderlineCheckBox.Checked then
      TestText.Font.Style := TestText.Font.Style + [fsUnderline];
    if StrikeCheckBox.Checked then
      TestText.Font.Style := TestText.Font.Style + [fsStrikeOut];
  end;
end;

procedure TDbPageColorDialogTemplate.SetFontStyle(Sender: TObject);
begin
  if Assigned(FFontStyleField) then
  begin
    if IsEditMode and not FFontStyleField.ReadOnly then
    begin
      if FontStyleCheckBox.Checked
      then FFontStyleField.AsInteger := FFontStyle
      else FFontStyleField.AsInteger := -1;
      PutFontStyle;
      UpdateControls(Sender);
    end;
  end;
end;

procedure TDbPageColorDialogTemplate.SetFontStyleProp(Sender: TObject);
begin
  FFontStyle := 0;
  if BoldCheckBox.Checked then FFontStyle := FFontStyle + 1;
  if ItalicCheckBox.Checked then FFontStyle := FFontStyle + 2;
  if UnderlineCheckBox.Checked then FFontStyle := FFontStyle + 4;
  if StrikeCheckBox.Checked then FFontStyle := FFontStyle + 8;
  SetFontStyle(Sender);
end;

end.
