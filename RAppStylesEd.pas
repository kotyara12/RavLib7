unit RAppStylesEd;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ComCtrls, ExtCtrls, RFonts, RAppStyles,
  ImgList, RavClrCombo, RavTreeView, TmplDialog, RavTreeView_Old;

{ == Редактор стилей =========================================================== }
type
  TFormStyles = class(TDialogTemplate)
    ApplyBtn: TBitBtn;
    ImageList: TImageList;
    ColorDialog: TColorDialog;
    FontDialog: TFontDialog;
    TreeViewLabel: TLabel;
    PanelLabel: TLabel;
    Panel: TPanel;
    Notebook: TNotebook;
    MainWinToolCheckBox: TCheckBox;
    MainWinStatusCheckBox: TCheckBox;
    BaseTextRColorComboLabel: TLabel;
    BaseFontPanelLabel: TLabel;
    DataWinToolCheckBox: TCheckBox;
    DataWinStatusCheckBox: TCheckBox;
    DataWinInfoCheckBox: TCheckBox;
    BaseFormRColorCombo: TRColorCombo;
    BaseFormColorBtn: TBitBtn;
    BaseFontPanel: TPanel;
    BaseFontBtn: TBitBtn;
    TreeRColorComboLabel: TLabel;
    TreeTextRColorComboLabel: TLabel;
    TreeFontPanelLabel: TLabel;
    TreeRColorCombo: TRColorCombo;
    TreeTextRColorCombo: TRColorCombo;
    TreeColorBtn: TBitBtn;
    TreeTextColorBtn: TBitBtn;
    TreeFontPanel: TPanel;
    TreeFontBtn: TBitBtn;
    DataRColorComboLabel: TLabel;
    DataTextRColorComboLabel: TLabel;
    DataFontPanelLabel: TLabel;
    DataRColorCombo: TRColorCombo;
    DataColorBtn: TBitBtn;
    DataTextColorBtn: TBitBtn;
    DataTextRColorCombo: TRColorCombo;
    DataFontPanel: TPanel;
    DataFontBtn: TBitBtn;
    InfoRColorComboLabel: TLabel;
    InfoLabelTextRColorComboLabel: TLabel;
    InfoLabelFontPanelLabel: TLabel;
    InfoTextRColorComboLabel: TLabel;
    InfoTextTextColorRColorComboLabel: TLabel;
    InfoTextFontPanelLabel: TLabel;
    InfoRColorCombo: TRColorCombo;
    InfoColorBtn: TBitBtn;
    InfoLabelTextRColorCombo: TRColorCombo;
    InfoLabelTextColorBtn: TBitBtn;
    InfoLabelFontPanel: TPanel;
    InfoLabelFontBtn: TBitBtn;
    InfoTextRColorCombo: TRColorCombo;
    InfoTextColorBtn: TBitBtn;
    InfoTextTextRColorCombo: TRColorCombo;
    InfoTextTextColorBtn: TBitBtn;
    InfoTextFontPanel: TPanel;
    InfoTextFontBtn: TBitBtn;
    DataWinCtl3DCheckBox: TCheckBox;
    DataPositiveRColorComboLabel: TLabel;
    DataPositiveRColorCombo: TRColorCombo;
    DataPositiveBtn: TBitBtn;
    CellPositiveRColorComboLabel: TLabel;
    CellPositiveRColorCombo: TRColorCombo;
    CellPositiveBtn: TBitBtn;
    DataNegativeRColorComboLabel: TLabel;
    DataNegativeRColorCombo: TRColorCombo;
    DataNegativeBtn: TBitBtn;
    CellNegativeRColorComboLabel: TLabel;
    CellNegativeRColorCombo: TRColorCombo;
    CellNegativeBtn: TBitBtn;
    TreeView: TRIDTreeView;
    ButtonsPanelRColorComboLabel: TLabel;
    ButtonsPanelRColorCombo: TRColorCombo;
    ButtonsPanelBtn: TBitBtn;
    BaseTextColorComboLabel: TLabel;
    BaseTextRColorCombo: TRColorCombo;
    BaseTextColorBtn: TBitBtn;
    ResetBtn: TBitBtn;
    DataWinShowCaptionsCheckBox: TCheckBox;
    MainWinUniqueMdi: TCheckBox;
    DataWinFindCheckBox: TCheckBox;
    DataWinFindDataFocus: TCheckBox;
    DataWinFindUseLeadAsterisk: TCheckBox;
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure BaseFormColorBtnClick(Sender: TObject);
    procedure ButtonsPanelBtnClick(Sender: TObject);
    procedure BaseFontBtnClick(Sender: TObject);
    procedure BaseTextColorBtnClick(Sender: TObject);
    procedure BaseTextRColorComboChange(Sender: TObject);
    procedure BaseFormRColorComboChange(Sender: TObject);
    procedure TreeColorBtnClick(Sender: TObject);
    procedure TreeTextColorBtnClick(Sender: TObject);
    procedure TreeTextRColorComboChange(Sender: TObject);
    procedure TreeFontBtnClick(Sender: TObject);
    procedure TreeRColorComboChange(Sender: TObject);
    procedure DataColorBtnClick(Sender: TObject);
    procedure DataTextColorBtnClick(Sender: TObject);
    procedure DataRColorComboChange(Sender: TObject);
    procedure DataTextRColorComboChange(Sender: TObject);
    procedure DataFontBtnClick(Sender: TObject);
    procedure DataPositiveBtnClick(Sender: TObject);
    procedure CellPositiveBtnClick(Sender: TObject);
    procedure DataNegativeBtnClick(Sender: TObject);
    procedure CellNegativeBtnClick(Sender: TObject);
    procedure InfoColorBtnClick(Sender: TObject);
    procedure InfoLabelTextColorBtnClick(Sender: TObject);
    procedure InfoRColorComboChange(Sender: TObject);
    procedure InfoLabelTextRColorComboChange(Sender: TObject);
    procedure InfoLabelFontBtnClick(Sender: TObject);
    procedure InfoTextColorBtnClick(Sender: TObject);
    procedure InfoTextTextColorBtnClick(Sender: TObject);
    procedure InfoTextRColorComboChange(Sender: TObject);
    procedure InfoTextTextRColorComboChange(Sender: TObject);
    procedure InfoTextFontBtnClick(Sender: TObject);
    procedure ResetBtnClick(Sender: TObject);
    procedure ApplyBtnClick(Sender: TObject);
    procedure DataWinToolCheckBoxClick(Sender: TObject);
    procedure DataWinFindCheckBoxClick(Sender: TObject);
  protected
    procedure InitForm; override;
  public
    AppMainWnd: THandle;
    procedure SetStyle; override;
    procedure PutStyles(const Value: PApplicationStyle);
    procedure CutStyles(var Value: PApplicationStyle);
  end;

{ == Общие процедуры =========================================================== }
procedure LoadStyles(const GlobalIniFile: string; const Style: PApplicationStyle);
procedure SaveStyles(const GlobalIniFile: string; const Style: PApplicationStyle);
procedure SetStyles(const MainWnd: THandle);
function  EditStyles(const MainWnd: THandle): Boolean;

const
  StyleExt       = '.thm';

implementation

{$R *.DFM}

uses
  RIniFiles, RSysUtils, RDialogs, RMessages, TmplBase;

resourcestring
  SMainFormNotFound      = 'Невозможно установить тему: не найден дескриптор главного окна приложения!';
  SErrAppStyleNull       = 'Глобальная переменная ApplicationStyle не инициализрована!';

const
  iniStyles              = 'STYLES';
  iniMainForm            = 'STYLE_MAIN_WINDOWS';
  iniDataForm            = 'STYLE_DATA_WINDOWS';
  iniGlobal              = 'ApplyStylesForAllApplications';
  iniTBVisible           = 'ShowToolBar';
  iniTBCaptions          = 'ShowToolButtonCaptions';
  iniSBVisible           = 'ShowStatusBar';
  iniIPVisible           = 'ShowInfoPanel';
  iniFastFindPanel       = 'ShowFindPanel';
  iniFastFindGotoData    = 'FastFindGotoTableAfterFind';
  iniFastFindLeadAst     = 'FastFindUseLeadAsterisk';
  iniMdiUnique           = 'UniqueMdiWins';
  iniCtl3D               = 'Controls3D';
  iniBaseFormColor       = 'Base_FormColor';
  iniBasePanelColor      = 'Base_ButtonsPanelColor';
  iniBaseFont            = 'Base_Font';
  iniTreeColor           = 'Tree_Color';
  iniTreeFont            = 'Tree_Font';
  iniDataColor           = 'Data_Color';
  iniDataFont            = 'Data_Font';
  iniDataPositive        = 'Data_DataPositive';
  iniDataNegative        = 'Data_DataNegative';
  iniCellPositive        = 'Data_CellPositive';
  iniCellNegative        = 'Data_CellNegative';
  iniInfoColor           = 'Info_Color';
  iniInfoTextColor       = 'InfoText_Color';
  iniInfoTextFont        = 'InfoText_Font';
  iniInfoLabelFont       = 'InfoLabel_Font';

  imApp                  = 0;
  imTree                 = 1;
  imData                 = 2;
  imInfo                 = 3;

  idMainWindow           = 0;
  idDataWindow           = 1;
  idTree                 = 2;
  idData                 = 3;
  idInfo                 = 4;

  snMainWindow           = 'Главное окно';
  snDataWindow           = 'Рабочее окно';
  snTree                 = 'Папки (объекты)';
  snData                 = 'Данные (таблицы)';
  snInfo                 = 'Детальные сведения';

{ == Элементы главного окна программы ========================================== }
procedure LoadMainFormStyle(const IniFileName: string; var Style: TMainFormStyle);
begin
  Style.TbVisible := InitReadIniBoolean(IniFileName, iniMainForm, iniTBVisible, Style.TbVisible);
  Style.SbVisible := InitReadIniBoolean(IniFileName, iniMainForm, iniSBVisible, Style.SbVisible);
  Style.MdiUnique := InitReadIniBoolean(IniFileName, iniMainForm, iniMdiUnique, Style.MdiUnique);
end;

procedure SaveMainFormStyle(const IniFileName: string; const Style: TMainFormStyle);
begin
  SaveIniBoolean(IniFileName, iniMainForm, iniTBVisible, Style.TbVisible);
  SaveIniBoolean(IniFileName, iniMainForm, iniSBVisible, Style.SbVisible);
  SaveIniBoolean(IniFileName, iniMainForm, iniMdiUnique, Style.MdiUnique);
end;

{ == Стиль окна программы ====================================================== }
procedure LoadDataFormStyle(const IniFileName: string; var Style: TDataFormStyle);
begin
  Style.TbVisible := InitReadIniBoolean(IniFileName, iniDataForm, iniTBVisible, Style.TbVisible);
  Style.TbCaptions := InitReadIniBoolean(IniFileName, iniDataForm, iniTBCaptions, Style.TbCaptions);
  Style.SbVisible := InitReadIniBoolean(IniFileName, iniDataForm, iniSBVisible, Style.SbVisible);
  Style.FastFindPanel := InitReadIniBoolean(IniFileName, iniDataForm, iniFastFindPanel, Style.FastFindPanel);
  Style.FastFindGotoData := InitReadIniBoolean(IniFileName, iniDataForm, iniFastFindGotoData, Style.FastFindGotoData);
  Style.FastFindLeadAsterisk := InitReadIniBoolean(IniFileName, iniDataForm, iniFastFindLeadAst, Style.FastFindLeadAsterisk);
  Style.InfoPanelVisible := InitReadIniBoolean(IniFileName, iniDataForm, iniIPVisible, Style.InfoPanelVisible);
  Style.Ctl3D := InitReadIniBoolean(IniFileName, iniDataForm, iniCtl3D, Style.Ctl3D);
  Style.FormColor := InitReadIniInteger(IniFileName, iniDataForm, iniBaseFormColor, Style.FormColor);
  Style.ButtonsPanelColor := InitReadIniInteger(IniFileName, iniDataForm, iniBasePanelColor, Style.ButtonsPanelColor);
  StringToFontData(InitReadIniString(IniFileName, iniDataForm, iniBaseFont, FontDataToString(Style.FormFont)), Style.FormFont);
  Style.TreeColor := InitReadIniInteger(IniFileName, iniDataForm, iniTreeColor, Style.TreeColor);
  StringToFontData(InitReadIniString(IniFileName, iniDataForm, iniTreeFont, FontDataToString(Style.TreeFont)), Style.TreeFont);
  Style.DataColor := InitReadIniInteger(IniFileName, iniDataForm, iniDataColor, Style.DataColor);
  StringToFontData(InitReadIniString(IniFileName, iniDataForm, iniDataFont, FontDataToString(Style.DataFont)), Style.DataFont);
  Style.DataPositive := InitReadIniInteger(IniFileName, iniDataForm, iniDataPositive, Style.DataPositive);
  Style.DataNegative := InitReadIniInteger(IniFileName, iniDataForm, iniDataNegative, Style.DataNegative);
  Style.CellPositive := InitReadIniInteger(IniFileName, iniDataForm, iniCellPositive, Style.CellPositive);
  Style.CellNegative := InitReadIniInteger(IniFileName, iniDataForm, iniCellNegative, Style.CellNegative);
  Style.InfoPanelColor := InitReadIniInteger(IniFileName, iniDataForm, iniInfoColor, Style.InfoPanelColor);
  Style.InfoTextColor := InitReadIniInteger(IniFileName, iniDataForm, iniInfoTextColor, Style.InfoTextColor);
  StringToFontData(InitReadIniString(IniFileName, iniDataForm, iniInfoTextFont, FontDataToString(Style.InfoTextFont)), Style.InfoTextFont);
  StringToFontData(InitReadIniString(IniFileName, iniDataForm, iniInfoLabelFont, FontDataToString(Style.InfoLabelFont)), Style.InfoLabelFont);
end;

procedure SaveDataFormStyle(const IniFileName: string; const Style: TDataFormStyle);
begin
  SaveIniBoolean(IniFileName, iniDataForm, iniTBVisible, Style.TbVisible);
  SaveIniBoolean(IniFileName, iniDataForm, iniTBCaptions, Style.TbCaptions);
  SaveIniBoolean(IniFileName, iniDataForm, iniSBVisible, Style.SbVisible);
  SaveIniBoolean(IniFileName, iniDataForm, iniFastFindPanel, Style.FastFindPanel);
  SaveIniBoolean(IniFileName, iniDataForm, iniFastFindGotoData, Style.FastFindGotoData);
  SaveIniBoolean(IniFileName, iniDataForm, iniFastFindLeadAst, Style.FastFindLeadAsterisk);
  SaveIniBoolean(IniFileName, iniDataForm, iniIPVisible, Style.InfoPanelVisible);
  SaveIniBoolean(IniFileName, iniDataForm, iniCtl3D, Style.Ctl3D);
  SaveIniInteger(IniFileName, iniDataForm, iniBaseFormColor, Style.FormColor);
  SaveIniInteger(IniFileName, iniDataForm, iniBasePanelColor, Style.ButtonsPanelColor);
  SaveIniString(IniFileName, iniDataForm, iniBaseFont, FontDataToString(Style.FormFont));
  SaveIniInteger(IniFileName, iniDataForm, iniTreeColor, Style.TreeColor);
  SaveIniString(IniFileName, iniDataForm, iniTreeFont, FontDataToString(Style.TreeFont));
  SaveIniInteger(IniFileName, iniDataForm, iniDataColor, Style.DataColor);
  SaveIniString(IniFileName, iniDataForm, iniDataFont, FontDataToString(Style.DataFont));
  SaveIniInteger(IniFileName, iniDataForm, iniDataPositive, Style.DataPositive);
  SaveIniInteger(IniFileName, iniDataForm, iniDataNegative, Style.DataNegative);
  SaveIniInteger(IniFileName, iniDataForm, iniCellPositive, Style.CellPositive);
  SaveIniInteger(IniFileName, iniDataForm, iniCellNegative, Style.CellNegative);
  SaveIniInteger(IniFileName, iniDataForm, iniInfoColor, Style.InfoPanelColor);
  SaveIniInteger(IniFileName, iniDataForm, iniInfoTextColor, Style.InfoTextColor);
  SaveIniString(IniFileName, iniDataForm, iniInfoTextFont, FontDataToString(Style.InfoTextFont));
  SaveIniString(IniFileName, iniDataForm, iniInfoLabelFont, FontDataToString(Style.InfoLabelFont));
end;

{ == Общие процедуры =========================================================== }
procedure SetStyles(const MainWnd: THandle);
begin
  if MainWnd > 0 then
    SendMessage(MainWnd, WM_SETFORMSTYLE, ID_SETFORMSTYLE, 0)
  else
    ErrorBox(SMainFormNotFound);
end;

procedure LoadStyles(const GlobalIniFile: string; const Style: PApplicationStyle);
var
  IniFileName: string;
begin
  if GlobalIniFile <> EmptyStr
  then IniFileName := ExtractFilePath(GetApplicationFileName) + GlobalIniFile
  else IniFileName := ChangeFileExt(GetApplicationFileName, StyleExt);
  LoadMainFormStyle(IniFileName, Style^.MainForm);
  LoadDataFormStyle(IniFileName, Style^.DataForm);
end;

procedure SaveStyles(const GlobalIniFile: string; const Style: PApplicationStyle);
var
  IniFileName: string;
begin
  if GlobalIniFile <> EmptyStr
  then IniFileName := ExtractFilePath(GetApplicationFileName) + GlobalIniFile
  else IniFileName := ChangeFileExt(GetApplicationFileName, StyleExt);
  SaveMainFormStyle(IniFileName, Style^.MainForm);
  SaveDataFormStyle(IniFileName, Style^.DataForm);
end;

function EditStyles(const MainWnd: THandle): Boolean;
var
  Editor: TFormStyles;
begin
  Editor := TFormStyles.Create(Application);
  try
    Editor.AppMainWnd := MainWnd;
    Editor.PutStyles(ApplicationStyle);
    Result := Editor.ShowModal = mrOk;
    if Result then
      Editor.CutStyles(ApplicationStyle);
  finally
    Editor.Free;
  end;
end;

{ == Редактор стилей =========================================================== }

procedure TFormStyles.SetStyle;
begin
  inherited;
  TreeView.Color := ApplicationStyle.DataForm.TreeColor;
  FontDataToFont(ApplicationStyle.DataForm.TreeFont, TreeView.Font);
end;

// Инициализация формы ---------------------------------------------------------
procedure TFormStyles.InitForm;
var
  Node: TTreeNode;
begin
  inherited;
  TreeView.Items.BeginUpdate;
  try
    TreeView.Items.Clear;
    TreeView.Selected := TreeView.AddFirstNode(idMainWindow, imApp, imApp, snMainWindow);
    Node := TreeView.AddFirstNode(idDataWindow, imApp, imApp, snDataWindow);
    TreeView.AddChildNode(Node, idTree, imTree, imTree, snTree);
    TreeView.AddChildNode(Node, idData, imData, imData, snData);
    TreeView.AddChildNode(Node, idInfo, imInfo, imInfo, snInfo);
  finally
    TreeView.Items.EndUpdate;
  end;
  Node.Expand(False);
  DataWinToolCheckBoxClick(nil);
  TreeViewChange(nil, TreeView.Selected);
end;

// Перемещение по дереву -------------------------------------------------------
procedure TFormStyles.TreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  Notebook.PageIndex := TreeView.GetNodeId(Node);
end;

// Установка и считывание стилей из контролов ----------------------------------
procedure TFormStyles.PutStyles(const Value: PApplicationStyle);
begin
  // Главное окно
  MainWinToolCheckBox.Checked := Value.MainForm.TbVisible;
  MainWinStatusCheckBox.Checked := Value.MainForm.SbVisible;
  MainWinUniqueMdi.Checked := Value.MainForm.MdiUnique;
  // Окно данных
  DataWinToolCheckBox.Checked := Value.DataForm.TbVisible;
  DataWinShowCaptionsCheckBox.Checked := Value.DataForm.TbCaptions;
  DataWinStatusCheckBox.Checked := Value.DataForm.SbVisible;
  DataWinFindCheckBox.Checked := Value.DataForm.FastFindPanel;
  DataWinFindDataFocus.Checked := Value.DataForm.FastFindGotoData;
  DataWinFindUseLeadAsterisk.Checked := Value.DataForm.FastFindLeadAsterisk;
  DataWinInfoCheckBox.Checked := Value.DataForm.InfoPanelVisible;
  DataWinCtl3DCheckBox.Checked := Value.DataForm.Ctl3D;
  BaseFormRColorCombo.ColorValue := Value.DataForm.FormColor;
  ButtonsPanelRColorCombo.ColorValue := Value.DataForm.ButtonsPanelColor;
  FontDataToFont(Value.DataForm.FormFont, BaseFontPanel.Font);
  BaseTextRColorCombo.ColorValue := Value.DataForm.FormFont.Color;
  BaseFontPanel.Caption := Value.DataForm.FormFont.Name;
  // Дерево
  TreeRColorCombo.ColorValue := Value.DataForm.TreeColor;
  FontDataToFont(Value.DataForm.TreeFont, TreeFontPanel.Font);
  TreeTextRColorCombo.ColorValue := Value.DataForm.TreeFont.Color;
  TreeFontPanel.Caption := Value.DataForm.TreeFont.Name;
  // Данные
  DataRColorCombo.ColorValue := Value.DataForm.DataColor;
  FontDataToFont(Value.DataForm.DataFont, DataFontPanel.Font);
  DataTextRColorCombo.ColorValue := Value.DataForm.DataFont.Color;
  DataFontPanel.Caption := Value.DataForm.DataFont.Name;
  DataPositiveRColorCombo.ColorValue := Value.DataForm.DataPositive;
  CellPositiveRColorCombo.ColorValue := Value.DataForm.CellPositive;
  DataNegativeRColorCombo.ColorValue := Value.DataForm.DataNegative;
  CellNegativeRColorCombo.ColorValue := Value.DataForm.CellNegative;
  // Панель информации
  InfoRColorCombo.ColorValue := Value.DataForm.InfoPanelColor;
  FontDataToFont(Value.DataForm.InfoLabelFont, InfoLabelFontPanel.Font);
  InfoLabelTextRColorCombo.ColorValue := Value.DataForm.InfoLabelFont.Color;
  InfoLabelFontPanel.Caption := Value.DataForm.InfoLabelFont.Name;
  InfoTextRColorCombo.ColorValue := Value.DataForm.InfoTextColor;
  FontDataToFont(Value.DataForm.InfoTextFont, InfoTextFontPanel.Font);
  InfoTextTextRColorCombo.ColorValue := Value.DataForm.InfoTextFont.Color;
  InfoTextFontPanel.Caption := Value.DataForm.InfoTextFont.Name;
end;

procedure TFormStyles.CutStyles(var Value: PApplicationStyle);
begin
  // Главное окно
  Value.MainForm.TbVisible := MainWinToolCheckBox.Checked;
  Value.MainForm.SbVisible := MainWinStatusCheckBox.Checked;
  Value.MainForm.MdiUnique := MainWinUniqueMdi.Checked;
  // Окно данных
  Value.DataForm.TbVisible := DataWinToolCheckBox.Checked;
  Value.DataForm.TbCaptions := DataWinShowCaptionsCheckBox.Checked;
  Value.DataForm.SbVisible := DataWinStatusCheckBox.Checked;
  Value.DataForm.FastFindPanel := DataWinFindCheckBox.Checked;
  Value.DataForm.FastFindGotoData := DataWinFindDataFocus.Checked;
  Value.DataForm.FastFindLeadAsterisk := DataWinFindUseLeadAsterisk.Checked;
  Value.DataForm.InfoPanelVisible := DataWinInfoCheckBox.Checked;
  Value.DataForm.Ctl3D := DataWinCtl3DCheckBox.Checked;
  Value.DataForm.FormColor := BaseFormRColorCombo.ColorValue;
  Value.DataForm.ButtonsPanelColor := ButtonsPanelRColorCombo.ColorValue;
  Value.DataForm.FormFont := FontToFontData(BaseFontPanel.Font);
  // Дерево
  Value.DataForm.TreeColor := TreeRColorCombo.ColorValue;
  Value.DataForm.TreeFont := FontToFontData(TreeFontPanel.Font);
  // Данные
  Value.DataForm.DataColor := DataRColorCombo.ColorValue;
  Value.DataForm.DataFont := FontToFontData(DataFontPanel.Font);
  Value.DataForm.DataPositive := DataPositiveRColorCombo.ColorValue;
  Value.DataForm.CellPositive := CellPositiveRColorCombo.ColorValue;
  Value.DataForm.DataNegative := DataNegativeRColorCombo.ColorValue;
  Value.DataForm.CellNegative := CellNegativeRColorCombo.ColorValue;
  // Панель информации
  Value.DataForm.InfoPanelColor := InfoRColorCombo.ColorValue;
  Value.DataForm.InfoLabelFont := FontToFontData(InfoLabelFontPanel.Font);
  Value.DataForm.InfoTextColor := InfoTextRColorCombo.ColorValue;
  Value.DataForm.InfoTextFont := FontToFontData(InfoTextFontPanel.Font);
end;

// Обработка нажатий кнопок - окно данных --------------------------------------
procedure TFormStyles.DataWinToolCheckBoxClick(Sender: TObject);
begin
  DataWinShowCaptionsCheckBox.Enabled := DataWinToolCheckBox.Checked;
end;

procedure TFormStyles.DataWinFindCheckBoxClick(Sender: TObject);
begin
  DataWinFindDataFocus.Enabled := DataWinFindCheckBox.Checked;
  DataWinFindUseLeadAsterisk.Enabled := DataWinFindCheckBox.Checked;
end;

procedure TFormStyles.BaseFormColorBtnClick(Sender: TObject);
begin
  ColorDialog.Color := BaseFormRColorCombo.ColorValue;
  if ColorDialog.Execute then BaseFormRColorCombo.ColorValue := ColorDialog.Color;
end;

procedure TFormStyles.ButtonsPanelBtnClick(Sender: TObject);
begin
  ColorDialog.Color := ButtonsPanelRColorCombo.ColorValue;
  if ColorDialog.Execute then ButtonsPanelRColorCombo.ColorValue := ColorDialog.Color;
end;

procedure TFormStyles.BaseTextColorBtnClick(Sender: TObject);
begin
  ColorDialog.Color := BaseTextRColorCombo.ColorValue;
  if ColorDialog.Execute then BaseTextRColorCombo.ColorValue := ColorDialog.Color;
end;

procedure TFormStyles.BaseFormRColorComboChange(Sender: TObject);
begin
  BaseFontPanel.Color := BaseFormRColorCombo.ColorValue;
end;

procedure TFormStyles.BaseTextRColorComboChange(Sender: TObject);
begin
  BaseFontPanel.Font.Color := BaseTextRColorCombo.ColorValue;
end;

procedure TFormStyles.BaseFontBtnClick(Sender: TObject);
begin
  FontDialog.Font := BaseFontPanel.Font;
  if FontDialog.Execute then
  begin
    BaseFontPanel.Font := FontDialog.Font;
    BaseFontPanel.Caption := FontDialog.Font.Name;
    BaseTextRColorCombo.ColorValue := FontDialog.Font.Color;
  end;
end;

// Обработка нажатий кнопок - дерево -------------------------------------------
procedure TFormStyles.TreeColorBtnClick(Sender: TObject);
begin
  ColorDialog.Color := TreeRColorCombo.ColorValue;
  if ColorDialog.Execute then TreeRColorCombo.ColorValue := ColorDialog.Color;
end;

procedure TFormStyles.TreeTextColorBtnClick(Sender: TObject);
begin
  ColorDialog.Color := InfoTextRColorCombo.ColorValue;
  if ColorDialog.Execute then InfoTextRColorCombo.ColorValue := ColorDialog.Color;
end;

procedure TFormStyles.TreeRColorComboChange(Sender: TObject);
begin
  TreeFontPanel.Color := TreeRColorCombo.ColorValue;
end;

procedure TFormStyles.TreeTextRColorComboChange(Sender: TObject);
begin
  TreeFontPanel.Font.Color := TreeTextRColorCombo.ColorValue;
end;

procedure TFormStyles.TreeFontBtnClick(Sender: TObject);
begin
  FontDialog.Font := TreeFontPanel.Font;
  if FontDialog.Execute then
  begin
    TreeFontPanel.Font := FontDialog.Font;
    TreeFontPanel.Caption := FontDialog.Font.Name;
    TreeTextRColorCombo.ColorValue := FontDialog.Font.Color;
  end;
end;

// Обработка нажатий кнопок - данные -------------------------------------------
procedure TFormStyles.DataColorBtnClick(Sender: TObject);
begin
  ColorDialog.Color := DataRColorCombo.ColorValue;
  if ColorDialog.Execute then DataRColorCombo.ColorValue := ColorDialog.Color;
end;

procedure TFormStyles.DataTextColorBtnClick(Sender: TObject);
begin
  ColorDialog.Color := DataTextRColorCombo.ColorValue;
  if ColorDialog.Execute then DataTextRColorCombo.ColorValue := ColorDialog.Color;
end;

procedure TFormStyles.DataRColorComboChange(Sender: TObject);
begin
  DataFontPanel.Color := DataRColorCombo.ColorValue;
end;

procedure TFormStyles.DataTextRColorComboChange(Sender: TObject);
begin
  DataFontPanel.Font.Color := DataTextRColorCombo.ColorValue;
end;

procedure TFormStyles.DataFontBtnClick(Sender: TObject);
begin
  FontDialog.Font := DataFontPanel.Font;
  if FontDialog.Execute then
  begin
    DataFontPanel.Font := FontDialog.Font;
    DataFontPanel.Caption := FontDialog.Font.Name;
    DataTextRColorCombo.ColorValue := FontDialog.Font.Color;
  end;
end;

procedure TFormStyles.DataPositiveBtnClick(Sender: TObject);
begin
  ColorDialog.Color := DataPositiveRColorCombo.ColorValue;
  if ColorDialog.Execute then DataPositiveRColorCombo.ColorValue := ColorDialog.Color;
end;

procedure TFormStyles.CellPositiveBtnClick(Sender: TObject);
begin
  ColorDialog.Color := CellPositiveRColorCombo.ColorValue;
  if ColorDialog.Execute then CellPositiveRColorCombo.ColorValue := ColorDialog.Color;
end;

procedure TFormStyles.DataNegativeBtnClick(Sender: TObject);
begin
  ColorDialog.Color := DataNegativeRColorCombo.ColorValue;
  if ColorDialog.Execute then DataNegativeRColorCombo.ColorValue := ColorDialog.Color;
end;

procedure TFormStyles.CellNegativeBtnClick(Sender: TObject);
begin
  ColorDialog.Color := CellNegativeRColorCombo.ColorValue;
  if ColorDialog.Execute then CellNegativeRColorCombo.ColorValue := ColorDialog.Color;
end;

// Обработка нажатий кнопок - панель информации --------------------------------
procedure TFormStyles.InfoColorBtnClick(Sender: TObject);
begin
  ColorDialog.Color := InfoRColorCombo.ColorValue;
  if ColorDialog.Execute then InfoRColorCombo.ColorValue := ColorDialog.Color;
end;

procedure TFormStyles.InfoLabelTextColorBtnClick(Sender: TObject);
begin
  ColorDialog.Color := InfoTextRColorCombo.ColorValue;
  if ColorDialog.Execute then InfoTextRColorCombo.ColorValue := ColorDialog.Color;
end;

procedure TFormStyles.InfoRColorComboChange(Sender: TObject);
begin
  InfoLabelFontPanel.Color := InfoRColorCombo.ColorValue;
end;

procedure TFormStyles.InfoLabelTextRColorComboChange(Sender: TObject);
begin
  InfoLabelFontPanel.Font.Color := InfoLabelTextRColorCombo.ColorValue;
end;

procedure TFormStyles.InfoLabelFontBtnClick(Sender: TObject);
begin
  FontDialog.Font := InfoLabelFontPanel.Font;
  if FontDialog.Execute then
  begin
    InfoLabelFontPanel.Font := FontDialog.Font;
    InfoLabelFontPanel.Caption := FontDialog.Font.Name;
    InfoLabelTextRColorCombo.ColorValue := FontDialog.Font.Color;
  end;
end;

procedure TFormStyles.InfoTextColorBtnClick(Sender: TObject);
begin
  ColorDialog.Color := InfoTextRColorCombo.ColorValue;
  if ColorDialog.Execute then InfoTextRColorCombo.ColorValue := ColorDialog.Color;
end;

procedure TFormStyles.InfoTextTextColorBtnClick(Sender: TObject);
begin
  ColorDialog.Color := InfoTextTextRColorCombo.ColorValue;
  if ColorDialog.Execute then InfoTextTextRColorCombo.ColorValue := ColorDialog.Color;
end;

procedure TFormStyles.InfoTextRColorComboChange(Sender: TObject);
begin
  InfoTextFontPanel.Color := InfoTextRColorCombo.ColorValue;
end;

procedure TFormStyles.InfoTextTextRColorComboChange(Sender: TObject);
begin
  InfoTextFontPanel.Font.Color := InfoTextTextRColorCombo.ColorValue;
end;

procedure TFormStyles.InfoTextFontBtnClick(Sender: TObject);
begin
  FontDialog.Font := InfoTextFontPanel.Font;
  if FontDialog.Execute then
  begin
    InfoTextFontPanel.Font := FontDialog.Font;
    InfoTextFontPanel.Caption := FontDialog.Font.Name;
    InfoTextTextRColorCombo.ColorValue := FontDialog.Font.Color;
  end;
end;

// Установить параметры по умолчанию -------------------------------------------
procedure TFormStyles.ResetBtnClick(Sender: TObject);
begin
  InitStyles(ApplicationStyle);
  PutStyles(ApplicationStyle);
  SetStyle;
  SetStyles(AppMainWnd);
end;

// Применить введенные данные --------------------------------------------------
procedure TFormStyles.ApplyBtnClick(Sender: TObject);
var
  Temp: TApplicationStyle;
begin
  Temp := ApplicationStyle^;
  try
    CutStyles(ApplicationStyle);
    SetStyle;
    SetStyles(AppMainWnd);
  finally
    ApplicationStyle^ := Temp;
  end;
end;

end.
