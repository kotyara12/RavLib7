unit RFrmStorage;

interface

uses
  Forms, Graphics, ExtCtrls, Grids, ComCtrls, RavTreeView, RVclUtils;

{ == Сохранение произвольных параметров формы ================================== }
procedure SaveFormBoolean(Form: TForm; const ParamName: string; Value: Boolean);
function  LoadFormBoolean(Form: TForm; const ParamName: string; Value: Boolean): Boolean;

{ == Сохранение позиции формы ================================================== }
procedure SaveFormPosition(Form: TForm);
procedure LoadFormPosition(Form: TForm; const LoadState, LoadPosition: Boolean);

{ == Сохранение произвольного параметра ======================================== }
procedure SaveFormParameterInt(Form: TForm; const ParamName: string; const Value: Integer);
function  LoadFormParameterInt(Form: TForm; const ParamName: string; const Value: Integer): Integer;
procedure SaveFormParameterStr(Form: TForm; const ParamName: string; const Value: string);
function  LoadFormParameterStr(Form: TForm; const ParamName: string; const Value: string): string;

{ == Сохранение панелей TCoolBar =============================================== }
procedure SaveCoolBar(Form: TForm; CoolBar: TCoolBar);
procedure LoadCoolBar(Form: TForm; CoolBar: TCoolBar);

{ == Сохранение ширины столбцов TCustomGrid ====================================== }
procedure SaveGridColumns(Form: TForm; Grid: TDrawGrid);
procedure LoadGridColumns(Form: TForm; Grid: TDrawGrid);

{ == Сохранение ширины столбцов TListView ====================================== }
procedure SaveListColumns(Form: TForm; ListView: TListView; const SaveState: Boolean = True);
procedure LoadListColumns(Form: TForm; ListView: TListView; const LoadState: Boolean = True);

{ == Сохранение параметров TTreeView =========================================== }
procedure SaveTreeViewParams(Form: TForm; TreeView: TRTreeView; const StoreExpMode: Boolean);
procedure LoadTreeViewParams(Form: TForm; TreeView: TRTreeView; var StoreExpMode: Boolean);

procedure SaveTreeViewParamsExt(Form: TForm; TreeView: TRTreeView;
  const StoreExpMode, StorePosition: Boolean; const LastKey: RNodeData);
procedure LoadTreeViewParamsExt(Form: TForm; TreeView: TRTreeView;
  var StoreExpMode, StorePosition: Boolean; var LastKey: RNodeData);

{ == Сохранение ширины панелей с изменяемой шириной ============================ }
procedure SavePanelSize(Form: TForm; Panel: TCustomPanel);
procedure LoadPanelSize(Form: TForm; Panel: TCustomPanel; const LoadHeight, LoadWidth, LoadState: Boolean);

{ == Сохранение шрифта ========================================================= }
procedure SaveFontData(Form: TForm; const ItemName: string; Font: TFont);
procedure LoadFontData(Form: TForm; const ItemName: string; Font: TFont);

implementation

uses
  SysUtils, IniFiles, Classes, Controls, Windows, Messages,
  RSysUtils, RxStrUtils, RDialogs, RavListView;

const
  SMDISuffix     = '.MDIChildren';
  SFormPos       = '%d,%d,%d,%d';
  SMultiSelect   = '%s.MultiSelect';
  SGridColumn    = '%s.Column%d';
  SListGridLines = '%s.GridLines';
  SListColumn    = '%s.Column%d';
  SCoolBar       = '%s.Band%d';
  SCoolBand      = '%s,%s,%s,%d';
  SRestPosition  = '%s.RestorePosition';
  SRestExpMode   = '%s.RestoreExpMode';
  SRootOnly      = '%s.ShowRootOnly';
  SSortType      = '%s.SortType';
  SExpMode       = '%s.AutoExpMode';
  SLastNode      = '%s.SelectedNode';
  SSizeItem      = '%s.Size';
  SSizeValues    = '%d,%d,%s';
  SSortValues    = '%d,%d';
  SItemValues    = '%d,%d';
  SFontValues    = '%s,%d,%d';


const
  siFlags        = 'Flags';
  siShowCmd      = 'ShowCmd';
  siMinMaxPos    = 'MinMaxPos';
  siMinMaxPosR   = 'MinMaxPos_%dx%d';
  siNormPos      = 'NormPos';
  siNormPosR     = 'NormPos_%dx%d';
  siPixels       = 'PixelsPerInch';
  siMDIChild     = 'MDI Children';
  siListCount    = 'Count';
  siItem         = 'Item%d';
  siFont         = 'Font%s';
  siMultiSelect  = 'MultiSelect';

const
  Delims = [',',' '];
  Delim  = [','];

{ == Генерация имени секции ==================================================== }
function GetFormSection(Form: TForm): string;
begin
  Result := Form.ClassName;
  if Form.FormStyle = fsMDIChild then Result := Result + SMDISuffix;
end;

{ == Сохранение произвольных параметров формы ================================== }
procedure SaveFormBoolean(Form: TForm; const ParamName: string; Value: Boolean);
var
  Ini: TMemIniFile;
begin
  Ini := TMemIniFile.Create(GetModuleFormFile);
  try
    Ini.WriteBool(GetFormSection(Form), ParamName, Value);
    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;

function LoadFormBoolean(Form: TForm; const ParamName: string; Value: Boolean): Boolean;
var
  Ini: TMemIniFile;
begin
  Ini := TMemIniFile.Create(GetModuleFormFile);
  try
    Result := Ini.ReadBool(GetFormSection(Form), ParamName, Value);
  finally
    Ini.Free;
  end;
end;

{ == Сохранение позиции формы ================================================== }
procedure SaveFormPosition(Form: TForm);
var
  Section: string;
  Placement: TWindowPlacement;
  Ini: TMemIniFile;
begin
  Placement.Length := SizeOf(TWindowPlacement);
  if GetWindowPlacement(Form.Handle, @Placement) then
  begin
    Ini := TMemIniFile.Create(GetModuleFormFile);
    try
      // Генерируем имя секции
      Section := GetFormSection(Form);
      // Устанавливаем свойства формы
      if (Form = Application.MainForm) and IsIconic(Application.Handle) then
        Placement.ShowCmd := SW_SHOWMINIMIZED;
      if (Form.FormStyle = fsMDIChild) and (Form.WindowState = wsMinimized) then
        Placement.Flags := Placement.Flags or WPF_SETMINPOSITION;
      // Сохраняем свойства формы
      Ini.WriteInteger(Section, siFlags, Placement.Flags);
      Ini.WriteInteger(Section, siShowCmd, Placement.ShowCmd);
      Ini.WriteInteger(Section, siPixels, Screen.PixelsPerInch);
      Ini.WriteString(Section, siMinMaxPos,
         Format(SFormPos, [Placement.ptMinPosition.X, Placement.ptMinPosition.Y,
         Placement.ptMaxPosition.X, Placement.ptMaxPosition.Y]));
      Ini.WriteString(Section, Format(siMinMaxPosR, [Screen.Width, Screen.Height]),
         Format(SFormPos, [Placement.ptMinPosition.X, Placement.ptMinPosition.Y,
         Placement.ptMaxPosition.X, Placement.ptMaxPosition.Y]));
      Ini.WriteString(Section, siNormPos,
         Format(SFormPos, [Placement.rcNormalPosition.Left, Placement.rcNormalPosition.Top,
         Placement.rcNormalPosition.Right, Placement.rcNormalPosition.Bottom]));
      Ini.WriteString(Section, Format(siNormPosR, [Screen.Width, Screen.Height]),
         Format(SFormPos, [Placement.rcNormalPosition.Left, Placement.rcNormalPosition.Top,
         Placement.rcNormalPosition.Right, Placement.rcNormalPosition.Bottom]));
      Ini.UpdateFile;
    finally
      Ini.Free;
    end;
  end;
end;

{ == Восстановление позиции формы ============================================== }
{$HINTS OFF}

type
  TNastyForm = class(TScrollingWinControl)
  private
    FActiveControl: TWinControl;
    FFocusedControl: TWinControl;
    FBorderIcons: TBorderIcons;
    FBorderStyle: TFormBorderStyle;
    FSizeChanging: Boolean;
    FWindowState: TWindowState; { !! }
  end;

  THackComponent = class(TComponent);

{$HINTS ON}

procedure LoadFormPosition(Form: TForm; const LoadState, LoadPosition: Boolean);
var
  Section, PosStr: string;
  Placement: TWindowPlacement;
  WinState: TWindowState;
  DataFound, UpdateForm: Boolean;
  Ini: TMemIniFile;
begin
  if LoadState or LoadPosition then
  begin
    Placement.Length := SizeOf(TWindowPlacement);
    if GetWindowPlacement(Form.Handle, @Placement) then
    begin
      Ini := TMemIniFile.Create(GetModuleFormFile);
      try
        // Генерируем имя секции
        Section := GetFormSection(Form);
        // Устанавливаем свойства формы
        if not IsWindowVisible(Form.Handle) then Placement.ShowCmd := SW_HIDE;
        UpdateForm := True;
        // Считываем позицию формы
        if LoadPosition then
        begin
          DataFound := False;
          Placement.Flags := Ini.ReadInteger(Section, siFlags, Placement.Flags);
          PosStr := Ini.ReadString(Section, Format(siMinMaxPosR, [Screen.Width, Screen.Height]), EmptyStr);
          if PosStr = EmptyStr then PosStr := Ini.ReadString(Section, siMinMaxPos, EmptyStr);
          if PosStr <> EmptyStr then
          begin
            DataFound := True;
            Placement.ptMinPosition.X := StrToIntDef(ExtractWord(1, PosStr, Delims), 0);
            Placement.ptMinPosition.Y := StrToIntDef(ExtractWord(2, PosStr, Delims), 0);
            Placement.ptMaxPosition.X := StrToIntDef(ExtractWord(3, PosStr, Delims), 0);
            Placement.ptMaxPosition.Y := StrToIntDef(ExtractWord(4, PosStr, Delims), 0);
          end;
          PosStr := Ini.ReadString(Section, Format(siNormPosR, [Screen.Width, Screen.Height]), EmptyStr);
          if PosStr = EmptyStr then PosStr := Ini.ReadString(Section, siNormPos, EmptyStr);
          if PosStr <> EmptyStr then
          begin
            DataFound := True;
            Placement.rcNormalPosition.Left := StrToIntDef(ExtractWord(1, PosStr, Delims), Form.Left);
            Placement.rcNormalPosition.Top := StrToIntDef(ExtractWord(2, PosStr, Delims), Form.Top);
            Placement.rcNormalPosition.Right := StrToIntDef(ExtractWord(3, PosStr, Delims), Form.Left + Form.Width);
            Placement.rcNormalPosition.Bottom := StrToIntDef(ExtractWord(4, PosStr, Delims), Form.Top + Form.Height);
          end;
          if Screen.PixelsPerInch <> Ini.ReadInteger(Section, siPixels,
            Screen.PixelsPerInch) then DataFound := False;
          if DataFound then
          begin
            if not (Form.BorderStyle in [bsSizeable, bsSizeToolWin]) then
              Placement.rcNormalPosition := Rect(Placement.rcNormalPosition.Left,
                Placement.rcNormalPosition.Top, Placement.rcNormalPosition.Left + Form.Width,
                Placement.rcNormalPosition.Top + Form.Height);
            if Placement.rcNormalPosition.Right > Placement.rcNormalPosition.Left then
            begin
              if (Form.Position in [poScreenCenter, poDesktopCenter]) and
                not (csDesigning in Form.ComponentState) then
              begin
                THackComponent(Form).SetDesigning(True);
                try
                  Form.Position := poDesigned;
                finally
                  THackComponent(Form).SetDesigning(False);
                end;
              end;
              SetWindowPlacement(Form.Handle, @Placement);
            end;
          end;
        end;
        // Считываем состояние формы
        if LoadState then
        begin
          WinState := wsNormal;
          if ((Application.MainForm = Form) or (Application.MainForm = nil))
          and ((Form.FormStyle = fsMDIForm) or ((Form.FormStyle = fsNormal)
          and (Form.Position = poDefault))) then WinState := wsMaximized;
          Placement.ShowCmd := Ini.ReadInteger(Section, siShowCmd, SW_HIDE);
          case Placement.ShowCmd of
            SW_SHOWNORMAL, SW_RESTORE, SW_SHOW:
              WinState := wsNormal;
            SW_MINIMIZE, SW_SHOWMINIMIZED, SW_SHOWMINNOACTIVE:
              WinState := wsMinimized;
            SW_MAXIMIZE: WinState := wsMaximized;
          end;
          if (WinState = wsMinimized) and ((Form = Application.MainForm)
          or (Application.MainForm = nil)) then
          begin
            TNastyForm(Form).FWindowState := wsNormal;
            PostMessage(Application.Handle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
            UpdateForm := False;
          end
          else begin
            if Form.FormStyle in [fsMDIChild, fsMDIForm] then
              TNastyForm(Form).FWindowState := WinState
            else Form.WindowState := WinState;
          end;
        end;
        if UpdateForm then Form.Update;
      finally
        Ini.Free;
      end;
    end;
  end;
end;

{ == Сохранение произвольного параметра ======================================== }
procedure SaveFormParameterInt(Form: TForm; const ParamName: string; const Value: Integer);
var
  Ini: TMemIniFile;
  SectionName: string;
begin
  Ini := TMemIniFile.Create(GetModuleFormFile);
  try
    SectionName := GetFormSection(Form);
    Ini.WriteInteger(SectionName, ParamName, Value);
    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;

function LoadFormParameterInt(Form: TForm; const ParamName: string; const Value: Integer): Integer;
var
  Ini: TMemIniFile;
  SectionName: string;
begin
  Ini := TMemIniFile.Create(GetModuleFormFile);
  try
    SectionName := GetFormSection(Form);
    Result := Ini.ReadInteger(SectionName, ParamName, Value);
  finally
    Ini.Free;
  end;
end;

procedure SaveFormParameterStr(Form: TForm; const ParamName: string; const Value: string);
var
  Ini: TMemIniFile;
  SectionName: string;
begin
  Ini := TMemIniFile.Create(GetModuleFormFile);
  try
    SectionName := GetFormSection(Form);
    Ini.WriteString(SectionName, ParamName, Value);
    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;

function LoadFormParameterStr(Form: TForm; const ParamName: string; const Value: string): string;
var
  Ini: TMemIniFile;
  SectionName: string;
begin
  Ini := TMemIniFile.Create(GetModuleFormFile);
  try
    SectionName := GetFormSection(Form);
    Result := Ini.ReadString(SectionName, ParamName, Value);
  finally
    Ini.Free;
  end;
end;

{ == Сохранение панелей TCoolBar =============================================== }
procedure SaveCoolBar(Form: TForm; CoolBar: TCoolBar);
var
  Ini: TMemIniFile;
  SectionName: string;
  i, iCount: Integer;
begin
  Ini := TMemIniFile.Create(GetModuleFormFile);
  try
    SectionName := GetFormSection(Form);
    iCount := CoolBar.Bands.Count - 1;
    for i := 0 to iCount do
      Ini.WriteString(SectionName, Format(SCoolBar, [CoolBar.Name, i]),
        Format(SCoolBand, [CoolBar.Bands[i].Control.Name,
          BoolToStr(CoolBar.Bands[i].Visible),
          BoolToStr(CoolBar.Bands[i].Break),
          CoolBar.Bands[i].Width]));
    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;

procedure LoadCoolBar(Form: TForm; CoolBar: TCoolBar);
var
  Ini: TMemIniFile;
  SectionName, DataStr: string;
  Control: TControl;
  Band: TCoolBand;
  i: Integer;
begin
  Ini := TMemIniFile.Create(GetModuleFormFile);
  try
    SectionName := GetFormSection(Form);
    i := 0;
    DataStr := Ini.ReadString(SectionName, Format(SCoolBar, [CoolBar.Name, i]), EmptyStr);
    while DataStr <> EmptyStr do
    begin
      Control := CoolBar.FindChildControl(Trim(ExtractWord(1, DataStr, Delims)));
      if Assigned(Control) then
      begin
        Band := CoolBar.Bands.FindBand(Control);
        if Assigned(Band) and (i < CoolBar.Bands.Count) then
        begin
          Band.Index := i;
          case WordCount(DataStr, Delims) of
            3:
            begin
              Band.Visible := True;
              Band.Break := StrToBoolDef(Trim(ExtractWord(2, DataStr, Delims)), Band.Break);
              Band.Width := StrToIntDef(Trim(ExtractWord(3, DataStr, Delims)), Band.Width);
            end;
            4:
            begin
              Band.Visible := StrToBoolDef(Trim(ExtractWord(2, DataStr, Delims)), Band.Visible);
              Band.Break := StrToBoolDef(Trim(ExtractWord(3, DataStr, Delims)), Band.Break);
              Band.Width := StrToIntDef(Trim(ExtractWord(4, DataStr, Delims)), Band.Width);
            end;
          end;
        end;
      end;
      Inc(i);
      DataStr := Ini.ReadString(SectionName, Format(SCoolBar, [CoolBar.Name, i]), EmptyStr);
    end;
  finally
    Ini.Free;
  end;
end;

{ == Сохранение ширины столбцов TCustomGrid ====================================== }
procedure SaveGridColumns(Form: TForm; Grid: TDrawGrid);
var
  Ini: TMemIniFile;
  SectionName: string;
  i, iCount: Integer;
begin
  Ini := TMemIniFile.Create(GetModuleFormFile);
  try
    SectionName := GetFormSection(Form);
    iCount := Grid.ColCount - 1;
    for i := 0 to iCount do
      Ini.WriteInteger(SectionName, Format(SGridColumn, [Grid.Name, i]), Grid.ColWidths[i]);
    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;

procedure LoadGridColumns(Form: TForm; Grid: TDrawGrid);
var
  Ini: TMemIniFile;
  SectionName: string;
  i, iCount: Integer;
begin
  Ini := TMemIniFile.Create(GetModuleFormFile);
  try
    SectionName := GetFormSection(Form);
    iCount := Grid.ColCount - 1;
    for i := 0 to iCount do
      Grid.ColWidths[i] := Ini.ReadInteger(SectionName, Format(SGridColumn, [Grid.Name, i]), Grid.ColWidths[i]);
  finally
    Ini.Free;
  end;
end;

{ == Сохранение ширины столбцов ListView ======================================= }
procedure SaveListColumns(Form: TForm; ListView: TListView; const SaveState: Boolean = True);
var
  Ini: TMemIniFile;
  SectionName: string;
  i, iCount: Integer;
begin
  Ini := TMemIniFile.Create(GetModuleFormFile);
  try
    SectionName := GetFormSection(Form);
    if SaveState then
    begin
      Ini.WriteBool(SectionName, Format(SListGridLines, [ListView.Name]), ListView.GridLines);
      Ini.WriteBool(SectionName, Format(SMultiSelect, [ListView.Name]), ListView.MultiSelect);
    end;
    if ListView is TRSortListView then begin
      Ini.WriteString(SectionName, Format(SSortType, [ListView.Name]),
        Format(SSortValues, [TRSortListView(ListView).SortColumn,
          Integer(TRSortListView(ListView).SortDirection)]));
    end;
    iCount := ListView.Columns.Count - 1;
    for i := 0 to iCount do
      Ini.WriteInteger(SectionName, Format(SListColumn, [ListView.Name, i]),
        ListView.Columns[i].Width);
    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;

procedure LoadListColumns(Form: TForm; ListView: TListView; const LoadState: Boolean = True);
var
  Ini: TMemIniFile;
  SectionName, SortValues: string;
  i, iCount: Integer;
begin
  Ini := TMemIniFile.Create(GetModuleFormFile);
  try
    SectionName := GetFormSection(Form);
    if LoadState then
    begin
      ListView.GridLines := Ini.ReadBool(SectionName, Format(SListGridLines, [ListView.Name]), ListView.GridLines);
      ListView.MultiSelect := Ini.ReadBool(SectionName, Format(SMultiSelect, [ListView.Name]), ListView.MultiSelect);
    end;
    if ListView is TRSortListView then
    begin
      SortValues := Ini.ReadString(SectionName, Format(SSortType, [ListView.Name]),
        Format(SSortValues, [TRSortListView(ListView).SortColumn,
          Integer(TRSortListView(ListView).SortDirection)]));
      if Trim(SortValues) <> EmptyStr then
      begin
        TRSortListView(ListView).SortColumn := StrToIntDef(Trim(
          ExtractWord(1, SortValues, Delims)),
            TRSortListView(ListView).SortColumn);
        TRSortListView(ListView).SortDirection := TSortDirection(
          (StrToIntDef(Trim(ExtractWord(2, SortValues, Delims)),
            Integer(TRSortListView(ListView).SortDirection))));
      end;
    end;
    iCount := ListView.Columns.Count - 1;
    for i := 0 to iCount do
      ListView.Columns[i].Width := Ini.ReadInteger(SectionName,
        Format(SListColumn, [ListView.Name, i]), ListView.Columns[i].Width);
  finally
    Ini.Free;
  end;
end;

{ == Сохранение параметров TTreeView =========================================== }
procedure SaveTreeViewParams(Form: TForm; TreeView: TRTreeView; const StoreExpMode: Boolean);
var
  Ini: TMemIniFile;
  SectionName: string;
begin
  Ini := TMemIniFile.Create(GetModuleFormFile);
  try
    SectionName := GetFormSection(Form);
    Ini.WriteBool(SectionName, Format(SRestExpMode, [TreeView.Name]), StoreExpMode);
    Ini.WriteInteger(SectionName, Format(SSortType, [TreeView.Name]), Integer(TreeView.SortType));
    Ini.WriteInteger(SectionName, Format(SExpMode, [TreeView.Name]), Integer(TreeView.AutoExpandMode));
    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;

procedure LoadTreeViewParams(Form: TForm; TreeView: TRTreeView; var StoreExpMode: Boolean);
var
  Ini: TMemIniFile;
  SectionName: string;
begin
  Ini := TMemIniFile.Create(GetModuleFormFile);
  try
    SectionName := GetFormSection(Form);
    StoreExpMode := Ini.ReadBool(SectionName, Format(SRestExpMode, [TreeView.Name]), StoreExpMode);
    TreeView.SortType := TSortMode(Ini.ReadInteger(SectionName, Format(SSortType, [TreeView.Name]),
      Integer(TreeView.SortType)));
    if StoreExpMode then
    begin
      TreeView.AutoExpandMode := TAExpMode(Ini.ReadInteger(SectionName, Format(SExpMode, [TreeView.Name]),
        Integer(TreeView.AutoExpandMode)));
    end;
  finally
    Ini.Free;
  end;
end;

procedure SaveTreeViewParamsExt(Form: TForm; TreeView: TRTreeView;
  const StoreExpMode, StorePosition: Boolean; const LastKey: RNodeData);
var
  Ini: TMemIniFile;
  SectionName: string;
begin
  Ini := TMemIniFile.Create(GetModuleFormFile);
  try
    SectionName := GetFormSection(Form);
    Ini.WriteBool(SectionName, Format(SRestExpMode, [TreeView.Name]), StoreExpMode);
    Ini.WriteBool(SectionName, Format(SRestPosition, [TreeView.Name]), StorePosition);
    Ini.WriteBool(SectionName, Format(SRootOnly, [TreeView.Name]), TreeView.ListRootOnly);
    Ini.WriteInteger(SectionName, Format(SSortType, [TreeView.Name]), Integer(TreeView.SortType));
    Ini.WriteInteger(SectionName, Format(SExpMode, [TreeView.Name]), Integer(TreeView.AutoExpandMode));
    Ini.WriteString(SectionName, Format(SLastNode, [TreeView.Name]), Format(SItemValues,
      [Integer(LastKey.NodeType), LastKey.RecordId]));
    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;

procedure LoadTreeViewParamsExt(Form: TForm; TreeView: TRTreeView;
  var StoreExpMode, StorePosition: Boolean; var LastKey: RNodeData);
var
  Ini: TMemIniFile;
  SectionName, Values: string;
begin
  Ini := TMemIniFile.Create(GetModuleFormFile);
  try
    SectionName := GetFormSection(Form);
    StoreExpMode := Ini.ReadBool(SectionName, Format(SRestExpMode, [TreeView.Name]), StoreExpMode);
    StorePosition := Ini.ReadBool(SectionName, Format(SRestPosition, [TreeView.Name]), StorePosition);
    TreeView.ListRootOnly := Ini.ReadBool(SectionName, Format(SRootOnly, [TreeView.Name]), TreeView.ListRootOnly);
    TreeView.SortType := TSortMode(Ini.ReadInteger(SectionName, Format(SSortType, [TreeView.Name]),
      Integer(TreeView.SortType)));
    if StoreExpMode then
    begin
      TreeView.AutoExpandMode := TAExpMode(Ini.ReadInteger(SectionName, Format(SExpMode, [TreeView.Name]),
        Integer(TreeView.AutoExpandMode)));
    end;
    if StorePosition then
    begin
      Values := Ini.ReadString(SectionName, Format(SLastNode, [TreeView.Name]), Format(SItemValues,
        [Integer(LastKey.NodeType), LastKey.RecordId]));
      LastKey.NodeType := TNodeType(StrToIntDef(Trim(ExtractWord(1, Values, Delims)), Integer(LastKey.NodeType)));
      LastKey.RecordId := StrToIntDef(Trim(ExtractWord(2, Values, Delims)), LastKey.RecordId);
    end;
  finally
    Ini.Free;
  end;
end;

{ == Сохранение ширины панелей с изменяемой шириной ============================ }
procedure SavePanelSize(Form: TForm; Panel: TCustomPanel);
var
  Ini: TMemIniFile;
  SectionName: string;
begin
  Ini := TMemIniFile.Create(GetModuleFormFile);
  try
    SectionName := GetFormSection(Form);
    Ini.WriteString(SectionName, Format(SSizeItem, [Panel.Name]),
      Format(SSizeValues, [Panel.Height, Panel.Width, BoolToStr(Panel.Visible)]));
    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;

procedure LoadPanelSize(Form: TForm; Panel: TCustomPanel;
  const LoadHeight, LoadWidth, LoadState: Boolean);
var
  Ini: TMemIniFile;
  SectionName, Values: string;
begin
  Ini := TMemIniFile.Create(GetModuleFormFile);
  try
    SectionName := GetFormSection(Form);
    Values := Ini.ReadString(SectionName, Format(SSizeItem, [Panel.Name]),
      Format(SSizeValues, [Panel.Height, Panel.Width, BoolToStr(Panel.Visible)]));
    if LoadHeight then
      Panel.Height := StrToIntDef(Trim(ExtractWord(1, Values, Delims)), Panel.Height);
    if LoadWidth then
      Panel.Width := StrToIntDef(Trim(ExtractWord(2, Values, Delims)), Panel.Width);
    if LoadState then
      Panel.Visible := StrToBoolDef(Trim(ExtractWord(3, Values, Delims)), Panel.Visible);
  finally
    Ini.Free;
  end;
end;

{ == Сохранение шрифта ========================================================= }
function FontStylesToInt(const Styles: TFontStyles): Integer;
begin
  Result := 0;
  if fsBold in Styles then Inc(Result, 1);
  if fsItalic in Styles then Inc(Result, 2);
  if fsUnderline in Styles then Inc(Result, 4);
  if fsStrikeOut in Styles then Inc(Result, 8);
end;

function IntToFontStyles(const IntValue: Integer): TFontStyles;
begin
  Result := [];
  if (IntValue and 1) = 1 then Result := Result + [fsBold];
  if (IntValue and 2) = 2 then Result := Result + [fsItalic];
  if (IntValue and 4) = 4 then Result := Result + [fsUnderline];
  if (IntValue and 8) = 8 then Result := Result + [fsStrikeOut];
end;

procedure SaveFontData(Form: TForm; const ItemName: string; Font: TFont);
var
  Ini: TMemIniFile;
  SectionName: string;
begin
  Ini := TMemIniFile.Create(GetModuleFormFile);
  try
    SectionName := GetFormSection(Form);
    Ini.WriteString(SectionName, Format(siFont, [ItemName]),
      Format(SFontValues, [Font.Name, Font.Size, FontStylesToInt(Font.Style)]));
    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;

procedure LoadFontData(Form: TForm; const ItemName: string; Font: TFont);
var
  Ini: TMemIniFile;
  SectionName, Values: string;
begin
  Ini := TMemIniFile.Create(GetModuleFormFile);
  try
    SectionName := GetFormSection(Form);
    Values := Ini.ReadString(SectionName, Format(siFont, [ItemName]),
      Format(SFontValues, [Font.Name, Font.Size, FontStylesToInt(Font.Style)]));
    if Values <> EmptyStr then
    begin
      Font.Name := Trim(ExtractWord(1, Values, Delim));
      Font.Size := StrToIntDef(Trim(ExtractWord(2, Values, Delim)), Font.Size);
      Font.Style := IntToFontStyles(StrToIntDef(Trim(ExtractWord(3, Values, Delim)),
        FontStylesToInt(Font.Style)));
    end;
  finally
    Ini.Free;
  end;
end;

end.
