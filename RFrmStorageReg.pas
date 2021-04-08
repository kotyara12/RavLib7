unit RFrmStorageReg;

interface

uses
  Forms, ComCtrls;

{ == Генерация ключа реестра ==================================================== }
function  GetFormRegKey(Form: TForm): string;

{ == Сохранение позиции формы ================================================== }
procedure SaveFormPosition(Form: TForm);
procedure LoadFormPosition(Form: TForm; const LoadState, LoadPosition: Boolean);

{ == Сохранение панелей TCoolBar =============================================== }
procedure SaveCoolBar(Form: TForm; CoolBar: TCoolBar);
procedure LoadCoolBar(Form: TForm; CoolBar: TCoolBar);

{ == Сохранение ширины столбцов TListView ====================================== }
procedure SaveListColumns(Form: TForm; ListView: TListView);
procedure LoadListColumns(Form: TForm; ListView: TListView);

implementation

uses
  SysUtils, Registry, Classes, Controls, Windows, Messages,
  RxStrUtils;

const
  SKeyName       = '\Software\RavSoft\%s\%s';
  SFormName      = '%s.%s';
  SMDISuffix     = '.MDIChildren';
  SFormPos       = '%d,%d,%d,%d';
  SListGridLines = '%s.GridLines';
  SListColumn    = '%s.Column_%d';
  SCoolBar       = '%s.Band_%d';
  SCoolBand      = '%s,%s,%d';

const
  siFlags        = 'Flags';
  siShowCmd      = 'ShowCmd';
  siMinMaxPos    = 'MinMaxPos';
  siNormPos      = 'NormPos';
  siPixels       = 'PixelsPerInch';
  siMDIChild     = 'MDI Children';
  siListCount    = 'Count';
  siItem         = 'Item%d';

{ == Генерация ключа реестра ==================================================== }
function GetFormRegKey(Form: TForm): string;
var
  FormName: string;
begin
  FormName := Format(SFormName, [Form.Name, Form.ClassName]);
  if Form.FormStyle = fsMDIChild then FormName := FormName + SMDISuffix;
  Result := Format(SKeyName,
    [ExtractFileName(ChangeFileExt(Application.ExeName, EmptyStr)), FormName]);
end;

{ == Сохранение позиции формы ================================================== }
procedure SaveFormPosition(Form: TForm);
var
  Section: string;
  Placement: TWindowPlacement;
  Reg: TRegIniFile;
begin
  Placement.Length := SizeOf(TWindowPlacement);
  if GetWindowPlacement(Form.Handle, @Placement) then
  begin
    Reg := TRegIniFile.Create(GetFormRegKey(Form));
    try
      // Устанавливаем свойства формы
      if (Form = Application.MainForm) and IsIconic(Application.Handle) then
        Placement.ShowCmd := SW_SHOWMINIMIZED;
      if (Form.FormStyle = fsMDIChild) and (Form.WindowState = wsMinimized) then
        Placement.Flags := Placement.Flags or WPF_SETMINPOSITION;
      // Сохраняем свойства формы
      Reg.WriteInteger(Section, siFlags, Placement.Flags);
      Reg.WriteInteger(Section, siShowCmd, Placement.ShowCmd);
      Reg.WriteInteger(Section, siPixels, Screen.PixelsPerInch);
      Reg.WriteString(Section, siMinMaxPos, Format(SFormPos,
        [Placement.ptMinPosition.X, Placement.ptMinPosition.Y,
         Placement.ptMaxPosition.X, Placement.ptMaxPosition.Y]));
      Reg.WriteString(Section, siNormPos, Format(SFormPos,
        [Placement.rcNormalPosition.Left, Placement.rcNormalPosition.Top,
         Placement.rcNormalPosition.Right, Placement.rcNormalPosition.Bottom]));
    finally
      Reg.Free;
    end;
  end;
end;

{ == Восстановление позиции формы ============================================== }
type
  TNastyForm = class(TScrollingWinControl)
  private
    FWindowState: TWindowState; { !! }
  end;

  THackComponent = class(TComponent);

procedure LoadFormPosition(Form: TForm; const LoadState, LoadPosition: Boolean);
const
  Delims = [',',' '];
var
  Section, PosStr: string;
  Placement: TWindowPlacement;
  WinState: TWindowState;
  DataFound, UpdateForm: Boolean;
  Reg: TRegIniFile;
begin
  if LoadState or LoadPosition then
  begin
    Placement.Length := SizeOf(TWindowPlacement);
    if GetWindowPlacement(Form.Handle, @Placement) then
    begin
      Reg := TRegIniFile.Create(GetFormRegKey(Form));
      try
        // Устанавливаем свойства формы
        if not IsWindowVisible(Form.Handle) then Placement.ShowCmd := SW_HIDE;
        UpdateForm := True;
        // Считываем позицию формы
        if LoadPosition then
        begin
          DataFound := False;
          Placement.Flags := Reg.ReadInteger(Section, siFlags, Placement.Flags);
          PosStr := Reg.ReadString(Section, siMinMaxPos, EmptyStr);
          if PosStr <> EmptyStr then
          begin
            DataFound := True;
            Placement.ptMinPosition.X := StrToIntDef(ExtractWord(1, PosStr, Delims), 0);
            Placement.ptMinPosition.Y := StrToIntDef(ExtractWord(2, PosStr, Delims), 0);
            Placement.ptMaxPosition.X := StrToIntDef(ExtractWord(3, PosStr, Delims), 0);
            Placement.ptMaxPosition.Y := StrToIntDef(ExtractWord(4, PosStr, Delims), 0);
          end;
          PosStr := Reg.ReadString(Section, siNormPos, EmptyStr);
          if PosStr <> EmptyStr then
          begin
            DataFound := True;
            Placement.rcNormalPosition.Left := StrToIntDef(ExtractWord(1, PosStr, Delims), Form.Left);
            Placement.rcNormalPosition.Top := StrToIntDef(ExtractWord(2, PosStr, Delims), Form.Top);
            Placement.rcNormalPosition.Right := StrToIntDef(ExtractWord(3, PosStr, Delims), Form.Left + Form.Width);
            Placement.rcNormalPosition.Bottom := StrToIntDef(ExtractWord(4, PosStr, Delims), Form.Top + Form.Height);
          end;
          if Screen.PixelsPerInch <> Reg.ReadInteger(Section, siPixels,
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
          Placement.ShowCmd := Reg.ReadInteger(Section, siShowCmd, SW_HIDE);
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
            if Form.FormStyle in [fsMDIChild, fsMDIForm]
            then TNastyForm(Form).FWindowState := WinState
            else Form.WindowState := WinState;
          end;
        end;
        if UpdateForm then Form.Update;
      finally
        Reg.Free;
      end;
    end;
  end;
end;

{ == Сохранение панелей TCoolBar =============================================== }
procedure SaveCoolBar(Form: TForm; CoolBar: TCoolBar);
var
  Reg: TRegIniFile;
  SectionName: string;
  i: Integer;
begin
  Reg := TRegIniFile.Create(GetFormRegKey(Form));
  try
    for i := 0 to CoolBar.Bands.Count - 1 do
      Reg.WriteString(SectionName, Format(SCoolBar, [CoolBar.Name, i]),
        Format(SCoolBand, [CoolBar.Bands[i].Control.Name, BoolToStr(CoolBar.Bands[i].Break),
          CoolBar.Bands[i].Width]));
  finally
    Reg.Free;
  end;
end;

procedure LoadCoolBar(Form: TForm; CoolBar: TCoolBar);
const
  Delims = [',',' '];
var
  Reg: TRegIniFile;
  SectionName, DataStr: string;
  Control: TControl;
  Band: TCoolBand;
  i: Integer;
begin
  Reg := TRegIniFile.Create(GetFormRegKey(Form));
  try
    i := 0;
    DataStr := Reg.ReadString(SectionName, Format(SCoolBar, [CoolBar.Name, i]), EmptyStr);
    while DataStr <> EmptyStr do
    begin
      Control := CoolBar.FindChildControl(Trim(ExtractWord(1, DataStr, Delims)));
      if Assigned(Control) then begin
        Band := CoolBar.Bands.FindBand(Control);
        if Assigned(Band) then begin
          Band.Index := i;
          Band.Break := StrToBoolDef(Trim(ExtractWord(2, DataStr, Delims)), Band.Break);
          Band.Width := StrToIntDef(Trim(ExtractWord(3, DataStr, Delims)), Band.Width);
        end;
      end;
      Inc(i);
      DataStr := Reg.ReadString(SectionName, Format(SCoolBar, [CoolBar.Name, i]), EmptyStr);
    end;
  finally
    Reg.Free;
  end;
end;

{ == Сохранение ширины столбцов ListView ======================================= }
procedure SaveListColumns(Form: TForm; ListView: TListView);
var
  Reg: TRegIniFile;
  SectionName: string;
  i: Integer;
begin
  Reg := TRegIniFile.Create(GetFormRegKey(Form));
  try
    Reg.WriteBool(SectionName, Format(SListGridLines, [ListView.Name]), ListView.GridLines);
    for i := 0 to ListView.Columns.Count - 1 do
      Reg.WriteInteger(SectionName, Format(SListColumn, [ListView.Name, i]),
        ListView.Columns[i].Width);
  finally
    Reg.Free;
  end;
end;

procedure LoadListColumns(Form: TForm; ListView: TListView);
var
  Reg: TRegIniFile;
  SectionName: string;
  i: Integer;
begin
  Reg := TRegIniFile.Create(GetFormRegKey(Form));
  try
    ListView.GridLines := Reg.ReadBool(SectionName, Format(SListGridLines, [ListView.Name]), ListView.GridLines);
    for i := 0 to ListView.Columns.Count - 1 do
      ListView.Columns[i].Width := Reg.ReadInteger(SectionName,
        Format(SListColumn, [ListView.Name, i]), ListView.Columns[i].Width);
  finally
    Reg.Free;
  end;
end;

end.
