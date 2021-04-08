unit RMsExcel;

interface

uses
  Classes, Graphics, RVclUtils;
  
{ == ��������� OLE - ����� MsExcel ============================================= }
function  GetMsExcelOleName: string;
{ == ��������� MsExcel ����� OLE =============================================== }
function  ConnectToMsExcel: Boolean;
{ == �������� ������� Excel ==================================================== }
procedure ShowMsExcel;
{ == �������� ������� Excel � ��������� ����� ================================== }
procedure ShowMsExcelAndDiconnect;
{ == ������� ����� Excel � ��������� ����� ===================================== }
procedure CloseMsExcelAndDiconnect;
{ == ������� ����� ����� ======================================================= }
function  AddWorkbook: Variant;
{ == ������� ����� ����� �� ��������� ========================================== }
function  SetDefaultWorkbook: Boolean;
{ == ������� ����� �� ����� ==================================================== }
function  OpenWorkbook(const FileName: string): Variant;
{ == ��������� ����� � ����� =================================================== }
procedure SaveWorkbook(Wb: Variant; const aFileName: string);
procedure SaveAsWorkbook(Wb: Variant; const aFileName: string);
{ == ��������� ����� �� ��������� � ����� ====================================== }
procedure SaveDefaultWorkbook(const aFileName: string);
{ == ���� ������ �� ����� ��� ��� ============================================== }
function  SheetIsEmpty(Sheet: Variant): Boolean;
{ == �������������� ����� ����� ================================================ }
function  CorrectSheetName(const AStr: string): string;
{ == ������� ���� ============================================================== }
function  AddSheet(Wb: Variant; const ACaption: string): Variant;
{ == ������� ���� � �������� ������� =========================================== }
function  AddNumSheet(Wb: Variant; const ANumber: Integer; const ACaption: string): Variant;
{ == ������� ��� ������� ���� ================================================== }
function  OpenSheet(Wb: Variant; const ANumber: Integer; const ACaption: string): Variant;
{ == ������� ��� ������� ���� �� ��������� ===================================== }
function  OpenSheetCW(const ANumber: Integer; const ACaption: string): Variant;
{ == ������� ������������ ���� �� ����� ======================================== }
function  OpenSheetOnName(Wb: Variant; const AName: string; const bCreateNew: Boolean = False): Variant;
{ == ��������� ���������� �������� ============================================= }
procedure SetPageOrientation(Sheet: Variant; const AValue: Integer; const HideError: Boolean = False);
{ == �������������� �������� ������������ ====================================== }
function  AlignmentToExcelH(DelphiAlignment: Integer): Integer; overload;
function  AlignmentToExcelH(DelphiAlignment: TAlignment): Integer; overload;
{ == �������������� ��������� ��������� �������� � �������� ==================== }
function  ColumnNumberToStr(const iCol: Integer): String;
function  ColumnStrToNumber(const sCol: string): Integer;
{ == ��������� ������ ������ ������ ������ ===================================== }
procedure SetSingleBorders(Range: Variant; const AWeight: Integer);
procedure SetCustomBorders(Range: Variant; const AType, AWeight: Integer);
procedure SetOutsideBorders(Range: Variant; const AWeight: Integer);
procedure SetInsideBorders(Range: Variant; const AWeight: Integer);
procedure SetTotalBorders(Range: Variant; const AWeight: Integer);
procedure SetInterior(Range: Variant; const Color: TColor);
{ == ��������� ���������� ������� ============================================== }
procedure SetColumnParams(Column: Variant; const Width: Real;
  const HAlignment, VAlignment, FontSize: Integer; const NumberFormat: string);
{ == ���������� ������ ========================================================= }
procedure RangeMerge(Sheet: Variant; const RowStart, RowEnd, ColStart, ColEnd: Integer;
  const Value: Variant; const FontSize: Integer; const FontStyle: TFontStyles;
  const BordersWeight: Integer; const HAlignment, VAlignment: Integer;
  const WordWrap: Boolean);
{ == ����� ������ � ������ ===================================================== }
procedure CellTextDefault(Cell: Variant; const Text: string; const BordersWeight: Integer);
procedure CellText(Cell: Variant; const Text: string;
  const FontSize: Integer; const FontStyle: TFontStyles;
  const BordersWeight: Integer; const HAlignment, VAlignment: Integer;
  const WordWrap: Boolean; const Format: string);

procedure CellValueDefault(Cell: Variant; const Value: Variant; const BordersWeight: Integer);
procedure CellValue(Cell: Variant; const Value: Variant;
  const FontSize: Integer; const FontStyle: TFontStyles;
  const BordersWeight: Integer; const HAlignment, VAlignment: Integer;
  const WordWrap: Boolean; const Format: string);

procedure CellFormulaDefault(Cell: Variant; const Formula: string; const BordersWeight: Integer);
procedure CellFormula(Cell: Variant; const Formula: string;
  const FontSize: Integer; const FontStyle: TFontStyles;
  const BordersWeight: Integer; const HAlignment, VAlignment: Integer;
  const WordWrap: Boolean; const Format: string);

procedure RangeTextDefault(Sheet: Variant; const RowStart, RowEnd, ColStart, ColEnd: Integer;
  const Values: Variant; const BordersWeight: Integer);
procedure RangeText(Sheet: Variant; const RowStart, RowEnd, ColStart, ColEnd: Integer;
  const Values: Variant; const FontSize: Integer; const FontStyle: TFontStyles;
  const BordersWeight: Integer; const HAlignment, VAlignment: Integer; const WordWrap: Boolean);
procedure RangeGroup(Sheet: Variant; const RowStart, RowEnd: Integer);

{ == ������� ������ ============================================================ }
function GetCellFloat(Sheet: Variant; const iRow, iCol: Integer): Double;

resourcestring
  SMsgExportExcel           = '������� � Excel';
  SMsgOpenExcel             = '����� Microsoft Excel...';
  SMsgOpenWorkbook          = '�������� ������� �����...';
  SMsgOpenWorksheet         = '�������� �����...';
  SMsgFormatPage            = '�������������� �����...';
  SMsgPrepareData           = '���������� ������ ��� ��������...';
  SMsgTransferData          = '�������� ������ � Microsoft Excel...';
  SMsgReportDate            = '�������� ������ %s.';
  SMsgExportComplete        = '������� ������ � Microsoft Excel ��������!';

  SWrnSizeExport            = '��������������! ��������� ����� ������ ������ %d �������!';

  SErrExportExcel           = '������ �������� ������ � Microsoft Excel!';
  SErrIncorrectNumber       = '������������ ����� �������: %s!';

const
  XlsTextFormat             = '@';
  MaxSheetNameLen           = 31;
  WidthGridToExcel          = 8;
  WidthFieldToExcel         = 1.3;
  WidthColumnToExcel        = 8;

var
  MsExcel, Workbook: Variant;

implementation

uses
  Registry, Windows, Variants, ComObj, SysUtils,
  RxStrUtils, ExcelConst, RExHandlers;

const
  ExcelOleNameDefault       = 'Excel.Application';
  ExcelOleNameRegistryKey   = '\Excel.Application\CurVer';

resourcestring
  SErrConnectMsExcel        = '������ ����������� � Microsoft Excel!';
  SErrCreateWorkbook        = '������ �������� ����� Microsoft Excel!';
  SErrOpenWorkbook          = '������ ������ ����� �� ����� "%s"!';
  SErrSaveWorkbook          = '������ ���������� ����� � ����� "%s"!';
  SErrCreateSheet           = '������ �������� ����� Microsoft Excel!';
  SErrOpenSheet             = '������ �������� (��������) ����� #%d!';
  SErrOpenSheetName         = '������ �������� ����� "%s"!';
  SErrSetPageOrientation    = '������ ��������� ���������� �������� Microsoft Excel!';
  SErrWorkbookNull          = '������� ����� Microsoft Excel �� ������� ��� �� �������';
  SErrSheetNotFound         = '���� "%s" �� ������!';

{ == ��������� OLE - ����� MsExcel ============================================= }
function GetMsExcelOleName: string;
var
  RegData: TRegistry;
begin
  Result := ExcelOleNameDefault;
  RegData := TRegistry.Create;
  RegData.RootKey := HKEY_CLASSES_ROOT;
  try
    if RegData.OpenKey(ExcelOleNameRegistryKey, False)
    then
      begin
        Result := RegData.ReadString('');
        RegData.CloseKey;
      end;
  finally
    RegData.Free;
  end;
end;

{ == ��������� MsExcel ����� OLE =============================================== }
function ConnectToMsExcel: Boolean;
begin
  try
    if not VarIsEmpty(MsExcel) then ShowMsExcelAndDiconnect;
    try
      MsExcel := CreateOleObject(GetMsExcelOleName);
      if not VarIsEmpty(MsExcel) then MsExcel.Application.EnableEvents := False;
    except
      on E: Exception do
        HandleExcept(E, nil, SErrConnectMsExcel);
    end;
  finally
    Result := not VarIsEmpty(MsExcel);
  end;
end;

{ == �������� ������� Excel ==================================================== }
procedure ShowMsExcel;
begin
  if not VarIsEmpty(MsExcel) then
  begin
    MsExcel.Application.EnableEvents := True;
    MsExcel.Visible := True;
    // MsExcel.BringToFront;
  end;
end;

{ == �������� ������� Excel � ��������� ����� ================================== }
procedure ShowMsExcelAndDiconnect;
begin
  ShowMsExcel;
  VarClear(Workbook);
  VarClear(MsExcel);
end;

{ == ������� ����� Excel � ��������� ����� ===================================== }
procedure CloseMsExcelAndDiconnect;
begin
  if not VarIsEmpty(MsExcel) then
    MsExcel.Quit;
  VarClear(Workbook);
  VarClear(MsExcel);
end;

{ == ������� ����� ����� ======================================================= }
function AddWorkbook: Variant;
begin
  VarClear(Result);
  if not VarIsEmpty(MsExcel) or ConnectToMsExcel
  then begin
    try
      Result := MsExcel.Workbooks.Add;
    except
      on E: Exception do
      begin
        VarClear(Result);
        HandleExcept(E, nil, SErrCreateWorkbook);
      end;
    end;
  end;
end;

{ == ������� ����� ����� �� ��������� ========================================== }
function SetDefaultWorkbook: Boolean;
begin
  try
    Workbook := AddWorkbook;
  finally
    Result := not VarIsEmpty(Workbook);
  end;
end;

{ == ������� ����� �� ����� ==================================================== }
function OpenWorkbook(const FileName: string): Variant;
begin
  VarClear(Result);
  if not VarIsEmpty(MsExcel) or ConnectToMsExcel
  then begin
    try
      Result := MsExcel.Workbooks.Open(Filename := FileName);
    except
      on E: Exception do
      begin
        VarClear(Result);
        HandleExcept(E, nil, Format(SErrOpenWorkbook, [FileName]));
      end;
    end;
  end;
end;

{ == ��������� ����� � ����� =================================================== }
procedure SaveAsWorkbook(Wb: Variant; const aFileName: string);
begin
  try
    if FileExists(aFileName) then
      DeleteFile(aFileName);

    Wb.SaveAs(Filename := aFileName, CreateBackup := False);
  except
    on E: Exception do
      HandleExcept(E, nil, Format(SErrSaveWorkbook, [aFileName]));
  end;
end;

procedure SaveWorkbook(Wb: Variant; const aFileName: string);
begin
  try
    if FileExists(aFileName)
    then Wb.Save
    else Wb.SaveAs(Filename := aFileName, CreateBackup := False);
  except
    on E: Exception do
      HandleExcept(E, nil, Format(SErrSaveWorkbook, [aFileName]));
  end;
end;

{ == ��������� ����� �� ��������� � ����� ====================================== }
procedure SaveDefaultWorkbook(const aFileName: string);
begin
  SaveWorkbook(Workbook, aFileName);
end;

{ == ���� ������ �� ����� ��� ��� ============================================== }
function SheetIsEmpty(Sheet: Variant): Boolean;
begin
  Result := VarIsEmpty(Sheet)
    or ((Sheet.UsedRange.Columns.Count = 1)
      and (Sheet.UsedRange.Rows.Count = 1)
      and (Sheet.Cells[1, 1].Text = EmptyStr));
end;

{ == �������������� ����� ����� ================================================ }
function CorrectSheetName(const AStr: string): string;
begin
  Result := DelChars(AStr, '"');
  Result := DelChars(Result, '''');
  Result := DelChars(Result, '*');
  Result := ReplaceStr(Result, '/', '-');
  Result := ReplaceStr(Result, '\', '-');
  Result := ReplaceStr(Result, '[', '(');
  Result := ReplaceStr(Result, ']', ')');
  Result := DelChars(Result, '?');
  Result := DelChars(Result, ':');
  Result := Trim(Copy(Result, 1, MaxSheetNameLen));
end;

{ == ������� ���� ============================================================== }
function AddSheet(Wb: Variant; const ACaption: string): Variant;
var
  i: Integer;
begin
  VarClear(Result);
  try
    if not VarIsEmpty(Wb) then
    begin
      if Wb.Sheets.Count = 0 then
        Result := Wb.Sheets.Add
      else begin
        for i := 1 to Wb.Sheets.Count do
        begin
          if SheetIsEmpty(Wb.Sheets[i]) then
          begin
            Result := Wb.Sheets[i];
            Break;
          end;
        end;

        if VarIsEmpty(Result) then
          Result := Wb.Sheets.Add(After := Wb.Sheets[Wb.Sheets.Count]);
      end;

      if not VarIsEmpty(Result) and (ACaption <> EmptyStr)
      then Result.Name := CorrectSheetName(ACaption);
    end
    else raise Exception.Create(SErrWorkbookNull);
  except
    on E: Exception do
    begin
      VarClear(Result);
      HandleExcept(E, nil, SErrCreateSheet);
    end;
  end;
end;

{ == ������� ���� � �������� ������� =========================================== }
function AddNumSheet(Wb: Variant; const ANumber: Integer; const ACaption: string): Variant;
begin
  VarClear(Result);
  try
    if not VarIsEmpty(Wb) then
    begin
      while Wb.Sheets.Count < ANumber do
        Wb.Sheets.Add;

      Result := Wb.Sheets[ANumber];
      if not VarIsEmpty(Result) and (ACaption <> EmptyStr)
      then Result.Name := CorrectSheetName(ACaption);
    end
    else raise Exception.Create(SErrWorkbookNull);
  except
    on E: Exception do
    begin
      VarClear(Result);
      HandleExcept(E, nil, SErrCreateSheet);
    end;
  end;
end;

{ == ������� ������������ ���� �� ����� ======================================== }
function OpenSheetOnName(Wb: Variant; const AName: string; const bCreateNew: Boolean = False): Variant;
var
  i: Integer;
begin
  VarClear(Result);
  try
    if not VarIsEmpty(Wb) then
    begin
      for i := 1 to Wb.Sheets.Count do
      begin
        if SameText(Wb.Sheets[i].Name, AName) then
        begin
          Result := Wb.Sheets[i];
          Break;
        end;
      end;
      if VarIsEmpty(Result) then
      begin
        if bCreateNew
        then Result := AddSheet(Wb, AName)
        else raise Exception.CreateFmt(SErrSheetNotFound, [AName]);
      end;
    end
    else raise Exception.Create(SErrWorkbookNull);
  except
    on E: Exception do
    begin
      VarClear(Result);
      HandleExcept(E, nil, Format(SErrOpenSheetName, [AName]));
    end;
  end;
end;

{ == ������� ��� ������� ���� ================================================== }
function OpenSheet(Wb: Variant; const ANumber: Integer; const ACaption: string): Variant;
begin
  VarClear(Result);
  try
    if not VarIsEmpty(Wb) then
    begin
      try
        Result := Wb.Sheets[ANumber];
        if not VarIsEmpty(Result) and (ACaption <> EmptyStr)
        then Result.Name := CorrectSheetName(ACaption);
      except
        Result := AddSheet(Wb, ACaption);
      end;
    end
    else raise Exception.Create(SErrWorkbookNull);
  except
    on E: Exception do
    begin
      VarClear(Result);
      HandleExcept(E, nil, Format(SErrOpenSheet, [ANumber]));
    end;
  end;
end;

{ == ������� ��� ������� ���� �� ��������� ===================================== }
function OpenSheetCW(const ANumber: Integer; const ACaption: string): Variant;
begin
  Result := OpenSheet(Workbook, ANumber, ACaption);
end;

{ == ��������� ���������� �������� ============================================= }
procedure SetPageOrientation(Sheet: Variant; const AValue: Integer; const HideError: Boolean = False);
begin
  if HideError then HandlerBlockMessages;
  try
    try
      Sheet.PageSetup.Orientation := AValue;
    except
      on E: Exception do
        HandleExcept(E, nil, SErrSetPageOrientation);
    end;
  finally
    if HideError then HandlerUnblockMessages;
  end;
end;

{ == �������������� �������� ������������ ====================================== }
function AlignmentToExcelH(DelphiAlignment: Integer): Integer;
begin
  case DelphiAlignment of
    Integer(taRightJustify): Result := xlHAlignRight;
    Integer(taCenter): Result := xlHAlignCenter;
    else Result := xlHAlignLeft;
  end;
end;

function AlignmentToExcelH(DelphiAlignment: TAlignment): Integer;
begin
  case DelphiAlignment of
    taRightJustify: Result := xlHAlignRight;
    taCenter: Result := xlHAlignCenter;
    else Result := xlHAlignLeft;
  end;
end;

{ == �������������� ��������� ��������� �������� � �������� ==================== }
function ColumnNumberToStr(const iCol: Integer): String;
var
  iInit, iBase: Byte;
begin
  if iCol < 1 then
    raise Exception.CreateFmt(SErrIncorrectNumber, [IntToStr(iCol)]);
  iInit := Ord('A') - 1;
  iBase := Ord('Z') - iInit;
  if iCol <= iBase then Result := Chr(iCol + iInit)
  else begin
    if (iCol mod iBase) > 0
    then Result := ColumnNumberToStr(iCol div iBase) + Chr(iCol mod iBase + iInit)
    else Result := ColumnNumberToStr((iCol div iBase) - 1) + Chr(iBase + iInit);
  end;
end;

function ColumnStrToNumber(const sCol: string): Integer;
var
  i, iLen: Integer;
  cCurr: Char;
begin
  Result := 0;
  iLen := Length(sCol);
  for i := 1 to iLen do
  begin
    cCurr := AnsiUpperCase(sCol)[i];
    if not (cCurr in ['A'..'Z']) then
      raise Exception.CreateFmt(SErrIncorrectNumber, [sCol]);
    Result := Result * (Ord('Z') - Ord('A') + 1) + (Ord(cCurr) - Ord('A') + 1);
  end;
end;

{ == ��������� ���������� ������� ============================================== }
procedure SetColumnParams(Column: Variant; const Width: Real;
  const HAlignment, VAlignment, FontSize: Integer; const NumberFormat: string);
begin
  if Width < High(Byte)
  then Column.ColumnWidth := Width
  else Column.ColumnWidth := High(Byte);
  Column.Font.Size := FontSize;
  Column.HorizontalAlignment := HAlignment;
  Column.VerticalAlignment := VAlignment;
  Column.NumberFormat := NumberFormat;
end;

{ == ��������� ������ ������ ������ ������ ===================================== }
procedure SetSingleBorders(Range: Variant; const AWeight: Integer);
begin
  Range.Borders.LineStyle := xlLinearTrend;
  Range.Borders.Weight := AWeight;
  Range.Borders.ColorIndex := xlColorIndexAutomatic;
end;

procedure SetCustomBorders(Range: Variant; const AType, AWeight: Integer);
begin
  Range.Borders[AType].LineStyle := xlContinuous;
  Range.Borders[AType].Weight := AWeight;
  Range.Borders[AType].ColorIndex := xlColorIndexAutomatic;
end;

procedure SetOutsideBorders(Range: Variant; const AWeight: Integer);
begin
  SetCustomBorders(Range, xlEdgeTop, AWeight);
  SetCustomBorders(Range, xlEdgeLeft, AWeight);
  SetCustomBorders(Range, xlEdgeRight, AWeight);
  SetCustomBorders(Range, xlEdgeBottom, AWeight);
end;

procedure SetInsideBorders(Range: Variant; const AWeight: Integer);
begin
  SetCustomBorders(Range, xlInsideHorizontal, AWeight);
  SetCustomBorders(Range, xlInsideVertical, AWeight);
end;

procedure SetTotalBorders(Range: Variant; const AWeight: Integer);
begin
  SetOutsideBorders(Range, AWeight);
  SetInsideBorders(Range, AWeight);
end;

procedure SetInterior(Range: Variant; const Color: TColor);
begin
  Range.Interior.Pattern := xlSolid;
  Range.Interior.Color := Integer(Color);
end;

{ == ����� ������ � ������ ===================================================== }
procedure CellText(Cell: Variant; const Text: string;
  const FontSize: Integer; const FontStyle: TFontStyles;
  const BordersWeight: Integer; const HAlignment, VAlignment: Integer;
  const WordWrap: Boolean; const Format: string);
begin
  if Format <> EmptyStr then
    Cell.NumberFormat := Format;
  Cell.Value := Text;
  Cell.Font.Size := FontSize;
  Cell.Font.Bold := fsBold in FontStyle;
  Cell.Font.Italic := fsItalic in FontStyle;
  Cell.Font.Underline := fsUnderline in FontStyle;
  Cell.Font.Strikethrough := fsStrikeOut in FontStyle;
  if BordersWeight > 0 then SetSingleBorders(Cell, BordersWeight);
  Cell.HorizontalAlignment := HAlignment;
  Cell.VerticalAlignment := VAlignment;
  Cell.WrapText := WordWrap;
end;

procedure CellValue(Cell: Variant; const Value: Variant;
  const FontSize: Integer; const FontStyle: TFontStyles;
  const BordersWeight: Integer; const HAlignment, VAlignment: Integer;
  const WordWrap: Boolean; const Format: string);
begin
  if Format <> EmptyStr then
    Cell.NumberFormat := Format;
  Cell.Value := Value;
  Cell.Font.Size := FontSize;
  Cell.Font.Bold := fsBold in FontStyle;
  Cell.Font.Italic := fsItalic in FontStyle;
  Cell.Font.Underline := fsUnderline in FontStyle;
  Cell.Font.Strikethrough := fsStrikeOut in FontStyle;
  if BordersWeight > 0 then SetSingleBorders(Cell, BordersWeight);
  Cell.HorizontalAlignment := HAlignment;
  Cell.VerticalAlignment := VAlignment;
  Cell.WrapText := WordWrap;
end;

procedure CellFormula(Cell: Variant; const Formula: string;
  const FontSize: Integer; const FontStyle: TFontStyles;
  const BordersWeight: Integer; const HAlignment, VAlignment: Integer;
  const WordWrap: Boolean; const Format: string);
begin
  if Format <> EmptyStr then
    Cell.NumberFormat := Format;
  Cell.Formula := Formula;
  Cell.Font.Size := FontSize;
  Cell.Font.Bold := fsBold in FontStyle;
  Cell.Font.Italic := fsItalic in FontStyle;
  Cell.Font.Underline := fsUnderline in FontStyle;
  Cell.Font.Strikethrough := fsStrikeOut in FontStyle;
  if BordersWeight > 0 then SetSingleBorders(Cell, BordersWeight);
  Cell.HorizontalAlignment := HAlignment;
  Cell.VerticalAlignment := VAlignment;
  Cell.WrapText := WordWrap;
end;

procedure CellTextDefault(Cell: Variant; const Text: string; const BordersWeight: Integer);
begin
  Cell.Value := Text;
  if BordersWeight > intDisable then SetSingleBorders(Cell, BordersWeight);
end;

procedure CellValueDefault(Cell: Variant; const Value: Variant; const BordersWeight: Integer);
begin
  Cell.Value := Value;
  if BordersWeight > intDisable then SetSingleBorders(Cell, BordersWeight);
end;

procedure CellFormulaDefault(Cell: Variant; const Formula: string; const BordersWeight: Integer);
begin
  Cell.Value := Formula;
  if BordersWeight > intDisable then SetSingleBorders(Cell, BordersWeight);
end;

procedure RangeMerge(Sheet: Variant; const RowStart, RowEnd, ColStart, ColEnd: Integer;
  const Value: Variant; const FontSize: Integer; const FontStyle: TFontStyles;
  const BordersWeight: Integer; const HAlignment, VAlignment: Integer;
  const WordWrap: Boolean);
begin
  Sheet.Range[Sheet.Cells[RowStart, ColStart], Sheet.Cells[RowEnd, ColEnd]].Merge;
  Sheet.Range[Sheet.Cells[RowStart, ColStart], Sheet.Cells[RowEnd, ColEnd]].Value := Value;
  Sheet.Range[Sheet.Cells[RowStart, ColStart], Sheet.Cells[RowEnd, ColEnd]].Font.Size := FontSize;
  Sheet.Range[Sheet.Cells[RowStart, ColStart], Sheet.Cells[RowEnd, ColEnd]].Font.Bold := fsBold in FontStyle;
  Sheet.Range[Sheet.Cells[RowStart, ColStart], Sheet.Cells[RowEnd, ColEnd]].Font.Italic := fsItalic in FontStyle;
  Sheet.Range[Sheet.Cells[RowStart, ColStart], Sheet.Cells[RowEnd, ColEnd]].Font.Underline := fsUnderline in FontStyle;
  Sheet.Range[Sheet.Cells[RowStart, ColStart], Sheet.Cells[RowEnd, ColEnd]].Font.Strikethrough := fsStrikeOut in FontStyle;
  if BordersWeight > 0 then
    SetSingleBorders(Sheet.Range[Sheet.Cells[RowStart, ColStart],
      Sheet.Cells[RowEnd, ColEnd]], BordersWeight);
  Sheet.Range[Sheet.Cells[RowStart, ColStart], Sheet.Cells[RowEnd, ColEnd]].HorizontalAlignment := HAlignment;
  Sheet.Range[Sheet.Cells[RowStart, ColStart], Sheet.Cells[RowEnd, ColEnd]].VerticalAlignment := VAlignment;
  Sheet.Range[Sheet.Cells[RowStart, ColStart], Sheet.Cells[RowEnd, ColEnd]].WrapText := WordWrap;
end;

procedure RangeText(Sheet: Variant; const RowStart, RowEnd, ColStart, ColEnd: Integer;
  const Values: Variant; const FontSize: Integer; const FontStyle: TFontStyles;
  const BordersWeight: Integer; const HAlignment, VAlignment: Integer; const WordWrap: Boolean);
begin
  Sheet.Range[Sheet.Cells[RowStart, ColStart], Sheet.Cells[RowEnd, ColEnd]].Value := Values;
  Sheet.Range[Sheet.Cells[RowStart, ColStart], Sheet.Cells[RowEnd, ColEnd]].Font.Size := FontSize;
  Sheet.Range[Sheet.Cells[RowStart, ColStart], Sheet.Cells[RowEnd, ColEnd]].Font.Bold := fsBold in FontStyle;
  Sheet.Range[Sheet.Cells[RowStart, ColStart], Sheet.Cells[RowEnd, ColEnd]].Font.Italic := fsItalic in FontStyle;
  Sheet.Range[Sheet.Cells[RowStart, ColStart], Sheet.Cells[RowEnd, ColEnd]].Font.Underline := fsUnderline in FontStyle;
  Sheet.Range[Sheet.Cells[RowStart, ColStart], Sheet.Cells[RowEnd, ColEnd]].Font.Strikethrough := fsStrikeOut in FontStyle;
  if BordersWeight > 0 then
    SetSingleBorders(Sheet.Range[Sheet.Cells[RowStart, ColStart],
      Sheet.Cells[RowEnd, ColEnd]], BordersWeight);
  Sheet.Range[Sheet.Cells[RowStart, ColStart], Sheet.Cells[RowEnd, ColEnd]].HorizontalAlignment := HAlignment;
  Sheet.Range[Sheet.Cells[RowStart, ColStart], Sheet.Cells[RowEnd, ColEnd]].VerticalAlignment := VAlignment;
  Sheet.Range[Sheet.Cells[RowStart, ColStart], Sheet.Cells[RowEnd, ColEnd]].WrapText := WordWrap;
end;

procedure RangeTextDefault(Sheet: Variant; const RowStart, RowEnd, ColStart, ColEnd: Integer;
  const Values: Variant; const BordersWeight: Integer);
begin
  Sheet.Range[Sheet.Cells[RowStart, ColStart], Sheet.Cells[RowEnd, ColEnd]].Value := Values;
  if BordersWeight > 0 then
    SetSingleBorders(Sheet.Range[Sheet.Cells[RowStart, ColStart],
      Sheet.Cells[RowEnd, ColEnd]], BordersWeight);
end;

procedure RangeGroup(Sheet: Variant; const RowStart, RowEnd: Integer);
begin
  Sheet.Range[Sheet.Rows[RowStart], Sheet.Rows[RowEnd]].Group;
end;

{ == ������� ������ ============================================================ }
function GetCellFloat(Sheet: Variant; const iRow, iCol: Integer): Double;
begin
  Result := 0;

  try
    if not VarIsEmpty(Sheet)
    and not VarIsEmpty(Sheet.Cells[iRow, iCol])
    and not VarIsNull(Sheet.Cells[iRow, iCol]) then
    begin
      Result := Sheet.Cells[iRow, iCol];
    end;
  except
    Result := 0;
  end;
end;

begin
  // ������������� ����������
  VarClear(MsExcel);
  VarClear(Workbook);
end.
