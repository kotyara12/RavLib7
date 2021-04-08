unit RDbFind;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDialog, StdCtrls, Buttons, ExtCtrls, RDbCustom, RDbCustomSearch;

type
{ == TRdbFind ================================================================== }

  TRDbFind = class(TRDbCustomFind)
  private
    FNoFindMsg: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    function  ShowDialog: Boolean; override;
  published
    property BadFindMessage: Boolean read FNoFindMsg write FNoFindMsg default True;
  end;

{ == Editor TRDbFind =========================================================== }

  TFormDbFind = class(TDialogTemplate)
    ComboBoxLabel: TLabel;
    ComboBox: TComboBox;
    EditLabel: TLabel;
    Edit: TComboBox;
    WordCheckBox: TCheckBox;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses
  Db, RVclUtils, RxStrUtils;

{$R *.dfm}

resourcestring
  SFindTitle          = 'Поиск';
  SErrorBadFind       = 'Строка поиска "%s" в поле "%s" не найдена!';

const
  FmtValue            = '%s;%d;%s';

{ == TRdbFind ================================================================== }

constructor TRDbFind.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNoFindMsg := True;
end;

function TRDbFind.ShowDialog: Boolean;
var
  Editor: TFormDbFind;
  i: Integer;
  FindField: TField;
  FindFld, FindText: string;
  FindWord: Integer;
  loFind: TLocateOptions;
begin
  Result := False;
  CheckDbLink;
  CheckActive;
  Editor := TFormDbFind.Create(Application);
  try
    with Editor do
    begin
      StartWait;
      try
        // Загружаем значения по умолчанию
        FindFld := DefaultField;
        FindText := EmptyStr;
        FindWord := 0;
        // Декодируем последнюю запись
        if (FLastFind <> EmptyStr) and (WordCount(FLastFind, [DelimChar]) > 2) then
        begin
          FindFld := Trim(ExtractWord(1, FLastFind, [DelimChar]));
          FindWord := StrToIntDef(Trim(ExtractWord(2, FLastFind, [DelimChar])), 0);
          for i := 3 to WordCount(FLastFind, [DelimChar]) do
          begin
            if FindText <> EmptyStr then FindText := FindText + DelimChar;
            FindText := FindText + ExtractWord(i, FLastFind, [DelimChar]);
          end;
        end;
        // Загружаем поля
        LoadKeyFieldsToCmbBox(ComboBox, DataSet, FindFld, True, False, UseHiddenFields);
        // Загружаем историю
        if olUseHistory in Options then Edit.Items.Assign(FHistory);
        // Вводим значения по умолчанию
        Edit.Text := FindText;
        WordCheckBox.Checked := FindWord = 1;
      finally
        StopWait;
      end;
      if ShowModal = mrOk then
      begin
        StartWait;
        try
          // Заполняем внутренние поля
          if WordCheckBox.Checked then FindWord := 1 else FindWord := 0;
          FindField := TField(ComboBox.Items.Objects[ComboBox.ItemIndex]);
          FindFld := FindField.FieldName;
          FindText := Edit.Text;
          FLastFind := Format(FmtValue, [FindFld, FindWord, FindText]);
          // Добавляем строку в историю
          if (olUseHistory in FOptions) and (FHistory.IndexOf(FindText) = -1) then
            FHistory.Insert(0, FindText);
          // Вычисление флагов поиска
          loFind := [loCaseInsensitive];
          if not WordCheckBox.Checked then loFind := loFind + [loPartialKey];
          // Собственно поиск
          if FindField.FieldKind <> fkLookup
          // Простой поиск
          then Result := DataSet.Locate(FindFld, FindText, loFind)
          // Поиск по Lookup-таблице
          else begin
            // Сначала поиск в Lookup-таблице
            Result := FindField.LookupDataSet.Locate(FindField.LookupResultField, FindText, loFind);
            if Result then
            begin
              // Если нашли запись, регенерим строку поиска
              FindText := EmptyStr;
              for i := 1 to WordCount(FindField.LookupKeyFields, [DelimChar]) do
              begin
                if FindText <> EmptyStr then FindText := FindText + DelimChar;
                FindText := FindText + FindField.LookupDataSet.FieldByName(
                  Trim(ExtractWord(i, FindField.LookupKeyFields, [DelimChar]))).AsString;
              end;
              // Ищем в основной таблице
              Result := DataSet.Locate(FindField.KeyFields, FindText, []);
            end;
          end;
        finally
          StopWait;
        end;
        // Вывод результата
        if not Result and FNoFindMsg
        then Application.MessageBox(PChar(Format(SErrorBadFind, [Edit.Text, ComboBox.Text])),
          PChar(SFindTitle), MB_ICONERROR + MB_OK);
      end;
    end;
  finally
    FreeAndNil(Editor);
  end;
end;

{ == Editor TRdbFind =========================================================== }

procedure TFormDbFind.FormShow(Sender: TObject);
begin
  OkBtn.Enabled := (ComboBox.Items.Count > 0) and (ComboBox.ItemIndex > -1)
    and (Edit.Text <> EmptyStr);
end;

end.
