unit RFileCopy;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, Buttons, RMsgTypes, RFileProcs;

type
  TFormFileCopy = class(TForm)
    Animate: TAnimate;
    FileLabel: TLabel;
    SizeLabel: TLabel;
    ProgressBar: TProgressBar;
    CancelBtn: TBitBtn;
    procedure CancelBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    fCloseFlag: Boolean;
    fBreakFlag: Boolean;
    procedure ShowPrgs(Sender: TObject; const CurrPos, MaxPos: Integer);
    procedure CheckBreak(Sender: TObject; var IsBreak: Boolean);
  end;

function ShowCopyFile(const SrcFile, DstFile: string; const DelSrcFile: Boolean;
  const Options: TRfoFileFlags; const BufferSizeKb: Word; const NetUsage: Byte): TRTransaction;

implementation

{$R *.dfm}

uses
  RVclUtils, RDialogs, RMsgRu, RExHandlers;

resourcestring
  SCopyFileName   = 'Копируется файл "%s"...';
  SCopyFileSize   = 'Скопировано %.2n кБ';
  SCopyFileError  = 'Ошибка копирования (перемещения) файла "%s" -> "%s"!';

procedure TFormFileCopy.FormCreate(Sender: TObject);
begin
  Font.Name := Screen.MenuFont.Name;
  fBreakFlag := False;
  fCloseFlag := False;
end;

procedure TFormFileCopy.CancelBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TFormFileCopy.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := fCloseFlag;
  if not fCloseFlag then
    fBreakFlag := QueryBoxStdNY(SQueryBreakOperation) = ID_YES;
end;

procedure TFormFileCopy.CheckBreak(Sender: TObject; var IsBreak: Boolean);
begin
  Application.ProcessMessages;
  IsBreak := fBreakFlag;
end;

procedure TFormFileCopy.ShowPrgs(Sender: TObject; const CurrPos, MaxPos: Integer);
begin
  if MaxPos >= CurrPos then
  begin
    ProgressBar.Max := MaxPos;
    ProgressBar.Position := CurrPos;
    SizeLabel.Caption := Format(SCopyFileSize, [CurrPos / 1024]);
    Update;
  end;
end;

function ShowCopyFile(const SrcFile, DstFile: string; const DelSrcFile: Boolean;
  const Options: TRfoFileFlags; const BufferSizeKb: Word; const NetUsage: Byte): TRTransaction;
begin
  with TFormFileCopy.Create(Application) do
  begin
    try
      FileLabel.Caption := Format(SCopyFileName, [ExtractFileName(SrcFile)]);
      Animate.Active := True;
      Show;
      Update;
      try
        Result := CopyFileEx(SrcFile, DstFile, DelSrcFile, Options, nil,
          BufferSizeKb, NetUsage, nil, nil, ShowPrgs, CheckBreak);
      except
        on E: Exception do
          HandleExcept(E, nil, Format(SCopyFileError, [SrcFile, DstFile]));
      end;
    finally
      Free;
    end;
  end;
end;

end.
