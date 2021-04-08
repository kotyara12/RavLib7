unit RScrConsts;

interface

const
  SRCmdErrAttr  : array [TRCmdErrAttr] of char = (
    'C', // eaContinue,         Продолжить выполнение скрипта
    'R', // eaRestore,          Остановить обработку и откатить файлы
    'U'  // eaRunUndo           Остановить обработку и запустить процедуру Undo
    );

implementation

end.
