////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Unit Name : server
//  * Purpose   : ���������������� ������ ������ �������
//  * Author    : ��������� (Rouse_) ������
//  * Copyright : � Fangorn Wizards Lab 1998 - 2010.
//  * Version   : 1.00
//  * Home Page : http://rouse.drkb.ru
//  ****************************************************************************
//

// ������� ��������� ������� ������ � ������� TFWPipeServer
// ������ ����������� �������� �� ������� ����� �����,
// ���������������� ��� � ��������� ��������� �������.
// ��� ��������� �� ������� ����� -1 ������ ������ ��������� ���� ������.

program server;

{$APPTYPE CONSOLE}

uses
  Windows,
  FWIOCompletionPipes,
  SysUtils;

type
  TSimpleObject = class
  private
    FServer: TFWPipeServer;
    NeedStop: Boolean;
  protected
    procedure Connect(Sender: TObject; PipeHandle: PFWPipeData);
    procedure Disconnect(Sender: TObject; PipeHandle: PFWPipeData);
    procedure Read(Sender: TObject; PipeInstance: PFWPipeData);
    procedure Idle(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    property Server: TFWPipeServer read FServer;
  end;

{ TSimpleObject }

//  ������������� �������
// =============================================================================
constructor TSimpleObject.Create;
begin
  // ���� NeedStop ������������ ��� ��������� �������
  NeedStop := False;
  // ������� ������
  FServer := TFWPipeServer.Create('FWIOCompletionPipeServer');
  // ��������� �����������
  FServer.OnConnect := Connect;
  FServer.OnDisconnect := Disconnect;
  FServer.OnNeedProcessReadAndWrite := Read;
  FServer.OnIdle := Idle;
end;

destructor TSimpleObject.Destroy;
begin
  FServer.Free;
  inherited;
end;

//  ����� ���������� ��� ������������� ������ �������
// =============================================================================
procedure TSimpleObject.Connect(Sender: TObject; PipeHandle: PFWPipeData);
begin
  Writeln('New client connected. Handle ', PipeHandle^.PipeHandle);
end;

//  ����� ���������� ��� ������������ �������
// =============================================================================
procedure TSimpleObject.Disconnect(Sender: TObject; PipeHandle: PFWPipeData);
begin
  Writeln('Client with handle ', PipeHandle^.PipeHandle, ' disconnected');
end;

//  ����� ���������� � ��� ������ ����� ������ ����� �� �����
// =============================================================================
procedure TSimpleObject.Idle(Sender: TObject);
begin
  if NeedStop then
    FServer.Active := False;
end;

//  ����� ���������� ��� ��������� ������ �� �������
// =============================================================================
procedure TSimpleObject.Read(Sender: TObject; PipeInstance: PFWPipeData);
var
  Data: DWORD;
begin
  // ��������� ������ ��������� �������.
  // � ������ ���� ������ ������ ������ ������
  // ���������� ������ �������� � 4 �����
  if PipeInstance^.ReadBuffSize <> 4 then
    raise Exception.Create('Wrong readbuff size.');
  // ������ ����������� ������
  Move(PipeInstance^.ReadBuff[0], Data, 4);
  // ��������, �������� �� ����� -1?
  if Data = DWORD(-1)  then
    // ���� �������� - �� ���������� ���� � ������������� ��������� �������
    // ������ ���� ����� ������� � ������ IDLE � ������ ����� ��������� ����������
    // ���� ������������� ������ ����� ������ �������� FServer.Active := False,
    // �� ������ ������� ������ � ��� ��� �� ������ ������� ����� ������ ���.
    NeedStop := True
  else
  begin
    // ���� �������� ����� �������� �� -1, �������������� ��� � ���������� �������
    Inc(Data);
    Move(Data, PipeInstance^.WriteBuff[0], 4);
    // ��� ���� �� �������� ������� ������ ������������� �������
    PipeInstance^.WriteBuffSize := 4;
  end;
end;

var
  SimpleObj: TSimpleObject;
begin
  try
    SimpleObj := TSimpleObject.Create;
    try
      // ������ ������, ����� ������� Server.Active := True ���������� ��
      // ��������� ������ ���� �� ���������� �� ��� ���, ���� ������
      // �� ����� ����������, �.�. �� ����� ��������� �������
      // Server.Active := False
      SimpleObj.Server.Active := True;
      Writeln('Server stopped');
    finally
      SimpleObj.Free;
    end;
  except
    on E:Exception do
      Writeln(E.Classname, ': ', E.Message);
  end;
  Readln;
end.
