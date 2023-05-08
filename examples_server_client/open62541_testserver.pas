unit open62541_testserver;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  open62541;

type
  //! Open62541 - Server Thread
  TOpen62541ServerThread = class(TThread)
  private
    FIsRunning : UA_Boolean;
    FServer    : PUA_Server;
    FOwner     : TObject;
    procedure Log(const msg: string);
  public
    constructor create(owner:TObject);
    destructor destroy;override;
    procedure execute;override;
    property OpcUaServer : PUA_Server read FServer;
  end;

implementation

{ TServerThread }

(*
procedure server_log_cb(logContext: Pointer; level: UA_LogLevel;
  category: UA_LogCategory; msg: PAnsiChar); cdecl;
begin
  TServerThread(logContext).Log(msg);
end;

destructor TServerThread.destroy;
begin
  running:=false;
  inherited destroy;
  UnloadOpen62541();
end;

procedure TServerThread.Log(const msg:string);
var
  s: PString;
begin
  new(s);
  s^:=msg;
  Application.QueueAsyncCall(@FOwner.LogServer,ptrint(s));
end;

procedure TServerThread.execute;
var
  res: UA_StatusCode;
  conf: PUA_ServerConfig;
begin
  server:=UA_Server_new();
  conf:=UA_Server_getConfig(server);
  if UA_Pascal_logger<>nil then
    conf^.logger:=UA_Pascal_logger(self, @server_log_cb);
  res:=UA_ServerConfig_setDefault(conf);
  running:=true;
  res:=UA_Server_run(server, @running);
end;

*)

{ TOpen62541ServerThread }

constructor TOpen62541ServerThread.create(owner: TObject);
begin
  LoadOpen62541();                               // DLL Laden
  FOwner:=Owner;
  inherited create(false);
end;

destructor TOpen62541ServerThread.destroy;
begin
  FIsRunning := false;
  inherited destroy;
end;

procedure TOpen62541ServerThread.execute;
var
  res: UA_StatusCode;
  conf: PUA_ServerConfig;
begin
  FServer := UA_Server_new();
  conf    := UA_Server_getConfig(FServer);
//???  if UA_Pascal_logger<>nil then conf^.logger:=UA_Pascal_logger(self, @server_log_cb);
  res     := UA_ServerConfig_setDefault(conf);
  FIsRunning := True;
  res := UA_Server_run(FServer, @FIsRunning);
end;

procedure TOpen62541ServerThread.Log(const msg: string);
begin

end;

finalization
  UnloadOpen62541();
end.

