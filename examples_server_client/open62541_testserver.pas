unit open62541_testserver;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  open62541, open62541_utils;

type
  //! Open62541 - Server Thread
  TOpen62541ServerThread = class(TThread)
  private  
    FServer    : PUA_Server;
    FIsRunning : UA_Boolean;
    FOwner     : TObject;
  public
    constructor create(owner:TObject);
    destructor destroy;override;
    procedure execute;override;
    property OpcUaServer : PUA_Server read FServer;
    procedure AttachLogger(logFunction: TUA_Pascallogger_LogFunction);
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
var
  lConfig : PUA_ServerConfig;
  lUsernamePasswordLoginList : PUA_UsernamePasswordLogin;
  tmpStr : string;
begin
  LoadOpen62541();                               // DLL Laden
  FOwner:=Owner;
  inherited create(false);

  LoadOpen62541PascalLog();

  FServer := UA_Server_new();
  lConfig := UA_Server_getConfig(FServer);
  UA_ServerConfig_setDefault( lConfig );

  lUsernamePasswordLoginList := AllocMem( 3 * sizeof(UA_UsernamePasswordLogin) );
  tmpStr := 'Frank';
  lUsernamePasswordLoginList[0].username := _UA_STRING(tmpStr);
  tmpStr := 'Paula';
  lUsernamePasswordLoginList[1].username := _UA_STRING(tmpStr);
  tmpStr := 'Anton';
  lUsernamePasswordLoginList[2].username := _UA_STRING(tmpStr);
  tmpStr := 'Frank123';
  lUsernamePasswordLoginList[0].password := _UA_STRING(tmpStr);
  tmpStr := 'Paula123';
  lUsernamePasswordLoginList[1].password := _UA_STRING(tmpStr);
  tmpStr := 'Anton123';
  lUsernamePasswordLoginList[2].password := _UA_STRING(tmpStr);
  UA_AccessControl_default(lConfig, True, nil, @(lConfig^.securityPolicies[lConfig^.securityPoliciesSize-1].policyUri), 3, lUsernamePasswordLoginList);
end;

procedure TOpen62541ServerThread.execute;
begin
  FIsRunning := True;
  UA_Server_run(FServer, @FIsRunning);
end;

destructor TOpen62541ServerThread.destroy;
begin
  FIsRunning := false;
  inherited destroy;
end;

procedure TOpen62541ServerThread.AttachLogger(logFunction: TUA_Pascallogger_LogFunction);
begin
  if UA_Pascal_logger<>nil
    then UA_Server_getConfig(FServer)^.logger := UA_Pascal_logger(nil, logFunction);
end;

finalization
  UnloadOpen62541();
  UnLoadOpen62541PascalLog();
end.
