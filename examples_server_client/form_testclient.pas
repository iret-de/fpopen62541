unit form_testclient;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  open62541;

type

  { TForm1 }

  TForm1 = class(TForm)
    btDisconnect: TButton;
    btFindNodeID: TButton;
    btnConnect: TButton;
    btReadVar1: TButton;
    btReadVar2: TButton;
    btSetVar1: TButton;
    btSubscribeVar1: TButton;
    btTestBrowse: TButton;
    btTestConection: TButton;
    btTestTranslate: TButton;
    Button1: TButton;
    cbServer: TComboBox;
    Edit1: TEdit;
    edNameSpace: TEdit;
    edPath: TEdit;
    edVar1: TEdit;
    edVar2: TEdit;
    Label1: TLabel;
    lbNameSpace: TLabel;
    lbSearchPath: TLabel;
    lbValueVar2: TLabel;
    Memo1: TMemo;
    PageControl1: TPageControl;
    panEdits: TPanel;
    panMemo: TPanel;
    panTop: TPanel;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TimerClientIterate: TTimer;
    procedure btDisconnectClick(Sender: TObject);
    procedure btFindNodeIDClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btReadVar1Click(Sender: TObject);
    procedure btReadVar2Click(Sender: TObject);
    procedure btSetVar1Click(Sender: TObject);
    procedure btSubscribeVar1Click(Sender: TObject);
    procedure btTestBrowseClick(Sender: TObject);
    procedure btTestConectionClick(Sender: TObject);
    procedure btTestTranslateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerClientIterateStartTimer(Sender: TObject);
    procedure TimerClientIterateTimer(Sender: TObject);
  private
    FClient       : PUA_Client;
    FStatusCode   : UA_StatusCode;           //!< Hilfsvariable für den Status von Server-Aufrufen
    FVariant      : UA_Variant;              //!< Hilfsvariable für das Setzen von Werten
    FSubscriptionID : UA_Int32;              //!< ID der "subscription" für das Monitoring von Variablen
    FTimerLocked  : Boolean;                 //!< Re-Entrance-Schutz für den Timer-Callback
    function CheckStatuscode(aStatus: UA_StatusCode; const aMessage: string): Boolean;
    function CheckClient : Boolean;
    procedure CreateSubscription;
    procedure LogDetail(const aMessage: string);
    procedure LogError(const aMessage: string);
    function ReadVariableAsString(const aNodeID: UA_NodeID; const aInfoName: string): string; overload;
    function ReadVariableAsString(const aID, aNamespaceIndex: integer): string; overload;
    procedure SetIntegerValue (const aName : string; aValue : integer);
    procedure InternalSetVarValue (const aName: string; const aVariant : UA_Variant);
    function IdentifierTypeToString(const idType: UA_NodeIdType): string;
    function NodeClassToString(const ua_class: UA_NodeClass): string;
  private
    procedure translateBrowsePathsToNodeIdsRequest;
    procedure browseServerRecursive (aNameSpace, aNodeID: integer); // Durchsucht alle Nodes die unter aNodeID zu finden sind und listet diese auf, nur dass sich die prozedur rekursiv selbst aufruft und alle weiteren Kinde mit auflistet.
    procedure browseServerRecursiveHelp (aNodeID: UA_NodeId; level: integer); // Hilfsprozedur, die die eigentliche Rekursion macht.
    function FindNode(const aNodeID: UA_NodeID; aSearchPath: string): UA_NODEID;
  public

  end;

var
  Form1: TForm1;

implementation

uses
  URIParser,
  open62541_helpers, open62541_utils,
  rec_test_const;

{$R *.lfm}

{ TForm1 }

procedure TForm1.LogError(const aMessage: string);
begin
  Memo1.Lines.Add ('Error: ' + ' -> ' + aMessage);
end;

procedure TForm1.LogDetail(const aMessage: string);
begin
  Memo1.Lines.Add ('Info : ' + ' -> ' + aMessage);
end;

procedure TForm1.btnConnectClick(Sender: TObject);
var
  conf: PUA_ClientConfig;
  lRetval : UA_StatusCode;

  lValue  : UA_Variant;  // Variants can hold scalar values and arrays of any type
  lNodeId : UA_NodeId;
  dts     : UA_DateTimeStruct;
  lInt32  : longint;
begin
  LoadOpen62541();                               // DLL Laden
  FClient := UA_Client_new();
  conf := UA_Client_getConfig(FClient);
  conf^.clientDescription.applicationName := _UA_LOCALIZEDTEXT_ALLOC('en-US','My Test Application');
  UA_ClientConfig_setDefault(conf);

  lRetval := UA_Client_connect(FClient, cbServer.Text); // 'opc.tcp://localhost:4840/');    // *********** *********
  if lRetval <> UA_STATUSCODE_GOOD
  then begin
    Memo1.Lines.Add ('Hat nicht geklappt! StatusCode: ' + IntToHex(lRetval));
  end
  else begin
    (* Read the value attribute of the node. UA_Client_readValueAttribute is a
     * wrapper for the raw read service available as UA_Client_Service_read. *)
    UA_Variant_init(lValue);
    // NodeId of the variable holding the current time
    lNodeID := UA_NODEID_NUMERIC(0, UA_NS0ID_SERVER_SERVERSTATUS_CURRENTTIME);
    lRetval := UA_Client_readValueAttribute(FClient, lNodeId, lValue);
    Memo1.Lines.Add ('Status-Code: ' + IntToHex(lRetval));
    if (lRetval = UA_STATUSCODE_GOOD)
       and (UA_Variant_hasScalarType(@lValue, @UA_TYPES[UA_TYPES_DATETIME]))
    then begin
      UA_Client_readValueRankAttribute(FClient, lNodeId, lInt32);
//??      raw_date := *(UA_DateTime *) value.data;
      dts      := UA_DateTime_toStruct(PUA_DateTime(lValue.data)^);
      Memo1.Lines.Append(Format('Date on server is: %d.%d.%d %d:%d:%d (ValueRank=%d)', [dts.day, dts.month, dts.year, dts.hour, dts.min, dts.sec, lInt32]));
(*
      UA_LOG_INFO(UA_Log_Stdout, UA_LOGCATEGORY_USERLAND, "date is: %u-%u-%u %u:%u:%u.%03u\n",
                  dts.day, dts.month, dts.year, dts.hour, dts.min, dts.sec, dts.milliSec);
      if and (UA_Variant_hasScalarType(@value, @UA_TYPES[UA_TYPES_DATETIME])) then begin
        UA_Client_readValueRankAttribute(client, nodeId, i32);
        dts := UA_DateTime_toStruct(PUA_DateTime(value.data)^);
*)
    end;

    UA_Variant_clear(lValue);
  end;
(*
*)
end;

procedure TForm1.btDisconnectClick(Sender: TObject);
begin
  if assigned (FClient)
    then UA_Client_delete(FClient);    // Disconnects the client internally
  FClient := nil;
end;

procedure TForm1.btFindNodeIDClick(Sender: TObject);
var
  lNode : UA_NodeId;
  lPath : string;
begin
  lPath := edPath.Text;
  lNode := FindNode(UA_NODEID_NUMERIC(0, UA_NS0ID_OBJECTSFOLDER), lPath);
  if UA_NodeId_isNull (@lNode)
    then Memo1.Lines.Add ('Object not found! ' + lPath)
  else if lNode.identifierType = UA_NODEIDTYPE_STRING
     then Memo1.Lines.Add ('Namespace: ' + IntToStr (lNode.namespaceIndex) + ' Node: ' + UA_NodeIdToStr(lNode))
  else if lNode.identifierType = UA_NODEIDTYPE_NUMERIC
    then Memo1.Lines.Add ('Namespace: ' + IntToStr (lNode.namespaceIndex) + ' Node: ' + IntToStr (lNode.identifier.numeric))
  else
    Memo1.Lines.Add ('WARN! NodeID foudn but not STRING or NUMERIC');
end;

function TForm1.CheckStatuscode(aStatus: UA_StatusCode; const aMessage: string): Boolean;
begin
  Result := aStatus = UA_STATUSCODE_GOOD;
  if Result
    then LogDetail(aMessage)
    else LogError(aMessage + ': ' + UA_Explain_StatusCode (aStatus));
end;

function TForm1.CheckClient: Boolean;
begin
  Result := assigned (FClient);
  if not Result
    then LogError('Kein Client verbunden!');
end;

procedure TForm1.btReadVar1Click(Sender: TObject);
var
  lNode : UA_NodeId;
begin
  lNode := UA_GetNodeIDFromPath(FClient, UA_NODEID_NUMERIC(0, UA_NS0ID_OBJECTSFOLDER), cOpcVarnameVar1);
  if UA_NodeId_isNull (@lNode)
    then Memo1.Lines.Add ('Object not found! ' + cOpcVarnameVar1)
    else edVar1.Text := ReadVariableAsString(lNode, cOpcVarnameVar1);
  UA_NodeId_delete(lNode);                          // Neccessary if it is a "UA_NODEIDTYPE_STRING"
end;

procedure TForm1.btReadVar2Click(Sender: TObject);
var
  lNode : UA_NodeId;
begin
  lNode := UA_GetNodeIDFromPath(FClient, UA_NODEID_NUMERIC(0, UA_NS0ID_OBJECTSFOLDER), edPath.Text);
  if UA_NodeId_isNull (@lNode)
    then Memo1.Lines.Add ('Object not found! ' + edPath.Text)
    else edVar2.Text := ReadVariableAsString(lNode, edPath.Text);
  UA_NodeId_delete(lNode);                          // Neccessary if it is a "UA_NODEIDTYPE_STRING"
end;

procedure TForm1.btSetVar1Click(Sender: TObject);
begin
  SetIntegerValue(cOpcVarnameVar1, StrToIntDef(edVar1.Text, -1));
end;

procedure TForm1.btSubscribeVar1Click(Sender: TObject);
begin
  CreateSubscription;
end;

procedure TForm1.btTestBrowseClick(Sender: TObject);
begin
  // toDelete: lNodeIdentifier := UA_GetNodeIDFromPath(FClient, UA_NODEID_NUMERIC(0, UA_NS0ID_OBJECTSFOLDER), cOpcVarnameBool1).identifier.numeric;
  // browseServer (0, UA_NS0ID_OBJECTSFOLDER);
  browseServerRecursive (0, UA_NS0ID_OBJECTSFOLDER);
end;

procedure TForm1.btTestConectionClick(Sender: TObject);
var
  lPath : string;
  lNodeId: UA_NodeId;
  lDisplayName: UA_LocalizedText;
  lStatus: UA_StatusCode;
begin
  lPath := edPath.Text;

  lNodeId := UA_NODEID_STRING_ALLOC(StrToInt(edNameSpace.Text), lPath);

  Memo1.Lines.Add('Looking up Node:');
  Memo1.Lines.Add('Node: ' + UA_NodeIdToStr(lNodeId));

  lStatus := UA_Client_readDisplayNameAttribute(FClient, lNodeId, lDisplayName);

  if CheckStatuscode(lStatus, 'Checked NodeDisplayName')
  then Memo1.Lines.Add('Displayname of Node: ' + UA_StringToStr(lDisplayName.text));
end;

procedure TForm1.btTestTranslateClick(Sender: TObject);
begin
  translateBrowsePathsToNodeIdsRequest();
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FSubscriptionID := -1;
  edPath.Text := cOpcFolderName + cOpcUAPathDelimiter + cOpcVarnameObj1
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  btDisconnectClick(nil);
end;

procedure TForm1.TimerClientIterateStartTimer(Sender: TObject);
begin
  FTimerLocked := False;
end;

procedure TForm1.TimerClientIterateTimer(Sender: TObject);
var
  lStatusCode   : UA_StatusCode;           // Status des Run_Iterate Aufrufs
begin
  if FTimerLocked then exit;
  FTimerLocked := True;
  try
    if assigned (FClient)
    then begin
      lStatusCode := UA_Client_run_iterate (FClient, 50);
      if lStatusCode <> UA_STATUSCODE_GOOD
        then CheckStatuscode(lStatusCode, 'run_iterate') ;
    end;
  finally
    FTimerLocked := False;
  end;
end;

function GetUA_VariantAsString (var aVariant : UA_Variant) : string;
begin
  if UA_Variant_hasScalarType(@aVariant, @UA_TYPES[UA_TYPES_BYTE]) then
    Result := IntToStr(UA_Variant_getByte(aVariant))
  else if UA_Variant_hasScalarType(@aVariant, @UA_TYPES[UA_TYPES_INT16]) then
    Result := IntToStr(UA_Variant_getSmallint(aVariant))
  else if UA_Variant_hasScalarType(@aVariant, @UA_TYPES[UA_TYPES_INT32]) then
    Result := IntToStr(UA_Variant_getInteger(aVariant))
  else if UA_Variant_hasScalarType(@aVariant, @UA_TYPES[UA_TYPES_STRING]) then
    Result := UA_Variant_getString(aVariant) // value._type^.memSize == SizeOf(UA_STRING)
  else if UA_Variant_hasScalarType(@aVariant, @UA_TYPES[UA_TYPES_BOOLEAN]) then
    Result := BoolToStr(PUA_Boolean(aVariant.data)^, True)
  else
    raise Exception.Create ('Unbekannter "UA_Variant"-Typ');
end;

function TForm1.ReadVariableAsString(const aNodeID : UA_NodeID; const aInfoName : string): string;
var
  dataType: UA_NodeId;
  value: UA_Variant;
begin
  Result := '<Error>';
  if not CheckClient then exit;

  // get data type of requested variable
  FStatusCode := UA_Client_readDataTypeAttribute(FClient, aNodeId, dataType);
  if CheckStatuscode (FStatusCode, 'UA_Client_readDataTypeAttribute')
    then Memo1.Lines.Append(Format('Node "%s" data type: %s (%s)', [aInfoName, UA_NodeIdToStr(dataType), UA_DataTypeToStr(dataType)]));

  // get value of requested variable
  FStatusCode := UA_Client_readValueAttribute(FClient, aNodeId, value);
  if CheckStatuscode (FStatusCode, 'UA_Client_readValueAttribute')
    then Result := GetUA_VariantAsString (value)
    else Result := '<Error>';
end;

function TForm1.ReadVariableAsString(const aID, aNamespaceIndex : integer): string;
var
  nodeId, dataType: UA_NodeId;
  value: UA_Variant;
begin
  Result := '<Error>';
  if not assigned (FClient)
  then begin
    Memo1.Lines.Add ('Keine Verbidnung zum Server');
    exit;
  end;
  NodeId := UA_NODEID_NUMERIC(aNamespaceIndex, aID);

  // get data type of requested variable
  FStatusCode := UA_Client_readDataTypeAttribute(FClient, nodeId, dataType);
  if CheckStatuscode (FStatusCode, 'UA_Client_readDataTypeAttribute')
    then Memo1.Lines.Append(Format('Node "%s" data type: %s (%s)', [IntTOStr (aID), UA_NodeIdToStr(dataType), UA_DataTypeToStr(dataType)]));

  // get value of requested variable
  FStatusCode := UA_Client_readValueAttribute(FClient, nodeId, value);
  if CheckStatuscode (FStatusCode, 'UA_Client_readValueAttribute')
    then Result := GetUA_VariantAsString (value)
    else ; //NodeId := UA_NODEID_STRING_ALLOC(1, aName);
  {$IFDEF UA_VER1_3}
  Memo1.Lines.Append(Format('Node "%s" read value: %s (Size=%d, Type=%s (typeId=%s,typeIndex=%d); Result=%x)', [IntToStr (aID), Result, value._type^.memSize, value._type^.typeName, UA_NodeIdToStr(value._type^.typeId), value._type^.typeIndex, FStatusCode]));
  {$ELSE}
  Memo1.Lines.Append(Format('Node "%s" read value: %s (Size=%d, Type=%s (typeId=%s); Result=%x)', [IntToStr (aID), Result, value._type^.memSize, value._type^.typeName, UA_NodeIdToStr(value._type^.typeId), FStatusCode]));
  {$ENDIF}
  UA_NodeId_clear(nodeId);

end;


//! Wert einer Integer Variablen anhand des Namens setzen
//!
procedure TForm1.SetIntegerValue (const aName : string; aValue : integer);
begin
  UA_Variant_init(FVariant);
  UA_Variant_setInteger(FVariant, aValue);
  InternalSetVarValue(aName, FVariant);
end;

//! Werte einer Variablen angand des Namens setzen
//! !!! "aVariant" muss passend zum Typ der Variablen erzeigt werden
//!
procedure TForm1.InternalSetVarValue (const aName: string; const aVariant : UA_Variant);
var
  intVariableNodeId: UA_NodeId;
begin
  if not CheckClient then exit;
  intVariableNodeId := UA_NODEID_STRING_ALLOC(1 {name space index}, aName);
  try
    FStatusCode := UA_Client_writeValueAttribute(FClient, intVariableNodeId, aVariant);
    CheckStatuscode(FStatusCode, 'Client write to variable "' + aName + '"');
  finally
    UA_NodeId_clear(intVariableNodeId);
  end;
end;

procedure handler_TheAnswerChanged(client: PUA_Client; subId: UA_UInt32; subContext: Pointer; monId: UA_UInt32;
  monContext: Pointer; aValue: PUA_DataValue); cdecl;
var
  lValue : UA_Int32;
begin
  lValue := UA_Variant_getInteger(aValue^.Value);
  Form1.edVar1.Text := IntToStr(lValue);
end;


procedure TForm1.CreateSubscription;
var
  request  : UA_CreateSubscriptionRequest;
  response : UA_CreateSubscriptionResponse;
  monRequest  : UA_MonitoredItemCreateRequest;
  monResponse : UA_MonitoredItemCreateResult;
  lVariableName : string;
begin
  if not CheckClient then exit;

  lVariableName := cOpcVarnameVar1;

  request  := UA_CreateSubscriptionRequest_default();
  response := UA_Client_Subscriptions_create(FClient, request, nil, nil, nil);

  FSubscriptionID := response.subscriptionId;
  if CheckStatuscode(response.responseHeader.serviceResult, 'Create subscription, the ID is ' + FSubscriptionID.ToString)
  then begin
    monRequest  := UA_MonitoredItemCreateRequest_default(UA_NODEID_STRING(1, lVariableName));
    monResponse := UA_Client_MonitoredItems_createDataChange(FClient, response.subscriptionId,
                                              UA_TIMESTAMPSTORETURN_BOTH,
                                              monRequest, nil, @handler_TheAnswerChanged, nil);
    CheckStatuscode (monResponse.statusCode, 'Monitoring "Var1-integer", ID is ' + monResponse.monitoredItemId.ToString);

    // The first publish request should return the initial value of the variable
//    UA_Client_run_iterate(FClient, 1000);
  end;
end;

// Just for testing we want to translate the following path to its corresponding node id
// /Objects/Server/ServerStatus/State
// Equals the following node IDs:
// /85/2253/2256/2259
//
// \note Code is similar to translateBrowsePathsToNodeIdsRequest tests/fuzz/corpus_generator.c
//
procedure TForm1.translateBrowsePathsToNodeIdsRequest;
const
  BROWSE_PATHS_SIZE = 3;
  paths              : array [0..BROWSE_PATHS_SIZE-1] of string = ('Server', 'ServerStatus', 'State');  // Hat jeder Server!!!!! Oder nur Open62541?
  ids                : array [0..BROWSE_PATHS_SIZE-1] of UA_UInt32 = (UA_NS0ID_ORGANIZES, UA_NS0ID_HASCOMPONENT, UA_NS0ID_HASCOMPONENT);
var
  browsePath : UA_BrowsePath;
  i          : integer;
  lElem       : PUA_RelativePathElement;
  request    : UA_TranslateBrowsePathsToNodeIdsRequest;
  response   : UA_TranslateBrowsePathsToNodeIdsResponse;
begin
  if not CheckClient then exit;
    UA_BrowsePath_init(browsePath);
    browsePath.startingNode := UA_NODEID_NUMERIC(0, UA_NS0ID_OBJECTSFOLDER);
    browsePath.relativePath.elements     := PUA_RelativePathElement(UA_Array_new(BROWSE_PATHS_SIZE, @UA_TYPES[UA_TYPES_RELATIVEPATHELEMENT]));
    browsePath.relativePath.elementsSize := BROWSE_PATHS_SIZE;

    for i := 0 to BROWSE_PATHS_SIZE-1
    do begin
        lElem := @browsePath.relativePath.elements[i];
        lElem^.referenceTypeId := UA_NODEID_NUMERIC(0, ids[i]);
        lElem^.targetName      := _UA_QUALIFIEDNAME_ALLOC(0, paths[i]);
    end;

    UA_TranslateBrowsePathsToNodeIdsRequest_init(request);
    request.browsePaths := @browsePath;
    request.browsePathsSize := 1;

    response := UA_Client_Service_translateBrowsePathsToNodeIds(FClient, request);

    CheckStatuscode(response.responseHeader.serviceResult, 'TestBrowsePath');
    if not (response.resultsSize = 1) then LogError ('response.resultsSize = 1')
    else if not (response.results[0].targetsSize = 1) then LogError ('response.results[0].targetsSize = 1')
    else if not (response.results[0].targets[0].targetId.nodeId.identifierType = UA_NODEIDTYPE_NUMERIC) then LogError ('response.results[0].targets[0].targetId.nodeId.identifierType, UA_NODEIDTYPE_NUMERIC')
    else if not (response.results[0].targets[0].targetId.nodeId.identifier.numeric = UA_NS0ID_SERVER_SERVERSTATUS_STATE) then LogError ('response.results[0].targets[0].targetId.nodeId.identifier.numeric, UA_NS0ID_SERVER_SERVERSTATUS_STATE');

    UA_BrowsePath_clear(@browsePath);
    UA_TranslateBrowsePathsToNodeIdsResponse_clear(@response);
end;

procedure TForm1.browseServerRecursive (aNameSpace, aNodeID : integer);
begin
  if not CheckClient then exit;

  Memo1.Lines.Add( Format ('%11-s %-28s %-3s %-6s %-7s %-13s',
                   ['Tree', 'DISPLAY NAME', 'NS', 'NODEID', 'idFier', 'node_class']) );
  browseServerRecursiveHelp(UA_NODEID_NUMERIC(aNamespace, aNodeID), 0);
end;

procedure TForm1.browseServerRecursiveHelp (aNodeID: UA_NodeID; level: integer);
var
  bReq : UA_BrowseRequest;
  bResp : UA_BrowseResponse;
  ref   : PUA_ReferenceDescription;
  i, j, k : integer;
  levelStr : string;
begin
  if level > 10 then exit();

  UA_BrowseRequest_init(bReq);
  try
    bReq.requestedMaxReferencesPerNode := 0;
    bReq.nodesToBrowse                 := UA_BrowseDescription_new();
    bReq.nodesToBrowseSize             := 1;
    bReq.nodesToBrowse[0].nodeId       := aNodeID; //* browse objects folder */
    bReq.nodesToBrowse[0].resultMask   := UA_Int32(UA_BROWSERESULTMASK_ALL);                      //* return everything */
    bResp := UA_Client_Service_browse(FClient, bReq);

    if level = 0
    then  levelStr := ' '
    else begin
      levelStr := '';
      k := 1;
      while k < level
      do begin
          levelStr := levelStr + ' ';
          inc(k);
      end;
      levelStr := levelStr + '> '
    end;
    i := 0;
    while i < bResp.resultsSize
    do begin
      j := 0;
      while j < bResp.results[i].referencesSize
      do begin
        ref := @(bResp.results[i].references[j]);
        if (ref^.nodeClass <> UA_NODECLASS_OBJECTTYPE) and
            (ref^.nodeClass <> UA_NODECLASS_VARIABLETYPE) and
            (ref^.nodeClass <> UA_NODECLASS_REFERENCETYPE)
        then
        begin
          Memo1.Lines.Add( Format ('%-40s %-3d %-6d %-7s %-13s', [
                   levelStr+UA_StringToStr(ref^.displayName.text),
                   ref^.nodeId.nodeId.namespaceIndex,
                   ref^.nodeId.nodeId.identifier.numeric,
                   IdentifierTypeToString(ref^.nodeId.nodeId.identifierType),
                   NodeClassToString(ref^.nodeClass)])
              );

          // rekursiver Aufruf
          browseServerRecursiveHelp(ref^.nodeId.nodeId, level+1);
        end;
          inc (j);
      end;
      inc (i);
    end;
  finally
    UA_BrowseRequest_delete(@bReq);
    UA_BrowseResponse_delete(@bResp);
  end;
end;

function TForm1.IdentifierTypeToString(const idType: UA_NodeIdType): string;
begin
  if idType = UA_NODEIDTYPE_NUMERIC then Result := 'numeric'
  else if idType = UA_NODEIDTYPE_STRING then Result := 'string'
  else if idType = UA_NODEIDTYPE_GUID then Result := 'guid'
  else if idType = UA_NODEIDTYPE_BYTESTRING then Result := 'bytestr';
end;

function TForm1.NodeClassToString(const ua_class: UA_NodeClass): string;
begin
  if ua_class = UA_NODECLASS_UNSPECIFIED
    then RESULT := 'unspecified'
  else if ua_class = UA_NODECLASS_OBJECT
    then Result := 'object'
  else if ua_class = UA_NODECLASS_VARIABLE
    then Result := 'variable'
  else if ua_class = UA_NODECLASS_METHOD
    then Result := 'method'
  else if ua_class = UA_NODECLASS_OBJECTTYPE
    then Result := 'objecttype'
  else if ua_class = UA_NODECLASS_VARIABLETYPE
    then Result := 'variabletype'
  else if ua_class = UA_NODECLASS_REFERENCETYPE
    then Result := 'referencetype'
  else if ua_class = UA_NODECLASS_DATATYPE
    then Result := 'datatype'
  else if ua_class = UA_NODECLASS_VIEW
    then Result := 'view'
  else Result := 'unknowen';
end;

function TForm1.FindNode (const aNodeID : UA_NodeID; aSearchPath : string): UA_NODEID;
begin
  Result := open62541_utils.UA_GetNodeIDFromPath (FClient, aNodeID, aSearchPath);
end;

end.


