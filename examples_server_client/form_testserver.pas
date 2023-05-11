unit form_testserver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Buttons,
  open62541,
  open62541_testserver, rec_test_const;

type

  { TForm1 }

  TForm1 = class(TForm)
    AddVar1: TBitBtn;
    AddVar2: TBitBtn;
    AddVar3: TBitBtn;
    btObjectVar1: TButton;
    btObjectVar2: TButton;
    btSetVar1: TButton;
    btSetVar2: TButton;
    btSetObjectVar1: TButton;
    btSetObjectVar2: TButton;
    btStart: TToggleBox;
    btSetVar3: TButton;
    btAddBool1: TButton;
    btInvertBool1: TButton;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Memo1: TMemo;
    panButtons: TPanel;
    pnaInfo: TPanel;
    procedure AddVar1Click(Sender: TObject);
    procedure AddVar2Click(Sender: TObject);
    procedure AddVar3Click(Sender: TObject);
    procedure btInvertBool1Click(Sender: TObject);
    procedure btObjectVar2Click(Sender: TObject);
    procedure btObjectVar1Click(Sender: TObject);
    procedure btSetVar1Click(Sender: TObject);
    procedure btSetVar2Click(Sender: TObject);
    procedure btSetObjectVar2Click(Sender: TObject);
    procedure btSetObjectVar1Click(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure btSetVar3Click(Sender: TObject);
    procedure btAddBool1Click(Sender: TObject);
    procedure btTest1Click(Sender: TObject);
    procedure btTest2Click(Sender: TObject);
    procedure btTest3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    FServer : TOpen62541ServerThread;
    FLocale : AnsiString;
    FVariableName    : AnsiString;              //!< Hilfsvariable zum Setzen eines Variablen-Namens
    FStatusCode      : UA_StatusCode;           //!< Hilfsvariable für den Status von Server-Aufrufen
    FVariant         : UA_Variant;              //!< Hilfsvariable für das Setzen von Werten
    FNodeIDFolder    : UA_NodeId;               //!< ID des Objects "folder"
    FNodeIDSubFolder : UA_NodeId;               //!< ID des Objects "folder/subfolder"
    FNodeObjectVar1  : UA_NodeId;               //!< ID der ersten Variablen im Objekt folder
    FNodeObjectVar2  : UA_NodeId;               //!< ID der zweiten Variablen im Objekt folder
    FCounter         : integer;                 //!< Dummy-Variable zum Erzeugen von Testwerten
    FStatus          : boolean;                 //!< Dummy-Status zum invertieren und setzen
    SubFolder2       : UA_NodeId;
    function AddObjectFolder : UA_NodeId;
    function AddObjectSubFolder: UA_NodeId;
    function AddStringVariable(var aName: string; aInitialValue: string) : UA_NodeId;
    function AddIntegerVariable (var aName : string; aInitialValue : integer) : UA_NodeId;
    function AddBooleanVariable (var aName : string; aInitialValue : boolean) : UA_NodeId;
    function AddStringVariableObject(var aName : string; aInitialValue : string; const aParent : UA_NodeId) : UA_NodeId;
    function InternalAddVariable(var aAttributes: UA_VariableAttributes; aName: string; aInitialValue: string) : UA_NodeId;
    procedure InternalSetVarValue(const aName: string; const aVariant: UA_Variant); overload;
    procedure InternalSetVarValue(const aNodeID: UA_NodeId; const aVariant: UA_Variant); overload;
    function InternalGetVarValue(const aName:string) : UA_Variant; overload;
    function ReadIntegerValue (const aName: string) : integer;
    procedure SetIntegerValue(const aName: string; aValue: integer); overload;
    procedure SetStringValue(const aName: string; aValue: string);
    procedure SetIntegerValue(const aNodeID : UA_NodeId; aValue: integer); overload;
    procedure SetBooleanValue(const aNodeId: UA_NodeId; aValue: boolean);
    procedure SetBooleanValue(const aName : string; aValue: boolean);
    function GetBooleanValue(const aName:string): boolean;
    function GetIntegerValue(const aName:string): integer;
    procedure StartStopServer;
    procedure LogError (const aNodeID : UA_NodeId;const aMessage : string);
    procedure LogDetail(const aNodeID : UA_NodeId;const aMessage : string);
    function CheckServer : Boolean;
    function CheckStatuscode (aStatus : UA_StatusCode; const aNodeID : UA_NodeId;const aMessage : string) : Boolean;
    procedure clearNodes;
  protected
    procedure ForceFolder;
    procedure ForceSubFolder;
  public

  end;

var
  Form1: TForm1;

implementation

uses
  StrUtils, open62541_helpers;

{$R *.lfm}

//! Duplicate code in form_irettestclient.pas
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
  else begin
    raise Exception.Create ('Unbekannter "UA_Variant"-Typ');
  end;
end;

procedure TForm1.StartStopServer;
begin
  if Assigned(FServer)
  then begin
    FreeAndNil(FServer);
    clearNodes;
    btStart.Checked := False;
    btStart.Caption := 'Start Server' //! If changed, also change Caption of Button in .lfm File
  end else
  begin
    FServer := TOpen62541ServerThread.create(self);
    btStart.Checked := True;
    btStart.Caption := 'Stop server';
  end;
  LogDetail (UA_NODEID_NULL, 'Server ' + ifthen (assigned (FServer), 'started', 'stopped'));
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FreeAndNil(FServer);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FLocale  := 'en-US';
  FCounter := 0;
  FStatus  := false;
  clearNodes;
end;

procedure TForm1.clearNodes;
begin
  FNodeIDFolder := UA_NODEID_NULL;
  FNodeObjectVar1 := UA_NODEID_NULL;
  FNodeObjectVar2 := UA_NODEID_NULL;
  FNodeIDSubFolder  := UA_NODEID_NULL;
end;

procedure TForm1.btStartClick(Sender: TObject);
begin
  StartStopServer();
end;

procedure TForm1.btSetVar3Click(Sender: TObject);
begin
  inc (FCounter);
  SetStringValue(cOpcVarnameVar3, 'Var3: string ' + FCounter.ToString);
end;

procedure TForm1.btInvertBool1Click(Sender: TObject);
begin
  FStatus := Not FStatus;
  SetBooleanValue(cOpcVarnameBool1, FStatus);
  if FStatus <> GetBooleanValue(cOpcVarnameBool1)
    then ShowMessage ('Unterschiedliche Werte beim Lesen und Schreiben!');
end;


procedure TForm1.AddVar1Click(Sender: TObject);
begin
  FVariableName := cOpcVarnameVar1;
  AddIntegerVariable(FVariableName, 17);
end;

procedure TForm1.AddVar2Click(Sender: TObject);
begin
  FVariableName := cOpcVarnameVar2;
  AddIntegerVariable(FVariableName, 42);
end;

procedure TForm1.AddVar3Click(Sender: TObject);
begin
  FVariableName := cOpcVarnameVar3;
  AddStringVariable(FVariableName, 'Var3: string ');
end;

procedure TForm1.btAddBool1Click(Sender: TObject);
begin
  FVariableName := cOpcVarnameBool1;
  AddBooleanVariable(FVariableName, false);
end;

procedure TForm1.btObjectVar2Click(Sender: TObject);
begin
  if not CheckServer then exit;
  FVariableName   := cOpcVarnameObj2;
  if FNodeObjectVar2.identifier.numeric <> UA_NODEID_NULL.identifier.numeric
    then LogDetail(FNodeObjectVar2, 'WARN!!! Wird doppelt erzeugt!');
  ForceSubFolder;
  FNodeObjectVar2 := AddStringVariableObject(FVariableName, 'Object.Var2', FNodeIDSubFolder);
end;

procedure TForm1.btObjectVar1Click(Sender: TObject);
begin
  if not CheckServer then exit;
  FVariableName   := cOpcVarnameObj1;
  if FNodeObjectVar1.identifier.numeric <> UA_NODEID_NULL.identifier.numeric
    then LogDetail(FNodeObjectVar1, 'WARN!!! Wird doppelt erzeugt!');
  ForceFolder;
  FNodeObjectVar1 := AddStringVariableObject(FVariableName, 'Object.Var1', FNodeIDFolder);
end;

procedure TForm1.btSetVar1Click(Sender: TObject);
var
  lValue : integer;
begin
  inc (FCounter);
  lValue := 42 + FCounter;
  SetIntegerValue(cOpcVarnameVar1, lValue);
  if lValue <> ReadIntegerValue (cOpcVarnameVar1)
    then ShowMessage ('Auslesen liefert einen anderen Wert als das Schreiben!');
end;

procedure TForm1.btSetVar2Click(Sender: TObject);
begin
  inc (FCounter);
  SetIntegerValue(cOpcVarnameVar2, 17+FCounter);
end;

procedure TForm1.btSetObjectVar2Click(Sender: TObject);
begin
  FVariableName := cOpcVarnameObj2;
  inc (FCounter);
  UA_Variant_init(FVariant);
  UA_Variant_setString(FVariant, 'Object.Var2: string ' + FCounter.ToString);
  InternalSetVarValue (FNodeObjectVar2, FVariant);
end;

procedure TForm1.btSetObjectVar1Click(Sender: TObject);
begin
  FVariableName := cOpcVarnameObj1;
  inc (FCounter);
  UA_Variant_init(FVariant);
  UA_Variant_setString(FVariant, 'Object.Var1: string ' + FCounter.ToString);
  InternalSetVarValue (FNodeObjectVar1, FVariant);
end;

procedure TForm1.LogError(const aNodeID: UA_NodeId; const aMessage: string);
begin
  Memo1.Lines.Add ('Error: ' +  UA_NodeIdToStr(aNodeID) + ' -> ' + aMessage);
end;

procedure TForm1.LogDetail(const aNodeID: UA_NodeId; const aMessage: string);
begin
  Memo1.Lines.Add ('Info : ' + UA_NodeIdToStr(aNodeID) + ' -> ' + aMessage);
end;

function TForm1.CheckServer : Boolean;
begin
  Result := assigned (FServer);
  if not Result
    then Memo1.Lines.Add ('Error: ' +  'Server not assigned');
end;

//! Liefert "True" wenn der Statuscode "UA_STATUSCODE_GOOD" ist
//! Protokolliert das Ergebnis
//!
function TForm1.CheckStatuscode(aStatus: UA_StatusCode; const aNodeID: UA_NodeId; const aMessage: string): Boolean;
begin
  Result := aStatus = UA_STATUSCODE_GOOD;
  if Result
    then LogDetail(aNodeId, aMessage)
    else LogError(aNodeID,  UA_Explain_StatusCode(aStatus) + #13+#10+'   Message : ' + aMessage);
end;

//! Ensures that folder 'folder' exists
//!
procedure TForm1.ForceFolder;
begin
  if (FNodeIDFolder.identifierType = UA_NODEID_NULL.identifierType)
     and (FNodeIDFolder.namespaceIndex = UA_NODEID_NULL.namespaceIndex)
     and (FNodeIDFolder.identifier.numeric = UA_NODEID_NULL.identifier.numeric)
    then FNodeIDFolder := AddObjectFolder;
end;

//! Ensures that folder 'subfolder' exists
//!
procedure TForm1.ForceSubFolder;
begin
  ForceFolder;
  if (FNodeIDSubFolder.identifierType = UA_NODEID_NULL.identifierType)
     and (FNodeIDSubFolder.namespaceIndex = UA_NODEID_NULL.namespaceIndex)
     and (FNodeIDSubFolder.identifier.numeric = UA_NODEID_NULL.identifier.numeric)
    then FNodeIDSubFolder := AddObjectSubFolder;
end;

//! Wert einer Integer Variablen anhand des Namens setzen
//!
procedure TForm1.SetIntegerValue (const aName : string; aValue : integer);
begin
  UA_Variant_init(FVariant);
  UA_Variant_setInteger(FVariant, aValue);
  InternalSetVarValue(aName, FVariant);
end;

//! Wert einer Integer Variablen anhand der Node-ID setzen
//!
procedure TForm1.SetIntegerValue(const aNodeID: UA_NodeId; aValue: integer);
begin
  UA_Variant_init(FVariant);
  UA_Variant_setInteger(FVariant, aValue);
  InternalSetVarValue (aNodeID, FVariant);
end;

//! Wert einer Boolean Variablen anhand des Namens setzen
//!
procedure TForm1.SetBooleanValue(const aName: string; aValue: boolean);
begin
  UA_Variant_init(FVariant);
  UA_Variant_setScalarCopy(@FVariant, @aValue, @UA_TYPES[UA_TYPES_BOOLEAN]);   // es gibt nicht: 'UA_Variant_setBoolean(...)'
  InternalSetVarValue (aName, FVariant);
end;

//! Wert einer Boolean Variablen anhand der Node-ID setzen
//!
procedure TForm1.SetBooleanValue(const aNodeId : UA_NodeId; aValue: boolean);
begin
  UA_Variant_init(FVariant);
  UA_Variant_setScalarCopy(@FVariant, @aValue, @UA_TYPES[UA_TYPES_BOOLEAN]);   // es gibt nicht: 'UA_Variant_setBoolean(...)'
  InternalSetVarValue (aNodeID, FVariant);
end;

//! Boolean-Wert eines Knotens anhand des Namens auslesen
//!
function TForm1.GetBooleanValue(const aName: string): boolean;
var
  intVariableNodeId: UA_NodeId;
begin
  if not CheckServer then exit;
  intVariableNodeId := UA_NODEID_STRING_ALLOC(1 {name space index}, aName);
  try
    FStatusCode := UA_Server_readValue(FServer.OpcUaServer, intVariableNodeId, Result);
    CheckStatuscode(FStatusCode, intVariableNodeId, 'Reading Boolean "' + aName + '" returned ' + BoolToStr(Result, True));
  finally
    UA_NodeId_clear(intVariableNodeId);
  end;
end;

//! Integer-Wert eines Knotens anhand des Namens auslesen
//!
function TForm1.GetIntegerValue(const aName: string): integer;
begin
  UA_Variant_init(FVariant);
  Result := PUA_Int32(FVariant.data)^;
  // Result := InternalGetVarValue(aName, FVariant);
end;

//! Wert einer Server-Variablen auslesen und als UA_Variant zurückliefern
//!
function TForm1.InternalGetVarValue(const aName: string): UA_Variant;
var
  intVariableNodeId: UA_NodeId;
begin
  if not CheckServer then exit;
  UA_Variant_init(Result);
  intVariableNodeId := UA_NODEID_STRING_ALLOC(1 {name space index}, aName);
  try
    FStatusCode := UA_Server_readValue(Fserver.OpcUaServer, intVariableNodeId, Result);
    CheckStatuscode(FStatusCode, intVariableNodeId, 'Server read variable "' + aName + '" Value: ' + GetUA_VariantAsString(Result));
  finally
    UA_NodeId_clear(intVariableNodeId);
  end;
end;

function TForm1.ReadIntegerValue(const aName: string): integer;
begin
  FVariant := InternalGetVarValue(aName);
  Result := UA_Variant_getInteger(FVariant);
end;

//! Wert einer String Variablen anhand des Namens setzen
//!
procedure TForm1.SetStringValue(const aName: string; aValue: string);
begin
  UA_Variant_init(FVariant);
  UA_Variant_setString(FVariant, aValue);
  InternalSetVarValue(aName, FVariant);
end;

//! Werte einer Variablen angand des Namens setzen
//! !!! "aVariant" muss passend zum Typ der Variablen erzeigt werden
//!
procedure TForm1.InternalSetVarValue (const aName: string; const aVariant : UA_Variant);
var
  intVariableNodeId: UA_NodeId;
begin
  if not CheckServer then exit;
  intVariableNodeId := UA_NODEID_STRING_ALLOC(1 {name space index}, aName);
  try
    FStatusCode := UA_Server_writeValue(Fserver.OpcUaServer, intVariableNodeId, aVariant);
    CheckStatuscode(FStatusCode, intVariableNodeId, 'Server write to variable "' + aName + '"');
  finally
    UA_NodeId_clear(intVariableNodeId);
  end;
end;

//! Werte einer Variablen anhand der Node-ID setzen
//! !!! "aVariant" muss passend zum Typ der Variablen erzeigt werden
//!
procedure TForm1.InternalSetVarValue(const aNodeID: UA_NodeId; const aVariant: UA_Variant);
begin
  if not CheckServer then exit;

  FStatusCode := UA_Server_writeValue(Fserver.OpcUaServer, aNodeID, aVariant);
  CheckStatuscode(FStatusCode, aNodeID, 'Server write to variable (by NodeID)' );
end;

//! Integer-Variable im globalen "Namespace" hinzufügen
//!
//! \retval Die NodeID des Objekts (kann alternativ zum Namen verwendet werden!)
//!
function TForm1.AddIntegerVariable(var aName: string; aInitialValue: integer): UA_NodeId;
var
  lOpcVarAttributes : UA_VariableAttributes;
  lOpcVariable      : UA_Int32;
begin
  if not CheckServer then exit;

  // Attribute setzen
  lOpcVarAttributes := UA_VariableAttributes_default;
  lOpcVariable      := aInitialValue;
  UA_Variant_setScalar(@lOpcVarAttributes.Value, @lOpcVariable, @UA_TYPES[UA_TYPES_INT32]);
  lOpcVarAttributes.dataType    := UA_TYPES[UA_TYPES_INT32].typeId;

  Result := InternalAddVariable(lOpcVarAttributes, aName, aInitialValue.ToString);
end;

//! Boolean-Variable im globalen "Namespace" hinzufügen
//!
//! \retval Die NodeID des Objekts (kann alternativ zum Namen verwendet werden!)
//!
function TForm1.AddBooleanVariable(var aName: string; aInitialValue: boolean): UA_NodeId;
var
  lOpcVarAttributes : UA_VariableAttributes;
  lOpcVariable      : UA_Boolean;
begin
  if not CheckServer then exit;

  // Attribute setzen
  lOpcVarAttributes := UA_VariableAttributes_default;
  lOpcVariable      := aInitialValue;
  UA_Variant_setScalar(@lOpcVarAttributes.Value, @lOpcVariable, @UA_TYPES[UA_TYPES_BOOLEAN]);
  lOpcVarAttributes.dataType    := UA_TYPES[UA_TYPES_BOOLEAN].typeId;

  Result := InternalAddVariable(lOpcVarAttributes, aName, aInitialValue.ToString);
end;

//! String-Variable im globalen "Namespace" hinzufügen
//!
//! \retval Die NodeID des Objekts (kann alternativ zum Namen verwendet werden!)
//!
function TForm1.AddStringVariable(var aName: string; aInitialValue: string): UA_NodeId;
var
  lOpcVarAttributes : UA_VariableAttributes;
  lOpcVariable      : UA_String;
begin
  if not CheckServer then exit;

  // Attribute setzen
  lOpcVarAttributes := UA_VariableAttributes_default;
  lOpcVariable      := _UA_STRING(aInitialValue);
  UA_Variant_setScalar(@lOpcVarAttributes.value, @lOpcVariable, @UA_TYPES[UA_TYPES_STRING]);
  lOpcVarAttributes.dataType    := UA_TYPES[UA_TYPES_STRING].typeId;

  Result := InternalAddVariable(lOpcVarAttributes, aName, aInitialValue);
end;

//! Einen Ordner mit Namen cOpcFolderName als Objekt innhalb des globalen "Namespace" hinzufügen
//!
//! \retval Die NodeID des Objekts (kann alternativ zum Namen verwendet werden!)
//!
function TForm1.AddObjectFolder: UA_NodeId;
var
  oAttr     : UA_ObjectAttributes;
  oName     : string;
  oNodeName : UA_QualifiedName;
begin
  oAttr := UA_ObjectAttributes_default;
  oName := cOpcFolderName;
  oAttr.displayName := _UA_LOCALIZEDTEXT(FLocale, oName);
  oNodeName         := _UA_QUALIFIEDNAME(1, oName);
  FStatusCode := UA_Server_addObjectNode(FServer.OpcUaServer, UA_NODEID_NULL,
                          UA_NODEID_NUMERIC(0, UA_NS0ID_OBJECTSFOLDER),
                          UA_NODEID_NUMERIC(0, UA_NS0ID_ORGANIZES),
                          oNodeName, UA_NODEID_NUMERIC(0, UA_NS0ID_BASEOBJECTTYPE),
                          oAttr, nil, @Result);
  CheckStatuscode(FStatusCode, Result, 'Neues Objet "'+cOpcFolderName+'" anlegen');
end;

//! Einen Ordner mit Namen cOpcSubFolderName als Objekt innhalb des globalen "Namespace" hinzufügen
//!
//! \retval Die NodeID des Objekts (kann alternativ zum Namen verwendet werden!)
//!
function TForm1.AddObjectSubFolder: UA_NodeId;
var
  oAttr     : UA_ObjectAttributes;
  oName     : string;
  oNodeName : UA_QualifiedName;
begin
  oAttr := UA_ObjectAttributes_default;
  oName := cOpcSubFolderName;
  oAttr.displayName := _UA_LOCALIZEDTEXT(FLocale, oName);
  oNodeName         := _UA_QUALIFIEDNAME(1, oName);
  FStatusCode := UA_Server_addObjectNode(FServer.OpcUaServer, UA_NODEID_NULL,
                          FNodeIDFolder, // UA_NODEID_NUMERIC(0, UA_NS0ID_OBJECTSFOLDER),
                          UA_NODEID_NUMERIC(0, UA_NS0ID_ORGANIZES),
                          oNodeName, UA_NODEID_NUMERIC(0, UA_NS0ID_BASEOBJECTTYPE),
                          oAttr, nil, @Result);
  CheckStatuscode(FStatusCode, Result, 'Neues Objet "'+cOpcSubFolderName+'" anlegen');
end;

//! Fuegt eine Variable zum Objekt "folder" hinzu
//!
//! \retval Die NodeID des Objekts (kann alternativ zum Namen verwendet werden!)
//!
function TForm1.AddStringVariableObject(var aName: string; aInitialValue: string; const aParent : UA_NodeId): UA_NodeId;
var
  lOpcVarAttributes : UA_VariableAttributes;
  lOpcVariable      : UA_String;
  lNodeName         : UA_QualifiedName;
begin
  if not CheckServer then exit;

  // Attribute setzen
  lOpcVarAttributes := UA_VariableAttributes_default;
  lOpcVariable      := _UA_STRING(aInitialValue);
  UA_Variant_setScalar(@lOpcVarAttributes.Value, @lOpcVariable, @UA_TYPES[UA_TYPES_STRING]);
  lOpcVarAttributes.description := _UA_LOCALIZEDTEXT(FLocale, aName);
  lOpcVarAttributes.displayName := _UA_LOCALIZEDTEXT(FLocale, aName);

  lOpcVarAttributes.dataType    := UA_TYPES[UA_TYPES_STRING].typeId;
  lOpcVarAttributes.accessLevel := UA_ACCESSLEVELMASK_READ or UA_ACCESSLEVELMASK_WRITE;

  // Add the variable node to the information model */
  lNodeName        := _UA_QUALIFIEDNAME(1, aName);

  FStatusCode      := UA_Server_addVariableNode(FServer.OpcUaServer,
                            UA_NODEID_NULL, aParent,
                            UA_NODEID_NUMERIC(0, UA_NS0ID_HASCOMPONENT),
                            lNodeName,
                            UA_NODEID_NUMERIC(0, UA_NS0ID_BASEDATAVARIABLETYPE), lOpcVarAttributes, nil, @Result);
  CheckStatuscode(FStatusCode, Result, 'Trying to add Variable "' + aName + '" with value: "' + aInitialValue + '"');
end;


//! Variable hinzufügen, "Typ" muss vorher in "aAttributes" festgelegt werden
//!
//! \retval Die NodeID des Objekts (kann alternativ zum Namen verwendet werden!)
//!
function TForm1.InternalAddVariable(var aAttributes: UA_VariableAttributes; aName: string; aInitialValue: string): UA_NodeId;
var
  lNodeID           : UA_NodeId;
  lNodeName         : UA_QualifiedName;
  lNodeParentID     : UA_NodeId;
  lParentRefNodeID  : UA_NodeId;
begin
  aAttributes.description := _UA_LOCALIZEDTEXT(FLocale, aName);
  aAttributes.displayName := _UA_LOCALIZEDTEXT(FLocale, aName);

  aAttributes.accessLevel := UA_ACCESSLEVELMASK_READ or UA_ACCESSLEVELMASK_WRITE;

  // Add the variable node to the information model */
  lNodeID          := UA_NODEID_STRING(1, aName);
  lNodeName        := _UA_QUALIFIEDNAME(1, aName);
  lNodeParentID    := UA_NODEID_NUMERIC(0, UA_NS0ID_OBJECTSFOLDER);
  lParentRefNodeID := UA_NODEID_NUMERIC(0, UA_NS0ID_ORGANIZES);
  FStatusCode      := UA_Server_addVariableNode(FServer.OpcUaServer,
                            lNodeID, lNodeParentID, lParentRefNodeID,
                            lNodeName,
                            UA_NODEID_NUMERIC(0, UA_NS0ID_BASEDATAVARIABLETYPE), aAttributes, nil, @Result);
  CheckStatuscode(FStatusCode, Result, 'Add Variable "' + aName + '" with value: "' + aInitialValue + '"');
end;

// Erzeugt aktuell ein VariableNode dessen VariableAtributes ein Variant hält, in dem sich ein 2-dimensionales UA_Int32-Array befindet.
procedure TForm1.btTest1Click(Sender: TObject);
var
  result_nodeid: UA_NodeID;
  created_node_id: UA_NodeID;
  lNodeParentID: UA_NodeID;
  lParentRefNodeID: UA_NodeID;
  lname: string;
  lparenname: string = 'sub2';
  atr: UA_VariableAttributes;
  array_value: array[0..9] of UA_Int32 = (2,3,5,7,11,13,17,19,23,29);
  array_dim: array[0..1] of UA_Int32 = (2,5);

  localString: string;
  displayName: string;
  description: string;
begin
  lname := 'MatrixTestNode';
  // created_node_id  := UA_NODEID_STRING(1, lname);
  //lNodeParentID    := UA_NODEID_STRING_ALLOC(1, lname);// UA_NODEID_NUMERIC(0, UA_NS0ID_OBJECTSFOLDER);
  lParentRefNodeID := UA_NODEID_NUMERIC(0, UA_NS0ID_ORGANIZES);

  atr := UA_VariableAttributes_default;
  localString := 'en_US';
  (*   Standardmäig wird der Name des Nodes kopiert, kann aber angepasst werden.
  displayName := 'DisplayName_MatrixTestAtr';
  atr.displayName := _UA_LOCALIZEDTEXT(localString, displayName);
  description := 'The attribute of a test note containing a varaint with an integer matrix';
  atr.description := _UA_LOCALIZEDTEXT(localString, description);
  atr.dataType    := UA_TYPES[UA_TYPES_INT32].typeId;
  atr.accessLevel := UA_ACCESSLEVELMASK_READ or UA_ACCESSLEVELMASK_WRITE;
  *)
  (* Keine Ahnung, warum man die Dimension im VariableAtribut setzen kann. Sie wegzulassen macht scheinbar keinen Unterschied.
  atr.valueRank := UA_VALUERANK_TWO_DIMENSIONS;
  atr.arrayDimensions := @array_dim;
  atr.arrayDimensionsSize := 2;
  *)

  UA_Variant_setArray(@atr.value, @array_value, 10, @UA_TYPES[UA_TYPES_INT32]);
  atr.value.arrayDimensions := @array_dim;
  atr.value.arrayDimensionsSize := 2;

  UA_Server_addVariableNode(FServer.OpcUaServer, UA_NODEID_NULL, SubFolder2,
                             lParentRefNodeID, _UA_QUALIFIEDNAME(1, lname),
                             UA_NODEID_NUMERIC(0, UA_NS0ID_BASEDATAVARIABLETYPE), atr, nil, @result_nodeid);
  CheckStatuscode(FStatusCode, result_nodeid, 'TestScript1: Versucht ein variableNode mit einem Array drin anzulegen.');
end;

// Eigene ObjektNode-Erstellung
procedure TForm1.btTest2Click(Sender: TObject);
var
  lname: string;
  result_nodeid: UA_NodeID;
  oAttr: UA_ObjectAttributes;
begin
  lname := 'sub2';
  oAttr := UA_ObjectAttributes_default;
  oAttr.displayName := _UA_LOCALIZEDTEXT(FLocale, lname);
  UA_Server_addObjectNode(FServer.OpcUaServer, UA_NODEID_NULL, FNodeIDFolder,
                                               UA_NODEID_NUMERIC(0, UA_NS0ID_ORGANIZES),
                                               _UA_QUALIFIEDNAME(1, lname),
                                               UA_NODEID_NUMERIC(0, UA_NS0ID_BASEOBJECTTYPE),
                                               oAttr, nil, @SubFolder2);
end;

procedure TForm1.btTest3Click(Sender: TObject);
begin

end;

end.
