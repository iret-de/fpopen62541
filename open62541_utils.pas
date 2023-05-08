(*
 * open62541 is licensed under the Mozilla Public License v2.0 (MPLv2).
 * This allows the open62541 library to be combined and distributed with any proprietary software.
 * Only changes to the open62541 library itself need to be licensed under the MPLv2 when copied and distributed.
 * The plugins, as well as the server and client examples are in the public domain (CC0 license).
 * They can be reused under any license and changes do not have to be published.
 *
 * Version 1.2-rc2 released on 23 Dec 2020
 *
 * Author: stefan hille <stefan.hille At iret de>
 *
 *)
unit open62541_utils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  open62541;

const
  cOpcUAPathDelimiter = '/';

  //! Search for a specific Node
  //! \param aSearchpath - Maybe partial or complete path (eg.: opc://..../
  //! \param aNodeID     - Starting node
  //!
  //! \retval UA_NODEID_NULL - nothing found
  //! \retval copy of the matching NodeID
  //!
  //! \warn I guess it is necessary to call UA_nodeid_delete if the Result is "UA_NODEIDTYPE_STRING"
  //!
  function UA_GetNodeIDFromPath (const aClient : PUA_Client; const aStartNodeID : UA_NodeID; aSearchPath : string): UA_NODEID;

  //! Load special pascal logging library
  procedure LoadOpen62541PascalLog;
  //! Unload special pascal logging library
  procedure UnloadOpen62541PascalLog;
  //! Logging category to string
  function UA_LogCategoryToString (aCategory : UA_LogCategory) : string;

type
  //! Prototype for Logging function
  TUA_Pascallogger_LogFunction = procedure (logContext: Pointer; level: UA_LogLevel; category: UA_LogCategory; msg: PAnsiChar);cdecl;

var
  //! Pointer to Pascal-logging function if library "ua_pascallog" is laoded
  UA_Pascal_logger:  function(context: pointer; func:TUA_Pascallogger_LogFunction):UA_logger;cdecl=nil;

implementation

uses
  URIParser;

var
  UaPascalLogLibraryHandle: TLibHandle;

const
{$IFDEF WINDOWS}
  cLibPascallog = 'ua_pascallog.dll';
{$ELSE}
  cLibPascallog = 'libua_pascallog.so';
{$ENDIF}

procedure LoadOpen62541PascalLog;
begin
  if pointer(UA_Pascal_logger) <> nil then exit;

  pointer(UA_Pascal_logger) := nil;
  UaPascalLogLibraryHandle := LoadLibrary(cLibPascallog);
  if UaPascalLogLibraryHandle=NilHandle then
  begin
    exit;
  end;
  pointer(UA_Pascal_logger) := GetProcedureAddress(UaPascalLogLibraryHandle,'UA_Pascal_logger');
end;

procedure UnloadOpen62541PascalLog;
begin
  pointer(UA_Pascal_logger) := nil;
  if UaPascalLogLibraryHandle<>NilHandle then
    UnloadLibrary(UaPascalLogLibraryHandle);
end;

function UA_LogCategoryToString(aCategory : UA_LogCategory) : string;
begin
  case aCategory of
    UA_LOGCATEGORY_NETWORK         : Result := 'UA_LOGCATEGORY_NETWORK       ';
    UA_LOGCATEGORY_SECURECHANNEL   : Result := 'UA_LOGCATEGORY_SECURECHANNEL ';
    UA_LOGCATEGORY_SESSION         : Result := 'UA_LOGCATEGORY_SESSION       ';
    UA_LOGCATEGORY_SERVER          : Result := 'UA_LOGCATEGORY_SERVER        ';
    UA_LOGCATEGORY_CLIENT          : Result := 'UA_LOGCATEGORY_CLIENT        ';
    UA_LOGCATEGORY_USERLAND        : Result := 'UA_LOGCATEGORY_USERLAND      ';
    UA_LOGCATEGORY_SECURITYPOLICY  : Result := 'UA_LOGCATEGORY_SECURITYPOLICY';
  else
    Result := 'UA_LOGCATEGORY_UNKNOWN';
  end;
end;

function UA_GetNodeIDFromPath_recursive(const aClient : PUA_Client; const aNodeID: UA_NodeID; aSearchPath: string): UA_NODEID; forward;

//! Get the first "node"-Name from an OPC-UA path
//!
//! \param aPath - Incoming path, will be shortend by the first "node"-name
//!
//! \return First "Node"-Name from aPath or ''
//!
//! examples:
//!    * aPath = '/Folder1/SubFolder/Document' --> Return "Folder1", aPath = 'SubFolder/Document/'
function ExtractFirstOpcUAPathElement (var aPath : string) : string;
var
  lPos : integer;
begin
  Result := '';

  while pos (cOpcUAPathDelimiter, aPath) = 1
    do system.Delete(aPath, 1, 1);

  if aPath <> ''
  then begin
    lPos := pos (cOpcUAPathDelimiter, aPath);
    if lPos = 0
    then begin
      Result := aPath;
      aPath  := '';
    end
    else begin
      Result := system.Copy(aPath, 1, lPos-1);
      system.Delete(aPath, 1, lPos);
    end;
  end;
end;

function UA_GetNodeIDFromPath(const aClient: PUA_Client; const aStartNodeID: UA_NodeID; aSearchPath: string): UA_NODEID;
var
  lURI : TURI;
begin
  Result := UA_NODEID_NULL;
  if not assigned (aClient) then exit;

  lURI := ParseURI(aSearchPath);

  Result := UA_GetNodeIDFromPath_recursive(aClient, aStartNodeID, lUri.Path + lUri.Document);
end;

//! Recursive "worker" for UA_GetNodeIDFromPath
//!
function UA_GetNodeIDFromPath_recursive(const aClient : PUA_Client; const aNodeID: UA_NodeID; aSearchPath: string): UA_NODEID;

var
  bReq    : UA_BrowseRequest;
  bResp   : UA_BrowseResponse;
  ref     : PUA_ReferenceDescription;
  i, j    : integer;

  lPattern : string;
  lMatch   : Boolean;
begin
  Result := UA_NODEID_NULL;
  if not assigned (aClient) then exit;

  lPattern := ExtractFirstOpcUAPathElement(aSearchPath);
  if (lPattern = '') then exit;                              // Gefunden!

  UA_BrowseRequest_init(bReq);
  try
    bReq.requestedMaxReferencesPerNode := 0;
    bReq.nodesToBrowse                 := UA_BrowseDescription_new();
    bReq.nodesToBrowseSize             := 1;
    bReq.nodesToBrowse[0].nodeId       := aNodeID;
    bReq.nodesToBrowse[0].resultMask   := UA_Int32(UA_BROWSERESULTMASK_ALL);                      // return everything
    bResp := UA_Client_Service_browse(aClient, bReq);
    i := 0;
    lMatch := False;
    while (lMatch = False) and (i < bResp.resultsSize)
    do begin
      j := 0;
      while (lMatch = False) and (j < bResp.results[i].referencesSize)
      do begin
        ref := @(bResp.results[i].references[j]);
        if SameText(lPattern, UA_StringToStr(ref^.browseName.name))
        then begin
          lMatch := True;
          if aSearchPath = ''
          then begin
            Result := ref^.nodeId.nodeId;
            if Result.identifierType = UA_NODEIDTYPE_STRING
              then Result := UA_NODEID_STRING_ALLOC (ref^.nodeId.nodeId.namespaceIndex, UA_StringToStr(ref^.nodeId.nodeId.identifier._string));
          end
          else
            Result := UA_GetNodeIDFromPath_recursive (aClient, ref^.nodeId.nodeId, aSearchPath);
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

initialization
  pointer(UA_Pascal_logger) := nil;
end.

