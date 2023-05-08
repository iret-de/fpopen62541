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
unit open62541_helpers;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  open62541;

  function UA_Explain_StatusCode (aStatus: UA_StatusCode) : string;

implementation

//! Return a textual description of the "aStatus"
//!
function UA_Explain_StatusCode (aStatus: UA_StatusCode) : string;
begin
  Result := IntToHex(aStatus) + ' => ';
  case aStatus of
    UA_STATUSCODE_GOOD                      : Result := 'Result=00000000, OK';
    UA_STATUSCODE_INFOTYPE_DATAVALUE        : Result := 'UA_STATUSCODE_INFOTYPE_DATAVALUE';
    UA_STATUSCODE_INFOBITS_OVERFLOW         : Result := 'UA_STATUSCODE_INFOBITS_OVERFLOW';
    UA_STATUSCODE_BADUNEXPECTEDERROR        : Result := 'An unexpected error occurred. ';
    UA_STATUSCODE_BADINTERNALERROR          : Result := 'An internal error occurred as a result of a programming or configuration error. ';
    UA_STATUSCODE_BADOUTOFMEMORY            : Result := 'Not enough memory to complete the operation. ';
    UA_STATUSCODE_BADRESOURCEUNAVAILABLE    : Result := 'An operating system resource is not available. ';
    UA_STATUSCODE_BADCOMMUNICATIONERROR     : Result := 'A low level communication error occurred. ';
    UA_STATUSCODE_BADENCODINGERROR          : Result := 'Encoding halted because of invalid data in the objects being serialized. ';
    UA_STATUSCODE_BADDECODINGERROR          : Result := 'Decoding halted because of invalid data in the stream. ';
    UA_STATUSCODE_BADENCODINGLIMITSEXCEEDED : Result := 'The message encoding/decoding limits imposed by the stack have been exceeded. ';
    UA_STATUSCODE_BADREQUESTTOOLARGE        : Result := 'The request message size exceeds limits set by the server. ';
    UA_STATUSCODE_BADRESPONSETOOLARGE       : Result := 'The response message size exceeds limits set by the client. ';
    UA_STATUSCODE_BADUNKNOWNRESPONSE        : Result := 'An unrecognized response was received from the server. ';
    UA_STATUSCODE_BADTIMEOUT                : Result := 'The operation timed out. ';
    UA_STATUSCODE_BADSERVICEUNSUPPORTED     : Result := 'The server does not support the requested service. ';
    UA_STATUSCODE_BADSHUTDOWN               : Result := 'The operation was cancelled because the application is shutting down. ';
    UA_STATUSCODE_BADSERVERNOTCONNECTED     : Result := 'The operation could not complete because the client is not connected to the server. ';
    UA_STATUSCODE_BADSERVERHALTED           : Result := 'The server has stopped and cannot process any requests. ';
    UA_STATUSCODE_BADNOTHINGTODO            : Result := 'There was nothing to do because the client passed a list of operations with no elements. ';
    UA_STATUSCODE_BADTOOMANYOPERATIONS      : Result := 'The request could not be processed because it specified too many operations. ';
    UA_STATUSCODE_BADTOOMANYMONITOREDITEMS  : Result := 'The request could not be processed because there are too many monitored items in the subscription. ';
    UA_STATUSCODE_BADDATATYPEIDUNKNOWN      : Result := 'The extension object cannot be (de)serialized because the data type id is not recognized. ';
    UA_STATUSCODE_BADCERTIFICATEINVALID     : Result := 'The certificate provided as a parameter is not valid. ';
    UA_STATUSCODE_BADSECURITYCHECKSFAILED   : Result := 'An error occurred verifying security. ';
    UA_STATUSCODE_BADCERTIFICATEPOLICYCHECKFAILED : Result := 'The certificate does not meet the requirements of the security policy. ';
    UA_STATUSCODE_BADCERTIFICATETIMEINVALID : Result := 'The certificate has expired or is not yet valid. ';
    UA_STATUSCODE_BADCERTIFICATEISSUERTIMEINVALID : Result := 'An issuer certificate has expired or is not yet valid. ';
    UA_STATUSCODE_BADCERTIFICATEHOSTNAMEINVALID : Result := 'The HostName used to connect to a server does not match a HostName in the certificate. ';
    UA_STATUSCODE_BADCERTIFICATEURIINVALID  : Result := 'The URI specified in the ApplicationDescription does not match the URI in the certificate. ';
    UA_STATUSCODE_BADCERTIFICATEUSENOTALLOWED : Result := 'The certificate may not be used for the requested operation. ';
    UA_STATUSCODE_BADCERTIFICATEISSUERUSENOTALLOWED : Result := 'The issuer certificate may not be used for the requested operation. ';
    UA_STATUSCODE_BADCERTIFICATEUNTRUSTED   : Result := 'The certificate is not trusted. ';
    UA_STATUSCODE_BADCERTIFICATEREVOCATIONUNKNOWN : Result := 'It was not possible to determine if the certificate has been revoked. ';
    UA_STATUSCODE_BADCERTIFICATEISSUERREVOCATIONUNKNOWN : Result := 'It was not possible to determine if the issuer certificate has been revoked. ';
    UA_STATUSCODE_BADCERTIFICATEREVOKED     : Result := 'The certificate has been revoked. ';
    UA_STATUSCODE_BADCERTIFICATEISSUERREVOKED : Result := 'The issuer certificate has been revoked. ';
    UA_STATUSCODE_BADCERTIFICATECHAININCOMPLETE : Result := 'The certificate chain is incomplete. ';
    UA_STATUSCODE_BADUSERACCESSDENIED       : Result := 'User does not have permission to perform the requested operation. ';
    UA_STATUSCODE_BADIDENTITYTOKENINVALID   : Result := 'The user identity token is not valid. ';
    UA_STATUSCODE_BADIDENTITYTOKENREJECTED  : Result := 'The user identity token is valid but the server has rejected it. ';
    UA_STATUSCODE_BADSECURECHANNELIDINVALID : Result := 'The specified secure channel is no longer valid. ';
    UA_STATUSCODE_BADINVALIDTIMESTAMP       : Result := 'The timestamp is outside the range allowed by the server. ';
    UA_STATUSCODE_BADNONCEINVALID           : Result := 'The nonce does appear to be not a random value or it is not the correct length. ';
    UA_STATUSCODE_BADSESSIONIDINVALID       : Result := 'The session id is not valid. ';
    UA_STATUSCODE_BADSESSIONCLOSED          : Result := 'The session was closed by the client. ';
    UA_STATUSCODE_BADSESSIONNOTACTIVATED    : Result := 'The session cannot be used because ActivateSession has not been called. ';
    UA_STATUSCODE_BADSUBSCRIPTIONIDINVALID  : Result := 'The subscription id is not valid. ';
    UA_STATUSCODE_BADREQUESTHEADERINVALID   : Result := 'The header for the request is missing or invalid. ';
    UA_STATUSCODE_BADTIMESTAMPSTORETURNINVALID : Result := 'The timestamps to return parameter is invalid. ';
    UA_STATUSCODE_BADREQUESTCANCELLEDBYCLIENT : Result := 'The request was cancelled by the client. ';
    UA_STATUSCODE_BADTOOMANYARGUMENTS       : Result := 'Too many arguments were provided. ';
    UA_STATUSCODE_BADLICENSEEXPIRED         : Result := 'The server requires a license to operate in general or to perform a service or operation ';
    UA_STATUSCODE_BADLICENSELIMITSEXCEEDED  : Result := 'The server has limits on number of allowed operations / objects ';
    UA_STATUSCODE_BADLICENSENOTAVAILABLE    : Result := 'The server does not have a license which is required to operate in general or to perform a service or operation. ';
    UA_STATUSCODE_GOODSUBSCRIPTIONTRANSFERRED : Result := 'The subscription was transferred to another session. ';
    UA_STATUSCODE_GOODCOMPLETESASYNCHRONOUSLY : Result := 'The processing will complete asynchronously. ';
    UA_STATUSCODE_GOODOVERLOAD              : Result := 'Sampling has slowed down due to resource limitations. ';
    UA_STATUSCODE_GOODCLAMPED               : Result := 'The value written was accepted but was clamped. ';
    UA_STATUSCODE_BADNOCOMMUNICATION        : Result := 'Communication with the data source is defined ';
    UA_STATUSCODE_BADWAITINGFORINITIALDATA  : Result := 'Waiting for the server to obtain values from the underlying data source. ';
    UA_STATUSCODE_BADNODEIDINVALID          : Result := 'The syntax of the node id is not valid. ';
    UA_STATUSCODE_BADNODEIDUNKNOWN          : Result := 'The node id refers to a node that does not exist in the server address space. ';
    UA_STATUSCODE_BADATTRIBUTEIDINVALID     : Result := 'The attribute is not supported for the specified Node. ';
    UA_STATUSCODE_BADINDEXRANGEINVALID      : Result := 'The syntax of the index range parameter is invalid. ';
    UA_STATUSCODE_BADINDEXRANGENODATA       : Result := 'No data exists within the range of indexes specified. ';
    UA_STATUSCODE_BADDATAENCODINGINVALID    : Result := 'The data encoding is invalid. ';
    UA_STATUSCODE_BADDATAENCODINGUNSUPPORTED : Result := 'The server does not support the requested data encoding for the node. ';
    UA_STATUSCODE_BADNOTREADABLE            : Result := 'The access level does not allow reading or subscribing to the Node. ';
    UA_STATUSCODE_BADNOTWRITABLE            : Result := 'The access level does not allow writing to the Node. ';
    UA_STATUSCODE_BADOUTOFRANGE             : Result := 'The value was out of range. ';
    UA_STATUSCODE_BADNOTSUPPORTED           : Result := 'The requested operation is not supported. ';
    UA_STATUSCODE_BADNOTFOUND               : Result := 'A requested item was not found or a search operation ended without success. ';
    UA_STATUSCODE_BADOBJECTDELETED          : Result := 'The object cannot be used because it has been deleted. ';
    UA_STATUSCODE_BADNOTIMPLEMENTED         : Result := 'Requested operation is not implemented. ';
    UA_STATUSCODE_BADMONITORINGMODEINVALID  : Result := 'The monitoring mode is invalid. ';
    UA_STATUSCODE_BADMONITOREDITEMIDINVALID : Result := 'The monitoring item id does not refer to a valid monitored item. ';
    UA_STATUSCODE_BADMONITOREDITEMFILTERINVALID : Result := 'The monitored item filter parameter is not valid. ';
    UA_STATUSCODE_BADMONITOREDITEMFILTERUNSUPPORTED : Result := 'The server does not support the requested monitored item filter. ';
    UA_STATUSCODE_BADFILTERNOTALLOWED       : Result := 'A monitoring filter cannot be used in combination with the attribute specified. ';
    UA_STATUSCODE_BADSTRUCTUREMISSING       : Result := 'A mandatory structured parameter was missing or null. ';
    UA_STATUSCODE_BADEVENTFILTERINVALID     : Result := 'The event filter is not valid. ';
    UA_STATUSCODE_BADCONTENTFILTERINVALID   : Result := 'The content filter is not valid. ';
    UA_STATUSCODE_BADFILTEROPERATORINVALID  : Result := 'An unrecognized operator was provided in a filter. ';
    UA_STATUSCODE_BADFILTEROPERATORUNSUPPORTED : Result := 'A valid operator was provided ';
    UA_STATUSCODE_BADFILTEROPERANDCOUNTMISMATCH : Result := 'The number of operands provided for the filter operator was less then expected for the operand provided. ';
    UA_STATUSCODE_BADFILTEROPERANDINVALID   : Result := 'The operand used in a content filter is not valid. ';
    UA_STATUSCODE_BADFILTERELEMENTINVALID   : Result := 'The referenced element is not a valid element in the content filter. ';
    UA_STATUSCODE_BADFILTERLITERALINVALID   : Result := 'The referenced literal is not a valid value. ';
    UA_STATUSCODE_BADCONTINUATIONPOINTINVALID : Result := 'The continuation point provide is longer valid. ';
    UA_STATUSCODE_BADNOCONTINUATIONPOINTS   : Result := 'The operation could not be processed because all continuation points have been allocated. ';
    UA_STATUSCODE_BADREFERENCETYPEIDINVALID : Result := 'The reference type id does not refer to a valid reference type node. ';
    UA_STATUSCODE_BADBROWSEDIRECTIONINVALID : Result := 'The browse direction is not valid. ';
    UA_STATUSCODE_BADNODENOTINVIEW          : Result := 'The node is not part of the view. ';
    UA_STATUSCODE_BADNUMERICOVERFLOW        : Result := 'The number was not accepted because of a numeric overflow. ';
    UA_STATUSCODE_BADSERVERURIINVALID       : Result := 'The ServerUri is not a valid URI. ';
    UA_STATUSCODE_BADSERVERNAMEMISSING      : Result := 'No ServerName was specified. ';
    UA_STATUSCODE_BADDISCOVERYURLMISSING    : Result := 'No DiscoveryUrl was specified. ';
    UA_STATUSCODE_BADSEMPAHOREFILEMISSING   : Result := 'The semaphore file specified by the client is not valid. ';
    UA_STATUSCODE_BADREQUESTTYPEINVALID     : Result := 'The security token request type is not valid. ';
    UA_STATUSCODE_BADSECURITYMODEREJECTED   : Result := 'The security mode does not meet the requirements set by the server. ';
    UA_STATUSCODE_BADSECURITYPOLICYREJECTED : Result := 'The security policy does not meet the requirements set by the server. ';
    UA_STATUSCODE_BADTOOMANYSESSIONS        : Result := 'The server has reached its maximum number of sessions. ';
    UA_STATUSCODE_BADUSERSIGNATUREINVALID   : Result := 'The user token signature is missing or invalid. ';
    UA_STATUSCODE_BADAPPLICATIONSIGNATUREINVALID : Result := 'The signature generated with the client certificate is missing or invalid. ';
    UA_STATUSCODE_BADNOVALIDCERTIFICATES    : Result := 'The client did not provide at least one software certificate that is valid and meets the profile requirements for the server. ';
    UA_STATUSCODE_BADIDENTITYCHANGENOTSUPPORTED : Result := 'The server does not support changing the user identity assigned to the session. ';
    UA_STATUSCODE_BADREQUESTCANCELLEDBYREQUEST : Result := 'The request was cancelled by the client with the Cancel service. ';
    UA_STATUSCODE_BADPARENTNODEIDINVALID    : Result := 'The parent node id does not to refer to a valid node. ';
    UA_STATUSCODE_BADREFERENCENOTALLOWED    : Result := 'The reference could not be created because it violates constraints imposed by the data model. ';
    UA_STATUSCODE_BADNODEIDREJECTED         : Result := 'The requested node id was reject because it was either invalid or server does not allow node ids to be specified by the client. ';
    UA_STATUSCODE_BADNODEIDEXISTS           : Result := 'The requested node id is already used by another node. ';
    UA_STATUSCODE_BADNODECLASSINVALID       : Result := 'The node class is not valid. ';
    UA_STATUSCODE_BADBROWSENAMEINVALID      : Result := 'The browse name is invalid. ';
    UA_STATUSCODE_BADBROWSENAMEDUPLICATED   : Result := 'The browse name is not unique among nodes that share the same relationship with the parent. ';
    UA_STATUSCODE_BADNODEATTRIBUTESINVALID : Result := 'The node attributes are not valid for the node class. ';
    UA_STATUSCODE_BADTYPEDEFINITIONINVALID : Result := 'The type definition node id does not reference an appropriate type node. ';
    UA_STATUSCODE_BADSOURCENODEIDINVALID   : Result := 'The source node id does not reference a valid node. ';
    UA_STATUSCODE_BADTARGETNODEIDINVALID   : Result := 'The target node id does not reference a valid node. ';
    UA_STATUSCODE_BADDUPLICATEREFERENCENOTALLOWED : Result := 'The reference type between the nodes is already defined. ';
    UA_STATUSCODE_BADINVALIDSELFREFERENCE  : Result := 'The server does not allow this type of self reference on this node. ';
    UA_STATUSCODE_BADREFERENCELOCALONLY    : Result := 'The reference type is not valid for a reference to a remote server. ';
    UA_STATUSCODE_BADNODELETERIGHTS        : Result := 'The server will not allow the node to be deleted. ';
    UA_STATUSCODE_UNCERTAINREFERENCENOTDELETED : Result := 'The server was not able to delete all target references. ';
    UA_STATUSCODE_BADSERVERINDEXINVALID    : Result := 'The server index is not valid. ';
    UA_STATUSCODE_BADVIEWIDUNKNOWN         : Result := 'The view id does not refer to a valid view node. ';
    UA_STATUSCODE_BADVIEWTIMESTAMPINVALID  : Result := 'The view timestamp is not available or not supported. ';
    UA_STATUSCODE_BADVIEWPARAMETERMISMATCH : Result := 'The view parameters are not consistent with each other. ';
    UA_STATUSCODE_BADVIEWVERSIONINVALID    : Result := 'The view version is not available or not supported. ';
    UA_STATUSCODE_UNCERTAINNOTALLNODESAVAILABLE : Result := 'The list of references may not be complete because the underlying system is not available. ';
    UA_STATUSCODE_GOODRESULTSMAYBEINCOMPLETE : Result := 'The server should have followed a reference to a node in a remote server but did not. The result set may be incomplete. ';
    UA_STATUSCODE_BADNOTTYPEDEFINITION     : Result := 'The provided Nodeid was not a type definition nodeid. ';
    UA_STATUSCODE_UNCERTAINREFERENCEOUTOFSERVER : Result := 'One of the references to follow in the relative path references to a node in the address space in another server. ';
    UA_STATUSCODE_BADTOOMANYMATCHES        : Result := 'The requested operation has too many matches to return. ';
    UA_STATUSCODE_BADQUERYTOOCOMPLEX       : Result := 'The requested operation requires too many resources in the server. ';
    UA_STATUSCODE_BADNOMATCH               : Result := 'The requested operation has no match to return. ';
    UA_STATUSCODE_BADMAXAGEINVALID : Result := 'The max age parameter is invalid. ';
    UA_STATUSCODE_BADSECURITYMODEINSUFFICIENT : Result := 'The operation is not permitted over the current secure channel. ';
    UA_STATUSCODE_BADHISTORYOPERATIONINVALID : Result := 'The history details parameter is not valid. ';
    UA_STATUSCODE_BADHISTORYOPERATIONUNSUPPORTED : Result := 'The server does not support the requested operation. ';
    UA_STATUSCODE_BADINVALIDTIMESTAMPARGUMENT : Result := 'The defined timestamp to return was invalid. ';
    UA_STATUSCODE_BADWRITENOTSUPPORTED     : Result := 'The server does not support writing the combination of value ';
    UA_STATUSCODE_BADTYPEMISMATCH          : Result := 'The value supplied for the attribute is not of the same type as the attribute''s value. ';
    UA_STATUSCODE_BADMETHODINVALID         : Result := 'The method id does not refer to a method for the specified object. ';
    UA_STATUSCODE_BADARGUMENTSMISSING      : Result := 'The client did not specify all of the input arguments for the method. ';
    UA_STATUSCODE_BADNOTEXECUTABLE         : Result := 'The executable attribute does not allow the execution of the method. ';
    UA_STATUSCODE_BADTOOMANYSUBSCRIPTIONS  : Result := 'The server has reached its maximum number of subscriptions. ';
    UA_STATUSCODE_BADTOOMANYPUBLISHREQUESTS : Result := 'The server has reached the maximum number of queued publish requests. ';
    UA_STATUSCODE_BADNOSUBSCRIPTION         : Result := 'There is no subscription available for this session. ';
    UA_STATUSCODE_BADSEQUENCENUMBERUNKNOWN  : Result := 'The sequence number is unknown to the server. ';
    UA_STATUSCODE_BADMESSAGENOTAVAILABLE    : Result := 'The requested notification message is no longer available. ';
    UA_STATUSCODE_BADINSUFFICIENTCLIENTPROFILE : Result := 'The client of the current session does not support one or more Profiles that are necessary for the subscription. ';
    UA_STATUSCODE_BADSTATENOTACTIVE         : Result := 'The sub-state machine is not currently active. ';
    UA_STATUSCODE_BADALREADYEXISTS          : Result := 'An equivalent rule already exists. ';
    UA_STATUSCODE_BADTCPSERVERTOOBUSY       : Result := 'The server cannot process the request because it is too busy. ';
    UA_STATUSCODE_BADTCPMESSAGETYPEINVALID  : Result := 'The type of the message specified in the header invalid. ';
    UA_STATUSCODE_BADTCPSECURECHANNELUNKNOWN : Result := 'The SecureChannelId and/or TokenId are not currently in use. ';
    UA_STATUSCODE_BADTCPMESSAGETOOLARGE     : Result := 'The size of the message specified in the header is too large. ';
    UA_STATUSCODE_BADTCPNOTENOUGHRESOURCES  : Result := 'There are not enough resources to process the request. ';
    UA_STATUSCODE_BADTCPINTERNALERROR       : Result := 'An internal error occurred. ';
    UA_STATUSCODE_BADTCPENDPOINTURLINVALID  : Result := 'The server does not recognize the QueryString specified. ';
    UA_STATUSCODE_BADREQUESTINTERRUPTED     : Result := 'The request could not be sent because of a network interruption. ';
    UA_STATUSCODE_BADREQUESTTIMEOUT         : Result := 'Timeout occurred while processing the request. ';
    UA_STATUSCODE_BADSECURECHANNELCLOSED    : Result := 'The secure channel has been closed. ';
    UA_STATUSCODE_BADSECURECHANNELTOKENUNKNOWN : Result := 'The token has expired or is not recognized. ';
    UA_STATUSCODE_BADSEQUENCENUMBERINVALID  : Result := 'The sequence number is not valid. ';
    UA_STATUSCODE_BADPROTOCOLVERSIONUNSUPPORTED : Result := 'The applications do not have compatible protocol versions. ';
    UA_STATUSCODE_BADCONFIGURATIONERROR     : Result := 'There is a problem with the configuration that affects the usefulness of the value. ';
    UA_STATUSCODE_BADNOTCONNECTED           : Result := 'The variable should receive its value from another variable ';
    UA_STATUSCODE_BADDEVICEFAILURE          : Result := 'There has been a failure in the device/data source that generates the value that has affected the value. ';
    UA_STATUSCODE_BADSENSORFAILURE          : Result := 'There has been a failure in the sensor from which the value is derived by the device/data source. ';
    UA_STATUSCODE_BADOUTOFSERVICE           : Result := 'The source of the data is not operational. ';
    UA_STATUSCODE_BADDEADBANDFILTERINVALID  : Result := 'The deadband filter is not valid. ';
    UA_STATUSCODE_UNCERTAINNOCOMMUNICATIONLASTUSABLEVALUE : Result := 'Communication to the data source has failed. The variable value is the last value that had a good quality. ';
    UA_STATUSCODE_UNCERTAINLASTUSABLEVALUE : Result := 'Whatever was updating this value has stopped doing so. ';
    UA_STATUSCODE_UNCERTAINSUBSTITUTEVALUE : Result := 'The value is an operational value that was manually overwritten. ';
    UA_STATUSCODE_UNCERTAININITIALVALUE    : Result := 'The value is an initial value for a variable that normally receives its value from another variable. ';
    UA_STATUSCODE_UNCERTAINSENSORNOTACCURATE : Result := 'The value is at one of the sensor limits. ';
    UA_STATUSCODE_UNCERTAINENGINEERINGUNITSEXCEEDED : Result := 'The value is outside of the range of values defined for this parameter. ';
    UA_STATUSCODE_UNCERTAINSUBNORMAL       : Result := 'The value is derived from multiple sources and has less than the required number of Good sources. ';
    UA_STATUSCODE_GOODLOCALOVERRIDE        : Result := 'The value has been overridden. ';
    UA_STATUSCODE_BADREFRESHINPROGRESS     : Result := 'This Condition refresh failed ';
    UA_STATUSCODE_BADCONDITIONALREADYDISABLED : Result := 'This condition has already been disabled. ';
    UA_STATUSCODE_BADCONDITIONALREADYENABLED : Result := 'This condition has already been enabled. ';
    UA_STATUSCODE_BADCONDITIONDISABLED     : Result := 'Property not available ';
    UA_STATUSCODE_BADEVENTIDUNKNOWN        : Result := 'The specified event id is not recognized. ';
    UA_STATUSCODE_BADEVENTNOTACKNOWLEDGEABLE : Result := 'The event cannot be acknowledged. ';
    UA_STATUSCODE_BADDIALOGNOTACTIVE         : Result := 'The dialog condition is not active. ';
    UA_STATUSCODE_BADDIALOGRESPONSEINVALID   : Result := 'The response is not valid for the dialog. ';
    UA_STATUSCODE_BADCONDITIONBRANCHALREADYACKED : Result := 'The condition branch has already been acknowledged. ';
    UA_STATUSCODE_BADCONDITIONBRANCHALREADYCONFIRMED : Result := 'The condition branch has already been confirmed. ';
    UA_STATUSCODE_BADCONDITIONALREADYSHELVED : Result := 'The condition has already been shelved. ';
    UA_STATUSCODE_BADCONDITIONNOTSHELVED : Result := 'The condition is not currently shelved. ';
    UA_STATUSCODE_BADSHELVINGTIMEOUTOFRANGE : Result := 'The shelving time not within an acceptable range. ';
    UA_STATUSCODE_BADNODATA : Result := 'No data exists for the requested time range or event filter. ';
    UA_STATUSCODE_BADBOUNDNOTFOUND : Result := 'No data found to provide upper or lower bound value. ';
    UA_STATUSCODE_BADBOUNDNOTSUPPORTED : Result := 'The server cannot retrieve a bound for the variable. ';
    UA_STATUSCODE_BADDATALOST : Result := 'Data is missing due to collection started/stopped/lost. ';
    UA_STATUSCODE_BADDATAUNAVAILABLE : Result := 'Expected data is unavailable for the requested time range due to an un-mounted volume ';
    UA_STATUSCODE_BADENTRYEXISTS : Result := 'The data or event was not successfully inserted because a matching entry exists. ';
    UA_STATUSCODE_BADNOENTRYEXISTS : Result := 'The data or event was not successfully updated because no matching entry exists. ';
    UA_STATUSCODE_BADTIMESTAMPNOTSUPPORTED : Result := 'The client requested history using a timestamp format the server does not support (i.e requested ServerTimestamp when server only supports SourceTimestamp). ';
    UA_STATUSCODE_GOODENTRYINSERTED : Result := 'The data or event was successfully inserted into the historical database. ';
    UA_STATUSCODE_GOODENTRYREPLACED : Result := 'The data or event field was successfully replaced in the historical database. ';
    UA_STATUSCODE_UNCERTAINDATASUBNORMAL : Result := 'The value is derived from multiple values and has less than the required number of Good values. ';
    UA_STATUSCODE_GOODNODATA : Result := 'No data exists for the requested time range or event filter. ';
    UA_STATUSCODE_GOODMOREDATA : Result := 'The data or event field was successfully replaced in the historical database. ';
    UA_STATUSCODE_BADAGGREGATELISTMISMATCH : Result := 'The requested number of Aggregates does not match the requested number of NodeIds. ';
    UA_STATUSCODE_BADAGGREGATENOTSUPPORTED : Result := 'The requested Aggregate is not support by the server. ';
    UA_STATUSCODE_BADAGGREGATEINVALIDINPUTS : Result := 'The aggregate value could not be derived due to invalid data inputs. ';
    UA_STATUSCODE_BADAGGREGATECONFIGURATIONREJECTED : Result := 'The aggregate configuration is not valid for specified node. ';
    UA_STATUSCODE_GOODDATAIGNORED : Result := 'The request specifies fields which are not valid for the EventType or cannot be saved by the historian. ';
    UA_STATUSCODE_BADREQUESTNOTALLOWED : Result := 'The request was rejected by the server because it did not meet the criteria set by the server. ';
    UA_STATUSCODE_BADREQUESTNOTCOMPLETE : Result := 'The request has not been processed by the server yet. ';
    UA_STATUSCODE_GOODEDITED : Result := 'The value does not come from the real source and has been edited by the server. ';
    UA_STATUSCODE_GOODPOSTACTIONFAILED : Result := 'There was an error in execution of these post-actions. ';
    UA_STATUSCODE_UNCERTAINDOMINANTVALUECHANGED : Result := 'The related EngineeringUnit has been changed but the Variable Value is still provided based on the previous unit. ';
    UA_STATUSCODE_GOODDEPENDENTVALUECHANGED : Result := 'A dependent value has been changed but the change has not been applied to the device. ';
    UA_STATUSCODE_BADDOMINANTVALUECHANGED : Result := 'The related EngineeringUnit has been changed but this change has not been applied to the device. The Variable Value is still dependent on the previous unit but its status is currently Bad. ';
    UA_STATUSCODE_UNCERTAINDEPENDENTVALUECHANGED : Result := 'A dependent value has been changed but the change has not been applied to the device. The quality of the dominant variable is uncertain. ';
    UA_STATUSCODE_BADDEPENDENTVALUECHANGED : Result := 'A dependent value has been changed but the change has not been applied to the device. The quality of the dominant variable is Bad. ';
    UA_STATUSCODE_GOODCOMMUNICATIONEVENT : Result := 'The communication layer has raised an event. ';
    UA_STATUSCODE_GOODSHUTDOWNEVENT : Result := 'The system is shutting down. ';
    UA_STATUSCODE_GOODCALLAGAIN : Result := 'The operation is not finished and needs to be called again. ';
    UA_STATUSCODE_GOODNONCRITICALTIMEOUT : Result := 'A non-critical timeout occurred. ';
    UA_STATUSCODE_BADINVALIDARGUMENT : Result := 'One or more arguments are invalid. ';
    UA_STATUSCODE_BADCONNECTIONREJECTED : Result := 'Could not establish a network connection to remote server. ';
    UA_STATUSCODE_BADDISCONNECT : Result := 'The server has disconnected from the client. ';
    UA_STATUSCODE_BADCONNECTIONCLOSED : Result := 'The network connection has been closed. ';
    UA_STATUSCODE_BADINVALIDSTATE : Result := 'The operation cannot be completed because the object is closed ';
    UA_STATUSCODE_BADENDOFSTREAM : Result := 'Cannot move beyond end of the stream. ';
    UA_STATUSCODE_BADNODATAAVAILABLE : Result := 'No data is currently available for reading from a non-blocking stream. ';
    UA_STATUSCODE_BADWAITINGFORRESPONSE : Result := 'The asynchronous operation is waiting for a response. ';
    UA_STATUSCODE_BADOPERATIONABANDONED : Result := 'The asynchronous operation was abandoned by the caller. ';
    UA_STATUSCODE_BADEXPECTEDSTREAMTOBLOCK : Result := 'The stream did not return all data requested (possibly because it is a non-blocking stream). ';
    UA_STATUSCODE_BADWOULDBLOCK : Result := 'Non blocking behaviour is required and the operation would block. ';
    UA_STATUSCODE_BADSYNTAXERROR : Result := 'A value had an invalid syntax. ';
    UA_STATUSCODE_BADMAXCONNECTIONSREACHED : Result := 'The operation could not be finished because all available connections are in use. ';
  else
    Result := 'UNKNOWN STATUSCODE';
  end;
  Result := 'Statuscode=' + IntToHex (aStatus) + ' -> ' + Result;
end;

end.

