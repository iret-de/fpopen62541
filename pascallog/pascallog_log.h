/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 *    Copyright 2017 (c) Fraunhofer IOSB (Author: Julius Pfrommer)
 *    Copyright 2017 (c) Stefan Profanter, fortiss GmbH
 * 
 * \note "stripped down" header with minimum definitions based on version 1.2.3 of open62541 (Stefan Hille, iret GmbH)
 * 
 */

#ifndef UA_PLUGIN_LOG_H_
#define UA_PLUGIN_LOG_H_

/**
 * Logging Plugin API
 * ==================
 *
 * Servers and clients define a logger in their configuration. The logger is a
 * plugin. A default plugin that logs to ``stdout`` is provided as an example.
 * The logger plugin is stateful and can point to custom data. So it is possible
 * to keep open file handlers in the logger context.
 *
 * Every log-message consists of a log-level, a log-category and a string
 * message content. The timestamp of the log-message is created within the
 * logger. */

typedef enum {
    UA_LOGLEVEL_TRACE = 0,
    UA_LOGLEVEL_DEBUG,
    UA_LOGLEVEL_INFO,
    UA_LOGLEVEL_WARNING,
    UA_LOGLEVEL_ERROR,
    UA_LOGLEVEL_FATAL
} UA_LogLevel;

typedef enum {
    UA_LOGCATEGORY_NETWORK = 0,
    UA_LOGCATEGORY_SECURECHANNEL,
    UA_LOGCATEGORY_SESSION,
    UA_LOGCATEGORY_SERVER,
    UA_LOGCATEGORY_CLIENT,
    UA_LOGCATEGORY_USERLAND,
    UA_LOGCATEGORY_SECURITYPOLICY
} UA_LogCategory;

typedef struct {
    /* Log a message. The message string and following varargs are formatted
     * according to the rules of the printf command. Use the convenience macros
     * below that take the minimum log-level defined in ua_config.h into
     * account. */
    void (*log)(void *logContext, UA_LogLevel level, UA_LogCategory category,
                const char *msg, va_list args);

    void *context; /* Logger state */

    void (*clear)(void *context); /* Clean up the logger plugin */
} UA_Logger;

#endif /* UA_PLUGIN_LOG_H_ */
