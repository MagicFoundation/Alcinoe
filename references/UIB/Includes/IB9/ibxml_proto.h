/*
 *	PROGRAM:	IBXML - InterBase XML library.
 *	MODULE:		ibxml_proto.h
 *	DESCRIPTION: header file containing the ibxml function prototypes
 *
 * Copyright (C) 2001-2008 Embarcadero Technologies Inc.
 * All Rights Reserved.
 */

#ifndef IBXML_PROTO_H
#define IBXML_PROTO_H

#include <ibase.h>
#include <ibxml.h>

#if (defined(_MSC_VER) && defined(_WIN32)) || \
    (defined(__BORLANDC__) && (defined(__WIN32__) || defined(__OS2__)))
#define  ISC_FAR
#define  ISC_EXPORT	__stdcall
#define  ISC_EXPORT_VARARG	__cdecl
    typedef           __int64  ISC_INT64;
    typedef  unsigned __int64  ISC_UINT64;
#define  ISC_INT64_DEFINED
#else					/* Not Windows/NT */
#if (defined(__IBMC__) && defined(__OS2__))
#define  ISC_FAR
#define  ISC_EXPORT	_System
#define  ISC_EXPORT_VARARG	ISC_EXPORT
#else					/* not IBM C Set++ for OS/2 */
#if ( defined( _Windows) || defined( _WINDOWS))
#define  ISC_FAR	__far
#define  ISC_EXPORT     ISC_FAR __cdecl __loadds __export
#define  ISC_EXPORT_VARARG	ISC_EXPORT
#else					/* Not Windows/NT, OS/2 or Windows */
#define  ISC_FAR
#define  ISC_EXPORT
#define  ISC_EXPORT_VARARG
#endif					/* Windows and Not Windows/NT or OS/2 */
#endif					/* IBM C Set++ for OS/2 */
#endif   				/* Windows/NT */

int ISC_EXPORT isc_dsql_xml_buffer_fetch(ISC_STATUS *status, isc_stmt_handle *stmt,
		char *buffer, int buffer_size,  USHORT da_version,  
		XSQLDA *sqlda, IB_XMLDA *ib_xmlda);

int ISC_EXPORT isc_dsql_xml_fetch(ISC_STATUS *status, isc_stmt_handle *stmt,
		USHORT da_version, XSQLDA *sqlda, IB_XMLDA *ib_xmlda);

int ISC_EXPORT isc_dsql_xml_fetch_all(ISC_STATUS *status, isc_stmt_handle *stmt,
		USHORT da_version, XSQLDA *sqlda, 
		IB_XMLDA *ib_xmlda);

#endif /* IBXML_PROTO_H */
