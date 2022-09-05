/*
 *	PROGRAM:	IBXML - InterBase XML library.
 *	MODULE:		ibxml.h
 *	DESCRIPTION: header file containing information shared by 
 *	             the ibxml functions
 *
 * Copyright (C) 2001-2008 Embarcadero Technologies Inc.
 * All Rights Reserved.
 */

#ifndef IBXML_H
#define IBXML_H

#ifndef _JRD_COMMON_H_
typedef short SSHORT;
typedef unsigned short USHORT;
typedef signed long SLONG;
typedef unsigned long ULONG;
typedef signed char SCHAR;

#endif

typedef struct vary {
    SSHORT	vary_length;
    SCHAR	vary_string [1];
} VARY;


#define DIGIT(c)        ((c) >= '0' && (c) <= '9')

typedef struct ib_xmlda
{
    char ISC_FAR    *xmlda_file_name;    /* pointer to a char string containing the name of the 
					    file used by xml_fetch(), ignored by the buffer 
					    function */
    char ISC_FAR    *xmlda_header_tag;   /* points to the string which is printed out as the 
					    header tag */
    char ISC_FAR    *xmlda_database_tag; /* points to the string which is printed out as 
					    the database tag in the xml file */
    char ISC_FAR    *xmlda_table_tag;    /* points to the string which is printed out as 
					    the tablename tag in the xml file */
    char ISC_FAR    *xmlda_row_tag;      /* points to the string which is printed out as 
				            the rowname tag in the xml file */
    FILE            *xmlda_file_ptr;     /* used internally by the API to hold the file pointer
					    can be POINTER type in non C, C++ programs */
    char ISC_FAR    **xmlda_temp_buffer; /* internal use only, used for storing the String array 
					    from fetch() */
    ISC_STATUS      xmlda_fetch_stat;    /* this element holds the return value from the 
					    isc_dsql_fetch() call, it indicates if we have 
					    received all the records or if we have a error */
    ULONG 	    xmlda_flags;         /* flags explained below */
    ULONG           xmlda_more_data;     /* used by the buffer call to maintian status of the last
					    record, 0 if there is no more data 1 if there is more 
				            data has been fetched but not put out in the buffer */
    ULONG           xmlda_temp_size;     /* internal use only, used to store the last records 
					    size */
    USHORT	    xmlda_status;        /* internal status must be set to 0 by user when called 
					    for the first time */
    USHORT          xmlda_more;          /* this flag is used in conjunction with the
				            buffered mode, if there is more XML data this is set */
    USHORT	    xmlda_version;       /* version of XMLDA */
    USHORT	    xmlda_array_size;    /* internal use only */
    SLONG	    xmlda_reserved;      /* reserved for future use */
} IB_XMLDA;




#define XMLDA_ATTRIBUTE_FLAG            0x01
#define XMLDA_NO_NULL_DISPLAY_FLAG      0x02
#define XMLDA_NO_HEADER_FLAG		0x04

#define MAXCHARSET_LENGTH  32   /* CHARSET names */
#define SHORT_LEN	7       /* NUMERIC (4,2) = -327.68 */
#define LONG_LEN	12	/* NUMERIC (9,2) = -21474836.48 */
#define INT64_LEN	21	/* NUMERIC(18,2) = -92233720368547758.08 */
#define QUAD_LEN	19
#define FLOAT_LEN	14	/* -1.2345678E+38 */
#define DOUBLE_LEN	23	/* -1.234567890123456E+300 */
#define DATE_LEN	11	/* 11 for date only */
#define DATETIME_LEN	25	/* 25 for date-time */
#define TIME_ONLY_LEN   13      /* 13 for time only */
#define DATE_ONLY_LEN   11
#define UNKNOWN_LEN     20      /* Unknown type: %d */

#define ERR_NOT_ENOUGH_MEMORY      -1
#define ERR_BUFFER_SIZE_NOT_ENOUGH -2

#endif /* IBXML_H */
