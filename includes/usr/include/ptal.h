/* ptal -- Peripheral Transport Abstraction Library */

/* Copyright (C) 2000-2003 Hewlett-Packard Company
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * is provided AS IS, WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, and
 * NON-INFRINGEMENT.  See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston,
 * MA 02111-1307, USA.
 *
 * In addition, as a special exception, Hewlett-Packard Company
 * gives permission to link the code of this program with any
 * version of the OpenSSL library which is distributed under a
 * license identical to that listed in the included LICENSE.OpenSSL
 * file, and distribute linked combinations including the two.
 * You must obey the GNU General Public License in all respects
 * for all of the code used other than OpenSSL.  If you modify
 * this file, you may extend this exception to your version of the
 * file, but you are not obligated to do so.  If you do not wish to
 * do so, delete this exception statement from your version.
 */
 
/* Original author: David Paschal */

#ifndef PTAL_H
#define PTAL_H

#ifdef __cplusplus
extern "C" {
#endif

#include <sys/time.h>

#define BEND_GET_SHORT(s) (((s)[0]<<8)|((s)[1]))
#define BEND_GET_LONG(s) (((s)[0]<<24)|((s)[1]<<16)|((s)[2]<<8)|((s)[3]))
#define BEND_SET_SHORT(s,x) ((s)[0]=((x)>>8)&0xFF,(s)[1]=(x)&0xFF)
#define BEND_SET_LONG(s,x) ((s)[0]=((x)>>24)&0xFF,(s)[1]=((x)>>16)&0xFF,(s)[2]=((x)>>8)&0xFF,(s)[3]=(x)&0xFF)
#define LEND_GET_SHORT(s) (((s)[1]<<8)|((s)[0]))
#define LEND_GET_LONG(s) (((s)[3]<<24)|((s)[2]<<16)|((s)[1]<<8)|((s)[0]))
#define LEND_SET_SHORT(s,x) ((s)[1]=((x)>>8)&0xFF,(s)[0]=(x)&0xFF)
#define LEND_SET_LONG(s,x) ((s)[3]=((x)>>24)&0xFF,(s)[2]=((x)>>16)&0xFF,(s)[1]=((x)>>8)&0xFF,(s)[0]=(x)&0xFF)

#define PTAL_OK				0
#define PTAL_ERROR			-1

#define PTAL_NO_FD			PTAL_ERROR

#define PTAL_STYPE_NONE			0
#define PTAL_STYPE_PRINT		1
#define PTAL_STYPE_SCAN			2
#define PTAL_STYPE_GENERIC		3
#define PTAL_STYPE_PML			4

#define PTAL_MAX_SERVICE_NAME_LEN	40

#define PTAL_DEFAULT_RETRY_COUNT	10
#define PTAL_SHORT_RETRY_COUNT		2
#define PTAL_DEFAULT_RETRY_DELAY	1


typedef struct ptalProvider_s *ptalProvider_t;
typedef struct ptalDevice_s *ptalDevice_t;
typedef struct ptalChannel_s *ptalChannel_t;
typedef struct ptalPmlObject_s *ptalPmlObject_t;

typedef int ptalProviderEnumerate_f(char *name,void *cbd);
typedef int ptalDeviceEnumerate_f(ptalDevice_t dev,void *cbd);
typedef int ptalChannelEnumerate_f(ptalChannel_t chan,void *cbd);
typedef int ptalPmlObjectEnumerate_f(ptalPmlObject_t obj,void *cbd);
typedef int ptalChannelSelectPoll_f(ptalChannel_t chan,void *cbd);

int ptalLogMsg(int level,char *format,...);
#define PTAL_LOG_ERROR(args...) ptalLogMsg(0,args)
#define PTAL_LOG_WARN(args...)  ptalLogMsg(1,args)
#define PTAL_LOG_DEBUG(args...) ptalLogMsg(2,args)

void ptalDeviceDump(ptalDevice_t dev,int level);
void ptalDump(int level);
void ptalChannelDump(ptalChannel_t chan,int level);

int ptalProviderEnumerate(ptalProviderEnumerate_f callback,void *cbd);
ptalDevice_t ptalDeviceOpen(char *name);
int ptalInit(void);
int ptalDeviceClose(ptalDevice_t dev);
int ptalDeviceDelete(ptalDevice_t dev);
int ptalDeviceEnumerate(ptalProvider_t provider,
    ptalDeviceEnumerate_f callback,void *cbd);
int ptalDeviceEnumeratePrintCallback(ptalDevice_t dev,void *cbd);
char *ptalDeviceGetName(ptalDevice_t dev);
int ptalDone(void);
void *ptalDeviceGetAppInfo(ptalDevice_t dev);
void ptalDeviceSetAppInfo(ptalDevice_t dev,void *appInfo);

int ptalDeviceGetDeviceIDString(ptalDevice_t dev,char *buffer,int maxlen);
int ptalDeviceGetPreviousDeviceIDString(ptalDevice_t dev,
    char *buffer,int maxlen);
int ptalDeviceIDGetField(char *_devID,char *field,
    char **pValue,int *pLenValue);
void ptalDeviceIDPruneField(char **pValue,int *pLenValue);
int ptalDeviceIDGetEitherField(char *devID,char *field1,char *field2,
    char **pValue,int *pLenValue);
int ptalDeviceIDGetManufacturer(char *devID,char **pValue,int *pLenValue);
int ptalDeviceIDGetModel(char *devID,char **pValue,int *pLenValue);
int ptalDeviceIDGetCommandSet(char *devID,char **pValue,int *pLenValue);
int ptalDeviceIDGetSerialNumber(char *devID,char **pValue,int *pLenValue);

ptalChannel_t ptalChannelAllocate(ptalDevice_t dev);
int ptalChannelEnumerate(ptalDevice_t dev,
    ptalChannelEnumerate_f callback,void *cbd);
int ptalChannelDeallocate(ptalChannel_t chan);
int ptalChannelDeallocateCbd(ptalChannel_t chan,void *cbd);
int ptalChannelGetRemoteService(ptalChannel_t chan,
    int *pServiceType,int *pSocketID,char **pServiceName);
int ptalChannelSetRemoteService(ptalChannel_t chan,
    int serviceType,int socketID,char *serviceName);
ptalChannel_t ptalChannelFindOrAllocate(ptalDevice_t dev,
    int serviceType,int socketID,char *serviceName);
int ptalChannelSetPacketSizes(ptalChannel_t chan,
    int desiredHPSize,int desiredPHSize);
int ptalChannelAdjustPacketSizes(ptalChannel_t chan,
    int *pDesiredHPSize,int *pDesiredPHSize);
int ptalChannelSetErrorHandling(ptalChannel_t chan,
    int retryCount,int retryDelay);
int ptalChannelIsOpen(ptalChannel_t chan);
int ptalChannelOpen(ptalChannel_t chan);
int ptalChannelOpenOrReopen(ptalChannel_t chan);
int ptalChannelClose(ptalChannel_t chan);
int ptalFdPrepareForSelect(int fd,int *pn,
    fd_set *prset,fd_set *pwset,fd_set *pxset);
int ptalChannelPrepareForSelect(ptalChannel_t chan,
    int *pfd,int *pn,fd_set *prset,fd_set *pwset,fd_set *pxset);
int ptalChannelSetSelectPollTimeout(ptalChannel_t chan,
    struct timeval *pTimeout);
int ptalChannelSetSelectPollCallback(ptalChannel_t chan,
    ptalChannelSelectPoll_f callback,void *cbd);
int ptalChannelSelect(ptalChannel_t chan,int *pr,int *pw,int *px,
    struct timeval *timeout);
int ptalChannelRead(ptalChannel_t chan,char *buffer,int count);
int ptalChannelIsStale(ptalChannel_t chan);
int ptalChannelReadTimeout(ptalChannel_t chan,char *buffer,int countdown,
    struct timeval *startTimeout,struct timeval *continueTimeout);
int ptalSclChannelRead(ptalChannel_t chan,char *buffer,int countdown,
    struct timeval *startTimeout,struct timeval *continueTimeout,
    int isSclResponse);
int ptalChannelFlush(ptalChannel_t chan,
    struct timeval *startTimeout,struct timeval *continueTimeout);
int ptalChannelWrite(ptalChannel_t chan,char *buffer,int count);

int ptalPmlOpen(ptalDevice_t dev);
int ptalPmlClose(ptalDevice_t dev);
ptalPmlObject_t ptalPmlAllocate(ptalDevice_t dev);
int ptalPmlEnumerate(ptalDevice_t dev,
    ptalPmlObjectEnumerate_f callback,void *cbd);
int ptalPmlDeallocate(ptalPmlObject_t obj);
int ptalPmlDeallocateCallback(ptalPmlObject_t obj,void *_cbd);
int ptalPmlDeallocateAll(ptalDevice_t dev);
int ptalPmlSetID(ptalPmlObject_t obj,char *oid);
ptalPmlObject_t ptalPmlAllocateID(ptalDevice_t dev,char *oid);
int ptalPmlSetAsciiID(ptalPmlObject_t obj,char *s);
int ptalPmlGetID(ptalPmlObject_t obj,char *buffer,int maxlen);
int ptalPmlSetValue(ptalPmlObject_t obj,int type,char *value,int len);
int ptalPmlSetStringValue(ptalPmlObject_t obj,int symbolSet,
    char *value,int len);
int ptalPmlSetIntegerValue(ptalPmlObject_t obj,int type,int value);
int ptalPmlGetType(ptalPmlObject_t obj);
int ptalPmlGetValue(ptalPmlObject_t obj,int *pType,char *buffer,int maxlen);
int ptalPmlGetStringValue(ptalPmlObject_t obj,int *pSymbolSet,
    char *buffer,int maxlen);
int ptalPmlGetIntegerValue(ptalPmlObject_t obj,int *pType,int *pValue);
int ptalPmlDoLastValuesDiffer(ptalPmlObject_t obj);
int ptalPmlGetStatus(ptalPmlObject_t obj);
int ptalPmlRequestSet(ptalPmlObject_t obj);
int ptalPmlRequestSetRetry(ptalPmlObject_t obj,int count,int delay);
int ptalPmlRequestGet(ptalPmlObject_t obj,ptalPmlObject_t next);
int ptalPmlRequestSetTrap(ptalPmlObject_t obj,int enable);
int ptalPmlIsTrapEnabled(ptalPmlObject_t obj);
int ptalPmlGetAndClearTrapCount(ptalPmlObject_t obj);
int ptalPmlServiceTraps(ptalDevice_t dev);

#define PTAL_PML_MAX_VALUE_LEN		1023
#define PTAL_PML_MAX_OID_LEN		32
#define PTAL_PML_MAX_DATALEN		4096

#define PTAL_PML_TYPE_MASK			0xFC
#define PTAL_PML_TYPE_OBJECT_IDENTIFIER		0x00
#define PTAL_PML_TYPE_ENUMERATION		0x04
#define PTAL_PML_TYPE_SIGNED_INTEGER		0x08
#define PTAL_PML_TYPE_REAL			0x0C
#define PTAL_PML_TYPE_STRING			0x10
#define PTAL_PML_TYPE_BINARY			0x14
#define PTAL_PML_TYPE_ERROR_CODE		0x18
#define PTAL_PML_TYPE_NULL_VALUE		0x1C
#define PTAL_PML_TYPE_COLLECTION		0x20

#define PTAL_PML_SYMSET_0E				0x000E
#define PTAL_PML_SYMSET_ISO_8859_2_LATIN_2		0x004E
#define PTAL_PML_SYMSET_ISO_8859_9_LATIN_5		0x00AE
#define PTAL_PML_SYMSET_KANA8				0x010B
#define PTAL_PML_SYMSET_ROMAN8				0x0115
#define PTAL_PML_SYMSET_ISO_8859_5_LATIN_CYRILLIC	0x014E
#define PTAL_PML_SYMSET_US_UNICODE			0x024E
#define PTAL_PML_SYMSET_UTF8_UNICODE			0xFDE8

#define PTAL_PML_OK					0x00
#define PTAL_PML_OK_END_OF_SUPPORTED_OBJECTS		0x01
#define PTAL_PML_OK_NEAREST_LEGAL_VALUE_SUBSTITUTED	0x02
#define PTAL_PML_ERROR					0x80
#define PTAL_PML_ERROR_UNKNOWN_REQUEST			0x80
#define PTAL_PML_ERROR_BUFFER_OVERFLOW			0x81
#define PTAL_PML_ERROR_COMMAND_EXECUTION_ERROR		0x82
#define PTAL_PML_ERROR_UNKNOWN_OBJECT_IDENTIFIER	0x83
#define PTAL_PML_ERROR_OBJECT_DOES_NOT_SUPPORT_REQUESTED_ACTION	0x84
#define PTAL_PML_ERROR_INVALID_OR_UNSUPPORTED_VALUE	0x85
#define PTAL_PML_ERROR_PAST_END_OF_SUPPORTED_OBJECTS	0x86
#define PTAL_PML_ERROR_ACTION_CAN_NOT_BE_PERFORMED_NOW	0x87
#define PTAL_PML_ERROR_SYNTAX_ERROR			0x88


typedef struct ptalMfpdtf_s *ptalMfpdtf_t;

#define PTAL_MFPDTF_RESULT_NEW_PAGE			0x00000001
#define PTAL_MFPDTF_RESULT_END_PAGE			0x00000002
#define PTAL_MFPDTF_RESULT_NEW_DOCUMENT			0x00000004
#define PTAL_MFPDTF_RESULT_END_DOCUMENT			0x00000008
#define PTAL_MFPDTF_RESULT_END_STREAM			0x00000010
#define PTAL_MFPDTF_RESULT_RESERVED_20			0x00000020
#define PTAL_MFPDTF_RESULT_RESERVED_40			0x00000040
#define PTAL_MFPDTF_RESULT_RESERVED_80			0x00000080

#define PTAL_MFPDTF_RESULT_00000100			0x00000100
#define PTAL_MFPDTF_RESULT_READ_TIMEOUT			0x00000200
#define PTAL_MFPDTF_RESULT_READ_ERROR			0x00000400
#define PTAL_MFPDTF_RESULT_OTHER_ERROR			0x00000800
#define PTAL_MFPDTF_RESULT_ERROR_MASK			0x00000E00

#define PTAL_MFPDTF_RESULT_NEW_DATA_TYPE		0x00001000
#define PTAL_MFPDTF_RESULT_NEW_VARIANT_HEADER		0x00002000
#define PTAL_MFPDTF_RESULT_GENERIC_DATA_PENDING		0x00004000
#define PTAL_MFPDTF_RESULT_ARRAY_DATA_PENDING		0x00008000

#define PTAL_MFPDTF_RESULT_NEW_START_OF_PAGE_RECORD	0x00010000
#define PTAL_MFPDTF_RESULT_IMAGE_DATA_PENDING		0x00020000
#define PTAL_MFPDTF_RESULT_NEW_END_OF_PAGE_RECORD	0x00040000
#define PTAL_MFPDTF_RESULT_00080000			0x00080000
#define PTAL_MFPDTF_RESULT_INNER_DATA_PENDING		0x00028000

enum ptalMfpdtfDataType_e {
	PTAL_MFPDTF_DT_UNKNOWN       = 0,
	PTAL_MFPDTF_DT_FAX_IMAGES    = 1,
	PTAL_MFPDTF_DT_SCANNED_IMAGES= 2,
	PTAL_MFPDTF_DT_DIAL_STRINGS  = 3,
	PTAL_MFPDTF_DT_DEMO_PAGES    = 4,
	PTAL_MFPDTF_DT_SPEED_DIALS   = 5,
	PTAL_MFPDTF_DT_FAX_LOGS      = 6,
	PTAL_MFPDTF_DT_CFG_PARMS     = 7,
	PTAL_MFPDTF_DT_LANG_STRS     = 8,
	PTAL_MFPDTF_DT_JUNK_FAX_CSIDS= 9,  /* PTAL_MFPDTF_DT_DIAL_STRINGS */
	PTAL_MFPDTF_DT_REPORT_STRS   =10,  /* PTAL_MFPDTF_DT_LANG_STRS    */
	PTAL_MFPDTF_DT_FONTS         =11,
	PTAL_MFPDTF_DT_TTI_BITMAP    =12,
	PTAL_MFPDTF_DT_COUNTERS      =13,
	PTAL_MFPDTF_DT_DEF_PARMS     =14,  /* PTAL_MFPDTF_DT_CFG_PARMS    */
	PTAL_MFPDTF_DT_SCAN_OPTIONS  =15,
	PTAL_MFPDTF_DT_FW_JOB_TABLE  =17
};
#define PTAL_MFPDTF_DT_MASK_IMAGE \
	((1<<PTAL_MFPDTF_DT_FAX_IMAGES) | \
	 (1<<PTAL_MFPDTF_DT_SCANNED_IMAGES) | \
	 (1<<PTAL_MFPDTF_DT_DEMO_PAGES))

enum ptalMfpdtfImageEncoding_e {
	PTAL_MFPDTF_RASTER_BITMAP =0,
	PTAL_MFPDTF_RASTER_GRAYMAP=1,
	PTAL_MFPDTF_RASTER_MH     =2,
	PTAL_MFPDTF_RASTER_MR     =3,
	PTAL_MFPDTF_RASTER_MMR    =4,
	PTAL_MFPDTF_RASTER_RGB    =5,
	PTAL_MFPDTF_RASTER_YCC411 =6,
	PTAL_MFPDTF_RASTER_JPEG   =7,
	PTAL_MFPDTF_RASTER_PCL    =8,
	PTAL_MFPDTF_RASTER_NOT    =9
};

struct ptalMfpdtfImageStartPageRecord_s {
	unsigned char encoding;
	unsigned char pageNumber[2];
	struct {
		unsigned char pixelsPerRow[2];
		unsigned char bitsPerPixel[2];
		unsigned char rowsThisPage[4];
		unsigned char xres[4];
		unsigned char yres[4];
	} black,color;
} __attribute__((packed));

struct ptalMfpdtfImageEndPageRecord_s {
	unsigned char unused[3];
	struct {
		unsigned char numberOfRows[4];
	} black,color;
} __attribute__((packed));

union ptalMfpdtfVariantHeader_u {
	struct ptalMfpdtfVariantHeaderFaxDataArtoo_s {
		unsigned char majorVersion;	/* 1 */
		unsigned char minorVersion;	/* 0 */
		unsigned char dataSource;	/* unknown=0,prev,host,fax,pollfax,scanner */
		unsigned char dataFormat;	/* unknown=0,ASCII=3,CCITT_G3=10 */
		unsigned char dataCompression;	/* native=1,MH,MR,MMR */
		unsigned char pageResolution;	/* fine=0,std,CCITT300,CCITT400 */
		unsigned char pageSize;		/* unknown=0 */
		unsigned char pixelsPerRow[2];
		unsigned char year;
		unsigned char month;
		unsigned char day;
		unsigned char hour;
		unsigned char minute;
		unsigned char second;
		unsigned char T30_CSI[20];
		unsigned char T30_SUB[20];
	} __attribute__((packed)) faxArtoo;

	struct ptalMfpdtfVariantHeaderFaxDataSolo_s {
		unsigned char majorVersion;	/* 1 */
		unsigned char minorVersion;	/* 1 */
		unsigned char dataSource;
		unsigned char dataFormat;
		unsigned char dataCompression;
		unsigned char pageResolution;
		unsigned char pageSize;
		unsigned char pixelsPerRow[2];
		unsigned char year;
		unsigned char month;
		unsigned char day;
		unsigned char hour;
		unsigned char minute;
		unsigned char second;
		unsigned char suppressTTI;
		unsigned char T30_CSI[20];
		unsigned char T30_SUB[20];
		unsigned char T30_PWD[20];
	} __attribute__((packed)) faxSolo;

	struct ptalMfpdtfVariantHeaderFaxData_s {
		unsigned char majorVersion;
		unsigned char minorVersion;
		/* TODO: Finish. */
	} __attribute__((packed)) fax;

	struct ptalMfpdtfVariantHeaderImageData_s {
		unsigned char majorVersion;
		unsigned char minorVersion;
		unsigned char sourcePages[2];
		unsigned char copiesPerPage[2];
		unsigned char zoomFactor[2];
		unsigned char jpegQualityFactor[2];
	} __attribute__((packed)) image;

	struct ptalMfpdtfVariantHeaderArrayData_s {
		unsigned char majorVersion;
		unsigned char minorVersion;
		unsigned char recordCount[2];
		unsigned char recordSize[2];
	} __attribute__((packed)) array;
} __attribute__((packed));

ptalMfpdtf_t ptalMfpdtfAllocate(ptalChannel_t chan);
int ptalMfpdtfDeallocate(ptalMfpdtf_t mfpdtf);
int ptalMfpdtfSetChannel(ptalMfpdtf_t mfpdtf,ptalChannel_t chan);
int ptalMfpdtfLogToFile(ptalMfpdtf_t mfpdtf,char *filename);
int ptalMfpdtfReadGetTimeout(ptalMfpdtf_t mfpdtf);
int ptalMfpdtfReadSetTimeout(ptalMfpdtf_t mfpdtf,int seconds);
int ptalMfpdtfReadGetSimulateImageHeaders(ptalMfpdtf_t mfpdtf);
int ptalMfpdtfReadSetSimulateImageHeaders(ptalMfpdtf_t mfpdtf,
    int simulateImageHeaders);
int ptalMfpdtfReadStart(ptalMfpdtf_t mfpdtf);
int ptalMfpdtfReadService(ptalMfpdtf_t mfpdtf);
int ptalMfpdtfReadGetDataType(ptalMfpdtf_t mfpdtf);
int ptalMfpdtfReadIsImageData(ptalMfpdtf_t mfpdtf);
int ptalMfpdtfReadIsArrayData(ptalMfpdtf_t mfpdtf);
int ptalMfpdtfReadGetArrayRecordCountSize(ptalMfpdtf_t mfpdtf,
    int *pCount,int *pSize);
int ptalMfpdtfReadGetFixedBlockBytesRemaining(ptalMfpdtf_t mfpdtf);
int ptalMfpdtfReadGetInnerBlockBytesRemaining(ptalMfpdtf_t mfpdtf);
int ptalMfpdtfReadGetLastServiceResult(ptalMfpdtf_t mfpdtf);
int ptalMfpdtfReadGetVariantHeader(ptalMfpdtf_t mfpdtf,
    union ptalMfpdtfVariantHeader_u *buffer,int maxlen);
int ptalMfpdtfReadGetStartPageRecord(ptalMfpdtf_t mfpdtf,
    struct ptalMfpdtfImageStartPageRecord_s *buffer,int maxlen);
int ptalMfpdtfReadGetEndPageRecord(ptalMfpdtf_t mfpdtf,
    struct ptalMfpdtfImageEndPageRecord_s *buffer,int maxlen);
int ptalMfpdtfReadGeneric(ptalMfpdtf_t mfpdtf,
    unsigned char *buffer,int datalen);
int ptalMfpdtfReadInnerBlock(ptalMfpdtf_t mfpdtf,
    unsigned char *buffer,int countdown);


#ifdef __cplusplus
}
#endif

#endif
