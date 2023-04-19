/***********************************************************************
 *
 *  dmk2sdf - Convert a floppy disk image in the DMK format to
 *            the SDF format used by the CoCo SDC hardware.
 *
 *  Written by Darren Atkinson.
 *
 *  This program is free software; you can redistribute it and/or
 *  modify it without restriction.
 *
 *  This program is provided in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 */

// At most, set only one of these to 1
#ifndef MS_WIN
  #define MS_WIN				0		// Building for Windows
#endif

#ifndef MAC_OSX
  #define MAC_OSX				1		// Building for Mac
#endif


#if MS_WIN
  // Windows API headers needed for GetStdHandle() etc..
  // #include "ansi_prefix.win32.h"
  #define STRICT
  #include "wtypes.h"
  #include "winbase.h"
#endif


// Standard Library Headers
#include  <stdlib.h>
#include  <stdarg.h>
#include  <stdio.h>
#include  <string.h>


// Version
#define kVersionString			"0.1a1"


// CPU Endian Type
#if MAC_OSX && (defined(powerc) || defined(__powerc) || defined(__POWERPC__))
  #define ENDIAN_BIG			1
#else
  #define ENDIAN_BIG			0
#endif


// End-Of-Line character(s)
#if MS_WIN
  // CR-LF
  #define EOL_STR				"\x0d\x0a"
  #define EOL_LEN				(2)
#else
  // LF
  #define EOL_STR				"\x0a"
  #define EOL_LEN				(1)
#endif


// Explicitly sized integer types
typedef signed char				sint8;
typedef unsigned char			uint8;
typedef signed short			sint16;
typedef unsigned short			uint16;
#ifndef __LP64__
  typedef signed long			sint32;
  typedef unsigned long			uint32;
#else
  #if __LP64__
	typedef signed int			sint32;
	typedef unsigned int		uint32;
  #else
	typedef signed long			sint32;
	typedef unsigned long		uint32;
  #endif
#endif


// no compiler padding in structures
#pragma pack(1)


/*======================================================================================================*/
// DMK FILE STRUCTURES

#define kDMKTrackDataSize		(6272)				// Number of raw track data bytes in a standard Double-Density DMK file
#define kDMKMaxSectors			(64)				// Maximum number of sectors per track in a DMK file


// floppy disk metrics
#define kMaxCylinders			(80)				// Maximum number of Cylinders per disk
#define kMFMTrackSize			(6250)				// Number of raw bytes in an MFM track at exactly 300 rpm


// header flags
enum {
	kDMKSingleSided				= 0x10,
	kDMKSingleDensity			= 0x40
};

typedef uint16					DMK_ID_TABLE [kDMKMaxSectors];

typedef struct {
	uint8						readOnly;			// 0xFF = write protected
	uint8						cylinders;			// number of cylinders
	uint16						trackSize;			// number of bytes per track including the ID table
	uint8						flags;				// format options (see above)
	uint8						reserved1;
	uint16						reserved2;
	uint8						reserved3 [8];
} DMK_HEADER;


// context record used while translating each track from the DMK file
typedef struct {
	FILE						*file;
	uint16						scanPos;
	uint8						fmDisk;
	uint8						dden;
	DMK_ID_TABLE				idTable;
	uint8						rawData [kMFMTrackSize];
} DMK_CTX;


/*======================================================================================================*/
// SDF FILE STRUCTURES

#define kSDFHeaderSignature		"SDF1"
#define kSDFMaxSectors			(31)				// Maximum number of sectors per track (31 * 8 bytes = 248 bytes)
#define kSDFTrackDataSize		(6400)				// Size of raw track data in an SDF file


// 512 byte File Header
typedef struct {
	uint8						sig[4];				// signature / version
	uint8						cylinders;			// number of cylinders
	uint8						sides;				// number of sides (1 or 2)
	uint8						readOnly;			// 0xFF = write protected
	uint8						nestedSectors;		// TRUE if disk is known to use the nested sector protection scheme
	uint8						reserved [504];
} SDF_HEADER;


// 8 byte structure containing info for a single sector
typedef struct {
	uint16						idPos;				// offset to first byte of ID field  (bit 7 set if CRC error)
	uint16						dataPos;			// offset to data field (0 = no data field, bit 7 set if CRC error, bit 6 set if Deleted Data Mark)
	uint8						cyl;				// \	cylinder number
	uint8						side;				//  |	side (0 or 1)
	uint8						sector;				//  |	sector ID
	uint8						sizeCode;			// /	sector data size code
} SDF_SECTOR_INFO;


enum {
	kBadCRCFlag					= 0x8000u,
	kDeletedMarkFlag			= 0x4000
};


// 256 byte structure containing the array of SDF_SECTOR_INFO structures for a single track
typedef struct {
	uint8						count;				// number of active sectors in table
	uint8						reserved0;
	uint16						reserved1;
	uint16						reserved2;
	uint16						reserved3;
	SDF_SECTOR_INFO				table [kSDFMaxSectors];
} SDF_TRACK_INFO;



// Global variables
static char 	gQuietOpt;		// Quiet option (-q)
static char		gOutputSides;	// Single or Double sided output (-s1 or -s2)


/*======================================================================================================*/

static inline uint16 BigEndian16 (uint16 inVal) {
	#if !(ENDIAN_BIG)
		return ((inVal >> 8) & 0x000ff) | ((inVal << 8) & 0x0ff00);
	#else
		return inVal;
	#endif
}


/*======================================================================================================*/

static inline uint16 LittleEndian16 (uint16 inVal) {
	#if ENDIAN_BIG
		return ((inVal >> 8) & 0x000ff) | ((inVal << 8) & 0x0ff00);
	#else
		return inVal;
	#endif
}


/*======================================================================================================*/
// write to stdout

static void WriteStdOut (const char* pstr)
{
	#if MS_WIN
		DWORD actual;
		WriteFile(GetStdHandle(STD_OUTPUT_HANDLE), pstr, lstrlen(pstr), &actual, NULL);
	#else
		fprintf(stdout, pstr);
	#endif
}


/*=====================================================================================================*/
// write to stderr and exit

static void ErrorAbort (const char *message)
{
	#if MS_WIN
		HANDLE		stdError;
		DWORD		actual;

		stdError = GetStdHandle(STD_ERROR_HANDLE);
		WriteFile(stdError, message, lstrlen(message), &actual, NULL);
		WriteFile(stdError, EOL_STR EOL_STR, EOL_LEN + EOL_LEN, &actual, NULL);
		ExitProcess (EXIT_FAILURE);
	#else
		fprintf(stderr, message);
		fprintf(stderr, EOL_STR EOL_STR);
		exit(1);
	#endif
}


/*======================================================================================================*/
// log status info to console

static void StatusOut (const char* pstr, ...)
{
	va_list		args;
	char		strz[1024];

	va_start(args, pstr);
	vsprintf(strz, pstr, args);
	va_end(args);

	if (gQuietOpt == 0) {
		WriteStdOut(strz);
	}
}


/*======================================================================================================*/
// report usage information and exit

static void Usage (void)
{
    WriteStdOut("===================================" EOL_STR
    			"       dmk2sdf - Version " kVersionString EOL_STR
    			"  DMK to SDF disk image converter" EOL_STR
    			"===================================" EOL_STR
				EOL_STR
				"Usage:" EOL_STR
				"   dmk2sdf dmk_file [option [option...]]  [-o sdf_file]"  EOL_STR
				EOL_STR
				"Options:" EOL_STR
				"   -h    Display this Help message." EOL_STR
				"   -q    Quiet run. Does not output any status information." EOL_STR
				"   -s1   Create single-sided image even if source is double-sided." EOL_STR
				"   -s2   Create double-sided image even if source is single-sided." EOL_STR
				EOL_STR
    );

    #if MS_WIN
    	ExitProcess (EXIT_FAILURE);
    #else
    	exit(1);
    #endif
}


/*=====================================================================================================*/

static void ExpandFMTrack (DMK_CTX *dmk)
{
	/*
		Expand raw track data from a single density DMK image by
		generating 2 output bytes for each input byte.
	*/

	uint8			*src;
	uint8			*dst;
	uint8			buf [kMFMTrackSize >> 1];
	unsigned int	i;

	// copy source data to a temp buffer
	memcpy(buf, dmk->rawData, kMFMTrackSize >> 1);
	src = buf;
	dst = dmk->rawData;

	// two bytes for every source byte
	for (i = 0; i < (kMFMTrackSize >> 1); i++) {
		*dst++ = *src;
		*dst++ = *src++;
	}
}


/*======================================================================================================*/

static uint16 AddByteToCRC (uint16 inCRC, uint8 inByte)
{
	uint16 n = (inCRC >> 8) ^ inByte;
	n ^= (n >> 4);
	return (inCRC << 8) ^ (n << 12) ^ (n << 5) ^ n;
}


/*======================================================================================================*/

static uint16 AddRunToCRC (uint16 crc, uint8 *dataPtr, int runLength)
{
	if (runLength) do {
		crc = AddByteToCRC(crc, *dataPtr++);
	} while (--runLength);

	return crc;
}


/*======================================================================================================*/

static void ScanOffsetDMK (DMK_CTX* dmk, sint16 offset)
{
	/*
		Move the scan pointer forwards or
		backwards and wrap-around if needed.

		Doubles the offset value if the DMK
		context is in single density mode.
	*/

	sint16 tmp = dmk->scanPos;
	if (!dmk->dden) offset *= 2;
	tmp += offset;

	// wrap-around ?
	if (tmp < 0) {
		tmp += kMFMTrackSize;
	} else if (tmp > kMFMTrackSize) {
		tmp -= kMFMTrackSize;
	}

	dmk->scanPos = tmp;
}


/*======================================================================================================*/

static void ReadDMK (DMK_CTX* dmk, void *buf, int len)
{
	/*
		Read a number of bytes from the raw track data starting
		at the current scan position.  Wrap-around if needed.
		Update the scan position as we go.

		Reads every-other byte if the DMK context
		is in single density mode.
	*/

	uint8 *ptr = (uint8*) buf;

	if (len) do {
		*ptr++ = dmk->rawData[dmk->scanPos];
		ScanOffsetDMK(dmk, 1);
	} while (--len);
}


/*======================================================================================================*/

static uint8 ExtractDMKSector (DMK_CTX *dmk, uint8 side, uint8 cyl, uint8 index, SDF_SECTOR_INFO *outInfo)
{
	/*
		Extract information for the nth sector from
		the currently loaded DMK track.
	*/

	sint32		size;
	uint16		idPos;
	uint16		crc;
	uint16		recordedCRC;
	sint16		adjustCount;
	uint8		byte;
	uint8		gap;
	uint8		sync;
	uint8		mark;
	uint8		result;
	uint8		buf [1024];

	RETRY:

	result = 0;
	idPos = dmk->idTable[index];
	dmk->dden = (idPos >> 15);
	idPos &= 0x3fff;

	if (idPos) {
		memset(outInfo, 0, sizeof(SDF_SECTOR_INFO));

		// Set scan postion to location of the IDAM and read the ID field
		dmk->scanPos = idPos;
		ReadDMK(dmk, &mark, 1);				// ID Address Mark
		ReadDMK(dmk, &outInfo->cyl, 4);		// ID Field
		ReadDMK(dmk, &recordedCRC, 2);		// CRC

		// If MFM mode then scan backwards for A1 bytes but don't wrap-around
		crc = 0xffff;
		sync = 0;
		dmk->scanPos = idPos;

		if (dmk->dden) {
			while (dmk->scanPos) {
				ScanOffsetDMK(dmk, -1);
				ReadDMK(dmk, &byte, 1);
				if (byte != 0xA1) break;
				crc = AddByteToCRC(crc, byte);
				sync += 1;
				ScanOffsetDMK(dmk, -1);
			}

			// Must have at least 1 sync byte to be a valid IDAM in MFM mode.
			// If not valid then delete it from the table and try again.
			if (sync == 0) {
				if ((adjustCount = kDMKMaxSectors - index - 1) > 0) {
					memmove(&dmk->idTable[index], &dmk->idTable[index+1], adjustCount * sizeof(idPos));
				}

				dmk->idTable[kDMKMaxSectors-1] = 0;
				goto RETRY;
			}
		}

		// Store position of the ID field in the SDF track header.
		result = 1;
		outInfo->idPos = LittleEndian16(idPos + sizeof(SDF_TRACK_INFO) + 1);

		// Set flag bit if the ID field has an incorrect CRC.
		crc = AddByteToCRC(crc, mark);
		if (AddRunToCRC(crc, &outInfo->cyl, 4) != BigEndian16(recordedCRC)) {
			outInfo->idPos |= LittleEndian16(kBadCRCFlag);
			StatusOut("Bad CRC in ID field (head=%d, cyl=%d, pos=%04x, crc=%04x)" EOL_STR, side, cyl, outInfo->idPos, crc);
		}

		// Report unusual ID Fields
		if (outInfo->side != side) {
			StatusOut("ID field has unexpected Side number of %d  (head=%d, cyl=%d, sector=%d)" EOL_STR, outInfo->side, side, cyl, outInfo->sector);
		}

		if (outInfo->cyl != cyl) {
			StatusOut("ID field has unexpected Track number of %d  (head=%d, cyl=%d, sector=%d)" EOL_STR, outInfo->cyl, side, cyl, outInfo->sector);
		}

		// Report sectors whose size is not 256 bytes
		if (outInfo->sizeCode != 1) {
			size = 128 << (outInfo->sizeCode & 0x03);
			StatusOut("Track has %d byte sector (head=%d, cyl=%d, sector=%d)" EOL_STR, size, side, cyl, outInfo->sector);
		}

		// Scan for the Data Mark within the maximum gap range
		dmk->scanPos = idPos;
		ScanOffsetDMK(dmk, 7);
		gap = 43;
		sync = 0;

		do {
			ReadDMK(dmk, &byte, 1);
			crc = AddByteToCRC(crc, byte);

			// Have we found a Data Mark?
			if (byte >= 0xF8 && byte <= 0xFB) {

				// Preset CRC at mark in FM mode
				if (!dmk->dden) {
					crc = AddByteToCRC(0xffff, byte);
					sync = 1;
				}

				if (sync) {
					outInfo->dataPos = LittleEndian16(dmk->scanPos + sizeof(SDF_TRACK_INFO));

					// Set flag bit for a Deleted Data Mark.
					if (!(byte & 0x02)) {
						outInfo->dataPos |= LittleEndian16(kDeletedMarkFlag);
					}
					break;
				}
			}

			// Watch for A1 sync bytes in MFM mode
			if (byte == 0xA1 && dmk->dden) {
				// preset CRC at first sync byte
				if (++sync == 1) {
					crc = AddByteToCRC(0xffff, byte);
				}
			} else {
				sync = 0;
			}
		} while (--gap);

		// If the Data Mark was found in time then read the data field and validate its CRC
		if (gap) {
			size = 128 << (outInfo->sizeCode & 0x03);
			ReadDMK(dmk, buf, size);
			crc = AddRunToCRC(crc, buf, size);
			ReadDMK(dmk, &recordedCRC, 2);

			// Set flag bit if the data field has an incorrect CRC.
			if (crc != BigEndian16(recordedCRC)) {
				outInfo->dataPos |= LittleEndian16(kBadCRCFlag);
				StatusOut("Bad CRC in DATA field (head=%d, cyl=%d, sector=%d, crc=%04x)" EOL_STR, side, cyl, outInfo->sector, crc);
			}
		} else {
			StatusOut("Missing DATA field for sector (head=%d, cyl=%d, sector=%d)" EOL_STR, side, cyl, outInfo->sector);
		}
	}

	// Return a zero-filled record for non-existing sector
	if (!result) {
		memset(outInfo, 0, sizeof(SDF_SECTOR_INFO));
	}

	return (result);
}


/*======================================================================================================*/

static void dmk2sdf (FILE *dmkFile, FILE *sdfFile)
{
	DMK_CTX			dmk;
	DMK_HEADER		dmkHdr;
	SDF_HEADER		sdfHdr;
	SDF_TRACK_INFO	trackInfo;
	unsigned char	cyl;
	unsigned char	side;
	unsigned char	inputSides;
	unsigned char	i;
	uint16			idPos;
	uint16			maxTrackData;

	// read DMK file header
	fread(&dmkHdr, sizeof(DMK_HEADER), 1, dmkFile);
	maxTrackData = LittleEndian16(dmkHdr.trackSize) - sizeof(DMK_ID_TABLE);
	dmkHdr.trackSize = maxTrackData + sizeof(DMK_ID_TABLE);

	if (maxTrackData > kDMKTrackDataSize || (dmkHdr.readOnly != 0x00 && dmkHdr.readOnly != 0xFF)) {
		WriteStdOut("Input file is not in correct format." EOL_STR);
	} else {
		// setup DMK context
		memset(&dmk, 0, sizeof(dmk));
		dmk.file = dmkFile;
		dmk.fmDisk = ((dmkHdr.flags & kDMKSingleDensity) != 0);

		// Determine number of sides for output file
		inputSides = (dmkHdr.flags & kDMKSingleSided) ? 1 : 2;
		if (gOutputSides == 0) {
			gOutputSides = inputSides;
		}

		// write SDF file header
		memset(&sdfHdr, 0, sizeof(SDF_HEADER));
		memcpy(sdfHdr.sig, kSDFHeaderSignature, 4);
		sdfHdr.cylinders = dmkHdr.cylinders;
		sdfHdr.sides = gOutputSides;
		sdfHdr.readOnly = dmkHdr.readOnly;
		fwrite(&sdfHdr, sizeof(SDF_HEADER), 1, sdfFile);

		if (maxTrackData > kMFMTrackSize) {
			maxTrackData = kMFMTrackSize;
		}

		StatusOut("    Sides: %d" EOL_STR, inputSides);
		StatusOut("Cylinders: %d" EOL_STR, sdfHdr.cylinders);
		StatusOut(" Expanded: %s" EOL_STR, ((dmk.fmDisk) ? "NO" : "YES"));
		StatusOut("Read-Only: %s" EOL_STR EOL_STR, ((sdfHdr.readOnly) ? "YES" : "NO"));

		// loop through all tracks
		for (cyl = 0; cyl < sdfHdr.cylinders; cyl++) {
			for (side = 0; side < gOutputSides; side++) {

				// skip tracks on 2nd side if the -s1 option was provided
				if (side && (gOutputSides == 1)) continue;

				// initialize data for an un-formatted track
				memset(dmk.rawData, 0xEB, kMFMTrackSize);
				memset(dmk.idTable, 0x00, sizeof(DMK_ID_TABLE));

				// initialize track header for the SDF image
				memset(&trackInfo, 0, sizeof(SDF_TRACK_INFO));

				if (side == 0 || inputSides == 2) {
					// read track's ID table from DMK file
					if (fseek(dmkFile, (cyl * inputSides + side) * dmkHdr.trackSize + sizeof(DMK_HEADER), SEEK_SET) == 0) {
						fread(dmk.idTable, sizeof(DMK_ID_TABLE), 1, dmkFile);
					} else {
						WriteStdOut("Input file size is less than it should be." EOL_STR);
						break;
					}

					// read track's raw data
					fread(dmk.rawData, 1, maxTrackData, dmkFile);

					// adjust ID table entries so they don't include the size of the table itself
					for (i = 0; i < kDMKMaxSectors; i++) {
						idPos = LittleEndian16(dmk.idTable[i]) & 0x3fff;

						if (idPos >= sizeof(DMK_ID_TABLE) && idPos < dmkHdr.trackSize) {
							idPos -= sizeof(DMK_ID_TABLE);
						} else {
							idPos = 0;
						}

						// single density images will be expanded
						if (dmk.fmDisk) {
							idPos <<= 1;
						}

						dmk.idTable[i] = idPos | (LittleEndian16(dmk.idTable[i]) & 0x8000);
					}

					// expand single density data to fill the double density track buffer
					if (dmk.fmDisk) {
						ExpandFMTrack(&dmk);
					}
				}

				// extract each sector from the DMK track to build the SDF track header
				for (i = 0; i < kSDFMaxSectors; i++) {
					if (!ExtractDMKSector(&dmk, side, cyl, i, &trackInfo.table[i])) break;
					trackInfo.count += 1;
				}

				if (i == kSDFMaxSectors && dmk.idTable[i]) {
					StatusOut("Too many sectors on one track (head=%d, cyl=%d)." EOL_STR, side, cyl);
				}

				// write the track header and raw track data to the SDF file
				fwrite(&trackInfo, 1, sizeof(SDF_TRACK_INFO), sdfFile);
				fwrite(dmk.rawData, 1, kMFMTrackSize, sdfFile);

				// extra padding for the track in an SDF file
				memset(dmk.rawData, 0xEE, kSDFTrackDataSize - kMFMTrackSize);
				fwrite(dmk.rawData, 1, kSDFTrackDataSize - kMFMTrackSize, sdfFile);
			}
		}
	}
}


/*======================================================================================================*/

int main (int argc, char **argv)
{
	FILE		*dmkFile;
	FILE		*sdfFile;
	size_t		optLen;
	int			argNum;
	char		opt;
	char		isLast;
	char		inFileName [200];
	char		outFileName [200];
	char		message [200];
	char		argStr [200];

 	if (argc < 2) {
 		Usage();
 	}

	inFileName[0] = 0;
	outFileName[0] = 0;
	gQuietOpt = 0;

	for (argNum = 1; argNum < argc; argNum++) {
		strncpy(argStr, argv[argNum], sizeof(argStr) - 1);
		argStr[sizeof(argStr)-1] = 0;
		isLast = (argNum == argc - 1);
		optLen = strlen(argStr) - 1;

		if (argStr[0] == '-') {
			opt = argStr[1];
			if (opt >= 'A' && opt <= 'Z') {
				opt -= ('a' - 'A');
			}

			if (optLen == 1) {
				switch (opt) {
					case 'h':
						Usage();
						break;

					case 'o':
						if (outFileName[0]) {
							ErrorAbort("Cannot specify more than one output file.");
						} else if (!isLast) {
							strcpy(outFileName, argv[++argNum]);
						} else {
							Usage();
						}
						break;

					case 'q':
						gQuietOpt = 1;
						break;

					default:
						sprintf(message, "Invalid option: %s", argStr);
						ErrorAbort(message);
				}
			} else if (optLen == 2) {
				switch (opt) {
					case 's':
						gOutputSides = argStr[1] - '0';
						if (gOutputSides && gOutputSides <= 2) break;
						/*** Fall Through ***/

					default:
						sprintf(message, "Invalid option: %s", argStr);
						ErrorAbort(message);
				}
			} else {
				sprintf(message, "Invalid option: %s", argStr);
				ErrorAbort(message);
			}
		} else if (inFileName[0]) {
			ErrorAbort("Cannot specify more than one input file.");
		} else {
			strcpy(inFileName, argStr);
		}
	}

	if (inFileName[0]) {
		dmkFile = fopen(inFileName, "rb");

		if (dmkFile) {
			if (outFileName[0] == 0) {
				char *dot;
				strcpy(outFileName, inFileName);
				if ((dot = strrchr(outFileName, '.')) != NULL) {
					*dot = '\0';
				}
				strcat(outFileName, ".sdf");
			}

			sdfFile = fopen(outFileName, "wb");

			if (sdfFile) {
				StatusOut("Converting %s to %s..." EOL_STR, inFileName, outFileName);
				dmk2sdf(dmkFile, sdfFile);
				fclose(sdfFile);
			} else {
				sprintf(message, "Unable to open output file: %s", outFileName);
				ErrorAbort(message);
			}

			fclose(dmkFile);
		} else {
			sprintf(message, "Unable to open file: %s", inFileName);
			ErrorAbort(message);
		}
	} else {
		ErrorAbort("No input file specified.");
	}

    #if MS_WIN
		ExitProcess(EXIT_SUCCESS);
	#endif

	return (0);
}

