#include <limits.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <time.h>

typedef unsigned char uchar, uint8;
typedef unsigned short uint16;
#ifndef __SMALLER_C__
#if UINT_MAX >= 0xFFFFFFFF
typedef unsigned uint32;
#else
typedef unsigned long uint32;
#endif
#else
typedef unsigned long uint32;
#endif
typedef unsigned uint;
typedef unsigned long ulong;

#ifndef __SMALLER_C__
#define C_ASSERT(expr) extern char CAssertExtern[(expr)?1:-1]
C_ASSERT(CHAR_BIT == 8);
C_ASSERT(sizeof(uint16) == 2);
C_ASSERT(sizeof(uint32) == 4);
#endif

#pragma pack (push, 1)

typedef struct tFATBPB1
{
  uint16                BytesPerSector;
  uint8                 SectorsPerCluster;
  uint16                ReservedSectorsCount;
  uint8                 NumberOfFATs;
  uint16                RootEntriesCount;
  uint16                TotalSectorsCount16;
  uint8                 MediaType;
  uint16                SectorsPerFAT1x;
  uint16                SectorsPerTrack;
  uint16                HeadsPerCylinder;
  uint32                HiddenSectorsCount;
  uint32                TotalSectorsCount32;
} tFATBPB1;

typedef union tFATBPB2
{
  struct
  {
  uint8                 DriveNumber;
  uint8                 reserved1;
  uint8                 ExtendedBootSignature;
  uint32                VolumeSerialNumber;
  char                  VolumeLabel[11];
  char                  FileSystemName[8];
  uchar                 aBootCode1x[0x1C];
  } FAT1x;
  struct
  {
  uint32                SectorsPerFAT32;
  uint16                ExtendedFlags;
  uint16                FSVersion;
  uint32                RootDirectoryClusterNo;
  uint16                FSInfoSectorNo;
  uint16                BackupBootSectorNo;
  uint8                 reserved[12];
  uint8                 DriveNumber;
  uint8                 reserved1;
  uint8                 ExtendedBootSignature;
  uint32                VolumeSerialNumber;
  char                  VolumeLabel[11];
  char                  FileSystemName[8];
  } FAT32;
} tFATBPB2;

typedef struct tFATBPB
{
  tFATBPB1              BPB1;
  tFATBPB2              BPB2;
} tFATBPB;

typedef struct tFATBootSector
{
  uchar                 aJump[3];
  char                  OEMName[8];
  tFATBPB               BPB;
  uchar                 aBootCode32[0x1A4];
  uint16                Signature0xAA55;
} tFATBootSector;

typedef enum tFATDirEntryAttribute
{
  dea_READ_ONLY         = 0x01,
  dea_HIDDEN            = 0x02,
  dea_SYSTEM            = 0x04,
  dea_VOLUME_ID         = 0x08,
  dea_DIRECTORY         = 0x10,
  dea_ARCHIVE           = 0x20,
  dea_LONG_NAME         = dea_READ_ONLY|dea_HIDDEN|dea_SYSTEM|dea_VOLUME_ID
} tFATDirEntryAttribute;

typedef struct tFATDirectoryEntry
{
  char                  Name[8];
  char                  Extension[3];
  uint8                 Attribute;
  uint8                 WinNTreserved;
  uint8                 CreationTimeSecTenths;
  uint16                CreationTime2Secs;
  uint16                CreationDate;
  uint16                LastAccessDate;
  uint16                FirstClusterHiWord;
  uint16                LastWriteTime;
  uint16                LastWriteDate;
  uint16                FirstClusterLoWord;
  uint32                Size;
} tFATDirectoryEntry;

#define DELETED_DIR_ENTRY_MARKER        0xE5

#pragma pack (pop)

#ifndef __SMALLER_C_32__
C_ASSERT(sizeof(tFATBootSector) == 512);
C_ASSERT(sizeof(tFATDirectoryEntry) == 32);
#endif

#define FBUF_SIZE 1024

char* BootSectName;

char* OutName = "floppy.img";

int UniqueSerial;

FILE* fout;

tFATBootSector BootSector;
uint32 Fat1Lba;
uint32 SectorsPerFat;
uint32 Fats;
uint32 RootDirLba;
uint32 DirEntriesPerSector;
uint32 RootDirEntries;
uint32 RootDirSectors;
uint32 Cluster2Lba;
uint32 SectorsPerCluster;
uint32 ClusterSize;
uint32 DataSectors;
uint32 Clusters;

uint8 FatSector[512];
uint32 Cluster;

tFATDirectoryEntry RootDirSector[512 / sizeof(tFATDirectoryEntry)];
uint32 RootDirEntryIdx;

void error(char* format, ...)
{
#ifndef __SMALLER_C__
  va_list vl;
  va_start(vl, format);
#else
  void* vl = &format + 1;
#endif

  if (fout)
    fclose(fout);
  remove(OutName);

  puts("");

  vprintf(format, vl);

#ifndef __SMALLER_C__
  va_end(vl);
#endif

  exit(EXIT_FAILURE);
}

FILE* Fopen(const char* filename, const char* mode)
{
  FILE* stream = fopen(filename, mode);
  if (!stream)
    error("Can't open/create file \"%s\"\n", filename);
  return stream;
}

void Fclose(FILE* stream)
{
  if (fclose(stream))
    error("Can't close a file\n");
}

void Fseek(FILE* stream, long offset, int whence)
{
  int r = fseek(stream, offset, whence);
  if (r)
    error("Can't seek a file\n");
}

void Fread(void* ptr, size_t size, FILE* stream)
{
  size_t r = fread(ptr, 1, size, stream);
  if (r != size)
    error("Can't read a file\n");
}

void Fwrite(const void* ptr, size_t size, FILE* stream)
{
  size_t r = fwrite(ptr, 1, size, stream);
  if (r != size)
    error("Can't write a file\n");
}

void FillWithByte(unsigned char byte, unsigned long size, FILE* stream)
{
  static unsigned char buf[FBUF_SIZE];
  memset(buf, byte, FBUF_SIZE);
  while (size)
  {
    unsigned long csz = size;
    if (csz > FBUF_SIZE)
      csz = FBUF_SIZE;
    Fwrite(buf, csz, stream);
    size -= csz;
  }
}

// Determines binary file size portably (when stat()/fstat() aren't available)
long fsize(FILE* binaryStream)
{
  long ofs, ofs2;
  int result;

  if (fseek(binaryStream, 0, SEEK_SET) != 0 ||
      fgetc(binaryStream) == EOF)
    return 0;

  ofs = 1;

  while ((result = fseek(binaryStream, ofs, SEEK_SET)) == 0 &&
         (result = (fgetc(binaryStream) == EOF)) == 0 &&
         ofs <= LONG_MAX / 4 + 1)
    ofs *= 2;

  // If the last seek failed, back up to the last successfully seekable offset
  if (result != 0)
    ofs /= 2;

  for (ofs2 = ofs / 2; ofs2 != 0; ofs2 /= 2)
    if (fseek(binaryStream, ofs + ofs2, SEEK_SET) == 0 &&
        fgetc(binaryStream) != EOF)
      ofs += ofs2;

  // Return -1 for files longer than LONG_MAX
  if (ofs == LONG_MAX)
    return -1;

  return ofs + 1;
}

void FlushFatSector(void)
{
  uint32 ofs = (Cluster * 3 / 2) & 511;
  uint32 i;

  if (ofs == 0 && (Cluster & 1) == 0)
    return;

  for (i = 0; i < Fats; i++)
  {
    uint32 ofs = Fat1Lba + i * SectorsPerFat;
    ofs += (Cluster * 3 / 2) / 512;
    Fseek(fout, ofs * 512, SEEK_SET);
    Fwrite(FatSector, sizeof FatSector, fout);
  }

  memset(FatSector, 0, sizeof FatSector);
}

void ChainCluster(uint32 nextCluster)
{
  uint32 ofs = (Cluster * 3 / 2) & 511;

  if (Cluster & 1)
    FatSector[ofs] |= nextCluster << 4;
  else
    FatSector[ofs] = nextCluster;

  if (ofs == 511)
    FlushFatSector();

  ofs = (ofs + 1) & 511;

  if (Cluster & 1)
    FatSector[ofs] = nextCluster >> 4;
  else
    FatSector[ofs] = (nextCluster >> 8) & 0xF;

  if (ofs == 511 && (Cluster & 1))
    FlushFatSector();

  Cluster++;
}

void FlushRootDirSector(void)
{
  uint32 ofs;

  if (RootDirEntryIdx % DirEntriesPerSector == 0)
    return;

  ofs = RootDirLba + RootDirEntryIdx / DirEntriesPerSector;

  Fseek(fout, ofs * 512, SEEK_SET);
  Fwrite(RootDirSector, sizeof RootDirSector, fout);
}

void AddRootDirEntry(tFATDirectoryEntry* de)
{
  RootDirSector[RootDirEntryIdx % DirEntriesPerSector] = *de;

  if ((RootDirEntryIdx + 1) % DirEntriesPerSector == 0)
    FlushRootDirSector();

  RootDirEntryIdx++;
}

void Init(void)
{
  if (BootSectName)
  {
    FILE* fsect = Fopen(BootSectName, "rb");
    Fread(&BootSector, sizeof BootSector, fsect);
    Fclose(fsect);
  }
  else
  {
    memcpy(BootSector.OEMName, "BootProg", 8);
    memcpy(BootSector.BPB.BPB2.FAT1x.VolumeLabel, "NO NAME    ", 11);
    memcpy(BootSector.BPB.BPB2.FAT1x.FileSystemName, "FAT12   ", 8);
    BootSector.aJump[0] = 0xEB; // jmp short $+0x3E
    BootSector.aJump[1] = 0x3C;
    BootSector.aJump[2] = 0x90; // nop
    // TBD??? replace the below with code to print an error message like "Not a system/bootable disk"?
    BootSector.BPB.BPB2.FAT1x.aBootCode1x[0] = 0xF4; // hlt
    BootSector.BPB.BPB2.FAT1x.aBootCode1x[1] = 0xEB; // jmp short $-1
    BootSector.BPB.BPB2.FAT1x.aBootCode1x[2] = 0xFD;
  }

  fout = Fopen(OutName, "wb");

  BootSector.BPB.BPB1.BytesPerSector = 512; // note, we're normally assuming 512 bytes per sector everywhere
  BootSector.BPB.BPB1.SectorsPerCluster = 1;
  BootSector.BPB.BPB1.ReservedSectorsCount = 1; // includes the boot sector
  BootSector.BPB.BPB1.NumberOfFATs = 2;
  BootSector.BPB.BPB1.RootEntriesCount = 224; // must be a multiple of 16 (16 32-byte entries in 512-byte sector)
  BootSector.BPB.BPB1.TotalSectorsCount16 = 2880;
  BootSector.BPB.BPB1.MediaType = 0xF0;
  BootSector.BPB.BPB1.SectorsPerFAT1x = 9;
  BootSector.BPB.BPB1.SectorsPerTrack = 18;
  BootSector.BPB.BPB1.HeadsPerCylinder = 2;
  BootSector.BPB.BPB1.HiddenSectorsCount = 0;
  BootSector.BPB.BPB1.TotalSectorsCount32 = 0;
  BootSector.BPB.BPB2.FAT1x.DriveNumber = 0;
  BootSector.BPB.BPB2.FAT1x.reserved1 = 0;
  BootSector.BPB.BPB2.FAT1x.ExtendedBootSignature = 0x29;
  BootSector.BPB.BPB2.FAT1x.VolumeSerialNumber = 0x11223344;
  if (UniqueSerial)
    BootSector.BPB.BPB2.FAT1x.VolumeSerialNumber = time(NULL);
  BootSector.Signature0xAA55 = 0xAA55;

  // Write the boot sector
  Fwrite(&BootSector, sizeof BootSector, fout);

  // Zero out the rest of the image
  FillWithByte(0, (BootSector.BPB.BPB1.TotalSectorsCount16 - 1) * 512UL, fout);

  // FAT12's first two entries need special initialization
  ChainCluster(0xF00 | BootSector.BPB.BPB1.MediaType);
  ChainCluster(0xFFF);

  // Helper variables

  Fat1Lba = BootSector.BPB.BPB1.ReservedSectorsCount;
  SectorsPerFat = BootSector.BPB.BPB1.SectorsPerFAT1x;
  Fats = BootSector.BPB.BPB1.NumberOfFATs;

  RootDirLba = Fat1Lba + SectorsPerFat * Fats;
  DirEntriesPerSector = 512 / sizeof(tFATDirectoryEntry);
  RootDirEntries = BootSector.BPB.BPB1.RootEntriesCount;
  RootDirSectors = (RootDirEntries * sizeof(tFATDirectoryEntry) + 511) / 512;

  Cluster2Lba = RootDirLba + RootDirSectors;
  SectorsPerCluster = BootSector.BPB.BPB1.SectorsPerCluster;
  ClusterSize = SectorsPerCluster * 512;
  DataSectors = BootSector.BPB.BPB1.TotalSectorsCount16 -
    BootSector.BPB.BPB1.ReservedSectorsCount - SectorsPerFat * Fats - RootDirSectors;
  Clusters = DataSectors / SectorsPerCluster;
}

void Done(void)
{
  FlushFatSector();
  FlushRootDirSector();
  Fclose(fout);
}

void NameTo8Dot3Name(const char* in, char out[8 + 3])
{
  static const char aInvalid8Dot3NameChars[] = "\"*+,./:;<=>?[\\]|";
  int i, j;
  int namelen = 0, dots = 0, extlen = 0;

  memset(out, ' ', 8 + 3);

  if (*in == '\0' || *in == '.')
    goto lerr;

  for (j = i = 0; in[i]; i++)
  {
    int c = (unsigned char)in[i];
    if (i >= 12) // at most 12 input chars can fit into an 8.3 name
      goto lerr;
    if (i == 0 && c == 0xE5)
    {
      // 0xE5 in the first character of the name is a marker for deleted files,
      // it needs to be translated to 0x05
      c = 0x05;
    }
    else if (c == '.')
    {
      if (dots++) // at most one dot allowed
        goto lerr;
      j = 8; // now writing extension
      continue;
    }
    if (c <= 0x20 || strchr(aInvalid8Dot3NameChars, c) != NULL)
      goto lerr;
    if (dots)
    {
      if (++extlen > 3) // at most 3 chars in extension
        goto lerr;
    }
    else
    {
      if (++namelen > 8) // at most 8 chars in name
        goto lerr;
    }
    if (c >= 'a' && c <= 'z')
      c -= 'a' - 'A';
    out[j++] = c;
  }

  // TBD??? error out on the following reserved names: "COM1"-"COM9", "CON", "LPT1"-"LPT9", "NUL", "PRN"?

  return;

lerr:
  error("Can't convert \"%s\" to an 8.3 DOS name\n", in);
}

void AddFile(char* fname)
{
  char* pslash = strrchr(fname, '/');
  char* pbackslash = strrchr(fname, '\\');
  char* pname;
  char name8_3[8 + 3];
  FILE* f;
  long size;
  tFATDirectoryEntry de;
  uint32 ofs;

  // First, find where the path ends in the file name, if any

  // In DOS/Windows paths can contain either '\\' or '/' as a separator between directories,
  // choose the right-most
  if (pslash && pbackslash)
  {
    if (pslash < pbackslash)
      pslash = pbackslash;
  }
  else if (!pslash)
  {
    pslash = pbackslash;
  }
  // If there's no slash, it could be "c:file"
  if (!pslash && ((*fname >= 'A' && *fname <= 'Z') || (*fname >= 'a' && *fname <= 'z')) && fname[1] == ':')
    pslash = fname + 1;

  pname = pslash ? pslash + 1 : fname;

  // Convert the name to 8.3
  NameTo8Dot3Name(pname, name8_3);

  // TBD!!! error out on duplicate files/names

  // Copy the file

  f = Fopen(fname, "rb");

  // Prepare the directory entry
  memset(&de, 0, sizeof de);
  memcpy(de.Name, name8_3, 8 + 3);
  de.Attribute = dea_ARCHIVE;
  de.Size = size = fsize(f);
  if (RootDirEntryIdx >= RootDirEntries ||
      size < 0 || (unsigned long)size > Clusters * ClusterSize)
    error("No space for file \"%s\"", fname);
  if (size)
  {
    de.FirstClusterLoWord = Cluster;
    de.FirstClusterHiWord = Cluster >> 16;
  }
  // TBD??? set file date/time to now?
  de.LastWriteDate = ((1990 - 1980) << 9) | (1 << 5) | 1; // 1990/01/01
  de.LastWriteTime = (12 << 11) | (0 << 5) | (0 >> 1); // 12(PM):00:00

  // Seek both files
  Fseek(f, 0, SEEK_SET);
  ofs = Cluster2Lba + (Cluster - 2) * SectorsPerCluster;
  Fseek(fout, ofs * 512, SEEK_SET);

  // Copy data sectors
  while (size)
  {
    uint8 sector[512];
    long sz = (size > 512) ? 512 : size;

    memset(sector, 0, 512); // pad with zeroes the last partial sector
    Fread(sector, sz, f);

    Fwrite(sector, 512, fout);

    size -= sz;
  }

  // Allocate and chain clusters in the FAT
  size = de.Size;
  while (size)
  {
    if (size > (long)ClusterSize)
    {
      // There's at least one more cluster in the chain
      ChainCluster(Cluster + 1);
      size -= ClusterSize;
    }
    else
    {
      // No more clusters, this is the last one in the chain
      ChainCluster(0xFF8);
      size = 0;
    }
    Clusters--;
  }

  // Write the directory entry
  AddRootDirEntry(&de);

  Fclose(f);
}

int main(int argc, char* argv[])
{
  int i;

  for (i = 1; i < argc; i++)
  {
    if (!strcmp(argv[i], "-o"))
    {
      if (i + 1 < argc)
      {
        argv[i++] = NULL;
        OutName = argv[i];
        argv[i] = NULL;
        continue;
      }
    }
    else if (!strcmp(argv[i], "-bs"))
    {
      if (i + 1 < argc)
      {
        argv[i++] = NULL;
        BootSectName = argv[i];
        argv[i] = NULL;
        continue;
      }
    }
    else if (!strcmp(argv[i], "-us"))
    {
      UniqueSerial = 1;
      argv[i++] = NULL;
      continue;
    }

    if (argv[i][0] == '-')
      error("Invalid or unsupported command line option\n");
  }

  Init();

  for (i = 1; i < argc; i++)
    if (argv[i])
      AddFile(argv[i]);

  Done();

  return 0;
}
