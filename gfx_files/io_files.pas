{***********************************************************************
Unit IO_FILES.PAS v 1.05 0799

    Delphi version : Delphi 3 / 4

    You may use this sourcecode for your freewareproducts.
    You may modify this sourcecode for your own use.
    You may recompile this sourcecode for your own use.

    Disclaimer of warranty: "This software is supplied as is. The author
    disclaims all warranties, expressed or implied, including, without
    limitation, the warranties of merchantability and of fitness for any
    purpose. The author assumes no liability for damages, direct or
    consequential, which may result from the use of this software."

    All brand and product names are marks or registered marks of their
    respective companies.

    Please report bugs to:
    Andreas Moser  AMoser@amoser.de

  Overview:

  IO_files provides some functions and some classes for filehandling.

  Classes:

  TFileRecList: a class derived from TList that allows easy
                        access to the properties of every file.

  The mainfunctions are:

    procedure ReadFiles(Directory:String;FileMask:String;FileType:TFileType;SortBy:TSortOrder);
    -> Read all files specified in FileMask and FileType  from the given directory into
       the TFileRecList.

    The following functions returns the value of the file specified in 'Index':

      function GetFileName(Index:Integer):String;
      function GetFileSize(Index:Integer):Integer;
      function GetFileDate(Index:Integer):TDateTime;
      function GetFileAttr(Index:Integer):Integer;
      function GetW32FileAttributes(Index:Integer):DWORD;
      function GetW32FileName(Index:Integer):string;
      function GetW32AlternateFileName(Index:Integer):string;
      function GetW32CreationTime(Index:Integer):TDateTime;
      function GetW32LastAccessTime(Index:Integer):TDateTime;
      function GetW32LastWriteTime(Index:Integer):TDateTime;

    To get the Index of a file knowing the filename just use:
      function GetIndexOfFileName(Name:String):Integer;

    To clear the FileRecList manually use:
      procedure ClearFileRecList;

************************************************************************}

unit io_files;

interface

uses forms,windows,classes,filectrl,sysutils,Dialogs,Controls,ComCtrls;

const msgAskReadOnly='%s is read-only, try to delete ?';
msgAskForMoveReadOnly='%s is read-only, try to move ?';
msgAskForOverwrite='%s already exists, try to overwrite ?';
msgAskForRenameReadOnly='%s is read-only, try to rename ?';
msgAskForDelete='Delete %s ?';

type TProgressCallBack=function(Min,Max,Position:Integer):Boolean;


//** TFileRecList
type TSortOrder=(srtName,srtSize,srtDate,srtNone,srtExt);

type TExtInfo=class(TObject)
public
    Archive:Boolean;
    Reserved:string;
    ID:LongInt;
end;
    pExtInfo=^TExtInfo;

type TExtSearchRec=record
     SRec:TSearchRec;
     Path:String[255];
     IsArchive:Boolean;
     ID:LongInt;
     end;

     pSearchRecord=^TExtSearchRec;
     EInvalidDest = class(EStreamError);
     EFCantMove = class(EStreamError);

     TSyncMode=(syBoth,sySrcToDst,syDstToSrc);

const
     Attributes: array[TFileAttr] of Word = (faReadOnly, faHidden, faSysFile,
                                             faVolumeID, faDirectory, faArchive, 0);
     SInvalidDest = 'Destination %s does not exist';
     SFCantMove = 'Cannot move file %s';

//-------------------------------------
//
// TFileRecList class interface
// A TList Class for keeping FileInfo records
//-------------------------------------
type
TFileRecList=class(TList)
  public
  function GetFileName(Index:Integer):String;
  function GetFilePath(Index:Integer):String;
  function GetCompleteFileName(Index:Integer):String;
  function GetFileNameWOPATH(Index:Integer):String;
  function GetFileSize(Index:Integer):Integer;
  function GetFileDate(Index:Integer):TDateTime;
  function GetFileAttr(Index:Integer):Integer;

  function GetW32FileAttributes(Index:Integer):Integer;
  function GetW32FileName(Index:Integer):string;
  function GetW32AlternateFileName(Index:Integer):string;
  function GetW32CreationTime(Index:Integer):TDateTime;
  function GetW32LastAccessTime(Index:Integer):TDateTime;
  function GetW32LastWriteTime(Index:Integer):TDateTime;

  function GetSearchRec(Index:Integer):TExtSearchRec;

  procedure ReadFiles(Directory:String;FileMask:String;FileType:TFileType;SortBy:TSortOrder);
  Procedure ScanTreeForFiles(Const path: String; Const mask: String;SortBy:TSortOrder);
  function  AddExtSearchRec(const Path,FileName:String):Boolean;
  function  AddExtFakeRec(const Path,FileName:String;ArchiveFile:Boolean):Boolean;
  function  GetIndexOfFileName(Name:String):Integer;
  function  GetIndexOfFileNameWOPATH(Name:String):Integer;
  function  GetIndexOfFileNameCI(Name:String):Integer;
  function  GetIndexOfCompleteFileName(Name:String):Integer;
  procedure ClearFileRecList;
end;

//-------------------------------------
//
// TIntegerList class interface
// A TList class like TStringList
//-------------------------------------
type TIntegerList =class(TList)
  public
   function  AddInteger(Value:DWORD):Integer;
   procedure DeleteInteger(Index:Integer);
   function  FindInteger(Value:DWord):Integer;
   procedure ClearIntegerList;
end;

function SortName(Item1, Item2: Pointer): Integer;
function SortDate(Item1, Item2: Pointer): Integer;
function SortSize(Item1, Item2: Pointer): Integer;
function SortExt(Item1, Item2: Pointer): Integer;


//-------------------------------------
//
// Class independent function interface
//
//-------------------------------------
//--------------------
//
// Path functions
//
//--------------------
procedure ScanTreeForFiles(ResultList:TStringList;Const path: String; Const mask: String);
procedure ClearStringListWObjects(List:TSTringList);
procedure ScanTreeForPaths(ResultList:TStringList;Const path: String);
procedure SyncTwoDirectories(DirectoryOne,DirectoryTwo:String;FileMask:String;FileType:TFileType;SyncMode:TSyncMode;Callback:TProgressCallback);
procedure SyncTwoTrees(BaseDirOne,BaseDirTwo:String;FileMask:String;FileType:TFileType;SyncMode:TSyncMode;Callback:TProgressCallback);
function  SwitchTwoFiles(FileNameOne,FileNameTwo:String):Boolean;
function  RenameInsertFile(Directory,FileNameOne,FileNameTwo:string;CharToBeAdded:Char):String;
function  CutDirectory(Directory:String;MinLength:Integer):String;
function  CompareDirectory(Path1,Path2:String):Boolean;
function  CompareDirectorySub(Path,SubPath:String):Boolean;
function  ExtFileDirExists (Path:string):Boolean;
function  ExtAddBackSlash(const Path:String):String;
function  ExtRemoveBackSlash(const Directory:String):String;
function  GetFDrive(Path:string):string;
function  GetFPath(Path:string):string;
function  GetLastPathPart(Path:string):string;
function  DeleteLastPathPart(Path:string):string;
function  IsDriveRemovable(Path:String;var ID:String; var Name:String):Boolean;
function  GetVolumeName(Drive:string):string;
function  GetVolumeIDName(Drive:string;var VolumeName:String; var Volume_id:Integer):Boolean;
function  GetVolumeID(Drive:string):Integer;

//--------------------
//
// File functions
//
//--------------------
function ExtCopyFile(const Source, Destination: TFileName;AskForOverwrite:Boolean):Boolean;
function ExtMoveFile(const Source, Destination: TFileName;AskForOverwrite:Boolean):Boolean;
function ExtRenameFile(const OldName, NewName: TFileName):Boolean;
function ExtDeleteFile(const FileName:TFileName;var DeleteMode:Integer;IntoRecycleBin:Boolean):Boolean;
function ExtGetFileSize(const FileName: string): LongInt;
function ExtFileDateTime(const FileName: string): TDateTime;
function ExtHasAttr(const FileName: string; Attr: Word): Boolean;
function ExtExecuteFile(const FileName, Params, DefaultDir: string;ShowCmd: Integer): THandle;
function ExtCreateDir(const Path:String):Boolean;
function ExtGetTempFileName(Prefix,AlternatePath:String):string;
function ExtGetTempPath:string;

procedure GetDriveInfo;
function IsVolumeAvailable(Volume_Name:String):Boolean;
procedure RetrieveDriveInfo(Drive:Char);

//--------------------
//
// CRC functions
//
//--------------------
procedure MakeCRCTable;
function CRC32(crc:longint;buffer:PByteArray;length:Integer):DWord;
function CRC32File(FileName:String):Dword;

//--------------------
//
// Misc. functions
//
//--------------------

function ExtFillUpWithZero(Number:Integer;Digits:Integer):String;
function DeleteToRecycleBin(FileName:String):Boolean;

type
  dtable_entry = record
    Drive: String;
    Volume_ID: Integer;
    Volume_Name: String;
  end;

var amOvrAll:Integer;
    crc32tablecreated:Boolean;
    crc_table : Array[0..255] of dword;

    drive_table : Array [0..30] of dtable_entry;
    drive_table_count : Integer;


implementation
uses ShellAPI,io_const;

//-------------------------------------
//
// Global sort functions for TFileRecList
//
//-------------------------------------

function SortSize(Item1, Item2: Pointer): Integer;
// -----------------------------------------------------------------------------
//     Purpose    : Sort filereclist by Srec.size
//     Parameter  : Pointer to the two processed items
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
begin
   if pSearchRecord(Item1).SRec.Size < pSearchRecord(Item2).SRec.Size then Result:=-1
   else
   if pSearchRecord(Item1).SRec.Size > pSearchRecord(Item2).SRec.Size then Result:=1
   else Result:=0;
end;

function SortDate(Item1, Item2: Pointer): Integer;
// -----------------------------------------------------------------------------
//     Purpose    : Sort filereclist by Srec.Date
//     Parameter  : Pointer to the two processed items
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
begin
   if pSearchRecord(Item1).SRec.Time < pSearchRecord(Item2).SRec.Time then Result:=-1
   else
   if pSearchRecord(Item1).SRec.Time > pSearchRecord(Item2).SRec.Time then Result:=1
   else Result:=0;
end;

function SortName(Item1, Item2: Pointer): Integer;
// -----------------------------------------------------------------------------
//     Purpose    : Sort filereclist by Srec.Name
//     Parameter  : Pointer to the two processed items
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
begin
   if UpperCase(pSearchRecord(Item1).SRec.Name) < UpperCase(pSearchRecord(Item2).SRec.Name) then Result:=-1
   else
   if UpperCase(pSearchRecord(Item1).SRec.Name) > UpperCase(pSearchRecord(Item2).SRec.Name) then Result:=1
   else Result:=0;
end;

function SortExt(Item1, Item2: Pointer): Integer;
// -----------------------------------------------------------------------------
//     Purpose    : Sort filereclist by Extension
//     Parameter  : Pointer to the two processed items
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
begin
   if UpperCase(ExtractFileExt(pSearchRecord(Item1).SRec.Name)) < UpperCase(ExtractFileExt(pSearchRecord(Item2).SRec.Name)) then Result:=-1
   else
   if UpperCase(ExtractFileExt(pSearchRecord(Item1).SRec.Name)) > UpperCase(ExtractFileExt(pSearchRecord(Item2).SRec.Name)) then Result:=1
   else Result:=0;
end;

//-------------------------------------
//
// TFileRecList class implementation
//
//-------------------------------------

function TFileRecList.GetFileName(Index:Integer):String;
// -----------------------------------------------------------------------------
//     Purpose    : Returns the Filename of SearchRecord
//     Parameter  : Index of record
//     Result     : The filename, '' if not found
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
begin
 if (Index>-1) and (Index<Count) then Result:=pSearchRecord(Items[Index])^.SRec.Name
 else Result:='';
end;


function TFileRecList.GetFilePath(Index:Integer):String;
// -----------------------------------------------------------------------------
//     Purpose    : Returns the Path of SearchRecord
//     Parameter  : Index of record
//     Result     : The filename, '' if not found
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
begin
 if (Index>-1) and (Index<Count) then Result:=pSearchRecord(Items[Index])^.Path
 else Result:='';
end;

function TFileRecList.GetCompleteFileName(Index:Integer):String;
// -----------------------------------------------------------------------------
//     Purpose    : Returns the Path of SearchRecord
//     Parameter  : Index of record
//     Result     : The filename, '' if not found
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
begin
 if (Index>-1) and (Index<Count) then Result:=ExtAddBackSlash(pSearchRecord(Items[Index])^.Path)+
  ExtractFileName(pSearchRecord(Items[Index])^.SRec.Name)

 else Result:='';
end;

function TFileRecList.GetFileNameWOPATH(Index:Integer):String;
// -----------------------------------------------------------------------------
//     Purpose    : Returns the Filename without path of SearchRecord
//     Parameter  : Index of record
//     Result     : The filename, '' if not found
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
begin
 if (Index>-1) and (Index<Count) then Result:=ExtractFileName(pSearchRecord(Items[Index])^.SRec.Name)
 else Result:='';
end;

function TFileRecList.GetFileSize(Index:Integer):Integer;
// -----------------------------------------------------------------------------
//     Purpose    : Returns the Filesize of SearchRecord
//     Parameter  : Index of record
//     Result     : The filesize, -1 if not found
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
begin
 if (Index>-1) and (Index<Count) then Result:=pSearchRecord(Items[Index])^.SRec.Size
 else Result:=-1;
end;

function TFileRecList.GetFileDate(Index:Integer):TDateTime;
// -----------------------------------------------------------------------------
//     Purpose    : Returns the Filedate of SearchRecord
//     Parameter  : Index of record
//     Result     : The filedate, -1 if not found
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
begin
 Result:=-1;
 if (Index>-1) and (Index<Count) then
 if
 pSearchRecord(Items[Index])^.SRec.Time<>0 then
 Result:=FileDateToDateTime(pSearchRecord(Items[Index])^.SRec.Time)
 else Result:=-1;
end;

function TFileRecList.GetFileAttr(Index:Integer):Integer;
// -----------------------------------------------------------------------------
//     Purpose    : Returns the fileattributes of SearchRecord
//     Parameter  : Index of record
//     Result     : The fileattributes, -1 if not found
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
begin
 if (Index>-1) and (Index<Count) then Result:=pSearchRecord(Items[Index])^.SRec.Attr
 else Result:=-1;
end;

function TFileRecList.GetW32FileName(Index:Integer):String;
// -----------------------------------------------------------------------------
//     Purpose    : Returns the W32Filename of SearchRecord
//     Parameter  : Index of record
//     Result     : The filename, '' if not found
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
begin
 if (Index>-1) and (Index<Count) then Result:=pSearchRecord(Items[Index])^.SRec.FindData.cFileName
 else Result:='';
end;

function TFileRecList.GetW32AlternateFileName(Index:Integer):String;
// -----------------------------------------------------------------------------
//     Purpose    : Returns the W32Filename of SearchRecord
//     Parameter  : Index of record
//     Result     : The filename, '' if not found
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
begin
 if (Index>-1) and (Index<Count) then Result:=pSearchRecord(Items[Index])^.SRec.FindData.cAlternateFileName
 else Result:='';
end;

function TFileRecList.GetW32FileAttributes(Index:Integer):Integer;
// -----------------------------------------------------------------------------
//     Purpose    : Returns the W32FileAttributes of SearchRecord
//     Parameter  : Index of record
//     Result     : The fileattributes, -1 if not found
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
begin
 if (Index>-1) and (Index<Count) then Result:=pSearchRecord(Items[Index])^.SRec.FindData.dwFileAttributes
 else Result:=-1;
end;

function TFileRecList.GetW32CreationTime(Index:Integer):TDateTime;
// -----------------------------------------------------------------------------
//     Purpose    : Returns the W32 file creationtime of SearchRecord
//     Parameter  : Index of record
//     Result     : The W32 file creationtime, -1 if not found
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
var t1:TFileTime;
    t2:TSystemTime;
    t3,t4:TDateTime;
begin
 if (Index>-1) and (Index<Count) then begin
 filetimetolocalfiletime(pSearchRecord(Items[Index])^.SRec.FindData.ftCreationTime,t1);
 FileTimeToSystemTime(t1,t2);
 t3:=EncodeDate(t2.wYear,t2.wMonth,t2.wDay);
 t4:=EncodeTime(t2.wHour,t2.wMinute,t2.wSecond,t2.wMilliseconds);
 Result:=t3+t4;
 end
 else Result:=-1;
end;

function TFileRecList.GetW32LastAccessTime(Index:Integer):TDateTime;
// -----------------------------------------------------------------------------
//     Purpose    : Returns the W32 file last access time of SearchRecord
//     Parameter  : Index of record
//     Result     : The W32 file last access time, -1 if not found
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
var t1:TFileTime;
    t2:TSystemTime;
    t3,t4:TDateTime;
begin
 if (Index>-1) and (Index<Count) then begin
 filetimetolocalfiletime(pSearchRecord(Items[Index])^.SRec.FindData.ftLastAccessTime,t1);
 FileTimeToSystemTime(t1,t2);
 t3:=EncodeDate(t2.wYear,t2.wMonth,t2.wDay);
 t4:=EncodeTime(t2.wHour,t2.wMinute,t2.wSecond,t2.wMilliseconds);
 Result:=t3+t4;
 end
 else Result:=-1;
end;

function TFileRecList.GetW32LastWriteTime(Index:Integer):TDateTime;
// -----------------------------------------------------------------------------
//     Purpose    : Returns the W32 file last write time of SearchRecord
//     Parameter  : Index of record
//     Result     : The W32 file last write time, -1 if not found
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
var t1:TFileTime;
    t2:TSystemTime;
    t3,t4:TDateTime;
begin
 if (Index>-1) and (Index<Count) then begin
 filetimetolocalfiletime(pSearchRecord(Items[Index])^.SRec.FindData.ftLastWriteTime,t1);
 FileTimeToSystemTime(t1,t2);
 t3:=EncodeDate(t2.wYear,t2.wMonth,t2.wDay);
 t4:=EncodeTime(t2.wHour,t2.wMinute,t2.wSecond,t2.wMilliseconds);
 Result:=t3+t4;
 end
 else Result:=-1;
end;

function TFileRecList.GetSearchRec(Index:Integer):TExtSearchRec;
// -----------------------------------------------------------------------------
//     Purpose    : Returns a SearchRecord
//     Parameter  : Index of record
//     Result     : SearchRecord
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
begin
  result:=pSearchRecord(Items[Index])^;
end;

procedure TFileRecList.ReadFiles(Directory:String;FileMask:String;FileType:TFileType;SortBy:TSortOrder);
// -----------------------------------------------------------------------------
//     Purpose    : Read the files of a directory to SearchRecList
//     Parameter  : Directory: the directory to be processed
//                  FileMask : the FileMask (e.g. '*.jpg')
//                  FileType : Mask file attributes
//                  SortBy   : SortOrder (srtName,srtDate,srtSize)
//     Result     : -
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
var
  AttrIndex: TFileAttr;
  FileExt: string;
  MaskPtr: PChar;
  Ptr: PChar;
  AttrWord: Word;
  FileInfo: TSearchRec;
  pFileInfo:pSearchRecord;

begin
  if DirectoryExists(Directory) then
  begin
  AttrWord := DDL_READWRITE;

    for AttrIndex := ftReadOnly to ftArchive do
      if AttrIndex in FileType then
        AttrWord := AttrWord or Attributes[AttrIndex];

    ChDir(Directory);
    ClearFileRecList;

    try
      MaskPtr := PChar(FileMask);
      while MaskPtr <> nil do
      begin
        Ptr := StrScan (MaskPtr, ';');
        if Ptr <> nil then
          Ptr^ := #0;
        if FindFirst(MaskPtr, AttrWord, FileInfo) = 0 then
        begin
          repeat
            if (ftNormal in FileType) or (FileInfo.Attr and AttrWord <> 0) then
              begin
              New(pFileInfo);
              pFileInfo^.SRec:=FIleInfo;
              pFileInfo^.IsArchive:=False;
              pFileInfo^.ID:=0;
              pFileInfo^.Path:=Directory;
              Add(pFileInfo);

              end
              else
              begin
                FileExt := AnsiLowerCase(ExtractFileExt(FileInfo.Name));
              end;
          until FindNext(FileInfo) <> 0;
          FindClose(FileInfo);
        end;
        if Ptr <> nil then
        begin
          Ptr^ := ';';
          Inc (Ptr);
        end;
        MaskPtr := Ptr;
      end;
        case SortBy of
          srtName:Sort(SortName);
          srtSize:Sort(SortSize);
          srtDate:Sort(SortDate);
          srtExt:begin
                   Sort(SortName);
                   Sort(SortExt);
                 end;
        end;
    finally
    end;
  end;
end;

function TFileRecList.GetIndexOfFileName(Name:String):Integer;
// -----------------------------------------------------------------------------
//     Purpose    : Get the index of <Name> (case sensitive!)
//     Parameter  : Name : Filename
//     Result     : Index, -1 if not found
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
var j:Integer;
begin
   result:=-1;
   for j:=0 to Count-1 do
   if pSearchRecord(Items[j])^.SRec.Name=Name then begin result:=j;break;end;
end;

function TFileRecList.GetIndexOfFileNameCI(Name:String):Integer;
// -----------------------------------------------------------------------------
//     Purpose    : Get the index of <Name> (case insensitive!)
//     Parameter  : name : FileName
//     Result     : Index, -1 if not found
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
var j:Integer;
begin
   result:=-1;
   for j:=0 to Count-1 do
   if LowerCase(pSearchRecord(Items[j])^.SRec.Name)=LowerCase(Name) then begin result:=j;break;end;
end;

function TFileRecList.GetIndexOfFileNameWOPATH(Name:String):Integer;
// -----------------------------------------------------------------------------
//     Purpose    : Get the index of <Name> (case sensitive!)
//                  (this function removes the path from <Name>)
//     Parameter  : Name : Filename
//     Result     : Index, -1 if not found
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
var j:Integer;
begin
   result:=-1;
   for j:=0 to Count-1 do
   if ExtractFileName(pSearchRecord(Items[j])^.SRec.Name)=Name then begin result:=j;break;end;
end;

function TFileRecList.GetIndexOfCompleteFileName(Name:String):Integer;
// -----------------------------------------------------------------------------
//     Purpose    : Get the index of <Path> and <Name> (case sensitive!)
//                  (this function removes the path from <Name>)
//     Parameter  : Name : Filename
//     Result     : Index, -1 if not found
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
var j:Integer;
begin
   result:=-1;
   for j:=0 to Count-1 do
   if
      CompareText(
        ExtractFileName(pSearchRecord(Items[j])^.SRec.Name),
        ExtractFileName(Name)
      )=0
    then
    begin
     if
      CompareText( ExtAddBackSlash(
        pSearchRecord(Items[j])^.Path),
        ExtAddBackSlash(ExtractFileDir(Name))
        ) =0
     then begin
       result:=j;
       break;
     end;
    end;
end;

Procedure TFileRecList.ScanTreeForFiles(Const path: String; Const mask: String;SortBy:TSortOrder);
// -----------------------------------------------------------------------------
//     Purpose    : Read the files of a directory tree to SearchRecList
//     Parameter  : path     : the toplevel-directory to be processed
//                  Mask     : the FileMask (e.g. '*.jpg')
//                  SortBy   : SortOrder (srtName,srtDate,srtSize)
//     Result     : -
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
  Var
    fullpath: String;
    pFileInfo:pSearchRecord;
    MaskPtr: PChar;
    Ptr: PChar;
  Function Recurse( Var path: String; Const mask: String ): Boolean;
    Var
      FileInfo: TSearchRec;
      retval: Integer;
      oldlen: Integer;
    Begin
      Recurse := True;
      oldlen := Length( path );
      MaskPtr := PChar(Mask);
      while MaskPtr <> nil do
      begin
        Ptr := StrScan (MaskPtr, ';');
        if Ptr <> nil then Ptr^ := #0;

        retval := FindFirst( path+maskptr, faAnyFile, FileInfo );
        While retval = 0 Do Begin
          If (FileInfo.Attr and (faDirectory or faVolumeID)) = 0 Then
          begin
            New(pFileInfo);
            pFileInfo^.SRec:=FIleInfo;
            pFileInfo^.IsArchive:=False;
            pFileInfo^.ID:=0;
            pFileInfo^.Path:=path;
            Add(pFileInfo);
          end;
          retval := FindNext( FileInfo );
        End;
        FindClose( FileInfo );
        if Ptr <> nil then
        begin
          Ptr^ := ';';
          Inc (Ptr);
        end;
        MaskPtr := Ptr;
      end;
      If not Result Then Exit;

      retval := FindFirst( path+'*.*', faDirectory, FileInfo );
      While retval = 0 Do Begin
        If (FileInfo.Attr and faDirectory) <> 0 Then
          If (FileInfo.Name <> '.') and (FileInfo.Name <> '..') Then Begin
            path := path + FileInfo.Name + '\';
            If not Recurse( path, mask ) Then Begin
              Result := False;
              Break;
            End;
            System.Delete( path, oldlen+1, 255 );
          End;
        retval := FindNext( FileInfo );
      End;
      FindClose( FileInfo );
    End;
  Begin
    ClearFileRecList;//ResultList.Clear;
    If path = '' Then
      GetDir(0, fullpath)
    Else
      fullpath := path;
    If fullpath[Length(fullpath)] <> '\' Then
      fullpath := fullpath + '\';
    If mask = '' Then
      Recurse( fullpath, '*.*' )
    Else
      Recurse( fullpath, mask );
    case SortBy of
      srtName:Sort(SortName);
      srtSize:Sort(SortSize);
      srtDate:Sort(SortDate);
          srtExt:begin
                   Sort(SortName);
                   Sort(SortExt);
                 end;
    end;
  End;

function TFileRecList.AddExtSearchRec(const Path,FileName:String):Boolean;
// -----------------------------------------------------------------------------
//     Purpose    : Add a file to the SearchRecList
//                  Note: Success only if file exists
//     Parameter  : Path, FileName
//     Result     : True on success, False on failure
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
var Retval:Integer;
    pFileInfo:pSearchRecord;
    FileInfo: TSearchRec;
begin
  Result:=false;
  ChDir(Path);
  retval := FindFirst( ExtAddBackSlash(path)+FileName, faAnyFile, FileInfo );
  if retval = 0 then
  Begin
    If (FileInfo.Attr and (faDirectory or faVolumeID)) = 0 Then
    begin
      New(pFileInfo);
      pFileInfo^.SRec:=FIleInfo;
      pFileInfo^.IsArchive:=False;
      pFileInfo^.ID:=0;
      pFileInfo^.Path:=path;
      Add(pFileInfo);
      Result:=true;
    end;
  end;
  FindClose( FileInfo );
end;

function TFileRecList.AddExtFakeRec(const Path,FileName:String;ArchiveFile:Boolean):Boolean;
// -----------------------------------------------------------------------------
//     Purpose    : Add a fake file to the SearchRecList
//                  This means that the file must not exist
//                  NOTE: The only valid entries of this Record
//                        are Filename and Path
//     Parameter  : Path, FileName
//     Result     : True on success, False on failure
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
var
    pFileInfo:pSearchRecord;
begin
  New(pFileInfo);
  pFileInfo^.SRec.Name:=FileName;
  pFileInfo^.IsArchive:=ArchiveFile;
  pFileInfo^.ID:=0;
  pFileInfo^.SRec.Time:=0;
  StrCopy(pFileInfo^.SRec.FindData.cFileName,PChar(FileName));
  pFileInfo^.Path:=path;
  Add(pFileInfo);
  Result:=true;
end;

procedure TFileRecList.ClearFileRecList;
// -----------------------------------------------------------------------------
//     Purpose    : Clear FileRecList
//     Parameter  : -
//     Result     : -
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
  var i:Integer;
  begin
    if Count>0 then
    for i:=0 to Count-1 do
    try
      if Assigned(Items[i]) then
      begin
        pSearchRecord(Items[i]).SRec.Name:='';
        Dispose(pSearchRecord(Items[i]));
      end;
    finally
        Items[i]:=nil;
    end;
    Clear;
  end;

//-------------------------------------
//
// TIntegerList class implementation
//
//-------------------------------------
function TIntegerList.AddInteger(Value:DWORD):Integer;
// -----------------------------------------------------------------------------
//     Purpose    : Add a Integer ( or DWORD)
//     Parameter  : Value
//     Result     : Index
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
var p:PDWORD;
begin
  New(p);
  p^:=value;
  result:=Add(p);
end;

function TIntegerList.FindInteger(Value:DWord):Integer;
// -----------------------------------------------------------------------------
//     Purpose    : Find the first Value
//     Parameter  : Value
//     Result     : Index, -1 if not found
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
var i:Integer;
begin
 result:=-1;
 for i:=0 to Count-1 do
 if pDWORD(Items[i])^=Value then
 begin
   result:=i;
 end;
end;

procedure TIntegerList.DeleteInteger(Index:Integer);
// -----------------------------------------------------------------------------
//     Purpose    : Delete an entry
//     Parameter  : Index
//     Result     : -
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
begin
  if Assigned(Items[Index]) then
  Dispose(pDWord(Items[Index]));
end;

procedure TIntegerList.ClearIntegerList;
// -----------------------------------------------------------------------------
//
//     Purpose    : Clear the IntegerList
//     Parameter  : -
//     Result     : -
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
  var i:Integer;
  begin
    if Count>0 then
    for i:=0 to Count-1 do
    try
      if Assigned(Items[i]) then
      begin
        Dispose(pDWord(Items[i]));
      end;
    finally
        Items[i]:=nil;
    end;
    Clear;
end;

//-------------------------------------
//
// Global functions implementation
//
//-------------------------------------
//-----------------------
//
// Path functions implementation
//
//-----------------------

Procedure ScanTreeForFiles(ResultList:TStringList;Const path: String; Const mask: String);
// -----------------------------------------------------------------------------
//     Purpose    : Scan tree for files
//     Parameter  : ResultList : StringList where result will be stored
//                  Path       : Top level directory
//                  Mask       : FileMask
//     Result     : -
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
  Var
    fullpath: String;
    MaskPtr: PChar;
    Ptr: PChar;
  Function Recurse( Var path: String; Const mask: String ): Boolean;
    Var
      FileInfo: TSearchRec;
      retval: Integer;
      oldlen: Integer;
    Begin
      Recurse := True;
      oldlen := Length( path );
      MaskPtr := PChar(Mask);
      while MaskPtr <> nil do
      begin
        Ptr := StrScan (MaskPtr, ';');
        if Ptr <> nil then Ptr^ := #0;

        retval := FindFirst( path+maskptr, faAnyFile, FileInfo );
        While retval = 0 Do Begin
          If (FileInfo.Attr and (faDirectory or faVolumeID)) = 0 Then
          ResultList.AddObject(path+FileInfo.Name,TExtInfo.Create);
          retval := FindNext( FileInfo );
        End;
        FindClose( FileInfo );
        if Ptr <> nil then
        begin
          Ptr^ := ';';
          Inc (Ptr);
        end;
        MaskPtr := Ptr;
      end;
      If not Result Then Exit;

      retval := FindFirst( path+'*.*', faDirectory, FileInfo );
      While retval = 0 Do Begin
        If (FileInfo.Attr and faDirectory) <> 0 Then
          If (FileInfo.Name <> '.') and (FileInfo.Name <> '..') Then Begin
            path := path + FileInfo.Name + '\';
            If not Recurse( path, mask ) Then Begin
              Result := False;
              Break;
            End;
            Delete( path, oldlen+1, 255 );
          End;
        retval := FindNext( FileInfo );
      End;
      FindClose( FileInfo );
    End;

  Begin
    ClearStringListWObjects(ResultList);
    If path = '' Then
      GetDir(0, fullpath)
    Else
      fullpath := path;
    If fullpath[Length(fullpath)] <> '\' Then
      fullpath := fullpath + '\';
    If mask = '' Then
      Recurse( fullpath, '*.*' )
    Else
      Recurse( fullpath, mask );
  End;

procedure ScanTreeForPaths(ResultList:TStringList;Const path: String);
// -----------------------------------------------------------------------------
//     Purpose    : Scan tree for Paths
//     Parameter  : ResultList : StringList where result will be stored
//                  Path       : Top level directory
//     Result     : -
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
  Var
    fullpath: String;
    MaskPtr: PChar;
    Ptr: PChar;
  Function Recurse( Var path: String): Boolean;
    Var
      FileInfo: TSearchRec;
      retval: Integer;
      oldlen: Integer;
    Begin
      Recurse := True;
      oldlen := Length( path );
      MaskPtr := PChar('*.*');
      while MaskPtr <> nil do
      begin
        Ptr := StrScan (MaskPtr, ';');
        if Ptr <> nil then Ptr^ := #0;
        retval := FindFirst( path+maskptr, faDirectory{faAnyFile}, FileInfo );
        While retval = 0 Do Begin
          If (FileInfo.Attr and (faDirectory {or faVolumeID)})) = faDirectory Then
          If (FileInfo.Name <> '.') and (FileInfo.Name <> '..') then
          ResultList.AddObject(path+FileInfo.Name,TExtInfo.Create);
          retval := FindNext(FileInfo);
        End;
        FindClose( FileInfo );
        if Ptr <> nil then
        begin
          Ptr^ := ';';
          Inc (Ptr);
        end;
        MaskPtr := Ptr;
      end;
      If not Result Then Exit;

      retval := FindFirst( path+'*.*', faDirectory, FileInfo );
      While retval = 0 Do Begin
        If (FileInfo.Attr and faDirectory) <> 0 Then
          If (FileInfo.Name <> '.') and (FileInfo.Name <> '..') Then Begin
            path := path + FileInfo.Name + '\';
            If not Recurse( path) Then Begin
              Result := False;
              Break;
            End;
            Delete( path, oldlen+1, 255 );
          End;
        retval := FindNext( FileInfo );
      End;
      FindClose( FileInfo );
    End;
  Begin
    ClearStringListWObjects(ResultList);
    If path = '' Then
      GetDir(0, fullpath)
    Else
      fullpath := path;
    If fullpath[Length(fullpath)] <> '\' Then
      fullpath := fullpath + '\';
//    If mask = '' Then
      Recurse( fullpath)
//    Else
//      Recurse( fullpath, mask );
  End;

procedure SyncTwoDirectories(DirectoryOne,DirectoryTwo:String;FileMask:String;FileType:TFileType;SyncMode:TSyncMode;Callback:TProgressCallback);
// -----------------------------------------------------------------------------
//     Purpose    : Synchronize the contents of two directories
//     Parameter  : Filemask
//                  FileType
//                  SyncMode (syBoth,sySrcToDst,syDstToSrc)
//     Result     : -
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
var vSrcList,vDstList:TFileRecList;
    procedure intSwapString(s1,s2:String);
    var s3:String;
    begin
      s3:=s1;
      s1:=s2;
      s2:=s3;
    end;

    procedure intSwap(sLst,dLst:TFileRecList);
    var i:Integer;
    begin
      for i:=0 to sLst.Count-1 do
      begin
        if dLst.GetIndexOfFileNameWOPATH(sLst.GetFileNameWOPATh(i)) = -1 then
           ExtCopyFile(ExtAddBackSlash(DirectoryOne)+sLst.GetFileNameWOPATh(i),
           ExtAddBackSlash(DirectoryTwo)+sLst.GetFileNameWOPATh(i),False);
           if Assigned(Callback) then if not Callback(0,sLst.Count-1,i) then Break;
        end;
    end;
begin
   vSrcList:=TFileRecList.Create;
   vDstList:=TFileRecList.Create;
   try
     vSrcList.ReadFiles(DirectoryOne,FileMask,FileType,srtNone);
     vDstList.readFiles(DirectoryTwo,FileMask,FileType,srtNone);
     case SyncMode of
       syDstToSrc:Begin
                     intSwap(vDstList,vSrcList);
                  end;
       sySrcToDst:Begin
                     intSwap(vSrcList,vDstList);
                  end;
       syBoth:    Begin
                     intSwap(vSrcList,vDstList);
                     intSwapString(DirectoryOne,DirectoryTwo);
                     intSwap(vDstList,vSrcList);
                  end;
     end;
   finally
     vSrcList.ClearFileRecList;
     vDstList.ClearFileRecList;
     vDstList.Free;
     vSrcList.Free;
   end;

end;

procedure SyncTwoTrees(BaseDirOne,BaseDirTwo:String;FileMask:String;FileType:TFileType;SyncMode:TSyncMode;Callback:TProgressCallback);
// -----------------------------------------------------------------------------
//     Purpose    : Synchronize the contents of two directory trees
//     Parameter  : Filemask
//                  FileType
//                  SyncMode (syBoth,sySrcToDst,syDstToSrc)
//     Result     : -
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
var vSrcList,vDstList:TStringList;
    procedure intSwapString(var s1,s2:String);
    var s3:String;
    begin
      s3:=s1;
      s1:=s2;
      s2:=s3;
    end;

    procedure intSwap(SrcList,DstList:TStringList);
    var
        i:Integer;
        bd1,bd2,td:String;
    begin
       for i:=0 to SrcList.Count-1 do
       begin
         if Assigned(Callback) then if not Callback(0,SrcList.Count-1,i) then break;
         td:='';
         bd1:=SrcList.Strings[i];
         Delete(bd1,1,Length(BaseDirOne));
         bd2:=ExtractFileDir(ExtAddBackSlash(BaseDirTwo)+bd1);
         if bd2<>td then begin
           if not DirectoryExists(bd2) then begin
             ExtCreateDir(bd2);
           end;
           td:=bd2;
         end;
         if not FileExists(ExtAddBackSlash(BaseDirTwo)+bd1)
         then CopyFile(PChar(SrcList.Strings[i]),PChar(ExtAddBackSlash(BaseDirTwo)+bd1),false);
       end;
     end;
begin
 vSrcList:=TStringList.Create;
 vDstList:=TStringList.Create;
 try
   ScanTreeForFiles(vSrcList,BaseDirOne,FileMask);
   ScanTreeForFiles(vDstList,BaseDirTwo,FileMask);
    case SyncMode of
       syDstToSrc:Begin
              //      intSwapString(BaseDirOne,BaseDirTwo);
                    intSwap(vDstList,vSrcList);
              //      intSwapString(BaseDirOne,BaseDirTwo);
                  end;
       sySrcToDst:Begin
                    intSwap(vSrcList,vDstList);
                  end;
       syBoth:    Begin
                    intSwap(vSrcList,vDstList);
                    intSwapString(BaseDirOne,BaseDirTwo);
                    intSwap(vDstList,vSrcList);
                    intSwapString(BaseDirOne,BaseDirTwo);
                  end;
     end;

 finally
   vSrcList.Free;
   vDstList.Free;
 end;
end;

function ExtAddBackSlash(const Path:String):String;
// -----------------------------------------------------------------------------
//     Purpose    : Add a '\' to a directory string
//     Parameter  : Path
//     Result     : Path +'\'
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
begin
  if Path<>'' then
  begin
  if AnsiLastChar(Path)^ <> '\' then
    Result := Path + '\'
  else
    Result := Path;
  end else     Result := '';
end;

function ExtRemoveBackSlash(const Directory:String):String;
// -----------------------------------------------------------------------------
//     Purpose    : Removes the last '\' from a directory string
//     Parameter  : Path
//     Result     : Path - '\'
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
var tDir:String;
begin
  tDir:=Directory;
  if Directory[Length(Directory)]='\' then Delete(tDir,Length(tDir),1);
  result:=tdir;
end;


function ExtFileDirExists (Path:string):Boolean;
// -----------------------------------------------------------------------------
//     Purpose    : Checks if directory exists
//     Parameter  : Path
//     Result     : True if existent, else false
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
var E : Integer;
begin
     E := SetErrorMode(SEM_FAILCRITICALERRORS);
     try
        Result := GetFileAttributes(PChar(Path))=FILE_ATTRIBUTE_DIRECTORY;	 //<> -1;
     finally
            SetErrorMode(E);
     end;
end;

function ExtCreateDir(const Path:String):Boolean;
// -----------------------------------------------------------------------------
//     Purpose    : Create a directory
//     Parameter  : Path
//     Result     : True if succeeded, else false
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
var Dir,Dir2,Dir3:String;
    i:Integer;
begin
   Result:=false;
   Dir:=ExtAddBackSlash(Path);
   if not DirectoryExists(Path) then
   begin
   Repeat
     if Length(Dir)=0 then Break;
     i:=Pos('\',Dir);
     Dir3:=Copy(Dir,1,i-1);
     Delete(Dir,1,i);
     Dir2:=Dir2+Dir3+'\';
     if not DirectoryExists(Dir2) then if not CreateDir(Dir2) then
     begin
       Result:=false;
       Exit;
     end else Result:=true;
   until Pos('\',Dir) =0;
   end;
end;

function GetFDrive(Path:string):string;
// -----------------------------------------------------------------------------
//     Purpose    : Get the drive of the path
//     Parameter  : Path
//     Result     : Drive
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
begin
  if Length(Path)>1 then
  result:=Copy(Path,1,2)
  else result:='';
end;

function GetFPath(Path:string):string;
// -----------------------------------------------------------------------------
//     Purpose    : Extract the path like this example : '\TEST\'
//     Parameter  : Path
//     Result     : Extracted string
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
begin
  if Length(Path)>=3 then
  result:=Copy(Path,3,Length(Path)-2)
  else result:='';
end;

function IsDriveRemovable(Path:String; var ID:String; var Name:String):Boolean;
// -----------------------------------------------------------------------------
//     Purpose    : Checks if a drive is a removable device
//     Parameter  : Path
//     Result     : True if succeeded, else false
//                  ID : the Volume-ID
//                  Name : the Volumename
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
var
    DriveType:Integer;
    VolSer : DWord;
    SysFlags : DWord;
    MaxLen : DWORD;
    Buf : string;
    VolName : array[0..255] of char;
begin
  Result:=False;
  ID:='';
  Name:='*';
  buf:=GetFDrive(Path)+'\';
  DriveType:=GetDriveType(PChar(buf));
//  if (DriveType=DRIVE_CDROM) or (DriveType=DRIVE_REMOVABLE)
//  then
  begin
    if GetVolumeInformation(pChar(Buf), VolName, 255,
    @VolSer, MaxLen, SysFlags, nil, 0)
    then
    begin
     ID:=IntToStr(VolSer);
     Name:=StrPas(VolName);
     if Name='' then Name:='*';
     Result:=True;
    end;
  end;
end;

function GetVolumeName(Drive:string):string;
// -----------------------------------------------------------------------------
//     Purpose    : Get the volumename of a drive
//     Parameter  : Drive
//     Result     : Volumename
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
var
    VolSer : DWord;
    SysFlags : DWord;
    MaxLen : DWORD;
    VolName : array[0..255] of char;
begin
   Result:='';
   if Length(Drive)>2 then Drive :=GetFDrive(Drive)+'\';
   if GetVolumeInformation(pChar(Drive), VolName, 255,
   @VolSer, MaxLen, SysFlags, nil, 0)
    then
    begin

     Result:=StrPas(VolName);
    end;
end;

function GetVolumeID(Drive:string):Integer;
// -----------------------------------------------------------------------------
//     Purpose    : Get the volumename of a drive
//     Parameter  : Drive
//     Result     : Volumename
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
var
    VolSer : DWord;
    SysFlags : DWord;
    MaxLen : DWORD;
    VolName : array[0..255] of char;
begin
   Result:=0;
   if Length(Drive)>2 then Drive :=GetFDrive(Drive)+'\';
   if GetVolumeInformation(pChar(Drive), VolName, 255,
   @VolSer, MaxLen, SysFlags, nil, 0)
    then
    begin

     Result:=VolSer;
    end;
end;

function GetVolumeIDName(Drive:string;var VolumeName:String; var Volume_id:Integer):Boolean;
// -----------------------------------------------------------------------------
//     Purpose    : Get the volumename of a drive
//     Parameter  : Drive
//     Result     : Volumename
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
var
    VolSer : DWord;
    SysFlags : DWord;
    MaxLen : DWORD;
    VolName : array[0..255] of char;
begin
   Result:=False;
   if Length(Drive)>2 then Drive :=GetFDrive(Drive)+'\';
   if GetVolumeInformation(pChar(Drive), VolName, 255,
   @VolSer, MaxLen, SysFlags, nil, 0)
    then
    begin
     Volume_id:=VolSer;
     VolumeName:=StrPas(VolName);
     Result:=True;
    end;
end;

function CutDirectory(Directory : String;MinLength:Integer) : String;
// -----------------------------------------------------------------------------
//     Purpose    : Cut a directory string ( like : 'C:\..\..\')
//     Parameter  : Directory
//                  MinLength
//     Result     : Cutted string
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
Var
  Count : Byte;  Drive : String;
  NPath : String;
Begin
  Result:='';
  if Length(Directory)<MinLength then
  result :=Directory
  else
  begin
    Count := 0;
    Repeat
      Count := Count + 1;
    Until Directory[Count] = '\';
    Drive := Copy(Directory,1, Count);
    Count := Length(Directory);
    Repeat
      Count := Count - 1;
    Until Directory[Count] = '\';
    NPath := Copy(Directory,Count, Length(Directory)-(Count-1));
    If Length(NPath) = 0 Then
    Result := Drive
    Else
    Result := Drive + '..' + NPath;
  end;
End;

function CompareDirectory(Path1,Path2:String):Boolean;
// -----------------------------------------------------------------------------
//     Purpose    : Compare two directorystrings
//     Parameter  : Path1, Path2
//     Result     : True if equal
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
begin
  result:=false;
  if StrComp(
  PCHar(
  UpperCase(ExtAddBackSlash(Path1))),
  PCHar(
  UpperCase(ExtAddBackSlash(Path2)))) = 0 then result:=true;
end;

function CompareDirectorySub(Path,SubPath:String):Boolean;
// -----------------------------------------------------------------------------
//     Purpose    : Check if SubPath is below Path
//     Parameter  : Path1, Subpath
//     Result     : True if SubPath below Path
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
var s:string;
begin
  result:=false;
  s:=Copy(SubPath,1,Length(Path));
  if StrComp(
  PCHar(
  UpperCase(ExtAddBackSlash(Path))),
  PCHar(
  UpperCase(ExtAddBackSlash(s)))) = 0 then result:=true;
end;

//-----------------------
//
// File functions implementation
//
//-----------------------

function SwitchTwoFiles(FileNameOne,FileNameTwo:String):Boolean;
// -----------------------------------------------------------------------------
//     Purpose    : Swith the filename of two files
//     Parameter  : FileNameOne FileNameTwo
//     Result     : True if succeeded
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
Var Mode:Integer;
begin
    Result:=false;
    if ExtCopyFile(FileNameOne,'~tmp1.tmp',false) then
    if ExtCopyFile(FileNameTwo,'~tmp2.tmp',false) then
    begin
       Mode:=mrAll;
       ExtDeleteFile(PChar(FileNameOne),Mode,False);
       ExtDeleteFile(PChar(FileNameTwo),Mode,False);
       ExtRenameFile('~tmp1.tmp',FileNameTwo);
       ExtRenameFile('~tmp2.tmp',FileNameOne);
       Result:=true;
    end;
end;

function RenameInsertFile(Directory,FileNameOne,FileNameTwo:string;CharToBeAdded:Char):String;
// -----------------------------------------------------------------------------
//
//     Purpose    : Testing ....
//
//     Parameter  :
//
//     Result     :
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
var
   sList:TFileRecList;
   s1,s2,s3:string;
begin
   result:='';
   if Directory<> GetCurrentDir then if DirectoryExists(Directory) then ChDir(Directory);
   sList:=TFileRecList.Create;
   try
     FileNameOne:=ExtractFileName(FileNameOne);
     FileNameTwo:=ExtractFileName(FileNameTwo);
     sList.ReadFiles(Directory,'*.*',[ftNormal,ftHidden,ftReadOnly,ftSystem],srtName);
//     s1:=Copy(FileNameTwo,1,Pos('.',FileNameTwo)-1);
//     s0:=s1;
     s2:=sList.GetFileName(sList.GetIndexOfFileName(FileNameTwo)-1);
     s2:=Copy(s2,1,Pos('.',s2)-1);
     s3:=sList.GetFileName(sList.GetIndexOfFileName(FileNameTwo));
     s3:=Copy(s3,1,Pos('.',s3)-1);
     if s2='' then s2:=s3;
     s1:=s2;
      if Length(s2)=Length(s3) then
      begin
         if s2[Length(s2)]>=s3[Length(s3)] then s1[Length(s1)]:=Chr(Ord(s2[Length(s2)])+1) else
         s1:=s1+s1[Length(s1)]+s2[Length(s2)];
       While FileExists(ExtAddBackSlash(Directory)+s1+ExtractFileExt(FileNameOne))
       do s1:=s1+Chr(Ord(s2[Length(s2)])-1);

      end
      else if Length(s2)<Length(s3) then
       begin
       s1[Length(s1)]:=Chr(Ord(s2[Length(s2)])+1);
       While FileExists(ExtAddBackSlash(Directory)+s1+ExtractFileExt(FileNameOne))
       do s1:=s1+Chr(Ord(s2[Length(s2)]));
       end
      else begin
       s1[Length(s1)]:=Chr(Ord(s2[Length(s2)])+1)
      end;

      s1:=s1+ExtractFileExt(FileNameOne);
      amOvrAll:=8;
      ExtRenameFile(FileNameOne,s1);
      result:=s1;
   finally
     sList.ClearFileRecList;
     sList.Free;
   end;
end;

function ExtCopyFile(const Source, Destination: TFileName;AskForOverwrite:Boolean):Boolean;
// -----------------------------------------------------------------------------
//     Purpose    : Copy file(s)
//     Parameter  : Sourcefilename, Destinationfilename
//                  AskForOverWrite: If false, existing files will be overwritten
//     Result     : True if succeeded
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
begin
  result:=false;
  if AnsiCompareFileName(Source, Destination) <> 0 then
  begin
     if FileExists(Destination) then
     begin
      if AskForOverWrite then if MessageDlg(Format(msgAskForOverwrite,[Destination]),mtWarning,[mbYes,mbNo],0)
      = IDNO then Exit;
      FileSetAttr(Destination,0);
      DeleteFile(PChar(Destination));
     end;
      result:=CopyFile(PChar(Source),PChar(Destination),false);
  end;
end;


function ExtMoveFile(const Source, Destination: TFileName;AskForOverwrite:Boolean):Boolean;
// -----------------------------------------------------------------------------
//     Purpose    : Move file(s)
//     Parameter  : Sourcefilename, Destinationfilename
//                  AskForOverWrite: If false, existing files will be overwritten
//     Result     : True if succeeded
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
var AttrBuffer:DWORD;
begin
  result:=false;
  if AnsiCompareFileName(Source, Destination) <> 0 then
  begin
    if not RenameFile(Source, Destination) then { try just renaming }
    begin
      AttrBuffer:=FileGetAttr(Source);
      if AskForOverWrite then
      begin
        if (FileGetAttr(Source) and faReadOnly)=faReadOnly then
        if amOvrAll=8 then FileSetAttr(Source,0) else
        begin
          amOvrAll:=MessageDlg(Format(msgAskForMoveReadOnly,[Source]),mtWarning,[mbYes,mbNo,mbAll],0);
          if (amOvrAll = IDYES) or (amOvrAll=8) then FileSetAttr(Source,0);
        end;
        if FileExists(Destination) then
        if MessageDlg(Format(msgAskForOverwrite,[Destination]),mtWarning,[mbYes,mbNo],0)
        = IDYES then
        begin
           FileSetAttr(Destination,0);
           result:=MoveFile(PChar(Source),PChar(Destination));
           if result then FileSetAttr(Destination,AttrBuffer);
        end;
      end else
      begin
        if FileExists(Destination) then
        begin
          if (FileGetAttr(Destination) and faReadOnly)=faReadOnly then FileSetAttr(Destination,0);
          DeleteFile(PChar(Destination));
        end;
        result:=MoveFile(PChar(Source),PChar(Destination));
      end;
    end else result:=true;
  end;
end;

function ExtDeleteFile(const FileName:TFileName;var DeleteMode:Integer;IntoRecycleBin:Boolean):Boolean;
// -----------------------------------------------------------------------------
//     Purpose    : Delete file(s)
//     Parameter  : Filename
//                  DeleteMode (0 or mrAll (8))
//     Result     : True if succeeded
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
procedure Ask;
begin
    DeleteMode:=MessageDlg(Format(msgAskForDelete,[FileName]),
       mtConfirmation, [mbYes, mbNo, mbAll], 0);
end;
begin
  result:=False;
  if DeleteMode<>mrAll then Ask;
  if (DeleteMode=mrAll) or (DeleteMode=mrYes) then
  if FileExists(FileName) then
  begin
   if (FileGetAttr(FileName) and faReadOnly)=faReadOnly then

      if amOvrAll=8 then FileSetAttr(FileName,0) else
      begin
       amOvrAll:=MessageDlg(Format(msgAskReadOnly,[FileName]),mtWarning,[mbYes,mbNo,mbAll],0);
       if (amOvrAll = IDYES) or (amOvrAll=8) then FileSetAttr(FileName,0);
      end;
      if IntoRecycleBin then
      result:=DeleteToRecycleBin(FileName) else
      result:=DeleteFile(PChar(FileName));
  end;
end;

function ExtRenameFile(const OldName, NewName: TFileName):Boolean;
// -----------------------------------------------------------------------------
//     Purpose    : Rename file(s)
//     Parameter  : OldName, NewName
//     Result     : True if succeeded
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
var AttrBuffer:DWORD;
begin
  result:=False;
  if FileExists(OldName) and (AnsiCompareFileName(OldName,Newname) <> 0) then
  begin
   AttrBuffer:=FileGetAttr(Oldname);
   if (AttrBuffer and faReadOnly)=faReadOnly then
      if amOvrAll=8 then FileSetAttr(OldName,0) else
      begin
       amOvrAll:=MessageDlg(Format(msgAskForRenameReadOnly,[Oldname]),mtWarning,[mbYes,mbNo,mbAll],0);
       if (amOvrAll = IDYES) or (amOvrAll=8) then FileSetAttr(OldName,0);
      end;
    result:=RenameFile(PChar(OldName),PChar(NewName));
    if result then FileSetAttr(NewName,AttrBuffer) else FileSetAttr(Oldname,AttrBuffer);
  end;
end;


function ExtGetFileSize(const FileName: string): LongInt;
// -----------------------------------------------------------------------------
//     Purpose    : Get the Filesize
//     Parameter  : Filename
//     Result     : Filesize in bytes
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
var
  SearchRec: TSearchRec;
begin
  if FindFirst(ExpandFileName(FileName), faAnyFile, SearchRec) = 0 then
    Result := SearchRec.Size
  else Result := -1;
end;

function ExtFileDateTime(const FileName: string): System.TDateTime;
// -----------------------------------------------------------------------------
//     Purpose    : Get the Filedate
//     Parameter  : Filename
//     Result     : Filesize as TDateTime
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
begin
  Result := FileDateToDateTime(FileAge(FileName));
end;

function ExtHasAttr(const FileName: string; Attr: Word): Boolean;
// -----------------------------------------------------------------------------
//     Purpose    : Check if file has attribute(s)
//     Parameter  : Filename
//     Result     : True if file has the attribute(s)
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
begin
  Result := (FileGetAttr(FileName) and Attr) = Attr;
end;

function ExtExecuteFile(const FileName, Params, DefaultDir: string;
  ShowCmd: Integer): THandle;
// -----------------------------------------------------------------------------
//     Purpose    : Execute File
//     Parameter  : Filename , Params, Default directory
//     Result     : Handle of process
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
//var
//  zFileName, zParams, zDir: array[0..79] of Char;
begin
  Result:=0;
  {Result := ShellExecute(Application.MainForm.Handle, nil,
    StrPCopy(zFileName, FileName), StrPCopy(zParams, Params),
    StrPCopy(zDir, DefaultDir), ShowCmd);}
end;

//-----------------------
//
// Misc functions implementation
//
//-----------------------

function ExtFillUpWithZero(Number:Integer;Digits:Integer):String;
// -----------------------------------------------------------------------------
//     Purpose    : Convert a Integer to string and fill up with n x NULL
//                  (e.g. 123 -> '000123')
//     Parameter  : Number, Digits
//     Result     : Resultstring
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
var t,n:String;
    i:Integer;
begin
  result:='';
  n:='';
  t:=IntToStr(Number);
  if Length(t)< Digits then
  begin
    for i:=Length(t) to Digits-1 do  n:=n+'0';
    result:=ConCat(n,t);
  end
  else if Length(t)=Digits then result:=t;
end;

//-----------------------
//
// CRC functions implementation
//
//-----------------------

procedure MakeCRCTable;
// -----------------------------------------------------------------------------
//     Purpose    : Create CRC table
//     Parameter  : -
//     Result     : -
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
var ii,jj:Integer;
begin
 if not crc32tablecreated then
 begin
  crc32tablecreated:=True;
  for ii := 0 to 256-1 do
  begin
    crc_table [ii] := ii ;
    for jj := 0  to 8-1 do
    begin
      if ((crc_table [ii] and 1) = 0)
       then  crc_table [ii]:= crc_table [ii] shr 1
      else
        crc_table [ii] := $EDB88320 xor (crc_table [ii] shr 1) ;
    end;
   end;
  end;
end;

function CRC32 (crc:longint;buffer:PByteArray;length:Integer):DWord;
// -----------------------------------------------------------------------------
//     Purpose    : CRC32 sub function
//     Parameter  : internal used
//     Result     : internal used
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
var ii:Integer;
    c:LongInt;
begin
  if not crc32tablecreated then MakeCRCTable;
  c:=crc;
  for ii := 0 to length-1  do
   c:=(c shr 8) xor  crc_table[buffer[ii] xor (c and $ff)];
  result:= c;
end;

function CRC32File (FileName:  string):DWord;
// -----------------------------------------------------------------------------
//     Purpose    : Create the CRC32 of a file
//     Parameter  : Filename
//     Result     : CRC32
//
//     (c) July 1999 by Andreas Moser, amoser@amoser.de
// -----------------------------------------------------------------------------
var
      BytesRead:  INTEGER;
      CRC32Val:  DWORD;
      CRCStream  :TFileStream;
      ByteArray: Array [0..1023] of Byte;
begin
  if not crc32tablecreated then MakeCRCTable;
  if FileExists(FileName) then
  begin
  CRCStream:=TFileStream.Create(FileName,fmOpenRead);
  try
    CRC32Val := $FFFFFFFF;

    repeat
      ZeroMemory(@ByteArray,SizeOf(ByteArray));
      BytesRead:=CRCStream.Read(ByteArray,SizeOf(ByteArray));
      if BytesRead > 0
      then begin
         CRC32Val:=CRC32 (CRC32Val,@ByteArray,BytesRead);
      end;
      until BytesRead = 0;
    CRC32Val := not CRC32Val;
  finally
    CRCStream.Free;
  end;
  result:=CRC32Val;
  end else result:=0;
end;

function DeleteToRecycleBin(Filename:String):Boolean;
Var T:TSHFileOpStruct;
begin
  FillChar( T, SizeOf( T ), 0 );
  With T do
  Begin
    Wnd:=Application.MainForm.Handle;
    wFunc:=FO_DELETE;
    pFrom:=Pchar(Filename+#0#0);
    fFlags:=FOF_ALLOWUNDO+FOF_NOCONFIRMATION;
  End;
  result:= true ;
  SHFileOperation(T);
end;

procedure ClearStringListWObjects(List:TSTringList);

var i:Integer;
begin
  if List.Count>0 then for i:=List.Count-1  downto 0
  do
  if Assigned(List.Objects[i]) then
//  if fSelList.Objects[i] is TExtInfo then
  try
    List.Objects[i].Free;
  finally
    List.Objects[i]:=nil;
  end;
  List.Clear;
end;

function GetLastPathPart(Path:string):string;
var idx:Integer;
begin
  Path:=ExtRemoveBackSlash(Path);
  result:='';
  while True do
  begin
    idx:=Pos('\',Path);
    if idx >0 then
    Delete(Path,1,idx)
    else
    begin
      Result:=Path;
      break;
    end;
  end;
end;

function DeleteLastPathPart(Path:string):string;
var idx:Integer;
    buf:string;
begin
  Path:=ExtRemoveBackSlash(Path);
  result:='';
  while True do
  begin
    idx:=Pos('\',Path);
    if idx >0 then
    begin
      buf:=buf+Copy(Path,1,idx);
      Delete(Path,1,idx);
    end

    else
    begin
      Result:=buf;
      break;
    end;
  end;
end;

function ExtGetTempFileName(Prefix,AlternatePath:String):string;
var Path:Array [0..255] of Byte;
    FileName:Array [0..255] of Byte;
    BufferLength:DWORD;
    r:UINT;
begin
  if GetTempPath(BufferLength,@Path) = 0 then
  begin
   r:=GetTempFileName(@PAth,PChar(Prefix),0,@FileName);
  end
   else
   r:=GetTempFileName(PChar(AlternatePath),PChar(Prefix),0,@FileName);

  //if r =0 then result:='' else result :=  StrPas(@FileName);

  if r =0 then result:='' else result :=  PWideChar(@FileName);
end;

function ExtGetTempPath:string;
begin

end;

procedure GetDriveInfo;
var
 __buf:array [0..30*4] of Char;
 __r,__i,__j:Integer;
 __st:string;
 E:Integer;
begin
  for __i:=0 to 30*4 do __buf[__i]:=CHar(32);
  __r:=GetLogicalDriveStrings(30*4,@__buf);
  __j:=0;
  E:=SetErrorMode(SEM_FAILCRITICALERRORS);
  for __i:=0 to __r div 4
  do begin
    drive_table[__j].Drive:=UpperCase(__buf[4*__i]+__buf[4*__i+1]);
    if (drive_table[__j].Drive<>'A:') and (drive_table[__j].Drive<>'B:')
    then
    begin
      drive_table[__j].Volume_ID:=GetVolumeID(drive_table[__j].Drive);
      drive_table[__j].Volume_Name:=GetVolumeName(drive_table[__j].Drive);
    end;
    inc(__j);
  end;
  drive_table_count:=__j;
  SetErrorMode(E);
end;

function IsVolumeAvailable(Volume_Name:String):Boolean;
var i:Integer;
begin
  result:=false;
  for i:=0 to drive_table_count-1 do
  if AnsiCompareText(Volume_Name,drive_table[i].Volume_Name)=0 then result:=true;
end;

procedure RetrieveDriveInfo(Drive:Char);
var i:Integer;
begin
  for i:=0 to drive_table_count-1 do
  if drive_table[i].Drive[1]=Drive then
  begin
    drive_table[i].Volume_ID:=GetVolumeID(drive_table[i].Drive);
    drive_table[i].Volume_Name:=GetVolumeName(drive_table[i].Drive);
    break;
  end;

end;

end.

