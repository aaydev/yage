{*******************************************************}
{ YAGE: Yet Another Global Encoder                      }
{ Unit: Common.ShellFileSupport                         }
{ Copyright(c) 2016 Alexander Alexeev                   }
{*******************************************************}

unit Common.ShellFileSupport;

interface

type
  TDriveNumber    = 0..25;
  TPathCharType   = (gctInvalid, gctLFNChar, gctSeparator, gctShortChar, gctWild);
  TPathCharTypes  = set of TPathCharType;
  TCleanupResult  = (pcsReplacedChar, pcsRemovedChar, pcsTruncated);
  TCleanupResults = set of TCleanupResult;
  PCleanupResults = ^TCleanupResults;

const
  InvalidDrive = TDriveNumber(-1);

// Returns the type of character from the path
function PathGetCharType(const AChar: Char): TPathCharTypes;

// Returns the disk number from the path (InvalidDrive on error)
function PathGetDriveNumber(const APath: string): TDriveNumber;

// Generates the path to the root directory of the specified disk
function PathBuildRoot(const ADrive: TDriveNumber): string;

// Canonizes a path by removing special directories from it. and '..'
function PathCanonicalize(const APath: string): string;

// Connects two paths, adding a path separator if necessary
function PathAppend(const APath, AMore: string): string;

// Similar to PathAppend, but returns the canonical path (with the remote '.' And '..')
function PathCombine(const APath, AMore: string): string;

// Returns True if the specified path (file / directory) exists
// Implement in case you don’t want to use the great FileExists / DirectoryExists from Delphi
// Cm.
// http://qc.embarcadero.com/wc/qcmain.aspx?d=3513
// http://qc.embarcadero.com/wc/qcmain.aspx?d=10731
// http://qc.embarcadero.com/wc/qcmain.aspx?d=52905
function PathFileExists(const APath: string): Boolean;

// Returns true if the path is a directory
// Implement in case you don’t want to use the great FileExists / DirectoryExists from Delphi
// Cm.
// http://qc.embarcadero.com/wc/qcmain.aspx?d=3513
// http://qc.embarcadero.com/wc/qcmain.aspx?d=10731
// http://qc.embarcadero.com/wc/qcmain.aspx?d=52905
function PathIsDirectory(const APath: string): Boolean;

// Returns True if the path does not contain path delimiters (':' and '\')
function PathIsFileSpec(const APath: string): Boolean;

// Returns true if the path is relative
function PathIsRelative(const APath: string): Boolean;

// Returns true if the path is absolute
function PathIsAbsolute(const APath: string): Boolean;

// Enclose the string in quotes if necessary (presence of spaces)
function PathQuoteSpaces(const APath: string; const AForce: Boolean = False): string;

// Generates a relative path to the ATo from (relatively) AFrom (the slave '\' denotes the directory)
function PathRelativePathTo(const AFrom, ATo: string): string;

// Allows relative name to absolute, additionally canonizing the path
function PathSearchAndQualify(const APath: string): string;

// Returns a short name for a long
function PathGetShortPath(const APath: string): string;

// Returns a long name for a short
function PathGetLFNPath(const APath: string): string;

// Returns True, if the path is valid
function PathIsValid(const APath: string): Boolean;

// Creates a command line to run the program. The result of this function can be passed to CreateProcess.
function PathProcessCommand(const AProgram: string; const AParameters: array of string): string;

implementation

{$A+}
{$R+}
{$Z4}
{$WARN SYMBOL_PLATFORM OFF}

uses
  Windows, SysUtils;

function Kernel32: HMODULE; forward;
function ShlwAPI: HMODULE; forward;

{$IFNDEF UNICODE}
type
  Unicodestring = Widestring;
{$ENDIF}

procedure CreateBuffer(out Buffer: string; const ALen: Integer); overload;
begin
  SetLength(Buffer, ALen);
  FillChar(Pointer(Buffer)^, ALen * SizeOf(Char), 0);
end;

procedure CreateBuffer(out Buffer: string; const APath: string); overload;
begin
  CreateBuffer(Buffer, MAX_PATH);
  Move(Pointer(APath)^, Pointer(Buffer)^, Length(APath) * SizeOf(Char));
end;

{$IFNDEF UNICODE}
procedure CreateBuffer(out Buffer: Unicodestring; const ALen: Integer); overload;
begin
  SetLength(Buffer, ALen);
  FillChar(Pointer(Buffer)^, ALen * SizeOf(WideChar), 0);
end;

procedure CreateBuffer(out Buffer: Unicodestring; const APath: string); overload;
var
  Path: Unicodestring;
begin
  CreateBuffer(Buffer, MAX_PATH);
  Path := APath;
  Move(Pointer(Path)^, Pointer(Buffer)^, Length(APath) * SizeOf(WideChar));
end;
{$ENDIF}

function PathQuoteSpaces(const APath: string; const AForce: Boolean): string;
begin
  if (not AForce) and
     (Pos(' ', APath) <= 0) and
     (Pos('"', APath) <= 0) then
  begin
    Result := APath;
    Exit;
  end;

  Result := '"' + stringReplace(APath, '"', '\"', [rfReplaceAll]) + '"';
  if (Length(Result) > 2) and
     (Result[Length(Result) - 1] = '"') then
    Insert('\', Result, Length(Result) - 1);
end;

var
  FPathRelativePathTo: function(APath, AFrom: PChar; AttrFrom: DWORD; ATo: PChar; AttrTo: DWORD): BOOL; stdcall;

function PathRelativePathTo(const AFrom, ATo: string): string;
var
  Buffer, From, ToD: string;
  AttrFrom, AttrTo: DWORD;

begin
  if not Assigned(FPathRelativePathTo) then
  begin
    FPathRelativePathTo := GetProcAddress(ShlwAPI, {$IFDEF UNICODE}'PathRelativePathToW'{$ELSE}'PathRelativePathToA'{$ENDIF});
    Win32Check(Assigned(FPathRelativePathTo));
  end;

  Assert(AFrom <> '');
  Assert(ATo <> '');

  if AFrom[Length(AFrom)] = PathDelim then
    AttrFrom := FILE_ATTRIBUTE_DIRECTORY
  else
    AttrFrom := 0;
  if ATo[Length(ATo)] = PathDelim then
    AttrTo := FILE_ATTRIBUTE_DIRECTORY
  else
    AttrTo := 0;

  From := ExcludeTrailingPathDelimiter(PathCanonicalize(AFrom));
  ToD  := ExcludeTrailingPathDelimiter(PathCanonicalize(ATo));

  CreateBuffer(Buffer, MAX_PATH);
  if FPathRelativePathTo(PChar(Buffer), PChar(From), AttrFrom, PChar(ToD), AttrTo) then
    Result := PChar(Buffer)
  else
    Result := '';
end;

var
  FGetShortPathName: function(ALong, AShort: PChar; Len: Integer): Integer; stdcall;

function PathGetShortPath(const APath: string): string;
begin
  if not Assigned(FGetShortPathName) then
  begin
    FGetShortPathName := GetProcAddress(Kernel32, {$IFDEF UNICODE}'GetShortPathNameW'{$ELSE}'GetShortPathNameA'{$ENDIF});
    Win32Check(Assigned(FGetShortPathName));
  end;

  CreateBuffer(Result, 32768);
  SetLength(Result, FGetShortPathName(PChar(APath), PChar(Result), 32768));
  if Result = '' then
    Result := APath;
end;

var
  FGetLongPathName: function(AShort, ALong: PChar; Len: Integer): Integer; stdcall;

function PathGetLFNPath(const APath: string): string;
begin
  if not Assigned(FGetLongPathName) then
  begin
    FGetLongPathName := GetProcAddress(Kernel32, {$IFDEF UNICODE}'GetLongPathNameW'{$ELSE}'GetLongPathNameA'{$ENDIF});
    Win32Check(Assigned(FGetLongPathName));
  end;

  CreateBuffer(Result, 32768);
  SetLength(Result, FGetLongPathName(PChar(APath), PChar(Result), 32768));
  if Result = '' then
    Result := APath;
end;

function PathProcessCommand(const AProgram: string; const AParameters: array of string): string;
var
  X: Integer;
  Param: string;
begin
  Result := PathQuoteSpaces(AProgram);

  for X := 0 to High(AParameters) do
  begin
    if PathFileExists(AParameters[X]) then
      Param := PathQuoteSpaces({$IFDEF UNICODE}PathGetShortPath({$ENDIF}AParameters[X]{$IFDEF UNICODE}){$ENDIF})
    else
      Param := PathQuoteSpaces(AParameters[X]);
    Result := Result + ' ' + Param;
  end;
end;

function PathIsValid(const APath: string): Boolean;
const
  UNCWPrefix = '\\?';
var
  Path: string;
  I: Integer;
begin
  Result := False;
  if APath = '' then
    Exit;

  // The function splits the path into parts and checks each part by calling MoveFile
  // MoveFile will return either OK or ERROR_ALREADY_EXISTS for the correct parts;
  // and returns other errors for reserved characters, reserved names (COM, etc.), unsupported characters to the underlying file system  Result := False;
  Path := APath;
  repeat
    I := LastDelimiter('\/', Path);
    if (Path <> '') and
       (
         (Path[Length(Path)] = '.') or
         (Path[Length(Path)] = ' ')
       ) then
      Exit;
    MoveFile(nil, PChar(Path));
    if (GetLastError = ERROR_ALREADY_EXISTS) or
       (
         (GetFileAttributes(PChar(Copy(Path, I + 1, MaxInt))) = INVALID_FILE_ATTRIBUTES) and
         (GetLastError = ERROR_INVALID_NAME)
       ) then
      Exit;
    if I > 0 then
      Path := Copy(Path, 1, I - 1);
    if (I = 4) and (Path = UNCWPrefix) then
      I := 0;
  until I = 0;
  Result := True;
end;

function PathAppend(const APath, AMore: string): string;
var
  Path, More: string;
begin
  if AMore = '' then
  begin
    Result := APath;
    Exit;
  end;

  Path := stringReplace(APath, '/', PathDelim, [rfReplaceAll]);
  More := stringReplace(AMore, '/', PathDelim, [rfReplaceAll]);
  if More[1] = PathDelim then
    Result := ExcludeTrailingPathDelimiter(Path) + More
  else
    Result := IncludeTrailingPathDelimiter(Path) + More;
end;

function PathCombine(const APath, AMore: string): string;
begin
  Result := PathCanonicalize(PathAppend(APath, AMore));
end;

var
  FPathGetCharType: function(Ch: Char): UINT; stdcall;

function PathGetCharType(const AChar: Char): TPathCharTypes;
const
  GCT_INVALID   = 0;
  GCT_LFNCHAR   = 1;
  GCT_SHORTCHAR = 2;
  GCT_WILD      = 4;
  GCT_SEPARATOR = 8;
var
  R: UINT;
begin
  Result := [];

  if not Assigned(FPathGetCharType) then
  begin
    FPathGetCharType := GetProcAddress(ShlwAPI, {$IFDEF UNICODE}'PathGetCharTypeW'{$ELSE}'PathGetCharTypeA'{$ENDIF});
    Win32Check(Assigned(FPathGetCharType));
  end;

  R := FPathGetCharType(AChar);
  if R = GCT_INVALID then
  begin
    Result := [gctInvalid];
    Exit;
  end;

  if (R and GCT_LFNCHAR) <> 0 then
    Include(Result, gctLFNChar);
  if (R and GCT_SEPARATOR) <> 0 then
    Include(Result, gctSeparator);
  if (R and GCT_SHORTCHAR) <> 0 then
    Include(Result, gctShortChar);
  if (R and GCT_WILD) <> 0 then
    Include(Result, gctWild);
end;

var
  FPathGetDriveNumber: function(Path: PChar): Integer; stdcall;

function PathGetDriveNumber(const APath: string): TDriveNumber;
var
  R: Integer;
begin
  if not Assigned(FPathGetDriveNumber) then
  begin
    FPathGetDriveNumber := GetProcAddress(ShlwAPI, {$IFDEF UNICODE}'PathGetDriveNumberW'{$ELSE}'PathGetDriveNumberA'{$ENDIF});
    Win32Check(Assigned(FPathGetDriveNumber));
  end;

  R := FPathGetDriveNumber(PChar(APath));
  if R < 0 then
    Result := InvalidDrive
  else
    Result := TDriveNumber(R);
end;

var
  FPathBuildRoot: function(Root: PChar; I: Integer): PChar; stdcall;

function PathBuildRoot(const ADrive: TDriveNumber): string;
var
  Buffer: string;
begin
  if not Assigned(FPathBuildRoot) then
  begin
    FPathBuildRoot := GetProcAddress(ShlwAPI, {$IFDEF UNICODE}'PathBuildRootW'{$ELSE}'PathBuildRootA'{$ENDIF});
    Win32Check(Assigned(FPathBuildRoot));
  end;

  CreateBuffer(Buffer, 4);
  Result := FPathBuildRoot(PChar(Buffer), Ord(ADrive));
end;

var
  FPathCanonicalize: function(ADst, ASrc: PChar): BOOL; stdcall;

function PathCanonicalize(const APath: string): string;
var
  Buffer, Path: string;
  X: Integer;
begin
  if not Assigned(FPathCanonicalize) then
  begin
    FPathCanonicalize := GetProcAddress(ShlwAPI, {$IFDEF UNICODE}'PathCanonicalizeW'{$ELSE}'PathCanonicalizeA'{$ENDIF});
    Win32Check(Assigned(FPathCanonicalize));
  end;

  CreateBuffer(Buffer, MAX_PATH);
  Path := stringReplace(APath, '/', PathDelim, [rfReplaceAll]);
  Win32Check(FPathCanonicalize(PChar(Buffer), PChar(Path)));
  Result := PChar(Buffer);

  // Remove double '\'
  for X := Length(Result) downto 3 do
    if (Result[X] = PathDelim) and
       (Result[X - 1] = PathDelim) then
      Delete(Result, X, 1);
end;

var
  FPathSearchAndQualify: function(APath, AFullyQualifiedPath: PChar; Len: UINT): BOOL; stdcall;

function PathSearchAndQualify(const APath: string): string;
var
  Buffer: string;
begin
  if not Assigned(FPathSearchAndQualify) then
  begin
    FPathSearchAndQualify := GetProcAddress(ShlwAPI, {$IFDEF UNICODE}'PathSearchAndQualifyW'{$ELSE}'PathSearchAndQualifyA'{$ENDIF});
    Win32Check(Assigned(FPathSearchAndQualify));
  end;

  CreateBuffer(Buffer, MAX_PATH);
  Win32Check(FPathSearchAndQualify(PChar(APath), PChar(Buffer), MAX_PATH));
  Result := PChar(Buffer);
end;

var
  FPathFileExists: function(Path: PChar): BOOL; stdcall;

function PathFileExists(const APath: string): Boolean;
begin
  if not Assigned(FPathFileExists) then
  begin
    FPathFileExists := GetProcAddress(ShlwAPI, {$IFDEF UNICODE}'PathFileExistsW'{$ELSE}'PathFileExistsA'{$ENDIF});
    Win32Check(Assigned(FPathFileExists));
  end;

  Result := FPathFileExists(PChar(APath));
end;

var
  FPathIsDirectory: function(Path: PChar): UINT; stdcall;

function PathIsDirectory(const APath: string): Boolean;
begin
  if not Assigned(FPathIsDirectory) then
  begin
    FPathIsDirectory := GetProcAddress(ShlwAPI, {$IFDEF UNICODE}'PathIsDirectoryW'{$ELSE}'PathIsDirectoryA'{$ENDIF});
    Win32Check(Assigned(FPathIsDirectory));
  end;

  Result := FPathIsDirectory(PChar(APath)) <> 0;
end;

var
  FPathIsFileSpec: function(Path: PChar): BOOL; stdcall;

function PathIsFileSpec(const APath: string): Boolean;
begin
  if not Assigned(FPathIsFileSpec) then
  begin
    FPathIsFileSpec := GetProcAddress(ShlwAPI, {$IFDEF UNICODE}'PathIsFileSpecW'{$ELSE}'PathIsFileSpecA'{$ENDIF});
    Win32Check(Assigned(FPathIsFileSpec));
  end;

  Result := FPathIsFileSpec(PChar(APath));
end;

var
  FPathIsRelative: function(Path: PChar): BOOL; stdcall;

function PathIsRelative(const APath: string): Boolean;
var
  X: Integer;
begin
  // http://stackoverflow.com/questions/26099361/is-it-a-winapi-bug-with-pathisrelative-function
  X := Pos(':', APath);
  if (X > 0) and
     (X < Length(APath)) and
     (APath[X + 1] <> PathDelim) and
     (APath[X + 1] <> '/') then
  begin
    Result := True;
    Exit;
  end;

  if not Assigned(FPathIsRelative) then
  begin
    FPathIsRelative := GetProcAddress(ShlwAPI, {$IFDEF UNICODE}'PathIsRelativeW'{$ELSE}'PathIsRelativeA'{$ENDIF});
    Win32Check(Assigned(FPathIsRelative));
  end;

  Result := FPathIsRelative(PChar(APath));
end;

function PathIsAbsolute(const APath: string): Boolean;
begin
  Result := not PathIsRelative(APath);
end;

var
  FKernelLib: HMODULE;
  FShlwAPILib: HMODULE;

function Kernel32: HMODULE;
const
  DLLName = 'kernel32.dll';
begin
  if FKernelLib = 0 then
  begin
    FKernelLib := LoadLibrary(DLLName);
    Win32Check(FKernelLib <> 0);
  end;
  Result := FKernelLib;
end;

function ShlwAPI: HMODULE;
const
  DLLName = 'shlwapi.dll';
begin
  if FShlwAPILib = 0 then
  begin
    FShlwAPILib := LoadLibrary(DLLName);
    Win32Check(FShlwAPILib <> 0);
  end;
  Result := FShlwAPILib;
end;

end.
