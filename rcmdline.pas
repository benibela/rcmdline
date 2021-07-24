{Copyright (C) 2006  Benito van der Zander

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
}
(*** @abstract(
  Command line reader
)

See TCommandLineReader
*)
unit rcmdline;

interface
{$IFDEF FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch autoderef}
{$ENDIF}

//{$define unitcheck_rcmdline}

uses sysutils,classes;
type
  TStringArray=array of string;
  TLongintArray=array of longint;
  TFloatArray=array of extended;
  TBooleanArray=array of boolean;
  TCommandLineReaderLanguage=(clrlEnglish,clrlGerman);
  ECommandLineParseException=class(Exception);
  TKindOfProperty=(kpStr,kpFile,kpInt,kpFloat,kpFlag);
  TProperty=record
   category: string;
   name,description,strvalue,strvalueDefault:string;
   strenumeration: TStringArray;
   found: boolean;
   abbreviation: char;
   case kind: TKindOfProperty of
     kpStr,kpFile: ();
     kpInt: (intvalue, intvalueDefault: longint);
     kpFloat: (floatvalue, floatvalueDefault: extended);
     kpFlag: (flagvalue,flagdefault: boolean)
  end;
  PProperty=^TProperty;
  TOptionReadEvent = procedure (sender: TObject; const name, value: string) of object;
  TOptionInterpretationEvent = procedure (sender: TObject; var name, value: string; const args: TStringArray; var argpos: integer) of object;
  TCommandLineReaderShowError=procedure (sender: TObject; errorDescription: string) of object;

  { TCommandLineReader }

  (*** @abstract(
    A command line reader class that checks for valid arguments and automatically prints a formatted help.
    )
                                                                                                           @br
                                                                                                           @br
    Usage: @orderedList(
      @item(  Declare all allowed arguments with the corresponding DeclareXXXX functions )
      @item(  Call parse to explicitely read the actual command line (optionally) )
      @item(  Use readXXX to read a declared argument )
    )


    On the command line arguments can be given in different ways, e.g.
    @preformatted(
      --name=value
      /name=value
      --name value
      /name value
      --name="value"
      /name="value"
    )


    Declared flags can be changed with @code(--enable-flag) or @code(--disable-flag) or @code(--flag) where
    latter option negates the default value.@br
    File names are checked for spaces, so it is not always necessary to surround them with quotes.

    Internally, the allowed command line arguments are called both "properties" and "options".

    Example:

    @longCode(#
var cmdline: TCommandLineReader;
begin
  cmdline := TCommandLineReader.create;

  //Initial declaration of some command line parameters:
  cmdline.declareString('name', 'An example string property');
  cmdline.declareString('foo', 'Another example string property', 'bar');
  cmdline.declareInt('count', 'An example integer property', 123);
  cmdline.declareFlag('flag', 'An example boolean property');

  //cmdline.parse();



  //1. Parsing some command line options
  cmdline.parse('--name="some name" --count 9');

  cmdline.readString('name'); //some name
  cmdline.readString('foo');  //bar
  cmdline.readInt('count');   //9
  cmdline.readFlag('flag');   //false

  cmdline.existsProperty('name'); //true
  cmdline.existsProperty('foo');  //false




  //2. Parsing some other command line options
  cmdline.parse('--foo barbar --flag');

  cmdline.readString('name'); //
  cmdline.readString('foo');  //barbar
  cmdline.readInt('count');   //123
  cmdline.readFlag('flag');   //true

  cmdline.existsProperty('name'); //false
  cmdline.existsProperty('foo');  //true




  //3. Parsing some other command line options
  cmdline.parse('--help');

  //prints automatically generated help to stdout and halts:
  {
  The following command line options are valid:

  --name=<string> 	An example string property
  --foo=<string>  	Another example string property (default: bar)
  --count=<int>   	An example integer property (default: 123)
  --flag          	An example boolean property
  }




  //4. Some more advanced usage
  cmdline.addAbbreviation('f'); //abbreviation for the last option (--flag)
  cmdline.allowDOSStyle := true; //DOS slash options. Default is only true on Windows

  cmdline.parse('/name "x y z" -f /count=1 /foo="abc"');
  cmdline.readString('name'); //x y z
  cmdline.readString('foo');  //abc
  cmdline.readInt('count');   //1
  cmdline.readFlag('flag');   //true

  cmdline.existsProperty('name'); //true
  cmdline.existsProperty('foo');  //true
    #)

  *)
  TCommandLineReader=class
  protected
    parsed: boolean;
    propertyArrayBuffer: array of TProperty;
    propertyCount: SizeInt;
    FPropertySortedCache: TStringList;
    nameless: TStringArray;
    currentDeclarationCategory: String;
    FOnOptionRead: TOptionReadEvent;
    FOnOptionInterpretation: TOptionInterpretationEvent;
    FAllowOverrides: boolean;
    function findProperty(name:string):PProperty;
    function declareProperty(name,description,default:string;kind: TKindOfProperty):PProperty;
    function getPropertySortedCache: TStringList;

    procedure raiseErrorWithHelp(message: string);
    procedure parseSingleValue(var prop: TProperty);


    class function splitCommandLine(s: string; skipFirst: boolean): TStringArray;
  public
    language:TCommandLineReaderLanguage; //not implemented yet
    //** Event called when an invalid option is found during parsing of the command line.
    onShowError: TCommandLineReaderShowError;
    //** Automatically show an error message when the parsing finds an invalid option.
    showErrorAutomatically: boolean;
    //** Allow @code(/name value) option syntax.
    allowDOSStyle: boolean;

    constructor create;
    destructor destroy;override;

    //** Returns a human-readable summary about the declared options. @br
    //** It is automatically printed when an unknown option is given by the user.
    function availableOptions:string;

    //** Resets all options to their default values.
    procedure reset();

    //** Reads the standard command line parameters.
    procedure parse(autoReset: boolean = true);overload;virtual;
    //** Reads the command line parameters from the string s.
    procedure parse(const s:string; skipFirst: boolean = false; autoReset: boolean = true);overload;virtual;
    //** Reads the command line parameters from the array args.
    procedure parse(const args:TStringArray; autoReset: boolean = true);overload;virtual;

    //** Adds a new option category. The category is printed in the --help and availableOptions output
    procedure beginDeclarationCategory(category: string);

    //**DeclareFlag declares a boolean property.                 @br
    //**Example:                                                @br
    //**  @code(declareFlag('flag', 'description', 'f', true);) @br
    //**Following command-line options are then allowed:
    //**@preformatted(
    //**    --enable-flag      =>     flag:=true
    //**    --disable-flag     =>     flag:=false
    //**    --flag             =>     flag:=not default
    //**    -xfy               =>     flag:=not default  (when flags x and y have also been declared)
    //**)
    procedure declareFlag(const name,description:string;flagNameAbbreviation:char;default:boolean=false);overload;
    //**Overloaded @link(DeclareFlag) without a single letter abbreviation.
    procedure declareFlag(const name,description:string;default:boolean=false);overload;


    //**DeclareFile declares a file name property.
    //**
    //**Example:                                                                      @br
    //**  @code(declareFile('file', 'description');)                                  @br
    //**  Following command-line options are then allowed:                            @br
    //**@preformatted(
    //**    --file C:\test                  =>     file:=C:\test
    //**    --file 'C:\test'                =>     file:=C:\test
    //**    --file "C:\test"                =>     file:=C:\test
    //**    --file='C:\test'                =>     file:=C:\test
    //**    --file="C:\test"                =>     file:=C:\test
    //**    --file C:\Program Files\a.exe   =>     file:=C:\Program
    //**                                           or file:=C:\Program Files\a.exe if C:\Program does not exist
    //**)
    procedure declareFile(const name,description:string;default:string='');overload;

    //**DeclareInt declares an integer property.
    //**
    //**Example:                                                    @br
    //**   @code(declareInt('prop', 'description');)                @br
    //**  Following command-line options are then allowed:          @br
    //**  @preformatted(
    //**    --prop 123                  =>     prop:=123
    //**    --prop '123'                =>     prop:=123
    //**    --prop "123"                =>     prop:=123
    //**    --prop='123'                =>     prop:=123
    //**    --prop="123"                =>     prop:=123
    //**   )
    procedure declareInt(const name,description:string;value: longint=0);overload;
    //** DeclareString declares a string property. @br
    //** See declareInt for a related example.
    procedure declareString(const name,description:string;value: string='');overload;
    //** DeclareFloat declares a float property. @br
    //** See declareInt for a related example.
    procedure declareFloat(const name,description:string;value: extended=0);overload;

    //**Allows to use -abbreviation=... additionally to --originalName=... @br
    //**With windows style /abbreviation and /originalName will behave in the same way
    //**(only single letter abbreviations are allowed like in unix commands).
    procedure addAbbreviation(const abbreviation: char; const originalName: string = '');

    //**Only allow certain values for property @code(originalName).
    procedure addEnumerationValues(const originalName: string; const enumeration: array of string);overload;
    //**Only allow certain values for the last property.
    procedure addEnumerationValues(const enumeration: array of string);overload;
  protected
    procedure addEnumerationValues(p: PProperty; const enumeration: array of string);overload;

  public
    //** Reads a previously declared string property.
    function readString(const name:string):string; overload;
    //** Reads a previously declared int property.
    function readInt(const name:string):longint;overload;
    //** Reads a previously declared float property.
    function readFloat(const name:string):extended; overload;
    //** Reads a previously declared boolean property.
    function readFlag(const name:string):boolean;overload;

    //** Tests if a declared property named name has been found on the command line.
    function existsProperty(const name:string):boolean;

    //** Reads all file names that are given on the command line and do not belong to a declared option (doesn't check for non existing files, yet).
    function readNamelessFiles():TStringArray;
    //** Reads all strings that are given on the command line and do not belong to a declared option.
    function readNamelessString():TStringArray;
    //** Reads all integers that are given on the command line and do not belong to a declared option.
    function readNamelessInt():TLongintArray;
    //** Reads all floats that are given on the command line and do not belong to a declared option.
    function readNamelessFloat():TFloatArray;
    //** Reads all booleans (true, false) that are given on the command line and do not belong to a declared option.
    function readNamelessFlag():TBooleanArray;

    //** Event called when an option has been parsed. (e.g. to read all values if an option is given multiple times)
    //** @code(name) contains the declared name of the property (not necessarily the same as the name the user used).
    //** @code(value) the value read.
    property onOptionRead: TOptionReadEvent read FOnOptionRead write FOnOptionRead;
    //** Event  called when an option is being parsed. (e.g. to allow custom abbreviations of names).@br
    //** @code(name) contains the read name of the property.@br
    //** @code(value) the value read; or the next value for boolean options (which will ignored).@br
    //** @code(args) all arguments.@br
    //** @code(argpos) the current argument. @br
    property onCustomOptionInterpretation: TOptionInterpretationEvent read FOnOptionInterpretation write FOnOptionInterpretation;
    //** If the same option may be given multiple times. Only the last value is stored.
    property allowOverrides: boolean read FAllowOverrides write FAllowOverrides;
  end;

implementation

{$ifdef win32}{$define windows}{$endif} //Delphi 4 does not know the windows-define
{$ifdef MSWINDOWS}{$define windows}{$endif} // for 64-bit compilers

uses {$ifdef unitcheck_rcmdline}classes,{$endif}
     {$ifdef windows}windows
     {$else}baseunix,termio {$endif}
     ;

function strJoin(a: TStringArray): string;
var
  i: Integer;
begin
  result := '';
  if length(a) = 0 then exit;
  result := a[0];
  for i := 1 to high(a) do
    result := result + ', ' + a[i];
end;

function test(const s: string): integer;
begin
  result := ord(s = 'XXXX');
end;
procedure TestSub;
begin
  Writeln('TestSub');
end;


{$ifdef fpc}
function equalCaseInseq(const a, b: string): boolean;
begin
  result := SameText(a,b);
end;
{$else}
function equalCaseInseq(const a, b: string): boolean;
begin
  result := (length(a) = length(b)) and (strLiComp(pchar(a), pchar(b), length(a)) = 0);
end;

const LineEnding = #13#10;
{$endif}

function getTerminalWidth: integer;
{$ifdef windows}
var csbi: TCONSOLESCREENBUFFERINFO;
    handle: THANDLE;
{$else}
var winsize: TWinSize;
{$endif}
begin
  result := 80;
  {$ifdef windows}
  handle := GetStdHandle(STD_OUTPUT_HANDLE);
  if handle = INVALID_HANDLE_VALUE then exit;
  if not GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), csbi) then exit;
  result := csbi.srWindow.Right - csbi.srWindow.Left + 1;
  {$else}
  if FpIOCtl(StdOutputHandle, TIOCGWINSZ, @winsize) = 0 then
    result := winsize.ws_col;
  {$endif}
  if result < 10 then result := 80;
end;

constructor TCommandLineReader.create;
begin
  parsed:=false;
  {$IFDEF windows}
    allowDOSStyle:=true;
  {$ELSE}
    allowDOSStyle:=false;
  {$ENDIF}
  language:=clrlEnglish;
  onShowError:=nil;
  {searchNameLessFile:=false;
  searchNameLessInt:=false;
  searchNameLessFloat:=false;
  searchNameLessFlag:=false;}
  showErrorAutomatically:=not IsLibrary;
  FAllowOverrides:=false;
end;
destructor TCommandLineReader.destroy;
begin
  FPropertySortedCache.free;
  inherited;
end;

function TCommandLineReader.availableOptions: string;

  //from bbutils
  function strWrap(const Line: string; MaxCol: Integer; lineBreak: string): string;
  var res: string;
    procedure add(x: string);
    begin
      if res = '' then res := x
      else res := res + lineBreak + x;
    end;

  const BreakChars = [' ',#9];
  var i: integer;
      lastTextStart, lastBreakChance: integer;
      tempBreak: Integer;
  begin
    result := '';
    lastTextStart:=1;
    lastBreakChance:=0;
    result := '';
    for i := 1 to length(line) do begin
      if line[i] in [#13,#10] then begin
        if lastTextStart > i  then continue;
        add(copy(Line,lastTextStart,i-lastTextStart));
        lastTextStart:=i+1;
        if (i < length(line)) and (line[i] <> line[i+1]) and (line[i+1] in [#13, #10]) then inc(lastTextStart);
      end;
      if (i < length(line)) and (line[i+1] in BreakChars) then begin
        lastBreakChance:=i+1;
        if lastTextStart = lastBreakChance then inc(lastTextStart); //merge seveal break characters into a single new line
      end;
      if i - lastTextStart + 1 >= MaxCol then begin
        if lastBreakChance >= lastTextStart then begin
          tempBreak := lastBreakChance;
          while (tempBreak > 1) and  (line[tempBreak-1] in BreakChars) do dec(tempBreak); //remove spaces before line wrap
          add(copy(Line,lastTextStart,tempBreak-lastTextStart));
          lastTextStart:=lastBreakChance+1;
        end else begin
          add(copy(Line, lastTextStart, MaxCol));
          lastTextStart:=i+1;
        end;
      end;
    end;
    if lastTextStart <= length(line) then add(copy(line, lastTextStart, length(line)));
    result := res;
  end;

  function mydup(count: integer): string;
  var
    i: Integer;
  begin
    result := '';
    for i := 1 to count do result := result + ' ';
  end;

var i:integer;
  cur, description, dupped: String;
  j: Integer;
  p: integer;

  names: array of string;
  multiline : boolean;
  maxLen: Integer;
  category: String;
  terminalWidth: Integer;
  pseudoLineBreak: String;
  prop: PProperty;
begin
  names := nil;
  setlength(names, propertyCount);
  maxLen := 0;
  multiline:=false;
  category := '';
  terminalWidth := getTerminalWidth;
  for i:=0 to propertyCount - 1 do begin
    cur:='--'+propertyArrayBuffer[i].name;
    case propertyArrayBuffer[i].kind of
      kpFlag: ;
      kpInt: cur := cur + '=<int> ';
      kpFloat: cur := cur + '=<float> ';
      kpStr: cur := cur + '=<string> ';
      kpFile: cur := cur + '=<file> ';
      else cur:=cur+'=';
    end;
    if propertyArrayBuffer[i].abbreviation<>#0 then cur := cur + ' or -'+propertyArrayBuffer[i].abbreviation;
    names[i] := cur;
    if length(cur) > maxLen then maxLen := length(cur);
    multiline:=multiline or (pos(LineEnding, propertyArrayBuffer[i].description) > 0) or (length(propertyArrayBuffer[i].strenumeration) > 0);
  end;

  dupped := '';
  for j:=1 to maxLen do dupped := dupped + ' ';
             ;

  result:='';
  for i:=0 to propertyCount - 1 do begin
    prop := @propertyArrayBuffer[i];
    if prop.category <> category then begin
      category := prop.category;
      result := result + LineEnding + LineEnding + category + LineEnding+LineEnding;
    end;
    cur:=names[i];
    if category <> '' then cur := '  ' + cur;


    pseudoLineBreak := LineEnding+dupped;
    if category <> '' then pseudoLineBreak := pseudoLineBreak + ' ';
    pseudoLineBreak := pseudoLineBreak + #9;

    description := prop.description;
    if (prop.strvalueDefault <> '') and (prop.strvalueDefault <> '0') then
      description += ' (default: '+prop.strvalueDefault+')';
    if length(prop.strenumeration) > 0 then
      description := description + LineEnding + 'Allowed values: ' + strJoin(prop.strenumeration);
    if (not multiline or ( pos(LineEnding, description) = 0 )) and (length(description)+maxLen+10 < terminalWidth)  then
       cur := cur + mydup(maxLen - length(cur)) + #9 + description + LineEnding
    else begin
      cur := cur + mydup(maxLen - length(cur));
      p := pos(LineEnding, description);
      while p > 0 do begin
        cur := cur + #9 + strWrap(copy(description, 1, p - 1), terminalWidth - 10 - maxLen, pseudoLineBreak) + LineEnding + dupped;
        if category <>' ' then cur := cur + '  ';
        delete(description, 1, p + length(LineEnding) - 1);
        p := pos(LineEnding, description);
      end;
      cur := cur + #9 + strWrap(description, terminalWidth - 10 - maxLen, pseudoLineBreak) + LineEnding;
    end;
    result:=result+cur;
  end;
end;

procedure TCommandLineReader.reset();
var
  i: Integer;
begin
  SetLength(nameless,0);
  for i:=0 to propertyCount - 1 do begin
    with propertyArrayBuffer[i] do begin
      if found then begin
        found:=false;
        case kind of
          kpStr,kpFile: strvalue := strvalueDefault;
          kpInt: intvalue:=intvalueDefault;
          kpFloat: floatvalue:=floatvalueDefault;
          kpFlag: flagvalue:=flagdefault;
        end;
      end;
    end;
  end;
end;


procedure TCommandLineReader.parse(autoReset: boolean = true);
{$ifndef windows}
var args: TStringArray;
  i: Integer;
{$endif}
begin
  if Paramcount = 0 then exit;

  {$ifdef windows}
  parse(string(getcommandline), true, autoReset);
  {$else}
  args := nil;
  setlength(args, Paramcount);
  for i:=0 to high(args) do args[i] := paramstr(i+1);
  parse(args, autoReset);
  {$endif}
end;

procedure TCommandLineReader.parse(const s:string; skipFirst: boolean = false; autoReset: boolean = true);
var
  args: TStringArray;
begin
  args := splitCommandLine(s, skipFirst);
  parse(args, autoReset);
end;

procedure TCommandLineReader.parse(const args: TStringArray; autoReset: boolean = true);
var a: string;

  procedure raiseError(message: string);
  begin
    raiseErrorWithHelp('Error: '+message+LineEnding+'(error occured when reading argument: '+a+')');
  end;

  procedure raiseNoProperty(name: string);
  begin
    if (name = 'help') or (name = '?') then raiseErrorWithHelp('')
    else raiseError('Unknown option: '+name);
  end;


  function findPropertyIndex(const name: string; allowLong, allowAbbreviation, allowMissing: boolean): integer;
  var
    i: Integer;
  begin
    if allowLong then begin
      i := getPropertySortedCache.IndexOf(name);
      if i >= 0 then begin
        result := ptrint(FPropertySortedCache.Objects[i]);
        exit;
      end;
    end;

    if allowAbbreviation then
      for i:=0 to propertyCount - 1 do
        if propertyArrayBuffer[i].abbreviation = name then begin
          result:=i;
          exit;
        end;

    if allowMissing then result := -1
    else raiseNoProperty(name)
  end;


var argpos: Integer;

  procedure setPropertyFromStringValue(currentPropertyIndex: integer; value: string);
  var
    i: Integer;
    currentProperty: PProperty;
    found: Boolean;
  begin
    currentProperty := @propertyArrayBuffer[currentPropertyIndex];
    if length(currentProperty^.strenumeration) > 0 then begin
      found := false;
      for i := 0 to high(currentProperty^.strenumeration) do
        if currentProperty^.strenumeration[i] = value then begin
          found := true;
          break;
        end;
      if not found then raiseError('Invalid value for: '+currentProperty^.name + LineEnding + 'Allowed values are: ' + strjoin(currentProperty^.strenumeration));
    end;
    currentProperty^.strvalue := value;
    if (currentProperty^.kind = kpFile) then begin
      for i := 0 to length(args) - argpos do begin
        if FileExists(value) then begin
          inc(argpos, i);
          currentProperty^.strvalue := value;
          break;
        end;
        if i = length(args) - argpos then break; //not found
        value := value + ' ' + args[argpos + i];
      end;
    end else parseSingleValue(currentProperty^);
    if not FAllowOverrides and currentProperty^.found then raiseError('Duplicated option: '+currentProperty^.name);
    currentProperty^.found:=true;
    if assigned(onOptionRead) then onOptionRead(self,currentProperty^.name, currentProperty^.strvalue);
  end;

  function invertedFlag(flagId: integer): string;
  begin
    if propertyArrayBuffer[flagId].flagvalue then result := 'false' else result := 'true';
  end;

var currentProperty:longint;
    i:integer;
    index: integer;
    name: String;                          
    value: String;
    j: Integer;
    noFlagExpansion: Boolean;
    allowAbbreviation: Boolean;
    weAreDoneInterpreting: Boolean;
begin
  if autoReset then reset();

  parsed:=true; //mark as parsed, so readXXX can be used within the event called by onOptionRead
  weAreDoneInterpreting := false;

  argpos := 0;
  while argpos < length(args) do begin
    a := args[argpos];
    inc(argpos);
    if a = '' then continue;
    if a = '--' then begin
      if Assigned(FOnOptionInterpretation) then begin
        value := '';
        FOnOptionInterpretation(self, a, value, args, argpos);
        if a <> '--' then continue;
      end;
      weAreDoneInterpreting:=true;
      continue;
    end;
    allowAbbreviation := true; //for special handling of DOS style args. /x is prefered to be --x but can fallback to abbreviated -x
    if not weAreDoneInterpreting and (a <> '-') and (a <> '--')
       and ((a[1] = '-') or (allowDOSStyle and (a[1]='/'))) then begin
      //Start of property name
      if (length(a) > 1) and ((a[1]='/') or (a[2]='-') ) then begin //long property
        if (a[2]='-') then begin
          delete(a, 1, 2);
          allowAbbreviation := false;
        end else delete(a, 1, 1);
        if a = '' then continue;

        if (StrLIComp(@a[1],'enable-',7) = 0)or
           (StrLIComp(@a[1],'disable-',8) = 0)  then begin
          //long flag
          if a[1]='e' then begin
            delete(a, 1, 7);
            value := 'true';
          end else begin
            delete(a, 1, 8);
            value := 'false';
          end;
          if (propertyArrayBuffer[findPropertyIndex(a, true, false, false)].kind <> kpFlag) then raiseError('No flag: '+a);
          a := a + '=' + value; //this will be split again in the next step, but simplifies the code
        end;
      end else begin
        noFlagExpansion := false;
        for j:=2 to length(a) do begin //2 to skip leading -
          i:=findPropertyIndex(a[j], false, true, false);
          if propertyArrayBuffer[i].kind=kpFlag then begin
            setPropertyFromStringValue(i, invertedFlag(i));
          end else if (j = length(a)) or (a[j+1] = '=') then begin
            noFlagExpansion := true;
            a := propertyArrayBuffer[i].name + copy(a, j+1, length(a) - j);
            break
          end else raiseError('Invalid abbreviation: '+a[j]+ LineEnding +'(use -- or / for arguments)');
        end;
        if not noFlagExpansion then continue;
      end;
      //a now contains a long property something or something=value
      index := pos('=', a);
      if index > 0 then begin
        name := copy(a, 1, index - 1);
        value := copy(a, index + 1, length(a) - index);
        currentProperty := findPropertyIndex(name, true, allowAbbreviation, true);
        if currentProperty >= 0 then name := propertyArrayBuffer[currentProperty].name;
      end else begin
        name := a;
        currentProperty := findPropertyIndex(name, true, allowAbbreviation, true);
        if currentProperty >= 0 then name := propertyArrayBuffer[currentProperty].name;
        if (currentProperty >= 0) and (propertyArrayBuffer[currentProperty].kind = kpFlag) then value := invertedFlag(currentProperty)
        else if (argpos < length(args)) then begin
          value := args[argpos];
          inc(argpos);
        end else value := '';
      end;

      if Assigned(FOnOptionInterpretation) then FOnOptionInterpretation(self, name, value, args, argpos);

      j := findPropertyIndex(name, true, false, false);
      if (index = 0) and (value = '') and (argpos >= length(args)) then
        raiseError('No value for option '+name+' given');

      setPropertyFromStringValue(j, value);
    end else begin
      if not weAreDoneInterpreting and Assigned(FOnOptionInterpretation) then begin
        name := '';
        FOnOptionInterpretation(self, name, a, args, argpos);
        if name <> '' then begin
          setPropertyFromStringValue(findPropertyIndex(name, true, false, false), value);
          continue;
        end;
      end;
      //value without variable name
      SetLength(nameless,length(nameless)+1);
      nameless[high(nameless)] := a;
      if assigned(onOptionRead) then onOptionRead(self,'', a);
    end;
  end;

  {debug things: for i:= 0 to high(propertyArray) do
  if propertyArray[i].found then begin
    write(propertyArray[i].name , ' => ', propertyArray[i].strvalue);
    if propertyArray[i].kind =kpFlag then writeln( '(',propertyArray[i].flagvalue,')')
    else writeln;
  end;
  for i:= 0 to high(nameless) do writeln('no: ', nameless[i]);}

end;

procedure TCommandLineReader.beginDeclarationCategory(category: string);
begin
  currentDeclarationCategory := category;
end;

function TCommandLineReader.findProperty(name:string):PProperty;
var i:integer;
begin
  i := getPropertySortedCache.IndexOf(name);
  if i < 0 then
    raise ECommandLineParseException.Create('Property not found: '+name);
  result := @propertyArrayBuffer[ptrint(FPropertySortedCache.Objects[i])];
end;

function TCommandLineReader.declareProperty(name,description,default:string;kind: TKindOfProperty):PProperty;
begin
  propertyCount := propertyCount + 1;
  if length(propertyArrayBuffer) < propertycount then
    if length(propertyArrayBuffer) < 4 then SetLength(propertyArrayBuffer, 4)
    else if length(propertyArrayBuffer) < 64 then SetLength(propertyArrayBuffer, length(propertyArrayBuffer) * 2)
    else SetLength(propertyArrayBuffer, length(propertyArrayBuffer) + 64);
  result:=@propertyArrayBuffer[propertyCount - 1];
  result^.category:=currentDeclarationCategory;
  result^.name:=lowercase(name);
  result^.description:=description;
  result^.strvalue:=default;
  result^.strvalueDefault:=default;
  result^.kind:=kind;
end;

function TCommandLineReader.getPropertySortedCache: TStringList;
var
  i: sizeint;
begin
  if FPropertySortedCache = nil then begin
    FPropertySortedCache := TStringList.Create;
    FPropertySortedCache.Options:=[];
    FPropertySortedCache.CaseSensitive := true;
    FPropertySortedCache.OwnsObjects:=false;
  end;
  result := FPropertySortedCache;
  if FPropertySortedCache.Count = propertyCount then exit;
  FPropertySortedCache.Clear;
  FPropertySortedCache.Sorted := false;
  for i := 0 to propertyCount - 1 do
    FPropertySortedCache.AddObject(propertyArrayBuffer[i].name, tobject(i));
  FPropertySortedCache.Sorted := true;
end;

procedure TCommandLineReader.raiseErrorWithHelp(message: string);
var errorMessage: string;
begin
  if assigned(onShowError) or showErrorAutomatically then begin
    errorMessage:=message+LineEnding;
    if propertyCount=0 then
      errorMessage:=errorMessage+LineEnding+LineEnding+'You are not allowed to use command line options starting with -'
     else
      errorMessage:=errorMessage+ LineEnding+LineEnding+'The following command line options are valid: '+LineEnding+LineEnding+ availableOptions;
  end;

  if assigned(onShowError) then
    onShowError(self,errorMessage);
  if showErrorAutomatically then begin
    if system.IsConsole then begin
      writeln(errorMessage);
      halt;
    end;
     {else
      ShowMessage(errorMessage);} //don't want to link against showMessage in console applications.
  ;
  end;
  raise ECommandLineParseException.create(message);
end;

procedure TCommandLineReader.parseSingleValue(var prop: TProperty);
begin
  try
    case prop.kind of
      kpInt: prop.intvalue:=StrToInt(prop.strvalue);
      kpFloat:  prop.floatvalue:=StrToFloat(prop.strvalue);
      kpFlag: begin
        prop.flagvalue:=equalCaseInseq(prop.strvalue, 'true');
        if not prop.flagvalue and not equalCaseInseq(prop.strvalue, 'false') then
          raiseErrorWithHelp('Only "true" and "false" are valid flag values for option '+prop.name);
      end;
    end;
  except
    raiseErrorWithHelp('Invalid value: '+prop.strvalue+' for option '+prop.name);
  end;
end;

class function TCommandLineReader.splitCommandLine(s: string; skipFirst: boolean): TStringArray;
var args: TStringArray;
  cmd: pchar;
  marker: pchar;
  stringstart: Char;
  hasEscapes, newArgument: boolean;

  const SPACE = [' ',#9];

  procedure pushMarked;
  var
    addLen: longint;
  begin
    if marker = nil then exit;
    if skipFirst then begin
      skipFirst:=false;
      marker := nil;
      exit;
    end;
    if newArgument then begin
      setlength(args, length(args)+1);
      newArgument:=false;
    end;
    addLen := cmd - marker;
    if addLen <= 0 then begin
      marker := nil;
      exit;
    end;
    setlength(args[high(args)], length(args[high(args)]) + addLen);
    move(marker^, args[high(args)][ length(args[high(args)]) - addLen + 1 ], SizeOf(Char) * addLen);
    if hasEscapes then begin
      args[high(args)] := StringReplace(StringReplace(args[high(args)], '\'+stringstart, stringstart, [rfReplaceAll]),
                                                                        '\\', '\', [rfReplaceAll]); //todo: are these all cases
      hasEscapes := false;
    end;
    marker := nil;
  end;
var backslashCount: integer;
begin
  if s = '' then exit;
  cmd := @s[1];
  marker := nil;
  newArgument := true;
  hasEscapes := false;
  while true do begin
    case cmd^ of
      ' ', #9, #0: begin
        pushMarked;
        while cmd^ in SPACE do inc(cmd);
        if cmd^ = #0 then break;
        newArgument := true;
      end;
      '"', '''': begin
        pushMarked;
        stringstart := cmd^;
        inc(cmd);
        marker:=cmd;
        backslashCount:=0; hasEscapes := false;
        while ((cmd^ <> stringstart) or (odd(backslashCount))) and (cmd^ <> #0) do begin
          if cmd^ = '\' then inc(backslashCount)
          else backslashCount:=0;
          if cmd^ = stringstart then     //Special handling of escapes (see below)
            hasEscapes:=true;
          inc(cmd);
        end;
        pushMarked;
        if cmd^ = #0 then break;
        inc(cmd);
      end;
      '\': begin
        //Special handling of escapes:
        //    Only replace \\ by \, if there is also a \" or \'
        //    So you can e.g. use \\127.0.0.1\DIR on windows
        //                    as well as a\"b to escape a "
        if marker = nil then marker := cmd;
        inc(cmd);
        if cmd^ in ['"', ''''] then begin
          stringstart:=cmd^;
          inc(cmd);
          hasEscapes:=true;
          pushMarked;
        end else if not (cmd^ in (SPACE+[#0])) then inc(cmd);
      end;
      else begin
        if marker = nil then marker := cmd;
        inc(cmd);
      end;
    end;
  end;
  result := args;
end;

procedure TCommandLineReader.declareFlag(const name,description:string;flagNameAbbreviation:char;default:boolean=false);
begin
  with declareProperty(name,description,'',kpFlag)^ do begin
    flagvalue:=default;
    flagdefault:=default;
    abbreviation:=flagNameAbbreviation;
    if default then strvalueDefault:='true';
  end;
end;
procedure TCommandLineReader.declareFlag(const name,description:string;default:boolean=false);
begin
  declareFlag(name,description,#0,default);
end;

procedure TCommandLineReader.declareFile(const name,description:string;default:string='');
begin
  declareProperty(name,description,default,kpFile)
end;

procedure TCommandLineReader.declareString(const name,description:string;value: string='');
begin
  declareProperty(name,description,value,kpStr)
end;
procedure TCommandLineReader.declareInt(const name,description:string;value: longint=0);
begin
  if value<>0 then
    with declareProperty(name,description,IntToStr(value),kpInt)^ do begin
      intvalue:=value;
      intvalueDefault:=intvalue;
    end
  else  with declareProperty(name,description,IntToStr(value),kpInt)^ do begin
    intvalue:=value;
    intvalueDefault:=intvalue;
  end;
end;
procedure TCommandLineReader.declareFloat(const name,description:string;value: extended=0);
begin
  with declareProperty(name,description,FloatToStr(value),kpFloat)^ do begin
    floatvalue:=value;
    floatvalueDefault:=value;
  end;
end;

procedure TCommandLineReader.addAbbreviation(const abbreviation: char; const originalName: string = '');
begin
  if originalName <> '' then
    findProperty(originalName)^.abbreviation:=abbreviation
   else begin
     if propertyCount = 0 then raise ECommandLineParseException.Create('No properties defined');
     propertyArrayBuffer[propertyCount - 1].abbreviation:=abbreviation;
   end;
end;

procedure TCommandLineReader.addEnumerationValues(const originalName: string; const enumeration: array of string);
begin
  addEnumerationValues(findProperty(originalName), enumeration);
end;

procedure TCommandLineReader.addEnumerationValues(const enumeration: array of string);
begin
  if propertyCount = 0 then raise ECommandLineParseException.Create('No properties defined');
  addEnumerationValues(@propertyArrayBuffer[propertyCount - 1], enumeration);
end;

procedure TCommandLineReader.addEnumerationValues(p: PProperty; const enumeration: array of string);
var
  i: Integer;
begin
  setlength(p^.strenumeration, length(enumeration));
  for i := 0 to high(enumeration) do p^.strenumeration[i] := enumeration[i];
end;

function TCommandLineReader.readString(const name:string):string;
begin
  if not parsed then parse;
  result:=findProperty(name)^.strvalue;
end;
function TCommandLineReader.readInt(const name:string):longint;
var prop: PProperty;
begin
  if not parsed then parse;
  prop:=findProperty(name);
  if prop^.kind<>kpInt then raise ECommandLineParseException.create('No integer property: '+name);
  result:=prop^.intvalue;
end;
function TCommandLineReader.readFloat(const name:string):extended;
var prop: PProperty;
begin
  if not parsed then parse;
  prop:=findProperty(name);
  if prop^.kind<>kpFloat then raise ECommandLineParseException.create('No extended property: '+name);
  result:=prop^.Floatvalue;
end;
function TCommandLineReader.readFlag(const name:string):boolean;
var prop: PProperty;
begin
  if not parsed then parse;
  prop:=findProperty(name);
  if prop^.kind<>kpFlag then raise ECommandLineParseException.create('No flag property: '+name);
  result:=prop^.flagvalue;
end;

function TCommandLineReader.existsProperty(const name:string):boolean;
begin
  if not parsed then parse;
  result:=findProperty(name)^.found;
end;

function TCommandLineReader.readNamelessFiles():TStringArray;
begin
  Result:=nameless;
end;
function TCommandLineReader.readNamelessString():TStringArray;
begin
  result:=nameless;
end;
function TCommandLineReader.readNamelessInt():TLongintArray;
var i,p:integer;
begin
  result := nil;
  SetLength(result,length(nameless));
  p:=0;
  for i:=0 to high(nameless) do
    try
      result[p]:=StrToInt(nameless[i]);
      inc(p);
    except
    end;
  SetLength(result,p);
end;
function TCommandLineReader.readNamelessFloat():TFloatArray;
var i,p:integer;
begin
  result := nil;
  SetLength(result,length(nameless));
  p:=0;
  for i:=0 to high(nameless) do
    try
      result[p]:=StrToFloat(nameless[i]);
      inc(p);
    except
    end;
  SetLength(result,p);
end;
function TCommandLineReader.readNamelessFlag():TBooleanArray;
var i,p:integer;
begin
  result := nil;
  SetLength(result,length(nameless));
  p:=0;
  for i:=0 to high(nameless) do begin
    if lowercase(nameless[i])='true' then Result[p]:=true
    else if lowercase(nameless[i])='false' then Result[p]:=false
    else dec(p);
    inc(p);
  end;
  SetLength(result,p);
end;


{$ifdef unitcheck_rcmdline}
var cmdLineReader: TCommandLineReader;
    tsl: tstringlist;
  procedure say(s: string);
  begin
    if IsConsole then writeln(s)
    //else ShowMessage(s);
  end;

var cmdlinetest: integer = 0;
  procedure testSplitCommandLineRaw(line: string; skipFirst: boolean; expected: array of string);
  var
    args: TStringArray;
    i: Integer;
    ok: boolean;
  begin
    args := TCommandLineReader.splitCommandLine(line, skipFirst);
    ok := true;
    cmdlinetest := cmdlinetest + 1;
    if length(args) <> length(expected) then begin
      ok := false;
    end;
    if ok then
    for i:=0 to high(args) do
      if (args[i] <> expected[i]) then begin
        ok := false;
      end;
    if not ok then begin
      writeln(cmdlinetest, ' (',line,') failed ');
      write('  Got:     '#9);
      for i := 0 to high(args) do write('>', args[i], '<,'#9);
      writeln;
      write('  Expected:'#9);
      for i := 0 to high(expected) do write('>',expected[i],'<,'#9);
      writeln;
    end;
  end;

  procedure testSplitCommandLine(line: string; expected: array of string);
  var temp: array of string;
    i: Integer;
  begin
    testSplitCommandLineRaw(line, false, expected);
    testSplitCommandLineRaw('  '+line, false, expected);
    testSplitCommandLineRaw('  '#9+line, false, expected); //can't test trailing withspace, since there are unclosed quotes

    temp := nil;
    setlength(temp, length(expected)-1);
    if length(temp) > 0 then begin
      for i:=0 to high(temp) do temp[i] := expected[i+1];
      testSplitCommandLineRaw(line, true, temp);
      testSplitCommandLineRaw('  '#9+line, true, temp);
    end;
  end;

begin
  Formatsettings.DecimalSeparator:='.';

  testSplitCommandLine('abc',            ['abc']);
  testSplitCommandLine('abc def',        ['abc', 'def']);
  testSplitCommandLine('abc "def"',      ['abc', 'def']);
  testSplitCommandLine('abc "d ef"',     ['abc', 'd ef']);
  testSplitCommandLine('abc "''def''"',  ['abc', '''def''']);
  testSplitCommandLine('abc ''"de f"''', ['abc', '"de f"']);
  testSplitCommandLine('abc   ''"de f"''', ['abc', '"de f"']);
  testSplitCommandLine('abc   foo" "bar',  ['abc', 'foo bar']);
  testSplitCommandLine('abc   foo''  ''bar',  ['abc', 'foo  bar']);
  testSplitCommandLine('abc'#9'foo" "bar',  ['abc', 'foo bar']);
  testSplitCommandLine('abc   foo"'#9'"bar',  ['abc', 'foo'#9'bar']);
  testSplitCommandLine('1 2  3 4',  ['1', '2', '3', '4']);
  testSplitCommandLine('A  haus"maus"  Z',  ['A', 'hausmaus', 'Z']);
  testSplitCommandLine('A  haus''maus''  Z',  ['A', 'hausmaus', 'Z']);
  testSplitCommandLine('A  haus""maus  Z',  ['A', 'hausmaus', 'Z']);
  testSplitCommandLine('A  haus''''maus  Z',  ['A', 'hausmaus', 'Z']);
  testSplitCommandLine('"un closed',  ['un closed']);
  testSplitCommandLine('''un closed',  ['un closed']);
  testSplitCommandLine('un closed"',  ['un' , 'closed']);
  testSplitCommandLine('un closed''',  ['un' , 'closed']);
  testSplitCommandLine('a "" b', ['a', '', 'b']);
  testSplitCommandLine('a '''' b', ['a', '', 'b']);
  //backslashes (see special case documentation in split)
  testSplitCommandLine('a "\" b', ['a', '" b']);
  testSplitCommandLine('a "\\" b', ['a', '\\', 'b']);   //special case!
  testSplitCommandLine('a "\\\" b', ['a', '\" b']);
  testSplitCommandLine('a ''\'' b', ['a', ''' b']);
  testSplitCommandLine('a ''\\'' b', ['a', '\\', 'b']); //special case!
  testSplitCommandLine('a ''\\\'' b', ['a', '\'' b']);
  testSplitCommandLine('a \ b', ['a', '\' , 'b']);
  testSplitCommandLine('a \\ b', ['a', '\\', 'b']);
  testSplitCommandLine('a \\\\ b', ['a', '\\\\', 'b']);
  testSplitCommandLine('a \" b', ['a', '"', 'b']);
  testSplitCommandLine('a \\" b', ['a', '\\ b']);
  testSplitCommandLine('a \\\" b', ['a', '\"', 'b']);
  testSplitCommandLine('a \\\\" b', ['a', '\\\\ b']);
  testSplitCommandLine('a \\\\\" b', ['a', '\\"', 'b']);
  testSplitCommandLine('a \', ['a', '\']);
  testSplitCommandLine('a \\', ['a', '\\']);
  testSplitCommandLine('a \"', ['a', '"']);
  testSplitCommandLine('a \\"', ['a', '\\']);
  testSplitCommandLine('a \\\"', ['a', '\"']);

  cmdLineReader:=TCommandLineReader.create;
  cmdLineReader.allowDOSStyle:=true;
  cmdLineReader.declareFlag('flag1','Test Flag 1');
  cmdLineReader.declareFlag('flag2','Test Flag 2');
  cmdLineReader.declareFlag('flag3','Well, you know, what this is');
  cmdLineReader.declareFlag('flag4','');
  cmdLineReader.declareFlag('flag5','');
  cmdLineReader.declareFlag('flag6','','a');
  cmdLineReader.declareFlag('flag7','','b');
  cmdLineReader.declareFlag('flag8','','c');
  cmdLineReader.declareFlag('flag9','','d');
  cmdLineReader.parse('--flag1 --enable-flag2 --disable-flag3 /flag5 -abc');
  if (cmdLineReader.readFlag('flag1')<>true) or
     (cmdLineReader.readFlag('flag2')<>true) or
     (cmdLineReader.readFlag('flag3')<>false) or
     (cmdLineReader.readFlag('flag4')<>false) or
     (cmdLineReader.readFlag('flag5')<>true) or
     (cmdLineReader.readFlag('flag6')<>true) or
     (cmdLineReader.readFlag('flag7')<>true) or
     (cmdLineReader.readFlag('flag8')<>true) or
     (cmdLineReader.readFlag('flag9')<>false) then
    say('test 1 (boolean flag test) failed')
   else
    say('test 1 (boolean flag test) passed');
  cmdLineReader.free;

  cmdLineReader:=TCommandLineReader.create;
  cmdLineReader.allowDOSStyle:=true;
  cmdLineReader.declareInt('p1','',1);
  cmdLineReader.declareInt('p2','',2);
  cmdLineReader.declareInt('p3','',3);
  cmdLineReader.declareInt('p4','',4);
  cmdLineReader.declareInt('p5','',5);
  cmdLineReader.declareInt('p6','',6);
  cmdLineReader.parse('--p1=42 --p3 2124 713 --p5="123"   /p4   ''100'' ');
  if (cmdLineReader.readInt('p1')<>42) or
     (cmdLineReader.readInt('p2')<>2) or
     (cmdLineReader.readInt('p3')<>2124) or
     (cmdLineReader.readInt('p4')<>100) or
     (cmdLineReader.readInt('p5')<>123) or
     (cmdLineReader.readInt('p6')<>6) or
     (length(cmdLineReader.readNamelessString())<>1) or
     (length(cmdLineReader.readNamelessInt())<>1) or
     (cmdLineReader.readNamelessInt()[0]<>713)
  then
    say('test 2 (int test) failed')
   else
    say('test 2 (int test) passed');
  cmdLineReader.free;

  cmdLineReader:=TCommandLineReader.create;
  cmdLineReader.allowDOSStyle:=false; //!!
  cmdLineReader.declareFloat('p1','',1);
  cmdLineReader.declareFloat('p2','',2);
  cmdLineReader.declareFloat('p3','',3);
  cmdLineReader.declareFloat('p4','',4);
  cmdLineReader.declareFloat('p5','',5);
  cmdLineReader.declareFloat('p6','',6);
  cmdLineReader.parse('--p1=4.2 /p2=20.3 --p3 "443.2" some dummy string ' {+ '--p4=''2.2'''  }+ ' --p4=5');
  if (abs(cmdLineReader.readFloat('p1')-4.2)>1e-4) or
     (abs(cmdLineReader.readFloat('p2')-2)>1e-4) or
     (abs(cmdLineReader.readFloat('p3')-443.2)>1e-4) or
     (abs(cmdLineReader.readFloat('p4')-5)>1e-4) or
     (abs(cmdLineReader.readFloat('p5')-5)>1e-4) or
     (abs(cmdLineReader.readFloat('p6')-6)>1e-4)
  then
    say('test 3 (Float test) failed')
   else
    say('test 3 (Float test) passed');
  cmdLineReader.free;

  cmdLineReader:=TCommandLineReader.create;
  cmdLineReader.allowDOSStyle:=true;
  cmdLineReader.declareString('p1','','s1');
  cmdLineReader.declareString('p2','','s2');
  cmdLineReader.declareString('p3','','s3');
  cmdLineReader.declareString('p4','','s4');
  cmdLineReader.declareString('p5','','s5');
  cmdLineReader.declareString('p6','','s6');
  cmdLineReader.declareString('p7','','s7');
  cmdLineReader.parse('--p1="Hallo Welt!" --p2=''test string'' --p3=''p4="123"'' --p5=string /p6 abc --p7 "I''m testing"');
  if (cmdLineReader.readString('p1')<>'Hallo Welt!') or
     (cmdLineReader.readString('p2')<>'test string') or
     (cmdLineReader.readString('p3')<>'p4="123"') or
     (cmdLineReader.readString('p4')<>'s4') or
     (cmdLineReader.readString('p5')<>'string') or
     (cmdLineReader.readString('p6')<>'abc') or
     (cmdLineReader.readString('p7')<>'I''m testing')
  then
    say('test 4 (string test) failed')
   else
    say('test 4 (string test) passed');
  cmdLineReader.free;

  tsl:=TStringList.create;
  tsl.SaveToFile('test file 234234');
  tsl.free;
  cmdLineReader:=TCommandLineReader.create;
  cmdLineReader.allowDOSStyle:=true;
  cmdLineReader.declareFile('file1','','f1');
  cmdLineReader.declareFile('file2','','f2');
  cmdLineReader.parse('--file1=test file 234234 --file2="file not found ???"');
  if (cmdLineReader.readString('file1')<>'test file 234234') or
     (cmdLineReader.readString('file2')<>'file not found ???')
  then
    say('test 5 (file test) failed')
   else
    say('test 5 (file test) passed');
  cmdLineReader.free;
  DeleteFile('test file 234234');


  //abbreviation test
  cmdLineReader:=TCommandLineReader.create;
  cmdLineReader.allowDOSStyle:=true;
  cmdLineReader.declareFlag('f1','','f');
  cmdLineReader.declareFlag('f2','','g');
  cmdLineReader.declareString('s0','','init');
  cmdLineReader.declareString('s1','',''); cmdLineReader.addAbbreviation('t');
  cmdLineReader.declareString('s2','',''); cmdLineReader.addAbbreviation('u');
  cmdLineReader.addAbbreviation('s', 's0');

  cmdLineReader.parse('-f -g -s "abc" /t def -u=foobar ');
  if (cmdLineReader.readString('s0')<>'abc') or
     (cmdLineReader.readString('s1')<>'def') or
     (cmdLineReader.readString('s2')<>'foobar') or
     (not cmdLineReader.readFlag('f1')) or
     (not cmdLineReader.readFlag('f2'))
  then say('test 6a (abbreviation test) failed')
  else say('test 6a (abbreviation test) passed');


  cmdLineReader.parse('/f');
  if (cmdLineReader.readString('s0')<>'init') or
     (cmdLineReader.readString('s1')<>'') or
     (cmdLineReader.readString('s2')<>'') or
     (not cmdLineReader.readFlag('f1')) or
     (cmdLineReader.readFlag('f2'))
  then say('test 6b (repeated test) failed')
  else say('test 6b (repeated test) passed');

    cmdLineReader.parse('/g /u=''xyz''');
  if (cmdLineReader.readString('s0')<>'init') or
     (cmdLineReader.readString('s1')<>'') or
     (cmdLineReader.readString('s2')<>'xyz') or
     (cmdLineReader.readFlag('f1')) or
     (not cmdLineReader.readFlag('f2'))
  then say('test 6c (repeated test) failed')
  else say('test 6c (repeated test) passed');

  cmdLineReader.free;




  cmdLineReader:=TCommandLineReader.create;
  cmdLineReader.allowDOSStyle:=true;
  cmdLineReader.declareString('s0','','init');
  cmdLineReader.declareString('s1','','init');
  cmdLineReader.declareString('s2','','init');
  cmdLineReader.parse('--s0"=ab''c" --s"1=def" --s2="foo\"\''bar" ');
  if (cmdLineReader.readString('s0') <> 'ab''c') or
     (cmdLineReader.readString('s1') <> 'def') or
     (cmdLineReader.readString('s2') <> 'foo"\''bar')
     then say('test 7a (double quotes) failed')
     else say('test 7a (double quotes) passed');

  cmdLineReader.parse('--s0''=abc"2'' --s''1=def2'' --s2=''foo\"\''bar''');
  if (cmdLineReader.readString('s0') <> 'abc"2') or
     (cmdLineReader.readString('s1') <> 'def2') or
     (cmdLineReader.readString('s2') <> 'foo\"''bar')
     then say('test 7b (single quotes) failed')
     else say('test 7b (single quotes) passed');

  cmdLineReader.parse('--s0=""abc3"" --s1=''''def3'''' --s2=te''"''st');
  if (cmdLineReader.readString('s0') <> 'abc3') or
     (cmdLineReader.readString('s1') <> 'def3') or
     (cmdLineReader.readString('s2') <> 'te"st')
     then say('test 7c failed')
     else say('test 7c passed');

  cmdLineReader.Free;


  cmdLineReader:=TCommandLineReader.create;
  cmdLineReader.allowDOSStyle:=true;
  cmdLineReader.declareString('s','','init');
  cmdLineReader.declareString('sconfusion', 'x', 'x'); cmdLineReader.addAbbreviation('s');
  cmdLineReader.declareFlag('flag1', 'f', 'f');
  cmdLineReader.declareFlag('flag2', 'g', 'g');
  cmdLineReader.declareFlag('flag3', 'h', 'h');
  cmdLineReader.declareString('xyztemp','','init'); cmdLineReader.addAbbreviation('a');
  cmdLineReader.parse('--s "no abbr" -fha arg7 -s "this is abbrv"');
  if (cmdLineReader.readString('s') <> 'no abbr') or
     (cmdLineReader.readString('xyztemp') <> 'arg7') or
     (cmdLineReader.readFlag('flag1') <> true) or
     (cmdLineReader.readFlag('flag2') <> false) or
     (cmdLineReader.readFlag('flag3') <> true) or
     (cmdLineReader.readString('sconfusion') <> 'this is abbrv')
     then say('test 8 (abbrv) failed')
     else say('test 8 (abbrv) passed');

  cmdLineReader.parse('/s="no abbr2" -ga=arg7b -s="this is abbrv2" /h');
  if (cmdLineReader.readString('s') <> 'no abbr2') or
     (cmdLineReader.readString('xyztemp') <> 'arg7b') or
     (cmdLineReader.readFlag('flag1') <> false) or
     (cmdLineReader.readFlag('flag2') <> true) or
     (cmdLineReader.readFlag('flag3') <> true) or
     (cmdLineReader.readString('sconfusion') <> 'this is abbrv2')
     then say('test 8b (abbrv) failed')
     else say('test 8b (abbrv) passed');

  cmdLineReader.Free;

  cmdLineReader:=TCommandLineReader.create;
  cmdLineReader.declareString('s','a','init');
  cmdLineReader.declareString('t', 'b', 'init');
  cmdLineReader.declareString('u','c','init');
  cmdLineReader.declareString('v', 'd', 'init');
  cmdLineReader.parse('--s --t -- --u --v');
  if (cmdLineReader.readString('s') <> '--t') or
     (cmdLineReader.readString('t') <> 'init') or
     (cmdLineReader.readString('u') <> 'init') or
     (cmdLineReader.readString('v') <> 'init')
     then say('test 9 (protection) failed')
     else say('test 9 (protection) passed');
  cmdLineReader.Free;


  say('rcmdline unit test completed');
{$endif}
end.

