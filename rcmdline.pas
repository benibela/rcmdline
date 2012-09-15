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
)*)
unit rcmdline;


interface
{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

//{$define unitcheck_rcmdline}

uses sysutils; //for exceptions
type
  TStringArray=array of string;
  TLongintArray=array of longint;
  TFloatArray=array of extended;
  TBooleanArray=array of boolean;
  TCommandLineReaderLanguage=(clrlEnglish,clrlGerman);
  TCommandLineReaderShowError=procedure (errorDescription: string) of object;
  ECommandLineParseException=class(Exception);
  TKindOfProperty=(kpStr,kpFile,kpInt,kpFloat,kpFlag);
  TProperty=record
   category: string;
   name,desc,strvalue,strvalueDefault:string;
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

  { TCommandLineReader }

  (*** @abstract(
    A command line reader class that checks for valid arguments and automatically prints a formatted help.
    )
                                                                                                           @br
                                                                                                           @br
    Usage: @orderedList(
      @item(  Declare all allowed arguments with the corresponding DeclareXXXX functions )
      @item(  (optional) Call parse to explicitely read the actual command line )
      @item(  Use readXXX to read a declared argument )
    )

    On the command line arguments can be given in different ways, e.g.
      @code(--name=value), @code(/name=value), @code(--name value), @code(/name value)                     @br
    Declared flags can be changed with @code(--enable-flag) or @code(--disable-flag) or @code(--flag) where
    latter option negates the default value.@br
    File are checked for spaces, so it is not always necessary to include them in quotes.
  *)
  TCommandLineReader=class
  protected
    parsed{,searchNameLessFile,searchNameLessInt,searchNameLessFloat,searchNameLessFlag}: boolean;
    propertyArray: array of TProperty;
    nameless: TStringArray;
    currentDeclarationCategory: String;
    FOnOptionRead: TOptionReadEvent;
    FAllowOverrides: boolean;
    function findProperty(name:string):PProperty;
    function declareProperty(name,description,default:string;kind: TKindOfProperty):PProperty;

    procedure raiseErrorWithHelp(message: string);
    procedure parseSingleValue(var prop: TProperty);

    class function splitCommandLine(s: string; skipFirst: boolean): TStringArray;
  public
    language:TCommandLineReaderLanguage; //not implemented yet
    onShowError: TCommandLineReaderShowError;
    automaticalShowError: boolean;
    allowDOSStyle: boolean;

    constructor create;
    destructor destroy;override;

    //** Returns the option summary printed by unknown errors
    function availableOptions:string;

    //** Resets all options to their default values
    procedure reset();

    //** Reads the standard command line parameters
    procedure parse();overload;virtual;
    //** Reads the command line parameters from the string s
    procedure parse(const s:string; skipFirst: boolean = false);overload;virtual;
    //** Reads the command line parameters from the array args
    procedure parse(const args:TStringArray);overload;virtual;

    //** Adds a new option category. The category is just printed in the --help output
    procedure beginDeclarationCategory(category: string);

    //**DeclareFlag allows the use of flags                     @br
    //**Example:                                                @br
    //**  @code(declareFlag('flag','f',true);)                  @br
    //**  Following command-line options are always possible    @br
    //**    --enable-flag      =>     flag:=true                @br
    //**    --disable-flag     =>     flag:=false               @br
    //**    --flag             =>     flag:=not default         @br
    //**    -xfy               =>     flag:=not default
    procedure declareFlag(const name,description:string;flagNameAbbreviation:char;default:boolean=false);overload;
    procedure declareFlag(const name,description:string;default:boolean=false);overload;


    //**DeclareFile allows the use of a file name                                     @br
    //**Example:                                                                      @br
    //**  @code(declareFile('file');)                                                 @br
    //**  Following command-line options are  possible                                @br
    //**    --file C:\test                  =>     file:=C:\test                      @br
    //**    --file 'C:\test'                =>     file:=C:\test                      @br
    //**    --file "C:\test"                =>     file:=C:\test                      @br
    //**    --file='C:\test'                =>     file:=C:\test                      @br
    //**    --file="C:\test"                =>     file:=C:\test                      @br
    //**    --file C:\Eigene Dateien\a.bmp  =>     file:=C:\Eigene                    @br
    //**                                           or file:=C:\Eigene Dateien\a.bmp,  @br
    //**                                             if C:\Eigene does not exist
    procedure declareFile(const name,description:string;default:string='');overload;

    //**DeclareXXXX allows the use of string, int, float, ...
    //**Example:                                                    @br
    //**   @code(declareInt('property');)                           @br
    //**  Following command-line options are  possible              @br
    //**    --file 123                  =>     file:=123            @br
    //**    --file '123'                =>     file:=123            @br
    //**    --file "123"                =>     file:=123            @br
    //**    --file='123'                =>     file:=123            @br
    //**    --file="123"                =>     file:=123            @br

    procedure declareString(const name,description:string;value: string='');overload;
    procedure declareInt(const name,description:string;value: longint=0);overload;
    procedure declareFloat(const name,description:string;value: extended=0);overload;

    //**Allows to use -abbreviation=... additionally to --originalName=... @br
    //**With windows style /abbreviation and /originalName will behave in the same way
    //**(only single letter abbreviations are allowed like in unix commands)
    procedure addAbbreviation(const abbreviation: char; const originalName: string = '');

    //** Reads a previously declared string property
    function readString(const name:string):string; overload;
    //** Reads a previously declared int property
    function readInt(const name:string):longint;overload;
    //** Reads a previously declared float property
    function readFloat(const name:string):extended; overload;
    //** Reads a previously declared boolean property
    function readFlag(const name:string):boolean;overload;

    //** Tests if a declared property named name has been read
    function existsProperty(const name:string):boolean;

    //** Reads all file names that are given on the command line and do not belong to an declared option (doesn't check for non existing files, yet)
    function readNamelessFiles():TStringArray;
    //** Reads all strings that are given on the command line and do not belong to an declared option
    function readNamelessString():TStringArray;
    //** Reads all integers that are given on the command line and do not belong to an declared option
    function readNamelessInt():TLongintArray;
    //** Reads all floats that are given on the command line and do not belong to an declared option
    function readNamelessFloat():TFloatArray;
    //** Reads all booleans (true, false) that are given on the command line and do not belong to an declared option
    function readNamelessFlag():TBooleanArray;

    property onOptionRead: TOptionReadEvent read FOnOptionRead write FOnOptionRead;
    property allowOverrides: boolean read FAllowOverrides write FAllowOverrides;
  end;

implementation
{$ifdef unitcheck_rcmdline}
uses classes {$ifdef win32}, windows{$endif};
{$else}
{$ifdef win32}
uses windows;
{$endif}
{$endif}


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

constructor TCommandLineReader.create;
begin
  parsed:=false;
  {$IFDEF Win32}
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
  automaticalShowError:=not IsLibrary;
  FAllowOverrides:=false;
end;
destructor TCommandLineReader.destroy;
begin
  inherited;
end;

function TCommandLineReader.availableOptions: string;

  function mydup(count: integer): string;
  var
    i: Integer;
  begin
    result := '';
    for i := 1 to count do result := result + ' ';
  end;

var i:integer;
  cur, temp, dupped: String;
  j: Integer;
  p: integer;

  names: array of string;
  multiline : boolean;
  maxLen: Integer;
  category: String;
begin
  setlength(names, length(propertyArray));
  maxLen := 0;
  multiline:=false;
  category := '';
  for i:=0 to high(propertyArray) do begin
    cur:='--'+propertyArray[i].name;
    case propertyArray[i].kind of
      kpFlag: ;
      kpInt: cur := cur + '=<int> ';
      kpFloat: cur := cur + '=<float> ';
      kpStr: cur := cur + '=<string> ';
      kpFile: cur := cur + '=<file> ';
      else cur:=cur+'=';
    end;
    if propertyArray[i].abbreviation<>#0 then cur := cur + ' or -'+propertyArray[i].abbreviation;
    names[i] := cur;
    if length(cur) > maxLen then maxLen := length(cur);
    multiline:=multiline or (pos(LineEnding, propertyArray[i].desc) > 0);
  end;

  dupped := '';
  for j:=1 to maxLen do dupped := dupped + ' ';

  result:='';
  for i:=0 to high(propertyArray) do begin
    if propertyArray[i].category <> category then begin
      category := propertyArray[i].category;
      result := result + LineEnding + LineEnding + category + LineEnding+LineEnding;
    end;
    cur:=names[i];
    if category <> '' then cur := '  ' + cur;
    if not multiline or ( pos(LineEnding, propertyArray[i].desc) = 0 ) then cur := cur + mydup(maxLen - length(cur)) + #9 + propertyArray[i].desc + LineEnding
    else begin
      cur := cur + mydup(maxLen - length(cur));
      temp := propertyArray[i].desc;
      p := pos(LineEnding, temp);
      while p > 0 do begin
        cur := cur + #9 + copy(temp, 1, p - 1) + LineEnding + dupped;
        if category <>' ' then cur := cur + '  ';
        delete(temp, 1, p + length(LineEnding) - 1);
        p := pos(LineEnding, temp);
      end;
      cur := cur + #9 + temp + LineEnding;
    end;
    result:=result+cur;
  end;
end;

procedure TCommandLineReader.reset;
var
  i: Integer;
begin
  SetLength(nameless,0);
  for i:=0 to high(propertyArray) do begin
    with propertyArray[i] do begin
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

procedure TCommandLineReader.parse();
{$ifndef win32}
var args: TStringArray;
  i: Integer;
{$endif}
begin
  if Paramcount = 0 then exit;

  {$ifdef win32}
  parse(string(getcommandline), true);
  {$else}
  setlength(args, Paramcount);
  for i:=0 to high(args) do args[i] := paramstr(i+1);
  parse(args);
  {$endif}
end;

procedure TCommandLineReader.parse(const s:string; skipFirst: boolean = false);
var
  args: TStringArray;
begin
  args := splitCommandLine(s, skipFirst);
  parse(args);
end;

procedure TCommandLineReader.parse(const args: TStringArray);
var a: string;

  procedure raiseError(message: string);
  begin
    raiseErrorWithHelp('Error '+message+' (when reading argument: '+a+')');
  end;


var currentProperty:longint;
    flagValue: boolean;
    i:integer;
    index: integer;
    name: String;                          
    value: String;
    argpos: Integer;
    j: Integer;
begin
  reset();

  parsed:=true; //mark as parsed, so readXXX can be used within the event called by onVariableRead

  argpos := 0;
  while argpos < length(args) do begin
    a := args[argpos];
    inc(argpos);
    if a = '' then continue;
    if (a <> '-') and (a <> '--') and ((a[1] = '-') or (allowDOSStyle and (a[1]='/'))) then begin
      //Start of property name
      if (length(a) > 1) and ( (a[1]='/') or (a[2]='-')  //long property
        or (length(a) = 2) or ((length(a) > 3) and (a[3] = '='))) //or "-a ..."  or "-a=..." abbreviation
      then begin
        if a[2]='-' then delete(a, 1, 2) else delete(a, 1, 1);
        if a = '' then continue;



        currentProperty:=-1;
        if (StrLIComp(@a[1],'enable-',7) = 0)or
           (StrLIComp(@a[1],'disable-',8) = 0)  then begin
          //long flag
          flagValue:=a[1]='e';
          if flagValue then delete(a, 1, 7) else delete(a, 1, 8);

          for i:=0 to high(propertyArray) do
            if (propertyArray[i].kind=kpFlag) and equalCaseInseq(propertyArray[i].name, a) then begin
              propertyArray[i].flagvalue:=flagValue;
              if not FAllowOverrides and propertyArray[i].found then raiseError('Duplicated option: '+propertyArray[i].name);
              propertyArray[i].found:=true;
              if assigned(onOptionRead) then
                if propertyArray[i].flagvalue then onOptionRead(self,propertyArray[i].name, 'true')
                else onOptionRead(self,propertyArray[i].name, 'false');
              currentProperty:=i;
              break;
            end;
          if currentProperty = -1 then raiseError('Unknown option: '+a);
        end else begin
          //flag switch or value setting
          //i.e --flag or --name=value or --name value
          name := a;
          index := pos('=', a);
          if index > 0 then name := copy(a, 1, index - 1);

          if length(name) = 1 then begin //option like -x [optional]
            for i:=0 to high(propertyArray) do begin
              if propertyArray[i].abbreviation = name[1] then begin
                currentProperty := i;
                break;
              end;
            end;
          end else
            for i:=0 to high(propertyArray) do
              if equalCaseInseq(propertyArray[i].name, name) then begin
                currentProperty:=i;
                break;
              end;

          if currentProperty=-1 then raiseError('Unknown option: '+name);

          if not FAllowOverrides and propertyArray[currentProperty].found then raiseError('Duplicated option: '+name);
          propertyArray[currentProperty].found:=true;
          if (propertyArray[currentProperty].kind=kpFlag) and (index = 0) then begin
            propertyArray[currentProperty].flagvalue:=not propertyArray[currentProperty].flagdefault;
            if assigned(onOptionRead) then
              if propertyArray[i].flagvalue then onOptionRead(self,propertyArray[i].name, 'true')
              else onOptionRead(self,propertyArray[i].name, 'false');
            continue;
          end;


          if index = 0 then begin
            if (argpos = length(args)) then raiseError('No value for option '+name+' given');
            value := args[argpos];
            inc(argpos);
          end else value := copy(a, index + 1, length(a) - index);

          propertyArray[currentProperty].strvalue := value;
          if propertyArray[currentProperty].kind = kpFile then begin
            for i := 0 to length(args) - argpos do begin
              if FileExists(value) then begin
                inc(argpos, i);
                propertyArray[currentProperty].strvalue := value;
                break;
              end;
              if i = length(args) - argpos then break; //not found
              value := value + ' ' + args[argpos + i];
            end;
          end else parseSingleValue(propertyArray[currentProperty]);
          if assigned(onOptionRead) then onOptionRead(self,propertyArray[currentProperty].name, propertyArray[currentProperty].strvalue);
        end;
      end else begin
        for j:=2 to length(a) do begin //2 to skip leading -
          currentProperty:=-1;
          for i:=0 to high(propertyArray) do
            if (propertyArray[i].kind=kpFlag) and (propertyArray[i].abbreviation=a[j]) then begin
              propertyArray[i].flagvalue:=not propertyArray[i].flagdefault;
              if not FAllowOverrides and propertyArray[i].found then raiseError('Duplicated option: '+propertyArray[i].name);
              propertyArray[i].found:=true;
              if assigned(onOptionRead) then
                if propertyArray[i].flagvalue then onOptionRead(self,propertyArray[i].name, 'true')
                else onOptionRead(self,propertyArray[i].name, 'false');
              currentProperty:=i;
            end;
          if currentProperty = -1 then raiseError('Unknown abbreviation: '+a[j]+ LineEnding +'(use -- or / for arguments)');
        end;
      end;
    end else begin
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
  name:=lowercase(name);
  for i:=0 to high(propertyArray) do
    if propertyArray[i].name=name then begin
      result:=@propertyArray[i];
      exit;
    end;
  raise ECommandLineParseException.Create('Property not found: '+name);
end;

function TCommandLineReader.declareProperty(name,description,default:string;kind: TKindOfProperty):PProperty;
begin
  SetLength(propertyArray,length(propertyArray)+1);
  result:=@propertyArray[high(propertyArray)];
  result^.category:=currentDeclarationCategory;
  result^.name:=lowercase(name);
  result^.desc:=description;
  result^.strvalue:=default;
  result^.kind:=kind;
end;

procedure TCommandLineReader.raiseErrorWithHelp(message: string);
var errorMessage: string;
begin
  if assigned(onShowError) or automaticalShowError then begin
    errorMessage:=message+LineEnding;
    if length(propertyArray)=0 then
      errorMessage:=errorMessage+LineEnding+LineEnding+'You are not allowed to use command line options starting with -'
     else
      errorMessage:=errorMessage+ LineEnding+LineEnding+'The following command line options are valid: '+LineEnding+LineEnding+ availableOptions;
  end;

  if assigned(onShowError) then
    onShowError(errorMessage);
  if automaticalShowError then begin
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
    move(marker^, args[high(args)][ length(args[high(args)]) - addLen + 1 ], addLen);
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
  end;
end;
procedure TCommandLineReader.declareFlag(const name,description:string;default:boolean=false);
begin
  if default<>false then declareFlag(name,description+' (default: true)',#0,default)
  else declareFlag(name,description,#0,default);
end;

procedure TCommandLineReader.declareFile(const name,description:string;default:string='');
begin
  declareProperty(name,description,'',kpFile)^.strvalueDefault:=default;
end;

procedure TCommandLineReader.declareString(const name,description:string;value: string='');
begin
  declareProperty(name,description,value,kpStr)^.strvalueDefault:=value;
end;
procedure TCommandLineReader.declareInt(const name,description:string;value: longint=0);
begin
  if value<>0 then
    with declareProperty(name,description+' (default: '+IntToStr(value)+')',IntToStr(value),kpInt)^ do begin
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
     if length(propertyArray) = 0 then raise ECommandLineParseException.Create('No properties defined');
     propertyArray[high(propertyArray)].abbreviation:=abbreviation;
   end;
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

    setlength(temp, length(expected)-1);
    if length(temp) > 0 then begin
      for i:=0 to high(temp) do temp[i] := expected[i+1];
      testSplitCommandLineRaw(line, true, temp);
      testSplitCommandLineRaw('  '#9+line, true, temp);
    end;
  end;

begin
  DecimalSeparator:='.';

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
  cmdLineReader.parse('--s0"=ab''c" --s"1=def" --s2="foo\"\''bar"');
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



  say('rcmdline unit test completed');
{$endif}
end.
