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
  TCommandLineReaderShowError=procedure (errorDescription: string);
  ECommandLineParseException=class(Exception);
  TKindOfProperty=(kpStr,kpFile,kpInt,kpFloat,kpFlag);
  TProperty=record
   category: string;
   name,desc,strvalue:string;
   found: boolean;
   case kind: TKindOfProperty of
     kpStr,kpFile: ();
     kpInt: (intvalue: longint);
     kpFloat: (floatvalue: extended);
     kpFlag: (flagvalue,flagdefault: boolean;
              abbreviation: char)
  end;
  PProperty=^TProperty;

  { TCommandLineReader }

  TCommandLineReader=class
  protected
    parsed{,searchNameLessFile,searchNameLessInt,searchNameLessFloat,searchNameLessFlag}: boolean;
    propertyArray: array of TProperty;
    nameless: TStringArray;
    currentDeclarationCategory: String;
    function findProperty(name:string):PProperty;
    function declareProperty(name,description,default:string;kind: TKindOfProperty):PProperty;
  public
    language:TCommandLineReaderLanguage; //not implemented yet
    onShowError: TCommandLineReaderShowError;
    automaticalShowError: boolean;
    allowDOSStyle: boolean;

    constructor create;
    destructor destroy;override;

    function availableOptions:string;

    procedure parse();overload;
    procedure parse(const s:string);overload;
    procedure parse(const args:array of string);overload;

    procedure beginDeclarationCategory(category: string);

    //DeclareFlag allows the use of flags
    //Example:
    //   declareFlag('flag','f',true);
    //  Following command-line options are always possible
    //    --enable-flag      =>     flag:=true
    //    --disable-flag     =>     flag:=false
    //    --flag             =>     flag:=not default
    //    -xfy               =>     flag:=not default
    procedure declareFlag(const name,description:string;flagNameAbbreviation:char;default:boolean=false);overload;
    procedure declareFlag(const name,description:string;default:boolean=false);overload;


    //DeclareFlag allows the use of a file name
    //Example:
    //   declareFile('file');
    //  Following command-line options are  possible
    //    --file C:\test                  =>     file:=C:\test
    //    --file 'C:\test'                =>     file:=C:\test
    //    --file "C:\test"                =>     file:=C:\test
    //    --file='C:\test'                =>     file:=C:\test
    //    --file="C:\test"                =>     file:=C:\test
    //    --file C:\Eigene Dateien\a.bmp  =>     file:=C:\Eigene
    //                                           or file:=C:\Eigene Dateien\a.bmp,
    //                                             if C:\Eigene does not exist
    procedure declareFile(const name,description:string;default:string='');overload;

    //**DeclareXXXX allows the use of string, int, float, ...
    //**Example:
    //**   declareFlag('property');
    //**  Following command-line options are  possible
    //**    --file 123                  =>     file:=123
    //**    --file '123'                =>     file:=123
    //**    --file "123"                =>     file:=123
    //**    --file='123'                =>     file:=123
    //**    --file="123"                =>     file:=123

    procedure declareString(const name,description:string;value: string='');overload;
    procedure declareInt(const name,description:string;value: longint=0);overload;
    procedure declareFloat(const name,description:string;value: extended=0);overload;

    function readString(const name:string):string; overload;
    function readInt(const name:string):longint;overload;
    function readFloat(const name:string):extended; overload;
    function readFlag(const name:string):boolean;overload;

    //**has this property been read
    function existsProperty(const name:string):boolean;

    function readNamelessFiles():TStringArray;
    function readNamelessString():TStringArray;
    function readNamelessInt():TLongintArray;
    function readNamelessFloat():TFloatArray;
    function readNamelessFlag():TBooleanArray;
  end;

implementation
{$ifdef unitcheck_rcmdline}
     uses classes;
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
  p: SizeInt;

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
      kpFlag: if propertyArray[i].abbreviation<>#0 then
                cur:=cur+' or -'+propertyArray[i].abbreviation;
      kpInt: cur := cur + '=<int> ';
      kpFloat: cur := cur + '=<float> ';
      kpStr: cur := cur + '=<string> ';
      kpFile: cur := cur + '=<file> ';
      else cur:=cur+'=';
    end;
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
      result += LineEnding + LineEnding + category + LineEnding+LineEnding;
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

procedure TCommandLineReader.parse();
var args: array of string;
  i: Integer;
begin
  if Paramcount = 0 then exit;

  //parse(string(getcommandline));

  setlength(args, Paramcount);
  for i:=0 to high(args) do args[i] := paramstr(i+1);
  parse(args);
end;

procedure TCommandLineReader.parse(const s:string);
var args: array of string;
  i: Integer;
  cmd: pchar;
  marker: pchar;
  stringstart: Char;

  procedure pushMarked;
  var
    addLen: longint;
  begin
    if (marker = @s[1]) or (marker = cmd) or ((marker-1)^ = ' ') or (length(args) = 0) then
      setlength(args, length(args)+1);
    addLen := cmd - marker;
    if addLen <= 0 then exit;
    setlength(args[high(args)], length(args[high(args)]) + addLen);
    move(marker^, args[high(args)][ length(args[high(args)]) - addLen + 1 ], addLen);
    marker := cmd;
  end;
begin
  if s = '' then exit;
  cmd := @s[1];
  marker := cmd;
  while true do begin
    if (cmd^ in [' ', #0]) then begin
      if marker^ <> ' ' then pushMarked;
      if cmd^ = #0 then break;
    end else if cmd^ in ['"', ''''] then begin //todo escaped quotes (e.g. a"\"\""b)
      if marker^ = ' ' then marker := cmd;
      pushMarked;
      stringstart := cmd^;
      inc(cmd);
      marker:=cmd;
      while (cmd^ <> stringstart) and (cmd^ <> #0) do inc(cmd);
      pushMarked;
      marker := cmd + 1;
    end else if marker^ = ' ' then marker:=cmd;
    inc(cmd);
  end;
  parse(args);
end;

procedure TCommandLineReader.parse(const args: array of string);
var a: string;

  procedure raiseError(message: string);
  var errorMessage: string;
      i:integer;
  begin
    if assigned(onShowError) or automaticalShowError then begin
      errorMessage:='Error '+message+' (when reading argument: '+a+')'+LineEnding;
      if length(propertyArray)=0 then
        errorMessage+=LineEnding+LineEnding+'You are not allowed to use command line options starting with -'
       else
        errorMessage+=LineEnding+LineEnding+'The following command line options are valid: '+LineEnding+LineEnding+ availableOptions;
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
    raise ECommandLineParseException.create('Error '+message+' when reading argument: '+a);
  end;


var currentProperty:longint;
    valueStart: pchar;
    valueLength: longint;
    flagValue: boolean;
    stringStart: char;
    i:integer;
    index: SizeInt;
    name: String;
    value: String;
    argpos: Integer;
    j: Integer;
begin
  currentProperty:=-1;
  SetLength(nameless,0);
  for i:=0 to high(propertyArray) do
    propertyArray[i].found:=false;

  argpos := 0;
  while argpos < length(args) do begin
    a := args[argpos];
    argpos += 1;
    if a = '' then continue;
    if (a[1] = '-') or (allowDOSStyle and (a[1]='/')) then begin
      //Start of property name
      if (a[1]='/') or ((length(a) > 1) and (a[2]='-')) then begin //long property
        if a[1]<>'/' then delete(a, 1, 2) else delete(a, 1, 1);
        if a = '' then continue;

        currentProperty:=-1;
        if (StrLIComp(@a[1],'enable-',7) = 0)or
           (StrLIComp(@a[1],'disable-',8) = 0)  then begin
          //long flag
          flagValue:=a[1]='e';
          if flagValue then delete(a, 1, 7) else delete(a, 1, 8);

          for i:=0 to high(propertyArray) do
            if (propertyArray[i].kind=kpFlag) and SameText(propertyArray[i].name, a) then begin
              propertyArray[i].flagvalue:=flagValue;
              propertyArray[i].found:=true;
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

          for i:=0 to high(propertyArray) do
            if SameText(propertyArray[i].name, name) then begin
              if (propertyArray[i].kind=kpFlag) and (index = 0) then
                propertyArray[i].flagvalue:=not propertyArray[i].flagdefault;
              currentProperty:=i;
              propertyArray[i].found:=true;
              break;
            end;
          if currentProperty=-1 then raiseError('Unknown option: '+name);
          if (propertyArray[currentProperty].kind=kpFlag) and (index = 0) then continue;

          if index = 0 then begin
            if (argpos = length(args)) then raiseError('No value for option '+name+' given');
            value := args[argpos];
            argpos += 1;
          end else value := copy(a, index + 1, length(a) - index);

          propertyArray[currentProperty].strvalue := value;
          try
            case propertyArray[currentProperty].kind of
              kpInt: propertyArray[currentProperty].intvalue:=StrToInt(value);
              kpFloat:  propertyArray[currentProperty].floatvalue:=StrToFloat(value);
              kpFile: begin
                for i := 0 to length(args) - argpos do begin
                  if FileExists(value) then begin
                    argpos += i;
                    propertyArray[currentProperty].strvalue := value;
                    break;
                  end;
                  if i = length(args) - argpos then break; //not found
                  value := value + ' ' + args[argpos + i];
                end;
              end;
              kpFlag: begin
                propertyArray[currentProperty].flagvalue:=SameText(value, 'true');
                if not propertyArray[currentProperty].flagvalue and not SameText(value, 'false') then raiseError('Only "true" and "false" are valid flag values');
              end;
            end;
          except
            raiseError('Invalid value: '+value+' for option '+name);
          end;
        end;
      end else begin
        //flag abbreviation string
        for j:=2 to length(a) do begin //2 to skip beginning -
          currentProperty:=-1;
          for i:=0 to high(propertyArray) do
            if (propertyArray[i].kind=kpFlag) and (propertyArray[i].abbreviation=a[j]) then begin
              propertyArray[i].flagvalue:=not propertyArray[i].flagdefault;
              propertyArray[i].found:=true;
              currentProperty:=i;
            end;
          if currentProperty = -1 then raiseError('Unknown abbreviation: '+a[j]+ LineEnding +'(use -- or / for arguments)');
        end;
      end;
    end else begin
      //value without variable name
      SetLength(nameless,length(nameless)+1);
      nameless[high(nameless)] := a;
    end;
  end;

  {debug things: for i:= 0 to high(propertyArray) do
  if propertyArray[i].found then begin
    write(propertyArray[i].name , ' => ', propertyArray[i].strvalue);
    if propertyArray[i].kind =kpFlag then writeln( '(',propertyArray[i].flagvalue,')')
    else writeln;
  end;
  for i:= 0 to high(nameless) do writeln('no: ', nameless[i]);}

  parsed:=true;
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
  declareProperty(name,description,'',kpFile);
end;

procedure TCommandLineReader.declareString(const name,description:string;value: string='');
begin
  declareProperty(name,description,value,kpStr);
end;
procedure TCommandLineReader.declareInt(const name,description:string;value: longint=0);
begin
  if value<>0 then declareProperty(name,description+' (default: '+IntToStr(value)+')',IntToStr(value),kpInt)^.intvalue:=value
  else declareProperty(name,description,IntToStr(value),kpInt)^.intvalue:=value;
end;
procedure TCommandLineReader.declareFloat(const name,description:string;value: extended=0);
begin
  declareProperty(name,description,FloatToStr(value),kpFloat)^.floatvalue:=value;
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
begin
  DecimalSeparator:='.';

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
  cmdLineReader.parse('--p1=4.2 /p2=20.3 --p3 "443.2" some dummy string --p4=''2.2'' --p4=5');
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


  say('rcmdline unit test completed');
{$endif}
end.
