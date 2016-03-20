(*** @abstract(
  Request reader for CGI applications (fpc only)
)*)
unit rcmdlinecgi;

{$ifdef fpc}{$mode delphi}{$endif}

interface

uses
  Classes, SysUtils, rcmdline, custcgi, custweb, HTTPDefs;

type

{ TCommandLineReaderGUI }

(*** @abstract(

  Request reader for CGI applications, like TCommandLineReader (fpc only)

)


*)

{ TCommandLineReaderCGI }

TCgiHandlerForCommandLineReader = class;

TCommandLineReaderCGI=class(TCommandLineReader)
protected
  handlerOwned: TCgiHandlerForCommandLineReader;
  req: TCGIRequest;
  response: TCGIResponse;
  procedure showErrorCGI(error: string);
public
  //**Creates a reader using the standard CGI interface
  constructor create();overload;
  //**Creates a reader reading the parameters from the given request, and writing errors to the given response
  constructor create(request: TCGIRequest; response: TCGIResponse = nil);overload;
  destructor destroy; override;

  //**Actually reads the CGI data (called automatically, when a read.. function is called)
  procedure parse(autoReset: boolean = true);override;


  //** Return
  function urlEncodeParams: string;
public
  //**If this is false (=default), parameters declared with declareFile will be disabled
  AllowFiles: boolean;
end;


{ TCommandLineReaderCGIApplication }

{ TCgiHandlerForCommandLineReader }

TCgiHandlerForCommandLineReader = class(TCgiHandler)
public
  procedure init;
end;


implementation

uses strutils, bbutils;
{ TCgiHandlerForCommandLineReader }

procedure TCgiHandlerForCommandLineReader.init;
var
  temp: TRequest;
  temp2: TResponse;
begin
  WaitForRequest(temp, temp2);
end;

{ TCommandLineReaderCGI }

procedure TCommandLineReaderCGI.showErrorCGI(error: string);
 procedure answer(s: string);
 begin
   if response = nil then writeln(s)
   else response.Contents.Add(s);
 end;

begin
  if response = nil then begin //response seems to handle the content-type
    answer('Content-Type: text/html');
    answer('');
  end;
  answer('<html><head>');
  answer('<title>Error</title>');
  answer('</head><body>');
  answer('<h1>Error</h1>');
  answer('<pre>');
  answer(error);
  answer('</pre>');
  answer('</body></html>');
  if handlerOwned <> nil then begin
    response.SendResponse;
    halt;
  end;
end;

constructor TCommandLineReaderCGI.create;
begin
  handlerOwned := TCgiHandlerForCommandLineReader.Create(nil);
  handlerOwned.init;
  create(handlerOwned.Request, handlerOwned.Response);
end;



constructor TCommandLineReaderCGI.create(request: TCGIRequest; response: TCGIResponse = nil);
begin
  inherited create;
  onShowError := showErrorCGI;
  AllowFiles:=false;
  req := request;
  self.response := response;
end;


destructor TCommandLineReaderCGI.destroy;
begin
  handlerOwned.free;
  inherited destroy;
end;

procedure TCommandLineReaderCGI.parse(autoReset: boolean = true);
  procedure parseVariables(list: TStrings);
  var
    name: String;
    value: String;
    j, i: Integer;
    found: Boolean;
  begin
    for j:=0 to list.Count-1 do begin
      list.GetNameValue(j, name, value);
      if (name = '') then continue; //workaround for empty input created by codemirror
      found := false;
      for i := 0 to high(propertyArray) do begin
        if SameText(propertyArray[i].name, name) then begin
          propertyArray[i].strvalue := value;
          propertyArray[i].found := true;
          parseSingleValue(propertyArray[i]);
          if assigned(onOptionRead) then onOptionRead(self, name, value);
          found := true;
          break;
        end;
      end;
      if not found then raiseErrorWithHelp('Unknown parameter: '+name);
    end;
  end;
var
  i: Integer;
  temp: TStringArray;
begin
  inherited parse(autoReset);

  parsed := true;

  req.QueryFields.Clear; //recreate req.QueryFields because fpc is completely broken
  temp := strSplit(GetEnvironmentVariable('QUERY_STRING'), '&');
  for i := 0 to high(temp) do
    req.QueryFields.add(HTTPDecode(temp[i]));

  parseVariables(req.QueryFields);
  parseVariables(req.ContentFields);

  if not AllowFiles then
    for i:=0 to high(propertyArray) do
      if (propertyArray[i].found) and (propertyArray[i].kind = kpFile) then
        raiseErrorWithHelp('File parameters (like '+propertyArray[i].name+') have been forbidden due to security reasons');
end;

function TCommandLineReaderCGI.urlEncodeParams: string;
  function encode(s: string): string;
  begin
    result := StringsReplace(s, ['%',     #9,   #10,   #13,    '"',   '<',   '>',   '#',   '$',   '&',   '+', ' ',   ',',   '/',   ':',  ';',   '=',   '?', '@'],
                                ['%25', '%09', '%0A', '%0D', '%22', '%3C', '%3E', '%23', '%24', '%26', '%2B', '+', '%2C', '%2F', '%3A','%3B', '%3D', '%3F', '%40'],
                                [rfReplaceAll]);
  end;


var
  i: Integer;
begin
  result := '';
  for i := 0 to high(propertyArray) do begin
    if not propertyArray[i].found then continue;
    if result <> '' then result += '&';
    result += encode(propertyArray[i].name) + '=' + encode(propertyArray[i].strvalue);
  end;
end;

end.

