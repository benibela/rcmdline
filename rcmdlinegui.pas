(*** @abstract(
  Command line reader for GUI applications
)*)
unit rcmdlinegui;

{$ifdef fpc}{$mode delphi}{$endif}

interface

uses
  Classes, SysUtils, rcmdline;

type

{ TCommandLineReaderGUI }

(*** @abstract(

  Command line reader like TCommandLineReader for GUI applications

)

  @br@br

  The only difference is that it shows the --help output as messagebox instead of using writeln.
*)
TCommandLineReaderGUI=class(TCommandLineReader)
protected
  procedure showErrorGUI(error: string);
public
  constructor create;
end;

implementation
uses dialogs;

{ TCommandLineReaderGUI }

procedure TCommandLineReaderGUI.showErrorGUI(error: string);
begin
  showMessage(error);
  halt;
end;

constructor TCommandLineReaderGUI.create;
begin
  inherited;
  onShowError := showErrorGUI;
end;

end.

