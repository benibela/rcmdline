TCommandLineReader
==================
 
This unit provides an advanced, platform-independent command line parser for Lazarus and Delphi.

It checks for allowed options, automatically prints a help with a list of all of them, and -- contrary to the parser in the rtl -- behaves the same on Windows and Linux.

Example:
 
 ```pascal
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

  
```


See my webpage for the detailed [rcmdline documentation](http://www.benibela.de/sources_en.html#rcmdline)
