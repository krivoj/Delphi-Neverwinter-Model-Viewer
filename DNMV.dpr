program DNMV;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  UglContext in 'UglContext.pas',
  Unit3DS in 'Unit3DS.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
