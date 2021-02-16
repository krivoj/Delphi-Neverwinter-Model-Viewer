program DNMV;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  UglContext in 'UglContext.pas',
  Unit3DS in 'Unit3DS.pas',
  DSE_SearchFiles in 'DSE_SearchFiles.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Delphi Neverwinter Model Viewer';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
