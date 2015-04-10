program ConstrPaletas1;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  parser1 in 'parser1\parser1.pas',
  FormExpr in 'parser1\FormExpr.pas',
  oObjects in 'parser1\oObjects.pas',
  ParseClass in 'parser1\ParseClass.pas',
  ParseExpr in 'parser1\ParseExpr.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
