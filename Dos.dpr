program Dos;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {AppPDI},
  UHisto in 'UHisto.pas' {FormHisto},
  UBase in 'UBase.pas',
  UPuntuales in 'UPuntuales.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TAppPDI, AppPDI);
  Application.CreateForm(TFormHisto, FormHisto);
  Application.Run;
end.
