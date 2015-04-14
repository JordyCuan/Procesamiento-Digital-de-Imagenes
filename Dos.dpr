program Dos;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {AppPDI},
  UHisto in 'UHisto.pas' {FormHisto},
  UBase in 'UBase.pas',
  UPuntuales in 'UPuntuales.pas',
  URegionales in 'URegionales.pas',
  UGeometricos in 'UGeometricos.pas',
  UIntRotacion in 'UIntRotacion.pas' {FormRot},
  UCalc in 'UCalc.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TAppPDI, AppPDI);
  Application.CreateForm(TFormHisto, FormHisto);
  Application.CreateForm(TFormRot, FormRot);
  Application.Run;
end.
