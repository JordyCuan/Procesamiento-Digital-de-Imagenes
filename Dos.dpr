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
  UCalc in 'UCalc.pas',
  UnitZoom in 'UnitZoom.pas' {Form2},
  UParBin in 'UParBin.pas' {Form3},
  UEspeciales in 'UEspeciales.pas',
  UnitExpHisto in 'UnitExpHisto.pas' {Form4};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TAppPDI, AppPDI);
  Application.CreateForm(TFormHisto, FormHisto);
  Application.CreateForm(TFormRot, FormRot);
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TForm3, Form3);
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
