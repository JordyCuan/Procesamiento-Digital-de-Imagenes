program FFT_2D;

uses
  Forms,
  UFourier in 'UFourier.pas' {FFourier},
  UFApoyo in 'UFApoyo.pas',
  UFTipos in 'UFTipos.pas',
  UFPatron in 'UFPatron.pas' {FFPatron},
  UFCapZonas in 'UFCapZonas.pas' {FFCapZonas};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFFourier, FFourier);
  Application.CreateForm(TFFPatron, FFPatron);
  Application.CreateForm(TFFCapZonas, FFCapZonas);
  Application.Run;
end.
