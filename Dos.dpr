program Dos;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {AppPDI};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TAppPDI, AppPDI);
  Application.Run;
end.
