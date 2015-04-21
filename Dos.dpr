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
  UnitExpHisto in 'UnitExpHisto.pas' {Form4},
  UFCapZonas in 'Fourier\UFCapZonas.pas' {FFCapZonas},
  FormAbout in 'DirectX\FormAbout.pas' {AboutBox},
  Frame_Video in 'DirectX\Frame_Video.pas' {Frame1: TFrame},
  VFrames in 'DirectX\VFrames.pas',
  VSample in 'DirectX\VSample.pas',
  VSampleDemo_MainForm in 'DirectX\VSampleDemo_MainForm.pas' {Form_Main},
  Direct3D9 in 'DirectX\DirectX\Direct3D9.pas',
  DirectDraw in 'DirectX\DirectX\DirectDraw.pas',
  DirectShow9 in 'DirectX\DirectX\DirectShow9.pas',
  DirectSound in 'DirectX\DirectX\DirectSound.pas',
  DXTypes in 'DirectX\DirectX\DXTypes.pas',
  gfx_basedef in 'gfx_files\gfx_basedef.pas',
  gfx_colors in 'gfx_files\gfx_colors.pas',
  gfx_compression in 'gfx_files\gfx_compression.pas',
  gfx_const in 'gfx_files\gfx_const.pas',
  gfx_crypt in 'gfx_files\gfx_crypt.pas',
  gfx_effectex in 'gfx_files\gfx_effectex.pas',
  gfx_effects in 'gfx_files\gfx_effects.pas',
  gfx_errors in 'gfx_files\gfx_errors.pas',
  gfx_files in 'gfx_files\gfx_files.pas',
  gfx_masks in 'gfx_files\gfx_masks.pas',
  gfx_pcd in 'gfx_files\gfx_pcd.pas',
  gfx_pcx in 'gfx_files\gfx_pcx.pas',
  gfx_png in 'gfx_files\gfx_png.pas',
  gfx_rotresize in 'gfx_files\gfx_rotresize.pas',
  gfx_tga in 'gfx_files\gfx_tga.pas',
  gfx_tiff in 'gfx_files\gfx_tiff.pas',
  io_const in 'gfx_files\io_const.pas',
  io_files in 'gfx_files\io_files.pas',
  pnglib in 'gfx_files\pnglib.pas',
  PNGZLIB in 'gfx_files\PNGZLIB.pas',
  UFourier in 'Fourier\UFourier.pas' {FFourier},
  UFTipos in 'Fourier\UFTipos.pas',
  UFourierBase in 'Fourier\UFourierBase.pas',
  UFPatron in 'Fourier\UFPatron.pas' {FFPatron},
  UDirectX in 'UDirectX.pas' {FDirectX};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TAppPDI, AppPDI);
  Application.CreateForm(TFormHisto, FormHisto);
  Application.CreateForm(TFormRot, FormRot);
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TForm3, Form3);
  Application.CreateForm(TFDirectX, FDirectX);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.CreateForm(TForm_Main, Form_Main);
  Application.CreateForm(TFFCapZonas, FFCapZonas);
  Application.CreateForm(TFFCapZonas, FFCapZonas);
  Application.CreateForm(TFFourier, FFourier);
  Application.CreateForm(TFFPatron, FFPatron);
  Application.CreateForm(TFFCapZonas, FFCapZonas);
  Application.CreateForm(TForm4, Form4);
  Application.CreateForm(TFFCapZonas, FFCapZonas);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.CreateForm(TForm_Main, Form_Main);
  Application.CreateForm(TFFourier, FFourier);
  Application.CreateForm(TFFPatron, FFPatron);
  Application.Run;
end.
