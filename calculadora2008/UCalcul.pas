// Unidad de Calculadora de Imágenes

// Manuel Martín Ortíz
// 24 abril 2008
// Facultad de Ciencias de la Computación
// Universidad Autónoma de Puebla
// Puebla, Puebla, México

// Versión 0.0.5


unit UCalcul;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtDlgs, ExtCtrls, ComCtrls, JPEG, Math, ImTools,
  gfx_files;

const
  SS     : array [0..2] of byte = (0,8,16);

type

  TFCalcul = class(TForm)
    BotAbrirA: TButton;
    BotAbrirB: TButton;
    BotSalida: TButton;
    SBsuma: TSpeedButton;
    SBRestaAbs: TSpeedButton;
    SBmultiplicacion: TSpeedButton;
    SBdivision: TSpeedButton;
    SBmax: TSpeedButton;
    SBmin: TSpeedButton;
    ScrollBox1: TScrollBox;
    ScrollBox2: TScrollBox;
    ScrollBox3: TScrollBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    OpenPD: TOpenPictureDialog;
    SBRestaShift: TSpeedButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Label4: TLabel;
    Bevel1: TBevel;
    SBRestaNegro: TSpeedButton;
    SBand: TSpeedButton;
    SBor: TSpeedButton;
    Bevel2: TBevel;
    BotR_A: TButton;
    BotR_B: TButton;
    Label6: TLabel;
    Label7: TLabel;
    SBDivision_CorrRango: TSpeedButton;
    Label8: TLabel;
    BotA_B: TButton;
    Label9: TLabel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    SavePD: TSavePictureDialog;
    Label10: TLabel;
    Bevel5: TBevel;
    Edit4: TEdit;
    Label11: TLabel;
    Edit5: TEdit;
    Label12: TLabel;
    Bola: TShape;
    SBxor: TSpeedButton;
    BotSalvar: TButton;
    SBNegativoR: TSpeedButton;
    SBRangoDin: TSpeedButton;
    Bevel6: TBevel;
    Bevel7: TBevel;
    Bevel8: TBevel;
    Label13: TLabel;
    Bevel9: TBevel;
    Label14: TLabel;
    BCopia: TButton;
    Label15: TLabel;
    Label16: TLabel;
    SBStat: TSpeedButton;
    Edit6: TEdit;
    Edit7: TEdit;
    Label17: TLabel;
    Label18: TLabel;
    SBCombLineal: TButton;
    SpeedButton1: TSpeedButton;
    Timer1: TTimer;
    SpeedButton2: TSpeedButton;
    Label5: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    SBSumaRango: TSpeedButton;
    Bevel11: TBevel;
    Bevel12: TBevel;
    Bevel13: TBevel;
    Panel1: TPanel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    ImageSel: TImage;
    Edit8: TEdit;
    Bevel14: TBevel;
    Bevel15: TBevel;
    Bevel16: TBevel;
    SBReset: TSpeedButton;
    SBnand: TSpeedButton;
    procedure BotAbrirAClick(Sender: TObject);
    procedure BotAbrirBClick(Sender: TObject);
    procedure BotSalidaClick(Sender: TObject);
    procedure SBsumaClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SBRestaAbsClick(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SBRestaShiftClick(Sender: TObject);
    procedure SBRestaNegroClick(Sender: TObject);
    procedure SBandClick(Sender: TObject);
    procedure SBorClick(Sender: TObject);
    procedure BotR_AClick(Sender: TObject);
    procedure BotR_BClick(Sender: TObject);
    procedure SBmaxClick(Sender: TObject);
    procedure SBmultiplicacionClick(Sender: TObject);
    procedure SBdivisionClick(Sender: TObject);
    procedure SBDivision_CorrRangoClick(Sender: TObject);
    procedure SBminClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BotA_BClick(Sender: TObject);
    procedure SBxorClick(Sender: TObject);
    procedure BotSalvarClick(Sender: TObject);
    procedure SBNegativoRClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BCopiaClick(Sender: TObject);
    procedure SBRangoDinClick(Sender: TObject);
    procedure SBStatClick(Sender: TObject);
    procedure SBCombLinealClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SBSumaRangoClick(Sender: TObject);
    procedure RadioButton1Click(Sender: TObject);
    procedure RadioButton2Click(Sender: TObject);
    procedure SBResetClick(Sender: TObject);
    procedure ImageSelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageSelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageSelMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SBnandClick(Sender: TObject);
  private
    { Private declarations }
    ncol,nren       : integer;
    nrenA, ncolA    : integer;
    nrenB, ncolB    : integer;
    banA,banB,
    banC            : boolean;
    t1,t2           : TDateTime;
    time1,time2     : TTimeStamp;
    tt1,tt2         : integer;
    dt              : real;
    mat1,mat2,mat3  : mat24;

    // para el morphing
    NC              : integer;
    Kmorph          : integer;
    Dalf            : real;

    // Para la insercion
    modo_alinear    : char;
    x0,y0           : integer;
    xm,ym           : integer;
    ban_inser       : boolean;

    procedure PreparaBM3;overload;
    procedure PreparaBM3(modo : char);overload;

    procedure Inicia_Tiempo;
    procedure Termina_Tiempo;
    procedure Copia(BM : TBitMap; var mm :mat24);overload;
    procedure Copia(m1 : mat24  ; var m2 :mat24; ncol,mren:integer);overload;
    procedure Vacia(mm :mat24; BM : TBitMap);overload;
    procedure Vacia(mm :mat24; n,m : integer; BM : TBitMap);overload;
    function  SConvRGB(x : integer) : String;
    procedure dib_rect(a,b,p,q : integer);

    // Proceso generico
    procedure Procesa;

  public
    { Public declarations }
    BM1,BM2,BM3  : TBitMap;
    bsal         : boolean;
    nomb         : string;
  end;

var
  FCalcul        : TFCalcul;
  operacion      : function(a,b : single): single;
  cc             : real;
  alf,bet        : byte;

implementation

uses Unit1, Unit31, Unit32;

{$R *.DFM}

// ---------------- Funciones de operación específicas ----

// ------------  Aritméticas ------------------------------

// suma con peso
function p_suma(a,b:single):single;
begin
   p_suma := (a+b)/2;
end;

// suma con corrección de rango
function p_suma_CR(a,b:single):single;
var y : single;
begin
  y  := a+b;
  p_suma_CR := y;
end;

function p_resta_abs(a,b:single):single;
begin
  p_resta_abs  := abs(a-b) ;
end;

function p_resta_shift(a,b:single):single;
begin
  p_resta_shift  := 127 + (a-b)/2 ;
end;

function p_resta_a_negro(a,b:single):single;
var y : single;
begin
   y  := a-b; if y<0 then y := 0;
   p_resta_a_negro := y;
end;

function p_max(a,b:single):single;
begin
   p_max  := Max(a,b);
end;

function p_min(a,b:single):single;
begin
   p_min  := Min(a,b);
end;

function p_div(a,b:single):single;
begin
  p_div  := _lambda - (_lambda*min(a,b))/(max(a,b)+1);
end;

function p_div_corr(a,b:single):single;
begin
  p_div_corr  := cc*ln(1 + max(a,b)/(min(a,b)+1)) ;
end;

function p_mult(a,b:single):single;
begin
  p_mult  := a*b/_lambda;
end;

function p_Comb_Lin(a,b:single):single;
begin
  p_Comb_Lin  := (alf*a+bet*b)/100;
end;

// ------------  Lógicas ----------------------------------

function p_and(a,b:single):single;
begin
  p_and  := ajusta255(a) and ajusta255(b);
end;

function p_or(a,b:single):single;
begin
  p_or  := ajusta255(a) or ajusta255(b);
end;

function p_xor(a,b:single):single;
begin
  p_xor  := ajusta255(a) xor ajusta255(b);
end;

function p_nand(a,b:single):single;
begin
  p_nand  := (not ajusta255(a)) and ajusta255(b);
end;

// ..................................
// .  Proceso Genérico de operacion .
// ..................................

procedure TFCalcul.Procesa;
var
  i,j,ii,jj,k   : integer;
  x1,x2,y       : single;
begin
  if (banA and banB) then begin
    PreparaBM3(modo_alinear);
    Inicia_Tiempo;

    case modo_alinear of
    's' : begin
            for j := 0 to nren-1 do begin
              for i := 0 to ncol-1 do
                for k := 0 to 2 do begin
                  x1 := mat1[i,j,k];
                  x2 := mat2[i,j,k];
                  y  := operacion(x1,x2);
                  mat3[i,j,k] := y
                end;
            end;
            Vacia(mat3,ncol,nren,BM3);
          end;
    'i' : begin
            // Copia M2 en M3
            copia (mat2,mat3,ncolB,nrenB);

            // inicia proceso de insercion
            j := Y0; jj := 0;
            while (j<nrenB) and (jj<nrenA) do begin
              i := X0; ii := 0;
              while (i<ncolB) and (ii<ncolA) do begin
                for k := 0 to 2 do begin
                  x1 := mat1[ii,jj,k];
                  x2 := mat2[i,j,k];
                  y  := operacion(x1,x2);
                  mat3[i,j,k] := y
                end;
                inc(i); inc(ii);
              end;
              inc(j); inc(jj);
            end;
            Vacia(mat3,ncolB,nrenB,BM3);
          end;
    end; // del case

    Image3.Picture.Assign(BM3);
    Termina_Tiempo;
    Label4.caption :='Suma';
  end
  else ShowMessage('Para ejecutar debe haber leido 2 imagénes');
end;

// --------------------------------------------------------

// Creacion de la forma
procedure TFCalcul.FormCreate(Sender: TObject);
begin
  BM1 := TBitMap.Create;
  BM2 := TBitMap.Create;
  BM3 := TBitMap.Create;

  Image1.Visible := False;
  Image2.Visible := False;
  Image3.Visible := False;

  banA := False; banB := False; banC := False;

  Edit1.Text := '??';
  Edit2.Text := '??';
  Edit3.Text := '??';
  Edit4.Text := '0';
  Edit5.Text := '??';
  Edit6.Text := '??';
  Edit7.Text := '??';
  Label4.Caption :='Esperando ...';
  Bola.Visible := False;

  // Oculta Lienzo de seleccion para el modo de "insercion".
  ImageSel.Visible := False;

  // Modo default = "Alineacion superior"
  modo_alinear := 's';

  X0 := 0;
  Y0 := 0;
  Edit8.Enabled   := false;
  Edit8.Visible   := false;
  SBReset.Visible := false;

  // Valores de los dialogos de I/O de archivos
  OpenPD.Filter := _OpenFilterString;
  OpenPD.FilterIndex := 0;
  OpenPD.InitialDir := '.';
  SavePD.Filter := _SaveFilterString;
  SavePD.FilterIndex := 0;
  SavePD.InitialDir := '.';

end;

{------------------- On Show Form Calculadora -----------------------}

procedure TFCalcul.FormShow(Sender: TObject);
begin
  Top  := Principal.Top+30;
  Left := Principal.Left;

  bsal := false;
  Label15.Visible := false;
  Label16.Visible := false;

  if NOT BM1.Empty then begin
     banA := False;
     nomb := Principal.Name1;
     Label15.Caption := nomb;
     Label15.Visible := true;

     Image1.Picture.Assign(BM1);
     Image1.Visible := true;
     if BM1.PixelFormat <> pf24bit
       then ShowMessage('Formato no soportado / Calculadora')
       else begin
         ncolA := BM1.Width;
         nrenA := BM1.Height;
         banA  := true;
         Copia(BM1,mat1);
       end;
    end;
end;

{------------------- Archivos            ----------------------------}

// Abrir Imagen A
procedure TFCalcul.BotAbrirAClick(Sender: TObject);
begin
   if OpenPD.execute then begin
     banA := False;
     nomb := OpenPD.filename;
     Ext  := UpperCase(ExtractFileExt(nomb));

     if      (Ext <> '.JPG')
         and (Ext <> '.JPEG')
         and (Ext <> '.BMP')
         and (Ext <> '.PNG')
         and (Ext <> '.TIF')
         and (Ext <> '.PCD')
         and (Ext <> '.PCX')
         and (Ext <> '.TGA')
         and (Ext <> '.WMF')
         and (Ext <> '.EMF')
       then begin
         ShowMessage('Solo se soportan archivo de tipo:'+#13+#10+
                     'JPG,JPEG,BMP,PNG,TIF,PCD,PCX,TGA,WMF,EMF');
         exit;
       end;

     // Carga la imagen usando las utilerias de gfx
     LoadAnyImageToBMP(BM1, nomb, false, 0, 0);


     Image1.Picture.Assign(BM1);
     Image1.Visible := true;
     if BM1.PixelFormat <> pf24bit
       then ShowMessage('Formato no soportado')
       else begin
         ncolA := BM1.Width;
         nrenA := BM1.Height;
         banA  := true;
         Copia(BM1,mat1);
         label15.Caption := nomb;
         label15.Show;
       end;
   end;
end;

// Abrir Imagen B
procedure TFCalcul.BotAbrirBClick(Sender: TObject);
begin
   if OpenPD.execute then begin
     banB := False;
     nomb := OpenPD.filename;
     Ext  := UpperCase(ExtractFileExt(nomb));

     if      (Ext <> '.JPG')
         and (Ext <> '.JPEG')
         and (Ext <> '.BMP')
         and (Ext <> '.PNG')
         and (Ext <> '.TIF')
         and (Ext <> '.PCD')
         and (Ext <> '.PCX')
         and (Ext <> '.TGA')
         and (Ext <> '.WMF')
         and (Ext <> '.EMF')
       then begin
         ShowMessage('Solo se soportan archivo de tipo:'+#13+#10+
                     'JPG,JPEG,BMP,PNG,TIF,PCD,PCX,TGA,WMF,EMF');
         exit;
       end;

     // Carga la imagen usando las utilerias de gfx
     LoadAnyImageToBMP(BM2, nomb, false, 0, 0);

     Image2.Picture.assign(BM2);
     Image2.Visible := true;
     if BM2.PixelFormat <> pf24bit
       then ShowMessage('Formato no soportado')
       else begin
         ncolB := BM2.Width;
         nrenB := BM2.Height;
         banB  := true;
         Copia(BM2,mat2);
         label16.Caption := nomb;
         label16.show;

         // Preparacion del Lienzo de Seleccion
         with ImageSel do begin
           Width  := ncolB;
           Height := nrenB;

           xm := ncolA shr 1;
           ym := nrenA shr 1;

           X0 := xm; Y0 := ym;

           Transparent := true;
           Canvas.Pen.Color := clWhite;
           Canvas.Brush.Color := clWhite;
           Canvas.Rectangle(0,0,Width-1,Height-1);

           Canvas.Pen.Color := clYellow;
           dib_rect(X0,Y0,xm,ym);

           Canvas.Pen.Color := clYellow;
           SendToBack;
           Edit8.Visible := false;
         end; // del with ImageSel
       end;
   end;
end;

// Salvar Resultado a Archivo
procedure TFCalcul.BotSalvarClick(Sender: TObject);
var
  nomb, Ext  : string;
begin
  if banC then
    if SavePD.execute then begin
      nomb := SavePD.FileName;

      Ext := ExtractFileExt(nomb);
      Ext := UpperCase(Ext);

      if      (Ext <> '.JPG')
          and (Ext <> '.BMP')
          and (Ext <> '.JPEG')
          and (Ext <> '.PCX')
          and (Ext <> '.PNG')
          and (Ext <> '.TGA')
          and (Ext <> '.TIF')
          and (Ext <> '.WMF')
          and (Ext <> '.EMF')
        then
          ShowMessage('La extensión seleccionada '+Ext+' no esta soportada'+
                          #13+#10+'Acción abortada ... sic')
        else SaveBMPToAnyFile(BM3, nomb, 0, 0);
    end;
end;

// Copiar a Principal
procedure TFCalcul.BCopiaClick(Sender: TObject);
begin
  Principal.BM.Assign(BM3);
  Bsal := true;
  close;
end;

// Prepara BM3
procedure TFCalcul.PreparaBM3;
begin
    banC := true;
    Image3.Visible := true;

    ncol := ncolA; if ncolB < ncolA then ncol := ncolB;
    nren := nrenA; if nrenB < nrenA then nren := nrenB;

    SetLength(mat3,ncol,nren,3);

    BM3.Width  := ncol;
    BM3.Height := nren;
    BM3.PixelFormat := pf24bit;
    BM3.Canvas.Rectangle(0,0,ncol-1,nren-1);
end;

// Prepara BM3 con modo
procedure TFCalcul.PreparaBM3(modo : char);
begin
  banC := true;
  Image3.Visible := true;
  BM3.PixelFormat := pf24bit;

  case modo of
   's': begin
          ncol := ncolA; if ncolB < ncolA then ncol := ncolB;
          nren := nrenA; if nrenB < nrenA then nren := nrenB;

          SetLength(mat3,ncol,nren,3);

          BM3.Width  := ncol;
          BM3.Height := nren;
          BM3.Canvas.Rectangle(0,0,ncol-1,nren-1);
        end;
   'i': begin
          // Redimensiona BM3 y Mat3
          SetLength(mat3,ncolB,nrenB,3);
          BM3.Width  := ncolB;
          BM3.Height := nrenB;
          BM3.Canvas.Rectangle(0,0,ncolB-1,nrenB-1);
          ncol := ncolB;
          nren := nrenB;
        end;
   end; // del case
   Image3.Visible := true;

end;

{------------------- Ariméticas    ----------------------------}

// suma
procedure TFCalcul.SBsumaClick(Sender: TObject);
begin
  operacion := p_suma;
  Procesa;
end;

// Suma con correccion de rango
procedure TFCalcul.SBSumaRangoClick(Sender: TObject);
begin
  operacion := p_suma_CR;
  Procesa;
end;

// Resta con Valor Absoluto
procedure TFCalcul.SBRestaAbsClick(Sender: TObject);
begin
  operacion := p_resta_abs;
  Procesa;
end;

// Resta con Shift
procedure TFCalcul.SBRestaShiftClick(Sender: TObject);
begin
  operacion := p_resta_shift;
  Procesa;
end;

// Resta a negro
procedure TFCalcul.SBRestaNegroClick(Sender: TObject);
begin
   operacion := p_resta_a_negro;
   Procesa;
end;

//Máximo
procedure TFCalcul.SBmaxClick(Sender: TObject);
begin
   operacion := p_max;
   Procesa;
end;

// Mínimo
procedure TFCalcul.SBminClick(Sender: TObject);
begin
   operacion := p_min;
   Procesa;
end;

// Division
procedure TFCalcul.SBdivisionClick(Sender: TObject);
begin
   operacion := p_div;
   Procesa;
end;

procedure TFCalcul.SBDivision_CorrRangoClick(Sender: TObject);
begin
  operacion :=  p_div_corr;
  Procesa;
end;

// Multiplicación
procedure TFCalcul.SBmultiplicacionClick(Sender: TObject);
begin
  operacion :=  p_mult;
  Procesa;
end;

procedure TFCalcul.SBCombLinealClick(Sender: TObject);
begin
  FormCombLin.ShowModal;
  if FormCombLin.ModalResult = mrOK then begin
    alf := FormCombLin.alf;
    bet := 100 - alf;

    operacion :=  p_Comb_Lin;
    Procesa;
  end;  
end;

{------------------- Logicas       ----------------------------}

// And
procedure TFCalcul.SBandClick(Sender: TObject);
begin
  operacion :=  p_and;
  Procesa;
end;

// Or
procedure TFCalcul.SBorClick(Sender: TObject);
begin
  operacion :=  p_or;
  Procesa;
end;

// Xor
procedure TFCalcul.SBxorClick(Sender: TObject);
begin
  operacion :=  p_xor;
  Procesa;
end;

// Nand
procedure TFCalcul.SBnandClick(Sender: TObject);
begin
  operacion :=  p_nand;
  Procesa;
end;

{--------- Negativo  Sobre el Resultado ---------------------}

procedure TFCalcul.SBNegativoRClick(Sender: TObject);
var
  i,j,k : integer;
begin
  if banC then begin
    PreparaBM3;
    Inicia_Tiempo;

    for i := 0 to ncol-1 do begin
      for j := 0 to nren-1 do
        for k := 0 to 2 do mat3[i,j,k] := _lambda - mat3[i,j,k];
    end;
    Vacia(mat3,BM3);
    Image3.Picture.Assign(BM3);
    Termina_Tiempo;
    Label4.caption :='Negativo de R';
  end
  else ShowMessage('Requiere tener una imagen en Resultado...');

end;

{--------- Logaritmo Sobre el Resultado ---------------------}

procedure TFCalcul.SBRangoDinClick(Sender: TObject);
var
  cc    : single;
  i,j,k : integer;
begin
  if banC then begin
    PreparaBM3;
    Inicia_Tiempo;

    cc := 255/lnXP1(255) ;

    for i := 0 to ncol-1 do begin
      for j := 0 to nren-1 do
        for k := 0 to 2 do mat3[i,j,k] := cc*lnXP1(mat3[i,j,k]);
    end;
    Vacia(mat3,BM3);
    Image3.Picture.Assign(BM3);
    Termina_Tiempo;
    Label4.caption :='Negativo de R';
  end
  else ShowMessage('Requiere tener una imagen en Resultado...');

end;

{------------------- Media por canal y global ---------------}

procedure TFCalcul.SBStatClick(Sender: TObject);
var
  i,j,k,n        : integer;
  sum,sigma,gris : single;
  med            : Vec3;
  ss             : String;
begin
  if banC then begin
    Inicia_Tiempo;

     // limpia medias
    for k := 0 to 3 do med[k] := 0;
    sigma := 0.0;

    for i := 0 to ncol-1 do begin
      for j := 0 to nren-1 do begin
        gris := 0;
        sum := 0.0;
        for k := 0 to 2 do begin
          gris   := mat3[i,j,k] + gris;
          med[k] := med[k] + mat3[i,j,k];
          sum := sum + sqr(mat1[i,j,k] - mat2[i,j,k]);
        end;
        sigma := sigma + sum;
        med[3] := med[3] + gris/3;
      end;
    end;

    n := ncol*nren;

    ss := '';
    for k := 0 to 3 do begin
      med[k] := med[k]/n;
      ss := ss + Format('%10.4f,',[med[k]]);
    end;
    Edit6.Text := Copy(ss,1,length(ss)-1);

    sigma := sqrt(sigma)/(n-3);
    Edit7.Text := FloatToStr(sigma);

    Termina_Tiempo;
    Label4.caption :='Medias por canal del resultado';
  end
  else ShowMessage('Requiere tener una imagen en Resultado...');
end;

{------------------- Memoria     ----------------------------}

procedure TFCalcul.BotR_AClick(Sender: TObject);
begin
  if banC then begin
      BM1.Assign(BM3);
      Copia(BM1,mat1);
      Image1.Picture.Assign(BM1);
      Bola.Brush.Color := clRed;
      Label4.caption :='Res -> A';
  end
  else ShowMessage('Requiere existir el Resultado para esta acción ...');
end;

procedure TFCalcul.BotR_BClick(Sender: TObject);
begin
  if banC
    then begin
      BM2.Assign(BM3);
      Copia(BM2,mat2);
      Image2.Picture.Assign(BM2);
      Bola.Brush.Color := clRed;
      Label4.caption :='Res -> B';
    end
    else ShowMessage('Requiere existir el Resultado para esta acción ...');
end;

// Intercambia A con B
procedure TFCalcul.BotA_BClick(Sender: TObject);
var
  BMtemp : TBitMap;
begin
  if (banA and banb) then begin
    BMTemp:= TBitMap.Create;
    BMTemp.Width  := BM1.Width;
    BMTemp.Height := BM1.Height;
    BMTemp.Assign(BM1);
    BM1.Assign(BM2);
    BM2.Assign(BMTemp);
    BMTemp.Free;

    Image1.Picture.Assign(BM1);
    Copia(BM1,mat1);
    Image2.Picture.Assign(BM2);
    Copia(BM2,mat2);
    Bola.Brush.Color := clRed;
    Label4.caption :='A <-> B';
  end
  else ShowMessage('Debe haber cargado A y B para poder hacer esto ...');


end;

{------------------- Generales   ----------------------------}

// Destrucción de la forma
procedure TFCalcul.FormDestroy(Sender: TObject);
begin
  BM1.Free;
  BM2.Free;
  BM3.Free;
end;

// Botón de Salida
procedure TFCalcul.BotSalidaClick(Sender: TObject);
begin
  close;
end;

// Movimiento del ratón sobre la imagen A
procedure TFCalcul.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  c1,c2,c3  : integer;
  cx,cy,ss  : string;
begin
   if (banC) then begin
     c1 := BM1.Canvas.pixels[x,y] ;
     ss := SConvRGB(c1);
     Edit1.Text := ss;

     c2 := BM2.Canvas.pixels[x,y] ;
     ss := SConvRGB(c2);
     Edit2.Text := ss;

     c3 := BM3.Canvas.pixels[x,y] ;
     ss := SConvRGB(c3);
     Edit3.Text := ss;

     cx := IntToStr(X) ;
     cy := IntToStr(Y) ;
     Edit5.Text := '(' + cx + ',' + cy + ')';
   end;
end;

// Inicializa Tiempo, Barra y Cursor de espera
procedure TFCalcul.Inicia_Tiempo;
begin
    screen.Cursor       := crHourGlass;
    t1                  := now;
    Edit4.Text          := '0';
end;

// Termina Tiempo, Barra y Cursor.
procedure TFCalcul.Termina_Tiempo;
begin
    Image3.Picture.Assign(BM3);
    screen.Cursor := crDefault;

    t2  := Now;

    time1 := DateTimeToTimeStamp(t1);
    time2 := DateTimeToTimeStamp(t2);

    tt1 := time1.time;;
    tt2 := time2.time;
    dt  := (tt2 - tt1) / 1000;
    Edit4.Text := FloatToStr(dt);

    Bola.Brush.Color := clGreen;
    Bola.Visible := True;
end;

// Copia la matriz m1 en m2 (dimensiones ncol x mren)
procedure TFCalcul.Copia(m1 : mat24  ; var m2 :mat24;ncol,mren:integer);
var
 i,j,k : integer;
begin
  for j := 0 to mren-1 do
    for i := 0 to ncol-1 do
      for k := 0 to 2 do
        m2[i,j,k] := m1[i,j,k];
end;


// Copia BitMap -> matriz
procedure TFCalcul.Copia(BM : TBitMap; var mm :mat24);
var
  i,j,k   : integer;
  ren,col : integer;
  p       : PByteArray;
begin
  ren := BM.Height;
  col := BM.Width;
  SetLength(mm,col,ren,3);
  for j := 0 to pred(ren) do begin
    p := BM.ScanLine[j];
    for i := 0 to pred(col) do
      for k := 0 to 2 do mm[i,j,k] := p[3*i+k];
  end;
end;

// Vacia Mat -> BitMap con parametros
procedure TFCalcul.Vacia(mm :mat24; BM : TBitMap);
var
  i,j,k   : integer;
  p       : PByteArray;
begin
  for j := 0 to pred(nren) do begin
    p := BM.ScanLine[j];
    for i := 0 to pred(ncol) do
      for k := 0 to 2 do p[3*i+k] := ajusta255(mm[i,j,k]) ;
  end;
end;

// Vacia Mat -> BitMap con parametros
procedure TFCalcul.Vacia(mm :mat24; n,m : integer; BM : TBitMap);
var
  i,j,k   : integer;
  p       : PByteArray;
begin
  for j := 0 to pred(m) do begin
    p := BM.ScanLine[j];
    for i := 0 to pred(n) do
      for k := 0 to 2 do p[3*i+k] := ajusta255(mm[i,j,k]) ;
  end;
end;

// Convierte un pixel empacado (int) en la cadena (r,g,b)
function TFCalcul.SConvRGB(x : integer) : String;
var
  r,g,b   : integer;
  tt      : string;
begin
  r :=  x         and $FF;
  g := (x shr  8) and $FF;
  b := (x shr 16) and $FF;

  tt := '(' + IntToStr(r) + ',' + IntToStr(g) + ',' + IntToStr(b) + ')';

  SConvRGB := tt;
end;

// --------------------- Morphing elemetal ----------------------

procedure TFCalcul.SpeedButton1Click(Sender: TObject);
begin
  FormMorph.ShowModal;
end;

procedure TFCalcul.SpeedButton2Click(Sender: TObject);
begin
  if (banA and banB) then begin
    PreparaBM3;
    NC     := FormMorph.NC;
    KMorph := 0;
    Dalf   := 1.0/NC;

    Timer1.Interval := 50;
    Timer1.Enabled := true;
    cursor := crHourGlass;
  end
  else ShowMessage('Debe abrir dos imágenes para realizar el proceso');
end;

procedure TFCalcul.Timer1Timer(Sender: TObject);
var
  alf,bet,y,x1,x2 : single;
  i,j,k           : integer;
begin
  bet := Dalf*Kmorph;
  alf := 1.0 - bet;

  for j := 0 to nren-1 do begin
    for i := 0 to ncol-1 do
      for k := 0 to 2 do begin
        x1 := mat1[i,j,k];
        x2 := mat2[i,j,k];
        y  := alf*x1+bet*x2;
        mat3[i,j,k] := round(y) ;
      end;
  end;
  Vacia(mat3,BM3);

  Image3.Picture.Assign(BM3);

  Image3.Refresh;

  inc(Kmorph);
  if Kmorph > NC then begin
    Timer1.Enabled := False;
    cursor := crDefault;
  end;
end;

// ------------------ Manejo del cuadro de seleccion -----------

// Cambia a modo de alineacion superior
procedure TFCalcul.RadioButton1Click(Sender: TObject);
begin
  modo_alinear := 's';
  ImageSel.Visible := false;
  ImageSel.SendToBack;
  Edit8.Visible := false;
  ban_inser := false;
end;

// Cambia a modo de inscripcion
procedure TFCalcul.RadioButton2Click(Sender: TObject);
begin
  modo_alinear := 'i';
  ImageSel.Visible := true;
  ImageSel.BringToFront;

  Edit8.Visible    := true;
  Edit8.Text := Format('(%d,%d)',[X0,Y0]);
end;

procedure TFCalcul.SBResetClick(Sender: TObject);
begin
  X0 := 0; Y0 := 0;

  dib_rect(xm,ym,xm,ym);
  SBReset.Visible := false;
  Edit8.Text := Format('(%d,%d)',[X0,Y0]);
end;

procedure TFCalcul.ImageSelMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  x1,y1,x2,y2 : integer;
begin
  x1 := X-xm; y1 := Y-ym;
  x2 := X+xm; y2 := Y+ym;
  if (x1>=0) and (y1>=0) and (x2<=ncolB) and (y2<=nrenB) then begin
    ban_inser := true;
    SBReset.Visible := false;

    dib_rect(X,Y,xm,ym);
    X0 := X ; Y0 := Y;
    Edit8.Text := Format('(%d,%d)',[X0,Y0]);
  end;
end;

// dibuja rectangulo de insercion en ImageSel
procedure TFCalcul.dib_rect(a,b,p,q : integer);
begin
  with ImageSel.Canvas do begin
    pen.Color := clWhite;
    Rectangle(0,0,ncolB-1,nrenB-1);

    pen.Color := clYellow;
    Rectangle(a-q,b-p,a+p,b+q);
  end;
end;

procedure TFCalcul.ImageSelMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ban_inser := false;
  cursor := crDefault;
  SBReset.Visible := true;
end;

procedure TFCalcul.ImageSelMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  x1,y1,x2,y2 : integer;
begin
  if ban_inser then begin
     x1 := X-xm; y1 := Y-ym;
     x2 := X+xm; y2 := Y+ym;

     if (x1>=0) and (y1>=0) and (x2<=ncolB) and (y2<=nrenB) then begin
       dib_rect(X,Y,xm,ym);
       X0 := x1; Y0 := y1;
       Edit8.Text := Format('(%d,%d)',[X0,Y0]);
     end;
   end;
end;

{ ----------------------------- FIN ----------------------------------------- }
{                            Calculadora                                      }
{ --------------------------------------------------------------------------- }


begin
  cc := 255/ln(256);
end.


