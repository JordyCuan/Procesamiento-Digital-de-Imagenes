unit Unit1;

interface

{$define  GIF}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, gfx_files, ExtCtrls, ComCtrls, StdCtrls;

type
  TForm1 = class(TForm)
    ScrollBox1: TScrollBox;
    Image1: TImage;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    StatusBar1: TStatusBar;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Memo1: TMemo;
    Button4: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    BB   : TBitMap;
    nom  : string;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Image1.AutoSize := true;
  OpenDialog1.Filter := _OpenFilterString;
  OpenDialog1.FilterIndex := 0;
  OpenDialog1.InitialDir := '.';
  SaveDialog1.Filter := _SaveFilterString;
  SaveDialog1.FilterIndex := 0;
  SaveDialog1.InitialDir := '.';

  BB := TBitMap.Create;
  Memo1.Clear;
  Memo1.ReadOnly := True;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  close;
end;

procedure TForm1.Button1Click(Sender: TObject);
const
  tform : array [0..8] of string =
           ('pfDevice', 'pf1bit', 'pf4bit', 'pf8bit', 'pf15bit',
            'pf16bit', 'pf24bit', 'pf32bit', 'pfCustom');

var
  ss : String;
  id : file of Byte;
  pf : TPixelFormat;
  kpf: byte;
begin
  if OpenDialog1.Execute then begin
     nom := OpenDialog1.FileName;
     StatusBar1.Panels[0].Text := nom;
     LoadAnyImageToBMP(BB,nom,false,0,0);
     Image1.Picture.Assign(BB);

     // Datos de la Imagen
     with Memo1.Lines do begin
       Memo1.Font.color := clBlue;
       Memo1.Font.Size   := 10;
       ss := ExtractFileName(nom);
       add('-------------');
       add('Archivo: '+ss);
       assignFile(id,nom);
       reset(id);
       add('Tamaño: '+ IntToStr(FileSize(id)));
       closeFile(id);

       pf  := BB.PixelFormat;
       kpf := ord(pf);
       add('Formato: '+ tform[kpf] );

       add('Ancho: '+ IntToStr(BB.Width ));
       add('Alto : '+ IntToStr(BB.Height ));
     end;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  noms : string;
begin
  if SaveDialog1.Execute then begin
    noms := SaveDialog1.FileName;
    SaveBMPToAnyFile(BB,noms,0,0);
    if nom <> noms then begin
      nom := noms;
      StatusBar1.Panels[0].Text := nom;
    end;
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  Memo1.Clear;
end;

end.
