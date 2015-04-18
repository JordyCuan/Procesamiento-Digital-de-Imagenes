unit UnitZoom;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons;

type
  TForm2 = class(TForm)
    BitBtn3: TBitBtn;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    RadioButton1: TRadioButton;
    Label3: TLabel;
    RadioButton2: TRadioButton;
    Label4: TLabel;
    Label5: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Label6: TLabel;
    Label7: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
  private
    { Private declarations }
  public
  nnc,nnr:integer;
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.BitBtn2Click(Sender: TObject);
var
test  : integer;
err   : integer;
begin
val(Edit1.Text,test,err);
if(err<>0)then begin
  ShowMessage('Valor Entero Mal Formado');
  exit;
end;
test:=strToInt(Edit1.Text);
if(test<16)or (test>=3000) then begin
  ShowMessage('Error Ancho...');
  exit;
end;
test:=strToInt(Edit2.Text);
if(err<>0)then begin
  ShowMessage('Valor Entero Mal Formado');
  exit;
end;

test:=strToInt(Edit2.Text);
if(test<=16) or (test>=3000)then begin
  ShowMessage('Error Alto...');
  exit;
end;
BitBtn3.Enabled:=true;
end;

procedure TForm2.FormClose(Sender: TObject; var Action: TCloseAction);
begin
nnc:=StrToInt(Edit1.Text);
nnr:=StrToInt(Edit2.Text);

end;

procedure TForm2.BitBtn3Click(Sender: TObject);
begin
BitBtn3.Enabled:=false;
end;

end.
