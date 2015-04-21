unit UnitZoom;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ComCtrls,
  Vcl.ExtCtrls;

type
  TForm2 = class(TForm)
    BitBtn3: TBitBtn;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Label6: TLabel;
    Label7: TLabel;
    RadioGroup1: TRadioGroup;
    Label3: TLabel;
    TrackBar1: TTrackBar;
    Label8: TLabel;
    Label9: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    { Private declarations }
  public
  nnc,nnr:integer;
  pos,nnrp,nncp,temp    :single;
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
if RadioGroup1.ItemIndex=0 then  begin
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

if RadioGroup1.ItemIndex=1 then begin
BitBtn3.Enabled:=true;
end;
end;

procedure TForm2.FormClose(Sender: TObject; var Action: TCloseAction);
begin
nnc:=StrToInt(Edit1.Text);
nnr:=StrToInt(Edit2.Text);

end;

procedure TForm2.RadioGroup1Click(Sender: TObject);
begin
  if RadioGroup1.ItemIndex= 0 then begin
    Edit1.Enabled:=true;
    Edit2.Enabled:=true;
    TrackBar1.Enabled:= false;
  end
    else begin
      Edit1.Enabled:=false;
      Edit2.Enabled:=false;
      TrackBar1.Enabled:=true;
    end;

end;

procedure TForm2.TrackBar1Change(Sender: TObject);
begin
  pos:=TrackBar1.Position;
  Label8.Caption := Format('%5.1f',[pos]) + ' %';
end;

procedure TForm2.BitBtn3Click(Sender: TObject);
begin
BitBtn3.Enabled:=false;
end;

end.
