unit UCapZonas;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids;

type
  TFFCapZonas = class(TForm)
    Label1: TLabel;
    StringGrid1: TStringGrid;
    Button1: TButton;
    Button2: TButton;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    NR : byte;
  public
    { Public declarations }
  end;

var
  FFCapZonas: TFFCapZonas;

implementation

uses Unit1;

{$R *.dfm}

procedure TFFCapZonas.Button1Click(Sender: TObject);
begin
  Form1.Elimina_ruido();
end;

procedure TFFCapZonas.Button2Click(Sender: TObject);
var
  i,j : integer;
begin
  for i := 1 to NR - 1 do
    for j := 1 to 4 do
      StringGrid1.Cells[j,i] := '';
  Form1.p_ruido := 1;
end;

procedure TFFCapZonas.FormCreate(Sender: TObject);
var
  k : byte;
begin
  StringGrid1.Cells[1,0] := 'X';
  StringGrid1.Cells[2,0] := 'Y';
  StringGrid1.Cells[3,0] := 'L';
  StringGrid1.Cells[4,0] := 'E';

  NR := StringGrid1.RowCount;
  for k := 0 to NR - 1 do
    StringGrid1.Cells[0,k+1] := IntToStr(k+1);

end;

end.
