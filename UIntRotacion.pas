unit UIntRotacion;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ComCtrls;

type
  TFormRot = class(TForm)
    StaticText1: TStaticText;
    TrackBar1: TTrackBar;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Label1: TLabel;
    procedure TrackBar1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    angRot : single;
  end;

var
  FormRot: TFormRot;

implementation

{$R *.dfm}

procedure TFormRot.TrackBar1Change(Sender: TObject);
begin
  angRot := TrackBar1.Position / 10;
  Label1.Caption := //FloatToStr(angRot) + ' °';
                    Format('%5.1f',[angRot]) + ' °';
end;

end.
