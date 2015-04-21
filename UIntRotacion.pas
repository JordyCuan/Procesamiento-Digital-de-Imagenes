unit UIntRotacion;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ComCtrls,
  Vcl.ExtCtrls;

type
  TFormRot = class(TForm)
    StaticText1: TStaticText;
    TrackBar1: TTrackBar;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Label1: TLabel;
    StaticText2: TStaticText;
    TrackBar2: TTrackBar;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    RadioGroup1: TRadioGroup;
    procedure TrackBar1Change(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    angRot,angRot2 : single;
  end;

var
  FormRot: TFormRot;

implementation

{$R *.dfm}

procedure TFormRot.RadioGroup1Click(Sender: TObject);
begin
    if RadioGroup1.ItemIndex = 0 then begin
      TrackBar1.Enabled:=true;
      TrackBar2.Enabled:= false;
      angRot := TrackBar1.Position / 10;
    end
      else begin
      TrackBar1.Enabled:=false;
      TrackBar2.Enabled:= true;
      angRot2:=TrackBar2.Position/10;
      end;
end;

procedure TFormRot.TrackBar1Change(Sender: TObject);
begin
  angRot := TrackBar1.Position / 10;
  Label1.Caption := Format('%5.1f',[angRot]) + ' °';
  angRot2:=TrackBar2.Position/10;
  Label3.Caption:=Format('%5.1f',[angRot2])+' ';
end;

end.
