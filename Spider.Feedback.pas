unit Spider.Feedback;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.Buttons, Vcl.ActnList, System.Actions;

type
  TfrmFeedback = class(TForm)
    acSend: TAction;
    AL: TActionList;
    btnSend: TBitBtn;
    cbbType: TComboBoxEx;
    eEMail: TEdit;
    lbEmail: TLabel;
    lbFeedbackType: TLabel;
    lbMessage: TLabel;
    mMessage: TMemo;
    procedure acSendExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function GetFeedbackText: String;
    function GetFeedbackType: String;
  public
    property FeedbackType: String read GetFeedbackType;
    property FeedbackText: String read GetFeedbackText;
  end;

implementation

uses
  Spider.ShareData;

{$R *.dfm}

procedure TfrmFeedback.acSendExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfrmFeedback.FormCreate(Sender: TObject);
begin
  cbbType.ItemIndex := 0;
  mMessage.Text := '';
end;

function TfrmFeedback.GetFeedbackText: String;
begin
  Result := Format('[%s] %s', [eEMail.Text, mMessage.Text]);
end;

function TfrmFeedback.GetFeedbackType: String;
begin
  Result := cbbType.ItemsEx.ComboItems[cbbType.ItemIndex].Caption;
end;

end.
