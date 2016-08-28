unit Spider.ShareData;

interface

uses
  System.SysUtils, System.Classes, Vcl.ImgList, Vcl.Controls, Vcl.XPMan,
  Vcl.ActnMan, Vcl.ActnColorMaps, SynEditHighlighter, SynHighlighterPas,
  SynEditMiscClasses, SynEditRegexSearch, SynEditSearch, SynEditOptionsDialog;

type
  TdmShareData = class(TDataModule)
    ilActionsSmall: TImageList;
    imlMainSmall: TImageList;
    imlMain: TImageList;
    synPas: TSynPasSyn;
    synRegexSearch: TSynEditRegexSearch;
    synEditSearch: TSynEditSearch;
    synEditOptDlg: TSynEditOptionsDialog;
  end;

var
  dmShareData: TdmShareData;

implementation

{$R *.dfm}

end.
