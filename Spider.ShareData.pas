unit Spider.ShareData;

interface

uses
  SysUtils, Classes, ImgList, Controls, XPMan, ActnMan, ActnColorMaps,
  SynEditHighlighter, SynHighlighterPas, SynEditMiscClasses,
  SynEditRegexSearch, SynEditSearch, SynEditOptionsDialog;

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
