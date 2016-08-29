program Spider;

uses
  Vcl.Themes,
  Vcl.Styles,
  Forms,
  Collections.Bags in 'Collections\Collections.Bags.pas',
  Collections.Base in 'Collections\Collections.Base.pas',
  Collections.BidiDictionaries in 'Collections\Collections.BidiDictionaries.pas',
  Collections.BidiMaps in 'Collections\Collections.BidiMaps.pas',
  Collections.Dictionaries in 'Collections\Collections.Dictionaries.pas',
  Collections.Dynamic in 'Collections\Collections.Dynamic.pas',
  Collections.Lists in 'Collections\Collections.Lists.pas',
  Collections.MultiMaps in 'Collections\Collections.MultiMaps.pas',
  Collections.Queues in 'Collections\Collections.Queues.pas',
  Collections.Serialization in 'Collections\Collections.Serialization.pas',
  Collections.Sets in 'Collections\Collections.Sets.pas',
  Collections.Stacks in 'Collections\Collections.Stacks.pas',
  CollectList in 'CollectList.pas',
  GdiPlus in 'GdiPlus\GdiPlus.pas',
  GdiPlusHelpers in 'GdiPlus\GdiPlusHelpers.pas',
  JclPeImage in 'JclPeImage.pas',
  JclTD32Ex in 'JclTD32Ex.pas',
  ClassUtils in 'ClassUtils.pas',
  DbgCodeProfiler in 'DbgCodeProfiler.pas',
  DbgHookTypes in 'DbgHookTypes.pas',
  DbgMemoryProfiler in 'DbgMemoryProfiler.pas',
  DbgSamplingProfiler in 'DbgSamplingProfiler.pas',
  DbgSyncObjsProfiler in 'DbgSyncObjsProfiler.pas',
  DbgWorkerThread in 'DbgWorkerThread.pas',
  Debugger in 'Debugger.pas',
  DebuggerTypes in 'DebuggerTypes.pas',
  DebugHook in 'DebugHook.pas',
  DebugInfo in 'DebugInfo.pas',
  DelphiDebugInfo in 'DelphiDebugInfo.pas',
  MapDebugInfo in 'MapDebugInfo.pas',
  Spider.ActionController in 'Spider.ActionController.pas',
  Spider.DebugerThread in 'Spider.DebugerThread.pas',
  Spider.ExceptionHook in 'Spider.ExceptionHook.pas',
  Spider.Feedback in 'Spider.Feedback.pas' {frmFeedback},
  Spider.GA in 'Spider.GA.pas',
  Spider.Main in 'Spider.Main.pas' {MainForm},
  Spider.ProcessList in 'Spider.ProcessList.pas' {frmProcessList},
  Spider.ProjectOptions in 'Spider.ProjectOptions.pas' {fmProjectOptions},
  Spider.RWLock in 'Spider.RWLock.pas',
  Spider.SelectSource in 'Spider.SelectSource.pas' {fmSelectSource},
  Spider.ShareData in 'Spider.ShareData.pas' {dmShareData: TDataModule},
  Spider.SharedObject in 'Spider.SharedObject.pas',
  Spider.SourceViewFrame in 'Spider.SourceViewFrame.pas' {SourceViewFrame: TFrame},
  Spider.SpiderOptions in 'Spider.SpiderOptions.pas',
  Spider.SQLiteDB in 'Spider.SQLiteDB.pas',
  Spider.UpdateInfo in 'Spider.UpdateInfo.pas',
  WinAPIUtils in 'WinAPIUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  TStyleManager.TrySetStyle('Silver');
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Spider';
  Application.CreateForm(TdmShareData, dmShareData);
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
