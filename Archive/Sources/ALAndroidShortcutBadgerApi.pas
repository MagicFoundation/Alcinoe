unit ALAndroidShortcutBadgerApi;

interface

uses
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.App;

type

  {**************************}
  JShortcutBadger = interface;

  {********************************************}
  JShortcutBadgerClass = interface(JObjectClass)
    ['{9B06CB54-F019-4862-9F80-9EB0AB319E87}']
    {class} function applyCount(context: JContext; badgeCount: Integer): Boolean; cdecl;
    {class} procedure applyCountOrThrow(context: JContext; badgeCount: Integer); cdecl;
    {class} procedure applyNotification(context: JContext; notification: JNotification; badgeCount: Integer); cdecl;
    {class} function isBadgeCounterSupported(context: JContext): Boolean; cdecl;
    {class} function removeCount(context: JContext): Boolean; cdecl;
    {class} procedure removeCountOrThrow(context: JContext); cdecl;
  end;

  {********************************************************}
  [JavaSignature('me/leolin/shortcutbadger/ShortcutBadger')]
  JShortcutBadger = interface(JObject)
    ['{EA827BF1-5D29-4B36-B03A-63C980B1DCF8}']
  end;
  TJShortcutBadger = class(TJavaGenericImport<JShortcutBadgerClass, JShortcutBadger>) end;


implementation

procedure RegisterTypes;
begin
  TRegTypes.RegisterType('ALAndroidShortcutBadgerApi.JShortcutBadger', TypeInfo(ALAndroidShortcutBadgerApi.JShortcutBadger));
end;

initialization
  RegisterTypes;

end.
