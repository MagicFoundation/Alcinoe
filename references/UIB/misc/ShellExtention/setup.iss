#define MyAppName "UIB Database Tool"
#define MyAppVerName "UIB Database Tool 1.0"
#define MyAppPublisher "progdigy.com"
#define MyAppURL "http://www.progdigy.com"

[Setup]
AppName={#MyAppName}
AppVerName={#MyAppVerName}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName={pf}\UIBDatabaseTool
DefaultGroupName=My Program
OutputBaseFilename=setup
Compression=lzma
SolidCompression=yes

[Files]
Source: "D:\developpement\UIBUtil\UIBFileHandler.dll"; DestDir: "{app}"; Flags: ignoreversion regserver

