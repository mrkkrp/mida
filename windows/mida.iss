;;;;
;;;; Inno Setup Script for MIDA 0.3.0
;;;;
;;;; Copyright (c) 2014 Mark Karpov
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Please note that in order to use this file with Inno Setup you will need
;; to recreate prototype of installation directory in 'mida' subdirectory.

;; Proposed directory structure:
;; mida
;; |-- doc
;; |   |-- index.html
;; |   `-- ascetic.css
;; |-- LICENSE.md
;; |-- mida.exe
;; |-- NEWS.md
;; `-- README.md

[Setup]
AppId = {{4BB579A5-13F5-4DE4-B5F4-68937769D84B}
AppName = "MIDA"
AppVersion = "0.3.0"
AppPublisher = "Mark Karpov"
AppPublisherURL = "https://github.com/mrkkrp/mida/"
AppSupportURL = "https://github.com/mrkkrp/mida/"
AppUpdatesURL = "http://github.com/mrkkrp/mida/"
DefaultDirName = "{pf}\mida"
DefaultGroupName = "MIDA"
LicenseFile = "LICENSE.md"
InfoBeforeFile = "README.md"
OutputDir = "."
OutputBaseFilename = "mida-0.3.0"
SetupIconFile = ""
Compression = "lzma"
SolidCompression = yes
ChangesAssociations = yes

[Languages]
Name: "english";     MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[Files]
Source: "mida\*";    DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs

[Icons]
Name: "{group}\MIDA";            Filename: "{app}\mida.exe"
Name: "{group}\Uninstall MIDA";  Filename: "{uninstallexe}"

[Run]
Filename: "{app}\mida.exe"; Description: "{cm:LaunchProgram,MIDA}"; Flags: nowait postinstall skipifsilent

[Registry]
Root: HKLM; Subkey: "SYSTEM\CurrentControlSet\Control\Session Manager\Environment"; ValueType: expandsz; ValueName: "Path"; ValueData: "{olddata};{app}"; Check: NeedsAddPath('{app}')

[Code]

function NeedsAddPath(Param: string): boolean;
var
  OrigPath: string;
begin
  if not RegQueryStringValue(HKEY_LOCAL_MACHINE,
    'SYSTEM\CurrentControlSet\Control\Session Manager\Environment',
    'Path', OrigPath)
  then begin
    Result := True;
    exit;
  end;
  Result := Pos(';' + ExpandConstant(Param) + ';', ';' + OrigPath + ';') = 0;
end;
