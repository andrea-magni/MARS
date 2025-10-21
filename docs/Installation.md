# MARS Curiosity Installation

* MARS has an executable installer. Check [latest release page](https://github.com/andrea-magni/MARS/releases/latest).

# MARS Curiosity Manual Installation

1. Grab a copy of MARS (git clone or download ZIP)
1. Add five folders to your Library Path:
    * [MARS Folder]\Source
    * [MARS Folder]\ThirdParty\delphi-jose-jwt\Source
    * [MARS Folder]\ThirdParty\mORMot\Source
    * [MARS Folder]\ThirdParty\Neslib.Yaml
    * [MARS Folder]\ThirdParty\Neslib.Yaml\Neslib
1. Packages (example for 13 Florence):
    * Open [MARS Folder]\Packages\13Florence\MARS.groupproj
      * Build All
    * Open [MARS Folder]\Packages\13Florence\MARSClient.groupproj
      * Build All
      * Install MARSClient.CoreDesign
      * Install MARSClient.FireDACDesign 

(please adjust according to your Delphi version)

> Compatibility: **Recent Delphi versions (from 10.4 up to 13 Florence)** (older versions should be quite compatible, down to XE7)
