# MARS Curiosity Installation

1. Grab a copy of MARS (git clone or download zip)
1. Add five folders to your Library Path:
    * [MARS Folder]\Source
    * [MARS Folder]\ThirdParty\delphi-jose-jwt\Source
    * [MARS Folder]\ThirdParty\mORMot\Source
    * [MARS Folder]\ThirdParty\Neslib.Yaml
    * [MARS Folder]\ThirdParty\Neslib.Yaml\Neslib
1. Packages (example for 12 Athens):
    * Open [MARS Folder]\Packages\12Athens\MARS.groupproj
      * Build All
    * Open [MARS Folder]\Packages\12Athens\MARSClient.groupproj
      * Build All
      * Install MARSClient.CoreDesign
      * Install MARSClient.FireDACDesign 

(please correct accordingly to your Delphi version)

> Compatibility: **Recent Delphi versions (from XE7 up to 12 Athens)** (older versions should be quite compatible)
