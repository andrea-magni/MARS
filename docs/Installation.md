# MARS Curiosity Installation

1. Grab a copy of MARS (git clone or download zip)
1. Add three folders to your Library Path:
    * [MARS Folder]\Source
    * [MARS Folder]\ThirdParty\delphi-jose-jwt\Source
    * [MARS Folder]\ThirdParty\mORMot\Source
1. Packages (example for 10.2 Tokyo):
    * Open [MARS Folder]\Packages\102Tokyo\MARS.groupproj
      * Build All
    * Open [MARS Folder]\Packages\102Tokyo\MARSClient.groupproj
      * Build All
      * Install MARSClient.CoreDesign
      * Install MARSClient.FireDACDesign 

(please correct accordingly to your Delphi version)

> Compatibility: **Recent Delphi versions (from XE7 up to 10.3 Rio)** (older versions should be quite compatible)
