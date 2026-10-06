# MARS Curiosity Installation

* MARS has an executable installer. Check [latest release page](https://github.com/andrea-magni/MARS/releases/latest).
* MARS can also be installed with [TMS Smart Setup](https://github.com/tmssoftware/smartsetup) (Delphi 10.4 and newer): `tms server-enable community true`, then `tms install andreamagni.mars`. See the [installation guide](https://andrea-magni.github.io/MARS/guide/installation#tms-smart-setup).

# MARS Curiosity Manual Installation

1. Grab a copy of MARS: `git clone --recurse-submodules https://github.com/andrea-magni/MARS.git` (ThirdParty\delphi-jose-jwt and ThirdParty\Delphi-Mocks are git submodules)
1. Add seven folders to your Library Path:
    * [MARS Folder]\Source
    * [MARS Folder]\ThirdParty\delphi-jose-jwt\Source\Common
    * [MARS Folder]\ThirdParty\delphi-jose-jwt\Source\JOSE
    * [MARS Folder]\ThirdParty\mORMot\Source
    * [MARS Folder]\ThirdParty\Neslib.Yaml
    * [MARS Folder]\ThirdParty\Neslib.Yaml\Neslib
    * [MARS Folder]\ThirdParty\Delphi-Mocks\Source (test projects)
1. Packages (example for 13 Florence):
    * Open [MARS Folder]\Packages\13Florence\MARS.groupproj
      * Build All
    * Open [MARS Folder]\Packages\13Florence\MARSClient.groupproj
      * Build All
      * Install MARSClient.CoreDesign
      * Install MARSClient.FireDACDesign 

(please adjust according to your Delphi version)

> Compatibility: **Delphi 10.4 Sydney up to 13 Florence**. Earlier versions are not supported.
