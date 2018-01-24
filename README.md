![MARS-curiosity logo](https://www.andreamagni.eu/images/MARS-Curiosity-d.png)

_\* The Delphi stylized helmet icon is trademark of Embarcadero Technologies._

# MARS - Curiosity
*Delphi REST Library*

Build your REST applications (server and client) with my library:
1. lightweight: no dictations on your application code, no heavy dependencies, take what you need of the library;
1. standard: build Delphi REST servers to be consumed by other technologies (including web apps, .Net, Java, php...) and build your client applications against any REST server;
1. Delphi-like: built using modern Delphi features and enabled with Delphi-to-Delphi specific facilities to get more power!

- Compatibility: **Recent Delphi versions (from XE7 up to 10.2.2 Tokyo)** (older versions should be quite compatible)

# Get started
* Grab a copy of MARS (git clone or download zip)
* Add three folders to your Library Path:
  * [MARS Folder]\Source
  * [MARS Folder]\ThirdParty\delphi-jose-jwt\Source
  * [MARS Folder]\ThirdParty\mORMot\Source
* Packages (example for 10.2 Tokyo Enterprise):
  * Open [MARS Folder]\Packages\102Tokyo\MARS.Enterprise.groupproj
    * Build All
  * Open [MARS Folder]\Packages\102Tokyo\MARSClient.Enterprise.groupproj
    * Build All
    * Install MARSClient.Core
    * Install MARSClient.FireDAC    

(please correct accordingly to your Delphi version and edition)

# Demos and MARSTemplate
* Try some demos (i.e. "Demos\HelloWorld", "Demos\Authorization", "Demos\FireDAC Basic")
* compile and run the MARScmd_VCL.dproj in [MARS Folder]\Utils\Source\MARScmd, it will help you to create your first project by cloning "Demos\MARSTemplate" into a new folder

# Map (list most functionalities and concepts)

[PDF](media/MARS-Curiosity%20Map.pdf) | [PNG](media/MARS-Curiosity%20Map.png)
![MARS map](media/MARS-Curiosity%20Map.png)

# Contributions
This is an open source project, so obviously every contribution/help/suggestion will be very appreciated.
Most of the code has been written by me with some significant contributions by Nando Dessena and Stefan Glienke. Some of my customers actually act as beta testers and early adopters (I want to thank them all for the trust and efforts).

[Andrea Magni](http://www.andreamagni.eu)
