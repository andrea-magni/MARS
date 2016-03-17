![MARS-curiosity logo](http://www.andreamagni.eu/images/MARS-Curiosity-d.png)

_\* The Delphi stylized helmet icon is trademark of Embarcadero Technologies._

# MARS - Curiosity
*Delphi REST Library*

Pure REST approach, standard concepts in a familiar Delphi flavor (including a component based client library).

- Known compatibility: **Delphi versions from XE4 to 10 Seattle**
- *Some functionalities requires FireDAC*
- *Should compile since Delphi XE, maybe 2010, with minor changes*

# The project

I started this project in 2015, it was formerly named WiRL and it started as an experiment to show Delphi's modern language features applied.
At the time (December, 2015) the project was published on GitHub under its new name of MARS-library, most (>80%) of the code was written by me with contributions by Nando Dessena, Stefan Glienke and Paolo Rossi, plus some other minor contributors who provided some bugfixes here and there.

In January, 2016, I decided to start my own fork of the MARS-library project and to give it the name of MARS-Curiosity.
What really makes me happy about this project is that some of real world applications are already out there, built with this technology and developers who had the chance to try it were happy to embrace its philosophy and ease of use.

## The name

It is always challenging to find a good name for a new project: usually you have to balance between a brand new name and some classical or already taken names. The first option mean originality, the second can carry some more meaning with the name itself.
I have chosen MARS-curiosity (not exactly an original name) not only because I love space exploration but also because they represents the highest level of human skills applied to technology and I admire the enthusiasm and strength that can lead so many people to work hard to bring success in these space exploration missions.

### MARS

Mars is the fourth planet in the solar sistem. One of the smallest, compared to other planets in the system, but it represents the next "giant leap" for humanity since it has been choosen as the target for next manned space exploration mission. Currently, it hosts seven functioning spacecraft (5 in orbite, 2 on the surface), making it a planet under robot control :wink:

### Curiosity
Curiosity is the name of the Mars Science Laboratory rover. It was launched on November 2011 26th and successfully landed on Mars (Gale Crater) on August 2012 6th.
The overall objective of the mission is to investigate on habitability of Mars, studying its climate and geology, and collecting data for a manned mission to Mars. The rover carries a variety of scientific instruments designed by an international team and the landing process has been terrific, with the use a powered descent stage and a innovative sky crane system. (https://en.wikipedia.org/wiki/Mars_Science_Laboratory)

The name of the rover was given after running a contest through the NASA website. The winner had these (very inspiring) words:
> Curiosity is the passion that drives us through our everyday lives. We have become explorers and scientists with our need to ask questions and to wonder.
> - Clara Ma

# Change log
### Since the fork (Jan 2016)
- fixed binary streaming
- several fixes for bugs and memory leaks
- integration of (part of) JWT method (http://jwt.io/) against Indy's (cookie based) session management
- enhanced support for HTTP custom headers
- refactoring (from Java-like to more Delphi-like approach)
- added some utilities (i.e. Parameters support) and shortcuts
- added Engine registry
- restructured Source folder (unique folder now)

# The roadmap

### Major functionalities:
- [ ] Fix the MARS Client library (JWT changes)
- [ ] Refactor the MARS Client library (separate core from components)
- [ ] Move Token handling to a plugin
- [ ] Deploy server as a Windows service
- [ ] Improve error management (server side)
- [ ] Improve error management (client side)
- [ ] Enable parallel requests from client library
- [ ] Readers (automatic deserialization from http request body and parameters)
- [ ] Deploy server as Apache module
- [ ] Deploy server as IIS ISAPI module 
- [ ] Implement a session persistence mechanism (server-side)
- [ ] Cache mechanisms support

### Minor functionalities:
- [ ] Parameters JSON persistence
- [ ] Enable emission of API schema and information
- [ ] Provide an example of high scability (multiple application servers, possibly with a session persistence server node)
- [ ] Fix the static webserver extension

Aside of these functionalities, I will add unit and integration testing (refactoring code where needed).

# Resources
- [MARS-curiosity Google+ Community](https://plus.google.com/communities/109223723982639531185)
- [Andrea Magni's Blog](http://www.andreamagni.eu/wp/tags/mars-curiosity)

# Contributions
This is an open source project, so obviously every contribution/help/suggestion will be very appreciated.

[Andrea Magni](http://www.andreamagni.eu)
