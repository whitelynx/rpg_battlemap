RPG Battlemap
=====

RPG Battlemap is a digital map for table top games (role-play or
otherwise). It allows game masters to create simple maps to share with the
other players. The players can add or remove tokens or scenery. All of this
is saved and updates in connected browsers in real time.

Technologies
=====

[Persona](https://login.persona.org/) is used for authentication.

[AngularJS](http://angularjs.org) is used for the front end.

[Cowboy](http://ninenines.eu/docs/en/cowboy/HEAD/guide/toc) is used for the
web backend.

Installation
====

While rebar is used under the hood for the build system, Make is used as
the primary interface.

To do a quick development setup:

```bash
$ git clone http://github.com/lordnull/rpg_battlemap.git
$ cd rpg_battlemap
% ./hooks.sh pre_compile
$ make devrel
$ ./rel/rpg_battlemap/bin/rpg_battlemap start
```

By default the application runs at https://localhost:9090 . If you want to
use a different port or host, edit
'rel/rpg_battlemap/releases/1/sys.config'.

Tests
=====

Currently playing with angularjs and testacular. To get running:

1. install node.js
2. install npm
3. install testacular
4. install phantomjs

make test will only run the erlang test suite.
