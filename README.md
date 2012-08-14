Vanguard
========

[![Build Status](https://secure.travis-ci.org/brendanhay/vanguard.png)](http://travis-ci.org/brendanhay/vanguard)

Table of Contents
-----------------

* [Introduction](#introduction)
* [Features](#features)
* [Build](#build)
* [Testing](#testing)
* [Configure](#configure)
* [Contribute](#contribute)
* [Licence](#licence)


<a name="introduction" />

Introduction
------------

> TODO

It is currently in a prototype stage.


<a name="features" />

Features
--------

> TODO


<a name="build" />

Build
-----

```shell
make
```


<a name="testing" />

Testing
-------

> TODO

```shell
make test        # All
make unit        # Unit + Property tests only
make integration # Integration suites
```

Both `unit` and `integration` targets support an `ENV` variable `T` specifying a specific suite to run. For example:

```shell
make unit T=vanguard_util            # The eunit module, minus the '_test.erl' suffix
make integration T=vanguard_listener # A common_test suite, minus the '_SUITE.erl' suffix
```

There is also a sub-directory `./dev` which contains a set of [foreman](github.com/ddollar/foreman) related configuration to start the two backend nodes
referenced in the default configuration.


<a name="configure" />

Configure
---------

> TODO


<a name="contribute" />

Contribute
----------

For any problems, comments or feedback please create an issue [here on GitHub](github.com/brendanhay/vanguard/issues).


<a name="licence" />

Licence
-------

Vanguard is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/)
