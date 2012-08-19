Vanguard
========

[![Build Status](https://secure.travis-ci.org/brendanhay/vanguard.png)](http://travis-ci.org/brendanhay/vanguard)


Table of Contents
-----------------

* [Introduction](#introduction)
* [Run](#run)
* [Testing](#test)
* [Contribute](#contribute)
* [Licence](#licence)


<a name="introduction" />

Introduction
------------

![Vanguard](https://raw.github.com/brendanhay/vanguard/master/img/vanguard.png)


<a name="run" />

Run
---

Vanguard is deployed internally at [SoundCloud](http://soundcloud.com) following something like the [12factor](http://www.12factor.net/) approach - this means all configuration is set through `ENV` variables:

```shell
PORT=8080
BACKENDS=http://guest:guest@localhost:55670,http://guest:guest@localhost:55680
```

`PORT` is the HTTP listener's port, and `BACKENDS` is a comma seperated
string of URIs containing auth, host, and port information for the backend
RabbitMQ HTTP API instances. These will all need to be exported under the user running Vanguard.

To play around with Vanguard locally you will need two seperate terminals open. In the first terminal:

```shell
make dev
```

Which will start two backend RabbitMQ instances. (You can configure which `AMQP` port range is used by looking in `./dev/run`). In the second terminal:

```shell
foreman start
```

Which will use the `.env` file in the root directory, and start Vanguard connecting to the two previously started RabbitMQ APIs. Then point your browser at [localhost:8080](http://localhost:8080).


<a name="test" />

Test
----

Run all the tests:

```shell
make test
```

The `unit` target supports an `ENV` variable `T` specifying a specific suite to run. For example:

```shell
make unit T=vanguard_json # Name of the eunit module, minus the '_test.erl' suffix
```


<a name="contribute" />

Contribute
----------

For any problems, comments or feedback please create an issue [here on GitHub](github.com/brendanhay/vanguard/issues).


<a name="licence" />

Licence
-------

Vanguard is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/)
