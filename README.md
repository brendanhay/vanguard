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

At [SoundCloud](http://soundcloud.com) we have a number of disparate RabbitMQ instances which are all loosely related and interconnected via federation links or shovel configurations.

While the default RabbitMQ Management UI works great for cluster overviews, it's definitely a chore in an unclustered topology to have check many instances of the Management UI to get a feel for node health and thoroughput.

Vanguard is an attempt at keeping the UI useful and accessible by providing a very lightweight proxy server which runs the Management UI, calls out to seperate
backend API instances, aggregates/munges/unions the result, and presents it back to the user in the standard UI.

The following screenshot shows Vanguard serving the Management UI for two seperate RabbitMQ backends which are not clustered in any way:

![Vanguard](https://raw.github.com/brendanhay/vanguard/master/img/vanguard.png)

Vanguard ships with a copy of the static assets from the [rabbitmq-management](https://github.com/rabbitmq/rabbitmq-management) plugin, which at the time of writing is at version `2.8.4`.

If you are running an older version of RabbitMQ than this, there may be some strange behaviour on some of the tabs. For example `/#/exchanges` doesn't work correctly with `2.7.0` backends.

> Vanguard is still under development, with the intention of ironing out a few bugs
in any of the readonly actions. Write actions (such as publishing messages, deleting bindings, etc.) will not be supported.

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

> I strongly suggest using RabbitMQ users which have readonly permissions and the `monitoring` tag set in the `BACKENDS` URIs.

To play around with Vanguard locally you will need two seperate terminals open. In the first terminal:

```shell
make dev
```

Which will start two backend RabbitMQ instances. (You can configure which `AMQP` port range is used by looking in `./dev/run`). In the second terminal:

```shell
make build
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
