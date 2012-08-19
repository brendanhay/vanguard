Vanguard
========

[![Build Status](https://secure.travis-ci.org/brendanhay/vanguard.png)](http://travis-ci.org/brendanhay/vanguard)


Table of Contents
-----------------

* [Introduction](#introduction)
* [Running](#run)
* [Testing](#test)
* [Contribute](#contribute)
* [Licence](#licence)


<a name="introduction" />

Introduction
------------

At [SoundCloud](http://soundcloud.com) we run a number of disparate RabbitMQ instances which are all loosely related and interconnected via federation links or shovel configurations.

While the default RabbitMQ Management UI works great for cluster overviews, it's a chore in an unclustered topology to check many instances of the Management UI for node health and thoroughput.

Vanguard is an attempt at keeping the UI useful and accessible in the above scenario by serving a copy of the Management UI which proxies requests to seperate backend API instances, aggregates/munges/unions the result, and presents it back to the user.

The following screenshot shows Vanguard configured for two seperate RabbitMQ backends which are not clustered:

![Vanguard](https://raw.github.com/brendanhay/vanguard/master/img/vanguard.png)

Vanguard ships with a copy of the static assets from the `2.8.4` version of the [rabbitmq-management](https://github.com/rabbitmq/rabbitmq-management) plugin - so if you have an older version of RabbitMQ than `2.8.4`, there may be unintended behaviour on some of the tabs, ie. `/#/exchanges` don't work correctly with `2.7.0` backends.

> Vanguard is still under development, with the intention of ironing out a few bugs in any of the readonly actions. Write actions such as publishing messages, deleting bindings, and so on will not be supported.


<a name="run" />

Running
-------

Vanguard is deployed following something along the lines of a [12factor](http://www.12factor.net/) approach - this means all configuration is set through `ENV` variables:

```shell
PORT=8080
BACKENDS=http://guest:guest@localhost:55670,http://guest:guest@localhost:55680
```

`PORT` is the HTTP listener's port, and `BACKENDS` is a comma seperated
string of URIs containing auth, host, and port information for the backend
RabbitMQ HTTP API instances. These will all need to be exported under the user running Vanguard.

> I strongly suggest only using RabbitMQ users which have readonly permissions and the `monitoring` tag set, as the auth information in the `BACKENDS` URIs.

To play around with Vanguard locally you will need two seperate terminals open. In the first terminal:

```shell
make dev
```

Which starts two backend RabbitMQ instances. (You can configure which `AMQP` port range is used by looking in `./dev/run`). Then, in the second terminal:

```shell
make build
foreman start
```

Which will use the `.env` file in the root directory, and start Vanguard connecting to the two previously started RabbitMQ APIs. Point your browser at [localhost:8080](http://localhost:8080) to see it in action.


<a name="test" />

Testing
-------

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

Please look in the `./priv/www` directory for copies of the related [rabbitmq-management](https://github.com/rabbitmq/rabbitmq-management) licenses.
