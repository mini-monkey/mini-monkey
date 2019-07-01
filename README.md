Mini Monkey (pre-alpha)
=======================

[![License Apache 2](https://img.shields.io/badge/License-Apache2-blue.svg)](https://www.apache.org/licenses/LICENSE-2.0)
[![Build Status](https://travis-ci.org/Raphexion/minimonkey.svg?branch=master)](https://travis-ci.org/Raphexion/minimonkey)
[![codecov.io](https://codecov.io/gh/Raphexion/minimonkey/coverage.svg?branch=master)](https://codecov.io/gh/Raphexion/minimonkey?branch=master)
[![Docker build](https://img.shields.io/docker/cloud/build/raphexion/minimonkey.svg?color=green)](https://hub.docker.com/r/raphexion/minimonkey)

![Logo](doc/minimonkey_small.png)

Mini Monkey is a minimal message routing system.
Considerably smaller and simpler than [MQTT](https://en.wikipedia.org/wiki/MQTT).
Like other message brokers (RabbitMQ, EMQX, VerneMQ), Mini Monkey is implemented in Erlang.

Quick Start
-----------

```sh
docker run -d -p 1773:1773 -e god_token=myToken raphexion/minimonkey
```

Clients
-------

- [Client in Acceptence Tester](https://github.com/Raphexion/minimonkey-tests/blob/master/libraries/MiniMonkey.py)

Project Plan
------------

- [ ] Streamline debug/error messages
- [ ] Add benchmark tests
- [ ] Add more tests concerning permissions
- [ ] Add more examples in README
- [ ] Death Message

Three perspectives
------------------

When designing Mini Monkey we need to focus on our three main uses-cases:

1. A embedded device produces data and is remotely controllable.
2. A controller is cosuming data from one or more producers and send control messages.
3. An administrator configures (programmaticially) who can publish/subscribe.

![Three perspectives](doc/three_perspectives.png)

Design decisions
----------------

Mini Monkey is a publish / subscribe broker than only support routing keys.
Especially it does not implement topics.

Mini Monkey only cares about routing blobs.
Especially it does not use JSON / Protocol-buffers or other serialization.

Mini Monkey is designed around small payloads.

Mini Monkey is desgined to be very simple and concise.
The goal is to reach feature-completion fast and leave as much as possible outside the broker.
The protocol and architecture should be so easy that anyone can implement a client in under and hour.

Mini Monkey uses stateful connections where previous _commands_ affect future commands.
The reasons is to keep all payloads small.

Bootstrap and the God Token
---------------------------

Then Mini Monkey starts it will read the environment variable **god_token**.
The purpose is to have one token that can bootstrap the broker.
This token can be used to provision the broker and the routing keys (rooms).

| Variable  | Comment                                 |
|-----------|-----------------------------------------|
| god_token | A token that has all rights             |


Port
----

Mini Monkey only uses one port **1773**.
Connections are made over TCP/IP.

| Port | Comment                                 |
|------|-----------------------------------------|
| 1773 | Controllers, Devices and Administrators |

Death Message
-------------

Sometimes it is good to communicate when a user disappears.
It is therefore (not implemented yet) to set a message, that
will be delivered if the user disconnects unexpected.

Protocal
--------

All messages, both to and from the server follow a trivial binary protocol.

| Size    | Comment                          |
|---------|----------------------------------|
| 1 byte  | Function Code                    |
| 2 bytes | Payload size (little endian)     |
| N bytes | Optional payload                 |

Function Codes
--------------

| Code | Comment                                                         | Payload meaning                      |
|------|-----------------------------------------------------------------|--------------------------------------|
| 0x00 | Reseved for future use                                          |                                      |
| 0x01 | Authenticate with token                                         | Token                                |
| 0x02 | Enter room (persistent until changed or reconnect)              | Room                                 |
| 0x03 | Publish binary payload                                          | Published data                       |
| 0x04 | Subscribe to current routing key                                | Tag                                  |
| 0x10 | Add admin permissions for token                                 | Additional admin token in room       |
| 0x11 | Revoke admin permissions for token                              | Token to be revoked                  |
| 0x12 | Add publish permissions for token                               | Additional publish token in room     |
| 0x13 | Revoke publish permissions for token                            | Token to be revoked                  |
| 0x14 | Add subscription permissions for token                          | Additional subs. token in room       |
| 0x15 | Revoke subscription permissions for token                       | Token to be revoked                  |
| 0x16 | Add login                                                       | Additional token that can login      |
| 0x17 | Revoke login                                                    | Revoke a token for login             |
| 0x20 | Set death message payload                                       | Data that is sent when disconnect    |
| 0x21 | Set death message room                                          | Room where the messages is published |
| 0x22 | Enable death message                                            |                                      |
| 0x23 | Disable death message                                           |                                      |
| 0x30 | Forward messages in this room to another room                   | Destiantion room for forward         |
| 0x31 | Unforward message in this room to anther room                   | Destionation room for un-forward     |
| 0xEE | Error message                                                   | Message                              |
| 0xFF | Debug message                                                   | Message                              |

Examples
--------

A client logins in and publish 3 messages:

| Purpose          | Bytes (little endian) | Optional Payload         | Comment        |
|------------------|-----------------------|--------------------------|----------------|
| Auth with token  | 0x01 0x04 0x00        | 0x41 0x42 0x43 0x44      | Token: ABCD    |
| Pick routing key | 0x02 0x03 0x00        | 0x51 0x52 0x53           | Key: QRS       |
| Publish          | 0x03 0x04 0x00        | 0x01 0x02 0x03 0x04 0x05 | Binary payload |
| Publish          | 0x03 0x02 0x00        | 0xFF 0xEE                | Binary payload |
| Pick routing key | 0x02 0x04 0x00        | 0x51 0x52 0x53 0x032     | Key: QRS2      |
| Publish          | 0x03 0x01 0x00        | 0xAB                     | Binary payload |
