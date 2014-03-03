alcove
------

Sandbox for Erlang ports.

Tests
=====

The tests currently rely on having a statically linked version of
busybox. On Ubuntu:

    apt-get install busybox-static

A statically linked executable is required because the tests do a
chroot(2) to /bin before exec'ing the binary.
