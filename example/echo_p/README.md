# barrel echo example

Like the echo example but instead we spawn a new process to handle the
request and pass the control of the socket to it.

To compile this example :

    $ ../../rebar get-deps compile


You can then start the erlang node with the following command:

    $ ./start.sh

Then telnet localhotst 10001, and start to type, the server should echo
the result.

> Note: type "." to EXIT.
