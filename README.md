miniclip_gpb
=====

An OTP application

Build
-----

    $ rebar3 compile

For testing
-----------

    $ rebar3 test

Update the sys.config parameter using relevant values.
Then use following method to test the functionalities.

To store data,

    $ miniclip_gpb_client_test:set_data(Key, Value).

To retrieve data,

    $ miniclip_gpb_client_test:get_data(Key).


