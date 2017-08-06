# Twerl

[![Build Status](https://travis-ci.org/lucaspiller/twerl.png)](https://travis-ci.org/lucaspiller/twerl)

An Erlang client for the Twitter Streaming API.

**Important notice:** As is this library isn't currently usable since [Twitter deprecated basic auth](https://dev.twitter.com/docs/streaming-apis/connecting). OAuth support is WIP, see the [oauth branch](https://github.com/lucaspiller/twerl/tree/oauth) and [issue](https://github.com/lucaspiller/twerl/issues/2) for further details.

## Goals

* A lightweight client which can consume the Twitter Streaming API.
* A client manager which allows you to change the search queries without losing any data (WIP).

## Usage

You probably want to include it as a depedency in Rebar:

    {deps,
      [
        {twerl, ".*", {git, "https://github.com/lucaspiller/twerl.git", "master"}},
      ]
    }.

### Client

Generate auth headers:

    Headers = stream_client_util:generate_auth_headers(TwitterUsername, TwitterPassword).

Generate params:

    {ok, Params} = stream_client_util:keywords_to_track(["erlang"]).

Build a callback function:

    Callback = fun(Data) ->
      Tweet = proplists:get_value(<<"text">>, Data),
      io:format("Erlang <3: ~s~n", [Tweet])
    end.

Start streaming:

    stream_client:connect(stream_client_util:filter_url(), Headers, Params, Callback).

### Manager

The function `stream_client:connect/4` blocks, and only returns once the stream ends. You can use the manager which is a simple gen\_server which wraps the functionality and adds some nice additions, such as allowing you to change filter params and the callback.

Start the manager:

    stream_client_manager:start_link(awesomeness).

Set auth headers:

    Headers = stream_client_util:generate_auth_headers(TwitterUsername, TwitterPassword),
    stream_client_manager:set_headers(awesomeness, Headers).

Set params:

    {ok, Params} = stream_client_util:keywords_to_track(["erlang"]),
    stream_client_manager:set_params(awesomeness, Params).

Start streaming (you can set a callback after, and change it without losing data):

    stream_client_manager:start_stream(awesomeness).

Set a callback:

    stream_client_manager:set_callback(awesomeness, fun(Data) ->
      Tweet = proplists:get_value(<<"text">>, Data),
      io:format("Erlang <3: ~s~n", [Tweet])
    end).

Stop streaming:

    stream_client_manager:stop_stream(awesomeness).

Stop the manager:

    stream_client_manager:stop(awesomeness).

One of the goals is to be able to change the filter params without losing any data. This is still WIP as data could be lost or duplicated.

## Development

Start an Erlang shell with reloader support:

    make dev

Compile new code:

    make compile

Run tests (written in [espec](https://github.com/lucaspiller/espec)):

    make test

## Contributing

* Fork the project.
* Make your feature addition or bug fix.
* Use the same indentation (4 spaces).
* Add a line to `CHANGELOG.md` describing your change.
* Add tests. Pull requests without tests will be ignored.
* Send me a pull request. Bonus points for topic branches.

## License

MIT License. See `LICENSE.md` for details.

## Copyright

Copyright (c) 2011-2013 Luca Spiller.

