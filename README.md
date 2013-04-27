# Twerl

[![Build Status](https://travis-ci.org/lucaspiller/twerl.png)](https://travis-ci.org/lucaspiller/twerl)

An Erlang client for the Twitter Streaming API.

## Goals

* A lightweight client which can consume the Twitter Streaming API (DONE).
* A client manager which allows you to change the search queries without losing any data (WIP).

## Usage

You probably want to include it as a depedency in Rebar:

    {deps,
      [
        {twerl, ".*", {git, "https://github.com/lucaspiller/twerl.git", "master"}},
      ]
    }.

### Manager

WIP

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

## Development

Start an Erlang shell with reloader support:

    make dev

Compile new code:

    make compile

Run eunit tests:

    make eunit

## Contributing

* Fork the project.
* Make your feature addition or bug fix.
* Add tests. Pull requests without tests will be ignored.
* Send me a pull request. Bonus points for topic branches.
