%%%-------------------------------------------------------------------
%% @doc peering public API
%% @end
%%%-------------------------------------------------------------------

-module(peering_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    peering_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
