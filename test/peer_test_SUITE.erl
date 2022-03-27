-module(peer_test_SUITE).
-behaviour(ct_suite).
-export([all/0]).
-export([
         origin_is_not_alive/1, 
         origin_is_short/1,
         origin_is_long/1,
         origin_is_long_name/1,
         origin_is_long_name_host/1,
         long_and_host/1, 
         long_full/1, 
         default_exec_ssh/1,
         origin_short_default_exec_ssh/1,
         origin_short_default_exec_ssh_name/1,

         init_per_testcase/2,
         end_per_testcase/2]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [
     origin_is_not_alive, 
     origin_is_short,
     %%origin_is_long,
     origin_is_long_name,
     origin_is_long_name_host,
     long_and_host,
     long_full, 
     default_exec_ssh,
     origin_short_default_exec_ssh,
     origin_short_default_exec_ssh_name
    ].

init_per_testcase(_TestCase, Config) ->
    net_kernel:stop(),
    false = is_alive(),
    Config.

end_per_testcase(_TestCase, Config) ->
    net_kernel:stop(),
    false = is_alive(),
    Config.

origin_is_not_alive(Config) when is_list(Config) ->
    peer:start_link().
    %% error:not_alive

origin_is_short(Config) when is_list(Config) ->
    {ok, _} = net_kernel:start(noob, #{name_domain => shortnames}),
    {ok, _Pid, _Node} = peer:start_link().
    %% ok

origin_is_long(Config) when is_list(Config) ->
    {ok, _} = net_kernel:start('noob@127.0.0.1', #{name_domain => longnames}),
    peer:start_link().
    %% exit:timeout

origin_is_long_name(Config) when is_list(Config) ->
    {ok, _} = net_kernel:start('noob@127.0.0.1', #{name_domain => longnames}),
    peer:start_link(#{name => sasasa}).
    %% exit:timeout

origin_is_long_name_host(Config) when is_list(Config) ->
    {ok, _} = net_kernel:start('noob@127.0.0.1', #{name_domain => longnames}),
    peer:start_link(#{name => sasasa, host => "127.0.0.1"}).
    %% exit:timeout

long_and_host(Config) when is_list(Config) ->
    {ok, _} = net_kernel:start('noob@127.0.0.1', #{name_domain => longnames}),
    peer:start_link(#{host => "127.0.0.1"}).
    %% error:not_alive


long_full(Config) when is_list(Config) ->
    {ok, _} = net_kernel:start('noob@127.0.0.1', #{name_domain => longnames}),
    {ok, _Pid, _Node} = peer:start_link(#{name => othr, host => "127.0.0.1"}).
    %% ok

default_exec_ssh(Config) when is_list(Config) ->
    Ssh = os:find_executable("ssh"),
    Erl = os:find_executable("erl"),
    {ok, _Pid, _Node} = peer:start_link(#{exec => {Ssh, ["localhost", Erl]}}).

origin_short_default_exec_ssh(Config) when is_list(Config) ->
    {ok, _} = net_kernel:start(noob, #{name_domain => shortnames}),
    true = is_alive(),
    Ssh = os:find_executable("ssh"),
    Erl = os:find_executable("erl"),
    {ok, _Pid, _Node} = peer:start_link(#{exec => {Ssh, ["localhost", Erl]}}).
    %% from peer source:
    %% alternative connection must be requested for non-distributed node,
    %%  or a distributed node when origin is not alive
    %is_map_key(connection, Options) orelse
    %                                  (is_map_key(name, Options) andalso erlang:is_alive()) orelse error(not_alive),

    %% error:not_alive

origin_short_default_exec_ssh_name(Config) when is_list(Config) ->
    {ok, _} = net_kernel:start(noob, #{name_domain => shortnames}),
    true = is_alive(),
    Ssh = os:find_executable("ssh"),
    Erl = os:find_executable("erl"),
    {ok, _Pid, _Node} = peer:start_link(#{name => nanana, exec => {Ssh, ["localhost", Erl]}}).



%% io:format(".~p.~p.~n", [A,B])
%%    catch A:B -> io:format(".~p.~p.~n", [A,B])
