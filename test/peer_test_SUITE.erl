-module(peer_test_SUITE).
-behaviour(ct_suite).
-export([all/0]).
-export([t1/1, t2/1, t3/1, t4/1, t5/1, t6/1,
         init_per_testcase/2,
         end_per_testcase/2]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [
     t1, t2, t4, t5, t6
    ].

init_per_testcase(_TestCase, Config) ->
    net_kernel:stop(),
    false = is_alive(),
    Config.

end_per_testcase(_TestCase, Config) ->
    net_kernel:stop(),
    false = is_alive(),
    Config.

t1(Config) when is_list(Config) ->
     try   peer:start_link()
    catch error:not_alive -> ok
    end.

t2(Config) when is_list(Config) ->
    {ok, _} = net_kernel:start(noob, #{name_domain => shortnames}),
    {ok, _Pid, _Node} = peer:start_link().

t3(Config) when is_list(Config) ->
    {ok, _} = net_kernel:start('noob@127.0.0.1', #{name_domain => longnames}),
    try   peer:start_link()
    catch exit:timeout -> ok
    end.

t4(Config) when is_list(Config) ->
    {ok, _} = net_kernel:start('noob@127.0.0.1', #{name_domain => longnames}),
    try   peer:start_link(#{host => "127.0.0.1"})
    catch error:not_alive -> ok
    end.

t5(Config) when is_list(Config) ->
    {ok, _} = net_kernel:start('noob@127.0.0.1', #{name_domain => longnames}),
    {ok, _Pid, _Node} = peer:start_link(#{name => othr, host => "127.0.0.1"}).

t6(Config) when is_list(Config) ->
    Ssh = os:find_executable("ssh"),
    Erl = os:find_executable("erl"),
    {ok, _Pid, _Node} = peer:start_link(#{
                                          exec => {Ssh, ["localhost", Erl]}}).


%% io:format(".~p.~p.~n", [A,B])
%%    catch A:B -> io:format(".~p.~p.~n", [A,B])
