-module(benchmark_test_SUITE).
-include_lib("stdlib/include/assert.hrl").
-include_lib("eunit/include/eunit.hrl").

benchmark_test() ->
    block_mat:first_try_benchmark(),
    erlang:display(block_mat:get_max_length()).