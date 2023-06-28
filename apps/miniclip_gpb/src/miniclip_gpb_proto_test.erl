%%%-------------------------------------------------------------------
%%% @author hasitha
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Jun 2023 4:18 PM
%%%-------------------------------------------------------------------
-module(miniclip_gpb_proto_test).
-author("hasitha").

%% API
-export([
  create_empty_person/0,
  create_dummy_data/0]).

% This imports the record for you to use from the generated file
-include("kv_pb.hrl").


create_empty_person() ->
  #data{}.

create_dummy_data() ->
  #data{key = "test", value = "test_data"}.