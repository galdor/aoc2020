%% Copyright (c) 2020 Nicolas Martyanoff <khaelin@gmail.com>.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
%% REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
%% AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
%% INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
%% LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
%% OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
%% PERFORMANCE OF THIS SOFTWARE.

-module(aoc2020_02).

-behaviour(aoc2020_day).

-export([answer/0, answer/1,
         is_entry_valid_1/1, count/2]).

-type entry() :: {Letter :: byte(), pos_integer(), pos_integer(),
                  Password :: string()}.

-spec answer() -> {integer(), integer()}.
answer() ->
  answer(aoc2020:input(2)).

-spec answer(binary()) -> {integer(), integer()}.
answer(Input) ->
  Lines = aoc2020:lines(Input),
  Entries = lists:map(fun parse_entry/1, Lines),
  Count1 = count(fun is_entry_valid_1/1, Entries),
  Count2 = count(fun is_entry_valid_2/1, Entries),
  {Count1, Count2}.

-spec parse_entry(binary()) -> entry().
parse_entry(String) ->
  RE = "([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)",
  Options = [{capture, all_but_first, list}, anchored],
  case re:run(String, RE, Options) of
    {match, [MinString, MaxString, [Letter], Password]} ->
      Min = list_to_integer(MinString),
      Max = list_to_integer(MaxString),
      {Letter, Min, Max, Password};
    nomatch ->
      error({invalid_entry_format, String})
  end.

-spec is_entry_valid_1(entry()) -> boolean().
is_entry_valid_1({Letter, Min, Max, Password}) ->
  Pred = fun (L) -> L =:= Letter end,
  Count = count(Pred, Password),
  (Count >= Min) and (Count =< Max).

-spec is_entry_valid_2(entry()) -> boolean().
is_entry_valid_2({Letter, Pos1, Pos2, Password}) ->
  Letter1 = lists:nth(Pos1, Password),
  Letter2 = lists:nth(Pos2, Password),
  (Letter1 =:= Letter) xor (Letter2 =:= Letter).

-spec count(fun((term()) -> boolean()), list()) -> non_neg_integer().
count(Pred, List) ->
  count(Pred, List, 0).

-spec count(fun((term()) -> boolean()), list(), non_neg_integer()) ->
        non_neg_integer().
count(_, [], Count) ->
  Count;
count(Pred, [Element | Rest], Count) ->
  case Pred(Element) of
    true ->
      count(Pred, Rest, Count+1);
    false ->
      count(Pred, Rest, Count)
  end.
