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

-module(aoc2020).

-export([input/1, lines/1, lines_as_integers/1]).

-export_type([day/0]).

-type day() :: pos_integer().

-spec input(day()) -> binary().
input(Day) ->
  Dir = code:priv_dir(aoc2020),
  Filename = io_lib:format("~2..0b.txt", [Day]),
  Path = filename:join([Dir, "input", Filename]),
  {ok, Data} = file:read_file(Path),
  Data.

-spec lines(binary()) -> [binary()].
lines(Data) ->
  binary:split(Data, <<"\n">>, [global, trim_all]).

-spec lines_as_integers(binary()) -> [integer()].
lines_as_integers(Data) ->
  lists:map(fun erlang:binary_to_integer/1, lines(Data)).
