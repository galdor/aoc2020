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

-module(aoc2020_01).

-behaviour(aoc2020_day).

-export([answer/0, answer/1]).

-spec answer() -> {integer(), integer()}.
answer() ->
  answer(aoc2020:input(1)).

-spec answer(binary()) -> {integer(), integer()}.
answer(Input) ->
  Numbers = aoc2020:lines_as_integers(Input),
  {product2(Numbers, 2020), product3(Numbers, 2020)}.

-spec product2([integer()], integer()) -> integer().
product2(Numbers, Sum) ->
  [{N1, N2} | _] = [{N1, N2} ||
                     N1 <- Numbers, N2 <- Numbers,
                     N1 + N2 == Sum],
  N1 * N2.

-spec product3([integer()], integer()) -> integer().
product3(Numbers, Sum) ->
  [{N1, N2, N3} | _] = [{N1, N2, N3} ||
                         N1 <- Numbers, N2 <- Numbers, N3 <- Numbers,
                         N1 + N2 + N3 == Sum],
  N1 * N2 * N3.
