-module(day04).
-export([
  startA/0,
  startB/0
]).
-include_lib("eunit/include/eunit.hrl").

read_input(Filename) ->
  {ok, DataBin} = file:read_file(Filename),
  Range = [ list_to_integer(X) || X <- string:tokens(binary_to_list(DataBin), "-$\n")],
  Range.

increasing([_]) -> true;
increasing([F, S | _]) when F > S -> false;
increasing([_, S | Rest]) -> increasing([S | Rest]).

startA() ->
  [L, H] = read_input(atom_to_list(?MODULE) ++ ".txt"),
  RangePasswords = lists:seq(L, H),
  PossiblePasswords = [Password || Password <- RangePasswords, validPasswordA(Password)],
  length(PossiblePasswords).

validPasswordA(Password) ->
  PasswordString = integer_to_list(Password),
  increasing(PasswordString) andalso constant(PasswordString).

constant([_]) -> false;
constant([F, S | _]) when F == S -> true;
constant([_, S | Rest]) -> constant([S | Rest]).

startB() ->
  [L, H] = read_input(atom_to_list(?MODULE) ++ ".txt"),
  RangePasswords = lists:seq(L, H),
  PossiblePasswords = [Password || Password <- RangePasswords, validPasswordB(Password)],
  length(PossiblePasswords).

validPasswordB(Password) ->
  PasswordString = integer_to_list(Password),
  increasing(PasswordString) andalso constantB(PasswordString).

constantB([]) -> false;
constantB([_]) -> false;
constantB([F, S | Rest]) -> case count(F, [S | Rest]) of
                              0 -> constantB([S | Rest]);
                              1 -> true;
                              Count -> constantB(lists:sublist(Rest, Count, length(Rest)))
                            end .

count(_, []) -> 0;
count(X, [X|Xs]) -> 1 + count(X, Xs);
count(_, _) -> 0.

givenA_test() ->
  ?assertEqual(true, validPasswordA(122345)),
  ?assertEqual(true, validPasswordA(111123)),
  ?assertEqual(false, validPasswordA(135679)),
  ?assertEqual(true, validPasswordA(111111)),
  ?assertEqual(false, validPasswordA(223450)),
  ?assertEqual(false, validPasswordA(123789)).

givenB_test() ->
  ?assertEqual(true, validPasswordB(112233)),
  ?assertEqual(false, validPasswordB(123444)),
  ?assertEqual(true, validPasswordB(111122)).
