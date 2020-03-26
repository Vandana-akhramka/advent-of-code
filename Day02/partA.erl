-module(partA).
-export([start/0]).
-include_lib("eunit/include/eunit.hrl").

%% Lists are immutable in Erlang, how do you ensure performance?
%% Lists start with position 1, not 0, making the code a bit confusing to read.

start() ->
  IntCode = read_input("input.txt"),
  UpdatedIntCode = programAlarm1202(IntCode),
  Result = intcodeProgram(UpdatedIntCode),
  FirstPos = lists:nth(1, Result),
  FirstPos.

read_input(File) ->
  {ok, DataBin} = file:read_file(File),
  IntCode = [list_to_integer(StringInt) || StringInt <- string:tokens(binary_to_list(DataBin), ",$\n")],
  IntCode.

programAlarm1202(IntCode) ->
  Pos1 = 12,
  Pos2 = 2,
  UpdatedIntCode = [lists:nth(1, IntCode)] ++ [Pos1] ++ [Pos2] ++ lists:nthtail(3, IntCode),
  UpdatedIntCode.

intcodeProgram(List) -> intcodeProgram(List, 1).
intcodeProgram(List, Pos) ->
  Opcode = lists:nth(Pos, List),
  case Opcode of
    1 -> executeProgram(fun(A, B) -> A + B end, List, Pos);
    2 -> executeProgram(fun(A, B) -> A * B end, List, Pos);
    99 -> List;
    _ -> error
  end.

executeProgram(F, List, Pos) ->
  A = lists:nth(Pos+1, List),
  B = lists:nth(Pos+2, List),
  C = F(lists:nth(A+1, List), lists:nth(B+1, List)),
  UpdateItem = lists:nth((Pos+3), List),
  UpdatedList = lists:sublist(List, UpdateItem) ++ [C] ++ lists:nthtail(UpdateItem+1, List),
  intcodeProgram(UpdatedList, Pos+4).

given_test() ->
  ?assertEqual([2,0,0,0,99], intcodeProgram([1,0,0,0,99])),
  ?assertEqual([2,3,0,6,99], intcodeProgram([2,3,0,3,99])),
  ?assertEqual([2,4,4,5,99,9801], intcodeProgram([2,4,4,5,99,0])),
  ?assertEqual([30,1,1,4,2,5,6,0,99], intcodeProgram([1,1,1,4,99,5,6,0,99])).