-module(partB).
-export([start/0]).

start() ->
  IntCode = read_input("input.txt"),
  Result = recurse_try(IntCode),
  Result.

read_input(File) ->
  {ok, DataBin} = file:read_file(File),
  IntCode = [list_to_integer(StringInt) || StringInt <- string:tokens(binary_to_list(DataBin), ",$\n")],
  IntCode.

recurse_try(IntCode) ->
  recurse_try(IntCode, 0, 0).
recurse_try(_IntCode, First, Second) when First > 99; Second > 99 -> error;
recurse_try(IntCode, First, Second) ->
  UpdatedIntCode = updateInput(IntCode, First, Second),
  Result = intcodeProgram(UpdatedIntCode),
  FirstPos = lists:nth(1, Result),
  case FirstPos of
    19690720 -> 100*(lists:nth(2, Result)) + (lists:nth(3, Result));
    _ -> case Second of
           99 -> recurse_try(IntCode, First+1, 0);
           _ -> recurse_try(IntCode, First, Second+1)
         end
  end.

updateInput(IntCode, Pos1, Pos2) ->
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