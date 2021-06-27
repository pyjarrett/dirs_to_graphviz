-- Prints a graphviz version of a directory tree.

with Ada.Characters.Latin_1;
with Ada.Command_Line;
with Ada.Text_IO;

with DTG;

procedure Dir_To_Graphviz is
    package ACL renames Ada.Command_Line;
    package AIO renames Ada.Text_IO;
    use Ada.Characters;

    Result : DTG.Report;

begin
    if ACL.Argument_Count = 0 then
        AIO.Put_Line ("No arguments provided.");
        return;
    end if;

    Result.Include_Dot_Files := True;
    AIO.Put_Line ("digraph directories {");
    AIO.Put_Line ("rankdir=" & Latin_1.Quotation & "LR" & Latin_1.Quotation & ";");
    for Index in 1 .. ACL.Argument_Count loop
        DTG.Evaluate(Result, ACL.Argument(Index));
    end loop;
    AIO.Put_Line ("}");
end Dir_To_Graphviz;
