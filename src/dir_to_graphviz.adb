-- Prints a graphviz version of a directory tree.
with Ada.Command_Line;
with Ada.Text_IO;

with DTG;

procedure Dir_To_Graphviz is
    package ACL renames Ada.Command_Line;
    package AIO renames Ada.Text_IO;

begin
    if ACL.Argument_Count = 0 then
        AIO.Put_Line ("No arguments provided.");
        AIO.Put_Line ("Usage: " & ACL.Command_Name & "DIR...");
        return;
    end if;

    declare
        Result : DTG.Report := DTG.Create ("example.dot");
    begin
        Result.Include_Dot_Files := False;
        for Index in 1 .. ACL.Argument_Count loop
            DTG.Evaluate (Result, ACL.Argument (Index));
        end loop;
    end;
end Dir_To_Graphviz;
