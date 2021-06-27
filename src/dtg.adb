with Ada.Characters.Latin_1;
with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;

with Dir_Iterators.Recursive;

package body DTG is
    use Ada.Characters.Latin_1;
    use Ada.Text_IO;
    use Ada.Text_IO.Unbounded_IO;

    --  function Include_Everything (E : AD.Directory_Entry_Type) return Boolean is
    --  begin
    --      pragma Unreferenced (E);
    --      return True;
    --  end Include_Everythingde_Everything;

    function Skip_Dot_Files (E : AD.Directory_Entry_Type) return Boolean is
        Name : constant String := AD.Simple_Name (E);
    begin
        if Name'Length = 1 then
            -- Current directory '.', check the longer name.
            declare
                Parent_Name : constant String := AD.Simple_Name (AD.Containing_Directory (AD.Full_Name (E)));
            begin
                if Parent_Name'Length > 1 and then Parent_Name (1) = '.' then
                    return False;
                end if;
            end;
        elsif Name (1) = '.' then
            return False;
        end if;

        return True;
    end Skip_Dot_Files;

    function Should_Include (R : Report; E : AD.Directory_Entry_Type) return Boolean is
    begin
        return R.Include_Dot_Files or else Skip_Dot_Files (E);
    end Should_Include;

    procedure Add (R : in Report; E : in AD.Directory_Entry_Type) is
        package AD renames Ada.Directories;
        use type ASU.Unbounded_String;

        Name        : constant ASU.Unbounded_String := Node_Name (AD.Full_Name (E));
        Parent_Name : constant ASU.Unbounded_String := Node_Name (Parent (AD.Full_Name (E)));
    begin
        pragma Unreferenced (R);
        declare
            Source       : ASU.Unbounded_String;
            Destination  : ASU.Unbounded_String;
            Element_Name : ASU.Unbounded_String;
        begin
            if AD.Simple_Name (E) = "." then
                Source       := Node_Name (Parent (Parent (AD.Full_Name (E))));
                Destination  := Parent_Name;
                Element_Name := ASU.To_Unbounded_String (AD.Simple_Name (Parent (AD.Full_Name (E))));
            else
                Source       := Parent_Name;
                Destination  := Name;
                Element_Name := ASU.To_Unbounded_String (AD.Simple_Name (E));
            end if;

            Put_Line (Destination & "[shape=box label=" & Quotation & Element_Name & Quotation & "];");
            Put_Line (Source & " -> " & Destination & ";");
        end;
    end Add;

    function Node_Name (Full_Name : String) return ASU.Unbounded_String is
        Safe_Name : ASU.Unbounded_String;
    begin
        for C of Full_Name loop
            case C is
                when '/' | '\' | '.' | ':' | ' ' | '-' =>
                    ASU.Append (Safe_Name, '_');
                when others =>
                    ASU.Append (Safe_Name, C);
            end case;
        end loop;
        return Safe_Name;
    end Node_Name;

    function Replace (Full_Name : String) return ASU.Unbounded_String is
        Result : ASU.Unbounded_String;
    begin
        for C of Full_Name loop
            case C is
                when '\' =>
                    ASU.Append (Result, '/');
                when others =>
                    ASU.Append (Result, C);
            end case;
        end loop;
        return Result;
    end Replace;

    function Parent (Full_Name : String) return String is
    begin
        return AD.Containing_Directory (Full_Name);
    end Parent;

    procedure Evaluate (Result : in out Report; Dir_Root : String) is
        function Filter (E : AD.Directory_Entry_Type) return Boolean is
        begin
            pragma Unreferenced (E);
            if Result.Include_Dot_Files then
                return False;
            else
                return True;
            end if;
        end Filter;
        pragma Unreferenced(Filter);

        Walk : constant Dir_Iterators.Recursive.Recursive_Dir_Walk :=
            Dir_Iterators.Recursive.Walk (Dir_Root, null);
    begin
        for Dir_Entry of Walk loop
            if AD.Full_Name (Dir_Entry) /= AD.Full_Name (Dir_Root) and DTG.Should_Include (Result, Dir_Entry) then
                DTG.Add (Result, Dir_Entry);
            end if;
        end loop;
    end Evaluate;

end DTG;
