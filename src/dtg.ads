with Ada.Directories;
with Ada.Strings.Unbounded;

package DTG is
    package AD renames Ada.Directories;
    package ASU renames Ada.Strings.Unbounded;

    type Report is record
        Include_Dot_Files : Boolean := False;
    end record;

    function Should_Include (R : in Report; E : in AD.Directory_Entry_Type) return Boolean;
    procedure Add (R : in Report; E : in AD.Directory_Entry_Type);

    function Replace (Full_Name : String) return ASU.Unbounded_String;
    function Node_Name (Full_Name : String) return ASU.Unbounded_String;
    function Parent (Full_Name : String) return String;

    procedure Evaluate (Result : in out Report; Dir_Root : String);
end DTG;
