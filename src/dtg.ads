with Ada.Directories;
with Ada.Finalization;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

package DTG is
    package AD renames Ada.Directories;
    package ASU renames Ada.Strings.Unbounded;
    package AIO renames Ada.Text_IO;

    type Report is new Ada.Finalization.Limited_Controlled with record
        Output_File : AIO.File_Type;
        Include_Dot_Files : Boolean := False;
    end record;

    function Create(File_Name : String) return Report;

    overriding
    procedure Finalize(Self : in out Report);

    function Should_Include (R : in Report; E : in AD.Directory_Entry_Type) return Boolean;
    procedure Add (R : in Report; E : in AD.Directory_Entry_Type);

    function Replace (Full_Name : String) return ASU.Unbounded_String;
    function Node_Name (Full_Name : String) return ASU.Unbounded_String;
    function Parent (Full_Name : String) return String;

    procedure Evaluate (Result : in out Report; Dir_Root : String);
end DTG;
