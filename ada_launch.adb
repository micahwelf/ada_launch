with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Gnat.Command_Line; use Gnat.Command_Line;
with Ada.Environment_Variables;
with Ada.Directories; use Ada.Directories;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Ada.Containers.Indefinite_Hashed_Maps; use Ada.Containers;
with Ada; use Ada;
with Ada.Command_Line.Environment;
with Ada.Strings;

procedure Ada_Launch is
   -- Ada_Launch is the default name, but is meant to be renamed
   -- to the executable name that this executable launches

   package Env renames Ada.Environment_Variables;
   package CL renames Ada.Command_Line;
   package CLenv renames Ada.Command_Line.Environment;
   -- Env, CL, and CLenv are just abbreviations for:
   -- Environment_Variables, Command_Line, and Command_Line.Environment

   Launch_Name : String :=
     Locate_Exec_On_Path (
                          Simple_Name (
                           Command_Name)).all;
   -- Application to run: Current filename without directory, as found in PATH

   Pre_List : GNAT.OS_Lib.Argument_List
     := (
         1 => new String'("--")
        );
   Post_List : GNAT.OS_Lib.Argument_List
     := (
         1 => new String'("")
        );
   -- Command line options to apply at the beginning and at the end of
   -- the options given at execution.  * !!! Must comment out/remove
   -- both here and following any empty/unused lists.

   Condition_Option : String := "--new-window";
   --Launch_Name      : String := "/bin/" & Simple_Name (Command_Name);
   -- The file name to execute/launch

   Launch_Arguments :
   GNAT.OS_Lib.Argument_List (
                              1 .. (Argument_Count
                                + Pre_List'Length
                                + Post_List'Length
                               ));
   -- The arguments to give the executable

   Launch_Status : Boolean;
   -- The return status of the command + arguments

begin
   for N in 1 .. Pre_List'Length loop
      Launch_Arguments (N)
        := Pre_List (N);
   end loop;
   for N in 1 .. Post_List'Length loop
      Launch_Arguments ((N + Pre_List'Length + Argument_Count))
        := Post_List(N);
   end loop;
   for N in 1 .. Argument_Count loop
      Launch_Arguments ((N + Pre_List'Length))
        := new String'(Argument (N));
   end loop;
   -- Simply copy/convey all arguments to the new command launch.

   Spawn (Launch_Name, Launch_Arguments, Launch_Status);
   -- Launch the new process with conveyed arguments and capture general
   -- return status as Success or Failure.  A number may be used in the future.

   if not Launch_Status
   then
      Set_Exit_Status (Failure);
   else
      Set_Exit_Status (Success);
   end if;
   --Give return status back to calling os/environment.

   for N in 1 .. Pre_List'Length loop
      Free (Pre_List (N));
   end loop;
   -- Clean up

end Ada_Launch;


