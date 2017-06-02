with Ada.Text_IO;                           use Ada.Text_IO;
with Ada.Command_Line;                      use Ada.Command_Line;
with GNAT.Command_Line;                     use GNAT.Command_Line;
with Ada.Environment_Variables;
with Ada.Directories;                       use Ada.Directories;
with GNAT.OS_Lib;                           use GNAT.OS_Lib;
with Ada.Containers.Indefinite_Hashed_Maps; use Ada.Containers;
with Ada;                                   use Ada;
with Ada.Command_Line.Environment;

procedure Xterm is
   -- Ada_Launch is the default name, but is meant to be renamed
   -- to the executable or app name that this executable launches

   package Env renames Ada.Environment_Variables;
   package Cl renames Ada.Command_Line;
   package Clenv renames Ada.Command_Line.Environment;
   package Files renames Ada.Directories;
   -- Env, CL, and CLenv are just abbreviations for:
   -- Environment_Variables, Command_Line, and Command_Line.Environment

   -- xterm  -bg  black -fg  white  +sb  +sm  -fn  10x20  -sl  4000  -cr  yellow

   Launch_Name : String := Locate_Exec_On_Path ("/usr/bin/xterm").all;

   Xterm_Background                 : String  := "black";
   Xterm_Foreground                 : String  := "white";
   Xterm_Scrollbar                  : Boolean := False;
   Xterm_Sessionmanagementcallbacks : Boolean := False;
   Xterm_Loginshell                 : Boolean := False;
   Xterm_Font                       : String  :=
     "-gnu-unifont-medium-r-normal-sans-16-160-75-75-c-80-iso10646-1";
   Xterm_Lineshistory : String := "4000";
   Xterm_Cursorcolor  : String := "yellow";

   Launch_Num_Of_Arguments : Integer := 13;

   Env_Display : String := Env.Value ("DISPLAY", ":0");

   --Launch_Name      : String := "/bin/" & Simple_Name (Command_Name);
   -- The file name to execute/launch

   Launch_Arguments : GNAT.OS_Lib
     .Argument_List
   (1 .. Argument_Count + Launch_Num_Of_Arguments);
   -- The arguments to give the executable

   Launch_Status : Boolean;
   -- The return status of the command + arguments

begin
--   Env.Clear;
--   Env.Set ("DISPLAY", Env_Display);
   Launch_Arguments (1) := new String'("-bg");
   Launch_Arguments (2) := new String'(Xterm_Background);
   Launch_Arguments (3) := new String'("-fg");
   Launch_Arguments (4) := new String'(Xterm_Foreground);
   if Xterm_Scrollbar then
      Launch_Arguments (5) := new String'("-sb");
   else
      Launch_Arguments (5) := new String'("+sb");
   end if;
   if Xterm_Sessionmanagementcallbacks then
      Launch_Arguments (6) := new String'("-sm");
   else
      Launch_Arguments (6) := new String'("+sm");
   end if;
   if Xterm_Loginshell then
      Launch_Arguments (7) := new String'("-ls");
   else
      Launch_Arguments (7) := new String'("+ls");
   end if;
   Launch_Arguments (8)  := new String'("-fn");
   Launch_Arguments (9)  := new String'(Xterm_Font);
   Launch_Arguments (10) := new String'("-sl");
   Launch_Arguments (11) := new String'(Xterm_Lineshistory);
   Launch_Arguments (12) := new String'("-cr");
   Launch_Arguments (13) := new String'(Xterm_Cursorcolor);

   for N in 1 .. Argument_Count loop
      Launch_Arguments (N + Launch_Num_Of_Arguments) :=
        new String'(Argument (N));
   end loop;
   -- Simply copy/convey all arguments to the new command launch.

   --Put_Line (Launch_Name);
   -- DEBUG ACTION - remove this statement when the performance is sufficient.

   if Files.Exists (Launch_Name) then
      Spawn (Launch_Name, Launch_Arguments, Launch_Status);
      -- Launch the new process with conveyed arguments and capture general
      -- return status as Success or Failure.  A number may be used in the future.
   else
      Launch_Status := False;
   end if;

   if not Launch_Status then
      Set_Exit_Status (Failure);
   else
      Set_Exit_Status (Success);
   end if;
   --Give return status back to calling os/environment.

end Xterm;
