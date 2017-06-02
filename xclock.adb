with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Gnat.Command_Line; use Gnat.Command_Line;
with Ada.Environment_Variables;
with Ada.Directories; use Ada.Directories;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada; use Ada;
with Ada.Command_Line.Environment;

procedure Xclock is
   -- Ada_Launch is the default name, but is meant to be renamed
   -- to the executable or app name that this executable launches

   package Env renames Ada.Environment_Variables;
   package CL renames Ada.Command_Line;
   package CLenv renames Ada.Command_Line.Environment;
   package Files renames Ada.Directories;
   -- Env, CL, and CLenv are just abbreviations for:
   -- Environment_Variables, Command_Line, and Command_Line.Environment

   -- xterm  -bg  black -fg  white  +sb  +sm  -fn  10x20  -sl  4000  -cr  yellow

   Launch_Name      : String := Locate_Exec_On_Path ("/usr/bin/xclock").all;
   package Associative_Array is
     new Ada.Containers.Indefinite_Ordered_Maps (Key_Type     => String,
                                                 Element_Type => String);
   Xrms : Associative_Array.Map;

   --        Xterm_ScrollBar           : Boolean := False,
   --        Xterm_SessionManagementCallbacks  : Boolean := False;
   --        Xterm_LoginShell                  : Boolean := False;
   --        Xterm_Font                        : String := "fixed";
   --        Xterm_LinesHistory                : String := "4000";
   --        Xterm_CursorColor                 : String := "yellow";
   Launch_Num_Of_Arguments : Integer := 13;

   Env_Display : String := Env.Value ("DISPLAY", ":0");

   --Launch_Name      : String := "/bin/" & Simple_Name (Command_Name);
   -- The file name to execute/launch

   Launch_Arguments :
   GNAT.OS_Lib.Argument_List (1 .. Argument_Count + Launch_Num_Of_Arguments);
   -- The arguments to give the executable

   Launch_Status    : Boolean;
   -- The return status of the command + arguments

begin
   Xrms ("Background") := "darkgreen";
   Env.Clear;
   Env.Set ("DISPLAY", Env_Display);
   Launch_Arguments (1) := new String'("-bg");
   Launch_Arguments (2) := new String'(Xterm_Background);
   Launch_Arguments (3) := new String'("-fg");
   Launch_Arguments (4) := new String'(Xterm_Foreground);
   if Xterm_ScrollBar
   then
      Launch_Arguments (5) := new String'("-sb");
   else
      Launch_Arguments (5) := new String'("+sb");
   end if;
   if Xterm_SessionManagementCallbacks
   then
      Launch_Arguments (6) := new String'("-sm");
   else
      Launch_Arguments (6) := new String'("+sm");
   end if;
      if Xterm_LoginShell
   then
      Launch_Arguments (7) := new String'("-ls");
   else
      Launch_Arguments (7) := new String'("+ls");
   end if;
   Launch_Arguments (8) := new String'("-fn");
   Launch_Arguments (9) := new String'(Xterm_Font);
   Launch_Arguments (10) := new String'("-sl");
   Launch_Arguments (11) := new String'(Xterm_LinesHistory);
   Launch_Arguments (12) := new String'("-cr");
   Launch_Arguments (13) := new String'(Xterm_CursorColor);

   for N in 1 .. Argument_Count loop
      Launch_Arguments (N + Launch_Num_Of_Arguments) := new String'(Argument (N));
   end loop;
   -- Simply copy/convey all arguments to the new command launch.

   --Put_Line (Launch_Name);
   -- DEBUG ACTION - remove this statement when the performance is sufficient.

   if Files.Exists (Launch_Name)
   then
      Spawn (Launch_Name,
             (
              new String'("-digital"),
              new String'("-xrm"),
              new String'("*fontColor: lightgreen"),
              new String'("-xrm"),
              new String'("*foreground: lightgreen"),
              new String'("-xrm"),
              new String'("*XClock.twentyfour: True")
             ) & Launch_Arguments, Launch_Status);
      -- Launch the new process with conveyed arguments and capture general
      -- return status as Success or Failure.  A number may be used in the future.
   else
      Launch_Status := False;
   end if;

   if not Launch_Status
   then
      Set_Exit_Status (Failure);
   else
      Set_Exit_Status (Success);
   end if;
   --Give return status back to calling os/environment.

end Xclock;


