with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Command_Line;  use Ada.Command_Line;
with GNAT.Command_Line; use GNAT.Command_Line;
with Ada.Environment_Variables;
with Ada.Directories;   use Ada.Directories;
with GNAT.OS_Lib;       use GNAT.OS_Lib;
with Ada;               use Ada;
with Ada.Command_Line.Environment;

procedure Startxterminal is
   -- Ada_Launch is the default name, but is meant to be renamed to the
   -- executable or app name that this executable launches

   package Env renames Ada.Environment_Variables;
   package Cl renames Ada.Command_Line;
   package Clenv renames Ada.Command_Line.Environment;
   package Files renames Ada.Directories;
--   package OS renames gnat.os_lib;
   -- Env, CL, and CLenv are just abbreviations for: Environment_Variables,
   -- Command_Line, and Command_Line.Environment

   -- xterm -bg black -fg white +sb +sm -fn 10x20 -sl 4000 -cr yellow

   Launch_Name : String := Locate_Exec_On_Path ("/usr/bin/xterm").all;

   Xterm_Background                   : String  := "black";
   Xterm_Foreground                   : String  := "white";
   Xterm_Scrollbar                    : Boolean := False;
   Xterm_Session_Management_Callbacks : Boolean := False;
   Xterm_Loginshell                   : Boolean := False;
--   Xterm_Font                         : String  := "adobe-source code pro*";
--   Xterm_Font                         : String  := "gnu-unifont*";
   Xterm_Font         : String := "gnu-unifont*";
   Xterm_Font_Size    : String := "9";
   Xterm_Lineshistory : String := "4000";
   Xterm_Cursorcolor  : String := "yellow";
   Xterm_Geometry     : String := "200x50+-128+-128";
--   Xterm_Geometry                     : String  := "260x70+-128+-128";
   Xterm_Border : String := "256";

   Launch_Num_Of_Arguments : Integer := 15;

   Env_Display : String := Env.Value ("DISPLAY", ":0");

   --Launch_Name      : String := "/bin/" & Simple_Name (Command_Name);
   -- The file name to execute/launch

--   Launch_Arguments : GNAT.OS_Lib.Argument_List (1 .. Launch_Num_Of_Arguments);
   Launch_Arguments : GNAT.OS_Lib.Argument_List :=
     (new String'("-bg"),
      new String'(Xterm_Background),
      new String'("-fg"),
      new String'(Xterm_Foreground),
      (if Xterm_Scrollbar then new String'("-sb") else new String'("+sb")),
      (if Xterm_Session_Management_Callbacks then new String'("-sm")
       else new String'("+sm")),
      (if Xterm_Loginshell then new String'("-ls") else new String'("+ls")),
      new String'("-fs"),
      new String'(Xterm_Font_Size),
      new String'("-fn"),
      new String'
        ("-gnu-unifont-medium-r-normal-sans-16-160-75-75-c-80-iso10646-1"),
--      new String'("-fa"),
--      new String'(Xterm_Font),
      new String'("-sl"),
      new String'(Xterm_Lineshistory),
      new String'("-cr"),
      new String'(Xterm_Cursorcolor),
      new String'("-geometry"),
      new String'(Xterm_Geometry),
      new String'("-b"),
      new String'(Xterm_Border),
      new String'("-xrm"),
      new String'("*overrideRedirect: True"));

--   (1 ..
--        (if Argument_Count = 0 then Launch_Num_Of_Arguments
--         else Launch_Num_Of_Arguments + 2));
   -- The arguments to give the executable

   Launch_Status : Boolean;
   -- The return status of the command + arguments

   function Command_Arguments return GNAT.OS_Lib.Argument_List is
      Empty_Words   : GNAT.OS_Lib.Argument_List (1 .. 0) := (others => null);
      Command_Words : GNAT.OS_Lib.Argument_List (1 .. 2) := (others => null);
      function More (X : Integer := 0; S : String := "") return String is
         Arg_Ind : Integer := X + 1;
         Sp      : String  := (if S'Length = 0 then "" else " ");
      begin
         null;
         if Argument_Count > Arg_Ind
         then
            return S & More (Arg_Ind, S & Sp & Argument (Arg_Ind));
         else
            return S & Sp & Argument (Arg_Ind);
         end if;
      end More;
   begin
      null;
      if Argument_Count > 0
      then
         Command_Words (1) := new String'("-e");
         Command_Words (2) := new String'(More);
         return Command_Words;
      else
         return Empty_Words;
      end if;
   end Command_Arguments;

   procedure Launch
     (Command   : String;
      Arguments : GNAT.OS_Lib.Argument_List)
   is
      Launch_Arguments : GNAT.OS_Lib.Argument_List := Arguments;
   begin
      Spawn (Command, Launch_Arguments, Launch_Status);
      for I in Launch_Arguments'Range
      loop
         Free (Launch_Arguments (I));
      end loop;
   end Launch;

begin
   Env.Set ("DISPLAY", Env_Display);
--   Launch_Arguments (1) := new String'("-bg");
--   Launch_Arguments (2) := new String'(Xterm_Background);
--   Launch_Arguments (3) := new String'("-fg");
--   Launch_Arguments (4) := new String'(Xterm_Foreground);
--   if Xterm_Scrollbar
--   then
--      Launch_Arguments (5) := new String'("-sb");
--   else
--      Launch_Arguments (5) := new String'("+sb");
--   end if;
--   if Xterm_Sessionmanagementcallbacks
--   then
--      Launch_Arguments (6) := new String'("-sm");
--   else
--      Launch_Arguments (6) := new String'("+sm");
--   end if;
--   if Xterm_Loginshell
--   then
--      Launch_Arguments (7) := new String'("-ls");
--   else
--      Launch_Arguments (7) := new String'("+ls");
--   end if;
--   Launch_Arguments (8)  := new String'("-fn");
--   Launch_Arguments (9)  := new String'(Xterm_Font);
--   Launch_Arguments (10) := new String'("-sl");
--   Launch_Arguments (11) := new String'(Xterm_Lineshistory);
--   Launch_Arguments (12) := new String'("-cr");
--   Launch_Arguments (13) := new String'(Xterm_Cursorcolor);
--   Launch_Arguments (14) := new String'("-geometry");
--   Launch_Arguments (15) := new String'(Xterm_Geometry);
--
--   for N in 1 .. Argument_Count
--   loop
--      Launch_Arguments (N + Launch_Num_Of_Arguments) :=
--        new String'(Argument (N));
--   end loop;
   -- Simply copy/convey all arguments to the new command launch.

   --Put_Line (Launch_Name);
   -- DEBUG ACTION - remove this statement when the performance is sufficient.

   if Files.Exists (Launch_Name)
   then
      Launch (Launch_Name, Launch_Arguments & Command_Arguments);
   -- Launch the new process with conveyed arguments and capture general return
   -- status as Success or Failure. A number may be used in the future.
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

end Startxterminal;
