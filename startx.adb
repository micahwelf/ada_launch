with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Environment_Variables;
with Ada.Directories;
with GNAT.OS_Lib;
with GNAT.Directory_Operations;
with GNAT.Regpat;
with Ada;              use Ada;

procedure Startx is
   -- XTerminal is a terminal bast in Xorg -- meant to be used as a login
   -- terminal.

   use GNAT.OS_Lib;

   package Env renames Ada.Environment_Variables;
   package Cl renames Ada.Command_Line;
   package Files renames Ada.Directories;
   package Os renames GNAT.OS_Lib;
   package Os_Files renames GNAT.Directory_Operations;
   package Regex renames GNAT.Regpat;

--   package OS renames gnat.os_lib;
   -- Env, CL, and CLenv are just abbreviations for: Environment_Variables,
   -- Command_Line, and Command_Line.Environment

   -- xterm -bg black -fg white +sb +sm -fn 10x20 -sl 4000 -cr yellow

   -- /usr/bin/Xnest :1 -geometry 1024x768+0+0 -ac -name Windowmaker & wmaker
   -- -display :1

   Empty_Argument_List : constant Os.String_List (1 .. 0) := (others => null);
   Empty_List_Access   : Os.String_List_Access            :=
     new Os.String_List'(Empty_Argument_List);

   function Image (Num : Natural) return String is
      N : Natural          := Num;
      S : String (1 .. 48) := (others => ' ');
      L : Natural          := 0;
      procedure Image_Natural
        (N : in     Natural;
         S : in out String;
         L : in out Natural) is
      begin
         null;
         if N >= 10 then
            Image_Natural (N / 10, S, L);
            L     := L + 1;
            S (L) := Character'Val (48 + (N rem 10));
         else
            L     := L + 1;
            S (L) := Character'Val (48 + (N rem 10));
         end if;
      end Image_Natural;
   begin
      null;
      Image_Natural (N, S, L);
      return S (1 .. L);
   end Image;

   function Create_Display_Number return String is
      use Ada.Directories;
      New_Number : Natural := 0;

   begin
      if Exists ("/tmp") then
         loop
            if Exists ("/tmp/.X" & Image (New_Number) & "-lock") then
               New_Number := New_Number + 1;

            else
               return Image (New_Number);

            end if;

         end loop;

      else
         return "0";

      end if;
   end Create_Display_Number;

   -- Using GNAT.Directory_Operations, make given filesystem path compatible
   -- with the current Operating System:  Path ("to/file") => "to\file"(windows)
   function Path (Original : String) return String is
      use Os_Files;
      Result : Path_Name :=
        Os_Files.Format_Pathname
          (Path  => Path_Name (Original),
           Style => System_Default);
   begin
      return String (Result);
   end Path;

   -- Using GNAT.Directory_Operations, make given filesystem path compatible
   -- with the current Operating System:
   --   Path ("${HOME}/to/file") => "C:\msys\home\user\to\file" (windows)
   function Expand_Path (Original : String) return String is
      use Os_Files;
      Result : Path_Name :=
        Os_Files.Format_Pathname
          (Path =>
             Os_Files.Expand_Path (Path => Path_Name (Original), Mode => Both),
           Style => System_Default);
   begin
      return String (Result);
   end Expand_Path;

   -- Using GNAT.Directory_Operations and GNAT.OS_LIB, make given filesystem
   -- path compatible with the current Operating System, returning the full
   -- path:  Full_Path ("../to/file")  =>  "C:\dir\dir\to\file" (windows)
   function Full_Path (Original : String) return String is
      use Os;
   begin
      return Os.Normalize_Pathname (Path (Original));
   end Full_Path;

   User_Clientrc   : String := Expand_Path ("${HOME}/.xinitrc");
   System_Clientrc : String := Path ("/etc/X11/xinit/xinitrc");

   User_Serverrc   : String := Expand_Path ("${HOME}/.xserverrc");
   System_Serverrc : String := Path ("/etc/X11/xinit/xserverrc");
   Default_Client  : String := "xterm";
   Default_Server  : String := Path ("/usr/bin/X");

   Default_Client_Arguments_Access : Os.String_List_Access :=
     Os.Argument_String_To_List ("");

   Default_Server_Arguments_Access : Os.String_List_Access :=
     Os.Argument_String_To_List ("");

   Default_Client_Arguments : Os.String_List :=
     (if
        Default_Client_Arguments_Access /= null
      then
        Default_Client_Arguments_Access.all
      else Empty_List_Access.all);

   Default_Server_Arguments : Os.String_List :=
     (if
        Default_Server_Arguments_Access /= null
      then
        Default_Server_Arguments_Access.all
      else Empty_List_Access.all);

--   Defaultdisplay = ":0"
--   Clientargs = ""
--   Serverargs = ""
--   Vtarg = ""
--   Enable_Xauth = 1

   Env_Display : String := Env.Value ("DISPLAY", "");
   New_Display : String := ":" & Create_Display_Number;

   Custom_Client_Access : Os.String_Access :=
     (if
        Ada.Command_Line.Argument_Count > 0
      then
        Os.Locate_Exec_On_Path (Ada.Command_Line.Argument (1))
      else null);
   Manager_Command_Access : Os.String_Access :=
     Os.Locate_Exec_On_Path ("/usr/bin/dbus-launch");
   Xterm_Command_Access : Os.String_Access :=
     Os.Locate_Exec_On_Path ("/usr/bin/xterm");
   Xnest_Command_Access : Os.String_Access :=
     Os.Locate_Exec_On_Path ("/usr/bin/Xnest");
   Xserver_Command_Access : Os.String_Access :=
     Os.Locate_Exec_On_Path ("/usr/bin/Xserver");
   Xinit_Command_Access : Os.String_Access := Os.Locate_Exec_On_Path ("xinit");

   Xterm_Background                   : String  := "black";
   Xterm_Foreground                   : String  := "white";
   Xterm_Scrollbar                    : Boolean := False;
   Xterm_Session_Management_Callbacks : Boolean := False;
   Xterm_Loginshell                   : Boolean := False;
--   Xterm_Font                         : String  := "adobe-source code pro*";
--   Xterm_Font                         : String  := "gnu-unifont*";
   Xterm_Font : String :=
     "-gnu-unifont-medium-r-normal-sans-16-160-75-75-c-80-iso10646-1";
   Xterm_Font_Size    : String := "9";
   Xterm_Lineshistory : String := "4000";
   Xterm_Cursorcolor  : String := "yellow";
   Xterm_Border       : String := "256";
   Xterm_Geometry     : String := "200x50+-128+-128";
--   Xterm_Geometry                     : String  := "260x70+-128+-128";

   Xterm_Arguments : Os.Argument_List :=
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
   Xterm_Command_Arguments : Os.Argument_List :=
     (new String'("--"), Xterm_Command_Access) & Xterm_Arguments;

   Xnest_Title : String := "Graphical Session";
--   Xnest_Foreground : String := "white";
--   Xnest_Background : String := "black";
   Xnest_Geometry : String := "1920x1080+-0+-0";
   Xnest_Border   : String := "64";

   Empty_Arguments : GNAT.OS_Lib.Argument_List (1 .. 0) := (others => null);

   Xnest_Arguments : GNAT.OS_Lib.Argument_List :=
     (new String'(New_Display),
      new String'("-display"),
      new String'(Env_Display),
      new String'("-geometry"),
      new String'(Xnest_Geometry),
      new String'("-name"),
      new String'(Xnest_Title),
      new String'("-reset"),
      new String'("-terminate"),
      new String'("-fn"),
      new String'
        ("-gnu-unifont-medium-r-normal-sans-16-160-75-75-c-80-iso10646-1"),
      new String'("-bw"),
      new String'(Xnest_Border));

   Xserver_Arguments : GNAT.OS_Lib.Argument_List :=
     (new String'(New_Display),
      new String'("-display"),
      new String'(Env_Display),
      new String'("-geometry"),
      new String'(Xnest_Geometry),
      new String'("-name"),
      new String'(Xnest_Title),
      new String'("-reset"),
      new String'("-terminate"),
      new String'("-fn"),
      new String'
        ("-gnu-unifont-medium-r-normal-sans-16-160-75-75-c-80-iso10646-1"),
      new String'("-bw"),
      new String'(Xnest_Border));

   Launch_Status : Boolean := True;
   -- The return status of the command + arguments

   function Command_Arguments return GNAT.OS_Lib.Argument_List is
      Command_Words : GNAT.OS_Lib.Argument_List (1 .. Argument_Count) :=
        (others => null);
      Xterm_Command_Words : Os.String_List := Xterm_Command_Arguments;

   begin
      for N in 1 .. Argument_Count loop
         Command_Words (N) := new String'(Argument (N));

      end loop;

      if Command_Words'Length > 0 then
         return Command_Words;
      else
         return Xterm_Command_Words;
      end if;
   end Command_Arguments;

   procedure Launch
     (Command   : String;
      Arguments : GNAT.OS_Lib.Argument_List) is
      Launch_Arguments : GNAT.OS_Lib.Argument_List := Arguments;
   begin
      GNAT.OS_Lib.Spawn (Command, Launch_Arguments, Launch_Status);
   end Launch;

   Launch_Error_Count : Integer := 0;

begin
   Env.Clear ("SESSION_MANAGER");
   Env.Set ("DISPLAY", Env_Display);

   declare
      Arguments         : Os.Argument_List := Command_Arguments;
      Xnest             : Os.Process_Id;
      Xserver           : Os.Process_Id;
      Session_Manager   : Os.Process_Id;
      Process_With_Exit : Os.Process_Id;
      -- Arguments => checked and tweaked argument list given at progam start
      -- Xnest => The instance of Xnest or Xorg that will be monitored, Session_Manager

--      Xterm             : OS.Process_Id;

   begin

      if Env_Display /= "" then
         if Xnest_Command_Access /= null
           and then Manager_Command_Access /= null
         then
            Xnest :=
              Os.Non_Blocking_Spawn
                (Xnest_Command_Access.all,
                 Xnest_Arguments);
            Env.Set ("DISPLAY", New_Display);
            delay 0.02;
            Session_Manager :=
              Os.Non_Blocking_Spawn (Manager_Command_Access.all, Arguments);
            Os.Wait_Process (Process_With_Exit, Launch_Status);

         else
            Launch_Status   := False;
            Xnest           := Os.Invalid_Pid;
            Session_Manager := Invalid_Pid;

         end if;
      else
         --- Remaining startx operations go here. ---
         if Xserver_Command_Access /= null
           and then Manager_Command_Access /= null
         then
            Xserver :=
              Os.Non_Blocking_Spawn
                (Xserver_Command_Access.all,
                 Xserver_Arguments);
            Env.Set ("DISPLAY", New_Display);
            delay 0.02;
            Session_Manager :=
              Os.Non_Blocking_Spawn (Manager_Command_Access.all, Arguments);
            Os.Wait_Process (Process_With_Exit, Launch_Status);

         else
            Launch_Status   := False;
            Xnest           := Os.Invalid_Pid;
            Session_Manager := Invalid_Pid;

         end if;

      end if;

      if not Launch_Status then
         Launch_Error_Count := Launch_Error_Count + 1;
         Set_Exit_Status (Failure);

      else
         Launch_Error_Count := 0;
         Set_Exit_Status (Success);
      end if;
         --Give return status back to calling os/environment.

      for I in Arguments'Range loop
         Free (Arguments (I));
      end loop;

   end;

end Startx;
