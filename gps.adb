with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Gnat.Command_Line; use Gnat.Command_Line;
with Ada.Environment_Variables;
with Ada.Directories; use Ada.Directories;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Ada.Containers.Indefinite_Hashed_Maps; use Ada.Containers;
with Ada; use Ada;
with Ada.Command_Line.Environment;

procedure Gps is
   -- Ada_Launch is the default name, but is meant to be renamed
   -- to the executable name that this executable launches

   package Env renames Ada.Environment_Variables;
   package CL renames Ada.Command_Line;
   package CLenv renames Ada.Command_Line.Environment;
   -- Env, CL, and CLenv are just abbreviations for:
   -- Environment_Variables, Command_Line, and Command_Line.Environment

   Root        : String := "/opt/gps2016";
   Pixbuf_Base : String := Root & "/lib/gps/gdk-pixbuf-2.0/2.10.0/";
   -- AdaCore GPS install location

   Prog        : String := "gps";
   Launch_Name : String := Root & "/bin/gps_exe";
   Ld_Library_Path : String
     := Root & "/lib/gps:"
     & Root & "/lib:"
     & Env.Value ("LD_LIBRARY_PATH", "/lib");
   Dyld_Fallback_Library_Path : String
     := Root & "/lib/gps:"
     & Root & "/lib:"
     & Env.Value ("DYLD_FALLBACK_LIBRARY_PATH", "/lib");
   Xdg_Config_Dirs : String := Root & "/share";
   Xdg_Config_Home : String := Root & "/etc";
   Gtk_Data_Prefix:String:=Root
   -- GPS Command and Environment Variables Profile to use.
   -- The Environment to implement should be set up before the actual
   -- environment is cleared, so that existing Environment can be referenced.

   --Launch_Name      : String := "/bin/" & Simple_Name (Command_Name);
   -- The file name to execute/launch

   Launch_Arguments :
   GNAT.OS_Lib.Argument_List (1 .. Argument_Count);
   -- The arguments to give the executable

   Launch_Status    : Boolean;
   -- The return status of the command + arguments

begin

   for N in 1 .. Argument_Count loop
      Launch_Arguments (N) := new String'(Argument (N));
   end loop;
   -- Simply copy/convey all arguments to the new command launch.

   --Put_Line (Launch_Name);
   -- DEBUG ACTION - remove this statement when the performance is sufficient.


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

end Gps;


