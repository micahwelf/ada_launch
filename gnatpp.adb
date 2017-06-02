with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Environment_Variables;
with Ada.Directories; use Ada.Directories;
with GNAT.OS_Lib; use GNAT.OS_Lib;

procedure Gnatpp is
   -- Ada_Launch is the default name, but is meant to be renamed
   -- to the executable name that this executable launches

   package Env renames Ada.Environment_Variables;
   package CL renames Ada.Command_Line;
   package Fs renames Ada.Directories;
   -- Env, CL, and CLenv are just abbreviations for:
   -- Environment_Variables, Command_Line, and Command_Line.Environment
   --app-id=gbchcmhmhahfdphkhkmpfmihenigjmpp
   Root        : String := "/opt/gps2016";
   Pixbuf_Base : String := Root & "/lib/gps/gdk-pixbuf-2.0/2.10.0/";
   Root_Build : String
     := Root & "/lib/gcc/x86_64-pc-linux-gnu/4.9.4/rts-native";
   -- AdaCore GPS install location

   Prog        : String := Simple_Name(Command_Name);
   Launch_Name : String := Root & "/bin/" & "gnatpp";
   -- The file name to execute/launch

   Launch_Arguments :
   GNAT.OS_Lib.Argument_List (1 .. Argument_Count);
   -- The arguments to give the executable

   Launch_Status    : Boolean;
   -- The return status of the command + arguments

begin

   Env.Set ("LD_LIBRARY_PATH", Root & "/lib/gps:"
            & Root & "/lib:"
            & Env.Value ("LD_LIBRARY_PATH", "/lib"));
   Env.Set ("DYLD_FALLBACK_LIBRARY_PATH", Root & "/lib/gps:"
            & Root & "/lib:"
            & Env.Value ("DYLD_FALLBACK_LIBRARY_PATH", "/lib"));
   Env.Set ("XDG_CONFIG_DIRS", Root & "/share");
   Env.Set ("XDG_CONFIG_HOME", Root & "/etc");
   Env.Set ("GTK_DATA_PREFIX", Root);
   Env.Set ("GTK_EXE_PREFIX", Root);
   Env.Set ("GI_TYPELIB_PATH", Root & "/lib/girepository-1.0");
   Env.Set ("GSETTINGS_BACKEND", "memory");

   -- Test that GPS with GTK is fully installed.
   case Kind (Name => Root & "/lib/gps") is
   when Directory =>
      Env.Clear ("GTK2_RC_FILES");
   when others =>
      Set_Exit_Status (4);
      return;
   end case;

   -- Test for very local font configuration
   case Kind (Name => Root & "/etc/fonts/fonts.conf") is
   when Ordinary_File =>
      Env.Set ("FONTCONFIG_FILE", Root & "/etc/fonts/fonts.conf");
   when others =>
      Set_Exit_Status (4);
      return;
   end case;

   -- Test for very local Pango configuration
   if Fs.Exists (Name => Root & "/etc/pango/pangorc")
   then
      if Kind (Name => Root & "/etc/pango/pangorc") = Ordinary_File
      then
         Env.Set ("PANGO_RC_FILE", Root & "/etc/pango/pangorc");
      end if;
--     else
--        Put_Line (Standard_Error, "Problem finding pangorc");
   end if;

   --Test that PixBuf in in place
   case Kind (Name => Pixbuf_Base & "/loaders.cache") is
   when Ordinary_File =>
      Env.Set ("GDK_PIXBUF_MODULE_FILE", Pixbuf_Base & "/loaders.cache");
      Env.Set ("GDK_PIXBUF_MODULEDIR", Pixbuf_Base & "/loaders");
   when others =>
      Put_Line (Standard_Error, "Problem finding PixBuf cache");
      return;
   end case;

   -- Test that if CHARSET is set to use.
   if Env.Value ("CHARSET", "") = ""
   then
      Env.Set ("CHARSET", "ISO-8859-1");
   end if;

   Env.Set ("PATH", Root & "/bin:/usr/bin:/bin");
   Env.Set ("ADA_INCLUDE_PATH", Root_Build & "/adainclude"
            & ":/usr/share/ada/adainclude:"
            & "/usr/local/share/ada/adainclude");
   Env.Set ("ADA_OBJECTS_PATH", Root_Build & "/adalib"
            & ":/usr/share/ada/adalib:"
            & "/usr/local/share/ada/adalib");

   for N in 1 .. Argument_Count loop
      Launch_Arguments (N) := new String'(Argument (N));
   end loop;
   -- Simply copy/convey all arguments to the new command launch.

   --Put_Line (Launch_Name);
   -- DEBUG ACTION - remove this statement when the performance is sufficient.


   Spawn (Launch_Name, Launch_Arguments, Launch_Status);
   -- Launch the new process with conveyed arguments and capture general
   -- return status as Success or Failure.  A number may be used in the future.

   for N in 1 .. Launch_Arguments'Length
   loop
      null;
      -- deallocate memory not in use anymore...
   end loop;

   if not Launch_Status
   then
      Set_Exit_Status (Failure);
   else
      Set_Exit_Status (Success);
   end if;
   --Give return status back to calling os/environment.

end Gnatpp;


