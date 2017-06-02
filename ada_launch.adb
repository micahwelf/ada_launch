with GNAT.OS_Lib;

package body Ada_Launch is

   function Current_Os return Os_Type is
      Detected_Os : Os_Type := Other_Os;
      function Windows_Test return Boolean is
      begin
         if Full_Name
             (Env.Value ("USERDRIVE", "C:") & Env.Value ("USERPATH", "\")) =
           Env.Value ("USERDRIVE", "C:") & Env.Value ("USERPATH", "\")
         then
            return True;
         else
            return False;
         end if;
      exception
         when Name_Error =>
            return False;
      end Windows_Test;
      function Unix_Test return Boolean is
      begin
         if Full_Name
             (Env.Value ("USERDRIVE", "C:") & Env.Value ("USERPATH", "\")) =
           Env.Value ("USERDRIVE", "C:") & Env.Value ("USERPATH", "\")
         then
            return True;
         else
            return False;
         end if;
      exception
         when Name_Error =>
            return False;
      end Unix_Test;
      function Mac_Test return Boolean is
      begin
         if Full_Name (Env.Value ("HOME", "/home")) =
           Env.Value ("HOME", "/home") and
           String (Env.Value ("HOME", "      ")) (1 .. 6) = "/Users"
         then
            return True;
         else
            return False;
         end if;
      exception
         when Name_Error =>
            return False;
      end Mac_Test;

   begin
      if Saved_Os = Other_Os then
         if Windows_Test then
            Detected_Os := Dos;
         elsif Unix_Test then
            Detected_Os := Unix;
         elsif Mac_Test then
            Detected_Os := Other_Os;
         end if;
      else
         Detected_Os := Saved_Os;
      end if;
      return Detected_Os;
   end Current_Os;

   function Current_Environment return App_Env.Map is
      New_Env : App_Env.Map := App_Env.Empty_Map.Copy;
      procedure Copy_Env (Name : String; Value : String) is
      begin
         New_Env (Name) := Value;
      end Copy_Env;
   begin
      Env.Iterate (Copy_Env'Access);
      return New_Env;
   end Current_Environment;

   procedure Set_Environment (New_Env : App_Env.Map) is
      procedure Set_Item (C : App_Env.Cursor) is
      begin
         Env.Set (App_Env.Key (C), App_Env.Element (C));
      end Set_Item;
   begin
      Env.Clear;
      New_Env.Iterate (Set_Item'Access);
   end Set_Environment;

   function "+" (Right : String) return Arguments.Vector is
      New_Vector : Arguments.Vector := Arguments.Empty_Vector.Copy;
   begin
      New_Vector.Append (Right);
      return New_Vector;
   end "+";

   -- This '&' is provided as a more traditional Ada style for a list/vector.
   function "&"
     (Left  : Arguments.Vector;
      Right : String) return Arguments.Vector is
   begin
      return Left & Arguments.Vector'(+Right);
   end "&";

   function "+"
     (Left  : Arguments.Vector;
      Right : String) return Arguments.Vector is
   begin
      return Left & Arguments.Vector'(+Right);
   end "+";

   function "+"
     (Left  : String;
      Right : Arguments.Vector) return Arguments.Vector is
   begin
      return Arguments.Vector'(+Left) & Right;
   end "+";

   function "+" (S : String) return Os.String_Access is
      New_String_Access : Os.String_Access := new String'(S);
   begin
      return New_String_Access;
   end "+";

   function "+" (S : String) return Os.Argument_List is
      New_Argument_List : Os.Argument_List (1 .. 1);
   begin
      New_Argument_List (1) := new String'(S);
      return New_Argument_List;
   end "+";

   function "+" (V : Arguments.Vector) return Os.Argument_List is
      New_Argument_List : Os.Argument_List (V.First_Index .. V.Last_Index);
   begin
      for I in V.First_Index .. V.Last_Index loop
         New_Argument_List (I) := new String'(V (I));
      end loop;

      return New_Argument_List;
   end "+";

   function Clone_Arguments return Arguments.Vector is
      New_Arguments : Arguments.Vector := Arguments.Empty_Vector.Copy;
   begin
      for I in 1 .. Argument_Count loop
         New_Arguments.Append (Argument (I));
      end loop;
      return New_Arguments;
   end Clone_Arguments;

   ---------------
   -- Put_Error --
   ---------------

   procedure Put_Error (Message : String := "") is
   begin
      Ada.Text_IO.Put_Line
        (File => Ada.Text_IO.Standard_Error,
         Item => Message);
   end Put_Error;

   ------------------
   -- Chromium_App --
   ------------------

   procedure Chromium_App
     (Profile    : in String           := "";
      App_Id     : in Id_String        := Null_Id;
      Url        : in String           := "";
      New_Window : in Boolean          := True;
      Options    : in Arguments.Vector := Arguments.Empty_Vector.Copy) is
      Search_List : Arguments.Vector :=
        (+"/usr/bin/chromium" +
         "C:\Program Files (x86)\Google\Chrome\Application\chrome.exe" +
         "chromium" +
         "Chrome.exe" +
         "google-chrome" +
         "google-chrome-stable" +
         "google-chrome-beta");

      function Get_Executable return String is
         Value : Os.String_Access;
      begin
         for C in Search_List.Iterate loop
            Value := Os.Locate_Exec_On_Path (Search_List (C));
            if Value /= null then
               declare
                  New_String : String := Value.all;
               begin
                  Free (Value);
                  return New_String;
               end;
            end if;
         end loop;
         return "";
      end Get_Executable;

      Spawn_Success   : Boolean          := False;
      Executable_Name : String           := Get_Executable;
      Arg_Vector      : Arguments.Vector := Options.Copy;
   begin
      if New_Window then
         Arg_Vector.Prepend (New_Item => "--new-window");
      end if;

      if App_Id /= Null_Id then
         Arg_Vector.Prepend (New_Item => "--app-id=" & String (App_Id));

      elsif Url'Length > 0 then
         Arg_Vector.Prepend (New_Item => "--app=" & Url);

      else
         null;

      end if;

      Arg_Vector.Prepend (New_Item => "--profile-directory=" & Profile);

      declare
         Spawn_Arguments : Os.Argument_List := +Arg_Vector;
      begin
         Spawn
           (Program_Name => Executable_Name,
            Args         => Spawn_Arguments,
            Success      => Spawn_Success);
         for X of Spawn_Arguments loop
            Os.Free (X);
         end loop;
      end;

      if Spawn_Success then
         Ada.Command_Line.Set_Exit_Status (Success);
      else
         Ada.Command_Line.Set_Exit_Status (Failure);
         Put_Error
           ("Execution of '" & Executable_Name & "' failed.  Please fix.");
      end if;
   end Chromium_App;

   ----------
   -- Xapp --
   ----------

   procedure Xapp
     (X_Command : String           := "xterm";
      Options   : Arguments.Vector := Arguments.Empty_Vector.Copy;
      New_Env   : App_Env.Map      := App_Env.Empty_Map.Copy;
      Xrm       : X_Resources.Map  := X_Resources.Empty_Map.Copy) is
      Spawn_Success     : Boolean            := False;
      Spawn_Arguments   : Arguments.Vector   := Options.Copy;
      Spawn_Environment : App_Env.Map        := New_Env.Copy;
      Spawn_Xrm         : X_Resources.Map    := Xrm.Copy;
      Xrm_Index         : X_Resources.Cursor := Spawn_Xrm.First;
      procedure To_Spawn_Arguments (Item : in X_Resources.Cursor) is
      begin
         Spawn_Arguments.Prepend
         (X_Resources.Key (Item) & ": " & X_Resources.Element (Item));
         Spawn_Arguments.Prepend ("-xrm");
      end To_Spawn_Arguments;
   begin
      Spawn_Xrm.Iterate (Process => To_Spawn_Arguments'Access);
      --raise Program_Error with "Unimplemented procedure Xapp";
   end Xapp;

   ---------------
   -- Xterm_App --
   ---------------

   procedure Xterm_App
     (Command  : String           := "top";
      Options  : Arguments.Vector := Arguments.Empty_Vector.Copy;
      Geometry : String           := "";
      Managed  : Boolean          := True;
      New_Env  : App_Env.Map      := App_Env.Empty_Map.Copy;
      Xrm      : X_Resources.Map  := X_Resources.Empty_Map.Copy) is
      Spawn_Success     : Boolean          := False;
      Spawn_Arguments   : Arguments.Vector := Options.Copy;
      Spawn_Environment : App_Env.Map      := New_Env.Copy;
      Spawn_Xrm         : X_Resources.Map  := Xrm.Copy;
      Test_V            : Arguments.Vector;
      procedure To_Spawn_Arguments (Item : in X_Resources.Cursor) is
      begin
         Spawn_Arguments.Prepend
         (X_Resources.Key (Item) & ": " & X_Resources.Element (Item));
         Spawn_Arguments.Prepend ("-xrm");
      end To_Spawn_Arguments;
   begin

      if Managed then
         Spawn_Xrm.Include ("*overrideRedirect", "False");
      else
         Spawn_Xrm.Include ("*overrideRedirect", "True");
      end if;

      if New_Env.Contains ("DISPLAY") then
         Set_Environment (New_Env);
      end if;

      if Geometry'Length > 0 then
         Spawn_Arguments.Prepend ("-geometry");
      end if;

      Spawn_Xrm.Iterate (Process => To_Spawn_Arguments'Access);
      --  Generated stub: replace with real body!
      --pragma Compile_Time_Warning (Standard.True, "Xterm_App unimplemented");
      --raise Program_Error with "Unimplemented procedure Xterm_App";
   end Xterm_App;

   ------------------
   -- Clone_Launch --
   ------------------

   procedure Clone_Launch
     (Command : in String := "/usr/bin/" & Simple_Name (Command_Name);
      Prepend_Arguments : in Arguments.Vector := Arguments.Empty_Vector.Copy;
      Append_Arguments : in Arguments.Vector := Arguments.Empty_Vector.Copy) is
      New_Arguments : Arguments.Vector :=
        Prepend_Arguments & Clone_Arguments & Append_Arguments;
   begin
      Ada_Launch.Command (Command, New_Arguments);
      --raise Program_Error with "Unimplemented procedure Clone_Launch";
   end Clone_Launch;

   -------------
   -- Command --
   -------------

   procedure Command
     (Command : String           := "true";
      Args    : Arguments.Vector := Arguments.Empty_Vector.Copy) is
      use GNAT.OS_Lib;
      Spawn_Success : Boolean := False;
      Spawn_Access  : Os.String_Access;
   begin
      Spawn_Access := Os.Locate_Exec_On_Path (Command);
      if Spawn_Access /= null then
         declare
            Executable_Name : String           := Spawn_Access.all;
            Spawn_Arguments : Os.Argument_List := +Args;
         begin
            Spawn
              (Program_Name => Executable_Name,
               Args         => Spawn_Arguments,
               Success      => Spawn_Success);
            for X of Spawn_Arguments loop
               Os.Free (X);
            end loop;
         end;
      end if;
      Os.Free (Spawn_Access);
   end Command;

   procedure Practice is
   begin
      Command ("echo", +"this is" + "a test");
   end Practice;

end Ada_Launch;
