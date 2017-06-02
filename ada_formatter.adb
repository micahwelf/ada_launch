with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with Ada.Streams.Stream_IO;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Characters;
with Ada.Characters.Latin_1;
with GNAT.OS_Lib;
with Ada; use Ada;

procedure Ada_Formatter is
   -- Nano formatting tool for the Ada language.

   use Ada.Text_IO;
   use Ada.Text_IO.Text_Streams;
   use GNAT.OS_Lib;
   use Ada.Command_Line;
   use Ada.Characters.Latin_1;

   -- formatter gnatpp -nM --separate-loop-then -c4 -rnb
   Pretty_Print_Command   : String_Access := Locate_Exec_On_Path ("gnatpp");
   Pretty_Print_Arguments : GNAT.OS_Lib.Argument_List :=
     (new String'("-nM"),
      new String'("--separate-loop-then"),
      new String'("-c4"),
      new String'("-rnb"));

   Launch_Status : Boolean := False;

   Pointless_Character : Character := ' ';

   function Command_Arguments return GNAT.OS_Lib.Argument_List is
      Command_Words : GNAT.OS_Lib.Argument_List (1 .. Argument_Count) :=
        (others => null);
   begin
      for N in 1 .. Argument_Count loop
         Command_Words (N) := new String'(Argument (N));
      end loop;
      return Command_Words;
   end Command_Arguments;

   procedure Launch
     (Command   : String;
      Arguments : GNAT.OS_Lib.Argument_List) is
      Launch_Arguments : GNAT.OS_Lib.Argument_List := Arguments;
   begin
      GNAT.OS_Lib.Spawn (Command, Launch_Arguments, Launch_Status);
   end Launch;

begin

   if Pretty_Print_Command /= null then
      Put_Line ("" & CR);
      Launch
        (Pretty_Print_Command.all,
         Pretty_Print_Arguments & Command_Arguments);

      if not Launch_Status then
         Set_Exit_Status (Failure);
         Put_Line ("" & CR);
         String'Write
           (Stream (Current_Output),
            "Press any key to return to Nano.");
         Character'Read (Stream (Current_Input), Pointless_Character);

      else
         Set_Exit_Status (Success);

      end if;

   end if;

   for I in Pretty_Print_Arguments'Range loop
      Free (Pretty_Print_Arguments (I));
   end loop;

end Ada_Formatter;
