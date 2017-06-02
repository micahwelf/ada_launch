with Ada.Text_IO;
with Ada.Streams;
with Ada.Characters.Latin_1;
with Ada.Text_IO.Text_Streams;
with Ada.Streams.Stream_IO;
with Ada.Calendar;
with Ada.Calendar.Time_Zones;
with Ada.Calendar.Formatting;
with Ada.Command_Line;
with GNAT.Calendar.Time_IO;
with GNAT.Regpat;
with GNAT.Formatted_String;
with Ada.Storage_IO;
--with AWS;

procedure Ada_Time is
   use Ada.Text_IO;
   use Ada.Characters.Latin_1;
   use Ada.Text_IO.Text_Streams;
   use Ada.Calendar;
   use Ada.Calendar.Time_Zones;
   use Ada.Command_Line;
   use GNAT.Calendar.Time_IO;

   Now : Time := Clock;

   function Unix_Time return Time is
      use Ada.Calendar.Formatting;

      U_Year     : Year_Number;
      U_Month    : Month_Number;
      U_Day      : Day_Number;
      U_Hour     : Hour_Number;
      U_Minute   : Minute_Number;
      U_Second   : Second_Number;
      U_Duration : Second_Duration;
   begin
      Split
        (Date       => Now,
         Year       => U_Year,
         Month      => U_Month,
         Day        => U_Day,
         Hour       => U_Hour,
         Minute     => U_Minute,
         Second     => U_Second,
         Sub_Second => U_Duration,
         Time_Zone  => 0);

      return Time_Of
          (Year       => U_Year,
           Month      => U_Month,
           Day        => U_Day,
           Hour       => U_Hour,
           Minute     => U_Minute,
           Second     => U_Second,
           Sub_Second => U_Duration,
           Time_Zone  => UTC_Time_Offset);
   end Unix_Time;

   function To_Multimedia
     (Time     : String;
      Fraction : Boolean := True) return String
   -- Convert pure seconds ***.*** to multimedia time string **h**m**s Fraction
   -- boolean value specifies whether to include the fractional part of the
   -- time: **h**m**.***s
    is
   begin
      null;
      return "";
   end To_Multimedia;

   function To_Clock
     (Time     : String;
      Fraction : Boolean := True) return String
   -- Convert pure seconds ***.*** to multimedia time string **:**:** Fraction
   -- boolean value specifies whether to include the fractional part of the
   -- time: **h**m**.***s
    is
   begin
      null;
      return "";
   end To_Clock;

   function To_Seconds (Time : String) return String
   -- Convert a Multimedia time string **h**m**s or **:**:**.*** to pure
   -- seconds ***.***
    is
   begin
      null;
      return "";
   end To_Seconds;

   function "+" (Left : String; Right : String) return String is
      use Ada.Characters.Latin_1;
   begin
      return Left & LF & Right;
   end "+";

--     Unix_Time : Time :=
--       Value (Date      => Image (Date => Now, Include_Time_Fraction => True),
--              Time_Zone => UTC_Time_Offset);
begin

   for A in 1 .. Argument_Count loop
      if Argument (A) = "-iso" or
        Argument (A) = "-I" or
        Argument (A) = "-i"
      then
         String'Write
           (Stream (Current_Output),
            Image (Date => Now, Picture => "%Y-%m-%d"));

      elsif Argument (A) = "-n" or Argument (A) = "-nano" then
         String'Write
           (Stream (Current_Output),
            Image (Date => Unix_Time, Picture => "%s.%o"));

      elsif Argument (A) (1) = '+' then
         declare
            Feature_Argument : String := Argument (A);

         begin
            String'Write
              (Stream (Current_Output),
               Image
                 (Date    => Now,
                  Picture =>
                    Picture_String
                      (Feature_Argument
                         (Feature_Argument'First + 1 ..
                              Feature_Argument'Last))));

         end;

      elsif Argument (A) = "-h" or Argument (A) = "--help" then
         String'Write
           (Stream (Current_Error),
            Command_Name &
            " [ -n ] [ +<format_string> ] [ -I ]" +
            "" +
            "Print unix time in base 10 by default." +
            "With '-n', include nano seconds." +
            "With '-i', '-I', or '--iso' print ISO date." +
            "With a custom format string starting with '+', print current time with given GNU Date format." +
            "With '-h' or '--help' print this message." +
            "With '-g' or '--gnu-help' print the Gnat GNU Date format directives." +
            "" +
            "(c) Micah Waddoups <dev@micahwelf.us>");

      elsif Argument (A) = "-g" or Argument (A) = "--gnu-help" then
         String'Write
           (Stream (Current_Error),
            "   %    a literal %" +
            "   n    a newline" +
            "   t    a horizontal tab" +
            "" +
            "   Time fields:" +
            "" +
            "   %H   hour (00..23)" +
            "   %I   hour (01..12)" +
            "   %k   hour ( 0..23)" +
            "   %l   hour ( 1..12)" +
            "   %M   minute (00..59)" +
            "   %p   locale's AM or PM" +
            "   %r   time, 12-hour (hh:mm:ss [AP]M)" +
            "   %s   seconds  since 1970-01-01  00:00:00 UTC" +
            "           (a nonstandard extension)" +
            "   %S   second (00..59)" +
            "   %T   time, 24-hour (hh:mm:ss)" +
            "" +
            "   Date fields:" +
            "" +
            "   %a   locale's abbreviated weekday name (Sun..Sat)" +
            "   %A   locale's    full   weekday   name,    variable   length" +
            "           (Sunday..Saturday)" +
            "   %b   locale's abbreviated month name (Jan..Dec)" +
            "   %B   locale's    full    month    name,   variable    length" +
            "           (January..December)" +
            "   %c   locale's date and time (Sat Nov 04 12:02:33 EST 1989)" +
            "   %d   day of month (01..31)" +
            "   %D   date (mm/dd/yy)" +
            "   %h   same as %b" +
            "   %j   day of year (001..366)" +
            "   %m   month (01..12)" +
            "   %U   week number  of year with  Sunday as first day  of week" +
            "           (00..53)" +
            "   %w   day of week (0..6) with 0 corresponding to Sunday" +
            "   %W   week number  of year with  Monday as first day  of week" +
            "           (00..53)" +
            "   %x   locale's date representation (mm/dd/yy)" +
            "   %y   last two digits of year (00..99)" +
            "   %Y   year (1970...)" +
            "" +
            "   By default,  date pads numeric fields with zeroes.  GNU date" +
            "   recognizes the following nonstandard numeric modifiers:" +
            "" +
            "   -    (hyphen) do not pad the field" +
            "   _    (underscore) pad the field with spaces" +
            "" +
            "   Here are some GNAT extensions to the GNU Date specification:" +
            "" +
            "   %i   milliseconds (3 digits)" +
            "   %e   microseconds (6 digits)" +
            "   %o   nanoseconds  (9 digits)" +
            "" +
            "" &
            Copyright_Sign &
            "(c) Free Software Foundation, Inc. -- GNAT, the Ada Compiler" +
            "(c) Micah Waddoups <dev@micahwelf.us>");

      else
         String'Write
           (Stream (Current_Output),
            Image (Date => Unix_Time, Picture => "%s"));

      end if;

   end loop;

   if Argument_Count = 0 then
      String'Write
        (Stream (Current_Output),
         Image (Date => Unix_Time, Picture => "%s"));

   end if;

end Ada_Time;

--  This is a string to describe date and time output format. The string
--  is a set of standard character and special tag that are replaced by the
--  corresponding values. It follows the GNU Date specification. Here are the
--  recognized directives :
--
--          %    a literal %
--          n    a newline
--          t    a horizontal tab
--
--          Time fields:
--
--          %H   hour (00..23)
--          %I   hour (01..12)
--          %k   hour ( 0..23)
--          %l   hour ( 1..12)
--          %M   minute (00..59)
--          %p   locale's AM or PM
--          %r   time, 12-hour (hh:mm:ss [AP]M)
--          %s   seconds  since 1970-01-01  00:00:00 UTC
--                (a nonstandard extension)
--          %S   second (00..59)
--          %T   time, 24-hour (hh:mm:ss)
--
--          Date fields:
--
--          %a   locale's abbreviated weekday name (Sun..Sat)
--          %A   locale's    full   weekday   name,    variable   length
--                  (Sunday..Saturday)
--          %b   locale's abbreviated month name (Jan..Dec)
--          %B   locale's    full    month    name,   variable    length
--                  (January..December)
--          %c   locale's date and time (Sat Nov 04 12:02:33 EST 1989)
--          %d   day of month (01..31)
--          %D   date (mm/dd/yy)
--          %h   same as %b
--          %j   day of year (001..366)
--          %m   month (01..12)
--          %U   week number  of year with  Sunday as first day  of week
--                  (00..53)
--          %w   day of week (0..6) with 0 corresponding to Sunday
--          %W   week number  of year with  Monday as first day  of week
--                  (00..53)
--          %x   locale's date representation (mm/dd/yy)
--          %y   last two digits of year (00..99)
--          %Y   year (1970...)
--
--          By default,  date pads numeric fields with zeroes.  GNU date
--          recognizes the following nonstandard numeric modifiers:
--
--          -    (hyphen) do not pad the field
--          _    (underscore) pad the field with spaces
--
--  Here are some GNAT extensions to the GNU Date specification:
--
--          %i   milliseconds (3 digits)
--          %e   microseconds (6 digits)
--          %o   nanoseconds  (9 digits)
