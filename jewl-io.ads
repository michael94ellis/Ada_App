------------------------------------------------------------------------------
--                                                                          --
--                             J E W L . I O                                --
--                                                                          --
--   An extended input-output package for beginners, using graphical        --
--   dialogs for input which also write log information to the standard     --
--   output.                                                                --
--                                                                          --
--   The documentation below assumes that you are familiar with the main    --
--   features of the input and output facilities provide by Ada.Text_IO.    --
--                                                                          --
--   Copyright (C) John English 2000. Contact address: je@brighton.ac.uk    --
--   This software is released under the terms of the GNU General Public    --
--   License and is intended primarily for educational use. Please contact  --
--   the author to report bugs, suggestions and modifications.              --
--                                                                          --
------------------------------------------------------------------------------
-- $Id: jewl-io.ads 1.7 2007/01/08 17:00:00 JE Exp $
------------------------------------------------------------------------------
--
-- $Log: jewl-io.ads $
-- Revision 1.7  2007/01/08 17:00:00  JE
-- * Fixed linker options in JEWL.Win32_Interface to accommodate changes to GNAT
--   GPL 2006 compiler (thanks to John McCormick for this)
-- * Added delay in message loop to avoid the appearance of hogging 100% of CPU
--   time
--
-- Revision 1.6  2001/11/02 16:00:00  JE
-- * Fixed canvas bug when saving an empty canvas
-- * Restore with no prior save now acts as erase
-- * Removed redundant variable declaration in Image function
--
-- Revision 1.5  2001/08/22 15:00:00  JE
-- * Minor bugfix to Get_Text for combo boxes
-- * Minor changes to documentation (including new example involving dialogs)
--
-- Revision 1.4  2001/01/25 09:00:00  je
-- Changes visible to the user:
--
-- * Added support for drawing bitmaps on canvases (Draw_Image operations
--   and new type Image_Type)
-- * Added Play_Sound
-- * Added several new operations on all windows: Get_Origin, Get_Width,
--   Get_Height, Set_Origin, Set_Size and Focus
-- * Added several functions giving screen and window dimensions: Screen_Width,
--   Screen_Height, Frame_Width, Frame_Height, Dialog_Width, Dialog_Height and
--   Menu_Height
-- * Canvases can now handle keyboard events: new constructor and Key_Code added
-- * Added procedure Play_Sound
-- * Operations "+" and "-" added for Point_Type
-- * Pens can now be zero pixels wide
-- * The absolute origin of a frame can now have be specified when the frame
--   is created
-- * Added new File_Dialog operations Add_Filter and Set_Directory
-- * Added Get_Line renames to JEWL.IO for compatibility with Ada.Text_IO
-- * Added all the Get(File,Item) operations mentioned in documentation but
--   unaccountably missing :-(
-- * Documentation updated to reflect the above changes
-- * HTML versions of public package specifications added with links from
--   main documentation pages
--
-- Other internal changes:
--
-- * Canvas fonts, pens etc. now use JEWL.Reference_Counted_Type rather than
--   reinventing this particular wheel, as do images
-- * Various minor code formatting changes: some code reordered for clarity,
--   some comments added or amended,
-- * Changes introduced in 1.2 to support GNAT 3.10 have been reversed, since
--   GNAT 3.10 still couldn't compile this code correctly... ;-(
--
-- Outstanding issues:
--
-- * Optimisation breaks the code (workaround: don't optimise)
--
-- Revision 1.3  2000/07/07 12:00:00  je
-- * JEWL.Simple_Windows added; JEWL.IO modified to use JEWL.Simple_Windows.
-- * JEWL.IO bug fix: Put_Line to file wrote newline to standard output
--   instead of to the file (thanks to Jeff Carter for pointing this out).
-- * Panels fixed so that mouse clicks are passed on correctly to subwindows.
-- * Memos fixed so that tabs are handled properly.
-- * Password feature added to editboxes.
-- * Minor typos fixed in comments within the package sources.
-- * Documentation corrected and updated following comments from Moti Ben-Ari
--   and Don Overheu.
--
-- Revision 1.2  2000/04/18 20:00:00  je
-- * Minor code changes to enable compilation by GNAT 3.10
-- * Minor documentation errors corrected
-- * Some redundant "with" clauses removed
--
-- Revision 1.1  2000/04/09 21:00:00  je
-- Initial revision
--
------------------------------------------------------------------------------

with Ada.Text_IO;

package JEWL.IO is

  ----------------------------------------------------------------------------
  --
  -- Some local names for types from Ada.Text_IO
  --
  subtype Positive_Count is Ada.Text_IO.Positive_Count;
  subtype File_Type      is Ada.Text_IO.File_Type;

  ----------------------------------------------------------------------------
  --
  -- Exceptions that this package might raise
  --
  Input_Cancelled : exception;      -- the user cancelled an input dialog

  ----------------------------------------------------------------------------
  --
  -- Routines to display message boxes
  --
  procedure Error   (Text : in String);                 -- an error message
  function  Query   (Text : in String) return Boolean;  -- a yes/no query
  procedure Message (Text : in String);                 -- an information message

  ----------------------------------------------------------------------------
  --
  -- Output a newline, as in Ada.Text_IO
  --
  procedure New_Line (Spacing : in Ada.Text_IO.Positive_Count := 1)
            renames Ada.Text_IO.New_Line;

  ----------------------------------------------------------------------------
  --
  -- Opening and closing files, using standard file dialogs to get filenames
  --
  procedure Open   (File  : in out File_Type;   -- open an existing file
                    Title : in String := "Select input file");
  procedure Create (File  : in out File_Type;   -- create a new file
                    Title : in String := "Select output file");
  procedure Append (File  : in out File_Type;   -- append to existing/new file
                    Title : in String := "Select output file");
  procedure Close  (File   : in out Ada.Text_IO.File_Type)
                                renames Ada.Text_IO.Close;
                                                -- close an open file

  ----------------------------------------------------------------------------
  --
  --  Standard file positioning operations and queries, as in Ada.Text_IO
  --
  procedure New_Line    (File    : in Ada.Text_IO.File_Type;
                         Spacing : in Positive_Count := 1)
                                renames Ada.Text_IO.New_Line;
  procedure Skip_Line   (File    : in Ada.Text_IO.File_Type;
                         Spacing : in Positive_Count := 1)
                                renames Ada.Text_IO.Skip_Line;
  function  End_Of_Line (File    : in Ada.Text_IO.File_Type)
                         return Boolean
                                renames Ada.Text_IO.End_Of_Line;
  function  End_Of_File (File    : in Ada.Text_IO.File_Type)
                         return Boolean
                                renames Ada.Text_IO.End_Of_File;

  ----------------------------------------------------------------------------
  --
  --      P R I M A R Y   I N P U T / O U T P U T   R O U T I N E S
  --
  --                         ---- Input ----
  --
  --  For each scalar type (with generic packages for enumerations, integral
  --  types and floating point types, and support provided as standard for
  --  String, Character, Integer, Float, and Boolean):
  --
  --  Get (Item    => X,       -- get a value into X,
  --       Default => Y,       -- with an initial default value of Y (optional)
  --       Prompt  => Z);      -- displaying Z as a prompt (optional),
  --
  --  Get (File    => F,
  --       Item    => X);      -- get a value into X from file F
  --
  --  The prompt is always a string; the default value depends on the type of
  --  data involved. In the case of String there is a potential ambiguity as
  --  the prompt and default values are both strings. It is recommended that
  --  the Prompt and Default parameters are always specified as "Prompt=>X"
  --  and "Default=>X" to avoid confusion.
  --
  --  Strings are a bit different: there is a function which gets a String
  --  (unconstrained) and a procedure which gets a value into a String but
  --  which also returns the length of the string in Length:
  --
  --  S := Get (Prompt  => Y,  -- optional
  --            Default => Z); -- optional
  --
  --  Get (Item    => X,       -- get a value into X,
  --       Length  => L,       -- whose actual length is L,
  --       Prompt  => Y,       -- displaying Y as a prompt (optional),
  --       Default => Z);      -- with an initial default value of Z (optional)
  --
  --  Get (File    => F,
  --       Item    => X,       -- get a value into X from file F,
  --       Length  => L);      -- whose actual length is L
  --
  --                        ---- Output ----
  --
  --  Values can be output to the standard output or a file, with or without
  --  a following newline:
  --
  --  Put (Item => X);         -- write the value of X on the standard output
  --  Put (File => F,
  --       Item => X);         -- write the value of X on the file F
  --
  --  Put_Line (Item => X);    -- write X and a newline on the standard output
  --  Put_Line (File => F,
  --            Item => X);    -- write X and a newline on the file F
  --
  --                    ---- Type conversion ----
  --
  --  S := To_String(X)        -- convert a (non-string) type to a String
  --  S := S & X;              -- concatenate String and X
  --  S := X & S;              -- concatenate X and String
  --
  ----------------------------------------------------------------------------
  --
  --  String and Character input
  --
  function  Get (Prompt  : in  String := "Enter your text:";
                 Default : in  String := "")
                 return String;     -- display a dialog with a label (Prompt)
                                    -- and an editbox (initial value Default)
                                    -- and return the contents of the editbox
                                    -- as a String (unconstrained)

  procedure Get (Item    : out String;
                 Length  : out Natural;
                 Prompt  : in  String := "Enter your text:";
                 Default : in  String := "");
                                    -- use the same edit dialog to get a string
                                    -- into a variable, the maximum length being
                                    -- limited by the size of Item and the actual
                                    -- length being stored in Length, with an
                                    -- initial default value
  procedure Get (File    : in Ada.Text_IO.File_Type;
                 Item    : out Character)
                                renames Ada.Text_IO.Get;
                                    -- get a character from a file, from Text_IO
  procedure Get (File    : in Ada.Text_IO.File_Type;
                 Item    : out String;
                 Length  : out Natural)
                                renames Ada.Text_IO.Get_Line;
                                    -- get a string and its length from a file,
                                    -- from Text_IO

  ----------------------------------------------------------------------------
  --  Renamings for consistency with Ada.Text_IO
  ----------------------------------------------------------------------------

  procedure Get_Line (Item    : out String;
                      Length  : out Natural     ;
                      Prompt  : in  String := "Enter your text:";
                      Default : in  String := "")  renames Get;

  procedure Get_Line (File    : in Ada.Text_IO.File_Type;
                      Item    : out String;
                      Length  : out Natural)       renames Get;

  ----------------------------------------------------------------------------
  --
  --  String and Character output
  --
  procedure Put (Item    : in String)
                                renames Ada.Text_IO.Put;
                                    -- output a string
  procedure Put (Item    : in Character)
                                renames Ada.Text_IO.Put;
                                    -- output a character
  procedure Put (File    : in Ada.Text_IO.File_Type;
                 Item    : in Character)
                                renames Ada.Text_IO.Put;
                                    -- output a character to a file
  procedure Put (File    : in Ada.Text_IO.File_Type;
                 Item    : in String)
                                renames Ada.Text_IO.Put;
                                    -- output a string to a file

  ----------------------------------------------------------------------------
  --
  --  String and Character output, with newlines
  --
  procedure Put_Line (Item : in String)
                                renames Ada.Text_IO.Put_Line;
                                    -- output a string and newline
  procedure Put_Line (Item : in Character);
                                    -- output a character and newline
  procedure Put_Line (File : in Ada.Text_IO.File_Type;
                      Item : in String)
                                renames Ada.Text_IO.Put_Line;
                                    -- output a string and newline to a file
  procedure Put_Line (File : in File_Type;
                      Item : in Character);
                                    -- output a character and newline to a file

  ----------------------------------------------------------------------------
  --
  --               INTEGER INPUT, OUTPUT AND CONVERSION
  --
  ----------------------------------------------------------------------------
  --
  --  Integer input
  --
  procedure Get (Item    : out Integer;
                 Prompt  : in  String := "Enter an integer:");
                                    -- display a dialog with a label (Prompt)
                                    -- and a blank editbox which returns the
                                    -- contents of the editbox as an Integer
  procedure Get (Item    : out Integer;
                 Default : in  Integer;
                 Prompt  : in  String := "Enter an integer:");
                                    -- display the same dialog with the editbox
                                    -- initialised to the value of Default
  procedure Get (File    : in  File_Type;
                 Item    : out Integer);
                                    -- read an Integer from a file

  ----------------------------------------------------------------------------
  --
  --  Integer output
  --
  procedure Put (Item    : in Integer);
                                    -- output an Integer
  procedure Put (File    : in File_Type;
                 Item    : in Integer);
                                    -- output an Integer to a file

  ----------------------------------------------------------------------------
  --
  --  Integer output, with newlines
  --
  procedure Put_Line (Item : in Integer);
                                    -- output an Integer and a newline
  procedure Put_Line (File : in File_Type;
                      Item : in Integer);
                                    -- output an Integer and a newline to a file

  ----------------------------------------------------------------------------
  --
  --  Integer conversion routines
  --
  function  To_String (Item : Integer) return String;
                                    -- convert an integer to a string

  function  "&" (Left    : String;
                 Right   : Integer)    return String;
                                    -- concatenate String & Integer
  function  "&" (Left    : Integer;
                 Right   : String)     return String;
                                    -- concatenate Integer & String

  ----------------------------------------------------------------------------
  --
  --                FLOAT INPUT, OUTPUT AND CONVERSION
  --
  ----------------------------------------------------------------------------
  --
  --  Float input
  --
  procedure Get (Item    : out Float;
                 Prompt  : in  String := "Enter a number:");
                                    -- display a dialog with a label (Prompt)
                                    -- and a blank editbox which returns the
                                    -- contents of the editbox as an Integer
  procedure Get (Item    : out Float;
                 Default : in  Float;
                 Prompt  : in  String := "Enter a number:");
                                    -- display the same dialog with the editbox
                                    -- initialised to the value of Default
  procedure Get (File    : in  File_Type;
                 Item    : out Float);
                                    -- read a Float from a file

  ----------------------------------------------------------------------------
  --
  --  Float output
  --
  procedure Put (Item    : in  Float);
  procedure Put (File    : in  File_Type;
                 Item    : in  Float);

  ----------------------------------------------------------------------------
  --
  --  Float output, with newlines
  --
  procedure Put_Line (Item : in Float);
  procedure Put_Line (File : in File_Type;
                      Item : in Float);

  ----------------------------------------------------------------------------
  --
  --   Float conversion routines
  --
  function  To_String (Item : Float) return String;

  function  "&" (Left    : String;
                 Right   : Float)    return String;
  function  "&" (Left    : Float;
                 Right   : String)   return String;

  ----------------------------------------------------------------------------
  --
  --               BOOLEAN INPUT, OUTPUT AND CONVERSION
  --
  ----------------------------------------------------------------------------
  --
  --  Boolean input
  --
  procedure Get (Item    : out Boolean;
                 Prompt  : in  String := "Yes or no?");
                                    -- display a dialog with Yes/No/Cancel
                                    -- buttons
  procedure Get (Item    : out Boolean;
                 Default : in  Boolean;
                 Prompt  : in  String := "Yes");
                                    -- display a dialog with a checkbox
                                    -- initialised as specified by Default
  procedure Get (File    : in  File_Type;
                 Item    : out Boolean);
                                    -- read a Boolean from a file

  ----------------------------------------------------------------------------
  --
  --  Boolean output
  --
  procedure Put (Item    : in Boolean);
  procedure Put (File    : in File_Type;
                 Item    : in Boolean);

  ----------------------------------------------------------------------------
  --
  --  Boolean output, with newlines
  --
  procedure Put_Line (Item : in Boolean);
  procedure Put_Line (File : in File_Type;
                      Item : in Boolean);

  ----------------------------------------------------------------------------
  --
  --  Boolean conversion routines
  --
  function  To_String (Item : Boolean) return String;

  function  "&" (Left    : String;
                 Right   : Boolean)    return String;
  function  "&" (Left    : Boolean;
                 Right   : String)     return String;

  ----------------------------------------------------------------------------
  --
  --             ENUMERATION INPUT, OUTPUT AND CONVERSION
  --
  ----------------------------------------------------------------------------

  generic
    type Item_Type is (<>);
  package Enumeration_IO is

    --------------------------------------------------------------------------
    --
    --  Enumeration input
    --
    procedure Get (Item    : out Item_Type;
                   Prompt  : in  String := "Choose a value:");
                                    -- display a dialog with a label (Prompt)
                                    -- and a combobox listing all values of
                                    -- type Item_Type
    procedure Get (Item    : out Item_Type;
                   Default : in  Item_Type;
                   Prompt  : in  String := "Choose a value:");
                                    -- display the same dialog with the combobox
                                    -- initialised to Default
    procedure Get (File    : in  File_Type;
                   Item    : out Item_Type);
                                    -- read an Item_Type value from a file

    --------------------------------------------------------------------------
    --
    --  Enumeration output
    --
    procedure Put (Item    : in  Item_Type);
    procedure Put (File    : in File_Type;
                   Item    : in Item_Type);

    --------------------------------------------------------------------------
    --
    --  Enumeration output, with newlines
    --
    procedure Put_Line (Item : in Item_Type);
    procedure Put_Line (File : in File_Type;
                        Item : in Item_Type);

    --------------------------------------------------------------------------
    --
    --  Enumeration conversion routines
    --
    function  To_String (Item : Item_Type) return String;

    function  "&" (Left    : String;
                   Right   : Item_Type)    return String;
    function  "&" (Left    : Item_Type;
                   Right   : String)       return String;

  end Enumeration_IO;

  ----------------------------------------------------------------------------
  --
  --         GENERIC INTEGRAL INPUT, OUTPUT AND CONVERSION
  --
  ----------------------------------------------------------------------------

  generic
    type Item_Type is range <>;
  package Integer_IO is

    --------------------------------------------------------------------------
    --
    --  Generic integral input
    --
    procedure Get (Item    : out Item_Type;
                   Prompt  : in  String := "Enter an integer:");
                                    -- display a dialog with a label (Prompt)
                                    -- and a blank editbox which returns the
                                    -- contents of the editbox as an Item_Type
                                    -- integral value
    procedure Get (Item    : out Item_Type;
                   Default : in  Item_Type;
                   Prompt  : in  String := "Enter an integer:");
                                    -- display the same dialog with the editbox
                                    -- initialised to the value of Default
    procedure Get (File    : in  File_Type;
                   Item    : out Item_Type);
                                    -- read an Item_Type value from a file

    --------------------------------------------------------------------------
    --
    --  Generic integral output
    --
    procedure Put (Item    : in  Item_Type);
    procedure Put (File    : in File_Type;
                   Item    : in Item_Type);

    --------------------------------------------------------------------------
    --
    --  Generic integral output, with newlines
    --
    procedure Put_Line (Item : in Item_Type);
    procedure Put_Line (File : in File_Type;
                        Item : in Item_Type);

    --------------------------------------------------------------------------
    --
    -- Generic integral conversion routines
    --
    function  To_String (Item : Item_Type) return String;

    function  "&" (Left    : String;
                   Right   : Item_Type)    return String;
    function  "&" (Left    : Item_Type;
                   Right   : String)       return String;

  end Integer_IO;

  ----------------------------------------------------------------------------
  --
  --       GENERIC FLOATING-POINT INPUT, OUTPUT AND CONVERSION
  --
  ----------------------------------------------------------------------------

  generic
    type Item_Type is digits <>;
  package Float_IO is

    --------------------------------------------------------------------------
    --
    --  Generic floating-point input
    --
    procedure Get (Item    : out Item_Type;
                   Prompt  : in  String := "Enter a number:");
                                    -- display a dialog with a label (Prompt)
                                    -- and a blank editbox which returns the
                                    -- contents of the editbox as an Item_Type
                                    -- floating-point value
    procedure Get (Item    : out Item_Type;
                   Default : in  Item_Type;
                   Prompt  : in  String := "Enter a number:");
                                    -- display the same dialog with the editbox
                                    -- initialised to the value of Default
    procedure Get (File    : in  File_Type;
                   Item    : out Item_Type);
                                    -- read an Item_Type value from a file

    --------------------------------------------------------------------------
    --
    --  Generic floating-point output
    --
    procedure Put (Item    : in  Item_Type);
    procedure Put (File    : in File_Type;
                   Item    : in Item_Type);

    --------------------------------------------------------------------------
    --
    --  Generic floating-point output, with newlines
    --
    procedure Put_Line (Item : in Item_Type);
    procedure Put_Line (File : in File_Type;
                        Item : in Item_Type);

    --------------------------------------------------------------------------
    --
    --  Generic floating-point conversion routines
    --
    function  To_String (Item : Item_Type) return String;

    function  "&" (Left    : String;
                   Right   : Item_Type)    return String;
    function  "&" (Left    : Item_Type;
                   Right   : String)       return String;

  end Float_IO;

end JEWL.IO;
