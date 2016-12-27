------------------------------------------------------------------------------
--                                                                          --
--                                 J E W L                                  --
--                                                                          --
--   Top-level package in a hierarchy providing I/O and Windows facilities  --
--   for beginners.                                                         --
--                                                                          --
--   Copyright (C) John English 2000. Contact address: je@brighton.ac.uk    --
--   This software is released under the terms of the GNU General Public    --
--   License and is intended primarily for educational use. Please contact  --
--   the author to report bugs, suggestions and modifications.              --
--                                                                          --
------------------------------------------------------------------------------
-- $Id: jewl.ads 1.7 2007/01/08 17:00:00 JE Exp $
------------------------------------------------------------------------------
--
-- $Log: jewl.ads $
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
-- Revision 1.1  2000/04/18 19:34:15  je
-- Initial revision
--
-- Revision 1.1  2000/04/09 21:00:00  je
-- Initial revision
--
------------------------------------------------------------------------------

with Ada.Finalization;

package JEWL is

  ----------------------------------------------------------------------------
  --
  --                      S U P P O R T   T Y P E S
  --
  --  These types are used elsewhere throughout this package library:
  --
  --  Angle_Type      : an angle specified as an integral number of
  --                    degrees (0 to 359)
  --  Colour_Type     : a colour specified as an RGB value.
  --  Font_Type       : a font specified by a name, point size and style
  --                    options.
  --  Point_Type      : a pair of (X,Y) coordinates within a window.
  --  Point_List      : an array of (X,Y) coordinate pairs.
  --
  ----------------------------------------------------------------------------

  type    Angle_Type     is mod 360;
  subtype Colour_Range   is Integer range 0..255;
  type    Colour_Type    is record
                              Red    : Colour_Range;
                              Green  : Colour_Range;
                              Blue   : Colour_Range;
                            end record;
  type    Font_Type (Length : Natural)
                         is record
                              Name   : String (1..Length);
                              Size   : Positive;
                              Bold   : Boolean := False;
                              Italic : Boolean := False;
                            end record;

  type    Point_Type     is record
                              X,Y : Integer;
                            end record;
  type    Point_List     is array (Positive range <>) of Point_Type;

  ----------------------------------------------------------------------------
  --
  --        O P E R A T I O N S   O N   S U P P O R T   T Y P E S
  --
  --  Colour operations:
  --    Light    : Generate a lightened version of a colour, e.g. Light(Red).
  --    Dark     : Generate a darkened version of a colour, e.g. Dark(Green).
  --
  --  Font operations:
  --    Font     : Generate a font with the specified properties.
  --    Name     : Get the name of the font typeface.
  --    Size     : Get the font size in points.
  --    Bold     : Test if the font is bold.
  --    Italic   : Test if the font is italic.
  --
  --  Point operations:
  --    Endpoint : Calculate the endpoint of a line from a starting point,
  --               length and angle.
  --    Inside   : Test if a specified point is inside a specified rectangle
  --               (defined by the coordinates of two diagonally opposite
  --               corners).
  --    P1 + P2  : Add two points (P1.X+P2.X, P1.Y+P2.Y).
  --    P1 - P2  : Subtract two points (P1.X-P2.X, P1.Y-P2.Y).
  --
  ----------------------------------------------------------------------------

  function Light    (Colour : Colour_Type) return Colour_Type;
  function Dark     (Colour : Colour_Type) return Colour_Type;

  function Font     (Name   : String;
                     Size   : Positive;
                     Bold   : Boolean  := False;
                     Italic : Boolean  := False)
                                           return Font_Type;
  function Name     (Font   : Font_Type)   return String;
  function Size     (Font   : Font_Type)   return Natural;
  function Bold     (Font   : Font_Type)   return Boolean;
  function Italic   (Font   : Font_Type)   return Boolean;

  function Endpoint (From   : Point_Type;
                     Length : Positive;
                     Angle  : Angle_Type)  return Point_Type;
  function Inside   (Point  : Point_Type;
                     From   : Point_Type;
                     To     : Point_Type)  return Boolean;
  function "+"      (P1, P2 : Point_Type)  return Point_Type;
  function "-"      (P1, P2 : Point_Type)  return Point_Type;

  ----------------------------------------------------------------------------
  --  Renamings for our transatlantic cousins, in the hope that some day
  --  they'll repay the favour/favor... ;-)
  ----------------------------------------------------------------------------

  subtype Color_Range is Colour_Range;
  subtype Color_Type  is Colour_Type;

private

  ----------------------------------------------------------------------------
  --  The private part of this package also provides a convenient place to
  --  declare the reference-counted types used by the generic package
  --  JEWL.Windows. Although they could be declared in the generic
  --  package JEWL.Windows, the use of a controlled type in a generic
  --  package would make it necessary to make all instantiations of the
  --  package at library level, a restriction which would probably baffle
  --  those for whom this software is intended.
  ----------------------------------------------------------------------------
  --
  --  A type which includes a reference count:
  --
  type Reference_Counted_Type is tagged limited
    record
      Count : Natural := 1;
    end record;

  ----------------------------------------------------------------------------
  --
  --  A primitive operation called when the last reference is deleted:
  --
  procedure Cleanup (Object : in out Reference_Counted_Type);

  ----------------------------------------------------------------------------
  --
  --  A pointer to the class of reference-counted types:
  --
  type Reference_Counted_Ptr is access all Reference_Counted_Type'Class;

  ----------------------------------------------------------------------------
  --
  --  A record which contains a pointer to a reference-counted object:
  --
  type Controlled_Type is new Ada.Finalization.Controlled with
    record
      Pointer : Reference_Counted_Ptr;
    end record;

  ----------------------------------------------------------------------------
  --
  --  Controlled operations:
  --
  procedure Finalize (Object : in out Controlled_Type);
  procedure Adjust   (Object : in out Controlled_Type);

end JEWL;
