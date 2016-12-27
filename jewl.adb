------------------------------------------------------------------------------
--                                                                          --
--                                 J E W L                                  --
--                                                                          --
--   Body of the top-level package providing I/O and Windows facilities     --
--   for beginners.                                                         --
--                                                                          --
--   Copyright (C) John English 2000. Contact address: je@brighton.ac.uk    --
--   This software is released under the terms of the GNU General Public    --
--   License and is intended primarily for educational use. Please contact  --
--   the author to report bugs, suggestions and modifications.              --
--                                                                          --
------------------------------------------------------------------------------
-- $Id: jewl.adb 1.7 2007/01/08 17:00:00 JE Exp $
------------------------------------------------------------------------------
--
-- $Log: jewl.adb $
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
-- Revision 1.1  2000/04/18 20:00:00  je
-- Initial revision
--
-- Revision 1.1  2000/04/09 21:00:00  je
-- Initial revision
--
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

package body JEWL is

  ----------------------------------------------------------------------------
  --
  --        O P E R A T I O N S   O N   S U P P O R T   T Y P E S
  --
  ----------------------------------------------------------------------------
  --
  --  Light: generate a lightened version of a given colour by increasing the
  --         intensity of each hue by 50%.
  --
  function Light (Colour : Colour_Type) return Colour_Type is
  begin
    return (Red   => (Colour.Red+Colour_Range'Last+1)/2,
            Green => (Colour.Green+Colour_Range'Last+1)/2,
            Blue  => (Colour.Blue+Colour_Range'Last+1)/2);
  end Light;

  ----------------------------------------------------------------------------
  --
  --  Dark: generate a darkened version of a given colour by decreasing the
  --        intensity of each hue by 50%.
  --
  function Dark (Colour : Colour_Type) return Colour_Type is
  begin
    return (Red   => Colour.Red/2,
            Green => Colour.Green/2,
            Blue  => Colour.Blue/2);
  end Dark;

  ----------------------------------------------------------------------------
  --
  --  Font: create a Font_Type structure which has the length of the font
  --        name as a discriminant.
  --
  function Font  (Name   : String;
                  Size   : Positive;
                  Bold   : Boolean  := False;
                  Italic : Boolean  := False) return Font_Type is
    F : Font_Type(Name'Length);
  begin
    F.Name   := Name;
    F.Size   := Size;
    F.Bold   := Bold;
    F.Italic := Italic;
    return F;
  end Font;

  ----------------------------------------------------------------------------
  --
  --  Name: get the name of a font's typeface.
  --
  function Name (Font : Font_Type) return String is
  begin
    return Font.Name;
  end Name;

  ----------------------------------------------------------------------------
  --
  --  Size: get the size of a font in points.
  --
  function Size (Font : Font_Type) return Natural is
  begin
    return Font.Size;
  end Size;

  ----------------------------------------------------------------------------
  --
  --  Bold: True is the specified font is bold.
  --
  function Bold (Font : Font_Type) return Boolean is
  begin
    return Font.Bold;
  end Bold;

  ----------------------------------------------------------------------------
  --
  --  Italic: True is the specified font is italic.
  --
  function Italic (Font : Font_Type) return Boolean is
  begin
    return Font.Italic;
  end Italic;

  ----------------------------------------------------------------------------
  --
  --  Endpoint: calculate the endpoint of a line drawn from a specified origin
  --            for a given length at a given angle.
  --
  function Endpoint (From   : Point_Type;
                     Length : Positive;
                     Angle  : Angle_Type) return Point_Type is
  begin
    return (From.X + Integer(Float(Length)*Sin(Float(Angle),360.0)),
            From.Y - Integer(Float(Length)*Cos(Float(Angle),360.0)));
  end Endpoint;

  ----------------------------------------------------------------------------
  --
  --  Inside: test if Point is inside the rectangle defined by From and To,
  --          bearing in mind that any two opposite corners can be given
  --          (not necessarily top left and bottom right).
  --
  function Inside (Point : Point_Type;
                   From  : Point_Type;
                   To    : Point_Type) return Boolean is
  begin
    return Point.X >= Integer'Min(From.X,To.X) and
           Point.X <= Integer'Max(From.X,To.X) and
           Point.Y >= Integer'Min(From.Y,To.Y) and
           Point.Y <= Integer'Max(From.Y,To.Y);
  end Inside;

  ----------------------------------------------------------------------------
  --
  --  "+": add two points (P1.X + P2.X, P1.Y + P2.Y).
  --
  function "+" (P1, P2 : Point_Type) return Point_Type is
  begin
    return (X => P1.X+P2.X, Y => P1.Y+P2.Y);
  end "+";

  ----------------------------------------------------------------------------
  --
  --  "-": subtract two points (P1.X - P2.X, P1.Y - P2.Y).
  --
  function "-" (P1, P2 : Point_Type) return Point_Type is
  begin
    return (X => P1.X-P2.X, Y => P1.Y-P2.Y);
  end "-";

  ----------------------------------------------------------------------------
  --
  --                I N T E R N A L   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Free: deallocate a reference-counted object.
  --
  procedure Free is new Ada.Unchecked_Deallocation
                                (Reference_Counted_Type'Class,
                                 Reference_Counted_Ptr);

  ----------------------------------------------------------------------------
  --
  --  Cleanup: the finalisation primitive for reference-counted types.
  --           Override this for derived types to do something useful.
  --
  procedure Cleanup (Object : in out Reference_Counted_Type) is
  begin
    null;
  end Cleanup;

  ----------------------------------------------------------------------------
  --
  --  Finalize: decrement the reference count when an object containing
  --            a pointer to a reference-counted object is destroyed.
  --            When the reference count reaches zero, finalise the
  --            reference-counted object and free its memory.
  --
  procedure Finalize (Object : in out Controlled_Type) is
  begin
    if Object.Pointer /= null then
      if Object.Pointer.Count > 0 then
        Object.Pointer.Count := Object.Pointer.Count - 1;
        if Object.Pointer.Count = 0 then
          Cleanup (Object.Pointer.all);
          Free (Object.Pointer);
        end if;
      end if;
    end if;
  end Finalize;

  ----------------------------------------------------------------------------
  --  Adjust: bump the reference count when copying an object containing a
  --          pointer to a reference-counted object. Do nothing if the
  --          pointer is null.
  --
  procedure Adjust (Object : in out Controlled_Type) is
  begin
    if Object.Pointer /= null then
      Object.Pointer.Count := Object.Pointer.Count + 1;
    end if;
  end Adjust;

end JEWL;
