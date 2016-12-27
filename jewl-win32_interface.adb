------------------------------------------------------------------------------
--                                                                          --
--                 J E W L . W I N 3 2 _ I N T E R F A C E                  --
--                                                                          --
--   This is the body of a private package containing implementation        --
--   details for JEWL.Windows. It contains type conversions where a         --
--   bit-for-bit conversion is inadequate. It also contains some            --
--   functions which provide a more tasteful Ada wrapping for a few         --
--   common sequences of Win32 magic spells.                                --
--                                                                          --
--   Copyright (C) John English 2000. Contact address: je@brighton.ac.uk    --
--   This software is released under the terms of the GNU General Public    --
--   License and is intended primarily for educational use. Please contact  --
--   the author to report bugs, suggestions and modifications.              --
--                                                                          --
------------------------------------------------------------------------------
-- $Id: jewl-win32_interface.adb 1.7 2007/01/08 17:00:00 JE Exp $
------------------------------------------------------------------------------
--
-- $Log: jewl-win32_interface.adb $
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

with Interfaces.C;

package body JEWL.Win32_Interface is

  pragma Linker_Options ("-luser32");
  pragma Linker_Options ("-lgdi32");
  pragma Linker_Options ("-lcomdlg32");
  pragma Linker_Options ("-lwinmm");

  use type Win32_DWORD, Win32_INT, Win32_UINT, Win32_LONG, Win32_BYTE;
  use type System.Address;

  ----------------------------------------------------------------------------
  --
  --                   T Y P E   C O N V E R S I O N S
  --
  ----------------------------------------------------------------------------

  function To_LPSTR (S : Win32_String) return Win32_LPSTR is
    function UC is new Ada.Unchecked_Conversion
                            (System.Address, Win32_LPSTR);
  begin
    return UC(S(S'First)'Address);
  end To_LPSTR;

  function To_LPCSTR (S : Win32_String) return Win32_LPCSTR is
    function UC is new Ada.Unchecked_Conversion
                            (System.Address, Win32_LPCSTR);
  begin
    return UC(S(S'First)'Address);
  end To_LPCSTR;

  function To_LPARAM (S : Win32_String) return Win32_LPARAM is
    function UC is new Ada.Unchecked_Conversion
                            (System.Address, Win32_LPARAM);
  begin
    return UC(S(S'First)'Address);
  end To_LPARAM;

  function To_String (S : Win32_String) return String is
  begin
    return Interfaces.C.To_Ada(S);
  end To_String;

  function To_Array (S : String) return Win32_String is
  begin
    return Interfaces.C.To_C(S);
  end To_Array;

  function RGB (Colour : Colour_Type) return Win32_COLORREF is
    use type Win32_BYTE;
  begin
    return Win32_COLORREF(Colour.Red + Colour.Green*2**8 + Colour.Blue*2**16);
  end RGB;
  
  function MakePoint (Value: Win32_LPARAM) return Win32_POINTS is
    use type Interfaces.Unsigned_32;
    function UC is new Ada.Unchecked_Conversion
                           (Win32_LPARAM,Interfaces.Unsigned_32);
    function UC is new Ada.Unchecked_Conversion
                           (Win32_WORD,Win32_SHORT);
    P : Win32_POINTS;
    V : Interfaces.Unsigned_32 := UC(Value);
  begin
    P.X := UC(Win32_WORD(V and 16#FFFF#));
    P.Y := UC(Win32_WORD(Interfaces.Shift_Right(V,16) and 16#FFFF#));
    return P;
  end MakePoint;
  
  ----------------------------------------------------------------------------
  --
  --                  U T I L I T Y   F U N C T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Message_Box: an Ada wrapper for the Win32 MessageBox function.
  --
  function Message_Box (Message : String;
                        Title   : String;
                        Flags   : Win32_UINT) return Integer is
    M : Win32_String := To_Array(Message);
    T : Win32_String := To_Array(Title);
  begin
    return Integer(MessageBox(System.Null_Address,
                              To_LPCSTR(M), To_LPCSTR(T),
                              Flags or MB_TASKMODAL or MB_SETFOREGROUND));
  end Message_Box;

  ----------------------------------------------------------------------------
  --
  --  Create_Font: an Ada wrapper for the Win32 CreateFont function.
  --
  function Create_Font (Font : Font_Type) return Win32_HFONT is
    F : Win32_HFONT;
    L : aliased Win32_LOGFONT := Set_Font(Font);
  begin
    F := CreateFontIndirect (L'Unchecked_Access);
    return F;
  end Create_Font;

  ----------------------------------------------------------------------------
  --
  --  Set_Font: convert a Font_Type object to a Win32 font.
  --
  function Set_Font (Font : Font_Type) return Win32_LOGFONT is
    F : Win32_LOGFONT;
    H : Win32_HDC;
    I : Win32_INT;
  begin
    H := GetDC(System.Null_Address);
    F.lfHeight := -Win32_LONG(GetDeviceCaps(H,LOGPIXELSY) *
                              Win32_INT(Font.Size) / 72);
    I := ReleaseDC(System.Null_Address,H);
    if Font.Bold then
      F.lfWeight := 700;
    else
      F.lfWeight := 400;
    end if;
    F.lfItalic := Boolean'Pos(Font.Italic);
    if Font.Name'Length < F.lfFaceName'Length then
      F.lfFaceName(0..Font.Name'Length) := To_Array(Font.Name);
    else
      F.lfFaceName := To_Array(Font.Name(1..F.lfFaceName'Length-1));
    end if;
    return F;
  end Set_Font;

  ----------------------------------------------------------------------------
  --
  --  Get_Font: convert a Win32 font to a Font_Type object.
  --
  function Get_Font (Font : Win32_LOGFONT) return Font_Type is
    H : Win32_HDC;
    I : Win32_INT;
    S : Float;
  begin
    H := GetDC(System.Null_Address);
    S := 72.0 / Float(GetDeviceCaps(H,LOGPIXELSY));
    I := ReleaseDC (System.Null_Address, H);
    return JEWL.Font(To_String(Font.lfFaceName),
                     abs Integer(Float(Font.lfHeight)*S),
                     Font.lfWeight > 500, Font.lfItalic /= 0);
  end Get_Font;

end JEWL.Win32_Interface;
