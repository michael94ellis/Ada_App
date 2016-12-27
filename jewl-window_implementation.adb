------------------------------------------------------------------------------
--                                                                          --
--           J E W L . W I N D O W _ I M P L E M E N T A T I O N            --
--                                                                          --
--   This is the body of a private package containing implementation        --
--   details for JEWL.Windows. It defines the cleanup procedures which      --
--   are called automatically to destroy a window's Window_Internals        --
--   structure when the last reference to the window is deleted.            --
--                                                                          --
--   Copyright (C) John English 2000. Contact address: je@brighton.ac.uk    --
--   This software is released under the terms of the GNU General Public    --
--   License and is intended primarily for educational use. Please contact  --
--   the author to report bugs, suggestions and modifications.              --
--                                                                          --
------------------------------------------------------------------------------
-- $Id: jewl-window_implementation.adb 1.7 2007/01/08 17:00:00 JE Exp $
------------------------------------------------------------------------------
--
-- $Log: jewl-window_implementation.adb $
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

with JEWL.Message_Handling;

package body JEWL.Window_Implementation is

  use JEWL.Canvas_Implementation;
  use JEWL.Message_Handling;
  use JEWL.Win32_Interface;

  use type System.Address;
  use type Win32_BOOL, Win32_DWORD;

  ----------------------------------------------------------------------------
  --
  --  Cleanup: destructor for Window_Internals.
  --
  --  This destroys the font handle associated with the window.
  --
  ----------------------------------------------------------------------------

  procedure Cleanup (Object : in out Window_Internals) is
  begin
    if Object.Font /= System.Null_Address then
      Bool_Dummy := DeleteObject (Object.Font);
    end if;
  end Cleanup;

  ----------------------------------------------------------------------------
  --
  --  Cleanup: destructor for Main_Window_Internals.
  --
  --  This asks the message loop to destroy the window if it still exists to
  --  ensure that the count of top-level windows is decremented properly.
  --
  ----------------------------------------------------------------------------

  procedure Cleanup (Object : in out Main_Window_Internals) is
  begin
    if IsWindow(Object.Handle) /= 0 then
      Message_Loop.Destroy_Window (Object.Handle);
    end if;
    Cleanup (Window_Internals(Object));
  end Cleanup;

  ----------------------------------------------------------------------------
  --
  --  Cleanup: destructor for Canvas_Internals.
  --
  --  This deletes the drawing list associated with the canvas.
  --
  ----------------------------------------------------------------------------

  procedure Cleanup (Object : in out Canvas_Internals) is
  begin
    if IsWindow(Object.Handle) /= 0 then
      Object.Monitor.Clear;
    end if;
    Cleanup (Window_Internals(Object));
  end Cleanup;

  ----------------------------------------------------------------------------
  --
  --           C O M M O N   D I A L O G   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Show_Dialog: show a colour dialog.
  --
  function Show_Dialog (Dialog : access Colour_Dialog_Internals)
                                                      return Boolean is
    B : Win32_BOOL;
  begin
    Dialog.Struct.rgbResult := RGB (Dialog.Colour);
    Dialog.Struct.hwndOwner := GetActiveWindow;
    B := ChooseColor (Dialog.Struct'Access);
    if B /= 0 then
      Dialog.Colour.Red   := Integer(Dialog.Struct.rgbResult mod 256);
      Dialog.Colour.Green := Integer((Dialog.Struct.rgbResult/2**8) mod 256);
      Dialog.Colour.Blue  := Integer((Dialog.Struct.rgbResult/2**16) mod 256);
    end if;
    return B /= 0;
  end Show_Dialog;

  ----------------------------------------------------------------------------
  --
  --  Show_Dialog: show a font dialog.
  --
  function Show_Dialog (Dialog : access Font_Dialog_Internals)
                                                    return Boolean is
  begin
    Dialog.Struct.hwndOwner := GetActiveWindow;
    return ChooseFont (Dialog.Struct'Access) /= 0;
  end Show_Dialog;

  ----------------------------------------------------------------------------
  --
  --  Show_Dialog: show an open file dialog.
  --
  function Show_Dialog (Dialog : access Open_Dialog_Internals)
                                                    return Boolean is
  begin
    Dialog.Struct.hwndOwner := GetActiveWindow;
    return GetOpenFileName(Dialog.Struct'Access) /= 0;
  end Show_Dialog;

  ----------------------------------------------------------------------------
  --
  --  Show_Dialog: show a save file dialog.
  --
  function Show_Dialog (Dialog : access Save_Dialog_Internals)
                                                    return Boolean is
  begin
    Dialog.Struct.hwndOwner := GetActiveWindow;
    return GetSaveFileName(Dialog.Struct'Access) /= 0;
  end Show_Dialog;

end JEWL.Window_Implementation;
