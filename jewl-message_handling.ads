------------------------------------------------------------------------------
--                                                                          --
--                J E W L . M E S S A G E _ H A N D L I N G                 --
--                                                                          --
--   This is a private package which defines the message-handling task      --
--   required by JEWL.Windows, the protected record used to communicate     --
--   with it, and related operations.                                       --
--                                                                          --
--   Copyright (C) John English 2000. Contact address: je@brighton.ac.uk    --
--   This software is released under the terms of the GNU General Public    --
--   License and is intended primarily for educational use. Please contact  --
--   the author to report bugs, suggestions and modifications.              --
--                                                                          --
------------------------------------------------------------------------------
-- $Id: jewl-message_handling.ads 1.7 2007/01/08 17:00:00 JE Exp $
------------------------------------------------------------------------------
--
-- $Log: jewl-message_handling.ads $
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

with JEWL.Win32_Interface;       use JEWL.Win32_Interface;
with JEWL.Window_Implementation; use JEWL.Window_Implementation;

with Ada.Exceptions;
with System;

private package JEWL.Message_Handling is

  ----------------------------------------------------------------------------
  --
  --                       M E S S A G E _ L O O P
  --
  --  This task is responsible for pumping the Windows message loop.
  --  Windows requires that all windows are created and destroyed by
  --  the same task that will handle their messages, so task entries
  --  are used to ask the message task to create and destroy windows.
  --  The entries are:
  --
  --  Create_Window  : create a top-level window, given the name of the
  --                   window class, the window title, extended and normal
  --                   window styles, and whether the window should be
  --                   made visible.
  --  Create_Child   : create a child window, given the parent window's
  --                   handle, the name of the window class, the window
  --                   title, style, and coordinates.
  --  Show_Dialog    : show a common dialog and return its result.
  --  Destroy_Window : destroy a window (specified by its handle).
  --
  ----------------------------------------------------------------------------

  task Message_Loop is
    entry Create_Child   (Window : in Window_Ptr;
                          Parent : in Container_Ptr;
                          Class  : in Win32_String;
                          Title  : in Win32_String;
                          XStyle : in Win32_DWORD;
                          Style  : in Win32_DWORD;
                          Top    : in Integer;
                          Left   : in Integer;
                          Width  : in Integer;
                          Height : in Integer);
    entry Create_Window  (Window : in Main_Window_Ptr;
                          Class  : in Win32_String;
                          Title  : in Win32_String;
                          XStyle : in Win32_DWORD;
                          Style  : in Win32_DWORD;
                          Show   : in Boolean);
    entry Show_Dialog    (Dialog : in Common_Dialog_Ptr;
                          Result : out Boolean);
    entry Set_Focus      (Window : in Win32_HWND);
    entry Destroy_Window (Handle : in Win32_HWND);
  end Message_Loop;

  ----------------------------------------------------------------------------
  --
  --                        W I N D O W _ I N F O
  --
  --  This protected record is used for global communication between the
  --  message loop task and the main task. The private data this contains
  --  is as follows:
  --
  --  Command       : the current command code (0 if no command available)
  --  Dialog        : the current active dialog (null if no dialog active)
  --  Task_Failed   : True if the message loop task has failed
  --  Failure_Info  : the exception that caused the message loop to fail
  --
  --  The operations provide are as follows:
  --
  --  Get_Command   : wait for a command and then return its code
  --  Test_Command  : test if there is a command pending
  --  Set_Command   : set the current command
  --  Get_Dialog    : get the handle of the current dialog (null if none)
  --  Active_Window : set the handle of the active dialog window and get
  --                  the old value of the handle
  --  Record_Error  : record an exception that caused the message loop
  --                  to fail
  --
  ----------------------------------------------------------------------------

  protected Window_Info is
    entry     Get_Command   (Cmd : out Natural);
    function  Test_Command  return Boolean;
    procedure Set_Command   (Cmd : in  Natural);
    procedure Get_Dialog    (Dlg : in out Win32_HWND);
    function  Active_Window return Win32_HWND;
    procedure Record_Error  (Err : in Ada.Exceptions.Exception_Occurrence);
  private
    Command      : Natural := 0;
    Dialog       : Win32_HWND := System.Null_Address;
    Task_Failed  : Boolean := False;
    Failure_Info : Ada.Exceptions.Exception_Occurrence;
  end Window_Info;

  ----------------------------------------------------------------------------
  --
  --                   M E S S A G E   H A N D L E R S
  --
  --  These functions are associated with the window classes for frames,
  --  dialogs and canvases when the window classes are registered (in the
  --  initialisation section at the end of the body of JEWL.Windows).
  --  Windows will call the appropriate function when a message is sent
  --  to a window belonging to the corresponding class, specifying the
  --  window handle, message code and any additional parameters.
  --
  ----------------------------------------------------------------------------

  function Frame_Proc  (hwnd   : Win32_HWND;
                        msg    : Win32_UINT;
                        wParam : Win32_WPARAM;
                        lParam : Win32_LPARAM) return Win32_LONG;
                      pragma Convention(StdCall, Frame_Proc);

  function Dialog_Proc (hwnd   : Win32_HWND;
                        msg    : Win32_UINT;
                        wParam : Win32_WPARAM;
                        lParam : Win32_LPARAM) return Win32_LONG;
                      pragma Convention(StdCall, Dialog_Proc);

  function Canvas_Proc (hwnd   : Win32_HWND;
                        msg    : Win32_UINT;
                        wParam : Win32_WPARAM;
                        lParam : Win32_LPARAM) return Win32_LONG;
                      pragma Convention(StdCall, Canvas_Proc);

  function Panel_Proc  (hwnd   : Win32_HWND;
                        msg    : Win32_UINT;
                        wParam : Win32_WPARAM;
                        lParam : Win32_LPARAM) return Win32_LONG;
                      pragma Convention(StdCall, Panel_Proc);

  function Memo_Proc   (hwnd   : Win32_HWND;
                        msg    : Win32_UINT;
                        wParam : Win32_WPARAM;
                        lParam : Win32_LPARAM) return Win32_LONG;
                      pragma Convention(StdCall, Memo_Proc);

  ----------------------------------------------------------------------------
  --
  --                 U T I L I T Y   P R O C E D U R E S
  --
  --  Get_Actual_Bounds : test if a set of window dimensions is relative to
  --                      the dimensions of the parent. If they are, set the
  --                      Resize parameter True and set the dimensions to
  --                      the corresponding absolute (parent-based) values.
  --
  ----------------------------------------------------------------------------

  procedure Get_Actual_Bounds (Parent  : in Win32_HWND;
                               Top     : in out Integer;
                               Left    : in out Integer;
                               Width   : in out Integer;
                               Height  : in out Integer;
                               Resize  : out Boolean);

end JEWL.Message_Handling;
