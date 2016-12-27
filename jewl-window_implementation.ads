------------------------------------------------------------------------------
--                                                                          --
--           J E W L . W I N D O W _ I M P L E M E N T A T I O N            --
--                                                                          --
--   This is a private package containing implementation details for        --
--   JEWL.Windows. Because this package is non-generic, the type            --
--   Window_Internals can be defined here at library level by deriving      --
--   from JEWL.Reference_Counted_Type, thus avoiding scope problems         --
--   arising from the use of a controlled type in a generic package         --
--   (which would otherwise have to be instantiated at library level).      --
--   Besides, JEWL.Windows is far too big anyway...                         --
--                                                                          --
--   Copyright (C) John English 2000. Contact address: je@brighton.ac.uk    --
--   This software is released under the terms of the GNU General Public    --
--   License and is intended primarily for educational use. Please contact  --
--   the author to report bugs, suggestions and modifications.              --
--                                                                          --
------------------------------------------------------------------------------
-- $Id: jewl-window_implementation.ads 1.7 2007/01/08 17:00:00 JE Exp $
------------------------------------------------------------------------------
--
-- $Log: jewl-window_implementation.ads $
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

with JEWL.Canvas_Implementation;  use JEWL.Canvas_Implementation;
with JEWL.Win32_Interface;        use JEWL.Win32_Interface;
with System;

private package JEWL.Window_Implementation is

  type Window_Internals;
  type Window_Ptr is access all Window_Internals'Class;

  type Container_Internals;
  type Container_Ptr is access all Container_Internals;

  ----------------------------------------------------------------------------
  --
  --                   W I N D O W _ I N T E R N A L S
  --
  --  Window_Internals (or a type derived from Window_Internals) is what
  --  the Controlled_Type object in every Window_Type object actually
  --  points to. It contains the following fields:
  --
  --  Handle  : the native Windows handle for the window
  --  Parent  : the parent window (null for top-level windows)
  --  Next    : link to the next sibling of this window
  --  First   : link to the first child window of this window
  --  Last    : link to the last child window
  --  Action  : the command code associated with window (-1 if none)
  --  Font    : font handle for the window's font
  --  Top     : position of top of window (negative if relative to parent)
  --  Left    : position of top of window (negative if relative to parent)
  --  Height  : position of top of window (non-positive if relative to parent)
  --  Width   : position of top of window (non-positive if relative to parent)
  --  WndProc : old window procedure for subclassed windows
  --
  --  The Cleanup procedure is called automatically when a Window_Internals
  --  object is deleted and should not be called directly.
  --
  ----------------------------------------------------------------------------

  type Window_Internals is new Reference_Counted_Type with
    record
      Handle      : Win32_HWND := System.Null_Address;
      Parent      : Container_Ptr;
      Next        : Controlled_Type;
      First       : Controlled_Type;
      Last        : Window_Ptr;
      Action      : Integer := -1;
      Font        : Win32_HFONT := System.Null_Address;
      Top         : Integer := 0;
      Left        : Integer := 0;
      Height      : Integer := 0;
      Width       : Integer := 0;
      WndProc     : Win32_LONG := 0;
    end record;

  procedure Cleanup (Object : in out Window_Internals);

  ----------------------------------------------------------------------------
  --
  --                C O N T A I N E R _ I N T E R N A L S
  --
  --  This is a type derived from Window_Internals for use by container
  --  windows. It includes the following additional component:
  --
  --  Group : a flag to determine whether the WS_GROUP style should be
  --          applied to child controls (used to ensure that radiobutton
  --          groups are correctly delimited).
  --
  ----------------------------------------------------------------------------

  type Container_Internals is new Window_Internals with
    record
      Group : Boolean := True;
    end record;

  ----------------------------------------------------------------------------
  --
  --              M A I N _ W I N D O W _ I N T E R N A L S
  --
  --  This is a type derived from Container_Internals for use by top-level
  --  windows. It includes the following additional component:
  --
  --  Focus : the handle of the child window to activate when the top-level
  --          window is activated (if any).
  --
  ----------------------------------------------------------------------------

  type Main_Window_Internals is new Container_Internals with
    record
      Focus : Win32_HWND := System.Null_Address;
    end record;

  type Main_Window_Ptr is access all Main_Window_Internals;

  procedure Cleanup (Object : in out Main_Window_Internals);

  ----------------------------------------------------------------------------
  --
  --                   C A N V A S _ I N T E R N A L S
  --
  --  This is a type derived from Window_Internals for use by canvas
  --  window. It includes the following additional component:
  --
  --  Monitor : a protected record (defined in Canvas_Implementation)
  --            to record the drawing operations and mouse state and
  --            to synchronise accesses from the message loop task.
  --
  ----------------------------------------------------------------------------

  type Canvas_Internals is new Window_Internals with
    record
      Monitor  : Canvas_Monitor;
      Keypress : Integer := -1;
    end record;

  procedure Cleanup (Object : in out Canvas_Internals);

  type Canvas_Ptr is access all Canvas_Internals;

  ----------------------------------------------------------------------------
  --
  --            C O M M O N _ D I A L O G _ I N T E R N A L S
  --
  --  Common_Dialog_Internals (or a type derived from it) is what the
  --  Controlled_Type object in every Common_Dialog_Type object actually
  --  points to.
  --
  ----------------------------------------------------------------------------

  type Common_Dialog_Internals is
                     abstract new Reference_Counted_Type with null record;
  type Common_Dialog_Ptr is access all Common_Dialog_Internals'Class;
  function Show_Dialog (Dialog : access Common_Dialog_Internals)
                                               return Boolean is abstract;

  ----------------------------------------------------------------------------
  --
  --            C O L O U R _ D I A L O G _ I N T E R N A L S
  --
  ----------------------------------------------------------------------------

  type Custom_Colours is array (1..16) of aliased Win32_COLORREF;

  type Colour_Dialog_Internals is new Common_Dialog_Internals with
    record
      Struct : aliased Win32_CHOOSECOLOR;
      Colour : Colour_Type;
      Custom : Custom_Colours;
    end record;

  type Colour_Dialog_Ptr is access all Colour_Dialog_Internals'Class;

  function Show_Dialog (Dialog : access Colour_Dialog_Internals)
                                                      return Boolean;

  ----------------------------------------------------------------------------
  --
  --              F O N T _ D I A L O G _ I N T E R N A L S
  --
  ----------------------------------------------------------------------------

  type Font_Dialog_Internals is new Common_Dialog_Internals with
    record
      Struct : aliased Win32_CHOOSEFONT;
      Font   : aliased Win32_LOGFONT;
    end record;

  type Font_Dialog_Ptr is access all Font_Dialog_Internals'Class;

  function Show_Dialog (Dialog : access Font_Dialog_Internals)
                                                    return Boolean;

  ----------------------------------------------------------------------------
  --
  --              F I L E _ D I A L O G _ I N T E R N A L S
  --
  ----------------------------------------------------------------------------

  type File_Dialog_Internals (N : Win32_SIZE) is
                                  abstract new Common_Dialog_Internals with
    record
      Struct    : aliased Win32_OPENFILENAME;
      Title     : Win32_String (1..N);
      Buffer    : Win32_String (1..300);
      Directory : aliased Win32_String (1..300);
      Filter    : aliased Win32_String (1..300);
      Length    : Win32_SIZE := 0;
    end record;

  type File_Dialog_Ptr is access all File_Dialog_Internals'Class;
  
  ----------------------------------------------------------------------------
  --
  --              O P E N _ D I A L O G _ I N T E R N A L S
  --
  ----------------------------------------------------------------------------

  type Open_Dialog_Internals is new File_Dialog_Internals with null record;

  function Show_Dialog (Dialog : access Open_Dialog_Internals)
                                                    return Boolean;

  ----------------------------------------------------------------------------
  --
  --              S A V E _ D I A L O G _ I N T E R N A L S
  --
  ----------------------------------------------------------------------------

  type Save_Dialog_Internals is new File_Dialog_Internals with null record;

  function Show_Dialog (Dialog : access Save_Dialog_Internals)
                                                    return Boolean;

end JEWL.Window_Implementation;
