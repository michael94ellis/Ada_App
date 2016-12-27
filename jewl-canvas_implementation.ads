------------------------------------------------------------------------------
--                                                                          --
--             J E W L . C A N V A S _ I M P L E M E N T A T I O N          --
--                                                                          --
--   This is a private package containing the internal implementation       --
--   details of the canvases, as defined in JEWL.Windows.                   --
--                                                                          --
--   Copyright (C) John English 2000. Contact address: je@brighton.ac.uk    --
--   This software is released under the terms of the GNU General Public    --
--   License and is intended primarily for educational use. Please contact  --
--   the author to report bugs, suggestions and modifications.              --
--                                                                          --
------------------------------------------------------------------------------
-- $Id: jewl-canvas_implementation.ads 1.7 2007/01/08 17:00:00 JE Exp $
------------------------------------------------------------------------------
--
-- $Log: jewl-canvas_implementation.ads $
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

with Ada.Finalization;
with JEWL.Win32_Interface;

private package JEWL.Canvas_Implementation is

  use JEWL.Win32_Interface;

  type Point_List is array (Positive range <>) of aliased Win32_POINT;

  ----------------------------------------------------------------------------
  --  Forward declarations (defined fully later).
  ----------------------------------------------------------------------------

  type Canvas_Object_Type;
  type Canvas_Object_Ptr is access Canvas_Object_Type'Class;

  type Image_Internals;
  type Image_Ptr is access all Image_Internals;

  ----------------------------------------------------------------------------
  --
  --                     C A N V A S _ M O N I T O R
  --
  --  Every canvas contains one of these protected records to synchronise
  --  drawing operations with accesses from the message loop task. The
  --  canvas stores the objects which are drawn on it as a linked list
  --  of drawing objects (defined in JEWL.Objects).
  --
  --  The private data of a Canvas_Monitor is as follows:
  --
  --  First_Object : pointer to the first object in the drawing list
  --  Last_Object  : pointer to the last object in the drawing list
  --  Save_Pointer : pointer to the saved position in the drawing list
  --  BG_Brush     : the brush used to paint the background of the canvas
  --  Start_X      : the X position where the mouse button was pressed
  --  Start_Y      : the Y position where the mouse button was pressed
  --  End_X        : the most recent X position while the mouse is down
  --  End_Y        : the most recent Y position while the mouse is down
  --  Button       : true if the mouse is down
  --  Moved        : true is the mouse has moved while the button is down
  --  Keycode      : the key that was pressed (or NUL if none)
  --
  --  The operations on a Canvas_Monitor are as follows:
  --
  --  Clear        : delete all objects in the drawing list
  --  Save         ; save a pointer to the current end of the drawing list
  --  Restore      : truncate the drawing list to a previously saved point
  --  Draw         : draw all objects in the drawing list (called from the
  --                 message loop task)
  --  Add          : add an object to the end of the drawing list
  --  Set_Brush    : set the background brush
  --  Background   : get the background brush (called from the message loop
  --                 task)
  --  Set_Start    : set the start position when the mouse button is pressed
  --                 (called from the message loop task)
  --  Get_Start    : get the start position when the mouse button is pressed
  --  Set_End      : set the current mouse position and the mouse-move flag
  --                 (called from the message loop task)
  --  Get_End      : get the current mouse position
  --  Set_Button   : set the mouse button state (called from the message loop
  --                 task)
  --  Mouse_Down   : return the mouse button state
  --  Mouse_Moved  : return the mouse-move flag and reset it
  --  Set_Key      : set the keycode for a key press
  --  Get_Key      : get and reset the current keycode
  --
  ----------------------------------------------------------------------------

  protected type Canvas_Monitor is
    procedure Clear;
    procedure Save;
    procedure Restore;
    procedure Draw       (Handle : in Win32_HWND;
                          Font   : in Win32_HFONT);
    procedure Add        (Object : in Canvas_Object_Ptr);
    procedure Set_Brush  (Brush : in Win32_HBRUSH);
    function  Background  return Win32_HBRUSH;
    procedure Set_Start  (X, Y : in  Integer);
    procedure Get_Start  (X, Y : out Integer);
    procedure Set_End    (X, Y : in  Integer);
    procedure Get_End    (X, Y : out Integer);
    procedure Set_Button (B : in  Boolean);
    function  Mouse_Down  return Boolean;
    function  Mouse_Moved return Boolean;
    procedure Set_Key    (C : in  Character);
    procedure Get_Key    (C : out Character);
  private
    First_Object : Canvas_Object_Ptr;
    Last_Object  : Canvas_Object_Ptr;
    Save_Pointer : Canvas_Object_Ptr;
    BG_Brush     : Win32_HBRUSH;
    Start_X      : Integer   := 0;
    Start_Y      : Integer   := 0;
    End_X        : Integer   := 0;
    End_Y        : Integer   := 0;
    Button       : Boolean   := False;
    Moved        : Boolean   := False;
    Keycode      : Character := ASCII.NUL;
  end Canvas_Monitor;

  ----------------------------------------------------------------------------
  --
  --                 C A N V A S _ O B J E C T _ T Y P E
  --
  --  Canvas_Object_Type is the base type from which all drawing objects
  --  are derived, and Canvas_Object_Ptr is a class-wide pointer. Each
  --  object contains a pointer so that a singly-linked list of objects
  --  can be constructed. Every object must define a Draw procedure which
  --  draws the object on a window (which is identified by a handle to a
  --  Windows display context).
  --
  ----------------------------------------------------------------------------

  type Canvas_Object_Type is abstract tagged
    record
      Next : Canvas_Object_Ptr;
    end record;

  procedure Draw (Object : in out Canvas_Object_Type;
                  Window : in Win32_HDC) is abstract;

  ----------------------------------------------------------------------------
  --
  --  Text_Type: an object type containing a text string
  --
  type Text_Type (Length : Natural) is new Canvas_Object_Type with
    record
      Text     : String (1..Length);
      From, To : JEWL.Point_Type;
      Align    : Integer;
    end record;

  procedure Draw (Object : in out Text_Type;
                  Window : in Win32_HDC);

  ----------------------------------------------------------------------------
  --
  --  Line_Type: an object type which defines a line by a pair of endpoints.
  --
  type Line_Type is new Canvas_Object_Type with
    record
      From, To : JEWL.Point_Type;
    end record;

  procedure Draw (Object : in out Line_Type;
                  Window : in Win32_HDC);

  ----------------------------------------------------------------------------
  --
  --  Rectangle_Type: an object type which defines a rectangle as a pair
  --                  of coordinates for two opposite corners.
  --
  type Rectangle_Type is new Canvas_Object_Type with
    record
      From, To : JEWL.Point_Type;
    end record;

  procedure Draw (Object : in out Rectangle_Type;
                  Window : in Win32_HDC);

  ----------------------------------------------------------------------------
  --
  -- Rounded_Rectangle_Type: an object type which defines a rectangle with
  --                         rounded corners using a pair of coordinates
  --                         for two opposite corners and a point to define
  --                         the size of the arc used to round the corners.
  --
  type Rounded_Rectangle_Type is new Canvas_Object_Type with
    record
      From, To, Corner : Point_Type;
    end record;

  procedure Draw (Object : in out Rounded_Rectangle_Type;
                  Window : in Win32_HDC);

  ----------------------------------------------------------------------------
  --
  -- Ellipse_Type: an object type which defines an ellipse or circle using
  --               a pair of coordinates for two opposite corners of the
  --               bounding rectangle.
  --
  type Ellipse_Type is new Canvas_Object_Type with
    record
      From, To : JEWL.Point_Type;
    end record;

  procedure Draw (Object : in out Ellipse_Type;
                  Window : in Win32_HDC);

  ----------------------------------------------------------------------------
  --
  -- Polyline_Type: an object type which defines an open figure as a
  --                sequence of lines.
  --
  type Polyline_Type (Count : Positive) is new Canvas_Object_Type with
    record
      Points : Point_List(1..Count);
    end record;

  procedure Draw (Object : in out PolyLine_Type;
                  Window : in Win32_HDC);

  ----------------------------------------------------------------------------
  --
  -- Polygon_Type: an object type which defines an open figure as a
  --               sequence of lines.
  --
  type Polygon_Type (Count : Positive) is new Canvas_Object_Type with
    record
      Points : Point_List(1..Count);
    end record;

  procedure Draw (Object : in out Polygon_Type;
                  Window : in Win32_HDC);

  ----------------------------------------------------------------------------
  --
  -- Handle_Type: an object type which encapsulates a handle to a Windows
  --              GDI object (e.g. a pen or a brush).
  --
  type Handle_Type is new Canvas_Object_Type with
    record
      Handle : JEWL.Controlled_Type;
    end record;

  procedure Draw (Object : in out Handle_Type;
                  Window : in Win32_HDC);

  ----------------------------------------------------------------------------
  --
  -- Bitmap_Type: an object type representing a bitmap image.
  --
  type Bitmap_Type is new Canvas_Object_Type with
    record
      From   : JEWL.Point_Type;
      Width  : Natural;
      Height : Natural;
      Bitmap : JEWL.Controlled_Type;
    end record;

  procedure Draw (Object : in out Bitmap_Type;
                  Window : in Win32_HDC);

  ----------------------------------------------------------------------------
  --
  --                   C O N T R O L L E D   T Y P E S
  --
  --  Each type below is a reference counted type which contains a handle to
  --  a Windows GDI object. The handle will be deleted automatically when the
  --  last object which refers to it is destroyed.
  --
  ----------------------------------------------------------------------------
  --
  --  Image_Internals: used to store bitmaps in drawings
  --
  type Image_Internals is new Reference_Counted_Type with
    record
      Image  : Win32_HBITMAP;
      Width  : Natural;
      Height : Natural;
    end record;
    
  procedure Cleanup (Object : in out Image_Internals);

  ----------------------------------------------------------------------------
  --
  --  Counted_Handle_Type: used to store pens, brushes etc. in drawings
  --
  type Counted_Handle_Type is new JEWL.Reference_Counted_Type with
    record
      Handle : Win32_HGDIOBJ;
    end record;

  procedure Cleanup (Object : in out Counted_Handle_Type);

  ----------------------------------------------------------------------------
  --
  --  Handle: a function to create a controlled type object which contains
  --          a Counted_Handle_Type object.
  --
  function  Handle (Object : Win32_HGDIOBJ) return JEWL.Controlled_Type;

end JEWL.Canvas_Implementation;
