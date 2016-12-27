------------------------------------------------------------------------------
--                                                                          --
--             J E W L . C A N V A S _ I M P L E M E N T A T I O N          --
--                                                                          --
--   This is the body of a private package containing the internal          --
--   implementation details of canvases, as defined in JEWL.Windows.        --
--                                                                          --
--   Copyright (C) John English 2000. Contact address: je@brighton.ac.uk    --
--   This software is released under the terms of the GNU General Public    --
--   License and is intended primarily for educational use. Please contact  --
--   the author to report bugs, suggestions and modifications.              --
--                                                                          --
------------------------------------------------------------------------------
-- $Id: jewl-canvas_implementation.adb 1.7 2007/01/08 17:00:00 JE Exp $
------------------------------------------------------------------------------
--
-- $Log: jewl-canvas_implementation.adb $
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

with Ada.Unchecked_Deallocation;
with Ada.Text_IO;

package body JEWL.Canvas_Implementation is

  use JEWL.Win32_Interface;

  procedure Free is new Ada.Unchecked_Deallocation
                            (Canvas_Object_Type'Class, Canvas_Object_Ptr);

  ----------------------------------------------------------------------------
  --
  --                     C A N V A S _ M O N I T O R
  --
  --  This protected type mediates between the canvas operations and the
  --  message loop task.
  --
  ----------------------------------------------------------------------------

  protected body Canvas_Monitor is

    --------------------------------------------------------------------------
    --
    --  Clear: delete all objects on the drawing list by restoring to the
    --         beginning of the list.
    --
    procedure Clear is
    begin
      Save_Pointer := null;
      Restore;
    end Clear;

    --------------------------------------------------------------------------
    --
    --  Save: record the current position in the drawing list.
    --
    procedure Save is
    begin
      Save_Pointer := Last_Object;
    end Save;

    --------------------------------------------------------------------------
    --
    --  Restore: truncate the drawing list back to the saved position (or
    --           the beginning, if the saved position is null) and delete
    --           any objects removed in the process.
    --
    procedure Restore is
      P,Q : Canvas_Object_Ptr;
    begin
      if Save_Pointer = null then
        P := First_Object;
        First_Object := null;
        Last_Object  := null;
      else
        Last_Object := Save_Pointer;
        P := Last_Object.Next;
        Last_Object.Next := null;
      end if;
      while P /= null loop
        Q := P;
        P := P.Next;
        Free (Q);
      end loop;
    end Restore;

    --------------------------------------------------------------------------
    --
    --  Draw: draw all objects on the drawing list.
    --
    procedure Draw (Handle : in Win32_HWND;
                    Font   : in Win32_HFONT) is
      P : Canvas_Object_Ptr := First_Object;
      D : Win32_HDC;
      H : Win32_HANDLE;
      S : aliased Win32_PAINTSTRUCT;
      I : Win32_INT;
      L : aliased Win32_LOGBRUSH;
      B : Win32_HBRUSH;
    begin
      L.lbStyle := BS_HOLLOW;
      B := CreateBrushIndirect(L'Unchecked_Access);

      -- Start drawing using the initial tool set: a transparent brush,
      -- standard black pen, and the canvas font

      D := BeginPaint (Handle, S'Access);
      H := SelectObject(D, B);
      H := SelectObject(D, GetStockObject(BLACK_PEN));
      H := SelectObject(D, Font);
      I := SetBkMode (D, TRANSPARENT);

      -- Draw the objects in the drawing list

      while P /= null loop
        Draw (P.all, D);
        P := P.Next;
      end loop;

      -- Finish painting and destroy the brush

      Bool_Dummy := EndPaint (Handle, S'Access);
      Bool_Dummy := DeleteObject (B);
    end Draw;

    --------------------------------------------------------------------------
    --
    --  Add: add a new object to the end of the drawing list.
    --
    procedure Add (Object : in Canvas_Object_Ptr) is
    begin
      if Last_Object = null then
        First_Object := Object;
      else
        Last_Object.Next := Object;
      end if;
      Last_Object := Object;
      Object.Next := null;
    end Add;

    --------------------------------------------------------------------------
    --
    --  Set_Brush: store the brush used to erase the background.
    --
    procedure Set_Brush (Brush : in Win32_HBRUSH) is
    begin
      Bool_Dummy := DeleteObject(BG_Brush);
      BG_Brush := Brush;
    end Set_Brush;

    --------------------------------------------------------------------------
    --
    --  Background: get the background brush. This is called by the message
    --              loop task in response to a WM_ERASEBKGND message.
    --
    function Background return Win32_HBRUSH is
    begin
      return BG_Brush;
    end Background;

    --------------------------------------------------------------------------
    --
    --  Set_Start: store the position where the mouse button was pressed.
    --             This is called by the message loop task in response to
    --             a WM_LBUTTONDOWN message. The end position is initially
    --             set to match the start position.
    --
    procedure Set_Start (X, Y : in Integer) is
    begin
      Start_X := X;
      Start_Y := Y;
      End_X := X;
      End_Y := Y;
      Moved := False;
    end Set_Start;

    --------------------------------------------------------------------------
    --
    --  Get_Start: get the position where the mouse button was pressed.
    --
    procedure Get_Start (X, Y : out Integer) is
    begin
      X := Start_X;
      Y := Start_Y;
    end Get_Start;

    --------------------------------------------------------------------------
    --
    --  Set_End: store the current mouse position. This is called by
    --           the message loop task in response to a WM_MOUSEMOVE
    --           or WM_LBUTTONUP message. The Moved flag is set true
    --           to indicate that the mouse has moved.
    --
    procedure Set_End (X, Y : in Integer) is
    begin
      End_X := X;
      End_Y := Y;
      Moved := True;
    end Set_End;

    --------------------------------------------------------------------------
    --
    --  Get_End: get the current mouse position. The Moved flag is reset
    --           so that Mouse_Moved will return False until the mouse is
    --           moved again.
    --
    procedure Get_End (X, Y : out Integer) is
    begin
      X := End_X;
      Y := End_Y;
      Moved := False;
    end Get_End;

    --------------------------------------------------------------------------
    --
    --  Set_Button: store the current mouse button state. This is called
    --              from the message loop task in response to WM_LBUTTONUP
    --              or WM_LBUTTONDOWN messages.
    --
    procedure Set_Button (B : in Boolean) is
    begin
      Button := B;
    end Set_Button;

    --------------------------------------------------------------------------
    --
    --  Mouse_Down: get the current mouse button state.
    --
    function Mouse_Down return Boolean is
    begin
      return Button;
    end Mouse_Down;

    --------------------------------------------------------------------------
    --
    --  Mouse_Moved: test if the mouse has moved since the last time that
    --               Get_End was called.
    --
    function Mouse_Moved return Boolean is
    begin
      return Moved;
    end Mouse_Moved;

    --------------------------------------------------------------------------
    --
    --  Set_Key: store the character corresponding to a key that has been
    --           pressed. This is called from the message loop in response
    --           to WM_CHAR messages.
    --
    procedure Set_Key (C : in Character) is
    begin
      Keycode := C;
    end Set_Key;

    --------------------------------------------------------------------------
    --
    --  Get_Key: return the character corresponding to a key that has been
    --           pressed.
    --
    procedure Get_Key (C : out Character) is
    begin
      C := Keycode;
      Keycode := ASCII.NUL;
    end Get_Key;

  end Canvas_Monitor;

  ----------------------------------------------------------------------------
  --
  --                 D R A W I N G   O P E R A T I O N S
  --
  --  The following procedures are the implementations of Draw for the
  --  different types of canvas objects. They are called by Draw in the
  --  canvas monitor, which dispatches to the appropriate procedure for
  --  each object in the drawing list.
  --
  ----------------------------------------------------------------------------
  --
  --  Draw a text string
  --
  procedure Draw (Object : in out Text_Type;
                  Window : in Win32_HDC) is
    I : Win32_INT;
    R : aliased Win32_RECT;
    W : Win32_UINT;
    S : Win32_String := To_Array(Object.Text);
  begin
    -- Calculate the bounding rectangle

    R.Left   := Win32_LONG(Object.From.X);
    R.Top    := Win32_LONG(Object.From.Y);
    R.Right  := Win32_LONG(Object.To.X);
    R.Bottom := Win32_LONG(Object.To.Y);

    -- Select the appropriate alignment flag (-1 is used to indicate
    -- that the text is not clipped by the bounding rectangle, and 0
    -- upwards are values generated by Alignment_Type'Pos).

    if Object.Align < 0 then
      W := DT_NOCLIP;
    elsif Object.Align = 0 then
      W := DT_LEFT;
    elsif Object.Align = 1 then
      W := DT_CENTER;
    else
      W := DT_RIGHT;
    end if;

    -- Now draw the text

    I := DrawText (Window, To_LPCSTR(S), Win32_INT(Object.Length),
                   R'Unchecked_Access, W);
  end Draw;

  ----------------------------------------------------------------------------
  --
  --  Draw a line
  --
  procedure Draw (Object : in out Line_Type;
                  Window : in Win32_HDC) is
  begin
    Bool_Dummy := MoveToEx (Window, Win32_INT(Object.From.X),
                                    Win32_INT(Object.From.Y),
                                    null);
    Bool_Dummy := LineTo (Window, Win32_INT(Object.To.X),
                                  Win32_INT(Object.To.Y));
  end Draw;

  ----------------------------------------------------------------------------
  --
  --  Draw a rectangle
  --
  procedure Draw (Object : in out Rectangle_Type;
                  Window : in Win32_HDC) is
  begin
    Bool_Dummy := Rectangle (Window, Win32_INT(Object.From.X),
                                     Win32_INT(Object.From.Y),
                                     Win32_INT(Object.To.X),
                                     Win32_INT(Object.To.Y));
  end Draw;

  ----------------------------------------------------------------------------
  --
  --  Draw a rectangle with rounded corners
  --
  procedure Draw (Object : in out Rounded_Rectangle_Type;
                  Window : in Win32_HDC) is
  begin
    Bool_Dummy := RoundRect (Window, Win32_INT(Object.From.X),
                                     Win32_INT(Object.From.Y),
                                     Win32_INT(Object.To.X),
                                     Win32_INT(Object.To.Y),
                                     Win32_INT(Object.Corner.X),
                                     Win32_INT(Object.Corner.Y));
  end Draw;

  ----------------------------------------------------------------------------
  --
  --  Draw an ellipse
  --
  procedure Draw (Object : in out Ellipse_Type;
                  Window : in Win32_HDC) is
  begin
    Bool_Dummy := Ellipse (Window, Win32_INT(Object.From.X),
                                   Win32_INT(Object.From.Y),
                                   Win32_INT(Object.To.X),
                                   Win32_INT(Object.To.Y));
  end Draw;

  ----------------------------------------------------------------------------
  --
  --  Draw a polyline
  --
  procedure Draw (Object : in out Polyline_Type;
                  Window : in Win32_HDC) is
  begin
    Bool_Dummy := Polyline (Window, Object.Points(1)'Unchecked_Access,
                                    Win32_INT(Object.Count));
  end Draw;

  ----------------------------------------------------------------------------
  --
  --  Draw a polygon
  --
  procedure Draw (Object : in out Polygon_Type;
                  Window : in Win32_HDC) is
  begin
    Bool_Dummy := Polygon (Window, Object.Points(1)'Unchecked_Access,
                                   Win32_INT(Object.Count));
  end Draw;

  ----------------------------------------------------------------------------
  --
  --  Draw a bitmap
  --
  procedure Draw (Object : in out Bitmap_Type;
                  Window : in Win32_HDC) is
    H : Win32_HDC := CreateCompatibleDC (Window);
    B : Win32_BITMAP;
    P : aliased Win32_POINT;
    Q : aliased Win32_POINT;
    I : Image_Ptr := Image_Ptr(Object.Bitmap.Pointer);
    W : Win32_HBITMAP := I.Image;
    N : Win32_INT;
  begin
    Long_Dummy := To_LONG (SelectObject (H, W));
    N := SetMapMode (H, GetMapMode(Window));
    N := GetObject (W, Win32_INT(Win32_BITMAP'Size/Win32_BYTE'Size),
                    B'Address);
    P := (X => B.bmWidth, Y => B.bmHeight);
    Bool_Dummy := DPtoLP (Window, P'Unchecked_Access, 1);
    Q := (0,0);
    Bool_Dummy := DPtoLP (H, Q'Unchecked_Access, 1);
    Bool_Dummy := StretchBlt (Window,
                              Win32_INT(Object.From.X), Win32_INT(Object.From.Y),
                              Win32_INT(Object.Width), Win32_INT(Object.Height),
                              H,
                              Win32_INT(Q.X), Win32_INT(Q.Y),
                              Win32_INT(I.Width), Win32_INT(I.Height));
    Bool_Dummy := DeleteDC (H);
  end Draw;

  ----------------------------------------------------------------------------
  --
  --  Select a drawing tool
  --
  procedure Draw (Object : in out Handle_Type;
                  Window : in Win32_HDC) is
    H : Win32_HGDIOBJ;
    W : Win32_HBITMAP :=
                Counted_Handle_Type(Object.Handle.Pointer.all).Handle;
  begin
    H := SelectObject (Window, W);
  end Draw;

  ----------------------------------------------------------------------------
  --
  --         C O N T R O L L E D   T Y P E   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Cleanup: destroy a bitmap handle in an Image_Internals object.
  --
  procedure Cleanup (Object : in out Image_Internals) is
  begin
    Bool_Dummy := DeleteObject (Object.Image);
  end Cleanup;

  ----------------------------------------------------------------------------
  --
  --  Cleanup: destroy a handle to a Windows GDI object.
  --
  procedure Cleanup (Object : in out Counted_Handle_Type) is
  begin
    Bool_Dummy := DeleteObject (Object.Handle);
  end Cleanup;

  ----------------------------------------------------------------------------
  --
  --  Handle: create a reference counted object for a Windows handle.
  --
  function  Handle (Object : Win32_HGDIOBJ) return JEWL.Controlled_Type is
    C : JEWL.Controlled_Type;
  begin
    C.Pointer := new Counted_Handle_Type;
    Counted_Handle_Type(C.Pointer.all).Handle := Object;
    return C;
  end Handle;

end JEWL.Canvas_Implementation;
