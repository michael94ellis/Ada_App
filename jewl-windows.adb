------------------------------------------------------------------------------
--                                                                          --
--                         J E W L . W I N D O W S                          --
--                                                                          --
--   The body of the GUI package for use with Microsoft Windows.            --
--                                                                          --
--   All window types contain a Controlled_Type object, which contains a    --
--   pointer to a Reference_Counted_Type. The type Window_Internals is      --
--   derived from Reference_Counted_Type to provide the internal data       --
--   structures needed for all windows, and further derivations are made    --
--   for specific types of window. Since Reference_Counted_Type is a        --
--   controlled type, all derivations must occur at library level, so       --
--   this is done in a separate non-generic private package which also      --
--   contains other implementation details used by this package body.       --
--   Even so, this is a large package!                                      --
--                                                                          --
--   Copyright (C) John English 2000. Contact address: je@brighton.ac.uk    --
--   This software is released under the terms of the GNU General Public    --
--   License and is intended primarily for educational use. Please contact  --
--   the author to report bugs, suggestions and modifications.              --
--                                                                          --
------------------------------------------------------------------------------
-- $Id: jewl-windows.adb 1.7 2007/01/08 17:00:00 JE Exp $
------------------------------------------------------------------------------
--
-- $Log: jewl-windows.adb $
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

with JEWL.Window_Implementation; use JEWL.Window_Implementation;
with JEWL.Message_Handling;      use JEWL.Message_Handling;
with JEWL.Canvas_Implementation; use JEWL.Canvas_Implementation;
with JEWL.Win32_Interface;       use JEWL.Win32_Interface;

with Ada.Exceptions;             use Ada.Exceptions;
with Ada.Tags;                   use Ada.Tags;
with Ada.Streams.Stream_IO;      use Ada.Streams.Stream_IO;
with System;

package body JEWL.Windows is

  use type System.Address;
  use type Ada.Streams.Stream_Element_Offset;
  use type Win32_BOOL, Win32_LONG, Win32_WORD,
           Win32_UINT, Win32_SIZE, Win32_DWORD;

  ----------------------------------------------------------------------------
  --  Win32 window class names
  ----------------------------------------------------------------------------

  Frame_Class  : constant Win32_String := To_Array("JEWL.Windows.Frame");
  Dialog_Class : constant Win32_String := To_Array("JEWL.Windows.Dialog");
  Canvas_Class : constant Win32_String := To_Array("JEWL.Windows.Canvas");

  ----------------------------------------------------------------------------
  --  End-of-line string
  ----------------------------------------------------------------------------

  EOL : constant String := ASCII.CR & ASCII.LF;

  ----------------------------------------------------------------------------
  --
  --              M I S C E L L A N E O U S   R O U T I N E S
  --
  ----------------------------------------------------------------------------
  --
  --  Show_Error: display a message box with an OK button and stop sign.
  --
  procedure Show_Error (Text  : in String;
                        Title : in String := "Error") is
    I : Integer;
  begin
    I := Message_Box (Text, Title, MB_OK+MB_ICONSTOP);
  end Show_Error;

  ----------------------------------------------------------------------------
  --
  --  Show_Query: display a message box with Yes/No buttons and a question
  --              mark.
  --
  function Show_Query (Text  : in String;
                       Title : in String := "Query") return Boolean is
  begin
    return Message_Box (Text, Title, MB_YESNO+MB_ICONQUESTION) = IDYES;
  end Show_Query;

  ----------------------------------------------------------------------------
  --
  --  Show_Message: display a message box with an OK button and an
  --                information sign.
  --
  procedure Show_Message (Text  : in String;
                          Title : in String := "Message") is
    I : Integer;
  begin
    I := Message_Box (Text, Title, MB_OK+MB_ICONINFORMATION);
  end Show_Message;

  ----------------------------------------------------------------------------
  --
  --  Play_Sound: play a sound held in a wave file.
  --
  procedure Play_Sound (Sound : in String) is
  begin
    Bool_Dummy := PlaySound (To_LPCSTR(To_Array(Sound)), System.Null_Address,
                             SND_FILENAME + SND_NODEFAULT + SND_ASYNC);
  end Play_Sound;

  ----------------------------------------------------------------------------
  --
  --  Screen_Width: get width of display screen in pixels.
  --
  function Screen_Width return Natural is
  begin
    return Natural(GetSystemMetrics(SM_CXSCREEN));
  end Screen_Width;

  ----------------------------------------------------------------------------
  --
  --  Screen_Height: get height of display screen in pixels.
  --
  function Screen_Height return Natural is
  begin
    return Natural(GetSystemMetrics(SM_CYSCREEN));
  end Screen_Height;

  ----------------------------------------------------------------------------
  --
  --                I N T E R N A L   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Get_Internals: check that a window has been initialised and return a
  --                 pointer to its Window_Internals structure, or raise an
  --                 Invalid_Window exception. Additional parameters are
  --                 used to generate a meaningful message to accompany
  --                 the exception.
  --
  function Get_Internals (Window    : in Window_Type'Class;
                          Operation : in String) return Window_Ptr is
  begin
    if Window.Internals.Pointer = null then
      Raise_Exception (Invalid_Window'Identity,
                       External_Tag(Window'Tag) &
                       ": window not initialised in call to " & Operation);
    end if;
    return Window_Ptr(Window.Internals.Pointer);
  end Get_Internals;

  ----------------------------------------------------------------------------
  --
  --  Get_Internals: check that a common dialog has been initialised and
  --                 return a pointer to its Common_Dialog_Internals, or
  --                 raise an Invalid_Window exception. Additional parameters
  --                 are used to generate a meaningful message to accompany
  --                 the exception.
  --
  function Get_Internals (Dialog    : in Common_Dialog_Type'Class;
                          Operation : in String) return Common_Dialog_Ptr is
  begin
    if Dialog.Internals.Pointer = null then
      Raise_Exception (Invalid_Window'Identity,
                       External_Tag(Dialog'Tag) &
                       ": dialog not initialised in call to " & Operation);
    end if;
    return Common_Dialog_Ptr(Dialog.Internals.Pointer);
  end Get_Internals;

  ----------------------------------------------------------------------------
  --
  --  Add: add an object to the end of a canvas drawing list and invalidate
  --       the canvas window so it will be repainted. The second parameter
  --       enables the actual operation name to be passed to Get_Internals.
  --
  procedure Add (Canvas    : in Canvas_Type;
                 Operation : in String;
                 Object    : in Canvas_Object_Ptr) is
    C : Canvas_Ptr := Canvas_Ptr(Get_Internals(Canvas, Operation));
  begin
    C.Monitor.Add (Object);
    Bool_Dummy := InvalidateRect (C.Handle, null, 1);
  end;

  ----------------------------------------------------------------------------
  --
  --  Create_Child: create a child window with specified characteristics.
  --                The last parameter enables the actual operation name
  --                to be passed to Get_Internals.
  --
  procedure Create_Child (Window    : in out Window_Type'Class;
                          Parent    : in Container_Type'Class;
                          Class     : in String;
                          Title     : in String;
                          XStyle    : in Win32_DWORD;
                          Style     : in Win32_DWORD;
                          Origin    : in Point_Type;
                          Width     : in Integer;
                          Height    : in Integer;
                          Font      : in Font_Type;
                          ID        : in Integer;
                          Operation : in String) is
    P : Container_Ptr := Container_Ptr (Get_Internals (Parent, Operation));
    X : Window_Ptr := Get_Internals (Window, Operation);
    T : Integer := Origin.Y;
    L : Integer := Origin.X;
    H : Integer := Height;
    W : Integer := Width;
    B : Boolean;
    C : Win32_String := To_Array(Class);
    N : Win32_String := To_Array(Title);
  begin
    -- Fill in links to parent and siblings

    X.Parent := P;
    if P.Last = null then
      P.First := Window.Internals;
    else
      P.Last.Next := Window.Internals;
    end if;
    P.Last := X;

    -- Fill in the command code associated with this window

    X.Action := ID;

    -- Fill in the window dimensions

    X.Top    := Origin.Y;
    X.Left   := Origin.X;
    X.Height := Height;
    X.Width  := Width;

    -- Calculate the actual window dimensions (the dimensions given may be
    -- relative to the parent window)

    Get_Actual_Bounds (P.Handle, T, L, W, H, B);

    -- Ask the message loop task to create the child window

    Message_Loop.Create_Child (X, P, C, N, XStyle, Style, T, L, W, H);

    -- Create the font, or use the parent font if no font name is given

    if Font.Length > 0 then
      Set_Font (Window, Font);
    else
      X.Font := System.Null_Address;
    end if;

  end Create_Child;

  ----------------------------------------------------------------------------
  --
  --                   I M A G E   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Image: load a bitmap image from a specified file.
  --
  function Image (Name : String) return Image_Type is
    subtype Offset   is Ada.Streams.Stream_Element_Offset;
    subtype Elements is Ada.Streams.Stream_Element_Array;

    Image   : Image_Type;
    File    : Ada.Streams.Stream_IO.File_Type;
    Stream  : Ada.Streams.Stream_IO.Stream_Access;
    Header  : Win32_BITMAPFILEHEADER;
    Info    : aliased Win32_BITMAPINFOHEADER;
    Colours : Offset;
    Bytes   : Offset;
    Bitmap  : Win32_HBITMAP;
    Pointer : Image_Ptr;
  begin
    Image.Internals.Pointer := null;

    -- Create a stream to read the bitmap file

    Ada.Streams.Stream_IO.Open (File, Name => Name, Mode => In_File);
    Stream := Ada.Streams.Stream_IO.Stream (File);

    -- Read and check the file header

    Win32_BITMAPFILEHEADER'Read (Stream, Header);
    if Header.bfType /= BITMAP_MAGIC then
      raise Data_Error;
    end if;

    -- Read and check the bitmap info header

    Win32_BITMAPINFOHEADER'Read (Stream, Info);
    if Info.biSize /= Info'Size/Win32_BYTE'Size or Info.biPlanes /= 1 then
      raise Data_Error;
    end if;

    -- Calculate no. of colour table entries following the info header

    if Info.biClrUsed /= 0 then
      Colours := Offset(Info.biClrUsed);
    elsif Info.biBitCount <= 8 then
      Colours := Offset(2 ** Integer(Info.biBitCount));
    elsif Info.biCompression = BI_BITFIELDS then
      Colours := 3;
    else
      Colours := 0;
    end if;

    -- Calculate size of bitmap data

    Bytes := Offset(Info.biSizeImage);
    if Bytes = 0 then
      Bytes := (Offset(Info.biWidth) * Offset(Info.biBitCount) + 31) / 32
               * Offset(Info.biHeight);
    end if;

    -- Process the rest of the file

    declare
      C : Elements (1 .. Colours * 4 + Offset(Info.biSize));
      D : Elements (1 .. Offset(Bytes));
      E : Offset;
      P : Win32_BITMAPINFOHEADER;     for P'Address use C(1)'Address;
      H : Win32_HDC;
    begin

      -- Copy the bitmap info header into the header block

      P := Info;

      -- Read the colour table into the header block

      Ada.Streams.Read (Stream.all, C (Offset(Info.biSize)+1 .. C'Last), E);
      if E /= C'Length then
        raise Data_Error;
      end if;

      -- Read the rest of the file into the data block

      Ada.Streams.Read (Stream.all, D, E);
      if E /= D'Length then
        raise Data_Error;
      end if;

      -- Get a device context for the display

      H := CreateDC (To_LPCSTR(To_Array("DISPLAY")),
                     null, null, System.Null_Address); 
      if H = System.Null_Address then
        raise Data_Error;
      end if;

      -- Create the bitmap using the display context

      Bitmap := CreateDIBitmap (H, Info'Unchecked_Access, CBM_INIT,
                                D(1)'Address, C(1)'Address, DIB_RGB_COLORS);
      if Bitmap = System.Null_Address then
        raise Data_Error;
      end if;
    end;

    -- Fill in image structure

    Pointer := new Image_Internals;

    Pointer.Image  := Bitmap;
    Pointer.Width  := Natural(Info.biWidth);
    Pointer.Height := Natural(Info.biHeight);

    Image.Internals.Pointer := Reference_Counted_Ptr(Pointer);

    Close (File);
    return Image;

  exception
    when others =>
      Close (File);
      return Image;

  end Image;

  ----------------------------------------------------------------------------
  --
  --  Valid: get the width of the specified image.
  --
  function Valid (Image : Image_Type) return Boolean is
  begin
    return Image.Internals.Pointer /= null;
  end Valid;

  ----------------------------------------------------------------------------
  --
  --  Width: get the width of the specified image.
  --
  function Width (Image : Image_Type) return Natural is
  begin
    if Valid(Image) then
      return Image_Internals(Image.Internals.Pointer.all).Width;
    else
      return 0;
    end if;
  end Width;

  ----------------------------------------------------------------------------
  --
  --  Height: get the height of the specified image.
  --
  function Height (Image : Image_Type) return Natural is
  begin
    if Valid(Image) then
      return Image_Internals(Image.Internals.Pointer.all).Height;
    else
      return 0;
    end if;
  end Height;

  ----------------------------------------------------------------------------
  --
  --                  W I N D O W   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Show: make a window visible or invisible, bringing visible windows to
  --        the foreground.
  --
  procedure Show (Window  : in Window_Type;
                  Visible : in Boolean := True) is
    P : Window_Ptr := Get_Internals (Window, "Show");
  begin
    if Visible then
      Bool_Dummy := ShowWindow(P.Handle,SW_SHOW);
      Bool_Dummy := SetForegroundWindow (P.Handle);
    else
      Bool_Dummy := ShowWindow(P.Handle,SW_HIDE);
    end if;
  end Show;

  ----------------------------------------------------------------------------
  --
  --  Hide: use Show (above) to hide a window.
  --
  procedure Hide (Window : in Window_Type) is
    P : Window_Ptr := Get_Internals (Window, "Hide");
  begin
    Show (Window, False);
  end Hide;

  ----------------------------------------------------------------------------
  --
  --  Focus: give the input focus to the specified window.
  --
  procedure Focus (Window : in Window_Type) is
    P : Window_Ptr := Get_Internals (Window, "Focus");
  begin
    Message_Loop.Set_Focus (P.Handle);
  end Focus;

  ----------------------------------------------------------------------------
  --
  --  Visible: test if a window is visible.
  --
  function Visible (Window : Window_Type) return Boolean is
    P : Window_Ptr := Get_Internals (Window, "Visible");
  begin
    return IsWindowVisible(P.Handle) /= 0;
  end Visible;

  ----------------------------------------------------------------------------
  --
  --  Get_Origin: get the coordinates of a window's top left corner.
  --
  function Get_Origin (Window : Window_Type) return Point_Type is
    P : Window_Ptr := Get_Internals (Window, "Visible");
    R : aliased Win32_RECT;
  begin
    Bool_Dummy := GetWindowRect (P.Handle, R'Unchecked_Access);
    return (Integer(R.Left),Integer(R.Top));
  end Get_Origin;

  ----------------------------------------------------------------------------
  --
  --  Get_Width: get the width of a window.
  --
  function Get_Width (Window : Window_Type) return Natural is
    P : Window_Ptr := Get_Internals (Window, "Get_Width");
    R : aliased Win32_RECT;
  begin
    Bool_Dummy := GetWindowRect (P.Handle, R'Unchecked_Access);
    return Natural (R.Right - R.Left);
  end Get_Width;

  ----------------------------------------------------------------------------
  --
  --  Get_Height: get the height of a window.
  --
  function Get_Height (Window : Window_Type) return Natural is
    P : Window_Ptr := Get_Internals (Window, "Get_Height");
    R : aliased Win32_RECT;
  begin
    Bool_Dummy := GetWindowRect (P.Handle, R'Unchecked_Access);
    return Natural (R.Bottom - R.Top);
  end Get_Height;

  ----------------------------------------------------------------------------
  --
  --  Set_Origin: set the coordinates of a window's top left corner.
  --
  procedure Set_Origin (Window : in Window_Type;
                        Origin : in Point_Type) is
    P : Window_Ptr := Get_Internals (Window, "Set_Origin");
  begin
    Bool_Dummy := SetWindowPos (P.Handle, System.Null_Address,
                                Win32_INT(Origin.X), Win32_INT(Origin.Y),
                                0, 0, SWP_NOZORDER + SWP_NOSIZE);
  end Set_Origin;

  ----------------------------------------------------------------------------
  --
  --  Set_Size: set the width and height of a window.
  --
  procedure Set_Size (Window : in Window_Type;
                      Width  : in Natural := 0;
                      Height : in Natural := 0) is
    P : Window_Ptr := Get_Internals (Window, "Set_Size");
    W : Natural := Width;
    H : Natural := Height;
  begin
    if Width = 0 then
      W := Get_Width (Window);
    end if;
    if Height = 0 then
      H := Get_Height (Window);
    end if;
    Bool_Dummy := SetWindowPos (P.Handle, System.Null_Address, 0, 0,
                                Win32_INT(W), Win32_INT(H),
                                SWP_NOZORDER + SWP_NOMOVE);
  end Set_Size;

  ----------------------------------------------------------------------------
  --
  --  Set_Font: change the font associated with a window and invalidate it
  --            so that it will be repainted.
  --
  procedure Set_Font (Window : in Window_Type;
                      Font   : in Font_Type) is
    P : Window_Ptr := Get_Internals (Window, "Set_Font");
  begin
    if Font.Length > 0 then
      if P.Font /= System.Null_Address then
        Bool_Dummy := DeleteObject (P.Font);
      end if;
      P.Font := Create_Font (Font);
      Long_Dummy := SendMessage (P.Handle, WM_SETFONT, To_WPARAM(P.Font), 0);
      Bool_Dummy := InvalidateRect (P.Handle, null, 1);
    end if;
  end Set_Font;

  ----------------------------------------------------------------------------
  --
  --  Get_Font: build a Font_Type structure for a window's current font.
  --
  function Get_Font (Window : Window_Type) return Font_Type is
    P : Window_Ptr := Get_Internals (Window, "Get_Font");
    I : Win32_INT;
    F : Win32_LOGFONT;
  begin
    while P.Font = System.Null_Address loop
      P := Window_Ptr(P.Parent);
      exit when P = null;
    end loop;
    if P = null or else P.Font = System.Null_Address then
      return Default_Font;
    else
      I := GetObject (P.Font, Win32_LOGFONT'Size/Win32_BYTE'Size, F'Address);
      return Get_Font (F);
    end if;
  end Get_Font;

  ----------------------------------------------------------------------------
  --
  --                   F R A M E   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Frame: construct a frame with the specified characteristics.
  --
  function Frame (Origin  : Point_Type;
                  Width   : Positive;
                  Height  : Positive;
                  Title   : String;
                  Command : Command_Type;
                  Font    : Font_Type := Default_Font) return Frame_Type is
    W : Frame_Type;
    M : Main_Window_Ptr := new Main_Window_Internals;
    P : Window_Ptr      := Window_Ptr(M);
    T : Win32_String    := To_Array(Title);
  begin
    -- Set up the Window_Internals structure for a window with default
    -- placement

    W.Internals.Pointer := Reference_Counted_Ptr(P);
    P.Action := Command_Type'Pos(Command);
    P.Top    := Origin.Y;
    P.Left   := Origin.X;
    P.Width  := Width;
    P.Height := Height;

    -- Ask the message loop to create a top-level window

    Message_Loop.Create_Window (M, Frame_Class, T,
                                WS_EX_CLIENTEDGE or WS_EX_APPWINDOW,
                                WS_OVERLAPPEDWINDOW,
                                True);

    -- Set the font now that the frame exists

    if Font.Length > 0 then
      Set_Font (W, Font);
    else
      Set_Font (W, Default_Font);
    end if;

    -- Bring the window to the front and return the window object

    Bool_Dummy := SetForegroundWindow(P.Handle);
    return W;
  end Frame;

  ----------------------------------------------------------------------------
  --
  --  Frame: construct a frame with the specified characteristics and
  --         default placement.
  --
  function Frame (Width   : Positive;
                  Height  : Positive;
                  Title   : String;
                  Command : Command_Type;
                  Font    : Font_Type := Default_Font) return Frame_Type is
  begin
    return Frame ((Integer(CW_USEDEFAULT),Integer(CW_USEDEFAULT)),
                  Width, Height, Title, Command, Font);
  end Frame;

  ----------------------------------------------------------------------------
  --
  --  Close: close and destroy a frame.
  --
  procedure Close (Frame : in Frame_Type) is
    P : Window_Ptr := Get_Internals (Frame, "Close");
  begin
    if IsWindow(P.Handle) /= 0 then
      Message_Loop.Destroy_Window (P.Handle);
    end if;
  end Close;

  ----------------------------------------------------------------------------
  --
  --  Valid: test if a frame is valid (i.e. if the window exists).
  --
  function Valid (Frame : Frame_Type) return Boolean is
    P : Window_Ptr := Window_Ptr(Frame.Internals.Pointer);
  begin
    return P /= null and then
           IsWindow(P.Handle) /= 0;
  end Valid;

  ----------------------------------------------------------------------------
  --
  --  Frame_Width: return the width of a frame's border.
  --
  function Frame_Width return Natural is
  begin
    return Natural
           ((GetSystemMetrics(SM_CXFRAME) + GetSystemMetrics(SM_CXEDGE)) * 2);
  end Frame_Width;

  ----------------------------------------------------------------------------
  --
  --  Frame_Height: return the height of a frame's border.
  --
  function Frame_Height return Natural is
  begin
    return Natural
           ((GetSystemMetrics(SM_CYFRAME) + GetSystemMetrics(SM_CYEDGE)) * 2 +
            GetSystemMetrics(SM_CYCAPTION));
  end Frame_Height;

  ----------------------------------------------------------------------------
  --
  --  Next_Command: ask the info monitor for the next command.
  --
  function Next_Command return Command_Type is
    Cmd : Natural;
  begin
    Window_Info.Get_Command (Cmd);
    return Command_Type'Val(Cmd);
  end Next_Command;

  ----------------------------------------------------------------------------
  --
  --  Command_Ready: ask the info monitor if there is a command pending.
  --
  function Command_Ready return Boolean is
  begin
    return Window_Info.Test_Command;
  end Command_Ready;

  ----------------------------------------------------------------------------
  --
  --                  D I A L O G   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Dialog: create a top-level dialog window.
  --
  function Dialog (Width   : Positive;
                   Height  : Positive;
                   Title   : String;
                   Command : Command_Type;
                   Font    : Font_Type := Default_Font) return Dialog_Type is
    W : Dialog_Type;
    X : Integer := Integer(GetSystemMetrics(SM_CXSCREEN)) / 2;
    Y : Integer := Integer(GetSystemMetrics(SM_CYSCREEN)) / 2;
    M : Main_Window_Ptr  := new Main_Window_Internals;
    P : Window_Ptr       := Window_Ptr(M);
    T : Win32_String := To_Array(Title);
  begin
    -- Set up the Window_Internals structure for a centred window

    W.Internals.Pointer := Reference_Counted_Ptr(P);
    P.Action := Command_Type'Pos(Command);
    P.Top    := Y - Height/2;
    P.Left   := X - Width/2;
    P.Width  := Width;
    P.Height := Height;

    -- Ask the message loop to create a hidden top-level window

    Message_Loop.Create_Window (M, Dialog_Class, T,
                                0, WS_DLGFRAME or WS_SYSMENU,
                                False);

    -- Set the font now that the dialog exists

    if Font.Length > 0 then
      Set_Font (W, Font);
    else
      Set_Font (W, Default_Font);
    end if;

    -- Return the window object

    return W;
  end Dialog;

  ----------------------------------------------------------------------------
  --
  --  Execute: run a dialog until it issues a command. Note that dialogs
  --           are hidden rather than destroyed so that closing a dialog
  --           them won't make any attached controls disappear.
  --
  function Execute (Dialog  : in Dialog_Type) return Command_Type is
    D : Win32_HWND;
    C : Natural;
    P : Window_Ptr := Get_Internals (Dialog, "Execute");
  begin
    -- Record this window as the currently active dialog

    D := P.Handle;
    Window_Info.Get_Dialog (D);

    -- Make the window visible and bring it to the foreground

    Bool_Dummy := ShowWindow (P.Handle, SW_SHOW);
    Bool_Dummy := SetForegroundWindow (P.Handle);

    -- Wait for a command (which must be from this dialog, as dialog
    -- windows disable all other windows belonging to the application)

    Window_Info.Get_Command (C);

    -- Restore the original active dialog setting

    Window_Info.Get_Dialog (D);

    -- Hide the dialog window and return the command code

    Bool_Dummy := ShowWindow (P.Handle, SW_HIDE);
    return Command_Type'Val(C);
  end Execute;

  ----------------------------------------------------------------------------
  --
  --  Dialog_Width: return the width of a dialog's border.
  --
  function Dialog_Width return Natural is
  begin
    return Natural (GetSystemMetrics(SM_CXDLGFRAME) * 2);
  end Dialog_Width;

  ----------------------------------------------------------------------------
  --
  --  Dialog_Height: return the height of a dialog's border.
  --
  function Dialog_Height return Natural is
  begin
    return Natural (GetSystemMetrics(SM_CYDLGFRAME) * 2 +
                    GetSystemMetrics(SM_CYCAPTION));
  end Dialog_Height;

  ----------------------------------------------------------------------------
  --
  --                   P A N E L   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Panel: create a panel (which is actually a Windows groupbox if it has
  --         a title, or a static control with an etched border if not).
  --
  function Panel (Parent : Container_Type'Class;
                  Origin : Point_Type;
                  Width  : Integer;
                  Height : Integer;
                  Title  : String := "";
                  Font   : Font_Type := Parent_Font) return Panel_Type is
    W : Panel_Type;
    C : String(1..6);
    S : Win32_DWORD := WS_GROUP;
    P : Container_Ptr := new Container_Internals;
  begin
    -- Choose the actual window class and style

    if Title = "" then
      C := "static";
      S := S or SS_ETCHEDFRAME;
    else
      C := "button";
      S := S or BS_GROUPBOX;
    end if;

    -- Create the window and return it

    W.Internals.Pointer := Reference_Counted_Ptr(P);
    Create_Child (W, Parent, C, Title, WS_EX_CONTROLPARENT, S,
                  Origin, Width, Height, Font, -1, "Panel");
    P.WndProc := GetWindowLong (P.Handle, GWL_WNDPROC);
    Long_Dummy := SetWindowLong (P.Handle, GWL_WNDPROC,
                                 To_LONG(Panel_Proc'Access));
    return W;
  end Panel;

  ----------------------------------------------------------------------------
  --
  --                    M E N U   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Menu: create a menu attached to a frame.
  --
  function Menu (Parent : Frame_Type'Class;
                 Text   : String) return Menu_Type is
    M : Menu_Type;
    H : Win32_HMENU;
    P : Window_Ptr := Get_Internals (Parent, "Menu");
    W : Window_Ptr := new Window_Internals;
    T : Win32_String := To_Array(Text);
  begin
    -- Get the frame's menu bar (and create it if it doesn't exist)

    H := GetMenu (P.Handle);
    if H = System.Null_Address then
      H := CreateMenu;
      Bool_Dummy := SetMenu (P.Handle, H);
    end if;

    -- Create a new menu and attach it to the menu bar

    W.Handle := CreateMenu;
    Bool_Dummy := AppendMenu(H, MF_POPUP, To_WPARAM(W.Handle), To_LPCSTR(T));

    -- Redraw the menu bar and return the menu object

    Bool_Dummy := DrawMenuBar (P.Handle);
    M.Internals.Pointer := Reference_Counted_Ptr(W);
    return M;
  end Menu;

  ----------------------------------------------------------------------------
  --
  --  Menu: create a menu attached to another menu.
  --
  function Menu (Parent : Menu_Type'Class;
                 Text   : String) return Menu_Type is
    M : Menu_Type;
    P : Window_Ptr := Get_Internals (Parent, "Menu");
    W : Window_Ptr := new Window_Internals;
    H : Win32_HWND := P.Handle;
    T : Win32_String := To_Array(Text);
  begin
    -- Create a new submenu and attach it to the parent menu

    W.Handle := CreateMenu;
    Bool_Dummy := AppendMenu(H, MF_POPUP, To_WPARAM(W.Handle), To_LPCSTR(T));

    -- Find the enclosing top-level window and redraw its menu bar

    while GetParent(H) /= System.Null_Address loop
      H := GetParent(H);
    end loop;
    Bool_Dummy := DrawMenuBar(H);

    -- Return the menu object

    M.Internals.Pointer := Reference_Counted_Ptr(W);
    return M;
  end Menu;

  ----------------------------------------------------------------------------
  --
  --  Menu_Height: return the height of a menubar.
  --
  function Menu_Height return Natural is
  begin
    return Natural (GetSystemMetrics(SM_CYMENU));
  end Menu_Height;

  ----------------------------------------------------------------------------
  --
  --                 C O N T R O L   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Enable: enable or disable a control.
  --
  procedure Enable (Control : in Control_Type;
                    Enabled : in Boolean := True) is
    P : Window_Ptr := Get_Internals (Control, "Enable");
  begin
    Bool_Dummy := EnableWindow (P.Handle, Win32_BOOL(Boolean'Pos(Enabled)));
  end Enable;

  ----------------------------------------------------------------------------
  --
  --  Disable: use Enable (above) to disable a control.
  --
  procedure Disable (Control : in Control_Type) is
    P : Window_Ptr := Get_Internals (Control, "Disable");
  begin
    Enable (Control_Type'Class(Control), False);
  end Disable;

  ----------------------------------------------------------------------------
  --
  --  Enabled: test if a control is enabled.
  --
  function Enabled (Control : Control_Type) return Boolean is
    P : Window_Ptr := Get_Internals (Control, "Enabled");
  begin
    return IsWindowEnabled(P.Handle) /= 0;
  end Enabled;

  ----------------------------------------------------------------------------
  --
  --            T E X T   C O N T R O L   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Get_Length: get the length of the text in a text control.
  --
  function Get_Length (Control : Text_Control_Type) return Natural is
    P : Window_Ptr := Get_Internals (Control, "Get_Length");
  begin
    return Natural(SendMessage(P.Handle, WM_GETTEXTLENGTH, 0, 0));
  end Get_Length;

  ----------------------------------------------------------------------------
  --
  --  Get_Text: get the text from a text control.
  --
  function Get_Text (Control : Text_Control_Type) return String is
    P : Window_Ptr := Get_Internals (Control, "Get_Text");
    L : Natural;
  begin
    declare
      A : Win32_String(1..Win32_SIZE(Get_Length(Control)+1)) := (others => ' ');
    begin
      L := Natural(SendMessage(P.Handle, WM_GETTEXT,
                               Win32_WPARAM(A'Length), To_LPARAM(A)));
      return To_String(A);
    end;
  end Get_Text;

  ----------------------------------------------------------------------------
  --
  --  Get_Text: get the text from a text control into a fixed-length
  --            string variable.
  --
  procedure Get_Text (Control : in  Text_Control_Type;
                      Text    : out String;
                      Length  : out Natural) is
    S : constant String := Get_Text (Control);
  begin
    if S'Length > Text'Length then
      Text := S(S'First..S'First+Text'Length-1);
      Length := Text'Length;
    else
      Text(Text'First..Text'First+S'Length-1) := S;
      Length := S'Length;
    end if;
  end Get_Text;

  ----------------------------------------------------------------------------
  --
  --  Set_Text: store the specified text in a text control.
  --
  procedure Set_Text (Control : in  Text_Control_Type;
                      Text    : in  String) is
    P : Window_Ptr := Get_Internals (Control, "Set_Text");
    T : Win32_String := To_Array(Text);
  begin
    Long_Dummy := SendMessage (P.Handle, WM_SETTEXT, 0, To_LPARAM(T));
  end Set_Text;

  ----------------------------------------------------------------------------
  --
  --                 B U T T O N   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Button: create a button as specified.
  --
  function Button (Parent  : Container_Type'Class;
                   Origin  : Point_Type;
                   Width   : Integer;
                   Height  : Integer;
                   Text    : String;
                   Command : Command_Type;
                   Default : Boolean := False;
                   Font    : Font_Type := Parent_Font) return Button_Type is
    W : Button_Type;
    P : Window_Ptr  := new Window_Internals;
    S : Win32_DWORD := WS_TABSTOP or WS_GROUP;
  begin
    if Default then
      S := S or BS_DEFPUSHBUTTON;
    else
      S := S or BS_PUSHBUTTON;
    end if;
    W.Internals.Pointer := Reference_Counted_Ptr(P);
    Create_Child (W, Parent, "button", Text,
                  0, S, Origin, Width, Height, Font,
                  Command_Type'Pos(Command)+WM_USER, "Button");
    return W;
  end Button;

  ----------------------------------------------------------------------------
  --
  --                  L A B E L   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Label: create a label as specified.
  --
  function Label (Parent : Container_Type'Class;
                  Origin : Point_Type;
                  Width  : Integer;
                  Height : Integer;
                  Text   : String;
                  Align  : Alignment_Type := Left;
                  Font   : Font_Type := Parent_Font) return Label_Type is
    W : Label_Type;
    P : Window_Ptr := new Window_Internals;
    S : Win32_DWORD := WS_GROUP or SS_NOPREFIX;
  begin
    if Align = Right then
      S := S or SS_RIGHT;
    elsif Align = Centre then
      S := S or SS_CENTER;
    end if;
    W.Internals.Pointer := Reference_Counted_Ptr(P);
    Create_Child (W, Parent, "static", Text, 0, S,
                  Origin, Width, Height, Font, -1, "Label");
    return W;
  end Label;

  ----------------------------------------------------------------------------
  --
  --                E D I T B O X   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Editbox: create an editbox as specified.
  --
  function Editbox (Parent   : Container_Type'Class;
                    Origin   : Point_Type;
                    Width    : Integer;
                    Height   : Integer;
                    Text     : String := "";
                    Password : Boolean := False;
                    Font     : Font_Type := Parent_Font) return Editbox_Type is
    W : Editbox_Type;
    P : Window_Ptr := new Window_Internals;
    E : Win32_DWORD := ES_AUTOHSCROLL or WS_BORDER or WS_TABSTOP or WS_GROUP;
  begin
    if Password then
      E := E or ES_PASSWORD;
    end if;
    W.Internals.Pointer := Reference_Counted_Ptr(P);
    Create_Child (W, Parent, "edit", Text, 0, E,
                  Origin, Width, Height, Font, -1, "Editbox");
    return W;
  end Editbox;

  ----------------------------------------------------------------------------
  --
  --  Modified: test if the user has modified the editbox since the last
  --            time this function was called.
  --
  function Modified (Editbox : Editbox_Type) return Boolean is
    P : Window_Ptr := Get_Internals(Editbox, "Modified");
    B : Boolean;
  begin
    B := SendMessage(P.Handle,EM_GETMODIFY,0,0) /= 0;
    Long_Dummy := SendMessage(P.Handle, EM_SETMODIFY, 0, 0);
    return B;
  end Modified;

  ----------------------------------------------------------------------------
  --
  --         B O O L E A N   C O N T R O L   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Get_State: test if a Boolean control is checked.
  --
  function Get_State (Control : Boolean_Control_Type) return Boolean is
    P : Window_Ptr := Get_Internals (Control, "Get_State");
  begin
    return SendMessage (P.Handle, BM_GETCHECK, 0, 0) = 1;
  end Get_State;

  ----------------------------------------------------------------------------
  --
  --  Set_State: set the state of a Boolean control as specified.
  --
  procedure Set_State (Control : in Boolean_Control_Type;
                       State   : in Boolean) is
    P : Window_Ptr := Get_Internals (Control, "Set_State");
  begin
    Long_Dummy := SendMessage (P.Handle, BM_SETCHECK,
                               Boolean'Pos(State), 0);
  end Set_State;

  ----------------------------------------------------------------------------
  --
  --                M E N U I T E M   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Menuitem: create a menuitem.
  --
  function Menuitem (Parent  : Menu_Type'Class;
                     Text    : String;
                     Command : Command_Type) return Menuitem_Type is
    M : Menuitem_Type;
    P : Window_Ptr := Get_Internals (Parent, "Menuitem");
    W : Window_Ptr := new Window_Internals;
    H : Win32_HWND := P.Handle;
    T : Win32_String := To_Array(Text);
  begin
    -- Set the command code and set the internal handle to be the parent
    -- handle (since Win32 menuitems are not real windows and do not have
    -- handles of their own)

    M.Internals.Pointer := Reference_Counted_Ptr(W);
    W.Handle := P.Handle;
    W.Action := Command_Type'Pos(Command);

    -- Add the menuitem to the parent menu

    Bool_Dummy := AppendMenu(P.Handle, MF_STRING,
                             Win32_UINT(W.Action+WM_USER),
                             To_LPCSTR(T));

    -- Find the enclosing top-level window and redraw its menu bar

    while GetParent(H) /= System.Null_Address loop
      H := GetParent(H);
    end loop;
    Bool_Dummy := DrawMenuBar(H);

    -- Return the menuitem object

    return M;
  end Menuitem;

  ----------------------------------------------------------------------------
  --
  --  Separator: create a separator for a menu.
  --
  function Separator (Parent : Menu_Type'Class) return Menuitem_Type is
    M : Menuitem_Type;
    P : Window_Ptr := Get_Internals (Parent, "Separator");
    W : Window_Ptr := new Window_Internals;
    H : Win32_HWND := P.Handle;
  begin
    -- Set the command code and set the internal handle to be the parent
    -- handle (since Win32 menuitems are not real windows and do not have
    -- handles of their own)

    M.Internals.Pointer := Reference_Counted_Ptr(W);
    W.Handle := P.Handle;
    W.Action := -1;

    -- Add the menuitem to the parent menu

    Bool_Dummy := AppendMenu(P.Handle, MF_STRING or MF_SEPARATOR, 0, null);

    -- Find the enclosing top-level window and redraw its menu bar

    while GetParent(H) /= System.Null_Address loop
      H := GetParent(H);
    end loop;
    Bool_Dummy := DrawMenuBar(H);

    -- Return the menuitem object

    return M;
  end Separator;

  ----------------------------------------------------------------------------
  --
  --  Enable: enable or disable a menu item using its command code.
  --
  procedure Enable (Control : in Menuitem_Type;
                    Enabled : in Boolean := True) is
    P : Window_Ptr := Get_Internals (Control, "Enable");
    E : Win32_UINT;
  begin
    if P.Action >= 0 then
      if Enabled then
        E := MF_BYCOMMAND or MF_ENABLED;
      else
        E := MF_BYCOMMAND or MF_GRAYED;
      end if;
      Bool_Dummy := EnableMenuItem (P.Handle,
                                    Win32_UINT(P.Action+WM_USER), E);
    end if;
  end Enable;

  ----------------------------------------------------------------------------
  --
  --  Enabled: test if a menu item is enabled using its command code.
  --
  function Enabled (Control : Menuitem_Type) return Boolean is
    P : Window_Ptr := Get_Internals (Control, "Enabled");
    S : Win32_UINT;
  begin
    if P.Action < 0 then
      return False;
    else
      S := GetMenuState (P.Handle,
                         Win32_UINT(P.Action+WM_USER), MF_BYCOMMAND);
      return (S and MF_DISABLED) = 0;
    end if;
  end Enabled;

  ----------------------------------------------------------------------------
  --
  --  Get_Length: get the length of the text in a menuitem.
  --
  function Get_Length (Control : Menuitem_Type) return Natural is
    P : Window_Ptr := Get_Internals (Control, "Get_Length");
  begin
    if P.Action < 0 then
      return 0;
    else
      return Natural(GetMenuString(P.Handle, Win32_UINT(P.Action+WM_USER),
                                   null, 0, MF_BYCOMMAND));
    end if;
  end Get_Length;

  ----------------------------------------------------------------------------
  --
  --  Get_Text: get the text from a menuitem.
  --
  function Get_Text (Control : Menuitem_Type) return String is
    P : Window_Ptr := Get_Internals (Control, "Get_Text");
    L : Natural;
  begin
    if P.Action < 0 then
      return "";
    else
      declare
        A : Win32_String(1..Win32_SIZE(Get_Length(Control)+1)) :=
                                                      (others => ' ');
      begin
        L := Natural(GetMenuString(P.Handle, Win32_UINT(P.Action+WM_USER),
                                   To_LPSTR(A), A'Length, MF_BYCOMMAND));
        return To_String(A);
      end;
    end if;
  end Get_Text;

  ----------------------------------------------------------------------------
  --
  --  Set_Text: store the specified text in a text control.
  --
  procedure Set_Text (Control : in  Menuitem_Type;
                      Text    : in  String) is
    P : Window_Ptr := Get_Internals (Control, "Set_Text");
    T : Win32_String := To_Array(Text);
    H : Win32_HWND;
  begin
    if P.Action >= 0 then      -- ignore menu separators
      Bool_Dummy := ModifyMenu (P.Handle, Win32_UINT(P.Action+WM_USER),
                                MF_BYCOMMAND or MF_STRING,
                                Win32_UINT(P.Action), To_LPCSTR(T));
      H := P.Handle;
      while GetParent(H) /= System.Null_Address loop
        H := GetParent(H);
      end loop;
      Bool_Dummy := DrawMenuBar(H);
    end if;
  end Set_Text;

  ----------------------------------------------------------------------------
  --
  --  Get_State: test if a menuitem is checked.
  --
  function Get_State (Control : Menuitem_Type) return Boolean is
    P : Window_Ptr := Get_Internals (Control, "Get_State");
  begin
    return (GetMenuState(P.Handle,Win32_UINT(P.Action+WM_USER),
                         MF_BYCOMMAND) and MF_CHECKED) /= 0;
  end Get_State;

  ----------------------------------------------------------------------------
  --
  --  Set_State: set the state of a menuitem as specified.
  --
  procedure Set_State (Control : in Menuitem_Type;
                       State   : in Boolean) is
    P : Window_Ptr := Get_Internals (Control, "Set_State");
    D : Win32_DWORD;
  begin
    if State then
      D := CheckMenuItem (P.Handle, Win32_UINT(P.Action+WM_USER),
                                    MF_BYCOMMAND or MF_CHECKED);
    else
      D := CheckMenuItem (P.Handle, Win32_UINT(P.Action+WM_USER),
                                    MF_BYCOMMAND or MF_UNCHECKED);
    end if;
  end Set_State;

  ----------------------------------------------------------------------------
  --
  --               C H E C K B O X   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Checkbox: create a checkbox with the specified initial state.
  --
  function Checkbox (Parent  : Container_Type'Class;
                     Origin  : Point_Type;
                     Width   : Integer;
                     Height  : Integer;
                     Text    : String;
                     Checked : Boolean := False;
                     Font    : Font_Type := Parent_Font)
                                                  return Checkbox_Type is
    W : Checkbox_Type;
    P : Window_Ptr := new Window_Internals;
  begin
    W.Internals.Pointer := Reference_Counted_Ptr(P);
    Create_Child (W, Parent, "button", Text, 0,
                  BS_AUTOCHECKBOX or WS_TABSTOP or WS_GROUP,
                  Origin, Width, Height, Font, -1, "Checkbox");
    Set_State (W, Checked);
    return W;
  end Checkbox;

  ----------------------------------------------------------------------------
  --
  --            R A D I O B U T T O N   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Radiobutton: create a radiobutton with the specified initial state.
  --
  function Radiobutton (Parent  : Container_Type'Class;
                        Origin  : Point_Type;
                        Width   : Integer;
                        Height  : Integer;
                        Text    : String;
                        Checked : Boolean   := False;
                        Font    : Font_Type := Parent_Font)
                                                  return Radiobutton_Type is
    W : Radiobutton_Type;
    P : Window_Ptr := new Window_Internals;
  begin
    W.Internals.Pointer := Reference_Counted_Ptr(P);
    Create_Child (W, Parent, "button", Text, 0,
                  BS_AUTORADIOBUTTON or WS_TABSTOP,
                  Origin, Width, Height, Font, -1, "Radiobutton");
    Set_State (W, Checked);
    return W;
  end Radiobutton;

  ----------------------------------------------------------------------------
  --
  --              M U L T I L I N E   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Get_Text: get the text of a specified line into a fixed-length
  --            string variable by dispatching to the appropriate
  --            Get_Text function.
  --
  procedure Get_Text  (Control : in  Multiline_Type;
                       Line    : in  Natural := 0;
                       Text    : out String;
                       Length  : out Natural) is
    S : constant String := Get_Text (Multiline_Type'Class(Control), Line);
  begin
    if S'Length > Text'Length then
      Text := S(S'First..S'First+Text'Length-1);
      Length := Text'Length;
    else
      Text(Text'First..Text'First+S'Length-1) := S;
      Length := S'Length;
    end if;
  end Get_Text;

  ----------------------------------------------------------------------------
  --
  --  Get_Actual_Line: convert a line number in a multiline control
  --                   (which may be zero or out-of-range) to an
  --                   absolute line number (internal use only).
  --
  function Get_Actual_Line (Control : in Multiline_Type'Class;
                            Line    : in Natural;
                            Name    : in String) return Natural is
    L : Natural := Line;
  begin
    if L > Get_Count(Control) then
      Raise_Exception (Constraint_Error'Identity,
                       External_Tag(Control'Tag) &
                       ": Line number out of range in " & Name);
    end if;
    if L = 0 then
      L := Get_Line(Control);
    end if;
    return L;
  end Get_Actual_Line;

  ----------------------------------------------------------------------------
  --
  --                L I S T B O X   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Listbox: create a listbox.
  --
  function Listbox (Parent : Container_Type'Class;
                    Origin : Point_Type;
                    Width  : Integer;
                    Height : Integer;
                    Font   : Font_Type := Parent_Font) return Listbox_Type is
    W : Listbox_Type;
    P : Window_Ptr := new Window_Internals;
  begin
    W.Internals.Pointer := Reference_Counted_Ptr(P);
    Create_Child (W, Parent, "listbox", "", 0,
                  WS_HSCROLL or WS_VSCROLL or WS_BORDER or
                  WS_TABSTOP or WS_GROUP,
                  Origin, Width, Height, Font, -1, "Listbox");
    return W;
  end Listbox;

  ----------------------------------------------------------------------------
  --
  --  Get_Count: get the number of lines in the listbox.
  --
  function Get_Count (Control : Listbox_Type) return Natural is
    P : Window_Ptr := Get_Internals (Control, "Get_Count");
  begin
    return Natural(SendMessage(P.Handle,LB_GETCOUNT,0,0));
  end Get_Count;

  ----------------------------------------------------------------------------
  --
  --  Get_Line: get the number of the current line (0 if no line is
  --            selected).
  --
  function Get_Line (Control : Listbox_Type) return Natural is
    P : Window_Ptr := Get_Internals (Control, "Get_Line");
  begin
    return Natural(SendMessage(P.Handle,LB_GETCURSEL,0,0) + 1);
  end Get_Line;

  ----------------------------------------------------------------------------
  --
  --  Get_Length: get the length of the specified line (0 if no line
  --              is selected).
  --
  function Get_Length (Control : Listbox_Type;
                       Line    : Natural := 0) return Natural is
    P : Window_Ptr := Get_Internals (Control, "Get_Length");
    L : Natural := Get_Actual_Line (Control, Line, "Get_Length");
  begin
    if L = 0 then
      return 0;
    else
      return Natural(SendMessage(P.Handle, LB_GETTEXTLEN,
                                 Win32_WPARAM(L)-1, 0));
    end if;
  end Get_Length;

  ----------------------------------------------------------------------------
  --
  --  Get_Text: get the text of the specified line (the empty string if
  --            the current line is specified and no line is selected).
  --
  function Get_Text (Control : Listbox_Type;
                     Line    : Natural := 0) return String is
    P : Window_Ptr := Get_Internals (Control, "Get_Text");
    L : Natural := Get_Actual_Line (Control, Line, "Get_Text");
  begin
    if L = 0 then
      return "";
    else
      declare
        A : Win32_String(1..Win32_SIZE(Get_Length(Control,L)+1)) :=
                                                           (others => ' ');
      begin
        L := Natural(SendMessage(P.Handle, LB_GETTEXT,
                                 Win32_WPARAM(L)-1, To_LPARAM(A)));
        return To_String(A);
      end;
    end if;
  end Get_Text;

  ----------------------------------------------------------------------------
  --
  --  Set_Text: set the text of the specified line (delete the current
  --            line and insert its replacement).
  --
  procedure Set_Text (Control : in Listbox_Type;
                      Text    : in String;
                      Line    : in Natural := 0) is
    L : Natural := Get_Actual_Line (Control, Line, "Set_Text");
  begin
    Delete_Line (Control, L);
    Insert_Line (Control, Text, L);
  end Set_Text;

  ----------------------------------------------------------------------------
  --
  --  Select_Line: set the line number for the current selection (deselect
  --               all lines if the line number is 0).
  --
  procedure Select_Line (Control : in Listbox_Type;
                         Line    : in Natural := 0) is
    P : Window_Ptr := Get_Internals (Control, "Select_Line");
  begin
    if Line > Get_Count(Control) then
      Raise_Exception (Constraint_Error'Identity,
                       External_Tag(Multiline_Type'Class(Control)'Tag) &
                       ": Line number out of range in Select_Line");
    end if;
    Long_Dummy := SendMessage(P.Handle, LB_SETCURSEL,
                              Win32_WPARAM(Line)-1, 0);
  end Select_Line;

  ----------------------------------------------------------------------------
  --
  --  Append_Line: add a line containing the specified line to the end
  --               of the listbox.
  --
  procedure Append_Line (Control : in Listbox_Type;
                         Text    : in String) is
    P : Window_Ptr := Get_Internals (Control, "Append_Line");
    T : Win32_String := To_Array(Text);
  begin
    Long_Dummy := SendMessage(P.Handle, LB_ADDSTRING, 0, To_LPARAM(T));
  end Append_Line;

  ----------------------------------------------------------------------------
  --
  --  Insert_Line: insert a new line above the specified line. If the real
  --               line number is zero (no current line), append the line
  --               as above.
  --
  procedure Insert_Line (Control : in Listbox_Type;
                         Text    : in String;
                         Line    : in Natural := 0) is
    P : Window_Ptr := Get_Internals (Control, "Insert_Line");
    L : Natural := Get_Actual_Line (Control, Line, "Insert_Line");
    T : Win32_String := To_Array(Text);
  begin
    if L = 0 then
      Append_Line (Control, Text);
    else
      Long_Dummy := SendMessage(P.Handle, LB_INSERTSTRING,
                                Win32_WPARAM(L)-1, To_LPARAM(T));
    end if;
  end Insert_Line;

  ----------------------------------------------------------------------------
  --
  --  Delete_Line: delete the specified line.
  --
  procedure Delete_Line (Control : in Listbox_Type;
                         Line    : in Natural := 0) is
    P : Window_Ptr := Get_Internals (Control, "Delete_Line");
    L : Natural := Get_Actual_Line (Control, Line, "Delete_Line");
  begin
    Long_Dummy := SendMessage(P.Handle, LB_DELETESTRING,
                              Win32_WPARAM(L)-1, 0);
  end Delete_Line;

  ----------------------------------------------------------------------------
  --
  --  Delete_All: delete all lines in the listbox.
  --
  procedure Delete_All (Control : in Listbox_Type) is
    P : Window_Ptr := Get_Internals (Control, "Delete_All");
  begin
    Long_Dummy := SendMessage(P.Handle, LB_RESETCONTENT, 0, 0);
  end Delete_All;

  ----------------------------------------------------------------------------
  --
  --               C O M B O B O X   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Combobox: create a combobox.
  --
  function Combobox (Parent   : Container_Type'Class;
                     Origin   : Point_Type;
                     Width    : Integer;
                     Editable : Boolean := True;
                     Font     : Font_Type := Parent_Font)
                                                  return Combobox_Type is
    W : Combobox_Type;
    P : Window_Ptr := new Window_Internals;
    S : Win32_DWORD := CBS_AUTOHSCROLL or WS_GROUP;
  begin
    if Editable then
      S := S or CBS_DROPDOWN;
    else
      S := S or CBS_DROPDOWNLIST;
    end if;
    W.Internals.Pointer := Reference_Counted_Ptr(P);
    Create_Child (W, Parent, "combobox", "", 0,
                  S or WS_HSCROLL or WS_VSCROLL or WS_BORDER or WS_TABSTOP,
                  Origin, Width, 120, Font, -1, "Combobox");
    return W;
  end Combobox;

  ----------------------------------------------------------------------------
  --
  --  Get_Count: get the number of lines in the combobox.
  --
  function Get_Count (Control : Combobox_Type) return Natural is
    P : Window_Ptr := Get_Internals (Control, "Get_Count");
  begin
    return Natural(SendMessage(P.Handle,CB_GETCOUNT,0,0));
  end Get_Count;

  ----------------------------------------------------------------------------
  --
  --  Get_Line: get the number of the current line (0 if no line is
  --            selected, or if the text in the editbox part of the
  --            control is not a string selected from the listbox
  --            part).
  --
  function Get_Line (Control : Combobox_Type) return Natural is
    P : Window_Ptr := Get_Internals (Control, "Get_Line");
  begin
    return Natural(SendMessage(P.Handle,CB_GETCURSEL,0,0) + 1);
  end Get_Line;

  ----------------------------------------------------------------------------
  --
  --  Get_Length: get the length of the specified line (0 if no line
  --              is selected).
  --
  function Get_Length (Control : Combobox_Type;
                       Line    : Natural := 0) return Natural is
    P : Window_Ptr := Get_Internals (Control, "Get_Length");
    L : Natural := Get_Actual_Line (Control, Line, "Get_Length");
  begin
    if L = 0 then
      return Natural(SendMessage(P.Handle, WM_GETTEXTLENGTH, 0, 0));
    else
      return Natural(SendMessage(P.Handle, CB_GETLBTEXTLEN,
                                 Win32_WPARAM(L)-1, 0));
    end if;
  end Get_Length;

  ----------------------------------------------------------------------------
  --
  --  Get_Text: get the text of the specified line (the text of the editbox
  --            part of the control if the line number is 0).
  --
  function Get_Text (Control : Combobox_Type;
                     Line    : Natural := 0) return String is
    P : Window_Ptr := Get_Internals (Control, "Get_Text");
    L : Natural := Get_Actual_Line (Control, Line, "Get_Text");
  begin
    declare
      A : Win32_String(1..Win32_SIZE(Get_Length(Control,L)+1)) :=
                                                      (others => ' ');
    begin
      if L = 0 then
        Long_Dummy := SendMessage(P.Handle, WM_GETTEXT,
                                  Win32_WPARAM(A'Length), To_LPARAM(A));
      else
        Long_Dummy := SendMessage(P.Handle, CB_GETLBTEXT,
                                  Win32_WPARAM(L-1), To_LPARAM(A));
      end if;
      return To_String(A);
    end;
  end Get_Text;

  ----------------------------------------------------------------------------
  --
  --  Set_Text: set the text of the specified line (delete the current
  --            line and insert its replacement).
  --
  procedure Set_Text (Control : in Combobox_Type;
                      Text    : in String;
                      Line    : in Natural := 0) is
    L : Natural := Get_Actual_Line (Control, Line, "Set_Text");
  begin
    Delete_Line (Control, L);
    if L > Get_Count(Control) then
      L := 0;
    end if;
    Insert_Line (Control, Text, L);
  end Set_Text;

  ----------------------------------------------------------------------------
  --
  --  Select_Line: set the line number for the current selection (deselect
  --               all lines if the line number is 0).
  --
  procedure Select_Line (Control : in Combobox_Type;
                         Line    : in Natural := 0) is
    P : Window_Ptr := Get_Internals (Control, "Select_Line");
    L : Natural := Get_Actual_Line (Control, Line, "Select_Line");
  begin
    Long_Dummy := SendMessage(P.Handle, CB_SETCURSEL,
                              Win32_WPARAM(Line)-1, 0);
  end Select_Line;

  ----------------------------------------------------------------------------
  --
  --  Append_Line: add a line containing the specified line to the end
  --               of the listbox part of the combobox.
  --
  procedure Append_Line (Control : in Combobox_Type;
                         Text    : in String) is
    P : Window_Ptr := Get_Internals (Control, "Append_Line");
    T : Win32_String := To_Array(Text);
  begin
    Long_Dummy := SendMessage(P.Handle, CB_ADDSTRING, 0, To_LPARAM(T));
  end Append_Line;

  ----------------------------------------------------------------------------
  --
  --  Insert_Line: insert a new line above the specified line. If the real
  --
  procedure Insert_Line (Control : in Combobox_Type;
                         Text    : in String;
                         Line    : in Natural := 0) is
    P : Window_Ptr := Get_Internals (Control, "Insert_Line");
    L : Natural := Get_Actual_Line (Control, Line, "Insert_Line");
    T : Win32_String := To_Array(Text);
  begin
    if L = 0 then
      Append_Line (Control, Text);
    else
      Long_Dummy := SendMessage(P.Handle, CB_INSERTSTRING,
                                Win32_WPARAM(L)-1, To_LPARAM(T));
    end if;
  end Insert_Line;

  ----------------------------------------------------------------------------
  --
  --  Delete_Line: delete the specified line.
  --
  procedure Delete_Line (Control : in Combobox_Type;
                         Line    : in Natural := 0) is
    P : Window_Ptr := Get_Internals (Control, "Delete_Line");
    L : Natural := Get_Actual_Line (Control, Line, "Delete_Line");
  begin
    if L = 0 then
      Select_Line (Control);
    else
      Long_Dummy := SendMessage(P.Handle, CB_DELETESTRING,
                                Win32_WPARAM(L)-1, 0);
    end if;
  end Delete_Line;

  ----------------------------------------------------------------------------
  --
  --  Delete_All: delete all lines in the combobox.
  --
  procedure Delete_All (Control : in Combobox_Type) is
    P : Window_Ptr := Get_Internals (Control, "Delete_All");
  begin
    Long_Dummy := SendMessage(P.Handle, CB_RESETCONTENT, 0, 0);
  end Delete_All;

  ----------------------------------------------------------------------------
  --
  --                   M E M O   O P E R A T I O N S
  --
  --  Memos are slightly peculiar because Windows always reports them as
  --  having at least one line, even when they're completely empty. I've
  --  decided that a blank last line won't count as a line -- a CR/LF at
  --  the end of a line is part of the line it ends, and only lines with
  --  characters in them should count. So there.
  --
  ----------------------------------------------------------------------------
  --
  --  Last_Line: returns character index of start of last line (for internal
  --             use only).
  --
  function Last_Line (Memo : in Win32_HWND) return Win32_LONG is
    L : Win32_LONG;
  begin
    L := SendMessage (Memo, EM_GETLINECOUNT, 0, 0);
    return SendMessage (Memo, EM_LINEINDEX, Win32_WPARAM(L-1), 0);
  end Last_Line;

  ----------------------------------------------------------------------------
  --
  --  Length: returns length of memo text (for internal use only).
  --
  function Length (Memo : in Win32_HWND) return Win32_LONG is
  begin
    return SendMessage (Memo, WM_GETTEXTLENGTH, 0, 0);
  end Length;

  ----------------------------------------------------------------------------
  --
  --  Memo: create a memo control as specified.
  --
  function Memo (Parent : Container_Type'Class;
                 Origin : Point_Type;
                 Width  : Integer;
                 Height : Integer;
                 Font   : Font_Type := Parent_Font) return Memo_Type is
    W : Memo_Type;
    P : Window_Ptr := new Window_Internals;
  begin
    W.Internals.Pointer := Reference_Counted_Ptr(P);
    Create_Child (W, Parent, "edit", "", WS_EX_CLIENTEDGE,
                  ES_MULTILINE or ES_WANTRETURN or ES_NOHIDESEL or
                  ES_AUTOHSCROLL or ES_AUTOVSCROLL or
                  WS_HSCROLL or WS_VSCROLL or
                  WS_TABSTOP or WS_GROUP,
                  Origin, Width, Height, Font, -1, "Memo");
    P.WndProc := GetWindowLong (P.Handle, GWL_WNDPROC);
    Long_Dummy := SetWindowLong (P.Handle, GWL_WNDPROC,
                                 To_LONG(Memo_Proc'Access));
    return W;
  end Memo;

  ----------------------------------------------------------------------------
  --
  --  Get_Column: find the column number where the caret is positioned.
  --
  function Get_Column (Memo : Memo_Type) return Natural is
    P : Window_Ptr := Get_Internals (Memo, "Get_Column");
    S : Integer;
    L : Win32_LONG;
  begin
    Long_Dummy := SendMessage (P.Handle, EM_GETSEL,
                               To_WPARAM(S'Address), 0);
    L := SendMessage (P.Handle, EM_LINEFROMCHAR,
                      Win32_WPARAM(S), 0);
    L := SendMessage(P.Handle, EM_LINEINDEX, Win32_WPARAM(L), 0);
    return S - Integer(L) + 1;
  end Get_Column;

  ----------------------------------------------------------------------------
  --
  --  Modified: test if the user has modified the memo since the last
  --            time this function was called.
  --
  function Modified (Memo : Memo_Type) return Boolean is
    P : Window_Ptr := Get_Internals(Memo, "Modified");
    B : Boolean;
  begin
    B := SendMessage(P.Handle,EM_GETMODIFY,0,0) /= 0;
    Long_Dummy := SendMessage(P.Handle, EM_SETMODIFY, 0, 0);
    return B;
  end Modified;

  ----------------------------------------------------------------------------
  --
  --  Cut_Selection: cut the current selection to the clipboard.
  --
  procedure Cut_Selection (Memo : in Memo_Type) is
    P : Window_Ptr := Get_Internals (Memo, "Cut_Selection");
  begin
    Long_Dummy := SendMessage (P.Handle, WM_CUT, 0, 0);
  end Cut_Selection;

  ----------------------------------------------------------------------------
  --
  --  Copy_Selection: copy the current selection to the clipboard.
  --
  procedure Copy_Selection (Memo : in Memo_Type) is
    P : Window_Ptr := Get_Internals (Memo, "Copy_Selection");
  begin
    Long_Dummy := SendMessage (P.Handle, WM_COPY, 0, 0);
  end Copy_Selection;

  ----------------------------------------------------------------------------
  --
  --  Paste_Selection: paste the clipboard over the current selection.
  --
  procedure Paste_Selection (Memo : in Memo_Type) is
    P : Window_Ptr := Get_Internals (Memo, "Paste_Selection");
  begin
    Long_Dummy := SendMessage (P.Handle, WM_PASTE, 0, 0);
  end Paste_Selection;

  ----------------------------------------------------------------------------
  --
  --  Undo_Change: undo the user's last change to the text of the memo.
  --
  procedure Undo_Change (Memo : in Memo_Type) is
    P : Window_Ptr := Get_Internals (Memo, "Undo_Change");
  begin
    Long_Dummy := SendMessage (P.Handle, WM_UNDO, 0, 0);
  end Undo_Change;

  ----------------------------------------------------------------------------
  --
  --  Show_Selection: scroll the memo so that the caret is in view.
  --
  procedure Show_Selection (Memo : in Memo_Type) is
    P : Window_Ptr := Get_Internals(Memo, "Show_Selection");
  begin
    Long_Dummy := SendMessage (P.Handle, EM_SCROLLCARET, 0, 0);
  end Show_Selection;

  ----------------------------------------------------------------------------
  --
  --  Get_Count: get the number of lines in the memo.
  --
  function Get_Count (Control : Memo_Type) return Natural is
    P : Window_Ptr := Get_Internals (Control, "Get_Count");
  begin
    return Natural(SendMessage(P.Handle, EM_GETLINECOUNT, 0, 0)) -
           Boolean'Pos(Last_Line(P.Handle) = Length(P.Handle));
  end Get_Count;

  ----------------------------------------------------------------------------
  --
  --  Get_Line: get the number of the line where the caret is positioned.
  --            Return zero if it's on a blank last line.
  --
  function Get_Line (Control : Memo_Type) return Natural is
    P : Window_Ptr := Get_Internals (Control, "Get_Line");
  begin
    if Last_Line(P.Handle) = Length(P.Handle) then
      return 0;
    else
      return Natural(SendMessage(P.Handle,EM_LINEFROMCHAR,-1,0)) + 1;
    end if;
  end Get_Line;

  ----------------------------------------------------------------------------
  --
  --  Get_Length: get the length of the specified line.
  --
  function Get_Length (Control : Memo_Type;
                       Line    : Natural := 0) return Natural is
    P : Window_Ptr := Get_Internals (Control, "Get_Length");
    L : Natural := Get_Actual_Line (Control, Line, "Get_Length");
    R : Win32_LONG;
  begin
    if L = 0 then
      return 0;
    else
      R := SendMessage (P.Handle, EM_LINEINDEX, Win32_WPARAM(L)-1, 0);
      return Natural(SendMessage(P.Handle,EM_LINELENGTH,Win32_WPARAM(R),0));
    end if;
  end Get_Length;

  ----------------------------------------------------------------------------
  --
  --  Get_Text: get the text of the specified line. Note that the EM_GETLINE
  --            message takes the line length in the first two bytes of the
  --            destination string, and no terminating null is copied (so
  --            the rest of the destination string must be initialised to
  --            nulls).
  --
  function Get_Text (Control : Memo_Type;
                     Line    : Natural := 0) return String is
    P : Window_Ptr := Get_Internals(Control, "Get_Text");
    L : Natural := Get_Actual_Line (Control, Line, "Get_Text");
    W : Natural;
  begin
    W := Get_Length (Control, L);
    if W = 0 then
      return "";
    else
      declare
        A : Win32_String(1..Win32_SIZE(W+1)) :=
                                     (1 => Win32_CHAR'Val(W mod 16#100#),
                                      2 => Win32_CHAR'Val(W / 16#100#),
                                      others => Win32_Char'Val(0));
      begin
        Long_Dummy := SendMessage(P.Handle, EM_GETLINE,
                                  Win32_WPARAM(L)-1, To_LPARAM(A));
        return To_String(A);
      end;
    end if;
  end Get_Text;

  ----------------------------------------------------------------------------
  --
  --  Set_Text: set the text of the specified line (select the line and
  --            replace the selection).
  --
  procedure Set_Text (Control : in Memo_Type;
                      Text    : in String;
                      Line    : in Natural := 0) is
    P : Window_Ptr := Get_Internals (Control, "Set_Text");
    L : Natural := Get_Actual_Line (Control, Line, "Set_Text");
    S : Win32_LONG;     -- start position (start of line)
    E : Win32_LONG;     -- end position (start of next line)
    T : Win32_String := To_Array(Text);
  begin
    if L = 0 then
      Append_Line (Control, Text);
    else
      S := SendMessage(P.Handle, EM_LINEINDEX, Win32_WPARAM(L)-1, 0);
      E := S + Win32_LONG(Get_Length(Control,L));
      Long_Dummy := SendMessage (P.Handle, EM_SETSEL,
                                 Win32_WPARAM(S), Win32_LPARAM(E));
      Long_Dummy := SendMessage (P.Handle, EM_REPLACESEL, 0,
                                 To_LPARAM(T));
    end if;
  end Set_Text;

  ----------------------------------------------------------------------------
  --
  --  Select_Line: set the line number for the caret position.
  --
  procedure Select_Line (Control : in Memo_Type;
                         Line    : in Natural := 0) is
    P : Window_Ptr := Get_Internals (Control, "Select_Line");
    L : Natural := Get_Actual_Line (Control, Line, "Select_Line");
    R : Win32_LONG;
  begin
    if L = 0 then
      R := Length(P.Handle);
    else
      R := SendMessage(P.Handle, EM_LINEINDEX, Win32_WPARAM(L)-1, 0);
    end if;
    Long_Dummy := SendMessage (P.Handle, EM_SETSEL,
                               Win32_WPARAM(R), Win32_LPARAM(R));
  end Select_Line;

  ----------------------------------------------------------------------------
  --
  --  Append_Line: add a line containing the specified line to the end
  --               of the memo. If the last line is not blank, add a
  --               preceding EOL to start a new line
  --
  procedure Append_Line (Control : in Memo_Type;
                         Text    : in String) is
    P : Window_Ptr := Get_Internals (Control, "Append_Line");
    C : Integer;
  begin
    C := Integer(Length(P.Handle));
    Long_Dummy := SendMessage (P.Handle, EM_SETSEL,
                               Win32_WPARAM(C), Win32_LPARAM(C));
    if Last_Line(P.Handle) = Length(P.Handle) then
      declare
        T : Win32_String := To_Array(Text);
      begin
        Long_Dummy := SendMessage (P.Handle, EM_REPLACESEL, 0, To_LPARAM(T));
      end;
    else
      declare
        T : Win32_String := To_Array(EOL & Text);
      begin
        Long_Dummy := SendMessage (P.Handle, EM_REPLACESEL, 0, To_LPARAM(T));
      end;
    end if;
  end Append_Line;

  ----------------------------------------------------------------------------
  --
  --  Insert_Line: insert a new line above the specified line. If the line
  --               number is zero, append the line as above.
  --
  procedure Insert_Line (Control : in Memo_Type;
                         Text    : in String;
                         Line    : in Natural := 0) is
    P : Window_Ptr := Get_Internals (Control, "Insert_Line");
    L : Natural := Get_Actual_Line (Control, Line, "Select_Line");
    T : Win32_String := To_Array(Text & EOL);
  begin
    if L = 0 then
      Append_Line (Control, Text);
    else
      Select_Line (Control, Line);
      Long_Dummy := SendMessage (P.Handle, EM_REPLACESEL, 0, To_LPARAM(T));
    end if;
  end Insert_Line;

  ----------------------------------------------------------------------------
  --
  --  Delete_Line: delete the specified line.
  --
  procedure Delete_Line (Control : in Memo_Type;
                         Line    : in Natural := 0) is
    P : Window_Ptr := Get_Internals (Control, "Delete_Line");
    L : Natural;
    S : Win32_LONG;
    E : Win32_LONG;
    N : Win32_String := To_Array("");
  begin
    L := Get_Actual_Line (Control, Line, "Delete_Line");
    if L > 0 then
      S := SendMessage(P.Handle, EM_LINEINDEX, Win32_WPARAM(L)-1, 0);
      E := SendMessage(P.Handle, EM_LINEINDEX, Win32_WPARAM(L), 0);
      if E < 0 then
        E := Length(P.Handle);
      end if;
      Long_Dummy := SendMessage (P.Handle, EM_SETSEL,
                                 Win32_WPARAM(S), Win32_LPARAM(E));
      Long_Dummy := SendMessage (P.Handle, EM_REPLACESEL, 0,
                                 To_LPARAM(N));
    end if;
  end Delete_Line;

  ----------------------------------------------------------------------------
  --
  --  Delete_All: delete all lines in the memo.
  --
  procedure Delete_All (Control : in Memo_Type) is
    P : Window_Ptr := Get_Internals (Control, "Delete_All");
    N : Win32_String := To_Array("");
  begin
    Long_Dummy := SendMessage (P.Handle, WM_SETTEXT, 0,
                               To_LPARAM(N));
  end Delete_All;

  ----------------------------------------------------------------------------
  --
  --                 C A N V A S   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Canvas: create a canvas window which does not generate a command.
  --
  function Canvas (Parent  : Container_Type'Class;
                   Origin  : Point_Type;
                   Width   : Integer;
                   Height  : Integer;
                   Font    : Font_Type := Parent_Font) return Canvas_Type is
    W : Canvas_Type;
    P : Canvas_Ptr := new Canvas_Internals;
  begin
    W.Internals.Pointer := Reference_Counted_Ptr(P);
    Create_Child (W, Parent, To_String(Canvas_Class), "",
                  0, WS_BORDER or WS_GROUP, 
                  Origin, Width, Height, Font, -1, "Canvas");
    Set_Fill (W);
    return W;
  end Canvas;

  ----------------------------------------------------------------------------
  --
  --  Canvas: create a canvas window which generates a command when the
  --          mouse button is pressed within it.
  --
  function Canvas (Parent  : Container_Type'Class;
                   Origin  : Point_Type;
                   Width   : Integer;
                   Height  : Integer;
                   Command : Command_Type;
                   Font    : Font_Type := Parent_Font) return Canvas_Type is
    W : Canvas_Type;
    P : Canvas_Ptr := new Canvas_Internals;
  begin
    W.Internals.Pointer := Reference_Counted_Ptr(P);
    Create_Child (W, Parent, To_String(Canvas_Class), "",
                  0, WS_BORDER or WS_GROUP,
                  Origin, Width, Height, Font,
                  Command_Type'Pos(Command), "Canvas");
    Set_Fill (W);
    return W;
  end Canvas;

  ----------------------------------------------------------------------------
  --
  --  Canvas: create a canvas window which generates a command when the
  --          mouse button or a key is pressed within it.
  --
  function Canvas (Parent   : Container_Type'Class;
                   Origin   : Point_Type;
                   Width    : Integer;
                   Height   : Integer;
                   Command  : Command_Type;
                   Keypress : Command_Type;
                   Font     : Font_Type := Parent_Font) return Canvas_Type is
    W : Canvas_Type;
    P : Canvas_Ptr := new Canvas_Internals;
  begin
    W.Internals.Pointer := Reference_Counted_Ptr(P);
    Create_Child (W, Parent, To_String(Canvas_Class), "",
                  0, WS_BORDER or WS_GROUP,
                  Origin, Width, Height, Font,
                  Command_Type'Pos(Command), "Canvas");
    P.Keypress := Command_Type'Pos(Keypress);
    Focus (W);
    Set_Fill (W);
    return W;
  end Canvas;

  ----------------------------------------------------------------------------
  --
  --  Set_Colour: ask the monitor to set the background colour.
  --
  procedure Set_Colour (Canvas : in Canvas_Type;
                        Colour : in Colour_Type := White) is
    B : Win32_COLORREF := RGB(Colour);
    C : Canvas_Ptr := Canvas_Ptr(Get_Internals(Canvas, "Set_Colour"));
  begin
    C.Monitor.Set_Brush (CreateSolidBrush(B));
    Bool_Dummy := InvalidateRect (C.Handle, null, 1);
  end Set_Colour;

  ----------------------------------------------------------------------------
  --
  --  Erase: ask the monitor to delete the drawing list and then redraw
  --         the window.
  --
  procedure Erase (Canvas : in Canvas_Type) is
    C : Canvas_Ptr := Canvas_Ptr(Get_Internals(Canvas, "Erase"));
  begin
    C.Monitor.Clear;
    Bool_Dummy := InvalidateRect (C.Handle, null, 1);
  end Erase;

  ----------------------------------------------------------------------------
  --
  --  Save: ask the monitor to save the current position in the drawing list.
  --
  procedure Save (Canvas : in Canvas_Type) is
    C : Canvas_Ptr := Canvas_Ptr(Get_Internals(Canvas, "Save"));
  begin
    C.Monitor.Save;
  end Save;

  ----------------------------------------------------------------------------
  --
  --  Restore: revert to a previously saved position in the drawing list
  --           (ignored if there is no saved position). This is safe because
  --           the list always grows unless it is erased, so the saved
  --           position will be valid until Erase is called, at which point
  --           the monitor will reset it to null.
  --
  procedure Restore (Canvas : in Canvas_Type) is
    C : Canvas_Ptr := Canvas_Ptr(Get_Internals(Canvas, "Restore"));
  begin
    C.Monitor.Restore;
    Bool_Dummy := InvalidateRect (C.Handle, null, 1);
  end Restore;

  ----------------------------------------------------------------------------
  --
  --  Set_Font: add a font handle to the drawing list.
  --
 procedure Set_Font (Canvas    : in Canvas_Type;
                     Font      : in Font_Type) is
    P : Canvas_Object_Ptr := new Handle_Type;
    H : Win32_HFONT       := Create_Font (Font);
  begin
    Handle_Type(P.all).Handle := Handle(H);
    Add (Canvas, "Set_Font", P);
  end Set_Font;

  ----------------------------------------------------------------------------
  --
  --  Set_Pen: add a pen handle to the drawing list.
  --
  procedure Set_Pen (Canvas : in Canvas_Type;
                     Colour : in Colour_Type := Black;
                     Width  : in Natural     := 1) is
    P : Canvas_Object_Ptr := new Handle_Type;
    S : Win32_COLORREF    := RGB(Colour);
  begin
    if Width > 0 then
      Handle_Type(P.all).Handle := Handle (CreatePen(0,Win32_INT(Width),S));
    else
      Handle_Type(P.all).Handle := Handle (GetStockObject(NULL_PEN));
    end if;
    Add (Canvas, "Set_Pen", P);
  end Set_Pen;

  ----------------------------------------------------------------------------
  --
  --  Set_Fill: add a solid brush handle to the drawing list.
  --
  procedure Set_Fill (Canvas : in Canvas_Type;
                      Colour : in Colour_Type) is
    P : Canvas_Object_Ptr := new Handle_Type;
    S : Win32_COLORREF    := RGB(Colour);
  begin
    Handle_Type(P.all).Handle := Handle (CreateSolidBrush(S));
    Add (Canvas, "Set_Fill", P);
  end Set_Fill;

  ----------------------------------------------------------------------------
  --
  --  Set_Fill: add a transparent brush handle to the drawing list.
  --
  procedure Set_Fill (Canvas : in Canvas_Type) is
    P : Canvas_Object_Ptr := new Handle_Type;
    L : aliased Win32_LOGBRUSH;
  begin
    L.lbStyle := BS_HOLLOW;
    Handle_Type(P.all).Handle :=
                        Handle (CreateBrushIndirect(L'Unchecked_Access));
    Add (Canvas, "Set_Fill", P);
  end Set_Fill;

  ----------------------------------------------------------------------------
  --
  --  Draw_Text: add a text string to the drawing with the top left
  --             corner at the specified point.
  --
  procedure Draw_Text (Canvas   : in Canvas_Type;
                       From     : in Point_Type;
                       Text     : in String) is
    P : Canvas_Object_Ptr  := new Text_Type (Text'Length);
  begin
    Text_Type(P.all).Text  := Text;
    Text_Type(P.all).From  := From;
    Text_Type(P.all).To    := From;
    Text_Type(P.all).Align := -1;
    Add (Canvas, "Draw_Text", P);
  end Draw_Text;

  ----------------------------------------------------------------------------
  --
  --  Draw_Text: add a text string to the drawing within a rectangle
  --             specified by diagonally opposite corners.
  --
  procedure Draw_Text (Canvas   : in Canvas_Type;
                       From     : in Point_Type;
                       To       : in Point_Type;
                       Text     : in String;
                       Align    : in Alignment_Type := Left) is
    P : Canvas_Object_Ptr  := new Text_Type (Text'Length);
  begin
    Text_Type(P.all).Text  := Text;
    Text_Type(P.all).From  := From;
    Text_Type(P.all).To    := To;
    Text_Type(P.all).Align := Alignment_Type'Pos(Align);
    Add (Canvas, "Draw_Text", P);
  end Draw_Text;

  ----------------------------------------------------------------------------
  --
  --  Draw_Text: calculate the text rectangle from a height and width.
  --
  procedure Draw_Text (Canvas   : in Canvas_Type;
                       From     : in Point_Type;
                       Width    : in Integer;
                       Height   : in Integer;
                       Text     : in String;
                       Align    : in Alignment_Type := Left) is
  begin
    Draw_Text (Canvas, From, (From.X+Width,From.Y+Height), Text, Align);
  end Draw_Text;

  ----------------------------------------------------------------------------
  --
  --  Draw_Line: add a line to the drawing between two points.
  --
  procedure Draw_Line (Canvas : in Canvas_Type;
                       From   : in Point_Type;
                       To     : in Point_Type) is
    P : Canvas_Object_Ptr := new Line_Type;
  begin
    Line_Type(P.all).From := From;
    Line_Type(P.all).To   := To;
    Add (Canvas, "Draw_Line", P);
  end Draw_Line;

  ----------------------------------------------------------------------------
  --
  --  Draw_Line: calculate the line endpoint from a length and angle.
  --
  procedure Draw_Line (Canvas : in Canvas_Type;
                       From   : in Point_Type;
                       Length : in Positive;
                       Angle  : in Angle_Type) is
    To : Point_Type;
  begin
    To := Endpoint(From,Length,Angle);
    Draw_Line (Canvas, From, To);
  end Draw_Line;

  ----------------------------------------------------------------------------
  --
  --  Draw_Line_List: add a polyline to the drawing. Ignore polylines with
  --                  less than two points, and draw an ordinary line for a
  --                  polyline with only two points.
  --
  procedure Draw_Line_List (Canvas : in Canvas_Type;
                            Points : in Point_List) is
    P : Window_Ptr := Get_Internals (Canvas, "Draw_Line_List");
  begin
    if Points'Length = 2 then
      Draw_Line (Canvas, Points(Points'First), Points(Points'Last));
    elsif Points'Length > 2 then
      declare
        P : Canvas_Object_Ptr := new Polyline_Type(Points'Length);
      begin
        P.Next := null;
        for I in 1..Points'Length loop
          Polyline_Type(P.all).Points(I) :=
                            (Win32_LONG(Points(Points'First+I-1).X),
                             Win32_LONG(Points(Points'First+I-1).Y));
        end loop;
        Add (Canvas, "Draw_Line_List", P);
      end;
    end if;
  end Draw_Line_List;

  ----------------------------------------------------------------------------
  --
  --  Draw_Rectangle: add either a normal rectangle or a rounded rectangle
  --                  to the drawing, depending on whether the rounding is
  --                  zero or not.
  --
  procedure Draw_Rectangle (Canvas   : in Canvas_Type;
                            From     : in Point_Type;
                            To       : in Point_Type;
                            Rounding : in Point_Type := (0,0)) is
    P : Canvas_Object_Ptr;
  begin
    if Rounding = (0,0) then
      P := new Rectangle_Type;
      Rectangle_Type(P.all).From := From;
      Rectangle_Type(P.all).To   := To;
    else
      P := new Rounded_Rectangle_Type;
      Rounded_Rectangle_Type(P.all).From   := From;
      Rounded_Rectangle_Type(P.all).To     := To;
      Rounded_Rectangle_Type(P.all).Corner := Rounding;
    end if;
    Add (Canvas, "Draw_Rectangle", P);
  end Draw_Rectangle;

  ----------------------------------------------------------------------------
  --
  --  Draw_Rectangle: calculate the rectangle size from a height and width.
  --
  procedure Draw_Rectangle (Canvas   : in Canvas_Type;
                            From     : in Point_Type;
                            Width    : in Positive;
                            Height   : in Positive;
                            Rounding : in Point_Type := (0,0)) is
  begin
    Draw_Rectangle (Canvas, From, (From.X+Width, From.Y+Height), Rounding);
  end Draw_Rectangle;

  ----------------------------------------------------------------------------
  --
  --  Draw_Ellipse: draw an ellipse whose size is specified by a bounding
  --                rectangle.
  --
  procedure Draw_Ellipse (Canvas : in Canvas_Type;
                          From   : in Point_Type;
                          To     : in Point_Type) is
    P : Canvas_Object_Ptr := new Ellipse_Type;
  begin
    Ellipse_Type(P.all).From := From;
    Ellipse_Type(P.all).To   := To;
    Add (Canvas, "Draw_Ellipse", P);
  end Draw_Ellipse;

  ----------------------------------------------------------------------------
  --
  --  Draw_Ellipse: calculate the bounding rectangle from a height and width.
  --
  procedure Draw_Ellipse (Canvas : in Canvas_Type;
                          From   : in Point_Type;
                          Width  : in Positive;
                          Height : in Positive) is
  begin
    Draw_Ellipse (Canvas, From, (From.X + Width, From.Y + Height));
  end Draw_Ellipse;

  ----------------------------------------------------------------------------
  --
  --  Draw_Circle: draw an ellipse in a bounding square calculated from
  --               the centre point and the radius.
  --
  procedure Draw_Circle (Canvas : in Canvas_Type;
                         Centre : in Point_Type;
                         Radius : in Positive) is
    P : Window_Ptr := Get_Internals (Canvas, "Draw_Circle");
  begin
    Draw_Ellipse (Canvas, (Centre.X - Radius,Centre.Y - Radius),
                          (Centre.X + Radius,Centre.Y + Radius));
  end Draw_Circle;

  ----------------------------------------------------------------------------
  --
  --  Draw_Polygon: create and fill a Windows-style array with the coordinates
  --                of the vertices, then add the polygon to the drawing list.
  --                Draw a line if there are only two vertices, and do nothing
  --                if there are less than two vertices.
  --
  procedure Draw_Polygon (Canvas : in Canvas_Type;
                          Points : in Point_List) is
    P : Window_Ptr := Get_Internals (Canvas, "Draw_Polygon");
  begin
    if Points'Length = 2 then
      Draw_Line (Canvas, Points(Points'First), Points(Points'Last));
    elsif Points'Length > 2 then
      declare
        P : Canvas_Object_Ptr := new Polygon_Type(Points'Length);
      begin
        P.Next := null;
        for I in 1..Points'Length loop
          Polygon_Type(P.all).Points(I) :=
                            (Win32_LONG(Points(Points'First+I-1).X),
                             Win32_LONG(Points(Points'First+I-1).Y));
        end loop;
        Add (Canvas, "Draw_Polygon", P);
      end;
    end if;
  end Draw_Polygon;

  ----------------------------------------------------------------------------
  --
  --  Draw_Image: draw the specified image on the canvas starting at the
  --              specified top-left corner point.
  --
  procedure Draw_Image (Canvas : in Canvas_Type;
                        From   : in Point_Type;
                        Image  : in Image_Type) is
    I : Image_Ptr;
  begin
    if Valid(Image) then
      I := Image_Ptr(Image.Internals.Pointer);
      Draw_Image (Canvas, From, I.Width, I.Height, Image);
    end if;
  end Draw_Image;

  ----------------------------------------------------------------------------
  --
  --  Draw_Image: draw the specified image on the canvas starting at the
  --              specified top-left corner point, stretching it to the
  --              specified bottom-right corner point.
  --
  procedure Draw_Image (Canvas   : in Canvas_Type;
                        From     : in Point_Type;
                        To       : in Point_Type;
                        Image    : in Image_Type) is
    L : Integer := Integer'Min(From.X,To.X);
    T : Integer := Integer'Min(From.Y,To.Y);
    R : Integer := Integer'Max(From.X,To.X);
    B : Integer := Integer'Max(From.Y,To.Y);
  begin
    Draw_Image (Canvas, (L,T), R-L, B-T, Image);
  end Draw_Image;

  ----------------------------------------------------------------------------
  --
  --  Draw_Image: draw the specified image on the canvas starting at the
  --              specified top-left corner point, stretching it to the
  --              specified width and height.
  --
  procedure Draw_Image (Canvas   : in Canvas_Type;
                        From     : in Point_Type;
                        Width    : in Natural;
                        Height   : in Natural;
                        Image    : in Image_Type) is
    P : Window_Ptr := Get_Internals (Canvas, "Draw_Image");
    B : Canvas_Object_Ptr := new Bitmap_Type;
    I : Image_Ptr;
  begin
    if Valid(Image) then
      I := Image_Ptr(Image.Internals.Pointer);
      Bitmap_Type(B.all).From   := From;
      Bitmap_Type(B.all).Width  := Width;
      Bitmap_Type(B.all).Height := Height;
      Bitmap_Type(B.all).Bitmap := Image.Internals;
      Add (Canvas, "Draw_Image", B);
    end if;
  end Draw_Image;

  ----------------------------------------------------------------------------
  --
  --  Mouse_Down: test whether the mouse was pressed within a specific
  --              canvas.
  --
  function Mouse_Down (Canvas : Canvas_Type) return Boolean is
    C : Canvas_Ptr := Canvas_Ptr(Get_Internals(Canvas, "Mouse_Down"));
  begin
    return C.Monitor.Mouse_Down;
  end Mouse_Down;

  ----------------------------------------------------------------------------
  --
  --  Mouse_Moved: test if the mouse has moved within a canvas, which
  --               will only be true while the mouse is down after being
  --               pressed inside this canvas.
  --
  function Mouse_Moved (Canvas : Canvas_Type) return Boolean is
    C : Canvas_Ptr := Canvas_Ptr(Get_Internals(Canvas, "Mouse_Moved"));
  begin
    return C.Monitor.Mouse_Moved;
  end Mouse_Moved;

  ----------------------------------------------------------------------------
  --
  --  Start_Point: get the position where the mouse was pressed within the
  --               specified canvas.
  --
  function Start_Point (Canvas : Canvas_Type) return Point_Type is
    C : Canvas_Ptr := Canvas_Ptr(Get_Internals(Canvas, "Start_Point"));
    P : Point_Type;
  begin
    C.Monitor.Get_Start (P.X, P.Y);
    return P;
  end Start_Point;

  ----------------------------------------------------------------------------
  --
  --  End_Point: get the latest mouse position within the specified canvas.
  --
  function End_Point (Canvas : Canvas_Type) return Point_Type is
    C : Canvas_Ptr := Canvas_Ptr(Get_Internals(Canvas, "End_Point"));
    P : Point_Type;
  begin
    C.Monitor.Get_End (P.X, P.Y);
    return P;
  end End_Point;

  ----------------------------------------------------------------------------
  --
  --  Key_Code: get the latest key position within the specified canvas.
  --
  function Key_Code (Canvas : Canvas_Type) return Character is
    C : Canvas_Ptr := Canvas_Ptr(Get_Internals(Canvas, "Key_Code"));
    K : Character;
  begin
    C.Monitor.Get_Key (K);
    return K;
  end Key_Code;

  ----------------------------------------------------------------------------
  --
  --           C O M M O N   D I A L O G   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Execute: execute a common dialog by asking the message loop to call
  --           its Show_Dialog primitive and return the result.
  --
  function Execute (Dialog : Common_Dialog_Type) return Boolean is
    P : Common_Dialog_Ptr := Get_Internals (Dialog, "Execute");
    B : Boolean;
  begin
    Message_Loop.Show_Dialog (P,B);
    return B;
  end Execute;

  ----------------------------------------------------------------------------
  --
  --  Colour_Dialog: construct a colour dialog.
  --
  function Colour_Dialog return Colour_Dialog_Type is
    D : Colour_Dialog_Type;
    P : Colour_Dialog_Ptr := new Colour_Dialog_Internals;
  begin
    D.Internals.Pointer := Reference_Counted_Ptr(P);
    P.Struct.lStructSize  := P.Struct'Size / Win32_BYTE'Size;
    P.Struct.lpCustColors := P.Custom(P.Custom'First)'Access;
    return D;
  end Colour_Dialog;

  ----------------------------------------------------------------------------
  --
  --  Set_Colour: set the colour stored in a colour dialog.
  --
  procedure Set_Colour (Dialog : in Colour_Dialog_Type;
                        Colour : in Colour_Type) is
    P : Colour_Dialog_Ptr := Colour_Dialog_Ptr(Get_Internals(Dialog,
                                                             "Set_Colour"));
  begin
    P.Colour := Colour;
  end Set_Colour;

  ----------------------------------------------------------------------------
  --
  --  Get_Colour: get the colour stored in a colour dialog.
  --
  function Get_Colour (Dialog : in Colour_Dialog_Type) return Colour_Type is
    P : Colour_Dialog_Ptr := Colour_Dialog_Ptr(Get_Internals(Dialog,
                                                             "Get_Colour"));
  begin
    return P.Colour;
  end Get_Colour;

  ----------------------------------------------------------------------------
  --
  --  Font_Dialog: construct a font dialog.
  --
  function Font_Dialog return Font_Dialog_Type is
    D : Font_Dialog_Type;
    P : Font_Dialog_Ptr := new Font_Dialog_Internals;
  begin
    D.Internals.Pointer := Reference_Counted_Ptr(P);
    P.Struct.lStructSize := P.Struct'Size / Win32_BYTE'Size;
    P.Struct.lpLogFont   := P.Font'Access;
    P.Font  := Set_Font(Font("Arial",9));
    return D;
  end Font_Dialog;

  ----------------------------------------------------------------------------
  --
  --  Set_Font: set the font stored in a font dialog.
  --
  procedure Set_Font (Dialog : in Font_Dialog_Type;
                      Font   : in Font_Type) is
    P : Font_Dialog_Ptr := Font_Dialog_Ptr(Get_Internals(Dialog,
                                                         "Set_Font"));
  begin
    P.Font := Set_Font(Font);
  end Set_Font;

  ----------------------------------------------------------------------------
  --
  --  Get_Font: get the font stored in a font dialog.
  --
  function Get_Font (Dialog : in Font_Dialog_Type) return Font_Type is
    P : Font_Dialog_Ptr := Font_Dialog_Ptr(Get_Internals(Dialog,
                                                         "Get_Font"));
  begin
    return Get_Font (P.Font);
  end Get_Font;

  ----------------------------------------------------------------------------
  --
  --  Set_Name: set the filename stored in a file dialog.
  --
  procedure Set_Name (Dialog : in File_Dialog_Type;
                      Name   : in String) is
    P : File_Dialog_Ptr := File_Dialog_Ptr(Get_Internals(Dialog,
                                                         "Set_Name"));
  begin
    if P.Buffer'Length > Name'Length then
      P.Buffer(P.Buffer'First..P.Buffer'First+Name'Length) := To_Array(Name);
    else
      P.Buffer := To_Array(Name(Name'First..Name'First+P.Buffer'Length-2));
    end if;
  end Set_Name;

  ----------------------------------------------------------------------------
  --
  --  Get_Name: get the filename stored in a file dialog.
  --
  function Get_Name (Dialog : in File_Dialog_Type) return String is
    P : File_Dialog_Ptr := File_Dialog_Ptr(Get_Internals(Dialog,
                                                         "Get_Name"));
  begin
    return To_String(P.Buffer);
  end Get_Name;

  ----------------------------------------------------------------------------
  --
  --  Add_Filter: add a filename filter to a file dialog.
  --
  procedure Add_Filter (Dialog : in File_Dialog_Type;
                        Text   : in String;
                        Filter : in String) is
    P : File_Dialog_Ptr := File_Dialog_Ptr(Get_Internals(Dialog,
                                                         "Add_Filter"));
  begin
    if P.Length + Text'Length + Filter'Length + 2 < P.Filter'Length then
      P.Filter (P.Filter'First+P.Length ..
                P.Filter'First+P.Length+Text'Length) := To_Array(Text);
      P.Length := P.Length + Text'Length + 1;
      P.Filter (P.Filter'First+P.Length ..
                P.Filter'First+P.Length+Filter'Length) := To_Array(Filter);
      P.Length := P.Length + Filter'Length + 1;
      P.Filter (P.Filter'First+P.Length) := Win32_CHAR'First;
      if P.Struct.lpstrFilter = null then
        P.Struct.lpstrFilter := To_LPCSTR(P.Filter);
      end if;
    end if;
  end Add_Filter;

  ----------------------------------------------------------------------------
  --
  --  Set_Directory: select the initial directory for a file dialog.
  --
  procedure Set_Directory (Dialog : in File_Dialog_Type;
                           Name   : in String) is
    P : File_Dialog_Ptr := File_Dialog_Ptr(Get_Internals(Dialog,
                                                         "Set_Directory"));
    L : Win32_SIZE := Win32_SIZE'Min(Name'Length,P.Directory'Length-1);
  begin
    P.Directory (P.Directory'First .. P.Directory'First+L) :=
                        To_Array(Name(Name'First..Name'First+Integer(L)-1));
    if P.Struct.lpstrInitialDir = null then
      P.Struct.lpstrInitialDir := To_LPCSTR(P.Directory);
    end if;
  end Set_Directory;

  ----------------------------------------------------------------------------
  --
  --  Open_Dialog: construct a file open dialog.
  --
  function Open_Dialog (Title : String) return Open_Dialog_Type is
    D : Open_Dialog_Type;
    P : File_Dialog_Ptr := new Open_Dialog_Internals (Title'Length+1);
  begin
    D.Internals.Pointer  := Reference_Counted_Ptr(P);
    P.Struct.lStructSize := P.Struct'Size / Win32_BYTE'Size;
    P.Struct.lpstrTitle  := To_LPCSTR(P.Title);
    P.Struct.lpstrFile   := P.Buffer(P.Buffer'First)'Access;
    P.Struct.nMaxFile    := P.Buffer'Length;
    P.Struct.Flags       := OFN_HIDEREADONLY or OFN_FILEMUSTEXIST or
                            OFN_PATHMUSTEXIST;
    P.Buffer(P.Buffer'First) := Win32_CHAR'Val(0);
    P.Title := To_Array(Title);
    return D;
  end Open_Dialog;

  ----------------------------------------------------------------------------
  --
  --  Save_Dialog: construct a file save dialog.
  --
  function Save_Dialog (Title  : String;
                        Create : Boolean := True) return Save_Dialog_Type is
    D : Save_Dialog_Type;
    P : File_Dialog_Ptr := new Save_Dialog_Internals (Title'Length+1);
  begin
    D.Internals.Pointer  := Reference_Counted_Ptr(P);
    P.Struct.lStructSize := P.Struct'Size / Win32_BYTE'Size;
    P.Struct.lpstrTitle  := To_LPCSTR(P.Title);
    P.Struct.lpstrFile   := P.Buffer(P.Buffer'First)'Access;
    P.Struct.nMaxFile    := P.Buffer'Length;
    P.Struct.Flags       := OFN_HIDEREADONLY or OFN_PATHMUSTEXIST;
    if Create then
      P.Struct.Flags := P.Struct.Flags or OFN_OVERWRITEPROMPT;
    else
      P.Struct.Flags := P.Struct.Flags or OFN_CREATEPROMPT;
    end if;
    P.Buffer(1) := Win32_CHAR'Val(0);
    P.Title := To_Array(Title);
    return D;
  end Save_Dialog;

------------------------------------------------------------------------------
--
--               P A C K A G E   I N I T I A L I S A T I O N
--
--    Register the window classes if there is no previous module instance.
--
------------------------------------------------------------------------------

begin
  if Get_hPrevInstance = System.Null_Address then
    declare
      Class : aliased Win32_WNDCLASS;
      Dummy : Win32_ATOM;
    begin

      -- Set up general window class information

      Class.Style         := CS_HREDRAW or CS_VREDRAW;
      Class.cbClsExtra    := 0;
      Class.cbWndExtra    := 0;
      Class.hInstance     := Get_hInstance;
      Class.hIcon         := LoadIcon(System.Null_Address,
                                      To_LPCSTR(IDI_APPLICATION));
      Class.hCursor       := LoadCursor(System.Null_Address,
                                        To_LPCSTR(IDC_ARROW));
      Class.hbrBackground := To_Handle(COLOR_BTNFACE+1);
      Class.lpszMenuName  := null;

      -- Set frame-specific information and register the frame class

      Class.lpszClassName := To_LPCSTR(Frame_Class);
      Class.lpfnWndProc   := Frame_Proc'Access;

      Dummy := RegisterClass(Class'Unchecked_Access);

      -- Set dialog-specific information and register the dialog class

      Class.lpszClassName := To_LPCSTR(Dialog_Class);
      Class.lpfnWndProc   := Dialog_Proc'Access;

      Dummy := RegisterClass(Class'Unchecked_Access);

      -- Set canvas-specific information and register the canvas class

      Class.lpszClassName := To_LPCSTR(Canvas_Class);
      Class.hbrBackground := System.Null_Address;
      Class.lpfnWndProc   := Canvas_Proc'Access;

      Dummy := RegisterClass(Class'Unchecked_Access);
    end;
  end if;
end JEWL.Windows;
