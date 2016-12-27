------------------------------------------------------------------------------
--                                                                          --
--                J E W L . M E S S A G E _ H A N D L I N G                 --
--                                                                          --
--   The body of a private package which defines the message-handling task  --
--   required by JEWL.Windows, the protected record used to communicate     --
--   with it, and related operations.                                       --
--                                                                          --
--   Copyright (C) John English 2000. Contact address: je@brighton.ac.uk    --
--   This software is released under the terms of the GNU General Public    --
--   License and is intended primarily for educational use. Please contact  --
--   the author to report bugs, suggestions and modifications.              --
--                                                                          --
------------------------------------------------------------------------------
-- $Id: jewl-message_handling.adb 1.7 2007/01/08 17:00:00 JE Exp $
------------------------------------------------------------------------------
--
-- $Log: jewl-message_handling.adb $
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

with Ada.Unchecked_Conversion;

package body JEWL.Message_Handling is

  use JEWL.Window_Implementation;
  use JEWL.Win32_Interface;
  use Ada.Exceptions;

  use type System.Address;
  use type Win32_BOOL, Win32_DWORD, Win32_LONG, Win32_UINT;

  -----------------------------------------------------------------------------
  --  Type conversions (needed below)
  -----------------------------------------------------------------------------

  function To_Window_Ptr is new Ada.Unchecked_Conversion
                                            (Win32_LONG,Window_Ptr);
  function To_LONG       is new Ada.Unchecked_Conversion
                                            (Window_Ptr,Win32_LONG);

  ----------------------------------------------------------------------------
  --
  --                        W I N D O W _ I N F O
  --
  ----------------------------------------------------------------------------

  protected body Window_Info is

    --------------------------------------------------------------------------
    --
    --  Get_Command: block until a command is available (or the message loop
    --               has failed) and then return it. The command is also reset.
    --               Program_Error is raised if the message loop has failed.
    --
    entry Get_Command (Cmd : out Natural)
                       when Command /= 0 or Task_Failed is
    begin
      if Task_Failed then
        Raise_Exception (Program_Error'Identity,
                         "caused by " & Exception_Name(Failure_Info) &
                         ": " & Exception_Message(Failure_Info));
      end if;
      Cmd := Command - WM_USER;
      Command := 0;
    end Get_Command;

    --------------------------------------------------------------------------
    --
    --  Test_Command: test if a command is pending. Program_Error is raised
    --                if the message loop task has failed.
    --
    function Test_Command return Boolean is
    begin
      if Task_Failed then
        Raise_Exception (Program_Error'Identity,
                         "caused by " & Exception_Name(Failure_Info) &
                         ": " & Exception_Message(Failure_Info));
      end if;
      return Command /= 0;
    end Test_Command;

    --------------------------------------------------------------------------
    --
    --  Set_Command: store the code for an available command. This is a
    --               procedure, not an entry, so the the message loop won't
    --               stall. If commands aren't handled in time, they'll be
    --               overwritten by the next one that comes along.
    --
    procedure Set_Command (Cmd : in Natural) is
    begin
      Command := Cmd;
    end Set_Command;

    --------------------------------------------------------------------------
    --
    --  Get_Dialog: swap the handle of the current active window with the
    --              parameter (i.e. record the new handle and return the
    --              old one).
    --
    procedure Get_Dialog (Dlg : in out Win32_HWND) is
      D : Win32_HWND := Dialog;
    begin
      Dialog := Dlg;
      Dlg := D;
    end Get_Dialog;

    --------------------------------------------------------------------------
    --
    --  Active_Window: get the handle of the current active window.
    --
    function Active_Window return Win32_HWND is
    begin
      return Dialog;
    end Active_Window;

    --------------------------------------------------------------------------
    --
    --  Record_Error : record the occurrence of an exception which aborted
    --                 the message loop task.
    --
    procedure Record_Error (Err : in Ada.Exceptions.Exception_Occurrence) is
    begin
      Task_Failed := True;
      Ada.Exceptions.Save_Occurrence (Failure_Info, Err);
    end Record_Error;

  end Window_Info;

  ----------------------------------------------------------------------------
  --
  --                  U T I L I T Y   F U N C T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Get_Actual_Bounds: check if any bounds supplied are non-positive
  --                     (i.e. relative to parent size) and recalculate
  --                     the bounds if so. Resize is set True if any
  --                     bounds have been updated.
  --
  procedure Get_Actual_Bounds (Parent : in Win32_HWND;
                               Top    : in out Integer;
                               Left   : in out Integer;
                               Width  : in out Integer;
                               Height : in out Integer;
                               Resize : out Boolean) is
    R : aliased Win32_RECT;
  begin
    -- Do any bounds need recalculationg?

    Resize := Top < 0 or Left < 0 or Width <= 0 or Height <= 0;

    if Resize then
      -- Top-level windows take their bounds from the screen, child windows
      -- take them from their parent

      if Parent = System.Null_Address then
        R := (0, 0, Win32_LONG(GetSystemMetrics(SM_CXSCREEN)),
                    Win32_LONG(GetSystemMetrics(SM_CYSCREEN)));
      else
        Bool_Dummy := GetClientRect (Parent, R'Unchecked_Access);
      end if;

      -- Recalculate left side relative to parent bounds if necessary

      if Left < 0 then
        Left := Left + Integer(R.Right);
      end if;

      -- Recalculate top relative to parent bounds if necessary

      if Top < 0 then
        Top := Top + Integer(R.Bottom);
      end if;

      -- Recalculate width relative to parent bounds if necessary

      if Width <= 0 then
        Width := Width + Integer(R.Right);
      end if;

      -- Recalculate height relative to parent bounds if necessary

      if Height <= 0 then
        Height := Height + Integer(R.Bottom);
      end if;

    end if;

  end Get_Actual_Bounds;

  ----------------------------------------------------------------------------
  --
  --  Build_Window: construct a top-level window of the specified class,
  --                with extended and normal styles as specified, and
  --                make the window visible or invisible as requested.
  --
  procedure Build_Window (Window : in Main_Window_Ptr;
                          Class  : in Win32_String;
                          Title  : in Win32_String;
                          XStyle : in Win32_DWORD;
                          Style  : in Win32_DWORD;
                          Show   : in Boolean) is
    S : Win32_DWORD := Style;
  begin
    -- Add the VISIBLE style if the window is to be shown

    if Show then
      S := S or WS_VISIBLE;
    end if;

    -- Create the window using the supplied parameters

    Window.Handle := CreateWindowEx (XStyle,
                                     To_LPCSTR(Class),
                                     To_LPCSTR(Title),
                                     S,
                                     Win32_INT(Window.Left),
                                     Win32_INT(Window.Top),
                                     Win32_INT(Window.Width),
                                     Win32_INT(Window.Height),
                                     System.Null_Address,
                                     System.Null_Address,
                                     Get_hInstance,
                                     Window.all'Address);

    -- Store a pointer to the Window_Internals in the user data area
    -- so that the message loop can find it

    Long_Dummy := SendMessage (Window.Handle, WM_SETFONT,
                               To_WPARAM(Window.Font),0);

    -- Show or hide the window as requested

    if Show then
      Bool_Dummy := ShowWindow (Window.Handle, SW_SHOWNORMAL);
    else
      Bool_Dummy := ShowWindow (Window.Handle, SW_HIDE);
    end if;

    -- Mark the window for repainting

    Bool_Dummy := UpdateWindow (Window.Handle);

  end Build_Window;

  ----------------------------------------------------------------------------
  --
  --  Build_Child: build a child window attached to the specified parent
  --               belonging to the specified class, using the specified
  --               title (caption text) as appropriate. Use the specified
  --               extended styles and normal styles, and set the window
  --               dimensions (relative to the parent's client area).
  --
  procedure Build_Child (Window : in Window_Ptr;
                         Parent : in Container_Ptr;
                         Class  : in Win32_String;
                         Title  : in Win32_String;
                         XStyle : in Win32_DWORD;
                         Style  : in Win32_DWORD;
                         Top    : in Integer;
                         Left   : in Integer;
                         Width  : in Integer;
                         Height : in Integer) is
    W : Window_Ptr := Window;
    H : Win32_HWND;
    S : Win32_DWORD := Style;
  begin
    -- The parent window's Group flag determines whether the WS_GROUP style
    -- should be applied to the child control in case it isn't already set.

    if Parent.Group then
      S := S or WS_GROUP;
    end if;

    -- All child windows except radiobuttons have WS_GROUP set anyway; the
    -- first radiobutton in a group also needs it set, so record if it was
    -- already set in the requested style to ensure that it will be applied
    -- to the next control regardless.

    Parent.Group := (Style and WS_GROUP) /= 0;

    -- Create the window using the supplied parameters

    Window.Handle := CreateWindowEx
                        (XStyle,
                         To_LPCSTR(Class), To_LPCSTR(Title),
                         S or WS_CHILD or WS_VISIBLE,
                         Win32_INT(Left), Win32_INT(Top),
                         Win32_INT(Width), Win32_INT(Height),
                         Parent.Handle, To_Handle(Window.Action),
                         Get_hInstance,
                         System.Null_Address);

    -- Store a pointer to the Window_Internals in the user data area
    -- so that the message loop can find it

    Long_Dummy := SetWindowLong (Window.Handle,
                                 GWL_USERDATA, To_LONG(Window));

    -- Scan the parent chain until a font is found if there isn't one
    -- for this window (i.e. Parent_Font has been used)

    while W.Font = System.Null_Address and W.Parent /= null loop
      W := Window_Ptr(W.Parent);
    end loop;

    -- If a font was found, select it

    if W.Font /= System.Null_Address then
      Long_Dummy := SendMessage (Window.Handle, WM_SETFONT,
                                 To_WPARAM(W.Font),0);
    end if;

    -- If this is a tabstop control (i.e. the tab key will activate it),
    -- find the top-level parent and check if it's the first tabstop in
    -- the window. If it is, give it the keyboard focus. The WM_ACTIVATE
    -- message handler will track it once it's been set. Don't explicitly
    -- set the focus unless the control is visible.

    if (Style and WS_TABSTOP) /= 0 then
      while W.Parent /= null loop
        W := Window_Ptr(W.Parent);              -- climb the parent chain
      end loop;
      if Main_Window_Ptr(W).Focus = System.Null_Address then

        -- At this point, we know that this is the first tabstop control
        -- to be attached to the top-level window W. Record the fact, and
        -- focus on this window if it's visible.

        Main_Window_Ptr(W).Focus := Window.Handle;
        if IsWindowVisible(W.Handle) /= 0 then
          H := SetFocus (Window.Handle);        -- set focus if visible
        end if;
      end if;
    end if;

  end Build_Child;

  ----------------------------------------------------------------------------
  --
  --                       M E S S A G E _ L O O P
  --
  --  This task drives the Windows message loop, and starts as soon as the
  --  package body is elaborated. It uses a global variable Frames to keep
  --  track of the number of top-level windows to allow it to terminate if
  --  there are no top-level windows open. The repetition of entry handlers
  --  is required because we need a terminate alternative (which rules out
  --  having an else part) and we also need to handle Windows messages (which
  --  must be done by the task that creates the windows, i.e. this one) which
  --  can only be done by checking for messages in an else part.
  --
  ----------------------------------------------------------------------------

  Frames : Integer := 0;    -- a global variable used by the message loop
                            -- to record the number of top-level windows.
  task body Message_Loop is
    M : aliased Win32_MSG;
    H : Win32_HWND;
  begin
    loop

      -- The following select statement is executed when there are no
      -- top level windows. The alternatives are to create a window,
      -- destroy a window, show a common dialog or to terminate. The
      -- destroy alternative is needed because the main window cleanup
      -- operation will call it if the window handle is still valid
      -- when the internal window structure is being finalized.

      select
        accept Create_Window (Window : in Main_Window_Ptr;
                              Class  : in Win32_String;
                              Title  : in Win32_String;
                              XStyle : in Win32_DWORD;
                              Style  : in Win32_DWORD;
                              Show   : in Boolean)
        do
          Build_Window (Window, Class, Title, XStyle, Style, Show);
          Frames := Frames + 1;
        end Create_Window;
      or
        accept Destroy_Window (Handle : in Win32_HWND)
        do
          Bool_Dummy := DestroyWindow (Handle);
        end Destroy_Window;
      or
        accept Show_Dialog (Dialog : in Common_Dialog_Ptr;
                            Result : out Boolean)
        do
          Result := Show_Dialog(Dialog);
        end Show_Dialog;
      or
        terminate;
      end select;

      -- Once the first top-level window has been created, this inner loop
      -- will accept requests to create top-level or child windows, to show
      -- common dialogs or to destroy windows. When there are no pending
      -- requests it will look to see if there is a pending Windows message
      -- and process it  if so. The loop ends when the last top-level window
      -- is destroyed, which then takes us back to the top of the outer loop
      -- to wait for a top-level window to be created or destroyed, or a
      -- termination request.

      while Frames > 0 loop
        select
          accept Create_Child (Window : in Window_Ptr;
                               Parent : in Container_Ptr;
                               Class  : in Win32_String;
                               Title  : in Win32_String;
                               XStyle : in Win32_DWORD;
                               Style  : in Win32_DWORD;
                               Top    : in Integer;
                               Left   : in Integer;
                               Width  : in Integer;
                               Height : in Integer)
          do
            Build_Child (Window, Parent, Class, Title, XStyle, Style,
                         Top, Left, Width, Height);
          end Create_Child;
        or
          accept Create_Window (Window : in Main_Window_Ptr;
                                Class  : in Win32_String;
                                Title  : in Win32_String;
                                XStyle : in Win32_DWORD;
                                Style  : in Win32_DWORD;
                                Show   : in Boolean)
          do
            Build_Window (Window, Class, Title, XStyle, Style, Show);
            Frames := Frames + 1;
          end Create_Window;
        or
          accept Set_Focus (Window : in Win32_HWND)
          do
            Long_Dummy := To_LONG (SetFocus(Window));
          end Set_Focus;
        or
          accept Destroy_Window (Handle : in Win32_HWND)
          do
            Bool_Dummy := DestroyWindow (Handle);
          end Destroy_Window;
        or
          accept Show_Dialog (Dialog : in Common_Dialog_Ptr;
                              Result : out Boolean)
          do
            Result := Show_Dialog(Dialog);
          end Show_Dialog;
        else

          -- If nothing else appeals: there is at least one window in
          -- existence, so try to pump any pending Windows messages

          while PeekMessage(M'Unchecked_Access, System.Null_Address,
                            0, 0, PM_REMOVE) /= 0 loop
            -- A message is pending, so find the top-level parent of the
            -- window it's aimed at

            H := M.hwnd;
            while GetParent(H) /= System.Null_Address loop
              H := GetParent(H);
            end loop;

            -- Now dispatch it in the classic Windows fashion, using the
            -- top-level window handle to ensure that dialog messages
            -- (TAB and other navigation keys) are translated before
            -- any other processing

            if IsDialogMessage (H, M'Unchecked_Access) = 0 then
              Bool_Dummy := TranslateMessage(M'Unchecked_Access);
              Long_Dummy := DispatchMessage(M'Unchecked_Access);
            end if;

          end loop;
          delay 0.001;        -- to avoid hogging the CPU
        end select;
      end loop;
    end loop;

  exception
    when E : others =>        -- task failure
      Window_Info.Record_Error (E);
  end Message_Loop;

  ----------------------------------------------------------------------------
  --
  --                 C A L L B A C K   F U N C T I O N S
  --
  --  These functions are called from the Windows message handler callbacks.
  --
  --  Enable (Window, Active): enable or disable the specified window
  --  Resize (Window, unused): recalculate the size of the specified window
  --
  ----------------------------------------------------------------------------

  function Enable (Window : Win32_HWND;
                   Active : Win32_LPARAM) return Win32_BOOL;
  pragma Convention(StdCall, Enable);

  function Resize (Window : Win32_HWND;
                   unused : Win32_LPARAM) return Win32_BOOL;
  pragma Convention(StdCall, Resize);

  ----------------------------------------------------------------------------
  --
  --  Enable Window to the opposite of its current state
  --
  function Enable (Window : Win32_HWND;
                   Active : Win32_LPARAM) return Win32_BOOL is
  begin
    if Window /= Window_Info.Active_Window then
      Bool_Dummy := EnableWindow (Window, Boolean'Pos(Active=0));
    end if;
    return 1;                               -- continue with next window
  end Enable;

  ----------------------------------------------------------------------------
  --
  --  Resize Window if necessary by reference to the size of its parent.
  --
  function Resize (Window : Win32_HWND; unused : Win32_LPARAM) return Win32_BOOL is
    P : Window_Ptr;
    T : Integer;
    L : Integer;
    W : Integer;
    H : Integer;
    B : Boolean;
    X : Win32_LONG := GetWindowLong(Window,GWL_USERDATA);
  begin

    -- Not all windows will have been created by this library, but all that
    -- haven't will have their user data (now in X) set to zero.

    if X /= 0 then

      -- X is really a Window_Ptr if we reach this point

      P := To_Window_Ptr(X);

      -- Copy the original (relative) coordinates
      
      T := P.Top;
      L := P.Left;
      W := P.Width;
      H := P.Height;

      -- Convert them to absolute (parent-based) coordinates

      Get_Actual_Bounds (GetParent(Window), T, L, W, H, B);

      -- B will have been set true if any of T/L/W/H are relative coordinates
      -- and T/L/W/H will have been set to absolute (parent-based) values, so
      -- resize the window if necessary

      if B then
        Bool_Dummy := SetWindowPos(Window, System.Null_Address,
                                           Win32_INT(L), Win32_INT(T),
                                           Win32_INT(W), Win32_INT(H),
                                           SWP_NOZORDER);
      end if;
    end if;

    return 1;                               -- continue with next window
  end Resize;

  ----------------------------------------------------------------------------
  --
  --                       F R A M E _ P R O C
  --
  --  The Windows message handler callback for Frame_Type windows.
  --
  ----------------------------------------------------------------------------

  function Frame_Proc (hwnd   : Win32_HWND;
                       msg    : Win32_UINT;
                       wParam : Win32_WPARAM;
                       lParam : Win32_LPARAM) return Win32_LONG is
    L : Win32_LONG := GetWindowLong(hwnd,GWL_USERDATA);
    P : Main_Window_Ptr := Main_Window_Ptr(To_Window_Ptr(L));
    H : Win32_HWND;
    N : Natural;
  begin
    case msg is

      -- Frame creation: a pointer to the Window_Internals is passed
      -- in the CREATESTRUCT pointed to by lParam, so save this for
      -- later use in the user data area of the Windows data structure
      -- (retrieved into P, above, which will be zero until initialised).

      when WM_CREATE =>
        L := To_LONG(To_CREATESTRUCT(lParam).lpCreateParams);
        Long_Dummy := SetWindowLong (hwnd, GWL_USERDATA, L);

      -- Frame activation or deactivation: save or restore the focused
      -- control for the next activation

      when WM_ACTIVATE =>
        if (wParam and 16#FFFF#) /= 0 then          -- activation
          H := SetFocus (P.Focus);
        elsif GetFocus /= System.Null_Address then  -- deactivation
          P.Focus := GetFocus;
        end if;
        return 0;                                   -- don't do anything else

      -- Frame closing: issue the command for this frame

      when WM_CLOSE =>
        Window_Info.Set_Command (P.Action + WM_USER);

      -- Frame being destroyed: decrement the frame count and clear the
      -- window handle

      when WM_DESTROY =>
        Frames := Frames - 1;

      -- Frame resized: resize all the child windows which have relative
      -- sizes or positions

      when WM_SIZE =>
        Bool_Dummy := EnumChildWindows (hwnd, Resize'Access, 0);

      -- An action has occurred (command code in low 16 bits of wParam);
      -- issue a command code if it's in the appropriate range, or ignore
      -- it if not

      when WM_COMMAND =>
        N := Natural(wParam and 16#FFFF#);
        if N in WM_USER .. 16#7FFF# then
          Window_Info.Set_Command (N);
          return 0;                                 -- don't do anything else
        end if;

      -- Ignore all other messages

      when others =>
        null;

    end case;

    -- Perform the default action for any messages that don't return before
    -- this point

    return DefWindowProc(hwnd, msg, wParam, lParam);

  end Frame_Proc;

  ----------------------------------------------------------------------------
  --
  --                      D I A L O G _ P R O C
  --
  --  The Windows message handler callback for Dialog_Type windows.
  --
  ----------------------------------------------------------------------------

  function Dialog_Proc (hwnd   : Win32_HWND;
                        msg    : Win32_UINT;
                        wParam : Win32_WPARAM;
                        lParam : Win32_LPARAM) return Win32_LONG is
    P : Window_Ptr := To_Window_Ptr(GetWindowLong(hwnd,GWL_USERDATA));
    H : Win32_HWND;
  begin
    case msg is

      -- Dialog deactivation: don't do anything more so that the current
      -- focused control isn't saved, and so the first control will get
      -- the focus the next time the dialog is activated

      when WM_ACTIVATE =>
        if (wParam and 16#FFFF#) = 0 then
          return 0;                                 -- don't do anything else
        end if;

      -- Dialog being shown/hidden: disable/enable all the other windows
      -- for this thread (i.e. this application), and set a new active
      -- window if the dialog is being hidden

      when WM_SHOWWINDOW =>
        Bool_Dummy := EnumThreadWindows (GetCurrentThreadID,
                                         Enable'Access,
                                         Win32_LONG(wParam));
        if wParam = 0 then
          H := SetActiveWindow(GetWindow(P.Handle,GW_HWNDNEXT));
        end if;

      -- Dialog closing: don't close, just hide

      when WM_CLOSE =>
        Bool_Dummy := ShowWindow (hwnd, SW_HIDE);
        Window_Info.Set_Command (P.Action + WM_USER);
        return 0;                                   -- don't do anything else

      -- Ignore all other messages

      when others =>
        null;

    end case;

    -- If nothing else has happened yet, treat this the same way as a
    -- Frame_Type top-level window

    return Frame_Proc(hwnd, msg, wParam, lParam);

  end Dialog_Proc;

  ----------------------------------------------------------------------------
  --
  --                      C A N V A S _ P R O C
  --
  --  The Windows message handler callback for Canvas_Type windows.
  --
  ----------------------------------------------------------------------------

  function Canvas_Proc (hwnd   : Win32_HWND;
                        msg    : Win32_UINT;
                        wParam : Win32_WPARAM;
                        lParam : Win32_LPARAM) return Win32_LONG is
    C : Canvas_Ptr := Canvas_Ptr(To_Window_Ptr(GetWindowLong(hwnd,GWL_USERDATA)));
    H : Win32_HWND;
    M : Win32_POINTS;
  begin
    case msg is
      -- Canvas background being erased: get the client area rectangle and
      -- fill it with the chosen background colour (from the canvas monitor)

      when WM_ERASEBKGND =>
        declare
          R : aliased Win32_RECT;
          I : Win32_INT;
        begin
          Bool_Dummy := GetClientRect(hwnd,R'Unchecked_Access);
          I := FillRect (To_HDC(wParam), R'Unchecked_Access,
                         C.Monitor.Background);
        end;
        return 1;     -- result of this message must be 1, not 0

      -- Canvas being painted: ask the monitor to draw all the objects in the
      -- drawing list

      when WM_PAINT =>
        C.Monitor.Draw (hwnd, C.Font);

      -- Mouse button down (ignored if this canvas does not generate a
      -- command): capture the mouse and ask the canvas monitor to record
      -- the mouse position and button state, then issue the command

      when WM_LBUTTONDOWN =>
        if C.Action >= 0 then
          H := SetCapture(hwnd);
          M := MakePoint (lParam);
          C.Monitor.Set_Start (Integer(M.X), Integer(M.Y));
          C.Monitor.Set_Button (True);
          Window_Info.Set_Command (C.Action + WM_USER);
          return 0;                                 -- don't do anything else
        end if;

      -- Mouse button up (ignored if this canvas does not generate a
      -- a command): release the mouse and ask the canvas monitor to
      -- record the current position and button state

      when WM_LBUTTONUP =>
        if C.Action >= 0 then
          Bool_Dummy := ReleaseCapture;
          M := MakePoint (lParam);
          C.Monitor.Set_End (Integer(M.X), Integer(M.Y));
          C.Monitor.Set_Button (False);
          return 0;                                 -- don't do anything else
        end if;

      -- Mouse has moved (ignored if this canvas does not generate
      -- a command or the mouse button isn't down): ask the canvas
      -- monitor to record the current mouse position

      when WM_MOUSEMOVE =>
        if C.Action >= 0 and C.Monitor.Mouse_Down then
          M := MakePoint (lParam);
          C.Monitor.Set_End (Integer(M.X), Integer(M.Y));
          return 0;                                 -- don't do anything else
        end if;

      -- Key pressed (ignored if this canvas does not generate a keypress
      -- command): capture the mouse and ask the canvas monitor to record
      -- the mouse position and button state, then issue the command

      when WM_CHAR =>
        if C.Keypress >= 0 then
          C.Monitor.Set_Key (Character'Val(wParam and 16#FF#));
          Window_Info.Set_Command (C.Keypress + WM_USER);
          return 0;                                 -- don't do anything else
        end if;

      -- Because messages are processed by IsDialogMessage, WM_CHAR messages
      -- won't be seen unless they are asked for in response to WM_GETDLCODE
      
      when WM_GETDLGCODE =>
        return DLGC_WANTMESSAGE;   -- request WM_CHAR for all keyboard input
        
      -- Ignore all other messages

      when others =>
        null;

    end case;
    
    -- Perform the default action for any messages that don't return before
    -- this point

    return DefWindowProc(hwnd, msg, wParam, lParam);

  end Canvas_Proc;

  ----------------------------------------------------------------------------
  --
  --                       P A N E L _ P R O C
  --
  --  The Windows message handler callback for Panel_Type windows. Command
  --  messages are sent to the parent window, all others are processed in
  --  the normal way.
  --
  ----------------------------------------------------------------------------

  function Panel_Proc (hwnd   : Win32_HWND;
                       msg    : Win32_UINT;
                       wParam : Win32_WPARAM;
                       lParam : Win32_LPARAM) return Win32_LONG is
    W : Window_Ptr := To_Window_Ptr(GetWindowLong(hwnd,GWL_USERDATA));
  begin
    case msg is
      when WM_COMMAND =>
        Long_Dummy := SendMessage (GetParent(hwnd), msg, wParam, lParam);
      when others =>
        null;
    end case;
    return CallWindowProc (W.WndProc, hwnd, msg, wParam, lParam);
  end Panel_Proc;

  ----------------------------------------------------------------------------
  --
  --                        M E M O _ P R O C
  --
  --  The Windows message handler callback for Memo_Type windows. Tab keys
  --  cause a tab to be inserted, all other messages are processed in the
  --  normal way.
  --
  ----------------------------------------------------------------------------

  function Memo_Proc (hwnd   : Win32_HWND;
                      msg    : Win32_UINT;
                      wParam : Win32_WPARAM;
                      lParam : Win32_LPARAM) return Win32_LONG is
    W : Window_Ptr := To_Window_Ptr(GetWindowLong(hwnd,GWL_USERDATA));
    S : aliased Win32_String := To_Array (ASCII.HT & ASCII.NUL);
  begin
    case msg is
      when WM_KEYDOWN =>
        if wParam = Character'Pos(ASCII.HT) then
          Long_Dummy := SendMessage (hwnd, EM_REPLACESEL, 1, To_LPARAM(S));
        end if;
      when others =>
        null;
    end case;
    return CallWindowProc (W.WndProc, hwnd, msg, wParam, lParam);
  end Memo_Proc;

end JEWL.Message_Handling;
