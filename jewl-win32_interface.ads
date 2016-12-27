------------------------------------------------------------------------------
--                                                                          --
--                 J E W L . W I N 3 2 _ I N T E R F A C E                  --
--                                                                          --
--   This is a private package containing definitions relating to the       --
--   use of the underlying Win32 API targetted by this implementation       --
--   of the JEWL.Windows package.                                           --
--                                                                          --
--   Copyright (C) John English 2000. Contact address: je@brighton.ac.uk    --
--   This software is released under the terms of the GNU General Public    --
--   License and is intended primarily for educational use. Please contact  --
--   the author to report bugs, suggestions and modifications.              --
--                                                                          --
------------------------------------------------------------------------------
-- $Id: jewl-win32_interface.ads 1.7 2007/01/08 17:00:00 JE Exp $
------------------------------------------------------------------------------
--
-- $Log: jewl-win32_interface.ads $
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
with System;
with Interfaces.C;

private package JEWL.Win32_Interface is

  use type Interfaces.C.Unsigned_Long;   -- reported by Pierre Breguet to be
                                         -- required by Object Ada compiler

  ----------------------------------------------------------------------------
  --  Win32 flags and constants
  ----------------------------------------------------------------------------

  BI_BITFIELDS           : constant := 3;
  BITMAP_MAGIC           : constant := 16#4D42#;        -- "BM"
  BLACK_PEN              : constant := 7;
  BM_GETCHECK            : constant := 240;
  BM_SETCHECK            : constant := 241;
  BS_AUTOCHECKBOX        : constant := 16#00000003#;
  BS_AUTORADIOBUTTON     : constant := 16#00000009#;
  BS_DEFPUSHBUTTON       : constant := 16#00000001#;
  BS_GROUPBOX            : constant := 16#00000007#;
  BS_HOLLOW              : constant := 1;
  BS_PUSHBUTTON          : constant := 16#00000000#;
  CBM_INIT               : constant := 4;
  CBS_AUTOHSCROLL        : constant := 16#00000040#;
  CBS_DROPDOWN           : constant := 16#00000002#;
  CBS_DROPDOWNLIST       : constant := 16#00000003#;
  CB_ADDSTRING           : constant := 323;
  CB_DELETESTRING        : constant := 324;
  CB_GETCOUNT            : constant := 326;
  CB_GETCURSEL           : constant := 327;
  CB_GETLBTEXT           : constant := 328;
  CB_GETLBTEXTLEN        : constant := 329;
  CB_INSERTSTRING        : constant := 330;
  CB_RESETCONTENT        : constant := 331;
  CB_SETCURSEL           : constant := 334;
  CC_RGBINIT             : constant := 16#00000001#;
  CF_FORCEFONTEXIST      : constant := 16#00010000#;
  CF_INITTOLOGFONTSTRUCT : constant := 16#00000040#;
  CF_SCREENFONTS         : constant := 16#00000001#;
  COLOR_BTNFACE          : constant := 15;
  CS_HREDRAW             : constant := 16#00000002#;
  CS_VREDRAW             : constant := 16#00000001#;
  CW_USEDEFAULT          : constant := Interfaces.C.Int'First;
  DIB_RGB_COLORS         : constant := 0;
  DLGC_WANTMESSAGE       : constant := 16#0004#;
  DT_CENTER              : constant := 16#00000001#;
  DT_LEFT                : constant := 16#00000000#;
  DT_NOCLIP              : constant := 16#00000100#;
  DT_RIGHT               : constant := 16#00000002#;
  EM_GETLINE             : constant := 196;
  EM_GETLINECOUNT        : constant := 186;
  EM_GETMODIFY           : constant := 184;
  EM_GETSEL              : constant := 176;
  EM_LINEFROMCHAR        : constant := 201;
  EM_LINEINDEX           : constant := 187;
  EM_LINELENGTH          : constant := 193;
  EM_REPLACESEL          : constant := 194;
  EM_SCROLLCARET         : constant := 183;
  EM_SETMODIFY           : constant := 185;
  EM_SETSEL              : constant := 177;
  EM_SETTABSTOPS         : constant := 203;
  ES_AUTOHSCROLL         : constant := 16#0080#;
  ES_AUTOVSCROLL         : constant := 16#0040#;
  ES_MULTILINE           : constant := 16#0004#;
  ES_NOHIDESEL           : constant := 16#0100#;
  ES_PASSWORD            : constant := 16#0020#;
  ES_WANTRETURN          : constant := 16#1000#;
  GWL_USERDATA           : constant := -21;
  GWL_WNDPROC            : constant := -4;
  GW_HWNDNEXT            : constant := 2;
  IDC_ARROW              : constant := 32512;
  IDI_APPLICATION        : constant := 32512;
  IDYES                  : constant := 6;
  LB_ADDSTRING           : constant := 384;
  LB_DELETESTRING        : constant := 386;
  LB_GETCOUNT            : constant := 395;
  LB_GETCURSEL           : constant := 392;
  LB_GETTEXT             : constant := 393;
  LB_GETTEXTLEN          : constant := 394;
  LB_INSERTSTRING        : constant := 385;
  LB_RESETCONTENT        : constant := 388;
  LB_SETCURSEL           : constant := 390;
  LOGPIXELSX             : constant := 88;
  LOGPIXELSY             : constant := 90;
  MB_ICONINFORMATION     : constant := 16#00000040#;
  MB_ICONQUESTION        : constant := 16#00000020#;
  MB_ICONSTOP            : constant := 16#00000010#;
  MB_OK                  : constant := 16#00000000#;
  MB_SETFOREGROUND       : constant := 16#00010000#;
  MB_TASKMODAL           : constant := 16#00002000#;
  MB_YESNO               : constant := 16#00000004#;
  MF_BYCOMMAND           : constant := 16#0000#;
  MF_CHECKED             : constant := 16#0008#;
  MF_DISABLED            : constant := 16#0002#;
  MF_ENABLED             : constant := 16#0000#;
  MF_GRAYED              : constant := 16#0001#;
  MF_POPUP               : constant := 16#0010#;
  MF_SEPARATOR           : constant := 16#0800#;
  MF_STRING              : constant := 16#0000#;
  MF_UNCHECKED           : constant := 16#0000#;
  NULL_BRUSH             : constant := 5;
  NULL_PEN               : constant := 8;
  OFN_CREATEPROMPT       : constant := 16#00002000#;
  OFN_FILEMUSTEXIST      : constant := 16#00001000#;
  OFN_HIDEREADONLY       : constant := 16#00000004#;
  OFN_OVERWRITEPROMPT    : constant := 16#00000002#;
  OFN_PATHMUSTEXIST      : constant := 16#00000800#;
  PM_REMOVE              : constant := 1;
  SM_CXDLGFRAME          : constant := 7;
  SM_CXEDGE              : constant := 45;
  SM_CXFRAME             : constant := 32;
  SM_CXSCREEN            : constant := 0;
  SM_CYCAPTION           : constant := 4;
  SM_CYDLGFRAME          : constant := 8;
  SM_CYEDGE              : constant := 46;
  SM_CYFRAME             : constant := 33;
  SM_CYMENU              : constant := 15;
  SM_CYSCREEN            : constant := 1;
  SND_ASYNC              : constant := 16#00000001#;
  SND_FILENAME           : constant := 16#00020000#;
  SND_NODEFAULT          : constant := 16#00000002#;
  SS_CENTER              : constant := 16#00000001#;
  SS_ETCHEDFRAME         : constant := 16#00000012#;
  SS_NOPREFIX            : constant := 16#00000080#;
  SS_RIGHT               : constant := 16#00000002#;
  SWP_NOMOVE             : constant := 16#00000002#;
  SWP_NOSIZE             : constant := 16#00000001#;
  SWP_NOZORDER           : constant := 16#00000004#;
  SW_HIDE                : constant := 0;
  SW_SHOW                : constant := 5;
  SW_SHOWNORMAL          : constant := 1;
  TRANSPARENT            : constant := 1;
  WM_ACTIVATE            : constant := 6;
  WM_CHAR                : constant := 258;
  WM_CLOSE               : constant := 16;
  WM_COMMAND             : constant := 273;
  WM_COPY                : constant := 769;
  WM_CREATE              : constant := 1;
  WM_CUT                 : constant := 768;
  WM_DESTROY             : constant := 2;
  WM_ERASEBKGND          : constant := 20;
  WM_GETDLGCODE          : constant := 135;
  WM_GETTEXT             : constant := 13;
  WM_GETTEXTLENGTH       : constant := 14;
  WM_KEYDOWN             : constant := 256;
  WM_LBUTTONDOWN         : constant := 513;
  WM_LBUTTONUP           : constant := 514;
  WM_MOUSEMOVE           : constant := 512;
  WM_PAINT               : constant := 15;
  WM_PASTE               : constant := 770;
  WM_SETFONT             : constant := 48;
  WM_SETTEXT             : constant := 12;
  WM_SHOWWINDOW          : constant := 24;
  WM_SIZE                : constant := 5;
  WM_UNDO                : constant := 772;
  WS_BORDER              : constant := 16#00800000#;
  WS_CHILD               : constant := 16#40000000#;
  WS_DLGFRAME            : constant := 16#00400000#;
  WS_EX_APPWINDOW        : constant := 16#00040000#;
  WS_EX_CLIENTEDGE       : constant := 16#00000200#;
  WS_EX_CONTROLPARENT    : constant := 16#00010000#;
  WS_GROUP               : constant := 16#00020000#;
  WS_HSCROLL             : constant := 16#00100000#;
  WS_OVERLAPPEDWINDOW    : constant := 16#00CF0000#;
  WS_SYSMENU             : constant := 16#00080000#;
  WS_TABSTOP             : constant := 16#00010000#;
  WS_VISIBLE             : constant := 16#10000000#;
  WS_VSCROLL             : constant := 16#00200000#;

  ----------------------------------------------------------------------------
  --  Win32 data types
  ----------------------------------------------------------------------------

  subtype Win32_ATOM           is Interfaces.C.Unsigned_Short;
  subtype Win32_BOOL           is Interfaces.C.Int;
  subtype Win32_BYTE           is Interfaces.C.Unsigned_Char;
  subtype Win32_CHAR           is Interfaces.C.Char;
  subtype Win32_DWORD          is Interfaces.C.Unsigned_Long;
  subtype Win32_WORD           is Interfaces.C.Unsigned_Short;
  subtype Win32_INT            is Interfaces.C.Int;
  subtype Win32_LONG           is Interfaces.C.Long;
  subtype Win32_SHORT          is Interfaces.C.Short;
  subtype Win32_UINT           is Interfaces.C.Unsigned;
  subtype Win32_WPARAM         is Interfaces.C.Unsigned;
  subtype Win32_LPARAM         is Interfaces.C.Long;
  subtype Win32_COLORREF       is Interfaces.C.Unsigned_Long;
  subtype Win32_SIZE           is Interfaces.C.Size_T;

  subtype Win32_String         is Interfaces.C.Char_Array;
  type    Win32_LPCSTR         is access constant Interfaces.C.Char;
  type    Win32_LPSTR          is access all Interfaces.C.Char;

  subtype Win32_LPVOID         is System.Address;
  subtype Win32_HANDLE         is System.Address;
  subtype Win32_HWND           is System.Address;
  subtype Win32_HBRUSH         is System.Address;
  subtype Win32_HBITMAP        is System.Address;
  subtype Win32_HDC            is System.Address;
  subtype Win32_HGDIOBJ        is System.Address;
  subtype Win32_HFONT          is System.Address;
  subtype Win32_HMENU          is System.Address;
  subtype Win32_HCURSOR        is System.Address;
  subtype Win32_HICON          is System.Address;
  subtype Win32_HPEN           is System.Address;
  subtype Win32_HINSTANCE      is System.Address;
  subtype Win32_HMODULE        is System.Address;

  type Win32_WNDPROC is
    access function (hWnd   : Win32_HWND;
                     Msg    : Win32_UINT;
                     wParam : Win32_WPARAM;
                     lParam : Win32_LPARAM) return Win32_LONG;
  pragma Convention (Stdcall, Win32_WNDPROC);

  type Win32_HOOKPROC is
    access function (hWnd   : Win32_HWND;
                     Msg    : Win32_UINT;
                     wParam : Win32_WPARAM;
                     lParam : Win32_LPARAM) return Win32_UINT;
  pragma Convention (Stdcall, Win32_HOOKPROC);

  type Win32_WNDENUMPROC is
    access function (hWnd   : Win32_HWND;
                     lParam : Win32_LPARAM) return Win32_BOOL;
  pragma Convention (Stdcall, Win32_WNDENUMPROC);

  type Win32_RECT is
    record
      Left   : Win32_LONG;
      Top    : Win32_LONG;
      Right  : Win32_LONG;
      Bottom : Win32_LONG;
    end record;

  type Win32_CREATESTRUCT is
    record
      lpCreateParams : Win32_HANDLE;
      hInstance      : Win32_HANDLE;
      hMenu          : Win32_HMENU;
      hWndParent     : Win32_HWND;
      CY             : Win32_INT;
      CX             : Win32_INT;
      Y              : Win32_INT;
      X              : Win32_INT;
      Style          : Win32_LONG;
      lpszName       : Win32_LPCSTR;
      lpszClass      : Win32_LPCSTR;
      dwExStyle      : Win32_DWORD;
    end record;

  type Win32_POINT is
    record
      X,Y : Win32_LONG;
    end record;

  type Win32_POINTS is
    record
      X,Y : Win32_SHORT;
    end record;

  type Win32_PAINTSTRUCT is
    record
      hDC        : Win32_HDC;
      fErase     : Win32_BOOL;
      rcPaint    : Win32_RECT;
      fRestore   : Win32_BOOL;
      fIncUpdate : Win32_BOOL;
      Reserved   : Win32_String (0..31);
    end record;

  type Win32_LOGBRUSH is
    record
      lbStyle : Win32_UINT;
      lbColor : Win32_COLORREF;
      lbHatch : Win32_LONG;
    end record;

  type Win32_LOGFONT is
    record
      lfHeight         : Win32_LONG;
      lfWidth          : Win32_LONG := 0;
      lfEscapement     : Win32_LONG := 0;
      lfOrientation    : Win32_LONG := 0;
      lfWeight         : Win32_LONG;
      lfItalic         : Win32_BYTE;
      lfUnderline      : Win32_BYTE := 0;
      lfStrikeOut      : Win32_BYTE := 0;
      lfCharSet        : Win32_BYTE := 0;
      lfOutPrecision   : Win32_BYTE := 0;
      lfClipPrecision  : Win32_BYTE := 0;
      lfQuality        : Win32_BYTE := 0;
      lfPitchAndFamily : Win32_BYTE := 0;
      lfFaceName       : Win32_String(0..31);
    end record;

  type Win32_MSG is
    record
      hWnd    : Win32_HWND;
      Message : Win32_UINT;
      wParam  : Win32_WPARAM;
      lParam  : Win32_LPARAM;
      Time    : Win32_DWORD;
      Point   : Win32_POINT;
    end record;

  type Win32_WNDCLASS is
    record
      Style         : Win32_UINT;
      lpfnWndProc   : Win32_WNDPROC;
      cbClsExtra    : Win32_INT;
      cbWndExtra    : Win32_INT;
      hInstance     : Win32_HINSTANCE;
      hIcon         : Win32_HICON;
      hCursor       : Win32_HCURSOR;
      hbrBackground : Win32_HBRUSH;
      lpszMenuName  : Win32_LPCSTR;
      lpszClassName : Win32_LPCSTR;
    end record;

    type Win32_BITMAPFILEHEADER is
      record
        bfType      : Win32_WORD;
        bfSize      : Win32_DWORD;
        bfReserved1 : Win32_WORD;
        bfReserved2 : Win32_WORD;
        bfOffBits   : Win32_DWORD;
      end record;

    type Win32_BITMAPINFOHEADER is
      record
        biSize          : Win32_DWORD;
        biWidth         : Win32_LONG;
        biHeight        : Win32_LONG;
        biPlanes        : Win32_WORD;
        biBitCount      : Win32_WORD;
        biCompression   : Win32_DWORD;
        biSizeImage     : Win32_DWORD;
        biXPelsPerMeter : Win32_LONG;
        biYPelsPerMeter : Win32_LONG;
        biClrUsed       : Win32_DWORD;
        biClrImportant  : Win32_DWORD;
      end record;

  type Win32_BITMAP is
    record
      bmType       : Win32_LONG;
      bmWidth      : Win32_LONG;
      bmHeight     : Win32_LONG;
      bmWidthBytes : Win32_LONG;
      bmPlanes     : Win32_WORD;
      bmBitsPixel  : Win32_WORD;
      bmBits       : Win32_LPVOID;
    end record;

  type Win32_LPRECT             is access all Win32_RECT;
  type Win32_LPCREATESTRUCT     is access all Win32_CREATESTRUCT;
  type Win32_LPPOINT            is access all Win32_POINT;
  type Win32_LPLOGBRUSH         is access all Win32_LOGBRUSH;
  type Win32_LPMSG              is access all Win32_MSG;
  type Win32_LPCOLORREF         is access all Win32_COLORREF;
  type Win32_LPLOGFONT          is access all Win32_LOGFONT;
  type Win32_LPBITMAPINFOHEADER is access all Win32_BITMAPINFOHEADER;
  type Win32_LPBITMAP           is access all Win32_BITMAP;

  type Win32_CHOOSEFONT is
    record
      lStructSize       : Win32_DWORD;
      hwndOwner         : Win32_HWND      := System.Null_Address;
      hDC               : Win32_HDC       := System.Null_Address;
      lpLogFont         : Win32_LPLOGFONT;
      iPointSize        : Win32_INT       := 0;
      Flags             : Win32_DWORD     := CF_SCREENFONTS or
                                             CF_FORCEFONTEXIST or    
                                             CF_INITTOLOGFONTSTRUCT;
      rgbColors         : Win32_COLORREF  := 0;
      lCustData         : Win32_LPARAM    := 0;
      lpfnHook          : Win32_HOOKPROC  := null;
      lpTemplateName    : Win32_LPCSTR    := null;
      hInstance         : Win32_HINSTANCE := System.Null_Address;
      lpszStyle         : Win32_LPSTR     := null;
      nFontType         : Win32_WORD      := 0;
      MISSING_ALIGNMENT : Win32_WORD      := 0;
      nSizeMin          : Win32_INT       := 0;
      nSizeMax          : Win32_INT       := 0;
    end record;

  type Win32_CHOOSECOLOR is
    record
        lStructSize    : Win32_DWORD;
        hwndOwner      : Win32_HWND       := System.Null_Address;
        hInstance      : Win32_HWND       := System.Null_Address;
        rgbResult      : Win32_COLORREF   := 0;
        lpCustColors   : Win32_LPCOLORREF;
        Flags          : Win32_DWORD      := CC_RGBINIT;
        lCustData      : Win32_LPARAM     := 0;
        lpfnHook       : Win32_HOOKPROC   := null;
        lpTemplateName : Win32_LPCSTR     := null;
    end record;

  type Win32_OPENFILENAME is
    record
      lStructSize       : Win32_DWORD;
      hWndOwner         : Win32_HWND      := System.Null_Address;
      hInstance         : Win32_HINSTANCE := System.Null_Address;
      lpstrFilter       : Win32_LPCSTR    := null;
      lpstrCustomFilter : Win32_LPSTR     := null;
      nMaxCustFilter    : Win32_DWORD     := 0;
      nFilterIndex      : Win32_DWORD     := 1;
      lpstrFile         : Win32_LPSTR;
      nMaxFile          : Win32_DWORD;
      lpstrFileTitle    : Win32_LPSTR     := null;
      nMaxFileTitle     : Win32_DWORD     := 0;
      lpstrInitialDir   : Win32_LPCSTR    := null;
      lpstrTitle        : Win32_LPCSTR;
      Flags             : Win32_DWORD;
      nFileOffset       : Win32_WORD;
      nFileExtension    : Win32_WORD;
      lpstrDefExt       : Win32_LPCSTR;
      lCustData         : Win32_LPARAM    := 0;
      lpfnHook          : Win32_HOOKPROC  := null;
      lpTemplateName    : Win32_LPCSTR    := null;
    end record;

  ----------------------------------------------------------------------------
  --  Dummy variables for unused results from Win32 functions.
  ----------------------------------------------------------------------------

  Bool_Dummy : Win32_BOOL;
  Long_Dummy : Win32_LONG;

  ----------------------------------------------------------------------------
  --  The start of the range of Windows message numbers used for commands.
  ----------------------------------------------------------------------------

  WM_USER  : constant := 16#0400#;

  ----------------------------------------------------------------------------
  --  Assorted type conversions
  ----------------------------------------------------------------------------

  function To_Handle       is new Ada.Unchecked_Conversion
                                            (Integer,System.Address);
  function To_LPCSTR       is new Ada.Unchecked_Conversion
                                            (Integer,Win32_LPCSTR);
  function To_Integer      is new Ada.Unchecked_Conversion
                                            (System.Address,Integer);
  function To_WPARAM       is new Ada.Unchecked_Conversion
                                            (System.Address,Win32_WPARAM);
  function To_HDC          is new Ada.Unchecked_Conversion
                                            (Win32_WPARAM,Win32_HDC);
  function To_LONG         is new Ada.Unchecked_Conversion
                                            (System.Address,Win32_LONG);
  function To_LONG         is new Ada.Unchecked_Conversion
                                            (Win32_WNDPROC, Win32_LONG);
  function To_CREATESTRUCT is new Ada.Unchecked_Conversion
                                            (Win32_LPARAM,
                                             Win32_LPCREATESTRUCT);
  function To_BMIH         is new Ada.Unchecked_Conversion
                                            (System.Address,
                                             Win32_LPBITMAPINFOHEADER);

  function To_LPSTR  (S : Win32_String) return Win32_LPSTR;
  function To_LPCSTR (S : Win32_String) return Win32_LPCSTR;
  function To_LPARAM (S : Win32_String) return Win32_LPARAM;
  function To_String (S : Win32_String) return String;
  function To_Array  (S : String)       return Win32_String;

  ----------------------------------------------------------------------------
  --  Other utility functions
  ----------------------------------------------------------------------------

  function  Message_Box (Message : String;
                         Title   : String;
                         Flags   : Win32_UINT)    return Integer;

  function  Create_Font (Font    : Font_Type)     return Win32_HFONT;
  function  Set_Font    (Font    : Font_Type)     return Win32_LOGFONT;
  function  Get_Font    (Font    : Win32_LOGFONT) return Font_Type;

  function  MakePoint   (Value   : Win32_LPARAM)  return Win32_POINTS;

  function  RGB         (Colour  : Colour_Type)   return Win32_COLORREF;

  ----------------------------------------------------------------------------
  --  Imports from Win32 and RTS libraries
  ----------------------------------------------------------------------------

  function AppendMenu          (hMenu      : Win32_HMENU;
                                uFlags     : Win32_UINT;
                                uIDNewItem : Win32_UINT;
                                lpNewItem  : Win32_LPCSTR)
                                     return Win32_BOOL;
  function BeginPaint          (hWnd    : Win32_HWND;
                                lpPaint : access Win32_PAINTSTRUCT)
                                     return Win32_HDC;
  function CallWindowProc      (lpPrevWndFunc : Win32_LONG;
                                hWnd          : Win32_HWND;
                                Msg           : Win32_UINT;
                                wParam        : Win32_WPARAM;
                                lParam        : Win32_LPARAM)
                                     return Win32_LONG;
  function CheckMenuItem       (hMenu        : Win32_HMENU;
                                uIDCheckItem : Win32_UINT;
                                uCheck       : Win32_UINT)
                                     return Win32_DWORD;
  function ChooseColor         (lpcc : access Win32_CHOOSECOLOR)
                                     return Win32_BOOL;
  function ChooseFont          (lpcf : access Win32_CHOOSEFONT)
                                     return Win32_BOOL;
  function CreateBrushIndirect (lplb : Win32_LPLOGBRUSH)
                                     return Win32_HBRUSH;
  function CreateCompatibleDC  (hdc : Win32_HDC)
                                     return Win32_HDC;
  function CreateDC            (lpszDriver : Win32_LPCSTR;
                                lpszDevice : Win32_LPCSTR;
                                lpszOutput : Win32_LPCSTR;
                                lpInitData : Win32_LPVOID)
                                     return Win32_HDC;
  function CreateDIBitmap      (hdc        : Win32_HDC;
                                lpbmih     : Win32_LPBITMAPINFOHEADER;
                                dwInit     : Win32_DWORD;
                                lpvBits    : Win32_LPVOID;
                                lpbmi      : Win32_LPVOID;
                                fnColorUse : Win32_UINT)
                                     return Win32_HBITMAP;
  function CreateDIBitmap      (lplb : Win32_LPLOGBRUSH)
                                     return Win32_HBRUSH;
  function CreateFontIndirect  (lplf : access Win32_LOGFONT)
                                     return Win32_HFONT;
  function CreateMenu                return Win32_HMENU;
  function CreatePen           (fnPenStyle : Win32_INT;
                                nWidth     : Win32_INT;
                                clrref     : Win32_COLORREF)
                                     return Win32_HPEN;
  function CreateSolidBrush    (clrref : Win32_COLORREF)
                                     return Win32_HBRUSH;
  function CreateWindowEx      (dwExStyle    : Win32_DWORD;
                                lpClassName  : Win32_LPCSTR;
                                lpWindowName : Win32_LPCSTR;
                                dwStyle      : Win32_DWORD;
                                X            : Win32_INT;
                                Y            : Win32_INT;
                                nWidth       : Win32_INT;
                                nHeight      : Win32_INT;
                                hWndParent   : Win32_HWND;
                                hMenu        : Win32_HMENU;
                                hInstance    : Win32_HINSTANCE;
                                lpParam      : Win32_LPVOID)
                                     return Win32_HWND;
  function DefWindowProc       (hWnd   : Win32_HWND;
                                Msg    : Win32_UINT;
                                wParam : Win32_WPARAM;
                                lParam : Win32_LPARAM)
                                     return Win32_LONG;
  function DeleteDC            (hdc : Win32_HDC)
                                     return Win32_BOOL;
  function DeleteObject        (hgdiobj : Win32_HGDIOBJ)
                                     return Win32_BOOL;
  function DestroyWindow       (hWnd : Win32_HWND)
                                     return Win32_BOOL;
  function DispatchMessage     (lpMsg : Win32_LPMSG)
                                     return Win32_LONG;
  function DPTOLP              (hdc      : Win32_HDC;
                                lpPoints : access Win32_POINT;
                                nCount   : Win32_INT)
                                     return Win32_BOOL;
  function DrawMenuBar         (hWnd : Win32_HWND)
                                     return Win32_BOOL;
  function DrawText            (hDC      : Win32_HDC;
                                lpString : Win32_LPCSTR;
                                nCount   : Win32_INT;
                                lpRect   : Win32_LPRECT;
                                uFormat  : Win32_UINT)
                                     return Win32_INT;
  function Ellipse             (hdc         : Win32_HDC;
                                nLeftRect   : Win32_INT;
                                nTopRect    : Win32_INT;
                                nRightRect  : Win32_INT;
                                nBottomRect : Win32_INT)
                                     return Win32_BOOL;
  function EnableMenuItem      (hMenu         : Win32_HMENU;
                                uIDEnableItem : Win32_UINT;
                                uEnable       : Win32_UINT)
                                     return Win32_BOOL;
  function EnableWindow        (hWnd    : Win32_HWND;
                                bEnable : Win32_BOOL)
                                     return Win32_BOOL;
  function EndPaint            (hWnd    : Win32_HWND;
                                lpPaint : access Win32_PAINTSTRUCT)
                                     return Win32_BOOL;
  function EnumChildWindows    (hWndParent : Win32_HWND;
                                lpEnumFunc : Win32_WNDENUMPROC;
                                lParam     : Win32_LPARAM)
                                     return Win32_BOOL;
  function EnumThreadWindows   (dwThreadId : Win32_DWORD;
                                lpfn       : Win32_WNDENUMPROC;
                                lParam     : Win32_LPARAM)
                                     return Win32_BOOL;
  function FillRect            (hDC  : Win32_HDC;
                                lprc : Win32_LPRECT;
                                hbr  : Win32_HBRUSH)
                                     return Win32_INT;
  function Get_hInstance             return Win32_HINSTANCE;
  function Get_hPrevInstance         return Win32_HINSTANCE;
  function GetActiveWindow           return Win32_HWND;
  function GetClientRect       (hWnd   : Win32_HWND;
                                lpRect : Win32_LPRECT)
                                     return Win32_BOOL;
  function GetCurrentThreadId        return Win32_DWORD;
  function GetDC               (hWnd : Win32_HWND)
                                     return Win32_HDC;
  function GetDeviceCaps       (hdc         : Win32_HDC;
                                iCapability : Win32_INT)
                                     return Win32_INT;
  function GetFocus                  return Win32_HWND;
  function GetMapMode          (hdc : Win32_HDC)
                                     return Win32_INT;
  function GetMenu             (hWnd : Win32_HWND)
                                     return Win32_HMENU;
  function GetMenuState        (hMenu  : Win32_HMENU;
                                uId    : Win32_UINT;
                                uFlags : Win32_UINT)
                                     return Win32_UINT;
  function GetMenuString       (hMenu     : Win32_HMENU;
                                uIDItem   : Win32_UINT;
                                lpString  : Win32_LPSTR;
                                nMaxCount : Win32_INT;
                                uFlag     : Win32_UINT)
                                     return Win32_INT;
  function GetObject           (hgdiobj   : Win32_HGDIOBJ;
                                cbBuffer  : Win32_INT;
                                lpvObject : Win32_LPVOID)
                                     return Win32_INT;
  function GetOpenFileName     (lpofn : access Win32_OPENFILENAME)
                                     return Win32_BOOL;
  function GetParent           (hWnd : Win32_HWND)
                                     return Win32_HWND;
  function GetSaveFileName     (lpofn : access Win32_OPENFILENAME)
                                     return Win32_BOOL;
  function GetStockObject      (fnObject : Win32_INT)
                                     return Win32_HGDIOBJ;
  function GetSystemMetrics    (nIndex : Win32_INT)
                                     return Win32_INT;
  function GetWindow           (hWnd : Win32_HWND;
                                uCmd : Win32_UINT)
                                     return Win32_HWND;
  function GetWindowLong       (hWnd   : Win32_HWND;
                                nIndex : Win32_INT)
                                     return Win32_LONG;
  function GetWindowRect       (hWnd   : Win32_HWND;
                                lpRect : Win32_LPRECT)
                                     return Win32_BOOL;
  function InvalidateRect      (hWnd   : Win32_HWND;
                                lpRect : Win32_LPRECT;
                                bErase : Win32_BOOL)
                                     return Win32_BOOL;
  function IsDialogMessage     (hDlg  : Win32_HWND;
                                lpMsg : access Win32_MSG)
                                     return Win32_BOOL;
  function IsWindow            (hWnd : Win32_HWND)
                                     return Win32_BOOL;
  function IsWindowEnabled     (hWnd : Win32_HWND)
                                     return Win32_BOOL;
  function IsWindowVisible     (hWnd : Win32_HWND)
                                     return Win32_BOOL;
  function LineTo              (hdc  : Win32_HDC;
                                xEnd : Win32_INT;
                                yEnd : Win32_INT)
                                     return Win32_BOOL;
  function LoadCursor          (hInstance    : Win32_HINSTANCE;
                                lpCursorName : Win32_LPCSTR)
                                     return Win32_HCURSOR;
  function LoadIcon            (hInstance  : Win32_HINSTANCE;
                                lpIconName : Win32_LPCSTR)
                                     return Win32_HICON;
  function MessageBox          (hWnd      : Win32_HWND;
                                lpText    : Win32_LPCSTR;
                                lpCaption : Win32_LPCSTR;
                                uType     : Win32_UINT)
                                     return Win32_INT;
  function ModifyMenu          (hMnu       : Win32_HMENU;
                                uPosition  : Win32_UINT;
                                uFlags     : Win32_UINT;
                                uIDNewItem : Win32_UINT;
                                lpNewItem  : Win32_LPCSTR)
                                     return Win32_BOOL;
  function MoveToEx            (hdc     : Win32_HDC;
                                X       : Win32_INT;
                                Y       : Win32_INT;
                                lpPoint : Win32_LPPOINT)
                                     return Win32_BOOL;
  function PeekMessage         (lpMsg         : access Win32_MSG;
                                hWnd          : Win32_HWND;
                                wMsgFilterMin : Win32_UINT;
                                wMsgFilterMax : Win32_UINT;
                                wRemoveMsg    : Win32_UINT)
                                     return Win32_BOOL;
  function PlaySound           (pszSound : Win32_LPCSTR;
                                hmod     : Win32_HMODULE;
                                fdwSound : Win32_DWORD)
                                     return Win32_BOOL;
  function Polygon             (hdc      : Win32_HDC;
                                lpPoints : Win32_LPPOINT;
                                nCount   : Win32_INT)
                                     return Win32_BOOL;
  function Polyline            (hdc     : Win32_HDC;
                                lppt    : Win32_LPPOINT;
                                cPoints : Win32_INT)
                                     return Win32_BOOL;
  function Rectangle           (hdc         : Win32_HDC;
                                nLeftRect   : Win32_INT;
                                nTopRect    : Win32_INT;
                                nRightRect  : Win32_INT;
                                nBottomRect : Win32_INT)
                                     return Win32_BOOL;
  function RegisterClass       (lpWndClass : access Win32_WNDCLASS)
                                     return Win32_ATOM;
  function ReleaseCapture            return Win32_BOOL;
  function ReleaseDC           (hWnd : Win32_HWND;
                                hDC  : Win32_HDC)
                                     return Win32_INT;
  function RoundRect           (hdc            : Win32_HDC;
                                nLeftRect      : Win32_INT;
                                nTopRect       : Win32_INT;
                                nRightRect     : Win32_INT;
                                nBottomRect    : Win32_INT;
                                nEllipseWidth  : Win32_INT;
                                nEllipseHeight : Win32_INT)
                                     return Win32_BOOL;
  function SelectObject        (hdc     : Win32_HDC;
                                hgdiobj : Win32_HGDIOBJ)
                                     return Win32_HGDIOBJ;
  function SendMessage         (hWnd   : Win32_HWND;
                                Msg    : Win32_UINT;
                                wParam : Win32_WPARAM;
                                lParam : Win32_LPARAM)
                                     return Win32_LONG;
  function SetActiveWindow     (hWnd : Win32_HWND)
                                     return Win32_HWND;
  function SetBkMode           (hdc      : Win32_HDC;
                                fnBkMode : Win32_INT)
                                     return Win32_INT;
  function SetCapture          (hWnd : Win32_HWND)
                                     return Win32_HWND;
  function SetFocus            (hWnd : Win32_HWND)
                                     return Win32_HWND;
  function SetForegroundWindow (hWnd : Win32_HWND)
                                     return Win32_BOOL;
  function SetMapMode          (hdc       : Win32_HDC;
                                fnmapMode : Win32_INT)
                                     return Win32_INT;
  function SetMenu             (hWnd  : Win32_HWND;
                                hMenu : Win32_HMENU)
                                     return Win32_BOOL;
  function SetWindowLong       (hWnd      : Win32_HWND;
                                nIndex    : Win32_INT;
                                dwNewLong : Win32_LONG)
                                     return Win32_LONG;
  function SetWindowPos        (hWnd            : Win32_HWND;
                                hWndInsertAfter : Win32_HWND;
                                X               : Win32_INT;
                                Y               : Win32_INT;
                                cx              : Win32_INT;
                                cy              : Win32_INT;
                                uFlags          : Win32_UINT)
                                     return Win32_BOOL;
  function ShowWindow          (hWnd     : Win32_HWND;
                                nCmdShow : Win32_INT)
                                     return Win32_BOOL;
  function StretchBlt          (hdcDest      : Win32_HDC;
                                nXOriginDest : Win32_INT;
                                nYOriginDest : Win32_INT;
                                nWidthDest   : Win32_INT;
                                nHeightDest  : Win32_INT;
                                hdcSrc       : Win32_HDC;
                                nXOriginSrc  : Win32_INT;
                                nYOriginSrc  : Win32_INT;
                                nWidthSrc    : Win32_INT;
                                nHeightSrc   : Win32_INT;
                                dwRop        : Win32_DWORD := 16#CC0020#)
                                     return Win32_BOOL;
  function TranslateMessage    (lpMsg : Win32_LPMSG)
                                     return Win32_BOOL;
  function UpdateWindow        (hWnd : Win32_HWND)
                                     return Win32_BOOL;

private     -- mappings to external libraries

  pragma Import (Stdcall, AppendMenu,           "AppendMenuA");
  pragma Import (Stdcall, BeginPaint,           "BeginPaint");
  pragma Import (Stdcall, CallWindowProc,       "CallWindowProcA");
  pragma Import (Stdcall, CheckMenuItem,        "CheckMenuItem");
  pragma Import (Stdcall, ChooseColor,          "ChooseColorA");
  pragma Import (Stdcall, ChooseFont,           "ChooseFontA");
  pragma Import (Stdcall, CreateBrushIndirect,  "CreateBrushIndirect");
  pragma Import (Stdcall, CreateCompatibleDC,   "CreateCompatibleDC");
  pragma Import (Stdcall, CreateDC,             "CreateDCA");
  pragma Import (Stdcall, CreateDIBitmap,       "CreateDIBitmap");
  pragma Import (Stdcall, CreateFontIndirect,   "CreateFontIndirectA");
  pragma Import (Stdcall, CreateMenu,           "CreateMenu");
  pragma Import (Stdcall, CreatePen,            "CreatePen");
  pragma Import (Stdcall, CreateSolidBrush,     "CreateSolidBrush");
  pragma Import (Stdcall, CreateWindowEx,       "CreateWindowExA");
  pragma Import (Stdcall, DefWindowProc,        "DefWindowProcA");
  pragma Import (Stdcall, DeleteDC,             "DeleteDC");
  pragma Import (Stdcall, DeleteObject,         "DeleteObject");
  pragma Import (Stdcall, DestroyWindow,        "DestroyWindow");
  pragma Import (Stdcall, DispatchMessage,      "DispatchMessageA");
  pragma Import (Stdcall, DPtoLP,               "DPtoLP");
  pragma Import (Stdcall, DrawMenuBar,          "DrawMenuBar");
  pragma Import (Stdcall, DrawText,             "DrawTextA");
  pragma Import (Stdcall, Ellipse,              "Ellipse");
  pragma Import (Stdcall, EnableMenuItem,       "EnableMenuItem");
  pragma Import (Stdcall, EnableWindow,         "EnableWindow");
  pragma Import (Stdcall, EndPaint,             "EndPaint");
  pragma Import (Stdcall, EnumChildWindows,     "EnumChildWindows");
  pragma Import (Stdcall, EnumThreadWindows,    "EnumThreadWindows");
  pragma Import (Stdcall, FillRect,             "FillRect");
  pragma Import (C      , Get_hInstance ,       "rts_get_hInstance");
  pragma Import (C      , Get_hPrevInstance ,   "rts_get_hPrevInstance");
  pragma Import (Stdcall, GetActiveWindow,      "GetActiveWindow");
  pragma Import (Stdcall, GetClientRect,        "GetClientRect");
  pragma Import (Stdcall, GetCurrentThreadId,   "GetCurrentThreadId");
  pragma Import (Stdcall, GetDC,                "GetDC");
  pragma Import (Stdcall, GetDeviceCaps,        "GetDeviceCaps");
  pragma Import (Stdcall, GetFocus,             "GetFocus");
  pragma Import (Stdcall, GetMapMode,           "GetMapMode");
  pragma Import (Stdcall, GetMenu,              "GetMenu");
  pragma Import (Stdcall, GetMenuState,         "GetMenuState");
  pragma Import (Stdcall, GetMenuString,        "GetMenuStringA");
  pragma Import (Stdcall, GetObject,            "GetObjectA");
  pragma Import (Stdcall, GetOpenFileName,      "GetOpenFileNameA");
  pragma Import (Stdcall, GetParent,            "GetParent");
  pragma Import (Stdcall, GetSaveFileName,      "GetSaveFileNameA");
  pragma Import (Stdcall, GetStockObject,       "GetStockObject");
  pragma Import (Stdcall, GetSystemMetrics,     "GetSystemMetrics");
  pragma Import (Stdcall, GetWindow,            "GetWindow");
  pragma Import (Stdcall, GetWindowLong,        "GetWindowLongA");
  pragma Import (Stdcall, GetWindowRect,        "GetWindowRect");
  pragma Import (Stdcall, InvalidateRect,       "InvalidateRect");
  pragma Import (Stdcall, IsDialogMessage,      "IsDialogMessage");
  pragma Import (Stdcall, IsWindow,             "IsWindow");
  pragma Import (Stdcall, IsWindowEnabled,      "IsWindowEnabled");
  pragma Import (Stdcall, IsWindowVisible,      "IsWindowVisible");
  pragma Import (Stdcall, LineTo,               "LineTo");
  pragma Import (Stdcall, LoadCursor,           "LoadCursorA");
  pragma Import (Stdcall, LoadIcon,             "LoadIconA");
  pragma Import (Stdcall, MessageBox,           "MessageBoxA");
  pragma Import (Stdcall, ModifyMenu,           "ModifyMenuA");
  pragma Import (Stdcall, MoveToEx,             "MoveToEx");
  pragma Import (Stdcall, PeekMessage,          "PeekMessageA");
  pragma Import (Stdcall, PlaySound,            "PlaySoundA");
  pragma Import (Stdcall, Polygon,              "Polygon");
  pragma Import (Stdcall, Polyline,             "Polyline");
  pragma Import (Stdcall, Rectangle,            "Rectangle");
  pragma Import (Stdcall, RegisterClass,        "RegisterClassA");
  pragma Import (Stdcall, ReleaseCapture,       "ReleaseCapture");
  pragma Import (Stdcall, ReleaseDC,            "ReleaseDC");
  pragma Import (Stdcall, RoundRect,            "RoundRect");
  pragma Import (Stdcall, SelectObject,         "SelectObject");
  pragma Import (Stdcall, SendMessage,          "SendMessageA");
  pragma Import (Stdcall, SetActiveWindow,      "SetActiveWindow");
  pragma Import (Stdcall, SetBkMode,            "SetBkMode");
  pragma Import (Stdcall, SetCapture,           "SetCapture");
  pragma Import (Stdcall, SetFocus,             "SetFocus");
  pragma Import (Stdcall, SetForegroundWindow,  "SetForegroundWindow");
  pragma Import (Stdcall, SetMapMode,           "SetMapMode");
  pragma Import (Stdcall, SetMenu,              "SetMenu");
  pragma Import (Stdcall, SetWindowLong,        "SetWindowLongA");
  pragma Import (Stdcall, SetWindowPos,         "SetWindowPos");
  pragma Import (Stdcall, ShowWindow,           "ShowWindow");
  pragma Import (Stdcall, StretchBlt,           "StretchBlt");
  pragma Import (Stdcall, TranslateMessage,     "TranslateMessage");
  pragma Import (Stdcall, UpdateWindow,         "UpdateWindow");

end JEWL.Win32_Interface;
