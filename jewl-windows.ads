------------------------------------------------------------------------------
--                                                                          --
--                         J E W L . W I N D O W S                          --
--                                                                          --
--   A package for developing GUI-based programs for beginners.             --
--                                                                          --
--   This is a large package, but splitting it into child packages would    --
--   require multiple generic instantiations in order to use it.            --
--                                                                          --
--   Copyright (C) John English 2000. Contact address: je@brighton.ac.uk    --
--   This software is released under the terms of the GNU General Public    --
--   License and is intended primarily for educational use. Please contact  --
--   the author to report bugs, suggestions and modifications.              --
--                                                                          --
------------------------------------------------------------------------------
-- $Id: jewl-windows.ads 1.7 2007/01/08 17:00:00 JE Exp $
------------------------------------------------------------------------------
--
-- $Log: jewl-windows.ads $
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

generic                         -- This is a generic package which must be
  type Command_Type is (<>);    -- instantiated with a list of possible
                                -- command values to be generated whenever
package JEWL.Windows is         -- a window is closed, a button is clicked
                                -- or a menu item is selected.

  ----------------------------------------------------------------------------
  --  Miscellaneous operations
  ----------------------------------------------------------------------------

  procedure Show_Error   (Text  : in String;   -- an error message
                          Title : in String := "Error");
  function  Show_Query   (Text  : in String;   -- a yes/no query
                          Title : in String := "Query")
                                return Boolean;
  procedure Show_Message (Text  : in String;   -- an information message
                          Title : in String := "Message");

  procedure Play_Sound   (Sound : in String);  -- play a sound file

  function  Screen_Width  return Natural;      -- width of display screen
  function  Screen_Height return Natural;      -- height of display screen

  ----------------------------------------------------------------------------
  --
  --                      S U P P O R T   T Y P E S
  --
  --  Except for Alignment_Type, these types are defined in the top-level
  --  package JEWL and are renamed here for convenience:
  --
  --  Alignment_Type  : used to specify text alignment with a window.
  --  Angle_Type      : an angle specified as an integral number of
  --                    degrees (0 to 359)
  --  Colour_Type     : a colour specified as an RGB value.
  --  Font_Type       : a font specified by a name, point size and bold
  --                    and italic style options.
  --  Point_Type      : a pair of (X,Y) coordinates within a window.
  --  Point_List      : an array of (X,Y) coordinate pairs.
  --
  ----------------------------------------------------------------------------

  type    Alignment_Type is (Left, Centre, Right);
  subtype Angle_Type     is JEWL.Angle_Type;
  subtype Colour_Range   is JEWL.Colour_Range;
  subtype Colour_Type    is JEWL.Colour_Type;
  subtype Font_Type      is JEWL.Font_Type;
  type    Image_Type     is private;
  subtype Point_Type     is JEWL.Point_Type;
  subtype Point_List     is JEWL.Point_List;

  ----------------------------------------------------------------------------
  --  Renamings for our transatlantic cousins, in the hope that some day
  --  they'll repay the favour/favor... ;-)
  ----------------------------------------------------------------------------

  subtype  Color_Range is Colour_Range;
  subtype  Color_Type  is Colour_Type;
  function Center      return Alignment_Type renames Centre;

  ----------------------------------------------------------------------------
  --
  --        O P E R A T I O N S   O N   S U P P O R T   T Y P E S
  --
  --  These are renamings of the operations defined in the parent package
  --  provided for convenience.
  --
  --  Colour operations:
  --    Light    : Generate a lightened version of a colour, e.g. Light(Red).
  --    Dark     : Generate a darkened version of a colour, e.g. Dark(Green).
  --
  --  Font operations:
  --    Font     : Generate a font with the specified properties.
  --    Name     : Get the name of the font typeface.
  --    Size     : Get the font size in points.
  --    Bold     : Test if the font is bold.
  --    Italic   : Test if the font is italic.
  --
  --  Image operations:
  --    Image    : Load an image from a bitmap file.
  --    Valid    : Test if an image is valid.
  --    Width    : Get the width of an image.
  --    Height   : Get the height of an image.
  --
  --  Point operations:
  --    Endpoint : Calculate the endpoint of a line from a starting point,
  --               length and angle
  --    Inside   : Test if a specified point is inside a specified rectangle
  --               (defined by the coordinates of two diagonally opposite
  --               corners).
  --    P1 + P2  : Add two points (P1.X+P2.X, P1.Y+P2.Y).
  --    P1 - P2  : Subtract two points (P1.X-P2.X, P1.Y-P2.Y).
  --
  ----------------------------------------------------------------------------

  function Light    (Colour : JEWL.Colour_Type) return  JEWL.Colour_Type
                                                renames JEWL.Light;
  function Dark     (Colour : JEWL.Colour_Type) return  JEWL.Colour_Type
                                                renames JEWL.Dark;

  function Font     (Name   : String;
                     Size   : Positive;
                     Bold   : Boolean := False;
                     Italic : Boolean := False) return  JEWL.Font_Type
                                                renames JEWL.Font;
  function Name     (Font   : Font_Type)        return String
                                                renames JEWL.Name;
  function Size     (Font   : Font_Type)        return Natural
                                                renames JEWL.Size;
  function Bold     (Font   : Font_Type)        return Boolean
                                                renames JEWL.Bold;
  function Italic   (Font   : Font_Type)        return Boolean
                                                renames JEWL.Italic;

  function Image    (Name   : String)           return Image_Type;
  function Valid    (Image  : Image_Type)       return Boolean;
  function Width    (Image  : Image_Type)       return Natural;
  function Height   (Image  : Image_Type)       return Natural;

  function Endpoint (From   : JEWL.Point_Type;
                     Length : Positive;
                     Angle  : JEWL.Angle_Type)  return  JEWL.Point_Type
                                                renames JEWL.Endpoint;
  function Inside   (Point  : JEWL.Point_Type;
                     From   : JEWL.Point_Type;
                     To     : JEWL.Point_Type)  return  Boolean
                                                renames JEWL.Inside;
  function "+"      (P1, P2 : Point_Type)       return Point_Type
                                                renames JEWL."+";
  function "-"      (P1, P2 : Point_Type)       return Point_Type
                                                renames JEWL."-";

  ----------------------------------------------------------------------------
  --
  --             S U P P O R T   T Y P E   C O N S T A N T S
  --
  --  Angles  : North, South, East and West
  --  Colours : Black, White, Red, Green, Blue, etc.
  --  Fonts   : Default_Font (the default font for top-level windows) and
  --            Parent_Font (the same font as a window's parent uses)
  --
  ----------------------------------------------------------------------------

  North        : constant Angle_Type  :=   0;
  South        : constant Angle_Type  := 180;
  East         : constant Angle_Type  :=  90;
  West         : constant Angle_Type  := 270;

  Black        : constant Colour_Type := (  0,  0,  0);
  White        : constant Colour_Type := (255,255,255);
  Red          : constant Colour_Type := (255,  0,  0);
  Green        : constant Colour_Type := (  0,255,  0);
  Blue         : constant Colour_Type := (  0,  0,255);
  Gray         : constant Colour_Type := (128,128,128);
  Yellow       : constant Colour_Type := (255,255,  0);
  Cyan         : constant Colour_Type := (  0,255,255);
  Magenta      : constant Colour_Type := (255,  0,255);

  Default_Font : constant Font_Type   := Font("Arial",9);
  Parent_Font  : constant Font_Type   := Font("",1);

  ----------------------------------------------------------------------------
  --
  --                        W I N D O W _ T Y P E
  --
  --  An abstract class providing basic behaviour which all windows share.
  --
  ----------------------------------------------------------------------------

  type Window_Type is abstract tagged private;

  Invalid_Window : exception;   -- raised if an attempt is made to use an
                                -- invalid (non-open) window

  ----------------------------------------------------------------------------
  --
  --  Window operations (common to all windows):
  --
  --  Show       (Window,   -- make the window visible or invisible depending
  --              Visible)  -- on the value of Visible (default: True).
  --  Hide       (Window)   -- make the window invisible.
  --  Focus      (Window)   -- give the window the input focus.
  --  Visible    (Window)   -- return True if the window is visible.
  --  Get_Origin (Window)   -- get the origin (top left point) of the
  --                        -- specified window.
  --  Get_Width  (Window)   -- get the width of the specified window.
  --  Get_Height (Window)   -- get the height of the specified window.
  --  Set_Origin (Window,   -- set the origin (top left point) of the
  --              Origin)   -- specified window to this value.
  --  Set_Size   (Window,   -- set the size of the specified window
  --              Width,    -- to this width (optional)
  --              Height)   -- and this height (optional).
  --  Get_Font   (Window)   -- get the font for the specified window.
  --  Set_Font   (Window,   -- set the font for the specified window
  --              Font)     -- to this one.
  --
  ----------------------------------------------------------------------------

  procedure Show       (Window  : in Window_Type;
                        Visible : in Boolean := True);
  procedure Hide       (Window  : in Window_Type);
  procedure Focus      (Window  : in Window_Type);
  function  Visible    (Window  : Window_Type) return Boolean;

  function  Get_Origin (Window  : Window_Type) return Point_Type;
  function  Get_Width  (Window  : Window_Type) return Natural;
  function  Get_Height (Window  : Window_Type) return Natural;

  procedure Set_Origin (Window  : in Window_Type;
                        Origin  : in Point_Type);
  procedure Set_Size   (Window  : in Window_Type;
                        Width   : in Natural := 0;
                        Height  : in Natural := 0);

  function  Get_Font   (Window  : Window_Type) return Font_Type;
  procedure Set_Font   (Window  : in Window_Type;
                        Font    : in Font_Type);

  ----------------------------------------------------------------------------
  --
  --                  W I N D O W   S U B C L A S S E S
  --
  --  The primary window subclasses are containers and controls.  They
  --  share the behaviour common to all windows (above) and provide extra
  --  behaviour as well.
  --
  --  Container_Type : the abstract base type for all containers.
  --  Control_Type   : the abstract base type for all controls.
  --
  ----------------------------------------------------------------------------

  type Container_Type is abstract new Window_Type with private;
  type Control_Type   is abstract new Window_Type with private;

  ----------------------------------------------------------------------------
  --
  --                         C O N T A I N E R S
  --
  --  Containers are windows which can contain other windows. All windows
  --  except frames and dialogs (see below) must be contained within some
  --  other container window. There are some restrictions on the types of
  --  container that a window can be attached to (for example, a menu item
  --  must be attached to a menu).
  --
  --  Most windows specify an origin, a width and a height whose coordinates
  --  are taken relative to the enclosing container. Positive widths and
  --  heights are absolute values, but zero and negative widths and heights
  --  are interpreted as being relative to the width and height of the
  --  enclosing container (so a width of 0 means the width of the enclosing
  --  container, a height of -10 means 10 pixels less than the height of the
  --  enclosing container).
  --
  --  The types of container windows available are as follows:
  --
  --  Frame_Type  : a main window with a title bar, system menu, minimise
  --                and maximise buttons, and a close button.
  --  Dialog_Type : a top level window which is used for modal interaction,
  --                disabling other windows while the interaction takes
  --                place.
  --  Panel_Type  : a plain window which is used as a container for other
  --                subwindows.
  --  Menu_Type   : a menu which can contain menu items and submenus.
  --
  ----------------------------------------------------------------------------

  type Frame_Type  is new Container_Type with private;
  type Dialog_Type is new Container_Type with private;
  type Panel_Type  is new Container_Type with private;
  type Menu_Type   is new Container_Type with private;

  ----------------------------------------------------------------------------
  --
  --                             F R A M E S
  --
  --  A frame is a top level window with a title bar, system menu, minimise
  --  and maximise buttons, and a close button. Closing a frame generates a
  --  command. Frames are normally visible, but can be hidden if required.
  --  A frame should be used as the main window for an application.
  --
  --  Frame operations:
  --
  --  Frame   (Origin,      -- create a frame at the specified position
  --           Width,       -- with the specified width
  --           Height,      -- and height in pixels,
  --           Title,       -- with the specified title in the title bar,
  --           Command,     -- generating this command when it is closed,
  --           Font)        -- using this font (default: Default_Font).
  --  Frame   (Width,       -- create a frame with the specified width
  --           Height,      -- and height in pixels, placed randomly,
  --           Title,       -- with the specified title in the title bar,
  --           Command,     -- generating this command when it is closed,
  --           Font)        -- using this font (default: Default_Font).
  --
  --  Close   (Frame)       -- close the frame.
  --  Valid   (Frame)       -- true until the frame is closed.
  --
  --  Frame_Width           -- return the width of the frame border.
  --  Frame_Height          -- return the height of the frame border.
  --
  --  Next_Command          -- return the next command generated by any
  --                        -- control in any existing frame.
  --  Command_Ready         -- test if there is a command pending
  --
  ----------------------------------------------------------------------------

  function  Frame   (Origin  : Point_Type;
                     Width   : Positive;
                     Height  : Positive;
                     Title   : String;
                     Command : Command_Type;
                     Font    : Font_Type := Default_Font) return Frame_Type;
  function  Frame   (Width   : Positive;
                     Height  : Positive;
                     Title   : String;
                     Command : Command_Type;
                     Font    : Font_Type := Default_Font) return Frame_Type;

  procedure Close   (Frame   : in Frame_Type);
  function  Valid   (Frame   : Frame_Type) return Boolean;

  function  Frame_Width   return Natural;
  function  Frame_Height  return Natural;

  function  Next_Command  return Command_Type;
  function  Command_Ready return Boolean;

  ----------------------------------------------------------------------------
  --
  --                            D I A L O G S
  --
  --  A dialog is a top level window like a frame, but it only has a close
  --  button on its title bar. Dialogs are intended for user interaction.
  --  When a dialog is executed it becomes visible and all other windows
  --  are disabled. Execution of a dialog continues until a command is
  --  generated by closing the dialog window or by clicking on a button
  --  attached to the dialog. Dialog windows do not have a menu bar.
  --
  --  Dialog operations:
  --
  --  Dialog  (Width,       -- create a dialog with the given width and
  --           Height,      -- and height in pixels,
  --           Title,       -- with the specified title in the title bar,
  --           Command,     -- generating this command when it is closed,
  --           Font)        -- using this font (default: Default_Font).
  --
  --  Execute (Dialog)      -- execute a dialog and return the command code
  --                        -- used to exit from it.
  --
  --  Dialog_Width          -- return the width of the dialog border.
  --  Dialog_Height         -- return the height of the dialog border.
  --
  ----------------------------------------------------------------------------

  function Dialog  (Width   : Positive;
                    Height  : Positive;
                    Title   : String;
                    Command : Command_Type;
                    Font    : Font_Type := Default_Font) return Dialog_Type;

  function Execute (Dialog  : in Dialog_Type) return Command_Type;

  function Dialog_Width   return Natural;
  function Dialog_Height  return Natural;

  ----------------------------------------------------------------------------
  --
  --                             P A N E L S
  --
  --
  --  Panel operations:
  --
  --  Panel (Parent,        -- create a panel inside a parent container, with
  --         Origin,        -- top left coordinates relative to the parent,
  --         Width,         -- with the specified width
  --         Height,        -- and the specified height, and
  --         Title,         -- with this title on the border (default: none)
  --         Font)          -- using this font (default: same as Parent).
  --
  ----------------------------------------------------------------------------

  function Panel (Parent : Container_Type'Class;
                  Origin : Point_Type;
                  Width  : Integer;
                  Height : Integer;
                  Title  : String := "";
                  Font   : Font_Type := Parent_Font) return Panel_Type;

  ----------------------------------------------------------------------------
  --
  --                              M E N U S
  --
  --  A menu is a pull-down list of items attached to a frame's menu bar.
  --  The items on a menu are either menuitems (which generate a command
  --  when they are selected, submenus (which display another menu when
  --  selected) or separators (horizontal bars used to separate a menu
  --  into subsections.
  --
  --  If the text label for a menu contains the character "&", the character
  --  which follows is underlined and the menu can be activated by pressing
  --  Alt + that character. The character "&" is not displayed.
  --
  --  Menu operations:
  --
  --  Menu (Parent,       -- create a menu attached to a frame or a menu
  --        Text)         -- with the specified text label.
  --
  --  Menu_Height         -- return the height of a menu bar.
  --
  ----------------------------------------------------------------------------

  function Menu  (Parent : Frame_Type'Class;
                  Text   : String) return Menu_Type;
  function Menu  (Parent : Menu_Type'Class;
                  Text   : String) return Menu_Type;

  function Menu_Height   return Natural;

  ----------------------------------------------------------------------------
  --
  --                           C O N T R O L S
  --
  --  Controls are windows for user interaction. They hold values (e.g. a
  --  text string) which can normally be set by the user, as well as being
  --  accessed and altered by the program itself. Some controls (e.g. menu
  --  items) generate command values which can be used to trigger actions
  --  in the program. The following operations are common to all controls:
  --
  --  Enable  (Control,     -- enable or disable the control depending on
  --           Enabled)     -- the value of Enabled (default: True).
  --  Disable (Control)     -- disable the control.
  --  Enabled (Control)     -- test if the control is enabled.
  --
  ----------------------------------------------------------------------------

  procedure Enable  (Control : in Control_Type;
                     Enabled : in Boolean := True);
  procedure Disable (Control : in Control_Type);
  function  Enabled (Control : Control_Type) return Boolean;

  ----------------------------------------------------------------------------
  --
  --  The types of control available are as follows:
  --
  --  Menuitem_Type     : controls which can appear on pull-down menus
  --  Text_Control_Type : controls containing with a single-line text string
  --  Multiline_Type    : controls containing multiple text strings
  --  Canvas_Type       : a plain window for drawing arbitrary shapes on
  --
  ----------------------------------------------------------------------------

  type Text_Control_Type  is abstract new Control_Type with private;
  type Multiline_Type     is abstract new Control_Type with private;
  type Canvas_Type        is new Control_Type with private;

  ----------------------------------------------------------------------------
  --
  --                      T E X T   C O N T R O L S
  --
  --  Text controls include a single-line text string. The following
  --  operations are common to all text controls:
  --
  --  Get_Length (Control)  -- get the length of the text associated with the
  --                        -- control.
  --  Get_Text   (Control)  -- get the text associated with the control as a
  --                        -- string of indefinite size.
  --  Get_Text   (Control,  -- get the text associated with the control into
  --              Text,     -- this fixed-size string variable
  --              Length)   -- and set this integer variable to the actual
  --                        -- number of characters copied.
  --  Set_Text   (Control,  -- set the text associated with the control.
  --              Text)     -- to the specified new text value.
  --
  ----------------------------------------------------------------------------

  function  Get_Length (Control : Text_Control_Type) return Natural;
  function  Get_Text   (Control : Text_Control_Type) return String;
  procedure Get_Text   (Control : in  Text_Control_Type;
                        Text    : out String;
                        Length  : out Natural);
  procedure Set_Text   (Control : in  Text_Control_Type;
                        Text    : in  String);

  ----------------------------------------------------------------------------
  --
  --  The types of text control available are as follows:
  --
  --  Button_Type      : a pushbutton which generates a command code.
  --  Label_Type       : a static non-interactive label.
  --  Editbox_Type     : a single-line edit control for text input.
  --
  --  There is a further subclass of text control, as follows:
  --
  --  Boolean_Control_Type : a text control with an associated Boolean state
  --
  ----------------------------------------------------------------------------

  type Button_Type      is new Text_Control_Type with private;
  type Label_Type       is new Text_Control_Type with private;
  type Editbox_Type     is new Text_Control_Type with private;

  type Boolean_Control_Type is abstract new Text_Control_Type with private;

  ----------------------------------------------------------------------------
  --
  --                            B U T T O N S
  --
  --  Buttons are rectangular labelled controls which generate a command
  --  code when pressed. "Default" buttons are displayed with a heavier
  --  border and respond when the Enter key is pressed.
  --
  --  Button operations:
  --
  --  Button (Parent,     -- create a button in a parent container, with
  --          Origin,     -- top left coordinates relative to the parent,
  --          Width,      -- with the specified width
  --          Height,     -- and the specified height,
  --          Text,       -- labelled with the specified text,
  --          Command,    -- generating this command when it is pressed,
  --          Default,    -- set up as a "default" button (default: False),
  --          Font)       -- using this font (default: same as Parent).
  --
  ----------------------------------------------------------------------------

  function Button (Parent  : Container_Type'Class;
                   Origin  : Point_Type;
                   Width   : Integer;
                   Height  : Integer;
                   Text    : String;
                   Command : Command_Type;
                   Default : Boolean := False;
                   Font    : Font_Type := Parent_Font) return Button_Type;

  ----------------------------------------------------------------------------
  --
  --                             L A B E L S
  --
  --  A label is a static text control that can be used to label other
  --  controls. Labels do not respond to user interaction, but their
  --  values can be read and altered by the program in the same way as
  --  any other text control.
  --
  --  Label operations:
  --
  --  Label (Parent,        -- create a label inside a container, with
  --         Origin,        -- top left coordinates relative to the parent,
  --         Width,         -- with the specified width
  --         Height,        -- and the specified height, and
  --         Text,          -- labelled with the specified text
  --         Align,         -- aligned left, right or centre (default: Left),
  --         Font)          -- using this font (default: same as Parent).
  --
  ----------------------------------------------------------------------------

  function Label (Parent : Container_Type'Class;
                  Origin : Point_Type;
                  Width  : Integer;
                  Height : Integer;
                  Text   : String;
                  Align  : Alignment_Type := Left;
                  Font   : Font_Type := Parent_Font) return Label_Type;

  ----------------------------------------------------------------------------
  --
  --                          E D I T B O X E S
  --
  --  An editbox is a text control containing a single line of text whose
  --  value can be edited by the user.
  --
  --  Editbox operations:
  --
  --  Editbox  (Parent,     -- create an editbox inside a container, with
  --            Origin,     -- top left coordinates relative to the parent,
  --            Width,      -- with the specified width
  --            Height,     -- and the specified height, and
  --            Text,       -- initialised with the specified text,
  --            Password,   -- optionally set up as a password editbox,
  --            Font)       -- using this font (default: same as Parent).
  --
  --  Modified (Editbox)    -- test if the editbox has been modified by the
  --                        -- user.
  --
  ----------------------------------------------------------------------------

  function Editbox  (Parent   : Container_Type'Class;
                     Origin   : Point_Type;
                     Width    : Integer;
                     Height   : Integer;
                     Text     : String    := "";
                     Password : Boolean   := False;
                     Font     : Font_Type := Parent_Font) return Editbox_Type;

  function Modified (Editbox  : Editbox_Type) return Boolean;

  ----------------------------------------------------------------------------
  --
  --                   B O O L E A N   C O N T R O L S
  --
  --  Boolean controls are text controls which can be toggled between two
  --  states (checked or unchecked). The following operations are common
  --  to all Boolean controls:
  --
  --  Get_State (Control)   -- get the current state of the control
  --  Set_State (Control,   -- set the current state of the control
  --             State)     -- to the specified value
  --
  ----------------------------------------------------------------------------

  function  Get_State (Control : Boolean_Control_Type) return Boolean;
  procedure Set_State (Control : in Boolean_Control_Type;
                       State   : in Boolean);


  ----------------------------------------------------------------------------
  --
  --  The types of Boolean controls available are as follows:
  --
  --  Checkbox_Type    : a checkbox which can be checked or unchecked.
  --  Radiobutton_Type : a radio button which can be checked or unchecked.
  --
  ----------------------------------------------------------------------------

  type Menuitem_Type    is new Boolean_Control_Type with private;
  type Checkbox_Type    is new Boolean_Control_Type with private;
  type Radiobutton_Type is new Boolean_Control_Type with private;

  ----------------------------------------------------------------------------
  --
  --                         M E N U   I T E M S
  --
  --  Menu items can only be attached to menus. When a menu is clicked, a
  --  pull-down menu appears which can consist of menu items (which generate
  --  a command when clicked) or further menus.
  --
  --  Each menu and menu item has a text label in which the character "&"
  --  is treated specially. "&" will not be displayed, but the following
  --  character (e.g. 'X') will be underlined to indicate that it can be
  --  selected by pressing Alt + 'X' when the menu is visible.
  --
  --  Menuitem operations:
  --
  --  Menuitem  (Parent,    -- create a menu item attached to a parent menu
  --             Text,      -- with this text label,
  --             Command)   -- generating this command code when selected
  --
  --  Separator (Parent)    -- create a separator (an inactive horizontal
  --                        -- bar) attached to a parent menu
  --
  ----------------------------------------------------------------------------

  function Menuitem  (Parent  : Menu_Type'Class;
                      Text    : String;
                      Command : Command_Type)     return Menuitem_Type;

  function Separator (Parent  : Menu_Type'Class)  return Menuitem_Type;

  ----------------------------------------------------------------------------
  --  Menu items behave slightly differently to other controls, so the
  --  following operations are overridden:
  --
  procedure Enable     (Control : in Menuitem_Type;
                        Enabled : in Boolean := True);
  function  Enabled    (Control : Menuitem_Type) return Boolean;
  function  Get_Length (Control : Menuitem_Type) return Natural;
  function  Get_Text   (Control : Menuitem_Type) return String;
  procedure Set_Text   (Control : in Menuitem_Type;
                        Text    : in String);
  function  Get_State  (Control : Menuitem_Type) return Boolean;
  procedure Set_State  (Control : in Menuitem_Type;
                        State   : in Boolean);

  ----------------------------------------------------------------------------
  --
  --                         C H E C K B O X E S
  --
  --  A checkbox is a labelled control with a left-aligned box that can be
  --  checked or unchecked. Clicking on a checkbox (or pressing Space when
  --  it is selected) toggles the checkbox between the checked and unchecked
  --  states.
  --
  --  Checkbox operations:
  --
  --  Checkbox  (Parent,    -- create a checkbox in a container, with
  --             Origin,    -- top left coordinates relative to the parent,
  --             Width,     -- with the specified width
  --             Height,    -- and the specified height, and
  --             Text,      -- labelled with the specified text, and
  --             Checked,   -- set to this initial state (default: False),
  --             Font)      -- using this font (default: same as Parent).
  --
  ----------------------------------------------------------------------------

  function Checkbox (Parent  : Container_Type'Class;
                     Origin  : Point_Type;
                     Width   : Integer;
                     Height  : Integer;
                     Text    : String;
                     Checked : Boolean := False;
                     Font    : Font_Type := Parent_Font) return Checkbox_Type;

  ----------------------------------------------------------------------------
  --
  --                       R A D I O B U T T O N S
  --
  --  A radiobutton is a Boolean control with a left-aligned box that can be
  --  checked or unchecked. Radiobuttons attached to the same container form
  --  a group. Clicking on an unchecked radiobutton will set the radiobutton
  --  to the checked state and will also uncheck the other radiobuttons which
  --  belong to the same group (i.e. which are attached to the same container).
  --  Unlike a checkbox, a radiobutton cannot be turned off by clicking on it;
  --  you can only uncheck a radiobutton by checking another one in the same
  --  group.
  --
  --  Radiobutton operations:
  --
  --  Radiobutton (Parent,      -- create a radiobutton in a container, with
  --               Origin,      -- top left coordinates relative to the parent,
  --               Width,       -- with the specified width
  --               Height,      -- and the specified height, and
  --               Text,        -- labelled with the specified text, and
  --               Checked,     -- set to this initial state (default: False),
  --               Font)        -- using this font (default: Default_Font).
  --
  ----------------------------------------------------------------------------

  function Radiobutton (Parent  : Container_Type'Class;
                        Origin  : Point_Type;
                        Width   : Integer;
                        Height  : Integer;
                        Text    : String;
                        Checked : Boolean := False;
                        Font    : Font_Type := Parent_Font)
                                                    return Radiobutton_Type;

  ----------------------------------------------------------------------------
  --
  --                 M U L T I L I N E   C O N T R O L S
  --
  --  Multiline controls contain multiple lines of text numbered from 1
  --  upwards.  Individual lines can be accessed by specifying a line
  --  number.  The user can select a particular line by clicking on it
  --  with the mouse or using the keyboard arrow keys when the control
  --  is selected.  Specifying the line number to access as 0 will access
  --  the currently selected line.  If no line is selected, the current
  --  line number will be reported as 0 but its contents can still be
  --  accessed .  A Constraint_Error will be raised if an attempt is made
  --  to access a line beyond the last one.
  --
  --  The following operations are common to all multiline controls, but
  --  their precise effects vary slightly according to the actual type of
  --  the control (see below):
  --
  --  Get_Count   (Control) -- get the number of lines of text in the control.
  --  Get_Line    (Control) -- get the number of the currently selected line.
  --  Get_Length  (Control, -- get the length of the specified line
  --               Line)    -- (default: current line).
  --  Get_Text    (Control, -- get the text of the specified line as a string
  --               Line)    -- of indefinite length (default: current line).
  --
  --  Get_Text    (Control, -- get the text of the control
  --               Line,    -- from the specified line (default: current line)
  --               Text,    -- into a fixed-size string variable, together
  --               Length)  -- with the number of characters transferred.
  --  Set_Text    (Control, -- set the text of the control
  --               Line,    -- at the specified line
  --               Text)    -- to the specified value.
  --
  --  Select_Line (Control, -- select the line at the specified line number
  --               Line)    -- (default: 0, meaning deselect).
  --  Append_Line (Control, -- append a line to the end of the control, where
  --               Text)    -- this is the text to append.
  --  Insert_Line (Control, -- insert a new line above the specified
  --               Line,    -- line number, where
  --               Text)    -- this is the text to insert.
  --  Delete_Line (Control, -- delete the specified line.
  --               Line)
  --  Delete_All  (Control) -- delete all lines.
  --
  ----------------------------------------------------------------------------

  function  Get_Count   (Control : Multiline_Type) return Natural is abstract;
  function  Get_Line    (Control : Multiline_Type) return Natural is abstract;
  function  Get_Length  (Control : Multiline_Type;
                         Line    : Natural := 0)   return Natural is abstract;
  function  Get_Text    (Control : Multiline_Type;
                         Line    : Natural := 0)   return String  is abstract;

  procedure Get_Text    (Control : in  Multiline_Type;
                         Line    : in  Natural := 0;
                         Text    : out String;
                         Length  : out Natural);
  procedure Set_Text    (Control : in  Multiline_Type;
                         Text    : in  String;
                         Line    : in  Natural := 0)              is abstract;

  procedure Select_Line (Control : in Multiline_Type;
                         Line    : in Natural := 0)               is abstract;
  procedure Append_Line (Control : in Multiline_Type;
                         Text    : in String)                     is abstract;
  procedure Insert_Line (Control : in Multiline_Type;
                         Text    : in String;
                         Line    : in Natural := 0)               is abstract;
  procedure Delete_Line (Control : in Multiline_Type;
                         Line    : in Natural := 0)               is abstract;
  procedure Delete_All  (Control : in Multiline_Type)             is abstract;

  ----------------------------------------------------------------------------
  --
  --  The types of multiline controls available are as follows:
  --
  --  Listbox_Type  : a list of text items in a scrollable window
  --  Combobox_Type : an editbox in combination with a drop-down list box
  --  Memo_Type     : a multi-line text editor
  --
  ----------------------------------------------------------------------------

  type Listbox_Type  is new Multiline_Type with private;
  type Combobox_Type is new Multiline_Type with private;
  type Memo_Type     is new Multiline_Type with private;

  ----------------------------------------------------------------------------
  --
  --                          L I S T B O X E S
  --
  --  A listbox is a list of lines of text (initially empty). The lines are
  --  sorted into ascending order by default, but can be left unsorted if
  --  required. For a sorted list, the position at which a new line is added
  --  will be ignored, with the new line being inserted at the appropriate
  --  position according to its value. When no line has been selected, the
  --  contents of the current line will be reported as an empty string ("").
  --
  --  Listbox operations:
  --
  --  Listbox (Parent,      -- create a listbox inside a container, with
  --           Origin,      -- top left coordinates relative to the parent,
  --           Width,       -- the specified width
  --           Height,      -- and the specified height, using
  --           Font)        -- this font (default: same as Parent).
  --
  ----------------------------------------------------------------------------

  function Listbox (Parent : Container_Type'Class;
                    Origin : Point_Type;
                    Width  : Integer;
                    Height : Integer;
                    Font   : Font_Type := Parent_Font) return Listbox_Type;

  ----------------------------------------------------------------------------
  --
  --                         C O M B O B O X E S
  --
  --  A combobox consists of an edit control together with a drop-down list
  --  (effectively a combination of an editbox and a listbox). The currently
  --  selected line is displayed in the edit control, and you can specify
  --  whether the user is able to manually edit this value. If not, only
  --  the values in the drop-down list can be selected.
  --
  --  If the contents of the edit control match one of the values in the list,
  --  the position of the corresponding line in the list is reported as the
  --  number of the currently selected line. Otherwise, the number of the
  --  current line is reported as 0. Accessing the value of the current line
  --  (line 0) will report the current value of the edit control.
  --
  --  Combobox operations:
  --
  --  Combobox (Parent,     -- create a combobox inside a container, with
  --            Origin,     -- top left coordinates relative to the parent,
  --            Width,      -- with the specified width, whose value can
  --            Editable,   -- be manually edited (default: True),
  --            Font)       -- using this font (default: same as Parent).
  --
  ----------------------------------------------------------------------------

  function Combobox (Parent   : Container_Type'Class;
                     Origin   : Point_Type;
                     Width    : Integer;
                     Editable : Boolean := True;
                     Font     : Font_Type := Parent_Font) return Combobox_Type;

  ----------------------------------------------------------------------------
  --
  --                              M E M O S
  --
  --  A memo is a simple multi-line text editor similar to Windows Notepad.
  --  There are several memo-specific operations in addition to the standard
  --  operations on multi-line controls. The user can select a block of text
  --  spanning multiple lines using the mouse (or by moving the cursor with
  --  the Shift key held down) and there are operations to fetch, replace
  --  and delete the selected text, find the line and column position of
  --  the start and end of the selected text, and get its total length (which
  --  will include one or more additional end-of-line characters if the text
  --  spans more than one line).
  --
  --  Memo operations:
  --
  --  Memo (Parent,           -- create a memo inside a container, with
  --        Origin,           -- top left coordinates relative to the parent,
  --        Width,            -- the specified width
  --        Height)           -- and the specified height
  --        Font)             -- using this font (default: same as Parent).
  --
  --  Get_Column      (Memo)  -- get the column position of the current
  --                          -- selection.
  --  Modified        (Memo)  -- test if the user has modified the memo.
  --  Cut_Selection   (Memo)  -- cut the current selection to the clipboard.
  --  Copy_Selection  (Memo)  -- copy the current selection to the clipboard.
  --  Paste_Selection (Memo)  -- paste the clipboard contents to the memo,
  --                          -- replacing the current selection.
  --  Undo_Change     (Memo)  -- undo the user's last change to the memo.
  --  Show_Selection  (Memo)  -- scroll the memo so that the current position
  --                          -- is visible on the screen.
  --
  ----------------------------------------------------------------------------

  function  Memo (Parent : Container_Type'Class;
                  Origin : Point_Type;
                  Width  : Integer;
                  Height : Integer;
                  Font   : Font_Type := Parent_Font) return Memo_Type;

  function  Get_Column      (Memo : Memo_Type) return Natural;
  function  Modified        (Memo : Memo_Type) return Boolean;
  procedure Cut_Selection   (Memo : in Memo_Type);
  procedure Copy_Selection  (Memo : in Memo_Type);
  procedure Paste_Selection (Memo : in Memo_Type);
  procedure Undo_Change     (Memo : in Memo_Type);
  procedure Show_Selection  (Memo : in Memo_Type);

  ----------------------------------------------------------------------------
  --  Most operations on multiline types are implemented in different ways,
  --  so they are overridden here:
  --
  function  Get_Count   (Control : Listbox_Type)  return Natural;
  function  Get_Count   (Control : Combobox_Type) return Natural;
  function  Get_Count   (Control : Memo_Type)     return Natural;

  function  Get_Line    (Control : Listbox_Type)  return Natural;
  function  Get_Line    (Control : Combobox_Type) return Natural;
  function  Get_Line    (Control : Memo_Type)     return Natural;

  function  Get_Length  (Control : Listbox_Type;
                         Line    : Natural := 0)  return Natural;
  function  Get_Length  (Control : Combobox_Type;
                         Line    : Natural := 0)  return Natural;
  function  Get_Length  (Control : Memo_Type;
                         Line    : Natural := 0)  return Natural;

  function  Get_Text    (Control : Listbox_Type;
                         Line    : Natural := 0)  return String;
  function  Get_Text    (Control : Combobox_Type;
                         Line    : Natural := 0)  return String;
  function  Get_Text    (Control : Memo_Type;
                         Line    : Natural := 0)  return String;

  procedure Set_Text    (Control : in  Listbox_Type;
                         Text    : in  String;
                         Line    : in  Natural := 0);
  procedure Set_Text    (Control : in  Combobox_Type;
                         Text    : in  String;
                         Line    : in  Natural := 0);
  procedure Set_Text    (Control : in  Memo_Type;
                         Text    : in  String;
                         Line    : in  Natural := 0);

  procedure Select_Line (Control : in  Listbox_Type;
                         Line    : in  Natural := 0);
  procedure Select_Line (Control : in  Combobox_Type;
                         Line    : in  Natural := 0);
  procedure Select_Line (Control : in  Memo_Type;
                         Line    : in  Natural := 0);

  procedure Append_Line (Control : in Listbox_Type;
                         Text    : in String);
  procedure Append_Line (Control : in Combobox_Type;
                         Text    : in String);
  procedure Append_Line (Control : in Memo_Type;
                         Text    : in String);

  procedure Insert_Line (Control : in Listbox_Type;
                         Text    : in String;
                         Line    : in Natural := 0);
  procedure Insert_Line (Control : in Combobox_Type;
                         Text    : in String;
                         Line    : in Natural := 0);
  procedure Insert_Line (Control : in Memo_Type;
                         Text    : in String;
                         Line    : in Natural := 0);

  procedure Delete_Line (Control : in Listbox_Type;
                         Line    : in Natural := 0);
  procedure Delete_Line (Control : in Combobox_Type;
                         Line    : in Natural := 0);
  procedure Delete_Line (Control : in Memo_Type;
                         Line    : in Natural:= 0);

  procedure Delete_All  (Control : in Listbox_Type);
  procedure Delete_All  (Control : in Combobox_Type);
  procedure Delete_All  (Control : in Memo_Type);

  ----------------------------------------------------------------------------
  --
  --                           C A N V A S E S
  --
  --  A canvas is a blank rectangle for drawing on which can optionally
  --  be set to generate a command code when the mouse is clicked on it.
  --  A canvas has an associated font (the same as the parent window by
  --  default), a background colour (initially white), a pen for drawing
  --  lines (initially black, one pixel wide) and a fill colour used to
  --  colour closed shapes (initially white).
  --
  --  The freedom of expression available with canvases make these the
  --  most complex component of all, with over 20 available operations.
  --  There are operations to draw lines, rectangles (with or without
  --  rounded corners), ellipses, circles, line sequences, polygons and
  --  text. The font, pen size and colour and fill colour can be changed
  --  at any time any will affect all subsequent drawing operations (but
  --  everything drawn previously will be unaffected). Rectangles can be
  --  given rounded corners by specifying a rounding factor, which gives
  --  the X and Y offsets from the corners to the points where the rounded
  --  corner begins.
  --
  --  Anything drawn on a canvas will normally stay there, but the canvas
  --  can be erased or the current state of a drawing can be saved and then
  --  restored later (which provides a basic "undo" facility). For example,
  --  a clock can be drawn by drawing a circle, saving the drawing, and then
  --  drawing the hands. To change the position of the hands, restore the
  --  saved drawing (thus erasing the hands) and then redraw the hands in
  --  the new position. You can only save one copy of the drawing, so if you
  --  save it a second time you will lose the previous saved copy.
  --
  --  A canvas can be set up to generate a command when the mouse button
  --  is pressed inside its borders. There are operations to get the
  --  position at which the button was pressed, to get the current mouse
  --  position, and to test if the mouse button is still down and whether
  --  the mouse has been moved. As long as the button is down, the mouse
  --  position will be tracked even if the mouse is moved outside the canvas.
  --  You can track the mouse visually by saving the drawing when the mouse
  --  is first pressed, then restoring the drawing and drawing a new line
  --  from the initial mouse position to the current one.
  --
  --  Canvas operations:
  --
  --  Canvas  (Parent,            -- create a canvas inside a container, with
  --           Origin,            -- top left coordinates relative to Parent,
  --           Width,             -- the specified width
  --           Height,            -- and the specified height, using
  --           Font)              -- this font (default: same as Parent).
  --
  --  Canvas  (Parent,            -- create a canvas inside a container, with
  --           Origin,            -- top left coordinates relative to the parent,
  --           Width,             -- the specified width
  --           Height,            -- and the specified height,
  --           Command,           -- which generates this command code, and
  --           Font)              -- uses this font (default: same as Parent).
  --
  --  Canvas  (Parent,            -- create a canvas inside a container, with
  --           Origin,            -- top left coordinates relative to the parent,
  --           Width,             -- the specified width
  --           Height,            -- and the specified height,
  --           Command,           -- which generates this command code, and
  --           Keypress,          -- this one when a key is pressed, and
  --           Font)              -- uses this font (default: same as Parent).
  --
  --  Set_Colour     (Canvas,     -- set the background colour for a canvas
  --                  Colour)     -- using this colour (default: white)
  --
  --  Erase          (Canvas)     -- erase the drawing in a canvas. This will
  --                              -- also erase any saved drawing.
  --  Save           (Canvas)     -- save the current drawing.
  --  Restore        (Canvas)     -- restore a saved drawing, erasing anything
  --                              -- drawn since. If the drawing has never been
  --                              -- saved, or the window has been erased using
  --                              -- Erase, this will do nothing.
  --  Set_Font       (Canvas,     -- set a new font for all subsequent text
  --                  Font)       -- drawn on the canvas.
  --  Set_Pen        (Canvas,     -- set the pen used to draw lines on the
  --                  Colour,     -- canvas to this colour (default: black)
  --                  Width)      -- and thickness (default: 1 pixel).
  --  Set_Fill       (Canvas,     -- set the colour used to fill subsequent
  --                  Colour)     -- closed shapes drawn on the canvas.
  --  Set_Fill       (Canvas)     -- clear the colour used to fill subsequent
  --                              -- closed shapes drawn on the canvas.
  --  Draw_Text      (Canvas,     -- draw a text string on the canvas
  --                  From,       -- from this top-left point, where
  --                  Text)       -- this is the text to be drawn.
  --  Draw_Text      (Canvas,     -- draw a text string on the canvas
  --                  From,       -- in a rectangle between this point
  --                  To,         -- and this one, where
  --                  Text,       -- this is the text to be drawn
  --                  Align)      -- with this alignment (default: left).
  --  Draw_Text      (Canvas,     -- draw a text string on the canvas
  --                  From,       -- in a rectangle starting at this point
  --                  Width,      -- with this width
  --                  Height,     -- and this height, where
  --                  Text,       -- this is the text to be drawn
  --                  Align)      -- with this alignment (default: left).
  --
  --  Draw_Line      (Canvas,     -- draw a line on the canvas
  --                  From,       -- from this point
  --                  To)         -- to this one.
  --  Draw_Line      (Canvas,     -- draw a line on the canvas
  --                  From,       -- from this point
  --                  Length,     -- for this length
  --                  Angle)      -- at this angle.
  --  Draw_Line_List (Canvas,     -- draw lines on the canvas connecting
  --                  Points)     -- the points in this list.
  --
  --  Draw_Rectangle (Canvas,     -- draw a rectangle on the canvas
  --                  From,       -- from this corner point
  --                  To,         -- to this diagonally-opposite point
  --                  Rounding)   -- with corners rounded this much
  --                              -- (default: no rounding).
  --  Draw_Rectangle (Canvas,     -- draw a rectangle on the canvas
  --                  From,       -- from this top-left point
  --                  Width,      -- with this width
  --                  Height,     -- and this height
  --                  Rounding)   -- with corners rounded this much
  --                              -- (default: no rounding).
  --
  --  Draw_Ellipse   (Canvas,     -- draw an ellipse on the canvas
  --                  From,       -- bounded by a rectangle from this point
  --                  To)         -- to this point.
  --  Draw_Ellipse   (Canvas,     -- draw an ellipse on the canvas
  --                  From,       -- bounded by a rectangle from this point
  --                  Width,      -- with this width.
  --                  Height)     -- and this height
  --  Draw_Circle    (Canvas,     -- draw a circle on the canvas
  --                  Centre,     -- with this centre point
  --                  Radius)     -- and this radius.
  --
  --  Draw_Polygon   (Canvas,     -- draw a polygon on the canvas
  --                  Points)     -- with vertices at these points.
  --
  --  Draw_Image     (Canvas,     -- draw an image on the canvas
  --                  From,       -- from this point
  --                  Image)      -- using this image object.
  --  Draw_Image     (Canvas,     -- draw an image on the canvas
  --                  From,       -- from this point
  --                  To,         -- to this one (stretching the image to fit)
  --                  Image)      -- using this image object.
  --  Draw_Image     (Canvas,     -- draw an image on the canvas
  --                  From,       -- from this point
  --                  Width,      -- with this width
  --                  Height,     -- and this height
  --                  Image)      -- using this image object.
  --
  --  Mouse_Down     (Canvas)     -- test if the left mouse button is down.
  --  Mouse_Moved    (Canvas)     -- test if the mouse has been moved.
  --  Start_Point    (Canvas)     -- get the point where the mouse button
  --                              -- was first pressed.
  --  End_Point      (Canvas)     -- get the point where the mouse is now
  --                              -- (or its final position when the left
  --                              -- button was released).
  --  Key_Code       (Canvas)     -- get the character code for the last
  --                              -- key pressed.
  --
  ----------------------------------------------------------------------------

  function Canvas (Parent   : Container_Type'Class;
                   Origin   : Point_Type;
                   Width    : Integer;
                   Height   : Integer;
                   Font     : Font_Type := Parent_Font) return Canvas_Type;

  function Canvas (Parent   : Container_Type'Class;
                   Origin   : Point_Type;
                   Width    : Integer;
                   Height   : Integer;
                   Command  : Command_Type;
                   Font     : Font_Type := Parent_Font) return Canvas_Type;

  function Canvas (Parent   : Container_Type'Class;
                   Origin   : Point_Type;
                   Width    : Integer;
                   Height   : Integer;
                   Command  : Command_Type;
                   Keypress : Command_Type;
                   Font     : Font_Type := Parent_Font) return Canvas_Type;

  procedure Set_Colour (Canvas : in Canvas_Type;
                        Colour : in Colour_Type := White);

  procedure Erase      (Canvas   : in Canvas_Type);
  procedure Save       (Canvas   : in Canvas_Type);
  procedure Restore    (Canvas   : in Canvas_Type);

  procedure Set_Font   (Canvas   : in Canvas_Type;
                        Font     : in Font_Type);
  procedure Set_Pen    (Canvas   : in Canvas_Type;
                        Colour   : in Colour_Type := Black;
                        Width    : in Natural     := 1);
  procedure Set_Fill   (Canvas   : in Canvas_Type;
                        Colour   : in Colour_Type);
  procedure Set_Fill   (Canvas   : in Canvas_Type);

  procedure Draw_Text  (Canvas   : in Canvas_Type;
                        From     : in Point_Type;
                        Text     : in String);
  procedure Draw_Text  (Canvas   : in Canvas_Type;
                        From     : in Point_Type;
                        To       : in Point_Type;
                        Text     : in String;
                        Align    : in Alignment_Type := Left);
  procedure Draw_Text  (Canvas   : in Canvas_Type;
                        From     : in Point_Type;
                        Width    : in Integer;
                        Height   : in Integer;
                        Text     : in String;
                        Align    : in Alignment_Type := Left);

  procedure Draw_Line  (Canvas   : in Canvas_Type;
                        From     : in Point_Type;
                        To       : in Point_Type);
  procedure Draw_Line  (Canvas   : in Canvas_Type;
                        From     : in Point_Type;
                        Length   : in Positive;
                        Angle    : in Angle_Type);

  procedure Draw_Line_List
                       (Canvas   : in Canvas_Type;
                        Points   : in Point_List);

  procedure Draw_Rectangle
                       (Canvas   : in Canvas_Type;
                        From     : in Point_Type;
                        To       : in Point_Type;
                        Rounding : in Point_Type := (0,0));
  procedure Draw_Rectangle
                       (Canvas   : in Canvas_Type;
                        From     : in Point_Type;
                        Width    : in Positive;
                        Height   : in Positive;
                        Rounding : in Point_Type := (0,0));

  procedure Draw_Ellipse
                       (Canvas   : in Canvas_Type;
                        From     : in Point_Type;
                        To       : in Point_Type);
  procedure Draw_Ellipse
                       (Canvas   : in Canvas_Type;
                        From     : in Point_Type;
                        Width    : in Positive;
                        Height   : in Positive);
  procedure Draw_Circle
                       (Canvas   : in Canvas_Type;
                        Centre   : in Point_Type;
                        Radius   : in Positive);

  procedure Draw_Polygon
                       (Canvas   : in Canvas_Type;
                        Points   : in Point_List);

  procedure Draw_Image (Canvas   : in Canvas_Type;
                        From     : in Point_Type;
                        Image    : in Image_Type);
  procedure Draw_Image (Canvas   : in Canvas_Type;
                        From     : in Point_Type;
                        To       : in Point_Type;
                        Image    : in Image_Type);
  procedure Draw_Image (Canvas   : in Canvas_Type;
                        From     : in Point_Type;
                        Width    : in Natural;
                        Height   : in Natural;
                        Image    : in Image_Type);

  function Start_Point (Canvas   : Canvas_Type) return Point_Type;
  function End_Point   (Canvas   : Canvas_Type) return Point_Type;
  function Mouse_Down  (Canvas   : Canvas_Type) return Boolean;
  function Mouse_Moved (Canvas   : Canvas_Type) return Boolean;
  function Key_Code    (Canvas   : Canvas_Type) return Character;

  ----------------------------------------------------------------------------
  --
  --                     C O M M O N   D I A L O G S
  --
  --  Common dialogs are pre-packaged dialog widgets which are not treated
  --  as normal windows (although they are made to look similar for ease of
  --  use).
  --
  --  Common dialog operations (common to all dialogs):
  --
  --  Execute (Dialog)      -- execute the dialog and return True if the OK
  --                        -- button was used to close the dialog and False
  --                        -- otherwise.
  --
  ----------------------------------------------------------------------------

  type Common_Dialog_Type is abstract tagged private;

  function Execute (Dialog : Common_Dialog_Type) return Boolean;
                                
  ----------------------------------------------------------------------------
  --
  --           C O M M O N   D I A L O G   S U B C L A S S E S
  --
  --  The available dialog subclasses are colour, font and file dialogs.
  --
  --  Colour_Dialog_Type : a dialog to allow the user to select a colour.
  --  Font_Dialog_Type   : a dialog to allow the user to select a font.
  --  File_Dialog_Type   : a dialog to allow the user to select a file name.
  --
  ----------------------------------------------------------------------------

  type Colour_Dialog_Type is new Common_Dialog_Type with private;
  type Font_Dialog_Type   is new Common_Dialog_Type with private;
  type File_Dialog_Type   is abstract new Common_Dialog_Type with private;
  
  ----------------------------------------------------------------------------
  --
  --                     C O L O U R   D I A L O G S
  --
  --  Colour dialogs allow the user to select or create a colour.
  --
  --  Colour dialog operations:
  --
  --  Colour_Dialog           -- create a colour dialog.
  --  Set_Colour    (Dialog,  -- set the initial colour displayed in the
  --                 Colour)  -- dialog to this colour.
  --  Get_Colour    (Dialog)  -- get the colour contained in the dialog.
  --
  ----------------------------------------------------------------------------

  function  Colour_Dialog return Colour_Dialog_Type;

  procedure Set_Colour    (Dialog : in Colour_Dialog_Type;
                           Colour : in Colour_Type);
  function  Get_Colour    (Dialog : in Colour_Dialog_Type) return Colour_Type;

  ----------------------------------------------------------------------------
  --
  --                       F O N T   D I A L O G S
  --
  --  Font dialogs allow the user to select a font.
  --
  --  Font dialog operations:
  --
  --  Font_Dialog             -- create a font dialog.
  --  Set_Font    (Dialog,    -- set the initial font displayed in the
  --               Font)      -- dialog to this font.
  --  Get_Font    (Dialog)    -- get the font contained in the dialog.
  --
  ----------------------------------------------------------------------------

  function  Font_Dialog return Font_Dialog_Type;
  
  procedure Set_Font    (Dialog : in Font_Dialog_Type;
                         Font   : in Font_Type);
  function  Get_Font    (Dialog : in Font_Dialog_Type) return Font_Type;

  ----------------------------------------------------------------------------
  --
  --                       F I L E   D I A L O G S
  --
  --  File dialogs allow the user to select or enter a file name. This is an
  --  abstract type which is further subclassed below.
  --
  --  File dialog operations (common to all file dialogs):
  --
  --  Set_Name      (Dialog,    -- set the initial file name displayed in the
  --                 Name)      -- dialog to this string.
  --  Get_Name      (Dialog)    -- get the file name contained in the dialog.
  --  Add_Filter    (Dialog,    -- add a file type filter to the dialog
  --                 Text,      -- with this description
  --                 Filter)    -- to match this wildcard specification.
  --  Set_Directory (Dialog,    -- set the initial directory for the dialog
  --                 Name)      -- to this directory.
  --
  ----------------------------------------------------------------------------

  procedure Set_Name      (Dialog : in File_Dialog_Type;
                           Name   : in String);
  function  Get_Name      (Dialog : in File_Dialog_Type) return String;

  procedure Add_Filter    (Dialog : in File_Dialog_Type;
                           Text   : in String;
                           Filter : in String);

  procedure Set_Directory (Dialog : in File_Dialog_Type;
                           Name   : in String);

  ----------------------------------------------------------------------------
  --
  --                       O P E N   D I A L O G S
  --
  --  Open dialogs allow the user to select or enter a file name for use as
  --  an input file. The file name selected must be the name of an existing
  --  file.
  --
  --  Open dialog operations:
  --
  --  Open_Dialog (Title)     -- create an open file dialog with this title.
  --
  ----------------------------------------------------------------------------

  type Open_Dialog_Type is new File_Dialog_Type with private;

  function Open_Dialog (Title  : String) return Open_Dialog_Type;

  ----------------------------------------------------------------------------
  --
  --                       S A V E   D I A L O G S
  --
  --  Save dialogs allow the user to select or enter a file name for use as
  --  an output file. If the Create parameter to the constructor function
  --  below is True (as it is by default) and an existing file is selected,
  --  the user will be asked if the file should be overwritten. If it is
  --  False and the file does not exist, the user will be asked if it should
  --  be created.
  --
  --  Save dialog operations:
  --
  --  Save_Dialog (Title,     -- create a save file dialog with this title
  --               Create)    -- which will prompt the user as described above.
  --
  ----------------------------------------------------------------------------

  type Save_Dialog_Type is new File_Dialog_Type with private;

  function Save_Dialog (Title  : String;
                        Create : Boolean := True) return Save_Dialog_Type;

  ----------------------------------------------------------------------------
  --  Renamings for our transatlantic cousins, in the hope that some day
  --  they'll repay the favour/favor...
  ----------------------------------------------------------------------------

  procedure Set_Color (Canvas : in Canvas_Type;
                       Colour : in Colour_Type := White) renames Set_Colour;

  subtype   Color_Dialog_Type is Colour_Dialog_Type;

  function  Color_Dialog  return Colour_Dialog_Type renames Colour_Dialog;
  procedure Set_Color    (Dialog : in Colour_Dialog_Type;
                          Colour : in Colour_Type) renames Set_Colour;
  function  Get_Color    (Dialog : in Colour_Dialog_Type) return Colour_Type
                                                   renames Get_Colour;

private       -- and deliberately uninformative!

  type Window_Type is abstract tagged
    record
      Internals : JEWL.Controlled_Type;   -- see package JEWL
    end record;

  type Container_Type       is abstract new Window_Type with null record;
  type Control_Type         is abstract new Window_Type with null record;

  type Frame_Type           is new Container_Type with null record;
  type Dialog_Type          is new Container_Type with null record;
  type Panel_Type           is new Container_Type with null record;
  type Menu_Type            is new Container_Type with null record;

  type Text_Control_Type    is abstract new Control_Type with null record;
  type Multiline_Type       is abstract new Control_Type with null record;
  type Canvas_Type          is new Control_Type with null record;

  type Button_Type          is new Text_Control_Type with null record;
  type Label_Type           is new Text_Control_Type with null record;
  type Editbox_Type         is new Text_Control_Type with null record;
  type Boolean_Control_Type is abstract new Text_Control_Type with null record;

  type Menuitem_Type        is new Boolean_Control_Type with null record;
  type Checkbox_Type        is new Boolean_Control_Type with null record;
  type Radiobutton_Type     is new Boolean_Control_Type with null record;

  type Listbox_Type         is new Multiline_Type with null record;
  type Combobox_Type        is new Multiline_Type with null record;
  type Memo_Type            is new Multiline_Type with null record;

  type Common_Dialog_Type is abstract tagged
    record
      Internals : JEWL.Controlled_Type;   -- see package JEWL
    end record;

  type Colour_Dialog_Type is new Common_Dialog_Type with null record;
  type Font_Dialog_Type   is new Common_Dialog_Type with null record;
  type File_Dialog_Type   is abstract new Common_Dialog_Type with null record;

  type Open_Dialog_Type   is new File_Dialog_Type with null record;
  type Save_Dialog_Type   is new File_Dialog_Type with null record;

  type Image_Type is
    record
      Internals : JEWL.Controlled_Type;   -- see package JEWL
    end record;

end JEWL.Windows;
