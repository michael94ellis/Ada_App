pragma Ada_95;
pragma Warnings (Off);
pragma Source_File_Name (ada_main, Spec_File_Name => "b__main.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__main.adb");
pragma Suppress (Overflow_Check);

with System.Restrictions;
with Ada.Exceptions;

package body ada_main is

   E115 : Short_Integer; pragma Import (Ada, E115, "system__os_lib_E");
   E015 : Short_Integer; pragma Import (Ada, E015, "system__soft_links_E");
   E025 : Short_Integer; pragma Import (Ada, E025, "system__exception_table_E");
   E100 : Short_Integer; pragma Import (Ada, E100, "ada__io_exceptions_E");
   E165 : Short_Integer; pragma Import (Ada, E165, "ada__numerics_E");
   E048 : Short_Integer; pragma Import (Ada, E048, "ada__strings_E");
   E050 : Short_Integer; pragma Import (Ada, E050, "ada__strings__maps_E");
   E054 : Short_Integer; pragma Import (Ada, E054, "ada__strings__maps__constants_E");
   E102 : Short_Integer; pragma Import (Ada, E102, "ada__tags_E");
   E099 : Short_Integer; pragma Import (Ada, E099, "ada__streams_E");
   E065 : Short_Integer; pragma Import (Ada, E065, "interfaces__c_E");
   E142 : Short_Integer; pragma Import (Ada, E142, "interfaces__c__strings_E");
   E027 : Short_Integer; pragma Import (Ada, E027, "system__exceptions_E");
   E118 : Short_Integer; pragma Import (Ada, E118, "system__file_control_block_E");
   E198 : Short_Integer; pragma Import (Ada, E198, "ada__streams__stream_io_E");
   E110 : Short_Integer; pragma Import (Ada, E110, "system__file_io_E");
   E113 : Short_Integer; pragma Import (Ada, E113, "system__finalization_root_E");
   E111 : Short_Integer; pragma Import (Ada, E111, "ada__finalization_E");
   E184 : Short_Integer; pragma Import (Ada, E184, "system__storage_pools_E");
   E188 : Short_Integer; pragma Import (Ada, E188, "system__finalization_masters_E");
   E186 : Short_Integer; pragma Import (Ada, E186, "system__storage_pools__subpools_E");
   E152 : Short_Integer; pragma Import (Ada, E152, "system__task_info_E");
   E008 : Short_Integer; pragma Import (Ada, E008, "ada__calendar_E");
   E006 : Short_Integer; pragma Import (Ada, E006, "ada__calendar__delays_E");
   E063 : Short_Integer; pragma Import (Ada, E063, "system__object_reader_E");
   E043 : Short_Integer; pragma Import (Ada, E043, "system__dwarf_lines_E");
   E180 : Short_Integer; pragma Import (Ada, E180, "system__pool_global_E");
   E246 : Short_Integer; pragma Import (Ada, E246, "system__random_seed_E");
   E019 : Short_Integer; pragma Import (Ada, E019, "system__secondary_stack_E");
   E222 : Short_Integer; pragma Import (Ada, E222, "system__tasking__initialization_E");
   E038 : Short_Integer; pragma Import (Ada, E038, "system__traceback__symbolic_E");
   E136 : Short_Integer; pragma Import (Ada, E136, "ada__real_time_E");
   E097 : Short_Integer; pragma Import (Ada, E097, "ada__text_io_E");
   E204 : Short_Integer; pragma Import (Ada, E204, "system__tasking__protected_objects_E");
   E218 : Short_Integer; pragma Import (Ada, E218, "system__tasking__protected_objects__entries_E");
   E230 : Short_Integer; pragma Import (Ada, E230, "system__tasking__queuing_E");
   E238 : Short_Integer; pragma Import (Ada, E238, "system__tasking__stages_E");
   E164 : Short_Integer; pragma Import (Ada, E164, "computer_E");
   E169 : Short_Integer; pragma Import (Ada, E169, "jewl_E");
   E210 : Short_Integer; pragma Import (Ada, E210, "jewl__win32_interface_E");
   E202 : Short_Integer; pragma Import (Ada, E202, "jewl__canvas_implementation_E");
   E240 : Short_Integer; pragma Import (Ada, E240, "jewl__window_implementation_E");
   E214 : Short_Integer; pragma Import (Ada, E214, "jewl__message_handling_E");
   E242 : Short_Integer; pragma Import (Ada, E242, "jewl__windows_E");
   E196 : Short_Integer; pragma Import (Ada, E196, "jewl__simple_windows_E");
   E195 : Short_Integer; pragma Import (Ada, E195, "jewl__simple_windows_E");
   E167 : Short_Integer; pragma Import (Ada, E167, "gameboard_E");

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      E240 := E240 - 1;
      E214 := E214 - 1;
      declare
         procedure F1;
         pragma Import (Ada, F1, "jewl__message_handling__finalize_spec");
      begin
         F1;
      end;
      declare
         procedure F2;
         pragma Import (Ada, F2, "jewl__window_implementation__finalize_spec");
      begin
         F2;
      end;
      E202 := E202 - 1;
      declare
         procedure F3;
         pragma Import (Ada, F3, "jewl__canvas_implementation__finalize_spec");
      begin
         F3;
      end;
      E169 := E169 - 1;
      declare
         procedure F4;
         pragma Import (Ada, F4, "jewl__finalize_spec");
      begin
         F4;
      end;
      E218 := E218 - 1;
      declare
         procedure F5;
         pragma Import (Ada, F5, "system__tasking__protected_objects__entries__finalize_spec");
      begin
         F5;
      end;
      E097 := E097 - 1;
      declare
         procedure F6;
         pragma Import (Ada, F6, "ada__text_io__finalize_spec");
      begin
         F6;
      end;
      declare
         procedure F7;
         pragma Import (Ada, F7, "system__file_io__finalize_body");
      begin
         E110 := E110 - 1;
         F7;
      end;
      E188 := E188 - 1;
      E186 := E186 - 1;
      E180 := E180 - 1;
      declare
         procedure F8;
         pragma Import (Ada, F8, "system__pool_global__finalize_spec");
      begin
         F8;
      end;
      declare
         procedure F9;
         pragma Import (Ada, F9, "system__storage_pools__subpools__finalize_spec");
      begin
         F9;
      end;
      declare
         procedure F10;
         pragma Import (Ada, F10, "system__finalization_masters__finalize_spec");
      begin
         F10;
      end;
      E198 := E198 - 1;
      declare
         procedure F11;
         pragma Import (Ada, F11, "ada__streams__stream_io__finalize_spec");
      begin
         F11;
      end;
      declare
         procedure Reraise_Library_Exception_If_Any;
            pragma Import (Ada, Reraise_Library_Exception_If_Any, "__gnat_reraise_library_exception_if_any");
      begin
         Reraise_Library_Exception_If_Any;
      end;
   end finalize_library;

   procedure adafinal is
      procedure s_stalib_adafinal;
      pragma Import (C, s_stalib_adafinal, "system__standard_library__adafinal");

      procedure Runtime_Finalize;
      pragma Import (C, Runtime_Finalize, "__gnat_runtime_finalize");

   begin
      if not Is_Elaborated then
         return;
      end if;
      Is_Elaborated := False;
      Runtime_Finalize;
      s_stalib_adafinal;
   end adafinal;

   type No_Param_Proc is access procedure;

   procedure adainit is
      Main_Priority : Integer;
      pragma Import (C, Main_Priority, "__gl_main_priority");
      Time_Slice_Value : Integer;
      pragma Import (C, Time_Slice_Value, "__gl_time_slice_val");
      WC_Encoding : Character;
      pragma Import (C, WC_Encoding, "__gl_wc_encoding");
      Locking_Policy : Character;
      pragma Import (C, Locking_Policy, "__gl_locking_policy");
      Queuing_Policy : Character;
      pragma Import (C, Queuing_Policy, "__gl_queuing_policy");
      Task_Dispatching_Policy : Character;
      pragma Import (C, Task_Dispatching_Policy, "__gl_task_dispatching_policy");
      Priority_Specific_Dispatching : System.Address;
      pragma Import (C, Priority_Specific_Dispatching, "__gl_priority_specific_dispatching");
      Num_Specific_Dispatching : Integer;
      pragma Import (C, Num_Specific_Dispatching, "__gl_num_specific_dispatching");
      Main_CPU : Integer;
      pragma Import (C, Main_CPU, "__gl_main_cpu");
      Interrupt_States : System.Address;
      pragma Import (C, Interrupt_States, "__gl_interrupt_states");
      Num_Interrupt_States : Integer;
      pragma Import (C, Num_Interrupt_States, "__gl_num_interrupt_states");
      Unreserve_All_Interrupts : Integer;
      pragma Import (C, Unreserve_All_Interrupts, "__gl_unreserve_all_interrupts");
      Detect_Blocking : Integer;
      pragma Import (C, Detect_Blocking, "__gl_detect_blocking");
      Default_Stack_Size : Integer;
      pragma Import (C, Default_Stack_Size, "__gl_default_stack_size");
      Leap_Seconds_Support : Integer;
      pragma Import (C, Leap_Seconds_Support, "__gl_leap_seconds_support");
      Bind_Env_Addr : System.Address;
      pragma Import (C, Bind_Env_Addr, "__gl_bind_env_addr");

      procedure Runtime_Initialize (Install_Handler : Integer);
      pragma Import (C, Runtime_Initialize, "__gnat_runtime_initialize");

      Finalize_Library_Objects : No_Param_Proc;
      pragma Import (C, Finalize_Library_Objects, "__gnat_finalize_library_objects");
   begin
      if Is_Elaborated then
         return;
      end if;
      Is_Elaborated := True;
      Main_Priority := -1;
      Time_Slice_Value := -1;
      WC_Encoding := 'b';
      Locking_Policy := ' ';
      Queuing_Policy := ' ';
      Task_Dispatching_Policy := ' ';
      System.Restrictions.Run_Time_Restrictions :=
        (Set =>
          (False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, True, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False),
         Value => (0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         Violated =>
          (True, False, False, True, True, False, False, True, 
           False, False, True, True, True, True, False, False, 
           True, False, False, True, True, False, True, True, 
           False, True, True, True, True, False, True, False, 
           False, False, True, False, True, True, False, True, 
           True, True, True, False, True, False, True, True, 
           False, False, True, False, True, False, False, False, 
           True, False, True, True, True, True, True, False, 
           False, True, False, True, True, True, False, True, 
           True, False, True, True, True, True, False, False, 
           True, False, False, False, True, True, True, True, 
           False, True, False),
         Count => (0, 0, 0, 1, 4, 5, 2, 0, 4, 0),
         Unknown => (False, False, False, False, False, False, True, False, True, False));
      Priority_Specific_Dispatching :=
        Local_Priority_Specific_Dispatching'Address;
      Num_Specific_Dispatching := 0;
      Main_CPU := -1;
      Interrupt_States := Local_Interrupt_States'Address;
      Num_Interrupt_States := 0;
      Unreserve_All_Interrupts := 0;
      Detect_Blocking := 0;
      Default_Stack_Size := -1;
      Leap_Seconds_Support := 0;

      Runtime_Initialize (1);

      Finalize_Library_Objects := finalize_library'access;

      System.Soft_Links'Elab_Spec;
      System.Exception_Table'Elab_Body;
      E025 := E025 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E100 := E100 + 1;
      Ada.Numerics'Elab_Spec;
      E165 := E165 + 1;
      Ada.Strings'Elab_Spec;
      E048 := E048 + 1;
      Ada.Strings.Maps'Elab_Spec;
      Ada.Strings.Maps.Constants'Elab_Spec;
      E054 := E054 + 1;
      Ada.Tags'Elab_Spec;
      Ada.Streams'Elab_Spec;
      E099 := E099 + 1;
      Interfaces.C'Elab_Spec;
      Interfaces.C.Strings'Elab_Spec;
      System.Exceptions'Elab_Spec;
      E027 := E027 + 1;
      System.File_Control_Block'Elab_Spec;
      E118 := E118 + 1;
      Ada.Streams.Stream_Io'Elab_Spec;
      E198 := E198 + 1;
      System.Finalization_Root'Elab_Spec;
      E113 := E113 + 1;
      Ada.Finalization'Elab_Spec;
      E111 := E111 + 1;
      System.Storage_Pools'Elab_Spec;
      E184 := E184 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Storage_Pools.Subpools'Elab_Spec;
      System.Task_Info'Elab_Spec;
      E152 := E152 + 1;
      Ada.Calendar'Elab_Spec;
      Ada.Calendar'Elab_Body;
      E008 := E008 + 1;
      Ada.Calendar.Delays'Elab_Body;
      E006 := E006 + 1;
      System.Object_Reader'Elab_Spec;
      System.Dwarf_Lines'Elab_Spec;
      System.Pool_Global'Elab_Spec;
      E180 := E180 + 1;
      System.Random_Seed'Elab_Body;
      E246 := E246 + 1;
      E186 := E186 + 1;
      System.Finalization_Masters'Elab_Body;
      E188 := E188 + 1;
      System.File_Io'Elab_Body;
      E110 := E110 + 1;
      E142 := E142 + 1;
      E065 := E065 + 1;
      Ada.Tags'Elab_Body;
      E102 := E102 + 1;
      E050 := E050 + 1;
      System.Soft_Links'Elab_Body;
      E015 := E015 + 1;
      System.Os_Lib'Elab_Body;
      E115 := E115 + 1;
      System.Secondary_Stack'Elab_Body;
      E019 := E019 + 1;
      E043 := E043 + 1;
      E063 := E063 + 1;
      System.Traceback.Symbolic'Elab_Body;
      E038 := E038 + 1;
      System.Tasking.Initialization'Elab_Body;
      E222 := E222 + 1;
      Ada.Real_Time'Elab_Spec;
      Ada.Real_Time'Elab_Body;
      E136 := E136 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E097 := E097 + 1;
      System.Tasking.Protected_Objects'Elab_Body;
      E204 := E204 + 1;
      System.Tasking.Protected_Objects.Entries'Elab_Spec;
      E218 := E218 + 1;
      System.Tasking.Queuing'Elab_Body;
      E230 := E230 + 1;
      System.Tasking.Stages'Elab_Body;
      E238 := E238 + 1;
      JEWL'ELAB_SPEC;
      E169 := E169 + 1;
      E210 := E210 + 1;
      JEWL.CANVAS_IMPLEMENTATION'ELAB_SPEC;
      E202 := E202 + 1;
      JEWL.WINDOW_IMPLEMENTATION'ELAB_SPEC;
      JEWL.MESSAGE_HANDLING'ELAB_SPEC;
      JEWL.MESSAGE_HANDLING'ELAB_BODY;
      E214 := E214 + 1;
      E240 := E240 + 1;
      E242 := E242 + 1;
      JEWL.SIMPLE_WINDOWS'ELAB_SPEC;
      JEWL.SIMPLE_WINDOWS'ELAB_BODY;
      E196 := E196 + 1;
      E167 := E167 + 1;
      E164 := E164 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_main");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer
   is
      procedure Initialize (Addr : System.Address);
      pragma Import (C, Initialize, "__gnat_initialize");

      procedure Finalize;
      pragma Import (C, Finalize, "__gnat_finalize");
      SEH : aliased array (1 .. 2) of Integer;

      Ensure_Reference : aliased System.Address := Ada_Main_Program_Name'Address;
      pragma Volatile (Ensure_Reference);

   begin
      gnat_argc := argc;
      gnat_argv := argv;
      gnat_envp := envp;

      Initialize (SEH'Address);
      adainit;
      Ada_Main_Program;
      adafinal;
      Finalize;
      return (gnat_exit_status);
   end;

--  BEGIN Object file/option list
   --   C:\Users\candi\Documents\Programming\Ada\jewl.o
   --   C:\Users\candi\Documents\Programming\Ada\jewl-win32_interface.o
   --   C:\Users\candi\Documents\Programming\Ada\jewl-canvas_implementation.o
   --   C:\Users\candi\Documents\Programming\Ada\jewl-message_handling.o
   --   C:\Users\candi\Documents\Programming\Ada\jewl-window_implementation.o
   --   C:\Users\candi\Documents\Programming\Ada\jewl-windows.o
   --   C:\Users\candi\Documents\Programming\Ada\jewl-simple_windows.o
   --   C:\Users\candi\Documents\Programming\Ada\gameboard.o
   --   C:\Users\candi\Documents\Programming\Ada\computer.o
   --   C:\Users\candi\Documents\Programming\Ada\main.o
   --   -LC:\Users\candi\Documents\Programming\Ada\
   --   -LC:\Users\candi\Documents\Programming\Ada\
   --   -LC:/gnat/2016/lib/gcc/i686-pc-mingw32/4.9.4/adalib/
   --   -static
   --   -luser32
   --   -lgdi32
   --   -lcomdlg32
   --   -lwinmm
   --   -lgnarl
   --   -lgnat
   --   -Xlinker
   --   --stack=0x200000,0x1000
   --   -mthreads
   --   -Wl,--stack=0x2000000
--  END Object file/option list   

end ada_main;
