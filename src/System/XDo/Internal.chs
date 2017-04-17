{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards          #-}

module System.XDo.Internal where

import           Foreign.C
import           Foreign.C.Error
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Storable

import qualified Graphics.X11.Types      as X11
import qualified Graphics.X11.Xlib.Types as X11

#include <xdo.h>

-- | An XDo context; analogous to ~const xdo_t*~ from ~xdo.h~.
type XDC = Ptr ()

-- | A mutable XDo context; analogous to ~xdo_t*~ from ~xdo.h~.
type MutableXDC = Ptr ()

type XPos     = CInt
type YPos     = CInt
type Width    = CUInt
type Height   = CUInt
type ScreenID = CInt
type Button   = CInt
type Repeat   = CInt
type Delay    = {# type useconds_t #}
type KeySeq   = CString
type MapState = CInt
type PID      = CInt
type Desktop  = CLong
type RetVal   = CInt

data CharCodeMap -- FIXME
data Search

foreign import ccall "xdo.h xdo_new"
  c_new :: CString -- display
        -> IO MutableXDC
foreign import ccall "xdo.h xdo_new_with_opened_display"
  c_new_with_opened_display :: X11.Display   -- xdpy
                            -> CString       -- display
                            -> CInt          -- close_display_when_freed
                            -> IO MutableXDC
foreign import ccall "xdo.h xdo_version"
  c_version :: IO CString
foreign import ccall "xdo.h xdo_free"
  c_free :: MutableXDC -> IO ()
foreign import ccall "xdo.h xdo_move_mouse"
  c_move_mouse :: XDC -> XPos -> YPos -> ScreenID -> IO RetVal
foreign import ccall "xdo.h xdo_move_mouse_relative_to_window"
  c_move_mouse_relative_to_window :: XDC -> X11.Window -> XPos -> YPos -> IO RetVal
foreign import ccall "xdo.h xdo_move_mouse_relative"
  c_move_mouse_relative :: XDC -> XPos -> YPos -> IO RetVal
foreign import ccall "xdo.h xdo_mouse_down"
  c_mouse_down :: XDC -> X11.Window -> Button -> IO RetVal
foreign import ccall "xdo.h xdo_mouse_up"
  c_mouse_up :: XDC -> X11.Window -> Button -> IO RetVal
foreign import ccall "xdo.h xdo_get_mouse_location"
  c_get_mouse_location :: XDC -> Ptr XPos -> Ptr YPos -> Ptr ScreenID -> IO RetVal
foreign import ccall "xdo.h xdo_get_window_at_mouse"
  c_get_window_at_mouse :: XDC -> Ptr X11.Window -> IO RetVal
foreign import ccall "xdo.h xdo_wait_for_mouse_move_from"
  c_wait_for_mouse_move_from :: XDC -> XPos -> YPos -> IO RetVal
foreign import ccall "xdo.h xdo_wait_for_mouse_move_to"
  c_wait_for_mouse_move_to :: XDC -> XPos -> YPos -> IO RetVal
foreign import ccall "xdo.h xdo_click_window"
  c_click_window :: XDC -> X11.Window -> Button -> IO RetVal
foreign import ccall "xdo.h xdo_click_window_multiple"
  c_click_window_multiple :: XDC -> X11.Window -> Button -> Delay -> IO RetVal
foreign import ccall "xdo.h xdo_enter_text_window"
  c_enter_text_window :: XDC -> X11.Window -> CString -> Delay -> IO RetVal
foreign import ccall "xdo.h xdo_send_keysequence_window"
  c_send_keyseq_window :: XDC -> X11.Window -> KeySeq -> Delay -> IO RetVal
foreign import ccall "xdo.h xdo_send_keysequence_window_up"
  c_send_keyseq_window_up :: XDC -> X11.Window -> KeySeq -> Delay -> IO RetVal
foreign import ccall "xdo.h xdo_send_keysequence_window_down"
  c_send_keyseq_window_down :: XDC -> X11.Window -> KeySeq -> Delay -> IO RetVal
foreign import ccall "xdo.h xdo_send_keysequence_window_list_do"
  c_send_keyseq_window_list_do :: XDC -> X11.Window
                               -> Ptr CharCodeMap -- keys
                               -> CInt            -- nkeys
                               -> CInt            -- pressed
                               -> Ptr CInt        -- modifier
                               -> Delay           -- delay
                               -> IO RetVal
foreign import ccall "xdo.h xdo_get_active_keys_to_keycode_list"
  c_get_active_keys_to_keycode_list :: XDC
                                    -> Ptr (Ptr CharCodeMap) -- keys
                                    -> Ptr CInt              -- nkeys
                                    -> IO RetVal
foreign import ccall "xdo.h xdo_wait_for_window_map_state"
  c_wait_for_window_map_state :: XDC -> X11.Window -> MapState -> IO RetVal
foreign import ccall "xdo.h xdo_wait_for_window_size"
  c_wait_for_window_size :: XDC -> X11.Window -> Width -> Height
                         -> CInt -- flags
                         -> CInt -- to_or_from
                         -> IO RetVal
foreign import ccall "xdo.h xdo_move_window"
  c_move_window :: XDC -> X11.Window -> XPos -> YPos -> IO RetVal
foreign import ccall "xdo.h xdo_translate_window_with_sizehint"
  c_translate_window_with_sizehint :: XDC -> X11.Window
                                   -> Width -> Height
                                   -> Ptr Width -> Ptr Height
                                   -> IO RetVal
foreign import ccall "xdo.h xdo_set_window_size"
  c_set_window_size :: XDC -> X11.Window
                    -> CInt -- width
                    -> CInt -- height
                    -> CInt -- flags
                    -> IO RetVal
foreign import ccall "xdo.h xdo_set_window_property"
  c_set_window_property :: XDC -> X11.Window
                        -> CString -- property
                        -> CString -- value
                        -> IO RetVal
foreign import ccall "xdo.h xdo_set_window_class"
  c_set_window_class :: XDC -> X11.Window
                     -> CString -- name
                     -> CString -- class
                     -> IO RetVal
foreign import ccall "xdo.h xdo_set_window_urgency"
  c_set_window_urgency :: XDC -> X11.Window
                       -> CInt -- urgency
                       -> IO RetVal
foreign import ccall "xdo.h xdo_set_window_override_redirect"
  c_set_window_override_redirect :: XDC -> X11.Window
                                 -> CInt -- override_redirect
                                 -> IO RetVal
foreign import ccall "xdo.h xdo_focus_window"
  c_focus_window :: XDC -> X11.Window -> IO RetVal
foreign import ccall "xdo.h xdo_raise_window"
  c_raise_window :: XDC -> X11.Window -> IO RetVal
foreign import ccall "xdo.h xdo_activate_window"
  c_activate_window :: XDC -> X11.Window -> IO RetVal
foreign import ccall "xdo.h xdo_map_window"
  c_map_window :: XDC -> X11.Window -> IO RetVal
foreign import ccall "xdo.h xdo_unmap_window"
  c_unmap_window :: XDC -> X11.Window -> IO RetVal
foreign import ccall "xdo.h xdo_minimize_window"
  c_minimize_window :: XDC -> X11.Window -> IO RetVal
foreign import ccall "xdo.h xdo_reparent_window"
  c_reparent_window :: XDC
                    -> X11.Window -- wid_source
                    -> X11.Window -- wid_target
                    -> IO RetVal
foreign import ccall "xdo.h xdo_get_focused_window"
  c_get_focused :: XDC -> Ptr X11.Window -> IO RetVal
foreign import ccall "xdo.h xdo_get_focused_window_sane"
  c_get_focused_sane :: XDC -> Ptr X11.Window -> IO RetVal
foreign import ccall "xdo.h xdo_wait_for_window_focus"
  c_wait_for_window_focus :: XDC -> X11.Window
                          -> CInt -- want_focus
                          -> IO RetVal
foreign import ccall "xdo.h xdo_get_pid_window"
  c_get_pid_window :: XDC -> X11.Window -> IO PID
foreign import ccall "xdo.h xdo_wait_for_window_active"
  c_wait_for_window_active :: XDC -> X11.Window
                           -> CInt -- active
                           -> IO PID
foreign import ccall "xdo.h xdo_get_window_location"
  c_get_window_location :: XDC -> X11.Window -> Ptr XPos -> Ptr YPos
                        -> Ptr (Ptr X11.Screen) -> IO RetVal
foreign import ccall "xdo.h xdo_get_window_size"
  c_get_window_size :: XDC -> X11.Window -> Ptr Width -> Ptr Height -> IO RetVal
foreign import ccall "xdo.h xdo_get_active_window"
  c_get_active_window :: XDC -> Ptr X11.Window -> IO RetVal
foreign import ccall "xdo.h xdo_select_window_with_click"
  c_select_window_with_click :: XDC -> Ptr X11.Window -> IO RetVal
foreign import ccall "xdo.h xdo_set_number_of_desktops"
  c_set_number_of_desktops :: XDC -> CLong -> IO RetVal
foreign import ccall "xdo.h xdo_get_number_of_desktops"
  c_get_number_of_desktops :: XDC -> Ptr CLong -> IO RetVal
foreign import ccall "xdo.h xdo_set_current_desktop"
  c_set_current_desktop :: XDC -> Desktop -> IO RetVal
foreign import ccall "xdo.h xdo_get_current_desktop"
  c_get_current_desktop :: XDC -> Ptr Desktop -> IO RetVal
foreign import ccall "xdo.h xdo_set_desktop_for_window"
  c_set_desktop_for_window :: XDC -> X11.Window -> Desktop -> IO RetVal
foreign import ccall "xdo.h xdo_get_desktop_for_window"
  c_get_desktop_for_window :: XDC -> X11.Window -> Ptr Desktop -> IO RetVal
foreign import ccall "xdo.h xdo_search_windows"
  c_search_windows :: XDC -> Ptr Search
                   -> Ptr (Ptr X11.Window) -- windowlist_ret
                   -> Ptr CUInt            -- nwindows_ret
                   -> IO RetVal
foreign import ccall "xdo.h xdo_get_window_property_by_atom"
  c_get_window_property_by_atom :: XDC -> X11.Window
                                -> X11.Atom     -- atom
                                -> Ptr CLong    -- nitems
                                -> Ptr X11.Atom -- type
                                -> Ptr CInt     -- size
                                -> IO CUChar
foreign import ccall "xdo.h xdo_get_window_property"
  c_get_window_property :: XDC -> X11.Window
                        -> CString          -- property
                        -> Ptr (Ptr CUChar) -- value
                        -> Ptr CLong        -- nitems
                        -> Ptr X11.Atom     -- type
                        -> Ptr CInt         -- size
                        -> IO RetVal
foreign import ccall "xdo.h xdo_get_input_state"
  c_get_input_state :: XDC -> IO CUInt
foreign import ccall "xdo.h xdo_get_symbol_map"
  c_get_symbol_map :: IO (Ptr CString)
foreign import ccall "xdo.h xdo_get_active_modifiers"
  c_get_active_modifiers :: XDC
                         -> Ptr (Ptr CharCodeMap) -- keys
                         -> Ptr CInt              -- nkeys
                         -> IO RetVal
foreign import ccall "xdo.h xdo_clear_active_modifiers"
  c_clear_active_modifiers :: XDC -> X11.Window
                           -> Ptr CharCodeMap -- active_mods
                           -> CInt            -- active_mods_n
                           -> IO RetVal
foreign import ccall "xdo.h xdo_set_active_modifiers"
  c_set_active_modifiers :: XDC -> X11.Window
                         -> Ptr CharCodeMap -- active_mods
                         -> CInt            -- active_mods_n
                         -> IO RetVal
foreign import ccall "xdo.h xdo_get_desktop_viewport"
  c_get_desktop_viewport :: XDC -> Ptr XPos -> Ptr YPos -> IO RetVal
foreign import ccall "xdo.h xdo_set_desktop_viewport"
  c_set_desktop_viewport :: XDC -> XPos -> YPos -> IO RetVal
foreign import ccall "xdo.h xdo_kill_window"
  c_kill_window :: XDC -> X11.Window -> IO RetVal
foreign import ccall "xdo.h xdo_close_window"
  c_close_window :: XDC -> X11.Window -> IO RetVal

foreign import ccall "xdo.h xdo_find_window_client"
  c_find_window_client :: XDC -> X11.Window -> Ptr X11.Window
                       -> CInt -- direction
                       -> IO RetVal
foreign import ccall "xdo.h xdo_get_window_name"
  c_get_window_name :: XDC -> X11.Window
                    -> Ptr (Ptr CUChar) -- name_ret
                    -> Ptr CInt         -- name_len_ret
                    -> Ptr CInt         -- name_type
                    -> IO RetVal
foreign import ccall "xdo.h xdo_disable_feature"
  c_disable_feature :: XDC
                    -> CInt -- feature
                    -> IO ()
foreign import ccall "xdo.h xdo_enable_feature"
  c_enable_feature :: XDC
                   -> CInt -- feature
                   -> IO ()
foreign import ccall "xdo.h xdo_has_feature"
  c_has_feature :: XDC
                -> CInt -- feature
                -> IO RetVal
foreign import ccall "xdo.h xdo_get_viewport_dimensions"
  c_get_viewport_dimensions :: XDC -> Ptr Width -> Ptr Height -> ScreenID
                            -> IO RetVal
