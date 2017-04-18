{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}

module System.XDo.Internal where

import           Control.Concurrent         (threadDelay)
import           Control.Monad              (void)
import           Control.Monad.State.Strict

import           Data.Word
import           Data.Int

import           Foreign.C
import           Foreign.C.Error
import           Foreign.C.String
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Storable
import           Foreign.Marshal.Alloc

import qualified Graphics.X11.Types         as X11
import qualified Graphics.X11.Xlib.Types    as X11

import           Control.DeepSeq            (force, NFData)
import           Data.Data                  (Data)
import           GHC.Generics               (Generic)

#include <xdo.h>
#define SEARCH_ANY 0
#define SEARCH_ALL 1

{# pointer *xdo_t as XDoContext newtype #}

{# enum define Search { SEARCH_ANY         as SearchAny
                      , SEARCH_ALL         as SearchAll
                      , SEARCH_CLASS       as SearchClass
                      , SEARCH_NAME        as SearchName
                      , SEARCH_PID         as SearchPID
                      , SEARCH_ONLYVISIBLE as SearchOnlyVisible
                      , SEARCH_SCREEN      as SearchScreen
                      , SEARCH_CLASSNAME   as SearchClassName
                      , SEARCH_DESKTOP     as SearchDesktop
                      } #}

newtype XDM a
  = XDM { runXDoM :: StateT XDoContext IO a }
  deriving (Functor, Applicative, Monad, MonadState XDoContext, MonadIO)

type XDC      = XDoContext
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
type CWChar   = {# type wchar_t #}
type KeyCode  = {# type KeyCode #}
type KeySym   = {# type KeySym #}
type KeyMask  = CInt

newtype LayoutIndex
  = MkLayoutIndex { fromLayoutIndex :: Int32 }
  deriving (Eq, Show, Generic, Data, NFData, Storable)

data CharCodeMap
  = MkCharCodeMap
    { ccmKey          :: {-# UNPACK #-} !CWChar
      -- ^ The letter for this key, e.g.: @"a"@.
    , ccmCode         :: {-# UNPACK #-} !KeyCode
      -- ^ The keycode that this key is on.
    , ccmSymbol       :: {-# UNPACK #-} !KeySym
      -- ^ The symbol representing this key.
    , ccmGroup        :: {-# UNPACK #-} !LayoutIndex
      -- ^ The keyboard group that has this key in it.
    , ccmModMask      :: {-# UNPACK #-} !KeyMask
      -- ^ The modifiers to apply when sending this key.
    , ccmNeedsBinding ::                !Bool
      -- ^ Does this key need to be bound at runtime because it does not exist
      --   in the current keymap?
    }
  deriving (Eq, Show, Generic)

instance NFData CharCodeMap

instance Storable CharCodeMap where
  alignment _ = {# alignof charcodemap_t #}
  sizeOf    _ = {# sizeof  charcodemap_t #}

  peek p = do
    let (<#>) = flip (<$>)
    let fromNB 1 = True
        fromNB _ = False

    ccmKey          <- ({# get charcodemap_t.key     #} p)
    ccmCode         <- ({# get charcodemap_t.code    #} p)
    ccmSymbol       <- ({# get charcodemap_t.symbol  #} p)
    ccmGroup        <- ({# get charcodemap_t.group   #} p)
                       <#> (\(CInt i) -> MkLayoutIndex i)
    ccmModMask      <- ({# get charcodemap_t.modmask #} p)
    ccmNeedsBinding <- ({# get charcodemap_t.needs_binding #} p)
                       <#> fromNB

    pure (MkCharCodeMap {..})

  poke p (MkCharCodeMap {..}) = do
    {# set charcodemap_t.key           #} p ccmKey
    {# set charcodemap_t.code          #} p ccmCode
    {# set charcodemap_t.symbol        #} p ccmSymbol
    {# set charcodemap_t.group         #} p (CInt (fromLayoutIndex ccmGroup))
    {# set charcodemap_t.modmask       #} p ccmModMask
    {# set charcodemap_t.needs_binding #} p (if ccmNeedsBinding then 1 else 0)


lissajousXDM :: XDM ()
lissajousXDM = do (w, h) <- getViewportDimensions scr
                  let (w', h') = (fromIntegral w / 2.0, fromIntegral h / 2.0)
                  let transform (x, y) = (w' * (1.0 + x), h' * (1.0 - y))
                  let move :: Float -> Float -> XDM ()
                      move x y = let (x', y') = transform (x, y)
                                 in void (moveMouse (round x') (round y') scr)
                  go move 0.0
  where
    go :: (Float -> Float -> XDM ()) -> Float -> XDM ()
    go move t = if t > 10000.0
                then pure ()
                else do let x = cos (t * a)
                        let y = sin (t * b)
                        move x y
                        liftIO $ threadDelay (round (dt * 100000.0))
                        go move (t + dt)

    a, b, dt :: Float
    (a, b) = (5.0, 4.0)
    dt = 0.0005
    scr = 0

withXDC' :: (NFData a) => String -> XDM a -> IO a
withXDC' display m = withCString display $ \cs -> do
  xdc <- c_new cs
  !result <- force <$> evalStateT (runXDoM m) xdc
  c_free xdc
  pure result

moveMouse :: XPos -> YPos -> ScreenID -> XDM RetVal
moveMouse x y screen = do
  xdc <- get
  liftIO $ c_move_mouse xdc x y screen

getViewport :: ScreenID -> XDM (XPos, YPos, Width, Height)
getViewport screen = do
  (x, y) <- getDesktopViewport
  (w, h) <- getViewportDimensions screen
  pure (x, y, w, h)

getDesktopViewport :: XDM (XPos, YPos)
getDesktopViewport = get >>= \xdc -> liftIO $ do
  x_ret <- malloc
  y_ret <- malloc
  c_get_desktop_viewport xdc x_ret y_ret
  x <- peek x_ret
  y <- peek y_ret
  free x_ret
  free y_ret
  pure (x, y)

getViewportDimensions :: ScreenID -> XDM (Width, Height)
getViewportDimensions screen = get >>= \xdc -> liftIO $ do
  w_ret <- malloc
  h_ret <- malloc
  c_get_viewport_dimensions xdc w_ret h_ret screen
  w <- peek w_ret
  h <- peek h_ret
  free w_ret
  free h_ret
  pure (w, h)


----------------------------- -- ^ xxx

foreign import ccall "xdo.h xdo_new"
  c_new
  :: CString                  -- ^ display
  -> IO XDC                   -- ^ xdo

foreign import ccall "xdo.h xdo_new_with_opened_display"
  c_new_with_opened_display
  :: X11.Display              -- ^ xdpy
  -> CString                  -- ^ display
  -> CInt                     -- ^ close_display_when_freed
  -> IO XDC                   -- ^ xdo

foreign import ccall "xdo.h xdo_version"
  c_version
  :: IO CString               -- ^ version

foreign import ccall "xdo.h xdo_free"
  c_free
  :: XDC                      -- ^ xdo
  -> IO ()

foreign import ccall "xdo.h xdo_move_mouse"
  c_move_mouse
  :: XDC                      -- ^ xdo
  -> XPos                     -- ^ x
  -> YPos                     -- ^ y
  -> ScreenID                 -- ^ screen
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_move_mouse_relative_to_window"
  c_move_mouse_relative_to_window
  :: XDC                      -- ^ xdo
  -> X11.Window               -- ^ window
  -> XPos                     -- ^ x
  -> YPos                     -- ^ y
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_move_mouse_relative"
  c_move_mouse_relative
  :: XDC                      -- ^ xdo
  -> XPos                     -- ^ x
  -> YPos                     -- ^ y
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_mouse_down"
  c_mouse_down
  :: XDC                      -- ^ xdo
  -> X11.Window               -- ^ window
  -> Button                   -- ^ button
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_mouse_up"
  c_mouse_up
  :: XDC                      -- ^ xdo
  -> X11.Window               -- ^ window
  -> Button                   -- ^ button
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_get_mouse_location"
  c_get_mouse_location
  :: XDC                      -- ^ xdo
  -> Ptr XPos                 -- ^ x_ret
  -> Ptr YPos                 -- ^ y_ret
  -> Ptr ScreenID             -- ^ screen_ret
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_get_window_at_mouse"
  c_get_window_at_mouse
  :: XDC                      -- ^ xdo
  -> Ptr X11.Window           -- ^ window_ret
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_wait_for_mouse_move_from"
  c_wait_for_mouse_move_from
  :: XDC                      -- ^ xdo
  -> XPos                     -- ^ x
  -> YPos                     -- ^ y
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_wait_for_mouse_move_to"
  c_wait_for_mouse_move_to
  :: XDC                      -- ^ xdo
  -> XPos                     -- ^ x
  -> YPos                     -- ^ y
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_click_window"
  c_click_window
  :: XDC                      -- ^ xdo
  -> X11.Window               -- ^ window
  -> Button                   -- ^ button
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_click_window_multiple"
  c_click_window_multiple
  :: XDC                      -- ^ xdo
  -> X11.Window               -- ^ window
  -> Button                   -- ^ button
  -> Delay
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_enter_text_window"
  c_enter_text_window
  :: XDC                      -- ^ xdo
  -> X11.Window               -- ^ window
  -> CString                  -- ^ text
  -> Delay                    -- ^ delay
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_send_keysequence_window"
  c_send_keyseq_window
  :: XDC                      -- ^ xdo
  -> X11.Window               -- ^ window
  -> KeySeq                   -- ^ keysequence
  -> Delay                    -- ^ delay
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_send_keysequence_window_up"
  c_send_keyseq_window_up
  :: XDC                      -- ^ xdo
  -> X11.Window               -- ^ window
  -> KeySeq                   -- ^ keysequence
  -> Delay                    -- ^ delay
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_send_keysequence_window_down"
  c_send_keyseq_window_down
  :: XDC                      -- ^ xdo
  -> X11.Window               -- ^ window
  -> KeySeq                   -- ^ keysequence
  -> Delay                    -- ^ delay
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_send_keysequence_window_list_do"
  c_send_keyseq_window_list_do
  :: XDC                      -- ^ xdo
  -> X11.Window               -- ^ window
  -> Ptr CharCodeMap          -- ^ keys
  -> CInt                     -- ^ nkeys
  -> CInt                     -- ^ pressed
  -> Ptr CInt                 -- ^ modifier
  -> Delay                    -- ^ delay
  -> IO RetVal                -- ^ retval

-- foreign import ccall "xdo.h xdo_get_active_keys_to_keycode_list"
--   c_get_active_keys_to_keycode_list
--   :: XDC                      -- ^ xdo
--   -> Ptr (Ptr CharCodeMap)    -- ^ keys
--   -> Ptr CInt                 -- ^ nkeys
--   -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_wait_for_window_map_state"
  c_wait_for_window_map_state
  :: XDC                      -- ^ xdo
  -> X11.Window               -- ^ window
  -> MapState                 -- ^ map_state
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_wait_for_window_size"
  c_wait_for_window_size
  :: XDC                      -- ^ xdo
  -> X11.Window               -- ^ window
  -> Width                    -- ^ width
  -> Height                   -- ^ height
  -> CInt                     -- ^ flags
  -> CInt                     -- ^ to_or_from
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_move_window"
  c_move_window
  :: XDC                      -- ^ xdo
  -> X11.Window               -- ^ window
  -> XPos                     -- ^ x
  -> YPos                     -- ^ y
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_translate_window_with_sizehint"
  c_translate_window_with_sizehint
  :: XDC                      -- ^ xdo
  -> X11.Window               -- ^ window
  -> Width                    -- ^ width
  -> Height                   -- ^ height
  -> Ptr Width                -- ^ width_ret
  -> Ptr Height               -- ^ height_ret
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_set_window_size"
  c_set_window_size
  :: XDC                      -- ^ xdo
  -> X11.Window               -- ^ window
  -> CInt                     -- ^ width
  -> CInt                     -- ^ height
  -> CInt                     -- ^ flags
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_set_window_property"
  c_set_window_property
  :: XDC                      -- ^ xdo
  -> X11.Window               -- ^ window
  -> CString                  -- ^ property
  -> CString                  -- ^ value
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_set_window_class"
  c_set_window_class
  :: XDC                      -- ^ xdo
  -> X11.Window               -- ^ window
  -> CString                  -- ^ name
  -> CString                  -- ^ class
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_set_window_urgency"
  c_set_window_urgency
  :: XDC                      -- ^ xdo
  -> X11.Window               -- ^ window
  -> CInt                     -- ^ urgency
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_set_window_override_redirect"
  c_set_window_override_redirect
  :: XDC                      -- ^ xdo
  -> X11.Window               -- ^ window
  -> CInt                     -- ^ override_redirect
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_focus_window"
  c_focus_window
  :: XDC                      -- ^ xdo
  -> X11.Window               -- ^ window
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_raise_window"
  c_raise_window
  :: XDC                      -- ^ xdo
  -> X11.Window               -- ^ window
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_activate_window"
  c_activate_window
  :: XDC                      -- ^ xdo
  -> X11.Window               -- ^ window
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_map_window"
  c_map_window
  :: XDC                      -- ^ xdo
  -> X11.Window               -- ^ window
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_unmap_window"
  c_unmap_window
  :: XDC                      -- ^ xdo
  -> X11.Window               -- ^ window
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_minimize_window"
  c_minimize_window
  :: XDC                      -- ^ xdo
  -> X11.Window               -- ^ window
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_reparent_window"
  c_reparent_window
  :: XDC                      -- ^ xdo
  -> X11.Window               -- ^ wid_source
  -> X11.Window               -- ^ wid_target
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_get_focused_window"
  c_get_focused
  :: XDC                      -- ^ xdo
  -> Ptr X11.Window           -- ^ window_ret
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_get_focused_window_sane"
  c_get_focused_sane
  :: XDC                      -- ^ xdo
  -> Ptr X11.Window           -- ^ window_ret
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_wait_for_window_focus"
  c_wait_for_window_focus
  :: XDC                      -- ^ xdo
  -> X11.Window               -- ^ window
  -> CInt                     -- ^ want_focus
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_get_pid_window"
  c_get_pid_window
  :: XDC                      -- ^ xdo
  -> X11.Window               -- ^ window
  -> IO PID                   -- ^ process ID

foreign import ccall "xdo.h xdo_wait_for_window_active"
  c_wait_for_window_active
  :: XDC                      -- ^ xdo
  -> X11.Window               -- ^ window
  -> CInt                     -- ^ active
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_get_window_location"
  c_get_window_location
  :: XDC                      -- ^ xdo
  -> X11.Window               -- ^ window
  -> Ptr XPos                 -- ^ x_ret
  -> Ptr YPos                 -- ^ y_ret
  -> Ptr (Ptr X11.Screen)     -- ^ screen_ret
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_get_window_size"
  c_get_window_size
  :: XDC                      -- ^ xdo
  -> X11.Window               -- ^ window
  -> Ptr Width                -- ^ width_ret
  -> Ptr Height               -- ^ height_ret
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_get_active_window"
  c_get_active_window
  :: XDC                      -- ^ xdo
  -> Ptr X11.Window           -- ^ window_ret
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_select_window_with_click"
  c_select_window_with_click
  :: XDC                      -- ^ xdo
  -> Ptr X11.Window           -- ^ window_ret
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_set_number_of_desktops"
  c_set_number_of_desktops
  :: XDC                      -- ^ xdo
  -> CLong                    -- ^ ndesktops
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_get_number_of_desktops"
  c_get_number_of_desktops
  :: XDC                      -- ^ xdo
  -> Ptr CLong                -- ^ ndesktops_ret
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_set_current_desktop"
  c_set_current_desktop
  :: XDC                      -- ^ xdo
  -> Desktop                  -- ^ desktop
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_get_current_desktop"
  c_get_current_desktop
  :: XDC                      -- ^ xdo
  -> Ptr Desktop              -- ^ desktop_ret
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_set_desktop_for_window"
  c_set_desktop_for_window
  :: XDC                      -- ^ xdo
  -> X11.Window               -- ^ window
  -> Desktop                  -- ^ desktop
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_get_desktop_for_window"
  c_get_desktop_for_window
  :: XDC                      -- ^ xdo
  -> X11.Window               -- ^ window
  -> Ptr Desktop              -- ^ desktop_ret
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_search_windows"
  c_search_windows
  :: XDC                      -- ^ xdo
  -> Ptr Search               -- ^ search
  -> Ptr (Ptr X11.Window)     -- ^ windowlist_ret
  -> Ptr CUInt                -- ^ nwindows_ret
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_get_window_property_by_atom"
  c_get_window_property_by_atom
  :: XDC                      -- ^ xdo
  -> X11.Window               -- ^ window
  -> X11.Atom                 -- ^ atom
  -> Ptr CLong                -- ^ nitems
  -> Ptr X11.Atom             -- ^ type
  -> Ptr CInt                 -- ^ size
  -> IO (Ptr CUChar)          -- ^ property

foreign import ccall "xdo.h xdo_get_window_property"
  c_get_window_property
  :: XDC                      -- ^ xdo
  -> X11.Window               -- ^ window
  -> CString                  -- ^ property
  -> Ptr (Ptr CUChar)         -- ^ value
  -> Ptr CLong                -- ^ nitems
  -> Ptr X11.Atom             -- ^ type
  -> Ptr CInt                 -- ^ size
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_get_input_state"
  c_get_input_state
  :: XDC                      -- ^ xdo
  -> IO CUInt                 -- ^ input state

foreign import ccall "xdo.h xdo_get_symbol_map"
  c_get_symbol_map
  :: IO (Ptr CString)         -- ^ symbol map

foreign import ccall "xdo.h xdo_get_active_modifiers"
  c_get_active_modifiers
  :: XDC                      -- ^ xdo
  -> Ptr (Ptr CharCodeMap)    -- ^ keys
  -> Ptr CInt                 -- ^ nkeys
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_clear_active_modifiers"
  c_clear_active_modifiers
  :: XDC                      -- ^ xdo
  -> X11.Window               -- ^ window
  -> Ptr CharCodeMap          -- ^ active_mods
  -> CInt                     -- ^ active_mods_n
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_set_active_modifiers"
  c_set_active_modifiers
  :: XDC                      -- ^ xdo
  -> X11.Window               -- ^ window
  -> Ptr CharCodeMap          -- ^ active_mods
  -> CInt                     -- ^ active_mods_n
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_get_desktop_viewport"
  c_get_desktop_viewport
  :: XDC                      -- ^ xdo
  -> Ptr XPos                 -- ^ x_ret
  -> Ptr YPos                 -- ^ y_ret
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_set_desktop_viewport"
  c_set_desktop_viewport
  :: XDC                      -- ^ xdo
  -> XPos                     -- ^ x
  -> YPos                     -- ^ y
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_kill_window"
  c_kill_window
  :: XDC                      -- ^ xdo
  -> X11.Window               -- ^ window
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_close_window"
  c_close_window
  :: XDC                      -- ^ xdo
  -> X11.Window               -- ^ window
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_find_window_client"
  c_find_window_client
  :: XDC                      -- ^ xdo
  -> X11.Window               -- ^ window
  -> Ptr X11.Window           -- ^ window_ret
  -> CInt                     -- ^ direction
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_get_window_name"
  c_get_window_name
  :: XDC                      -- ^ xdo
  -> X11.Window               -- ^ window
  -> Ptr (Ptr CUChar)         -- ^ name_ret
  -> Ptr CInt                 -- ^ name_len_ret
  -> Ptr CInt                 -- ^ name_type
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_disable_feature"
  c_disable_feature
  :: XDC                      -- ^ xdo
  -> CInt                     -- ^ feature
  -> IO ()

foreign import ccall "xdo.h xdo_enable_feature"
  c_enable_feature
  :: XDC                      -- ^ xdo
  -> CInt                     -- ^ feature
  -> IO ()

foreign import ccall "xdo.h xdo_has_feature"
  c_has_feature
  :: XDC                      -- ^ xdo
  -> CInt                     -- ^ feature
  -> IO RetVal                -- ^ retval

foreign import ccall "xdo.h xdo_get_viewport_dimensions"
  c_get_viewport_dimensions
  :: XDC                      -- ^ xdo
  -> Ptr Width                -- ^ width_ret
  -> Ptr Height               -- ^ height_ret
  -> ScreenID                 -- ^ screen
  -> IO RetVal                -- ^ retval
