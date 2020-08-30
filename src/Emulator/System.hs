module Emulator.System where

import Emulator.CPU (CPU)
import SDL (Renderer)

data SystemState s = SystemState
    { s_cpu :: CPU s
    , s_renderer :: Renderer
    , s_stepsPerFrame :: Int
    }