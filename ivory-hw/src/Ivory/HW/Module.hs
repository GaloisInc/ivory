--
-- Module.hs --- Ivory module for 'ivory-hw'.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.HW.Module where

import Ivory.Language
import Ivory.HW.Prim

hwModule :: Module
hwModule = package "ivory_hw" $ do
  inclHeader "ivory_hw_prim.h"
  incl ioRegReadU8
  incl ioRegWriteU8
  incl ioRegReadU16
  incl ioRegWriteU16
  incl ioRegReadU32
  incl ioRegWriteU32
