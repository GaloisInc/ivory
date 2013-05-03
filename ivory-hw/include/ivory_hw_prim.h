/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 4 -*- */
/*
 * ivory_hw_prim.h --- Ivory HW primitives for I/O register access.
 *
 * Copyright (C) 2013, Galois, Inc.
 * All Rights Reserved.
 */

#ifndef __SMACCMPILOT_IVORY_HW_PRIM_H__
#define __SMACCMPILOT_IVORY_HW_PRIM_H__

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

static inline uint8_t ivory_hw_io_read_u8(uint32_t addr)
{
    return *((volatile uint8_t *)addr);
}

static inline void ivory_hw_io_write_u8(uint32_t addr, uint8_t value)
{
    *((volatile uint8_t *)addr) = value;
}

static inline uint16_t ivory_hw_io_read_u16(uint32_t addr)
{
    return *((volatile uint16_t *)addr);
}

static inline void ivory_hw_io_write_u16(uint32_t addr, uint16_t value)
{
    *((volatile uint16_t *)addr) = value;
}

static inline uint32_t ivory_hw_io_read_u32(uint32_t addr)
{
    return *((volatile uint32_t *)addr);
}

static inline void ivory_hw_io_write_u32(uint32_t addr, uint32_t value)
{
    *((volatile uint32_t *)addr) = value;
}

#ifdef __cplusplus
}
#endif

#endif  /* !defined __SMACCMPILOT_IVORY_HW_PRIM_H__ */
