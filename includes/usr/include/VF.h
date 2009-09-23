/*
 * VF.h
 *
 *  Programmmed by Hirotsugu Kakugawa, Hiroshima University
 *  E-Mail:  h.kakugawa@computer.org
 *
 *  Edition History
 *  29 Oct 1993
 *  28 Dec 1993  Added VF_GetOutline().
 *  20 Jan 1994  Added VF_GetOutline2().
 *  10 May 1995  Added VF_ReserveBitmap().
 *  12 May 1995  Added VF_HAVE_RESERVEBITMAP
 *
 */


/* This file is part of VFlib
 *
 * Copyright (C) 1993-1998  Hirotsugu KAKUGAWA.   All rights reserved.
 *
 * This file is part of the VFlib Library.  This library is free
 * software; you can redistribute it and/or modify it under the terms of
 * the GNU Library General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your
 * option) any later version.  This library is distributed in the hope
 * that it will be useful, but WITHOUT ANY WARRANTY; without even the
 * implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
 * PURPOSE.  See the GNU Library General Public License for more details.
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */


#ifndef __VF_H__
#define __VF_H__


/**
 ** EXPORTED FUNCTIONS
 **/
#define VF_HAVE_RESERVEBITMAP
int     VF_Init();
int     VF_Deinit();
int     VF_OpenFont();
int     VF_CloseFont();
int     VF_CloseAllFonts();
int     VF_GetBitmap();
int     VF_ReserveBitmap();
long*   VF_GetOutline();
long*   VF_GetOutline2();
int     VF_DrawOutline();
int     VF_FreeOutline();
char    *VF_Fn2Ent_TeX();
char    *VF_Fn2Ent_AsItIs();
void    VF_SetFn2EnTFunc();


/** 
 ** FOR OUTLINE DATA
 **/
#define VFD_TOKEN       0x80000000L
#define VFD_CHAR        0x80000001L
#define VFD_CWCURV      0x80000002L
#define VFD_CCWCURV     0x80000004L
#define VFD_LINE        0x80000008L
#define VFD_ARC         0x80000010L
#define VFD_BEZ         0x80000020L
#define VFD_X_MASK      0x7fff0000L
#define VFD_Y_MASK      0x00007fffL
#define VFD_GET_X(n)      (((n)&VFD_X_MASK) >> 16)
#define VFD_GET_Y(n)      (((n)&VFD_Y_MASK))
#define VFD_MAKE_XY(x,y)  (((long)(x)<<16) | (long)(y))
#define	OUTLINE_RANGE   32768
#define	OUTLINE_SIZE     8192
#define	OUTLINE_OFFSET  ((OUTLINE_RANGE-OUTLINE_SIZE)/2)


/**
 ** Coordinate Class for Outline Data
 **/
#define  VF_SONY_COORDINATES         1
#define  VF_ZEIT_COORDINATES         2
#define  VF_JG_COORDINATES           3
#define  VF_TT_COORDINATES           4

#endif /* __VF_H__ */ 
