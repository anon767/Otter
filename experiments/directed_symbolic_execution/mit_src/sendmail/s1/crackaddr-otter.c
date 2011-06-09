
/*

   MIT Copyright Notice

   Copyright 2003 M.I.T.

   Permission is hereby granted, without written agreement or royalty fee, to use,
   copy, modify, and distribute this software and its documentation for any
   purpose, provided that the above copyright notice and the following three
   paragraphs appear in all copies of this software.

   IN NO EVENT SHALL M.I.T. BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT, SPECIAL,
   INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OF THIS SOFTWARE
   AND ITS DOCUMENTATION, EVEN IF M.I.T. HAS BEEN ADVISED OF THE POSSIBILITY OF
   SUCH DAMANGE.

   M.I.T. SPECIFICALLY DISCLAIMS ANY WARRANTIES INCLUDING, BUT NOT LIMITED TO
   THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE,
   AND NON-INFRINGEMENT.

   THE SOFTWARE IS PROVIDED ON AN "AS-IS" BASIS AND M.I.T. HAS NO OBLIGATION TO
   PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.

   $Author: tleek $
   $Date: 2004/01/05 17:27:48 $
   $Header: /mnt/leo2/cvs/sabo/hist-040105/sendmail/s1/crackaddr-bad.c,v 1.1.1.1 2004/01/05 17:27:48 tleek Exp $



*/


/*

   Sendmail Copyright Notice


   Copyright (c) 1998-2003 Sendmail, Inc. and its suppliers.
   All rights reserved.
   Copyright (c) 1983, 1995-1997 Eric P. Allman.  All rights reserved.
   Copyright (c) 1988, 1993
   The Regents of the University of California.  All rights reserved.

   By using this file, you agree to the terms and conditions set
   forth in the LICENSE file which can be found at the top level of
   the sendmail distribution.


   $Author: tleek $
   $Date: 2004/01/05 17:27:48 $
   $Header: /mnt/leo2/cvs/sabo/hist-040105/sendmail/s1/crackaddr-bad.c,v 1.1.1.1 2004/01/05 17:27:48 tleek Exp $



*/


/*

   <source>

*/

// NB: Derived from sendmail/headers.c

/*
 * Copyright (c) 1998-2003 Sendmail, Inc. and its suppliers.
 *      All rights reserved.
 * Copyright (c) 1983, 1995-1997 Eric P. Allman.  All rights reserved.
 * Copyright (c) 1988, 1993
 *      The Regents of the University of California.  All rights reserved.
 *
 * By using this file, you agree to the terms and conditions set
 * forth in the LICENSE file which can be found at the top level of
 * the sendmail distribution.
 *
 */




#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/* ccured needs this */
//#pragma ccuredvararg("scanf", printf(1))

#define MACROEXPAND    ((unsigned char)0201)    /* macro expansion */
#define MAXNAME        30                       /* max length of a name */
#define MAXMACROID     0377                     /* max macro id number */
#define MACBUFSIZE     4096                     /* max expanded macro buffer size */
#define MAXLINE        2048                     /* max line length */
#define CONDIF         ((unsigned char)0232)    /* conditional if-then */
#define CONDELSE       ((unsigned char)0233)    /* conditional else */
#define CONDFI         ((unsigned char)0234)    /* conditional fi */
#define H_FROM         0x00000100               /* this is a from-type field */
#define bitset(bit, word)  (((word) & (bit)) != 0)
#define bitidx(bit)        ((unsigned int) (bit) & 0xff)
enum bool {false, true};


typedef struct header
{
    char        *h_value;   /* the value of that field */
    unsigned long   h_flags;    /* status bits, see below */
} HDR;

typedef struct
{
    char        *mac_table[MAXMACROID + 1]; /* macros */
} MACROS_T;

typedef struct
{
    MACROS_T    mci_macro;  /* macro definitions */
} MCI;

typedef struct envelope ENVELOPE;
struct envelope {
    MCI         *e_mci;     /* connection info */
    MACROS_T    e_macro;    /* macro definitions */
    ENVELOPE    *e_parent;  /* the message this one encloses */
    HDR         *e_header;  /* head of header list */
};

int          ColonOkInAddr;    /* single colon legal in address */
char         *MustQuoteChars;  /* quote these characters in phrases */
int          MaxMacroRecursion;/* maximum depth of macro recursion */
MACROS_T     GlobalMacros;     /* Otter: need to set this up */


// Prateek added to get rid of debugging prints.
//#define printf if(0) printf

/*
 **  MACVALUE -- return uninterpreted value of a macro.
 **
 **    Does fancy path searching.
 **    The low level counterpart is macget.
 **
 **    Parameters:
 **        n -- the name of the macro.
 **        e -- envelope in which to start looking for the macro.
 **
 **    Returns:
 **        The value of n.
 **
 **    Side Effects:
 **        none.
 */
char *
macvalue(n, e)
    int n;
    register ENVELOPE *e;
{
    n = bitidx(n);
    if (e != NULL && e->e_mci != NULL)
    {
        register char *p = e->e_mci->mci_macro.mac_table[n];

        if (p != NULL)
            return p;
    }
    while (e != NULL)
    {
        register char *p = e->e_macro.mac_table[n];

        if (p != NULL)
            return p;
        if (e == e->e_parent)
            break;
        e = e->e_parent;
    }
    return GlobalMacros.mac_table[n];
}
/*
**  EXPAND -- macro expand a string using $x escapes.
**
**    Parameters:
**        s -- the string to expand.
**        buf -- the place to put the expansion.
**        bufsize -- the size of the buffer.
**        e -- envelope in which to work.
**
**    Returns:
**        none.
**
**    Side Effects:
**        none.
*/
void
expand(s, buf, bufsize, e)
    register char *s;
    register char *buf;
    size_t bufsize;
    register ENVELOPE *e;
{
    register char *xp;
    register char *q;
    enum bool skipping;        /* set if conditionally skipping output */
    enum bool recurse;        /* set if recursion required */
    size_t i;
    int skiplev;        /* skipping nesting level */
    int iflev;        /* if nesting level */
    char xbuf[MACBUFSIZE];
    static int explevel = 0;

    printf("Inside expand!\n");

    recurse = false;
    skipping = false;
    skiplev = 0;
    iflev = 0;
    if (s == NULL)
        s = "";
    for (xp = xbuf; *s != '\0'; s++)
    {
        int c;

        /*
         **  Check for non-ordinary (special?) character.
         **    'q' will be the interpolated quantity.
         */

        q = NULL;
        c = *s;
        switch (c & 0377)
        {
            case CONDIF:        /* see if var set */
                iflev++;
                c = *++s;
                if (skipping)
                    skiplev++;
                else
                {
                    char *mv;

                    mv = macvalue(c, e);
                    skipping = (mv == NULL || *mv == '\0');
                }
                continue;

            case CONDELSE:    /* change state of skipping */
                if (iflev == 0)
                    break;    /* XXX: error */
                if (skiplev == 0)
                    skipping = !skipping;
                continue;

            case CONDFI:        /* stop skipping */
                if (iflev == 0)
                    break;    /* XXX: error */
                iflev--;
                if (skiplev == 0)
                    skipping = false;
                if (skipping)
                    skiplev--;
                continue;

            case MACROEXPAND:    /* macro interpolation */
                c = bitidx(*++s);
                if (c != '\0')
                    q = macvalue(c, e);
                else
                {
                    s--;
                    q = NULL;
                }
                if (q == NULL)
                    continue;
                break;
        }

        /*
         **  Interpolate q or output one character
         */

        if (skipping || xp >= &xbuf[sizeof xbuf - 1])
            continue;
        if (q == NULL)
            *xp++ = c;
        else
        {
            /* copy to end of q or max space remaining in buf */
            while ((c = *q++) != '\0' && xp < &xbuf[sizeof xbuf - 1])
            {
                /* check for any sendmail metacharacters */
                if ((c & 0340) == 0200)
                    recurse = true;
                *xp++ = c;
            }
        }
    }
    *xp = '\0';

    /* recurse as appropriate */
    if (recurse)
    {
        if (explevel < MaxMacroRecursion)
        {
            explevel++;
            expand(xbuf, buf, bufsize, e);
            explevel--;
            return;
        }
        printf("expand: recursion too deep (%d max)",
                MaxMacroRecursion);
        exit(1);
    }

    /* copy results out */
    i = xp - xbuf;
    if (i >= bufsize)
        i = bufsize - 1;
    memmove(buf, xbuf, i);
    buf[i] = '\0';
}

/*
 **  CRACKADDR -- parse an address and turn it into a macro
 **
 **    This doesn't actually parse the address -- it just extracts
 **    it and replaces it with "$g".  The parse is totally ad hoc
 **    and isn't even guaranteed to leave something syntactically
 **    identical to what it started with.  However, it does leave
 **    something semantically identical.
 **
 **    This algorithm has been cleaned up to handle a wider range
 **    of cases -- notably quoted and backslash escaped strings.
 **    This modification makes it substantially better at preserving
 **    the original syntax.
 **
 **    Parameters:
 **        addr -- the address to be cracked.
 **
 **    Returns:
 **        a pointer to the new version.
 **
 **    Side Effects:
 **        none.
 **
 **    Warning:
 **        The return value is saved in local storage and should
 **        be copied if it is to be reused.
 */
char *
crackaddr(addr)
    register char *addr;
{
    register char *p;
    register char c;
    int cmtlev;
    int realcmtlev;
    int anglelev, realanglelev;
    int copylev;
    int bracklev;
    enum bool qmode;
    enum bool realqmode;
    enum bool skipping;
    enum bool putgmac = false;
    enum bool quoteit = false;
    enum bool gotangle = false;
    enum bool gotcolon = false;
    register char *bp;
    char *obp;
    char *buflim;
    char *bufhead;
    char *addrhead;
    /* Otter: the ordering of test_buf and buf is rotated. It seems to be compiler dependent. */
    static char test_buf[10]; /* will use as a canary to detect overflow */
    static char buf[MAXNAME + 1];
    /* of buf[] */

    strcpy(test_buf, "GOOD");

    printf("Inside crackaddr!\n");


    /* strip leading spaces */
    while (*addr != '\0' && isascii((int)*addr) && isspace((int)*addr))
        addr++;

    /*
     **  Start by assuming we have no angle brackets.  This will be
     **  adjusted later if we find them.
     */


    bp = bufhead = buf;
    obp = bp;
    buflim = &buf[sizeof buf - 7];
    p = addrhead = addr;
    copylev = anglelev = realanglelev = cmtlev = realcmtlev = 0;
    bracklev = 0;
    qmode = realqmode = false;

    printf("qmode = %d\n", qmode);

    while ((c = *p++) != '\0')
    {
        /*
         **  If the buffer is overful, go into a special "skipping"
         **  mode that tries to keep legal syntax but doesn't actually
         **  output things.
         */

        printf("c = %c\n", c);

        skipping = bp >= buflim;

        if (copylev > 0 && !skipping)
        {
            /*BAD*/
            *bp++ = c;
        }
        /* check for backslash escapes */
        if (c == '\\')
        {
            /* arrange to quote the address */
            if (cmtlev <= 0 && !qmode)
                quoteit = true;

            if ((c = *p++) == '\0')
            {
                /* too far */
                p--;
                goto putg;
            }
            if (copylev > 0 && !skipping)
            {
                /*BAD*/
                *bp++ = c;
            }
            goto putg;
        }

        /* check for quoted strings */
        if (c == '"' && cmtlev <= 0)
        {
            printf("quoted string...\n");
            qmode = !qmode;
            if (copylev > 0 && !skipping)
                realqmode = !realqmode;
            continue;
        }
        if (qmode)
            goto putg;

        /* check for comments */
        if (c == '(')
        {
            printf("left ( seen....\n");
            cmtlev++;

            /* allow space for closing paren */
            if (!skipping)
            {
                buflim--;
                realcmtlev++;
                if (copylev++ <= 0)
                {
                    if (bp != bufhead)
                    {
                        /*BAD*/
                        *bp++ = ' ';
                    }

                    /*BAD*/
                    *bp++ = c;
                }
            }
        }
        if (cmtlev > 0)
        {
            if (c == ')')
            {
                cmtlev--;
                copylev--;
                if (!skipping)
                {
                    realcmtlev--;
                    buflim++;
                }
            }
            continue;
        }
        else if (c == ')')
        {
            /* syntax error: unmatched ) */
            if (copylev > 0 && !skipping)
                bp--;
        }

        /* count nesting on [ ... ] (for IPv6 domain literals) */
        if (c == '[')
            bracklev++;
        else if (c == ']')
            bracklev--;

        /* check for group: list; syntax */
        if (c == ':' && anglelev <= 0 && bracklev <= 0 &&
                !gotcolon && !ColonOkInAddr)
        {
            register char *q;

            /*
             **  Check for DECnet phase IV ``::'' (host::user)
             **  or **  DECnet phase V ``:.'' syntaxes.  The latter
             **  covers ``user@DEC:.tay.myhost'' and
             **  ``DEC:.tay.myhost::user'' syntaxes (bletch).
             */

            if (*p == ':' || *p == '.')
            {
                if (cmtlev <= 0 && !qmode)
                    quoteit = true;
                if (copylev > 0 && !skipping)
                {
                    /*BAD*/
                    *bp++ = c;
                    /*BAD*/
                    *bp++ = *p;
                }
                p++;
                goto putg;
            }

            gotcolon = true;

            bp = bufhead;
            if (quoteit)
            {
                /*BAD*/
                *bp++ = '"';

                /* back up over the ':' and any spaces */
                --p;

                while (isascii((int) *--p) && isspace((int) *p))
                    continue;
                p++;
            }
            for (q = addrhead; q < p; )
            {
                c = *q++;
                if (bp < buflim)
                {
                    if (quoteit && c == '"')
                        /*BAD*/
                        *bp++ = '\\';
                    /*BAD*/
                    *bp++ = c;

                }
            }
            if (quoteit)
            {
                if (bp == &bufhead[1])
                    bp--;
                else{
                    /*BAD*/
                    *bp++ = '"';
                }
                while ((c = *p++) != ':')
                {
                    if (bp < buflim){
                        /*BAD*/
                        *bp++ = c;
                    }
                }
                /*BAD*/
                *bp++ = c;
            }

            /* any trailing white space is part of group: */
            while (isascii((int) *p) && isspace((int)*p) && bp < buflim)
            {
                /*BAD*/
                *bp++ = *p++;
            }
            copylev = 0;
            putgmac = quoteit = false;
            bufhead = bp;
            addrhead = p;
            continue;
        }

        if (c == ';' && copylev <= 0 && !ColonOkInAddr)
        {
            if (bp < buflim)
                /*BAD*/
                *bp++ = c;
        }

        /* check for characters that may have to be quoted */
        if (strchr(MustQuoteChars, c) != NULL)
        {
            /*
             **  If these occur as the phrase part of a <>
             **  construct, but are not inside of () or already
             **  quoted, they will have to be quoted.  Note that
             **  now (but don't actually do the quoting).
             */

            if (cmtlev <= 0 && !qmode)
                quoteit = true;
        }

        /* check for angle brackets */
        if (c == '<')
        {
            register char *q;

            /* assume first of two angles is bogus */
            if (gotangle)
                quoteit = true;
            gotangle = true;

            /* oops -- have to change our mind */
            anglelev = 1;
            if (!skipping)
                realanglelev = 1;

            bp = bufhead;
            if (quoteit)
            {
                /*BAD*/
                *bp++ = '"';

                /* back up over the '<' and any spaces */
                --p;
                while (isascii((int)*--p) && isspace((int)*p))
                    continue;
                p++;
            }
            for (q = addrhead; q < p; )
            {
                c = *q++;
                if (bp < buflim)
                {
                    if (quoteit && c == '"')
                        /*BAD*/
                        *bp++ = '\\';
                    /*BAD*/
                    *bp++ = c;
                }
            }
            if (quoteit)
            {
                if (bp == &buf[1])
                    bp--;
                else
                    /*BAD*/
                    *bp++ = '"';
                while ((c = *p++) != '<')
                {
                    if (bp < buflim)
                        /*BAD*/
                        *bp++ = c;
                }
                /*BAD*/
                *bp++ = c;
            }
            copylev = 0;
            putgmac = quoteit = false;
            continue;
        }

        if (c == '>')
        {
            if (anglelev > 0)
            {
                anglelev--;
                if (!skipping)
                {
                    realanglelev--;
                    buflim++;
                }
            }
            else if (!skipping)
            {
                /* syntax error: unmatched > */
                if (copylev > 0)
                    bp--;
                quoteit = true;
                continue;
            }
            if (copylev++ <= 0)
                /*BAD*/
                *bp++ = c;
            continue;
        }

        /* must be a real address character */
putg:
        if (copylev <= 0 && !putgmac)
        {
            if (bp > bufhead && bp[-1] == ')')
                /*BAD*/
                *bp++ = ' ';
            /*BAD*/
            *bp++ = MACROEXPAND;
            /*BAD*/
            *bp++ = 'g';
            putgmac = true;
        }
        printf("Buf = %s\n", buf);
    }

    /* repair any syntactic damage */
    if (realqmode)
        /*BAD*/
        *bp++ = '"';
    while (realcmtlev-- > 0)
        /*BAD*/
        *bp++ = ')';
    while (realanglelev-- > 0)
        /*BAD*/
        *bp++ = '>';
    /*BAD*/
    *bp++ = '\0';

    printf("test_buf should equal GOOD\n");
    printf("test_buf = %s\n", test_buf);

    return buf;
}

/*
 **  EATHEADER -- run through the stored header and extract info.
 **
 **    Parameters:
 **        e -- the envelope to process.
 **        full -- if set, do full processing (e.g., compute
 **            message priority).  This should not be set
 **            when reading a queue file because some info
 **            needed to compute the priority is wrong.
 **        log -- call logsender()?
 **
 **    Returns:
 **        none.
 **
 **    Side Effects:
 **        Sets a bunch of global variables from information
 **            in the collected header.
 */
/* Otter: 
 * 1. full and log are taken out.
 * 2. Possible advantage of CCBSE: doesn't need harness code 
 */
void
eatheader(e)
    register ENVELOPE *e;
{
    register HDR *h;
    char buf[MAXLINE];

    h = e->e_header;
    printf("Call expand!\n");
    expand(h->h_value, buf, sizeof buf, e);
    printf("Exit expand!\n");
    if (buf[0] != '\0')
    {
        if (bitset(H_FROM, h->h_flags))
            expand(crackaddr(buf), buf, sizeof buf, e);
    }
}

int main(){
    ENVELOPE e;
    HDR h;

    ColonOkInAddr = 0;  /* allow colon in address */
    MustQuoteChars = "@,;:\\()[].'";
    MaxMacroRecursion = 10;

    // TODO: setup GlobalMacros;

    h.h_value = "<><><><><><><><><><><><><><><><><><><><><>";
    e.e_header = &h;

    eatheader(&e);
    return 0;
}

int main2(){

    char address[100];
    char *res_addr;

    printf("Type 1 or 0 to allow or disallow colons in email address:\n");
    ColonOkInAddr = 0;  /* allow colon in address */

    MustQuoteChars = "@,;:\\()[].'";
    MaxMacroRecursion = 10;

    printf("Enter email address:\n");

    // Good: strcpy(address, "<><><><><><><><><><><><><><>");
    strcpy(address, "<><><><><><><><><><><><><><><><><><><><><>");

    /* Otter TODO: initialize eatheader instead */
    res_addr = crackaddr(address);
    printf("result = %s\n", res_addr);
    printf("buf len = %d\n", strlen(res_addr));

    return 0;
}

/*

   </source>

*/

