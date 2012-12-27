/*
ledmon
Copyright (C) 2008 John Goerzen
<jgoerzen@complete.org>
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA
*/


#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/XKBlib.h>
#include <stdio.h>
#include <stdlib.h>

enum {
  CapsLock = 0,
  NumLock,
  ScrollLock
};

void displayState(unsigned int state);

static int xkb_event_base = 0;
static int xkb_error_base = 0;

int main(void) {
  Display *disp;
  int opcode;
  unsigned int state;
  int maj = XkbMajorVersion;
  int min = XkbMinorVersion;
  XkbEvent ev;

  /* Open Display */
  if ( !(disp = XOpenDisplay(NULL))) {
    fprintf(stderr, "Can't open display: CHECK DISPLAY VARIABLE\n");
    exit(1);
  }

  if (!XkbLibraryVersion(&maj, &min)) {
    fprintf(stderr, "Couldn't get Xkb library version\n");
    exit(1);
  }
  
  if (!XkbQueryExtension(disp, &opcode, &xkb_event_base, &xkb_error_base, &maj, &min)) {
    fprintf(stderr, "XkbQueryExtension error\n");
    exit(1);
  }

  if (!XkbSelectEvents(disp, XkbUseCoreKbd, XkbIndicatorStateNotifyMask, 
                       XkbIndicatorStateNotifyMask)) {
    fprintf(stderr, "XkbSelectEvents\n");
    exit(1);
  }

  XkbGetIndicatorState(disp, XkbUseCoreKbd, &state);
  displayState(state);
  
  /*
  win = XCreateSimpleWindow(disp, DefaultRootWindow(display), 0, 0, 200, 300, 5, white,
                            black);
  */

  while (1) {
    XNextEvent(disp, &ev.core);
    if (ev.type == xkb_event_base && ev.any.xkb_type == XkbIndicatorStateNotify) {
      displayState(ev.indicators.state);
    }
  }
  return 0;
}

void displayState(unsigned int state) {
  if (state & (1 << 0)) {
    printf("CAPS ");
  }
  if (state & (1 << 1)) {
    printf("NUM ");
  }
  if (state & (1 << 2)) {
    printf("SCROLL");
  }
  printf("\n");
  fflush(stdout);
}
