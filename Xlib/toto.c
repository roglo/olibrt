// sudo apt install libxft-dev libfreetype-dev libxrandr-dev
// gcc toto.c -o toto -lX11 -lXft -lXrandr -lfontconfig -I/usr/include/freetype2
#include <X11/Xlib.h>
#include <X11/Xft/Xft.h>
#include <X11/extensions/Xrandr.h>
#include <fontconfig/fontconfig.h>
#include <stdio.h>

void print_font_info(Display* display, XftFont* font) {
    if (!font) {
        printf("Aucune police chargée.\n");
        return;
    }
    // Utilise directement le pattern de la police
    FcPattern* pattern = font->pattern;
    if (!pattern) {
        printf("Impossible d'obtenir le pattern de la police.\n");
        return;
    }
    // Extrait les propriétés du pattern
    FcChar8* family;
    FcChar8* style;
    double size;

    if (FcPatternGetString(pattern, FC_FAMILY, 0, &family) == FcResultMatch) {
        printf("Famille : %s\n", family);
    }

    if (FcPatternGetString(pattern, FC_STYLE, 0, &style) == FcResultMatch) {
        printf("Style : %s\n", style);
    }

    if (FcPatternGetDouble(pattern, FC_SIZE, 0, &size) == FcResultMatch) {
        printf("Taille : %.2f\n", size);
    }
}

void get_screen_size_mm(Display *display, int *width_mm, int *height_mm) {
  int event_base, error_base, r;
  XRROutputInfo *output_info;
  XRRScreenResources *resources;
  Window root;
  root = RootWindow(display, 0);
  if (!XRRQueryExtension(display, &event_base, &error_base)) {
    fprintf(stderr, "<W> XRandR not available.\n");
    r = 0;
  }
  else {
    resources = XRRGetScreenResources(display, root);
    if (!resources) {
      fprintf (stderr, "<W> Impossible to get XRandR resources.\n");
      r = 0;
    }
    else {
      r = 0;
      for (int i = 0; i < resources->noutput && r == 0; i++) {
        output_info = XRRGetOutputInfo(display, resources, resources->outputs[i]);
	if (output_info) {
	  if (output_info->connection == RR_Connected &&
	      output_info->mm_width > 0) {
	    *width_mm = output_info->mm_width;
	    *height_mm = output_info->mm_height;
	    r = 1;
	  }
	  XRRFreeOutputInfo(output_info);
	}
      }
      XRRFreeScreenResources(resources);
    }
  }
  if (r == 0) {
    *width_mm = DisplayWidthMM(display, DefaultScreen(display));
    *height_mm = DisplayHeightMM(display, DefaultScreen(display));
  }
}

void main ()
{
  Display* display;
  int screen;
  XftFont* font;
  Window window;
  XftColor color;
  XEvent xev;
  XftDraw *draw;
  XWindowAttributes attrs;
  float dpmm;
  int x_pixels, width_mm, height_mm;

  display = XOpenDisplay(NULL);
  screen = DefaultScreen(display);
  printf("screen width = %d\n", DisplayWidth(display, screen));
  printf("screen width mm = %d (not sure)\n", DisplayWidthMM(display, screen));
  get_screen_size_mm(display, &width_mm, &height_mm);
  printf("screen (width, height) in mm = (%d, %d)\n", width_mm, height_mm);
  dpmm =
    (double)DisplayWidth(display, screen) /
    (double)DisplayWidthMM(display, screen);
  printf("dpmm = %g (not sure)\n", dpmm);
  dpmm = (double)DisplayWidth(display, screen) / (double)width_mm;
  printf("dpmm = %g\n", dpmm);
  font = XftFontOpenName(display, screen, "mono:size=12");
  if (font) print_font_info(display, font);
  window = XCreateSimpleWindow(display, DefaultRootWindow(display),
			       0, 0, (int)(200 * dpmm), (int)(150 * dpmm), 0, 0, 0);
  XSelectInput(display, window, ExposureMask);
  XMapWindow(display, window);
  XGetWindowAttributes(display, window, &attrs);
  draw = XftDrawCreate(display, window, attrs.visual, attrs.colormap);
  XftColorAllocName(display, attrs.visual, attrs.colormap, "white", &color);
  while (1) {
    XNextEvent(display, &xev);
    if (xev.type == Expose) {
      if (xev.xexpose.count == 0) {
	XftDrawString8(draw, &color, font, (int)(10 * dpmm), (int)(20 * dpmm),
		       (XftChar8 *)"Bonjour", strlen("Bonjour"));
	XFlush(display);
      }
    }
  }
  XftFontClose(display, font);
}

