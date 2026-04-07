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

float get_screen_dpi(Display *display) {
    int event_base, error_base;
    if (!XRRQueryExtension(display, &event_base, &error_base)) {
        fprintf(stderr,
		"XRandR non disponible. Utilisation de 96 DPI par défaut.\n");
        return 96.0;
    }
    Window root = RootWindow(display, 0);
    XRRScreenResources *resources = XRRGetScreenResources(display, root);
    if (!resources) {
      fprintf
	(stderr,
	 "Impossible de récupérer les ressources XRandR. \
 Utilisation de 96 DPI.\n");
        return 96.0;
    }
    float dpi = 96.0;
    for (int i = 0; i < resources->noutput; i++) {
        XRROutputInfo *output_info =
	  XRRGetOutputInfo(display, resources, resources->outputs[i]);
        if (!output_info) continue;

        if (output_info->connection == RR_Connected &&
	    output_info->mm_width > 0) {
            XRRCrtcInfo *crtc_info =
	      XRRGetCrtcInfo(display, resources, output_info->crtc);
            if (crtc_info && crtc_info->width > 0) {
                dpi = (crtc_info->width * 25.4) / output_info->mm_width;
                XRRFreeCrtcInfo(crtc_info);
                break;
            }
            if (crtc_info) XRRFreeCrtcInfo(crtc_info);
        }
        XRRFreeOutputInfo(output_info);
    }

    XRRFreeScreenResources(resources);
    return dpi;
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
  int x_pixels;

  display = XOpenDisplay(NULL);
  screen = DefaultScreen(display);
  printf("screen width = %d\n", DisplayWidth(display, screen));
  printf("screen width mm = %d\n", DisplayWidthMM(display, screen));
  dpmm = DisplayWidth(display, screen) / DisplayWidthMM(display, screen);
  printf("dpmm = %g\n", dpmm);
  printf("get_screen_dpi = %g\n", get_screen_dpi(display));
  font = XftFontOpenName(display, screen, "mono:size=12");
  if (font) print_font_info(display, font);
  window = XCreateSimpleWindow(display, DefaultRootWindow(display),
			       0, 0, 400, 300, 0, 0, 0);
  XSelectInput(display, window, ExposureMask);
  XMapWindow(display, window);
  XGetWindowAttributes(display, window, &attrs);
  draw = XftDrawCreate(display, window, attrs.visual, attrs.colormap);
  XftColorAllocName(display, attrs.visual, attrs.colormap, "white", &color);
  while (1) {
    XNextEvent(display, &xev);
    if (xev.type == Expose) {
      if (xev.xexpose.count == 0) {
	XftDrawString8(draw, &color, font, 10, 20,
		       (XftChar8 *)"Bonjour", strlen("Bonjour"));
	XFlush(display);
      }
    }
  }
  XftFontClose(display, font);
}

