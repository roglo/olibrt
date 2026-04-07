// sudo apt install libxft-dev libfreetype-dev
// gcc toto.c -o toto -lX11 -lXft -lfontconfig -I/usr/include/freetype2
#include <X11/Xlib.h>
#include <X11/Xft/Xft.h>
#include <fontconfig/fontconfig.h>
#include <stdio.h>

XftFont* open_xft_font(Display* display, int screen, const char* font_name) {
    // Charge la police Xft par son nom (ex: "mono:size=12")
    XftFont* font = XftFontOpenName(display, screen, font_name);
    if (!font) {
        fprintf(stderr, "Erreur : Impossible de charger la police '%s'.\n", font_name);
        // Optionnel : Essayer une police de secours (ex: "fixed:size=12")
        font = XftFontOpenName(display, screen, "fixed:size=12");
        if (!font) {
            fprintf(stderr, "Erreur : Impossible de charger la police de secours.\n");
            return NULL;
        }
    }
    return font;
}

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

void main ()
{
  Display* display = XOpenDisplay(NULL);  // Supposé déjà ouvert
  int screen = DefaultScreen(display);
  XftFont* font = open_xft_font(display, screen, "mono:size=12");
  Window window =
    XCreateSimpleWindow(display, DefaultRootWindow(display),
			0, 0, 400, 300, 0, 0, 0);
  Pixmap pixmap = XCreatePixmap(display, window, 100, 100, 8);
  XftColor color;
  XEvent xev;
  XftDraw *draw;
  XWindowAttributes attrs;

  if (font) {
    print_font_info(display, font);
  }
  XftColorAllocName(display, DefaultVisual(display, 0),
		    DefaultColormap(display, 0), "white", &color);
  XMapWindow(display, window);
  XGetWindowAttributes(display, window, &attrs);
  draw = XftDrawCreate(display, window, attrs.visual, attrs.colormap);
  XftDrawString8(draw, &color, font, 10, 20,
		 (XftChar8 *)"Bonjour", strlen("Bonjour"));
  while (1) {
    XNextEvent(display, &xev);
  }
  XftFontClose(display, font);
}

