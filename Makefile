VERSION = $(shell grep -m1 VERSION $(SRC) | cut -f 2 -d'"')

PREFIX ?= /usr/local
MANPREFIX ?= $(PREFIX)/share/man
DESKTOPPREFIX ?= $(PREFIX)/share/applications
DESKTOPICONPREFIX ?= $(PREFIX)/share/icons/hicolor
STRIP ?= strip
PKG_CONFIG ?= pkg-config
INSTALL ?= install
CP ?= cp

CFLAGS_OPTIMIZATION ?= -O3

O_DEBUG := 0  # debug binary
O_NORL := 1  # no readline support - my Emacs has issues loading the shared library
O_PCRE := 0  # link with PCRE library
O_NOLOC := 0  # no locale support
O_NOMOUSE := 0  # no mouse support
O_NOBATCH := 0  # no built-in batch renamer
O_NOFIFO := 0  # no FIFO previewer support
O_CTX8 := 0  # enable 8 contexts
O_ICONS := 0  # support icons-in-terminal
O_NERD := 0  # support icons-nerdfont
O_QSORT := 0  # use Alexey Tourbin's QSORT implementation
O_BENCH := 0  # benchmark mode (stops at first user input)
O_NOSSN := 0  # enable session support

# convert targets to flags for backwards compatibility
ifneq ($(filter debug,$(MAKECMDGOALS)),)
	O_DEBUG := 1
endif
ifneq ($(filter norl,$(MAKECMDGOALS)),)
	O_NORL := 1
endif
ifneq ($(filter noloc,$(MAKECMDGOALS)),)
	O_NORL := 1
	O_NOLOC := 1
endif

ifeq ($(O_DEBUG),1)
	CPPFLAGS += -DDBGMODE
	CFLAGS += -g
endif

$(echo no rl)
CPPFLAGS += -DNORL



ifeq ($(O_PCRE),1)
	CPPFLAGS += -DPCRE
	LDLIBS += -lpcre
endif

ifeq ($(O_NOLOC),1)
	CPPFLAGS += -DNOLOCALE
endif

ifeq ($(O_NOMOUSE),1)
	CPPFLAGS += -DNOMOUSE
endif

ifeq ($(O_NOBATCH),1)
	CPPFLAGS += -DNOBATCH
endif

ifeq ($(O_NOFIFO),1)
	CPPFLAGS += -DNOFIFO
endif

ifeq ($(O_CTX8),1)
	CPPFLAGS += -DCTX8
endif

ifeq ($(O_ICONS),1)
	CPPFLAGS += -DICONS
endif

ifeq ($(O_NERD),1)
	CPPFLAGS += -DNERD
endif

ifeq ($(O_QSORT),1)
	CPPFLAGS += -DTOURBIN_QSORT
endif

ifeq ($(O_BENCH),1)
	CPPFLAGS += -DBENCH
endif

ifeq ($(O_NOSSN),1)
	CPPFLAGS += -DNOSSN
endif

ifeq ($(shell $(PKG_CONFIG) ncursesw && echo 1),1)
	CFLAGS_CURSES ?= $(shell $(PKG_CONFIG) --cflags ncursesw)
	LDLIBS_CURSES ?= $(shell $(PKG_CONFIG) --libs   ncursesw)
else ifeq ($(shell $(PKG_CONFIG) ncurses && echo 1),1)
	CFLAGS_CURSES ?= $(shell $(PKG_CONFIG) --cflags ncurses)
	LDLIBS_CURSES ?= $(shell $(PKG_CONFIG) --libs   ncurses)
else
	LDLIBS_CURSES ?= -lncurses
endif

CFLAGS += -std=c11 -Wall -Wextra -Wshadow -fPIC
#CFLAGS += $(CFLAGS_OPTIMIZATION)
CFLAGS += $(CFLAGS_CURSES)
CFLAGS += $(CPPFLAGS)

LDLIBS += $(LDLIBS_CURSES)

# static compilation needs libgpm development package
ifeq ($(O_STATIC),1)
	LDFLAGS += -static
	LDLIBS += -lgpm
endif

.PHONY: clean

nnn-module.so: nnn-module.o nnn/src/nnn.o
	$(CC) $(CFLAGS) $(LDFLAGS) -shared -o $@ $^ # $(LDLIBS)
nnn-module.o: nnn-module.c nnn-module.h nnn/src/nnn.h
nnn/src/nnn.o: nnn/src/nnn.c nnn/src/nnn.h nnn/src/qsort.h

clean:
	rm -f *.o *.so
	rm -f nnn/src/*.o
