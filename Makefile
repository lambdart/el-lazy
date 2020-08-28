# Emacs invocation
# Don't load an init file: -q
# Avoid processing X resources: -Q
# Send messages to stderr: --batch
EMACS = emacs -Q -q --batch

# Remove command
RM = rm

# Additional emacs load-path and autoload
LOAD_PATH := -L ${PWD}
LOAD_AUTOLOAD := -l autoload

# Define Compile Command
# Call batch-byte-compile function: -f
COMPILE  = -f batch-byte-compile

# AUTOLOAD related variables
AUTOLOAD_UPDATE = -f batch-update-autoloads
AUTOLOAD_FILE := "${PWD}/lazy-autoloads.el"
AUTOLOAD_EVAL := --eval '(setq generated-autoload-file ${AUTOLOAD_FILE})'

# Expand the source code files
EL != ls *.el

# Compiled files
ELC = ${EL:.el=.elc}

# Default goal
all: compile autoload

# Compile needed files
compile: $(ELC)

# Translate lisp text (.el) files in byte compiled (.elc) files
$(ELC): $(EL)
	${EMACS} ${LOAD_PATH} ${COMPILE} ${.ALLSRC}

autoload:
	${EMACS} ${LOAD_AUTOLOAD} ${AUTOLOAD_EVAL} ${AUTOLOAD_UPDATE}

clean:
	${RM} ${ELC}
