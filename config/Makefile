all: bin2NC.x library

DIR = .

include make.macros

# Suffix-rules:  Begin by throwing away all old suffix-
# rules, and then create new ones for compiling
# *.f90-files.

.SUFFIXES:
.SUFFIXES: .f90 .o
.SUFFIXES: .F90 .o

# Compilation rules
$(DIR)/%.o: $(DIR)/%.f90
	$(FC) $(F_FLAGS_EXTD) $(FFLAGS) ${INCLUDES} -c $(DIR)/$*.f90 -o $(DIR)/$*.o

$(DIR)/%.o: $(DIR)/%.F90
	$(FC) $(F_FLAGS_EXTD) $(F_PRE_PROCESS) $(FFLAGS) ${INCLUDES} -c $(DIR)/$*.F90 -o $(DIR)/$*.o

# Include the dependency-list created by makedepf90 below
include .depend

# target 'clean' for deleting object- *.mod- and other
# unwanted files

clean:
	rm -f *.mod *.o *genmod* libbin2NC.a bin2NC.x

strict: clean depend all

# Create a dependency list using makedepf90.  All files
# that needs to be compiled to build the program,
# i.e all source files except include files, should
# be given on the command line to makedepf90.
#
# The argument to the '-o' option will be the name of the
# resulting program when running 'make'

depend .depend:
		$(FMAKEDEPEND) -o bin2NC.x $(DIR)/*.f* $(DIR)/*.F* > .depend

library:
		ar -cvr libbin2NC.a $(FOBJ) *.mod

# The compilation line of makedepf90 is by default:
#   3 emic.x: $(FOBJ)
#   4 →       $(FC) -o $@ $(FFLAGS) $(LDFLAGS) $(FOBJ) $(LIBS)

# The End of All Things (op. cit.)
