# Makefile for bosor5
#
HOME       = ${BUSHNELL}
PROG       = $(HOME)/bosor5

# macro definitions
# =================

include    $(HOME)/Makefile.in

#----------------------------------------------------------------------------

.PRECIOUS:  $(EXE)/assemble.$(MACHINE)\
	    $(EXE)/bosormain.$(MACHINE)\
	    $(EXE)/bosorpost.$(MACHINE)\
	    $(EXE)/bosorread.$(MACHINE)\
	    $(EXE)/getsegs.$(MACHINE)\
	    $(EXE)/help5.$(MACHINE)\
	    $(EXE)/input.$(MACHINE)\
	    $(EXE)/mainsetup.$(MACHINE)\
	    $(EXE)/modify.$(MACHINE)\
	    $(EXE)/postsetup.$(MACHINE)

bosor5: $(EXE)/assemble.$(MACHINE)\
        $(EXE)/bosormain.$(MACHINE)\
        $(EXE)/bosorpost.$(MACHINE)\
        $(EXE)/bosorread.$(MACHINE)\
        $(EXE)/getsegs.$(MACHINE)\
        $(EXE)/help5.$(MACHINE)\
        $(EXE)/input.$(MACHINE)\
	$(EXE)/mainsetup.$(MACHINE)\
        $(EXE)/modify.$(MACHINE)\
	$(EXE)/postsetup.$(MACHINE)

$(EXE)/assemble.$(MACHINE): $(LIB)/assemble.a
	ar xv $(LIB)/assemble.a assmbl.o
	$(FC) $(FFLAGS) -o $@ assmbl.o $(LIB)/assemble.a
	/bin/rm -f assmbl.o

$(EXE)/bosormain.$(MACHINE): $(LIB)/bosormain.a $(UTIL)/ieeexx_$(MACHINE).o
	ar xv $(LIB)/bosormain.a main.o
	$(FC) $(FFLAGS) -o $@ main.o $(LIB)/bosormain.a \
                                     $(UTIL)/ieeexx_$(MACHINE).o
	/bin/rm -f main.o

$(EXE)/bosorpost.$(MACHINE): $(LIB)/bosorpost.a \
                             $(UTIL)/pakupk_$(MACHINE).o \
                             $(UTIL)/ieeexx_$(MACHINE).o
	ar xv $(LIB)/bosorpost.a post.o
	$(FC) $(FFLAGS) -o $@ post.o $(LIB)/bosorpost.a \
                                     $(UTIL)/pakupk_$(MACHINE).o \
                                     $(UTIL)/ieeexx_$(MACHINE).o
	/bin/rm -f post.o

$(EXE)/bosorread.$(MACHINE): $(LIB)/bosorread.a \
                             $(UTIL)/ieeexx_$(MACHINE).o
	ar xv $(LIB)/bosorread.a b5read.o
	$(FC) $(FFLAGS) -o $@ b5read.o $(LIB)/bosorread.a \
                                       $(UTIL)/ieeexx_$(MACHINE).o
	/bin/rm -f b5read.o

$(EXE)/getsegs.$(MACHINE): $(LIB)/getsegs.a
	ar xv $(LIB)/getsegs.a getseg.o
	$(FC) $(FFLAGS) -o $@ getseg.o $(LIB)/getsegs.a
	/bin/rm -f getseg.o

$(EXE)/help5.$(MACHINE): $(LIB)/help5.a
	ar xv $(LIB)/help5.a help5.o
	$(FC) $(FFLAGS) -o $@ help5.o $(LIB)/help5.a
	/bin/rm -f help5.o

$(EXE)/input.$(MACHINE):	$(LIB)/input.a
	ar xv $(LIB)/input.a input.o
	$(FC) $(FFLAGS) -o $@ input.o $(LIB)/input.a
	/bin/rm -f input.o

$(EXE)/mainsetup.$(MACHINE):	$(LIB)/mainsetup.a
	ar xv $(LIB)/mainsetup.a mains.o
	$(FC) $(FFLAGS) -o $@ mains.o $(LIB)/mainsetup.a
	/bin/rm -f mains.o

$(EXE)/modify.$(MACHINE): $(LIB)/modify.a
	ar xv $(LIB)/modify.a modify.o
	$(FC) $(FFLAGS) -o $@ modify.o $(LIB)/modify.a
	/bin/rm -f modify.o

$(EXE)/postsetup.$(MACHINE):	$(LIB)/postsetup.a
	ar xv $(LIB)/postsetup.a posts.o
	$(FC) $(FFLAGS) -o $@ posts.o $(LIB)/postsetup.a
	/bin/rm -f posts.o

# Update $(LIB)/assemble.a
$(LIB)/assemble.a: $(OBJ)/assemble.a
	cp $(OBJ)/assemble.a $@

# Update $(LIB)/bosormain.a
$(LIB)/bosormain.a: $(OBJ)/arrays.a $(OBJ)/buckle.a  $(OBJ)/gasp.a\
                    $(OBJ)/main.a  $(OBJ)/pre11.a    $(OBJ)/pre22.a\
                    $(OBJ)/pre33.a $(OBJ)/prompter.a $(OBJ)/stoget.a\
                    $(OBJ)/util.a
	csh $(BIN)/updatelib $@ $?

# Update $(LIB)/bosorpost.a
$(LIB)/bosorpost.a: $(OBJ)/gasp.a   $(OBJ)/mode.a   $(OBJ)/plast.a\
                    $(OBJ)/plotit.a $(OBJ)/post.a   $(OBJ)/prompter.a\
                    $(OBJ)/stoget.a $(OBJ)/util.a   $(UTIL)/dattim_$(MACHINE).o
	csh $(BIN)/updatelib $@ $?

# Update $(LIB)/bosorread.a
$(LIB)/bosorread.a: $(OBJ)/gasp.a     $(OBJ)/geom.a   $(OBJ)/hookup.a\
                    $(OBJ)/prompter.a $(OBJ)/readit.a $(OBJ)/ring.a\
                    $(OBJ)/segmnt.a   $(OBJ)/stoget.a $(OBJ)/util.a\
                    $(OBJ)/wall.a
	csh $(BIN)/updatelib $@ $?

# Update $(LIB)/getsegs.a
$(LIB)/getsegs.a: $(OBJ)/getsegs.a
	cp $(OBJ)/getsegs.a $@

# Update $(LIB)/help5.a 
$(LIB)/help5.a: $(OBJ)/help5.a $(OBJ)/felippa.a
	csh $(BIN)/updatelib $@ $?

# Update $(LIB)/input.a
$(LIB)/input.a: $(OBJ)/input.a $(OBJ)/questions.a $(OBJ)/prompter.a
	csh $(BIN)/updatelib $@ $?

# Update $(LIB)/mainsetup.a
$(LIB)/mainsetup.a: $(OBJ)/mainsetup.a $(OBJ)/prompter.a $(OBJ)/util.a
	csh $(BIN)/updatelib $@ $?

# Update $(LIB)/modify.a
$(LIB)/modify.a: $(OBJ)/modify.a $(OBJ)/input.a $(OBJ)/prompter.a\
                 $(OBJ)/questions.a
	csh $(BIN)/updatelib $@ $?
	-ar dv $@ input.o

# Update $(LIB)/postsetup.a
$(LIB)/postsetup.a: $(OBJ)/postsetup.a $(OBJ)/prompter.a $(OBJ)/util.a
	csh $(BIN)/updatelib $@ $?

# Update $(OBJ)/arrays.a
$(OBJ)/arrays.a: $(SRC)/arrays.src
	csh $(BIN)/updateobj $(FC) "$(OPTFFLAGS)" $(SRC)/arrays.src $@

# Update $(OBJ)/assemble.a
$(OBJ)/assemble.a: $(SRC)/assemble.src
	csh $(BIN)/updateobj $(FC) "$(FFLAGS)" $(SRC)/assemble.src $@

# Update $(OBJ)/buckle.a
$(OBJ)/buckle.a: $(SRC)/buckle.src
	csh $(BIN)/updateobj $(FC) "$(OPTFFLAGS)" $(SRC)/buckle.src $@

# Update $(OBJ)/felippa.a
$(OBJ)/felippa.a: $(SRC)/felippa.src
	csh $(BIN)/updateobj $(FC) "$(FFLAGS)" $(SRC)/felippa.src $@

# Update $(OBJ)/gasp.a
$(OBJ)/gasp.a: $(UTIL)/gasp.F $(UTIL)/bio.c
	cd $(UTIL) &&\
        /bin/rm -f gasp_$(MACHINE).f &&\
        fpp -P -D${MACHINE} gasp.F gasp_$(MACHINE).f &&\
        $(FC) $(FFLAGS) -c  gasp_$(MACHINE).f &&\
        /bin/rm -f gasp_$(MACHINE).f &&\
        cp -f bio.c bio_$(MACHINE).c &&\
        $(CC) $(CFLAGS) -c bio_$(MACHINE).c &&\
        ar rv $@ gasp_$(MACHINE).o bio_$(MACHINE).o &&\
        /bin/rm -f gasp_$(MACHINE).o bio_$(MACHINE).o &&\
        ranlib $@

# Update $(OBJ)/geom.a
$(OBJ)/geom.a: $(SRC)/geom.src
	csh $(BIN)/updateobj $(FC) "$(FFLAGS)" $(SRC)/geom.src $@

# Update $(OBJ)/getsegs.a
$(OBJ)/getsegs.a: $(SRC)/getsegs.src
	csh $(BIN)/updateobj $(FC) "$(FFLAGS)" $(SRC)/getsegs.src $@

# Update $(OBJ)/help5.a
$(OBJ)/help5.a: $(SRC)/help5.src
	csh $(BIN)/updateobj $(FC) "$(FFLAGS)" $(SRC)/help5.src $@

# Update $(OBJ)/hookup.a
$(OBJ)/hookup.a: $(SRC)/hookup.src
	csh $(BIN)/updateobj $(FC) "$(FFLAGS)" $(SRC)/hookup.src $@

# Update $(OBJ)/input.a
$(OBJ)/input.a: $(SRC)/input.src
	csh $(BIN)/updateobj $(FC) "$(FFLAGS)" $(SRC)/input.src $@

# Update $(OBJ)/main.a
$(OBJ)/main.a: $(SRC)/main.src
	csh $(BIN)/updateobj $(FC) "$(OPTFFLAGS)" $(SRC)/main.src $@

# Update $(OBJ)/mainsetup.a
$(OBJ)/mainsetup.a: $(SRC)/mainsetup.src
	csh $(BIN)/updateobj $(FC) "$(FFLAGS)" $(SRC)/mainsetup.src $@

# Update $(OBJ)/mode.a
$(OBJ)/mode.a: $(SRC)/mode.src
	csh $(BIN)/updateobj $(FC) "$(FFLAGS)" $(SRC)/mode.src $@

# Update $(OBJ)/modify.a
$(OBJ)/modify.a: $(SRC)/modify.src
	csh $(BIN)/updateobj $(FC) "$(FFLAGS)" $(SRC)/modify.src $@

# Update $(OBJ)/plast.a
$(OBJ)/plast.a: $(SRC)/plast.src
	csh $(BIN)/updateobj $(FC) "$(FFLAGS)" $(SRC)/plast.src $@

# Update $(OBJ)/plotit.a
$(OBJ)/plotit.a: $(SRC)/plotit.src
	csh $(BIN)/updateobj $(FC) "$(FFLAGS)" $(SRC)/plotit.src $@

# Update $(OBJ)/post.a
$(OBJ)/post.a: $(SRC)/post.src
	csh $(BIN)/updateobj $(FC) "$(FFLAGS)" $(SRC)/post.src $@

# Update $(OBJ)/postsetup.a
$(OBJ)/postsetup.a: $(SRC)/postsetup.src
	csh $(BIN)/updateobj $(FC) "$(FFLAGS)" $(SRC)/postsetup.src $@

# Update $(OBJ)/pre11.a
$(OBJ)/pre11.a: $(SRC)/pre11.src
	csh $(BIN)/updateobj $(FC) "$(OPTFFLAGS)" $(SRC)/pre11.src $@

# Update $(OBJ)/pre22.a
$(OBJ)/pre22.a: $(SRC)/pre22.src
	csh $(BIN)/updateobj $(FC) "$(FFLAGS)" $(SRC)/pre22.src $@

# Update $(OBJ)/pre33.a
$(OBJ)/pre33.a: $(SRC)/pre33.src
	csh $(BIN)/updateobj $(FC) "$(OPTFFLAGS)" $(SRC)/pre33.src $@

# Update $(OBJ)/prompter.a
$(OBJ)/prompter.a: $(SRC)/prompter.src
	csh $(BIN)/updateobj $(FC) "$(FFLAGS)" $(SRC)/prompter.src $@

# Update $(OBJ)/questions.a
$(OBJ)/questions.a: $(SRC)/questions.src
	csh $(BIN)/updateobj $(FC) "$(FFLAGS)" $(SRC)/questions.src $@

# Update $(OBJ)/readit.src
$(OBJ)/readit.a: $(SRC)/readit.src
	csh $(BIN)/updateobj $(FC) "$(FFLAGS)" $(SRC)/readit.src $@

# Update $(OBJ)/ring.a
$(OBJ)/ring.a: $(SRC)/ring.src
	csh $(BIN)/updateobj $(FC) "$(FFLAGS)" $(SRC)/ring.src $@

# Update $(OBJ)/segmnt
$(OBJ)/segmnt.a: $(SRC)/segmnt.src
	csh $(BIN)/updateobj $(FC) "$(FFLAGS)" $(SRC)/segmnt.src $@

# Update $(OBJ)/stoget.a
$(OBJ)/stoget.a: $(SRC)/stoget.src
	csh $(BIN)/updateobj $(FC) "$(FFLAGS)" $(SRC)/stoget.src $@

# Update $(OBJ)/util.a
$(OBJ)/util.a: $(SRC)/util.src
	csh $(BIN)/updateobj $(FC) "$(FFLAGS)" $(SRC)/util.src $@

# Update $(OBJ)/wall.a
$(OBJ)/wall.a: $(SRC)/wall.src
	csh $(BIN)/updateobj $(FC) "$(FFLAGS)" $(SRC)/wall.src $@

$(UTIL)/dattim_$(MACHINE).o: $(UTIL)/dattim.c
	cd $(UTIL) && \
	$(CC) $(CFLAGS) -c dattim.c && \
	mv dattim.o dattim_$(MACHINE).o

$(UTIL)/pakupk_$(MACHINE).o: $(UTIL)/pakupk.c
	cd $(UTIL) && \
        $(CC) $(CFLAGS) -D$(MACHINE) -c pakupk.c && \
        mv pakupk.o pakupk_$(MACHINE).o

$(UTIL)/sig_$(MACHINE).o: $(UTIL)/sig.F
	cd $(UTIL) &&\
        /bin/rm -f sig_$(MACHINE).f &&\
        fpp -P -D${MACHINE} sig.F sig_$(MACHINE).f &&\
        $(FC) $(FFLAGS) -c sig_$(MACHINE).f &&\
        /bin/rm -f sig_$(MACHINE).f
