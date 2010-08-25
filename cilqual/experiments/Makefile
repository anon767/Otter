MIXDIR=$(CURDIR)/..

MIX=env OCAMLRUNPARAM=b $(MIXDIR)/cilqual.pl \
		$(MIX_FLAGS) $(MIX_EXTRAFLAGS) $(MIX_PRELUDE)
MIX.mix=$(MIX) --domix
MIX.merge=$(MIX) --merge --keepmerged -D_FORTIFY_SOURCE=0

MIX_FLAGS=--stats

all : vsftpd

vsftpd : experiment//vsftpd-2.0.7.pldi2010
experiment//vsftpd-2.0.7.pldi2010 : MIX_GOAL=vsftpd
experiment//vsftpd-2.0.7.pldi2010 : MIX_COMB=vsftpd_comb.c

clean : clean-experiment//vsftpd-2.0.7.pldi2010

# Set up the directory containing the experiment by unarchiving an archive and applying a patch.
# An experiment is indicated by the existance of a patch file named $(TARGET).$(TAG).experiment.patch. This rule will
# extract the file $(TARGET).tar.gz into the directory $(TARGET).$(TAG).experiment and apply the patch. $(TAG) may
# be any string to distinguish different experiments for the same target.
.PRECIOUS : %.experiment
%.experiment : %.experiment.patch
	mkdir -p $*.experiment \
	&& tar xzvf $(basename $*).tar.gz --strip-components 1 -C $*.experiment \
	&& patch -p1 -d $*.experiment < $*.experiment.patch

# Set up the directory to put the results.
.PRECIOUS : %.results
%.results :
	mkdir -p $@


# Run the experiment by first taking a snapshot of the date and system profile, then building the target repeatedly.
# The file %.experiment.h, along with a macro counter named EXPERIMENT_CONDITION is used to customize the build.
# EXPERIMENT_CONDITION starts with 0, and is incremented for each build. The file %.experiment.h must signal the final
# build by returning an error using the compiler directive "#error No more experiment conditions.".
experiment//% : %.experiment %.experiment.h | %.results
	(date; \
			uname -a; \
			which -s system_profiler && system_profiler -detailLevel mini SPHardwareDataType;) \
		| tee $*.results/system_profile.log; \
	for ((EXPERIMENT_CONDITION=0; ; EXPERIMENT_CONDITION++)); do \
		if $(CC) -DEXPERIMENT_CONDITION=$$EXPERIMENT_CONDITION -E $(CURDIR)/$*.experiment.h 2>&1 \
				| grep -q "No more experiment conditions."; then \
			exit; \
		fi; \
		$(MAKE) \
			MIX_GOAL=$(MIX_GOAL) \
			MIX_COMB=$(MIX_COMB) \
			MIX_EXTRAFLAGS="-DEXPERIMENT_CONDITION=$$EXPERIMENT_CONDITION -include $(CURDIR)/$*.experiment.h" \
			mix//$*.experiment 2>&1 | tee $*.results/$*.$$EXPERIMENT_CONDITION.log; \
	done;

# Update the patch for the experiment.
update-experiment//% :
	if [ ! -d $*.experiment ]; then \
		echo "Experiment directory $*.experiment not found."; \
		exit 1; \
	fi; \
	mkdir -p $(basename $*).orig \
	&& tar xzvf $(basename $*).tar.gz --strip-components 1 -C $(basename $*).orig \
	&& diff -urb -x *.o -x *_comb.c -x ___extra_files $(basename $*).orig $*.experiment | tee $*.experiment.patch; \
	ERROR=$${PIPESTATUS[0]}; \
	$(RM) -r $(basename $*).orig \
	exit $$ERROR

# Delete the experiment directory.
clean-experiment//% : %.experiment.patch %.experiment.h
	$(RM) -r $*.experiment $*.results


# Rebuild and run Mix on the target. Do it in two passes, since running Cil's merger and Mix's analysis in one pass
# is unreliable (Cil's merger does not seem to merge all Cil metadata correctly).
mix//% : MAKEGOALS=CC="$(MIX.merge)" clean $(MIX_GOAL)
mix//% : make//%
	$(MIX.mix) $*/$(MIX_COMB)


.PRECIOUS : %/Makefile
%/Makefile : %/Makefile.in %/configure Makefile
	cd $* && ./configure $(CONFIGURE_FLAGS) $(CONFIGURE_EXTRAFLAGS)

%/Makefile : %/configure Makefile
	cd $* && ./configure $(CONFIGURE_FLAGS) $(CONFIGURE_EXTRAFLAGS)

.PRECIOUS : %/configure
%/configure : %/configure.ac
	cd $* && autoreconf

%/configure : %/configure.in
	cd $* && autoreconf

make//% : %/Makefile $(PRELUDE)
	$(MAKE) -C $* $(MAKEGOALS)

clean//% :
	$(MAKE) -C $* clean

