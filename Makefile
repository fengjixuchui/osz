
RAKE = rake

all:
	$(RAKE)

clean:
	$(RAKE) clobber

run: all 8086run/8086run
	$(RAKE) run

8086run/8086run:
	(cd 8086run; make)
