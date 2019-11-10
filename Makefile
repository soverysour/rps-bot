default: run

profile:
	-@clear
	-@stack build --profile
	-@stack exec --profile -- rps-bot-exe +RTS -p -hc -i0.01
	-@stack exec -- hp2ps -c rps-bot-exe.hp && ps2pdf rps-bot-exe.ps
	-@mv rps-bot-exe.prof profiling-data/prof/`date -Iminutes`.prof
	-@mv rps-bot-exe.hp profiling-data/heap-prof/`date -Iminutes`.hp
	-@mv rps-bot-exe.pdf profiling-data/heap-vis/`date -Iminutes`.pdf
	-@profiteur profiling-data/prof/`date -Iminutes`.prof
	-@mv profiling-data/prof/`date -Iminutes`.prof.html profiling-data/prof-vis/`date -Iminutes`.prof.html
	-@rm rps-bot-exe.aux
	-@rm rps-bot-exe.ps

clean-profile:
	-@rm profiling-data/heap-prof/*
	-@rm profiling-data/heap-vis/*
	-@rm profiling-data/prof/*
	-@rm profiling-data/prof-vis/*

run:
	-@stack run rps-bot-exe

swagger:
	-@stack run swagger-spec-exe

trace:
	-@stack test --trace --coverage

test:
	-@stack test --coverage

doc:
	-@stack haddock

clean:
	-@stack clean
	-@clear

.PHONY: default test run clean bench profile doc run-spec swagger ghcid ghcid-exe
