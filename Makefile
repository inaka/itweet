all:
	rebar get-deps && rebar compile
	erl -pa ebin -noinput +B -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'

clean:
	rebar clean

build_plt: all
	rebar skip_deps=true build-plt

analyze: all
	dialyzer --plt ~/.itweet_dialyzer_plt -Wunmatched_returns -Werror_handling -Wbehaviours ebin

doc: all
	rebar skip_deps=true doc

xref: all
	rebar skip_deps=true xref
	
test: all
	if [ -f test.config ]; then erl -noshell -config test -pa ebin -pa deps/riak_err/ebin -pa deps/ibrowse/ebin +Bc +K true -smp enable -s crypto -s ibrowse -run itweep_tests main; else erl -noshell -pa ebin -pa deps/riak_err/ebin -pa deps/ibrowse/ebin +Bc +K true -smp enable -s crypto -s ibrowse -run itweep_tests main; fi

shell: all
	erl -pa ebin -pa deps/riak_err/ebin -pa deps/ibrowse/ebin +Bc +K true -smp enable -boot start_sasl -s crypto -s ibrowse