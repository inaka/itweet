all:
	rebar get-deps && rebar compile
	erl -pa ebin -noinput +B -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'

clean:
	rebar clean

test: all
	rebar skip_deps=true eunit

build_plt: all
	rebar skip_deps=true build-plt

analyze: all
	dialyzer --plt ~/.itweet_dialyzer_plt -Wunmatched_returns -Werror_handling -Wbehaviours ebin

doc: all
	rebar skip_deps=true doc

xref: all
	rebar skip_deps=true xref
	
run: all
	erl -pa ebin -pa deps/riak_err/ebin -pa deps/ibrowse/ebin +Bc +K true -smp enable -boot start_sasl -s crypto -s ibrowse
