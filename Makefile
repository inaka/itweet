
ERLARGS?=-config itweet

all:
	rebar get-deps && rebar compile

clean:
	rebar clean

build_plt: all
	rebar skip_deps=true build-plt

analyze: all
	dialyzer -pa deps/*/ebin --plt ~/.itweet_dialyzer_plt -Wunmatched_returns -Werror_handling -Wbehaviours ebin

doc: all
	rebar skip_deps=true doc

xref: all
	rebar skip_deps=true xref

test: all
#	echo "Running eunit tests..."
#	if [ -f test.config ]; then \
#		erl -noshell -config test -pa ebin -pa deps/*/ebin +Bc +K true -smp enable -s crypto -s ibrowse -s ssl -s itweet -run itweep_tests main; \
#	else \
#		erl -noshell              -pa ebin -pa deps/*/ebin +Bc +K true -smp enable -s crypto -s ibrowse -s ssl -s itweet -run itweep_tests main; \
#	fi
	echo "Running common tests..."
	# Create the directory in which common test will output.
	mkdir -p log/ct
	# Tell rebar to run common test tests.
	# We define the flags that rebar will pass on to the erlang VM.
	ERL_FLAGS="${ERLARGS}" ERL_AFLAGS="${ERLARGS}" rebar skip_deps=true ct
	open log/ct/index.html

shell: all
	erl ${ERLARGS} -pa ebin -pa deps/*/ebin +Bc +K true -smp enable -boot start_sasl -s crypto -s ibrowse -s ssl -s lager -s itweet
