all:
	rm -rf *~ */*~ */*/*~;
	rm -rf */*.beam;
	rm -rf *.beam erl_crash.dump */erl_crash.dump */*/erl_crash.dump
doc_gen:
	rm -rf doc/*;
	erlc ../doc_gen.erl;
	erl -s doc_gen start -sname doc
test:
	# start lib_service
	rm -rf ../lib_service/ebin/* ../lib_service/test_ebin/* ../lib_service/erl_crash.dump;
	cp ../lib_service/src/*app ../lib_service/ebin;
	erlc -I ../include -o ../lib_service/ebin ../lib_service/src/*.erl;
	rm -rf *.beam ebin/* test_ebin/* erl_crash.dump;
	cp src/*app ebin;
	erlc -I ../include -o ebin src/*.erl;
	erlc -I ../include -o test_ebin test_src/*.erl;
	erl -pa ../lib_service/ebin -pa ebin -pa test_ebin -s master_service_tests start -sname master_test
