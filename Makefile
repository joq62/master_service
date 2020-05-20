all:
	rm -rf ebin/* src/*~;
	erlc -o ebin src/*.erl;
	cp src/*.app ebin;
	erl -pa ebin -s node_controller_service start -sname node_controller_service
server:
	rm -rf ebin/* src/*~ ;
	erlc -o ebin src/*.erl;
	cp src/*.app ebin;
	erl -pa ebin -sname test_tcp_server

test:
	rm -rf ebin/* src/*~ test_ebin/* test_src/*~;
#	cp /home/pi/erlang/d/source/include/*.hrl ".";
	erlc -D local -I /home/pi/erlang/d/source/include -o ebin src/*.erl;
	erlc -D local -I /home/pi/erlang/d/source/include -o test_ebin test_src/*.erl;
	cp src/*.app ebin;
	erl -pa ebin -pa test_ebin -s lib_service_tests start -sname test_lib_service
