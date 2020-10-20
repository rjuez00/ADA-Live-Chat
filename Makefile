all: client_chaining server_chaining clean


client_chaining: 
	gnatmake -I/usr/local/ll/lib -I./hash_maps_g_chaining chat_client_2.adb

server_chaining: 
	gnatmake -I/usr/local/ll/lib -I./hash_maps_g_chaining chat_server_2.adb

client_open: 
	gnatmake -I/usr/local/ll/lib -I./hash_maps_g_open chat_client_2.adb

server_open: 
	gnatmake -I/usr/local/ll/lib -I./hash_maps_g_open chat_server_2.adb

run_client1: 
	make client_open
	make clean
	./chat_client_2 $(shell hostname) 6123 rjuez

run_client2:
	./chat_client_2 $(shell hostname) 6123 tryell49

run_client3:
	./chat_client_2 $(shell hostname) 6123 adrilanza


run_server:
	make server_open
	make clean
	./chat_server_2 6123 2
	
test_open:
	gnatmake hash_maps_g_open/hash_maps_test_string.adb
	clear
	./hash_maps_test_string

test_chaining:
	gnatmake hash_maps_g_chaining/hash_maps_test_string.adb
	clear
	./hash_maps_test_string

test_ordered:
	gnatmake ordered_maps_test.adb
	./ordered_maps_test

clean: 
	rm -f *.o
	rm -f *.ali
	clear

deep_clean: 
	make clean
	rm -f chat_client_2 chat_server_2 hash_maps_test hash_maps_test_string ordered_maps_test ordered_maps_test_integer
	clear
