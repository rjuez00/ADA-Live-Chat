all: client server clean


client: 
	gnatmake -I/usr/local/ll/lib chat_client_2.adb

server: 
	gnatmake -I/usr/local/ll/lib chat_server_2.adb

run_client1: 
	make client
	make clean
	./chat_client_2 rodrigo-ubuntu 6123 rjuez

run_client2:
	./chat_client_2 rodrigo-ubuntu 6123 tryell49

run_client3:
	./chat_client_2 rodrigo-ubuntu 6123 adrilanza


run_server:
	make server
	make clean
	./chat_server_2 6123 2
	


clean: 
	rm -f *.o
	rm -f *.ali
	clear

deep_clean: 
	make clean
	rm -f chat_client_2
	rm -f chat_server_2
	clear

