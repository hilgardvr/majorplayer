develop: 
	docker stop majorplayer && docker run --rm  -p 5432:5432 --name majorplayer -e POSTGRES_PASSWORD=password -d postgres:16.2-alpine3.19

connect-db-local: 
	psql -h localhost -p 5432 -U postgres 
