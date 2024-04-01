develop: 
	(docker stop majorplayer 2> /dev/null || true) && (docker rm majorplayer 2> /dev/null || true) && docker run -p 5432:5432 --name majorplayer -e POSTGRES_PASSWORD=password -d postgres:16.2-alpine3.19

connect-db-local: 
	psql -h localhost -p 5432 -U postgres 
