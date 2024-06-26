develop: 
	(docker stop majorplayer-db 2> /dev/null || true) && (docker rm majorplayer-db 2> /dev/null || true) && docker run -p 5432:5432 --name majorplayer-db -e POSTGRES_PASSWORD=password -d postgres:16.2-alpine3.19

podman-develop: 
	(podman stop majorplayer-db 2> /dev/null || true) && (podman rm majorplayer-db 2> /dev/null || true) && podman run -p 5432:5432 --name majorplayer-db -e POSTGRES_PASSWORD=password -d postgres:16.2-alpine3.19

connect-db-local: 
	psql -h localhost -p 5432 -U postgres 

db-up:
	docker start majorplayer-db 
