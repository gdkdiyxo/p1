# FU-OJT-BE
Project OJT registration management for subject SWP391

### Hosted Server swagger
http://ojt-management.link/backend/swagger-ui.html

### Local swagger
http://localhost:9004/backend/swagger-ui.html

### To build
mvn clean install -DskipTests -f
docker-compose build

### To run with docker container
docker run -d -p 9004:9004 --name backend thanhthu0321/ojt-manager-backend:latest

due to network issue you can run database locally
docker run --name postgresql-container -p 5432:5432 -e POSTGRES_PASSWORD=sa123456 -d postgres
