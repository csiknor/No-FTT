# Use a base image with Elm compiler for the build stage
FROM node:alpine AS build

# Install Elm compiler
COPY elm_installer.sh /tmp/
RUN apk add --no-cache curl \
    && chmod +x /tmp/elm_installer.sh \
    && /tmp/elm_installer.sh

RUN npm install -g uglify-js

# Copy the source code and compile
COPY elm.json index.html compile_app.sh /tmp/
COPY src /tmp/src/
RUN --mount=type=cache,target=/root/.elm \
    chmod +x /tmp/compile_app.sh && /tmp/compile_app.sh

# Use the official Nginx image for the final stage
FROM nginx:alpine

# Copy only the generated elm.js and index.html from the build stage
COPY --from=build /tmp/elm.min.js /tmp/index.html /usr/share/nginx/html/

# Copy a custom Nginx configuration for API routing
COPY nginx.conf /etc/nginx/nginx.conf

# Expose port 80 for HTTP traffic
EXPOSE 80

# Start Nginx when the container launches
CMD ["nginx", "-g", "daemon off;"]
