# No-FTT

A web application that uses the Wise API to transfer money in a way that will avoid the extra fee introduced because of the FTT (Financial Transaction Tax) in Hungary. It splits the transfer amount into multiple smaller transfers which are below the personal limit and sends them to the recipient.

## Features
* Login into your Wise account with an API key
* Select balance to transfer from and recipient to transfer to (only own accounts)
* Calculates the limit for the transfer based on the rate between the selected currency and HUF
* Splits the transfer amount into smaller transfers below the limit
* Requests quotes, creates transfers and fund them after confirmation
* Provides option to cancel pending transfers

## Installation
1. Clone the repository
2. Build and run the docker image
    ```bash 
    docker build -t no-ftt:latest . && docker run -p 9000:80 --rm no-ftt:latest
    ```
3. Open the application in your browser at http://localhost:9000

## Usage
1. Create an API token on the Wise web UI: `Your account` / `Settings` / `Integrations and tools` / `Developer tools` / `API tokens`
    > **Warning!** The API token is sensitive information, do not share it with anyone. If you suspect that it has been compromised, you can revoke it on the Wise web UI.
    >
    > For extra security add your IP address to the `Allowed IPs` list. 
2. Visit the application in your browser at http://localhost:9000
3. Enter your username and API token and click `Connect`
4. Select the balance to transfer from and the recipient to transfer to
5. Enter the amount to transfer, change the auto-calculate limit if needed and click `Split & Quote`
6. Review the quotes created and ensure the total price is 0.
7. Add the transfer reference if any and click `Transfer`
8. Review the created transfers and click `Fund` to confirm
    * Alternatively, you can click `Cancel` to cancel the transfers

## Development

The application is written in [Elm](https://elm-lang.org/) and located in the `src` directory. It uses the [Bootstrap](https://getbootstrap.com/) CSS framework for styling.

> **Note:** The application is a single page application (SPA) and does not require a backend server. However, the Wise API doesn't support [CORS](https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS) so the application uses a proxy to route the API requests.

### Local development

1. Install Elm using the [official guide](https://guide.elm-lang.org/install/elm.html)
2. Build the application using the Elm compiler
    ```bash
    elm make src/Main.elm --optimize --output=elm.js
    ```
3. Install Node and pnpm using the [official guide](https://pnpm.io/installation)
4. Install the dependencies in the proxy-server directory
    ```bash
    cd proxy-server && pnpm install
    ```
5. Start the Express proxy server
    ```bash
    node proxy-server/server.js
    ```
6. Run the Elm Reactor
    ```bash
    elm reactor
    ```
7. Visit the application in your browser at http://localhost:8000/index.html

> **Note:** The Elm Reactor and the Express proxy server listen on different ports. Therefore, you need to modify the API URL in the [`src/Api.elm`](src/Api.elm) file to `http://localhost:3000`.
> ```elm
> wiseUrl =
>     B.crossOrigin "http://localhost:3000" [ "api" ] []
> ```

### Deployment

The application is deployed as a Docker container. The [`Dockerfile`](Dockerfile) builds the Elm application and serves it using an Nginx server which also proxies the Wise API requests.

> **Note:** The API URL in the [`src/Api.elm`](src/Api.elm) file is set to `/api` to match the [Nginx configuration](nginx.conf).
> ```elm
> wiseUrl =
>     B.absolute [ "api" ] []
> ```

1. Build and run the docker image
    ```bash 
    docker build -t no-ftt:latest . && docker run -p 9000:80 --rm no-ftt:latest
    ```
2. Open the application in your browser at http://localhost:9000

> **Note:** Given the application is built during the Docker image build process, it can be used for development as well.

## License
This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.