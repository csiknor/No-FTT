# Agent Guidelines for No-FTT

## Build & Test
- **Build Elm**: `elm make src/Main.elm --optimize --output=elm.js`
- **Dev Server**: `node proxy-server/server.js` (runs on http://localhost:3000)
- **Docker Build**: `docker build -t no-ftt:latest . && docker run -p 9000:80 --rm no-ftt:latest`
- **No test suite**: This project has no automated tests configured in elm.json

## Code Style
- **Language**: Elm 0.19.1, functional and immutable by design
- **Imports**: Group by stdlib (Browser, Html, Http), third-party, then local modules; expose specific functions
- **Formatting**: Use elm-format conventions (4-space indentation, explicit type annotations for top-level functions)
- **Types**: Always provide type annotations for top-level functions; use type aliases for complex structures
- **Naming**: camelCase for functions/values, PascalCase for types/modules; descriptive names (e.g., `getBalances`, `ApiState`)
- **Error Handling**: Use Result/Maybe types; convert HTTP errors to strings with `httpErrorToString`
- **Architecture**: Follow Elm Architecture (Model, Msg, update, view); separate concerns into domain modules (Balance, Transfer, etc.)

## Project Structure
- `src/`: Elm source modules (Main.elm is entry point)
- `proxy-server/`: Express.js proxy for Wise API (no CORS support)
- Uses Bootstrap CSS via elm-zen-css-bootstrap for styling
- Single-page application with no backend persistence
