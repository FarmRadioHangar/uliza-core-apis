# Node.js web app login and API access

This sample demonstrates how to use Auth0 to implement the [Authorization Code Grant](https://tools.ietf.org/html/rfc6749#section-4.1) in a Node.js application. In this authentication flow, the browser receives an Authorization Code from Auth0 (the Authorization Server). After receiving this code, the application will interact with Auth0 and exchange it for an `access_token`. The application then uses this token to call the API on behalf of the user (the Resource Owner).

This application can be used together with the [api-server example](auth0/api-server) to show a complete implementation of the [Authorization Code Grant](https://tools.ietf.org/html/rfc6749#section-4.1) flow. 

## Prerequisites

Make sure you have `node` and `npm` installed. Then install the dependencies.

```bash
npm install
```

## Configuration

In the project's root directory, create and populate the `.env` file with the application's `AUTH0_CLIENT_ID`, `AUTH0_CLIENT_SECRET`, and `AUTH0_DOMAIN`. The client secret must be retrieved from the [Auth0 dashboard](https://manage.auth0.com/#/clients/Kyn4B2Ce460JAW9QvrXc0Q3B04U98N0e/settings). 

#### Sample `.env` file

```
AUTH0_CLIENT_ID=Kyn4B2Ce460JAW9QvrXc0Q3B04U98N0e
AUTH0_DOMAIN=farmradio.eu.auth0.com
AUTH0_CLIENT_SECRET=xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
```

## Running the app

```bash
npm start
```

The app will be served at [http://localhost:3001/](http://localhost:3001/).

## API access

For this demo to work, the [api-server example](auth0/api-server) must be running and listening on port 8080. 
