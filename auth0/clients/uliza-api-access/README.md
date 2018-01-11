# Uliza API access

This sample demonstrates how to use Auth0 to access the Uliza API (http://dev.uliza.fm/api/v1/), by implementing the [Authorization Code Grant](https://tools.ietf.org/html/rfc6749#section-4.1) in a Node.js application. In this authentication flow, the browser receives an Authorization Code from Auth0 (known as the Authorization Server in OAuth2.0 terminology). Subsequent to receiving this code, the application will interact with Auth0 and exchange the code for an `access_token`. The application then uses this token to call the Uliza API on behalf of the user (the Resource Owner).

## Prerequisites

Make sure you have `node` and `npm` installed. Then install the dependencies.

```bash
npm install
```

## Configuration

In the project's root directory, create and populate the `.env` file with the application's `AUTH0_CLIENT_ID`, `AUTH0_CLIENT_SECRET`, and `AUTH0_DOMAIN`. The client secret must be retrieved from the [Auth0 dashboard](https://manage.auth0.com/#/clients/Kyn4B2Ce460JAW9QvrXc0Q3B04U98N0e/settings). 

#### Sample `.env` file

```
AUTH0_CLIENT_ID=ROCL8CvBnSJ2sVGk3NTZGDiAZOMlm26Z
AUTH0_DOMAIN=farmradio.eu.auth0.com
AUTH0_CLIENT_SECRET=xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
```

## Running the app

```bash
npm start
```

The app will be served at [http://localhost:3001/](http://localhost:3001/).
