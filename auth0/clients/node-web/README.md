# Node.js web app login 

This sample demonstrates how to implement Auth0 authentication in a Node.js application.

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

## Credits

This example uses code from [Auth0's quickstarts](https://auth0.com/docs/quickstarts) example projects.
