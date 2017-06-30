# Node.js web app login and API access

This sample demonstrates how to use Auth0 to implement the [Authorization Code Grant](https://tools.ietf.org/html/rfc6749#section-4.1) in a Node.js application. In this authentication flow, the browser receives an Authorization Code from Auth0. After receiving this code, the application will interact with Auth0 and exchange it for an `access_token`. The application then uses this `access_token` to call the API on behalf of the user.

This application can be used together with the `api-server` example to show a complete implementation of the [Authorization Code Grant](https://tools.ietf.org/html/rfc6749#section-4.1) flow. 

### Sample `.env` file

```
AUTH0_CLIENT_ID=Kyn4B2Ce460JAW9QvrXc0Q3B04U98N0e
AUTH0_DOMAIN=farmradio.eu.auth0.com
AUTH0_CLIENT_SECRET=xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
```

